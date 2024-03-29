PROGRAM chimerencdf2grib
!--------------------------------------------------------------------------
! Legge un file di netcdf di Chimere e lo scrive in formato GRIB
! Uso: chimerencdf2grib.exe filein fileout fileinfo igen
!                      [-out/-met/-bio/-ini/-bc/-emibio/-eminv/-aodem]
!
! NOTE:
! La gestione dei files NetCDF con aerosol in formato multi-bin
! NON E'TESTATA!!
!
! Stato delle opzioni: (label crev)
! - funzionanti: out
! - da verificare: met, bio, emibio, ini, eminv
! - da implementare: bc
! I grib in quota sono codificati come livelli ibridi, ma senza includere i
!   vertical coordinate parameters. 
!
! Per leggere dati MACC:
! - occorre specificare le opzioni: -grd MACCEU -rmis -999. -vt YYYYMMDDHH
! - i dati operativi Copernicus sono scritti con Scanning mode 000 (par. 1.6)
! - tutti i dati sono scritti in ug/m3, anche se la codifica grib-SIMC dei gas 
!   richiederebbe ppb.
!
!                               Versione 3.4.0, Michele & Enrico 30/06/2018
!--------------------------------------------------------------------------
use calendar 
use netcdf
IMPLICIT NONE

INTERFACE
  FUNCTION lowercase(chin) RESULT (chout)
    IMPLICIT NONE
    CHARACTER (*), INTENT(IN) :: chin
    CHARACTER (LEN=LEN_TRIM(chin)) :: chout
  END FUNCTION lowercase

  FUNCTION uppercase(chin) RESULT (chout)
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(IN) :: chin
    CHARACTER (LEN=LEN_TRIM(chin)) :: chout
  END FUNCTION uppercase
END INTERFACE

! Parametri costanti
INTEGER, PARAMETER :: maxdim = 1000000 ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxvar = 200     ! n.ro max var. in output Chimere
INTEGER, PARAMETER :: maxlev = 100     ! n.ro max livelli nel file di input .nc
INTEGER, PARAMETER :: maxbin = 12      ! n.ro max bin aerosol nel file di input .nc
REAL, PARAMETER :: eps = 0.0001        ! tolleranza per uguaglianza estemi griglia

CHARACTER (LEN=120) :: tab_path_def = PKGDATAROOTDIR
CHARACTER (LEN=120) :: tab_env = "MA_UTILS_DAT"

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),kword,kret,nbit
REAL    :: psec2(512),psec3(2)

! Altre variabili del programma
REAL, ALLOCATABLE :: conc_out(:,:,:),conc_outb(:,:,:,:),tot(:),conc_miss(:,:,:)
REAL :: x1,y1,x2,y2,dx,dy,xr,yr,rmis
INTEGER :: version
INTEGER :: nvarout,nx,ny,np,smf,nl,slen,mm,nxi,nyi,ntrov,ntrovs,nscri,nscris
INTEGER :: code_var(maxvar),tab_var(maxvar),aero_var(maxvar)
INTEGER :: lev_out(maxlev),bin_out(maxbin)
INTEGER :: cem,igen,idata,idata_ini,scad_ini
INTEGER :: iu,k,kp,kvar,kscad,klev,kbin,kday,kh,ios,eof,eor,cnt_grb,idp,kpt
INTEGER :: inp_fmt,info_fmt,p1,p2,nhead,iproj
CHARACTER (LEN=250) :: chrec
CHARACTER (LEN=120) :: filein,fileout,fileinfo,chdum,arg(4),tab_file(2),tab_path
CHARACTER (LEN=3) :: proj
CHARACTER (LEN=1) :: next_arg
LOGICAL :: verbose,dok,lnbin

CHARACTER(LEN=20):: namevar(maxvar)
!
! Length of a date buffer in CHIMERE
integer,parameter :: dlen=19

type::varmeta
   real::min
   real::max
   integer::id3
   integer::ndims

   logical::is3d
   logical::isreal
   character(len=16)::varname
end type varmeta

type(varmeta),allocatable,dimension(:)::varids1

integer,external::iargc
real,allocatable::buf2d1(:,:)
real,allocatable::emisb(:,:,:),emisb1(:,:)
real,allocatable::buf3d1(:,:,:)
real,allocatable::buf4d1(:,:,:,:)
real,allocatable::conc1(:),conc2(:,:),conc3(:,:,:),conc3b(:,:,:),conc4(:,:,:,:)
real,allocatable::conc(:,:,:)

! return status of all netCDF functions calls
integer :: ncstat,ncstat1,ncstat2,ncstat3,dimid,dimid1,dimid2
! file identifier
integer :: ncid
! dimension identifiers
integer :: dstrdimid
integer :: h_dimid
integer :: biospe_dimid
integer :: latspe_dimid

! dimension lengths
integer :: times1
integer :: dstrlen
integer :: nzonal
integer :: nmerid
integer :: nlev
integer :: nbin
integer :: nhori
! variable identifiers
integer :: nvarin
integer :: ivar,ivarid
integer:: vartype1,times1_varid,typeday,biospecies,biospe_varid,latspe_varid,species

character(len=dlen)   :: datebuf

logical::is3d1,rtm

integer::mm5date2numeric
character(len=10)::domain,rdomain

!==========================================================================
! 1) Elaborazioni preliminari

! Nei files NetCDF relativi ad output Chimere sono presenti 10 variabili fisse:
! out_fmt=1: lon,lat,a_vcoord,b_vcoord,cut_off_diameters,Times,hlay,airm,relh,temp (10)
! 
!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

idp = 0
inp_fmt = 1
version = 2007
rmis = -HUGE(0.)
domain = ""
next_arg = ""
rtm = .FALSE.

DO kp = 1,HUGE(kp)
   CALL getarg(kp,chdum)
   IF (TRIM(chdum) == "-h") THEN
     CALL write_help
     STOP 1
   ELSE IF (TRIM(chdum) == "") THEN  
     EXIT
   ELSE IF (TRIM(chdum) == "-grd") THEN
     next_arg = "G"
   ELSE IF (TRIM(chdum) == "-vt") THEN
     rtm = .TRUE.
     next_arg = "T"
   ELSE IF (TRIM(chdum) == "-rmis") THEN
     next_arg = "M"
   ELSE IF (next_arg == "G") THEN
     domain = TRIM(chdum)
     next_arg = ""
   ELSE IF (next_arg == "T") THEN
     READ (chdum,'(i10)') idata
     next_arg = ""
   ELSE IF (next_arg == "M") THEN
     READ (chdum,*) rmis
     next_arg = ""
   ELSE IF (TRIM(chdum) == "-out") THEN
     inp_fmt = 1  
   ELSE IF (TRIM(chdum) == "-met") THEN
     inp_fmt = 2  
   ELSE IF (TRIM(chdum) == "-bio") THEN
     inp_fmt = 3  
   ELSE IF (TRIM(chdum) == "-ini") THEN
     inp_fmt = 4  
   ELSE IF (TRIM(chdum) == "-bc") THEN
     inp_fmt = 5  
   ELSE IF (TRIM(chdum) == "-emibio") THEN
     inp_fmt = 6  
   ELSE IF (TRIM(chdum) == "-eminv") THEN
     inp_fmt = 7  
   ELSE IF (TRIM(chdum) == "-aodem") THEN
     inp_fmt = 8  
   ELSE IF (idp >= 4) THEN
     CALL write_help
     STOP 1
   ELSE   
      idp = idp + 1
      arg(idp) = chdum
   ENDIF
ENDDO

filein = arg(1)
fileout = arg(2)
fileinfo = arg(3)
READ (arg(4),*,IOSTAT=ios) igen

IF (filein == "" .OR. fileout == "" .OR. fileinfo == "" .OR. &
     ios /= 0 .OR. TRIM(filein) == "-h") THEN
   CALL write_help
   STOP 1
ENDIF

!--------------------------------------------------------------------------
! 1.2 Leggo dal file NetCDF le dimensioni della matrice dei dati (x,y,z,t,var)

! Apro il file
ncstat=nf90_open(filein,NF90_NOWRITE,ncid)

! Area
IF (domain == "") THEN
  ncstat=nf90_get_att(ncid,NF90_GLOBAL,'Domain',domain)
  IF (ncstat/=NF90_NOERR) CALL attr_err(filein,"Domain")
ENDIF

! Numero istanti
ncstat1 = nf90_inq_dimid(ncid,'Time',dimid1)
ncstat2 = nf90_inq_dimid(ncid,'time',dimid2)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=times1)
ELSE IF (ncstat2 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid2,len=times1)
ELSE
  WRITE (*,*) "N.ro istanti temporali non presente nel file NetCDF"
ENDIF

! Valore istanti
ncstat1=nf90_inq_varid(ncid,'Times',times1_varid)
ncstat2=nf90_inq_dimid(ncid,'DateStrLen',dstrdimid)
ncstat3=nf90_inquire_dimension(ncid,dstrdimid,len=dstrlen)
IF (.NOT.rtm .AND. (ncstat1/=NF90_NOERR .OR. ncstat2/=NF90_NOERR .OR. &
  ncstat3/=NF90_NOERR)) GOTO 9989

! Tipo di giorni (per emi)
ncstat=nf90_inq_dimid(ncid,'type_day',dimid)
IF (inp_fmt == 7 .AND. ncstat == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid,len=typeday)
ELSE
  typeday=1
ENDIF

! Nx
ncstat1 = nf90_inq_dimid(ncid,'west_east',dimid1)
ncstat2 = nf90_inq_dimid(ncid,'longitude',dimid2)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=nzonal)
ELSE IF (ncstat2 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid2,len=nzonal)
ELSE
  WRITE (*,*) "N.ro celle in direzione X non presente nel file NetCDF"
ENDIF

! Ny
ncstat1 = nf90_inq_dimid(ncid,'south_north',dimid1)
ncstat2 = nf90_inq_dimid(ncid,'latitude',dimid2)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=nmerid)
ELSE IF (ncstat2 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid2,len=nmerid)
ELSE
  WRITE (*,*) "N.ro celle in direzione Y non presente nel file NetCDF"
ENDIF

! Nz
IF (inp_fmt == 8) THEN
  nlev = 1

ELSE
  ncstat1 = nf90_inq_dimid(ncid,'bottom_top',dimid1)
  ncstat2 = nf90_inq_dimid(ncid,'level',dimid2)
  IF (ncstat1 == NF90_NOERR) THEN
    ncstat=nf90_inquire_dimension(ncid,dimid1,len=nlev)
  ELSE IF (ncstat2 == NF90_NOERR) THEN
    ncstat=nf90_inquire_dimension(ncid,dimid2,len=nlev)
  ELSE
    WRITE (*,*) "N.ro livelli non presente nel file NetCDF, assumo un solo livello"
    nlev = 1
  ENDIF

ENDIF

! Nbin
ncstat1 = nf90_inq_dimid(ncid,'nbins',dimid1)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=nbin)
  lnbin = .TRUE.
ELSE
  nbin = 1
  lnbin = .FALSE.
ENDIF

! Nhori (Solo per BC; numero celle di bordo? crev)
IF (inp_fmt == 5) THEN
  ncstat=nf90_inq_dimid(ncid,'h_boundary',h_dimid) 
  is3d1=(ncstat==NF90_NOERR)
  IF (is3d1) THEN
    ncstat=nf90_inquire_dimension(ncid,h_dimid,len=nhori)
  ELSE
    WRITE (*,*) "Quota livelli (h_boundary) non presente nel file NetCDF"
  ENDIF
ENDIF

! Nvarin (numero di variabili nel file NetCDF)
IF (inp_fmt==5) THEN !crev
  ncstat=nf90_inq_dimid(ncid,'Species',latspe_dimid)
  is3d1=(ncstat==NF90_NOERR)
  IF (is3d1) THEN
    ncstat=nf90_inquire_dimension(ncid,latspe_dimid,len=species)
    ncstat=nf90_inq_varid(ncid,'conc',latspe_varid)
    WRITE (*,*) 'latspe_varid',latspe_varid
    nvarin = species
  ELSE
    GOTO 9994
  ENDIF

ELSE IF (inp_fmt==6) THEN !crev
  ncstat=nf90_inq_dimid(ncid,'biospecies',biospe_dimid) !crev
  is3d1=(ncstat==NF90_NOERR)
  IF (is3d1) THEN
    ncstat=nf90_inquire_dimension(ncid,biospe_dimid,len=biospecies)
    ncstat=nf90_inq_varid(ncid,'emisb',biospe_varid)
    WRITE (*,*)'biospoe_varid',biospe_varid
    nvarin = biospecies
  ELSE
    GOTO 9994
  ENDIF

ELSE
  ncstat=nf90_inquire(ncid, nVariables=nvarin)

ENDIF

!--------------------------------------------------------------------------
! 1.3 Leggo dal file aree_geo.dat o aree_utm.dat i parametri della griglia

tab_path = ""
CALL GETENV(tab_env,tab_path)
IF (TRIM(tab_path) == "") tab_path = tab_path_def

WRITE (tab_file(1),'(2a)') TRIM(tab_path),"/aree_utm.dat"
WRITE (tab_file(2),'(2a)') TRIM(tab_path),"/aree_geo.dat"

tabs: DO kpt = 1,2
  OPEN (UNIT=31, FILE=tab_file(kpt), STATUS="OLD", ERR= 9993)
  dok = .FALSE.
  DO
    READ (31,'(a)',IOSTAT=ios) chrec
    IF (ios /= 0) CYCLE tabs
    IF (TRIM(chrec) == "" .OR. chrec(1:1) == "!") CYCLE

    IF (kpt == 1) THEN
      READ (chrec,'(a10,2(1x,i4),4(1x,f8.3))',IOSTAT=ios) &
        rdomain,nx,ny,x1,y1,x2,y2
    ELSE IF (kpt == 2) THEN
      READ (chrec,'(a10,2(1x,i4),6(1x,f8.3))',IOSTAT=ios) &
        rdomain,nx,ny,x1,y1,x2,y2,xr,yr
    ENDIF
    IF (ios /= 0) GOTO 9992
    
    IF (TRIM(ADJUSTL(lowercase(rdomain))) == &
        TRIM(ADJUSTL(lowercase(domain)))) THEN
      dok = .TRUE.
      EXIT tabs
    ENDIF
  ENDDO

ENDDO tabs
IF (.NOT. dok) GOTO 9988

! Grandezze derivate
IF (kpt == 1) THEN
  proj = "UTM"
ELSE IF (kpt == 2) THEN
  IF (ABS(xr) < eps .AND. ABS(yr) < eps) THEN
    proj = "GEO"
  ELSE
    proj = "ROT"
  ENDIF
ELSE
  GOTO 9998
ENDIF

dx = (x2-x1)/REAL(nx-1)
dy = (y2-y1)/REAL(ny-1)
np = nx*ny

IF (TRIM(domain) == "MACCEU") THEN
  smf = 0
ELSE
  smf = 64
ENDIF

IF (nx /= nzonal .OR. ny /= nmerid) GOTO 9991

! Se formato BC, stendo i 4 bordi su un vettore 1D ...
IF (inp_fmt == 5) THEN
   nx = (nmerid+nzonal)*2 + 4
   ny = 1
   x2 = x1 + (nx-1)*dx
   y2 = y1
ENDIF

! Proiezione geografica

!--------------------------------------------------------------------------
! 1.4 leggo da fileinfo livelli e variabili richiesti in output

OPEN (UNIT=30, FILE=fileinfo, STATUS="OLD", ERR=9999)

aero_var(:) = 0
k=0
DO
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT

! Salto righe vuote e di commento
  chrec = ADJUSTL(chrec)
  IF (chrec(1:1) == "!" .OR. TRIM(chrec) == "") CYCLE
  k = k+1

! Distinguo tra vecchio e nuovo formato di fileinfo
  IF (k ==1) THEN
    IF (TRIM(chrec) == "V2016") THEN
      info_fmt = 3
      nhead = 5
      if (.NOT. lnbin) GOTO 9986 
    ELSE IF (TRIM(chrec) == "V2013") THEN
      info_fmt = 2
      nhead = 5
    ELSE
      info_fmt = 1
      nbit = 24
      nhead = 6
    ENDIF
  ENDIF

! Interpreto la riga letta: vecchio formato (CHIMERE_INFO.DAT)
  IF (info_fmt == 1) THEN

    IF (k==1) THEN
      READ (chrec,*,IOSTAT=ios) cem
    ELSE IF (k==2 .OR. k==3) THEN
      CONTINUE
    ELSE IF (k==4) THEN
      READ (chrec,*,IOSTAT=ios) lev_out(1:nlev)
    ELSE IF (k==5) THEN
      READ (chrec,*,IOSTAT=ios) scad_ini
    ELSE IF (k>=nhead+1 .AND. k<=maxvar+nhead) THEN
       READ (chrec,*,IOSTAT=ios) code_var(k-nhead),tab_var(k-nhead)
       p1 = index(chrec,"!")
       p2 = index(chrec(p1+1:),"!") + p1
       IF (p2 > p1) THEN
         read(chrec(p1+1:p2-1),'(a)') namevar(k-nhead)
       ELSE
         read(chrec(p1+1:),'(a)') namevar(k-nhead)
       ENDIF
       namevar(k-nhead) = ADJUSTL(TRIM(namevar(k-nhead)))
    ELSE
       WRITE (*,*) "Troppe specie, elaboro le prime ",maxvar,&
            " (aumentare param. maxvar)"
       k = k-1
       EXIT

    ENDIF
    IF (ios /= 0) GOTO 9998

! Interpreto la riga letta: formato V2013 (CHIMERE_NCINFO.DAT)
  ELSE IF (info_fmt == 2) THEN

    IF (k==1) THEN
       CONTINUE
    ELSE IF (k==2) THEN
       READ (chrec,*,IOSTAT=ios) cem
    ELSE IF (k==3) THEN
       READ (chrec,*,IOSTAT=ios) nbit
    ELSE IF (k==4) THEN
       READ (chrec,*,IOSTAT=ios) scad_ini
    ELSE IF (k==5) THEN
       READ (chrec,*,IOSTAT=ios) lev_out(1:nlev)
    ELSE IF (k>=nhead+1 .AND. k<=maxvar+nhead) THEN
       READ (chrec,'(i3,1x,i3,1x,a)',IOSTAT=ios) &
         tab_var(k-nhead),code_var(k-nhead),namevar(k-nhead)
       namevar(k-nhead) = ADJUSTL(TRIM(namevar(k-nhead)))
    ELSE
       WRITE (*,*) "Troppe specie, elaboro le prime ",maxvar,&
            " (aumentare param. maxvar)"
       k = k-1
       EXIT
    ENDIF
    IF (ios /= 0) GOTO 9998

! Interpreto la riga letta: formato V2016 (CHIMERE_NCINFO.DAT)
  ELSE IF (info_fmt == 3) THEN

    IF (k==1) THEN
       CONTINUE
    ELSE IF (k==2) THEN
       READ (chrec,*,IOSTAT=ios) cem
    ELSE IF (k==3) THEN
       READ (chrec,*,IOSTAT=ios) nbit
    ELSE IF (k==4) THEN
       READ (chrec,*,IOSTAT=ios) scad_ini
    ELSE IF (k==5) THEN
       READ (chrec,*,IOSTAT=ios) lev_out(1:nlev)
    ELSE IF (k==6) THEN
       READ (chrec,*,IOSTAT=ios) bin_out(1:nbin)
    ELSE IF (k>=nhead+1 .AND. k<=maxvar+nhead) THEN
       READ (chrec,'(i3,1x,i3,1x,i1,1x,a)',IOSTAT=ios) &
         tab_var(k-nhead),code_var(k-nhead),aero_var(k-nhead),namevar(k-nhead)
       namevar(k-nhead) = ADJUSTL(TRIM(namevar(k-nhead)))
    ELSE
       WRITE (*,*) "Troppe specie, elaboro le prime ",maxvar,&
            " (aumentare param. maxvar)"
       k = k-1
       EXIT
    ENDIF
    IF (ios /= 0) GOTO 9998

  ENDIF
ENDDO
CLOSE(30)

nvarout = k - nhead

IF (cem <= 0 .OR. nlev < 0 .OR. & 
     x1 >= x2 .OR. y1 >= y2 .OR. nx <= 1 .OR. ny <= 1  .OR. &
     (nlev==0 .AND. inp_fmt/=2 .AND. inp_fmt/=3) .OR. &
     inp_fmt < 1 .OR. inp_fmt > 9 .OR. &
     inp_fmt == 9 .AND. nlev /= 1)   GOTO 9997

!--------------------------------------------------------------------------
! 1.5 Log a schermo dei dati richiesti

WRITE (*,*) "N step temporali: ",times1
WRITE (*,*) "Parametri griglia: (",TRIM(domain),")"
WRITE (*,*) "  proj,nx,ny,nz: ",proj,nx,ny,nlev
WRITE (*,*) "  x1,y1,x2,y2:   ",x1,y1,x2,y2
WRITE (*,*) "  dx,dy:         ",dx,dy
WRITE (*,*) "  scanning mode  ",smf
WRITE (*,*) ""
WRITE (*,*) "Numero di parametri:"
WRITE (*,*) "variabili totali in input:  ",nvarin
WRITE (*,*) "variabili richieste in out: ",nvarout
WRITE (*,*) "livelli in outuput:         ",COUNT(lev_out(1:nlev) /= 0)
IF (info_fmt == 3) &
  WRITE (*,*) "bin aerosol in output:      ",COUNT(bin_out(1:nlev) /= 0)
WRITE (*,*) ""
WRITE (*,*) "Parametri scrittura grib:"
WRITE (*,*) "scadenza iniziale: ",scad_ini
WRITE (*,*) "numero bit:        ",nbit

!--------------------------------------------------------------------------
! 1.6) Definisco la parte costante dell'header dei grib

! Sezione 1
ksec1(2) = cem
ksec1(3) = igen
ksec1(4) = 255
IF (inp_fmt == 7) THEN
   ksec1(5) = 192
ELSE IF (inp_fmt /= 1) THEN
   ksec1(5) = 128
ENDIF

ksec1(14) = 0
ksec1(15) = 1
ksec1(17) = 0
ksec1(18) = 0
ksec1(19) = 0
ksec1(20) = 0
ksec1(22:) = 0

! Sezione 2
ksec2(2) = nx
ksec2(3) = ny
ksec2(5) = NINT(x1 * 1000.)
ksec2(8) = NINT(x2 * 1000.)
IF (smf == 64) THEN
  ksec2(4) = NINT(y1 * 1000.)
  ksec2(7) = NINT(y2 * 1000.)
ELSE IF (smf == 0) THEN
  ksec2(4) = NINT(y2 * 1000.)
  ksec2(7) = NINT(y1 * 1000.)
ENDIF
ksec2(11)= smf
ksec2(12)= 0

IF (proj == "UTM" .OR. proj == "GEO") THEN
   ksec2(1) = 0
   ksec2(6) = 128
   ksec2(9) = NINT(dx * 1000.)
   ksec2(10) = NINT(dy * 1000.)
   ksec2(13) = 0
   ksec2(14) = 0

ELSE IF (proj == "ROT") THEN
   ksec2(1) = 10
   ksec2(6) = 0
   ksec2(9) = 0
   ksec2(10) = 0
   ksec2(13) = NINT((yr-90.) * 1000.)
   ksec2(14) = NINT(xr * 1000.)

ELSE
   GOTO 9987

ENDIF

DO k=15,22
   ksec2(k)=0
ENDDO

! Altre sezioni
DO k=1,11
   psec2(k)=0.
ENDDO
ksec3(1) = 0
psec3(2) = rmis
ksec4(1) = np
ksec4(2) = nbit
DO k=3,33
   ksec4(k) = 0
ENDDO

!--------------------------------------------------------------------------
! 1.7 Altre operazioni

! Alloco le varaibili
ALLOCATE (buf2d1(nzonal,nmerid))
ALLOCATE (buf3d1(nzonal,nmerid,nlev))
ALLOCATE (conc1(nzonal*nmerid))
ALLOCATE (conc2(nzonal*nmerid,nlev))
ALLOCATE (conc3(nzonal*nmerid,nlev,3))

IF (inp_fmt == 1) THEN
  ALLOCATE (buf4d1(nzonal,nmerid,nlev,nbin))
ELSE IF (inp_fmt == 7) THEN
  ALLOCATE (buf4d1(nzonal,nmerid,nlev,3))
ENDIF

SELECT CASE (inp_fmt)
CASE (1,2)
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE (varids1(nvarin))
  ALLOCATE (conc3b(nzonal*nmerid,nlev,nbin))
  ALLOCATE (conc_outb(nzonal*nmerid,nlev,nbin,nvarout))
CASE (3)
  ALLOCATE (varids1(nvarin))
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE (tot(nzonal*nmerid))
CASE (4) 
  ALLOCATE (varids1(nvarin))
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
CASE (5)
  ALLOCATE (conc(species,nhori,nlev))
  ALLOCATE (conc_out(nhori,nlev,nvarout))
  ALLOCATE (conc_miss(nhori+4,nlev,nvarout))
CASE (6)
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE (emisb(biospecies,nzonal,nmerid))
  ALLOCATE (emisb1(biospecies,nzonal*nmerid))
CASE (7) 
  ALLOCATE (varids1(nvarin))
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE (conc4(nzonal*nmerid,nlev,typeday,nvarout))
CASE (8)
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE (varids1(nvarin))
END SELECT

! Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! Trovo codice per EOF !crev
CALL get_eof_eor(eof,eor)

!==========================================================================
! 2) Lettura e scrittura

WRITE (*,*)
WRITE (*,*) "Inizio elaborazioni"
CALL PBOPEN (iu,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2.1) Formato OUTPUT

IF (inp_fmt == 1) THEN

  cnt_grb = 0
  ntrov = 0
  nscri = 0
  DO kscad = 1, times1
    IF (.NOT. rtm) THEN
      ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
      idata=mm5date2numeric(datebuf)
    ENDIF
    IF (kscad == 1) idata_ini = idata
    WRITE (*,*) "Elaboro scadenza ",idata

    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
       ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    DO ivar=1,nvarout
      ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
      IF (ncstat==NF90_NOERR) THEN
        IF (kscad ==1) ntrov = ntrov + 1

        ncstat=nf90_inquire_variable(ncid,ivarid,varids1(ivarid)%varname, &
          vartype1,varids1(ivarid)%ndims)
        IF (aero_var(ivar) == 1) THEN     ! multi-bin aerosol variable
          ncstat=nf90_get_var(ncid,ivarid,buf4d1,(/1,1,1,1,kscad/),  & ! start vector
            (/nbin,nzonal,nmerid,nlev,1/))                             ! count vector
        ELSE                              ! standard variable
          ncstat=nf90_get_var(ncid,ivarid,buf3d1,(/1,1,1,kscad/),  & ! start vector
            (/nzonal,nmerid,nlev,1/))                                ! count vector
        ENDIF
          
        IF (code_var(ivar) <= 0) CYCLE
        IF (kscad ==1) THEN
          WRITE (*,'(2a,1x,4i6)') "Trovata la variabile ", &
            namevar(ivar),ivar,ivarid,tab_var(ivar),code_var(ivar)
          nscri = nscri + 1
        ENDIF

! Elaboro variable aerosol multi-bin 
        IF (aero_var(ivar) == 1) THEN     
          conc3b = RESHAPE(buf3d1,(/nzonal*nmerid,nlev,nbin/))
          conc_outb(:,:,:,ivar) = conc3b(:,:,:)

          DO kbin = 1,nbin
    
              ksec1(1) = tab_var(ivar)
            ksec1(6) = code_var(ivar)
  
            DO klev = 1,nlev
              IF (lev_out(klev) == 0) CYCLE
              ksec1(7) = 109
              ksec1(8) = klev
              ksec1(9) = 0
              IF (ANY(conc_out(1:np,klev,ivar) == rmis)) THEN
                ksec1(5) = 192
              ELSE
                ksec1(5) = 128
              ENDIF
    
              CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   conc_out(1:np,klev,ivar),np,kbuffer,maxdim,kword,'C',kret)
              CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
              cnt_grb = cnt_grb + 1
            ENDDO                 ! livelli
          ENDDO                 ! bin aerosol

! Elaboro variabilie standard
        ELSE
          conc2 = RESHAPE(buf3d1,(/nzonal*nmerid,nlev/))
          conc_out(:,:,ivar) = conc2(:,:)
  
          ksec1(1) = tab_var(ivar)
          ksec1(6) = code_var(ivar)
          DO klev = 1,nlev
            IF (lev_out(klev) == 0) CYCLE
            ksec1(7) = 109
            ksec1(8) = klev
            ksec1(9) = 0
            IF (ANY(conc_out(1:np,klev,ivar) == rmis)) THEN
              ksec1(5) = 192
            ELSE
              ksec1(5) = 128
            ENDIF
  
            CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 conc_out(1:np,klev,ivar),np,kbuffer,maxdim,kword,'C',kret)
            CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
            cnt_grb = cnt_grb + 1
          ENDDO                 ! livelli
        ENDIF

      ELSE
        IF (kscad ==1) WRITE(*,'(2a,1x,4i6)') "Variabile non trovata ", &
          namevar(ivar),ivar,ivarid,tab_var(ivar),code_var(ivar)

      ENDIF
    ENDDO                      ! specie 
  ENDDO                        ! istanti

  ncstat=nf90_close(ncid)
  WRITE (*,*) "Variabili (3D): input ",ntrov," output ",nscri
  WRITE (*,*) "Scadenze elaborate ",kscad-1," grib scritti ",cnt_grb

!--------------------------------------------------------------------------
! 2.2) Formato METEO / Exdom

ELSE IF (inp_fmt == 2) THEN

  cnt_grb = 0
  ntrov = 0
  ntrovs = 0
  nscri = 0
  nscris = 0
  DO kscad = 1, times1
    IF (.NOT. rtm) THEN
      ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
      idata=mm5date2numeric(datebuf)
    ENDIF
    IF (kscad == 1) idata_ini = idata
    WRITE (*,*) "Elaboro scadenza ",idata

    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    DO ivar=1,nvarout
      ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
      IF (ncstat==NF90_NOERR) THEN

        ncstat=nf90_inquire_variable(ncid,ivarid,varids1(ivarid)%varname, &
          vartype1,varids1(ivarid)%ndims)

        IF (varids1(ivarid)%ndims==3) THEN           ! 2D field
          IF (kscad == 1) ntrovs = ntrovs + 1
          ncstat = nf90_get_var(ncid,ivarid,buf2d1,(/1,1,kscad/), &   ! start vector
            (/nzonal,nmerid,1/))                                      ! count vector
           nl = 1
           conc1 = RESHAPE(buf2d1,(/nzonal*nmerid/))
           conc_out(:,1,ivar) = conc1(:)

        ELSE IF (varids1(ivarid)%ndims==4) THEN      ! 3D field
          IF (kscad == 1) ntrov = ntrov + 1
          ncstat = nf90_get_var(ncid,ivarid,buf3d1,(/1,1,1,kscad/), & ! start vector
            (/nzonal,nmerid,nlev,1/))                                 ! count vector
           conc2 = RESHAPE(buf3d1,(/nzonal*nmerid,nlev/))
           conc_out(:,:,ivar) = conc2(:,:)
           nl = nlev

        ENDIF

        IF (code_var(ivar) <= 0) CYCLE
        IF (kscad ==1) THEN
          IF (nl == 1) THEN
            WRITE (*,'(2a,1x,4i6)') "Trovata la variabile (2D) ", &
              namevar(ivar),ivar,ivarid,tab_var(ivar),code_var(ivar)
            nscris = nscris + 1
          ELSE
            WRITE (*,'(2a,1x,4i6)') "Trovata la variabile (3D) ", &
              namevar(ivar),ivar,ivarid,tab_var(ivar),code_var(ivar)
            nscri = nscri + 1
          ENDIF
        ENDIF

        ksec1(1) = tab_var(ivar)
        ksec1(6) = code_var(ivar)
        DO klev = 1,nl
          IF (lev_out(klev) == 0) CYCLE

!         Codifica livelli
          IF (varids1(ivarid)%ndims==4) then
            ksec1(7) = 109
            ksec1(8) = klev
            ksec1(9) = 0
          ELSE IF (namevar(ivar) == "tem2" .OR. namevar(ivar) == "sreh" ) THEN
            ksec1(7) = 105
            ksec1(8) = 2
            ksec1(9) = 0
          ELSE IF (namevar(ivar) == "w10m" .OR. namevar(ivar) == "w10s" .OR. &
                   namevar(ivar) == "u10m" .OR. namevar(ivar) == "v10m") THEN
            ksec1(7) = 105
            ksec1(8) = 10
            ksec1(9) = 0
          ELSE IF (namevar(ivar) == "soim") THEN
            ksec1(7) = 112
            ksec1(8) = 0
            ksec1(9) = 10
          ELSE
            ksec1(7) = 1
            ksec1(8) = 0
            ksec1(9) = 0
          ENDIF

!         Correggo unita' di misura (da Chimere a GRIB). Verificate Tp!!
          IF (namevar(ivar) == "alb") THEN
            conc_out(:,1,ivar) = conc_out(:,1,ivar)/100.
          ENDIF

          CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               conc_out(1:np,klev,ivar),np,kbuffer,maxdim,kword,'C',kret)
          CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
          cnt_grb = cnt_grb + 1
        ENDDO                ! livelli

      ELSE
        IF (kscad ==1) WRITE(*,'(2a,1x,4i6)') "Variabile non trovata     ", &
          namevar(ivar),ivar,ivarid,tab_var(ivar),code_var(ivar)

      ENDIF
    ENDDO                    ! specie 
  ENDDO                      ! istanti
  ncstat=nf90_close(ncid)
  WRITE (*,*) "Variabili (3D): input ",ntrov," output ",nscri
  WRITE (*,*) "Variabili (2D): input ",ntrovs," output ",nscris
  WRITE (*,*) "Scadenze elaborate ",kscad-1," grib scritti ",cnt_grb

!--------------------------------------------------------------------------
! 2.3) Formato BIOGENIC

ELSE IF (inp_fmt == 3) THEN
!!! da fare 
  DO k = 1,np
     READ (31,*,IOSTAT=ios) conc_out(k,1,1:5)
     IF (ios /= 0) GOTO 9995
  ENDDO
  tot(1:np) = SUM(conc_out(1:np,1,1:5),DIM=2)

  ksec1(7) = 1
  ksec1(8) = 0
  ksec1(9) = 0

  ksec1(21) = 20
  ksec1(10) = 100
  ksec1(11) = 1
  ksec1(12) = 1
  ksec1(13) = 1

  ksec1(16) = 0

  ! Scrivo i potenziali di emissione biogenica
  DO kvar = 1,nvarout
     ksec1(1) = 200
     ksec1(6) = 130 + kvar

     CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
          conc_out(1:np,1,kvar),np,kbuffer,maxdim,kword,'C',kret)
     CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
     IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
  ENDDO

  ! Scrivo il potenziale di emissione totale
  ksec1(1) = 200
  ksec1(6) = 136
  ksec4(2) = 24

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
       tot(1:np),np,kbuffer,maxdim,kword,'C',kret)
  CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

  WRITE (*,*) "Scritti ",nvarout+1," grib"

!--------------------------------------------------------------------------
! 2.4) Formato INI / END

ELSE IF (inp_fmt == 4) THEN

!!!!  da fare
  ksec1(10) = 100
  ksec1(11) = 1
  ksec1(12) = 1
  ksec1(13) = 0
  ksec1(21) = 20

  ksec1(16) = 0

  cnt_grb = 0
  DO kscad = 1, times1
     ntrov=0
     nscri=0
     ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
     idata=mm5date2numeric(datebuf)

     IF (kscad == 1) idata_ini = idata
     CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
          ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

     do ivar=1,nvarout
        ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
        is3d1=(ncstat==NF90_NOERR)
        if(is3d1) then
           ntrov=ntrov+1
           write(6,*)'trovo la variabile  ',namevar(ivar),ivar,ivarid
           ncstat=nf90_inquire_variable(ncid,ivarid,varids1(ivarid)%varname,vartype1,varids1(ivarid)%ndims)


           ncstat=nf90_get_var( ncid,ivarid, buf3d1, &
                (/     1,      1,       1, kscad/),  & ! start vector
                (/nzonal, nmerid, nlev,     1/))    ! count vector

           conc2=reshape(buf3d1,(/nzonal*nmerid,nlev/))
           conc_out(:,:,ivar)=conc2(:,:)

           IF (code_var(ivar) <= 0) CYCLE
           write(6,*)'scrivo  la variabile  ',namevar(ivar),ivar
           nscri=nscri+1
           ksec1(1) = tab_var(ivar)
           ksec1(6) = code_var(ivar)
           DO klev = 1,nlev
              IF (lev_out(klev) == 0) CYCLE
              ksec1(7) = 109
              ksec1(8) = klev
              ksec1(9) = 0
              CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   conc_out(1:np,klev,ivar),np,kbuffer,maxdim,kword,'C',kret)

              CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
              IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
              cnt_grb = cnt_grb + 1
           ENDDO                ! livelli
        else
           write(6,*)'non ho trovato la variabile  ',namevar(ivar)
        end if
     end do                       !specie 
  ENDDO                     ! scadenze

  ncstat=nf90_close(ncid)
  WRITE (*,'(a,i3,a,i6,a,i3,a,i3,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
          cnt_grb," grib, Trovate ",ntrov, " N variabili  ,Scritte ",nscri ," N variabili"

!--------------------------------------------------------------------------
! 2.5) Formato Boundary Conditions

ELSE IF (inp_fmt == 5) THEN
  DO kscad = 1, times1
    ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
    idata=mm5date2numeric(datebuf)
    write(6,*)trim(datebuf),idata

    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    ncstat=nf90_get_var( ncid,latspe_varid, conc, &
      (/     1,        1,  1, kscad/),  & ! start vector
      (/species,nhori,nlev,    1/))    ! count vector
    write(6,*)'qu3',nyi,nxi

    write(6,*)nhori,nlev,species
    do k=1,nhori
    do nl=1,nlev
    do ivar=1,species
      conc_out(k,nl,ivar)=conc(ivar,k,nl)
    end do
    end do
    end do
                         
!   Inserisco valori mancanti negli angoli della cornice delle BC: nxi,nyi
    conc_miss(1:nyi,:,:) = conc_out(1:nyi,:,:)                           ! W
    conc_miss(nyi+1,:,:) = rmis
    conc_miss(nyi+2:2*nyi+1,:,:) = conc_out(nyi+1:2*nyi,:,:)             ! E
    conc_miss(2*nyi+2,:,:) = rmis
    conc_miss(2*nyi+3:2*nyi+nxi+2,:,:) = conc_out(2*nyi+1:2*nyi+nxi,:,:) ! S
    conc_miss(2*nyi+nxi+3,:,:) = rmis
    conc_miss(2*nyi+nxi+4:2*nyi+2*nxi+3,:,:) = &
    conc_out(2*nyi+nxi+1:2*nyi+2*nxi,:,:)                                ! N
    conc_miss(2*nyi+2*nxi+4,:,:) = rmis

    DO ivar=1,species
      IF (code_var(ivar) <= 0) CYCLE
      write(6,*)'scrivo  la variabile  ',namevar(ivar),ivar
      nscri=nscri+1
      ksec1(1) = tab_var(ivar)
      ksec1(6) = code_var(ivar)

      DO klev = 1,nlev
        IF (lev_out(klev) == 0) CYCLE
        ksec1(7) = 109
        ksec1(8) = klev
        ksec1(9) = 0
        CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             conc_miss(1:np,klev,ivar),np,kbuffer,maxdim,kword,'C',kret)
     
        CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
        IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
        cnt_grb = cnt_grb + 1
    
      ENDDO                ! livelli
      
    ENDDO                  ! specie
  ENDDO                    ! scadenze
  ncstat=nf90_close(ncid)

  WRITE (*,'(a,i3,a,i6,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
                 cnt_grb," grib"

!--------------------------------------------------------------------------
! 2.6) Formato BEMISSION 

ELSE IF (inp_fmt == 6) THEN

  cnt_grb = 0
  DO kscad = 1, times1
    ntrov=0
    nscri=0
    ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
    idata=mm5date2numeric(datebuf)

    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    ncstat=nf90_get_var( ncid,biospe_varid, emisb, &
      (/     1,        1,  1, kscad/),  & ! start vector
      (/biospecies,nzonal, nmerid,    1/))    ! count vector

    emisb1=reshape(emisb,(/biospecies,nzonal*nmerid/))
    do k=1,np
    do ivar=1,biospecies
      conc_out(k,1,ivar)=emisb1(ivar,k)
    end do
    end do
    do ivar=1,biospecies
      IF (code_var(ivar) <= 0) CYCLE
      write(6,*)'scrivo  la variabile  ',namevar(ivar),ivar
      nscri=nscri+1
      ksec1(1) = tab_var(ivar)
      ksec1(6) = code_var(ivar)
      DO klev = 1,nlev
        IF (lev_out(klev) == 0) CYCLE
        ksec1(7) = 109
        ksec1(8) = klev
        ksec1(9) = 0
        CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             conc_out(1:np,klev,ivar),np,kbuffer,maxdim,kword,'C',kret)

        CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
        IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
        cnt_grb = cnt_grb + 1
      ENDDO                ! livelli
    ENDDO                  ! specie 
  ENDDO                    ! scadenze

  ncstat=nf90_close(ncid)
  WRITE (*,'(a,i3,a,i6,a,i3,a,i3,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
    cnt_grb," grib, Trovate ",ntrov, " N variabili  ,Scritte ",nscri ," N variabili"

!--------------------------------------------------------------------------
! 2.7) Formato Emission Inventory

ELSE IF (inp_fmt == 7) THEN

  cnt_grb=0
  slen=LEN(TRIM(filein))
  READ(filein(slen-1:slen),'(i2)',iostat=ios)mm
  IF (ios/=0 .or. mm < 1 .or.mm>12) mm=1

  DO kscad = 1, times1
    ntrov=0
    nscri=0
    ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
    idata=mm5date2numeric(datebuf)
    write(6,*)trim(datebuf),idata
    DO ivar=1,nvarout
      ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
      is3d1=(ncstat==NF90_NOERR)

      IF (is3d1) THEN
        WRITE (6,*) 'ho trovato la variabile  ', &
          namevar(ivar),ivar,ivarid,code_var(ivar)
        ncstat=nf90_inquire_variable &
          (ncid,ivarid,varids1(ivarid)%varname,vartype1,varids1(ivarid)%ndims)
        ntrov=ntrov+1 
        write(6,*)ivarid,varids1(ivarid)%varname,namevar(ivar)
        ncstat=nf90_get_var( ncid,ivarid, buf4d1, &
             (/  1,   1,      1,       1, kscad/),  & ! start vector
             (/ nzonal, nmerid, nlev,typeday,1/)   & ! count vector
             )
        conc3=reshape(buf4d1,(/nzonal*nmerid,nlev,typeday/))
        conc4(:,:,:,ivar)=conc3(:,:,:)     
        IF (code_var(ivar)<=0) CYCLE
        WRITE (6,*)'scrivi la variabile ',namevar(ivar),ivar
        nscri=nscri+1 
        ksec1(1) = tab_var(ivar)
        ksec1(6) = code_var(ivar)
!       Elaboro i grib relativi a ciascuna specie / tipo di giorno 
!       (ciclo interno su ore e livelli)

        DO kday = 1,typeday
          conc_out(:,:,:)=conc4(:,:,kday,:)     
          ksec1(10) = 100
          ksec1(11) = mm
          ksec1(12) = kday
          ksec1(21) = 20
          ksec1(16) = 0
          DO klev = 1,nlev
            IF (lev_out(klev) == 0) CYCLE
            DO kh = 1,times1
              ksec1(7) = 109
              ksec1(8) = klev
              ksec1(9) = 0
              ksec1(13) = kscad
              CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   conc_out(1:np,klev,ivar),np,kbuffer,maxdim,kword,'C',kret)
              CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
              IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
              cnt_grb = cnt_grb + 1
            ENDDO                ! ore
          ENDDO                  ! livelli
        ENDDO                    ! tipi di giorno

      ELSE          
        WRITE(6,*)'non ho trovato la variabile  ',namevar(ivar)

      ENDIF
    ENDDO                        ! specie 
  ENDDO                          ! scadenze

  ncstat=nf90_close(ncid)
  WRITE (*,'(a,i3,a,i10,a,i3,a,i3,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
          cnt_grb," grib, Trovate ",ntrov, " N variabili  ,Scritte ",nscri ," N variabili"

!--------------------------------------------------------------------------
! 2.8) Formato AODEM

ELSE IF (inp_fmt == 8) THEN

  cnt_grb = 0
  ntrov=0
  DO kscad = 1, times1
     ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
     idata=mm5date2numeric(datebuf)

     IF (kscad == 1) idata_ini = idata
     CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
          ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

     DO ivar=1,nvarout
        ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
        is3d1=(ncstat==NF90_NOERR)

        IF (is3d1) THEN
           IF (kscad ==1) THEN
             WRITE(*,'(2a,2(1x,i3))') "Trovata la variabile: ", &
               namevar(ivar),ivar,ivarid
             ntrov=ntrov+1
           ENDIF

           ncstat=nf90_inquire_variable &
             (ncid,ivarid,varids1(ivarid)%varname,vartype1,varids1(ivarid)%ndims)

           ncstat=nf90_get_var( ncid,ivarid, buf2d1, &
                (/     1,      1, kscad/),  & ! start vector
                (/nzonal, nmerid,     1/))    ! count vector

           conc1 = RESHAPE(buf2d1,(/nzonal*nmerid/))
           conc_out(:,1,ivar) = conc1(:)

           IF (code_var(ivar) <= 0) CYCLE
           ksec1(1) = tab_var(ivar)
           ksec1(6) = code_var(ivar)

           ksec1(7) = 1
           ksec1(8) = 0
           ksec1(9) = 0
           CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                conc_out(1:np,1,ivar),np,kbuffer,maxdim,kword,'C',kret)

           CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
           IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
           cnt_grb = cnt_grb + 1

        ELSE
           IF (kscad ==1) WRITE(*,'(2a,2(1x,i3))') &
             "Variabile non trovata: ",namevar(ivar),ivar

        ENDIF
     ENDDO                       ! specie 
  ENDDO                          ! istanti

  ncstat=nf90_close(ncid)
  WRITE (*,'(3(a,i6))') "Scadenze elaborate ",kscad-1, &
    ", variabili scritte ",ntrov,", grib scritti ",cnt_grb

ENDIF

CALL PBCLOSE (iu,kret)

STOP 0

!==========================================================================
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileinfo)
STOP 1

9998 CONTINUE
WRITE (*,*) "Record illegale o mal posizionato in ",TRIM(fileinfo)
WRITE (*,*) "Numero record (esclusi commenti) ",k
WRITE (*,*) TRIM(chrec)
STOP 2

9997 CONTINUE
WRITE (*,*) "Parametri illegali leggendo ",TRIM(fileinfo)
STOP 2

9995 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP 2

9994 CONTINUE
WRITE (*,*) "Errore numero specie ",TRIM(filein)
STOP 3

9993 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(tab_file(kpt))
STOP 1

9992 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(tab_file(kpt))
STOP 2

9991 CONTINUE
WRITE (*,*) "Dati inconsistenti in ",TRIM(tab_file(kpt))," e ",TRIM(filein)
IF (nx /= nzonal) WRITE (*,*) "Nx: ",nx,nzonal
IF (ny /= nmerid) WRITE (*,*) "Nx: ",ny,nmerid
STOP 4

9990 CONTINUE
WRITE (*,*) "Area ",TRIM(domain)," non trovata in ",TRIM(tab_file(kpt))
STOP 5

9989 CONTINUE
WRITE (*,*) "Riferimenti temporali non trovati in ",TRIM(filein)," usare parametro -vt"
STOP 6

9988 CONTINUE
WRITE (*,*) "Area ",TRIM(domain)," non trovata in ",TRIM(tab_file(1)), &
   " ",TRIM(tab_file(2))
STOP 7

9987 CONTINUE
WRITE (*,*) "Errore, proiezione non gestita ",proj
STOP 8

9986 CONTINUE
WRITE (*,*) "Numero bin aerosol non trovato nel file NetCDF"
STOP 9

END PROGRAM chimerencdf2grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE chdata2ksec(idata,idata_ini,scad_ini,verbose, &
  ksec10,ksec11,ksec12,ksec13,ksec16,ksec21)
!--------------------------------------------------------------------------
! Data una data nel formato intero di chimere (YYYYMMDDHH), ritorna gli 
! elementi relativi a data e scadenza nella sezione 1 del grib.
!--------------------------------------------------------------------------
USE date_handler

IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: idata       ! data/ora letta dal record corrente
INTEGER, INTENT(IN) :: idata_ini   ! data/ora del 1o record del file
INTEGER, INTENT(IN) :: scad_ini
LOGICAL, INTENT(IN) :: verbose
INTEGER, INTENT(OUT) :: ksec10,ksec11,ksec12,ksec13,ksec16,ksec21

! Variabli locali
TYPE(date) :: datac,data_ini,data_emi
INTEGER :: hhc,hh_ini,hh_emi,scadc,bck_day
CHARACTER (LEN=10) :: chdata

!--------------------------------------------------------------------------

! Leggo data/ora corrente
WRITE (chdata,'(i10.10)') idata
READ (chdata,'(i4,3i2)',ERR=9999) datac%yy,datac%mm,datac%dd,hhc

IF (verbose) WRITE (*,'(a,i5,3i3)') "Elaboro istante: ", &
  datac%yy,datac%mm,datac%dd,hhc

! Richiedo tutte analisi
IF (scad_ini < 0) THEN

  ksec10 = 1 + MOD(datac%yy-1,100)
  ksec21 = 1 + (datac%yy-1)/100
  ksec11 = datac%mm
  ksec12 = datac%dd
  ksec13 = hhc
  ksec16 = 0

! Richiedo previsioni
ELSE

! Calcolo la data di emissione
  WRITE (chdata,'(i10.10)') idata_ini
  READ (chdata,'(i4,3i2)',ERR=9999) &
    data_ini%yy,data_ini%mm,data_ini%dd,hh_ini
  hh_emi = hh_ini - scad_ini
  IF (hh_emi >= 0) THEN
    data_emi = data_ini
  ELSE
    bck_day = (-1-hh_emi)/24 + 1
    data_emi = data_ini - bck_day
    hh_emi = hh_emi + bck_day * 24
  ENDIF

! Calcolo la scadenza di previsione
  scadc = hhc - hh_emi + 24 * (datac - data_emi)
  IF (verbose) WRITE (*,'(a,i5,3i3,a,i3)') "Codifico scadenza: ", &
    data_emi%yy,data_emi%mm,data_emi%dd,hh_emi," +",scadc

! Definisco la sezione1 GRIB
  ksec10 = 1 + MOD(data_emi%yy-1,100)
  ksec21 = 1 + (data_emi%yy-1)/100
  ksec11 = data_emi%mm
  ksec12 = data_emi%dd
  ksec13 = hh_emi
  ksec16 = scadc

ENDIF

RETURN

9999 CONTINUE
WRITE (*,*) "Data illegale in file input: ",idata
STOP

END SUBROUTINE chdata2ksec

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "Uso: chimere2grib.exe filein fileout fileinfo igen "
WRITE (*,*) "     [-out/-met/-bio/-ini/-bc/-emibio/-eminv/-aodem]"
WRITE (*,*) "     [-grd PROJ DOM] [-vt YYYYMMDDHH] [-rmis VAL]"
WRITE (*,*)
WRITE (*,*) "Legge un file di netcdf di Chimere e lo scrive in formato GRIB"
WRITE (*,*) "filein:   file di input (NetCDF)"
WRITE (*,*) "fileout:  file di output (GRIB)"
WRITE (*,*) "fileinfo: namelsit (formato CHIMERE_INFO.DAT o CHIMERE_NCINFO.DAT)"
WRITE (*,*) "igen:     processo generatore dei grib in output"
WRITE (*,*)
WRITE (*,*) "  -out:   analizza l'output di Chimere (concentrazioni) [DEFAULT]"
WRITE (*,*) "  -met:   analizza un file METEO o exdom"
WRITE (*,*) "  -bio:   analizza un file BIOFACS (??? calcola potenziale totale)"
WRITE (*,*) "  -ini:   analizza un file ini.sim o end.sim"
WRITE (*,*) "  -bc:    analizza un file LAT_CONCS o TOP_CONCS"
WRITE (*,*) "  -emibio:analizza un file BEMISSIONS ???" 
WRITE (*,*) "  -eminv: analizza un file EMISSIONS-area.mm (profili giorn. di emiss.)"
WRITE (*,*) "  -aodem: analizza l'output di AODEM (solo AOD, 2-dim)"
WRITE (*,*) ""
WRITE (*,*) "Opzioni per convertire i dati MACC:"
WRITE (*,*) "-grd DOM: usa i parametri griglia relativi all'aera DOM (default: legge DOM da"
WRITE (*,*) "          filein). DOM deve essere incluso in aree_geo.dat o aree_utm.dat"
WRITE (*,*) "          [di defualt legge i files in /usr/share/ma_utils; per usare un file "
WRITE (*,*) "           locale, assegnare la variabile $MA_UTILS_DAT]"
WRITE (*,*) "-vt YYYYMMDDHH: specifica il verification time del primo istante in output;"
WRITE (*,*) "          la scadenza viene letta da fileinfo. Se si sta elaborando l'output"
WRITE (*,*) "          standard di Chimere (default) queste informazioni sono lette da filein"
WRITE (*,*) "-rmis VAL: considera mancanti i valori pari a VAL (in MACC -999.)"
!            12345678901234567890123456789012345678901234567890123456789012345678901234567890

RETURN

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE attr_err(file,attr)
CHARACTER (LEN=*), INTENT(IN)  :: file,attr

WRITE (*,*) "Attributo non trovato in ",TRIM(file),": ",TRIM(attr)
STOP
END SUBROUTINE attr_err

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_eof_eor(eof, eor)
!-------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!-------------------------------------------------------------------------
IMPLICIT NONE

INTEGER, INTENT(OUT) :: eof,eor

INTEGER :: k, ios, idummy=0, iun=0
LOGICAL :: l1 = .TRUE.


! Cerco un'unita' libera per aprire il file di prova
DO k = 10,99
  INQUIRE (UNIT=k, OPENED=l1, IOSTAT=ios)
  IF (.NOT. l1 .AND. ios==0) THEN
    iun = k
    EXIT
  ENDIF
ENDDO
IF (iun == 0) GOTO 9999   ! non ho torvato nessuna unita' libera
!WRITE (*,*) "uso unita ",iun

! Cerco codice di errore per EOF
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
ENDFILE (k)
REWIND (k)
READ (k,*,IOSTAT=eof)
CLOSE(k)

! Cerco codice di errore per EOR
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
WRITE (k,'(a1)') "1" 
WRITE (k,'(a1)') "2"
REWIND (k)
READ (k,'(i1)',ADVANCE="NO",ERR=9999) idummy
READ (k,'(i1)',ADVANCE="NO",IOSTAT=eor) idummy
CLOSE(k)

!write (*,*) "eof,eor ",eof,eor
RETURN

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine get_eof_eor, usero' valori di default"
eof = -1
eor = -2
RETURN

END SUBROUTINE get_eof_eor

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION lowercase(chin) RESULT (chout)
!
! Replace uppercase letters with lowercase and takes off trailing blanks
! Non-literal characters are left unchanged.
!
IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN) :: chin
CHARACTER (LEN=LEN_TRIM(chin)) :: chout
!
INTEGER :: i,l
CHARACTER (LEN=26), PARAMETER :: &
upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
lower='abcdefghijklmnopqrstuvwxyz'

!--------------------------------------------------------------------------

chout=TRIM(chin)
DO i=1,LEN(chout)
  l=INDEX(upper,chin(i:i))
  IF (l == 0) THEN
    chout(i:i) = chin(i:i)
  ELSE
    chout(i:i) = lower(l:l)
  ENDIF
ENDDO

END FUNCTION lowercase

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION uppercase(chin) RESULT (chout)
!
! Replace lowercase letters with uppercase and takes off trailing blanks
! Non-literal characters are left unchanged.
!
IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN) :: chin
CHARACTER (LEN=LEN_TRIM(chin)) :: chout
!
INTEGER :: i,l
CHARACTER (LEN=26), PARAMETER :: &
upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
lower='abcdefghijklmnopqrstuvwxyz'

!--------------------------------------------------------------------------

chout=TRIM(chin)
DO i=1,LEN(chout)
  l=INDEX(lower,chin(i:i))
  IF (l == 0) THEN
    chout(i:i) = chin(i:i)
  ELSE
    chout(i:i) = upper(l:l)
  ENDIF
ENDDO

END FUNCTION uppercase

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

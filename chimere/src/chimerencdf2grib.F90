PROGRAM chimerencdf2grib
!--------------------------------------------------------------------------
! Legge un file di netcdf di Chimere e lo scrive in formato GRIB
! Uso: chimerencdf22grib.e filein fileout fileinfo igen
!                       [-out/-met/-bio/-ini/-bc/-emibio/-eminv/-aodem]
!
! NOTE:
! Stato delle opzioni: (label crev)
! - funzionanti: out
! - da verificare: met, bio, emibio, ini, eminv
! - da implementare: bc
! I grib in quota sono codificati come livelli ibridi, ma senza includere i
!   vertical coordinate parameters. 
!
!                               Versione 3.0.0, Michele & Enrico 22/11/2013
!--------------------------------------------------------------------------
use calendar 
use netcdf
IMPLICIT NONE
#define NCERR(lnum) if(ncstat/=NF90_NOERR) call nc_err(ncstat,lnum,'chimerencdf2grib.F90')

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
INTEGER, PARAMETER :: maxdim = 100000  ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxvar = 200     ! n.ro max var. in output Chimere
INTEGER, PARAMETER :: maxlev = 16      ! n.ro max livelli in output Chimere
REAL, PARAMETER :: rmis = -1.e9        ! codifica valore mancante
REAL, PARAMETER :: eps = 0.0001        ! tolleranza per uguaglianza estemi griglia

CHARACTER (LEN=120) :: tab_path_def = PKGDATAROOTDIR
CHARACTER (LEN=120) :: tab_env = "MA_UTILS_DAT"

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),kword,kret,nbit
REAL    :: psec2(512),psec3(2)

! Altre variabili del programma
REAL, ALLOCATABLE :: conc_out(:,:,:),tot(:),conc_miss(:,:,:),conc_out2d(:,:)
REAL :: x1,y1,x2,y2,dx,dy,xrot,yrot,x2r,y2r
INTEGER :: version
INTEGER :: nvarout,nx,ny,np,nvar_fix,nl,slen,mm,nxi,nyi,ntrov,nscri
INTEGER :: code_var(maxvar),tab_var(maxvar),lev_out(maxlev)
INTEGER :: cem,igen,idata,idata_ini,scad_ini
INTEGER :: iu,k,kp,kvar,kscad,klev,kday,kh,ios,eof,eor,cnt_grb,idp
INTEGER :: inp_fmt,info_fmt,p1,p2,nhead,iproj
CHARACTER (LEN=120) :: filein,fileout,fileinfo,chrec,chdum,arg(4),tab_file,tab_path
CHARACTER (LEN=3) :: proj
LOGICAL :: verbose

CHARACTER(LEN=10):: namevar(maxvar)
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
real,allocatable::conc1(:),conc2(:,:),conc3(:,:,:),conc4(:,:,:,:)
real,allocatable::conc(:,:,:)


! return status of all netCDF functions calls
integer :: ncstat
! file identifier
integer :: ncid
! dimension identifiers
integer :: times1_dimid,times2_dimid
integer :: dstrdimid
integer :: we1_dimid
integer :: sn1_dimid
integer :: bt1_dimid,h_dimid
integer :: biospe_dimid
integer :: latspe_dimid


! dimension lengths
integer :: times1
integer :: dstrlen
integer :: nzonal
integer :: nmerid
integer :: nlev
integer :: nhori
! variable identifiers
integer :: nvarin
integer :: ivar,ivarid
integer:: vartype1,times1_varid,typeday,biospecies,biospe_varid,latspe_varid,species

character(len=dlen)   :: datebuf
character(len=40)::buffatt1


logical::is3d1


integer::mm5date2numeric
character(len=6)::domain,rdomain

!==========================================================================
! 1) Elaborazioni preliminari

! nvar_fix: varibaili non chimiche presenti nei files NetCDF
! out_fmt=1: lon,lat,a_vcoord,b_vcoord,cut_off_diameters,Times,hlay,airm,relh,temp (10)
! 
!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

idp = 0
inp_fmt = 1
version=2007

DO kp = 1,HUGE(kp)
   CALL getarg(kp,chdum)
   IF (TRIM(chdum) == "-h") THEN
      CALL write_help
      STOP
   ELSE IF (TRIM(chdum) == "") THEN  
      EXIT
   ELSE IF (TRIM(chdum) == "-out") THEN
      inp_fmt = 1  
      nvar_fix = 10
   ELSE IF (TRIM(chdum) == "-met") THEN
      inp_fmt = 2  
      nvar_fix = 0
   ELSE IF (TRIM(chdum) == "-bio") THEN
      inp_fmt = 3  
      nvar_fix = 0
   ELSE IF (TRIM(chdum) == "-ini") THEN
      inp_fmt = 4  
      nvar_fix = 0
   ELSE IF (TRIM(chdum) == "-bc") THEN
      inp_fmt = 5  
      nvar_fix = 0
   ELSE IF (TRIM(chdum) == "-emibio") THEN
      inp_fmt = 6  
      nvar_fix = 0
   ELSE IF (TRIM(chdum) == "-eminv") THEN
      inp_fmt = 7  
      nvar_fix = 0
   ELSE IF (TRIM(chdum) == "-aodem") THEN
      inp_fmt = 8  
      nvar_fix = 0
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
   STOP
ENDIF

!--------------------------------------------------------------------------
! 1.2 Leggo dal file NetCDF le dimensioni della matrice dei dati (x,y,z,t,var)

! Apro il file
ncstat=nf90_open(filein,NF90_NOWRITE,ncid)
NCERR(__LINE__)

! Titolo
ncstat=nf90_get_att(ncid,NF90_GLOBAL,'Title',buffatt1)
NCERR(__LINE__)
ncstat=nf90_get_att(ncid,NF90_GLOBAL,'Domain',domain)
NCERR(__LINE__)

! Tempo
ncstat=nf90_inq_varid(ncid,'Times',times1_varid)
NCERR(__LINE__)

ncstat=nf90_inq_dimid(ncid,'DateStrLen',dstrdimid)
NCERR(__LINE__)
ncstat=nf90_inquire_dimension(ncid,dstrdimid,len=dstrlen)

ncstat=nf90_inq_dimid(ncid,'Time',times1_dimid)
NCERR(__LINE__)
ncstat=nf90_inquire_dimension(ncid,times1_dimid,len=times1)
NCERR(__LINE__)
WRITE (*,*)'N step temporali: ',times1

ncstat=nf90_inq_dimid(ncid,'type_day',times2_dimid)
is3d1=(ncstat==NF90_NOERR)
IF (is3d1) THEN
  NCERR(__LINE__)
  ncstat=nf90_inquire_dimension(ncid,times2_dimid,len=typeday)
  NCERR(__LINE__)
ELSE
  typeday=1
ENDIF

! Nx
ncstat=nf90_inq_dimid(ncid,'west_east',we1_dimid)
is3d1=(ncstat==NF90_NOERR)
IF (is3d1) THEN
  NCERR(__LINE__)                                  
  ncstat=nf90_inquire_dimension(ncid,we1_dimid,len=nzonal)
  NCERR(__LINE__)
ELSE
  WRITE (*,*) "N.ro celle in direzione X (west_est) non presente nel file NetCDF"
ENDIF

! Ny
ncstat=nf90_inq_dimid(ncid,'south_north',sn1_dimid)
is3d1=(ncstat==NF90_NOERR)
IF (is3d1) THEN
  NCERR(__LINE__)
  ncstat=nf90_inquire_dimension(ncid,sn1_dimid,len=nmerid)
  NCERR(__LINE__)
ELSE 
  WRITE (*,*) "N.ro celle in direzione Y (south-norht) non presente nel file NetCDF"
ENDIF

! Nz
IF (inp_fmt == 8) THEN
  nlev = 1
ELSE
  ncstat=nf90_inq_dimid(ncid,'bottom_top',bt1_dimid) 
  is3d1=(ncstat==NF90_NOERR)
  IF (is3d1) THEN
    NCERR(__LINE__)                                  
    ncstat=nf90_inquire_dimension(ncid,bt1_dimid,len=nlev)
    NCERR(__LINE__)
  ELSE
    WRITE (*,*) "N.ro livelli (bottom_top) non presente nel file NetCDF"
    nlev=1
  ENDIF
ENDIF

! Nhori (Solo per BC; numero celle di bordo? crev)
IF (inp_fmt == 5) THEN
  ncstat=nf90_inq_dimid(ncid,'h_boundary',h_dimid) 
  is3d1=(ncstat==NF90_NOERR)
  IF (is3d1) THEN
    NCERR(__LINE__)                                  
    ncstat=nf90_inquire_dimension(ncid,h_dimid,len=nhori)
    NCERR(__LINE__)
  ELSE
    WRITE (*,*) "Quota livelli (h_boundary) non presente nel file NetCDF"
  ENDIF
ENDIF

! Nvarin (numero di variabili nel file NetCDF)
IF (inp_fmt==5) THEN !crev
  ncstat=nf90_inq_dimid(ncid,'Species',latspe_dimid)
  is3d1=(ncstat==NF90_NOERR)
  IF (is3d1) THEN
    NCERR(__LINE__)
    ncstat=nf90_inquire_dimension(ncid,latspe_dimid,len=species)
    NCERR(__LINE__)
    ncstat=nf90_inq_varid(ncid,'conc',latspe_varid)
    NCERR(__LINE__)
    WRITE (*,*) 'latspe_varid',latspe_varid
    nvarin = species
  ELSE
    GOTO 9994
  ENDIF

ELSE IF (inp_fmt==6) THEN !crev
  ncstat=nf90_inq_dimid(ncid,'biospecies',biospe_dimid) !crev
  is3d1=(ncstat==NF90_NOERR)
  IF (is3d1) THEN
    NCERR(__LINE__)
    ncstat=nf90_inquire_dimension(ncid,biospe_dimid,len=biospecies)
    NCERR(__LINE__)
    ncstat=nf90_inq_varid(ncid,'emisb',biospe_varid)
    NCERR(__LINE__)
    WRITE (*,*)'biospoe_varid',biospe_varid
    nvarin = biospecies
  ELSE
    GOTO 9994
  ENDIF

ELSE
  ncstat=nf90_inquire(ncid, nVariables=nvarin)
  NCERR(__LINE__)

ENDIF

!--------------------------------------------------------------------------
! 1.3 Leggo dal file domainlist.nml i parametri della griglia

tab_path = ""
CALL GETENV(tab_env,tab_path)
IF (TRIM(tab_path) == "") tab_path = tab_path_def
WRITE (tab_file,'(2a)') TRIM(tab_path),"/domainlist.nml"

OPEN (UNIT=31, FILE=tab_file, STATUS="OLD", ERR= 9993)
READ (31,*)
DO
  READ (31,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9990
  rdomain = chrec(1:6)
  IF (ADJUSTL(rdomain) == ADJUSTL(domain)) THEN
    READ (chrec(7:),*,IOSTAT=ios) nx,ny,dx,dy,x1,y1,x2r,y2r,iproj
    IF (ios /= 0) GOTO 9992
    EXIT
  ENDIF
ENDDO

x2 = x1+(nx-1)*dx
y2 = y1+(ny-1)*dy
dx = (x2-x1)/REAL(nx-1)
dy = (y2-y1)/REAL(ny-1)
np = nx*ny

IF (nx /= nzonal .OR. ny /= nmerid .OR. ABS(x2r-x2) > eps .OR. ABS(y2r-y2) > eps) &
  GOTO 9991

! Se formato BC, stendo i 4 bordi su un vettore 1D ...
IF (inp_fmt == 5) THEN
   nx = (nmerid+nzonal)*2 + 4
   ny = 1
   x2 = x1 + (nx-1)*dx
   y2 = y1
ENDIF

! Proiezione geografica
IF (iproj == 1) THEN
  proj = "UTM"
ELSE IF (iproj == 2) THEN
  IF (y1 > 0.) THEN
    proj = "GEO"
  ELSE
    proj = "ROT"
    xrot = 10.
    yrot = 57.5
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 1.4 leggo da fileinfo livelli e variabili richiesti in output

OPEN (UNIT=30, FILE=fileinfo, STATUS="OLD", ERR=9999)

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
    IF (TRIM(chrec) == "V2013") THEN
      info_fmt = 2
      nhead = 5
    ELSE
      info_fmt = 1
      nbit = 24
      nhead = 5
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
           write(6,*)code_var(k-nhead),namevar(k-nhead),k-nhead,k
    ELSE
       WRITE (*,*) "Troppe specie, elaboro le prime ",maxvar,&
            " (aumentare param. maxvar)"
       k = k-1
       EXIT

    ENDIF
    IF (ios /= 0) GOTO 9998

! Interpreto la riga letta: nuovo formato (CHIMERE_NCINFO.DAT)
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

WRITE (*,*) "Parametri griglia: (",TRIM(domain),")"
WRITE (*,*) "proj,nx,ny,nz: ",proj,nx,ny,nlev
WRITE (*,*) "x1,y1,x2,y2: ",x1,y1,x2,y2
WRITE (*,*) "dx,dy: ",dx,dy
WRITE (*,*) ""
WRITE (*,*) "Numero di parametri:"
WRITE (*,*) "variabili totali in input:  ",nvarin
WRITE (*,*) "   di cui specie chimiche:  ",nvarin - nvar_fix
WRITE (*,*) "variabili richieste in out: ",nvarout
WRITE (*,*) "livelli in outuput:         ",COUNT(lev_out(1:nlev) /= 0)
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
ELSE
   ksec1(5) = 128
ENDIF

ksec1(15) = 1
ksec1(17) = 0
ksec1(18) = 0
ksec1(19) = 0
ksec1(20) = 0
ksec1(22:) = 0

! Sezione 2
ksec2(2) = nx
ksec2(3) = ny
ksec2(4) = NINT(y1 * 1000.)
ksec2(5) = NINT(x1 * 1000.)
ksec2(7) = NINT(y2 * 1000.)
ksec2(8) = NINT(x2 * 1000.)
ksec2(11)=64
ksec2(12)=0

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
   ksec2(13) = NINT((yrot-90.) * 1000.)
   ksec2(14) = NINT(xrot * 1000.)

ELSE
   WRITE (*,*) "Errore, proiezione non gestita ",proj
   STOP

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
ALLOCATE (buf4d1(nzonal,nmerid,nlev,3))
ALLOCATE (conc1(nzonal*nmerid))
ALLOCATE (conc2(nzonal*nmerid,nlev))
ALLOCATE (conc3(nzonal*nmerid,nlev,3))

SELECT CASE (inp_fmt)
CASE (1,2)
  ALLOCATE(conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE(varids1(nvarin))
CASE (3)
  ALLOCATE(varids1(nvarin))
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE (tot(nzonal*nmerid))
CASE (4) 
  ALLOCATE(varids1(nvarin))
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
CASE (5)
  ALLOCATE(conc(species,nhori,nlev))
  ALLOCATE (conc_out(nhori,nlev,nvarout))
  ALLOCATE (conc_miss(nhori+4,nlev,nvarout))
CASE (6)
  ALLOCATE(conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE(emisb(biospecies,nzonal,nmerid))
  ALLOCATE(emisb1(biospecies,nzonal*nmerid))
CASE (7) 
  ALLOCATE(varids1(nvarin))
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE (conc4(nzonal*nmerid,nlev,typeday,nvarout))
CASE (8)
  ALLOCATE (conc_out(nzonal*nmerid,nlev,nvarout))
  ALLOCATE(varids1(nvarin))
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
  ntrov=0
  DO kscad = 1, times1
     ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
     NCERR(__LINE__)
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
           NCERR(__LINE__)

           ncstat=nf90_get_var( ncid,ivarid, buf3d1, &
                (/     1,      1,       1, kscad/),  & ! start vector
                (/nzonal, nmerid, nlev,     1/))    ! count vector
           NCERR(__LINE__)

           conc2 = RESHAPE(buf3d1,(/nzonal*nmerid,nlev/))
           conc_out(:,:,ivar) = conc2(:,:)

           IF (code_var(ivar) <= 0) CYCLE
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
           ENDDO                 ! livelli

        ELSE
           IF (kscad ==1) WRITE(*,'(2a,2(1x,i3))') &
             "Variabile non trovata: ",namevar(ivar),ivar,ivarid

        ENDIF
     ENDDO                       ! specie 
  ENDDO                          ! istanti

  ncstat=nf90_close(ncid)
  NCERR(__LINE__)
  WRITE (*,'(3(a,i6))') "Scadenze elaborate ",kscad-1, &
    ", variabili scritte ",ntrov,", grib scritti ",cnt_grb

!--------------------------------------------------------------------------
! 2.2) Formato METEO

ELSE IF (inp_fmt == 2) THEN

  cnt_grb = 0
  ntrov=0
  nscri=0
  DO kscad = 1, times1
     ntrov=0
     nscri=0  
     ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
     NCERR(__LINE__)
     idata=mm5date2numeric(datebuf)
     write(6,*)trim(datebuf),idata

     IF (kscad == 1) idata_ini = idata
     CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
          ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))
     do ivar=1,nvarout
        ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
        is3d1=(ncstat==NF90_NOERR)
        if(is3d1) then
           write(6,*)'ho trovato la variabile  ',namevar(ivar),ivar,ivarid,code_var(ivar)
           ntrov=ntrov+1
           ncstat=nf90_inquire_variable(ncid,ivarid,varids1(ivarid)%varname,vartype1,varids1(ivarid)%ndims)
           NCERR(__LINE__)
           if(varids1(ivarid)%ndims==3)  then
              ! 2d fil

              ncstat=nf90_get_var( ncid,ivarid, buf2d1, &
                   (/     1,      1,        kscad/),  & ! start vector
                   (/nzonal, nmerid,    1/))    ! count vector
              NCERR(__LINE__)
              write(6,*)'2D ',varids1(ivarid)%varname
              nl=1
              conc1=reshape(buf2d1,(/nzonal*nmerid/))
              conc_out(:,1,ivar)=conc1(:)


           else if(varids1(ivarid)%ndims==4)  then

              ncstat=nf90_get_var( ncid,ivarid, buf3d1, &
                   (/     1,      1,       1, kscad/),  & ! start vector
                   (/nzonal, nmerid, nlev,     1/))    ! count vector
              NCERR(__LINE__)

              write(6,*)'3D ',varids1(ivarid)%varname
              conc2=reshape(buf3d1,(/nzonal*nmerid,nlev/))
              conc_out(:,:,ivar)=conc2(:,:)
              nl=nlev
           end if

           IF (code_var(ivar) <= 0) CYCLE
           write(6,*)'scrivo  la variabile  ',namevar(ivar),ivar
           nscri=nscri+1
           ksec1(1) = tab_var(ivar)
           ksec1(6) = code_var(ivar)
           DO klev = 1,nl
              IF (lev_out(klev) == 0) CYCLE
              IF (varids1(ivarid)%ndims==4) then
                 ksec1(7) = 109
                 ksec1(8) = klev
                 ksec1(9) = 0
              ELSE
                 ksec1(7) = 1
                 ksec1(8) = 0
                 ksec1(9) = 0
              ENDIF

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
  NCERR(__LINE__)
  WRITE (*,'(a,i3,a,i6,a,i3,a,i3,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
          cnt_grb," grib, Trovate ",ntrov, " N variabili  ,Scritte ",nscri ," N variabili"

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
  ksec1(14) = 0
  ksec1(21) = 20

  ksec1(16) = 0

  cnt_grb = 0
  DO kscad = 1, times1
     ntrov=0
     nscri=0
     ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
     NCERR(__LINE__)
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
           NCERR(__LINE__)


           ncstat=nf90_get_var( ncid,ivarid, buf3d1, &
                (/     1,      1,       1, kscad/),  & ! start vector
                (/nzonal, nmerid, nlev,     1/))    ! count vector
           NCERR(__LINE__)

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
  NCERR(__LINE__)
  WRITE (*,'(a,i3,a,i6,a,i3,a,i3,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
          cnt_grb," grib, Trovate ",ntrov, " N variabili  ,Scritte ",nscri ," N variabili"

!--------------------------------------------------------------------------
! 2.5) Formato Boundary Conditions

ELSE IF (inp_fmt == 5) THEN
  DO kscad = 1, times1
    ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
    NCERR(__LINE__)
    idata=mm5date2numeric(datebuf)
    write(6,*)trim(datebuf),idata

    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    ncstat=nf90_get_var( ncid,latspe_varid, conc, &
      (/     1,        1,  1, kscad/),  & ! start vector
      (/species,nhori,nlev,    1/))    ! count vector
    NCERR(__LINE__)
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
  NCERR(__LINE__)

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
    NCERR(__LINE__)
    idata=mm5date2numeric(datebuf)

    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    ncstat=nf90_get_var( ncid,biospe_varid, emisb, &
      (/     1,        1,  1, kscad/),  & ! start vector
      (/biospecies,nzonal, nmerid,    1/))    ! count vector
    NCERR(__LINE__)

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
  NCERR(__LINE__)
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
    NCERR(__LINE__)
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
        NCERR(__LINE__)
        ntrov=ntrov+1 
        write(6,*)ivarid,varids1(ivarid)%varname,namevar(ivar)
        ncstat=nf90_get_var( ncid,ivarid, buf4d1, &
             (/  1,   1,      1,       1, kscad/),  & ! start vector
             (/ nzonal, nmerid, nlev,typeday,1/)   & ! count vector
             )
        NCERR(__LINE__)
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
          ksec1(14) = 0
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
  NCERR(__LINE__)
  WRITE (*,'(a,i3,a,i10,a,i3,a,i3,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
          cnt_grb," grib, Trovate ",ntrov, " N variabili  ,Scritte ",nscri ," N variabili"

!--------------------------------------------------------------------------
! 2.8) Formato AODEM

ELSE IF (inp_fmt == 8) THEN

  cnt_grb = 0
  ntrov=0
  DO kscad = 1, times1
     ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
     NCERR(__LINE__)
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
           NCERR(__LINE__)

           ncstat=nf90_get_var( ncid,ivarid, buf2d1, &
                (/     1,      1, kscad/),  & ! start vector
                (/nzonal, nmerid,     1/))    ! count vector
           NCERR(__LINE__)

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
             "Variabile non trovata: ",namevar(ivar),ivar,ivarid

        ENDIF
     ENDDO                       ! specie 
  ENDDO                          ! istanti

  ncstat=nf90_close(ncid)
  NCERR(__LINE__)
  WRITE (*,'(3(a,i6))') "Scadenze elaborate ",kscad-1, &
    ", variabili scritte ",ntrov,", grib scritti ",cnt_grb

ENDIF

CALL PBCLOSE (iu,kret)

STOP

!==========================================================================
! 3) Gestione errori

9999       CONTINUE
           WRITE (*,*) "Errore aprendo ",TRIM(fileinfo)
           STOP

9998       CONTINUE
           WRITE (*,*) "Record illegale o mal posizionato in ",TRIM(fileinfo)
           WRITE (*,*) "Numero record (esclusi commenti) ",k
           WRITE (*,*) TRIM(chrec)
           STOP

9997       CONTINUE
           WRITE (*,*) "Parametri illegali leggendo ",TRIM(fileinfo)
           STOP

9995       CONTINUE
           WRITE (*,*) "Errore leggendo ",TRIM(filein)
           STOP

9994       CONTINUE
           WRITE (*,*) "Errore numero specie ",TRIM(filein)
           STOP

9993       CONTINUE
           WRITE (*,*) "Errore aprendo ",TRIM(tab_file)
           STOP

9992       CONTINUE
           WRITE (*,*) "Errore leggendo ",TRIM(tab_file)
           STOP

9991       CONTINUE
           WRITE (*,*) "Dati inconsistenti in ",TRIM(tab_file)," e ", &
             TRIM(filein)
           STOP

9990       CONTINUE
           WRITE (*,*) "Area ",TRIM(domain),"non trovata in ",TRIM(tab_file)

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
!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: chimere2grib.exe filein fileout fileinfo igen"
WRITE (*,*) "     [-out/-met/-bio/-ini/-bc/-emibio/-eminv/-aodem]"
WRITE (*,*)
WRITE (*,*) "Legge un file di netcdf di Chimere e lo scrive in formato GRIB"
WRITE (*,*) "filein:   file di input (NetCDF)"
WRITE (*,*) "fileout:  file di output (GRIB)"
WRITE (*,*) "fileinfo: namelsit (formato CHIMERE_INFO.DAT o CHIMERE_NCINFO.DAT)"
WRITE (*,*)
WRITE (*,*) "  -out: analizza l'output di Chimere (concentrazioni) [DEFAULT]"
WRITE (*,*) "  -met: analizza un file METEO"
WRITE (*,*) "  -bio: analizza un file BIOFACS (??? calcola potenziale totale)"
WRITE (*,*) "  -ini: analizza un file ini.sim o end.sim"
WRITE (*,*) "  -bc:  analizza un file LAT_CONCS o TOP_CONCS"
WRITE (*,*) "  -emibio: analizza un file BEMISSIONS ???" 
WRITE (*,*) "  -eminv:  analizza un file EMISSIONS-area.mm (profili giorn. di emiss.)"
WRITE (*,*) "  -aodem:  analizza l'output di AODEM (solo AOD, 2-dim)"
WRITE (*,*) ""
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN

END SUBROUTINE write_help

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

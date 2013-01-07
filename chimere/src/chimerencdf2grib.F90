PROGRAM chimerencdf2grib_completo
  !--------------------------------------------------------------------------
  ! Legge un file di output netcdf (oppure un file fisiografico) di Chimere e lo
  !   scrive in formato GRIB
  ! Uso: chimerencdf22grib.e filein fileout fileinfo igen
!!!!! 
  !                       [-lu/-bio/-meta/-ini/-eminv/-latbc/-topbc/-emibio]
  !
  ! NOTE:
  ! attualemente implemente le opzioni -lu  (legge file unformatted)
  ! attualemente implemente le opzioni -meta -ini -eminv  -emibio (file .nc)
  ! in sistemazione latbc (file .nc)
  !  
  !
  ! I grib in quota sono codificati come livelli ibridi, ma senza includere i
  !   vertical coordinate parameters. 
  !
  !                                        Versione 2.3.2, Michele 07/12/2012
  !--------------------------------------------------------------------------
  use calendar 
  use netcdf
  IMPLICIT NONE
#define NCERR(lnum) if(ncstat/=NF90_NOERR) call nc_err(ncstat,lnum,'chimerencdf2grib_completo.F90')

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
  INTEGER, PARAMETER :: nhead = 5        ! record d'intestazione in fileinfo
  INTEGER, PARAMETER :: nbit = 24        ! n.ro bit per codifica GRIB
  REAL, PARAMETER :: rmis = -1.e9        ! codifica valore mancante

  ! Dichiarazioni per GRIBEX.
  INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
  INTEGER :: kbuffer(maxdim),kword,kret
  REAL    :: psec2(512),psec3(2)

  ! Altre variabili del programma
!  DOUBLE PRECISION, ALLOCATABLE :: conc2p(:,:,:)
  REAL, ALLOCATABLE :: conc_out(:,:,:),tot(:),conc_miss(:,:,:)
  REAL :: x1,y1,x2,y2,dx,dy,xrot,yrot,rdum
  INTEGER :: version
  INTEGER :: nvar,nlev,nx,ny,np,nvar_out,nvar3d,nvar2d,nl,slen,mm,ii,nxi,nyi,ntrov,nscri
  INTEGER :: code_var(maxvar),tab_var(maxvar),lev_out(maxlev)
  INTEGER :: cem,igen,idata,idata_ini,scad_ini
  INTEGER :: iu,k,kp,kvar,kscad,klev,kday,kh,ios,eof,eor,cnt_grb,idp,inp_fmt,p1,p2
  CHARACTER (LEN=90) :: filein,fileout,fileinfo,chrec,chdum,arg(4)
  CHARACTER (LEN=3) :: proj
  LOGICAL :: verbose



  CHARACTER(LEN=10)::	namevar(maxvar)


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
  real,allocatable::conc2(:,:),conc3(:,:,:),conc4(:,:,:,:)
  real,allocatable::conc1(:)
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
  integer :: nlevels
  integer :: nhori
  ! variable identifiers
  integer :: nvarin
  integer :: ivar,ivarid
  integer::lon1_varid,lat1_varid,vartype1,times1_varid,typeday,biospecies,biospe_varid,latspe_varid,species

  character(len=dlen)   :: datebuf
  character(len=40)::buffatt1


  logical::is3d1


  integer::mm5date2numeric
  character(len=6)::domain

  !--------------------------------------------------------------------------
  ! 1) Elaborazioni preliminari

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
        !  ELSE IF (TRIM(chdum) == "-v") THEN  
        !    verbose = .TRUE.
        !  ELSE IF (TRIM(chdum) == "-V2003") THEN
        !    version=2003
        !  ELSE IF (TRIM(chdum) == "-V2004") THEN
        !version=2004
     ELSE IF (TRIM(chdum) == "-lu") THEN
        inp_fmt = 2  
     ELSE IF (TRIM(chdum) == "-bio") THEN
        inp_fmt = 3  
     ELSE IF (TRIM(chdum) == "-meta") THEN
        inp_fmt = 4  
     ELSE IF (TRIM(chdum) == "-ini") THEN
        inp_fmt = 5  
     ELSE IF (TRIM(chdum) == "-eminv") THEN
        inp_fmt = 6  
     ELSE IF (TRIM(chdum) == "-latbc") THEN
        inp_fmt = 7  
     ELSE IF (TRIM(chdum) == "-topbc") THEN
        inp_fmt = 8  
     ELSE IF (TRIM(chdum) == "-emibio") THEN
        inp_fmt = 9  
     ELSE
        idp = idp + 1
        arg(idp) = chdum
     ENDIF
  ENDDO

  filein = arg(1)
  fileout = arg(2)
  fileinfo = arg(3)
  !varname = arg(3)
  READ (arg(4),*,IOSTAT=ios) igen

  IF (filein == "" .OR. fileout == "" .OR. fileinfo == "" .OR. &
       ios /= 0 .OR. TRIM(filein) == "-h") THEN
     CALL write_help
     STOP
  ENDIF
  !--------------------------------------------------------------------------
  ! 1.1 Leggo elenco specie e definezioni area da file netcdf
  !leggo definzioni area e temp da file netcdf

  ncstat=nf90_open(filein,NF90_NOWRITE,ncid)
  NCERR(__LINE__)

  ! with theses dimensions.
  ncstat=nf90_inq_dimid(ncid,'Time',times1_dimid)
  NCERR(__LINE__)
  ncstat=nf90_inq_dimid(ncid,'DateStrLen',dstrdimid)
  NCERR(__LINE__)
  ncstat=nf90_inq_dimid(ncid,'west_east',we1_dimid)
  is3d1=(ncstat==NF90_NOERR)
  if(is3d1) then
  NCERR(__LINE__)                                  
  ncstat=nf90_inquire_dimension(ncid,we1_dimid,  len=nzonal)
  NCERR(__LINE__)
  else
  write(6,*)'non presente west-est'
  end if

  ncstat=nf90_inq_dimid(ncid,'south_north',sn1_dimid)
  is3d1=(ncstat==NF90_NOERR)
  if(is3d1) then
  NCERR(__LINE__)
  ncstat=nf90_inquire_dimension(ncid,sn1_dimid,  len=nmerid)
  NCERR(__LINE__)
  else
  write(6,*)'non presente south-north'
  end if

   ncstat=nf90_inq_dimid(ncid,'bottom_top',bt1_dimid) 
   is3d1=(ncstat==NF90_NOERR)
   if(is3d1) then
   NCERR(__LINE__)                                  
     ncstat=nf90_inquire_dimension(ncid,bt1_dimid,  len=nlevels)
     NCERR(__LINE__)
  else
     write(6,*)'non presente bottom_top '
     nlevels=1
  end if

   ncstat=nf90_inq_dimid(ncid,'h_boundary',h_dimid) 
   is3d1=(ncstat==NF90_NOERR)
   if(is3d1) then
   NCERR(__LINE__)                                  
     ncstat=nf90_inquire_dimension(ncid,h_dimid,  len=nhori)
     NCERR(__LINE__)
  else
     write(6,*)'non presente h_boundary '
  end if

  ncstat=nf90_inq_dimid(ncid,'type_day',times2_dimid)
  is3d1=(ncstat==NF90_NOERR)
  if(is3d1) then
  NCERR(__LINE__)
     ncstat=nf90_inquire_dimension(ncid,times2_dimid,len=typeday)
     NCERR(__LINE__)
  else
  typeday=1
  endif
   if(inp_fmt==7) then
  ncstat=nf90_inq_dimid(ncid,'Species',latspe_dimid)
  is3d1=(ncstat==NF90_NOERR)
  if(is3d1) then
  NCERR(__LINE__)
     ncstat=nf90_inquire_dimension(ncid,latspe_dimid,len=Species)
  NCERR(__LINE__)
    ncstat=nf90_inq_varid(ncid, 'conc',  latspe_varid)
  NCERR(__LINE__)
  write(6,*)'latspe_varid',species
  endif
  end if


  ncstat=nf90_inq_dimid(ncid,'biospecies',biospe_dimid)
  is3d1=(ncstat==NF90_NOERR)
  if(is3d1) then
  NCERR(__LINE__)
     ncstat=nf90_inquire_dimension(ncid,biospe_dimid,len=biospecies)
  NCERR(__LINE__)
    ncstat=nf90_inq_varid(ncid, 'emisb',  biospe_varid)
  NCERR(__LINE__)
   write(6,*)'biospoe_varid',biospe_varid
   endif
  ! Once we have got the identifiers, we can obtain the dimensions lengths
  ncstat=nf90_inquire_dimension(ncid,times1_dimid,len=times1)
  NCERR(__LINE__)
  ncstat=nf90_inquire_dimension(ncid,dstrdimid,len=dstrlen)


  ! copy titleattribute
  ncstat=nf90_get_att(ncid,NF90_GLOBAL,'Title',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_get_att(ncid,NF90_GLOBAL,'Domain',domain)
  NCERR(__LINE__)


  ncstat=nf90_inq_varid(ncid,'Times',times1_varid)
  NCERR(__LINE__)
  write(6,*)'N step temporali ' ,times1


  if(inp_fmt==9) then
   write(6,*)'N Variabili contenute nel file : ',trim(filein) ,biospecies
  else if(inp_fmt==7) then
   write(6,*)'N Variabili contenute nel file : ',trim(filein) ,species
  else
    ncstat=nf90_inquire(ncid, nVariables=nvarin)
      NCERR(__LINE__)
   write(6,*)'N Variabili contenute nel file : ',trim(filein) ,nvarin
   end if
  !
  if(inp_fmt==7) then
   nzonal=64 
   nmerid=41
  nlev=nlevels
   write(6,*)'setto ',nzonal 
  end if
  nx=nzonal
  ny=nmerid
  nlev=nlevels
  if(domain=="CTNBP2") then
     x1=262.5
     y1=4782.5
     x2=x1+(nx-1)*5
     y2=y1+(ny-1)*5
  else if(domain=="CTNEMR") then
     x1=502.5
     y1=4832.5
     x2=x1+(nx-1)*5
     y2=y1+(ny-1)*5
  else if(domain=="ARPAC") then
     x1=802.5
     y1=4402.5
     x2=x1+(nx-1)*5
     y2=y1+(ny-1)*5
  else if(domain=="CTNBP1") then
     x1=265.0
     y1=4785.0
     x2=x1+(nx-1)*10
     y2=y1+(ny-1)*10
  else if(domain=="EMR025") then
     x1=501.25
     y1=4831.25
     x2=x1+(nx-1)*2.5
     y2=y1+(ny-1)*2.5
  else if(domain=="CTNBP3") then
     x1=261.25
     y1=4781.25
     x2=x1+(nx-1)*2.5
     y2=y1+(ny-1)*2.5
  else if(domain=="TNOBP2") then
     x1=6.1875
     y1=42.53125
     x2=x1+(nx-1)*0.125
     y2=y1+(ny-1)*0.0625

 else if(domain=="VULC") then
     x1=6.00
     y1=42.5
     x2=x1+(nx-1)*0.1
     y2=y1+(ny-1)*0.1

else if(domain=="ISPBPA") then
     x1=262.5
     y1=4732.5
     x2=x1+(nx-1)*5
     y2=y1+(ny-1)*5

else if(domain=="ISPITA") then
     x1=285.0
     y1=4025.0
     x2=x1+(nx-1)*10
     y2=y1+(ny-1)*10

else if(domain=="LMSMR4") then
     x1=-8.5
     y1=-25.
     x2=x1+(nx-1)*0.0625
     y2=y1+(ny-1)*0.0625

else if(domain=="LMSMR5") then
     x1=-3.5
     y1=-21.925
     x2=x1+(nx-1)*0.025
     y2=y1+(ny-1)*0.025

else if(domain=="LAMAZ") then
     x1=-2.937
     y1=-21.125
     x2=x1+(nx-1)*0.0625
     y2=y1+(ny-1)*0.0625

end if

  allocate(buf2d1(nzonal,nmerid))
  allocate(buf3d1(nzonal,nmerid,nlevels))
  allocate(buf4d1(nzonal,nmerid,nlevels,3))
  allocate(conc2(nzonal*nmerid,nlevels))
  allocate(conc3(nzonal*nmerid,nlevels,3))
  allocate(conc1(nzonal*nmerid))

  ! 1.2 Leggo elenco specie ied altre grandezze dal file info
  OPEN (UNIT=30, FILE=fileinfo, STATUS="OLD", ERR=9999)
  k=0
  DO
     READ (30,'(a)',IOSTAT=ios) chrec
     IF (ios /= 0) EXIT
     ! Skippo righe vuote e di commento
     chrec = ADJUSTL(chrec)
     IF (chrec(1:1) == "!" .OR. TRIM(chrec) == "") CYCLE
     k = k+1

     ! Interpreto la riga letta
     SELECT CASE (k)
     CASE (1:1)
        READ (chrec,*,IOSTAT=ios) cem
          write(6,*)cem
     CASE (2)
        READ (chrec,'(a)',IOSTAT=ios) proj
     CASE (3)
        READ (chrec,*,IOSTAT=ios) xrot,yrot
     CASE (4)
        READ (chrec,*,IOSTAT=ios) lev_out(1:nlev)
     CASE (5)
        READ (chrec,*,IOSTAT=ios) scad_ini
          write(6,*)scad_ini
     CASE (6:maxvar+nhead)
        READ (chrec,*,IOSTAT=ios) code_var(k-nhead),tab_var(k-nhead)
        p1 = index(chrec,"!")
        p2 = index(chrec(p1+1:),"!") + p1
        IF (p2 > p1) THEN
          read(chrec(p1+1:p2-1),'(a)') namevar(k-nhead)
        ELSE
          read(chrec(p1+1:),'(a)') namevar(k-nhead)
        ENDIF
        namevar(k-nhead)=adjustl(trim(namevar(k-nhead)))
!           write(6,*)code_var(k-nhead),namevar(k-nhead),k-nhead,k
     CASE DEFAULT
        WRITE (*,*) "Troppe specie, elaboro le prime ",maxvar,&
             " (aumentare param. maxvar)"
        k = k-1
        EXIT

     END SELECT
     IF (ios /= 0) GOTO 9998

  ENDDO
  CLOSE(30)


        write(6,*)cem,nlev,x1,x2,y1,y2,nx,ny,inp_fmt

  ! Controlli e calcolo grandezze derivate
  IF (cem <= 0 .OR. nlev < 0 .OR. & 
       x1 >= x2 .OR. y1 >= y2 .OR. nx <= 1 .OR. ny <= 1  .OR. &
       (nlev==0 .AND. inp_fmt/=2 .AND. inp_fmt/=3) .OR. &
       inp_fmt < 1 .OR. inp_fmt > 9 .OR. &
       inp_fmt == 9 .AND. nlev /= 1)   GOTO 9997
  SELECT CASE (inp_fmt)
  CASE (1,4,5,6,7,8,9)          ! output,metg,meta,ini,eminv,latbc,topbc
     nvar = k - nhead
     nvar_out = COUNT(code_var(1:k-nhead) > 0)


  CASE (2)                      ! LANDUSE
     nvar = 9
     nvar_out = 10
     nlev = 1
  CASE (3)                      ! BIOGENIC
     nvar = 5
     nvar_out = 6
     nlev = 1
  END SELECT
  !IF ((inp_fmt == 4 .OR. inp_fmt == 5) .AND. nvar /= nvar3d+nvar2d) GOTO 9993

  dx = (x2-x1)/REAL(nx-1)
  dy = (y2-y1)/REAL(ny-1)

  ! Se formato BC, stendo i 4 bordi su un vettore 1D ...
  IF (inp_fmt == 7) THEN
        write(6,*)'aaaaaaaaaaaaaaaaa',nzonal,nmerid
     nxi = nzonal
     nyi = nmerid
        write(6,*)'aaaaaaaaaaaaaaaaa',nzonal,nmerid
!        write(6,*)nxi,nyi
     dx=10
     nx = (nxi+nyi)*2 + 4
     ny = 1
     x2 = x1 + (nx-1)*dx
     y2 = y1
  ENDIF

  np = nx*ny
  WRITE (*,'(a,3i5,2f10.5)') "Parametri griglia (nx,ny,nz,dx,dy) ", &
       nx,ny,nlev,dx,dy
  WRITE (*,'(a,a,i3,a,i3)') "Specie contenute nel file ", filein ,nvarin-6," da scrivere: ",nvar_out
  WRITE (*,'(a,i3)') "Livelli in output: ",COUNT(lev_out(1:nlev) /= 0)
  WRITE (*,*) 

  !--------------------------------------------------------------------------
  ! 1.3) Definisco la parte costante dell'header dei grib

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

  IF (UPPERCASE(proj) == "UTM" .OR. (xrot == 0. .AND. yrot == 0.)) THEN
     ksec2(1) = 0
     ksec2(6) = 128
     ksec2(9) = NINT(dx * 1000.)
     ksec2(10) = NINT(dy * 1000.)
     ksec2(13) = 0
     ksec2(14) = 0

  ELSE IF (UPPERCASE(proj) == "GEO" .AND. (xrot /= 0. .OR. yrot /= 0.)) THEN
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
     ksec4(k) = 0.
  ENDDO

  !--------------------------------------------------------------------------
  ! 1.4 Altre operazioni

  ! Alloco le varaibili
  SELECT CASE (inp_fmt)

  CASE (1,4,8)
     allocate(conc_out(nzonal*nmerid,nlevels,nvar))
  allocate(varids1(nvarin))
  CASE (2,3)
  allocate(varids1(nvarin))
     ALLOCATE (conc_out(nzonal*nmerid,nlevels,nvar))
     ALLOCATE (tot(nzonal*nmerid))
  CASE (5) 
  allocate(varids1(nvarin))
     ALLOCATE (conc_out(nzonal*nmerid,nlevels,nvar))
  CASE (6) 
  allocate(varids1(nvarin))
     ALLOCATE (conc_out(nzonal*nmerid,nlevels,nvar))
     ALLOCATE (conc4(nzonal*nmerid,nlevels,typeday,nvar))

  CASE (7)
  write(6,*)'np ',np,nhori
  allocate(conc(species,nhori,nlevels))
     ALLOCATE (conc_out(nhori,nlevels,nvar))
     ALLOCATE (conc_miss(nhori+4,nlevels,nvar))
  CASE (9)
     allocate(conc_out(nzonal*nmerid,nlevels,nvar))
  allocate(emisb(biospecies,nzonal,nmerid))
  allocate(emisb1(biospecies,nzonal*nmerid))

  END SELECT

  ! Apro i files unformatted
  SELECT CASE (inp_fmt)
     !CASE (1,4,6,8,9)
     !  OPEN (UNIT=31, FILE=filein, STATUS="OLD", FORM="UNFORMATTED", ERR=9996)
  CASE (2)
     OPEN (UNIT=31, FILE=filein, STATUS="OLD", FORM="FORMATTED", ERR=9996)
  END SELECT

  CALL PBOPEN (iu,fileout,'W',kret)

  ! Disabilito i controlli sui parametri GRIBEX
  CALL grsvck(0)

  ! Trovo codice per EOF
  CALL get_eof_eor(eof,eor)

  !--------------------------------------------------------------------------
  ! 2) Lettura e scrittura: 

  !--------------------------------------------------------------------------
  ! 2.1) Formato OUT.sim (ciclo sui record in input - scadenze orarie)

  IF (inp_fmt == 1) THEN
     !    

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

        do ivar=1,nvar
           ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
           is3d1=(ncstat==NF90_NOERR)
           if(is3d1) then
              ntrov=ntrov+1
              if( kscad ==1 )write(6,*)'trovo la variabile  ',namevar(ivar),ivar,ivarid
              ncstat=nf90_inquire_variable(ncid,ivarid,varids1(ivarid)%varname,vartype1,varids1(ivarid)%ndims)
              NCERR(__LINE__)


              ncstat=nf90_get_var( ncid,ivarid, buf3d1, &
                   (/     1,      1,       1, kscad/),  & ! start vector
                   (/nzonal, nmerid, nlevels,     1/))    ! count vector
              NCERR(__LINE__)

              conc2=reshape(buf3d1,(/nzonal*nmerid,nlevels/))
              conc_out(:,:,ivar)=conc2(:,:)

              IF (code_var(ivar) <= 0) CYCLE
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
              if (kscad ==1) write(6,*)'non ho trovato la variabile  ',namevar(ivar)
           end if
        end do                       !specie 
     ENDDO                     ! scadenze

     ncstat=nf90_close(ncid)
     NCERR(__LINE__)
     WRITE (*,'(a,i3,a,i6,a,i3,a,i3,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
          cnt_grb," grib, Trovate ",ntrov, " N variabili  ,Scritte ",nscri ," N variabili"

     !--------------------------------------------------------------------------
     ! 2.2) Formato LANDUSE

  ELSE IF (inp_fmt == 2) THEN

     DO k = 1,np
        READ (31,*,IOSTAT=ios) conc_out(k,1,1:9)
        IF (ios /= 0) GOTO 9995
     ENDDO
     tot(1:np) = REAL( MAXLOC(conc_out(1:np,1,1:9),DIM=2) )

     ksec1(7) = 1
     ksec1(8) = 0
     ksec1(9) = 0

     ksec1(21) = 20
     ksec1(10) = 100
     ksec1(11) = 1
     ksec1(12) = 1
     ksec1(13) = 1

     ksec1(16) = 0

     ! Scrivo le frazioni dei vari tipi di LU
     DO kvar = 1,nvar
        ksec1(1) = 200
        ksec1(6) = 130 + kvar

        CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             conc_out(1:np,1,kvar),np,kbuffer,maxdim,kword,'C',kret)
        CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
        IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
     ENDDO

     ! Scrivo il LU prevalente
     ksec1(1) = 200
     ksec1(6) = 121
     ksec4(2) = 24

     CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
          tot(1:np),np,kbuffer,maxdim,kword,'C',kret)
     CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
     IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

     WRITE (*,*) "Scritti ",nvar+1," grib"

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
     DO kvar = 1,nvar
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

     WRITE (*,*) "Scritti ",nvar+1," grib"

     !--------------------------------------------------------------------------
     ! 2.4 ) Formato METEO

  ELSE IF (inp_fmt == 4 ) THEN

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
        do ivar=1,nvar
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
                      (/nzonal, nmerid, nlevels,     1/))    ! count vector
                 NCERR(__LINE__)

                 write(6,*)'3D ',varids1(ivarid)%varname
                 conc2=reshape(buf3d1,(/nzonal*nmerid,nlevels/))
                 conc_out(:,:,ivar)=conc2(:,:)
                 nl=nlevels
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
     ! 2.5) Formato INI / END

  ELSE IF (inp_fmt == 5) THEN

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

        do ivar=1,nvar
           ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
           is3d1=(ncstat==NF90_NOERR)
           if(is3d1) then
              ntrov=ntrov+1
              write(6,*)'trovo la variabile  ',namevar(ivar),ivar,ivarid
              ncstat=nf90_inquire_variable(ncid,ivarid,varids1(ivarid)%varname,vartype1,varids1(ivarid)%ndims)
              NCERR(__LINE__)


              ncstat=nf90_get_var( ncid,ivarid, buf3d1, &
                   (/     1,      1,       1, kscad/),  & ! start vector
                   (/nzonal, nmerid, nlevels,     1/))    ! count vector
              NCERR(__LINE__)

              conc2=reshape(buf3d1,(/nzonal*nmerid,nlevels/))
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
     ! 2.6) Formato Emission Inventory

  ELSE IF (inp_fmt == 6) THEN
     ! da fare
    cnt_grb=0
   slen=LEN(TRIM(filein))
   READ(filein(slen-1:slen),'(i2)',iostat=ios)mm
  if(ios/=0 .or. mm < 1 .or.mm>12) mm=1

     DO kscad = 1, times1
       ntrov=0
       nscri=0
        ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
        NCERR(__LINE__)
        idata=mm5date2numeric(datebuf)
        write(6,*)trim(datebuf),idata
        do ivar=1,nvar
           ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
           is3d1=(ncstat==NF90_NOERR)
           if(is3d1) then
              write(6,*)'ho trovato la variabile  ',namevar(ivar),ivar,ivarid,code_var(ivar)
              ncstat=nf90_inquire_variable(ncid,ivarid,varids1(ivarid)%varname,vartype1,varids1(ivarid)%ndims)
              NCERR(__LINE__)
               ntrov=ntrov+1 
              write(6,*)ivarid,varids1(ivarid)%varname,namevar(ivar)
              ncstat=nf90_get_var( ncid,ivarid, buf4d1, &
                   (/  1,   1,      1,       1, kscad/),  & ! start vector
                   (/ nzonal, nmerid, nlevels,typeday,1/)   & ! count vector
                   )
              NCERR(__LINE__)
                 write(6,*)'qui'
              conc3=reshape(buf4d1,(/nzonal*nmerid,nlevels,typeday/))
                 write(6,*)'quic4'
              conc4(:,:,:,ivar)=conc3(:,:,:)     
                 write(6,*)'quic3'
              if(code_var(ivar)<=0) cycle
               write(6,*)'scrivi la variabile ',namevar(ivar),ivar
                  nscri=nscri+1 
                    ksec1(1) = tab_var(ivar)
                    ksec1(6) = code_var(ivar)
                 !   Elaboro i grib relativi a ciascuna specie / tipo di giorno 
                 !   (clclo interno su ore e livelli)
                 DO kday = 1,typeday
                  conc_out(:,:,:)=conc4(:,:,kday,:)     
                    ksec1(10) = 100
                    ksec1(11) = mm
                    ksec1(12) = kday
                    ksec1(14) = 0
                    ksec1(21) = 20
                    ksec1(16) = 0
                    DO klev = 1,nlevels
                       IF (lev_out(klev) == 0) CYCLE
!                       DO kh = 1,times1
                          ksec1(7) = 109
                          ksec1(8) = klev
                          ksec1(9) = 0
                          ksec1(13) = kscad
                          CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                               conc_out(1:np,klev,ivar),np,kbuffer,maxdim,kword,'C',kret)
                          CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
                         IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
                          cnt_grb = cnt_grb + 1
!                     ENDDO                ! ore
                    ENDDO                  ! livelli
                 ENDDO                    ! tipi di giorno
               else          
              write(6,*)'non ho trovato la variabile  ',namevar(ivar)
           end if
        end do                       !specie 
     ENDDO                     ! scadenze

     ncstat=nf90_close(ncid)
     NCERR(__LINE__)
     WRITE (*,'(a,i3,a,i10,a,i3,a,i3,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
          cnt_grb," grib, Trovate ",ntrov, " N variabili  ,Scritte ",nscri ," N variabili"


              !--------------------------------------------------------------------------
              ! 2.7) Formato Lateral Boundary conditions

           ELSE IF (inp_fmt == 7) THEN
     DO kscad = 1, times1
    ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
        NCERR(__LINE__)
        idata=mm5date2numeric(datebuf)
              write(6,*)trim(datebuf),idata

        CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
             ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

              ncstat=nf90_get_var( ncid,latspe_varid, conc, &
                   (/     1,        1,  1, kscad/),  & ! start vector
                   (/species,nhori,nlevels,    1/))    ! count vector
              NCERR(__LINE__)
          write(6,*)'qu3',nyi,nxi

               write(6,*)nhori,nlevels,species
              do k=1,nhori
                do nl=1,nlevels
                do ivar=1,species
              conc_out(k,nl,ivar)=conc(ivar,k,nl)
                  end do
                end do
                end do
                             
              !   Inserisco valori mancanti negli angoli della cornice delle BC
              !   nxi,nyi
                  conc_miss(1:nyi,:,:) = conc_out(1:nyi,:,:)                              ! W
                  conc_miss(nyi+1,:,:) = rmis
                  conc_miss(nyi+2:2*nyi+1,:,:) = conc_out(nyi+1:2*nyi,:,:)                ! E
                  conc_miss(2*nyi+2,:,:) = rmis
                  conc_miss(2*nyi+3:2*nyi+nxi+2,:,:) = conc_out(2*nyi+1:2*nyi+nxi,:,:)    ! S
                  conc_miss(2*nyi+nxi+3,:,:) = rmis
                 conc_miss(2*nyi+nxi+4:2*nyi+2*nxi+3,:,:) = &
                    conc_out(2*nyi+nxi+1:2*nyi+2*nxi,:,:)                                 ! N
                  conc_miss(2*nyi+2*nxi+4,:,:) = rmis
              
              


              do ivar=1,species
              IF (code_var(ivar) <= 0) CYCLE
              write(6,*)'scrivo  la variabile  ',namevar(ivar),ivar
              nscri=nscri+1
              ksec1(1) = tab_var(ivar)
              ksec1(6) = code_var(ivar)
              DO klev = 1,nlevels
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
              ! 2.8) Formato Top BC

           ELSE IF (inp_fmt == 8) THEN

              cnt_grb = 0
              DO kscad = 1, HUGE(kscad)

                 READ (31,IOSTAT=ios) idata, &
                      ((conc_out(k,1,kvar),kvar=1,nvar),k=1,np)
                 IF (ios == eof) THEN
                    EXIT
                 ELSE IF (ios /= 0) THEN
                    GOTO 9995
                 ENDIF

                 IF (kscad == 1) idata_ini = idata
                 CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
                      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

                 DO kvar = 1,nvar

                    IF (code_var(kvar) <= 0) CYCLE

                    ksec1(1) = tab_var(kvar)
                    ksec1(6) = code_var(kvar)

                    DO klev = 1,nlev
                       IF (lev_out(klev) == 0) CYCLE
                       ksec1(7) = 109
                       ksec1(8) = klev
                       ksec1(9) = 0

                       CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                            conc_out(1:np,klev,kvar),np,kbuffer,maxdim,kword,'C',kret)

                       CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
                       IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

                       cnt_grb = cnt_grb + 1

                    ENDDO                ! livelli
                 ENDDO                  ! specie
              ENDDO                    ! scadenze

              WRITE (*,'(a,i3,a,i6,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
                   cnt_grb," grib"



 ! 2.9) Formato BEMISSION 

  ELSE IF (inp_fmt == 9) THEN
          write(6,*)'qui'
     !    
     cnt_grb = 0
     DO kscad = 1, times1
        ntrov=0
        nscri=0
        ncstat=nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
        NCERR(__LINE__)
          write(6,*)'qu2'
        idata=mm5date2numeric(datebuf)

        CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
             ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

              ncstat=nf90_get_var( ncid,biospe_varid, emisb, &
                   (/     1,        1,  1, kscad/),  & ! start vector
                   (/biospecies,nzonal, nmerid,    1/))    ! count vector
              NCERR(__LINE__)
          write(6,*)'qu3'

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
!           else
!              write(6,*)'non ho trovato la variabile  ',namevar(ivar)
!           end if
              end do                       !specie 
     ENDDO                     ! scadenze

     ncstat=nf90_close(ncid)
     NCERR(__LINE__)
     WRITE (*,'(a,i3,a,i6,a,i3,a,i3,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
          cnt_grb," grib, Trovate ",ntrov, " N variabili  ,Scritte ",nscri ," N variabili"


     ENDIF

           CALL PBCLOSE (iu,kret)



           !--------------------------------------------------------------------------
           ! 3) Rileggo il 1o record di filein per verificare se contiene altri dati

           SELECT CASE (inp_fmt) 

           CASE (2)
              READ (31,*,IOSTAT=ios) rdum
              IF (ios == 0) WRITE (*,*) "Warning: il file ",TRIM(filein), &
                   " contiene altri dati!"
              CLOSE(31)


           END SELECT

           STOP
           !--------------------------------------------------------------------------
           ! 4) Gestione erori

9999       CONTINUE
           WRITE (*,*) "AHHH!! Errore aprendo ",TRIM(fileinfo)
           STOP

9998       CONTINUE
           WRITE (*,*) "Record illegale o mal posizionato in ",TRIM(fileinfo)
           WRITE (*,*) "Numero record (esclusi commenti) ",k
           WRITE (*,*) TRIM(chrec)
           STOP

9997       CONTINUE
           WRITE (*,*) "Parametri illegali leggendo ",TRIM(fileinfo)
           STOP

9996       CONTINUE
           WRITE (*,*) "Errore aprendo ",TRIM(filein)
           STOP

9995       CONTINUE
           WRITE (*,*) "Errore leggendo ",TRIM(filein)
           STOP

9993       CONTINUE
           WRITE (*,'(3a,i3,a)') "Numero specie illegale in ",TRIM(fileinfo), &
                " (attese ",nvar2d+nvar3d,")"
           STOP

9992       CONTINUE
           WRITE (*,*) "Date inconsistenti in ",TRIM(filein)
           STOP

         END PROGRAM chimerencdf2grib_completo



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
WRITE (*,*) "Uso: chimere2grib.exe filein fileout fileinfo igen [-V2003] [-v2004] [-v]"
WRITE (*,*) "     [-lu / -bio / -metg / -meta / -ini / -eminv / -latbc]"
WRITE (*,*)
WRITE (*,*) "  con 4 parametri analizza un file col tracciato standard chimere: "
WRITE (*,*) "    un record per ora, con data e matrice 4d (X,Y,Z,t,specie)." 
WRITE (*,*) "    Questo tracciato vale per i ifles: out.sim, par.sim, AEMISSIONS,"
WRITE (*,*) "    BEMISSIONS, LAT_CONCS, TOP_CONCS, par_2D, par_3D"
WRITE (*,*)
WRITE (*,*) "  -lu:   analizza un file LANDUSE (record senza data; calcola tipo prev.)"
WRITE (*,*) "  -bio:  analizza un file BIOFACS (calcola potenziale totale)"
WRITE (*,*) "  -metg: analizza un file METEO, modello gas (record con grandezze 3d e 2d)"
WRITE (*,*) "  -meta: analizza un file METEO, modello aerosol"
WRITE (*,*) "  -ini:  analizza un file ini.sim, end.sim (un record per ogni specie 3d)"
WRITE (*,*) "  -eminv:analizza un file EMISSIONS-area.mm (profili giornalieri di emiss.)"
WRITE (*,*) "  -latbc:analizza un file LAT_CONCS"
WRITE (*,*) "  -topbc:analizza un file TOP_CONCS (mettere nz = 1)"
WRITE (*,*) ""
WRITE (*,*) "  -V2003:legge i files della versione Chimere 200310F (solo per METEO)"
WRITE (*,*) "  -V2004:legge i files della versione Chimere 200410A (solo per METEO)"
WRITE (*,*) "  -v:    verbose (log a schermo esteso)"
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






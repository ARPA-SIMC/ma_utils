PROGRAM calmet2grib
!--------------------------------------------------------------------------
! Programma per leggere l'output di calmet (calmet.dat) e riscriverlo in
! formato GRIB. Sostituisce met2grib.f 
!
! Uso: calmet2grib.exe filedat filegrb igen [-w] [-wcart] [-h] 
!
!                                         Versione 4.0.1, Enrico 05/11/2012
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE       

!--------------------------------------------------------------------------
! 0) Dichiarazioni
        
! 0.1) Parametri costanti

! Dimensionani max arrays (compatibili con il file params.met di calmet)
INTEGER, PARAMETER :: mxnx=150        ! Max number of X grid cells
INTEGER, PARAMETER :: mxny=150        ! Max number of Y grid cells
INTEGER, PARAMETER :: mxnz=12         ! Max number of layers
INTEGER, PARAMETER :: mxss=150        ! Max number of surf. met. stations
INTEGER, PARAMETER :: mxus=150        ! Max number of upper air stations
INTEGER, PARAMETER :: mxps=60         ! Max number of precipit. stations
INTEGER, PARAMETER :: mxows=15        ! Max number of overwater stations
INTEGER, PARAMETER :: mxnzp1=mxnz+1   ! Max number of face cells
INTEGER, PARAMETER :: npmx = 15

! Altri parametri
REAL, PARAMETER :: mo_max = 1000.     ! max valore per ABS(MO)
REAL, PARAMETER :: rmis = -9999.      ! valore per dati mancanti
INTEGER, PARAMETER :: mxhr = 10000    ! max numero di ore in filedat

! 0.2) Dichiarazioni per GRIBEX.
INTEGER, PARAMETER :: maxdim = 100000 ! dimensione massima dei GRIB
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! 0.3) Dichiarazioni per lettura headers
REAL :: zfacem(mxnzp1),xssta(mxss),yssta(mxss),xusta(mxus),yusta(mxus)
REAL :: xPsta(mxps),ypsta(mxps),zo(mxnx,mxny),elev(mxnx,mxny),xlai(mxnx,mxny)
REAL :: dgrid,xorigr,yorigr,rver
REAL :: xlat0,xlon0,conec,xlat1,xlat2,rlat0,rlon0
INTEGER :: ilandu(mxnx,mxny),nears(mxnx,mxny)
INTEGER :: ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype
INTEGER :: nx,ny,nz,iutmzn,iwfcod,ncom
INTEGER :: nssta,nusta,npsta,nowsta,nlu,iwat1,iwat2
INTEGER :: ibyrn,ibmon,ibdyn,ibhrn,ibsecn
INTEGER :: ieyrn,iemon,iedyn,iehrn,iesecn
CHARACTER (LEN=134) :: comment
CHARACTER (LEN=80) :: title(3)
CHARACTER (LEN=64) :: datamod
CHARACTER (LEN=16) :: dataset,dataver
CHARACTER (LEN=8) :: ver,level,pmap,datum
LOGICAL :: lcalgrd,llconf
 
! 0.4) Dichiarazioni per lettura data records
REAL :: u(mxnx,mxny,mxnz),v(mxnx,mxny,mxnz),w(mxnx,mxny,mxnz)
REAL :: ztemp(mxnx,mxny,mxnz)      
REAL :: ustar(mxnx,mxny),zi(mxnx,mxny),el(mxnx,mxny)
REAL :: wstar(mxnx,mxny),rmm(mxnx,mxny)
REAL :: tempk(mxss),rho(mxss),qsw(mxss)
REAL :: tempk2d(mxnx,mxny),rho2d(mxnx,mxny),qsw2d(mxnx,mxny)
REAL :: daten,feast,fnorth,utmhem,rnlat0,relon0
INTEGER :: ipgt(mxnx,mxny)
INTEGER :: irh(mxss),ipcode(mxss),irh2d(mxnx,mxny),ipcode2d(mxnx,mxny)
INTEGER :: ndathr(20),iutmz
CHARACTER (LEN=8) :: clabel,axtz
  
! 0.5) Variabili locali
TYPE (date) :: data_out(mxhr)
REAL dhdx(mxnx,mxny),dhdy(mxnx,mxny),wcart(mxnx,mxny)
REAL :: liv_uv(mxnz),x1,y1,x2,y2,dgridg,xorigrg,yorigrg
INTEGER :: nxg,nyg,iutmzng,luse(mxnx,mxny)
INTEGER :: ora_out(mxhr),iaa,ijj,hhgmt,cnt_par,kpar,iver
INTEGER :: igen,iuout,iuorog,ios,ier,idum,j,i,ih,k,kl,kg,cntg
CHARACTER (LEN=80) :: filedat,filegrb,str_gen,chpar,next_arg
LOGICAL :: outw_tf,outw_cart,orok

!--------------------------------------------------------------------------
! 1) Preliminari:

!--------------------------------------------------------------------------
! 1.1) Parametri da riga comando

iver = 52
next_arg = ""
outw_tf = .FALSE.
outw_cart = .FALSE.
cnt_par = 0

DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(chpar) == "-v63") THEN
    iver = 63
  ELSE IF (TRIM(chpar) == "-w") THEN
    outw_tf = .TRUE.
  ELSE IF (TRIM(chpar) == "-wcart") THEN
    outw_cart = .TRUE.
  ELSE IF (cnt_par == 0) THEN
    cnt_par = 1
    filedat = chpar
  ELSE IF (cnt_par == 1) THEN
    cnt_par = 2
    filegrb = chpar
  ELSE IF (cnt_par == 2) THEN
    cnt_par = 3
    READ (chpar,*,IOSTAT=ios) igen
  ELSE
    CALL scrive_help
    STOP
  ENDIF
ENDDO

IF (cnt_par /= 3 .OR. ios /= 0) THEN
  CALL scrive_help
  STOP
ENDIF  

! Disabilito controlli GRIBEX
CALL GRSVCK(0)

! Apro filedat
OPEN (UNIT=20, FILE=filedat, FORM='UNFORMATTED', STATUS='OLD', ERR=9999)

! Apro filelog
OPEN (UNIT=90, FILE="calmet2grib.log", FORM='FORMATTED', STATUS='REPLACE')

!--------------------------------------------------------------------------
! 1.2 Leggo header records

IF (iver < 60) THEN

  ! HR 1: Run title
  READ (20) title
   
  ! HR 2: versione Calmet, run and grid information
  READ (20) ver,level,ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
    nx,ny,nz,dgrid,xorigr,yorigr,iutmzn,iwfcod,nssta, &
    nusta,npsta,nowsta,nlu,iwat1,iwat2,lcalgrd 
  READ (ver,*) rver 
  iver = NINT(rver*10.)
  
  ! HR 3: Lambert Conformal parameters
  IF (iver >= 52) THEN
    READ (20) xlat0,xlon0,llconf,conec,xlat1,xlat2,rlat0,rlon0
  ENDIF

ELSE IF (iver >= 60) THEN

  ! HR 1: File Declaration -- 24 words
  READ (20) dataset,dataver,datamod

  ! HR 2 - NCOM+2: Comment lines
  READ (20) ncom
  DO k = 1,ncom
    READ (20)
  ENDDO

  ! HR NCOM+3: run control parameters -- 39 words
  READ (20) ibyrn,ibmon,ibdyn,ibhrn,ibsecn, &
    ieyrn,iemon,iedyn,iehrn,iesecn, &
    axtz,irlg,irtype, nx,ny,nz,dgrid,xorigr,yorigr, &
    iwfcod,nssta,nusta,npsta,nowsta, nlu,iwat1,iwat2, &
    lcalgrd,pmap,datum,daten,feast,fnorth,utmhem,iutmzn, &
    rnlat0,relon0,xlat1,xlat2

ENDIF

!--------------------------------------------------------------------------
! 1.3) Leggo arrays constanti

CALL read1d(20,iver,zfacem,mxnzp1,nz+1,idum,clabel)      ! cell face
CALL read1d(20,iver,xssta,mxss,nssta,idum,clabel)        ! surf.stations coord.
CALL read1d(20,iver,yssta,mxss,nssta,idum,clabel)
CALL read1d(20,iver,xusta,mxus,nusta,idum,clabel)        ! upper stations coord.
CALL read1d(20,iver,yusta,mxus,nusta,idum,clabel)
IF (npsta > 0) THEN
  CALL read1d(20,iver,xpsta,mxps,npsta,idum,clabel)      ! precip.stations coord
  CALL read1d(20,iver,ypsta,mxps,npsta,idum,clabel)
ENDIF

CALL read2d(20,iver,zo,mxnx,mxny,nx,ny,idum,clabel)      ! roughness
CALL readi2d(20,iver,ilandu,mxnx,mxny,nx,ny,idum,clabel) ! land use 
CALL read2d(20,iver,elev,mxnx,mxny,nx,ny,idum,clabel)    ! elevations
CALL read2d(20,iver,xlai,mxnx,mxny,nx,ny,idum,clabel)    ! leaf area index
IF (nssta > 0) &
  CALL readi2d(20,iver,nears,mxnx,mxny,nx,ny,idum,clabel)! nearest surf.station

!--------------------------------------------------------------------------
! 1.4) Calcoli e controlli dipendenti da headers e campi costanti

IF (nz > mxnz .OR. nx > mxnx .OR. ny > mxny .OR. &
    nssta > mxss .OR. nusta > mxus .OR. npsta > mxps) GOTO 9993

x1 = xorigr + dgrid/2.
y1 = yorigr + dgrid/2.
x2 = x1 + (nx-1) * dgrid
y2 = y1 + (ny-1) * dgrid
liv_uv(1:nz) = (zfacem(1:nz) + zfacem(2:nz+1)) / 2.

IF (iver >= 60) THEN
  CALL ch2ibtz(axtz,ibtz)
ENDIF

rver = 0.1 * REAL(iver)

WRITE (*,'(2a)') "calmet2grib.exe: analisi file input ",TRIM(filedat)
WRITE (*,'(a,f4.1,a,i3)') "  versione ",rver," scadenze ",irlg
WRITE (*,'(a,3i4,3x,f8.3)') "  nx,ny,nz,dgrid: ",nx,ny,nz,dgrid/1000.
WRITE (*,'(a,4(2x,f8.3))') "  xmin,ymin,xmax,ymax: ", &
  x1/1000.,y2/1000.,x2/1000.,y2/1000.

IF (irlg > mxhr) GOTO 9998

!--------------------------------------------------------------------------
! 1.5) Se richiesto calcolo GRAD(elev) in ogni punto 
!      NB: per i punti interni prendo la diff. centrata, per i punti al 
!          bordo la differenza asimmetrica

IF (outw_cart) THEN

! Dh/Dx: punti interni, bordo E, bordo W
  dhdx(2:nx-1,1:ny) = (elev(3:nx,1:ny) - elev(1:nx-2,1:ny)) / (2*dgrid)
  dhdx(1,1:ny) = (elev(2,1:ny) - elev(1,1:ny)) / dgrid
  dhdx(nx,1:ny) = (elev(nx,1:ny) - elev(nx-1,1:ny)) / dgrid

! Dh/dy: punti interni, bordo S, bordo N
  dhdy(1:nx,2:ny-1) = (elev(1:nx,3:ny) - elev(1:nx,1:ny-2)) / (2*dgrid)
  dhdy(1:nx,1) = (elev(1:nx,2) - elev(1:nx,1)) / dgrid
  dhdy(1:nx,ny) = (elev(1:nx,ny) - elev(1:nx,ny-1)) / dgrid

ENDIF

!---------------------------------------------------------------------
! 1.6) Apro filegrib e assegno gli elementi costanti degli header

CALL PBOPEN (iuout,filegrb,'W',kret)

! sezione 1
ksec1(1) = 200
ksec1(2) = 200
ksec1(3) = igen
ksec1(4) = 255
ksec1(5) = 128
ksec1(9) = 0

ksec1(15) = 1
ksec1(16) = 0
ksec1(17) = 0
ksec1(18) = 10
ksec1(19) = 0
ksec1(20) = 0
ksec1(22:) = 0

! sezione 2
ksec2(1) = 0
ksec2(2) = nx
ksec2(3) = ny
ksec2(4) = NINT(y1)         ! 4852.500
ksec2(5) = NINT(x1)         ! 402.500
ksec2(6) = 128
ksec2(7) = NINT(y2)         ! 5107.500
ksec2(8) = NINT(x2)         ! 847.500
ksec2(9) = NINT(dgrid)      ! 5.000
ksec2(10) = NINT(dgrid)     ! 5.000
ksec2(11) = 64
ksec2(12:) = 0

psec2(:) = 0.

! sezione 3
ksec3(:) = 0
psec3(:) = rmis

! sezione 4
ksec4(1) = ksec2(2) * ksec2(3)
!ksec4(2) = 1
ksec4(2) = 24
ksec4(3:) = 0

!---------------------------------------------------------------------
! 2) Ciclo sulle scadenze

cntg = 0
DO ih=1,irlg

! 2.1) Lettura data records da filedat
! Dovrebbero essere ripetuti IRLG volte i.e. la durata del run in ore

  ndathr(:) = -9999

! Componenti U, V, W 
  WRITE (90,*) "Inizio lettura UVW"
  DO k=1,nz
    CALL read2d(20,iver,u(1:mxnx,1:mxny,k),mxnx,mxny,nx,ny,ndathr(1),clabel)
    CALL read2d(20,iver,v(1:mxnx,1:mxny,k),mxnx,mxny,nx,ny,ndathr(2),clabel)
    IF (lcalgrd) &
      CALL read2d(20,iver,w(1:mxnx,1:mxny,k),mxnx,mxny,nx,ny,ndathr(3),clabel)
  ENDDO

! 3-D temperature field 
  IF (lcalgrd .AND. irtype == 1) THEN
    WRITE (90,*) "Inizio lettura T" 
    DO k=1,nz
      CALL read2d(20,iver,ztemp(1:mxnx,1:mxny,k),mxnx,mxny,nx,ny,ndathr(4),clabel)
    ENDDO
  ENDIF

! 2-D meteorological fields 
  IF (irtype == 1) THEN 
    WRITE (90,*) "Inizio lettura 2D"
    CALL readi2d(20,iver,ipgt,mxnx,mxny,nx,ny,ndathr(5),clabel)
    CALL read2d(20,iver,ustar,mxnx,mxny,nx,ny,ndathr(6),clabel)
    CALL read2d(20,iver,zi,mxnx,mxny,nx,ny,ndathr(7),clabel)
    CALL read2d(20,iver,el,mxnx,mxny,nx,ny,ndathr(8),clabel)
    CALL read2d(20,iver,wstar,mxnx,mxny,nx,ny,ndathr(9),clabel)
    IF (npsta /= 0) &
      CALL read2d(20,iver,rmm,mxnx,mxny,nx,ny,ndathr(10),clabel)
  ENDIF
 
! 1-D meteorological fields 
  IF (iver < 60 .AND. irtype == 1) THEN 
    CALL read1d(20,iver,tempk,mxss,nssta,ndathr(11),clabel)
    CALL read1d(20,iver,rho,mxss,nssta,ndathr(12),clabel)
    CALL read1d(20,iver,qsw,mxss,nssta,ndathr(13),clabel)
    CALL readi1d(20,iver,irh,mxss,nssta,ndathr(14),clabel)
    IF (npsta /= 0) &
      CALL readi1d(20,iver,ipcode,mxss,nssta,ndathr(15),clabel)

  ELSE IF (iver >= 60) THEN
    CALL read2d(20,iver,tempk2d,mxnx,mxny,nx,ny,ndathr(16),clabel)
    CALL read2d(20,iver,rho2d,mxnx,mxny,nx,ny,ndathr(17),clabel)
    CALL read2d(20,iver,qsw2d,mxnx,mxny,nx,ny,ndathr(18),clabel)
    CALL readi2d(20,iver,irh2d,mxnx,mxny,nx,ny,ndathr(19),clabel)
    IF (npsta /= 0) &
      CALL readi2d(20,iver,ipcode2d,mxnx,mxny,nx,ny,ndathr(20),clabel)

  ENDIF
                   
!---------------------------------------------------------------------
! 2.2) Calcolo la data corrispondente alla scadenza corrente

  IF (ndathr(2) /= ndathr(1)) GOTO 9994
  IF (lcalgrd .AND. irtype == 1 .AND. ndathr(3) /= ndathr(1)) GOTO 9994
  IF (irtype == 1 .AND. ANY(ndathr(5:9) /= ndathr(1))) GOTO 9994
  IF (irtype == 1 .AND. npsta /= 0 .AND. ndathr(10) /= ndathr(1)) GOTO 9994
  IF (irtype == 1 .AND. iver < 60 .AND. ANY(ndathr(11:14) /= ndathr(1))) GOTO 9994
  IF (irtype == 1 .AND. iver >= 60 .AND. ANY(ndathr(16:19) /= ndathr(1))) GOTO 9994
  IF (irtype == 1 .AND. iver < 60 .AND. npsta /= 0 .AND. ndathr(15) /= ndathr(1)) GOTO 9994
  IF (irtype == 1 .AND. iver >= 60 .AND. npsta /= 0 .AND. ndathr(20) /= ndathr(1)) GOTO 9994


  iaa=int(ndathr(1)/100000)   
  ijj=int((ndathr(1)-iaa*100000)/100)
  ora_out(ih) = ndathr(1)-iaa*100000-ijj*100

  IF (rver < 5.2) THEN               ! versioni < 5.2: anno in 2 cifre
    IF (iaa <= 50) THEN 
      data_out(ih) = greg(ijj,iaa + 2000)
    ELSE
      data_out(ih) = greg(ijj,iaa + 1900)
    ENDIF
  ELSE                               ! versioni da 5.2: anno in 4 cifre
    data_out(ih) = greg(ijj,iaa)
  ENDIF

! Passo da LST a GMT
  hhgmt = ora_out(ih) + ibtz
  IF (hhgmt < 0) THEN
    ora_out(ih) = hhgmt + 24
    data_out(ih) = data_out(ih) - 1
  ELSE IF (hhgmt > 23) THEN
    ora_out(ih) = hhgmt - 24
    data_out(ih) = data_out(ih) + 1
  ELSE
    ora_out(ih) = hhgmt
  ENDIF

  WRITE (*,'(a,i4,a,i4.4,3(1x,i2.2),a)') "  Scrivo scadenza ",ih, &
    " (",data_out(ih)%yy,data_out(ih)%mm,data_out(ih)%dd,ora_out(ih)," GMT)"
  WRITE (90,'(a,i4,a,i4.4,3(1x,i2.2),a)') "  Scrivo scadenza ",ih, &
    " (",data_out(ih)%yy,data_out(ih)%mm,data_out(ih)%dd,ora_out(ih)," GMT)"

  ksec1(10) = 1 + MOD(data_out(ih)%yy-1,100)
  ksec1(11) = data_out(ih)%mm
  ksec1(12) = data_out(ih)%dd
  ksec1(13) = ora_out(ih)
  ksec1(14) = 0
  ksec1(21) = 1 + (data_out(ih)%yy-1)/100

!---------------------------------------------------------------------
! 2.3) Scrittura GRIB

  DO kl = 1,nz

!   u-wind
    ksec1(7) = 105
    ksec1(8) = NINT(liv_uv(kl))
    ksec1(6) = 33

    field(1:nx*ny) = RESHAPE(u(1:nx,1:ny,kl),(/nx*ny/))
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
    cntg = cntg + 1

!   v-wind
    ksec1(7) = 105
    ksec1(8) = NINT(liv_uv(kl))
    ksec1(6) = 34

    field(1:nx*ny) = RESHAPE(v(1:nx,1:ny,kl),(/nx*ny/))
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
    cntg = cntg + 1

!   Temperature
    ksec1(7) = 105
    ksec1(8) = NINT(liv_uv(kl))
    ksec1(6) = 11

    field(1:nx*ny) = RESHAPE(ztemp(1:nx,1:ny,kl),(/nx*ny/))
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
    cntg = cntg + 1

!   Velocita' verticale cartesiana
    IF (outw_cart) THEN
      ksec1(7) = 105
      ksec1(8) = NINT(liv_uv(kl))
      ksec1(6) = 41

      IF (kl == 1) THEN
        wcart(1:nx,1:ny) = w(1:nx,1:ny,kl)/2. + &
          u(1:nx,1:ny,kl)*dhdx(1:nx,1:ny) + &
          v(1:nx,1:ny,kl)*dhdy(1:nx,1:ny)
      ELSE 
        wcart(1:nx,1:ny) = &
          (w(1:nx,1:ny,kl) + w(1:nx,1:ny,kl-1))/2. + &
          u(1:nx,1:ny,kl)*dhdx(1:nx,1:ny) + &
          v(1:nx,1:ny,kl)*dhdy(1:nx,1:ny)
      ENDIF

      field(1:nx*ny) = RESHAPE(wcart(1:nx,1:ny),(/nx*ny/))
      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   field,maxdim,kbuffer,maxdim,klen,'C',kret)
      IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
      CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
      IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
      cntg = cntg + 1
    ENDIF

!   Velocita' verticale terrain following
    IF (outw_tf) THEN
      ksec1(7) = 105
      ksec1(8) = NINT(zfacem(kl+1))
      ksec1(6) = 40
  
      field(1:nx*ny) = RESHAPE(w(1:nx,1:ny,kl),(/nx*ny/))
      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   field,maxdim,kbuffer,maxdim,klen,'C',kret)
      IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
      CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
      IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
      cntg = cntg + 1
    ENDIF
 
  ENDDO

! 2D fields
  ksec1(7) = 1
  ksec1(8) = 0

! ipgt
  ksec1(6) = 100
  field(1:nx*ny) = RESHAPE(ipgt(1:nx,1:ny),(/nx*ny/))
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntg = cntg + 1

! ustar
  ksec1(6) = 101
  field(1:nx*ny) = RESHAPE(ustar(1:nx,1:ny),(/nx*ny/))
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntg = cntg + 1

! hmix
  ksec1(6) = 102
  field(1:nx*ny) = RESHAPE(zi(1:nx,1:ny),(/nx*ny/))
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntg = cntg + 1

! el
  ksec1(6) = 103
  field(1:nx*ny) = RESHAPE(el(1:nx,1:ny),(/nx*ny/))
  field(1:nx*ny) = MAX(field(1:nx*ny),-mo_max)
  field(1:nx*ny) = MIN(field(1:nx*ny),mo_max)

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntg = cntg + 1

! wstar
  ksec1(6) = 104
  field(1:nx*ny) = RESHAPE(wstar(1:nx,1:ny),(/nx*ny/))
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntg = cntg + 1
  
ENDDO

!---------------------------------------------------------------------
! 3) Conclusione

CLOSE(20)
! A volte PBCLOSE da' errori misteriosi (segfault, memory fault...)
! CALL PBCLOSE(iuout)
WRITE (*,*) "  calmet2grib.exe: scritti ",cntg," grib"

STOP

!---------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filedat)
STOP

9998 CONTINUE
WRITE (*,*) "Richieste troppe scadenze, aumentare il parametro mxhr"
STOP

9994 CONTINUE
WRITE (*,*) "Trovate date diverse nella stessa scadenza, prob. errore di lettura"
WRITE (*,*) ndathr(1:20)
STOP

9993 CONTINUE
WRITE (*,*) "Dimensionamenti max insufficienti"
STOP

END PROGRAM calmet2grib

!==========================================================================
SUBROUTINE read1d(iu,iver,field,maxdim,nval,ndathr,clabel)
!
! Legge un campo 1d (reale) da un file calmet.dat
!
IMPLICIT NONE
INTEGER, INTENT(IN) :: iu,iver,maxdim,nval
REAL, INTENT(OUT) :: field(maxdim)
INTEGER, INTENT(OUT) :: ndathr
CHARACTER (LEN=8), INTENT(OUT) :: clabel
!
INTEGER :: ndathrb,ndathre,ibsec,iesec,k
!

IF (nval == 0) THEN
  WRITE (90,*) "read1d: non leggo record con 0 valori"
  RETURN
ENDIF

IF (iver < 60) THEN
  READ (iu) clabel,ndathrb,(field(k),k=1,nval)
ELSE
  READ (iu) clabel,ndathrb,ibsec,ndathre,iesec,(field(k),k=1,nval)
ENDIF

ndathr = ndathrb

WRITE (90,*) "read1d#",TRIM(clabel),"#",ndathr,field(1),field(nval)

RETURN
END SUBROUTINE read1d

!==========================================================================
SUBROUTINE readi1d(iu,iver,field,maxdim,nval,ndathr,clabel)
!
! Legge un campo 1d (intero) da un file calmet.dat
!
IMPLICIT NONE
INTEGER, INTENT(IN) :: iu,iver,maxdim,nval
INTEGER, INTENT(OUT) :: field(maxdim)
INTEGER, INTENT(OUT) :: ndathr
CHARACTER (LEN=8), INTENT(OUT) :: clabel
!
INTEGER :: ndathrb,ndathre,ibsec,iesec,k
!
IF (nval == 0) THEN
  WRITE (90,*) "readi1d: non leggo record con 0 valori"
  RETURN
ENDIF

IF (iver < 60) THEN
  READ (iu) clabel,ndathrb,(field(k),k=1,nval)
ELSE
  READ (iu) clabel,ndathrb,ibsec,ndathre,iesec,(field(k),k=1,nval)
ENDIF

ndathr = ndathrb

WRITE (90,*) "readi1d#",TRIM(clabel),"#",ndathr,field(1),field(nval)

RETURN
END SUBROUTINE readi1d

!==========================================================================
SUBROUTINE read2d(iu,iver,field,mxnx,mxny,nx,ny,ndathr,clabel)
!
! Legge un campo 2d (reale) da un file calmet.dat
!
IMPLICIT NONE
INTEGER, INTENT(IN) :: iu,iver,mxnx,mxny,nx,ny
REAL, INTENT(OUT) :: field(mxnx,mxny)
INTEGER, INTENT(OUT) :: ndathr
CHARACTER (LEN=8), INTENT(OUT) :: clabel
!
INTEGER :: ndathrb,ndathre,ibsec,iesec,i,j
!

IF (iver < 60) THEN
  READ (iu) clabel,ndathrb,((field(i,j),i=1,nx),j=1,ny)
ELSE
  READ (iu) clabel,ndathrb,ibsec,ndathre,iesec,((field(i,j),i=1,nx),j=1,ny)
ENDIF

ndathr = ndathrb

WRITE (90,*) "read2d#",TRIM(clabel),"#",ndathr,field(1,1),field(nx,ny)

RETURN
END SUBROUTINE read2d

!==========================================================================
SUBROUTINE readi2d(iu,iver,field,mxnx,mxny,nx,ny,ndathr,clabel)
!
! Legge un campo 2d (intero) da un file calmet.dat
!
IMPLICIT NONE
INTEGER, INTENT(IN) :: iu,iver,mxnx,mxny,nx,ny
INTEGER, INTENT(OUT) :: field(mxnx,mxny),ndathr
CHARACTER (LEN=8), INTENT(OUT) :: clabel
!
INTEGER :: ndathrb,ndathre,ibsec,iesec,i,j
!

IF (iver < 60) THEN
  READ (iu) clabel,ndathrb,((field(i,j),i=1,nx),j=1,ny)
ELSE
  READ (iu) clabel,ndathrb,ibsec,ndathre,iesec,((field(i,j),i=1,nx),j=1,ny)
ENDIF

ndathr = ndathrb

WRITE (90,*) "readi2d#",TRIM(clabel),"#",ndathr,field(1,1),field(nx,ny)

RETURN
END SUBROUTINE readi2d

!==========================================================================
SUBROUTINE ch2ibtz(axtz,ibtz)
! 
! Data la stirnga IBTZ (Calmet versione 6) ritorna ibtz 
! (convenzione calmet, i.e. -1)
! 
IMPLICIT NONE

CHARACTER (LEN=8), INTENT (IN) :: axtz
INTEGER, INTENT(OUT) :: ibtz

REAL :: xbtz
INTEGER :: ihr,imin
!
READ (axtz(4:6),'(i3)') ihr
READ (axtz(7:8),'(i2)') imin
IF (ihr < 0) imin = -imin

xbtz = ihr+imin/60.

! Flip sign as base time convention is opposite UTC/GMT
xbtz = -xbtz

IF (xbtz == 0.) THEN
  ibtz = 0
ELSE IF (xbtz > 0) THEN
  ibtz = NINT(xbtz)
ELSE IF (xbtz < 0) THEN
  ibtz = -NINT(-xbtz)
ENDIF

RETURN
END SUBROUTINE ch2ibtz

!==========================================================================

SUBROUTINE scrive_help
!
! Scrive a schermo un breve help
!
IMPLICIT NONE

WRITE (*,*) "Uso: calmet2grib.exe filedat filegrb igen [-v63] [-w] [-wcart] [-h]"
WRITE (*,*) "  filedat:  in formato calmet.dat"
WRITE (*,*) "  filegrb:  in formato grb (di default senza w)"
WRITE (*,*) "  igen:     processo generatore per codifica GRIB"
WRITE (*,*) "  -v63:     legge output versioni 6.3 e successive"
WRITE (*,*) "  -w:       scrive anche la velocita' verticale terrain following"
WRITE (*,*) "  -wcart:   calcola e scrive la velocita' verticale cartesiana (sui"
WRITE (*,*) "            livelli interi)"
WRITE (*,*) "  -h:       visualizza questo help"

RETURN
END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

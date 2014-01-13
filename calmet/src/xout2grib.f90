PROGRAM xout2grib
!--------------------------------------------------------------------------
! Programma per leggere l'output di calmet (calmet.dat) e riscriverlo in
! formato GRIB.
! Sostituisce met2grib.f 
! Uso: xout2grib.exe filedat filegrb igen
!
!                                         Versione 1.0.2, Enrico 13/01/2014
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
INTEGER, PARAMETER :: mxus=20         ! Max number of upper air stations
INTEGER, PARAMETER :: mxps=60         ! Max number of precipit. stations
INTEGER, PARAMETER :: mxows=15        ! Max number of overwater stations
INTEGER, PARAMETER :: mxnzp1=mxnz+1   ! Max number of face cells
INTEGER, PARAMETER :: npmx = 15

! Altri parametri
REAL, PARAMETER :: rmis = -9999.      ! dati mancanti (come in params.met)
INTEGER, PARAMETER :: mxhr = 100      ! max numero di ore in filedat

! 0.2) Dichiarazioni per GRIBEX.
INTEGER, PARAMETER :: maxdim = 100000 ! dimensione massima dei GRIB
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! 0.3) Dichiarazioni per lettura headers
REAL :: dgrid,xorigr,yorigr
INTEGER :: ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype,iutmznm
INTEGER :: nx,ny,nz
CHARACTER (LEN=80) :: title(3)
CHARACTER (LEN=8) :: ver,level

! 0.4) Dichiarazioni per lettura data records
REAL :: swb(mxnx,mxny),lwb(mxnx,mxny),lhf(mxnx,mxny),shf(mxnx,mxny)
REAL :: cfr(mxnx,mxny)
INTEGER :: ndathr
CHARACTER (LEN=8) :: clab(5)
  
! 0.5) Variabili locali
TYPE (date) :: data_out(mxhr)
REAL :: rver,x1,y1,x2,y2
INTEGER :: ora_out(mxhr),iaa,ijj,hhgmt
INTEGER :: igen,iuout,ios,idum,j,i,ih,k,kl,cntg
CHARACTER (LEN=200) :: filedat,filegrb,str_gen

!--------------------------------------------------------------------------
! 1) Preliminari:

!--------------------------------------------------------------------------
! 1.1) Parametri da riga comando
CALL getarg(1,filedat)
CALL getarg(2,filegrb)
CALL getarg(3,str_gen)
READ (str_gen,*,IOSTAT=ios) igen

IF (filedat == "" .OR. TRIM(filedat) == "-h" .OR. filegrb == "" .OR. &
    igen == 0 .OR. ios /= 0) THEN
  WRITE (*,*) "Uso: xout2grib.exe [-h] filedat filegrb igen"
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.2) Apro filedat e leggo header

OPEN (UNIT=20, FILE=filedat, FORM='UNFORMATTED', STATUS='OLD', ERR=9999)

! Lettura header record 1: run title
READ (20) title

! Lettura header record 2: run control parameters
READ (20) ver,level,ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmznm

READ (ver,*) rver ! numero della versione (reale)

x1 = xorigr + dgrid/2.
y1 = yorigr + dgrid/2.
x2 = x1 + (nx-1) * dgrid
y2 = y1 + (ny-1) * dgrid

WRITE (*,'(2a)') "xout2grib.exe: analisi file input ",TRIM(filedat)
WRITE (*,'(a,f4.1,a,i3)') "  versione ",rver," scadenze ",irlg
WRITE (*,'(a,3i4,3x,f8.3)') "  nx,ny,nz,dgrid: ",nx,ny,nz,dgrid/1000.
WRITE (*,'(a,4(2x,f8.3))') "  xmin,ymin,xmax,ymax: ", &
  x1/1000.,y2/1000.,x2/1000.,y2/1000.

!---------------------------------------------------------------------
! 1.3) Apro filegrib e assegno gli elementi costanti degli header

CALL GRSVCK(0)
CALL PBOPEN (iuout,filegrb,'W',kret)

! sezione 1
ksec1(1) = 200
ksec1(2) = 200
ksec1(3) = igen
ksec1(4) = 255
ksec1(5) = 192
ksec1(7) = 1
ksec1(8) = 0
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

!---------------------------------------------------------------------
! 2.1) Lettura data records da filedat
  IF (irtype == 1) THEN 
    READ (20) clab(1),ndathr,((swb(i,j),i=1,nx),j=1,ny)
    READ (20) clab(2),ndathr,((lwb(i,j),i=1,nx),j=1,ny)
    READ (20) clab(3),ndathr,((lhf(i,j),i=1,nx),j=1,ny)
    READ (20) clab(4),ndathr,((shf(i,j),i=1,nx),j=1,ny)
    READ (20) clab(5),ndathr,((cfr(i,j),i=1,nx),j=1,ny)
  ENDIF
                   
!---------------------------------------------------------------------
! 2.2) Calcolo la data corrispondente alla scadenza corrente

  iaa=int(ndathr/100000)   
  ijj=int((ndathr-iaa*100000)/100)
  ora_out(ih) = ndathr-iaa*100000-ijj*100

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

  WRITE (*,'(a,i2,a,i4.4,3(1x,i2.2),a)') "  Scrivo scadenza ",ih, &
    " (",data_out(ih)%yy,data_out(ih)%mm,data_out(ih)%dd,ora_out(ih)," GMT)"

  ksec1(10) = 1 + MOD(data_out(ih)%yy-1,100)
  ksec1(11) = data_out(ih)%mm
  ksec1(12) = data_out(ih)%dd
  ksec1(13) = ora_out(ih)
  ksec1(14) = 0
  ksec1(21) = 1 + (data_out(ih)%yy-1)/100

!---------------------------------------------------------------------
! 2.3) Scrittura GRIB

! SWB
  ksec1(6) = 110
  field(1:nx*ny) = RESHAPE(swb(1:nx,1:ny),(/nx*ny/))
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntg = cntg + 1

! LWB
  ksec1(6) = 111
  field(1:nx*ny) = RESHAPE(lwb(1:nx,1:ny),(/nx*ny/))
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntg = cntg + 1

! LHF
  ksec1(6) = 112
  field(1:nx*ny) = RESHAPE(lhf(1:nx,1:ny),(/nx*ny/))
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntg = cntg + 1

! SHF
  ksec1(6) = 113
  field(1:nx*ny) = RESHAPE(shf(1:nx,1:ny),(/nx*ny/))
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  cntg = cntg + 1

! CFR
  ksec1(6) = 114
  field(1:nx*ny) = RESHAPE(cfr(1:nx,1:ny),(/nx*ny/))
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
WRITE (*,*) "  xout2grib.exe: scritti ",cntg," grib"

STOP

!---------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filedat)
STOP

END PROGRAM xout2grib

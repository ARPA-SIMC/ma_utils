PROGRAM rw_dat_calmet
!--------------------------------------------------------------------------
! Programma per leggere l'output di calmet (calmet.dat) e riscriverlo
! cambiando le date. Gestisce le versioni di Calmet 5.0 e 5.2
!
!                                         Versione 1.1.1, Enrico 13/01/2014
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
  
! 0.5) Altre variabili del programma
TYPE (date) :: data_in,data_out,data_in1,data_out1,data_req
INTEGER, PARAMETER :: iuin=30, iuout=31
INTEGER :: hh_in,hh_in1,hh_out,hh_out1,hh_req,hht,ndathr_out
INTEGER :: ios,idum,iyy,ijj,ih,j,i,k
REAL :: rver
CHARACTER (LEN=200) :: filein,fileout,charg

!=====================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,charg)
READ (charg,'(i4,3i2)',IOSTAT=ios) &
  data_out1%yy,data_out1%mm,data_out1%dd,hh_out1

IF (filein == "-h" .OR. filein == "" .OR. fileout == "" .OR. ios /= 0) THEN
  WRITE (*,*) "Uso: rw_xout_calmet.exe filein fileout data1_new (yyyymmddhh)"
  WRITE (*,*) "Legge un file calmet_xout.dat e lo riscrive attribuendo al primo istante la data data1_new"
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.2 Apro files
OPEN (UNIT=iuin,FILE=filein,FORM='UNFORMATTED',STATUS='OLD',ERR=9999)
OPEN (UNIT=iuout,FILE=fileout,FORM='UNFORMATTED',STATUS='REPLACE')

!--------------------------------------------------------------------------
! 1.3 Leggo headers

! Lettura header record 1: run title
READ (iuin) title

! Lettura header record 2: run control parameters
READ (iuin) ver,level,ibyr,ibmo,ibdy,ibhr,ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmznm

READ (ver,*) rver ! numero della versione (reale)

WRITE (*,'(a,f4.1,a,i4)') "Versione calmet ",rver," numero istanti ",irlg

!--------------------------------------------------------------------------
! 1.4 Scrivo headers

! Header record 1: run title
WRITE (iuout) title

! Header record 2: run control parameters
WRITE (iuout) ver,level,data_out1%yy,data_out1%mm,data_out1%dd,ibhr, &
  ibtz,irlg,irtype, &
  nx,ny,nz,dgrid,xorigr,yorigr,iutmznm

WRITE (*,'(2(a,i4.4,3i2.2))') &
  "Header: data originale ",ibyr,ibmo,ibdy,ibhr, &
  " data nuova ",data_out1%yy,data_out1%mm,data_out1%dd,ibhr

!=====================================================================
! 2) Leggo e scrivo data records

DO ih=1,irlg

!---------------------------------------------------------------------
! 2.1) Leggo i dati

  IF (irtype == 1) THEN 
    READ (iuin) clab(1),ndathr,((swb(i,j),i=1,nx),j=1,ny)
    READ (iuin) clab(2),ndathr,((lwb(i,j),i=1,nx),j=1,ny)
    READ (iuin) clab(3),ndathr,((lhf(i,j),i=1,nx),j=1,ny)
    READ (iuin) clab(4),ndathr,((shf(i,j),i=1,nx),j=1,ny)
    READ (iuin) clab(5),ndathr,((cfr(i,j),i=1,nx),j=1,ny)
  ENDIF
                   
!--------------------------------------------------------------------------
! 2.2) Controlli, calcolo nuova data

! Parsing data corrente
  iyy=int(ndathr/100000)   
  IF (rver < 5.2 .AND. iyy <= 50) THEN    ! versioni < 5.2: anno in 2 cifre
    iyy = 2000 + iyy
  ELSE IF (rver < 5.2 .AND. iyy <= 50) THEN
    iyy = 1900 + iyy
  ENDIF  
  ijj=int((ndathr-iyy*100000)/100)
  hh_in = ndathr-iyy*100000-ijj*100
  data_in = greg(ijj,iyy)

! Controlli
  IF (ih == 1) THEN
    data_in1 = data_in
    hh_in1 = hh_in
  ELSE
    hht = hh_in1+ih-1
    hh_req = MOD(hht,24)
    data_req = data_in1 + hht/24
    IF (data_req /= data_in .OR. hh_req /= hh_in) GOTO 9998
  ENDIF

! Calcolo nuova data
  hht = hh_out1+ih-1
  hh_out = MOD(hht,24)
  data_out = data_out1 + hht/24
  IF (rver < 5.2) THEN  
    ndathr_out = MOD(data_out%yy,100)*100000 + jul(data_out)*100 + hh_out
  ELSE
    ndathr_out = data_out%yy*100000 + jul(data_out)*100 + hh_out
  ENDIF

  WRITE (*,'(a,i4,2(a,i4.4,3i2.2))') "Istante ",ih, &
    " data originale ",data_in%yy,data_in%mm,data_in%dd,hh_in, &
    " data nuova ",data_out%yy,data_out%mm,data_out%dd,hh_out

!--------------------------------------------------------------------------
! 2.3) Scrivo i dati

  IF (irtype == 1) THEN 
    WRITE (iuout) clab(1),ndathr_out,((swb(i,j),i=1,nx),j=1,ny)
    WRITE (iuout) clab(2),ndathr_out,((lwb(i,j),i=1,nx),j=1,ny)
    WRITE (iuout) clab(3),ndathr_out,((lhf(i,j),i=1,nx),j=1,ny)
    WRITE (iuout) clab(4),ndathr_out,((shf(i,j),i=1,nx),j=1,ny)
    WRITE (iuout) clab(5),ndathr_out,((cfr(i,j),i=1,nx),j=1,ny)
  ENDIF

ENDDO
STOP

!---------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,'(a,2(a,i4.4,3i2.2))') "Data disallineata in input; ", &
  "attesa: ",data_req%yy,data_req%mm,data_req%dd,hh_req, &
  "trovata :",data_in%yy,data_in%mm,data_in%dd,hh_in
STOP


END PROGRAM rw_dat_calmet

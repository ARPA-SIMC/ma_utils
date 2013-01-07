PROGRAM correct_lamaz
!--------------------------------------------------------------------------
! Legge un giorno di grib dell'archivio LAMAZ "old style" e lo sistema
! Scrive sui files gbex_fop.grb e gbex_sop.grb
! Uso: correct_lamaz.exe [-h] filein
!
!                                         Versione 2.0.0, Enrico 20/08/2012
!--------------------------------------------------------------------------

USE file_utilities
USE datetime_class
IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: maxdim = 1000000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
TYPE(datetime) :: dt_lay40_first,dt_bad_first,dt_bad_last,datahc
TYPE (csv_record) :: out_rec
INTEGER :: iuin,iuout1,iuout2,cnt,cnt_sop,cnt_u,cnt_v,cnt_bad,cnt_12h,yy,mm,dd,hh
CHARACTER (LEN=80) :: filein,fileout1,fileout2
CHARACTER (LEN=10) :: ch10
LOGICAL :: lsop

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)

IF (filein == "" .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: correct_lamaz.exe [-h] filein"
  STOP
ENDIF

fileout1 = "to_fop.grb"
fileout2 = "to_sop.grb"

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
CALL PBOPEN (iuout1,fileout1,'W',kret)
CALL PBOPEN (iuout2,fileout2,'W',kret)

! Date rilevanti del dataset LAMA
dt_lay40_first = datetime_new(year=2006, month=1, day=26, hour=1)
dt_bad_first = datetime_new(year=2009, month=3, day=25, hour=1)
dt_bad_last = datetime_new(year=2009, month=3, day=30, hour=0)

!--------------------------------------------------------------------------
! 2) Leggo / Scrivo (ciclo sui grib)

cnt_sop = 0
cnt_u = 0
cnt_v = 0
cnt_bad = 0
cnt_12h = 0

DO cnt = 1,HUGE(cnt)

! 2.1 Leggo il grib 
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

  yy = ksec1(10) + 100 * (ksec1(21) - 1)
  mm = ksec1(11)
  dd = ksec1(12)
  hh = ksec1(13)
  datahc = datetime_new(year=yy, month=mm, day=dd, hour=hh)

! 2.2 Lo modifico

! 2.2.1 Se sono in una data con dati corrotti, passo oltre
  IF (datahc >= dt_bad_first .AND. datahc <= dt_bad_last) THEN
    cnt_bad = cnt_bad + 1
    CYCLE
  ENDIF

! 2.2.2 Se e' una cumulata in 12 ore, passo oltre
  IF (ALL(ksec1(15:18) == (/1,0,12,13/))) THEN
    cnt_12h = cnt_12h + 1
    CYCLE
  ENDIF

! 2.2.3 Se e' scritto col second order packing, lo riporto a first order
  IF (ksec4(4) == 64) THEN
    ksec4(2) = 16
    ksec4(3:) = 0
    lsop = .TRUE.
    cnt_sop = cnt_sop + 1
  ELSE
    lsop = .FALSE.
  ENDIF

! 2.2.4 Se e' vento sui model layers, riporto su griglia U/V
  IF (ksec1(1) == 2 .AND. ksec1(6) == 33 .AND. ksec1(7) == 110) THEN
    ksec2(5) = -2906    ! Xmin
    ksec2(8) = 7594     ! Xmax
    cnt_u = cnt_u + 1
  ELSE IF (ksec1(1) == 2 .AND. ksec1(6) == 34 .AND. ksec1(7) == 110) THEN
    ksec2(4) = -21094   ! Ymin
    ksec2(7) = -9844    ! Ymax
    cnt_v = cnt_v + 1
  ENDIF

! 2.2.5 metto igen=30 per le date con 35 livelli, 32 per quelle con 40 
 IF (datahc < dt_lay40_first) THEN
    ksec1(3) = 30
  ELSE
    ksec1(3) = 32
  ENDIF

! 2.3 Scrivo
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  
  IF (lsop) THEN
    CALL PBWRITE (iuout2,kbuffer,ksec0(1),kret)
  ELSE
    CALL PBWRITE (iuout1,kbuffer,ksec0(1),kret)
  ENDIF
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret


ENDDO

!--------------------------------------------------------------------------
! 3) Chiudo i files e termino

CALL PBCLOSE (iuin,kret)
CALL PBCLOSE (iuout1,kret)
CALL PBCLOSE (iuout2,kret)

WRITE (*,'(a)') "Data,grib totali,sop,u,v,bad,c12h"
CALL init(out_rec)
CALL getval(datahc,SIMPLEDATE=ch10)
CALL csv_record_addfield(out_rec,ch10)
CALL csv_record_addfield(out_rec,cnt-1)
CALL csv_record_addfield(out_rec,cnt_sop)
CALL csv_record_addfield(out_rec,cnt_u)
CALL csv_record_addfield(out_rec,cnt_v)
CALL csv_record_addfield(out_rec,cnt_bad)
CALL csv_record_addfield(out_rec,cnt_12h)
WRITE (*,'(a)') csv_record_getrecord(out_rec)
CALL delete(out_rec)

STOP

END PROGRAM correct_lamaz

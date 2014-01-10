PROGRAM stat_qrs_temp
!--------------------------------------------------------------------------
! Programma che legge da files separati i grib di T, Qw, Qi, Qr, Qs e 
! calcola il rapporto tra acqua liquida e ghiaccio alle varie temperature.
!
!                                         Versione 1.0.1, Enrico 10/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1_sav(1024),ksec2_sav(1024)

INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2(512),psec3(2)
REAL :: field(maxdim)

! Altre variabili del programma
INTEGER, PARAMETER :: tt1=253, tt2=283, ttstep=1, ntt=(tt2-tt1)/ttstep+1
REAL, ALLOCATABLE :: dati(:,:)
REAL :: sum1(ntt),sum2(ntt),sum3(ntt),thr(5)
INTEGER :: np,kf,kg,kt,tc1,tc2
INTEGER (KIND=8) :: nok1(ntt),nok2(ntt),nok3(ntt)
INTEGER :: iuin(5)
CHARACTER (LEN=80), PARAMETER :: filein(5) = &
  (/"tt.grb","qw.grb","qi.grb","qr.grb","qs.grb"/)
CHARACTER (LEN=80) :: charg
LOGICAL, ALLOCATABLE :: ttmask(:)
LOGICAL :: ksec2_diff

!--------------------------------------------------------------------------
! 1) Preliminari

CALL getarg(1,charg)
IF (TRIM(charg) /= "") THEN
  WRITE (*,*) "Uso: si lancia senza parametri"
  WRITE (*,*) "Richiede la presenza dei files: tt.grb, qw.grb, qi.grb, qr.grb, qs.grb"
  STOP
ENDIF

! 1.1 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.2 Apro i files
DO kf = 1,5
  CALL PBOPEN (iuin(kf),filein(kf),'R',kret)
  IF (kret /= 0) THEN 
    WRITE(*,*) "Errore aprendo ",filein(kf)," kret ",kret
    STOP
  ENDIF
ENDDO

OPEN (UNIT=30, FILE="stat_qrs_temp.log", STATUS="REPLACE", ACTION="WRITE")
!                 123---123---12345678-123456---12345678-123456---12345678-123456
WRITE (30,'(a)') "T >   T<=        Qw/(Qw+Qi)        Qr/(Qr+Qs)      (Qw+Qr)/Qtot"

!--------------------------------------------------------------------------
! 2) Lettura (ciclo sui grib)

sum1(:) = 0.
sum2(:) = 0.
sum3(:) = 0.
nok1(:) = 0
nok2(:) = 0
nok3(:) = 0

grib: DO kg = 1, HUGE(0)
!grib: DO kg = 1, 400

! ciclo sui parametri (files)
  DO kf = 1,5

!   Leggo e decodifico
    CALL PBGRIB(iuin(kf),kbuffer,maxdim*4,klen,kret)
    IF (kf == 1 .AND. kret == -1) THEN 
      EXIT grib
    ELSE IF (kret == -1) THEN
      WRITE (*,*) "Il file ",TRIM(filein(kf))," contiene meno dati!"
      EXIT grib
    ELSE IF (kret < -1) THEN
      WRITE(*,*) "Error pbgrib: kret ",kret
      STOP
    ENDIF

    psec3(2) = rmis                                   ! dati mancanti = rmis
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    if (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

!   Controlli sull'area
    IF (kf == 1 .AND. kg == 1) THEN
      ksec2_sav(:) = ksec2(:)
      np = ksec4(1)
      ALLOCATE (dati(np,5),ttmask(np))
    ELSE 
      IF (ksec2_diff(ksec2(1:14),ksec2_sav(1:14))) GOTO 9999
    ENDIF

!   Controlli su data, scadenza, livello
    IF (kf == 1) THEN
      ksec1_sav(:) = ksec1(:)
    ELSE 
      IF (ANY(ksec1(7:18) /= ksec1_sav(7:18))) GOTO 9998
    ENDIF
 
!   Salvo i valori
    dati(1:np,kf) = field(1:np)

  ENDDO

! Incremento i contatori statistici
  thr(1:5) =  MAXVAL(dati(1:np,1:5),DIM=1) / 1000.
  WRITE (95,*) thr(2:5)

  DO kt = 1,ntt
    tc1 = tt1 + (kt-1)*ttstep
    tc2 = tt1 + kt*ttstep
    ttmask =  dati(:,1) > REAL(tc1) .AND. dati(:,1) <= REAL(tc2)

!   qw / (qi+qw)
    sum1(kt) = sum1(kt) + SUM( dati(:,2)/(dati(:,2)+dati(:,3)), &
      MASK = (dati(:,2) > thr(2) .OR. dati(:,3) > thr(3)) .AND. ttmask(:))
    nok1(kt) = nok1(kt) + &
      COUNT((dati(:,2) > thr(2) .OR. dati(:,3) > thr(3)) .AND. ttmask(:))
    
!   qr / (qs+qr)
    sum2(kt) = sum2(kt) + SUM( dati(:,4)/(dati(:,4)+dati(:,5)), &
      MASK = (dati(:,4) > thr(4) .OR. dati(:,5) > thr(5)) .AND. ttmask(:))
    nok2(kt) = nok2(kt) + &
      COUNT((dati(:,4) > thr(4) .OR. dati(:,5) > thr(5)) .AND. ttmask(:))
    
!   (qw+qr) / (qi+qw+qs+qr)
    sum3(kt) = sum3(kt) + SUM( (dati(:,2)+dati(:,4)) / &
      (dati(:,2)+dati(:,3)+dati(:,4)+dati(:,5)), &
      MASK = (dati(:,2) > thr(2) .OR. dati(:,3) > thr(3) .OR. &
      dati(:,4) > thr(4) .OR. dati(:,5) > thr(5)) .AND. ttmask(:))
    nok3(kt) = nok3(kt) + &
      COUNT((dati(:,2) > thr(2) .OR. dati(:,3) > thr(3) .OR. &
      dati(:,4) > thr(4) .OR. dati(:,5) > thr(5)) .AND. ttmask(:))
  ENDDO

  IF (MOD(kg,10) == 0) WRITE (*,*) "Elaborati campi ",kg
ENDDO grib

WRITE (*,*) "Elaborazioni completate, campi elaborati ",kg-1

sum1(:) = 100. * sum1(:) / REAL(nok1(:))
sum2(:) = 100. * sum2(:) / REAL(nok2(:))
sum3(:) = 100. * sum3(:) / REAL(nok3(:))

DO kt = 1,ntt
  tc1 = tt1 + (kt-1)*ttstep
  tc2 = tt1 + kt*ttstep
  WRITE (30,'(i3,3x,i3,3x,3(i8,1x,f6.2,3x))') &
    tc1,tc2,nok1(kt),sum1(kt),nok2(kt),sum2(kt),nok3(kt),sum3(kt)
  IF (MOD(kt,10) == 0) WRITE (30,*)
ENDDO
WRITE (30,*)
WRITE (30,'(i3,3x,i3,3x,3(i8,1x,f6.2,3x))') &
  tt1,tt2,SUM(nok1(:)),0.,SUM(nok2(:)),0.,SUM(nok3(:)),0.


STOP

9999 CONTINUE
WRITE (*,*) "Trovata area diversa: file ",TRIM(filein(kf))," campo ",kg
STOP

9998 CONTINUE
WRITE (*,*) "Trovata data/scad/liv diversa: file ",TRIM(filein(kf))," campo ",kg
STOP

END PROGRAM stat_qrs_temp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION ksec2_diff(ksec2a,ksec2b) RESULT(is_diff)
!
! Controlla se due array ksec2 scritti da Gribex corrispondono alla stessa 
! griglia
!
IMPLICIT NONE
LOGICAL :: is_diff
INTEGER, INTENT(IN) :: ksec2a(14),ksec2b(14)

IF (ANY(ksec2a((/1,2,3,4,5,6,7,8,11/)) /= ksec2b((/1,2,3,4,5,6,7,8,11/)))) THEN
  is_diff = .TRUE.

ELSE IF (ksec2a(6) == 128 .AND. &
  (ksec2a(9) /= ksec2b(9) .OR. ksec2a(10) /= ksec2b(10))) THEN
  is_diff = .TRUE.

ELSE IF (ksec2a(1) == 10 .AND. &
  (ksec2a(13) /= ksec2b(13) .OR. ksec2a(14) /= ksec2b(14))) THEN
  is_diff = .TRUE.

ELSE 
  is_diff = .FALSE.

ENDIF

END FUNCTION ksec2_diff


PROGRAM test_1grib
!--------------------------------------------------------------------------
! Legge il primo grib di un file e ritorna alcune informazioni (per ora
! edition number e proiezione). Gestisce GRIB1 e GRIB2.
! Usato da ak_getgrib.ksh per stabilire l'estensione del file in output e
! da ak_seriet.ksh per individuare i files Grib1-UTM
!
!                                             Versione 2, Enrico 12/04/2011
!--------------------------------------------------------------------------
USE grib_api
IMPLICIT NONE

REAL :: yi,yf
INTEGER :: kpar,if,ig,iret,en
CHARACTER (LEN=80) :: filein,chpar,gt
CHARACTER (LEN=3) :: proj

!--------------------------------------------------------------------------
! Parametri da riga comando

filein = ""
DO kpar = 1,HUGE(kpar)
  CALL getarg(kpar,chpar)
  IF (TRIM(chpar) == "")THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    WRITE (*,*) "Uso: test_1grib.exe filein"
    STOP 1
  ELSE
    filein = TRIM(chpar)
  ENDIF
ENDDO

IF (TRIM(filein) == "") THEN
  WRITE (*,*) "Uso: test_1grib.exe filein"
  STOP 1
ENDIF

!--------------------------------------------------------------------------
! Esecuzione

ig = 0
CALL grib_open_file(if,filein,"r",iret)
CALL grib_new_from_file(if,ig,iret)
CALL grib_get(ig,"editionNumber",en)
CALL grib_get(ig,"gridType",gt)

IF (gt == "UTM") THEN
  proj = "UTM"

ELSE IF (gt == "rotated_ll") THEN
  proj = "ROT"

ELSE IF (gt == "regular_ll") THEN
  IF (en == 1) THEN
    CALL grib_get(ig,"latitudeOfFirstGridPointInDegrees",yi)
    CALL grib_get(ig,"latitudeOfLastGridPointInDegrees",yf)
    IF (yi > 90. .OR. yf > 90.) THEN
      proj = "UTM"
    ELSE
      proj = "GEO"
    ENDIF
  ELSE IF (en == 2) THEN
    proj = "GEO"
  ENDIF

ELSE
  proj = "XXX"

ENDIF

WRITE (*,'(i1,a1,a3)') en,",",proj
END PROGRAM test_1grib

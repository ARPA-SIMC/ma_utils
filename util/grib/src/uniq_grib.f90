PROGRAM uniq_grib
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive saltando i grib consecutivi
! identici. Richiede che tutti i campi abbiano lo stesso numero di punti
! (questa condizione potrebbe essere eliminata...)
!
!                                          Versione 1.0.0 Enrico 21/11/2011
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

INTEGER :: ifin,ifout,igin,igout,iret,kg
CHARACTER(LEN=80) :: filein,fileout

REAL, ALLOCATABLE :: field(:),field_sav(:)
INTEGER :: ni,nj,cnt_skip,ni_sav,nj_sav

!--------------------------------------------------------------------------
! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help") THEN
  WRITE (*,*) "Uso: uniq_grib.exe filein fileout"
  STOP
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

! Ciclo sui grib
cnt_skip = 0
DO kg = 1,HUGE(0)

! Leggo il prossimo campo
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

  CALL grib_get(igin,"numberOfPointsAlongAParallel",ni)
  CALL grib_get(igin,"numberOfPointsAlongAMeridian",nj)
  IF (kg == 1) THEN
    ALLOCATE (field(ni*nj),field_sav(ni*nj))
    ni_sav = ni
    nj_sav = nj
  ELSE
    IF (ni /= ni_sav .OR. nj /= nj_sav) GOTO 9997
  ENDIF

  CALL grib_get(igin,"values",field)

! Guardo se e' uguale al precedente
  IF (kg == 1) THEN
    CALL grib_write (igin,ifout)

  ELSE
    IF (ALL(field(:) == field_sav(:))) THEN
      cnt_skip = cnt_skip + 1
    ELSE
      CALL grib_write (igin,ifout)
    ENDIF

  ENDIF

  field_sav(:) = field(:)

! Lo riscrivo

ENDDO

WRITE (*,*) "Elaborazioni completate, campi letti ",kg," skipped ",cnt_skip
STOP

!--------------------------------------------------------------------------
! Gestion errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

9997 CONTINUE
WRITE (*,*) "Numero di punti diversi in ",TRIM(filein)," grib n.ro " ,kg

END PROGRAM uniq_grib

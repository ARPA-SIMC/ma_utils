PROGRAM grib_skip_first
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive saltando i primi N campi
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

INTEGER :: ifin,ifout,igin,igout,iret,kg,ios
CHARACTER(LEN=80) :: filein,fileout,chpar

REAL, ALLOCATABLE :: field(:)
INTEGER :: ni,nj,bpv,nskip

!--------------------------------------------------------------------------
! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,chpar)
READ (chpar,*,IOSTAT=ios) nskip
IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help" .OR. &
  ios /= 0) THEN
  WRITE (*,*) "Uso: grib_skip_first.exe filein fileout nskip"
  STOP
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

! Ciclo sui grib
DO kg = 1,HUGE(0)

! Leggo il prossimo campo
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

! Lo riscrivo
  IF (kg > nskip) CALL grib_write (igin,ifout)

ENDDO

WRITE (*,*) "Elaborazioni completate, letti ",kg-1," campi"
STOP

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

END PROGRAM grib_skip_first

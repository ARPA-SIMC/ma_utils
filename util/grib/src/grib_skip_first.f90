PROGRAM grib_skip_first
!--------------------------------------------------------------------------
! Legge un file con molti grib, riscrive i prini o gli ultimi campi
!
!                                         Versione 2.0.1, Enrico 02/01/2015
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

INTEGER :: ifin,ifout,igin=0,iret,kg,ios
CHARACTER(LEN=200) :: filein,fileout,chpar

REAL, ALLOCATABLE :: field(:)
INTEGER :: ni,nj,bpv,nskip,cnt_out

!--------------------------------------------------------------------------
! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,chpar)
READ (chpar,*,IOSTAT=ios) nskip
IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help" .OR. &
  ios /= 0) THEN
  WRITE (*,*) "Uso: grib_skip_first.exe filein fileout NSKIP"
  WRITE (*,*) "NSKIP > 0: salta i primi NSKIP campi"
  WRITE (*,*) "NSKIP < 0: scrive solo i primi -NSKIP campi"
  STOP
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

! Ciclo sui grib
cnt_out = 0
DO kg = 1,HUGE(0)

! Leggo il prossimo campo
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

! Se richiesto, lo riscrivo
  IF (nskip > 0) THEN
    IF (kg > nskip) THEN
      CALL grib_write (igin,ifout)
      cnt_out = cnt_out + 1
    ELSE
      CYCLE
    ENDIF

  ELSE IF (nskip < 0) THEN
    IF (kg <= -nskip) THEN
      CALL grib_write (igin,ifout)
      cnt_out = cnt_out + 1
    ELSE
      CALL grib_release(igin)
      EXIT
    ENDIF

  ENDIF

! Libero memoria
  CALL grib_release(igin)
ENDDO

! Coclusione
IF (nskip > 0) THEN 
  WRITE (*,*) "Elaborazioni completate, campi letti ",kg-1," scritti ",cnt_out
ELSE
  IF (cnt_out /= kg-1) WRITE (*,*) "Codizione inattesa, chiamare l'assistenza!"
  WRITE (*,*) "Elaborazioni completate, letti e scritti ",cnt_out," campi"
ENDIF
STOP

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

END PROGRAM grib_skip_first

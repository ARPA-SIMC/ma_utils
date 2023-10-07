PROGRAM maskout_grib
!--------------------------------------------------------------------------
! Legge un file con molti grib, li riscrive mettento a "dato mancante" i
! valori richiesti
!
!                                         Versione 1.0.0, Enrico 08/03/2023
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
IMPLICIT NONE

INTEGER :: ifin=0,ifout=0,ifmask=0,igin=0,igout=0,igmask=0,iret,kg
CHARACTER(LEN=200) :: filein,fileout,filemask,chdum
REAL, ALLOCATABLE :: field(:),mask(:)

REAL :: thrs
INTEGER :: idp,kp,ios
INTEGER :: ni,nj,bpv,np,np_mask,nom_in,nom_mask,nom_out,nocv
CHARACTER(LEN=2) :: cond
CHARACTER(LEN=2) :: next_par
LOGICAL :: lmask

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
idp = 0
next_par = ""
lmask = .FALSE.
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (next_par == "m") THEN
    filemask = chdum
    next_par = ""
  ELSE IF (TRIM(chdum) == "-m") THEN
    lmask = .TRUE.
    next_par = "m"
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      READ (chdum,'(2a)') cond
    CASE (3)
      READ (chdum,*,IOSTAT=ios) thrs
    CASE (4)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

! Check
IF (idp /= 4) THEN
  CALL write_help
  STOP 2
ELSE IF (ios /= 0) THEN
  CALL write_help
  STOP 3
ELSE IF (cond /= "eq" .AND. cond /= "ne" .AND. cond /= "gt" .AND. &
         cond /= "ge" .AND. cond /= "lt" .AND. cond /= "le") THEN
  WRITE (*,*) cond
  CALL write_help
  STOP 4
ENDIF

! Apro i filein e fileout
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

! SE richiesto, leggo la maschera
IF (lmask) THEN
  CALL grib_open_file(ifmask,filemask,"r",iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9997
  CALL grib_new_from_file(ifmask,igmask,iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9996

  CALL grib_get(igmask,"getNumberOfValues",np_mask) ! totale di punti nel grib
  CALL grib_get(igmask,"numberOfMissing",nom_mask)    ! n.ro dati mancanti mask
  ALLOCATE (mask(np_mask))
  IF (nom_mask > 0) THEN
    CALL grib_set(igmask,"missingValue",rmiss)
  ENDIF
  CALL grib_get(igmask,"values",mask)
  WRITE (*,*) "Letta maschera ",TRIM(filemask),": np, missing ",np_mask,nom_mask
  write (90,*) mask(1:np_mask)
ENDIF
  
!--------------------------------------------------------------------------
! 2) Esecuzione (ciclo sui grib)

DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

  CALL grib_get(igin,"getNumberOfValues",np)    ! totale di punti nel grib
  IF (lmask .AND. np /= np_mask) GOTO 9995
  
  CALL grib_get(igin,"numberOfMissing",nom_in)    ! n.ro dati mancanti input
  CALL grib_get(igin,"numberOfCodedValues",nocv)  ! n.ro dati validi input
  ALLOCATE (field(np))
  IF (.NOT. lmask) ALLOCATE (field(np))
  IF (nom_in > 0) THEN
    CALL grib_set(igin,"missingValue",rmiss)
  ENDIF
  CALL grib_get(igin,"values",field)

! 2.2 Lo modifico
  CALL grib_clone(igin,igout)
  IF (.NOT. lmask) mask(:) = field(:)

  IF (cond == "eq" ) THEN
    WHERE (mask(:) == thrs) field(:) = rmiss
  ELSE IF (cond == "ne" ) THEN
    WHERE (mask(:) /= thrs) field(:) = rmiss
  ELSE IF (cond == "gt" ) THEN
    WHERE (mask(:) > thrs) field(:) = rmiss
  ELSE IF (cond == "ge" ) THEN
    WHERE (mask(:) >= thrs) field(:) = rmiss
  ELSE IF (cond == "lt" ) THEN
    WHERE (mask(:) <= thrs) field(:) = rmiss
  ELSE IF (cond == "le" ) THEN
    WHERE (mask(:) < thrs) field(:) = rmiss
  ENDIF
    
! bitmapPresent: 0=NO, 1=SI; 
! in alcuni grib anche (?) bitMapIndicator: 0=c'e, 255=non c'e'
  nom_out = COUNT(field(:) == rmiss)
  IF (nom_out > 0) THEN
    CALL grib_set(igout,"missingValue",rmiss)
    CALL grib_set(igout,"bitmapPresent",1)
  ENDIF

  CALL grib_set(igout,"values",field)

! 2.3 Lo riscrivo
  WRITE (*,*) "Write grib: np, missing in, missing out ",np,nom_in,nom_out
  CALL grib_write (igout,ifout)
  CALL grib_release(igin)
  CALL grib_release(igout)
  DEALLOCATE (field)
  IF (.NOT. lmask) DEALLOCATE (mask)
  
ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "Elaborazioni completate, elaborati ",kg-1," campi"

CALL grib_close_file(ifin)
CALL grib_close_file(ifout)
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 10

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP 11

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filemask)
STOP 12

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filemask)
STOP 13

9995 CONTINUE
WRITE (*,*) "Numero di punti in filein ",np," in filemask ",np_mask," campo ",kg
stop 20

END PROGRAM maskout_grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: maskout_grib.exe [-h] [-m filemask] filein condition threshold fileout"
WRITE (*,*) "Set to ""missing"" the values of filein that satisfy ""condition threshold"" "
WRITE (*,*) "With the option -m, condition is evaluated on filemask, otherwise on filein "
WRITE (*,*) "  filemask: single field, with the same number of points of filein"
WRITE (*,*) "  filein: grib1 or grib2"
WRITE (*,*) "  condition: eq, ne, gt, ge, lt, le"
WRITE (*,*) "  threshold: value to be applied to condition"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

PROGRAM grib_fill_missing
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive, modificando i valori mancanti
! Programma scritto originarimente per rimepire i valori mancanti di un
! output Icon non strutturato scritto con io_nml:lmask_boundary = .true.
!
!                                         Versione 2.0.0, Enrico 30/09/2025
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

REAL, PARAMETER :: rmiss = HUGE(1.0)
INTEGER, PARAMETER :: imiss = HUGE(0)

! Varibili locali
REAL, ALLOCATABLE :: field(:),field_out(:),field_mask(:)
REAL :: fill_value
INTEGER :: ifin,ifout,ifmask,igin,igout,igmask,iret,kg
INTEGER :: kp,idp,ni,nj,np,np_sav,nom,gdtn
CHARACTER(LEN=200) :: filein,fileout,filemask,chdum
CHARACTER (LEN=3) :: next_arg, mode
LOGICAL :: lmask

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comando
lmask = .FALSE.
mode = "avg"
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-v") THEN
    next_arg = "val"
  ELSE IF (TRIM(chdum) == "-m") THEN
    next_arg = "msk"
  ELSE IF (next_arg == "val") THEN
    READ (chdum,*) fill_value
    mode = "val"
    next_arg = ""
  ELSE IF (next_arg == "msk") THEN
    filemask = chdum
    lmask = .TRUE.
    next_arg = ""
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO
IF (idp /= 2) THEN
  CALL write_help
  STOP 1
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

! Se richiesto leggo la maschera dei valori da modificare
IF (lmask) THEN
  CALL grib_open_file(ifmask,filemask,"r",iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9997
  igmask = -1
  CALL grib_new_from_file(ifmask,igmask,iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9996
  
  CALL grib_get(igin,"gridDefinitionTemplateNumber",gdtn)
  IF (gdtn == 101) THEN
    CALL grib_get(igin,"numberOfDataPoints",np)
  ELSE
    CALL grib_get(igin,"numberOfPointsAlongAParallel",ni)
    CALL grib_get(igin,"numberOfPointsAlongAMeridian",nj)
    np = ni*nj
  ENDIF
  
  np_sav = np
  ALLOCATE (field_mask(np),field(np),field_out(np))
  CALL grib_set(igmask,"missingValue",rmiss)
  CALL grib_get(igmask,"values",field_mask(:))
ENDIF

!--------------------------------------------------------------------------
! 2) Ciclo sui grib

np_sav = imiss
DO kg = 1,HUGE(0)

! Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

  CALL grib_get(igin,"gridDefinitionTemplateNumber",gdtn)
  IF (gdtn == 101) THEN
    CALL grib_get(igin,"numberOfDataPoints",np)
  ELSE
    CALL grib_get(igin,"numberOfPointsAlongAParallel",ni)
    CALL grib_get(igin,"numberOfPointsAlongAMeridian",nj)
    np = ni*nj
  ENDIF

  IF (kg == 1 .AND. .NOT. lmask) THEN
    np_sav = np
    ALLOCATE (field(np),field_out(np)) 

  ELSE IF (np /= np_sav .AND. lmask) THEN
    GOTO 9995  
    
  ELSE IF (np /= np_sav .AND. .NOT. lmask) THEN
    DEALLOCATE(field,field_out)
    ALLOCATE (field(np),field_out(np)) 
    np_sav = np

  ENDIF

  CALL grib_clone(igin,igout)

  CALL grib_get(igin,"numberOfMissing",nom)
  WRITE (*,*) "Campo ",kg," missing ",nom
  IF (nom /= 0) THEN
    IF (mode == "avg") CALL grib_get(igin,"average",fill_value)

    CALL grib_set(igin,"missingValue",rmiss)
    CALL grib_get(igin,"values",field(:))
    IF (lmask) THEN
      WHERE (field_mask(:) /= rmiss)
        field_out(:) = field(:)
      ELSEWHERE
        field_out(:) = fill_value
      ENDWHERE
    ELSE
      WHERE (field(:) /= rmiss)
        field_out(:) = field(:)
      ELSEWHERE
        field_out(:) = fill_value
      ENDWHERE
    ENDIF

    CALL grib_set(igout,"values",field_out(:))  
  ENDIF

! Lo scrivo in output
  CALL grib_write(igout,ifout)
  CALL grib_release(igout)
  CALL grib_release(igin)

ENDDO
STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filemask)
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filemask)
STOP

9995 CONTINUE
WRITE (*,*) "Trovato campo con numero di punti diverso da filemask "
WRITE (*,*) " punti trovati, attesi ",np,np_sav," campo ",kg," file ",TRIM(filein)
STOP

END PROGRAM grib_fill_missing

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib_fill_missin.exe filein fileout [-v value] [-m filemask]"
WRITE (*,*) "Senza il parametro -v sostituisce ai dati mancanti la media del campo"
WRITE (*,*) "Con parametro -m: sostituisce i valori di filein che sono mancanti nel"
WRITE (*,*) "  primo campo di filemask"

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


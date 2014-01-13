PROGRAM grib_uniq
!--------------------------------------------------------------------------
! Programma che legge e riscrive un file con molti grib; quando trova due
! o piu' grib consecutivi che sono uguali in base ai criteri 
! specificati, scrive solo il primo.
!
!                                         Versione 2.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
USE file_utilities
USE grib2_utilities
IMPLICIT NONE

! Variabili locali
TYPE (csv_record) :: csvline
LOGICAL, PARAMETER :: lverbose = .FALSE.
INTEGER, PARAMETER :: max_keys = 100
INTEGER :: idp,kp,kk,ier,kg,cnt_out,clret(0:5),nkeys
INTEGER :: ifin,ifout,iret,igin=0,iglast=0,igout=0
CHARACTER (LEN=500) :: chdum,strcl_meta,strcl_keys
CHARACTER (LEN=200) :: filein,fileout
CHARACTER(LEN=80) :: cl_keys(max_keys),chkey_in,chkey_last
LOGICAL :: cl_grid,cl_time,cl_vtime,cl_lev,cl_var
LOGICAL :: lcheck_meta,lcheck_keys,lout

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Default
lcheck_meta = .FALSE.
lcheck_keys = .FALSE.
cl_grid  = .FALSE.
cl_time  = .FALSE.
cl_vtime = .FALSE.
cl_lev   = .FALSE.
cl_var   = .FALSE.

! 1.2 Parametri da riga comando
strcl_meta = ""
strcl_keys = ""

idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (chdum(1:7) == "-check=") THEN
    lcheck_meta = .TRUE.
    strcl_meta = chdum(8:)
  ELSE IF (chdum(1:6) == "-keys=") THEN
    lcheck_keys = .TRUE.
    strcl_keys = chdum(7:)
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    END SELECT
  ENDIF
ENDDO

! Controlli di default (se non specifico nessun meta-elemento ne' key grib-api)
IF (.NOT. lcheck_meta .AND. .NOT. lcheck_keys) THEN
  lcheck_meta = .TRUE.
  cl_grid  = .TRUE.
  cl_time  = .TRUE.
  cl_lev   = .TRUE.
  cl_var   = .TRUE.
ENDIF

! 1.3 Parsing dei parametri e controlli
IF (idp /= 2) THEN
  CALL write_help
  STOP 1
ENDIF




IF (lcheck_meta .AND. LEN(TRIM(strcl_meta)) > 0) THEN 
  CALL parse_check_list (strcl_meta,cl_grid,cl_time,cl_vtime,cl_lev,cl_var,ier)
  IF (ier /= 0) GOTO 9996
ENDIF

IF (lcheck_keys) THEN
  CALL init(csvline,RECORD=strcl_keys,NFIELD=nkeys)
  IF (nkeys == 0) GOTO 9995
  DO kk = 1,nkeys
    CALL csv_record_getfield(csvline,FIELD=cl_keys(kk),IER=ier)
    IF (ier /= 0) GOTO 9995
  ENDDO
  CALL delete(csvline)
ENDIF

! 1.4 Inizializzazioni
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w",iret)

!--------------------------------------------------------------------------
! 2) Ciclo sui grib

cnt_out = 0
DO kg = 1,HUGE(0)

! Leggo il prossimo GRIB
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

  IF (kg == 1) THEN
    lout = .TRUE.
  
  ELSE
    lout = .FALSE.

!   Verifico se tutti i meta-elementi sono uguali a quelli dell'ultimo grib
!   scritto
    IF (lcheck_meta) THEN
      CALL check_consistency(igin,iglast,cl_grid,cl_time,cl_vtime,cl_lev, &
        cl_var,lverbose,clret,ier)
      IF (ier == 1) THEN
        lout = .TRUE.
      ELSE IF (ier == 2) THEN
        GOTO 9997
      ENDIF
    ENDIF
    
!   Verifico se tutte le chiavi grib-api sono uguali a quelle dell'ultimo 
!   grib scritto
    IF (lcheck_keys) THEN
      DO kk = 1,nkeys
        CALL grib_get(igin,cl_keys(kk),chkey_in)
        CALL grib_get(iglast,cl_keys(kk),chkey_last)
        IF (chkey_in /= chkey_last) THEN
          lout = .TRUE.
          EXIT
        ENDIF
      ENDDO
    ENDIF

  ENDIF

! Se il GRIB appena letto e' "diverso" dall'ultimo scritto, lo scrivo
  IF (lout) THEN
    CALL grib_write (igin,ifout)
    cnt_out = cnt_out + 1
    iglast = igin
  ENDIF

ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione; log a schermo

WRITE (*,*) "Ealborazioni termintate: campi letti ",kg-1," scritti ",cnt_out
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*)  "File non trovato ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*)  "Errore leggendo ",TRIM(filein)," campo ",kg
STOP 2

9997 CONTINUE
WRITE (*,*)  "Errore nel controllo dei meta-elementi (subr. check_consistency)"
WRITE (*,*)  "File ",TRIM(filein)," campo ",kg
STOP 2

9996 CONTINUE
WRITE (*,*) "Errore nell'interpretazione della check_list"
CALL write_help
STOP 1

9995 CONTINUE
WRITE (*,*) "Errore nell'interpretazione della key_list, campo ",kk
CALL write_help
STOP 1

END PROGRAM grib_uniq

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE parse_check_list(check_list,cl_grid,cl_time,cl_vtime,cl_lev, &
  cl_var,ier)
!
! Interpreta la check-list da riga comando
!
IMPLICIT NONE

CHARACTER (LEN=500), INTENT(IN) :: check_list
LOGICAL, INTENT(OUT) :: cl_grid,cl_time,cl_vtime,cl_lev,cl_var
INTEGER, INTENT(OUT) :: ier

INTEGER :: p1,p2,pp
LOGICAL :: lexit

!--------------------------------------------------------------------------

cl_grid  = .FALSE.
cl_time  = .FALSE.
cl_vtime = .FALSE.
cl_lev   = .FALSE.
cl_var   = .FALSE.

ier = 0
p1 = 0
lexit = .FALSE.
DO
  pp = INDEX(check_list(p1+1:),",")
  IF (pp == 0) THEN
    p2 = LEN(TRIM(check_list(p1+1:))) + p1 + 1
    lexit = .TRUE.
  ELSE
    p2 = pp + p1
  ENDIF

  SELECT CASE (check_list(p1+1:p2-1))
  CASE("all")
    cl_grid  = .TRUE.
    cl_time  = .TRUE.
    cl_lev   = .TRUE.
    cl_var   = .TRUE.
  CASE("grid")
    cl_grid  = .TRUE.
  CASE("vtime")
    cl_vtime = .TRUE.
  CASE("time")
    cl_time  = .TRUE.
  CASE("lev")
    cl_lev   = .TRUE.
  CASE("var")
    cl_var   = .TRUE.
  CASE("nil")
    cl_grid  = .FALSE.
    cl_vtime = .FALSE.
    cl_time  = .FALSE.
    cl_lev   = .FALSE.
    cl_var   = .FALSE.
  CASE DEFAULT
    ier = 1
    RETURN
  END SELECT
  p1 = p2

  IF (lexit) EXIT
ENDDO

RETURN
END SUBROUTINE parse_check_list

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib_uniq.exe filein fileout [-h] [-check=check_list] [-keys=key_list]"
WRITE (*,*) ""
WRITE (*,*) "Legge e riscrive un file con molti grib; se trova 2 o piu' campi consecutivi" 
WRITE (*,*) "uguali scrive solo il primo."
WRITE (*,*) "I criteri per considerare ""uguali"" 2 campi possono essere specificati come"
WRITE (*,*) "lista di chiavi grib-api o di meta-elementi (variabile, reftime, etc.)"
WRITE (*,*) "Il programma considera uguali i grib che soddisfano tutti i criteri specificati"
WRITE (*,*) "(default: -check=all)"
WRITE (*,*) ""
WRITE (*,*) "filein      : file (grib1 o grib2) da leggere"
WRITE (*,*) "fileout     : file di output"
WRITE (*,*) "check_list  : definisce i meta-elementi in base ai quali due grib sono"
WRITE (*,*) "              considerati uguali. Ammesse piu' chiavi, sperate da virgole."
WRITE (*,*) "    all       : grid,time,lev,var"
WRITE (*,*) "    grid      : grigliato"
WRITE (*,*) "    vtime     : verification time"
WRITE (*,*) "    time      : reference time e timerange"
WRITE (*,*) "    lev       : livello"
WRITE (*,*) "    var       : parametro"
WRITE (*,*) "    nil       : solo la forma della griglia (nx, ny)"
WRITE (*,*) ""
WRITE (*,*) "key_list    : definisce le chiavi grib-api in base ai quali due grib sono"
WRITE (*,*) "              considerati uguali. Ammesse piu' chiavi, sperate da virgole."
WRITE (*,*) "-h          : visualizza questo help"
WRITE (*,*) ""
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

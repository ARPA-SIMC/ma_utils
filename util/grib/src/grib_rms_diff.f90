PROGRAM grib_rms_diff
!--------------------------------------------------------------------------
! Legge due file grib con campi corrispondenti, scrive un campo con
! l' "RMS differnce" fra tutti i campi.
! Programma largamente ispirato a math_grib.f90: lo tengo separato, perche'
! math_grib scrive un campo per ogni istante, questo solo un campo
! complessivo.
! Le flag "lverbose" ed "lforce" (prese da math_grib) non sono testate
! Questo programma potrebbe essere facilmente esteso ad altre statistiche:
! man abs. diff, max diff, etc.

!                                         Versione 1.0.0, Enrico 16/01/2025
!--------------------------------------------------------------------------
USE grib_api
USE missing_values
USE grib2_utilities
IMPLICIT NONE

! 1.1 Variabili locali
REAL, ALLOCATABLE :: valuesa(:),valuesb(:),valuesout(:),rmsd(:)
INTEGER, ALLOCATABLE :: nok(:)
INTEGER :: ifa,ifb,ifout,iga=0,igb=0,igout=0
INTEGER :: idp,kp,iret,ier,ni,nj,np,ni_sav,nj_sav,np_sav,gnov,nom,nocv,en,k,kga,nmiss
INTEGER :: clret(0:6),cllog(0:6)
CHARACTER (LEN=250) :: filea,fileb,fileout,chdum,check_list
CHARACTER(LEN=40) :: gta
LOGICAL :: cl_grid,cl_time,cl_vtime,cl_lev,cl_var,lverbose,lforce

! 1.2 Parametri da riga comando
check_list = ""
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (chdum(1:7) == "-check=") THEN
    check_list = chdum(8:)
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filea = chdum
    CASE (2)
      fileb = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO
IF (idp < 2) THEN
  CALL write_help
  STOP 1
ENDIF

! 1.3 Inizializzazioni

lverbose = .FALSE.
lforce = .FALSE.
fileout = "rms_diff.grib"

CALL grib_open_file(ifa,filea,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifb,fileb,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998
CALL grib_open_file(ifout,fileout,"w",iret)

IF (lforce) THEN
  cllog(:) = 0
  nmiss = 0
ENDIF

!--------------------------------------------------------------------------
! 2) Ciclo sui grib

DO kga = 1,HUGE(0)

! 2.1) Leggo il prossimo GRIB da fileA e fileB
  CALL grib_new_from_file(ifa,iga,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9997

  CALL grib_new_from_file(ifb,igb,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9996

! 2.2) Controlli di consistenza  
  CALL check_consistency(iga,igb,cl_grid,cl_time,cl_vtime,cl_lev,cl_var, &
    lverbose,clret,ier)
  IF (.NOT. lforce .AND. ier /= 0) THEN
    GOTO 9995
  ELSE IF (lforce .AND. ier /= 0) THEN
    nmiss = nmiss + 1
    WHERE (clret(:) /= 0)
      cllog(:) = cllog(:) + 1
    ENDWHERE
  ENDIF

! 2.3) Alloco gli array per i campi
  CALL grib_get(iga,"gridType",gta)
  IF (gta == "regular_ll" .OR. gta == "rotated_ll") THEN
    CALL grib_get(iga,"numberOfPointsAlongAParallel",ni)
    CALL grib_get(iga,"numberOfPointsAlongAMeridian",nj)
    np = ni*nj
  ELSE IF (gta == "unstructured_grid") THEN
    CALL grib_get(iga,"numberOfDataPoints",np)
    ni = imiss
    nj = imiss
  ELSE
    CALL grib_get(iga,"Ni",ni)
    CALL grib_get(iga,"Nj",nj)
    np = ni*nj
  ENDIF

  IF (kga == 1) THEN
    ALLOCATE (valuesa(np),valuesb(np),valuesout(np),rmsd(np),nok(np))
    ni_sav = ni
    nj_sav = nj
    np_sav = np
    rmsd(:) = 0.
    nok(:) = 0
    CALL grib_clone(iga,igout)
  ELSE If (ni /= ni_sav .OR. nj /= nj_sav .OR. np /= np_sav) THEN
    GOTO 9992
  ENDIF

! 2.4) Leggo i campi
  CALL grib_get(iga,"getNumberOfValues",gnov)    ! totale di punti nel grib
  CALL grib_get(iga,"numberOfMissing",nom)       ! n.ro dati mancanti
  CALL grib_get(iga,"numberOfCodedValues",nocv)  ! n.ro dati validi
  IF (nocv == 0) THEN
    valuesa(:) = rmiss
  ELSE
    CALL grib_set(iga,"missingValue",rmiss)
    CALL grib_get(iga,"values",valuesa(:))
  ENDIF
  IF (nom + nocv /= gnov .OR. &
    (nocv /= 0 .AND. nocv /= COUNT(valuesa(:) /= rmiss))) GOTO 9994

  CALL grib_get(igb,"getNumberOfValues",gnov)    ! totale di punti nel grib
  CALL grib_get(igb,"numberOfMissing",nom)       ! n.ro dati mancanti
  CALL grib_get(igb,"numberOfCodedValues",nocv)  ! n.ro dati validi
  IF (nocv == 0) THEN
    valuesb(:) = rmiss
  ELSE
    CALL grib_set(igb,"missingValue",rmiss)
    CALL grib_get(igb,"values",valuesb(:))
  ENDIF
  IF (nom + nocv /= gnov .OR. &
    (nocv /= 0 .AND. nocv /= COUNT(valuesb(:) /= rmiss))) GOTO 9993

! 2.5) Incremento RMSE
  rmsd(:) = rmsd(:) + (valuesb(:)-valuesa(:))**2
  WHERE (valuesa(:) /= rmiss .AND. valuesb(:) /= rmiss)
    nok(:) = nok(:) + 1
  ENDWHERE

! 2.6 Libero memoria
  CALL grib_release(iga)
  CALL grib_release(igb)
ENDDO

!--------------------------------------------------------------------------
! 3) Output

! Calcolo RMSD
WHERE (nok(:) > 0)
 valuesout(:) = SQRT(rmsd(:) / REAL(nok(:)))
ELSEWHERE
 valuesout(:) = rmiss
ENDWHERE

! Inizializzo fileout
CALL grib_get(igout,"editionNumber",en)
IF (.NOT. ALL(c_e(valuesout(:)))) THEN
  IF (en == 1) THEN
    CALL grib_set(igout,"bitmapPresent",1)
  ELSE IF (en == 2) THEN
    CALL grib_set(igout,"bitMapIndicator",0)
  ENDIF
  CALL grib_set(igout,"missingValue",rmiss)
ENDIF

! Scrivo il risultato
CALL grib_set(igout,"values",valuesout(:))
CALL grib_write (igout,ifout)
CALL grib_release(igout)

! Log a schermo
WRITE (*,*) "Elaborati ",kga-1," campi"
IF (lforce .AND. nmiss > 0) THEN
  WRITE (*,*) "Campi inconsistenti messi mancanti: ",nmiss
  DO k = 0,5
    IF (cllog(k) > 0) WRITE (*,*) &
      "Test ",cllab(k)," fallito ",cllog(k)," volte"
  ENDDO
ENDIF
STOP

!--------------------------------------------------------------------------

9999 CONTINUE
WRITE (*,*)  "File non trovato ",TRIM(filea)
STOP 2

9998 CONTINUE
WRITE (*,*)  "File non trovato ",TRIM(fileb)
STOP 2

9997 CONTINUE
WRITE (*,*)  "Errore leggendo ",TRIM(filea)," campo ",kga
STOP 2

9996 CONTINUE
WRITE (*,*)  "Errore leggendo ",TRIM(fileb)," campo ",kga
STOP 2

9995 CONTINUE
WRITE (*,*)  "Grib inconsistenti, campo ",kga
DO k = 0,5
  IF (clret(k) == -1) WRITE (*,*) cllab(k),": test non eseguito"
  IF (clret(k) == 0)  WRITE (*,*) cllab(k),": test superato"
  IF (clret(k) == 1)  WRITE (*,*) cllab(k),": test non passato"
ENDDO
STOP 3

9994 CONTINUE
WRITE (*,*) "Errore nelle chiavi relative ai dati mancanti, file ",TRIM(filea)
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(valuesa(:) /= rmiss)
STOP 4

9993 CONTINUE
WRITE (*,*) "Errore nelle chiavi relative ai dati mancanti, file ",TRIM(fileb)
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(valuesb(:) /= rmiss)
STOP 4

9992 CONTINUE
WRITE (*,*) "Numero di punti diverso nel campo ",kga," mi fermo"
WRITE (*,*) "Trovati (ni,nj,np) ",ni,nj,np
WRITE (*,*) "Attesi  (ni,nj,np) ",ni_sav,nj_sav,np_sav
STOP 5

END PROGRAM grib_rms_diff

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE parse_check_list(check_list,cl_grid,cl_time,cl_vtime,cl_lev, &
  cl_var,ier)
!
! Interpreta la check-list da riga comando
!
IMPLICIT NONE

CHARACTER (LEN=250), INTENT(IN) :: check_list
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
WRITE (*,*) "Uso: grib_rms_diff.exe fileA fileB [-check=check_list]"
WRITE (*,*) ""
WRITE (*,*) "fileA,fileB : files da cui leggere i dati (contengono uno o piu' GRIB)"
WRITE (*,*) "  scrive il file: rms_diff.grib (con le chiavi come il primo campo di filea)"
WRITE (*,*) ""
WRITE (*,*) "check_list  : definisce gli elementi che devono essere uguali in due grib"
WRITE (*,*) "              corrispondenti di fileA e fileB. Possono essere specificate"
WRITE (*,*) "              piu' chiavi, sperate da virgole."
WRITE (*,*) "    [default] : grid,time,lev"
WRITE (*,*) "    all       : grid,time,lev,var"
WRITE (*,*) "    grid      : grigliato"
WRITE (*,*) "    vtime     : verification time"
WRITE (*,*) "    time      : reference time e timerange"
WRITE (*,*) "    lev       : livello"
WRITE (*,*) "    var       : parametro"
WRITE (*,*) "    nil       : solo la forma della griglia (nx-ny, oppure np)"
WRITE (*,*) ""
RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


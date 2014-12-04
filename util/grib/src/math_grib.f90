PROGRAM math_grib
!--------------------------------------------------------------------------
! Programma per compiere operazioni matematiche su tutti i campi contenuti
! in due files grib. 
! Gestisce GRIB1 e GRIB"
! Sostituisce ed integra somma_grib.f90 e moltiplica_grib.f90
!
!                                         Versione 1.2.0, Enrico 02/12/2014
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
USE grib2_utilities
IMPLICIT NONE

! Variabili locali
REAL, ALLOCATABLE :: valuesa(:),valuesb(:),valuesout(:),valuesout2(:)
REAL :: coeffa,coeffb
INTEGER :: ifa,ifb,ifout,iga=0,igb=0,igout=0
INTEGER :: idp,kp,ios(4),ier,iret,kga,k,bpv
INTEGER :: clret(0:6),cllog(0:6),nmiss,ni,nj,ni_sav,nj_sav,en,gnov,nom,nocv
CHARACTER (LEN=250) :: filea,fileb,fileout,chdum,check_list
CHARACTER(LEN=40) :: gta
CHARACTER (LEN=5) :: oper
CHARACTER (LEN=3) :: next_arg,lsgn
LOGICAL :: cl_grid,cl_time,cl_vtime,cl_lev,cl_var,lbconst,lforce,lverbose

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Default
lsgn="nil"
next_arg = ""
cl_grid  = .TRUE.
cl_time  = .TRUE.
cl_vtime = .FALSE.
cl_lev   = .TRUE.
cl_var   = .FALSE.
lbconst = .FALSE.
lforce = .FALSE.
lverbose = .FALSE.
bpv = imiss

! 1.2 Parametri da riga comando
check_list = ""
idp = 0
ios(:) = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (next_arg == "bpv") THEN
    READ (chdum,*,IOSTAT=ios(1)) bpv
    next_arg = ""
  ELSE IF (next_arg == "sgn") THEN
    READ (chdum,'(a)',IOSTAT=ios(2)) lsgn
    next_arg = ""
  ELSE IF (chdum(1:7) == "-check=") THEN
    check_list = chdum(8:)
  ELSE IF (TRIM(chdum) == "-nbit") THEN
    next_arg = "bpv"
  ELSE IF (TRIM(chdum) == "-sgn") THEN
    next_arg = "sgn"
  ELSE IF (TRIM(chdum) == "-bconst") THEN
    lbconst = .TRUE.
  ELSE IF (TRIM(chdum) == "-force") THEN
    lforce = .TRUE.
  ELSE IF (TRIM(chdum) == "-verbose") THEN
    lverbose = .TRUE.
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      READ (chdum,*,IOSTAT=ios(3)) coeffa
    CASE (2)
      filea = chdum
    CASE (3)
      READ (chdum,*,IOSTAT=ios(4)) coeffb
    CASE (4)
      fileb = chdum
    CASE (5)
      fileout = chdum
    CASE (6)
      oper = chdum(1:5)
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

! 1.3 Parsing dei parametri e controlli
ier = 0
IF (check_list /= "") CALL parse_check_list(check_list,cl_grid,cl_time, &
  cl_vtime,cl_lev,cl_var,ier)

IF (ANY(ios(:) /= 0) .OR. ier /= 0 .OR. idp /= 6 .OR. &
    (oper/="sum" .AND. oper/="mul" .AND. oper/="div" .AND. oper/="div2") .OR. &
    (lsgn/="nil" .AND. lsgn/="c" .AND. lsgn/="p" .AND. &
     lsgn/="m" .AND. lsgn/="zp" .AND. lsgn/="zm") ) &
    THEN
  CALL write_help
  STOP 1
ENDIF

! 1.4 Inizializzazioni
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

  IF (kga == 1 .OR. .NOT. lbconst) THEN
    CALL grib_new_from_file(ifb,igb,iret)
    IF (iret == GRIB_END_OF_FILE) EXIT
    IF (iret /= GRIB_SUCCESS) GOTO 9996
  ENDIF

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

! 2.3) Calcoli

! Alloco gli array per i campi
  CALL grib_get(iga,"gridType",gta)
  IF (gta == "regular_ll" .OR. gta == "rotated_ll") THEN
    CALL grib_get(iga,"numberOfPointsAlongAParallel",ni)
    CALL grib_get(iga,"numberOfPointsAlongAMeridian",nj)
  ELSE
    CALL grib_get(iga,"Ni",ni)
    CALL grib_get(iga,"Nj",nj)
  ENDIF
  IF (kga == 1 .OR. ni /= ni_sav .OR. nj /= nj_sav) THEN
    IF (kga > 1) DEALLOCATE (valuesa,valuesb,valuesout,valuesout2)
    ALLOCATE (valuesa(ni*nj),valuesb(ni*nj),valuesout(ni*nj),valuesout2(ni*nj))
    ni_sav = ni
    nj_sav = nj
  ENDIF

! Leggo i campi
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

  IF (kga == 1 .OR. .NOT. lbconst) THEN
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
  ENDIF

! Eseguo l'operazione aritmetica richiesta
  IF (ier /= 0) THEN
    valuesout2(:) = rmiss
  ELSE 
    valuesout(:) = rmiss
    SELECT CASE (oper)
    CASE("sum")  
      WHERE (valuesa(:) /= rmiss .AND. valuesb(:) /= rmiss)
        valuesout(:) = (coeffa * valuesa(:)) + (coeffb * valuesb(:))
      ENDWHERE
    CASE("mul")  
      WHERE (valuesa(:) /= rmiss .AND. valuesb(:) /= rmiss)
        valuesout(:) = (coeffa + valuesa(:)) * (coeffb + valuesb(:))
      ENDWHERE
    CASE("div")  
      WHERE (valuesa(:) /= rmiss .AND. valuesb(:) /= rmiss .AND. &
             coeffb + valuesb(:) /= 0)
        valuesout(:) = (coeffa + valuesa(:)) / (coeffb + valuesb(:))
      ENDWHERE
    CASE("div2")  
      WHERE (valuesa(:) /= rmiss .AND. valuesb(:) /= rmiss .AND. &
             coeffa + valuesa(:) /= 0)
        valuesout(:) = (coeffb + valuesb(:)) / (coeffa + valuesa(:))
      ENDWHERE
    END SELECT

!   Se richiesto, cambio il segno
    valuesout2(:) = valuesout(:)
    SELECT CASE (lsgn)
    CASE("c")  
      WHERE (valuesout(:) /= rmiss)
        valuesout2(:) = -valuesout(:)
      ENDWHERE
    CASE("p")  
      WHERE (valuesout(:) /= rmiss)
        valuesout2(:) = ABS(valuesout(:))
      ENDWHERE
    CASE("m")  
      WHERE (valuesout(:) /= rmiss)
        valuesout2(:) = -ABS(valuesout(:))
      ENDWHERE
    CASE("zp")  
      WHERE (valuesout(:) /= rmiss)
        valuesout2(:) = MAX(0.,valuesout(:))
      ENDWHERE
    CASE("zm")  
      WHERE (valuesout(:) /= rmiss)
        valuesout2(:) = MIN(0.,valuesout(:))
      ENDWHERE
    ENDSELECT

  ENDIF

! 2.4) Scrivo i risultati su fileout
  CALL grib_clone(iga,igout)
  CALL grib_get(igout,"editionNumber",en)

  IF (.NOT. ALL(c_e(valuesout2(:)))) THEN
    IF (en == 1) THEN
      CALL grib_set(igout,"bitmapPresent",1)
    ELSE IF (en == 2) THEN
      CALL grib_set(igout,"bitMapIndicator",0)
    ENDIF
    CALL grib_set(igout,"missingValue",rmiss)
  ENDIF

  IF (c_e(bpv)) THEN
    CALL grib_set(igout,"bitsPerValue",bpv)
  ENDIF

  CALL grib_set(igout,"values",valuesout2(:))

  CALL grib_write (igout,ifout)

! 2.5) Libero memoria
  CALL grib_release(iga)
  IF (.NOT. lbconst) CALL grib_release(igb)
  CALL grib_release(igout)
  
ENDDO

! 2.6) conclusione; log a schermo
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
WRITE (*,*) "Errore nelle chiavi realtive ai dati mancanti, file ",TRIM(filea)
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(valuesa(:) /= rmiss)
STOP 4

9993 CONTINUE
WRITE (*,*) "Errore nelle chiavi realtive ai dati mancanti, file ",TRIM(fileb)
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(valuesb(:) /= rmiss)
STOP 4

END PROGRAM math_grib

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
WRITE (*,*) "Uso: math_grib.exe a fileA b fileB fileout oper [-check=check_list]"
WRITE (*,*) "  [-nbit N] [-bconst] [-force] [-verbose] [-h]" 
WRITE (*,*) ""
WRITE (*,*) "Scorre i grib contenuti in fileA e fileB, scrive su fileout il risultato di"
WRITE (*,*) "  un'operazione aritmetica compiuta su ciascun punto dei campi corrispondenti."
WRITE (*,*) "  I grib in output hanno gli stessi header di fileA"
WRITE (*,*) ""
WRITE (*,*) "a, b        : numeri reali"
WRITE (*,*) "fileA,fileB : files da cui leggere i dati (contengono uno o piu' GRIB)"
WRITE (*,*) "fileout     : file su cui scrivere i risultati"
WRITE (*,*) "oper        : operazione da compiere sui dati. Puo' valere"
WRITE (*,*) "    sum       : fileout = (a * fileA) + (b * fileB)"
WRITE (*,*) "    mul       : fileout = (a + fileA) * (b + fileB)"
WRITE (*,*) "    div       : fileout = (a + fileA) / (b + fileB)"
WRITE (*,*) "    div2      : fileout = (b + fileB) / (a + fileA)"
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
WRITE (*,*) "    nil       : solo la forma della griglia (nx, ny)"
WRITE (*,*) ""
WRITE (*,*) "-bconst     : legge solo il primo campo di fileB, e applica queti valori a"
WRITE (*,*) "              tutti i campi contenuti in fileA"
WRITE (*,*) "-sgn OPT    : modifica il segno del cmapo in ouput. Valori ammessi per OPT:"
WRITE (*,*) "              c  -> -field"
WRITE (*,*) "              p  -> ABS(field)"
WRITE (*,*) "              m  -> -ABS(field)"
WRITE (*,*) "              zp -> MAX(0.,field)"
WRITE (*,*) "              zm -> MIN(0.,field)"
WRITE (*,*) ""
WRITE (*,*) "-nbit N     : forza il numero di bit nel grib in output (def: come fileA)"
WRITE (*,*) "-force      : se trova due grib inconsistenti, scrive un campo interamente"
WRITE (*,*) "              mancante e prosegue [default: si ferma]"
WRITE (*,*) ""
WRITE (*,*) "-verbose    : visualizza il dettaglio dei test di consistenza falliti"
WRITE (*,*) "-h          : visualizza questo help"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


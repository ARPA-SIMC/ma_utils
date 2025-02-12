PROGRAM grib_uv2ffdd
!--------------------------------------------------------------------------
! Legge due file grib con le componenti U e V del vento (campi
! corrispondenti), scrive due files con i campi "direzione" e "modulo"
! Programma derivato dalla vecchia versione gribex e da grib_rms_diff.f90
!  
!                                         Versione 1.0.0, Enrico 28/01/2025
!--------------------------------------------------------------------------
USE grib_api
USE missing_values
USE grib2_utilities
IMPLICIT NONE

! 1.1 Variabili locali
REAL, ALLOCATABLE :: valuesu(:),valuesv(:),valuesf(:),valuesd(:)
INTEGER :: ifu,ifv,iff,ifd,igu=0,igv=0,igf=0,igd=0
INTEGER :: idp,kp,iret,ier,ni,nj,np,ni_sav,nj_sav,np_sav,gnov,nom,nocv,k,kgu,nmiss
INTEGER :: enu,du,pcu,pnu,env,dv,pcv,pnv,enout
INTEGER :: clret(0:6),cllog(0:6)
CHARACTER (LEN=250) :: fileu,filev,filef,filed,chdum,check_list
CHARACTER(LEN=40) :: gtu
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
      fileu = chdum
    CASE (2)
      filev = chdum
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
filef = "ff.grib"
filed = "dd.grib"

CALL grib_open_file(ifu,fileu,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifv,filev,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998
CALL grib_open_file(iff,filef,"w",iret)
CALL grib_open_file(ifd,filed,"w",iret)

IF (lforce) THEN
  cllog(:) = 0
  nmiss = 0
ENDIF

!--------------------------------------------------------------------------
! 2) Ciclo sui grib

DO kgu = 1,HUGE(0)

! 2.1) Leggo il prossimo GRIB da fileu e filev
  CALL grib_new_from_file(ifu,igu,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9997

  CALL grib_new_from_file(ifv,igv,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9996

! 2.2) Controlli di consistenza  
  CALL check_consistency(igu,igv,cl_grid,cl_time,cl_vtime,cl_lev,cl_var, &
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
  CALL grib_get(igu,"gridType",gtu)
  IF (gtu == "regular_ll" .OR. gtu == "rotated_ll") THEN
    CALL grib_get(igu,"numberOfPointsAlongAParallel",ni)
    CALL grib_get(igu,"numberOfPointsAlongAMeridian",nj)
    np = ni*nj
  ELSE IF (gtu == "unstructured_grid") THEN
    CALL grib_get(igu,"numberOfDataPoints",np)
    ni = imiss
    nj = imiss
  ELSE
    CALL grib_get(igu,"Ni",ni)
    CALL grib_get(igu,"Nj",nj)
    np = ni*nj
  ENDIF

  IF (kgu == 1) THEN
    ALLOCATE (valuesu(np),valuesv(np),valuesf(np),valuesd(np))
    ni_sav = ni
    nj_sav = nj
    np_sav = np
  ELSE If (ni /= ni_sav .OR. nj /= nj_sav .OR. np /= np_sav) THEN
    GOTO 9992
  ENDIF

! 2.4.1) Leggo il prossimo campo U
  CALL grib_get(igu,"editionNumber",enu)         ! grib edition
  IF (enu == 1) THEN
    GOTO 9990
  ELSE
    CALL grib_get(igu,"discipline",du)
    CALL grib_get(igu,"parameterCategory",pcu)
    CALL grib_get(igu,"parameterNumber",pnu)
    IF (du/=0 .OR. pcu/=2 .OR. pnu/=2) GOTO 9989
  ENDIF  

  CALL grib_get(igu,"getNumberOfValues",gnov)    ! totale di punti nel grib
  CALL grib_get(igu,"numberOfMissing",nom)       ! n.ro dati mancanti
  CALL grib_get(igu,"numberOfCodedValues",nocv)  ! n.ro dati validi
  IF (nocv == 0) THEN
    valuesu(:) = rmiss
  ELSE
    CALL grib_set(igu,"missingValue",rmiss)
    CALL grib_get(igu,"values",valuesu(:))
  ENDIF
  IF (nom + nocv /= gnov .OR. &
    (nocv /= 0 .AND. nocv /= COUNT(valuesu(:) /= rmiss))) GOTO 9994

! 2.4.2) Leggo il prossimo campo V
  CALL grib_get(igv,"editionNumber",env)         ! grib edition
  IF (env == 1) THEN
    GOTO 9990
  ELSE
    CALL grib_get(igv,"discipline",dv)
    CALL grib_get(igv,"parameterCategory",pcv)
    CALL grib_get(igv,"parameterNumber",pnv)
    IF (dv/=0 .OR. pcv/=2 .OR. pnv/=3) GOTO 9988
 ENDIF

  CALL grib_get(igv,"getNumberOfValues",gnov)    ! totale di punti nel grib
  CALL grib_get(igv,"numberOfMissing",nom)       ! n.ro dati mancanti
  CALL grib_get(igv,"numberOfCodedValues",nocv)  ! n.ro dati validi
  IF (nocv == 0) THEN
    valuesv(:) = rmiss
  ELSE
    CALL grib_set(igv,"missingValue",rmiss)
    CALL grib_get(igv,"values",valuesv(:))
  ENDIF
  IF (nom + nocv /= gnov .OR. &
    (nocv /= 0 .AND. nocv /= COUNT(valuesv(:) /= rmiss))) GOTO 9993

  IF (enu /= env) THEN
    GOTO 9987
  ELSE
    enout = enu
  ENDIF
  
! 2.5) Calcolo FF e DD
  CALL uv2dirint(valuesu(1:np),valuesv(1:np),valuesd(1:np), &
    valuesf(1:np),np,rmiss)

! 2.6.1) Scrivo output: direzione
  CALL grib_clone(igu,igd)
  IF (.NOT. ALL(c_e(valuesd(:)))) THEN
    IF (enout == 1) THEN
      CALL grib_set(igd,"bitmapPresent",1)
    ELSE IF (enout == 2) THEN
      CALL grib_set(igd,"bitMapIndicator",0)
    ENDIF
    CALL grib_set(igd,"missingValue",rmiss)
  ENDIF
  CALL grib_set(igd,"parameterNumber",0)
  CALL grib_set(igd,"values",valuesd(:))
  CALL grib_write (igd,ifd)
  
! 2.6.1) Scrivo output: velocita'
  CALL grib_clone(igu,igf)
  IF (.NOT. ALL(c_e(valuesf(:)))) THEN
    IF (enout == 1) THEN
      CALL grib_set(igf,"bitmapPresent",1)
    ELSE IF (enout == 2) THEN
      CALL grib_set(igf,"bitMapIndicator",0)
    ENDIF
    CALL grib_set(igf,"missingValue",rmiss)
  ENDIF
  CALL grib_set(igf,"parameterNumber",1)
  CALL grib_set(igf,"values",valuesf(:))
  CALL grib_write (igf,iff)

! 2.7) Libero memoria
  CALL grib_release(igu)
  CALL grib_release(igv)
  CALL grib_release(igf)
  CALL grib_release(igd)
ENDDO

!--------------------------------------------------------------------------
! 3) Output

! Log a schermo
WRITE (*,*) "Elaborati ",kgu-1," campi"
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
WRITE (*,*)  "File non trovato ",TRIM(fileu)
STOP 2

9998 CONTINUE
WRITE (*,*)  "File non trovato ",TRIM(filev)
STOP 2

9997 CONTINUE
WRITE (*,*)  "Errore leggendo ",TRIM(fileu)," campo ",kgu
STOP 2

9996 CONTINUE
WRITE (*,*)  "Errore leggendo ",TRIM(filev)," campo ",kgu
STOP 2

9995 CONTINUE
WRITE (*,*)  "Grib inconsistenti, campo ",kgu
DO k = 0,5
  IF (clret(k) == -1) WRITE (*,*) cllab(k),": test non eseguito"
  IF (clret(k) == 0)  WRITE (*,*) cllab(k),": test superato"
  IF (clret(k) == 1)  WRITE (*,*) cllab(k),": test non passato"
ENDDO
STOP 3

9994 CONTINUE
WRITE (*,*) "Errore nelle chiavi relative ai dati mancanti, file ",TRIM(fileu)
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(valuesu(:) /= rmiss)
STOP 4

9993 CONTINUE
WRITE (*,*) "Errore nelle chiavi relative ai dati mancanti, file ",TRIM(filev)
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(valuesv(:) /= rmiss)
STOP 4

9992 CONTINUE
WRITE (*,*) "Numero di punti diverso nel campo ",kgu," mi fermo"
WRITE (*,*) "Trovati (ni,nj,np) ",ni,nj,np
WRITE (*,*) "Attesi  (ni,nj,np) ",ni_sav,nj_sav,np_sav
STOP 5

9990 CONTINUE
WRITE (*,*) "grib editon 1 not yet implemented ",kgu
STOP 6

9989 CONTINUE
WRITE (*,*) "Not a U component ",kgu
STOP 6

9988 CONTINUE
WRITE (*,*) "Not a V component ",kgu
STOP 6

9987 CONTINUE
WRITE (*,*) "Differen edition number for U and V ",kgu
STOP 6

END PROGRAM grib_uv2ffdd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE uv2dirint(u,v,dd,ff,nval,rmis)
!
! Dati i vettori di componenti u e v, ritona quelli di direzione e modulo
! Se una componente e' mancante dd e ff sono mancanti. 
! Se u=v=0, mette ff=dd=0
!

IMPLICIT NONE

INTEGER, INTENT(IN):: nval
REAL, INTENT(IN) :: u(nval),v(nval),rmis
REAL, INTENT(OUT) :: dd(nval),ff(nval)

REAL, PARAMETER :: dtr = 180./3.141592654
INTEGER k

!-------------------------------------------------------------------------
DO k = 1,nval

  IF (u(k) == rmis .OR. v(k) == rmis) THEN
    dd(k) = rmis
    ff(k) = rmis
    CYCLE
  ENDIF

  IF      (u(k) <= 0. .AND. v(k) < 0.) THEN    ! 0 - 90
    dd(k) = dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) < 0. .AND. v(k) >= 0.) THEN    ! 90 - 180
    dd(k) = 90. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) >= 0. .AND. v(k) > 0.) THEN    ! 180 - 270
    dd(k) = 180. + dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) > 0. .AND. v(k) <= 0.) THEN    ! 270 - 360
    dd(k) = 270. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) == 0. .AND. v(k) == 0.) THEN
    dd(k) = 0.
  ENDIF
 
  ff(k) = SQRT (u(k)*u(k) + v(k)*v(k))

ENDDO

RETURN
END SUBROUTINE uv2dirint

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
WRITE (*,*) "Uso: grib_uv2ffdd.exe fileU fileV [-check=check_list]"
WRITE (*,*) ""
WRITE (*,*) "fileU,fileV : files da cui leggere i dati (contengono uno o piu' GRIB)"
WRITE (*,*) "  scrive i files: dd.grib, ff.grib (con le chiavi come il primo campo di fileU)"
WRITE (*,*) ""
WRITE (*,*) "check_list  : definisce gli elementi che devono essere uguali in due grib"
WRITE (*,*) "              corrispondenti di fileU e fileV. Possono essere specificate"
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


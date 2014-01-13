PROGRAM calc_swdowm
!--------------------------------------------------------------------------
! Programma che calcola SW_down a partire da SW_budget e albedo.
! Il file Albedo puo' contenere valori orari oppure un solo campo.
!
! Note:
! Tutti i grib devono essere definiti sulla stessa area.
!
!                                         Versione 1.0.2, Enrico 13/01/2014
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

INTERFACE 
  SUBROUTINE test_grib (ig,nreq,same_grid,same_time_timerange, &
    same_verification_time,same_level,same_5d,eps,verbose,ier)
    INTEGER, INTENT(IN) :: ig(:)
    INTEGER, INTENT(IN) :: nreq
    LOGICAL, INTENT(OUT), OPTIONAL :: same_grid,same_time_timerange, &
      same_verification_time, same_level,same_5d
    REAL, INTENT(IN), OPTIONAL :: eps
    LOGICAL, INTENT(IN), OPTIONAL :: verbose
    INTEGER, INTENT(OUT), OPTIONAL :: ier
  END SUBROUTINE test_grib
END INTERFACE

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

! Variabili locali
REAL, ALLOCATABLE :: swb(:),alb(:),swdown(:)
INTEGER :: kp,idp,ifswb,ifalb,ifout,iret
INTEGER :: kg,tab,var,ni,nj,bpv
INTEGER :: igswb,igalb,igout
CHARACTER (LEN=200) :: fileswb,filealb,fileout,chdum
LOGICAL :: lalb1,lok(2)

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
lalb1 = .FALSE.
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-alb_one") THEN
    lalb1 = .TRUE.
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      fileswb = chdum
    CASE (2)
      filealb = chdum
    CASE (3)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

! 1.2) Apro i files
CALL grib_open_file(ifswb,fileswb,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

CALL grib_open_file(ifalb,filealb,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998

CALL grib_open_file(ifout,fileout,"w",iret)

!--------------------------------------------------------------------------
! 2) Elaborazioni (ciclo sui grib in input)

igswb = 0
igalb = 0
igout = 0

DO kg = 1,HUGE(0)

! 2.1) Leggo il prossimo campo
  CALL grib_new_from_file(ifswb,igswb,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9997

  IF (kg == 1 .OR. .NOT. lalb1) THEN
    CALL grib_new_from_file(ifalb,igalb,iret)
    IF (iret /= GRIB_SUCCESS) GOTO 9996
  ENDIF

! 2.2) Controlli

! controllo che i parametri siano "albedo" e "SWB"
  CALL grib_get(igswb,"table2Version",tab)
  CALL grib_get(igswb,"indicatorOfParameter",var)
  IF (.NOT. ((tab==200 .AND. var==110) .OR. &
    (tab==2 .AND. var==111))) GOTO 9995

  IF (kg == 1 .OR. .NOT. lalb1) THEN
    CALL grib_get(igalb,"table2Version",tab)
    CALL grib_get(igalb,"indicatorOfParameter",var)
    IF (.NOT. ((tab==200 .AND. var==124) .OR. &
      (tab==2 .AND. var==84))) GOTO 9994
  ENDIF

! controllo che griglia e date siano compatibili
  IF (lalb1) THEN
    CALL test_grib((/igswb,igalb/),2,same_grid=lok(1),verbose=.TRUE.)
    lok(2) = .TRUE.
  ELSE
    CALL test_grib((/igswb,igalb/),2,same_grid=lok(1), &
      same_time_timerange=lok(2),verbose=.TRUE.)
  ENDIF
  IF (.NOT. lok(1)) GOTO 9993
  IF (.NOT. lok(2)) GOTO 9992

! 2.3) Calcolo SWdown e la scrivo sul file di output
  CALL grib_get(igswb,"numberOfPointsAlongAParallel",ni)
  CALL grib_get(igswb,"numberOfPointsAlongAMeridian",nj)
  ALLOCATE (swb(ni*nj),alb(ni*nj),swdown(ni*nj))

  CALL grib_get(igswb,"values",swb)
  CALL grib_get(igalb,"values",alb)
  swdown(:) = swb(:) / (1.-0.01*alb(:))

  CALL grib_clone(igswb,igout)
  CALL grib_set(igout,"values",swdown)
  CALL grib_write (igout,ifout)

  DEALLOCATE (swb,alb,swdown)
  CALL grib_release(igswb)
  CALL grib_release(igout)
  IF (.NOT. lalb1) CALL grib_release(igalb)

ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "Elaborazioni completate, elaborati ",kg-1," campi"
STOP

!--------------------------------------------------------------------------
9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileswb)
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileout)
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(fileswb)
STOP

9996 CONTINUE
IF (lalb1) THEN
  WRITE (*,*) "Errore leggendo ",TRIM(filealb)
ELSE
  WRITE (*,*) TRIM(filealb)," contiene meno campi di ",TRIM(fileswb)
ENDIF
STOP

9995 CONTINUE
WRITE (*,*) TRIM(fileswb)," contiene dati non SWB ",tab,var
STOP

9994 CONTINUE
WRITE (*,*) TRIM(filealb)," contiene dati non ALB ",tab,var
STOP

9993 CONTINUE
WRITE (*,*) "Griglie incompatibili nei dati di input"
STOP

9992 CONTINUE
WRITE (*,*) "Date o timerange incompatibili nei dati di input"
STOP

END PROGRAM calc_swdowm

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE test_grib (ig,nreq,same_grid,same_time_timerange, &
  same_verification_time,same_level,same_5d,eps_req,verbose,ier)

!--------------------------------------------------------------------------
! Confronta alcune chiavi di un insieme di grib (vengono esaminati i primi
! ng grib puntati dagli elementi del vettore ig(:). Gestisce GRIB1 e GRIB2.

! I paramtri opzionali same_* sono messi a .T. se tutti i grib hanno gli
! stessi valori.
! - same_grid:              stessa griglia
! - same_time_timerange:    stesso reference time e timernage
! - same_verification_time: stesso verificaiton time
! - same_level:             stesso livello
! - same_5d:                stessa griglia, ref.time, timerange, livello
!
! Se verbose e' presente e .T., stampa la dagnositca dei test non passati
!
!--------------------------------------------------------------------------

USE datetime_class
USE grib_api
IMPLICIT NONE

REAL, PARAMETER :: eps_def = 0.00101 ! tolleranza per uguaglianze tra reali

INTEGER, INTENT(IN) :: ig(:)
INTEGER, INTENT(IN) :: nreq
LOGICAL, INTENT(OUT), OPTIONAL :: same_grid,same_time_timerange, &
  same_verification_time,same_level,same_5d
REAL, INTENT(IN), OPTIONAL :: eps_req
LOGICAL, INTENT(IN), OPTIONAL :: verbose
INTEGER, INTENT(OUT), OPTIONAL :: ier

! Variabili locali
TYPE(datetime) :: dtval(nreq)
REAL, DIMENSION(nreq) :: xi,xf,yi,yf,dx,dy
REAL :: eps
INTEGER, DIMENSION(nreq) :: ed,ni,nj,sm,dig,dd,dt
INTEGER :: scad(4,nreq),lev(3,nreq),toffs,tosfs,sfoffs,sfosfs
INTEGER :: ncfr,idig,jdig,ir,local_ier(2),kg
CHARACTER (LEN=20), DIMENSION(nreq) :: gt
CHARACTER (LEN=80) :: err_msg
CHARACTER (LEN=10) :: ch10(nreq)
LOGICAL :: ll(20)

!--------------------------------------------------------------------------
! 1) Controlli Preliminari

IF (nreq > SIZE(ig)) THEN
  WRITE (*,*) "Warning test_grib: grib richiesti ",nreq, &
    "puntatori passati ",SIZE(ig)
  WRITE (*,*) "esamino solo ",nreq," campi"
  ncfr = SIZE(ig)
ELSE
  ncfr = nreq
ENDIF

DO kg = 1,ncfr
  CALL grib_get(ig(kg),"edition",ed(kg))
ENDDO

IF (PRESENT(eps_req)) THEN
  eps = eps_req
ELSE
  eps = eps_def
ENDIF

!--------------------------------------------------------------------------
! 2) Leggo le chiavi richieste

IF (PRESENT(ier)) ier = 0
local_ier(:) = 0
ll(:) = .TRUE.

DO kg = 1,SIZE(ig)

! 2.1 Griglia
  IF (PRESENT(same_grid) .OR. PRESENT(same_5d)) THEN
    CALL grib_get(ig(kg),"gridType",gt(kg))
    CALL grib_get(ig(kg),"longitudeOfFirstGridPointInDegrees",xi(kg))
    CALL grib_get(ig(kg),"longitudeOfLastGridPointInDegrees",xf(kg))
    CALL grib_get(ig(kg),"latitudeOfFirstGridPointInDegrees",yi(kg))
    CALL grib_get(ig(kg),"latitudeOfLastGridPointInDegrees",yf(kg))
    CALL grib_get(ig(kg),"numberOfPointsAlongAParallel",ni(kg))
    CALL grib_get(ig(kg),"numberOfPointsAlongAMeridian",nj(kg))
    CALL grib_get(ig(kg),"scanningMode",sm(kg))

    IF (ed(kg) == 1) THEN
      CALL grib_get(ig(kg),"ijDirectionIncrementGiven",dig(kg))
    ELSE IF (ed(kg) == 2) THEN 
      CALL grib_get(ig(kg),"iDirectionIncrementGiven",idig)
      CALL grib_get(ig(kg),"jDirectionIncrementGiven",jdig)
      IF (idig == 1 .OR. jdig == 1) THEN
        dig(kg) = 1
      ELSE
        dig(kg) = 0
      ENDIF
    ENDIF
    IF (dig(kg) == 1) THEN
      CALL grib_get(ig(kg),"iDirectionIncrement",dx(kg))
      CALL grib_get(ig(kg),"jDirectionIncrement",dy(kg))
    ELSE
      dx(kg) = (xf(kg)-xi(kg)) / REAL (ni(kg)-1)
      dy(kg) = (yf(kg)-yi(kg)) / REAL (nj(kg)-1)
    ENDIF
  ENDIF

! 2.2 Refernce Time e Timerange
  IF (PRESENT(same_time_timerange) .OR. PRESENT(same_5d)) THEN
    CALL grib_get(ig(kg),"unitOfTimeRange",scad(1,kg))
    CALL grib_get(ig(kg),"startStep",scad(2,kg))
    CALL grib_get(ig(kg),"endStep",scad(3,kg))
    CALL grib_get(ig(kg),"timeRangeIndicator",scad(4,kg))
    CALL grib_get(ig(kg),"dataDate",dd)
    CALL grib_get(ig(kg),"dataTime",dt)
  ENDIF

! 2.3 Livello  
  IF (PRESENT(same_level) .OR. PRESENT(same_5d)) THEN
    IF (ed(kg) == 1) THEN
      CALL grib_get(ig(kg),"indicatorOfTypeOfLevel",lev(1,kg))
      IF (lev(1,kg) == 1 .OR. lev(1,kg) == 100 .OR. lev(1,kg) == 102 .OR. &
          lev(1,kg) == 105 .OR. lev(1,kg) == 109) THEN
        CALL grib_get(ig(kg),"level",lev(2,kg))
        lev(3,kg) = lev(2,kg)
      ELSE IF (lev(1,kg) == 110 .OR. lev(1,kg) == 112) THEN
        CALL grib_get(ig(kg),"bottomLevel",lev(2,kg))
        CALL grib_get(ig(kg),"topLevel",lev(3,kg))
      ENDIF

    ELSE IF (ed(kg) == 2) THEN
      CALL grib_get(ig(kg),"typeOfFirstFixedSurface",tosfs)
      CALL grib_get(ig(kg),"scaleFactorOfFirstFixedSurface",sfosfs)
      CALL grib_get(ig(kg),"scaledValueOfFirstFixedSurface",lev(2,kg))
      CALL grib_get(ig(kg),"typeOfSecondFixedSurface",toffs)
      CALL grib_get(ig(kg),"scaleFactorOfSecondFixedSurface",sfoffs)
      CALL grib_get(ig(kg),"scaledValueOfSecondFixedSurface",lev(3,kg))
      IF (sfoffs /= 0 .OR. sfosfs /= 0 .OR. toffs /= tosfs) THEN
        local_ier(1) = 1
      ELSE                 ! converto type of level da GRIB2 a GRIB1
        IF (toffs == 1) THEN         ! Surface
          lev(1,kg) = 1
        ELSE IF (toffs == 100) THEN  ! isobaric surface
          lev(1,kg) = 100
        ELSE IF (toffs == 101) THEN  ! MSL
          lev(1,kg) = 102
        ELSE IF (toffs == 103) THEN  ! spec.height above ground
          lev(1,kg) = 105
        ELSE IF (toffs == 105) THEN  ! Hybrid layer
          IF (lev(2,kg) == lev(3,kg)) THEN
            lev(1,kg) = 109
          ELSE
            lev(1,kg) = 110
          ENDIF
        ELSE IF (toffs == 106) THEN  ! Depth
          lev(1,kg) = 112
        ELSE
          local_ier(1) = 1
        ENDIF  
      ENDIF

    ENDIF
  ENDIF

! 2.4 Verification time
  IF (PRESENT(same_verification_time)) THEN
    CALL handler2datet_ver(ig(kg),dtval(kg),local_ier(2),err_msg)
  ENDIF

! 2.5 Segnalo errori nell'eleborazione di questo grib
  IF (ANY(local_ier(1:2) /= 0)) THEN
    WRITE (*,*) "Subr. test_grib: errore elaborando il grib ",kg

    IF (local_ier(1)/=0) WRITE (*,'(2(a,3i5))') &
      "Livello grib2 non gestito: 1st fix. surf ",toffs,sfoffs, &
      lev(3,kg)," 2nd fixed surf ",tosfs,sfosfs,lev(2,kg)

    IF (local_ier(2)/=0) &
      WRITE (*,'(a)') "Errore calcolo verification time (handler2datet_ver)"
      WRITE (*,'(a)') TRIM(err_msg)

    IF (PRESENT(ier)) ier = 1

  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 3) Confronti

! 3.1 Griglia
IF (PRESENT(same_grid) .OR. PRESENT(same_5d)) THEN
  ll(1) = ALL(gt(1:nreq) == gt(1))
  ll(2) = ALL(ABS(xi(1:nreq)-xi(1)) <= eps)
  ll(3) = ALL(ABS(xf(1:nreq)-xf(1)) <= eps)
  ll(4) = ALL(ABS(yi(1:nreq)-yi(1)) <= eps)
  ll(5) = ALL(ABS(yf(1:nreq)-yf(1)) <= eps)
  ll(6) = ALL(ni(1:nreq) == ni(1))
  ll(7) = ALL(nj(1:nreq) == nj(1))
  ll(8) = ALL(sm(1:nreq) == sm(1))
  ll(9) = ALL(ABS(dx(1:nreq)-dx(1)) <= eps)
  ll(10) = ALL(ABS(dy(1:nreq)-dy(1)) <= eps)
ENDIF

! 3.2 Reference Time e Timerange
IF (PRESENT(same_time_timerange) .OR. PRESENT(same_5d)) THEN
  ll(11) = ALL(scad(1,1:nreq) == scad(1,1))
  ll(12) = ALL(scad(2,1:nreq) == scad(2,1))
  ll(13) = ALL(scad(3,1:nreq) == scad(3,1))
  ll(14) = ALL(scad(4,1:nreq) == scad(4,1))
  ll(15) = ALL(dd(1:nreq) == dd(1))
  ll(16) = ALL(dt(1:nreq) == dt(1))
ENDIF

! 3.3 Livello  
IF (PRESENT(same_level) .OR. PRESENT(same_5d)) THEN
  ll(17) = ALL(lev(1,1:nreq) == lev(1,1))
  ll(18) = ALL(lev(2,1:nreq) == lev(2,1))
  ll(19) = ALL(lev(3,1:nreq) == lev(3,1))
ENDIF

! 3.4 Verification time
IF (PRESENT(same_verification_time)) THEN
  ll(20) = ALL(dtval(1:nreq) == dtval(1))
ENDIF


!--------------------------------------------------------------------------
! 4) Tiro le somme

IF (PRESENT(same_grid)) same_grid = ALL(ll(1:10))
IF (PRESENT(same_time_timerange)) same_time_timerange = ALL(ll(11:16))
IF (PRESENT(same_level)) same_level = ALL(ll(17:19))
IF (PRESENT(same_verification_time)) same_verification_time = ll(20)
IF (PRESENT(same_5d)) same_5d = ALL(ll(1:19))

IF (PRESENT(verbose)) THEN
IF (verbose .AND. .NOT. ALL(ll(:))) THEN
  IF (.NOT. ll(1)) WRITE (*,*) "gridType ",gt(1:nreq)
  IF (.NOT. ll(2)) WRITE (*,*) "xi ",xi(1:nreq)
  IF (.NOT. ll(3)) WRITE (*,*) "xf ",xi(1:nreq)
  IF (.NOT. ll(4)) WRITE (*,*) "yi ",yi(1:nreq)
  IF (.NOT. ll(5)) WRITE (*,*) "yf ",yf(1:nreq)
  IF (.NOT. ll(6)) WRITE (*,*) "ni ",ni(1:nreq)
  IF (.NOT. ll(7)) WRITE (*,*) "nj ",nj(1:nreq)
  IF (.NOT. ll(8)) WRITE (*,*) "scanningMode ",sm(1:nreq)
  IF (.NOT. ll(9)) WRITE (*,*) "dx ",dx(1:nreq)
  IF (.NOT. ll(10)) WRITE (*,*) "dy ",dy(1:nreq)
  IF (.NOT. ll(11)) WRITE (*,*) "scad(1) ",scad(1,1:nreq)
  IF (.NOT. ll(12)) WRITE (*,*) "scad(2) ",scad(2,1:nreq)
  IF (.NOT. ll(13)) WRITE (*,*) "scad(3) ",scad(3,1:nreq)
  IF (.NOT. ll(14)) WRITE (*,*) "scad(4) ",scad(4,1:nreq)
  IF (.NOT. ll(15)) WRITE (*,*) "dateDate ",dd(1:nreq)
  IF (.NOT. ll(16)) WRITE (*,*) "dateTime ",dt(1:nreq)
  IF (.NOT. ll(17)) WRITE (*,*) "lev(1) ",lev(1,1:nreq)
  IF (.NOT. ll(17)) WRITE (*,*) "lev(2) ",lev(2,1:nreq)
  IF (.NOT. ll(17)) WRITE (*,*) "lev(3) ",lev(3,1:nreq)
  IF (.NOT. ll(20)) THEN
    DO kg = 1,nreq
      CALL getval(dtval(kg),SIMPLEDATE=ch10(kg))
    ENDDO
    WRITE (*,*) "verification time",(" ",ch10(kg),kg=1,nreq)
  ENDIF
ENDIF
ENDIF

RETURN

END SUBROUTINE test_grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE handler2datet_ver(ig,dtval,ier,err_msg)
!
! Dato l'handler di un GRIB, ritorna la sua data-ora di validita'
!

USE grib_api
USE datetime_class

IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: ig
TYPE(datetime), INTENT(OUT) :: dtval
INTEGER, INTENT(OUT) :: ier
CHARACTER (LEN=80) :: err_msg

! Variabili locali
TYPE(datetime) :: dtref
INTEGER :: scad(3),idum(2),sca,yy,mm,dd,hh,edition,pdtn,iouotr,ft,iouftr,lotr

!--------------------------------------------------------------------------

CALL grib_get(ig,"edition",edition)

! Leggo il reference time) e lo salvo in dtref
CALL grib_get(ig,"dataDate",idum(1))
CALL grib_get(ig,"dataTime",idum(2))
yy = idum(1)/10000
mm = MOD(idum(1)/100,100)
dd = MOD(idum(1),100)
hh = idum(2) / 100
dtref= datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)

! Leggo dal grib i dati relativi alla scadenza, e calcolo quante ore devono
! essere aggiunte alla data di emissione
IF (edition == 1) THEN
  CALL grib_get(ig,"timeRangeIndicator",scad(1))
  CALL grib_get(ig,"startStep",scad(2))
  CALL grib_get(ig,"endStep",scad(3))

  SELECT CASE(scad(1))
  CASE (1,10,13)        ! Analisi (10: Calmet; 13: LAMA)
    sca = 0            
  CASE(0)               ! Forecast o analisi (forecast a  +0)
    sca = scad(2)      
  CASE(2,3,4,5,14)      ! prodotto riferito a un intervallo (14: Pesco)
    sca = scad(3)      
  CASE DEFAULT          ! time range non gestito
    ier = 1
    WRITE (err_msg,'(a,i5)') "Timerange GRIB1 non gestito ",scad(1)
    RETURN
  END SELECT

ELSE IF (edition == 2) THEN
  CALL grib_get(ig,"productDefinitionTemplateNumber",pdtn)
  CALL grib_get(ig,"indicatorOfUnitOfTimeRange",iouotr)
  CALL grib_get(ig,"forecastTime",ft)

  IF (pdtn == 0) THEN
    IF (iouotr == 1) THEN
      sca = ft
    ELSE
      WRITE (err_msg,'(a,i5)') "indicatorOfUnitOfTimeRange non gestito ",iouotr
      ier = 2
      RETURN
    ENDIF
  ELSE IF (pdtn == 8) THEN
    CALL grib_get(ig,"indicatorOfUnitForTimeRange",iouftr)
    CALL grib_get(ig,"lengthOfTimeRange",lotr)
      IF (iouftr == 1) THEN
        sca = ft + lotr
      ELSE
        WRITE (err_msg,'(a,i5)') "indicatorForUnitOfTimeRange non gestito ",iouotr
        ier = 2
        RETURN
      ENDIF
  ELSE 
    WRITE (err_msg,'(a,i5)') "productDefinitionTemplateNumber non gestito ",pdtn
    ier = 2
    RETURN
  ENDIF

ENDIF

! Calcolo data-ora di validita' e termino
dtval = dtref + timedelta_new(HOUR=sca)
ier = 0

RETURN
END SUBROUTINE handler2datet_ver

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: calc_swdown.exe file_swbudg file_alb file_out [-ist] [-h]" 
WRITE (*,*) "Calcosa SW down a partire da SW budget e albedo"
WRITE (*,*) "-alb_one: il file albedo contiene un solo campo (default: dati "
WRITE (*,*) "  corrispondenti a file_swbudg)"
WRITE (*,*) ""
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

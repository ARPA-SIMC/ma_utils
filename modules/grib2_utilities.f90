MODULE grib2_utilities
!--------------------------------------------------------------------------
! Utilita' per la getione dei GRIB2 in logica GRIB1
!
!                                         Versione 1.2.1, Enrico 06/03/2014
!--------------------------------------------------------------------------

USE missing_values

CHARACTER (LEN=10), PARAMETER :: cllab(0:5) = (/ &
  "ni-nj     ","griglia   ","t-trange  ", &
  "ver.time  ","livello   ","variabile "/)

CONTAINS

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_grib1_header(gaid,reftime,par,lev,scad,iret)
!--------------------------------------------------------------------------
! Dato il puntatore a un grib1 o grib2, ritorna alcune informazioni 
! contenute dell'header, nello stile "GRIB1"
! 1) reftime: reference time
! 2) par: centre, table, parameter
! 3) lev: level_type, level1, level2
! 4) scad: unit, p1, p2, timerange
!
! Note:
! - Per ora gestisce solo grib2 con pdtn = 0,8 o 40
!--------------------------------------------------------------------------

USE datetime_class
USE grib_api
IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: gaid
TYPE(datetime) ,INTENT(OUT), OPTIONAL :: reftime
INTEGER, INTENT(OUT), OPTIONAL :: par(3),lev(3),scad(4),iret
!TYPE(grib1_grid), INTENT(OUT), OPTIONAL :: grid

! Variabili locali
REAL :: voffs,vosfs
INTEGER :: en,ier,yy,mon,dd,hh,min
INTEGER :: toffs,svoffs,sfoffs,tosfs,sfosfs,svosfs,pc,pn,ct
INTEGER :: sortt,topd,pdtn,togp,ft,iouotr,toti,tosp,iouftr,lotr

!--------------------------------------------------------------------------

ier = 0
CALL grib_get(gaid,"editionNumber",en)

!--------------------------------------------------------------------------
! 1) Reference time

IF (PRESENT(reftime)) THEN
  CALL grib_get(gaid,"year",yy)
  CALL grib_get(gaid,"month",mon)
  CALL grib_get(gaid,"day",dd)
  CALL grib_get(gaid,"hour",hh)
  CALL grib_get(gaid,"minute",min)

  reftime = datetime_new(YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)
ENDIF

!--------------------------------------------------------------------------
! 2) Parametro

IF (PRESENT(par)) THEN
  par(:) = imiss
  CALL grib_get(gaid,"centre",par(1))

  IF (en == 1) THEN
    CALL grib_get(gaid,"table2Version",par(2))
    CALL grib_get(gaid,"indicatorOfParameter",par(3))

  ELSE IF (en == 2) THEN  
    CALL grib_get(gaid,"productDefinitionTemplateNumber",pdtn)

    IF (pdtn == 0 .OR. pdtn == 8) THEN    ! analisi o previ "standard"
      CALL grib_get(gaid,"parameterCategory",par(2))
      CALL grib_get(gaid,"parameterNumber",par(3))

    ELSE IF (pdtn == 40) THEN             ! Qualita dell'aria ECMWF
      CALL grib_get(gaid,"parameterCategory",pc)
      CALL grib_get(gaid,"parameterNumber",pn)
      CALL grib_get(gaid,"constituentType",ct)
      IF (pc /= 20 .OR. pn /= 0) THEN
        WRITE(*,*) "GRIB2 AQ ECMWF non gestito: pc, pn ",pc,pn
        ier = 5         
      ENDIF      

      par(2) = 200
      SELECT CASE (ct)
      CASE(1)    ! O3
        par(3) = 151
      CASE(5)    ! CO
        par(3) = 154
      CASE(6)    ! NO2
        par(3) = 153
      CASE(8)    ! NO
        par(3) = 152
      CASE(10)   ! SO2
        par(3) = 155
      CASE(17)   ! Radon (non codificato!)
        par(3) = 1
      CASE(40008) ! PM10
        par(3) = 220
      CASE(40009) ! PM25
        par(3) = 221
      CASE DEFAULT
        WRITE(*,*) "GRIB2 AQ ECMWF con parametro non gestito: ",ct
        ier = 6
      END SELECT

    ELSE
      WRITE (*,'(a,i4,a)') "Grib2 con pdtn = ",pdtn," non gestito"
      ier = 1
    ENDIF
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 3) Livello

IF (PRESENT(lev)) THEN
  lev(:) = imiss

  IF (en == 1) THEN
    CALL grib_get(gaid,"indicatorOfTypeOfLevel",lev(1))
    IF (lev(1) == 1 .OR. lev(1) == 100 .OR. lev(1) == 105 .OR. &
        lev(1) == 109 .OR. lev(1) == 111) THEN
      CALL grib_get(gaid,"level",lev(2))
      lev(3) = 0
    ELSE IF (lev(1) == 110 .OR. lev(1) == 112) THEN
      CALL grib_get(gaid,"bottomLevel",lev(2))
      CALL grib_get(gaid,"topLevel",lev(3))
    ENDIF
 
  ELSE IF (en == 2) THEN 
    CALL grib_get(gaid,"typeOfFirstFixedSurface",toffs)
    CALL grib_get(gaid,"scaledValueOfFirstFixedSurface",svoffs)
    CALL grib_get(gaid,"scaleFactorOfFirstFixedSurface",sfoffs)
    CALL grib_get(gaid,"typeOfSecondFixedSurface",tosfs)
    CALL grib_get(gaid,"scaledValueOfSecondFixedSurface",svosfs)
    CALL grib_get(gaid,"scaleFactorOfSecondFixedSurface",sfosfs)
    IF (toffs /= 255) THEN
      voffs = REAL(svoffs) * 10.**(-REAL(sfoffs))
    ELSE
      voffs = 0.
    ENDIF
    IF (tosfs /= 255) THEN
      vosfs = REAL(svosfs) * 10.**(-REAL(sfosfs))
    ELSE
      vosfs = 0.
    ENDIF

    IF (toffs == 1) THEN
      lev(1) = 1                  ! Surface
      lev(2) = 0
      lev(3) = 0
    ELSE IF (toffs == 100) THEN
      lev(1) = 100                ! Isobaric surface
      lev(2) = NINT(voffs)
      lev(3) = 0
    ELSE IF (toffs == 103) THEN
      lev(1) = 105                ! Specified height level above ground m
      lev(2) = NINT(voffs)
      lev(3) = 0
    ELSE IF (toffs == 105 .AND. (tosfs == 255 .OR. &
        (tosfs == toffs .AND. sfoffs == sfosfs .AND. svoffs == svosfs))) THEN
      lev(1) = 109                ! Hybrid level
      lev(2) = NINT(voffs)
      lev(3) = 0
    ELSE IF (toffs == 105 .AND. tosfs == toffs .AND. sfoffs == sfosfs .AND. &
        svoffs /= svosfs) THEN
      lev(1) = 110                ! Hybrid layer
      lev(2) = NINT(voffs)
      lev(3) = NINT(vosfs)
    ELSE IF (toffs == 106) THEN
      lev(1) = 112                ! Layer between two depths below land
      lev(2) = NINT(voffs)
      lev(3) = NINT(vosfs)
    ELSE
      WRITE (*,'(2a,6i8)') "Livello grib2 non gestito", &
        " toffs,sfoffs,svoffs,tosfs,sfosfs,svosfs", &
        toffs,sfoffs,svoffs,tosfs,sfosfs,svosfs
      ier = 2
    ENDIF

  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 4) Scadenza

IF (PRESENT(scad)) THEN
  scad(:) = imiss
  IF (en == 1) THEN
    CALL grib_get(gaid,"unitOfTimeRange",scad(1))
    CALL grib_get(gaid,"P1",scad(2))
    CALL grib_get(gaid,"P2",scad(3))
    CALL grib_get(gaid,"timeRangeIndicator",scad(4))
 
  ELSE IF (en == 2) THEN 
    CALL grib_get(gaid,"significanceOfReferenceTime",sortt)
    CALL grib_get(gaid,"typeOfProcessedData",topd)
    CALL grib_get(gaid,"productDefinitionTemplateNumber",pdtn)
    CALL grib_get(gaid,"typeOfGeneratingProcess",togp)
    CALL grib_get(gaid,"forecastTime",ft)
    CALL grib_get(gaid,"indicatorOfUnitOfTimeRange",iouotr)
    IF (pdtn == 8) THEN
      CALL grib_get(gaid,"typeOfTimeIncrement",toti)
      CALL grib_get(gaid,"typeOfStatisticalProcessing",tosp)
      CALL grib_get(gaid,"indicatorOfUnitForTimeRange",iouftr)
      CALL grib_get(gaid,"lengthOfTimeRange",lotr)
    ELSE
      toti = imiss
      tosp = imiss
      iouftr = imiss
      lotr = imiss
    ENDIF
    IF (iouotr/=1 .OR. (pdtn==8 .AND. iouftr/=1)) THEN
      WRITE (*,*) "Unit of timerange is not hour"
      ier = 3
    ENDIF
    IF (sortt==0 .AND. topd==0 .AND. pdtn==0 .AND. togp==0 .AND. &
        ft==0) THEN                  ! Analisi istantanea
      scad(1) = iouotr
      scad(2) = 0  
      scad(3) = 0
      scad(4) = 0
    ELSE IF (sortt==1 .AND. topd==1 .AND. pdtn==0 .AND. togp==2 .AND. &
        ft/=0) THEN                  ! Previsione istantanea
      scad(1) = iouotr
      scad(2) = ft  
      scad(3) = 0
      scad(4) = 0
    ELSE IF (sortt==0 .AND. topd==0 .AND. pdtn==8 .AND. togp==0 .AND. &
        ft==0 .AND. tosp==0 .AND. toti==1) THEN ! Analisi mediata (Pesco)
      scad(1) = iouotr
      scad(2) = 0
      scad(3) = lotr
      scad(4) = 14
    ELSE IF (sortt==0 .AND. topd==0 .AND. pdtn==8 .AND. togp==0 .AND. &
        ft==0 .AND. tosp==1 .OR. toti==1) THEN  ! Analisi cum (dep.Chimere)
      scad(1) = iouotr
      scad(2) = 0
      scad(3) = lotr
      scad(4) = 15
    ELSE IF (sortt==0 .AND. topd==0 .AND. pdtn==8 .AND. togp==0 .AND. &
        ft==0 .AND. tosp==2 .OR. toti==1) THEN  ! Analisi max (NinfaUB, O3/NO2)
      scad(1) = iouotr
      scad(2) = 0
      scad(3) = lotr
      scad(4) = 16
    ELSE IF (sortt==1 .AND. topd==1 .AND. pdtn==8 .AND. togp==2 .AND. &
        toti==2) THEN                ! Previsione mediata
      scad(1) = iouotr
      scad(2) = ft 
      scad(3) = ft + lotr
      scad(4) = 3
    ELSE IF (sortt==1 .AND. topd==2 .AND. pdtn==40 .AND. togp==2) THEN
      scad(1) = iouotr               ! Analisi o previ ist. MACC
      scad(2) = ft 
      scad(3) = 0
      scad(4) = 0
    ELSE
      WRITE (*,'(2a,6i8)') "[get_grib1_header] Timerange non gestito; ", &
        "sort,topd,pdtn,togp,ft,toti ",sortt,topd,pdtn,togp,ft,toti
      ier = 4
    ENDIF

  ENDIF
ENDIF

IF (PRESENT(iret)) iret = ier

RETURN
END SUBROUTINE get_grib1_header 

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_grib_time(gaid,rtime,vtime,iret)
!--------------------------------------------------------------------------
! Dato il puntatore a un grib1 o grib2, ne ritorna reference time e 
! verification time.
!
! Note:
! - Al momento la gestione dei GRIB2 e' incompleta, e dipende dalla subr.
! get_grib1_header. 
! - Questa subr. potrebbe essere inglobata in get_grib1_header: per ora la
!   lascio distinta, per leggibilita' e perche' in linea di principio 
!   potrebbe essere resa "standard" (ritorna variabili standard LibSIM 
!   invece dei vettori grib1 "obsoleti"). Per contro, se devo chiamere 
!   entrambe le subr. elaboro due volte le chiavi relative al timerange...
! - sarebbe bello usare direttamente una chiave tipo "*endOfTimeInterval*", 
!   ma non e' sempre definita nel grib...
!--------------------------------------------------------------------------

USE grib_api
USE datetime_class
IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: gaid
TYPE(datetime), INTENT(OUT), OPTIONAL :: rtime,vtime
INTEGER, INTENT(OUT), OPTIONAL :: iret

! Variabili locali
INTEGER :: scad(4),rdate(4),ier
!INTEGER :: dd,dt,dda
TYPE(datetime) :: rtime_work,vtime_work

!--------------------------------------------------------------------------

! 1) Trovo reference time
IF (PRESENT(rtime) .OR. PRESENT(vtime)) THEN
  CALL grib_get(gaid,"year",rdate(1))
  CALL grib_get(gaid,"month",rdate(2))                             
  CALL grib_get(gaid,"day",rdate(3))                               
  CALL grib_get(gaid,"hour",rdate(4))                              
  rtime_work = datetime_new(YEAR=rdate(1), MONTH=rdate(2), DAY=rdate(3), &
    HOUR=rdate(4))

! Formulazione alternativa
! CALL grib_get(gaid,"dataDate",dd)
! CALL grib_get(gaid,"dataTime",dt)
! rtime_work = datetime_new(YEAR=dda/10000, MONTH=MOD(dda/100,100), &
!    DAY=MOD(dda,100), HOUR=dta/100)
ENDIF

! 2) Trovo verification time
IF (PRESENT(vtime)) THEN
  ier = 0
  CALL get_grib1_header(gaid,scad=scad,iret=ier)

  IF (ier /= 0) THEN
    iret = 1
    vtime_work = datetime_miss

  ELSE
    SELECT CASE (scad(4))
    CASE(0)
      vtime_work = rtime_work + timedelta_new(hour=scad(2))

    CASE(1)
      vtime_work = rtime_work

    CASE(2,3,4,5,14,15)
      vtime_work = rtime_work + timedelta_new(hour=scad(3))

    CASE(13)
      vtime_work = rtime_work

    CASE DEFAULT
      iret = 2
      vtime_work = datetime_miss

    END SELECT
  ENDIF
ENDIF

! 3) Ritorno i parametri richiesti
IF (PRESENT(vtime)) vtime = vtime_work
IF (PRESENT(rtime)) rtime = rtime_work
iret = 0

RETURN
END SUBROUTINE get_grib_time

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE check_consistency(iga,igb,cl_grid,cl_time,cl_vtime,cl_lev, &
  cl_var,lverbose,clret,ier)
!--------------------------------------------------------------------------
! Verifica se due grib rispettano i criteri di consistenza richiesti 
! Ritorna:
! ier   : 0 se i grib pasano tutti i test richiesti, 1 se qualche test e' 
!         fallito, 2 in caso di errore
! clret : ogni elemento contine il risultato di un test: -1=non effettuato,
!         0=passato, 1=non passato. 
!         Gli elementi del vattore si riferiscono a: 0: ni-nj; 1: griglia; 
!         2: time/trange; 3: verfi. time; 4: livello; 5: variabile
!--------------------------------------------------------------------------

USE grib_api
USE datetime_class
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: iga,igb
LOGICAL, INTENT(IN) :: cl_grid,cl_time,cl_vtime,cl_lev,cl_var,lverbose
INTEGER, INTENT(OUT) :: clret(0:5),ier

! tolleranza per l'uguaglianza delle griglie (gradi)
REAL, PARAMETER :: eps = 0.0015

TYPE(datetime) :: vtimea,vtimeb
REAL :: xia,yia,xfa,yfa,xrota,yrota,fea
REAL :: xib,yib,xfb,yfb,xrotb,yrotb,feb
INTEGER :: nia,nja,sma,za,dda,dta
INTEGER :: nib,njb,smb,zb,ddb,dtb
INTEGER :: para(3),leva(3),scada(4),parb(3),levb(3),scadb(4)
INTEGER :: ireta,iretb
CHARACTER(LEN=40) :: gta,gtb

!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! 1) Preliminari

ier = 0
clret(:) = -1

! 1.1) se necessario calcolo le caratteristiche del grib in stile GRIB1
IF (cl_time .OR. cl_vtime .OR. cl_lev .OR. cl_var) THEN
  CALL get_grib1_header(iga,par=para,lev=leva,scad=scada,iret=ireta)
  CALL get_grib1_header(igb,par=parb,lev=levb,scad=scadb,iret=iretb)
  IF (ireta /= 0 .OR. iretb /= 0) THEN
    WRITE (*,*) "check_consistency: errore elborazione header"
    ier = 2
    RETURN
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 2) Test

!--------------------------------------------------------------------------
! 2.0) Test 0: forma della griglia (viene sempre controllata)

CALL grib_get(iga,"gridType",gta)
IF (gta == "regular_ll" .OR. gta == "rotated_ll") THEN
  CALL grib_get(iga,"numberOfPointsAlongAParallel",nia)
  CALL grib_get(iga,"numberOfPointsAlongAMeridian",nja)
ELSE
  CALL grib_get(iga,"Ni",nia)
  CALL grib_get(iga,"Nj",nja)
ENDIF

CALL grib_get(igb,"gridType",gtb)
IF (gtb == "regular_ll" .OR. gtb == "rotated_ll") THEN
  CALL grib_get(igb,"numberOfPointsAlongAParallel",nib)
  CALL grib_get(igb,"numberOfPointsAlongAMeridian",njb)
ELSE
  CALL grib_get(igb,"Ni",nib)
  CALL grib_get(igb,"Nj",njb)
ENDIF

IF (nia == nib .AND. nja == njb) THEN
  clret(0) = 0
ELSE
  ier = 1
  clret(0) = 1
ENDIF

!--------------------------------------------------------------------------
! 2.1) Test 1: griglia

IF (cl_grid) THEN
  
  IF (gta /= gtb) THEN
    ier = 1
    clret(1) = 1
  
  ELSE IF (gta == "regular_ll") THEN
    CALL grib_get(iga,"longitudeOfFirstGridPointInDegrees",xia)
    CALL grib_get(iga,"longitudeOfLastGridPointInDegrees",xfa)
    CALL grib_get(iga,"latitudeOfFirstGridPointInDegrees",yia)
    CALL grib_get(iga,"latitudeOfLastGridPointInDegrees",yfa)
    CALL grib_get(iga,"scanningMode",sma)
    CALL grib_get(igb,"longitudeOfFirstGridPointInDegrees",xib)
    CALL grib_get(igb,"longitudeOfLastGridPointInDegrees",xfb)
    CALL grib_get(igb,"latitudeOfFirstGridPointInDegrees",yib)
    CALL grib_get(igb,"latitudeOfLastGridPointInDegrees",yfb)
    CALL grib_get(igb,"scanningMode",smb)
    
    IF (ABS(xia-xib) < eps .AND. ABS(xfa-xfb) < eps .AND. &
        ABS(yia-yib) < eps .AND. ABS(yfa-yfb) < eps .AND. &
        sma == smb) THEN
      clret(1) = 0
    ELSE
      ier = 1
      clret(1) = 1
      IF (lverbose .AND. ABS(xia-xib) >= eps) WRITE (*,*) "xi ",xia,xib
      IF (lverbose .AND. ABS(xfa-xfb) >= eps) WRITE (*,*) "xf ",xfa,xfb
      IF (lverbose .AND. ABS(yia-yib) >= eps) WRITE (*,*) "yi ",yia,yib
      IF (lverbose .AND. ABS(yfa-yfb) >= eps) WRITE (*,*) "xf ",yfa,yfb
      IF (lverbose .AND. sma /= smb) WRITE (*,*) "sm ",sma,smb
    ENDIF
    
  ELSE IF (gta == "rotated_ll") THEN
    CALL grib_get(iga,"longitudeOfFirstGridPointInDegrees",xia)
    CALL grib_get(iga,"longitudeOfLastGridPointInDegrees",xfa)
    CALL grib_get(iga,"latitudeOfFirstGridPointInDegrees",yia)
    CALL grib_get(iga,"latitudeOfLastGridPointInDegrees",yfa)
    CALL grib_get(iga,"longitudeOfSouthernPoleInDegrees",xrota)
    CALL grib_get(iga,"latitudeOfSouthernPoleInDegrees",yrota)
    CALL grib_get(iga,"scanningMode",sma)
    CALL grib_get(igb,"longitudeOfFirstGridPointInDegrees",xib)
    CALL grib_get(igb,"longitudeOfLastGridPointInDegrees",xfb)
    CALL grib_get(igb,"latitudeOfFirstGridPointInDegrees",yib)
    CALL grib_get(igb,"latitudeOfLastGridPointInDegrees",yfb)
    CALL grib_get(igb,"longitudeOfSouthernPoleInDegrees",xrotb)
    CALL grib_get(igb,"latitudeOfSouthernPoleInDegrees",yrotb)
    CALL grib_get(igb,"scanningMode",smb)
    
    IF (ABS(xia-xib) < eps .AND. ABS(xfa-xfb) < eps .AND. &
        ABS(yia-yib) < eps .AND. ABS(yfa-yfb) < eps .AND. &
        ABS(xrota-xrotb) < eps .AND. ABS(yrota-yrotb) < eps .AND. &
        sma == smb) THEN
      clret(1) = 0
    ELSE
      ier = 1
      clret(1) = 1
      IF (lverbose .AND. ABS(xia-xib) >= eps) WRITE (*,*) "xi ",xia,xib
      IF (lverbose .AND. ABS(xfa-xfb) >= eps) WRITE (*,*) "xf ",xfa,xfb
      IF (lverbose .AND. ABS(yia-yib) >= eps) WRITE (*,*) "yi ",yia,yib
      IF (lverbose .AND. ABS(yfa-yfb) >= eps) WRITE (*,*) "xf ",yfa,yfb
      IF (lverbose .AND. ABS(xrota-xrotb) >= eps) WRITE (*,*) "xrot ",xrota,xrotb
      IF (lverbose .AND. ABS(yrota-yrotb) >= eps) WRITE (*,*) "yrot ",yrota,yrotb
      IF (lverbose .AND. sma /= smb) WRITE (*,*) "sm ",sma,smb
    ENDIF
    
  ELSE IF (gta == "UTM") THEN
    CALL grib_get(iga,"zone",za)
    CALL grib_get(iga,"falseEasting",fea)
    CALL grib_get(iga,"eastingOfFirstGridPoint",xia)
    CALL grib_get(iga,"eastingOfLastGridPoint",xfa)
    CALL grib_get(iga,"northingOfFirstGridPoint",yia)
    CALL grib_get(iga,"northingOfLastGridPoint",yfa)
    CALL grib_get(iga,"scanningMode",sma)
    CALL grib_get(igb,"zone",zb)
    CALL grib_get(igb,"falseEasting",feb)
    CALL grib_get(igb,"eastingOfFirstGridPoint",xib)
    CALL grib_get(igb,"eastingOfLastGridPoint",xfb)
    CALL grib_get(igb,"northingOfFirstGridPoint",yib)
    CALL grib_get(igb,"northingOfLastGridPoint",yfb)
    CALL grib_get(igb,"scanningMode",smb)
  
    IF (ABS(xia-xib) < eps .AND. ABS(xfa-xfb) < eps .AND. &
        ABS(yia-yib) < eps .AND. ABS(yfa-yfb) < eps .AND. &
        ABS(fea-feb) < eps .AND. za == zb .AND. sma == smb) THEN
      clret(1) = 0
    ELSE
      ier = 1
      clret(1) = 1
      IF (lverbose .AND. ABS(xia-xib) >= eps) WRITE (*,*) "xi ",xia,xib
      IF (lverbose .AND. ABS(xfa-xfb) >= eps) WRITE (*,*) "xf ",xfa,xfb
      IF (lverbose .AND. ABS(yia-yib) >= eps) WRITE (*,*) "yi ",yia,yib
      IF (lverbose .AND. ABS(yfa-yfb) >= eps) WRITE (*,*) "xf ",yfa,yfb
      IF (lverbose .AND. ABS(fea-feb) >= eps) WRITE (*,*) "fe ",fea,feb
      IF (lverbose .AND. za /= zb) WRITE (*,*) "z ",za,zb
      IF (lverbose .AND. sma /= smb) WRITE (*,*) "sm ",sma,smb
    ENDIF
    
  ELSE
    WRITE (*,*) "check_list: proiezione non gestita ",TRIM(gta)
    ier = 2
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 2.2) Test 2: time-timerange

IF (cl_time) THEN
  CALL grib_get(iga,"dataDate",dda)
  CALL grib_get(iga,"dataTime",dta)
  CALL grib_get(igb,"dataDate",ddb)
  CALL grib_get(igb,"dataTime",dtb)

  IF (dda == ddb .AND. dta == dtb .AND. ALL(scada(:) == scadb(:))) THEN
    clret(2) = 0
  ELSE
    ier = 1
    clret(2) = 1
    IF (lverbose .AND. dda /= ddb) WRITE (*,*) "dd ",dda,ddb
    IF (lverbose .AND. dta /= dtb) WRITE (*,*) "dt ",dta,dtb
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 2.3) Test 3: verification time

IF (cl_vtime) THEN
  CALL get_grib_time(iga,vtime=vtimea,iret=ireta)
  CALL get_grib_time(igb,vtime=vtimeb,iret=iretb)

  IF (ireta /= 0 .OR. iretb /= 0) THEN
    WRITE (*,*) "check_consistency: errore calcolo verification time"
    ier = 2
    RETURN
  ELSE IF (vtimea == vtimeb) THEN
    clret(3) = 0
  ELSE
    ier = 1
    clret(3) = 1
    IF (lverbose) WRITE (*,*) "vt ",to_char(vtimea)," ",to_char(vtimeb)
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 2.4) Test 4: livello

IF (cl_lev) THEN
  IF (ALL(leva(:) == levb(:))) THEN
    clret(4) = 0
  ELSE
    ier = 1
    clret(4) = 1
    IF (lverbose) WRITE (*,*) " lev ",leva,levb
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 2.5) Test 5: variabile

IF (cl_var) THEN
  IF (ALL(para(:) == parb(:))) THEN
    clret(5) = 0
  ELSE
    ier = 1
    clret(5) = 1
    IF (lverbose) WRITE (*,*) " par ",para,parb
  ENDIF
ENDIF

RETURN
END SUBROUTINE check_consistency

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

END MODULE grib2_utilities

PROGRAM crea_template_grib2_utm
!-------------------------------------------------------------------------------
! Crea un template GRIB2 in in proiezione UTM secondo le convenzioni SIMC.
! Partire dal template GRIB1 standard; programma ispirato all'analogo programma
! C di Enrico Zini
!                                                Versione 1.1, Enrico 22/02/2011
!-------------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

REAL, PARAMETER :: rmis = -9999.
REAL, ALLOCATABLE :: values(:)
INTEGER :: ig,iret,ifile
INTEGER :: templ_id,ni,nj,xi,xf,yi,yf,dx,dy
LOGICAL :: direction_increments_given
CHARACTER (LEN=80) :: chpar

!-------------------------------------------------------------------------------
! 1) User modifications

CALL getarg(1,chpar)
IF (chpar /= "") THEN
  WRITE (*,*) "Scrive sul file utm_grib2.tmpl un template GRIB2 in proiezione UTM "
  WRITE (*,*) "con le convenzioni SIMC-2010. Lanciare senza parametri"
  STOP
ENDIF

! parametri della griglia (Calmet operativo)
ni = 90
nj = 52
xi = 4852500
xf = 4952500
yi = 402500
yf = 412500

!-------------------------------------------------------------------------------
! 2) Costruisco il template (le chiavi con * nel commento sono usate da LibSim)

CALL grib_new_from_template(ig,"GRIB1",iret)

! 2.1 Section 0 (Indicator)
CALL grib_set(ig,"editionNumber",2)
CALL grib_set(ig,"discipline",0)                        ! 0=meteorological data; AQ non previsti...
                                                        
! 2.2 Section 1 (Identificator)                         
CALL grib_set(ig,"centre",200)                          !
CALL grib_set(ig,"subCentre",200)                       !
CALL grib_set(ig,"tablesVersion",5)                     ! Version of WMO "Master Table"
CALL grib_set(ig,"localTablesVersion",0)                ! Local tables non usate
CALL grib_set(ig,"significanceOfReferenceTime",0)       ! 0=Analysis, 1=start of forecast
CALL grib_set(ig,"year",0)                              !*reference time
CALL grib_set(ig,"month",1)                             
CALL grib_set(ig,"day",1)                               
CALL grib_set(ig,"hour",0)                              
CALL grib_set(ig,"minute",0)                            
CALL grib_set(ig,"second",0)                            
CALL grib_set(ig,"productionStatusOfProcessedData",0)   ! Operational product
CALL grib_set(ig,"typeOfProcessedData",0)               ! 0=Analysis, 1=forecast
                                                        
! 2.3 Section 3 (Grid)                                  
CALL grib_set(ig,"sourceOfGridDefinition",0)            ! 0=grid definition follows a template
CALL grib_set(ig,"numberOfDataPoints",ni*nj)            
CALL grib_set(ig,"numberOfOctectsForNumberOfPoints",0)  ! 0=regular grid
CALL grib_set(ig,"interpretationOfNumberOfPoints",0)    ! 0=regular grid
CALL grib_set(ig,"gridDefinitionTemplateNumber",32768)  !*
CALL grib_set(ig,"PLPresent",0)                         ! used to define typeOfGrid (section3.def)
CALL grib_set(ig,"datum",0)                             !*
CALL grib_set(ig,"shapeOfTheEarth",3)                   !*ellissoide con assi specificati (Clarke 1866)
CALL grib_set(ig,"scaleFactorOfEarthMajorAxis",5)       !*
CALL grib_set(ig,"scaledValueOfEarthMajorAxis",637820640)!*Asse maggiore (cm)
CALL grib_set(ig,"scaleFactorOfEarthMinorAxis",5)       !*
CALL grib_set(ig,"scaledValueOfEarthMinorAxis",635658380)!*Asse minore (cm)
CALL grib_set(ig,"falseEasting",500000)                 !*convenzione Calmet (nella versione 1.0 valeva 0.)
CALL grib_set(ig,"falseNorthing",0)                     !*
CALL grib_set(ig,"zone",32)                             !*
CALL grib_set(ig,"Ni",ni)                               !*
CALL grib_set(ig,"Nj",nj)                               !*
CALL grib_set(ig,"eastingOfFirstGridPoint",xi)          !*
CALL grib_set(ig,"eastingOfLastGridPoint",xf)           !*
CALL grib_set(ig,"northingOfFirstGridPoint",yi)         !*
CALL grib_set(ig,"northingOfLastGridPoint",yf)          !*
CALL grib_set(ig,"uvRelativeToGrid",1)                  !*0=wind comp. relative to North; 1=realtive to grid
CALL grib_set(ig,"iDirectionIncrementGiven",1)          !*1=dir.inc.given, 0=not given
CALL grib_set(ig,"jDirectionIncrementGiven",1)          !*1=dir.inc.given, 0=not given
CALL grib_set(ig,"scanningMode",64)                     !*64=from SW to NE; 0=from NW to SE

! 2.4 Section 4 (product; includes parameter, timerange, level)

! 2.4.1 Parameter at a point in time (Calmet, Chimere)
CALL grib_set(ig,"NV",0)                                ! n.er of Vert.coord.parameters
CALL grib_set(ig,"productDefinitionTemplateNumber",0)   !*0=valore istantaneo, 8=elaborato, 40=pollution
CALL grib_set(ig,"parameterCategory",200)               ! SIMC table (192-254 are reserved for local use)
CALL grib_set(ig,"parameterNumber",8)                   ! SIMC var
                                                        
CALL grib_set(ig,"typeOfGeneratingProcess",0)           ! 0=analysis, 2=forecast
CALL grib_set(ig,"backgroundProcess",0)                 
CALL grib_set(ig,"generatingProcessIdentifier",0)       ! Generating process
                                                        
CALL grib_set(ig,"hoursAfterDataCutoff",0)              
CALL grib_set(ig,"minutesAfterDataCutoff",0)            
CALL grib_set(ig,"indicatorOfUnitOfTimeRange",1)        !*1=hour
CALL grib_set(ig,"forecastTime",0)                      !*0=analysis; >0 = forecast (if average, put end of average interval)

! Possibile bug grib-api: se assegno la SecondFixedSurface dopo la FirstFixedSurface quest'ultima viene messa a 0...
    CALL grib_set(ig,"typeOfSecondFixedSurface",255)
    CALL grib_set_missing(ig,"scaleFactorOfSecondFixedSurface")
    CALL grib_set_missing(ig,"scaledValueOfSecondFixedSurface")
!  CALL grib_set(ig,"typeOfSecondFixedSurface",255)        !*
!  CALL grib_set(ig,"scaleFactorOfSecondFixedSurface",0)   !*
!  CALL grib_set(ig,"scaledValueOfSecondFixedSurface",0)   !*

CALL grib_set(ig,"typeOfFirstFixedSurface",1)           !*1=ground, 103=spec.height above ground, 105=hybrid layer, 255=missing
CALL grib_set(ig,"scaleFactorOfFirstFixedSurface",0)    !*always put 0 (scaledValue = Val * 10 ^ ScaleFactor)
CALL grib_set(ig,"scaledValueOfFirstFixedSurface",0)    !*surface -> 0; calmet -> height; hybrid layer -> index

! 2.4.2 Statistically processed value, eg. average (Pesco)
IF (.FALSE.) THEN
  CALL grib_set(ig,"yearOfEndOfOverallTimeInterval",0)
  CALL grib_set(ig,"monthOfEndOfOverallTimeInterval",0)
  CALL grib_set(ig,"dayOfEndOfOverallTimeInterval",0)
  CALL grib_set(ig,"hourOfEndOfOverallTimeInterval",0)
  CALL grib_set(ig,"minuteOfEndOfOverallTimeInterval",0)
  CALL grib_set(ig,"secondOfEndOfOverallTimeInterval",0)
  CALL grib_set(ig,"numberOfTimeRange",0)
  CALL grib_set(ig,"numberOfMissingInStatisticalProcess",0)
  CALL grib_set(ig,"typeOfStatisticalProcessing",0)     !*0=average
  CALL grib_set(ig,"typeOfTimeIncrement",1)             !*1=same forecast time (also analysis); 2=same start time
  CALL grib_set(ig,"indicatorOfUnitForTimeRange",1)     !*1=hour
  CALL grib_set(ig,"lengthOfTimeRange",24)              !*lenght of timerange over which average is evaluated
  CALL grib_set(ig,"indicatorOfUnitForTimeIncrement",1) ! 1=hour
  CALL grib_set(ig,"timeIncrement",1)                   ! time increment betwwen successive fields
ENDIF

! 2.5 Section 5 (data representation)
CALL grib_set(ig,"numberOfValues",ni*nj)
CALL grib_set(ig,"dataRepresentationTemplateNumber",0)  ! grid point data
CALL grib_set(ig,"bitsPerValue",16)
CALL grib_set(ig,"typeOfOriginalFieldValues",0)         ! original data are Floating

! Scrivo i dati (tutti mancanti)
CALL grib_set(ig,"bitMapIndicator",0)                   ! 0=bitmap present, 255=bitmap not present
CALL grib_set(ig,"missingValue",rmis)
ALLOCATE (values(ni*nj))
values(:) = rmis
CALL grib_set(ig,"values",values(:))

!-------------------------------------------------------------------------------
! 3) Scrivo il grib

CALL grib_open_file(ifile,"utm_grib2.tmpl","w",iret)
CALL grib_write(ig,ifile,iret)
CALL grib_close_file(ifile,iret)

STOP

END PROGRAM crea_template_grib2_utm

! Altre keys presenti nel programma di EZini la cui utilita' e' ignota

!CALL grib_set(ig,"binaryScaleFactor")   ! omettere?
!CALL grib_set(ig,"decimalScaleFactor")  ! omettere?
!CALL grib_set(ig,"referenceValue")      ! omettere?

!CALL grib_set(ig,"truncateLaplacian",0)
!CALL grib_set(ig,"truncateDegrees",0)
!CALL grib_set(ig,"dummy",1)	   
!CALL grib_set(ig,"changingPrecision",0)
!CALL grib_set(ig,"unitsFactor",1)
!CALL grib_set(ig,"unitsBias",0)  
!CALL grib_set(ig,"eps",0)	   
!CALL grib_set(ig,"timeRangeIndicatorFromStepRange",-1)


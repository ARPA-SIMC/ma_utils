PROGRAM geo_dat2grib
!--------------------------------------------------------------------------
! Legge un file GEO.DAT calmet e lo riscrive nel formato GRIB2-SIMC
! Sequenza dei campi in GEO.DAT: 
! Luse, Orog, Z0, Albedo, Bowen Ratio, SHF, Ant.heat flux, LAI
!
!                                          Versone 1.0.0, Enrico 16/01/2012
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
IMPLICIT NONE

INTEGER :: ifout,igout,iret,kg
CHARACTER(LEN=500) :: filein,fileout

REAL, ALLOCATABLE :: field(:)
REAL :: dx,dy,x1,y1,x2,y2
INTEGER :: nx,ny,k,k1,kpar,utmz,j
INTEGER, PARAMETER :: par(8) = (/121,122,123,124,125,126,127,128/)

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help") THEN
  WRITE (*,*) "Uso: geo_dat2grib.exe filein fileout"
  STOP
ENDIF

! Apro GEO.DAT, leggo header, alloco array
OPEN (UNIT=30, FILE=filein, STATUS="OLD", ACTION="READ", ERR=9999)
READ (30,*,ERR=9998)
READ (30,*,ERR=9998) nx,ny,dx,x1,y1,utmz
print *,nx,ny,dx,x1,y1,utmz
ALLOCATE (field(nx*ny))

! Calcolo estremi griglia; passo da bordo cella a centro cella
dy = dx
x1 = x1 + dx/2.
y1 = y1 + dy/2.
x2 = x1 + REAL(nx-1) * dx
y2 = y1 + REAL(ny-1) * dy

! Apro fileout
ifout = 0
igout = 0
CALL grib_open_file(ifout,fileout,"w",iret)

!--------------------------------------------------------------------------
! 2) Leggo geo.dat, scrivo fileout

DO kpar = 1,8

! 2.1 Leggo i dati
  field(:) = rmiss
  READ (30,*,ERR=9997)
  DO j = 1,ny
    k1 = (ny-j) * nx + 1
    READ (30,*,ERR=9997) field(k1:k1+nx-1)
  ENDDO
  WRITE (*,*) " par ",par(kpar)," ave ",SUM(field(1:nx*ny))/REAL(nx*ny)

! 2.2 Creo un nuovo messaggio GRIB2-UTM
  CALL grib_new_from_samples(igout,"utm_grib2",iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9996

! 2.3 Assegno le chiavi
  CALL grib_set(igout,"centre",200)
  CALL grib_set(igout,"significanceOfReferenceTime",0)
  CALL grib_set(igout,"year",2000)
  CALL grib_set(igout,"month",1)                             
  CALL grib_set(igout,"day",1)                               
  CALL grib_set(igout,"hour",0)                              
  CALL grib_set(igout,"typeOfProcessedData",0)

  CALL grib_set(igout,"sourceOfGridDefinition",0)            
  CALL grib_set(igout,"Ni",nx)
  CALL grib_set(igout,"Nj",ny)
  CALL grib_set(igout,"numberOfDataPoints",nx*ny)            
  CALL grib_set(igout,"iDirectionIncrementGiven",1)
  CALL grib_set(igout,"jDirectionIncrementGiven",1)
  CALL grib_set(igout,"iDirectionIncrement",dx*1000.)
  CALL grib_set(igout,"jDirectionIncrement",dy*1000.)
  CALL grib_set(igout,"scanningMode",64)
  CALL grib_set(igout,"zone",utmz)
  CALL grib_set(igout,"falseEasting",500000)
  CALL grib_set(igout,"eastingOfFirstGridPoint",x1*1000.)
  CALL grib_set(igout,"eastingOfLastGridPoint",x2*1000.)
  CALL grib_set(igout,"northingOfFirstGridPoint",y1*1000.)
  CALL grib_set(igout,"northingOfLastGridPoint",y2*1000.)

  CALL grib_set(igout,"productDefinitionTemplateNumber",0)
  CALL grib_set(igout,"parameterCategory",200)
  CALL grib_set(igout,"parameterNumber",par(kpar))
  CALL grib_set(igout,"typeOfGeneratingProcess",0)
  CALL grib_set(igout,"generatingProcessIdentifier",1)
  CALL grib_set(igout,"indicatorOfUnitOfTimeRange",1)
  CALL grib_set(igout,"forecastTime",0)

  CALL grib_set(igout,"typeOfFirstFixedSurface",1)
  CALL grib_set(igout,"scaleFactorOfFirstFixedSurface",0)
  CALL grib_set(igout,"scaledValueOfFirstFixedSurface",0)

  CALL grib_set(igout,"numberOfValues",nx*ny)

  IF (.NOT. ALL(c_e(field(:)))) THEN
    CALL grib_set(igout,"bitMapIndicator",0)
    CALL grib_set(igout,"missingValue",rmiss)
  ELSE  
    CALL grib_set(igout,"bitMapIndicator",255)
  ENDIF
  CALL grib_set(igout,"bitsPerValue",24)
  CALL grib_set(igout,"values",field(1:nx*ny))

! 2.4 Scrivo il grib
  CALL grib_write (igout,ifout)

ENDDO

CLOSE (30)
STOP

!--------------------------------------------------------------------------
! 3 Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo header ",TRIM(filein)
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo dati ",TRIM(filein)
STOP

9996 CONTINUE
WRITE (*,*) "Errore aprendo template GRIB2 UTM"
STOP

END PROGRAM geo_dat2grib

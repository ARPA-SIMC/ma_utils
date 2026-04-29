PROGRAM grib2latlon
!--------------------------------------------------------------------------
! Legge il primo grib di un file, e scrive 2 grib con latitudini e 
! longitudini dei punti della griglia. Evoluzione di gribex2latlon.f90
!
!                                         Versione 2.0.0, Enrico 10/03/2026
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

INTEGER :: ifin,ifout,igin=0,iglat=0,iglon=0,iret
CHARACTER(LEN=200) :: filein,fileout

REAL, ALLOCATABLE :: out_lat(:),out_lon(:)
REAL :: alatf,alonf,alatl,alonl,dx,dy
INTEGER :: latf,lonf,latl,lonl,en,gdtn,ni,nj,sm,i,j,k

!--------------------------------------------------------------------------
! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help") THEN
  WRITE (*,*) "Uso: grib2latlon.exe filein fileout"
  STOP
ENDIF

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

! Leggo il primo campo
CALL grib_new_from_file(ifin,igin,iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998

! Leggo i dati della griglia

CALL grib_get(igin,"editionNumber",en)
IF (en /= 2) GOTO 9997

CALL grib_get(igin,"gridDefinitionTemplateNumber",gdtn)
IF (gdtn /= 0) GOTO 9996

CALL grib_get(igin,"scanningMode",sm)
IF (sm /= 0 .AND. sm /= 64) GOTO 9995

IF (en == 2) THEN
   CALL grib_get(igin,"Ni",ni)
   CALL grib_get(igin,"Nj",nj)
   CALL grib_get(igin,"latitudeOfFirstGridPoint",latf)
   CALL grib_get(igin,"longitudeOfFirstGridPoint",lonf)
   CALL grib_get(igin,"latitudeOfLastGridPoint",latl)
   CALL grib_get(igin,"longitudeOfLastGridPoint",lonl)
ENDIF

ALLOCATE (out_lat(ni*nj),out_lon(ni*nj))

alatf = REAL(latf) / 1000000.
alonf = REAL(lonf) / 1000000.
alatl = REAL(latl) / 1000000.
alonl = REAL(lonl) / 1000000.

dy = ABS(alatl-alatf)/REAL(nj-1)
dx = ABS(alonl-alonf)/REAL(ni-1)
WRITE (*,*) "dx, dy: ",dx,dy

CALL grib_clone(igin,iglat)
CALL grib_clone(igin,iglon)

IF (sm == 64) THEN
  DO k = 1,ni*nj
    i = MOD((k-1), ni) + 1
    j = (k-1)/ni + 1
    out_lon(k) = alonf + (i-1)*dx 
    out_lat(k) = alatf + (j-1)*dy 
  ENDDO

ELSE IF (sm == 0) THEN
  DO k = 1,ni*ni
    i = MOD((k-1), ni) + 1
    j = nj - (k-1)/ni
    out_lon(k) = lonf + (i-1)*dx 
    out_lat(k) = latl + (j-1)*dy 
  ENDDO

ENDIF

WRITE (*,*) "Lat: min, max ",MINVAL(out_lat(:)),MAXVAL(out_lat(:))
WRITE (*,*) "Lon: min, max ",MINVAL(out_lon(:)),MAXVAL(out_lon(:))
  
! Scrivo output
CALL grib_set(iglat,"values",out_lat(:))
CALL grib_set(iglat,"discipline",0)
CALL grib_set(iglat,"parameterCategory",191)
CALL grib_set(iglat,"parameterNumber",1)
CALL grib_write(iglat,ifout)


CALL grib_set(iglon,"values",out_lon(:))
CALL grib_set(iglon,"discipline",0)
CALL grib_set(iglon,"parameterCategory",191)
CALL grib_set(iglon,"parameterNumber",2)
CALL grib_write(iglon,ifout)

! Libero memoria
CALL grib_release(igin)
CALL grib_release(iglat)
CALL grib_release(iglon)

STOP

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP

9997 CONTINUE
WRITE (*,*) "GRIB editon non gestita ",en
STOP

9996 CONTINUE
WRITE (*,*) "gridDefinitionTemplateNumber non gestito ",gdtn
STOP

9995 CONTINUE
WRITE (*,*) "Scanning mode non gestito ",sm
STOP

END PROGRAM grib2latlon

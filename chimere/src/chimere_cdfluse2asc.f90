PROGRAM chimere_cdfluse2asc
!--------------------------------------------------------------------------
! Legge un file NetCDF del landuse Chimere e scrive i vecchi file ASCII 
! COORD e LANDUSE
! Uso: chimerencdf2grib.exe filein
!                                           Versione 1.1, Enrico 12/02/2025
!--------------------------------------------------------------------------

USE netcdf

IMPLICIT NONE

INTEGER :: kp,idp,i,j,kl
INTEGER :: ncid,dimid1,dimid2,ncstat,ncstat1,ncstat2,ivarid
INTEGER :: nzonal,nmerid

REAL, ALLOCATABLE :: luse(:,:,:),luset(:,:)
REAL, ALLOCATABLE :: buf2d(:,:),lat(:,:),lon(:,:)

CHARACTER (LEN=23), PARAMETER :: luse_str(9) = (/ &
!  12345678901234567890123
  "Agricultural_land_crops", &
  "Grassland              ", &
  "Barren_land_bare_ground", &
  "Inland_Water           ", &
  "Urban                  ", &
  "Shrubs                 ", &
  "Needleaf_forest        ", &
  "Broadleaf_forest       ", &
  "Ocean                  " &
  /)

CHARACTER (LEN=120) :: filein,chdum,arg(2)
CHARACTER (LEN=1) :: next_arg

!--------------------------------------------------------------------------
! Parametri da riga comando
next_arg = ""
idp = 0
DO kp = 1,HUGE(kp)
   CALL getarg(kp,chdum)
   IF (TRIM(chdum) == "-h") THEN
     CALL write_help
     STOP 1
   ELSE IF (TRIM(chdum) == "") THEN  
     EXIT
   ELSE
     idp = idp + 1
     arg(idp) = chdum
   ENDIF
ENDDO
filein = arg(1)

!--------------------------------------------------------------------------
! Leggo dal file NetCDF le dimensioni della matrice dei dati (x,y,z,t,var)

! Apro il file
ncstat=nf90_open(filein,NF90_NOWRITE,ncid)

! Nx
ncstat1 = nf90_inq_dimid(ncid,'west_east',dimid1)
ncstat2 = nf90_inq_dimid(ncid,'longitude',dimid2)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=nzonal)
ELSE IF (ncstat2 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid2,len=nzonal)
ELSE
  WRITE (*,*) "N.ro celle in direzione X non presente nel file NetCDF"
ENDIF

! Ny
ncstat1 = nf90_inq_dimid(ncid,'south_north',dimid1)
ncstat2 = nf90_inq_dimid(ncid,'latitude',dimid2)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=nmerid)
ELSE IF (ncstat2 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid2,len=nmerid)
ELSE
  WRITE (*,*) "N.ro celle in direzione Y non presente nel file NetCDF"
ENDIF

! Alloco le varaibili
ALLOCATE (buf2d(nzonal,nmerid),lat(nzonal,nmerid),lon(nzonal,nmerid))
ALLOCATE (luse(9,nzonal,nmerid),luset(nzonal,nmerid))

!--------------------------------------------------------------------------
! Leggo lat e lon, scrivo il file COORD

ncstat=nf90_inq_varid(ncid,"lon",ivarid)
ncstat=nf90_get_var(ncid,ivarid,buf2d,(/1,1/),(/nzonal,nmerid/))
lon(:,:) = buf2d(:,:)

ncstat=nf90_inq_varid(ncid,"lat",ivarid)
ncstat=nf90_get_var(ncid,ivarid,buf2d,(/1,1/),(/nzonal,nmerid/))
lat(:,:) = buf2d(:,:)

WRITE (*,'(a,2i8)') "Nx,Ny:",nzonal,nmerid
WRITE (*,'(a,4f8.4)') "Estremi coordinate (W,S,E,N) ", &
 MINVAL(lon),MINVAL(lat),MAXVAL(lon),MAXVAL(lat)

OPEN (UNIT=30, FILE="COORD", STATUS="REPLACE", FORM="FORMATTED")
DO j = 1,nmerid
DO i = 1,nzonal
  WRITE (30,'(f8.4,1x,f8.4)') lon(i,j),lat(i,j)
ENDDO
ENDDO
CLOSE(30)

!--------------------------------------------------------------------------
! Leggo i dati, scrivo il file LANDUSE

! Leggo i dati
DO kl = 1,9
  ncstat=nf90_inq_varid(ncid,luse_str(kl),ivarid)
  ncstat=nf90_get_var(ncid,ivarid,buf2d,(/1,1/),(/nzonal,nmerid/))
  luse(kl,:,:) = buf2d(:,:)
ENDDO

! check
luset(:,:) = SUM(luse(1:9,:,:),DIM=1)
WRITE (*,'(a,2f8.5)') "Check chiusura luse (min, max; expected 1.)", &
  MINVAL(luset),MAXVAL(luset)

! scrivo il file LANDUSE
OPEN (UNIT=31, FILE="LANDUSE", STATUS="REPLACE", FORM="FORMATTED")
DO j = 1,nmerid
DO i = 1,nzonal
  WRITE (31,'(9f7.3)') luse(1:9,i,j)
ENDDO
ENDDO
CLOSE(31)

END PROGRAM chimere_cdfluse2asc

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "Uso: chimere_cdfluse2asc.exe filein"
WRITE (*,*) "filein:   in formato NetCDF Landuse (es. LANDUSE_CORINE_GLOBCOVER_ITA5.nc)"
WRITE (*,*) "Output: files COORD e LANDUSE (ASCII)"
RETURN

END SUBROUTINE write_help

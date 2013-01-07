!+ Module containing definitions and functions about gridded data
! 
MODULE grid_handler
 
! 
! Description: 
!   Module for handling gridded data inside f90 programs. Does not involve
!   IO, grib, ecc.
!   Writes fatal error messages on stdout and log on unit 88
!
! NOTE: in this module values are defined for real and integer "missing 
!       values" (rmis,imis); the user can change them in its program, but 
!       their definition is required by most module routines.
! 
! Current Code Owner: Enrico
! 
! History: 
!  
! Version   Date         Comment 
! -------   ----         ------- 
! 1         27/04/2001   initial version
! 2         05/04/2002   added UTM grids
! 2.1       12/07/2004   bug fix in SUBR. build_grid
! 2.2       08/08/2005   added subroutine antirot_wind 
! 2.3       07/03/2007   bugs fixed in SUBR. interp_scalar
! 2.4       11/06/2009   added interpolation rule "A" (sub. interp_scalar)
! 2.4.1     12/09/2012   reduced value of maxdim (with F16/gfortran programs 
!                        did not run: "segfault")
! 
! Module index:
!
!- parametrer definitions           : maximum size of grids, missing value...
!- type definition "grid"           : a grid; can be lat-lon (regular or 
!                                     rotated) or UTM
!- SUB. assign_grid (grid1,grid2)   : grid assignement
!- FUN. same_grid (grid1,grid2)     : .T. if grid2 and grid2 are defined on
!                                     the same grid
!- SUB. print_grid(grid,iunit)      : writes on unit iunit grid description
!                                     and a sample of the filed
!
!- SUB. build_grid(ksec2,field,grid): given ksec2 and field array from 
!                                     gribex, returns a "grid" type variable
!- SUB. expand_grid(grid,ksec2,field): given a "grid" type variable, returns
!                                     ksec2 and field array for grib encoding
!
!- SUB. interp_scalar(grid1,grid2,r): interpolate grid1 on the grid defined
!                                     by grid2, using the rule r.
!- SUB. interp_wind(...)            : interpolate a wind field (grids 1 & 2)
!                                     on the grid defined by grids 3 & 4
!- SUB. antirot_wind(...)           : anti-rotate a wind field (grids 1 & 2)
!
!- SUB. find_best_antir(grid1,grid2): finds the "best" regular grid to an-
!                                     tirot. grid1
!- SUB. transpose_to_010(...)       : given the grib "scanning mode" tran-
!                                     spose an array to the "standard" 010
!- SUB. bin2scan (bin,scan)         : given the value of ksec2(11), returns 
!                                     the 3 gribex "scanning flag"
! FROM HIBULIB (ARPA-SMR, f77)
!
!- SUB. rtlld
!- SUB. tlld
!- SUB. rltlwd
!- SUB. ltlwd
!
!!- End of module header
!==========================================================================
IMPLICIT NONE 

! Maximum size of grid arrays (con F16 e maxdim=1500000, interp_grib esplode)
INTEGER, PARAMETER :: maxdim = 500000

! Conversion from radiants to degrees
REAL, PARAMETER :: dtr = 180./3.1415926

! Conversion from longitude radiants & degrees to km
REAL, PARAMETER :: rad2km = 40000./(2*3.1416)
REAL, PARAMETER :: deg2km = 40000./360.

! missing values for interpolation and grib encoding
REAL  :: rmis = -999.                        ! real missing value
INTEGER  :: imis = -999                      ! integer missing value


TYPE :: grid
!
! Describes a grid, either regular (rotated or not) or UTM.
! In both cases, x1 & y1 refers to the SW grid point (this is different from
! Calmet, where they refer to the SW corner of SW cell)
! Some of the elements of this derived type are used only for one projection,
! i.e. only if the grid is geographic or utm.
!
! The values are always arranged according to the "scanning flag" 010, 
! corresponding to a points arrangement:
!
!      3 4
!      1 2
!
! If the grib is arranged differently, call subroutine "transpose_to_010"
! prior to defining or assigning the derived type variable.
!
  CHARACTER(LEN=3) :: proj              ! 'UTM' or 'GEO'
  INTEGER :: nx
  INTEGER :: ny
  REAL :: dx                            ! deg or km
  REAL :: dy                            ! deg or km
  REAL :: x1                            ! deg or km
  REAL :: y1                            ! deg or km
  REAL :: xrot                          ! only for GEO grid
  REAL :: yrot                          ! only for GEO grid
  INTEGER :: utmz                       ! only for UTM grid
  REAL :: field(maxdim)

END TYPE grid

!==========================================================================
! Operator definitions: 

INTERFACE ASSIGNMENT (=)
  MODULE PROCEDURE assign_grid
END INTERFACE

INTERFACE OPERATOR (.samegrid.)
  MODULE PROCEDURE same_grid
END INTERFACE

!==========================================================================
CONTAINS 
! 

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE assign_grid (grid1,grid2)
!
! Subroutine to extend assignemnt operato to "grid" type variables
!
IMPLICIT NONE

TYPE(grid), INTENT(OUT) :: grid1
TYPE(grid), INTENT(IN) :: grid2

grid1%proj = grid2%proj
grid1%nx = grid2%nx
grid1%ny = grid2%ny
grid1%dx = grid2%dx
grid1%dy = grid2%dy
grid1%x1 = grid2%x1
grid1%y1 = grid2%y1
grid1%xrot = grid2%xrot
grid1%yrot = grid2%yrot
grid1%utmz = grid2%utmz
grid1%field = grid2%field

RETURN

END SUBROUTINE  assign_grid


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION same_grid (grid1,grid2) RESULT (same)
!
!   Function to test if two "grid" variables are defined on the same grid.
!
TYPE(grid), INTENT(IN) :: grid1
TYPE(grid), INTENT(IN) :: grid2
LOGICAL :: same

! internal variables
REAL :: eps = EPSILON(0.) 

!

same = .FALSE.

! Controllo se sono due griglie geografiche uguali
IF ( grid1%proj=='GEO' .AND. grid2%proj=='GEO' .AND. &
     grid1%nx == grid2%nx .AND. grid1%ny == grid2%ny .AND. &
     ABS(grid1%dx-grid2%dx) < eps .AND. ABS(grid1%dy-grid2%dy) < eps .AND. &
     ABS(grid1%x1-grid2%x1) < eps .AND. ABS(grid1%y1-grid2%y1) < eps .AND. &
     ABS(grid1%xrot-grid2%xrot) < eps .AND. &
     ABS(grid1%yrot-grid2%yrot) < eps &
   ) same = .TRUE.

! Controllo se sono due griglie utm uguali
IF ( grid1%proj=='UTM' .AND. grid2%proj=='UTM' .AND. &
     grid1%nx == grid2%nx .AND. grid1%ny == grid2%ny .AND. &
     ABS(grid1%dx-grid2%dx) < eps .AND. ABS(grid1%dy-grid2%dy) < eps .AND. &
     ABS(grid1%x1-grid2%x1) < eps .AND. ABS(grid1%y1-grid2%y1) < eps .AND. &
     grid1%utmz == grid2%utmz &
   ) same = .TRUE.

RETURN
END FUNCTION same_grid


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE print_grid (grid1,iunit)
!
! Write un unit iunit some information about grid
!
IMPLICIT NONE

INTEGER, INTENT (IN) :: iunit
TYPE(grid), INTENT(IN) :: grid1

REAL :: ave, max, min
INTEGER :: nok,ntot,np
CHARACTER (LEN=30) :: chproj
!

! Calculation of grid-description parameters
ntot = grid1%nx * grid1%ny
nok = COUNT(grid1%field(1:ntot)/=rmis)

IF (nok > 0) THEN
  ave = SUM(grid1%field(1:ntot), MASK=grid1%field(1:ntot)/=rmis) / &
    float (nok)
ELSE
  ave = rmis
ENDIF

max = MAXVAL(grid1%field(1:ntot), MASK=grid1%field(1:ntot)/=rmis)
min = MINVAL(grid1%field(1:ntot), MASK=grid1%field(1:ntot)/=rmis)

IF (grid1%proj == 'GEO') THEN
  IF (grid1%xrot == 0. .AND. grid1%yrot == 0.) THEN
    chproj = "Lat-Lon non ruotata"
  ELSE
    chproj = "Lat-Lon ruotata"
  ENDIF
ELSE IF (grid1%proj == 'GEO') THEN
    WRITE (chproj,'(a,i2)') "UTM, fuso ",grid1%utmz
ELSE
    chproj = grid1%proj
ENDIF

! Output of grid characteristics
WRITE (iunit,*) 
WRITE (iunit,*) 'Griglia:'
WRITE (iunit,*) 'Tipo:      ',TRIM(chproj)
WRITE (iunit,*) 'nx,ny:     ',grid1%nx,grid1%ny
WRITE (iunit,*) 'dx,dy:     ',grid1%dx,grid1%dy
WRITE (iunit,*) 'x1,y1:     ',grid1%x1,grid1%y1
IF (grid1%proj=='GEO') WRITE (iunit,*) 'xrot,yrot: ',grid1%xrot,grid1%yrot

np = grid1%nx*grid1%ny
WRITE (iunit,*) 
WRITE (iunit,*) 'Campo:'
WRITE (iunit,*) 'Tot,buoni,ave,max,min: ',ntot,nok,ave,max,min
WRITE (iunit,*) 'primo, ultimo:   ',grid1%field(1),grid1%field(np)
WRITE (iunit,*) 'primi 20 valori:'
WRITE (iunit,*) grid1%field(1:20)
WRITE (iunit,*) 'primi valori multipli di 100 (punti: 100,200... 2000):'
IF (np >= 2000) THEN
  WRITE (iunit,*) grid1%field(100:2000:100)
ELSE
  WRITE (iunit,*) grid1%field(100:np:100)
ENDIF

RETURN

END SUBROUTINE print_grid


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE build_grid(ksec2,inp_field,out_grid)
!
!Subroutine to build a "grid" type variable, given ksec2 and inp_field 
! array from gribex.
! The %field component of the grid value will contain inp_field values
! transposed according to the "standard" scanning flags
! If the coordinates of all grid points are legal lat-lon coordinates 
! treats the grid as GEO, else considers it UTM.
!
IMPLICIT NONE

! subroutine arguments
INTEGER, INTENT(IN) :: ksec2(:)
REAL, INTENT(IN)    :: inp_field(maxdim)
TYPE(grid), INTENT(OUT) :: out_grid

!local parameters
INTEGER, PARAMETER :: def_utmz = 32

!local varibles
REAL    :: fieldt(maxdim)
REAL    :: dx, dy, x1, y1, x2, y2, xrot, yrot
INTEGER :: dim(1), nx, ny, scan(3), utmz
CHARACTER(LEN=3) :: proj

!--------------------------------------------------------------------------
! 1) check if ksec2 and maxdim are big enough
dim = SHAPE(ksec2)

IF (dim(1) < 14) THEN
  WRITE (*,*) "Error building grid: ksec2 is too small (",dim(1),")"
  STOP
ENDIF

IF (ksec2(2)*ksec2(3) > maxdim) THEN
  WRITE (*,*) "Error: decoded a grib with ",ksec2(2)*ksec2(3),"elements,"
  WRITE (*,*) "larger than maximum allowed (",maxdim,") : change parameter"
  WRITE (*,*) "maxdim in grid_module.f90"
  STOP
ENDIF

!--------------------------------------------------------------------------
! 2) extract grid parameters from ksec2

nx = ksec2(2)
ny = ksec2(3)

CALL bin2scan (ksec2(11),scan)
IF (scan(1) == 0) THEN
  x1 = ksec2(5) / 1000.
ELSE
  x1 = ksec2(8) / 1000.
ENDIF
IF (scan(2) == 1) THEN
  y1 = ksec2(4) / 1000.
ELSE
  y1 = ksec2(7) / 1000.
ENDIF

IF (ksec2(6) == 0) THEN               ! grid spacing not given
  dx = ABS(ksec2(8)/1000. - ksec2(5)/1000.) / FLOAT(nx-1)
  dy = ABS(ksec2(7)/1000. - ksec2(4)/1000.) / FLOAT(ny-1)
ELSE
  dx = ksec2(9) / 1000. 
  dy = ksec2(10) / 1000.
ENDIF

x2 = x1 + (nx-1)*dx
y2 = y1 + (ny-1)*dy

!--------------------------------------------------------------------------
! 3) detects proj. type and set grid parameters depending on it

IF (ksec2(1) == 0) THEN

  IF (ABS(x1) <= 360. .AND. ABS(x2) <= 360. .AND. &
      ABS(y1) <= 90.  .AND. ABS(y2) <= 90. ) THEN  ! geo, non-rot grid
    proj = 'GEO'
    xrot = 0.
    yrot = 0.
    utmz = 0 
 ELSE                                             ! utm grid
    proj = 'UTM'
    xrot = 0.
    yrot = 0.
    utmz = def_utmz
  ENDIf

ELSE IF (ksec2(1) == 10) THEN                      ! geo, rotated grid
  proj = 'GEO'
  xrot = ksec2(14) / 1000.
  yrot = (ksec2(13) / 1000.) + 90.
  utmz = 0
ELSE
  WRITE (*,*) "Error: grid projection ",ksec2(1)," is not handled!"
  STOP  
ENDIF

! transpose array to match the "standard" scanning flags
CALL transpose_to_010 (inp_field,nx,ny,scan,fieldt)

! builds "grid" type variable
out_grid = grid(proj,nx,ny,dx,dy,x1,y1,xrot,yrot,utmz,fieldt)

RETURN
END SUBROUTINE build_grid


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE expand_grid(grid1,ksec2,out_field)
!
!Subroutine to extract from a "grid" type variable the values array and 
! ksec2 for grib encoding. Force parameters in ksec2 to "standard" values
!
IMPLICIT NONE

! function arguments
TYPE(grid), INTENT(IN) :: grid1
INTEGER, INTENT(OUT) :: ksec2(1024)
REAL, INTENT(OUT)    :: out_field(maxdim)

!local variables
REAL :: xlast,ylast

!--------------------------------------------------------------------------

ksec2 = 0

!
! 1: parameters explicitly contained in "grid" type variable
!
ksec2(2) = grid1%nx 
ksec2(3) = grid1%ny
ksec2(9) = NINT (grid1%dx * 1000)
ksec2(10) = NINT (grid1%dy * 1000)
ksec2(5) = NINT (grid1%x1 * 1000)
ksec2(4) = NINT (grid1%y1 * 1000)
ksec2(14) = NINT (grid1%xrot * 1000)
ksec2(13) = NINT ((grid1%yrot - 90.)* 1000)
out_field = grid1%field

!
! 2: other KSEC2 parameters
!

! type of grid
IF (grid1%proj == 'GEO') THEN
  IF (grid1%xrot == 0. .AND. grid1%yrot == 0.) THEN
    ksec2(1) = 0            ! regular grid
  ELSE
    ksec2(1) = 10           ! rotated grid
  ENDIF
ELSE IF (grid1%proj == 'UTM') THEN
    ksec2(1) = 0            ! UTM grid
ENDIF

! force last point coordinates
xlast = grid1%x1 + (grid1%nx-1)*grid1%dx
ylast = grid1%y1 + (grid1%ny-1)*grid1%dy
ksec2(8) =  NINT (xlast * 1000)
ksec2(7) =  NINT (ylast * 1000)

! force explicit writing of grid increments (for GRADS)
ksec2(6) = 128            

! force scanning flags to "standard" values
ksec2(11) = 64            

! force wind components relative to grid North and East
ksec2(19) = 8            


RETURN
END SUBROUTINE expand_grid


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE interp_scalar(grid1,grid2,rule) 
!--------------------------------------------------------------------------
!Subroutine to interpolate grid1 on the grid defined by grid2, using the 
! rule r. The field of grid2 (if any)is overwritten. The "rule" parameter
! describes the interoplation thecnique:
! rule = 'L' : linear interpolation
! rule = 'N' : the nearest point is taken
! rule = 'M' : average of points in each output grid box (if there isn't
!              any, the nearest point is taken; if any of the points is 
!              missing, the average is set to missing)
! rule = 'A' : average of points in each output grid box (if there isn't
!              any, the nearest point is taken; if any of the points is 
!              missing, takes the average of vaild points)
! 
! Handle two rotated grids with different centre of rotation and UTM grids
!--------------------------------------------------------------------------
IMPLICIT NONE

TYPE (grid), INTENT(IN) :: grid1
TYPE (grid), INTENT(INOUT) :: grid2
CHARACTER (LEN=1), INTENT(IN) :: rule

! local variables
REAL :: xgrid2,ygrid2,xgrid1,ygrid1,rigrid1,rjgrid1,xgeo,ygeo
REAL :: cyrot1,syrot1,cyrot2,syrot2
REAL :: z1,z2,z3,z4,xp,yp,zp
REAL :: dx1_km,dx2_km,dy1_km,dy2_km, max_lat
REAL :: box_ratio,boundary,ave,avg
INTEGER :: i,j,k,igrid1,jgrid1,igrid2,jgrid2
INTEGER :: kgrid1,kgrid2,k1g1,k2g1,k3g1,k4g1
INTEGER :: cntok,nok,nmis,nin,nout,nmax,nmin
INTEGER :: iz

INTEGER :: n_att(grid2%nx,grid2%ny),mx_att
INTEGER, ALLOCATABLE :: att_points(:,:,:)

!==========================================================================

!--------------------------------------------------------------------------
! 1: Test and preliminary calculations
!

! test if maxdim is big enough for output grid
IF (grid2%nx*grid2%ny > maxdim) THEN
  WRITE (*,*) "Error: try to interpolate on a grib with ",grid2%nx*grid2%ny
  WRITE (*,*) "elements, maximum allowed is ",maxdim," :"
  WRITE (*,*) "change parameter maxdim in grid_module.f90"
  STOP
ENDIF

! test if projs are allowed
IF ((grid1%proj /= 'GEO' .AND. grid1%proj /= 'UTM') .OR. &
    (grid2%proj /= 'GEO' .AND. grid2%proj /= 'UTM')) THEN
  WRITE (*,'(3(a,1x))') "Errore interp_scalar: proj illegali:", &
    grid1%proj,grid2%proj
  STOP
ENDIF

! sin/cos of centres of rotation
cyrot1 = cos( grid1%yrot/dtr )
syrot1 = sin( grid1%yrot/dtr )
cyrot2 = cos( grid2%yrot/dtr )
syrot2 = sin( grid2%yrot/dtr )

!--------------------------------------------------------------------------
! 2: If "mean interpolation" is required, it is first necessary to 
!    attribute every point of grid1 to the corresponding grid cell of grid2
!    Later, grid cells with no corresponding points in grid1 will be 
!    assigned the value of the nearest point
!

IF (rule == 'M' .OR. rule == 'A') THEN

!----
! 2.1 find max. n.er of points of grid1 which could fall in one box of 
!     grid2

! min grid spacings of grid1, in km
  IF (grid1%proj=="GEO") THEN
    max_lat = MAX( ABS(grid1%y1), ABS(grid1%y1+(grid1%ny-1)*grid1%dy) )
    dx1_km = grid1%dx * deg2km * COS(max_lat * 3.14159 / 180.)
    dy1_km = grid1%dy * deg2km
  ELSE IF (grid1%proj=="UTM") THEN
    dx1_km = grid1%dx
    dy1_km = grid1%dy
  ENDIF

! max grid spacings of grid2, in km
  IF (grid2%proj=="GEO") THEN
    dx2_km = grid2%dx * deg2km
    dy2_km = grid2%dy * deg2km
  ELSE IF (grid2%proj=="UTM") THEN
    dx2_km = grid2%dx
    dy2_km = grid2%dy
  ENDIF

! estimate max n.er of points. 
! NB: add 10 to have a margin, particulary if dealing with similar resolution
  box_ratio = (dx2_km * dy2_km) / (dx1_km * dy1_km) 
  boundary = (dx2_km / dx1_km + dy2_km / dy1_km) * 2
  mx_att = INT (4. + boundary + box_ratio) + 10
  ALLOCATE (att_points(grid2%nx,grid2%ny,mx_att))

!----
! 2.2 Loop to attribute points in grid1

!initialisations
  n_att = 0
  att_points = 0

  DO i = 1,grid1%nx
  DO j = 1,grid1%ny

! position of current point in array grid1%field
    kgrid1 = (j-1)*grid1%nx + i

! find geographical coord. of current point of grid1
    xgrid1 = grid1%x1 + (i-1) * grid1%dx
    ygrid1 = grid1%y1 + (j-1) * grid1%dy

    IF (grid1%proj == "GEO") THEN
      CALL rtlld(xgrid1,ygrid1,grid1%xrot,cyrot1,syrot1,xgeo,ygeo)
    ELSE IF (grid1%proj == "UTM") THEN
      CALL utm2ll(xgrid1,ygrid1,grid1%utmz,ygrid1<0.,ygeo,xgeo)
    ENDIF

! find indexes of grid box of grid2 containing the current point of grid1 
    IF (grid2%proj == "GEO") THEN
      CALL tlld(xgeo,ygeo,grid2%xrot,cyrot2,syrot2,xgrid2,ygrid2)
    ELSE IF (grid2%proj == "UTM") THEN
      CALL ll2utm(ygeo,xgeo,grid2%utmz,xgrid2,ygrid2,iz)
    ENDIF

    igrid2 = NINT ((xgrid2-grid2%x1) / grid2%dx) + 1
    jgrid2 = NINT ((ygrid2-grid2%y1) / grid2%dy) + 1

    IF (igrid2 < 1 .OR. igrid2 > grid2%nx .OR. &
        jgrid2 < 1 .OR. jgrid2 > grid2%ny ) THEN
      nout = nout + 1
    ELSE
      nin = nin + 1
      n_att(igrid2,jgrid2) = n_att(igrid2,jgrid2) + 1
      IF (n_att(igrid2,jgrid2)+1 > mx_att) THEN
        WRITE (*,*) "Errore interp_scalar, mx_att risulta troppo piccolo: "
        WRITE (*,*) "Richiesti: ",n_att(igrid2,jgrid2)+1," max ",mx_att
        STOP
      ENDIF
      att_points(igrid2,jgrid2,n_att(igrid2,jgrid2)) = kgrid1
    ENDIF

  ENDDO
  ENDDO

!----
! 2.3 Statistics on points attribution

  nok  = COUNT(n_att > 0)
  nmis = COUNT(n_att == 0)
  nmax = MAXVAL(n_att)
  nmin = MINVAL(n_att)
  ave = FLOAT(SUM(n_att)) / FLOAT(nok)

  WRITE (88,*) 'Interp_scalar, mean interpolation, points attribution:'
  WRITE (88,*) '  input grid : tot, in, out  ',grid1%nx*grid1%ny,nin,nout
  WRITE (88,*) '  output grid: tot, ok, miss ',grid2%nx*grid2%ny,nok,nmis 
  WRITE (88,*) '  points assigned to each box: max,min,ave,thresh.', &
                nmax,nmin,ave,mx_att

ENDIF

!--------------------------------------------------------------------------
! 3: Interpolation. Performs a loop over points of grid2
!
nok = 0
nmis = 0

DO i = 1,grid2%nx
points2: DO j = 1,grid2%ny

! position of current point in array grid2%field
  kgrid2 = (j-1)*grid2%nx + i

! find geographical coord. of current point of grid2
  xgrid2 = grid2%x1 + (i-1) * grid2%dx
  ygrid2 = grid2%y1 + (j-1) * grid2%dy

  IF (grid2%proj == "GEO") THEN
    CALL rtlld(xgrid2,ygrid2,grid2%xrot,cyrot2,syrot2,xgeo,ygeo)
  ELSE IF (grid2%proj == "UTM") THEN
    CALL utm2ll(xgrid2,ygrid2,grid2%utmz,ygrid2<0.,ygeo,xgeo)
  ENDIF

! find coord. in grid1 of current point of grid2 
  IF (grid1%proj == "GEO") THEN
    CALL tlld(xgeo,ygeo,grid1%xrot,cyrot1,syrot1,xgrid1,ygrid1)
  ELSE IF (grid1%proj == "UTM") THEN
    CALL ll2utm(ygeo,xgeo,grid1%utmz,xgrid1,ygrid1,iz)
  ENDIF

  rigrid1 = (xgrid1-grid1%x1) / grid1%dx
  rjgrid1 = (ygrid1-grid1%y1) / grid1%dy

  SELECT CASE (rule)

!---------- Mean interpolation (box with missing set to missing) ---------
  CASE ('M')

    IF (n_att(i,j) > 0) THEN     ! there are points to average

      avg = 0.
      DO k = 1,n_att(i,j)
        IF (grid1%field( att_points(i,j,k) ) == rmis) THEN
          grid2%field(kgrid2) = rmis
          nmis = nmis + 1
          CYCLE points2
        ENDIF
        avg = avg + grid1%field( att_points(i,j,k) )
      ENDDO
      grid2%field(kgrid2) = avg / FLOAT(n_att(i,j))
      nok = nok + 1
      
    ELSE                         ! no points in box, switch to nearest point

!     locate nearest point of grid1
      igrid1 = NINT(rigrid1) + 1
      jgrid1 = NINT(rjgrid1) + 1

!     if I fall oustide grid1, set grid2 to missing value
      IF (igrid1 < 1 .OR. igrid1 > grid1%nx .OR. &
          jgrid1 < 1 .OR. jgrid1 > grid1%ny ) THEN
        grid2%field(kgrid2) = rmis
        nmis = nmis + 1
        CYCLE points2
      ENDIF

!     set grid2 to the value of nearest point in grid1
      kgrid1 = (jgrid1-1)*grid1%nx + igrid1
      grid2%field(kgrid2) = grid1%field(kgrid1)
      nok = nok + 1

    ENDIF
!---------- Mean interpolation (use all valid points) --------------------
  CASE ('A')

    IF (n_att(i,j) > 0) THEN     ! there are points to average

      avg = 0.
      cntok = 0
      DO k = 1,n_att(i,j)
        IF (grid1%field( att_points(i,j,k) ) /= rmis) THEN
          avg = avg + grid1%field( att_points(i,j,k) )
          cntok = cntok + 1
        ENDIF
      ENDDO

      IF (cntok > 0) THEN
        grid2%field(kgrid2) = avg / FLOAT(cntok)
        nok = nok + 1
      ELSE
        grid2%field(kgrid2) = rmis
        nmis = nmis + 1
      ENDIF
      
    ELSE                         ! no points in box, switch to nearest point

!     locate nearest point of grid1
      igrid1 = NINT(rigrid1) + 1
      jgrid1 = NINT(rjgrid1) + 1

!     if I fall oustide grid1, set grid2 to missing value
      IF (igrid1 < 1 .OR. igrid1 > grid1%nx .OR. &
          jgrid1 < 1 .OR. jgrid1 > grid1%ny ) THEN
        grid2%field(kgrid2) = rmis
        nmis = nmis + 1
        CYCLE points2
      ENDIF

!     set grid2 to the value of nearest point in grid1
      kgrid1 = (jgrid1-1)*grid1%nx + igrid1
      grid2%field(kgrid2) = grid1%field(kgrid1)
      nok = nok + 1

    ENDIF

!----------  Nearest grid point interpolation ----------------------------
  CASE ('N')

!   locate nearest point of grid1 and assign its value to current grid2 point
    igrid1 = NINT(rigrid1) + 1
    jgrid1 = NINT(rjgrid1) + 1

!   if I fall oustide grid1, set grid2 to missing value
    IF (igrid1 < 1 .OR. igrid1 > grid1%nx .OR. &
        jgrid1 < 1 .OR. jgrid1 > grid1%ny ) THEN
      grid2%field(kgrid2) = rmis
      nmis = nmis + 1
      CYCLE points2
    ENDIF

!   set grid2
    kgrid1 = (jgrid1-1)*grid1%nx + igrid1
    grid2%field(kgrid2) = grid1%field(kgrid1)
    nok = nok + 1

!----------  Linear interpolation ----------------------------------------
  CASE ('L') 

!   locate lower left point of grid1 square surrounding current g2 point 
    igrid1 = INT(rigrid1) + 1
    jgrid1 = INT(rjgrid1) + 1

!   if I fall oustide grid1, set grid2 to missing value
    IF (igrid1 < 1 .OR. igrid1+1 > grid1%nx .OR. &
        jgrid1 < 1 .OR. jgrid1+1 > grid1%ny ) THEN
      grid2%field(kgrid2) = rmis
      nmis = nmis + 1
      CYCLE points2
    ENDIF

!   find indexes and values of surrounding points
    k1g1 = (jgrid1-1)*grid1%nx + igrid1
    k2g1 = (jgrid1-1)*grid1%nx + igrid1 + 1
    k3g1 = jgrid1*grid1%nx + igrid1 + 1
    k4g1 = jgrid1*grid1%nx + igrid1

    z1 = grid1%field(k1g1)
    z2 = grid1%field(k2g1)
    z3 = grid1%field(k3g1)
    z4 = grid1%field(k4g1)

    xp = rigrid1 - INT(rigrid1)
    yp = rjgrid1 - INT(rjgrid1)

    IF (z1/=rmis .AND. z2/=rmis .AND. z3/=rmis .AND. z4/=rmis) THEN  
      CALL hbilin (z1,z2,z3,z4, 0.,0.,1.,1., xp,yp,zp)
      grid2%field(kgrid2) = zp
      nok = nok +1
    ELSE
      grid2%field(kgrid2) = rmis
      nmis = nmis + 1
    ENDIF

!----------  End interpolation section ----------------------------------------
  CASE DEFAULT
    WRITE (*,*) "Errore intrerp_scalar: regola di interpolazione non "// &
                "gestita ",rule
  END SELECT

ENDDO points2
ENDDO

IF (rule == 'M') THEN
  DEALLOCATE(att_points)
ENDIF

WRITE (88,*) 'interp_scalar: rule,nok,nims,tot ',rule,' ',nok,nmis, &
             grid2%nx*grid2%ny
WRITE (88,*)

END SUBROUTINE interp_scalar


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE interp_wind(grid_1u,grid_1v,grid_2u,grid_2v,rule) 
!--------------------------------------------------------------------------
!Subroutine to interpolate the wind field with components grid_1u and 
! grid_1v on the grid defined by grid2u, grid_2v, using the required 
! interpolatiojn rule.
! The fields of grid_2* (if any) are overwritten. 
! rule = 'L' : linear interpolation
! rule = 'N' : the nearest point is taken
! rule = 'M' : average of points in each output grid box (if there isn't
!              any, the nearest point is taken; if any of the points is 
!              missing, the average is set to missing)
! rule = 'A' : average of points in each output grid box (if there isn't
!              any, the nearest point is taken; if any of the points is 
!              missing, takes the average of vaild points)
! 
! Handle geographical (rotatedo or not) and utm grids.
! In UTM projection, wind components are referred to geograph. N and E (no
!   antirotation performed)
!--------------------------------------------------------------------------
IMPLICIT NONE

TYPE (grid), INTENT(IN) :: grid_1u,grid_1v
TYPE (grid), INTENT(INOUT) :: grid_2u,grid_2v
CHARACTER (LEN=1), INTENT(IN) :: rule

! local variables

TYPE (grid) :: grid_1u_antir, grid_1v_antir
REAL :: cyrot1,syrot1,cyrot2,syrot2
REAL :: xgrid1,ygrid1,xgrid2,ygrid2
REAL :: xgeo,ygeo,geou(maxdim),geov(maxdim),rotu(maxdim),rotv(maxdim)
INTEGER :: i,j,k

!--------------------------------------------------------------------------
! 1: if u and v components are not defined on the same grid, interpolate
!    separatly and returns
!

IF ((grid_1u .samegrid. grid_1v) .AND. (grid_2u .samegrid. grid_2v)) THEN
ELSE

  WRITE (88,'(a)') "*** interp_wind: components are defined on ", &
                   "different grids, interpolate separatley"

  CALL interp_scalar(grid_1u,grid_2u,rule) 
  CALL interp_scalar(grid_1v,grid_2v,rule) 

  RETURN
ENDIF

!--------------------------------------------------------------------------
! 2: preliminary calculations
!

cyrot1 = cos( grid_1u%yrot/dtr )
syrot1 = sin( grid_1u%yrot/dtr )
cyrot2 = cos( grid_2u%yrot/dtr )
syrot2 = sin( grid_2u%yrot/dtr )

!--------------------------------------------------------------------------
! 3: antirotate wind in grid1
!
IF (grid_1u%proj == "GEO") THEN

  DO i = 1,grid_1u%nx
  DO j = 1,grid_1v%ny

! position of current point grid1
    k = (j-1)*grid_1u%nx + i

! find geographical coord. of current point of grid1
    xgrid1 = grid_1u%x1 + (i-1) * grid_1u%dx
    ygrid1 = grid_1u%y1 + (j-1) * grid_1u%dy
    CALL rtlld(xgrid1,ygrid1,grid_1u%xrot,cyrot1,syrot1,xgeo,ygeo)

! antirotate wind of grid1 (if not missing)
    IF (grid_1u%field(k)/=rmis .AND. grid_1v%field(k)/=rmis) THEN
      CALL rltlwd(xgeo,ygeo,grid_1u%field(k),grid_1v%field(k), &
                  grid_1u%xrot,cyrot1,syrot1,geou(k),geov(k))
    ELSE
      geou(k) = rmis
      geov(k) = rmis
    ENDIF

  ENDDO
  ENDDO

ELSE IF (grid_1u%proj == "UTM") THEN

  geou = grid_1u%field
  geov = grid_1v%field

ENDIF

grid_1u_antir = grid_1u
grid_1v_antir = grid_1v
grid_1u_antir%field = geou
grid_1v_antir%field = geov

!--------------------------------------------------------------------------
! 4: interpolate geogr. (antirotated) wind components from grid1 to grid2
!

CALL interp_scalar(grid_1u_antir,grid_2u,rule) 
CALL interp_scalar(grid_1v_antir,grid_2v,rule) 

!--------------------------------------------------------------------------
! 5: rotate wind to grid2
!

IF (grid_1u%proj == "GEO") THEN

  DO i = 1,grid_2u%nx
  DO j = 1,grid_2v%ny

! position of current point grid2
    k = (j-1)*grid_2u%nx + i

! find geographical coord. of current point of grid2
    xgrid2 = grid_2u%x1 + (i-1) * grid_2u%dx
    ygrid2 = grid_2u%y1 + (j-1) * grid_2u%dy
    CALL rtlld(xgrid2,ygrid2,grid_2u%xrot,cyrot2,syrot2,xgeo,ygeo)

! rotate wind to grid2 (if not missing)
    IF (grid_2u%field(k)/=rmis .AND. grid_2v%field(k)/=rmis) THEN
      CALL ltlwd(xgeo,ygeo,grid_2u%field(k),grid_2v%field(k), &
                 grid_2u%xrot,cyrot2,syrot2,rotu(k),rotv(k))
    ELSE
      rotu(k) = rmis
      rotv(k) = rmis
    ENDIF

  ENDDO
  ENDDO

ELSE IF (grid_1u%proj == "UTM") THEN

  rotu = grid_2u%field
  rotv = grid_2v%field

ENDIF

grid_2u%field = rotu
grid_2v%field = rotv

WRITE (88,*) "wind interp. performed"

RETURN
END SUBROUTINE interp_wind


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE antirot_wind(grid_1u,grid_1v,grid_2u,grid_2v) 
!--------------------------------------------------------------------------
! Subroutine to antirotate wind components grid_1u and grid_1v. 
! Input fields must be defined on the same, geographical-rotated grid (UTM
!   projection not handled yet)
! This is merely a simplified (faster?) version of interp_wind
!--------------------------------------------------------------------------
IMPLICIT NONE

TYPE (grid), INTENT(IN) :: grid_1u,grid_1v
TYPE (grid), INTENT(INOUT) :: grid_2u,grid_2v

! local variables
REAL :: cyrot1,syrot1,cyrot2,syrot2
REAL :: xgrid1,ygrid1,xgrid2,ygrid2
REAL :: xgeo,ygeo,geou(maxdim),geov(maxdim)
INTEGER :: i,j,k

!--------------------------------------------------------------------------
! 1: test on input fields
!

IF (.NOT. (grid_1u .samegrid. grid_1v)) THEN
  WRITE (*,*) "Error antirot_wind: components defined on different grids"
  STOP
ENDIF

IF (grid_1u%proj /= "GEO") THEN
  WRITE  (*,*) "Error antirot_wind: UTM projection not handled (yet)"
  STOP
ENDIF

IF (grid_1u%xrot == 0. .AND. grid_1u%yrot == 0.) THEN
  WRITE (88,*) "Wind antirotation not required"
  grid_2u = grid_1u
  grid_2v = grid_1v
  RETURN
ENDIF

!--------------------------------------------------------------------------
! 2: preliminary calculations
!

cyrot1 = cos( grid_1u%yrot/dtr )
syrot1 = sin( grid_1u%yrot/dtr )
cyrot2 = cos( grid_2u%yrot/dtr )
syrot2 = sin( grid_2u%yrot/dtr )

!--------------------------------------------------------------------------
! 3: antirotate wind 
!

DO i = 1,grid_1u%nx
DO j = 1,grid_1v%ny

! position of current point grid1
  k = (j-1)*grid_1u%nx + i

! find geographical coord. of current point of grid1
  xgrid1 = grid_1u%x1 + (i-1) * grid_1u%dx
  ygrid1 = grid_1u%y1 + (j-1) * grid_1u%dy
  CALL rtlld(xgrid1,ygrid1,grid_1u%xrot,cyrot1,syrot1,xgeo,ygeo)

! antirotate wind of grid1 (if not missing)
  IF (grid_1u%field(k)/=rmis .AND. grid_1v%field(k)/=rmis) THEN
    CALL rltlwd(xgeo,ygeo,grid_1u%field(k),grid_1v%field(k), &
                grid_1u%xrot,cyrot1,syrot1,geou(k),geov(k))
  ELSE
    geou(k) = rmis
    geov(k) = rmis
  ENDIF

ENDDO
ENDDO

!--------------------------------------------------------------------------
! 4: define output
!

grid_2u = grid_1u
grid_2v = grid_1v
grid_2u%field = geou
grid_2v%field = geov

WRITE (88,*) "wind antirotation performed"

RETURN
END SUBROUTINE antirot_wind

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE find_best_antir(grid1,grid2)
!
! Finds the "best" regular grid (grid2) to antirotate grid1
!
IMPLICIT NONE

TYPE(grid), INTENT(IN) :: grid1
TYPE(grid), INTENT(OUT) :: grid2

! Local variables
INTEGER  :: i,j,ndig
REAL :: x1,x2,y1,y2,cyrot1,syrot1,dx,dy,ym
REAL :: xgrid,ygrid,xgeo,ygeo

!--------------------------------------------------------------------------
! 1: if input grid is regular, just choose the same grid
!

IF (grid1%proj == "GEO" .AND. grid1%xrot == 0. .AND. grid1%yrot == 0.) THEN
  grid2 = grid1
  RETURN
ENDIF

!--------------------------------------------------------------------------
! 2: finds extreme geog. coord. of points in grid1
!

cyrot1 = cos( grid1%yrot/dtr )
syrot1 = sin( grid1%yrot/dtr )

! Coor. of first point, to start searching for extremes
IF (grid1%proj == "GEO") THEN
  CALL rtlld(grid1%x1,grid1%y1,grid1%xrot,cyrot1,syrot1,xgeo,ygeo)
ELSE IF (grid1%proj == "UTM") THEN
  CALL utm2ll(grid1%x1,grid1%y1,grid1%utmz,grid1%y1<0.,ygeo,xgeo)
ENDIF

x1 = xgeo
y1 = ygeo
x2 = xgeo
y2 = ygeo

! Loop over points, to find extreme coordinates
DO i = 1,grid1%nx
DO j = 1,grid1%ny

  xgrid = grid1%x1 + (i-1) * grid1%dx
  ygrid = grid1%y1 + (j-1) * grid1%dy

  IF (grid1%proj == "GEO") THEN
    CALL rtlld(xgrid,ygrid,grid1%xrot,cyrot1,syrot1,xgeo,ygeo)
  ELSE IF (grid1%proj == "UTM") THEN
    CALL utm2ll(xgrid,ygrid,grid1%utmz,ygrid<0.,ygeo,xgeo)
  ENDIF

  IF (xgeo < x1) x1 = xgeo
  IF (xgeo > x2) x2 = xgeo
  IF (ygeo < y1) y1 = ygeo
  IF (ygeo > y2) y2 = ygeo

ENDDO
ENDDO

!print *,"Extreme coord. of input grid:"
!print *,'x1,x2,y1,y2',x1,x2,y1,y2

!--------------------------------------------------------------------------
! 3: find n.er of digits to use in output grid parameters
!

! dx & dy in gradi non ruotati al centro del dominio
ym = (y1+y2) / 2

IF (grid1%proj == "GEO") THEN
  dx = grid1%dx / COS(ym/dtr)
  dy = grid1%dy
ELSE
  dx = (grid1%dx / COS(ym/dtr)) / deg2km
  dy = grid1%dy / deg2km
ENDIF

ndig = 1 - NINT(LOG10 (MIN (dx,dy))) 

!--------------------------------------------------------------------------
! 4: set output grid parameters
!

grid2%dx = nround (dx,ndig)
grid2%dy = nround (dy,ndig)

grid2%x1 = lround(x1,ndig)
grid2%y1 = lround(y1,ndig)

x2 = uround(x2,ndig)
y2 = uround(y2,ndig)
grid2%nx = INT ((x2-x1) / grid2%dx) + 2
grid2%ny = INT ((y2-y1) / grid2%dy) + 2

grid2%xrot = 0.
grid2%yrot = 0.
grid2%proj = "GEO"

RETURN
END SUBROUTINE find_best_antir


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE transpose_to_010(field_org,nx,ny,scan,field_trans)
!
! Description:
! this routine re-arrange terms in field_org array, in order to dispose them
!   according to the 010 grib scanning flags.
!
! Input parameters:
!   field_org(nx*ny): field to be re-arranged (real)
!   nx, ny:           grid size (int)
!   scan(3):          grib scanning flags in field_org (int)
!
! Output parameters:
!   field_trans(nx*ny): filed re-arranged
!
IMPLICIT NONE

INTEGER, INTENT(IN) :: nx, ny, scan(3)
REAL, INTENT(IN) :: field_org(nx*ny)
REAL, INTENT(OUT) :: field_trans(nx*ny)

INTEGER :: s1, s2, s3
INTEGER :: newi,newj,newk,oldi,oldj,oldk

!- End of header
!==========================================================================

s1 = scan(1)      ! 0: +i direction
s2 = scan(2)      ! 0: -j direction
s3 = scan(3)      ! 0: adjacent pts in i direction are consecutive

IF (s3 /= 0) THEN
  WRITE (*,*) 'Error: scanning s3 /=0 not handled yet!'
  STOP
ENDIF

field_trans = 0.

DO oldi = 1, nx
DO oldj = 1, ny

  oldk = (oldj-1) * nx + oldi

  IF (s1 == 0) THEN
   newi = oldi
  ELSE
   newi = nx - oldi +1
  ENDIF

  IF (s2 == 0) THEN
   newj = ny - oldj + 1
  ELSE
   newj = oldj
  ENDIF

  newk = (newj-1) * nx + newi

  field_trans(newk) = field_org(oldk)

ENDDO
ENDDO

END SUBROUTINE transpose_to_010


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
SUBROUTINE bin2scan (bin,scan)
!
! given the value of ksec2(11), returns the 3 "scanning flag"
!
IMPLICIT NONE

INTEGER, INTENT(IN) :: bin
INTEGER, INTENT(OUT) :: scan(3)
INTEGER :: dum
!

IF (bin < 8) THEN        ! scanning flag written in reverse order (MM5)
                         ! WARNING: scan(1) might really be scan(3) !!!
  dum = bin
  scan(3) = MOD (dum,2)
  dum = dum / 2
  scan(2) = MOD (dum,2)
  dum = dum / 2
  scan(1) = MOD (dum,2)
  
ELSE                     ! scanning flags written in standard (ECMWF) way

  IF (MOD(bin,32) /= 0) WRITE (*,*) 'warning, strano scan ',bin

  dum = bin / 32
  scan(1) = MOD (dum,2)
  dum = dum / 2
  scan(2) = MOD (dum,2)
  dum = dum / 2
  scan(3) = MOD (dum,2)

ENDIF

RETURN
END SUBROUTINE bin2scan



!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
FUNCTION nround(x,n) RESULT(xrnd)
!
! Ritorna il numero reale x, arrotondato a n cifre significative
!

REAL :: x,xrnd
INTEGER :: n
!
xrnd = FLOAT( NINT(x * 10**n) ) / 10**n

RETURN
END FUNCTION nround

!--------------------------------------------------------------------------
FUNCTION lround(x,n) RESULT(xrnd)
!
! Ritorna il numero reale x, troncato a n cifre decimali
!
!
REAL :: x,xrnd
INTEGER :: n
!
xrnd = FLOAT( INT(x * 10**n) ) / 10**n

RETURN
END FUNCTION lround

!--------------------------------------------------------------------------
FUNCTION uround(x,n) RESULT(xrnd)
!
! Ritorna il numero reale x, innalzato a n cifre decimali
!
REAL :: x,xrnd
INTEGER :: n
!
xrnd = FLOAT( INT(x * 10**n)+1 ) / 10**n

RETURN
END FUNCTION uround



!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!     FROM CALMET LIBRARY (f77, converted to f90)
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!----------------------------------------------------------------------
      subroutine ll2utm(rlat,rlon,iz0,x,y,iz)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET
!    
! --- CALMET   Version: 5.0       Level: 970825                  LL2UTM
!
! --- PURPOSE:  Converts latitude/longitude to UTM coordinates
!
!           *** Universal Transverse Mercator (UTM) grid system divides
!           *** the world into 60 north-south zones, covering a 6 deg.
!           *** strip of longitude. Zone 1 begins between 180 and 174
!           *** degrees West longitude and progresses eastward to
!           *** zone 60.
!           *** This routine works in both No. & So. Hemispheres
!               Reference --
!                 "Map Projections--A Working Manual", p61,
!                  U.S. Geological Survey Professional Paper 1395,
!                    Note: assumes the Clarke 1866 ellipsoid
!               Adapted from --
!                  EPS version 2.0; subr. MAPGTU
!
! --- INPUTS:
!               RLAT - Real        - N Latitude in decimal degrees
!                                    (use negative for southern hemisphere)
!               RLON - Real        - E Longitude in decimal degrees
!                                    (use negative for western hemisphere)
!                IZ0 - Integer     - UTM zone override (used only if
!                                    IZ0 .ne. zero).
!
! --- OUTPUT:
!                  X - Real        - UTM easting in km
!                  Y - Real        - UTM northing in km
!                 IZ - Integer     - UTM zone
!
! --- LL2UTM called by:  READCF
! --- LL2UTM calls:      none
!----------------------------------------------------------------------

      real rlat,rlon,x,y
      real N,M
      real p,dl,sinp,tanp,cosp,t,c,a1,a2,a3,a4,a5,a6,t2,false_n
      integer iz0,iz

      REAL, PARAMETER :: k0=0.9996
      REAL, PARAMETER :: a=6378206.4
      REAL, PARAMETER :: e2=0.00676866
      REAL, PARAMETER :: ep2=0.0068148
      REAL, PARAMETER :: false_e=500000.0
      REAL, PARAMETER :: rdt=3.141592654/180.0

      if (iz0 .eq. 0) then
! ---   Locate natural zone
          iz = int((180.0+rlon)/6.0) + 1
      else
! ---   Zone override
          iz = iz0
      endif

! --- Compute delta longitude in radians
      dl = rdt*(rlon - (6.0*iz-183.0))

! --- Convert phi (latitude) to radians
      p = rdt*rlat

      sinp = sin(p)
      N = a/sqrt(1.0-e2*sinp*sinp)
      tanp = tan(p)
      T = tanp*tanp
      cosp = cos(p)
      C = ep2*cosp*cosp
      A1 = dl*cosp
      M = 111132.0894*rlat - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p) &
        - 0.02*sin(6.0*p)

      A2 = A1**2
      A3 = A2*A1
      A4 = A2**2
      A5 = A4*A1
      A6 = A4*A2
      T2 = T**2

! --- Compute UTM x and y (km)
      x = 0.001*(k0*N*(A1+(1.0-T+C)*A3/6.0          &     
        + (5.0-18.0*T+T2+72.0*C-58.0*ep2)*A5/120.0) &
        + false_e)
      y = (M+N*tanp * (A2/2.0 + (5.0-T+9.0*C+4.0*C*C)*A4/24.0 &
        + (61.0-58.0*T+T2+600.0*C-330.0*ep2)*A6/720.0))
      false_n = 0.
      if (rlat .lt. 0.) then
! --- in km, unlike false_e
        false_n = 10000.
      endif
      y = 0.001*k0*y + false_n

      return
      end subroutine ll2utm

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!----------------------------------------------------------------------
      subroutine utm2ll(x,y,iz,lsohem,rlat,rlon)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET
!
! --- CALMET   Version: 5.0       Level: 970825                  UTM2LL
!
! --- PURPOSE:  Converts UTM coordinates to latitude/longitude
!               Works in both Northern & Southern Hemispheres
!               Reference--
!                 "Map Projections--A Working Manual", p61,
!                  U.S. Geological Survey Professional Paper 1395,
!                    Note: assumes the Clarke 1866 ellipsoid
!               Adapted from --
!                  EPS version 2.0; subr. MAPUTG
!
! --- INPUTS:
!                  X - real    - UTM easting in km
!                  Y - real    - UTM northing in km
!                 IZ - integer - UTM zone (6 deg N-S strip, range=1,60)
!             LSOHEM - logical - TRUE = southern hemisphere
!                                FALSE = northern hemisphere
!
! --- OUTPUT:
!               RLAT - real    - N Latitude in decimal degrees
!               RLON - real    - E Longitude in decimal degrees
!
! --- UTM2LL called by:  READCF
! --- UTM2LL calls:      none
!----------------------------------------------------------------------

      real x,y,rlat,rlon
      real M,N1,l
      real rlon0,xm,ym,u,p1,cosp1,tanp1,c1,c2,t1,t2,sinp1,sin2p1,r0,r1
      real d,d2,d3,d4,d5,d6,p
      logical lsohem
      integer iz

! --- Parameter definitions
!      k0        -  scale on central meridian
!      a         -  Clarke 1866 equatorial radius
!      e2        -  squared Clarke 1866 eccentricity
!      ep2       -  (e2/(1.0-e2)
!      false_e   -  false easting

      REAL, PARAMETER :: k0=0.9996
      REAL, PARAMETER :: a=6378206.4
      REAL, PARAMETER :: e2=0.00676866
      REAL, PARAMETER :: ep2=0.0068148
      REAL, PARAMETER :: false_e=500000.0
      REAL, PARAMETER :: e1=0.001697916
      REAL, PARAMETER :: e11=3.0*e1/2.0 - 27.0*e1*e1*e1/32.0
      REAL, PARAMETER :: e12=21.0*e1*e1/16.0 - 55.0*e1*e1*e1*e1/32.0
      REAL, PARAMETER :: e13=151.0*e1*e1*e1/96.0
      REAL, PARAMETER :: e14=1097.0*e1*e1*e1*e1/512.0
      REAL, PARAMETER :: e4=e2*e2
      REAL, PARAMETER :: e6=e2*e4
!      REAL, PARAMETER :: dtr=180.0/3.141592654

! --- Central meridian
      rlon0 = iz*6.0 - 183.0

! --- Correct for false easting, southern hemisphere and change to meters
      xm = 1000.0*x - false_e
      if(LSOHEM) then
        ym = 1000.0 * (y-10000.)
      else
        ym = 1000.0 * y
      endif

      M = ym/k0
      u = M/(a*(1.0-e2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0))
      p1 = u + e11*sin(2.0*u) + e12*sin(4.0*u) + e13*sin(6.0*u) + &
               e14*sin(8.0*u)
      cosp1 = cos(p1)
      C1 = ep2*cosp1**2
      C2 = C1**2
      tanp1 = tan(p1)
      T1 = tanp1**2
      T2 = T1**2
      sinp1 = sin(p1)
      sin2p1 = sinp1**2
      N1 = a/sqrt(1.0-e2*sin2p1)
      R0 = 1.0-e2*sin2p1
      R1 = a*(1.0-e2)/sqrt(R0**3)

      D = xm/(N1*k0)
      D2=D**2
      D3=D*D2
      D4=D*D3
      D5=D*D4
      D6=D*D5

      p = p1 - (N1*tanp1/R1) * (D2/2.0                                  &
             - (5.0+3.0*T1+10.0*C1-4.0*C2-9.0*ep2)*D4/24.0              &
             + (61.0+90.0*T1+298.0*C1+45.0*T2-252*ep2-3.0*C2)*D6/720.0)
      rlat = dtr*p
      l = (D - (1.0+2.0*T1+C1)*D3/6.0                                   &
             + (5.0-2.0*C1+28*T1-3.0*C2+8.0*ep2+24.0*T2)*D5/120.0)/cosp1
      rlon = dtr*l + rlon0

      return
      end subroutine utm2ll

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!     FROM ARPA-SMR HIBULIB (f77)
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      SUBROUTINE TLLD(ALO1,ALA1,TLM0D,CTPH0,STPH0,ALO2,ALA2)
!
! Trasforma le coordinate geografiche lat lon in coordinate nel 
! sistema ruotato tipico dei modelli meteo LAMBO e LOKAL.
!
! input:
!
!  ALO1       real      longitudine sistema geografico (rapp. decimale)
!  ALA1       real      latitudine sistema geografico (rapp. decimale)
!  TLM0D      real      longitudine del punto intercetto del meridiano di 
!                       Greenwich con l'equatore del sistema ruotato, nel 
!                       sistema geografico (rapp. decimale)
!  CTPH0      real      coseno della latitudine del punto intercetto del 
!                       meridiano di Greenwich con l'equatore del sistema 
!                       ruotato nel sistema geografico
!  STPH0      real      seno della latitudine del punto intercetto del 
!                       meridiano di Greenwich con l'equatore del sistema 
!                       ruotato nel sistema geografico
!
! output: 
!
!  ALO2       real     longitudine nel sistema ruotato (rapp. decimale)
!  ALA2       real     latitudine nel sistema ruotato (rapp. decimale)
!
!-----------------------------------------------------------------------01700000
        REAL alo1,ala1,tlm0d,ctph0,stph0,alo2,ala2
        REAL relm,srlm,crlm,sph,cph,cc,anum,denom
!
	relm = (alo1-tlm0d)
	srlm = SIN(relm / dtr)           
	crlm = COS(relm / dtr)           
!                              
	sph = SIN(ala1 / dtr)             
	cph = COS(ala1 / dtr)             
!                              
	cc = cph * crlm              
	anum = cph * srlm            
	denom = ctph0*cc + stph0*sph 
!
        alo2 = dtr * ATAN2(anum,denom)       
        ala2 = dtr * ASIN(ctph0*sph - stph0*cc)
!
	RETURN             
	END SUBROUTINE tlld


! **************************************************************
      SUBROUTINE RTLLD(ALO1,ALA1,TLM0D,CTPH0,STPH0,ALO2,ALA2)    
! **************************************************************
!
! Trasforma le coordinate lat lon ruotate, tipiche dei modelli meteo LAMBO e
! LOKAL, in coordinate nel sistema geografico.
!
! input:
!
!  ALO1       real     longitudine nel sistema ruotato (rapp. decimale)
!  ALA1       real     latitudine nel sistema ruotato (rapp. decimale)
!  TLM0D      real     longitudine del punto intercetto del meridiano di 
!                      Greenwich con l'equatore del sistema ruotato, nel 
!                      sistema geografico (rapp. decimale)
!  CTPH0      real     coseno della latitudine del punto intercetto del 
!                      meridiano di Greenwich con l'equatore del sistema 
!                      ruotato nel sistema geografico
!  STPH0      real     seno della latitudine del punto intercetto del 
!                      meridiano di Greenwich con l'equatore del sistema 
!                      ruotato nel sistema geografico
!
! output: 
!
!  ALO2       real      longitudine sistema geografico (rapp. decimale)
!  ALA2       real      latitudine sistema geografico (rapp. decimale)
!
      real alo1,ala1,tlm0d,ctph0,stph0,alo2,ala2
      real stph,ctph,stlm,ctlm,cph
!
      stph = SIN(ala1 / dtr)                     
      ctph = COS(ala1 / dtr)                     
      ctlm = COS(alo1 / dtr)                      
      stlm = SIN(alo1 / dtr)                      
!                                          
      ala2 = dtr * ASIN(stph0*ctph*ctlm + ctph0*stph)
      cph = COS(ala2 / dtr)                        
!                                          
      alo2 = tlm0d + dtr*ASIN(stlm*ctph/cph)      
!             
      RETURN  
      END SUBROUTINE rtlld

! **************************************************************
      SUBROUTINE rltlwd(almd,aphd,tpus,tpvs,tlm0d,ctph0,stph0,pus,pvs)
! **************************************************************
!
! Trasforma le componeti u,v espresse nel sistema ruotato tipico dei modelli 
! meteo LAMBO e LOKAL a componenti u,v espresse nel sistema geografico
!
! input:
!
!  ALMD       real      longitudine sistema geografico (rapp. decimale)
!  APHD       real      latitudine sistema geografico (rapp. decimale)
!
!  tpus       real      componente u nel sistema ruotato
!  tpvs       real      componente v nel sistema ruotato
!
!  TLM0D      real      longitudine del punto intercetto del meridiano di 
!                       Greenwich con l'equatore del sistema ruotato, nel 
!                       sistema geografico
!                       
!  CTPH0      real      coseno della latitudine del punto intercetto del 
!                       meridiano di Greenwich con l'equatore del sistema 
!                       ruotato nel sistema geografico (rapp. decimale)
!  STPH0      real      seno della latitudine del punto intercetto del 
!                       meridiano di Greenwich con l'equatore del sistema 
!                       ruotato nel sistema geografico
!
! output: 
!
!  pus          real     componente u nel sistema geografico
!  pvs          real     componente v nel sistema geografico
!
      REAL :: almd,aphd,tpus,tpvs,tlm0d,ctph0,stph0,pus,pvs
      REAL :: relm,srlm,crlm,sph,cph,cc,tph,rctph,cray,dray,dc
!
      relm = (almd-tlm0d)                                         
      srlm = SIN(relm / dtr)                                           
      crlm = COS(relm / dtr)                                           
!                                                               
      sph = SIN(aphd / dtr)                                              
      cph = COS(aphd / dtr)                                              
!                                                               
      cc = cph*crlm                                               
      tph = ASIN(ctph0*sph-stph0*cc)                              
!                                                               
      rctph = 1./COS(tph)                                         
      cray = stph0*srlm*rctph                                     
      dray = (ctph0*cph+stph0*sph*crlm)*rctph                     
      dc = dray*dray+cray*cray                                    
      pus = (dray*tpus+cray*tpvs)/dc                              
      pvs = (dray*tpvs-cray*tpus)/dc                              
!                                                               
      RETURN                                                    
      END SUBROUTINE rltlwd

! **************************************************************
      SUBROUTINE ltlwd(almd,aphd,u,v,tlm0d,ctph0,stph0,tu,tv)
! **************************************************************
!
! Trasforma le componeti u,v espresse nel sistema geografico
! a componenti u,v espresse nel sistema ruotato tipico dei modelli 
! meteo LAMBO e LOKAL.
!
! input:
!
!  ALMD       real      longitudine sistema geografico (rapp. decimale)
!  APHD       real      latitudine sistema geografico (rapp. decimale)
!
!  u          real     componente u nel sistema geografico
!  v          real     componente v nel sistema geografico
!
!  TLM0D      real      longitudine del punto intercetto del meridiano di 
!                       Greenwich con l'equatore del sistema ruotato, nel 
!                       sistema geografico
!                       
!  CTPH0      real      coseno della latitudine del punto intercetto del 
!                       meridiano di Greenwich con l'equatore del sistema 
!                       ruotato nel sistema geografico (rapp. decimale)
!  STPH0      real      seno della latitudine del punto intercetto del 
!                       meridiano di Greenwich con l'equatore del sistema 
!                       ruotato nel sistema geografico
!
! output: 
!
!  tu         real     componente u nel sistema ruotato
!  tv         real     componente v nel sistema ruotato
!
      REAL :: almd,aphd,u,v,tlm0d,ctph0,stph0,tu,tv
      REAL :: cray,dray
!
      CALL ltlw1(almd,aphd,tlm0d,ctph0,stph0,dray,cray)
      CALL ltlw2(u,v,tu,tv,dray,cray)

      RETURN
      END SUBROUTINE ltlwd

! **************************************************************
      SUBROUTINE ltlw1(almd,aphd,tlm0d,ctph0,stph0,dray,cray)
!
      REAL :: almd,aphd,tlm0d,ctph0,stph0,dray,cray
      REAL :: relm,srlm,crlm,sph,cph,cc,tph,rctph
!
      relm = (almd-tlm0d)
      srlm = SIN(relm / dtr)
      crlm = COS(relm / dtr)
!
      sph = SIN(aphd / dtr)
      cph = COS(aphd / dtr)
!
      cc=cph*crlm
!
      tph = ASIN(ctph0*sph-stph0*cc)
!
      rctph = 1./COS(tph)
!
      cray = stph0*srlm*rctph
      dray = (ctph0*cph+stph0*sph*crlm)*rctph
!
      RETURN
      END SUBROUTINE ltlw1

! **************************************************************
      SUBROUTINE ltlw2(pus,pvs,tpus,tpvs,dray,cray)
!
      REAL :: pus,pvs,tpus,tpvs,dray,cray
!
      tpus = dray*pus-cray*pvs
      tpvs = cray*pus+dray*pvs
!
      RETURN
      END SUBROUTINE ltlw2

! **************************************************************
	SUBROUTINE hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp,zp)
! **************************************************************
! effettua interpolazione bilineare dati i valori nei punti
! 1,2,3,4 e le coordinate dei punti 1 e 3 oltre a quelle
! del punto p dove viene valutato il campo.
! I punti sono disposti nell'ordine:
! 4 3
! 1 2
!
      REAL :: z1,z2,z3,z4,x1,y1,x3,y3,xp,yp,zp
      REAL :: fc,z5,z6
!
      fc=((yp-y1)/(y3-y1))

      z5=(z4-z1)*fc+z1
      z6=(z3-z2)*fc+z2

      zp=(z6-z5)*((xp-x1)/(x3-x1))+z5

      RETURN
      END SUBROUTINE hbilin

END MODULE grid_handler





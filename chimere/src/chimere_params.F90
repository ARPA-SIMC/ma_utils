module chimere_params

  implicit none

! Precision of real tables
  integer, parameter :: iprec=8

  ! A - parameters that can be set-up by user's scripts

  character(len=32) :: simlab

  ! A-0: Domain
  character(len=32) :: domain

  ! A-1: Model geometry
  integer :: nzonal_entire_domain ! Number of zonal cells
  integer :: nmerid_entire_domain ! Number of meridional cells
  integer :: nzonalmax            ! Max Number of zonal cells for all subdomains
  integer :: nmeridmax            ! Max Number of meridional cells for all subdomains
  integer :: nverti               ! Number of vertical layers
  integer :: nsho                 ! Cell for screen display (NSHO<=NHORIZ)
  integer :: nivout               ! Number of vertical layers in output files
  integer :: nhoriz    ! Num. horiz. cells
  integer :: nbox      ! Num. cells
  integer :: nhbound   ! --- in one layer

! nwl: number of wavelengths for aod output.
!      Must be equal to the first dimension of WAA in fast-J, so fard hardcoded to 5 as well.
  integer,parameter :: nwl       = 5
  integer,parameter :: fj_nbins  = 11   ! number of particle size bins for the optical calculations
  integer,parameter :: nbinshr   = 400

  ! A-2: Model chemistry, emissions and other processes

  integer :: nspec    ! Number of active species
  integer :: nreac    ! Number of reactions
  integer :: nfam     ! Number of "family" species (for output)
  integer :: nemisa   ! Max number of anthropic emitted species
  integer :: nemisf   ! Max number of fire emitted species
  integer :: nemisb   ! Max number of biogenic emitted species
  integer :: nbiomgn  ! Number of input Megane Species
  integer :: nemissa  ! Max number of sea salt emitted species
  integer :: nemisd   ! Number of emitted dust species
  integer :: nlevemis ! Number of emission levels
  integer :: nspectot         ! Total num. species

! namelist flags
  integer :: mecachim
  integer :: aero
  integer :: seasalt
  integer :: carb
  integer :: trc
  integer :: soatyp
  integer :: bins
  integer :: reactive

  integer :: chimverb
  integer :: frames_per_file
  integer :: iadrydep
  integer :: online
  integer :: urbancorr
  integer :: iresusp
!!  simc agg
  integer :: kzexp
  real(kind=iprec) :: zimax,urbpblmin,uframin,uzkmin,umomin,obukmax


  ! A-3: Aerosol parameters : integer,parameter
  integer :: ms        ! Number of bin
  integer :: nkc       ! Number of components in aerosol
  integer :: nequil    ! Thermodynamic equilibrium
  integer :: npeq      ! Thermodynamic equilibrium


  ! A-4: Model numerical parameters
  integer :: dtphys     ! The physical time step (mn)
  integer :: dtchem     ! The chemical time step (mn)
  integer :: ichemstep  ! Number of chemical refined iterations
  integer :: nzdoms     ! Number of MPI subdomains in zonal direction
  integer :: nmdoms     ! Number of MPI subdomains in meridian direction
  integer :: nitgs      ! Number of Gauss-Seidel iters. for TWOSTEP
  integer :: nitgssu    ! Number of G-S iters. for TWOSTEP during spin-up
  integer :: ihoursu    ! Number of spin-up hours
  integer :: nsaveconcs ! Save concentrations for restart every ... hours
  integer :: nsavedepos ! Save cumulated deposition every ... hours

  ! A-6: Aerosol parameters
  real(kind=iprec):: dpmin, dpmax

!-------------------------------------------------------------------------------------------------
  ! B - Hard-coded parameters

  ! B-1: Model geometry

  ! B-2: Model chemistry, emissions and other processes
  integer,parameter :: nspresc     = 4     ! Number of prescribed species (O2, H2O...)
  integer,parameter :: nlduse      = 09    ! Max number of landuse classes
  integer,parameter :: nlduseurb   = 5     ! Adress of the "urban" category in LANDUSE
  integer,parameter :: nlevphotmax = 50    ! Max number of tabulated photolysis levels
  integer,parameter :: nphotmax    = 50    ! Max number of photolysis reactions
  integer,parameter :: ntabuzenmax = 20    ! Max number of tabulated zenith angles
  integer,parameter :: nvegtype    = 16
  integer,parameter :: nreactamax  = 4     ! Max number of reactants/reaction
  integer,parameter :: nprodamax  = 100    ! Max number of products/reaction
  integer,parameter :: ntypeday    = 7     ! Number of day types/week
  integer,parameter :: ntyperate   = 50    ! Max number of reaction types
  integer,parameter :: ntabmax     = 22    ! Max number of rate constants
  integer,parameter :: ntemps      = 4     ! Number of tabul. temperatures for stoichio.
  integer,parameter :: nvexcha     = 1     ! Number of soil layer (for POPs use)

! Aerosol parameters
  integer,parameter      :: ncomax = 10 ! Max number of tabulation data for coagulation

!  integer,parameter      :: ninrh  = 16 ! AEROMIN parameter
!  integer,parameter      :: ninte  = 22 ! AEROMIN parameter
!  integer,parameter      :: ninc   = 22 ! AEROMIN parameter

!  integer,parameter      :: kdimorg=6 ! AEROORG.bin parameter
!  integer,parameter      :: naeroorg=6   ! AEROORG.bin parameter
!  integer,parameter      :: kaeroorgmax=10   ! AEROORG.bin parameter
 
  real(kind=iprec),parameter         :: zacom=2.d-1
! cut-off relative humidity for aerosol processes
  real(kind=iprec),parameter         :: zrh_max = 0.97d0

! Model numerical parameters
  real(kind=iprec), parameter :: clipconc = 1d0     ! Clipping value for the TWOSTEP algorithm
  real(kind=iprec), parameter :: m2cm=100d0
  real(kind=iprec), parameter :: cm2m=1.d-2


! Model physical parameters
  real(kind=iprec),parameter      :: sfcrat    = 1d-1  ! Surface layer to PBL ratio
  real(kind=iprec),parameter      :: evolfrac  = 1d-1  ! Emission layer volume fraction
  real(kind=iprec),parameter      :: wxem      = 1.2d0 ! Emssion layer mixing rate
  real(kind=iprec),parameter      :: rkzmini   = 2.0d3 ! Minimal Kz value (cm2/s)
  integer,parameter               :: nparammax = 30    ! Max number of output parameters
  integer,parameter               :: nemismax  = 50    ! Max number of emitted species


! Aerosol constants
  real(kind=iprec),parameter :: turbds      = 1.d-3
  real(kind=iprec),parameter :: ake         = 1.d-1
  real(kind=iprec),parameter :: chi         = 1.d+0
  real(kind=iprec),parameter :: fslip       = 1.37d0
  real(kind=iprec),parameter :: stick       = 1.d+0
  real(kind=iprec),parameter :: gamma       = 1.d+0
  real(kind=iprec),parameter :: gasmw       = 29.d+00

! CFL parameters

  real(kind=iprec),parameter :: cflmax=0.8
  real(kind=iprec),parameter :: minprescfl=400 ! upper limit in hPa

!-------------------------------------------------------------------------------------------------
! Dust constants and parameters
!
! e_: Binding energies of the 3 aerosol populations (g.cm2/s2)
!     Initial values proposed by [Alfaro et al. 1998] are e_ = (/ 3.61, 3.52, 3.46 /)
!     with a correction factor 'div' as e=e_/div
!     For div increase, emissions increase.
!
!  To use a Weibull distribution, select nwb>1 (nwb=1 mean modelled wind speed directly)
!     - U10mMOD is the model wind speed
!     - wind10ms is the 10m wind speed (m/s) within the loop of the distrib.
!     - nwb: number of steps of the distribution.
!     - kref: the shape parameter value.

! Mineralogy of coarse sand, fine medium sand, silt, clay, salt
! soil type params (possibly to make dynamical depending on the file read)

  integer, parameter           :: nmode = 3            ! number of emissions modes for mineral dust
  integer, parameter           :: nmineral = 5         ! number of emitted soil particles
  integer, parameter           :: nsoildmax=20000      ! number of bins for the soil size distribution
  real(kind=iprec), parameter  :: soil_dmin = 1.d-4    ! soil size distribution min (cm)
  real(kind=iprec), parameter  :: soil_dmax = 2.d-1    ! soil size distribution max (cm)
  real(kind=iprec), parameter  :: rop = 2.65d0         ! particle density (g cm-3) i.e quartz
  real(kind=iprec), parameter  :: z1000cm = 1.d+3      ! 10m height in [cm]
  real(kind=iprec), parameter  :: umin = 21.d0         ! Min wind speed for the 'Owen' effect (cm.s-1)
  real(kind=iprec), parameter  :: woff = 0.5d0         ! Wind offset to smooth Richardson numbers (m/s)

  real(kind=iprec), dimension(nmineral), parameter :: mineral_dp = (/0.0690, 0.021, 0.0125, 0.0002, 0.052/)
  real(kind=iprec), dimension(nmineral), parameter :: mineral_sig = (/1.6, 1.8, 1.6, 2., 1.5/)

!----------------------------------------------------------------------
! Weibull distribution
  integer, parameter           :: nwb=12            ! Number of values for the distribution
  real(kind=iprec), parameter  :: dwindmax=16d0     ! Maximum of wind speed (m/s) for dust emissions

! Erodibility
! ierod=1: automatic affectation of erodibility values
! ierod=2: read a global datafile of MODIS surface reflectance
! ierod=3: Mix of USGS and MODIS (over arid surfaces)

  integer, parameter :: ierod=3

! % of erodibility per cell
  real(kind=iprec), parameter  :: perod_barren=1.d0        ! barren soils
  real(kind=iprec), parameter  :: perod_crop=0.2d0         ! cropland

! Use a threshold for the snow (in m)
  real(kind=iprec), parameter  :: snow_th=1.d-3         ! Snow cover if more that [snow_th] m
  real(kind=iprec), parameter  :: minsurf_th=0.d0      ! No flux if more than (minsurf_th x 100) % of water

!----------------------------------------------------------------------
! [Marticorena and Bergametti, 1995] parameters
  real(kind=iprec), dimension(nmode), parameter       :: cad = (/0.2, 0.6, 0.2/)


!----------------------------------------------------------------------
! [Alfaro and Gomes 2001] parameters
  real(kind=iprec), parameter  :: beta_dust=16300.d0
  real(kind=iprec), parameter   :: div = 3d0
  real(kind=iprec), dimension(nmode), parameter   :: e = (/3.61d0/div, 3.52d0/div, 3.46d0/div/)

!----------------------------------------------------------------------
! [Kok et al., 2014] parameters for the mono-modal distribution
! kok_sigmas must be > 1 for the sigma in log expression
! Default values are:
!    kok_ds=3.4d0      [um]
!    kok_sigmas=3.0d   [ad.]
!    kok_cv=12.62d0    [um]
!    kok_lambda=12.0d0 [um]

real(kind=iprec), parameter :: kok_ds=3.4d0
real(kind=iprec), parameter :: kok_sigmas=3.0d0
real(kind=iprec), parameter :: kok_cv=12.62d0
real(kind=iprec), parameter :: kok_lambda=12.0d0

!-------------------------------------------------------------------------------------------------
end module chimere_params





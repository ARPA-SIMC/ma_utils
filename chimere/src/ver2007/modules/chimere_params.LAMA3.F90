module chimere_params


  implicit none

  ! A - parameters that can be set-up by user's scripts
 
  ! A-0: Domain
  character(len=*),parameter :: domain ='LAMA3'

  ! A-1: Model geometry
  integer,parameter :: ivsurf    = 1 ! Emission sublayer (2) or not (1)
  integer,parameter :: nvert_raw = 8 ! Raw nb of vertical levels, before insert. of sublayer
  integer,parameter :: nzonal    = 241 ! Number of zonal cells
  integer,parameter :: nmerid    = 161 ! Number of meridional cells
  integer,parameter :: nsho      = 19641 ! Cell for screen display (NSHO<=NHORIZ)
  integer,parameter :: nivout    = 8 ! Number of vertical layers in output files
  integer,parameter :: nvexcha     = 1   ! Number of soil layer
  ! derived
  integer,parameter :: nverti    = nvert_raw+ivsurf-1 ! Number of vertical layers
  integer,parameter :: nhoriz    = nmerid*nzonal      ! Num. horiz. cells
  integer,parameter :: nbox      = nhoriz*nverti      ! Num. cells
  integer,parameter :: nhbound   = 2*(nmerid+nzonal)  ! --- in one layer
  integer,parameter :: nlatbound = nhbound*nverti     ! Number of lateral boundary cells
  integer,parameter :: nbound    = nlatbound+nhoriz   ! Total boundary cells
  integer,parameter :: ntbox     = nbox+nbound        ! Total num. cells

  ! A-2: Model chemistry, emissions and other processes
  !integer,parameter :: iaerosols = _AER_ !Aerosols / Gas phase switch
  integer,parameter :: nspec     = 182 ! Number of active species
  integer,parameter :: nreac     = 165 ! Number of reactions
  integer,parameter :: nfam      = 27 ! Number of "family" species (for output)
  integer,parameter :: nemisa    = 24 ! Max number of anthropic emitted species
  integer,parameter :: nemisb    = 16 ! Max number of biogenic emitted species
  integer,parameter :: nlevemis  = 8 ! Number of emission levels
  
  ! lmbb add new flags for chemistry
  integer,parameter :: aero     = 1
  integer,parameter :: seasalt  = 1
  integer,parameter :: pops     = 0
  integer,parameter :: dustout  = 1
  integer,parameter :: dust     = 1
  integer,parameter :: carb     = 1
  integer,parameter :: trc      = 0
  integer,parameter :: soatyp   = 2
  integer,parameter :: bins     = 1
  integer,parameter :: reactive = 1
  integer,parameter :: ibound   = 1
  
  integer,parameter :: ideepconv = 0
  
  ! A-3: Aerosol parameter : integer,parameter
  integer,parameter :: ms        = 8  ! Number of bin
  integer,parameter :: nkc       = 14  ! Number of components in aerosol
  integer,parameter :: nequil    = 0  ! Thermodynamic equilibrium
  integer,parameter :: npeq      = 1  ! Thermodynamic equilibrium
  ! derived
  integer,parameter :: naerosp   = ms*nkc ! Number of aerosol species
  
  
  ! A-4: Model numerical parameters
  integer,parameter :: nphour_ref  = 6 ! Number of physical steps per hour
  integer,parameter :: ichemstep   = 4 ! Number of chemical refined iterations
  
  integer,parameter :: nzdoms    = 9 ! Number of MPI subdomains in zonal direction
  integer,parameter :: nmdoms    = 5 ! Number of MPI subdomains in meridian direction
  integer,parameter :: nitgs     = 1 ! Number of Gauss-Seidel iters. for TWOSTEP
  integer,parameter :: nitgssu   = 1 ! Number of G-S iters. for TWOSTEP during spin-up
  integer,parameter :: ihoursu   = 1 ! Number of spin-up hours
  integer,parameter :: nsaveconcs= 24 ! Save concentrations for restart every ... hours
  integer,parameter :: nsavedepos= 24 ! Save cumulated deposition every ... hours
  ! derived
  integer,parameter      :: nprint   = nphour_ref      ! Frequency of printouts (in phys. steps)
!  real(kind=8),parameter :: dtr      = 3600d0/(nphour*ichemstep)  ! Time step
!  real(kind=8),parameter :: dtr2     = 2d0*dtr/3d0                ! 2/3.time step
  

  ! A-5: Model physical parameters

  ! A-6: Aerosol parameters
  real(kind=8),parameter :: dpmin      = 0.3906250E-07
  real(kind=8),parameter :: dpmax      = 0.1000000E-04
  

  ! B - Hard-coded parameters
  
  ! B-1: Model geometry
  
  ! B-2: Model chemistry, emissions and other processes
  integer,parameter :: nspresc     = 4  ! Number of prescribed species (O2, H2O...)
  integer,parameter :: nlduse      = 09 ! Max number of landuse classes
  integer,parameter :: nlduseurb   = 5  ! Adress of the "urban" category in LANDUSE
  integer,parameter :: nlevphotmax = 50 ! Max number of tabulated photolysis levels
  integer,parameter :: nphotmax    = 50 ! Max number of photolysis reactions
  integer,parameter :: ntabuzenmax = 20 ! Max number of tabulated zenith angles
  integer,parameter :: nvegtype    = 16    
  integer,parameter :: nreactamax  = 4  ! Max number of reactants/reaction
  integer,parameter :: ntypeday    = 3  ! Number of day types/week
  integer,parameter :: ntyperate   = 50 ! Max number of reaction types
  integer,parameter :: ntabmax     = 22 ! Max number of rate constants
  integer,parameter :: ntemps      = 4  ! Number of tabul. temperatures for stoichio.
  ! derived
  integer,parameter :: nspectot    = nspec+nspresc+nfam ! Total num. species
  
  
  ! B-3: Aerosol parameters
  integer,parameter      :: ncomax = 10 ! Max number of tabulation data for coagulation
  integer,parameter      :: ninrh  = 16 ! AEROMIN parameter
  integer,parameter      :: ninte  = 22 ! AEROMIN parameter
  integer,parameter      :: ninc   = 22 ! AEROMIN parameter

  integer,parameter      :: kdimorg=6 ! AEROORG.bin parameter
  integer,parameter      :: naeroorg=6   ! AEROORG.bin parameter
  integer,parameter      :: kaeroorgmax=10   ! AEROORG.bin parameter
  real(kind=8),parameter :: zacom=0.2


  ! B-4: Model numerical parameters
  real(kind=8),parameter :: clipconc = 1d0         ! Clipping value for the TWOSTEP algorithm

  
  ! B-5: Model physical parameters
  real(kind=8),parameter      :: sfcrat    = 1d-1  ! Surface layer to PBL ratio
  real(kind=8),parameter      :: evolfrac  = 1d-1  ! Emission layer volume fraction
  real(kind=8),parameter      :: wxem      = 1.2d0 ! Emssion layer mixing rate
  real(kind=8),parameter      :: rkzmini   = 2.0d3 ! Minimal Kz value (cm2/s)
  integer,parameter :: nparammax = 30    ! Max number of output parameters
  integer,parameter :: nemismax  = 50    ! Max number of output parameters


  ! B-6: Aerosol constants
  real(kind=8),parameter :: rhoini      = 1.5d+03
  real(kind=8),parameter :: turbds      = 1.d-3
  real(kind=8),parameter :: ake         = 1.d-1
  real(kind=8),parameter :: chi         = 1.d+0
  real(kind=8),parameter :: fslip       = 1.37d0
  real(kind=8),parameter :: stick       = 1.d+0
  real(kind=8),parameter :: gamma       = 1.d+0
  real(kind=8),parameter :: gasmw       = 29.d+00

end module chimere_params

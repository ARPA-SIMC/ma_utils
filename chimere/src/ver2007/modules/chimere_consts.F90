module chimere_consts

  ! Mathematical constants
  real(kind=8),parameter :: pi   = 3.1415926535898d0
  real(kind=8),parameter :: dzero= 0d0
  real(kind=8),parameter :: dun  = 1d0
  real(kind=8),parameter :: th1  = dun/3.d0

  ! Calendar constants
  integer,parameter :: nhourpday=24       ! Number of hours per day
  integer,parameter :: nmonth=12          ! Number of months per year

  ! Physical constants
  real,parameter :: R        = 287.04              ! R constant
  real,parameter :: Cp       = 1005.               ! Cp
  real,parameter :: Lv       = 2.45e6              ! Lv
  real,parameter :: Rv       = 461.5               ! Rv
  real,parameter :: rsurcp   = R/Cp                ! R/Cp
  real,parameter :: rsurl    = R/Lv                ! R/Lv
  real,parameter :: xkappa   = 0.2857              ! Kappa
  real,parameter :: chs0     = 1500.               ! Convective height scale (m)
  real,parameter :: p0       = 1e5                 ! Reference pressure (Pa)
  real(kind=8),parameter :: g       = 9.81d+00     ! g constant
  real(kind=8),parameter :: gravit  = g
  real(kind=8),parameter :: earthr  = 6371.0d5     ! Earth radius
  real(kind=8),parameter :: t0k     = 273.15d0     ! 0 celsius
  real(kind=8),parameter :: vkarm   = 4d-1         ! von Karman constant
  real(kind=8),parameter :: prandtl = 0.72d0       ! Prandtl number
  real(kind=8),parameter :: riso    = 8.31441d0    ! R in "P*V=R*T"
  real(kind=8),parameter :: rgas    = riso*1d3     ! with units used in chemistry routines
  real(kind=8),parameter :: bkiso   = 1.380622d-23 ! Boltzmann constant
  real(kind=8),parameter :: bk      = bkiso*1d7    ! with units used in chemistry routines

  ! Chemical constants
  real(kind=8),parameter :: avogadro = 6.022045d23 ! Avogadro number
  real(kind=8),parameter :: an = avogadro

  ! Software constants
  integer,parameter      :: dlen=19       ! Length of a date string in MM5 format  
  integer,parameter      :: splen=23      ! Length of a species name in netCDF files 

  ! Exchange constants
  real(kind=8),parameter :: surfspec = 80.     ! Specic surface of vegetation cm2/cm3
  real(kind=8),parameter :: rhoveget = 0.7     ! Mass volumic of vegetation g/cm3

  real(kind=8),parameter :: rhowater = 1.      ! Mass volumic of water g/cm3
  real(kind=8),parameter :: dmwabs   = 150.    ! Molar mass of the absorbing phase in particles
  real(kind=8),parameter :: delta0   = 4d-3    ! Surface molecular layer depth at zero wind speed cm (Sergeev et al., 1979)
  real(kind=8),parameter :: foamr    = 0.8     ! foam settling rate at the sea surface (Sergeev et al., 1979) cm/s
  real(kind=8),parameter :: waterdif = 5.14d-6 ! Molecular diffusion coefficient in water cm2/s
  real(kind=8),parameter :: sealeng  = 100.    ! Thickness of the first sea layer near the surface cm

  real(kind=8),parameter :: soilleng  = 0.1    ! Thickness of the first soil layer near the surface cm
  real(kind=8),parameter :: alpha_w   = 0.3    ! Volumetric water content of the soil
  real(kind=8),parameter :: alpha_a   = 0.2    ! Volumetric air content of the soil
  real(kind=8),parameter :: rhosoil   = 1.35   ! Mass volumic of soil g/cm3
  real(kind=8),parameter :: foc       = 0.05   ! Mass fraction of organic carbon in soil
  real(kind=8),parameter :: cdoc      = 0.17d-3! Concentration of dissolved organic carbon in soil solute (g/cm3)
  real(kind=8),parameter :: soilpor   = 0.5    ! Soil porosity
  real(kind=8),parameter :: bioturb   = 6d-8   ! Bioturbation factor cm2/s


end module chimere_consts

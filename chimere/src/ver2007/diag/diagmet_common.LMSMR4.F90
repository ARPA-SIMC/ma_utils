module diagmet_common
! modified  to mic2,enr5 (partialy) 23052008
  ! Diagmet types
  ! A structure to hold the various options of diagmet
  type :: options
     integer :: idstart    ! Starting date of the simulation
     integer :: nsho       ! index of pixel to print out
     integer :: w10m
     integer :: usta
     integer :: flux
     integer :: pblh
     integer :: cice
     integer :: rain
     real    :: upm        ! Urban min. PBL correction
     real    :: uwc        ! Urban wind correction
     real    :: ufx        ! Urban heat flux correction
     integer :: clol
     real    :: crhl
     integer :: clom
     real    :: crhm
     integer :: cloh
     real    :: crhh
     integer :: soim
     integer :: eros,resu
!
     integer :: upblcor     !Urban correction minimal HMIX
     integer :: cldmix      ! disable mixing heigh enanchment due to clouds  
     integer :: kzexp      !   
     real    :: zimax      !   
     real    :: uframin    !   
     real :: ukzmin     !  
     real  :: umomin     !   
  end type options

  ! This structure holds various scalars used by diagmet subroutines
  type :: diag_misc_type
     integer :: nlevels
     real :: apm
     real :: atu
     real :: auf
     real :: awf
     real :: az0
     real :: opdl
     real :: opdm
     real :: opdh
     real :: opl
     real :: opm
     real :: oph
     real :: potf
     real :: potfm
     real :: potts
     real :: rhmaxx
     real :: topcld
     real :: topcldw
     real :: wstar0
  end type diag_misc_type


  ! Variables
  ! geometry
  real,allocatable,dimension(:,:) :: xlong
  real,allocatable,dimension(:,:) :: xlati
  real,allocatable,dimension(:,:) :: xsize   ! Zonal cell length
  real,allocatable,dimension(:,:) :: ysize   ! Meridional cell length
  real,allocatable,dimension(:,:) :: xbasx   ! x coord of local i vector
  real,allocatable,dimension(:,:) :: xbasy   ! y coord of local i vector
  real,allocatable,dimension(:,:) :: ybasx   ! x coord of local j vector
  real,allocatable,dimension(:,:) :: ybasy   ! y coord of local j vector
  
  ! Input
  real,allocatable,dimension(:,:)   :: m_tem2 ! 2m temperature (K)
  real,allocatable,dimension(:,:)   :: m_copc ! convective precipitation (mm/h)
  real,allocatable,dimension(:,:)   :: m_lspc ! large-scale precipitation (mm/h)
  real,allocatable,dimension(:,:)   :: m_clol ! Low cloud fraction (0-1)
  real,allocatable,dimension(:,:)   :: m_clom ! Medium cloud fraction (0-1)
  real,allocatable,dimension(:,:)   :: m_cloh ! High cloud fraction (0-1)
  real,allocatable,dimension(:,:)   :: m_hght ! mixed layer height (m)
  real,allocatable,dimension(:,:)   :: m_usta ! U* (m/s)
  real,allocatable,dimension(:,:)   :: m_u10m ! 10m U-wind, not rotated (m/s)
  real,allocatable,dimension(:,:)   :: m_v10m ! 10m V-wind, not rotated (m/s)


  real,allocatable,dimension(:,:)   :: m_sshf ! Surface sensible heat flux (W/m2)
  real,allocatable,dimension(:,:)   :: m_slhf ! Surface latent heat flux (W/m2)
  real,allocatable,dimension(:,:)   :: m_soim ! Soil Moisture (m3/m3)
  real,allocatable,dimension(:,:)   :: m_swrd ! SW radiation (W/2) (for MEGAN)

  real,allocatable,dimension(:,:,:) :: m_alti ! Altitude field (m)
  real,allocatable,dimension(:,:,:) :: m_pres ! Pressure field (Pa)
  real,allocatable,dimension(:,:,:) :: m_temp ! Temperature field (K)
  real,allocatable,dimension(:,:,:) :: m_sphu ! Mixing ratio field (Kg/Kg)
  real,allocatable,dimension(:,:,:) :: m_winz ! Zonal wind (m/s)
  real,allocatable,dimension(:,:,:) :: m_winm ! Meridional wind (m/s)
  ! deep convection
  real,allocatable,dimension(:,:,:) :: m_divu ! surface velocity divergence
  real,allocatable,dimension(:,:,:) :: m_divq ! surface humidity divergence
  real,allocatable,dimension(:,:,:) :: m_winw ! vertical wind (m/s)
  real,allocatable,dimension(:,:,:) :: m_winwpa ! vertical wind (Pa/s) for deep. conv.
  
  real,allocatable,dimension(:,:,:) :: m_cliq ! Cloud liquid water mixing ratio (Kg/Kg)
  real,allocatable,dimension(:,:,:) :: m_cice ! Ice liquid water mixing ration (Kg/Kg)
  real,allocatable,dimension(:,:,:) :: m_rain ! Rain water mixing ration (Kg/Kg)
  ! Local
  real,allocatable,dimension(:) :: al  ! Local profile of altitude
  real,allocatable,dimension(:) :: cac !
  real,allocatable,dimension(:) :: cbc ! coefficients for layers (top) in CHIMERE 
  real,allocatable,dimension(:) :: de  ! Local profile of density in molec/cm3
  real,allocatable,dimension(:) :: op  ! Local integrated bottom-up optical depth profile
  real,allocatable,dimension(:) :: po  ! Local profile of virtual potential temperature
  real,allocatable,dimension(:) :: pr  ! Local profile of pressure
  real,allocatable,dimension(:) :: qr  ! Local profile of specific humidity
  real,allocatable,dimension(:) :: dv  ! Local profile of velocity divergence
  real,allocatable,dimension(:) :: dq  ! Local profile of humidity divergence
  real,allocatable,dimension(:) :: cl  ! Local profile of cloud liquid water content
  real,allocatable,dimension(:) :: rh  ! Local relative humidity profile
  real,allocatable,dimension(:) :: te  ! Local profile of temperature
  real,allocatable,dimension(:) :: th  ! Local profile of potential temperature
  real,allocatable,dimension(:) :: uw  ! Local profile of zonal  wind
  real,allocatable,dimension(:) :: vw  ! Local profile of merid. wind
  real,allocatable,dimension(:) :: ww  ! Local profile of vertical wind [m/s]
  real,allocatable,dimension(:) :: wwpa  ! Local profile of vertical wind [Pa/s]
  real,allocatable,dimension(:) :: wi  ! Local profile of wind velocity
  real,allocatable,dimension(:,:) :: zom ! Momentum Roughness height
  ! Output
  real,allocatable,dimension(:,:,:) :: d_alti ! height of CHIMERE layer tops
  real,allocatable,dimension(:,:,:) :: d_winz ! Layers zonal wind (m/s)
  real,allocatable,dimension(:,:,:) :: d_winm ! Layers meridional wind (m/s)
  real,allocatable,dimension(:,:,:) :: d_temp ! Layers temperature (K)
  real,allocatable,dimension(:,:,:) :: d_sphu ! Layers specific humidity (Kg/Kg)
  real,allocatable,dimension(:,:,:) :: d_airm ! Layers density(molec/cm3)
  real,allocatable,dimension(:,:,:) :: d_kzzz ! Kz at top CHIMERE layers
  real,allocatable,dimension(:,:,:) :: d_clwc ! Cloud water content (Kg/Kg)
  real,allocatable,dimension(:,:,:) :: d_dpeu ! Entrainment in updraft (kg/m2/s)
  real,allocatable,dimension(:,:,:) :: d_dpdu ! Detrainment in updraft (kg/m2/s)
  real,allocatable,dimension(:,:,:) :: d_dped ! Entrainment in downdraft (kg/m2/s)
  real,allocatable,dimension(:,:,:) :: d_dpdd ! Detrainment in downdraft (kg/m2/s)
  real,allocatable,dimension(:,:,:) :: d_winw ! Layers vertical wind (m/s)
  real,allocatable,dimension(:,:,:)   :: d_tchi ! potential temperature 
  real,allocatable,dimension(:,:)   :: d_tem2 ! 2m temperature (K)
  real,allocatable,dimension(:,:)   :: d_atte ! J attenuation (0-1)
  real,allocatable,dimension(:,:)   :: d_hght ! Mixed layer height (m)
  real,allocatable,dimension(:,:)   :: d_usta ! U* (m/s)
  real,allocatable,dimension(:,:)   :: d_aerr ! Aerodynamic resistance (s/m)
  real,allocatable,dimension(:,:)   :: d_obuk ! Obukov length (m)
  real,allocatable,dimension(:,:)   :: d_wsta ! W* (m/s)
  real,allocatable,dimension(:,:)   :: d_sreh ! Surface relative humidity
  real,allocatable,dimension(:,:)   :: d_topc ! Total precipitation
  real,allocatable,dimension(:,:)   :: d_w10m ! 10 m wind modulus
  real,allocatable,dimension(:,:)   :: d_w10s ! 10 m wind modulus, corrected for saltation
  real,allocatable,dimension(:,:)   :: d_soim ! Soil moisture
  real,allocatable,dimension(:,:)   :: d_swrd ! SW radiation at ground (MEGAN)
 
  ! for deep convection calculations
  real :: hlow,hupp,dratio,dtende,belowflux,fluxbal,evapo
  real :: uwest,qwest,ueast,qeast,usouth,qsouth,unorth,qnorth
  real,allocatable,dimension(:,:,:) :: div     ! Input divergence of velocity (/s)
  real,allocatable,dimension(:,:,:) :: divq    ! Input divergence of humidity (Kg/Kg/s)
  real,allocatable,dimension(:,:,:) :: thlayloc ! vertical layer thickness (m)
  real,allocatable,dimension(:,:,:) :: airmloc ! air density (molec/m3)
  real,allocatable,dimension(:,:,:) :: uwestg,hwestg,swestg
  real,allocatable,dimension(:,:,:) :: ueastg,heastg,seastg
  real,allocatable,dimension(:,:,:) :: usouthg,hsouthg,ssouthg
  real,allocatable,dimension(:,:,:) :: unorthg,hnorthg,snorthg
  real,allocatable,dimension(:,:,:) :: fluxw,fluxe,fluxs,fluxn
  real,allocatable,dimension(:,:,:) :: vfluxo
  real,allocatable,dimension(:,:,:,:) :: vfluxi
  real,allocatable,dimension(:) :: dqc     ! Local profile of humidity divergence
  real,allocatable,dimension(:) :: dvc     ! Local profile of velocity divergence
  real,allocatable,dimension(:) :: alc     ! Local profile of altitude (on chimere layers)
  real,allocatable,dimension(:) :: prc     ! Local profile of pressure (on chimere layers)
  real,allocatable,dimension(:) :: qrc     ! Local profile of specific humidity (on chimere layers)
  real,allocatable,dimension(:) :: wwc     ! Local profile of verti. wind (on chimere layers)
  real,allocatable,dimension(:) :: uvc     ! Local profile of zonal wind (on chimere layers)
  real,allocatable,dimension(:) :: vvc     ! Local profile of merid. wind (on chimere layers)
  real,allocatable,dimension(:) :: tec     ! Local profile of temperature (on chimere layers)
  real,allocatable,dimension(:) :: dpeu      ! Entrainment in updraft (kg/m2/s)
  real,allocatable,dimension(:) :: dpdu      ! Detrainment in updraft  (kg/m2/s)  
  real,allocatable,dimension(:) :: dped      ! Entrainment in downdraft (kg/m2/s)
  real,allocatable,dimension(:) :: dpdd      ! Detrainment in downdraft (kg/m2/s)
  real,allocatable,dimension(:) :: umflc   ! Updraft massflux (kg/m2/s)
  real,allocatable,dimension(:) :: dmflc   ! Downdraft massflux (kg/m2/s)
  real,allocatable,dimension(:)   :: euc     ! Entrainment in updraft (kg/m2/s)
  real,allocatable,dimension(:)   :: duc     ! Detrainment in updraft (kg/m2/s)   
  real,allocatable,dimension(:)   :: edc     ! Entrainment in downdraft (kg/m2/s)
  real,allocatable,dimension(:)   :: ddc     ! Detrainment in downdraft (kg/m2/s)

  ! structured variables to simplify arguments passing
  type(options)          :: opt
  type(diag_misc_type)   :: diag_misc
  namelist /metoptions/ opt

  !*****************************************************************************************
  ! Subroutines
contains

  !****************************************************
  subroutine alloc_diagmet_globals

    use chimere_params
    implicit none

    integer :: nlayers

    nlayers = nvert_raw

    allocate(d_alti(nzonal,nmerid,nlayers))
    allocate(d_winz(nzonal,nmerid,nlayers))
    allocate(d_winm(nzonal,nmerid,nlayers))
    allocate(d_temp(nzonal,nmerid,nlayers))
    allocate(d_sphu(nzonal,nmerid,nlayers))
    allocate(d_airm(nzonal,nmerid,nlayers))
    allocate(d_kzzz(nzonal,nmerid,nlayers))
    allocate(d_clwc(nzonal,nmerid,nlayers))
    allocate(d_tchi(nzonal,nmerid,nlayers))
  ! deep convection
    allocate(d_dpeu(nzonal,nmerid,nlayers))
    allocate(d_dped(nzonal,nmerid,nlayers))
    allocate(d_dpdu(nzonal,nmerid,nlayers))
    allocate(d_dpdd(nzonal,nmerid,nlayers))
    allocate(d_winw(nzonal,nmerid,nlayers))
  ! deep convection
    allocate(d_tem2(nzonal,nmerid))
    allocate(d_atte(nzonal,nmerid))
    allocate(d_hght(nzonal,nmerid))
    allocate(d_usta(nzonal,nmerid))
    allocate(d_aerr(nzonal,nmerid))
    allocate(d_obuk(nzonal,nmerid))
    allocate(d_wsta(nzonal,nmerid))
    allocate(d_sreh(nzonal,nmerid))
    allocate(d_topc(nzonal,nmerid))
    allocate(d_w10m(nzonal,nmerid))
    allocate(d_w10s(nzonal,nmerid))
    allocate(d_soim(nzonal,nmerid))
! SW radiation at ground for MEGAN
    allocate(d_swrd(nzonal,nmerid))

 
  end subroutine alloc_diagmet_globals
 
  !****************************************************
  subroutine dealloc_diagmet_globals

    deallocate(d_alti)
    deallocate(d_winz)
    deallocate(d_winm)
    deallocate(d_temp)
    deallocate(d_sphu)
    deallocate(d_airm)
    deallocate(d_kzzz)
    deallocate(d_clwc)
  ! deep convection
    deallocate(d_dpeu)
    deallocate(d_dped)
    deallocate(d_dpdu)
    deallocate(d_dpdd)
    deallocate(d_winw)
  ! deep convection
    deallocate(d_tem2)
    deallocate(d_atte)
    deallocate(d_hght)
    deallocate(d_usta)
    deallocate(d_aerr)
    deallocate(d_obuk)
    deallocate(d_wsta)
    deallocate(d_sreh)
    deallocate(d_topc)
    deallocate(d_w10m)
    deallocate(d_w10s)
    deallocate(d_soim)
! SW radiation at ground for MEGAN
    deallocate(d_swrd)

  end subroutine dealloc_diagmet_globals
 
end module diagmet_common

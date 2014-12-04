module diagbio_common
! modified  to mic2,enr5 (partialy)  23052008

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

  ! Input
  real,allocatable,dimension(:,:)   :: d_tem2 ! 2m temperature (K)
  real,allocatable,dimension(:,:)   :: d_atte ! J attenuation (0-1)
  real,allocatable,dimension(:,:)   :: d_usta ! U* (m/s)
  real,allocatable,dimension(:,:)   :: d_soim ! Soil moisture
  real,allocatable,dimension(:,:)   :: d_wsta ! W* (m/s)
  real,allocatable,dimension(:,:)   :: d_w10s ! 10 m wind modulus, corrected for saltation
 ! Local variables
  real,allocatable,dimension(:,:,:) :: biop   ! Bioegenic emissions parameters

  ! MEGAN Input
  real,allocatable,dimension(:,:)   :: d_swrd ! SW radiation (W/m2)
  real,allocatable,dimension(:,:,:) :: m_ef   ! MEGAN emission factors (ug/m2/h)
  real,allocatable,dimension(:,:,:) :: m_lai  ! Leaf area index (m2/m2)
  real,allocatable,dimension(:,:,:) :: m_dtem ! Daily temperatures
  real,allocatable,dimension(:,:,:) :: m_drad ! Daily radiation
  ! Output
  real,allocatable,dimension(:,:,:) :: c_biom ! Biogenic emissions
  real,allocatable,dimension(:) :: c_biom_buf ! Biogenic emissions
 real,allocatable,dimension(:,:,:) :: fland   !



  ! structured variables to simplify arguments passing
  type(options)          :: opt
  namelist /metoptions/ opt

contains

  !************************************************
  subroutine alloc_diagbio_globals

    use chimere_params

    allocate(d_wsta(nzonal,nmerid))
    allocate(c_biom(nemisb,nzonal,nmerid))
    allocate(c_biom_buf(nemisb))

  end subroutine alloc_diagbio_globals

  !************************************************
  subroutine dealloc_diagbio_globals

    deallocate(d_wsta)
    deallocate(c_biom)
    deallocate(c_biom_buf)

  end subroutine dealloc_diagbio_globals



end module diagbio_common

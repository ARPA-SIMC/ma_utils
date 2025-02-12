module cosmo_option

  ! Diagmet types
  ! A structure to hold the various options of diagmet
  type :: options
     integer :: idstart    ! Starting date of the simulation
     integer :: nhours       
     character::  fncoord       
     integer :: nzmet       
     integer :: w10m
     integer :: nx,ny
     integer :: uv10m
     integer :: ustar
     integer :: flux
     integer :: hmix
     integer :: cice        
     integer :: rain        
     integer :: clol
     real    :: crhl
     integer :: clom
     real    :: crhm
     integer :: cloh
     real    :: crhh
     integer :: soim
     integer :: cloudw
  end type options

  type(options)          :: opt
  namelist /metoptions/ opt

 
end module cosmo_option

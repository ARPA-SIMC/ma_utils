module diagbio_science

! MOD: erosion and resuspension can be dependent on land use :
!     resuspension flux is multiplied by land fraction, water cells must not
!     contribute  (enr)
!    23/01/2009


  use diagbio_common
  use chimere_consts
  use chimere_params

  !  Physical and mathematical constants specific to diagbio
  real,parameter :: woff    = 0.5         ! Wind offset to smooth Richardson numbers (m/s)

contains

  !*****************************************************************************************
  subroutine calculate_bioemissions(ustas,czen,imon,u10,xsea,izo,ime,fland) 

    ! Calculates biogenic emission (isoprene terpene and NO)
    ! NEMISB IS ASSUMED TO BE 16
    ! NBIOP ASSUMED TO BE = 16

    implicit none

    ! subroutine arguments
    real :: ustas
    real :: czen
    integer :: imon
    real :: u10
    real :: xsea
    integer :: izo,ime
    real :: fland(nzonal,nmerid,nlduse)

    ! parameters
    ! Emissions constants
    real,parameter :: c1 = 95000.0/8.314
    real,parameter :: c2 = 230000.0/8.314
    real,parameter :: c3 = 0.09
    real,parameter :: ts = 303.0
    real,parameter :: tm = 314.0
    ! Aerosols
    real,parameter :: ust0   = 0.1
    real,parameter :: soimt  = 0.1
    real,parameter :: satsm  = 0.4               ! Saturation volumetric moisture content
    real,parameter :: dmine  = 2.5               ! Non porous soil density
    real,parameter :: soild  = (1.-satsm)*dmine  ! Dry porous soil density
    real,parameter :: wated  = 1.0               ! Water density
    real,parameter :: fferod = 0.0040
    real,parameter :: sbeff = 5.e-5              ! Sandblasting efficiency
    real,parameter :: rhoa  = 1200.              ! Air density in g/m3

    ! hidden arguments
    real :: temp
    real :: atte
    real :: usta
    real :: soim
    integer :: iopero
    integer :: iopres

    ! local variable
    real,allocatable,dimension(:) :: bioem
    real :: salt,uthres,tsoil,terfac,ct,cr,r0,r1,r2,rt,ustc,ustm,resuspen
    real :: gsoim,ferod,fachum,tero,soims,fw,resusp1
    real :: unitf1, unitf2

    real ::erl(9)
    integer::nl



    !  Check point
    if(nemisb.ne.16) then
       print *,'*** ERROR: NEMISB MUST BE = 16 IN CALLING PROG'
       stop
    endif

! modifiche
    erl(1)=0.5
    erl(2)=0.5
    erl(3)=0.25
    erl(4)=0.
    erl(5)=1.
    erl(6)=0.75
    erl(7)=0.
    erl(8)=0.
    erl(9)=0.
!!!!


    allocate(bioem(nemisb))
    bioem = 0

    temp    = d_tem2(izo,ime)
    atte    = d_atte(izo,ime)
    usta    = d_usta(izo,ime)
    soim    = d_soim(izo,ime)
    iopero  = opt%eros
    iopres  = opt%resu


    !  Calculation of factors (radiation, temperature) for all species

    tsoil  = 1.03*(temp - t0k) + 2.9
    terfac = exp(c3*(temp - ts))
    ct     = exp(c1*(1.0/ts - 1.0/temp))                              &
         &       / (1.0 + exp(c2*(temp - tm) / (ts * temp)))
    if(czen.le.1e-2) then
       cr = 0.0
    else
       r0 = 6.89634*atte
       r1 = r0*exp(-0.16/czen)*czen
       r2 = 0.4*(r0 - r1)*czen
       rt = r1 + r2
       cr = rt/sqrt(1.0 + rt*rt)
    endif

    !  Simple Coarse Dust Aerosol emission wind erosion scheme
    !  Assumes an arbitrary "molar mass" of 100 for the dust class

    gsoim = soim*wated/soild
    if(gsoim.gt.soimt) then
       ustc = ust0*sqrt(1.0+1.21*(100.*(gsoim-soimt))**0.68)
    else
       ustc = ust0
    endif
    ustm = min(ustas,1.00)   ! Parameterizations fitted with u*<1

    !  Emissions in g/m2/s

!originale    ferod = 2.d+00*float(iopero)*fferod       ! Erodibility factor to be tuned
!modifiche
      if(iopero==0) then
        ferod=0
      else if(iopero==1) then
         ferod=2*iopero*fferod
      else if(iopero==2) then
          ferod=0
        do nl=1,9
        ferod=ferod+erl(nl)*fland(izo,ime,nl)*fferod
        end do
      end if
!end modifiche


    unitf1 = 0.01*avogadro*1e-4 ! Unit factor for molec/cm2/s

    !  Inhibitates dust erosion for GSOIM > 0.2

    if(gsoim.ge.0.2) then
       fachum = 0.
    elseif(gsoim.le.0.16) then
       fachum = 1.
    else
       fachum = (0.2 - gsoim)/0.04
    endif


    if(ustm.gt.ustc) then
     if(iopero==1) then
       tero = unitf1*(fland(izo,ime,1))*ferod*sbeff*2.61*(rhoa/g)    &
            &       * fachum*ustm**3*(1.-ustc**2/ustm**2)
     else if(iopero==2) then
       tero = unitf1*(1-fland(izo,ime,4)-fland(izo,ime,9))*ferod*sbeff*2.61*(rhoa/g)  &
                    * fachum*ustm**3*(1.-ustc**2/ustm**2)
    end if
    else
       tero = 0.
    endif

    !  Partition into 90% of Coarse and 10% of fine particles
    !  Half of emission is > 10 um


    bioem(9) = 0.5*tero
    bioem(10) = 0.45*tero
    bioem(11) = 0.05*tero

    !  Simple dust Resuspension scheme depending only on soil moisture

    soims = 0.3*wated/soild
    fw = min(1.,max(0.,(soims - gsoim)/(soims - soimt)))
! start modification 
      if(iopres==0) then
        resusp1=0
      else if(iopres==1) then
       resusp1  = fland(izo,ime,1)*iopres*1.8e3   ! originale
      else if(iopres==2) then
          ferod=0
        do nl=1,9
        resusp1=resusp1+erl(nl)*fland(izo,ime,nl)*1.8e3
        end do
       endif
! end modification

    unitf2   = 1e-2*1e-6*avogadro*1e-4/3600.  ! Transfo into molec/cm2/s
    resuspen = unitf2*fw*resusp1*usta**1.43   ! Resuspension flux

    !  Partition into 1/3 of Coarse and 2/3 of fine particles

    bioem(9) = bioem(9) + 0.00*resuspen
    bioem(10) = bioem(10) + 0.33*resuspen
    bioem(11) = bioem(11) + 0.67*resuspen


    !  Sea salt emissions

    uthres=min(30.,u10)
    salt=xsea*2.75e-14*((uthres)**3.41)*6.02e23/58.5

    bioem(12)=salt*0.25
    bioem(13)=salt*0.79*0.25                  ! Na
    bioem(14)=salt*0.91*0.25                  ! Cl
    bioem(15)=salt*0.05*0.25                  ! Sea SO4
    bioem(16)=salt*0.75*3.25                  ! Aerosol marine water emission
    
    c_biom(:,izo,ime) = bioem

    deallocate(bioem)

  end subroutine calculate_bioemissions

end module diagbio_science

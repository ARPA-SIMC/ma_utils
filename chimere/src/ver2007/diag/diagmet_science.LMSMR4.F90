
module diagmet_science
  !MODIFICHE
  !:: (mic1)  bug at   subroutine layer_top_altitudes 
  !:: (mic2)  allows minimal HMix to be parabolic function of urban fraction
  !           set  maximun value for mixing height     
  !:: (enr4)  prevent very stable condition in urban areas
  !            set a maximun value for abs(MO)
  !           set a minimum vale for kz in urban area
  !:: (enr5)  disable mixing  height enhancement due to clouds
  !           disable Kz  enhancement with in low  levels clouds (<1000 m)
  !           the exponent of Kz profile withn BPL is user define 
  !:: (joh)   uses Marth 1982 for stable cases insetad of Troen and Mahrth 1986

! 15/12/2008
  ! contains various scientific subroutines

  use diagmet_common
  use chimere_consts
  use chimere_params

  !  Physical and mathematical constants specific to diagmet
  real,parameter :: soimdef = 1.0         ! Default Soil Moisture (no resuspension/erosion)
  real,parameter :: woff    = 0.5         ! Wind offset to smooth Richardson numbers (m/s)
  real,parameter :: crhx    = 0.90        ! Min RH for cloud BLH enhancement
  real,parameter :: vkminup = 0.1         ! Minimum Kz above PBL (m2/s)
  real,parameter :: vkmax   = 500.        ! Maximum Kz
  real,parameter :: odclw   = 0.18e3      ! Optical depth rate for liquid water
  real,parameter :: odcic   = 0.06e3/0.9  ! Optical depth rate for ice

  ! external functions
  interface
     function notzero(x,limit)
       real :: x,limit
       real :: notzero
     end function notzero
  end interface

contains

  !****************************************************
  subroutine defcolumn(izo,ime)
    ! Definition of the 1D local column basic variables
    ! Calculation of additional column variables

    implicit none

    ! subroutine arguments
    integer,intent(in)       :: izo,ime

    ! hidden subroutine arguments
    integer :: nlevels  ! in

    ! parameters

    ! local variables
    integer :: ilev
    real    :: vapp,qsbt
    real,allocatable,dimension(:)  :: ci  ! Local profile of ice content
    real,allocatable,dimension(:)  :: rw  ! Local profile of rain water
    real,allocatable,dimension(:)  :: wa  ! Local profile of water column
    real,allocatable,dimension(:)  :: ro  ! Local profile of density in Kg/m3

    nlevels = diag_misc%nlevels

    allocate(ci(0:nlevels))
    allocate(rw(0:nlevels))
    allocate(wa(0:nlevels))
    allocate(ro(0:nlevels))

    ! Definition of the 1D local column basic variables
    do ilev=1,nlevels
       al(ilev) = m_alti(izo,ime,ilev)
       pr(ilev) = m_pres(izo,ime,ilev)
       uw(ilev) = m_winz(izo,ime,ilev)
       vw(ilev) = m_winm(izo,ime,ilev)
       te(ilev) = m_temp(izo,ime,ilev)
       qr(ilev) = m_sphu(izo,ime,ilev)
       cl(ilev) = m_cliq(izo,ime,ilev)
       ci(ilev) = m_cice(izo,ime,ilev)
       rw(ilev) = m_rain(izo,ime,ilev)
       ! deep convection -> w must be in Pa/s
       ww(ilev) = m_winw(izo,ime,ilev)
       wwpa(ilev) = m_winwpa(izo,ime,ilev)
       dv(ilev) = m_divu(izo,ime,ilev)
       dq(ilev) = m_divq(izo,ime,ilev)
    enddo

    al(0) = 0.
    pr(0) = pr(1)+(pr(1)-pr(2))*(al(1)/(al(2)-al(1)))

if (izo == 145 .AND. ime == 7) THEN
  print *,pr
  print *,al
endif

    uw(0) = 0.0
    vw(0) = 0.0
    ww(0) = 0.0
    te(0) = m_tem2(izo,ime) - (te(1) - m_tem2(izo,ime))*2./(al(1)-2.)
    qr(0) = max(1e-10,qr(1) - (qr(2) - qr(1)) &
         * al(1)/(al(2) - al(1)))
    cl(0) = max(1e-10,cl(1) - (cl(2) - cl(1)) &
         * al(1)/(al(2) - al(1)))
    ci(0) = max(1e-10,ci(1) - (ci(2) - ci(1)) &
         * al(1)/(al(2) - al(1)))
    rw(0) = max(1e-10,rw(1) - (rw(2) - rw(1)) &
         * al(1)/(al(2) - al(1)))
    wi(0) = 0.0
    op(0) = 0.0
    dv(0) = 0.0
    dq(0) = 0.0

    ! Calculation of additional column variables
    do ilev=0,nlevels
       wa(ilev) = cl(ilev) + ci(ilev) + rw(ilev)
       po(ilev) = (1.0 + 0.61*qr(ilev) - wa(ilev)) &
            * te(ilev)*(p0/pr(ilev))**xkappa
       th(ilev) = te(ilev)*(p0/pr(ilev))**xkappa
       wi(ilev) = sqrt(uw(ilev)**2 + vw(ilev)**2)
       de(ilev) = 7.2868e16*pr(ilev)/te(ilev)
       ro(ilev) = pr(ilev)/(R*te(ilev))
       !vapp = 611.*exp(17.27*(te(ilev)-t0k) &
       vapp = 611.*exp(17.27*(te(ilev)-273.15) &
            /(te(ilev)-35.86))
       qsbt = 0.622*vapp/(pr(ilev)-vapp)
       rh(ilev) = qr(ilev)/qsbt
       if(ilev.ge.1) then
          op(ilev) = op(ilev-1)                          &
               + 0.5*((cl(ilev  )+rw(ilev  ))*ro(ilev  ) &  ! Water clouds
               +(cl(ilev-1)+rw(ilev-1))*ro(ilev-1))      &
               *(al(ilev) - al(ilev-1))*odclw            &
               + 0.5*(ci(ilev  )*ro(ilev  )              &  ! Ice clouds
               + ci(ilev-1)*ro(ilev-1))                  &
               *(al(ilev) - al(ilev-1))*odcic
       endif
    enddo
    d_sreh(izo,ime) = min(1.,max(0.,rh(0)))

    deallocate(ci)
    deallocate(rw)
    deallocate(wa)
    deallocate(ro)

  end subroutine defcolumn

  !****************************************************
  subroutine low_cloud_top
    ! Low cloud top and rhmaxx

    implicit none

    ! hidden subroutine arguments
    integer :: nlevels ! in
    real :: rhmaxx     ! out
    real :: topcld     ! out
    real :: topcldw    ! out

    ! parameters
    real,parameter :: topcldmax=1000.

    ! local variables
    integer :: ilev

    nlevels = diag_misc%nlevels
    topcld = -1.
    rhmaxx = crhx
    do ilev=0,nlevels-1
       if(al(ilev).le.topcldmax) then
          rhmaxx = min(1.,max(rhmaxx,rh(ilev)))
          if(rh(ilev).ge.crhx) then
             topcld = al(ilev+1)
             if(rh(ilev+1).lt.crhx) then
                topcld = al(ilev)+(al(ilev+1)-al(ilev))*(rh(ilev)-crhx) &
                     /(rh(ilev)-rh(ilev+1))
             endif
          endif
       endif
    enddo
    topcld = min(topcld,topcldmax)
    topcldw = (rhmaxx - crhx)/(1. - crhx)

    diag_misc%topcld  = topcld
    diag_misc%topcldw = topcldw
    diag_misc%rhmaxx  = rhmaxx

  end subroutine low_cloud_top

  !****************************************************
  subroutine layer_top_altitudes(izo,ime,nlayers)
    ! CHIMERE Layer top altitudes

    implicit none

    ! subroutine arguments
    integer,intent(in)       :: izo,ime
    integer,intent(in)       :: nlayers

    ! hidden subroutine arguments
    integer :: nlevels  ! in

    ! parameters

    ! local variables
    integer :: ilay
    integer :: ilev
    logical :: ok
    real :: p

    !mic1    ok = .false.
    nlevels = diag_misc%nlevels

    do ilay=1,nlayers
       ok=.false.
       p = cac(ilay)*p0 + cbc(ilay)*pr(0)
       do ilev=0,nlevels-1
          if (p.gt.pr(ilev+1)) then
             d_alti(izo,ime,ilay) = al(ilev) + (al(ilev+1)-al(ilev)) &
                  * (pr(ilev)-p)/(pr(ilev)-pr(ilev+1))
             ok=.true.
             exit
          endif
       enddo
       if (.not. ok) then
          if(ilay.lt.nlayers) then
             print *,'*** PROBLEM: ALTITUDE OF LAYER:',ilay &
                  ,' ABOVE TOP INPUT DATA'
             print *,izo,ime,ilay,p/100,pr(0)/100,pr(1),pr(nlevels)
             stop '*** Exiting'
          else
             d_alti(izo,ime,nlayers) = al(nlevels)
          endif
       end if
    enddo
  end subroutine layer_top_altitudes

  !****************************************************
  subroutine cloud_optical_thickness(izo,ime)
    ! Clouds Optical Thickness

    implicit none

    ! subroutine arguments
    integer,intent(in)       :: izo,ime

    ! hidden subroutine arguments
    integer :: nlevels      ! in
    real :: crhl,crhm,crhh  ! in
    real :: opdl,opdm,opdh  ! out
    real :: opl,opm,oph     ! out

    ! parameters
    real,parameter :: cloh0= 2.0           ! High   clouds optical depth for cl fraction=1
    real,parameter :: clom0=10.0           ! Medium clouds optical depth for cl fraction=1
    real,parameter :: clol0=50.0           ! Low    clouds optical depth for cl fraction=1
    real,parameter :: clol2=0.025          ! Low    clouds optical depth /m for RH=1
    real,parameter :: clom2=0.010          ! Medium clouds optical depth /m for RH=1
    real,parameter :: cloh2=0.005          ! High   clouds optical depth /m for RH=1
    real,parameter :: topl=2500.           ! Low cloud top altitude AGL
    real,parameter :: topm=6000.           ! Medium cloud top altitude AGL
    real,parameter :: toph=20000.          ! High cloud top altitude AGL

    ! local variables
    integer :: ilev
    real :: totopd
    integer :: levl,levm,levh
    real :: opdl1,opdm1,opdh1
    real,allocatable,dimension(:) :: or         ! Working array

    nlevels = diag_misc%nlevels
    crhl = opt%crhl
    crhm = opt%crhm
    crhh = opt%crhh


    allocate(or(0:nlevels))

    ! First determination of levels just below separation altitudes

    do ilev=0,nlevels-1
       if(al(ilev).lt.topl.and.al(ilev+1).ge.topl) levl = ilev
       if(al(ilev).lt.topm.and.al(ilev+1).ge.topl) levm = ilev
       if(al(ilev).lt.toph.and.al(ilev+1).ge.topl) levh = ilev
    enddo
    if(al(nlevels).lt.topl) levl = nlevels
    if(al(nlevels).lt.topm) levm = nlevels
    if(al(nlevels).lt.toph) levh = nlevels

    !  Determination of liq/ice-water integrated COT

    if(levl.lt.nlevels) then
       opl = op(levl) + &
            (op(levl+1)-op(levl)) * (topl-al(levl)) / (al(levl+1)-al(levl))
    else
       opl = op(levl)
    endif
    if(levm.lt.nlevels) then
       opm = op(levm) + &
            (op(levm+1)-op(levm)) * (topm-al(levm)) / (al(levm+1)-al(levm))
    else
       opm = op(levm)
    endif
    if(levh.lt.nlevels) then
       oph = op(levh) + &
            (op(levh+1)-op(levh)) * (toph-al(levh)) / (al(levh+1)-al(levh))
    else
       oph = op(levh)
    endif

    ! 1: Option using cloudiness
    if(opt%cloh.eq.0) opdh = cloh0*m_cloh(izo,ime)
    if(opt%clom.eq.0) opdm = clom0*m_clom(izo,ime)
    if(opt%clol.eq.0) opdl = clol0*m_clol(izo,ime)

    ! 2: Option using liquid/ice water
    if(opt%cloh.eq.1) opdh = oph - opm
    if(opt%clom.eq.1) opdm = opm - opl
    if(opt%clol.eq.1) opdl = opl

    ! 3: Option using relative humidity
    if(opt%clol.eq.2) then
       or = 0.
       do ilev=1,nlevels
          or(ilev) = or(ilev-1) + &
               max(0.,0.5*(rh(ilev)+rh(ilev-1))-crhl) &
	       *clol2*(al(ilev)-al(ilev-1))/(1.-crhl)
       enddo
       if(levl.lt.nlevels) then
          opdl = or(levl) + &
               (or(levl+1)-or(levl)) * (topl-al(levl)) / (al(levl+1)-al(levl))
       else
          opdl = or(levl)
       endif
    endif

    if(opt%clom.eq.2) then
       or(0) = 0.
       do ilev=1,nlevels
          or(ilev) = or(ilev-1) + &
               max(0.,0.5*(rh(ilev)+rh(ilev-1))-crhm)&
               *clom2*(al(ilev)-al(ilev-1))/(1.-crhm)
       enddo
       if(levl.lt.nlevels) then
          opdl1 = or(levl) + &
               (or(levl+1)-or(levl))*(topl-al(levl)) / (al(levl+1)-al(levl))
       else
          opdl1 = or(levl)
       endif
       if(levm.lt.nlevels) then
          opdm1 = or(levm) + &
               (or(levm+1)-or(levm))*(topm-al(levm)) / (al(levm+1)-al(levm))
       else
          opdm1 = or(levm)
       endif
       opdm = opdm1 - opdl1
    endif

    if(opt%cloh.eq.2) then
       or(0) = 0.
       do ilev=1,nlevels
          or(ilev) = or(ilev-1) + &
               max(0.,0.5*(rh(ilev)+rh(ilev-1))-crhh)&
	       *cloh2*(al(ilev)-al(ilev-1))/(1.-crhh)
       enddo
       if(levm.lt.nlevels) then
          opdm1 = or(levm) + &
               (or(levm+1)-or(levm))*(topm-al(levm)) / (al(levm+1)-al(levm))
       else
          opdm1 = or(levm)
       endif
       if(levh.lt.nlevels) then
          opdh1 = or(levh) + &
               (or(levh+1)-or(levh))*(toph-al(levh)) / (al(levh+1)-al(levh))
       else
          opdh1 = or(levh)
       endif
       ! bug correction lmbb 2007/04       
       opdh = opdh1 - opdm1
    endif

    ! Contribution from all cloud levels
    totopd = opdl + opdm + opdh
    d_atte(izo,ime) = exp(-0.11*totopd**0.67)

    diag_misc%opdl = opdl
    diag_misc%opdm = opdm
    diag_misc%opdh = opdh
    diag_misc%opl = opl
    diag_misc%opm = opm
    diag_misc%oph = oph

    deallocate(or)
  end subroutine cloud_optical_thickness

  !****************************************************
  subroutine mean_z0_shf_extra_urban_temp(imon,zom,fland,izo,ime)
    ! Calculation of mean Z0 and sensible heat flux and extra urban temper.
    ! and wind correction

    implicit none

    ! subroutine arguments
    integer,intent(in)                           :: imon
    real,dimension(:,:),intent(in)               :: zom   ! Momentum Roughness height
    real,dimension(:,:,:),intent(in)             :: fland ! Land use fractions
    integer,intent(in)                           :: izo,ime

    ! hidden subroutine arguments
    real :: upm   ! in   ! Minimal PBL correction
    real :: uwc   ! in   ! Urban wind correction
    real :: ufx   ! in   ! Urban heat flux correction
    real :: apm   ! out
    real :: az0   ! out
    real :: awf   ! out
    real :: auf   ! out

    ! parameters
    real,parameter :: pblmin=20.       ! Minimum PBL height

    ! local variables
    real    :: wf,uf,pm
    integer :: ilan


    az0    = 0.
    awf    = 0.
    auf    = 0.
    apm    = 0.

    upm =  opt%upm
    uwc =  opt%uwc
    ufx =  opt%ufx


    do ilan=1,nlduse
       if(ilan.ne.5) then
          wf = 1.0
          uf = 0.0
          pm = pblmin
       else
          wf = uwc
          uf = ufx
          pm = max(pblmin,upm)
       endif
       az0  = az0 + fland(izo,ime,ilan)*zom(ilan,imon)
       awf  = awf + fland(izo,ime,ilan)*wf
       auf  = auf + fland(izo,ime,ilan)*uf
       apm  = apm + fland(izo,ime,ilan)*pm
    enddo

    !mic2

    if(opt%upblcor.eq.1)  then
!         write(6,*)'corr mic2'
       apm=-(upm-pblmin)*fland(izo,ime,5)**2+ 2*(upm-pblmin)*fland(izo,ime,5)+pblmin
    endif
    !mic2


    diag_misc%apm = apm
    diag_misc%az0 = az0
    diag_misc%awf = awf
    diag_misc%auf = auf

  end subroutine mean_z0_shf_extra_urban_temp

  !****************************************************
  subroutine sv_heat_flux(izo,ime)
    ! Calculation of sensible and virtual heat fluxes in K.m/s

    implicit none

    ! subroutine arguments
    integer,intent(in)        :: izo,ime

    ! hidden subroutine arguments
    real :: auf     ! in
    real :: potfm   ! out
    real :: potf    ! out
    real :: wstar0  ! out

    ! parameters

    ! local variables
    real :: heat,humf,dt1

    auf=diag_misc%auf

    if(opt%flux.eq.0) then
       heat = rsurcp*m_tem2(izo,ime)*m_sshf(izo,ime)/pr(0)
       humf = rsurl *m_tem2(izo,ime)*m_slhf(izo,ime)/pr(0)
       potf = heat*(1 + 0.61*qr(0)) + humf*0.61*th(0) + rsurcp*m_tem2(izo,ime)*auf/pr(0)

    elseif(opt%flux.eq.1) then
       ! Use the Priestley (1948) Formula
       if(po(2).lt.po(1)) then
          dt1 = (po(1)-po(2))**1.5
       else
          dt1 = 0.0
       endif
       potf = dt1*(2*g/(27*po(1)))**0.5        &
            / ((al(1)**(-1./3.)                &
            - ((al(2)+al(1)))**(-1./3.))**1.5) &
            + rsurcp*m_tem2(izo,ime)*auf/pr(0)
    endif
    potfm = max(potf,1.e-6)
    wstar0 = (g*potfm*chs0/m_tem2(izo,ime))**(1./3.)

    diag_misc%wstar0 = wstar0
    diag_misc%potf   = notzero(potf,1.e-30)
    diag_misc%potfm  = potfm

  end subroutine sv_heat_flux

  !****************************************************
  subroutine friction_velocity(izo,ime)
    ! Friction velocity from iboput (Option 0) and from Louis 82 (Option 1)

    implicit none

    ! subroutine arguments
    integer,intent(in)       :: izo,ime

    ! hidden subroutine arguments
    real    :: awf     ! in
    real    :: az0     ! in
    real    :: wstar0  ! in
    integer :: nlevels ! in

    ! parameters

    ! local variables
    real    :: w10s
    real    :: ustar,vustar,dtheta,rich,zustar,cdnm,cdn2,facm,fm,cd
    integer :: ilev

    awf     = diag_misc%awf
    az0     = diag_misc%az0
    wstar0  = diag_misc%wstar0
    nlevels = diag_misc%nlevels

    !  1: Determine variables for calculation

    if(opt%w10m.eq.0) then
       w10s = awf*d_w10m(izo,ime)
    else
       do ilev=0,nlevels-1
          if(al(ilev).lt.10..and.al(ilev+1).ge.10.) then
             w10s  = wi(ilev) + &
                  (wi(ilev+1) - wi(ilev)) * (10. - al(ilev))/(al(ilev+1) - al(ilev))
             w10s = awf*w10s
             go to 3765
          endif
       enddo
3765   continue
    endif

    ! 2: Use input data (Option 0) or Louis 82 (Option 1)

    if(opt%usta.eq.0) then
       ustar = m_usta(izo,ime)
    else
       zustar = 10.
       vustar = max(w10s,woff)
       dtheta = po(1) - po(0)
       rich = dtheta*g*zustar/(th(0)*vustar**2)
       cdnm = vkarm/log(zustar/az0)
       cdn2 = cdnm*cdnm
       facm = 75.0*cdn2*sqrt(zustar/az0)
       if(rich.lt.0.0) then
          fm = 1.0 - 10.0*rich/(1.0 + facm*sqrt(-rich))
       else
          fm = 1.0/(1.0 + 10.0*rich/sqrt(1.+5.*rich))
       endif
       cd = cdnm*sqrt(fm)
       ustar = cd*sqrt(vustar**2+(1.2*wstar0)**2)
    endif

    d_usta(izo,ime) = ustar
    d_w10s(izo,ime) = w10s

  end subroutine friction_velocity

  !***************************************************
  subroutine boundary_layer_hght(izo,ime)
    ! Calculation of Boundary Layer Height using a simplified version
    ! of Cheinet 2003 with one thermal mixed with Troen-Mahrt 1986
    ! in stable cases.

    implicit none

    ! subroutine arguments
    integer,intent(in)       :: izo,ime

    ! hidden subroutine arguments
    real :: atu        ! in
    real :: az0        ! in
    real :: apm        ! in
    real :: potfm      ! in
    real :: topcld     ! in
    real :: topcldw    ! in
    integer :: nlevels ! in
    real :: potts      ! out

    ! parameters
    real,parameter :: ztherm=25.0     ! Height of thermals start
    real,parameter :: ric=0.5         ! Troen & Mahrt suggestion for critical Ri for BL top
    real,parameter :: dvsca=1000.0    ! Entrain./Detrain. first guess vertical scale (m)
    real,parameter :: fcor=0.0001     ! joh
    ! local variables
    real,allocatable,dimension(:) :: wsq ! Local profile of thermal velocity
    real,allocatable,dimension(:) :: dth ! Local profile of thermal excess temperature
    integer :: ilev
    integer :: levstart
    integer :: nit
    real :: ustar
    real :: zh,tstart,x1,x2,z1,z2,pf,wlf,tlf,sigw,sigt
    real :: dt0,w20,zii,dz,zmid,vscale,beta1,beta2,buoy
    real :: dtstart,w2start,z0,zt          
    real :: pott0,pott1
    real :: testscale
    ! mic2
    real::zimax
    zimax =  opt%zimax

    apm     = diag_misc%apm
    atu     = diag_misc%atu
    az0     = diag_misc%az0
    potfm   = diag_misc%potfm
    topcld  = diag_misc%topcld
    topcldw = diag_misc%topcldw
    ustar   = d_usta(izo,ime)
    nlevels = diag_misc%nlevels

    allocate(wsq(0:nlevels))
    allocate(dth(0:nlevels))

    !  Calculation of Boundary Layer Height using a simplified version
    !  of Cheinet 2003 with one thermal mixed with Troen-Mahrt 1986
    !  in stable cases.
    !  Model level for thermals start

    do ilev=0,nlevels-1
       if(al(ilev).lt.ztherm.and.al(ilev+1).ge.ztherm) then
          potts = po(ilev) + &
               (po(ilev+1) - po(ilev)) * (ztherm - al(ilev))/(al(ilev+1) - al(ilev))
          levstart = ilev
          go to 4976
       endif
    enddo
4976 continue

    !  1: Stable case parameterization:
    !  Mixed layer height from Troen & Mahrt 1986  in fact
    !  calculated at all times but with an extra temperature
    !  being 0. ZH is then the height at which Ri = RIC

    if(opt%pblh.eq.0) then
       d_hght(izo,ime) = m_hght(izo,ime)      ! will be refined at end of subroutine
    end if
    if(opt%pblh.gt.0) then

       if(opt%pblh.eq.1) then
          zh = al(nlevels)
          tstart = potts
          do ilev=levstart,nlevels-1
             z1 = max(al(ilev),az0)
             z2 = al(ilev+1)
             x1 = tstart + ric*tstart*wi(ilev)**2/(g*z1)
             x2 = tstart + ric*tstart*wi(ilev+1)**2/(g*z2)
             if(x1.gt.po(ilev).and.x2.le.po(ilev+1)) then
                pf=(x1-po(ilev))/(x1-x2+po(ilev+1)-po(ilev))
                zh = z1 + pf*(z2 - z1)
                go to 9027
             endif
          enddo
9027      continue
          ! joh    Mixing heigth from Marth
       else if(opt%pblh.eq.2) then
!          write(6,*)'corr joh'
          zh = 0.06*ustar/fcor

       end if


       !  2: Unstable case: Thermals with an equation for theta
       !  and W with entrainment/detrainment as suggested by
       !  Cheinet et al. (2002)

       !  Standard deviations of thetav and w at reference level
       !  Taken from free convection case (Stull 88)

       wlf  = (g*potfm*ztherm/potts)**(1./3.)    ! Local v. velocity scale
       tlf  = potfm/wlf                          ! Local temperature scale
       sigw = max(sqrt(1.6)*wlf,sqrt(2.5)*ustar) ! W stand. dev.
       sigt = sqrt(2.0)*tlf                      ! Theta stand. dev.

       !  Thermal starts with 2*sigma for theta and 1*sigma for W, following results
       !  of Cheinet et al. (2002)

       dt0 = 2*sigt
       w20 = sigw

       !  The vertical velocity scale of entrainment/detrainment depends on zi
       !  then an iteration is necessary. zi is initialized at 1000m

       zii = dvsca

       do nit=1,10

          !  Vertical integration between reference level (ZTHERM) and the
          !  native level just above (LEVSTART+1)

          dz = al(levstart+1) - ztherm
          zmid = 0.5*(al(levstart+1) + ztherm)
          vscale = min(zii,5*zmid)
          beta1 = exp(-dz/vscale)
          beta2 = exp(-2*dz/vscale)
          buoy = (po(levstart+1) - potts)/dz
          dtstart = beta1*dt0 - 0.5*buoy*dz*(1. + beta1)
          w2start = beta2*w20 + dz*g*(beta2*dt0/potts + dtstart/po(levstart+1))

          wsq(levstart+1) = w2start
          dth(levstart+1) = dtstart

          if(w2start.le.0.0.or.dtstart.lt.0.0) then
             zt = ztherm
          else
             do ilev=levstart+1,nlevels-1
                z0 = al(ilev)
                z1 = al(ilev+1)
                pott0 = po(ilev)
                pott1 = po(ilev+1)
                dz = z1 - z0
                zmid = 0.5*(z0 + z1)
                vscale = min(zii,5*zmid)
                beta1 = exp(-dz/vscale)
                beta2 = exp(-2*dz/vscale)
                buoy = (pott1 - pott0)/dz
                dth(ilev+1) = beta1*dth(ilev) - 0.5*buoy*dz*(1. + beta1)
                wsq(ilev+1) = beta2*wsq(ilev) + dz*g*(beta2*dth(ilev)/pott0 + dth(ilev+1)/pott1)

                !  Stop when wsq is zero

                if(wsq(ilev).ge.0.and.wsq(ilev+1).lt.0) then
                   zt = z0 - wsq(ilev)*dz/(wsq(ilev+1) - wsq(ilev))
                   go to 1395
                endif
             enddo
             zt = al(nlevels)
1395         continue
          endif

          testscale = abs((zii-0.6*zt)/zii)
          if(testscale.lt.0.01) then
             go to 5762
          else
             zii = 0.6*zt
          endif
       enddo
5762   continue

       !  Height of the dry boundary layer taken as the max of the two estimates

       d_hght(izo,ime) = max(zh,zt)
    endif

    !  Minimal PBLH

    d_hght(izo,ime) = max(d_hght(izo,ime),apm)



    !mic2
    d_hght(izo,ime) = min(d_hght(izo,ime),zimax)
!     write(6,*)'corr mic2 hmix'
    !mic2
    !  Enhancement due to clouds


    if(topcld.gt.d_hght(izo,ime)) then
       !enr5
       if(opt%cldmix.eq.1) then
!     write(6,*)'corr enr5 hgt'
       else if(opt%cldmix.eq.0) then
          d_hght(izo,ime) = (1.-topcldw)*d_hght(izo,ime) + topcldw*topcld
       endif
       !enr5
    end if

    deallocate(wsq)
    deallocate(dth)

    diag_misc%potts = potts

  end subroutine boundary_layer_hght

  !****************************************************
!    subroutine obukov_length(izo,ime)
  subroutine obukov_length(izo,ime,fland)  ! enr4
    ! Obukov length and related parameters

    implicit none

    ! subroutine arguments
    integer,intent(in)     :: izo,ime
    real,dimension(:,:,:),intent(in)     :: fland

    ! hidden subroutine arguments
    real :: potts  ! in
    real :: potf   ! in
    real :: potfm  ! in
    real :: az0    ! in

    ! parameters

    ! local variables
    real :: obuklen
    real :: ustar
    real :: wstar
    real :: ra
    real :: zeta,zeta0,zchim1,zchim2,zln,eta0,eta

    real::uframin,umomin   ! enr4

    uframin=opt%uframin
    umomin=opt%umomin 

    potts = diag_misc%potts
    ustar = d_usta(izo,ime)
    potf  = diag_misc%potf
    potfm = diag_misc%potfm
    az0   = diag_misc%az0

    zchim1 = d_alti(izo,ime,1)
    zchim2 = zchim1*0.5

    obuklen  = -potts*ustar**3/(vkarm*g*potf)
    !enr4: set  maximum malue for ABS(MO) and prevent very stable condition in urban area


    if(obuklen.lt.-1.e4) obuklen=-1.e4
    if(obuklen.gt.1.e4) obuklen=1.e4
    if(obuklen.gt.0.and.obuklen.lt.umomin.and.fland(izo,ime,5).ge.uframin) obuklen=umomin
    !enr4   

    zeta  = zchim2/obuklen
    zeta0 = az0/obuklen
    zln   = log(zchim2/az0)
    if(obuklen.ge.0.) then
       ra = (zln + 4.7*(zeta - zeta0))/(vkarm*ustar)
       wstar = 0.0
    else
       eta0  = (1.-15.*zeta0)**0.25
       eta   = (1.-15.*zeta)**0.25
       ra = (zln + log((eta0**2+1)*(eta0+1)**2 &
            /(eta**2+1)/(eta+1)**2) &
            + 2*(atan(eta) - atan(eta0)))/(vkarm*ustar)
       wstar = (g*potfm*d_hght(izo,ime)/potts)**(1./3.)
    endif

    ! 2D Variables to be output
    d_aerr(izo,ime) = ra
    d_obuk(izo,ime) = obuklen
    d_wsta(izo,ime) = wstar

  end subroutine obukov_length


  !****************************************************
  !  subroutine vertical_turbulent_diffusivity(izo,ime,nlayers) 
  subroutine vertical_turbulent_diffusivity(izo,ime,nlayers,fland)  ! enr4
    ! Vertical turbulent diffusivity at top CHIMERE layers

    implicit none

    ! subroutine arguments
    integer,intent(in)                 :: izo,ime
    integer,intent(in)                 :: nlayers
    real,dimension(:,:,:),intent(in)   :: fland

    ! hidden subroutine arguments
    real :: rhmaxx     ! in
    integer :: nlevels ! in

    ! parameters
    real,parameter :: vkmindry=0.1   ! Minimum Kz in the dry boundary layer (m2/s)
    real,parameter :: vkminwet=5.0   ! Minimum Kz in cloudy boundary layer (m2/s)
    real,parameter :: rlam=150.      ! Upper air mixing length

    ! local variables
    real :: obuklen
    real :: ustar
    real :: wstar
    real :: pblh
    real :: vkminbl,zn,ep,zsl,wc,dzz,ss
    !real :: vkminur
    real :: rig,alph,chi,qbar,tbar,dk,srig,upkz
    real :: fk
    integer :: ilay,ilev,il
    real::ukzmin,uframin
    integer::kzexp
    rhmaxx  = diag_misc%rhmaxx
    nlevels = diag_misc%nlevels
    obuklen = d_obuk(izo,ime)
    pblh    = d_hght(izo,ime)
    ustar   = d_usta(izo,ime)
    wstar   = d_wsta(izo,ime)



    ukzmin=opt%ukzmin
    kzexp=opt%kzexp       
    uframin=opt%uframin      
    !enr5
    if(opt%cldmix.eq.0) then
!        write(6,*)'corr enr5'
       vkminbl = vkmindry + (vkminwet-vkmindry) &
            * (rhmaxx-crhx)/(1.-crhx)
    else
       vkminbl = vkmindry 
    end if
    ! enr5 
    ! enr4 set a minimun value kz for urban areas
    
    if(fland(izo,ime,5).ge.uframin) then
!         write(6,*)'corr enr4'
     vkminbl = max(vkminbl,ukzmin)
     end if
    ! enr4 

    ! vkminur = vkmindry + (8.-vkmindry)*fland(izo,ime,5)
    ! vkminbl = max(vkminbl,vkminur)

    do ilay=1,nlayers
       zn = d_alti(izo,ime,ilay)/pblh
       ep = min(0.1,zn)
       zsl= zn*pblh/obuklen
       if(obuklen.gt.0.) then
          wc = ustar/(1. + 4.7d0*zsl)
       else
          wc = (ustar**3 + ep*2.8d0*wstar**3)**0.3333
       endif
       if(zn.le.1.) then
          ! Within PBL
          !          d_kzzz(izo,ime,ilay) = vkarm*wc*pblh*zn*(1. - zn)**3
          d_kzzz(izo,ime,ilay) = vkarm*wc*pblh*zn*(1. - zn)**kzexp    !enr5
          d_kzzz(izo,ime,ilay) = max(d_kzzz(izo,ime,ilay),vkminbl)
          d_kzzz(izo,ime,ilay) = min(d_kzzz(izo,ime,ilay),vkmax)
          if(ilay.lt.nlayers) then
             if(d_alti(izo,ime,ilay+1).ge.pblh) then
                fk = (pblh       - d_alti(izo,ime,ilay)) / &
                     (d_alti(izo,ime,ilay+1) - d_alti(izo,ime,ilay))
                d_kzzz(izo,ime,ilay) = fk*d_kzzz(izo,ime,ilay) + (1.-fk)*vkminup
             endif
          end if
       else
          ! Above PBL
          do ilev=1,nlevels
             if(al(ilev-1).le.d_alti(izo,ime,ilay) &
                  .and.al(ilev).gt.d_alti(izo,ime,ilay)) then
                il = ilev
                go to 5653
             endif
          enddo
5653      continue
          dzz= al(il) - al(il-1)
          ss = 1e-6 + ((uw(il)-uw(il-1))**2 &
               +(vw(il)-vw(il-1))**2)/(dzz*dzz)
          rig = g*(po(il) - po(il-1))/(dzz*ss)/th(0)
          alph = 0.
          chi  = 0.
          ! Moist diffusion in clouds above BL
          if(rh(il-1).gt.crhx.or.rh(il).gt.crhx) then
             qbar = 0.5*(qr(il) + qr(il-1))
             tbar = 0.5*(te(il) + te(il-1))
             alph = Lv*qbar/(R*tbar)
             chi  = Lv*Lv*qbar/(Cp*Rv*tbar*tbar)
             rig=(1+alph)*(rig-(g*g*(chi-alph))/(1+chi)/ss/Cp/tbar)
          endif
          dk = sqrt(ss)/(1./(vkarm*d_alti(izo,ime,ilay))+1./rlam)**2
          if(rig.lt.0.) then
             srig = sqrt(-rig)
             upkz = dk*(1.-8.*rig/(1.+1.286*srig))
          else
             upkz = dk/(1.+5*rig)**2
          endif
          d_kzzz(izo,ime,ilay) = max(vkminup,upkz)
          d_kzzz(izo,ime,ilay) = min(d_kzzz(izo,ime,ilay),vkmax)
       endif
    enddo

  end subroutine vertical_turbulent_diffusivity
  !#############################################################################
  subroutine gridsize
    ! Calculation of chimere grid size [in meters]
    implicit none
    integer izo,ime
    ! warning: here use of earth radius in meters (earthrm)
    !         (in chimere: earthr is in cm)
    real :: earthrm  = 6371.0e3
    real :: dxx,dxy,dyx,dyy
    real,dimension(nzonal,nmerid)         :: clati   ! Cosine of latitudes
    real,dimension(nzonal,nmerid)         :: slati   ! Sine of latitudes

    ! subroutine arguments
    !  Calculation of the size of the cells (m) for transport
    !  and coordinates of grid mesh to be used 
    !  for the calculation of zenithal angles

    do ime=1,nmerid
       do izo=1,nzonal
          clati(izo,ime) = cos(pi*xlati(izo,ime)/180.)
          slati(izo,ime) = sin(pi*xlati(izo,ime)/180.)
       enddo
    enddo
    do ime=1,nmerid
       do izo=1,nzonal
          if(izo.gt.1.and.izo.lt.nzonal) then
             dxx = 0.5*(xlong(izo+1,ime)-xlong(izo-1,ime)) &
                  * pi*earthrm*clati(izo,ime)/180.
             dxy = 0.5*(xlati(izo+1,ime)-xlati(izo-1,ime)) &
                  * pi*earthrm/180.
          else if(izo.eq.1) then
             dxx = (xlong(2,ime)-xlong(1,ime)) &
                  * pi*earthrm*clati(1,ime)/180.
             dxy = (xlati(2,ime)-xlati(1,ime)) &
                  * pi*earthrm/180.
          else if(izo.eq.nzonal) then
             dxx = (xlong(nzonal,ime)-xlong(nzonal-1,ime)) &
                  * pi*earthrm*clati(nzonal,ime)/180.
             dxy = (xlati(nzonal,ime)-xlati(nzonal-1,ime)) &
                  * pi*earthrm/180.
          endif
          if(ime.gt.1.and.ime.lt.nmerid) then
             dyx = 0.5*(xlong(izo,ime+1)-xlong(izo,ime-1)) &
                  * pi*earthrm*clati(izo,ime)/180.
             dyy = 0.5*(xlati(izo,ime+1)-xlati(izo,ime-1)) &
                  * pi*earthrm/180.
          else if(ime.eq.1) then
             dyx = (xlong(izo,2)-xlong(izo,1)) &
                  * pi*earthrm*clati(izo,1)/180.
             dyy = (xlati(izo,2)-xlati(izo,1)) &
                  * pi*earthrm/180.
          else if(ime.eq.nmerid) then
             dyx = (xlong(izo,nmerid)-xlong(izo,nmerid-1)) &
                  * pi*earthrm*clati(izo,nmerid)/180.
             dyy = (xlati(izo,nmerid)-xlati(izo,nmerid-1)) &
                  * pi*earthrm/180.
          endif
          xsize(izo,ime) = sqrt(dxx*dxx+dxy*dxy)
          xbasx(izo,ime) = dxx/xsize(izo,ime)
          xbasy(izo,ime) = dxy/xsize(izo,ime)
          ysize(izo,ime) = sqrt(dyx*dyx+dyy*dyy)
          ybasx(izo,ime) = dyx/ysize(izo,ime)
          ybasy(izo,ime) = dyy/ysize(izo,ime)
       enddo
    enddo
    ! Boundaries
    xsize(0,:) = xsize(1,:) 
    xbasx(0,:) = xbasx(1,:)
    xbasy(0,:) = xbasy(1,:)
    xsize(nzonal+1,:) = xsize(nzonal,:) 
    xbasx(nzonal+1,:) = xbasx(nzonal,:)
    xbasy(nzonal+1,:) = xbasy(nzonal,:)
    xsize(:,0) = xsize(:,1) 
    xbasx(:,0) = xbasx(:,1)
    xbasy(:,0) = xbasy(:,1)
    xsize(:,nmerid+1) = xsize(:,nmerid) 
    xbasx(:,nmerid+1) = xbasx(:,nmerid)
    xbasy(:,nmerid+1) = xbasy(:,nmerid)
    ysize(0,:) = ysize(1,:) 
    ybasx(0,:) = ybasx(1,:)
    ybasy(0,:) = ybasy(1,:)
    ysize(nzonal+1,:) = ysize(nzonal,:) 
    ybasx(nzonal+1,:) = ybasx(nzonal,:)
    ybasy(nzonal+1,:) = ybasy(nzonal,:)
    ysize(:,0) = ysize(:,1) 
    ybasx(:,0) = ybasx(:,1)
    ybasy(:,0) = ybasy(:,1)
    ysize(:,nmerid+1) = ysize(:,nmerid) 
    ybasx(:,nmerid+1) = ybasx(:,nmerid)
    ybasy(:,nmerid+1) = ybasy(:,nmerid)
  end subroutine gridsize
  !#############################################################################

  !#############################################################################
  subroutine windiv
    ! Calculation of velocity divergence for deep convection
    ! units: m_winz and m_winm [m/s]
    !        m_sphu in [kg/kg]
    ! m_divu and m_divq: [s-1]
    implicit none
    integer :: nlevels,izo,ime,ive
    nlevels = diag_misc%nlevels

    do ive=1,nlevels
       do ime=1,nmerid
          do izo=1,nzonal
             if(izo.gt.1) then
                uwest = 0.5*(m_winz(izo,ime,ive)+m_winz(izo-1,ime,ive))
                qwest = 0.5*(m_sphu(izo,ime,ive)+m_sphu(izo-1,ime,ive))
             else
                uwest = m_winz(izo,ime,ive)
                qwest = m_sphu(izo,ime,ive)
             endif
             if(izo.lt.nzonal) then
                ueast = 0.5*(m_winz(izo,ime,ive)+m_winz(izo+1,ime,ive))
                qeast = 0.5*(m_sphu(izo,ime,ive)+m_sphu(izo+1,ime,ive))
             else
                ueast = m_winz(izo,ime,ive)
                qeast = m_sphu(izo,ime,ive)
             endif
             if(ime.gt.1) then
                usouth = 0.5*(m_winm(izo,ime,ive)+m_winm(izo,ime-1,ive))
                qsouth = 0.5*(m_sphu(izo,ime,ive)+m_sphu(izo,ime-1,ive))
             else
                usouth = m_winm(izo,ime,ive)
                qsouth = m_sphu(izo,ime,ive)
             endif
             if(ime.lt.nmerid) then
                unorth = 0.5*(m_winm(izo,ime,ive)+m_winm(izo,ime+1,ive))
                qnorth = 0.5*(m_sphu(izo,ime,ive)+m_sphu(izo,ime+1,ive))
             else
                unorth = m_winm(izo,ime,ive)
                qnorth = m_sphu(izo,ime,ive)
             endif
             m_divu(izo,ime,ive) = (ueast - uwest)/xsize(izo,ime) &
                  + (unorth - usouth)/ysize(izo,ime) 
             !  Calculation of the humidity divergence
             m_divq(izo,ime,ive) = (ueast*qeast-uwest*qwest)/xsize(izo,ime) &
                  + (unorth*qnorth-usouth*qsouth)/ysize(izo,ime)
          enddo
       enddo
    enddo
    ! need a parameter 'translation' between diagmet and chimere words
    ! [thayloc] vertical layer thickness in (m)
    ! [airmloc] air density must be in (molec/m3) rho=P/R.T
    ! [m_pres] read and already in Pa
    ! [m_temp] read and in K
    ! [m_alti] read and in meters
    do ive=1,nlevels
       do ime=1,nmerid
          do izo=1,nzonal
             if(ive.eq.1)then
                thlayloc(izo,ime,ive)=m_alti(izo,ime,ive)
             else
                thlayloc(izo,ime,ive)=m_alti(izo,ime,ive)    &
                     -m_alti(izo,ime,ive-1)
             endif
             !	 airmloc(izo,ime,ive)=7.2868e16*m_pres(izo,ime,ive)&
             !			/m_temp(izo,ime,ive) ! in molec/cm3
             airmloc(izo,ime,ive)=7.2868e22*m_pres(izo,ime,ive)&
                  /m_temp(izo,ime,ive) ! in molec/m3
          enddo
       enddo
    enddo
    ! first: HTRANSPORT
    do ive=1,nlevels
       do ime=1,nmerid
          do izo=1,nzonal

             !  Calculations of wind and layer thicknesses on cell sides
             !  These are just linearly interpolated

             if(izo.gt.1) then
                uwestg(izo,ime,ive) = 0.5*(m_winz(izo,ime,ive)*xbasx(izo,ime)     &
                     +      m_winm(izo,ime,ive)*xbasy(izo,ime)     &
                     +      m_winz(izo-1,ime,ive)*xbasx(izo-1,ime)     &
                     +      m_winm(izo-1,ime,ive)*xbasy(izo-1,ime))
                hwestg(izo,ime,ive) = 0.5*(thlayloc(izo,ime,ive)+thlayloc(izo-1,ime,ive))
                swestg(izo,ime,ive) = 0.5*(ysize(izo,ime) + ysize(izo-1,ime))
             else
                uwestg(izo,ime,ive) = 1.5*(m_winz(izo,ime,ive)*xbasx(izo,ime)     &
                     +       m_winm(izo,ime,ive)*xbasy(izo,ime))     &
                     - 0.5*(m_winz(izo+1,ime,ive)*xbasx(izo+1,ime)     &
                     +      m_winm(izo+1,ime,ive)*xbasy(izo+1,ime))
                hwestg(izo,ime,ive) = 1.5*thlayloc(izo,ime,ive)-0.5*thlayloc(izo+1,ime,ive)
                swestg(izo,ime,ive) = 1.5*ysize(izo,ime)-0.5*ysize(izo+1,ime)
             endif

             if(izo.lt.nzonal) then
                ueastg(izo,ime,ive) = 0.5*(m_winz(izo,ime,ive)*xbasx(izo,ime)     &
                     +        m_winm(izo,ime,ive)*xbasy(izo,ime)     &
                     +        m_winz(izo+1,ime,ive)*xbasx(izo+1,ime)     &
                     +        m_winm(izo+1,ime,ive)*xbasy(izo+1,ime))
                heastg(izo,ime,ive) = 0.5*(thlayloc(izo,ime,ive)+thlayloc(izo+1,ime,ive))
                seastg(izo,ime,ive) = 0.5*(ysize(izo,ime) + ysize(izo+1,ime))
             else
                ueastg(izo,ime,ive) = 1.5*(m_winz(izo,ime,ive)*xbasx(izo,ime)     &
                     +       m_winm(izo,ime,ive)*xbasy(izo,ime))     &
                     - 0.5*(m_winz(izo-1,ime,ive)*xbasx(izo-1,ime)     &
                     +      m_winm(izo-1,ime,ive)*xbasy(izo-1,ime))
                heastg(izo,ime,ive) = 1.5*thlayloc(izo,ime,ive)-0.5*thlayloc(izo-1,ime,ive)
                seastg(izo,ime,ive) = 1.5*ysize(izo,ime)-0.5*ysize(izo-1,ime)
             endif

             if(ime.gt.1) then
                usouthg(izo,ime,ive) = 0.5*(m_winz(izo,ime,ive)*ybasx(izo,ime)     &
                     +        m_winm(izo,ime,ive)*ybasy(izo,ime)     &
                     +        m_winz(izo,ime-1,ive)*ybasx(izo,ime-1)     &
                     +        m_winm(izo,ime-1,ive)*ybasy(izo,ime-1))
                hsouthg(izo,ime,ive) = 0.5*(thlayloc(izo,ime,ive)+thlayloc(izo,ime-1,ive))
                ssouthg(izo,ime,ive) = 0.5*(xsize(izo,ime) + xsize(izo,ime-1))
             else
                usouthg(izo,ime,ive) = 1.5*(m_winz(izo,ime,ive)*ybasx(izo,ime)     &
                     +       m_winm(izo,ime,ive)*ybasy(izo,ime))     &
                     -  0.5*(m_winz(izo,ime+1,ive)*ybasx(izo,ime+1)     &
                     +       m_winm(izo,ime+1,ive)*ybasy(izo,ime+1))
                hsouthg(izo,ime,ive) = 1.5*thlayloc(izo,ime,ive)-0.5*thlayloc(izo,ime+1,ive)
                ssouthg(izo,ime,ive) = 1.5*xsize(izo,ime)-0.5*xsize(izo,ime+1)
             endif

             if(ime.lt.nmerid) then
                unorthg(izo,ime,ive) = 0.5*(m_winz(izo,ime,ive)*ybasx(izo,ime)     &
                     +        m_winm(izo,ime,ive)*ybasy(izo,ime)     &
                     +        m_winz(izo,ime+1,ive)*ybasx(izo,ime+1)     &
                     +        m_winm(izo,ime+1,ive)*ybasy(izo,ime+1))
                hnorthg(izo,ime,ive) = 0.5*(thlayloc(izo,ime,ive)+thlayloc(izo,ime+1,ive))
                snorthg(izo,ime,ive) = 0.5*(xsize(izo,ime) + xsize(izo,ime+1))
             else
                unorthg(izo,ime,ive) = 1.5*(m_winz(izo,ime,ive)*ybasx(izo,ime)     &
                     +       m_winm(izo,ime,ive)*ybasy(izo,ime))     &
                     - 0.5*(m_winz(izo,ime-1,ive)*ybasx(izo,ime-1)     &
                     +       m_winm(izo,ime-1,ive)*ybasy(izo,ime-1))
                hnorthg(izo,ime,ive) = 1.5*thlayloc(izo,ime,ive)-0.5*thlayloc(izo,ime-1,ive)
                snorthg(izo,ime,ive) = 1.5*xsize(izo,ime)-0.5*xsize(izo,ime-1)
             endif
             !  Calculation of flux rates (independent of sign) on each cell side
             !  [s^-1]
             fluxw(izo,ime,ive) = uwestg(izo,ime,ive)/xsize(izo,ime)       &    ! Western cell side
                  * hwestg(izo,ime,ive)/thlayloc(izo,ime,ive)     &
                  * swestg(izo,ime,ive)/ysize(izo,ime)
             fluxe(izo,ime,ive) = ueastg(izo,ime,ive)/xsize(izo,ime)       &    ! Eastern cell side
                  * heastg(izo,ime,ive)/thlayloc(izo,ime,ive)     &
                  * seastg(izo,ime,ive)/ysize(izo,ime)
             fluxs(izo,ime,ive) = usouthg(izo,ime,ive)/ysize(izo,ime)      &    ! Southern cell side
                  * hsouthg(izo,ime,ive)/thlayloc(izo,ime,ive)     &
                  * ssouthg(izo,ime,ive)/xsize(izo,ime)
             fluxn(izo,ime,ive) = unorthg(izo,ime,ive)/ysize(izo,ime)       &   ! Northern cell side
                  * hnorthg(izo,ime,ive)/thlayloc(izo,ime,ive)     &
                  * snorthg(izo,ime,ive)/xsize(izo,ime)
          enddo
       enddo
    enddo

    ! from here: vtransport      
    ! vertical fluxes      
    vfluxo = 0. 
    vfluxi = 0. 
    !  Loop on model columns [units have to be m/s]

    ! patch: specific boundary conditions for air density
    airmloc(:,:,nlevels+1)=airmloc(:,:,nlevels)    ! top
    airmloc(0,:,:)=airmloc(1,:,:)                  ! zonal west
    airmloc(nzonal+1,:,:)=airmloc(nzonal,:,:)      ! zonal east
    airmloc(:,0,:)=airmloc(:,1,:)                  ! meridian south
    airmloc(:,nmerid+1,:)=airmloc(:,nmerid,:)      ! meridian north

    do ime=1,nmerid
       do izo=1,nzonal
          do ive=1,nlevels 
             hlow = thlayloc(izo,ime,ive) 
             if(ive.lt.nlevels)then
                hupp = thlayloc(izo,ime,ive+1) 
                dratio = airmloc(izo,ime,ive)/airmloc(izo,ime,ive+1)
             else
                dratio=1.
             endif
             dtende = 0.

             !  Calculation of mass flux imbalance due to horizontal advection       
             !  and vertical advection from below.                                   

             if(ive.gt.1) then 
                belowflux = vfluxi(izo,ime,ive,ive-1)*airmloc(izo,ime,ive-1) 
             else 
                belowflux = 0. 
             endif

             ! fluxbal in [s^-1]	    
             fluxbal = belowflux                                    &
                  + max( fluxw(izo,ime,ive),0.)*airmloc(izo-1,ime,ive)    &
                  + max(-fluxe(izo,ime,ive),0.)*airmloc(izo+1,ime,ive)    &
                  + max( fluxs(izo,ime,ive),0.)*airmloc(izo,ime-1,ive)    &
                  + max(-fluxn(izo,ime,ive),0.)*airmloc(izo,ime+1,ive)    &
                  - ( max(-fluxw(izo,ime,ive),0.)                        &
                  + max( fluxe(izo,ime,ive),0.)                          &
                  + max(-fluxs(izo,ime,ive),0.)                          &
                  + max( fluxn(izo,ime,ive),0.)                          &
                  + vfluxo(izo,ime,ive))                                    &
                  * airmloc(izo,ime,ive)                                    &
                  - dtende                                            

             !  Calculation of vertical wind and transport flux rates                

             if(fluxbal.gt.0.) then 
                m_winw(izo,ime,ive) = fluxbal*hlow/airmloc(izo,ime,ive) 
                vfluxo(izo,ime,ive) = vfluxo(izo,ime,ive) + m_winw(izo,ime,ive)/hlow 
                if(ive.lt.nlevels) then 
                   vfluxi(izo,ime,ive+1,ive) = &
                        vfluxi(izo,ime,ive+1,ive) + m_winw(izo,ime,ive)/hupp 
                endif
             else 
                m_winw(izo,ime,ive) = fluxbal*hlow/airmloc(izo,ime,ive+1) 
                vfluxi(izo,ime,ive,ive+1) = &
                     vfluxi(izo,ime,ive,ive+1) - m_winw(izo,ime,ive)/hlow 
                if(ive.lt.nlevels) then 
                   vfluxo(izo,ime,ive+1) = &
                        vfluxo(izo,ime,ive+1) - m_winw(izo,ime,ive)/hupp 
                endif
             endif
             ! vertical wind must be converted from m/s to Pa/s
             ! w [Pa/s] = w [m/s] . dp/dz [Pa/m]
             !          = dz/dt [m/s] . dp/dz [Pa/m]
             ! 	 with dp/dz = -rho . g
             ! 	 with g=9.81 m/s; rho = P/RT with P in mb and R=2.8702J/K.kg
             ! 	 dp/dz=(-g.p)/(100.R.T)
             ! with: p [Pa] = P [mb]/100.
             !       T: temp K
             m_winwpa(izo,ime,ive)=-m_winw(izo,ime,ive)*g*m_pres(izo,ime,ive) &
                  /100./2.87/m_temp(izo,ime,ive)
          enddo
       enddo
    enddo
  end subroutine windiv
  !#############################################################################



end module diagmet_science

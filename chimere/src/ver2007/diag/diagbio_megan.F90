module diagbio_megan

  !============================================================================
  ! MEGAN Biogenic Emissions module
  !
  ! Reference (herein after referred to as G06):
  ! Guenther, A., T. Karl, P. Harley, C. Wiedinmyer, P. I. Palmer,
  ! and C. Geron (2006), Estimates of global terrestrial isoprene
  ! emissions using MEGAN (Model of Emissions of Gases and Aerosols
  ! from Nature), Atmos. Chem. Phys., 6, 3181-3210,
  ! www.atmos-chem-phys.net/6/3181/2006/
  !
  ! MEGAN web site (free download of code & data) : http://bai.acd.ucar.edu
  !
  ! NOTE : The present code is adapted from v2.04 of MEGAN
  !
  ! Scientific algorithm (copy/pasted from original MEGAN code):
  !
  !        Emission = [EF][GAMMA][RHO]
  !
  !           where [EF]    = emission factor (ug/m2h)
  !                 [GAMMA] = emission activity factor (non-dimension)
  !                 [RHO]   = production and loss within plant canopies
  !                           (non-dimension)
  !                 Assumption: [RHO] = 1  (11/27/06)
  !
  !             GAMMA  = [GAMMA_CE][GAMMA_age][GAMMA_SM]
  !           where [GAMMA_CE]  = canopy correction factor
  !                 [GAMMA_age] = leaf age correction factor
  !                 [GAMMA_SM]  = soil moisture correction factor
  !                 Assumption: [GAMMA_SM]  = 1  (11/27/06)
  !             GAMMA_CE = [GAMMA_LAI][GAMMA_P][GAMMA_T]
  !           where [GAMMA_LAI] = leaf area index factor
  !                 [GAMMA_P]   = PPFD emission activity factor
  !                 [GAMMA_T]   = temperature response factor
  !
  !             Emission = [EF][GAMMA_LAI][GAMMA_P][GAMMA_T][GAMMA_age]
  !        Derivation:
  !             Emission = [EF][GAMMA_etc](1-LDF)+[EF][GAMMA_etc][LDF][GAMMA_P]
  !             Emission = [EF][GAMMA_etc]{ (1-LDF) + [LDF][GAMMA_P] }
  !             Emission = [EF][GAMMA_etc]{ (1-LDF) + [LDF][GAMMA_P] }
  !           where LDF = light dependent function (non-dimension)
  !
  !        Final Equation
  !             Emission = [EF][GAMMA_LAI][GAMMA_T][GAMMA_age]*
  !                        { (1-LDF) + [LDF][GAMMA_P] }
  !============================================================================

  use diagbio_common
  use chimere_consts

  implicit none

  ! Module parameters
  integer,parameter :: nbiomgn=9   ! Number of MEGAN species
                                   ! 1=isoprene, 2=a-pinene
                                   ! 3=b-pinene, 4=limonene
                                   ! 5=ocimene,  6=carene
                                   ! 7=sabinene, 8=myrcene
                                   ! 9=NO

  ! Module parameters
  real,dimension(nbiomgn) :: tdf,ldf,rho,mwt
  real,dimension(4,nbiomgn) :: adf

  ! Species Mass Weights (g/mol)
  data mwt /68.,136.,136.,136.,136.,136.,136.,136.,30./

  ! Temperature-dependent factor
  data tdf /0.09,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.11/

  ! Light-dependent factor
  data ldf /0.9999,0.1,0.1,0.05,0.8,0.05,0.1,0.05,0.0/

  ! Age-dependent factor
  data adf(:,1) /0.05,0.6,1.125,1.0/
  data adf(:,2) /2.0 ,1.8,0.95 ,1.0/
  data adf(:,3) /2.0 ,1.8,0.95 ,1.0/
  data adf(:,4) /2.0 ,1.8,0.95 ,1.0/
  data adf(:,5) /2.0 ,1.8,0.95 ,1.0/
  data adf(:,6) /2.0 ,1.8,0.95 ,1.0/
  data adf(:,7) /2.0 ,1.8,0.95 ,1.0/
  data adf(:,8) /2.0 ,1.8,0.95 ,1.0/
  data adf(:,9) /1.0 ,1.0,1.0  ,1.0/

  ! Production-Loss within canopy
  ! Assume rho=1.0 (G06)
  data rho /1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/

contains

  subroutine megan_bioemis(izo,ime,imon,m_day,czen,doy)

    ! Arguments
    integer,intent(in) :: izo,ime,imon,m_day,doy
    real,intent(in) :: czen

    ! Hidden arguments (through diagbio_common)
    real :: temp,swrd,dtem,drad
    real,dimension(nbiomgn) :: ef
    real :: laic,laip

    ! Mapping of MEGAN and Chimere bio species
    integer,dimension(nbiomgn),parameter :: mgn2chm=(/1,2,3,4,6,3,2,6,8/)

    ! Local variables
    integer :: im,ic,imonp
    real :: ppfd,dppfd
    real :: g_lai,g_p,g_sm
    real,dimension(nbiomgn) :: g_t,g_age

    integer::izomi,imemi
    ! Get local meteo variables
    temp=d_tem2(izo,ime)         ! Hourly temperature (K)
    swrd=d_swrd(izo,ime)         ! Hourly short-wave radiation (W/m2)
    dtem=m_dtem(izo,ime,m_day)   ! Daily temperature (K)
    drad=m_drad(izo,ime,m_day)   ! Daily short-wave radiation (W/m2)

    ! Get local EF
    do im=1,nbiomgn
      ef(im)=m_ef(im,izo,ime)
    enddo

    ! Get local LAI
    laic=m_lai(imon,izo,ime)   ! Current month
    imonp=imon-1
    if (imonp==0) imonp=12
    laip=m_lai(imonp,izo,ime)  ! Previous month

    ! Calculate PPFD (Photosynthetic Photon Flux Density, umol/m2/s)
    ! Here we assume (1) 4.766 umol/m2/s per W/m2
    !                (2) 1/2 of SWRD is in 400-700nm band
    ppfd =swrd*4.766*0.5   ! Hourly PPFD
    dppfd=drad*4.766*0.5   ! Daily PPFD

    ! Calculate GAMMA_LAI
    g_lai=gamma_lai(laic)

    ! Calculate GAMMA_P
    g_p=gamma_p(czen,ppfd,dppfd,doy)

    ! Calculate GAMMA_T
    ! Isoprene : GAMMA_TISOP
    im=1
    g_t(im)=gamma_tisop(temp,dtem)
    ! Non isoprene : GAMMA_TNISP
    do im=2,nbiomgn
      g_t(im)=gamma_tnisp(im,temp)
    enddo

    ! Calculate GAMMA_AGE
    do im=1,nbiomgn
      g_age(im)=gamma_age(im,laip,laic,dtem)
    enddo

    ! Calculate GAMMA_SM
    g_sm=gamma_sm()

    ! Sum of emmision rates
    c_biom_buf(:)=0.0
    do im=1,nbiomgn
      ic=mgn2chm(im)
      if (ic==0) cycle
      c_biom_buf(ic)=c_biom_buf(ic)+megan_er(im,ef(im),g_lai,g_p,g_t(im),g_age(im),g_sm)
    enddo
     
    ! Copy emission rates to output array
    do im=1,nbiomgn
      ic=mgn2chm(im)
      if (ic==0) cycle
      c_biom(ic,izo,ime)=c_biom_buf(ic)
    enddo
  
    do im=1,nbiomgn
      do izomi=1,izo
       do  imemi=1,ime
        if(c_biom(im,izomi,imemi)<0 ) write(76,*)c_biom(im,izomi,imemi),im,izomi,imemi
    enddo
    enddo
    enddo


  end subroutine megan_bioemis


  !==================================================================
  ! GAMMA_LAI : MEGAN LAI activity factor (eq. 15 G06)
  !
  !                          0.49 * LAI
  !         GAMMA_LAI = ---------------------
  !                      (1 + 0.2*LAI^2)^0.5
  !
  ! where : LAI = Leaf Area Index
  !==================================================================
  function gamma_lai(lai)

    ! Arguments
    real :: lai

    ! Output
    real :: gamma_lai

    ! GAMMA_LAI
    gamma_lai=0.49*lai/sqrt(1.+0.2*lai*lai)

  end function gamma_lai

  !==================================================================
  ! GAMMA_P : MEGAN PPFD (radiation) activity factor (eqs. 11-13 G06)
  !
  !   GAMMA_P = cos(z)*[2.46*(1+0.0005*(DPPFD-400.))*PHI - 0.9*PHI^2]
  !
  ! where : cos(z) = Cosine of solar zenith angle (see tools/subs.f90)
  !         DPPFD  = Daily average PPFD
  !         PHI    = Above canopy PPFD transmission
  !
  !         PHI = PPFD / (cos(z)*Ptoa)
  !         where : PPFD = Hourly PPFD
  !                 Ptoa = PPFD at top of the atmosphere
  !
  !         Ptoa = 3000 + 99*cos( 2pi*(DOY-10)/365 )
  !         where : DOY = Day Of the Year (Julian day)
  !
  ! NOTE : in G06, GAMMA_P eqs. contain the sine of solar zenith angle
  !        instead of cosine. This difference is due to the different
  !        definition of solar zenith angle in MEGAN and Chimere. The
  !        cos(z) calculated in Chimere is equivalent to sin(a) of eqs.
  !        11 and 12 of G06.
  !==================================================================
  function gamma_p(czen,ppfd,dppfd,doy)

    ! Arguments
    real,intent(in) :: czen,ppfd,dppfd
    integer,intent(in) :: doy

    ! Output
    real :: gamma_p

    ! Local variables
    real :: Ptoa,phi

    ! Top-of-atmosphere PPFD (umol/m2/s)
    Ptoa=3000.+99.*cos(2.*pi*(float(doy)-10.)/365.)

    ! Above canopy PPFD transmission (dimension-less)
    phi=ppfd/(czen*Ptoa)

    ! GAMMA_P
    gamma_p=czen*( phi*2.46*(1.+0.0005*(dppfd-400.)) - phi*phi*0.9 )
!    if(gamma_p <0 ) then
!      write(6,*)'gamma_p' ,gamma_p, czen,acos(czen),ppfd,ptoa,dppfd
!    gamma_p =0  
!    end if 
    
 end function gamma_p

  !==================================================================
  ! GAMMA_TISOP : MEGAN Temperature activity factor for Isoprene
  ! (eq. 14 G06)
  !
  !                        Eopt*CT2*exp(CT1*x)
  !           GAMMA_T = --------------------------
  !                      CT2 - CT1*(1-exp(CT2*x))
  !
  ! where : Eopt = 1.75*exp(0.08*(DTEM-297))
  !         CT2  = 200
  !         CT1  = 80
  !         x    = (1/Topt - 1/TEM) / 0.00831
  !         where : DTEM = daily average temperature
  !                 TEM  = hourly temperature
  !                 Topt = 313 + 0.6*(DTEM-297)
  !==================================================================
  function gamma_tisop(tem,dtem)

    ! Arguments
    real,intent(in) :: tem,dtem

    ! Output
    real :: gamma_tisop

    ! Local parameters
    real,parameter :: CT1=80.
    real,parameter :: CT2=200.

    ! Local variables
    real :: Eopt,x,Topt

    ! Optimal emission
    Eopt=1.75*exp(0.08*(dtem-297.))
    Topt=313.+0.6*(dtem-297.)
    x=(1./Topt-1./tem)/0.00831

    ! GAMMA_T
    gamma_tisop=Eopt*CT2*exp(CT1*x) / (CT2-CT1*(1.-exp(CT2*x)))

  end function gamma_tisop

  !==================================================================
  ! GAMMA_TNISP : MEGAN Temperature activity factor for non-isoprene
  ! (from MEGAN code)
  !
  !                      GAMMA_T = exp( BETA*(TEM-Ts) )
  !
  ! where : BETA = temperature dependent factor
  !         TEM  = hourly temperature
  !         Ts   = standard temperature (30 C)
  !==================================================================
  function gamma_tnisp(is,tem)

    ! Arguments
    integer,intent(in) :: is
    real,intent(in) :: tem

    ! Output
    real :: gamma_tnisp

    ! Local parameters
    real,parameter :: Ts=303.
    real,parameter :: T0k=273.

    ! Local variables
    real :: beta

    ! Temperature-dependent factor
    beta=tdf(is)

    ! GAMMA_T
    if(is.ne.9) then
      gamma_tnisp=exp(beta*(tem-Ts))
    else ! NO biogenic
      gamma_tnisp=exp(0.071*(1.03*(tem - t0k) + 2.9))
    endif
    

  end function gamma_tnisp

  !==================================================================
  ! GAMMA_AGE : MEGAN Age activity factor (eqs. 16-19 G06)
  !
  !    GAMMA_AGE = Fnew*Anew + Fgro*Agro + Fmat*Amat + Fold*Aold
  !
  ! where : F's : new, growing, mature and old foliage fractions
  !         A's : new, growing, mature and old emissions activities
  !
  ! Calculation of F's follows LAI tendency. Inputs are LAI of
  ! current (LAIc) and previous (LAIp) months.
  !
  !    Case 1) LAIp = LAIc
  !            Fnew = 0.0
  !            Fgro = 0.1
  !            Fmat = 0.8
  !            Fold = 0.1
  !
  !    Case 2) LAIp > LAIc
  !            Fnew = 0.0
  !            Fgro = 0.0
  !            Fmat = 1 - Fold
  !            Fold = (LAIp-LAIc)/LAIp
  !
  !    Case 3) LAIp < LAIc
  !            Fnew = 1 - LAIp/LAIc                       t <= ti
  !                 = (ti/t)*(1 - LAIp/LAIc)              t  > ti
  !            Fgro = 1 - Fnew - Fmat
  !            Fmat = LAIp/LAIc                           t <= tm
  !                 = LAIp/LAIc +
  !                   [(t-tm)/t]*(1 - LAIp/LAIc)          t  > tm
  !            Fold = 0.0
  !
  !            where : t  = length of LAI time step (1 month~30 days)
  !                    ti = number of days between budbreak and the
  !                         induction of emission
  !                    tm = number of days between budbreak and the
  !                         initiation of peak emission rates
  !
  !                    ti = 5 + (0.7*(300-Tt))            Tt <= 303
  !                       = 2.9                           Tt  > 303
  !                    tm = 2.3*ti
  !                    where : Tt = Average temperature at canopy top
  !                                 during current period. Daily T in
  !                                 this implementation.
  !==================================================================
  function gamma_age(is,LAIp,LAIc,Tt)

    ! Arguments
    integer, intent(in) :: is
    real,intent(in) :: LAIp,LAIc,Tt

    ! Output
    real :: gamma_age

    ! Local parameters
    real,parameter :: t=30.

    ! Local variables
    real :: Fnew,Fgro,Fmat,Fold
    real :: Anew,Agro,Amat,Aold
    real :: ti,tm

    ! Case 1
    if (LAIc==LAIp) then
      Fnew=0.0
      Fgro=0.1
      Fmat=0.8
      Fold=0.1

    ! Case 2
    elseif (LAIp>LAIc) then
      Fnew=0.0
      Fgro=0.0
      Fold=(LAIp-LAIc)/LAIp
      Fmat=1.-Fold

    ! Case 3
    elseif (LAIp<LAIc) then
      ! Calculate ti and tm
      if (Tt<=303.) then
        ti=5.+0.7*(300.-Tt)
      else
        ti=2.9
      endif
      tm=2.3*ti

      ! Fnew
      if (t<=ti) then
        Fnew=1.-LAIp/LAIc
      else
        Fnew=(ti/t)*(1.-LAIp/LAIc)
      endif

      ! Fmat
      if (t<=tm) then
        Fmat=LAIp/LAIc
      else
        Fmat=LAIp/LAIc + (t-tm)/t*(1.-LAIp/LAIc)
      endif

      ! Fgro, Fold
      Fgro=1.-Fnew-Fmat
      Fold=0.0

    endif  ! LAIp vs LAIc

    ! Get AGE activity factor
    Anew=adf(1,is)
    Agro=adf(2,is)
    Amat=adf(3,is)
    Aold=adf(4,is)

    ! GAMMA_AGE
    gamma_age=Fnew*Anew+Fgro*Agro+Fmat*Amat+Fold*Aold

  end function gamma_age

  !==================================================================
  ! GAMMA_SM : MEGAN Soil Moisture activity factor (from MEGAN code)
  !
  !              GAMMA_SM = 1.0    in v2.04 of MEGAN
  !
  !==================================================================
  function gamma_sm()

    ! Arguments

    ! Output
    real :: gamma_sm

    ! GAMMA_SM
    gamma_sm=1.0

  end function gamma_sm

  !==================================================================
  ! MEGAN Emissions Rate = [EF][GAMMA][RHO] (see header)
  !==================================================================
  function megan_er(is,ef,g_lai,g_p,g_t,g_age,g_sm)

    ! Arguments
    integer :: is
    real :: ef,g_lai,g_p,g_t,g_age,g_sm

    ! Output
    real :: megan_er

    ! Emission rate (ug/m2/h)
   if(g_t<0) write(6,*)'g_t' ,is,g_t  
   if(g_lai<0) write(6,*)'g_lai' ,is,g_lai  
    if(g_age<0) write(6,*)'g_age' ,is,g_age
     if(ef<0) write(6,*)'ef' ,is,ef
     if(rho(is)<0) write(6,*)'rho' ,is,rho(is)
     if(g_sm<0) write(6,*)'g_sm' ,is,g_sm
   if(ldf(is)>1) write(6,*)'ldf' ,is,ldf(is)
   
    


    megan_er=ef*rho(is)*g_lai*g_t*g_age*g_sm*((1-ldf(is))+g_p*ldf(is))

    ! Unit conversion (molec/cm2/s)
    megan_er=megan_er*an/mwt(is)/3.6e13

  end function megan_er

end module diagbio_megan

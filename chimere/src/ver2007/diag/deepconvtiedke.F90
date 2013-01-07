      subroutine convection(nlevels,pz,ppress,pt,pq,pw,pqac,    &
                            pqam,pqtur,umflx,dmflx,peu,pdu,ped,pdd)
     
!***********************************************************************
!****	cloud	- routine to calculate cloud properties
!
!	Purpose
!	-------
!	cloud calculates all properties associated with subgridscale
!	cloud mixing, i.e. massflux in updraft and downdraft, entrainment
!	and detrainment rates per level, and cloud properties: temperature,
!	moisture and liquid water and precipitation rate on each level
!	boundary.
!
!	input: (all variables are defined on level boundaries)
!		pz	geopotential height (m)
!		ppress	pressure 	(Pa)
!		pt	temperature 	(K)
!		pq	moisture 	(kg water/kg air)
!               pw      vertical velocity (negative upward)   (Pa s**-1)
!		pqac	div(q v) 	(kg water/kg air s**-1)
!		pqam	div(v) 		(s**-1)
!		pqtur	Fq surf-air	(kg water/m**2 s**-1)
!
!	output: variables defined on level boundaries:
!		peu	entrainment in updraft 		(kg m**-2 s**-1)
!		pdu	detrainment in updraft 		(same)
!		ped	entrainment in downdraft 	(kg m**-2 s**-1)
!		pdd	detrainment in downdraft 	(same)
!
!	method: updraft cloud properties are calculated according to Tiedke scheme
!	externals: needs functions qsat and dqsatdt
!	references: Tiedke, Mon. Wea. Rev., 117, 1779-1800, 1989.
!   including now cumulus downdraft and midlevel convection.
!   Calculates corresponding entrainment and detrainment rates
!
!-------------------------------------------------------------------------
      implicit none
! vertical resolution (no of model layers)
      integer nlevels,jplm,jplmm1,lme
! physical constants
      real pprgasd,pprgasv,ppeps
      real ppg,ppcpd,ppalv,ppzeta,ppvtcf
      parameter (pprgasd=287.05,pprgasv=461.51,ppeps=pprgasd/pprgasv)
      parameter(ppg=9.80665,ppcpd=1005.46,ppalv=2.5008e6)
      parameter(ppzeta=ppalv/ppcpd,ppvtcf=(1.-ppeps)/ppeps)
! constants for turbulent entrainment and detrainment rates
!  penetrative and midlevel convection
      real ppepsu,ppdeltu
      parameter (ppepsu=1.e-4,ppdeltu=1.e-4)
! shallow convection
      real ppepsus,ppdeltus
      parameter (ppepsus=3.e-4,ppdeltus=3.e-4)
! downdraft
      real ppepsd,ppdeltd
      parameter (ppepsd=2.e-4,ppdeltd=2.e-4)
! constants for precipitation parametrization
      real ppkmin,ppkval
      parameter (ppkmin=1500.,ppkval=2.e-3)
! parameter beta determines the overshoot of the detrainment plumes
! above level of no buoyancy (beta=0 : no overshoot)
      real ppbeta,ppbetas
! penetrative and midlevel convection
      parameter (ppbeta=0.0)
      parameter (ppbetas=0.15)
! parameter gamma determines downward massflux at the level of free
! sinking (LFS)
      real ppgamma
      parameter (ppgamma=0.3)
! parameter alpha determines specific humidity of air parcel at surface
! before starting the dry adiabatic ascent
! (alpha = 0 : air parcel has the specific humidity of the environment,
!  alpha = 1 : air parcel has saturation specific humidity of the
!              environment)
      real ppalpha
      parameter (ppalpha=0.2)
!            no of iterations for saturation calculation
      integer jpitermax
      parameter (jpitermax=5)
      integer klc,klt,klfs

      real pz(0:nlevels),pt(0:nlevels),pq(0:nlevels),pw(0:nlevels)
      real ppress(0:nlevels),pqac(0:nlevels),pqam(0:nlevels)
      real pqtur
      real pam(nlevels),peu(nlevels),pdu(nlevels),ped(nlevels)
      real pdd(nlevels)
      real pamu(0:nlevels),ptc(0:nlevels),pqc(0:nlevels),plc(0:nlevels)
      real pgp(0:nlevels),dmflx(0:nlevels),umflx(0:nlevels)
! logical variables 
! parameter llcudo=.true.  : cumulus downdraft turned on
! parameter llcudo=.false. : cumulus downdraft turned off
! parameter llmilc=.true.  : midlevel convection turned on
! parameter llmilc=.false. : midlevel convection turned off

      logical llcudo,llmilc
      data llcudo/.true./
      data llmilc/.true./
      
! external functions
      real qsat,dqsatdt
! local variables
      real zfck,zamub,zamdlfs,zamu,zamd,zlc,zqc,ztc
      real zlcklc,zqcklc,ztcklc,ztd,zqd,zdq1,zdq2,zsv,zscv,zgp
      real zpgp(nlevels),zepsu,zdeltu,zbeta
      integer jl,jjl,jiter,imlc
      real zdqcmin,zdqdmin,ztenwb,zqenwb,zttest,zqtest,zstv
      
      jplm=nlevels
      jplmm1=jplm-1
      lme=jplm
! default cloud properties on each layer boundary
      do jl=0,jplm
            pamu(jl)=0.
            dmflx(jl)=0.	    
            umflx(jl)=0.	    
            ptc(jl)=pt(jl)
 	    pqc(jl)=pq(jl)
	    plc(jl)=0.
	    pgp(jl)=0.
      enddo

! air-masses (kg/m**2) in each layer, default entrainment/detrainment
! and precipitation rates

      do jl=1,jplm
	     pam(jl)=(ppress(jl)-ppress(jl-1))/ppg
	     peu(jl)=0.
	     pdu(jl)=0.
	     ped(jl)=0.
	     pdd(jl)=0.
	     zpgp(jl)=0.
      enddo

! find condensation level by lifting an air parcel until supersaturation occurs

! we start the ascent with moisture and temperature of the upper boundary of
! the surface layer

      ztc=pt(jplmm1)
      zqc=pq(jplmm1) + ppalpha * (qsat(pt(jplmm1),ppress(jplmm1))-pq(jplmm1))

      do 10 jl=jplmm1-1,1,-1

! preliminary value of parcel temperature (dry adiabatic ascent)
        ztc=ztc-ppg*(pz(jl)-pz(jl+1))/ppcpd

! check for supersaturation
	    if(zqc.gt.qsat(ztc,ppress(jl))) then

! if supersaturation is detected we adjust moisture and temperature by
! condensation
! and set liquid water content equal to the condensate

	  zdq2=0.
	  do jiter=1,jpitermax
	    zdq1=(zqc-qsat(ztc,ppress(jl)))/(1.+ppzeta*dqsatdt(ztc,ppress(jl)))
	    zqc=zqc-zdq1
	    ztc=ztc+zdq1*ppzeta
	    zdq2=zdq2+zdq1
          enddo
        zlc=zdq2

! check if parcel is buoyant:
!   virtual temperature of parcel
          zscv=ztc*(1.+ppvtcf*zqc-zlc)

!   virtual temperature of environment
 	  zsv=pt(jl)*(1.+ppvtcf*pq(jl))

      if(zscv.gt.zsv) then
! if parcel is still buoyant then we have detected the cloud base
	    klc=jl
	    goto 20
 	  else
!  if parcel is not buoyant then there is no penetrative or shallow
!  convection
        if (llmilc) then
  	       goto 700
        else 
           goto 3000
	    endif
      endif
	
      endif

10    continue

! no condensation level found
      goto 3000

20    continue

! if we arrive here a cloud base is detected:
! klc is cloud base level, ztc is cloud temperature, zqc moisture (at
! saturation)
! and zlc the liquid water content in the updraft at base level

      ztcklc = ztc
      zqcklc = zqc
      zlcklc = zlc

! calculate large scale moisture convergence below cloud base
! (use trapezoidal integration formula)

      zfck=((pq(klc)*pqam(klc)-pqac(klc))*pam(klc+1)+(pq(klc)*pqam(jplm)-pqac(jplm))*pam(jplm))

         do jl=klc+1,jplm-1
            zfck=zfck+(pq(klc)*pqam(jl)-pqac(jl))*(pam(jl)+pam(jl+1))
         enddo

! check for shallow or penetrative convection, set proper parameter
! values

      if(zfck.gt.0.) then
!                                       penetrative and midlevel 
!                                       convection
        zepsu=ppepsu
        zdeltu=ppdeltu
        zbeta=ppbeta
      else
!                                       shallow convection
        zepsu=ppepsus
        zdeltu=ppdeltus
        zbeta=ppbetas
      endif

      zfck=pqtur+0.5*zfck


! if no moisture convergence then no penetrative or shallow 
! convection is present

      if(zfck.le.0.) then
        if (llmilc) then
           goto 700
        else
           goto 3000
        endif
      else
        goto 900
      endif

 700  continue

! check possibility for midlevel convection

      imlc = 0

! Check atmosphere from 2 layers above the surface to the middle of
! the atmosphere to see if midlevel convection might occur

      do 710 jl=jplmm1-1,jplm-int(lme/2.),-1

! large scale ascent and an environmental relative humidity of more than
! 90% are needed for midlevel convection to occur

         if ((pq(jl).gt.(0.9*qsat(pt(jl),ppress(jl)))).and.(pw(jl).lt.0.)) then

            if (imlc.eq.0) then
               ztc = pt(jl+1)
               zqc = pq(jl+1)
               zlc = 0.
               imlc = jl
            else if (imlc.gt.0) then
               if ((imlc-jl).eq.1) then
                  imlc = jl
                  goto 720
               else 
                  ztc = pt(jl+1)
                  zqc = pq(jl+1)
                  zlc = 0.
                  imlc = jl
               endif
            endif

 720        continue

! do adiabatic ascent

            ztc = ztc-ppg*(pz(jl)-pz(jl+1))/ppcpd

! check for supersaturation

            if (zqc.gt.qsat(ztc,ppress(jl))) then

! if supersaturation is detected we adjust moisture and temperature by
! condensation
! and set liquid water content equal to the condensate

	           zdq2=0.
	           do jiter=1,jpitermax
	              zdq1=(zqc-qsat(ztc,ppress(jl)))/(1.+ppzeta*dqsatdt(ztc,ppress(jl)))
	              zqc=zqc-zdq1
	              ztc=ztc+zdq1*ppzeta
	              zdq2=zdq2+zdq1
                  enddo
               zlc=zdq2

            endif

! check if parcel is buoyant

! virtual temperature of parcel

            zscv = ztc*(1.+ppvtcf*zqc-zlc)

! virtual temperature of environment

            zsv = pt(jl)*(1.+ppvtcf*pq(jl))

            if (zscv.gt.zsv) then

! parcel is buoyant and we have detected the cloud base of midlevel
! convection

               klc = jl
               zamub = -pw(klc)/ppg
               zepsu = ppepsu
               zdeltu = ppdeltu
               zbeta = ppbeta
               llcudo = .false.
               goto 1000
          
            endif

         endif
         
 710  continue
      
      goto 3000

 900  continue

! massflux at base of cloud

! limit specific humidity difference between cloud and environment at
! cloud base

      zdqcmin = max(0.01*pq(klc),1.e-10)
      zdqcmin = max(zdqcmin,zqc+zlc-pq(klc))

      zamub=zfck/zdqcmin

 1000 continue

! limit mass flux at cloud base
      zamub=min(zamub,1.0)

! set updraft entrainment rates below cloud base proportional
! to layer air masses
! set updraft detrainment rates below cloud base to zero

      do 28 jl=jplm,klc+1,-1
        peu(jl)=zamub*pam(jl)*ppg/(ppress(jplm)-ppress(klc))
	    pdu(jl)=0.
28    continue

! calculate now parcel ascent within cloud updraft
!    cloud mass flux               zamu,
!    cloud moisture                zqc,
!    cloud temperature             ztc,
!    cloud liquid water            zlc

      zamu=zamub

      do 30 jl=klc,2,-1
	    
! mass entrainment and detrainment
        peu(jl)=zepsu*zamu*(pz(jl-1)-pz(jl))
	    pdu(jl)=zdeltu*zamu*(pz(jl-1)-pz(jl))
	
! preliminary values of cloud temperature, moisture and cloud liquid water
        ztc=ztc-ppg*(pz(jl-1)-pz(jl))/ppcpd+zepsu*(pz(jl-1)-pz(jl))*(pt(jl)-ztc)
	zqc=zqc+zepsu*(pz(jl-1)-pz(jl))*(pq(jl)-zqc)
        zlc=zlc+zepsu*(pz(jl-1)-pz(jl))*(-zlc)

! adjust moisture and temperature by condensation

        zdq2=0.
	do jiter=1,jpitermax
	  zdq1=(zqc-qsat(ztc,ppress(jl)))/(1.+ppzeta*dqsatdt(ztc,ppress(jl)))
	  zqc=zqc-zdq1
	  ztc=ztc+zdq1*ppzeta
	  zdq2=zdq2+zdq1
        enddo

! precipitation rate out of layer jl

        if((pz(jl)+pz(jl-1))*0.5-pz(klc) .gt. ppkmin) then
          zgp=pam(jl)*ppkval/zamu
   	    else
	      zgp=0.
	    endif
	
! adjust liquid cloud water in updraft (use implicit scheme to prevent
! instability)


        zgp=zgp*zlc/(1+zgp)
	
	    zpgp(jl)=zgp*zamu
	
        zlc=zlc-zgp+zdq2

! check for level of free sinking (LFS) where cumulus downdraft starts
        
        if (.not.llcudo) then
     
! downdraft calculation already done or turned off
           
           goto 800

        endif

        if (zpgp(jl).eq.0.) then
           
! no downdraft exists since downdrafts are associated with convective
! precipitation from the updrafts
           
           goto 800

        endif

        if (jl.lt.3) then

! no downdraft since maximum possible cloud height is reached

           goto 800

        endif

! The LFS is assumed to be the highest model level where a mixture of equal
! parts of cloud air and environmental air (at wet bulb temperature) becomes
! negative buoyant with respect to the environmental air

! first step :
! calculate wet bulb temperature and moisture of the environmental air

        ztenwb = pt(jl-1)
        zqenwb = pq(jl-1)

! adjust temperature and moisture by evaporation
! zdq1 must be less equal 0 (zdq1=0 : environmental air is saturated,
! i.e. zqenwb = pq)

        do jiter = 1,jpitermax
           zdq1 = (zqenwb-qsat(ztenwb,ppress(jl-1)))/(1.+ppzeta*dqsatdt(ztenwb,ppress(jl-1)))
           zqenwb = zqenwb-zdq1
           ztenwb = ztenwb+zdq1*ppzeta
        enddo

! second step :
! do mixing with cloud air

        zttest = 0.5*(ztc+ztenwb)
        zqtest = 0.5*(zqc+zqenwb)

! third step :
! check for negative buoyancy

! virtual temperature of the air mixture

        zstv = zttest*(1.+ppvtcf*zqtest)

! virtual temperature of the environmental air

        zsv = pt(jl-1)*(1.+ppvtcf*pq(jl-1))

        if (zstv.lt.zsv) then

! level of free sinking (LFS) is found, start downdraft calculation

! massflux at LFS is assumed to be directly proportional to the (preliminary)
! upward massflux at cloud base

           klfs = jl
           zamdlfs = -ppgamma*zamub
           zamd = zamdlfs
           ztd = zttest
           zqd = zqtest

           ped(klfs) = (-zamd)
           pdd(klfs) = 0.

           if (klfs.eq.klc) goto 45

           do jjl = klfs+1,klc,1

! mass entrainment and detrainment

              ped(jjl) = ppepsd*zamd*(pz(jjl)-pz(jjl-1))
              pdd(jjl) = ppdeltd*zamd*(pz(jjl)-pz(jjl-1))

! preliminary values of cloud temperature and moisture in downdraft

              ztd = ztd-ppg*(pz(jjl)-pz(jjl-1))/ppcpd+ppepsd*(pz(jjl)-pz(jjl-1))*(ztd-pt(jjl-1))
              zqd = zqd+ppepsd*(pz(jjl)-pz(jjl-1))*(zqd-pq(jjl-1))

! adjust moisture and temperature by evaporation

              do jiter=1,jpitermax
                 zdq1 = (zqd-qsat(ztd,ppress(jjl-1)))/(1.+ppzeta*dqsatdt(ztd,ppress(jjl-1)))
                 zqd = zqd-zdq1
                 ztd = ztd+zdq1*ppzeta
              enddo

! downdraft massflux at lower layer boundary

              zamd = zamd - ped(jjl) + pdd(jjl)
	      
           enddo
 45        continue

           zamd = min(0.,zamd)

! set downdraft detrainment rates below cloud base proportional to layer
! air masses
! set downdraft entrainment rates below cloud base to zero

           do jjl = jplm,klc+1,-1
              ped(jjl) = 0.
              pdd(jjl) = zamd*pam(jjl)*ppg/(ppress(klc)-ppress(jplm))
           enddo

! recalculate updraft massflux at cloud base,
! now allowing for the downdraft massflux

           if (zamd.lt.0.) then
              zdqdmin = zqd-pq(klc)
              zamub = (zfck-zamd*zdqdmin)/zdqcmin
              if (zamub.le.0.) then
                 do jjl=1,jplm
                    peu(jjl)=0.
                    pdu(jjl)=0.
                    ped(jjl)=0.
                    pdd(jjl)=0.
                 enddo
                 goto 3000
               endif

! go back to cloud base and start updraft calculation again

              ztc = ztcklc
              zqc = zqcklc
              zlc = zlcklc
              llcudo = .false.
              goto 1000

           else
              goto 800
           endif   

        else

           goto 800

        endif

 800    continue

! check for buoyancy (in updraft)
!   virtual temperature in updraft at upper boundary of layer jl:
        zscv=ztc*(1.+ppvtcf*zqc-zlc)

!   virtual temperature outside of cloud
	zsv=pt(jl-1)*(1.+ppvtcf*pq(jl-1))

      if(zscv.le.zsv) then
	     klt=jl
	     goto 400
      endif

! updraft massflux at upper layer boundary
 	  zamu=zamu+peu(jl)-pdu(jl)

! store cloud properties on upper layer boundary
	   ptc(jl-1)=ztc
	   pqc(jl-1)=zqc
	   plc(jl-1)=zlc
	
30    continue

      klt=2

400   continue

! set detrainment in two layers above cloud

      pdu(klt-1)=zbeta*zamu
      peu(klt-1)=0.
      pdu(klt)=(1-zbeta)*zamu
      peu(klt)=0.
      
      do jl=klt-2,2,-1
         pamu(jl)=0.
	 peu(jl)=0.
	 pdu(jl)=0.
      enddo
      
! add up rainrate on each of the layer boundaries

      do 500 jl=klt+1,jplm
        pgp(jl)=pgp(jl-1)+zpgp(jl)
500   continue

! determine net mass flux on each of the layer boundaries
      
      dmflx(jplm)=0.
      umflx(jplm)=0.
      do 600 jl=jplm,1,-1
        pamu(jl-1)=pamu(jl)+(peu(jl)-pdu(jl))-(ped(jl)-pdd(jl))
	dmflx(jl-1)=dmflx(jl)+(ped(jl)-pdd(jl))
	umflx(jl-1)=umflx(jl)+(peu(jl)-pdu(jl))
600   continue

      llcudo = .true.
      llmilc = .true.
      return

! no cloud present, set cloud base and top to 0 and return

3000  continue
      klc=0
      klt=0
      llcudo = .true.
      llmilc = .true.

      end


      function qsat(t,p)
!---------------------------------------------------------------
! calculate saturation specific humidity
! t:	temperature (in K)
! p:	pressure (Pa)
!
!					mh, 28-jul-1989
!---------------------------------------------------------------

      parameter (rgasd=287.05,rgasv=461.51,eps=rgasd/rgasv)

      parameter (t0=273.16,c1=610.78,c3a=17.269,c3b=21.875,c4a=35.86,c4b= 7.66)

      if(p.le.0.) then
    	qsat=0.
	    return
      endif

! set function 'qsat' equal 0 for temperatures t less than 9K to ensure
! numerical stability (the argument of the following exponential 
! function would otherwise exceeds maximum)

      if (t.lt.9.) then
         qsat = 0.
         return
      endif

      if(t.ge.t0) then
	    es=c1*exp(c3a*(t-t0)/(t-c4a))
      else
	    es=c1*exp(c3b*(t-t0)/(t-c4b))
      endif

      qsat=eps/((p/es)-(1-eps))
      return
      end

      function dqsatdt(t,p)
!---------------------------------------------------------------
! calculate derivative of saturation specific humidity with
! respect to temperature
! t:	temperature (in K)
! p:	pressure (Pa)
!
!					mh, 28-jul-1989
!---------------------------------------------------------------

      parameter (rgasd=287.05,rgasv=461.51,eps=rgasd/rgasv)

      parameter (t0=273.16,c1=610.78,c3a=17.269,c3b=21.875,c4a=35.86,c4b= 7.66)

      if(p.lt.0.) then
    	dqsatdt=0.
	    return
      endif

! set function 'dqsatdt' equal 0 for temperatures t less than 9K to ensure
! numerical stability (the argument of the following exponential 
! function would otherwise exceeds maximum)

      if (t.lt.9.) then
         dqsatdt = 0.
         return
      endif

      if(t.ge.t0) then
	es=c1*exp(c3a*(t-t0)/(t-c4a))
	qsat=eps/((p/es)-(1-eps))
	dqsatdt=c3a*(t0-c4a)*qsat/((t-c4a)*(t-c4a)*(1.-(1.-eps)*es/p))
      else
	es=c1*exp(c3b*(t-t0)/(t-c4b))
	qsat=eps/((p/es)-(1-eps))
	dqsatdt=c3a*(t0-c4a)*qsat/((t-c4a)*(t-c4a)*(1.-(1.-eps)*es/p))
      endif

      return
      end




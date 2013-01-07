&metoptions
  opt%idstart = _START_ 
  opt%nsho    = _SHOW_
  !
  opt%w10m    = _UV10M_              ! 10m wind option (0=read from file, 1=recalculate)
  opt%usta    = _USTA_              ! Ustar option (0=read from file, 1=recalculate from Louis 1982)
  opt%flux    = _FLUX_              ! Flux  option (0=read, 1=recalculate from Priestley 1948)
  opt%pblh    = _HMIX_              ! Boundary layer height (0= read from file,1=recalculate)
  opt%cice    = _CLICE_             ! Cloud Ice Wat (0=Use in addition to cloud water, 1=don't)
  opt%rain    = _RAINWAT_            ! Rain Wat      (0=Use in addition to cloud water, 1=don't)
  !
  opt%upm     = _URBOPT1_      ! Urban corrections for BLH
  opt%ufx     = _URBOPT2_      ! Urban corrections for flux
  opt%uwc     = _URBOPT3_      ! Urban corrections for wind
  !
  opt%clol    = 2              ! Low cloud option (0=read cloudiness, 1=Use Liq. Wat., 2=Use RH)
  opt%crhl    = 0.85
  !
  opt%clom    = 1              ! Medium cloud option for attenuation ...
  opt%crhm    = 0.95
  !
  opt%cloh    = 1              ! High cloud option for attenuation ...
  opt%crhh    = 0.95
  !
  opt%soim    = 0          !  n
  opt%eros    = _EROS_         ! Erosion option
  opt%resu    = _RESU_         ! Resuspension option

  opt%upblcor   = _URBOPT4_      ! Urban corrections for wind
  opt%uframin   = _OPT1_      ! Urban corrections for wind
  opt%ukzmin   = _OPT2_      ! Urban corrections for wind
  opt%umomin   = _OPT3_      ! Urban corrections for wind
  opt%cldmix   = _HMIOPT1_      ! Urban corrections for wind
  opt%kzexp   = _HMIOPT2_      ! Urban corrections for wind
  opt%zimax     = _HMIOPT3_      ! Urban corrections for wind
/

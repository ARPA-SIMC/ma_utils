PROGRAM extrap_wind
!--------------------------------------------------------------------------
! Estrapola in verticale il vento a 10 metri di una stazione, usando alcuni
! degli schemi Calmet. Programma che andrebbe completato.
!
! Input: un file in formato .st1a oppure estra_orari (in questo caso deve 
!   contenere tutti e soli i campi: temperatura, modulo, direzione, cloud 
!   cover, pressione (per ora SLP)
!
! Output: un file in formato seriet
!
! Uso: extrap_wind.exe filein fileout [-h] [-c]
!
!                                         Versione 1.1.3, Enrico 15/01/2014
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

!--------------------------------------------------------------------------
! 0) Dichiarazioni

! Parametri costanti 
REAL, PARAMETER :: dtr = 3.141592654/180.
REAL, PARAMETER :: rmis_in =  9999.        ! dato mancante st1a, estra_orari
REAL, PARAMETER :: rmis_out = -9999.       ! dato mancante seriet
INTEGER, PARAMETER :: maxlev = 20          ! n.ro max livelli output
INTEGER, PARAMETER :: maxel = 1000000.     ! max valore per modulo di MO

! ALtre variabili del programma
REAL :: zzanem,z0,alb,bow,qan,hcg,xlat,xlon,zlev(maxlev)
REAL :: dirx(maxlev),modx(maxlev),ux(maxlev),vx(maxlev)
REAL :: mods,dirs,us,vs,cc,tt,pp,pexp2,fact,el,ustar,sinalp,sol_elev,rdum
INTEGER :: idum,ios,eof,eor,k
INTEGER :: yy,mm,dd,hh,njul,nlev,in_fmt
CHARACTER (LEN=200) :: filein,fileout,rule,chfmt1,chfmt2,chfmt3,ch80
CHARACTER (LEN=10) :: ch10
CHARACTER (LEN=5) :: ch5
LOGICAL :: sea

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comandi
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,rule)

IF (filein == "-c") THEN
  CALL scrive_namelist
  STOP

ELSE IF (filein == "-h" .OR. filein == "" .OR. fileout == "" .OR. & 
    (TRIM(rule) /= "1" .AND. TRIM(rule) /= "2") ) THEN
  WRITE (*,*) "Estrapola in verticale il vento di di una stazione,"
  WRITE (*,*) "   usando alcuni degli schemi Calmet"
  WRITE (*,*) "Legge le opzioni e i parametri fisiografici da extrap_wind.inp"
  WRITE (*,*) 
  WRITE (*,*) "Uso: extrap_wind.exe filein fileout rule [-c] [-h]"
  WRITE (*,*) "     filein: file con i dati osservati, in formato .st1a o"
  WRITE (*,*) "     estra_orari (la gestione del 2o e' incompleta)"
  WRITE (*,*) "     fileout: file su cui scrivere, in formato seriet"
  WRITE (*,*) "     rule: 1 = power law;   2 = similarity"
  STOP

ENDIF

! Parametri da namelist
OPEN (UNIT=40, FILE="extrap_wind.inp", STATUS="OLD", FORM="FORMATTED")
READ (40,*) z0
READ (40,*) alb
READ (40,*) bow 
READ (40,*) qan
READ (40,*) hcg
READ (40,*) zzanem
READ (40,*) xlat
READ (40,*) xlon
READ (40,*) sea
READ (40,*) nlev
DO k=1,nlev
  READ (40,*) zlev(k)
ENDDO

! Parametri derivati
IF (sea) THEN
  pexp2 = 0.286            ! sea
ELSE
  pexp2 = 0.143            ! land
ENDIF

IF (TRIM(rule) == "1") THEN
  ch10 = " power law"
ELSE IF (TRIM(rule) == "2") THEN
  ch10 = "similarity"
ENDIF

! Trovo codice per EOF
CALL get_eof_eor(eof, eor)

! Costruisco i formati
WRITE (chfmt1,'(a,i2,a)') "(a17,",2*nlev,"(1x,a10))"
WRITE (chfmt2,'(a,i2,a)') "(a17,",2*nlev,"(1x,i10))"
WRITE (chfmt3,'(a,2(i2,a))') "(i2,1x,i2,1x,i4,1x,i2,1x,i3,", &
  nlev,"(1x,f10.0),",nlev,"(1x,f10.1))"

! Trovo il formato del file di input e skippo header 
! (sarebbe da migliorare...)
OPEN (UNIT=30, FILE=filein, STATUS='OLD',ERR=9999)
READ (30,'(a)') ch5

IF (ch5 == " staz") THEN
  in_fmt = 1
  WRITE (*,*) "Formato di input: st1a"

ELSE IF (ch5 == "Rete ") THEN
  in_fmt = 2
  WRITE (*,*) "Formato di input: estra_orari"
  READ (30,*)
  READ (30,'(a)') ch80
  IF (TRIM(ch80) /= "aaaa mm gg hh      T ist     DD ist     FF ist" // &
    "        SLP      Cloud") THEN
    WRITE (*,*) "Lista parametri non gestita!"
    STOP
  ENDIF

ELSE
  in_fmt = 0
  STOP "Formato di input sconosciuto!"

ENDIF

! Apro i file(s) di ouptut e scrivo headers
OPEN (UNIT=31, FILE=fileout, FORM='FORMATTED', STATUS='REPLACE')
WRITE (31,*) 
WRITE (31,*) 
WRITE (31,chfmt1) "Modello ->       ",(ch10,k=1,2*nlev)
WRITE (31,chfmt2) "Livello ->       ",NINT(zlev(1:nlev)), &
  NINT(zlev(1:nlev))
WRITE (31,*) 
WRITE (31,chfmt1) "gg/mm/aaaa hh sca",("  Dir-wind",k=1,nlev), &
  ("  Mod-wind",k=1,nlev)

IF (TRIM(rule) == "2") THEN
  OPEN (UNIT=32, FILE="similarity.log", FORM='FORMATTED', STATUS='REPLACE')
  WRITE (32,*) 
  WRITE (32,*) 
  WRITE (32,'(a17,3(1x,a10))') "Modello ->       ",(ch10,k=1,3)
  WRITE (32,'(a17,3(1x,i10))') "Livello ->       ",(0,k=1,3)
  WRITE (32,*) 
  WRITE (32,'(a17,3(1x,a10))') "gg/mm/aaaa hh sca", &
    "Solar elev","  Monin_Ob","     Ustar"
ENDIF
!--------------------------------------------------------------------------
! 2) Esecuzione

DO

! Leggo una scadenza
  IF (in_fmt == 1) THEN
    READ (30,*,IOSTAT=ios) idum,idum,yy,mm,dd,hh,mods,dirs,rdum,rdum,rdum, &
      cc,rdum,tt,rdum,pp
  ELSE IF (in_fmt == 2) THEN
    READ (30,*,IOSTAT=ios) yy,mm,dd,hh,tt,dirs,mods,pp,cc
  ENDIF

  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998

!--------------------------------------------------------------------------
! 2.1) Estrapolazione:  legge di potenza
  IF (TRIM(rule) == "1") THEN

    IF (mods == rmis_in .OR. dirs == rmis_in) THEN
      ux(:) = rmis_out
      vx(:) = rmis_out

    ELSE
      us = -mods * SIN(dirs * dtr)
      vs = -mods * COS(dirs * dtr)

      DO k = 1,nlev
        fact = (zlev(k)/zzanem) ** pexp2
        ux(k) = us * fact
        vx(k) = vs * fact
      ENDDO

    ENDIF

!--------------------------------------------------------------------------
! 2.2) Estrapolazione: similarity
  ELSE IF (TRIM(rule) == "2") THEN

!   Calcolo angolo solare
    njul = jul(date(dd,mm,yy))
    CALL solar(njul,hh,xlat,xlon,sinalp)                     

    IF (mods == rmis_in .OR. dirs == rmis_in .OR. tt == rmis_in .OR. &
        pp == rmis_in .OR. cc == rmis_in) THEN
      ux(:) = rmis_out
      vx(:) = rmis_out
      el = rmis_out
      ustar = rmis_out

    ELSE IF (mods <= 0.001) THEN
      ux(:) = 0.
      vx(:) = 0.

    ELSE
      us = -mods * SIN(dirs * dtr)
      vs = -mods * COS(dirs * dtr)

!     Calcolo ustar e MO
      CALL elustr(us,vs,tt,pp,cc,zzanem,sinalp,rmis_out,z0,alb,bow,qan, &
        hcg,el,ustar)
      IF (el > maxel) el = maxel
      IF (el < -maxel) el = -maxel

!     Estraplo il vento  
      CALL similt(zzanem,xlat,el,z0,us,vs,zlev,nlev,ux,vx)

    ENDIF

  ENDIF
!--------------------------------------------------------------------------
! 2.3) Scrivo una scadenza

  CALL uv2dirint(ux,vx,dirx,modx,nlev,rmis_out)
  WRITE (31,chfmt3) dd,mm,yy,hh,0,dirx(1:nlev),modx(1:nlev)  

  IF (TRIM(rule) == "2") THEN
    IF (sinalp == rmis_out .OR. sinalp < -1. .OR. sinalp > 1.) THEN
      sol_elev = rmis_out
    ELSE
      sol_elev = ASIN(sinalp)*180./3.1415926
    ENDIF

    WRITE (32,"(i2.2,1x,i2.2,1x,i4.4,1x,i2.2,1x,i3.3,1x,f10.0,1x,f10.1,1x,f10.3)") &
      dd,mm,yy,hh,0,sol_elev,el,ustar
  ENDIF
 
ENDDO

CLOSE (30)
CLOSE (31)
CLOSE (32)

STOP

!-------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP

END PROGRAM extrap_wind

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE elustr (us,vs,tt,pp,cc,zzanem,sinalp,rmis,z0,alb,bow,qan,hcg, &
  el,ustar)
!--------------------------------------------------------------------------
! Calcola MO e Ustar per una stazione synop, usando lo schema di Calmet.
!--------------------------------------------------------------------------
IMPLICIT NONE

! Argomenti della subroutine
REAL, INTENT(IN) :: us,vs,tt,pp,cc          ! dati osservati
REAL, INTENT(IN) :: zzanem                  ! altezza anemometro
REAL, INTENT(IN) :: sinalp                  ! angolo solare
REAL, INTENT(IN) :: z0,alb,bow,qan,hcg      ! param. fisiografici
REAL, INTENT(IN) :: rmis                    ! dati mancanti

REAL, INTENT(OUT) :: el,ustar

! Parametri costanti
REAL, PARAMETER :: g=9.81, vk=0.4, xcrit=0.05, xmissm=990., cp=996.
REAL, PARAMETER :: ha1=990., ha2=-30., hb1=-0.75, hb2=3.4, hc3p1=1.12
REAL, PARAMETER :: hc1=5.31e-13, hc2=60.
REAL, PARAMETER :: wsmin=0.001              ! soglia per vento calmo

! Altre variabili locali
REAL :: rho,ws,xlnzz0,qsw,qqh,qstar,xl,xustar,xa,xb,psizl,psiz0l,xlold
REAL :: theta1,theta2,thetas,cdn,u02,cu2,toler
INTEGER :: niter,jcc,maxit,k

!--------------------------------------------------------------------------
! Controlli e calcoli preliminari
IF (us==rmis .OR. vs==rmis .OR. tt==rmis .OR. pp==rmis .OR. cc==rmis) THEN
  el = rmis
  ustar = rmis
  RETURN
ENDIF

ws = sqrt(us**2 + vs**2)

IF (ws < wsmin) THEN
  el = rmis
  ustar = 0.
  RETURN
ENDIF

rho = 0.3484321 * pp/tt          ! density
xlnzz0 = alog(zzanem/z0)

!--------------------------------------------------------------------------
! Heat flux: di notte metto a 0, di giorno uso Holtslag & van Ulden

qsw = (ha1 * sinalp + ha2) * (1. + hb1 * cc ** hb2)
qsw = amax1(qsw,0.0)

IF (sinalp <= 0.0) THEN
  qqh = -0.1
ELSE
  qstar = ((1. - alb) * qsw + hc1 * tt**6 - &
    5.67e-8 * tt**4 + hc2 * cc) / hc3p1
  qqh = bow * (qstar * (1. - hcg) + qan) / (1. + bow)
ENDIF

!--------------------------------------------------------------------------
! Condizioni instabili (giorno): uso Holstlag & van Ulden

IF (qqh > 0.0) THEN

! First guess -- assume neutral conditions
  xl = -9.9e9
  xustar = vk * ws / xlnzz0
  xustar = MAX(xustar,0.05)

! Iterate to refine estimate of u* and L
  xlold = xl
  maxit = 5
  DO niter = 1,maxit

!   New estimate of el (253.8226 = cp/(vk*g) with cp=996 m**2/(s**2 deg.))
    xl = -253.8226 * rho * tt * xustar ** 3 / qqh

!   New estimate of ustar
    xa = (1.0 - 15.0 * zzanem / xl) ** 0.25
    psizl = 2.0 * alog(0.5 * (1. + xa)) + alog(0.5 * (1. + xa * xa)) - &
      2.0 * ATAN(xa) + 1.5707963

    xb = (1.0 - 15.0 * z0 / xl) ** 0.25
    psiz0l = 2.0 * alog(0.5 * (1. + xb)) + alog(0.5 * (1. + xb * xb)) - &
      2.0 * ATAN(xb) + 1.5707963

    xustar = vk * ws / (xlnzz0 - psizl + psiz0l)
    xustar = MAX(xustar,0.05)

!   Check for convergence (5% criterion)
    IF (ABS((xl - xlold) / xl) < 0.005) EXIT
    xlold = xl

  ENDDO

! Valore finale
  ustar = xustar
  el = xl

!--------------------------------------------------------------------------
! Condizioni neutre

ELSE IF (qqh == 0.0) THEN
  xustar = vk * ws / xlnzz0
  ustar = MAX(xustar,0.05)
  el = -9.9e9

!--------------------------------------------------------------------------
! Condizioni stabili (notte): uso Venkatram(1980), Weil & Brower(1983)

ELSE

! Compute thetas (184.428 = 4*gamma*g with gamma=4.7, g=9.81 m/s**2)
  theta1 = 0.09 * (1. - .5 * cc**2)
  cdn = vk / xlnzz0
  theta2 = tt * cdn * ws ** 2 / (184.428 * zzanem)
  thetas = MIN(theta1,theta2)
  thetas = MAX(thetas,1.e-9)

! Compute ustar (46.107 = gamma*g, with gamma=4.7, g=9.81 m/s**2)
  u02 = 46.107 * zzanem * thetas / tt
  cu2 = MAX(0.0,ws * ws - 4. * u02 / cdn)
  ustar = 0.5 * cdn *(ws + SQRT(cu2))
  ustar = MAX(ustar,0.05)

! Product of (u*)(theta*) is not allowed to exceed XCRIT (=0.05)
  IF (ustar * thetas .gt. xcrit) THEN

!   Tolerance set at 5% above xcrit
    toler = 1.05 * xcrit
    maxit = 3

    DO k = 1,maxit
      thetas = xcrit / ustar
      u02 = 46.107 * zzanem * thetas / tt
      cu2 = MAX(0.0,ws * ws - 4. * u02 / cdn)
      ustar = 0.5 * cdn * (ws + SQRT(cu2))
      IF (ustar * thetas <= toler) EXIT
    ENDDO

  ENDIF

! Replace arbitrary nighttime heat flux indicator (-0.1 W/m**2)
! with actual value (996 m**2/(s**2 deg K) = cp)
  qqh = -996. * rho * ustar * thetas

! Compute Monin-Obukhov length (253.8226 = cp/(vk*g), cp=996 m**2/(s**2 deg.))
  el = -253.8226 * rho * tt * ustar ** 3 / qqh

ENDIF

END SUBROUTINE elustr

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE similt(zzanem,xlat,el,z0,us,vs,zlev,nlev,ux,vx)
!--------------------------------------------------------------------------
! Estrapola il vento usando lo schema di similarita' di Calmet
!--------------------------------------------------------------------------
IMPLICIT NONE

! Argomenti della subroutine
REAL, INTENT(IN) :: el            ! Monin Obukov
REAL, INTENT(IN) :: zzanem        ! altezza anemometro
REAL, INTENT(IN) :: xlat          ! latitudine richiesta
REAL, INTENT(IN) :: z0            ! Rugosita'
REAL, INTENT(IN) :: us,vs         ! vento alla superficie
INTEGER, INTENT(IN) :: nlev       ! n.ro livelli richiesti
REAL, INTENT(IN) :: zlev(nlev)    ! quota livelli richiesti

REAL, INTENT(OUT) :: ux(nlev),vx(nlev)

! Parametri costanti
REAL, PARAMETER :: d1=1.58, d2=1.0, factor=180./3.1415926
REAL, PARAMETER :: ztbl2=200.     ! from Table 2, Van Ulden & Holts. (1985)
REAL, PARAMETER :: eltbli(9) = &  ! Values of 1/L  
 (/-0.03333333, -0.01, -0.0027027, 0., 0.002857,0.007692, &
   0.0166667, 0.05, 0.1111111/)
REAL, PARAMETER :: dhtbl2(9) = (/12.,10.,9.,12.,18.,28.,35.,38.,39./)

! Altre variabili locali
REAL :: eladj,elinv,ws1,angle,wd1,dh200,dzan,xlnz1z2,xlnzz0,x,psim,ws,dwd
REAL :: wd,wdrad,psim1
INTEGER :: i,k

!--------------------------------------------------------------------------

! Restrict Monin-Obukhov length to be at least 5*z0
eladj = el
IF(ABS(eladj) < 5. * z0) THEN
  IF (eladj < 0.) THEN
    eladj = -5.0 * z0
  ELSE
    eladj = 5.0 * z0
  ENDIF
ENDIF

! Convert M-O length to 1/L (neutral = 0)
IF (ABS(eladj) >= 10000.) THEN
  elinv = 0.
ELSE
  elinv = 1. / eladj
ENDIF

! Recover surface wind speed and direction
ws1 = SQRT(us**2 + vs**2)
angle = 270. - ATAN2(vs,us) * factor
wd1 = amod(angle,360.)
IF (wd1 == 0.) wd1 = 360.

! Calculate ln(zzanem/z0) and stability function psi-m(zzanem/L) at level 1
! NOTA: in questa implementazione l'input e' il dato osservato, per cui
! non distinguo piu' tra altezza anemomentro e altezza 1o livello (il vento
! in input e' all'altezza anemometro, mentre in Calmet e' al 1o livello)
xlnz1z2 = LOG(zzanem / z0)

IF (ABS(elinv) < .00005) THEN                     ! Neutrale
  psim1 = 0.
ELSE IF (elinv > 0) THEN                          ! Stabile
  psim1 = -17. * (1. - exp(-0.29 * zzanem / eladj))
ELSE                                              ! Instabile
  x = (1. - 16. * zzanem / eladj) ** .25
  psim1 = 2. * LOG((1. + x) * .5) + LOG((1. + x * x) * .5) - &
    2. * ATAN(x) + 1.5707963
ENDIF

! Calculate turning angle D(h) using z = 200 m.
IF (ABS(elinv) < .00005) THEN         ! Neutrale
  dh200 = dhtbl2(4)

ELSE                                  ! Interpolate from Table 2 values
  DO i = 1,9
    IF (eltbli(i) >= elinv) THEN
      IF (i == 1) THEN
        dh200 = dhtbl2(1)
      ELSE
        dh200 = dhtbl2(i-1) + (dhtbl2(i) - dhtbl2(i-1)) * &
          (elinv - eltbli(i-1)) / (eltbli(i) - eltbli(i-1))
      ENDIF
      EXIT
    ENDIF
  ENDDO

  dh200 = dhtbl2(9)

ENDIF

! Calculate turning angle at anemometer height
dzan = dh200 * d1 * (1. - exp(-d2 * zzanem / ztbl2))

! Loop over all levels
DO k = 2, nlev+1

  xlnzz0 = LOG(zlev(k-1) / z0)
  IF (ABS(elinv) < .00005) THEN                   ! Neutrale
    psim = 0.
  ELSEIF (elinv > 0) then                         ! Stabile
    psim = -17. * (1. - EXP(-0.29 * zlev(k-1) / eladj))
  ELSE                                            ! Instabile
    x = (1. - 16. * zlev(k-1) / eladj) ** .25
    psim = 2. * LOG((1. + x) * .5) + LOG((1. + x * x) * .5) - &
      2. * ATAN(x) + 1.5707963
  ENDIF

! Apply wind speed adjustment, eqn 52 of Van Ulden & Holtslag (1985)
  ws = ws1 * (xlnzz0 - psim) / (xlnz1z2 - psim1)

! Turn winds, eqn 51 of Van Ulden & Holtslag (1985)
  dwd = dh200 * d1 * (1. - EXP(-d2 * zlev(k-1) / ztbl2))

  IF (xlat > 0.) THEN 
!   Winds veer in Northern Hemisphere
!   (Wind direction + shear between level and ground - shear between
!   anemometer and ground = WD + shear between level and anemometer)
    wd = wd1 + dwd - dzan
    IF (wd > 360.) wd = wd - 360.

  ELSE
!   Winds back in Southern Hemisphere
    wd = wd1 - dwd + dzan
    IF (wd <= 0.) wd = 360. + wd

  ENDIF

! Retrieve u and v components
  wdrad = 0.0174533 * wd
  ux(k-1) = -ws * SIN(wdrad)
  vx(k-1) = -ws * COS(wdrad)

ENDDO

RETURN
END SUBROUTINE similt

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE solar(njul,hh,xlat,xlon,sinalp)
!--------------------------------------------------------------------------
! Da calmet
! Calcola l'angolo solare in un punto per una data ora (GMT)
!--------------------------------------------------------------------------
IMPLICIT NONE

! Argomenti
INTEGER, INTENT(IN) :: njul                    ! giorno giuliano richiesto
INTEGER, INTENT(IN) :: hh                      ! ora GMT richiesta
REAL, INTENT(IN) :: xlat,xlon                  ! coord. punto richiesto

REAL, INTENT(OUT) :: sinalp                    ! angolo solare per ogni ora

! Altre variabili della subroutine
REAL :: radd,xsind,xcosd,rad2d,cos2d,sin2d,em,sigma,sincd,capd,coscd
REAL :: radlat,sinlat,coslat,solha,d,mxlon

!--------------------------------------------------------------------------
! Calcoli preliminari

! La longitudine di calmet ha il segno invertito!!
mxlon = -xlon

d = (float(njul) -1.) * 360./365.242
radd = 0.0174533 * d
xsind = SIN(radd)
xcosd = COS(radd)
rad2d = 2. * radd
sin2d = SIN(rad2d)
cos2d = COS(rad2d)
em = 12. + 0.12357 * xsind - 0.004289 * xcosd + 0.153809 * sin2d + &
     0.060783 * cos2d
sigma = 279.9348 + d + 1.914827 * xsind - 0.079525 * xcosd + 0.019938 * &
        sin2d - 0.00162 * cos2d
sincd = 0.39784989 * SIN(0.0174533 * sigma)
capd = ASIN(sincd)
coscd = COS(capd)

! Calcolo angolo solare
radlat = 0.0174533 * xlat
sinlat = SIN(radlat)
coslat = COS(radlat)

solha = 15. * (hh - em) - mxlon
sinalp = sinlat * sincd + coslat * coscd * &
  COS(0.0174533 * solha)
IF (sinalp < 0.) sinalp = 0.

RETURN
END SUBROUTINE solar

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE uv2dirint(u,v,dir,mod,nval,rmis)
!
! Dati i vettori di componenti u e v, ritona quelli di direzione e modulo
! Se una componente e' mancante dir e mod sono mancanti. 
! Se u=v=0, mette mod=dir=0
!

IMPLICIT NONE

INTEGER, INTENT(IN):: nval
REAL, INTENT(IN) :: u(nval),v(nval),rmis
REAL, INTENT(OUT) :: dir(nval),mod(nval)

REAL, PARAMETER :: dtr = 180./3.141592654
INTEGER k

!-------------------------------------------------------------------------
DO k = 1,nval

  IF (u(k) == rmis .OR. v(k) == rmis) THEN
    dir(k) = rmis
    mod(k) = rmis
    CYCLE
  ENDIF

  IF      (u(k) <= 0. .AND. v(k) < 0.) THEN    ! 0 - 90
    dir(k) = dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) < 0. .AND. v(k) >= 0.) THEN    ! 90 - 180
    dir(k) = 90. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) >= 0. .AND. v(k) > 0.) THEN    ! 180 - 270
    dir(k) = 180. + dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) > 0. .AND. v(k) <= 0.) THEN    ! 270 - 360
    dir(k) = 270. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) == 0. .AND. v(k) == 0.) THEN
    dir(k) = 0.
  ENDIF
 
  mod(k) = SQRT (u(k)*u(k) + v(k)*v(k))

ENDDO

RETURN
END SUBROUTINE uv2dirint

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_namelist
!
OPEN (UNIT=40, FILE="extrap_wind.inp", STATUS="REPLACE", FORM="FORMATTED")

WRITE (40,'(a)') "1.                   ! Rugosita"
WRITE (40,'(a)') "0.5                  ! Albedo"
WRITE (40,'(a)') "0.5                  ! Bowen Ratio"
WRITE (40,'(a)') "0.                   ! Flusso cal. antropogenico"
WRITE (40,'(a)') "0.5                  ! Soil Heat Flux Constant"
WRITE (40,'(a)') "10.                  ! altezza anemometro"
WRITE (40,'(a)') "45.                  ! latitudine"
WRITE (40,'(a)') "11.                  ! longitudine"
WRITE (40,'(a)') ".FALSE.              ! .T. se punto di mare (solo power law)"
WRITE (40,'(a)') "2                    ! n.ro di livelli da calcolare"
WRITE (40,'(a)') "100.                 ! altezza dalla sup. dei livelli"
WRITE (40,'(a)') "200.                 ! altezza dalla sup. dei livelli"

CLOSE (40)

END SUBROUTINE scrive_namelist

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_eof_eor(eof, eor)
!-------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!-------------------------------------------------------------------------
IMPLICIT NONE

INTEGER, INTENT(OUT) :: eof,eor

INTEGER :: k, ios, idummy=0, iun=0
LOGICAL :: l1 = .TRUE.


! Cerco un'unita' libera per aprire il file di prova
DO k = 10,99
  INQUIRE (UNIT=k, OPENED=l1, IOSTAT=ios)
  IF (.NOT. l1 .AND. ios==0) THEN
    iun = k
    EXIT
  ENDIF
ENDDO
IF (iun == 0) GOTO 9999   ! non ho torvato nessuna unita' libera
!WRITE (*,*) "uso unita ",iun

! Cerco codice di errore per EOF
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
ENDFILE (k)
REWIND (k)
READ (k,*,IOSTAT=eof)
CLOSE(k)

! Cerco codice di errore per EOR
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
WRITE (k,'(a1)') "1" 
WRITE (k,'(a1)') "2"
REWIND (k)
READ (k,'(i1)',ADVANCE="NO",ERR=9999) idummy
READ (k,'(i1)',ADVANCE="NO",IOSTAT=eor) idummy
CLOSE(k)

!write (*,*) "eof,eor ",eof,eor
RETURN

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine get_eof_eor, usero' valori di default"
eof = -1
eor = -2
RETURN

END SUBROUTINE get_eof_eor


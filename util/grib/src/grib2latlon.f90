PROGRAM grib2latlon
!--------------------------------------------------------------------------
! Legge il primo grib di un file, e scrive 2 grib con latitudini e 
! longitudini dei punti della griglia.
!
!                                           Versione 1.0, Enrico 04/11/2008
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 1000000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
REAL :: lat(maxdim),lon(maxdim)
REAL :: x1,y1,dx,dy,xrot,yrot,xx,yy
INTEGER :: iuin,iuout,nx,ny,i,j,k
CHARACTER (LEN=80) :: filein,fileout
CHARACTER (LEN=1) :: proj

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)

IF (filein == "" .OR. fileout == "" .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: grib2latlon.exe [-h] filein fileout" 
  WRITE (*,*) "Legge il primo campo di filein"
  WRITE (*,*) "scrive su fileout i campi lat e lon dei punti griglia (formato grib)"
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
CALL PBOPEN (iuout,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2) Leggo primo campo

CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
IF (kret <= -1) THEN
  WRITE(*,*) "Error pbgrib: kret ",kret
  STOP
ENDIF

CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             field,maxdim,kbuffer,maxdim,klen,'D',kret)
IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

!--------------------------------------------------------------------------
! 3) Calcolo coordinate punti griglia

IF (ksec2(11) /= 64) THEN
  WRITE (*,*) "Scanning mode ",ksec2(11)," non gestito"
  STOP
ENDIF

! Trovo parametri griglia
nx = ksec2(2)
ny = ksec2(3)
y1 = REAL(ksec2(4)) / 1000.
x1 = REAL(ksec2(5)) / 1000.
IF (ksec2(6) == 0) THEN               ! grid spacing not given
  dx = ABS(REAL(ksec2(8))/1000. - REAL(ksec2(5))/1000.) / REAL(nx-1)
  dy = ABS(REAL(ksec2(7))/1000. - REAL(ksec2(4))/1000.) / REAL(ny-1)
ELSE
  dx = REAL(ksec2(9)) / 1000. 
  dy = REAL(ksec2(10)) / 1000.
ENDIF
yrot = REAL(ksec2(13)) / 1000. + 90.
xrot = REAL(ksec2(14)) / 1000.

! Trovo tipo prioezione
IF (ksec2(1) == 10) THEN
  proj = "R"
ELSE IF (ksec2(1) == 0) THEN
  IF (x1 < 360. .AND. y1 < 90.) THEN
    proj = "G"
  ELSE
    proj = "U"
  ENDIF
ELSE
  WRITE (*,*) "Prioezione geografica non gestita, ksec1(2) ",ksec1(2)
  STOP
ENDIF

! Calcolo lat & lon
DO k = 1,nx*ny
  j = (k-1) / nx + 1
  i = k - (j-1)*nx
  xx = x1 + (i-1) * dx
  yy = y1 + (j-1) * dy
  
  IF (proj == "G") THEN
    lat(k) = yy
    lon(k) = xx
  ELSE IF (proj == "R") THEN
    CALL rtll(xx,yy,xrot,yrot,lon(k),lat(k)) 
  ELSE IF (proj == "U") THEN
    CALL utm2ll(xx,yy,32,.FALSE.,lat(k),lon(k))
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 4) Scrivo lat e lon su fileout

ksec1(1) = 203
ksec1(6) = 114
ksec4(2) = 24
CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             lat,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

ksec1(1) = 203
ksec1(6) = 115
ksec4(2) = 24
CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             lon,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

STOP

END PROGRAM grib2latlon

!----------------------------------------------------------------------
      subroutine utm2ll(x,y,iz,lsohem,rlat,rlon)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET
!
! --- CALMET   Version: 5.0       Level: 970825                  UTM2LL
!
! --- PURPOSE:  Converts UTM coordinates to latitude/longitude
!               Works in both Northern & Southern Hemispheres
!               Reference--
!                 "Map Projections--A Working Manual", p61,
!                  U.S. Geological Survey Professional Paper 1395,
!                    Note: assumes the Clarke 1866 ellipsoid
!               Adapted from --
!                  EPS version 2.0; subr. MAPUTG
!
! --- INPUTS:
!                  X - real    - UTM easting in km
!                  Y - real    - UTM northing in km
!                 IZ - integer - UTM zone (6 deg N-S strip, range=1,60)
!             LSOHEM - logical - TRUE = southern hemisphere
!                                FALSE = northern hemisphere
!
! --- OUTPUT:
!               RLAT - real    - N Latitude in decimal degrees
!               RLON - real    - E Longitude in decimal degrees
!
! --- UTM2LL called by:  READCF
! --- UTM2LL calls:      none
!----------------------------------------------------------------------

      real k0,M,N1,l
      logical lsohem

      parameter (k0=0.9996)
      parameter (a=6378206.4)
      parameter (e1=0.001697916)
      parameter (e11=3.0*e1/2.0 - 27.0*e1*e1*e1/32.0)
      parameter (e12=21.0*e1*e1/16.0 - 55.0*e1*e1*e1*e1/32.0)
      parameter (e13=151.0*e1*e1*e1/96.0)
      parameter (e14=1097.0*e1*e1*e1*e1/512.0)
      parameter (e2=0.00676866)
      parameter (e4=e2*e2)
      parameter (e6=e2*e4)
      parameter (ep2=0.0068148)
      parameter (false_e=500000.0)
      parameter (rtd=180.0/3.141592654)

! --- Parameter definitions
!      k0        -  scale on central meridian
!      a         -  Clarke 1866 equatorial radius
!      e2        -  squared Clarke 1866 eccentricity
!      ep2       -  (e2/(1.0-e2)
!      false_e   -  false easting
!      rtd       -  radians to degrees conversion

! --- Central meridian
      rlon0 = iz*6.0 - 183.0

! --- Correct for false easting, southern hemisphere and change to meters
      xm = 1000.0*x - false_e
      if(LSOHEM) then
        ym = 1000.0 * (y-10000.)
      else
        ym = 1000.0 * y
      endif

      M = ym/k0
      u = M/(a*(1.0-e2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0))
      p1 = u + e11*sin(2.0*u) + e12*sin(4.0*u) + e13*sin(6.0*u) + &
               e14*sin(8.0*u)
      cosp1 = cos(p1)
      C1 = ep2*cosp1**2
      C2 = C1**2
      tanp1 = tan(p1)
      T1 = tanp1**2
      T2 = T1**2
      sinp1 = sin(p1)
      sin2p1 = sinp1**2
      N1 = a/sqrt(1.0-e2*sin2p1)
      R0 = 1.0-e2*sin2p1
      R1 = a*(1.0-e2)/sqrt(R0**3)

      D = xm/(N1*k0)
      D2=D**2
      D3=D*D2
      D4=D*D3
      D5=D*D4
      D6=D*D5

      p = p1 - (N1*tanp1/R1) * (D2/2.0                                  &
             - (5.0+3.0*T1+10.0*C1-4.0*C2-9.0*ep2)*D4/24.0              &
             + (61.0+90.0*T1+298.0*C1+45.0*T2-252*ep2-3.0*C2)*D6/720.0)
      rlat = rtd*p
      l = (D - (1.0+2.0*T1+C1)*D3/6.0                                   &
             + (5.0-2.0*C1+28*T1-3.0*C2+8.0*ep2+24.0*T2)*D5/120.0)/cosp1
      rlon = rtd*l + rlon0

      return
      end subroutine utm2ll

!-------------------------------------------------------------------------
      SUBROUTINE RTLL(TLMD,TPHD,TLM0D,TPH0D,ALMD,APHD)        
!-------------------------------------------------------------------------
! trasforma le coordinate ruotate (TLMD,TPHD) in coordinate geografiche
! ordinarie (ALMD,APHD). I/O in gradi e decimi
! TLM0D, TPH0D: lon e lat del centro di rotazione, in gradi e decimi.
! 
      PARAMETER (DTR=3.141592654/180.)

      CTPH0=COS(TPH0D*DTR)
      STPH0=SIN(TPH0D*DTR)

      STPH=SIN(TPHD*DTR)
      CTPH=COS(TPHD*DTR)
      CTLM=COS(TLMD*DTR)
      STLM=SIN(TLMD*DTR)

      APH=ASIN(STPH0*CTPH*CTLM+CTPH0*STPH)                            
      CPH=COS(APH)                                                    

      ALMD=TLM0D+ASIN(STLM*CTPH/CPH)/DTR                               
      APHD=APH/DTR                                                    

      RETURN                                                          
      END   




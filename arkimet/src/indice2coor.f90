PROGRAM indice2coord
!--------------------------------------------------------------------------
! Dato l'indice (vettoriale) di una serie di punti di un'area grib, calcola 
! gli indici ij, le coordinate geo e le coordinate UTM 
! 
!                                                 V2.1.1, Enrico 28/02/2013
!--------------------------------------------------------------------------

IMPLICIT NONE

!
CHARACTER (LEN=40), PARAMETER :: aree_path_def = "/usr/share/ma_utils"
CHARACTER (LEN=40), PARAMETER :: aree_path = "MA_UTILS_DATA"
!
REAL :: x1,y1,x2,y2,xrot,yrot,dx,dy
REAL :: xgrid,ygrid,xgeo,ygeo,xutm,yutm
INTEGER :: nx,ny,utmz,scan(3)
INTEGER :: k,i,j
INTEGER :: ios,idum
CHARACTER (LEN=120) :: nfile
CHARACTER (LEN=80) :: ch80
CHARACTER (LEN=78) :: ch78
CHARACTER (LEN=61) :: ch61
CHARACTER (LEN=12) :: aree_name
CHARACTER (LEN=10) :: grid_area,dum_area
CHARACTER (LEN=3) :: proj
CHARACTER (LEN=1) :: ch1

!--------------------------------------------------------------------------
! 1: Input area

! 1.1 Nome e tipo area (interattivo)

9000 CONTINUE
WRITE (*,*) "Tipo di area: (G)eografica o (U)tm"
READ (*,*) ch1
SELECT CASE (ch1)
CASE("u","U")
  proj="utm"
  aree_name = "aree_utm.dat"
CASE("g","G")
  proj="geo"
  aree_name = "aree_geo.dat"
CASE DEFAULT
  GOTO 9000
END SELECT

WRITE (*,*) "Nome area (deve essere inclusa in aree_geo.dat)"
READ (*,*) grid_area

! 1.2 Leggo estremi area

ch80 = ""
CALL GETENV(aree_path,ch80)
IF (TRIM(ch80) == "") ch80 = aree_path_def
nfile = TRIM(ch80) // "/" // TRIM(aree_name)

IF (proj == "utm") THEN
  OPEN (UNIT=22, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9999)
  DO
    READ (22,'(a)',IOSTAT=ios) ch61    
    IF (ios /= 0) GOTO 9999
    IF (TRIM(ch61) == "" .OR. ch61(1:1) == "!") CYCLE
  
    READ (ch61,'(a10,2(1x,i4),4(1x,f8.3),1x,i4)',IOSTAT=ios) &
      dum_area,nx,ny,x1,y1,x2,y2,utmz
    IF (ios /= 0) GOTO 9998
  
    IF (TRIM(dum_area) == TRIM(grid_area)) EXIT
  ENDDO
  CLOSE(22)

ELSE IF (proj == "geo") THEN
  OPEN (UNIT=22, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9999)
  DO
    READ (22,'(a)',IOSTAT=ios) ch78    
    IF (ios /= 0) GOTO 9999
    IF (TRIM(ch78) == "" .OR. ch78(1:1) == "!") CYCLE
  
    READ (ch78,'(a10,2(1x,i4),6(1x,f8.3),1x,3i1)',IOSTAT=ios) &
      dum_area,nx,ny,x1,y1,x2,y2,xrot,yrot,scan(1:3)
    IF (ios /= 0) GOTO 9998
  
    IF (TRIM(dum_area) == TRIM(grid_area)) EXIT
  ENDDO
  CLOSE(22)

ENDIF

IF (proj == "geo" .AND. (xrot /= 0. .OR. yrot /= 0.)) proj = "rot"
dx = (x2-x1) / REAL(nx-1)
dy = (y2-y1) / REAL(ny-1)

!--------------------------------------------------------------------------
! 2: Calcolo indici & coordinate

DO
  WRITE (*,*) "Indice del punto (0 per terminare)"
  READ (*,*) k
  IF (k == 0) EXIT

! indici i,j
  j = ((k-1) / nx) + 1
  i = k - (nx*(j-1))
  xgrid = x1 + dx * REAL(i-1)
  ygrid = y1 + dy * REAL(j-1)

! coordinate georafiche
  IF (proj == "geo") THEN
    xgeo = xgrid
    ygeo = ygrid
  ELSE IF (proj == "rot") THEN
    CALL rtll(xgrid,ygrid,xrot,yrot,xgeo,ygeo)
  ELSE IF (proj == "utm") THEN
    CALL utm2ll(xgrid,ygrid,utmz,.FALSE.,ygeo,xgeo)
  ENDIF

! coordinate UTM
  IF (proj == "utm") THEN
    xutm = xgrid
    yutm = ygrid
  ELSE
    CALL ll2utm(ygeo,xgeo,32,xutm,yutm,idum)
  ENDIF

! Scrivo ouptut
  WRITE (*,'(a,2i10)')   "indici (i,j):     ",i,j
  WRITE (*,'(a,2(1x,f12.5))') "coord. UTM (x,y):",xutm,yutm
  WRITE (*,'(a,2(1x,f12.5))') "coord. GEO (x,y):",xgeo,ygeo
  WRITE (*,*) 

ENDDO
STOP

!--------------------------------------------------------------------------
! 3: Gestione errori

9999 CONTINUE
WRITE (*,*) "Area ",TRIM(grid_area)," non trovata in ",TRIM(aree_name)
STOP

9998 CONTINUE
WRITE (*,*) "Record illegale in ",TRIM(aree_name)
WRITE (*,'(a)') ch61
STOP

END PROGRAM indice2coord

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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
      end

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!----------------------------------------------------------------------
      subroutine ll2utm(rlat,rlon,iz0,x,y,iz)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET
!    
! --- CALMET   Version: 5.0       Level: 970825                  LL2UTM
!
! --- PURPOSE:  Converts latitude/longitude to UTM coordinates
!
!           *** Universal Transverse Mercator (UTM) grid system divides
!           *** the world into 60 north-south zones, covering a 6 deg.
!           *** strip of longitude. Zone 1 begins between 180 and 174
!           *** degrees West longitude and progresses eastward to
!           *** zone 60.
!           *** This routine works in both No. & So. Hemispheres
!               Reference --
!                 "Map Projections--A Working Manual", p61,
!                  U.S. Geological Survey Professional Paper 1395,
!                    Note: assumes the Clarke 1866 ellipsoid
!               Adapted from --
!                  EPS version 2.0; subr. MAPGTU
!
! --- INPUTS:
!               RLAT - Real        - N Latitude in decimal degrees
!                                    (use negative for southern hemisphere)
!               RLON - Real        - E Longitude in decimal degrees
!                                    (use negative for western hemisphere)
!                IZ0 - Integer     - UTM zone override (used only if
!                                    IZ0 .ne. zero).
!
! --- OUTPUT:
!                  X - Real        - UTM easting in km
!                  Y - Real        - UTM northing in km
!                 IZ - Integer     - UTM zone
!
! --- LL2UTM called by:  READCF
! --- LL2UTM calls:      none
!----------------------------------------------------------------------

      real k0
      real N,M

      parameter (k0=0.9996)
      parameter (a=6378206.4)
      parameter (e2=0.00676866)
      parameter (ep2=0.0068148)
      parameter (false_e=500000.0)
      parameter (dtr=3.141592654/180.0)

      if (iz0 .eq. 0) then
! ---   Locate natural zone
          iz = int((180.0+rlon)/6.0) + 1
      else
! ---   Zone override
          iz = iz0
      endif

! --- Compute delta longitude in radians
      dl = dtr*(rlon - (6.0*iz-183.0))

! --- Convert phi (latitude) to radians
      p = dtr*rlat

      sinp = sin(p)
      N = a/sqrt(1.0-e2*sinp*sinp)
      tanp = tan(p)
      T = tanp*tanp
      cosp = cos(p)
      C = ep2*cosp*cosp
      A1 = dl*cosp
      M = 111132.0894*rlat - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p) &
        - 0.02*sin(6.0*p)

      A2 = A1**2
      A3 = A2*A1
      A4 = A2**2
      A5 = A4*A1
      A6 = A4*A2
      T2 = T**2

! --- Compute UTM x and y (km)
      x = 0.001*(k0*N*(A1+(1.0-T+C)*A3/6.0          &     
        + (5.0-18.0*T+T2+72.0*C-58.0*ep2)*A5/120.0) &
        + false_e)
      y = (M+N*tanp * (A2/2.0 + (5.0-T+9.0*C+4.0*C*C)*A4/24.0 &
        + (61.0-58.0*T+T2+600.0*C-330.0*ep2)*A6/720.0))
      false_n = 0.
      if (rlat .lt. 0.) then
! --- in km, unlike false_e
        false_n = 10000.
      endif
      y = 0.001*k0*y + false_n

      return
      end subroutine ll2utm

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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


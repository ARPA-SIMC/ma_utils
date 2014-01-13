PROGRAM crea_coord_chimere
!--------------------------------------------------------------------------
! Scrive il file con l'elenco delle coordinate dei punti di un dominio per
! Chimere e programmi di appoggio
!
! Uso: crea_coord_chimere.exe [-c] [-h]
!
! Note: 
! Il file COORD_domain prodotto, contiene le coordinate dei centri cella
! Gli estremi dell'area contenuti in crea_coord_chimere.inp devono essere
! anche loro relativi ai centri cella.
!
!                                         Versione 2.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------
IMPLICIT NONE

REAL :: x1,x2,y1,y2,dx,dy,xrot,yrot,x,y,lat,lon
REAL :: lat_mx,lat_mn,lon_mx,lon_mn
INTEGER :: proj,ni,nj,utmz,i,j
CHARACTER (LEN=200) :: chpar,fileout
CHARACTER (LEN=6) :: area

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Eventuale parametro da riga comando
CALL getarg(1,chpar)
IF (TRIM(chpar) == "-h") THEN
  CALL scrive_help
  STOP
ELSE IF (TRIM(chpar) == "-c") THEN
  CALL scrive_esempio
  STOP
ENDIF

! 1.2 Leggo crea_coord_chimere.inp
OPEN (UNIT=21, FILE="crea_coord_chimere.inp", STATUS="OLD", &
  ACTION="READ", ERR=9999)

READ (21,*,ERR=9998,END=9998) proj
READ (21,*,ERR=9998,END=9998) ni
READ (21,*,ERR=9998,END=9998) nj
READ (21,*,ERR=9998,END=9998) x1
READ (21,*,ERR=9998,END=9998) y1
READ (21,*,ERR=9998,END=9998) x2
READ (21,*,ERR=9998,END=9998) y2
READ (21,*,ERR=9998,END=9998) utmz
READ (21,*,ERR=9998,END=9998) xrot
READ (21,*,ERR=9998,END=9998) yrot
READ (21,'(a6)',ERR=9998,END=9998) area

CLOSE(21)

! 1.3 Controlli e calcoli dipendenti dai parametri
IF ((proj /= 1 .AND. proj /= 2) .OR. &
     ni < 0 .OR. nj < 0 .OR. x2 < x1 .OR. y2 < y1) THEN
  WRITE (*,*) "Parametri illegali in crea_coord_chimere.inp"
  STOP
ENDIF

WRITE (fileout,'(2a)') "COORD_",area
dx = (x2-x1) / REAL(ni-1)
dy = (y2-y1) / REAL(nj-1)
WRITE (*,*) "Passo griglia: ",dx,dy

!--------------------------------------------------------------------------
!2) Calcolo le coordinate dei centri cella e le scrivo

lat_mx = -HUGE(0.)
lon_mx = -HUGE(0.)
lat_mn = HUGE(0.)
lon_mn = HUGE(0.)

OPEN (UNIT=22, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
DO j = 1,nj
DO i = 1,ni

  x = x1 + REAL(i-1) * dx
  y = y1 + REAL(j-1) * dy

  IF (proj == 1) THEN
    CALL utm2ll(x,y,utmz,.FALSE.,lat,lon)

  ELSE IF (proj == 2) THEN
    IF (xrot == 0. .AND. yrot == 0.) THEN
      lon = x
      lat = y
    ELSE
      CALL rtll(x,y,xrot,yrot,lon,lat)
    ENDIF

  ENDIF

  IF (lat < lat_mn) lat_mn = lat
  IF (lat > lat_mx) lat_mx = lat
  IF (lon < lon_mn) lon_mn = lon
  IF (lon > lon_mx) lon_mx = lon

  WRITE (22,'(f8.4,1x,f8.4)') lon,lat
ENDDO
ENDDO

CLOSE (22)

WRITE (*,*) "Estremi geografici dell'area:"
WRITE (*,*) "SUD:   ",lat_mn," NORD: ",lat_mx
WRITE (*,*) "OVEST: ",lon_mn," EST:  ",lon_mx

STOP

!--------------------------------------------------------------------------
! Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "Errore aprendo crea_coord_chimere.inp"
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo crea_coord_chimere.inp"
STOP

END PROGRAM crea_coord_chimere

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Visualizza a schermo l'help del programma
!
IMPLICIT NONE

WRITE (*,*) "Crea il file COORD per la catena chimere"
WRITE (*,*) "Ricordarsi di modificare il file DOMAINS"
WRITE (*,*) 
WRITE (*,*) "USO: crea_coord_chimere.exe [-h] [-c]"
WRITE (*,*) " -h  : visualizza questo help"
WRITE (*,*) " -c  : crea file crea_coord_chimere.inp di esempio"
WRITE (*,*) 

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_esempio
!
! Scrive un file crea_geo_calmet.inp di esempio
!
IMPLICIT NONE

OPEN (UNIT=20, FILE="crea_coord_chimere.inp", STATUS="REPLACE", &
  FORM="FORMATTED")

!                 123456789012345678901234567890123456789012345678901234567890
WRITE (20,'(a)') "1         ! Proiezione (1: UTM; 2: GEO)                        !01"
WRITE (20,'(a)') "90        ! NI                                                 !02"
WRITE (20,'(a)') "58        ! NJ                                                 !03"
WRITE (20,'(a)') "406.79    ! X1 (punto SW; km o gradi)                          !04"
WRITE (20,'(a)') "4787.86   ! Y1 (punto SW; km o gradi)                          !05"
WRITE (20,'(a)') "851.79    ! X2 (punto NE; km o gradi)                          !06"
WRITE (20,'(a)') "5072.86   ! Y2 (punto NE; km o gradi)                          !07"
WRITE (20,'(a)') "32        ! UTMZ (solo se griglia UTM)                         !08"
WRITE (20,'(a)') "0.        ! XROT (cetro di rotazione, solo se griglia GEO)     !09"
WRITE (20,'(a)') "0.        ! YROT (cetro di rotazione, solo se griglia GEO)     !10" 
WRITE (20,'(a)') "PADAN2    ! nome area (max 6 car., maiuscolo, per nome output) !11"

CLOSE (20)

RETURN

END SUBROUTINE scrive_esempio

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

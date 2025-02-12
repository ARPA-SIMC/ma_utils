program geo_utm
!--------------------------------------------------------------------------
! Legge una o piu' coppie di coordinate e le converte GEO <-> UTM
!
!                                         Versione 2.0.1, Enrico 11/09/2012
!--------------------------------------------------------------------------
IMPLICIT NONE

REAL :: ygeo,xgeo,x,y
INTEGER :: conv,iz,utmz
INTEGER :: i,ios,eof,eor,kpar,cnt_par,ier
CHARACTER (LEN=10) :: chutm,gpslon,gpslat
CHARACTER (LEN=100) :: filein,fileout,charg
LOGICAL :: lgps

!--------------------------------------------------------------------------
! 1) Parametri da riga comando

lgps = .FALSE.
conv = 0
utmz = 0
filein = ""
fileout = ""

cnt_par = 0
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,charg)

  IF (TRIM(charg) == "") THEN
    EXIT
  ELSE IF (TRIM(charg) == "-h") THEN
    WRITE (*,*) "Uso: geo_utm.exe [filein fileout] [-geo2utm/-utm2geo] [-gps]"
    WRITE (*,*) "Converte le coord. di uno o piu' punti da UTM a geo e viceversa"
    WRITE (*,*) "Senza parametri:   uso interattivo"
    WRITE (*,*) "filein, fileout:   contengono una coppia di coord. (x,y) per ogni record"
    WRITE (*,*) "-geo2utm/-utm2geo: specifico il tipo di conversione, e assumo che la zona"
    WRITE (*,*) "                   UTM sia 32 (consente uso batch)"
    WRITE (*,*) "-gps               coordinate geografiche espresse in gg.pp.ss"
    WRITE (*,*) "                   con input da file, formato di lettura (a10,2x,a10)"
    STOP
  ELSE IF (TRIM(charg) == "-gps") THEN  
    lgps = .TRUE.
  ELSE IF (TRIM(charg) == "-geo2utm") THEN  
    conv = 1
    utmz = 32
  ELSE IF (TRIM(charg) == "-utm2geo") THEN  
    conv = 2
    utmz = 32
  ELSE
    cnt_par = cnt_par + 1
    SELECT CASE (cnt_par)
    CASE (1)
      filein = charg
    CASE (2)
      fileout = charg
    END SELECT
  ENDIF
ENDDO

! Tipo di conversione e zona UTM non specificati
IF (conv == 0) THEN
1 WRITE (*,*) "Seleziona la conversione: geo->utm(1), utm->geo(2)"
  READ (*,*) conv
  IF (conv /= 1 .AND. conv /= 2) GOTO 1
  WRITE(*,*) "Zona UTM (def. 32)"
  utmz=0
  READ (*,'(a)') chutm
  READ (chutm,*,IOSTAT=ios) utmz
  IF (utmz==0 .OR. ios/=0) utmz = 32
ENDIF

!--------------------------------------------------------------------------
! 2) Elaboro le coordinate richieste

! Input interattivo  
IF (filein == "") THEN

  IF (conv==1) THEN
    IF (lgps) THEN
      WRITE (*,*) "Longitudine (X)? (gg.pp.ss)"
      READ (*,'(a)') gpslon
      WRITE (*,*) "Latitudione (Y)? (gg.pp.ss)"
      READ (*,'(a)') gpslat
      CALL gps2gd(gpslon,gpslat,xgeo,ygeo,ier)
     ELSE
      WRITE (*,*) "coordinate geografiche? (lon,lat; gradi.decimali)"
      READ (*,*) xgeo,ygeo
    ENDIF

    CALL ll2utm(ygeo,xgeo,utmz,x,y,iz)
    WRITE (*,'(a,i2,a)') "coordinate UTM",utmz," (x,y; km):"
    WRITE (*,*) x,y

  ELSE IF (conv==2) THEN
    WRITE(*,'(a,i2,a)') "coordinate UTM",utmz," (x,y; km):"
    READ(*,*) x,y
    CALL utm2ll(x,y,utmz,.FALSE.,ygeo,xgeo)
    IF (lgps) THEN
      CALL gd2gps(xgeo,ygeo,gpslon,gpslat,ier)
      WRITE (*,*) "coordinate geografiche (lon,lat):"
      WRITE (*,'(a10,2x,a10)') gpslon,gpslat
    ELSE
      WRITE (*,*) "coordinate geografiche (lon,lat):"
      WRITE (*,*) xgeo,ygeo
    ENDIF

  ENDIF

! Input da file
ELSE
  CALL get_eof_eor(eof,eor)
  OPEN (12, FILE=filein, STATUS="OLD", FORM="FORMATTED")
  OPEN (13, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")

  DO
    IF (conv==1) THEN
      IF (lgps) THEN
        READ (12,'(a10,2x,a10)',IOSTAT=ios) gpslon,gpslat
        IF (ios == eof) EXIT
        IF (ios /= 0) GOTO 9999
        CALL gps2gd(gpslon,gpslat,xgeo,ygeo,ier)
      ELSE
        READ (12,*,IOSTAT=ios) xgeo,ygeo
        IF (ios == eof) EXIT
        IF (ios /= 0) GOTO 9999
      ENDIF
      CALL ll2utm(ygeo,xgeo,utmz,x,y,iz)
      WRITE (13,'(f8.3,1x,f8.3)') x,y

    ELSE IF (conv==2) THEN
      READ (12,*,IOSTAT=ios) x,y
      IF (ios == eof) EXIT
      IF (ios /= 0) GOTO 9999
      CALL utm2ll(x,y,utmz,.FALSE.,ygeo,xgeo)
      IF (lgps) THEN
        CALL gd2gps(xgeo,ygeo,gpslon,gpslat,ier)
        WRITE (13,'(a,2x,a)') gpslon,gpslat
      ELSE  
        WRITE (13,'(2(f8.3,x))') xgeo,ygeo
      ENDIF

    ENDIF
    WRITE (*,*) "lon,lat,x,y",xgeo,ygeo,x,y
  ENDDO

ENDIF

STOP

!----------------------------------------------------------------------
9999 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP

END PROGRAM geo_utm

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
      end subroutine utm2ll

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE gps2gd(strx,stry,xx,yy,ier)
!
! Trasfoma le coordinate gradi.primi.secondi in gradi.decimi
! Probabilmente funziona solo per coordinate positive.
! ier=1: coordinate illegali
! ier=2: coordiante negative
! ier=3: parsing error nelle stringhe in input
!
IMPLICIT NONE
!
CHARACTER (LEN=10), INTENT(IN) :: strx,stry  !coord. in gradi.primi.secondi
REAL, INTENT(OUT) :: xx,yy                   !coord. in gradi.decimi
INTEGER, INTENT(OUT) :: ier

INTEGER :: gg,pp,ss,p1,p2
!

p1 = INDEX(strx,".")
p2 = INDEX(strx,".",BACK=.TRUE.)

IF (p1 == 0 .OR. p2 == 0) GOTO 9997
READ (strx(1:p1-1),*,ERR=9999) gg
READ (strx(p1+1:p2-1),*,ERR=9999) pp
READ (strx(p2+1:),*,ERR=9999) ss
IF (gg < -180 .OR. gg > 180 .OR. pp < 0 .OR. pp > 59 .OR. ss < 0 .OR. ss > 59) THEN
  GOTO 9999
ELSE IF (gg < 0) THEN
  GOTO 9998
ENDIF
xx = REAL(gg) + (REAL(pp) + REAL(ss)/60.) / 60.

p1 = INDEX(stry,".")
p2 = INDEX(stry,".",BACK=.TRUE.)
IF (p1 == 0 .OR. p2 == 0) GOTO 9997
READ (stry(1:p1-1),*,ERR=9999) gg
READ (stry(p1+1:p2-1),*,ERR=9999) pp
READ (stry(p2+1:),*,ERR=9999) ss
IF (gg < -90 .OR. gg > 90 .OR. pp < 0 .OR. pp > 59 .OR. ss < 0 .OR. ss > 59) THEN
  GOTO 9999
ELSE IF (gg < 0) THEN
  GOTO 9998
ENDIF
yy = REAL(gg) + (REAL(pp) + REAL(ss)/60.) / 60.

ier = 0
RETURN

9999 CONTINUE
WRITE (*,*) "Subroutine gps2gp: coordinate illegali"
ier = 1
RETURN

9998 CONTINUE
WRITE (*,*) "Subroutine gps2gp: coordinate negative non gestite"
ier = 2
RETURN

9997 CONTINUE
WRITE (*,*) "Subroutine gps2gp: parsing error"
ier = 3
RETURN

END SUBROUTINE gps2gd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE gd2gps(xx,yy,strx,stry,ier)
!
! Trasfoma le coordinate gradi.decimi in gradi.primi.secondi
! Probabilmente funziona solo per coordinate positive.
! ier=1: coordinate illegali
! ier=2: coordinate negative
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: xx,yy                    !coord. in gradi.decimi
CHARACTER (LEN=10), INTENT(OUT) :: strx,stry !coord. in gradi.primi.secondi
INTEGER, INTENT(OUT) :: ier

REAL :: ppr,ssr
INTEGER :: gg,pp,ss
!
IF (xx < -180. .OR. xx > 180. .OR. yy < 90. .OR. yy > 90.) THEN
  GOTO 9999
ELSE IF (xx < 0. .OR. yy < 0.) THEN
  GOTO 9998
ELSE
  ier = 0
ENDIF

gg = INT(xx)
ppr = (xx - REAL(gg)) * 60.
pp = INT(ppr)
ssr = (ppr - REAL(pp)) * 60.
ss = NINT(ssr)
IF (gg < 100) THEN
  WRITE(strx,'(i2.2,2(a1,i2.2))') gg,".",pp,".",ss
ELSE
  WRITE(strx,'(i3.3,2(a1,i2.2))') gg,".",pp,".",ss
ENDIF

gg = INT(yy)
ppr = (yy - REAL(gg)) * 60.
pp = INT(ppr)
ssr = (ppr - REAL(pp)) * 60.
ss = NINT(ssr)
WRITE(stry,'(i2.2,2(a1,i2.2))') gg,".",pp,".",ss

RETURN

9999 CONTINUE
WRITE (*,*) "Subroutine gd2gps: coordinate illegali"
ier = 1
RETURN

9998 CONTINUE
WRITE (*,*) "Subroutine gd2gps: coordinate negative non gestite"
ier = 2
RETURN

END SUBROUTINE gd2gps

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


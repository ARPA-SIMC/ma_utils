PROGRAM coord2marks
!--------------------------------------------------------------------------
! Legge una lista di coordinate nel formato: 'label' x y (estra_punti)
! La riscrive nel formato lat lon lab (per plot marks GRADS)
! Gestisce coordinate geografiche (gradi e decimi o gradi.primi.secondi), 
! ruotate e UTM
!
!                                         Versione 1.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

REAL,PARAMETER :: xrot=10., yrot=57.5     ! centro rotazione coord. ruotate
INTEGER, PARAMETER :: utmz = 32           ! zona per coordinate UTM 
!
REAL :: xgeo,ygeo,xout,yout,xread,yread
CHARACTER (LEN=200) :: filein,fileout
CHARACTER (LEN=10) :: strx,stry,lab
CHARACTER (LEN=3) :: inpc,outc
INTEGER :: ier,iz,eof,eor,k,ios

!--------------------------------------------------------------------------
! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,inpc)
CALL getarg(3,fileout)
CALL getarg(4,outc)
IF (filein == "" .OR. TRIM(filein) == "-h" .OR. fileout == "" .OR. &
  (inpc/="gps" .AND. inpc/="geo" .AND. inpc/="utm" .AND. inpc/="rot") .OR. &
  (outc/="gps" .AND. outc/="geo" .AND. outc/="utm" .AND. outc/="rot")) THEN
  WRITE (*,*) "Legge una lista di coordinate nel formato: label x y (estra_punti)"
  WRITE (*,*) "  e la riscrive nel formato lat lon lab (per plot marks GRADS)"
  WRITE (*,*) ""
  WRITE (*,*) "Uso: corrd2marks.exe filein inpc fileout outc"
  WRITE (*,*) "Valori ammessi per inpc e outc: gps,geo,utm,rot"
  WRITE (*,*) ""
  STOP
ENDIF

!--------------------------------------------------------------------------

CALL get_eof_eor(eof,eor)
OPEN (UNIT=40,FILE=filein,STATUS="OLD",ACTION="READ")
OPEN (UNIT=41,FILE=fileout,STATUS="REPLACE",ACTION="WRITE")

DO k = 1,HUGE(0)

! Leggo le coordinate e se necessario le converto a geografiche
  IF (inpc == "gps") THEN
    READ (40,*,IOSTAT=ios) lab,strx,stry
    IF (ios == eof) EXIT
    CALL gps2gd(strx,stry,xgeo,ygeo,ier)

  ELSE IF (inpc == "geo") THEN
    READ (40,*,IOSTAT=ios) lab,xgeo,ygeo
    IF (ios == eof) EXIT

  ELSE IF (inpc == "rot") THEN
    READ (40,*,IOSTAT=ios) lab,xread,yread
    IF (ios == eof) EXIT
    CALL rtll(xread,yread,xrot,yrot,xgeo,ygeo)

  ELSE IF (inpc == "utm") THEN
    READ (40,*,IOSTAT=ios) lab,xread,yread
    IF (ios == eof) EXIT
    CALL utm2ll(xread,yread,utmz,.FALSE.,ygeo,xgeo)

  ENDIF    

! Calcolo le coordinate nel sistema richiesto e scrivo
  IF (outc == "geo") THEN
    xout = xgeo
    yout = ygeo
    WRITE (41,'(f7.3,2x,f7.3,2x,a)') yout,xout,TRIM(lab)

  ELSE IF (outc == "rot") THEN
    CALL tll(xgeo,ygeo,xrot,yrot,xout,yout)
    WRITE (41,'(f7.3,2x,f7.3,2x,a)') yout,xout,TRIM(lab)

  ELSE IF (outc == "utm") THEN
    CALL ll2utm(ygeo,xgeo,utmz,xout,yout,iz)
    WRITE (41,'(f9.1,2x,f9.1,2x,a)') yout,xout,TRIM(lab)

  ENDIF    


ENDDO

WRITE (*,*) "Elaborati ",k-1," punti"

END PROGRAM coord2marks

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE gps2gd(strx,stry,xx,yy,ier)
!
! Trasfoma le coordinate gradi.primi.secondi in gradi.decimi
!
IMPLICIT NONE
!
CHARACTER (LEN=10), INTENT(IN) :: strx,stry
REAL, INTENT(OUT) :: xx,yy
INTEGER, INTENT(OUt) :: ier

INTEGER :: gg,pp,ss,p1,p2
!

p1 = INDEX(strx,".")
p2 = INDEX(strx,".",BACK=.TRUE.)

IF (p1 == 0 .OR. p2 == 0) GOTO 9999
READ (strx(1:p1-1),*,ERR=9999) gg
READ (strx(p1+1:p2-1),*,ERR=9999) pp
READ (strx(p2+1:),*,ERR=9999) ss
xx = REAL(gg) + (REAL(pp) + REAL(ss)/60.) / 60.

p1 = INDEX(stry,".")
p2 = INDEX(stry,".",BACK=.TRUE.)
IF (p1 == 0 .OR. p2 == 0) GOTO 9999
READ (stry(1:p1-1),*,ERR=9999) gg
READ (stry(p1+1:p2-1),*,ERR=9999) pp
READ (stry(p2+1:),*,ERR=9999) ss
yy = REAL(gg) + (REAL(pp) + REAL(ss)/60.) / 60.

ier = 0
RETURN

9999 CONTINUE
ier = 1
RETURN

END SUBROUTINE gps2gd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!----------------------------------------------------------------------
      subroutine ll2utm(rlat,rlon,iz0,x,y,iz)
!----------------------------------------------------------------------
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
      end

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

!!-------------------------------------------------------------------------
      SUBROUTINE TLL(ALMD,APHD,TLM0D,TPH0D,TLMD,TPHD)        
!-------------------------------------------------------------------------
! trasforma le coordinate geografiche ordinarie (ALMD,APHD) in
! coordinte ruotate (TLMD,TPHD). I/O in gradi e decimi.
! TLM0D, TPH0D: lon e lat del centro di rotazione, in gradi e decimi.
! 
      PARAMETER (DTR=3.141592654/180.)

      CTPH0=COS(TPH0D*DTR)
      STPH0=SIN(TPH0D*DTR)

      RELM=(ALMD-TLM0D)*DTR                                             
      SRLM=SIN(RELM)                                                    
      CRLM=COS(RELM)                                                    

      APH=APHD*DTR                                                      
      SPH=SIN(APH)                                                      
      CPH=COS(APH)                                                      

      CC=CPH*CRLM                                                       
      ANUM=CPH*SRLM                                                     
      DENOM=CTPH0*CC+STPH0*SPH                                          

      TLMD=ATAN2(ANUM,DENOM)/DTR
      TPHD=ASIN(CTPH0*SPH-STPH0*CC)/DTR

      RETURN                                                          
      END   

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

!-------------------------------------------------------------------------
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


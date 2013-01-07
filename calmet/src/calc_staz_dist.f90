PROGRAM calc_staz_dist
!--------------------------------------------------------------------------
! Programma per calcolare le distanze minime in un insieme stazioni.
!
! Legge un elenco di stazioni in formato condivisione interregionale e
! scrive:
! - per ciascuna stazione, nome e distanza delle ndist stazioni dell'
!   elenco che le sono piu' vicine (file staz_near.dat)
! - l'elenco delle stazioni nel formato richiesto dallo script grads 
!   draw_marks.gs (file anag_grads.dat)
!
! Uso:
! clac_staz_dist.exe [-h] nome_file_anag
!
! Note:
! Nasce per selezionare le stazioni per un run di Calmet, potrebbe essere
! esteso p.es per calcolare la densita' media dell'insieme (definire un 
! grigliato, calcl. la distanza della staz. piu' vicina a ciascun punto 
! griglia...
!--------------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------------
! 0) Dichiarazioni - costanti

INTEGER, PARAMETER :: ndist = 5       ! n.ro staz. piu' vicine da calcolare
INTEGER, PARAMETER :: mxstaz = 1000   ! n.ro max di stazioni in file_in
INTEGER, PARAMETER :: utmz = 32       ! zonoa UTM per calcolo distanze

REAL :: xutm(mxstaz),yutm(mxstaz),dist_near(mxstaz,ndist)
REAL :: rdum,dd,an_lat,an_lon
INTEGER :: nstaz,an_usr,id_near(mxstaz,ndist)
INTEGER :: ifmt,k,k2,kdist,ios,eof,eor,idum
CHARACTER (LEN=100) :: chpar,file_in,file_out,chfmt
CHARACTER (LEN=8) :: names(mxstaz)
CHARACTER (LEN=2) :: str_net
CHARACTER (LEN=1) :: ch1


!--------------------------------------------------------------------------
! 1) Preliminari (parametro da riga comandi)

CALL getarg(1,chpar)
IF (TRIM(chpar) == "" .OR. TRIM(chpar) == "-h") THEN
  WRITE (*,*) "Uso: clac_staz_dist [-h] nome_file_anag"
  STOP
ELSE
  file_in = chpar
ENDIF

CALL get_eof_eor(eof, eor)

!--------------------------------------------------------------------------
! 2) Leggo lista staz, calcolo coord. UTM e scrivo l'elanco stazioni nel
!    formato per GRADA

! 2.1: Prima lettura (trovo formato file)
OPEN (UNIT=20, FILE=file_in, STATUS="OLD", ACTION="READ", ERR=9999)
READ (20,*,ERR=9999)
READ (20,'(2x,a1)',ERR=9999) ch1
IF (ch1 == " ") THEN
  ifmt = 1                                  ! formato SMR - codici Oracle
  WRITE (*,*) "Il file di input e' in formato SMR - codici Oracle"
ELSE
  ifmt = 0                                  ! formato standard regioni
  WRITE (*,*) "Il file di input e' in formato standard regioni"
ENDIF
CLOSE (20)

! 2.2: Seconda lettura (leggo per davvero e calcolo coord. UTM)

OPEN (UNIT=31, FILE= "marks_anag.dat", STATUS="REPLACE", FORM="FORMATTED")
OPEN (UNIT=20, FILE=file_in, STATUS="OLD", ACTION="READ", ERR=9999)
READ (20,*,ERR=9999)

nstaz = 0
DO 
! Leggo un record e lo controllo
  IF (ifmt == 0) THEN
    READ (20,'(a2,i5,1x,20x,2(1x,f8.3))',IOSTAT=ios) &
      str_net,an_usr,an_lat,an_lon
  ELSE IF (ifmt == 1) THEN
    READ (20,'(a2,1x,i5,1x,20x,2(1x,f8.3))',IOSTAT=ios) &
      str_net,an_usr,an_lat,an_lon
  ENDIF

  IF (ios == eof) EXIT
  IF (ios /= 0) THEN
    WRITE (*,*) "Formato illegale o riga vuota, skippo"
    CYCLE
  ENDIF

  IF (nstaz >= mxstaz) THEN
    WRITE (*,'(a,i4,a)') "n.ro max di stazioni (",mxstaz, &
      ") superato, escludo le ultime"
    EXIT
  ENDIF

! Se la staz e' ok, calcolo coord UTM e la scrivo per GRADS
  nstaz = nstaz + 1

  IF (ifmt == 0) THEN
    WRITE (names(nstaz),'(a2,i5.5)') str_net,an_usr
  ELSE IF (ifmt == 1) THEN
    WRITE (names(nstaz),'(a2,1x,i5.5)') str_net,an_usr
  ENDIF
  CALL ll2utm(an_lat,an_lon,utmz,xutm(nstaz),yutm(nstaz),idum)  

  WRITE (31,*) an_lat,an_lon,names(nstaz)

ENDDO

CLOSE(20)
CLOSE(31)
WRITE (*,'(a,i4,a)') "Trovate ",nstaz," stazioni"  

!--------------------------------------------------------------------------
! 3) Trovo le stazioni piu' vicine

id_near(:,:) = 0
dist_near(:,:) = HUGE(rdum)

DO k = 1,nstaz
DO k2 = 1,nstaz
  IF (k2 == k) CYCLE
  dd = SQRT( (xutm(k)-xutm(k2))**2 + (yutm(k)-yutm(k2))**2 )

  DO kdist = 1,ndist
  IF (dd < dist_near(k,kdist)) THEN
    dist_near(k,kdist+1:ndist) = dist_near(k,kdist:ndist-1)
    id_near(k,kdist+1:ndist) = id_near(k,kdist:ndist-1)
    dist_near(k,kdist) = dd
    id_near(k,kdist) = k2
    EXIT
  ENDIF
  ENDDO

ENDDO
ENDDO
WRITE (*,*) "Calcolate distanze"

!--------------------------------------------------------------------------
! 4) Scrivo l'elenco delle stazioni piu' vicine

WRITE (chfmt,'(a,i2,a)') "(a8,a1,",ndist,"(4x,a8,1x,f7.2))"
OPEN (UNIT=30, FILE= "staz_near.dat", STATUS="REPLACE", FORM="FORMATTED")

DO k = 1, nstaz
  WRITE (30,chfmt) names(k),":", &
    (names(id_near(k,kdist)), dist_near(k,kdist), kdist=1,ndist)
ENDDO

CLOSE(30)
WRITE (*,*) "Scritto elenco stazioni piu' vicine"

STOP

!--------------------------------------------------------------------------
! 5) Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_in)
STOP

END PROGRAM calc_staz_dist

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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
      end

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

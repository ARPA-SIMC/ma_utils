PROGRAM sel_punti_grid
!--------------------------------------------------------------------------
! Trova i punti della griglia coarse che cadono all'interno della griglia 
! nested; scrive gli indici dei punti (sel_punti_grid.pts.csv) e gli
! estremi della sottoarea (sel_punti_grid.zoom)
!
!                                         Versione 3.0.4, Enrico 24/09/2013
!--------------------------------------------------------------------------
USE file_utilities
USE seriet_utilities

IMPLICIT NONE

! Path di default delle tabelle seriet
! PKGDATAROOTDIR viene sostituito in fase di compilazione con il path delle
! tabelle seriet (di solito /usr/share/ma_utils). La sostituzione sfrutta 
! il comando gfortran -D; vedi Makefile.am nelle singole dir.
CHARACTER (LEN=40) :: tab_path_def = PKGDATAROOTDIR
CHARACTER (LEN=40) :: tab_env = "MA_UTILS_DAT"

! Altri Parametri costanti
CHARACTER (LEN=80),PARAMETER :: file_out1 = "sel_punti_grid.pts.csv"
CHARACTER (LEN=80),PARAMETER :: file_out2 = "sel_punti_grid.zoom"

! Altre variabili del programma
TYPE (csv_record) :: csvline
REAL :: xf(2),xl(2),yf(2),yl(2),xrot(2),yrot(2),dx(2),dy(2),dxkm(2),dykm(2)
REAL :: xx_gn,yy_gn,xx_gc,yy_gc,xx_geo,yy_geo
REAL :: ri,rj,rimx,rjmx,rimn,rjmn,buffer,tot_area_gn
INTEGER :: nx(2),ny(2),scan(2,3),utmz(2)
INTEGER :: maxp,idskip
INTEGER :: iif,jjf,iil,jjl,npsel,npcal,nisel,njsel,nitry,njtry,ksk
INTEGER :: eof,eor,ios,ios2,idum,kpar,cnt_par,kg,i,j,k,cnt_sel
CHARACTER (LEN=120) :: nfile,chrec
CHARACTER (LEN=80) :: charg,tab_path
CHARACTER (LEN=78) :: ch78
CHARACTER (LEN=61) :: ch61
CHARACTER (LEN=40) :: str_lon,str_lat,label
CHARACTER (LEN=20) :: area(2),area_dum,chfmt
CHARACTER (LEN=1) :: next_arg,proj(2)

!--------------------------------------------------------------------------
! 1: Preliminari

! Parametri da riga comandi
buffer = 0.
maxp = 0
cnt_par = 0
next_arg = ""
ios = 0
ios2 = 0

DO kpar = 1,HUGE(kpar)
  CALL getarg(kpar,charg)

  IF (TRIM(charg) == "") THEN
    EXIT

  ELSE IF (TRIM(charg) == "-h") THEN
    CALL write_help
    STOP

  ELSE IF (TRIM(charg) == "-buffer") THEN  
    next_arg = "b"

  ELSE IF (TRIM(charg) == "-mxp") THEN  
    next_arg = "p"

  ELSE IF (next_arg == "b") THEN
    READ (charg,*,IOSTAT=ios) buffer
    next_arg = ""

  ELSE IF (next_arg == "p") THEN
    READ (charg,*,IOSTAT=ios2) maxp
    next_arg = ""

  ELSE
    cnt_par = cnt_par + 1
    SELECT CASE (cnt_par)
    CASE (1)
      area(1) = charg
    CASE (2)
      IF (TRIM(charg) == "geo" .OR. TRIM(charg) == "GEO") THEN
        proj(1) = "g"
      ELSE IF (TRIM(charg) == "utm" .OR. TRIM(charg) == "UTM") THEN
        proj(1) = "u"
      ELSE
        CALL write_help
        STOP 1
      ENDIF 
    CASE (3)
      area(2) = charg
    CASE (4)
      IF (TRIM(charg) == "geo" .OR. TRIM(charg) == "GEO") THEN
        proj(2) = "g"
      ELSE IF (TRIM(charg) == "utm" .OR. TRIM(charg) == "UTM") THEN
        proj(2) = "u"
      ELSE
        CALL write_help
        STOP 1
      ENDIF 
    END SELECT

  ENDIF
ENDDO

IF (cnt_par /= 4 .OR. TRIM(area(1)) == "" .OR. TRIM(area(2)) == "" .OR. &
    ios /= 0 .OR. ios2 /= 0 .OR. (maxp /= 0 .AND. maxp < 4)) THEN
  CALL write_help
  STOP 1
ENDIF  

! Trovo codice EOF
CALL get_eof_eor(eof,eor)

! Trovo path files aree_geo.dat e aree_utm.dat
tab_path = ""
CALL GETENV(tab_env,tab_path)
IF (TRIM(tab_path) == "") tab_path = tab_path_def

!--------------------------------------------------------------------------
! 2) Leggo i dati delle griglie

DO kg = 1,2
  IF (proj(kg) == "g") THEN
    WRITE (nfile,'(2a)') TRIM(tab_path),"/aree_geo.dat"
    OPEN (UNIT=22, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9999)
    DO
      READ (22,'(a)',IOSTAT=ios) ch78    
      IF (ios == eof) GOTO 9997
      IF (ios /= 0) GOTO 9998
      IF (TRIM(ch78) == "" .OR. ch78(1:1) == "!") CYCLE
    
      READ (ch78,'(a10,2(1x,i4),6(1x,f8.3),1x,3i1)',IOSTAT=ios) &
        area_dum,nx(kg),ny(kg),xf(kg),yf(kg),xl(kg),yl(kg), &
        xrot(kg),yrot(kg),scan(kg,1:3)
      IF (ios /= 0) GOTO 9998
      IF (TRIM(area_dum) == TRIM(area(kg))) EXIT
    ENDDO
    CLOSE(22)

  ELSE IF (proj(kg) == "u") THEN
    WRITE (nfile,'(2a)') TRIM(tab_path),"/aree_utm.dat"
    OPEN (UNIT=22, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9999)
    DO
      READ (22,'(a)',IOSTAT=ios) ch61

      IF (ios == eof) GOTO 9997
      IF (ios /= 0) GOTO 9998
      IF (TRIM(ch61) == "" .OR. ch61(1:1) == "!") CYCLE
      READ (ch61,'(a10,2(1x,i4),4(1x,f8.3),1x,i4)',IOSTAT=ios) &
        area_dum,nx(kg),ny(kg),xf(kg),yf(kg),xl(kg),yl(kg),utmz(kg)
      IF (ios /= 0) GOTO 9998
      IF (TRIM(area_dum) == TRIM(area(kg))) EXIT
    ENDDO
    CLOSE(22)

  ENDIF

  dx(kg) = (xl(kg) - xf(kg)) / REAL(nx(kg)-1)
  dy(kg) = (yl(kg) - yf(kg)) / REAL(ny(kg)-1)

  IF (proj(kg) == "g") THEN
    dxkm(kg) = dx(kg) * 111. * COS((yf(kg)+yl(kg))*3.1415/360.)
    dykm(kg) = dy(kg) * 111.
  ELSE
    dxkm(kg) = dx(kg)
    dykm(kg) = dy(kg)
  ENDIF
ENDDO

WRITE (*,'(a,2f8.3)') "Passo griglia coarse (km): ",dxkm(1),dykm(1)
WRITE (*,'(a,2f8.3)') "Passo griglia nested (km): ",dxkm(2),dykm(2)

!--------------------------------------------------------------------------
! 3) Trovo gli indici estremi dell'aera nested (relativi alla griglia 
!    coarse). Per ciascun punto della griglia nested, calcolo prima le 
!    coordinate geografiche, poi quelle nel sistema coarse e infine gli 
!    indici nella griglia coarse.

rimx = -HUGE(0.)
rjmx = -HUGE(0.)
rimn = HUGE(0.)
rjmn = HUGE(0.)

DO i = 1,nx(2)
DO j = 1,ny(2)
  xx_gn = xf(2) + (i-1) * dx(2)
  yy_gn = yf(2) + (j-1) * dy(2)

  IF (proj(2) == "u") THEN
    CALL utm2ll(xx_gn,yy_gn,utmz(2),.FALSE.,yy_geo,xx_geo)
  ELSE IF (proj(2) == "g" .AND. (xrot(2) /= 0. .OR. yrot(2) /= 0.)) THEN
    CALL rtll(xx_gn,yy_gn,xrot(2),yrot(2),xx_geo,yy_geo)
  ELSE
    xx_geo = xx_gn
    yy_geo = yy_gn
  ENDIF

  IF (proj(1) == "u") THEN
    CALL ll2utm(yy_geo,xx_geo,utmz(1),xx_gc,yy_gc,idum)
  ELSE IF (proj(1) == "g" .AND. (xrot(1) /= 0. .OR. yrot(1) /= 0.)) THEN
    CALL tll(xx_geo,yy_geo,xrot(1),yrot(1),xx_gc,yy_gc)
  ELSE
    xx_gc = xx_geo
    yy_gc = yy_geo
  ENDIF

  ri = (xx_gc - xf(1)) / dx(1) + 1.
  rj = (yy_gc - yf(1)) / dy(1) + 1.

  rimx = MAX(ri,rimx)
  rjmx = MAX(rj,rjmx)
  rimn = MIN(ri,rimn)
  rjmn = MIN(rj,rjmn)
ENDDO
ENDDO

WRITE (*,'(a)') &
  "Indici estremi dell'area nested nella griglia coarse (W,S,E,N):"
WRITE (*,'(4(2x,f7.3))') rimn,rjmn,rimx,rjmx

IF (rimx < rimn .OR. rjmx < rjmn) GOTO 9996

!--------------------------------------------------------------------------
! 4) Aggiungo il buffer (attorno all'area nested) e trovo i punti della 
!    griglia coarse che cadono al suo interno. Se sono troppi, sfoltisco,
!    in modo da includere in ogni caso tutta l'area nested + buffer

! Aggiungo buffer
iif = INT(rimn - buffer) + 1
jjf = INT(rjmn - buffer) + 1
iil = INT(rimx + buffer)
jjl = INT(rjmx + buffer)

! Se sono uscito dall'area coarse, restringo
IF (iif < 1) THEN
  iif = 1
  WRITE (*,*) "Area nested+buffer non inclusa nell'aera coarse, restringo lato W"
ENDIF
IF (jjf < 1) THEN
  jjf = 1
  WRITE (*,*) "Area nested+buffer non inclusa nell'aera coarse, restringo lato S"
ENDIF
IF (iil > nx(1)) THEN
  iil = nx(1)
  WRITE (*,*) "Area nested+buffer non inclusa nell'aera coarse, restringo lato E"
ENDIF
IF (jjl > ny(1)) THEN
  jjl = ny(1)
  WRITE (*,*) "Area nested+buffer non inclusa nell'aera coarse, restringo lato N"
ENDIF

! Log dei punti che selezionerei
nisel = iil-iif+1
njsel = jjl-jjf+1

npsel = nisel * njsel
tot_area_gn = (REAL(nx(2))*dxkm(2) + 2*buffer*dxkm(1)) * &
  (REAL(ny(2))*dykm(2) + 2*buffer*dykm(1))
npcal = tot_area_gn / (dxkm(1)*dykm(1))
WRITE (*,'(a,f10.3)') "Area griglia nested(km2): ",tot_area_gn
WRITE (*,'(a,i5,a,i5,a)') "punti entro area nested+buffer: ",npsel," (attesi: ",npcal,")"

! Se necessario, calcolo lo sfoltimento
IF (maxp > 0 .AND. npsel > maxp) THEN
  IF ((nisel*njsel+1)/2 <= maxp) THEN
    idskip = 1
    WRITE (*,*) "Applico sfoltimento 1/2 (a scacchiera)"
  ELSE
    DO ksk = 2,HUGE(0)
      nitry = (nisel+ksk-2)/ksk+1
      njtry = (njsel+ksk-2)/ksk+1
      IF (nitry*njtry <= maxp) EXIT
    ENDDO
    idskip = ksk
    WRITE (*,'(a,i4)') "Applico sfoltimento 1/",idskip**2
  ENDIF
ELSE
  idskip = 0
ENDIF

!--------------------------------------------------------------------------
! 5) Scrivo le informazioni sui punti griglia del modello nel file .pts.csv

! 5.1 Apro il file, scrivo header, inizializzaioni
OPEN (UNIT=20, FILE=file_out1, STATUS="REPLACE", ACTION="WRITE")
chrec = ""
CALL build_header("lspts",chrec)
WRITE (20,'(a)') TRIM(chrec)

! 5.2 Ciclo sui punti
cnt_sel = 0
DO j = jjf, MAX(jjl,jjl+idskip-1)
DO i = iif, MAX(iil,iil+idskip-1)
  k = (j-1)*nx(1) + i

! 5.2.1 Gestione skip dei punti
  IF (idskip == 0) THEN
    IF (i > iil .OR. j > jjl) CYCLE
  ELSE IF (idskip == 1) THEN
    IF (i > iil .OR. j > jjl .OR. MOD(i+j,2) /= 0) CYCLE
  ELSE IF (idskip > 1) THEN
    IF (MOD(i,idskip) /= 1 .OR. MOD(j,idskip) /= 1) CYCLE
  ENDIF
  cnt_sel = cnt_sel + 1

! 5.2.2 Calcolo coordinate
  xx_gc = xf(1) + (i-1) * dx(1)
  yy_gc = yf(1) + (j-1) * dy(1)
  IF (proj(1) == "u") THEN
    CALL utm2ll(xx_gc,yy_gc,utmz(1),.FALSE.,yy_geo,xx_geo)
  ELSE IF (proj(1) == "g" .AND. (xrot(1) /= 0. .OR. yrot(1) /= 0.)) THEN
    CALL rtll(xx_gc,yy_gc,xrot(1),yrot(1),xx_geo,yy_geo)
  ELSE
    xx_geo = xx_gc
    yy_geo = yy_gc
  ENDIF

! 5.2.3 Scrivo in formato .pts.csv
  WRITE (chfmt,'(a,i2,a,i2,a)') "(f",coo_ndec+5,".",coo_ndec,")"
  CALL init(csvline)

! Coordinate (con il numero di decimali richiesto in seriet_utilities)
  WRITE (str_lon,chfmt) xx_geo
  WRITE (str_lat,chfmt) yy_geo
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_lon)))
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_lat)))

! Label
  WRITE (label,'(a,i6.6)') "punto-",k
  CALL csv_record_addfield(csvline,TRIM(label))

! Indici
  CALL csv_record_addfield(csvline,i)
  CALL csv_record_addfield(csvline,j)
  CALL csv_record_addfield(csvline,k)

! Output su file
  WRITE (20,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

! Vecchia scrittura (file .ptn)
! WRITE (20,'(i6,1x,i1,a,2f8.3,2i5)') k,0," ! ",yy_geo,xx_geo,j,i

ENDDO
ENDDO
WRITE (*,*) "Selezionati ",cnt_sel," punti"

CLOSE (20)

!--------------------------------------------------------------------------
! 6) Scrivo gli estremi della sottoarea nel file .zoom

OPEN (UNIT=21, FILE=file_out2, STATUS="REPLACE", ACTION="WRITE")

WRITE (21,'(a)') "Indici estremi inclusi nella sottoarea (W S E N)"
WRITE (21,'(4(i4,1x))') iif,jjf,iil,jjl

CLOSE (21)
STOP

!--------------------------------------------------------------------------
! 7) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(nfile)
RETURN

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)
RETURN

9997 CONTINUE
WRITE (*,*) "Area ",TRIM(area(kg))," non trovata in ",TRIM(nfile)
RETURN

9996 CONTINUE
WRITE (*,*) "Errore nel calcolo estremi area nested"
RETURN

END PROGRAM sel_punti_grid

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE

!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) ""
WRITE (*,*) "sel_punti_grid.exe area_coarse proj_coarse area_nest proj_nest "
WRITE (*,*) "                   [-h] [-buffer X] [-mxp N]"
WRITE (*,*) "Trova i punti della griglia coarse che cadono all'interno della griglia nested"
WRITE (*,*) "Scrive i loro indici nel file sel_punti_grid.ptn"
WRITE (*,*) ""
WRITE (*,*) " -h:                      visualizza questo help"
WRITE (*,*) "area_coarse, area_nested: nomi inclusi nei files aree_geo.dat o aree_utm.dat"
WRITE (*,*) "proj_coarse, proj_nested: geo oppure utm"
WRITE (*,*) "-buffer X:                include anche i punti che cadono in una cornice di x"
WRITE (*,*) "                          passi della griglia coarse attorno all'area nested"
WRITE (*,*) "-mxp N:                   seleziona al massimo N punti (N>3 se necessario "
WRITE (*,*) "                          sfoltisce i punti scelti)"

RETURN

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_eof_eor(eof, eor)
!--------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!--------------------------------------------------------------------------
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
!
! Subroutnies di libreria per conversioni di coordinate
!
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

!-------------------------------------------------------------------------
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





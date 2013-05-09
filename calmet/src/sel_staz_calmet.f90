PROGRAM sel_staz_calmet
!--------------------------------------------------------------------------
! Programma per aiutare la selezione delle stazioni per girare calmet su di
! un'area specificata.
!
! Legge gli estremi dell'area richiesta (di cui deve essere specificato il
! codice) da aree_utm.dat, l'anagrafica da db_anagrafica.dat e i codici
! oracle dei parametri da code_parametri.dat
! E'possibile selezionare tutte le stazioni che cadono nell'area, oppure 
! applicare alcuni criteri di selezione (vedi file .inp).
! Scrive  i files srq_surf_AREA.lst e srq_temp_AREA.lst, nel formato della 
! condivisione interregionale (con in piu' le coord. UTM a fine record).
! Scrive inoltre le informazioni sulle stazioni selezionate in formato 
! anagrafica e in formato marks (per GRADS)
!
! Uso:
! sel_staz_calmet.exe [-h] [-c] codice_area
! -h      : visualizza help
! -c      : crea un file sel_staz_calmet.inp di esempio
!
! Compilazione:
! f90 (non usa moduli ne' librerie)
!
! Note:
! Per essere usato dalla catena calmet, srq_surf_AREA.lst deve essere 
! verificato/modificato manualmente e poi convertito nel formato giusto 
! (con i codici ORACLE dei vari parametri) dal programma conv_srq_surf.
!
!                                                  V2.1.2 Enrico 02/05/2013
!--------------------------------------------------------------------------

IMPLICIT NONE

!--------------------------------------------------------------------------
! 0) Dichiarazioni - costanti

! 0.1 Costanti ecc. relativi al file "db_anagrafica.dat"
CHARACTER (LEN=40), PARAMETER :: anag_env = "HOME_BONAFE"
CHARACTER (LEN=40), PARAMETER :: anag_path = "osservazioni/dat"
CHARACTER (LEN=40), PARAMETER :: anag_name = "db_anagrafica.dat"

! 0.2 Path di default delle tabelle seriet
! PKGDATAROOTDIR viene sostituito in fase di compilazione con il path delle
! tabelle seriet (di solito /usr/share/ma_utils). La sostituzione sfrutta 
! il comando gfortran -D; vedi Makefile.am nelle singole dir.
CHARACTER (LEN=40) :: tab_path_def = PKGDATAROOTDIR
CHARACTER (LEN=40) :: tab_env = "MA_UTILS_DAT"

! 0.3 Costanti ecc. relativi al file "staz_excl.lst"
INTEGER, PARAMETER :: max_excl = 500   ! n.ro max di stazioni escludibili
INTEGER :: ex_id_rete(max_excl),ex_id_usr(max_excl)

! 0.4 Gestione parametri e assegnazione path espliciti.
INTEGER, PARAMETER :: req_par = 1
INTEGER :: kpar,cnt_par
CHARACTER (LEN=100) :: chpar

! 0.5 Altre variabili del programma
INTEGER, PARAMETER :: max_reti = 10   ! n.ro max di reti (elenco in calmet.inp)

REAL :: an_lon,an_lat,max_quo,min_quo,rinf_surf,rinf_temp
REAL :: dx,dy,xorig,yorig,xlast,ylast,x1,x2,y1,y2
REAL :: xutm,yutm
INTEGER :: req_reti(max_reti)
INTEGER :: an_net,an_usr,an_db,an_quo
INTEGER :: nx,ny,utmz,idum,ios,k
INTEGER :: lexcl,nexcl,name_net_out,name_sta_out,track_out,code_out
INTEGER :: ieof,ieor,cnt_surf,cnt_temp,req_nreti
CHARACTER (LEN=200) :: chfmt_cp,nfile,header
CHARACTER (LEN=90) :: chrec
CHARACTER (LEN=80) :: ch80,tab_path
CHARACTER (LEN=61) :: ch61
CHARACTER (LEN=50) :: an_nome
CHARACTER (LEN=10) :: grid_area,dum_area
CHARACTER (LEN=8) :: ch8
CHARACTER (LEN=2) :: str_net

!--------------------------------------------------------------------------
! 1) Input: dati richiesti dall'utente e lettura da files costanti

! 1.1 parametri da riga comandi
cnt_par = 0
DO kpar = 1,HUGE(kpar)
  CALL getarg(kpar,chpar)

  SELECT CASE (TRIM(chpar))
  CASE ("")
    EXIT
  CASE ("-h")
    CALL scrive_help
    STOP
  CASE ("-c")
    CALL scrive_esempio
    STOP
  CASE DEFAULT
    cnt_par = cnt_par + 1
    grid_area = chpar
  END SELECT
ENDDO
IF (cnt_par /= req_par) STOP "Errore nei parametri "

! 1.2 leggo sel_staz_calmet.inp
OPEN (UNIT=21, FILE="sel_staz_calmet.inp", STATUS="OLD", &
  ACTION="READ", ERR=9999)
  READ (21,*,ERR=9999) req_nreti
  IF (req_nreti > max_reti) THEN
    WRITE (*,*) "Richieste troppe reti: ",req_nreti, " max ", max_reti
    STOP
  ENDIF
  READ (21,*,ERR=9999) req_reti(1:req_nreti) 
  READ (21,*,ERR=9999) rinf_surf 
  READ (21,*,ERR=9999) rinf_temp
  READ (21,*,ERR=9999) max_quo
  READ (21,*,ERR=9999) min_quo
  READ (21,*,ERR=9999) lexcl
  READ (21,*,ERR=9999) name_net_out
  READ (21,*,ERR=9999) name_sta_out
  READ (21,*,ERR=9999) track_out
CLOSE(21)

! 1.3 leggo gli estremi dell'area da aree_utm.dat
CALL get_eof_eor(ieof,ieor)

tab_path = ""
CALL GETENV(tab_env,tab_path)
IF (TRIM(tab_path) == "") tab_path = tab_path_def
WRITE (nfile,'(2a)') TRIM(tab_path),"/aree_utm.dat"
OPEN (UNIT=22, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9998)

DO
  READ (22,'(a)',IOSTAT=ios) ch61    
  IF (ios /= 0) THEN
    WRITE (*,*) "Area ",TRIM(grid_area)," non trovata in ",TRIM(nfile)
    RETURN
  ENDIF

  IF (TRIM(ch61) == "" .OR. ch61(1:1) == "!") CYCLE

  READ (ch61,'(a10,2(1x,i4),4(1x,f8.3),1x,i4)',IOSTAT=ios) &
    dum_area,nx,ny,x1,y1,x2,y2,utmz
  IF (ios /= 0) THEN
    WRITE (*,*) "Record illegale in ",TRIM(nfile)
    WRITE (*,'(a)') ch61
    RETURN
  ENDIF

  IF (TRIM(dum_area) == TRIM(grid_area)) EXIT
ENDDO

CLOSE(22)

! 1.4 Calcolo il passo griglia e allargo l'area per includere le calle ai bordi
dx = (x2-x1) / REAL(nx-1)
dy = (y2-y1) / REAL(ny-1)
xorig = x1 - dx/2.
yorig = y1 - dy/2.
xlast = x2 + dx/2.
ylast = y2 + dy/2.
WRITE (*,*) "Griglia ",TRIM(grid_area)," dx,dy: ",dx,dy

! 1.5 Se richiesto, leggo dal file staz_excl.lst l'elenco delle stazioni
!     da escludere a priori
IF (lexcl == 1) THEN

OPEN (UNIT=24, FILE="staz_excl.lst", STATUS= "OLD", &
  ACTION="READ", ERR=9996)
  DO k=1,max_excl
    READ (24,'(i2,i5)',IOSTAT=ios) ex_id_rete(k),ex_id_usr(k)
    IF (ios == ieof) THEN
      EXIT
    ELSE IF (ios /= 0) THEN
      GOTO 9996
    ENDIF
  ENDDO

  IF (k > max_excl) THEN
    WRITE (*,*) "Troppe stazioni in staz_excl.lst, uso le prime ",max_excl
    nexcl = max_excl
  ELSE
    nexcl = k - 1
  ENDIF

CLOSE(24)

WRITE (*,*) "Stazioni da escludere esplicitamente: ",nexcl
ENDIF

!--------------------------------------------------------------------------
! 2) Scorro l'anagrafica, scrivendo sui files di output le stazioni buone,
!    con le informazioni e il formato richiesti dai run di Calmet

!--------------------------------------------------------------------------
! 2.1 Arpo files & gestisco header

IF (track_out == 0) THEN    ! tracciato condivisione
  header = "NtUsrid Nome                      Lat" // &
           "     Long Quot"
ELSE                        ! tracciato esteso SMR
  header = "NtUsrid Nome                      Lat" // &
           "     Long Quot           XUTM      YUTM"
ENDIF

WRITE (nfile,'(3a)') "srq_surf_",TRIM(grid_area),".lst"
OPEN (UNIT=30, FILE= nfile, STATUS="REPLACE", FORM="FORMATTED")
WRITE (30,'(a)') TRIM(header)

WRITE (nfile,'(3a)') "srq_temp_",TRIM(grid_area),".lst"
OPEN (UNIT=31, FILE= nfile, STATUS="REPLACE", FORM="FORMATTED")
WRITE (31,'(a)') TRIM(header)

WRITE (nfile,'(3a)') "anag_",TRIM(grid_area),".dat"
OPEN (UNIT=40, FILE= nfile, STATUS="REPLACE", FORM="FORMATTED")
WRITE (nfile,'(3a)') "marks_",TRIM(grid_area),"_geo.dat"
OPEN (UNIT=41, FILE= nfile, STATUS="REPLACE", FORM="FORMATTED")
WRITE (nfile,'(3a)') "marks_",TRIM(grid_area),"_utm.dat"
OPEN (UNIT=42, FILE= nfile, STATUS="REPLACE", FORM="FORMATTED")

CALL GETENV(anag_env,ch80)
nfile = TRIM(ch80) // "/" // TRIM(anag_path) // "/" // TRIM(anag_name)
OPEN (UNIT=23, FILE=nfile, STATUS= "OLD",ACTION="READ", ERR=9995)
READ (23,*,ERR=9995)
READ (23,*,ERR=9995)
READ (23,*,ERR=9995)

!---------------------
! ciclo sulle stazioni
cnt_surf = 0 
cnt_temp = 0 

DO
  READ (23,'(a)',IOSTAT=ios) chrec

  IF (ios == ieof .OR. INDEX(chrec,"rows selected") /= 0) EXIT
  IF (chrec == "") CYCLE

  READ (chrec,'(i4,2i7,f9.3,f8.3,i6,1x,a20)',IOSTAT=ios) &
    an_net,an_usr,an_db,an_lon,an_lat,an_quo,an_nome
  IF (ios == ieof) THEN
    EXIT
  ELSE IF (ios /= 0) THEN
    WRITE (*,*) "Record illegale in db_anagrafica.dat, skippo"
    WRITE (*,*) TRIM(chrec)
    CYCLE
  ELSE IF (an_lon == 0. .AND. an_lat == 0.) THEN
    CYCLE
  ENDIF

  IF (an_quo == -9999) an_quo = -999    ! devo stare in 4 cifre

!--------------------------------------------------------------------------
! 2.2 verifico se la stazione corrente e' "buona"  (stazioni superficiali)
  IF (an_net /= 2) THEN
 
! scarto reti non richieste
    IF (.NOT. ANY(an_net == req_reti(1:req_nreti) )) CYCLE

! scarto fuori area
    CALL ll2utm(an_lat,an_lon,utmz,xutm,yutm,idum)
    IF (xutm < xorig - rinf_surf .OR. xutm > xlast + rinf_surf .OR. &
        yutm < yorig - rinf_surf .OR. yutm > ylast + rinf_surf) CYCLE

! scarto in base alla quota stazione
    IF (an_quo > NINT(max_quo) .OR. an_quo < NINT(min_quo)) CYCLE

! scarto stazioni escluse esplicitamente
    IF (lexcl == 1 .AND. ANY(an_net == ex_id_rete(1:nexcl) .AND. &
      an_usr == ex_id_usr(1:nexcl)) ) CYCLE

! se ho passato tutti i test, scrivo la stazione sul file di output
    cnt_surf = cnt_surf + 1
    SELECT CASE (an_net)
    CASE(1,10)
      str_net = "SY"
    CASE(11,13,15,20,21,22,23)
      str_net = "EM"
    CASE(24)
      str_net = "LI"
    CASE(25)
      str_net = "PI"
    CASE(26)
      str_net = "TR"
    CASE(27)
      str_net = "VE"
    CASE(28)
      str_net = "SA"
    CASE(29)
      str_net = "LO"
    CASE(35)
      str_net = "CL"
    CASE DEFAULT
      str_net = "XX"
    END SELECT

    IF (name_sta_out == 0) THEN
      code_out = an_usr
    ELSE
      code_out = an_db
    ENDIF

    IF (name_net_out == 0 .AND. track_out == 0) THEN       !uso id reti
      WRITE (30,'(i2.2,i5.5,1x,a20,2(1x,f8.3),1x,i4,1x,a3)') &
        an_net,code_out,an_nome(1:20),an_lat,an_lon,an_quo,"   "
    ELSE IF (name_net_out == 0 .AND. track_out == 1) THEN
      WRITE (30,'(i2.2,i5.5,1x,a20,2(1x,f8.3),1x,i4,1x,a3,1x,2f10.3)') &
        an_net,code_out,an_nome(1:20),an_lat,an_lon,an_quo,"   ",xutm,yutm
    ELSE IF (name_net_out == 1 .AND. track_out == 0) THEN  !uso sigla reti
      WRITE (30,'(a2,i5.5,1x,a20,2(1x,f8.3),1x,i4,1x,a3)') &
        str_net,code_out,an_nome(1:20),an_lat,an_lon,an_quo,"   "
    ELSE IF (name_net_out == 1 .AND. track_out == 1) THEN
      WRITE (30,'(a2,i5.5,1x,a20,2(1x,f8.3),1x,i4,1x,a3,1x,2f10.3)') &
        str_net,code_out,an_nome(1:20),an_lat,an_lon,an_quo,"   ",xutm,yutm
    ENDIF

!--------------------------------------------------------------------------
! 2.3 verifico se la stazione corrente e' "buona"  (stazioni TEMP)
  ELSE

! scarto fuori area
    CALL ll2utm(an_lat,an_lon,utmz,xutm,yutm,idum)
    IF (xutm < xorig - rinf_temp .OR. xutm > xlast + rinf_temp .OR. &
        yutm < yorig - rinf_temp .OR. yutm > ylast + rinf_temp) CYCLE

! scarto stazioni escluse esplicitamente
    IF (lexcl == 1 .AND. ANY(an_net == ex_id_rete(1:nexcl) .AND. &
      an_usr == ex_id_usr(1:nexcl)) ) CYCLE

! se ho passato tutti i test, scrivo la stazione sul file di output
    cnt_temp = cnt_temp + 1
    IF (name_sta_out == 0) THEN
      code_out = an_usr
    ELSE
      code_out = an_db
    ENDIF

    IF (track_out == 0) THEN                     ! tracciato condivisione
      WRITE (31,'(i2.2,i5.5,1x,a20,2(1x,f8.3),1x,i4,1x,a3)') &
        an_net,code_out,an_nome(1:20),an_lat,an_lon,an_quo,""
    ELSE IF (track_out == 1) THEN                ! tracciato esteso EMR
      WRITE (31,'(i2.2,i5.5,1x,a20,2(1x,f8.3),1x,i4,1x,a3,1x,2f10.3)') &
        an_net,code_out,an_nome(1:20),an_lat,an_lon,an_quo,"",xutm,yutm
    ENDIF

  ENDIF

!--------------------------------------------------------------------------
! 2.4 scrivo tutte le stazioni selezionate (sup e quota) nei formati 
!     accessori (anagrafica SIM, marks UTM, marks geo)

! formato anagrafica
  WRITE (40,'(a)') chrec
  
! formato marks GEO
  WRITE (41,'(f7.3,1x,f7.3,3x,i2.2,a1,i5.5)') &
    an_lat,an_lon,an_net,"_",an_usr

! formato marks UTM
  WRITE (42,'(f8.2,1x,f8.2,3x,i2.2,a1,i5.5)') &
    yutm,xutm,an_net,"_",an_usr

ENDDO
! chiuso ciclo stazioni
!----------------------

! 2.3 Chiudo files e termino

CLOSE (23)
CLOSE (30)
CLOSE (31)

WRITE (*,*) "Selezionate ",cnt_surf," stazioni superifciali"
WRITE (*,*) "Selezionate ",cnt_temp," stazioni in quota (TEMP)"

STOP

!--------------------------------------------------------------------------
! 3) Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "Errore leggendo sel_staz_calmet.inp"
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo code_parametri.dat"
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo staz_excl.lst"
STOP

9995 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)
STOP


END PROGRAM sel_staz_calmet

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE

!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) 
WRITE (*,*) "sel_staz_calmet.exe [-h] [-c] area"
WRITE (*,*) "area     : codice dell'aera richiesta (in aree_utm.dat)"
WRITE (*,*) " -h      : visualizza questo help"
WRITE (*,*) " -c      : crea file sel_staz_calmet.inp di esempio"
WRITE (*,*) "NB: si appoggia ai files aree_utm.dat (in $MA_UTILS_DAT) e db_anagrafica.dat"
WRITE (*,*) "   (in $HOME_BONAFE/osservazioni/dat)"
WRITE (*,*) 

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_esempio
!
! Scrive un file sel_staz_calmet.inp di esempio
!
IMPLICIT NONE

OPEN (UNIT=20, FILE="sel_staz_calmet.inp", STATUS="REPLACE", &
  FORM="FORMATTED")

!                            1234567890123456789012345678901234567890
WRITE (20,'(2a)')           "7             ! n.ro di reti da consider", &
                            "are"
WRITE (20,'(2a)')           "1,10,11,13,20,21,22     ! lista reti da ", &
                            "considerare (TEMP sempre inclusi)"
WRITE (20,'(2a)')           "30.           ! dist.max dall'area richi", &
                            "esta (km) per selezionare staz al suolo"
WRITE (20,'(2a)')           "60.           ! dist.max dall'area richi", &
                            "esta (km) per selezionare staz in quota"
WRITE (20,'(2a)')           "5000.         ! quota max. (m) per selez", &
                            "ionare staz. al suolo"
WRITE (20,'(2a)')           "-100.         ! quota min. (m) per selez", &
                            "ionare staz. al suolo"
WRITE (20,'(2a)')           "0             ! =1 esclude le staz elenc", &
                            "ate in staz_excl.lst (dir. corrente)"
WRITE (20,'(2a)')           "0             ! nomi reti in output (0: ", &
                            "codici Oracle; 1: sigla, due lettere)"
WRITE (20,'(2a)')           "0             ! nomi stazioni in output ", &
                            "(0: codici utente; 1: codici oracle)"
WRITE (20,'(2a)')           "0             ! tracciato record in outp", &
                            "ut (0: condivisione; 1: estensioni EMR)"
WRITE (20,*)
WRITE (20,'(2a)')           "----------------------------------------", &
                            "--------------------------------------"
WRITE (20,'(a)')            "NOTE:"
WRITE (20,'(2a)')           "- Usando il tracciato esteso, vengono ag", &
                            "giunte (per ora) le coordinate UTM"
CLOSE(20)
RETURN

END SUBROUTINE scrive_esempio

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


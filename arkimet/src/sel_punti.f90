PROGRAM sel_punti
!--------------------------------------------------------------------------
! Data una griglia (geo, rot o utm) e un elenco di coordinate (geo o UTM),
! scrive per ciascuna di queste:
! - le coordinate del punto griglia piu vicino e i suoi indici (I,J,K) 
!   rispetto alla griglia (formato .pts.csv e .ptn)
! - le stazioni meteo SIMC piu' vicine
! - le coordinate e gli indici estremi di una sottogriglia per un ritaglio
!   centrato nelle coordinate richieste (formato .zoom).
! Consente anche di definire griglie rettangolari di punti centrate nelle
! coordinate richieste
!
! Note: 
! - nel programma, le coordinate UTM sono sempre in km
! - tutte le griglie sono considerate piene (i.e. non staggherate); i 
!   grib andranno estratti con opzione destag.
!
! Log versioni:
! V1:      versione iniziale
! V2:      File .ptn allineato alla nuova catena seriet: indici (k,0) 
!          invece che (i,j), aggiunta delle coordinate alle label. 
!          Bug fix (griglia calmet). Se non si specifica -r, le labels
!          dei punti contengono le coor. del punto di Calmet. Se il punto
!          richiesto e' fuori area, calcola comunque quello che si puo'
! V3:      Legge anagrafica da db_anagrafica.dat; gestisce anche griglie
!          diverse da Calmet (Lambo, Gias); ristrutturata interfaccia e ; 
!          help nell'esempio; output .stz contiene tutte le staz piu' 
!          vicine; aggiunta opzione per input da lista di coordinate.
! V4:      Legge il nuovo formato di aree_utm.dat; usa getenv; aggiuinto
!          output con indici i,j; rese facolative le coordiante nelle 
!          labels dei punti; aggiunta lettura lista x,y con labels; 
!          aggiunta lettura coordinate gradi.primi.secondi
! V5:      Aggiunta gestione dei gruppi di punti contigui, con scrittura
!          degli indici estremi delle sottoaree risultanti; eliminate 
!          alcune opzioni obsolete (GIAS, pts_exact, esecuzione senza 
!          sel_punti.inp, label Land/Sea nei dati Calmet operativi),, 
!          resa oblligatoria la presenza delle coordinate nelle labels.
! V6:      Aggiunto output .pts.csv
!
!                                                 V6.0.1, Enrico 05/09/2012
!                               (versioni <3.0: vedi crea_punti_seriet.f90)
!--------------------------------------------------------------------------
USE file_utilities
USE char_utilities
USE seriet_utilities

IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: mxpunti = 500
INTEGER, PARAMETER :: iz0 = 32     ! zona UTM per coord. punti (32=calmet)

! Gestione parametri e assegnazione path espliciti.
INTEGER, PARAMETER :: req_par = 0
INTEGER :: kpar,cnt_par
CHARACTER (LEN=100) :: chpar

! Altre variabili del programma
TYPE (csv_record) :: csvline
REAL,ALLOCATABLE :: xgeo_grd(:,:,:),ygeo_grd(:,:,:)
REAL,ALLOCATABLE :: xutm_grd(:,:,:),yutm_grd(:,:,:)
REAL :: xread(mxpunti),yread(mxpunti)
REAL :: xutm(mxpunti),yutm(mxpunti),xgeo(mxpunti),ygeo(mxpunti)
INTEGER, ALLOCATABLE :: igrd(:,:,:),jgrd(:,:,:),kgrd(:,:,:),zoom_idx(:,:)
INTEGER :: i,j,k,kc,eof,eor,ios,ier,iz,ll
INTEGER :: pts_inp,pts_coor,n_list_stz,grid_coor,npunti
INTEGER :: niexp,njexp,nskipexp
CHARACTER (LEN=80) :: file_out,chrec
CHARACTER (LEN=40) :: progetto,pts_file,grid_area
CHARACTER (LEN=40) :: pts_coor_unit,pts_coor_str,str,str_lon,str_lat
CHARACTER (LEN=30) :: label_out
CHARACTER (LEN=20) :: label(mxpunti)
CHARACTER (LEN=10) :: strx,stry,chfmt
CHARACTER (LEN=9) :: ch9
CHARACTER (LEN=4) :: land_sea(mxpunti),ch4

!--------------------------------------------------------------------------
! 1: Preliminari

! Parametro da riga comandi
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
  END SELECT
ENDDO

! Leggo l'elenco dei dati richiesti da sel_punti.inp

OPEN (UNIT=30, FILE="sel_punti.inp", STATUS="OLD", ACTION="READ",ERR=9996)
READ (30,'(a)',ERR=9999) str

ll = LEN(TRIM(str))
IF (INDEX(str,"!") > 0) ll = MIN(ll,INDEX(str,"!"))
IF (INDEX(str," ") > 0) ll = MIN(ll,INDEX(str," "))
IF (ll > 0) THEN
  progetto = str(1:ll)
ELSE
  progetto = "punti"
  WRITE (*,*) "Nome progetto non specificato, uso default"
ENDIF 

READ (30,*,ERR=9999) pts_inp
READ (30,'(a)',ERR=9999) str
  ll = LEN(TRIM(str))
  IF (INDEX(str,"!") > 0) ll = MIN(ll,INDEX(str,"!"))
  IF (INDEX(str," ") > 0) ll = MIN(ll,INDEX(str," "))
  pts_file = str(1:ll)

READ (30,*,ERR=9999) pts_coor
READ (30,*,ERR=9999) niexp,njexp,nskipexp
READ (30,*,ERR=9999) grid_coor
READ (30,'(a)',ERR=9999) str
READ (30,*,ERR=9999) n_list_stz

  ll = LEN(TRIM(str))
  IF (INDEX(str,"!") > 0) ll = MIN(ll,INDEX(str,"!"))
  IF (INDEX(str," ") > 0) ll = MIN(ll,INDEX(str," "))
  grid_area = lowercase(str(1:ll))

CLOSE(30)

IF ((pts_inp /= 0 .AND. pts_inp /= 1 .AND. pts_inp /= 2 .AND. pts_inp /= 3) .OR. &
    (pts_coor /= 0 .AND. pts_coor /= 1 .AND. pts_coor /= 2) .OR. &
    (niexp < 1 .OR. njexp < 1 .OR. nskipexp < 1) .OR. &
    (grid_coor /= 0 .AND. grid_coor /= 1)) &
  GOTO 9995

IF (pts_inp == 1) pts_coor = 0

! Parametri dipendenti dai dati richiesti; inizializzazioni
IF (pts_coor == 0) THEN
  pts_coor_str = "Geo"
  pts_coor_unit = "gradi.decimi"
ELSE IF (pts_coor == 1) THEN
  pts_coor_str  = "UTM"
  pts_coor_unit = "km"
ELSE IF (pts_coor == 2) THEN
  pts_coor_str  = "Geo"
  pts_coor_unit = "gradi.primi.secondi"
ENDIF

label(:) = ""
land_sea(:) = ""

! Trovo codice EOF
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 2: Leggo le coordinate e le label dei punti richiesti

IF (pts_inp == 0) THEN                   ! input interattivo (default)

2 WRITE (*,*) "N.ro di punti richiesti"
  READ (*,*,ERR=2) npunti

  IF (npunti > mxpunti) THEN
    WRITE (*,*) "Richiesti troppi punti, modificare mxpunti nel sorgente"
    STOP
  ENDIF

  DO k = 1, npunti
3   WRITE (*,'(a,i3,5a)') "Punto ",k," inserire coordinate ",& 
      TRIM(pts_coor_str)," (x,y; in ",TRIM(pts_coor_unit),")"

    IF (pts_coor == 0 .OR. pts_coor == 1) THEN
      READ (*,*,ERR=3) xread(k),yread(k)
    ELSE IF (pts_coor == 2) THEN
      READ (*,*,ERR=3) strx,stry
      CALL gps2gd(strx,stry,xread(k),yread(k),ier)
      IF (ier /= 0) GOTO 3
    ENDIF

    WRITE (*,'(a,i2,5a)') "Punto ",k," inserire label (facoltativa)"
    READ (*,'(a)',IOSTAT=ios) label(k)
    IF (ios /= 0 .OR. label(k) == "") & 
      WRITE (label(k),'(a6,i3.3)') "Punto ",k
  ENDDO

ELSE IF (pts_inp == 1) THEN              ! input da anagrafica regioni

  OPEN (UNIT=10, FILE=pts_file, STATUS="OLD", ACTION="READ", ERR=9998)
  READ (10,*)

  npunti = 0
  DO k= 1,mxpunti
    READ (10,'(a2,a5,1x,a11,9x,2(1x,f8.3))',IOSTAT=ios) &
      label(k)(1:2),label(k)(4:8),label(k)(10:20),yread(k),xread(k)
    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9997
    npunti = npunti + 1
  ENDDO

  WRITE (*,*) "Lette le coordinate di ",npunti," punti"
  IF (npunti == mxpunti) THEN
    READ (10,*,IOSTAT=ios)
    IF (ios /= eof) WRITE (*,*) &
      "Warning: il file ",TRIM(pts_file)," contiene altri dati"
  ENDIF   
  CLOSE (10)

ELSE IF (pts_inp == 2) THEN              ! input da liste vecchio formato

  OPEN (UNIT=10, FILE=pts_file, STATUS="OLD", ACTION="READ", ERR=9998)

  npunti = 0
  DO k= 1,mxpunti
    IF (pts_coor == 0 .OR. pts_coor == 1) THEN
      READ (10,*,IOSTAT=ios) label(k),xread(k),yread(k)
    ELSE IF (pts_coor == 2) THEN
      READ (10,*,IOSTAT=ios) label(k),strx,stry
      CALL gps2gd(strx,stry,xread(k),yread(k),ier)
      IF (ier /= 0) GOTO 9997
    ENDIF

    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9997
    npunti = npunti + 1
  ENDDO

  WRITE (*,*) "Lette le coordinate di ",npunti," punti"
  IF (npunti == mxpunti) THEN
    READ (10,*,IOSTAT=ios)
    IF (ios /= eof) WRITE (*,*) &
      "Warning: il file ",TRIM(pts_file)," contiene altri dati"
  ENDIF   
  CLOSE (10)

ELSE IF (pts_inp == 3) THEN              ! input da lista coord. (x,y)

  OPEN (UNIT=10, FILE=pts_file, STATUS="OLD", ACTION="READ", ERR=9998)

  npunti = 0
  DO k= 1,mxpunti

    IF (pts_coor == 0 .OR. pts_coor == 1) THEN
      READ (10,*,IOSTAT=ios) xread(k),yread(k)
    ELSE IF (pts_coor == 2) THEN
      READ (10,*,IOSTAT=ios) strx,stry
      CALL gps2gd(strx,stry,xread(k),yread(k),ier)
      IF (ier /= 0) GOTO 9997
    ENDIF

    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9997
    WRITE (label(k),'(a,i3.3)') "Punto ",k
    npunti = npunti + 1
  ENDDO

  WRITE (*,*) "Lette le coordinate di ",npunti," punti"
  IF (npunti == mxpunti) THEN
    READ (10,*,IOSTAT=ios)
    IF (ios /= eof) WRITE (*,*) &
      "Warning: il file ",TRIM(pts_file)," contiene altri dati"
  ENDIF   
  CLOSE (10)

ENDIF

ALLOCATE (igrd(npunti,niexp,njexp),jgrd(npunti,niexp,njexp))
ALLOCATE (kgrd(npunti,niexp,njexp),zoom_idx(npunti,4))
ALLOCATE (xutm_grd(npunti,niexp,njexp),yutm_grd(npunti,niexp,njexp))
ALLOCATE (xgeo_grd(npunti,niexp,njexp),ygeo_grd(npunti,niexp,njexp))

!--------------------------------------------------------------------------
! 3: Calcolo le coordinate dei punti richiesti nell'altro sistema di 
!    riferimento (GEO o UTM)

IF (pts_coor == 0 .OR. pts_coor == 2) THEN

  xgeo(1:npunti) = xread(1:npunti)
  ygeo(1:npunti) = yread(1:npunti)
 
  DO k = 1, npunti
    CALL ll2utm(yread(k),xread(k),iz0,xutm(k),yutm(k),iz)
  ENDDO

ELSE IF (pts_coor == 1) THEN

  xutm(1:npunti) = xread(1:npunti)
  yutm(1:npunti) = yread(1:npunti)

  DO k = 1, npunti
    CALL utm2ll(xread(k),yread(k),iz0,.FALSE.,ygeo(k),xgeo(k))
  ENDDO

ENDIF

!--------------------------------------------------------------------------
! 4: Trovo le stazioni piu vicine e scrivo le informazioni relative sul 
!    file .stz

WRITE (*,*) "Trovo stazioni piu' vicine"
CALL find_staz(xutm(1:npunti),yutm(1:npunti),xgeo(1:npunti),ygeo(1:npunti),&
  label(1:npunti),npunti,n_list_stz,progetto,iz0)

!--------------------------------------------------------------------------
! 5: Per ciascuno dei punti richiesti, trovo indici e coordinate dei punti
!    della sottogriglia di estensione niexp,njexp centrata in esso.
!    igrd, jgrd, kgrd(npunti,niexp,njexp): indici
!    xutm_grd, yutm_grd (npunti,niexp,njexp): coordinate UTM
!    xgeo_grd, ygeo_grd (npunti,niexp,njexp): coordinate GEO

WRITE (*,*) "Trovo punti griglia piu' vicini"
DO k = 1, npunti

  IF (grid_coor == 0) THEN
    CALL find_cell_utm(xutm(k),yutm(k),grid_area,niexp,njexp,nskipexp, &
      igrd(k,:,:),jgrd(k,:,:),kgrd(k,:,:),xutm_grd(k,:,:),yutm_grd(k,:,:), &
      zoom_idx(k,:))
    DO i = 1,niexp
    DO j = 1,njexp
      CALL utm2ll(xutm_grd(k,i,j),yutm_grd(k,i,j),iz0,.FALSE., &
                  ygeo_grd(k,i,j),xgeo_grd(k,i,j))
    ENDDO
    ENDDO

  ELSE IF (grid_coor == 1) THEN
    CALL find_cell_geo(xgeo(k),ygeo(k),grid_area,niexp,njexp,nskipexp, &
      igrd(k,:,:),jgrd(k,:,:),kgrd(k,:,:),xgeo_grd(k,:,:),ygeo_grd(k,:,:), &
      zoom_idx(k,:))
    DO i = 1,niexp
    DO j = 1,njexp
      CALL ll2utm(ygeo_grd(k,i,j),xgeo_grd(k,i,j),iz0, &
                  xutm_grd(k,i,j),yutm_grd(k,i,j),iz)
    ENDDO
    ENDDO

  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 6 Scrittura output

! Formato per scrittura coordinate
WRITE (chfmt,'(a,i2,a,i2,a)') "(f",coo_ndec+5,".",coo_ndec,")"

! 6.1) Scrivo le informazioni sui punti griglia del modello nel file .ptn
file_out = TRIM(progetto)//".ptn"
OPEN (UNIT=20, FILE=file_out, STATUS="REPLACE", ACTION="WRITE")

DO k = 1,npunti
DO j = 1,njexp
DO i = 1,niexp

! Indici
  WRITE (ch9,'(i6,1x,i2)') kgrd(k,i,j),0

! Coordinate (con il numero di decimali richiesto in seriet_utilities)
  WRITE (str_lon,chfmt) xgeo_grd(k,i,j)
  WRITE (str_lat,chfmt) ygeo_grd(k,i,j)

! Label
  label_out = ""
  DO kc = 1,LEN(TRIM(label(k)))
    IF (label(k)(kc:kc) == " " .OR. label(k)(kc:kc) == "-" .OR. &
        label(k)(kc:kc) == "!") THEN
      label_out(kc:kc) = "_"
    ELSE
      label_out(kc:kc) = label(k)(kc:kc)
    ENDIF
  ENDDO
  IF (niexp > 1 .OR. njexp > 1) THEN
    ll = LEN(TRIM(label_out))
    WRITE (label_out(ll+1:ll+7),'(a1,2i3.3)') "-",i,j
  ENDIF

! Scrivo su file
  WRITE (20,'(5(a,1x),a)') ch9,"!",TRIM(ADJUSTL(str_lat)), &
    TRIM(ADJUSTL(str_lon)),"!",TRIM(label_out)

ENDDO
ENDDO
ENDDO

CLOSE (20)

! 6.2) Scrivo le informazioni sui punti richiesti nel file .pts.csv
file_out = TRIM(progetto)//".pts.csv"
OPEN (UNIT=20, FILE=file_out, STATUS="REPLACE", ACTION="WRITE")
chrec = ""
CALL build_header("lspts",chrec)
WRITE (20,'(a)') TRIM(chrec)

DO k = 1,npunti
DO j = 1,njexp
DO i = 1,niexp
  CALL init(csvline)

! Coordinate (con il numero di decimali richiesto in seriet_utilities)
  WRITE (str_lon,chfmt) xgeo_grd(k,i,j)
  WRITE (str_lat,chfmt) ygeo_grd(k,i,j)
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_lon)))
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_lat)))

! Label
  IF (niexp > 1 .OR. njexp > 1) THEN
    WRITE (label_out,'(2a,2i3.3)') TRIM(label(k)),"-",i,j
  ELSE
    label_out = label(k)
  ENDIF
  CALL csv_record_addfield(csvline,TRIM(label_out))

! Indici
  CALL csv_record_addfield(csvline,igrd(k,i,j))
  CALL csv_record_addfield(csvline,jgrd(k,i,j))
  CALL csv_record_addfield(csvline,kgrd(k,i,j))

! Scrivo su file
  WRITE (20,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

ENDDO
ENDDO
ENDDO

CLOSE (20)

! 6.3) Scrivo gli estremi delle sottoaree centrate in ciascun punto nel
!      file .zoom
file_out = TRIM(progetto)//".zoom"
OPEN (UNIT=21, FILE=file_out, STATUS="REPLACE", ACTION="WRITE")

WRITE (21,'(4(i4,1x),a,2f8.3,2a)') &
  MINVAL(zoom_idx(1:npunti,1)),MINVAL(zoom_idx(1:npunti,2)), &
  MAXVAL(zoom_idx(1:npunti,3)),MAXVAL(zoom_idx(1:npunti,4))," ! bounding box"

DO k = 1,npunti
  WRITE (21,'(4(i4,1x),a,2f8.3,2a)') &
    zoom_idx(k,1:4)," ! ",ygeo(k),xgeo(k)," ! ",TRIM(label(k))
ENDDO

CLOSE (21)

STOP

!--------------------------------------------------------------------------
! 7: Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore leggendo sel_punti.inp"
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(pts_file)
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(pts_file)," riga ",k
STOP

9996 CONTINUE
WRITE (*,*) "Errore aprendo sel_punti.inp"
WRITE (*,*) "Costruirlo (sel_punti.exe -c) e rilanciare il programma."
STOP

9995 CONTINUE
WRITE (*,*) "Parametri illegali in sel_punti.inp"
STOP

END PROGRAM sel_punti

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE find_cell_utm(xutm,yutm,grid_area,niexp,njexp,nskipexp, &
  igrd,jgrd,kgrd,xutm_grd,yutm_grd,zoom_idx)
!--------------------------------------------------------------------------
! Date le coord. UTM di un punto, trova indici e coord. della sottogriglia
! di ampiezza richiesta centrata nel punto.
!--------------------------------------------------------------------------
IMPLICIT NONE

! Parametri della subroutine
REAL, INTENT(IN) :: xutm,yutm
CHARACTER (LEN=40), INTENT(IN) :: grid_area
INTEGER, INTENT(IN) :: niexp,njexp,nskipexp
!
INTEGER, INTENT(OUT) :: igrd(niexp,njexp),jgrd(niexp,njexp),kgrd(niexp,njexp)
INTEGER, INTENT(OUT) :: zoom_idx(4)
REAL, INTENT(OUT) :: xutm_grd(niexp,njexp),yutm_grd(niexp,njexp)

! Parametri relativi al file "aree_utm.dat"
CHARACTER (LEN=40), PARAMETER :: aree_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: aree_path = "arkimet/dat"
CHARACTER (LEN=40), PARAMETER :: aree_name = "aree_utm.dat"

! Variabili locali
REAL :: dx,dy,xf,yf,xl,yl
REAL :: sgx,sgy,i1,j1
INTEGER :: i,j,ios
INTEGER :: nx,ny,utmz
CHARACTER (LEN=80) :: nfile
CHARACTER (LEN=61) :: ch61
CHARACTER (LEN=40) :: dum_area,ch40

!--------------------------------------------------------------------------
! 1) Leggo gli estremi dell'area da aree_utm.dat

CALL GETENV(aree_env,ch40)
nfile = TRIM(ch40) // "/" // TRIM(aree_path) // "/" // TRIM(aree_name)
OPEN (UNIT=22, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9999)

DO
  READ (22,'(a)',IOSTAT=ios) ch61    
  IF (ios /= 0) THEN
    WRITE (*,*) "Area ",TRIM(grid_area)," non trovata in ",TRIM(aree_name)
    RETURN
  ENDIF

  IF (TRIM(ch61) == "" .OR. ch61(1:1) == "!") CYCLE

  READ (ch61,'(a10,2(1x,i4),4(1x,f8.3),1x,i4)',IOSTAT=ios) &
    dum_area,nx,ny,xf,yf,xl,yl,utmz
  IF (ios /= 0) THEN
    WRITE (*,*) "Record illegale in ",TRIM(aree_name)
    WRITE (*,'(a)') ch61
    RETURN
  ENDIF

  IF (TRIM(dum_area) == TRIM(grid_area)) EXIT
ENDDO
CLOSE(22)

dx = (xl-xf) / REAL(nx-1)
dy = (yl-yf) / REAL(ny-1)

!--------------------------------------------------------------------------
! 2) Trovo le celle

! trovo gli indici del punto SW della sottogriglia
sgx = dx*0.5*REAL((niexp-1)*nskipexp)
i1 =  NINT(((xutm-sgx) - xf) / dx) + 1
sgy = dy*0.5*REAL((njexp-1)*nskipexp)
j1 =  NINT(((yutm-sgy) - yf) / dy) + 1

! Salvo gli estremi della sottoarea per ritaglio grib (W,S,E,N)
zoom_idx = (/i1,j1,i1+niexp-1,j1+njexp-1/)

! 2.1) Ciclo sui punti della sottogriglia 

DO i = 1,niexp
DO j = 1,njexp

! Trovo le coordinate
  igrd(i,j) = i1 + (i-1)*nskipexp
  jgrd(i,j) = j1 + (j-1)*nskipexp
  xutm_grd(i,j) = xf + (igrd(i,j)-1)*dx
  yutm_grd(i,j) = yf + (jgrd(i,j)-1)*dy

! Trovo gli indici k
  IF (igrd(i,j) < 1 .OR. igrd(i,j) > nx .OR. &
      jgrd(i,j) < 1 .OR. jgrd(i,j) > ny) THEN
    WRITE (*,*) "Punto esterno all'area. Indici: ",igrd(i,j),jgrd(i,j)
    kgrd(i,j) = 0
    CYCLE
  ELSE 
    kgrd(i,j) = (jgrd(i,j) - 1) * nx + igrd(i,j)
  ENDIF

ENDDO
ENDDO

RETURN

9999 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)
RETURN

END SUBROUTINE find_cell_utm

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE find_cell_geo(xgeo,ygeo,grid_area,niexp,njexp,nskipexp, &
  igrd,jgrd,kgrd,xgeo_grd,ygeo_grd,zoom_idx)
!--------------------------------------------------------------------------
! Date le coord. GEO di un punto, trova indici e coord. della sottogriglia
! di ampiezza richiesta centrata nel punto.
!--------------------------------------------------------------------------
IMPLICIT NONE

! Parametri della subroutine
REAL, INTENT(IN) :: xgeo,ygeo
CHARACTER (LEN=40), INTENT(IN) :: grid_area
INTEGER, INTENT(IN) :: niexp,njexp,nskipexp
!
INTEGER, INTENT(OUT) :: igrd(niexp,njexp),jgrd(niexp,njexp),kgrd(niexp,njexp)
INTEGER, INTENT(OUT) :: zoom_idx(4)
REAL, INTENT(OUT) :: xgeo_grd(niexp,njexp),ygeo_grd(niexp,njexp)

! Parametri relativi al file "aree_geo.dat"
CHARACTER (LEN=40), PARAMETER :: aree_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: aree_path = "arkimet/dat"
CHARACTER (LEN=40), PARAMETER :: aree_name = "aree_geo.dat"

! variabili locali
REAL :: dx,dy,xf,yf,xl,yl
REAL :: sgx,sgy,i1,j1
REAL :: xrot,yrot,xgeo_rot,ygeo_rot,xgeo_grd_rot,ygeo_grd_rot
INTEGER :: nx,ny,scan(3)
INTEGER :: i,j,ios
CHARACTER (LEN=80) :: nfile
CHARACTER (LEN=78) :: ch78
CHARACTER (LEN=40) :: dum_area,ch40

!--------------------------------------------------------------------------
! 1) Leggo gli estremi dell'area da aree_geo.dat

CALL GETENV(aree_env,ch40)
nfile = TRIM(ch40) // "/" // TRIM(aree_path) // "/" // TRIM(aree_name)
OPEN (UNIT=22, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9999)

DO
  READ (22,'(a)',IOSTAT=ios) ch78    
  IF (ios /= 0) THEN
    WRITE (*,*) "Area ",TRIM(grid_area)," non trovata in ",TRIM(aree_name)
    RETURN
  ENDIF

  IF (TRIM(ch78) == "" .OR. ch78(1:1) == "!") CYCLE

  READ (ch78,'(a10,2(1x,i4),6(1x,f8.3),1x,3i1)',IOSTAT=ios) &
    dum_area,nx,ny,xf,yf,xl,yl,xrot,yrot,scan(1:3)
  IF (ios /= 0) THEN
    WRITE (*,*) "Record illegale in ",TRIM(aree_name)
    WRITE (*,'(a)') ch78
    RETURN
  ENDIF

  IF (TRIM(dum_area) == TRIM(grid_area)) EXIT
ENDDO
CLOSE(22)

dx = (xl-xf) / REAL(nx-1)
dy = (yl-yf) / REAL(ny-1)

!--------------------------------------------------------------------------
! 2) Trovo le celle

! trovo le coordinate relative alla griglia del punto richiesto 
! (i.e. gestisco le griglie ruotate)
IF (xrot /= 0. .OR. yrot /= 0.) THEN
  CALL tll(xgeo,ygeo,xrot,yrot,xgeo_rot,ygeo_rot)
ELSE
  xgeo_rot = xgeo
  ygeo_rot = ygeo
ENDIF

! trovo gli indici del punto SW della sottogriglia
sgx = dx*0.5*REAL((niexp-1)*nskipexp)
i1 =  NINT(((xgeo_rot-sgx) - xf) / dx) + 1
sgy = dy*0.5*REAL((njexp-1)*nskipexp)
j1 =  NINT(((ygeo_rot-sgy) - yf) / dy) + 1

! Salvo gli estremi della sottoarea per ritaglio grib (W,S,E,N)
zoom_idx = (/i1,j1,i1+niexp-1,j1+njexp-1/)

! 2.1) Ciclo sui punti della sottogriglia 

DO i = 1,niexp
DO j = 1,njexp

! Trovo le coordinate
  igrd(i,j) = i1 + (i-1)*nskipexp
  jgrd(i,j) = j1 + (j-1)*nskipexp

  xgeo_grd_rot = xf + (igrd(i,j)-1)*dx
  ygeo_grd_rot = yf + (jgrd(i,j)-1)*dy
  IF (xrot /= 0. .OR. yrot /= 0.) THEN
    CALL rtll(xgeo_grd_rot,ygeo_grd_rot,xrot,yrot, &
              xgeo_grd(i,j),ygeo_grd(i,j))
  ELSE
    xgeo_grd(i,j) = xgeo_grd_rot
    ygeo_grd(i,j) = ygeo_grd_rot
  ENDIF

! Trovo gli indici k
  IF (igrd(i,j) < 1 .OR. igrd(i,j) > nx .OR. &
      jgrd(i,j) < 1 .OR. jgrd(i,j) > ny) THEN
    WRITE (*,*) "Punto esterno all'area. Indici: ",igrd(i,j),jgrd(i,j)
    kgrd(i,j) = 0
    CYCLE
  ELSE IF (ALL( scan == (/0,1,0/) )) THEN
    kgrd(i,j) = (jgrd(i,j)-1)*nx + igrd(i,j)
  ELSE IF (ALL( scan == (/0,0,0/) )) THEN
    kgrd(i,j) = (ny-jgrd(i,j))*nx + igrd(i,j)
  ELSE
    WRITE (*,'(a,3i2,a)') " Scanning ",scan(1:3)," non gestito!"
    STOP
  ENDIF

ENDDO
ENDDO

RETURN

! gestione errori I/O
9999 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)
RETURN

END SUBROUTINE find_cell_geo

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE find_staz(utmx_p,utmy_p,geox_p,geoy_p,label,npunti,n_list_stz, &
                     progetto,iz0)
!--------------------------------------------------------------------------
! Trova nell'anagrafica le n_list_stz stazioni di ciascun tipo piu' 
! vicine ai punti richiesti, e ne scrive codice e distanza sul file .stz
!--------------------------------------------------------------------------
USE seriet_utilities
IMPLICIT NONE

! Argomenti della suroutine
INTEGER, INTENT(IN) :: npunti, n_list_stz
REAL, INTENT(IN) :: utmx_p(npunti), utmy_p(npunti)
REAL, INTENT(IN) :: geox_p(npunti), geoy_p(npunti)
CHARACTER(LEN=20), INTENT(IN) :: label(npunti)
CHARACTER (LEN=40), INTENT(IN) :: progetto
INTEGER, INTENT(IN) :: iz0

! Parametri relativi al file "db_anagrafica.dat"
CHARACTER (LEN=40), PARAMETER :: anag_env = "HOME_BONAFE"
CHARACTER (LEN=40), PARAMETER :: anag_path = "osservazioni/dat"
CHARACTER (LEN=40), PARAMETER :: anag_name = "db_anagrafica.dat"
INTEGER, PARAMETER :: mxstaz = 50000

! Parametri relativi ai tipi di reti
INTEGER, PARAMETER :: nclass = 7                     ! n.ro calssi di staz.
INTEGER, PARAMETER :: nsubclass = 4                  ! n.ro sottoclassi
CHARACTER(LEN=5), PARAMETER :: class_id(nclass) = &  ! nome delle classi
  (/"LOCAL","AGRO ","IDRO ","SYNOP","TEMP ","METAR","DBALL"/)
INTEGER :: subclass_id(nclass,nsubclass)   !lista reti di ogni sottoclasse 

! Variabili locali
REAL,ALLOCATABLE :: dist_staz(:,:)
REAL :: utmx_s(mxstaz),utmy_s(mxstaz),lon,lat,dd
INTEGER, ALLOCATABLE :: prstaz_near(:,:)
INTEGER :: net(mxstaz),id(mxstaz),net_dum,usr_dum
INTEGER :: k,kpt,kst,kcl,kdis,cclass,idum
INTEGER :: ios,eof,eor,nstaz
CHARACTER (LEN=200) :: chrec
CHARACTER (LEN=80) :: file_out,nfile
CHARACTER (LEN=40) :: ch40
CHARACTER (LEN=20) :: nome(mxstaz),nome_dum,nomi_out(nclass)

!--------------------------------------------------------------------------
! 1) Preliminari: dati costanti, header

! codici dell'anagrafica corrispondenti alle staz. di ciascun tipo
subclass_id(1,:) = (/11, 0, 0, 0/) ! locali
subclass_id(2,:) = (/13, 0, 0, 0/) ! agro
subclass_id(3,:) = (/20,21,22, 0/) ! idro
subclass_id(4,:) = (/ 1, 0, 0, 0/) ! synop
subclass_id(5,:) = (/ 2, 3, 4, 5/) ! temp
subclass_id(6,:) = (/10, 0, 0, 0/) ! metar
subclass_id(7,:) = (/15, 0, 0, 0/) ! dball

ALLOCATE (prstaz_near(n_list_stz,nclass))
ALLOCATE (dist_staz(n_list_stz,nclass))

CALL get_eof_eor(eof, eor)

!--------------------------------------------------------------------------
! 1) Leggo anagrafica e calcolo coord UTM di ogni stazione

CALL GETENV(anag_env,ch40)
nfile = TRIM(ch40) // "/" // TRIM(anag_path) // "/" // TRIM(anag_name)

OPEN (UNIT=40, FILE=nfile, STATUS="OLD",ACTION="READ", ERR=9999)
READ (40,*,ERR=9998)
READ (40,*,ERR=9998)
READ (40,*,ERR=9998)

nstaz = 0
DO 

  READ (40,'(a)',IOSTAT=ios) chrec
  IF (ios == eof .OR. INDEX(chrec,"rows selected") /= 0) EXIT
  IF (chrec == "") CYCLE

  READ (chrec,'(i4,i7,7x,f9.3,f8.3,6x,1x,a20)',IOSTAT=ios) &
    net_dum,usr_dum,lon,lat,nome_dum
  IF (ios /= 0) THEN
    GOTO 9998
  ELSE IF (lon == 0. .AND. lat == 0.) THEN
    CYCLE
  ENDIF

  nstaz = nstaz + 1
  IF (nstaz > mxstaz) THEN
    WRITE (*,*) "Troppe stazioni in anagrafica!"
    RETURN
  ENDIF
 
  net(nstaz) = net_dum
  id(nstaz) = usr_dum
  nome(nstaz) = nome_dum
  CALL ll2utm(lat,lon,iz0,utmx_s(nstaz),utmy_s(nstaz),idum)

ENDDO

CLOSE (40)

!--------------------------------------------------------------------------
! 2) Apro il file di output e scrivo header generale (lista coord. punti)

file_out = TRIM(progetto)//".stz"
OPEN (UNIT=41, FILE=file_out, STATUS="REPLACE", ACTION="WRITE")

WRITE (41,'(a20,4(1x,a10))') "punti:              ", &
  "UTMx rich.","UTMy rich.","GEOx rich.","GEOy rich."
DO kpt = 1,npunti
  WRITE (41,'(a20,4(1x,f10.3))') &
    label(kpt),utmx_p(kpt),utmy_p(kpt),geox_p(kpt),geoy_p(kpt)
ENDDO

!--------------------------------------------------------------------------
! 3) Trovo le stazioni piu' vicine e le scrivo

DO kpt = 1,npunti

  dist_staz(:,:) = 1.E10
  prstaz_near(:,:) = 0

  DO kst = 1,nstaz

!   Trovo il tipo della stazione
    cclass = 0
    DO kcl = 1,nclass
      IF ( ANY(net(kst) == subclass_id(kcl,:)) ) cclass = kcl
    ENDDO 
    IF (cclass == 0) CYCLE

!   Calcolo distanza punto-stazione
    dd = SQRT ( (utmx_s(kst) - utmx_p(kpt))**2 + &
                (utmy_s(kst) - utmy_p(kpt))**2 )

!   Aggiorno la lista delle stazioni piu' vicine di questo tipo
    DO kdis = 1, n_list_stz
    IF (dd < dist_staz(kdis,cclass)) THEN
      dist_staz(kdis+1:n_list_stz,cclass) = &
        dist_staz(kdis:n_list_stz-1,cclass) 
      dist_staz(kdis,cclass) = dd
      prstaz_near(kdis+1:n_list_stz,cclass) = & 
        prstaz_near(kdis:n_list_stz-1,cclass) 
      prstaz_near(kdis,cclass) = kst
      EXIT
    ENDIF
    ENDDO

  ENDDO

! Scrivo l'elenco di codici e distanze delle staz. piu' vicine 
  WRITE (41,*)
  WRITE (41,'(2a)') "punto: ",label(kpt)
  WRITE (41,'(5(a15,a5,a1,4x))') &
    ("dist. id/nome (",class_id(kcl),")",kcl=1,5)
  DO kdis = 1, n_list_stz

    WRITE (41,'(5(f5.1,1x,i5.5,1x,a10,3x))') &
      (dist_staz(kdis,kcl),id(prstaz_near(kdis,kcl)), &
       nome(prstaz_near(kdis,kcl))(1:10), kcl=1,5)
  ENDDO

ENDDO
CLOSE (41)

RETURN

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "File ",TRIM(nfile)," non trovato"
CLOSE (41)
RETURN

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)
CLOSE (41)
RETURN

END SUBROUTINE find_staz

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

SUBROUTINE scrive_esempio
!
! Scrive un file sel_punti.inp di esempio
!
IMPLICIT NONE

OPEN (UNIT=20, FILE="sel_punti.inp", STATUS="REPLACE", &
  FORM="FORMATTED")

WRITE (20,'(a)')            "punti      ! nome del progetto"
WRITE (20,'(a)')            "0          ! tipo input per lista punti "
WRITE (20,'(a)')            "           ! nome file lista punti"
WRITE (20,'(a)')            "0          ! tipo coordinate punti"
WRITE (20,'(a)')            "1 1 1      ! parametri sottogriglia (Ni,Nj,Nskip)"
WRITE (20,'(a)')            "1          ! tipo di grigliato modello"
WRITE (20,'(a)')            "lamaz      ! nome area del modello"
WRITE (20,'(2a)')           "5          ! n.ro di stazioni piu' vicin", &
                            "e"
WRITE (20,*)
!                            1234567890123456789012345678901234567890
WRITE (20,'(a)')            "DOCUMENTAZIONE PARAMETRI:"
WRITE (20,*)
WRITE (20,'(2a)')           "- nome progetto: usato solo per costruir", &
                            "e i nomi dei files di output" 
WRITE (20,'(2a)')           "- input punti:   0 = da tastiera; 1 = da", &
                            " anagrafica in formato condivisione"
WRITE (20,'(2a)')           "                 2 = lista 'label',x,y; ", &
                            "3 = lista x,y"
WRITE (20,'(2a)')           "- file lista:    non usato se input punt", &
                            "i e' da tastiera"
WRITE (20,'(2a)')           "- tipo coor.pti: 0 = gradi.decimi; 1 = U", &
                            "TM (km); 2 = gradi.primi.secondi"
WRITE (20,'(2a)')           "- param. sottogriglia (NX,NY,NSKIP): per", &
                            " gestire estrazioni di punti contigui."
WRITE (20,'(2a)')           "                 Permette di selezionare", &
                            " una sottogriglia NXxNY centrata in cia-"
WRITE (20,'(2a)')           "                 scuno dei punti richies", &
                            "ti. (1,1,1) per selez. un singolo punto"
WRITE (20,'(2a)')           "                 Con NSKIP>1 seleziona d", &
                            "al grigliato modello un punto ogni NSKIP"
WRITE (20,*)
WRITE (20,'(2a)')           "- tipo griglia:  0=UTM (Calmet); 1=Geo (", &
                            "modelli)"
WRITE (20,'(2a)')           "- nome area:     nome del grigliato ric" , &
                            "hiesto; deve essere contenuto in "
WRITE (20,'(2a)')           "                 aree_utm.dat o aree_geo", &
                            ".dat"
WRITE (20,*)
WRITE (20,'(2a)')           "- n.ro stazioni: lunghezza delle liste d", &
                            "i stazioni piu' vicine calcolate, per"
WRITE (20,'(2a)')           "                 ciascun punto e per cia", &
                            "scuna rete"
CLOSE(20)
RETURN

END SUBROUTINE scrive_esempio

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE

!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) 
WRITE (*,*) "sel_punti.exe [-h] [-c]"
WRITE (*,*) " -h      : visualizza questo help"
WRITE (*,*) " -c      : crea file sel_punti.inp di esempio"
WRITE (*,*) "NB: legge le opzioni da sel_punti.inp"
WRITE (*,*) "    Si appoggia ai files aree_utm.dat e aree_geo.dat"
WRITE (*,*) "    Devono essere definite le variabili d'ambiente HOME_MINGUZZI e HOME_BONAFE"

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! Subroutnies di libreria per conversioni di coordinate
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!----------------------------------------------------------------------
SUBROUTINE ll2utm(rlat,rlon,iz0,x,y,iz)
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

IMPLICIT NONE

! Argomenti delle subroutine
REAL, INTENT(IN) :: rlat, rlon
INTEGER, INTENT(IN) :: iz0
REAL, INTENT(OUT) :: x,y
INTEGER, INTENT(OUT) :: iz

! Parametri
REAL, PARAMETER :: k0 = 0.9996
REAL, PARAMETER :: a = 6378206.4
REAL, PARAMETER :: e2 = 0.00676866
REAL, PARAMETER :: ep2 = 0.0068148
REAL, PARAMETER :: false_e = 500000.0
!REAL, PARAMETER :: dtr = 3.141592654/180.0
REAL, PARAMETER :: dtr = 3.141592/180.0

! Variabili locali
REAL :: dl,p,sinp,n,tanp,t,cosp,c,a1,m,a2,a3,a4,a5,a6,t2,false_n

!----------------------------------------------------------------------

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
!tanp = sin(p)/cos(p)
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

RETURN
END SUBROUTINE ll2utm

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
      sinp1 = sin(p1)
      cosp1 = cos(p1)
      tanp1 = sinp1/cosp1
      C1 = ep2*cosp1**2
      C2 = C1**2
      T1 = tanp1**2
      T2 = T1**2
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





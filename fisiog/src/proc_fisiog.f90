!==========================================================================
! Programma per costurire il file dei dati fisiografici per i run di Calmet
! (geo.dat) e Chimere (LANDUSE). Unifica i programmi landuse_corine.f90 e 
! crea_geo_calmet.f90. Gestisce griglie UTM, geografiche e ruotate. 
!
!--------------------------------------------------------------------------
! Dataset gestiti ($HOME_MINGUZZI/fisiog/dataset; alcuni files sono 
!   normalmente compressi, e deveono essere qunzippati prima di lanciare 
!   il programma):
!
! DEM:
! - USGS: (EROS) orografia globale, lat-lon, risoluzione 30" (1 km); nomi 
!         files E020N40.DEM e similari.
! - NASA: orografia globale, lat-lon, risoluzione 3" (90 m), divisa in
!         elementi di 1 grado x 1 grado (nomi files: N??E???.hgt).
!         Formato: INTEGR*2, big-endian, punti ordinati dall'angolo NW,
!         il nome file individua l'angolo SE.
!         Dati disponibli su: ftp://e0srp01u.ecs.nasa.gov/srtm/version2/SRTM3
!
! LAND USE:
! - BATS: dataset a 30" dal sito USGS, con classificazione BATS. File
!         unico: gbats2_0ll.img              *** ATTUALMENTE NON ATTIVO ***
! - USGS: dataset a 30" dal sito USGS, con classificazione USGS. File
!         unico: gusgs2_0ll.img
! - JRC:                                     *** ATTUALMENTE NON ATTIVO ***
! - Corine 2000: dataset europeo Corine da EEA. Proiezione Lambert 
!         Equivalent, ritagliato sull'area italiana e riscritto in formato
!         compatto (vedi sub. read_corine_utm).
!         Risoluzione: 100m. File unico: LAND_CLC00ita
!         NB: le coordinate del dataset sono sbagliate di 1-2 km, per cui
!         e' stata introdotta una correzione spannometrica. L'errore finale
!         dovrebbe essere di un centinaio di metri.
!
! Altri files di input:
! - proc_fisiog.inp: opzioni utente
! - $DATASET2fis_$STAG.dat, $DATASET2fis_year.dat: 
!   file annuale oppure 4 files stagionali per ogni dataset di land use; 
!   contengono i valori dei campi fisiografici per Calmet da attribuire a
!   ciascuna classe LU dei dataset.
! - LAND_$DATASET_AGGREGATION:
!   contengono i fattori di aggregazione per passare dalle classi LU dei 
!   dataset a quelle richieste da Chimere
!
! Files di output:
! - geo_AREA_STAG.dat:   1 file annuale o 4 files stagionali, pronti per 
!                        l'input a Calmet. 
! 
!--------------------------------------------------------------------------
! Note: 
! Benche' i campi di Calmet partano dall'angolo SW, il geo.dat e' scritto 
!   come una mappa, a partire da NW. 
! In tutti gli array:
!   l'ordine dei punti e' a partire da SW;
!   l'oridine delle classi e' definito da idcl_DSET (modulo local)
! Ordine dei campi: 
!   in geo.dat:           LU, Orog, Z0, Alb, Bowen, Soil HF, Antr.HF, LAI
!   nell' array fisiog:   LU, Z0, Alb, Bowen, Soil HF, Antr.HF, LAI, Orog
!   negli array fis_????: LU, Z0, Alb, Bowen, Soil HF, Antr.HF, LAI
! Coordinate estreme dell'area di output: 
!   come in Calmet, le coord. del punto iniziale si riferiscono all'angolo
!   SW della prima cella; quando bisogna usare il grigliato dei centri 
!   cella (attribuzione punti da USGS, scrittura GRIB) occorre aggiungere 
!   mezzo passo griglia.
! Quando calcolo fisiog, dovrei rinormalizzare dividendo per la somma delle
!   frazioni effettivamente impiegate... vabbe' che l'errore e' comunque
!   < eps_fr, pero' starei piu' tranquillo...
! Passo griglia esplicito nell'output grib ("direction increments given"):
!   - se lo metto ci possono essere errori di troncamento (gravi se il 
!     passo griglia e' piccolo, o se la griglia ha molti punti)
!   - se non lo metto, per visulaizzare con GRADS bisogna modificare a mano
!     il file .ctl
!
! DA COMPLETARE:
! - verificare ordine punti landuse Chimere
! - attivare dataset BATS (mancano i files di appoggio *2fis e AGGREGATION)
! - attivare dataset JRC (manca la subr. read_jrs + files *2fis)
! - volendo la correzione alle coordinate Lambert potrebbe essere affinata,
!   rendendo piu' precisa l'analisi nei punti di test (vedi subdir Lambert) 
!
!                                         Versione 2.4.4, Enrico 31/10/2012 
!==========================================================================

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

MODULE local
!
! Modulo generico per il trasferimento dati all'interno del programma
!
IMPLICIT NONE

! Kind per doppia precisione
INTEGER, PARAMETER :: kdble = SELECTED_REAL_KIND(10,9)

! Parametri del grigliato di output (x1_out e y1_out si riferiscono 
! all'angolo SW della cella 1,1)
INTEGER :: ni_out,nj_out,np_out
REAL :: dx_out,dy_out
REAL :: x1_out,y1_out,xrot_out,yrot_out
INTEGER :: utmz_out,int_orog
CHARACTER (LEN=10) :: area_out
CHARACTER (LEN=1) :: proj_out
REAL :: lat1_out,lat2_out,lon1_out,lon2_out,x2_out,y2_out

! Parametri realtivi al dataset di land-use BATS (by USGS)
! BATS non ha la classe urbana, quinid non definisco urb_cl_bats
INTEGER, PARAMETER :: ni_bats = 43200, nj_bats = 21600 !n.ro di celle
INTEGER, PARAMETER :: ncl_bats = 20                    !n.ro classi
INTEGER, PARAMETER :: idcl_bats(ncl_bats) = &          !lista id classi
  (/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20/)
CHARACTER (LEN=40), PARAMETER :: bats_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: bats_path = "fisiog/dataset/lu_bats"
CHARACTER (LEN=40), PARAMETER :: bats_name = "gbats2_0ll.img"

! Parametri realtivi al dataset di land-use USGS
INTEGER, PARAMETER :: ni_usgs = 43200, nj_usgs = 21600 !n.ro di celle
INTEGER, PARAMETER :: ncl_usgs = 26                    !n.ro classi
INTEGER, PARAMETER :: idcl_usgs(ncl_usgs) = &          !lista id classi
  (/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,100/)
INTEGER, PARAMETER :: urb_cl_usgs = 1                  !indice classe urb.
CHARACTER (LEN=40), PARAMETER :: usgs_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: usgs_path = "fisiog/dataset/lu_usgs"
CHARACTER (LEN=40), PARAMETER :: usgs_name = "gusgs2_0ll.img"

! Parametri realtivi al dataset di land-use Corine 2000
INTEGER, PARAMETER :: ncl_cori = 47                    !n.ro classi
INTEGER, PARAMETER :: idcl_cori(ncl_cori) = (/ &       !lista id classi
  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21, &
  22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,48,49,50/)
INTEGER, PARAMETER :: urb_cl_cori = 1                  !indice classe urb.
CHARACTER (LEN=40), PARAMETER :: corine_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: corine_path = "fisiog/dataset/lu_corine2000"

! Parametri realtivi al dataset di orografia USGS
! (vedi anche subr. read_orog_usgs)
CHARACTER (LEN=40), PARAMETER :: usgs_orog_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: usgs_orog_path = "fisiog/dataset/dem_usgs"

! Parametri realtivi al dataset di orografia NASA
! (vedi anche subr. read_orog_nasa)
CHARACTER (LEN=40), PARAMETER :: nasa_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: nasa_path = "fisiog/dataset/dem_nasa"

! Lista classi di LU Calmet (per classe prevalente e log statistico)
INTEGER, PARAMETER :: ncl_calmet = 12                  !n.ro classi
INTEGER, PARAMETER :: idcl_calmet(ncl_calmet) = &      !lista id classi
  (/-20,10,20,30,40,51,55,61,62,70,80,90/)
REAL :: fr_lu_calmet(ncl_calmet)
INTEGER :: natt(ncl_calmet)

! Lista classi di LU Chimere (per log statistico) 
CHARACTER (LEN=5), PARAMETER :: idcl_chimere(9) = &
  (/"CROP","GRAS","BARR","IWAT","URBA","SHRB","CONI","DECI","OCEA"/)

! Directory dei files per attribuzione dei parametri fisiografici Calmet 
! alle classi di LU (files $DATASET2fis_$STA.dat)
CHARACTER (LEN=40), PARAMETER :: cal_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: cal_path = "fisiog/dat"

! Directory dei files per aggregazioni delle classi LU dei dataset a quelle
! richieste da Chimere (files LAND_$DATASET_AGGREGATION)
CHARACTER (LEN=40), PARAMETER :: chi_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: chi_path = "fisiog/dat"

! Codifica grib dei campi fisiografici
INTEGER, PARAMETER :: code_grib_calmet(8) = &
  (/121,123,124,125,126,127,128,122/)

! Dati mancanti - soglie
REAL :: fr_mis = -9.99    ! fraz. di cella con un certo tipo di LU
REAL :: rmis = -999.      ! campi fisiografici, ecc.
REAL :: eps_fr = 0.01     ! Max scarto ammesso in una cella tra la somma 
                          ! delle frazioni di copertura LU e 1

! Gestione parametri e assegnazione path espliciti.
INTEGER, PARAMETER :: req_par = 0
INTEGER :: kpar,cnt_par
CHARACTER (LEN=100) :: chpar

END MODULE local

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

PROGRAM proc_fisiog

USE local
IMPLICIT NONE

! Frazioni di ciascun tipo di suolo sul grigliato di output.
! NB. sono allocati (nclassi, npunti), con punti ordinati a partire da SW 
! (34/12). Gli indici i_out, j_out e k_out seguono questa convenzione.
REAL, ALLOCATABLE :: fr_bats(:,:),fr_usgs(:,:),fr_jrc(:,:),fr_cori(:,:)
REAL, ALLOCATABLE :: orog_usgs(:),orog_nasa(:)

! Parametri fisiografici corrispondenti alle varie classi di Land-Use
REAL :: fis_usgs(ncl_usgs,7),fis_bats(ncl_bats,7),fis_cori(ncl_cori,7)

! Fattori di aggregazione per passare dalle classi LU dei dataset a quelle
! di Chimere
REAL :: agg_usgs(ncl_usgs,9),agg_bats(ncl_bats,9),agg_cori(ncl_cori,9)

! Parametri fisiografici per Calmet (npunti,8); orografia = 8
REAL, ALLOCATABLE :: fisiog_cal(:,:)

! Frazioni di LU per Chimere (npunti,9), classi lu prevalenti (npunti)
REAL, ALLOCATABLE :: lu_chi(:,:)
INTEGER, ALLOCATABLE :: cl_prev_chi(:)

! Orografia sola (npunti)
REAL, ALLOCATABLE :: orog(:)

! Altre variabili del programma
REAL :: lat,lon,xx,yy
REAL :: fis(7),x1c,y1c,x2c,y2c
REAL :: frurb_bats,frurb_usgs,frurb_jrc,frurb_cori
INTEGER :: iu_grb,kret,ios
INTEGER :: ldem_usgs,ldem_nasa,lbats,lusgs,ljrc,lcori,lurb_fis,grb_dir_inc
INTEGER :: lu_stag,lorogrb,lcaldat,lcalgrb,lchidat,lchigrb
INTEGER :: i,j,k,k1,k2,istag,istag1,istag2,kcl,kcl2,idcl,kpt,kfis,klu
INTEGER :: clprev(1),clprev2(1),nok,cntf
INTEGER :: cnt_dem_usgs,cnt_dem_nasa,cnt_dem_mis
CHARACTER (LEN=100) :: nfile
CHARACTER (LEN=40) :: chfmt1,chfmt2,ch40
CHARACTER (LEN=3), PARAMETER :: id_stag(0:4) = &
  (/"yea","mam","jja","son","djf"/)
CHARACTER (LEN=3) :: ch3
LOGICAL :: ok,req_lu

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
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
  END SELECT
ENDDO

!--------------------------------------------------------------------------
! 1.2 Leggo proc_fisiog.inp

! defulat (per compatibilita' con verisone vecchia di proc_fisiog.inp)
grb_dir_inc = 1

OPEN (UNIT=21, FILE="proc_fisiog.inp", STATUS="OLD", &
  ACTION="READ", ERR=9999)
  READ (21,*) 
  READ (21,'(a)') ch3
  READ (21,*) ni_out
  READ (21,*) nj_out
  READ (21,*) x1c
  READ (21,*) y1c
  READ (21,*) x2c
  READ (21,*) y2c
  READ (21,*) utmz_out
  READ (21,*) xrot_out
  READ (21,*) yrot_out
  READ (21,'(a)') area_out
  READ (21,*) 
  READ (21,*) 
  READ (21,*) ldem_usgs
  READ (21,*) ldem_nasa
  READ (21,*) lbats
  READ (21,*) lusgs
  READ (21,*) ljrc
  READ (21,*) lcori
  READ (21,*) 
  READ (21,*) 
  READ (21,*) frurb_bats
  READ (21,*) frurb_usgs
  READ (21,*) frurb_jrc
  READ (21,*) frurb_cori
  READ (21,*) lurb_fis
  READ (21,*) 
  READ (21,*) 
  READ (21,*) int_orog
  READ (21,*) lu_stag
  READ (21,*) lorogrb
  READ (21,*) lcaldat
  READ (21,*) lcalgrb
  READ (21,*) lchidat
  READ (21,*) lchigrb
  READ (21,*,IOSTAT=ios) grb_dir_inc
CLOSE(21)

np_out = ni_out * nj_out
IF (ch3 == "UTM" .OR. ch3 == "utm") THEN
  proj_out = "U"
ELSE IF (ch3 == "GEO" .OR. ch3 == "geo") THEN
  IF (xrot_out == 0. .AND. yrot_out == 0.) THEN
    proj_out = "G"
  ELSE
    proj_out = "R"
  ENDIF
ELSE
  GOTO 9998
ENDIF

!--------------------------------------------------------------------------
! 1.3 Controlli e calcoli dipendenti dai dati richiesti

dx_out = (x2c - x1c) / REAL (ni_out - 1)
dy_out = (y2c - y1c) / REAL (nj_out - 1)
x1_out = x1c - dx_out/2.
y1_out = y1c - dy_out/2.
x2_out = x2c + dx_out/2.
y2_out = y2c + dy_out/2.

! Controllo che i parametri di input siano legali
IF (ni_out <= 0 .OR. nj_out <= 0 .OR. dx_out <=0 .OR. dy_out <= 0. .OR. &
    (lu_stag/=1 .AND. lu_stag/=2) .OR. (int_orog/=0 .AND. int_orog/=1)) THEN
  WRITE (*,*) "Errore nei parametri di input (proc_fisiog.inp)"
  WRITE (*,*) "ni_out,nj_out,dx_out,dy_out,lu_stag,int_orog ",ni_out,nj_out,dx_out,dy_out,lu_stag,int_orog
  STOP
ENDIF

IF (dx_out /= dy_out) THEN
  IF (ABS((dx_out - dy_out)/(dx_out+dy_out)) < 0.0005) THEN
    WRITE (*,*) "Warning: passi griglia X e Y leggermente diversi, messi uguali"
    dy_out = dx_out
  ELSE
    WRITE (*,*) "Warning: passi griglia X e Y diversi"
    IF (lcaldat /= 0) THEN
      WRITE (*,*) "Il file geo.dat non sara' creato"
      lcaldat = 0
    ENDIF
  ENDIF
ENDIF

! Alloco gli array per i campi finali
IF (lcaldat /= 0 .OR. lcalgrb /= 0) ALLOCATE (fisiog_cal(np_out,8))    
IF (lchidat /= 0 .OR. lchigrb /= 0) &
  ALLOCATE (lu_chi(np_out,9),cl_prev_chi(np_out))
IF (lorogrb /= 0) ALLOCATE (orog(np_out))    

! Costruisco formati per output geo.dat
IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
  WRITE (chfmt1,'(a,i4,a)') "(",ni_out,"i4)"
  WRITE (chfmt2,'(a,i4,a)') "(",ni_out,"f10.3)"
ENDIF

! Stagioni da elaborare
IF (lu_stag == 1) THEN
  istag1 = 0
  istag2 = 0
ELSE 
  istag1 = 1
  istag2 = 4
ENDIF

! Se richiesto output grib, disabilito i controlli sui parametri GRIBEX
IF (lcalgrb /= 0 .OR. lchigrb /= 0 .OR. lorogrb /= 0) THEN
  CALL grsvck(0)
ENDIF

! Trovo le coordinate geografiche estreme dell'area richiesta

IF (proj_out == "U" .OR. proj_out == "R") THEN
  lat1_out = HUGE(0.)
  lat2_out = -HUGE(0.)
  lon1_out = HUGE(0.)
  lon2_out = -HUGE(0.)
  
  DO i = 1, ni_out+1
  DO j = 1, nj_out+1
    xx = x1_out + (i-1) * dx_out
    yy = y1_out + (j-1) * dy_out
    IF (proj_out == "U") THEN
      CALL utm2ll(xx,yy,utmz_out,.FALSE.,lat,lon)
    ELSE IF (proj_out == "R") THEN
      CALL rtll(xx,yy,xrot_out,yrot_out,lon,lat)
    ENDIF  
    lat1_out = MIN(lat1_out,lat)
    lat2_out = MAX(lat2_out,lat)
    lon1_out = MIN(lon1_out,lon)
    lon2_out = MAX(lon2_out,lon)
  ENDDO
  ENDDO

ELSE IF (proj_out == "G" ) THEN
  lat1_out = y1_out
  lat2_out = y2_out
  lon1_out = x1_out
  lon2_out = x2_out

ENDIF

WRITE (*,'(3a,2(1x,e11.4))') "Area richiesta: proj: ",proj_out, &
  " passo griglia (dx,dy): ",dx_out,dy_out
WRITE (*,'(a,4(1x,f8.3))') "Coordinate geogr. estreme: ", &
  lon1_out,lat1_out,lon2_out,lat2_out

!==========================================================================
! 2) Estraggo dai dataset richiesti l'orografia media e la frazione per 
!    ciascun tipo di land-cover, relative a ciascuna cella del grigliato di
!    di output richiesto.

! 2.1 Land-use BATS (by USGS/EROS)
IF (lbats /= 0) THEN
  ALLOCATE (fr_bats(ncl_bats,np_out))
  CALL read_bats(fr_bats)
ENDIF

! 2.2 Land-use USGS (EROS)
IF (lusgs /= 0) THEN
  ALLOCATE (fr_usgs(ncl_usgs,np_out))
  CALL read_usgs(fr_usgs)
ENDIF

! 2.3 Land-use JRC
IF (ljrc /= 0) THEN
!  ALLOCATE (fr_jrc(ncl_jrc,np_out))
!  CALL read_jrc(fr_usgs)
ENDIF

! 2.4 Land-use Corine
IF (lcori /= 0) THEN
  ALLOCATE (fr_cori(ncl_cori,np_out))
  CALL read_corine(fr_cori)
ENDIF

! 2.5 Orografia USGS (EROS)
IF (ldem_usgs /= 0) THEN
  ALLOCATE (orog_usgs(np_out))
  CALL read_orog_usgs(orog_usgs)
ENDIF

! 2.6 Orografia NASA
IF (ldem_nasa /= 0) THEN
  ALLOCATE (orog_nasa(np_out))
  CALL read_orog_nasa(orog_nasa)
ENDIF

!==========================================================================
! 3) Per i dataset di land-use da utilizzare e per ciascuna stagione, leggo:
!    - i parametri fisiografici corrispondenti a ciascuna classe (per Calmet)
!    - i fattori di aggregazione dal dataset alle classi Chimere (per Chimere)

! Apro il ciclo sulle stagioni
stagioni: DO istag = istag1,istag2
  WRITE (*,*)
  WRITE (*,'(2a)') "Elaboro la stagione: ",id_stag(istag)

!--------------------------------------------------------------------------
! 3.1 Land-use BATS (by EROS)

  IF (lbats /= 0) THEN

    IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
      CALL GETENV(cal_env,ch40)
      WRITE (nfile, '(8a)') TRIM(ch40),"/",TRIM(cal_path),"/", &
        "bats","2fis_",id_stag(istag),".txt"
      OPEN (UNIT=40, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9997)
      READ(40,*,ERR=9997)
      READ(40,*,ERR=9997)
  
      fis_bats = rmis
      DO kcl = 1, ncl_bats
        READ(40,*,ERR=9997) idcl,fis(:)
        DO kcl2 = 1, ncl_bats
          IF (idcl == idcl_bats(kcl2)) EXIT
        ENDDO
        IF (kcl2 > ncl_bats) THEN
          WRITE (*,'(3a,i5)') "Warning: Classe illegale in ",TRIM(nfile), &
            ", ",idcl
        ELSE
          fis_bats(kcl2,:) = fis(:)
        ENDIF
      ENDDO

      CLOSE(40)  
      IF (ANY(fis_bats == rmis)) WRITE (*,*) &
        "Warning: classi non gestite in ",TRIM(nfile)
    ENDIF

    IF (lchidat /= 0 .OR. lchigrb /= 0) THEN
      CALL GETENV(chi_env,ch40)
      WRITE (nfile, '(5a)') TRIM(ch40),"/",TRIM(chi_path),"/", &
        "LAND_bats_AGGREGATION"
      OPEN (UNIT=40, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9997)
      READ(40,*,ERR=9997)
  
      agg_bats = rmis
      DO kcl = 1, ncl_bats
        READ(40,*,ERR=9997) agg_bats(kcl,1:9)
      ENDDO

      CLOSE(40)
    ENDIF

  ENDIF

!--------------------------------------------------------------------------
! 3.2 Land-use USGS (by EROS)

  IF (lusgs /= 0) THEN

    IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
      CALL GETENV(cal_env,ch40)
      WRITE (nfile, '(8a)') TRIM(ch40),"/",TRIM(cal_path),"/", &
        "usgs","2fis_",id_stag(istag),".txt"
      OPEN (UNIT=40, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9997)
      READ(40,*,ERR=9997)
      READ(40,*,ERR=9997)
  
      fis_usgs = rmis
      DO kcl = 1, ncl_usgs
        READ(40,*,ERR=9997) idcl,fis(:)
        DO kcl2 = 1, ncl_usgs
          IF (idcl == idcl_usgs(kcl2)) EXIT
        ENDDO
        IF (kcl2 > ncl_usgs) THEN
          WRITE (*,'(3a,i5)') "Warning: Classe illegale in ",TRIM(nfile), &
            ", ",idcl
        ELSE
          fis_usgs(kcl2,:) = fis(:)
        ENDIF
      ENDDO

      CLOSE(40)
      IF (ANY(fis_usgs == rmis)) WRITE (*,*) &
        "Warning: classi non gestite in ",TRIM(nfile)
    ENDIF

    IF (lchidat /= 0 .OR. lchigrb /= 0) THEN
      CALL GETENV(chi_env,ch40)
      WRITE (nfile, '(5a)') TRIM(ch40),"/",TRIM(chi_path),"/", &
        "LAND_usgs_AGGREGATION"
      OPEN (UNIT=40, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9997)
      READ(40,*,ERR=9997)
  
      agg_usgs = rmis
      DO kcl = 1, ncl_usgs
        READ(40,*,ERR=9997) agg_usgs(kcl,1:9)
      ENDDO

      CLOSE(40)
    ENDIF

  ENDIF

!--------------------------------------------------------------------------
! 3.3 Land-use Corine

  IF (lcori /= 0) THEN
    IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
      CALL GETENV(cal_env,ch40)
      WRITE (nfile, '(8a)') TRIM(ch40),"/",TRIM(cal_path),"/", &
        "corine","2fis_",id_stag(istag),".txt"
      OPEN (UNIT=40, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9997)
      READ(40,*,ERR=9997)
      READ(40,*,ERR=9997)

      fis_cori = rmis
      DO kcl = 1, ncl_cori
        READ(40,*,ERR=9997) idcl,fis(:)
        DO kcl2 = 1, ncl_cori
          IF (idcl == idcl_cori(kcl2)) EXIT
        ENDDO
        IF (kcl2 > ncl_cori) THEN
          WRITE (*,'(3a,i5)') "Warning: Classe illegale in ",TRIM(nfile), &
            ", ",idcl
        ELSE
          fis_cori(kcl2,:) = fis(:)
        ENDIF
      ENDDO

      CLOSE(40)
      IF (ANY(fis_cori == rmis)) WRITE (*,*) &
        "Warning: classi non gestite in ",TRIM(nfile)
    ENDIF

    IF (lchidat /= 0 .OR. lchigrb /= 0) THEN
      CALL GETENV(chi_env,ch40)
      WRITE (nfile, '(5a)') TRIM(ch40),"/",TRIM(chi_path),"/", &
        "LAND_corine_AGGREGATION"
      OPEN (UNIT=40, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9997)
      READ(40,*,ERR=9997)
  
      agg_cori = rmis
      DO kcl = 1, ncl_cori
        READ(40,*,ERR=9997) agg_cori(kcl,1:9)
      ENDDO

      CLOSE(40)
    ENDIF

  ENDIF

  IF (lbats /= 0 .OR. lusgs /= 0 .OR. ljrc /= 0 .OR. lcori /= 0) &
    WRITE (*,'(2x,a)') "Letti i parametri fisiog. per le classi di Land Use"

!==========================================================================
! 4) Trasformo i dati grezzi nei parametri richiesti in ouptut.
!
!  Metodo: esamino in sequenza i dataset richiesti, e ad ogni passaggio
!          sovrascrivo tutte le celle possibili. 
!
!  Il programma verifica che:
!  - nessuna classe abbia frazioni mancanti.
!  - la somma delle frazioni nella cella valga 1 (a meno di eps_fr)
!
!  Se la frazione di terreno urbano nella cella e' > soglia specificata
!    (frurb_*): il LU Calmet viene messo a 10 (urbano), alla cella Chimere 
!    e' attribuita una frazione urbana paria a 1.0
!
! Note Calmet:
! - Per il calcolo del LU, devono essere disponibili i valori fisiografici 
!   per tutte le classi presenti nella cella
! - fr_lu_calmet(k) e' la somma della frazioni della cella associate a un 
!   dato valore di LU (secondo la classificazione Calmet)
! - Se richiesto (lurb_fis = 1), nelle celle parzialmente vengono usati i 
!   valori urbani per tutti i parametri fisiografici; questa opzione non  e'
!   disponibilie per il dataset BATS.
!
  IF (lcaldat /= 0 .OR. lcalgrb /= 0) fisiog_cal = rmis
  IF (lchidat /= 0 .OR. lchigrb /= 0) lu_chi = rmis
  IF (lorogrb /= 0) orog = rmis
  req_lu = lcaldat /= 0 .OR. lcalgrb /= 0 .OR. lchidat /= 0 .OR. lchigrb /= 0

!--------------------------------------------------------------------------
! 4.1 Land-use BATS (by EROS)

  IF (req_lu .AND. lbats /= 0) THEN

!   Calcolo paramteri fisiogrfici Calmet
    IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
      ok = .TRUE.
      cntf = 0
      DO kpt = 1, np_out
      DO kfis = 1,7
        IF (.NOT. ANY(fr_bats(:,kpt) /= 0. .AND. fis_bats(:,kfis) == rmis) &
            .AND. ALL(fr_bats(:,kpt) /= fr_mis) .AND. &
            ABS(1. - SUM(fr_bats(:,kpt))) < eps_fr) THEN
  
          IF (kfis == 1) THEN     ! LAND USE
            DO kcl = 1,ncl_calmet
              fr_lu_calmet(kcl) = &
                SUM(fr_bats(:,kpt), MASK = fis_bats(:,1) == idcl_calmet(kcl))
            ENDDO
                               
            IF (fr_lu_calmet(2) > frurb_bats) THEN    ! forzo urbano
              fisiog_cal(kpt,1) = idcl_calmet(2)
              cntf = cntf + 1
            ELSE                                      ! uso classe prevalente
              clprev = MAXLOC(fr_lu_calmet(:))
              fisiog_cal(kpt,1) = idcl_calmet(clprev(1))
            ENDIF

          ELSE                    ! ALTRI PARAMETRI (media pesata)
            fisiog_cal(kpt,kfis) = SUM (fr_bats(:,kpt) * fis_bats(:,kfis)) 
  
          ENDIF
        ELSE
          ok = .FALSE. 
        ENDIF
      ENDDO
      ENDDO
    ENDIF

!   Calcolo frazioni di Land-Use Chimere
    IF (lchidat /= 0 .OR. lchigrb /= 0) THEN
      ok = .TRUE.
      cntf = 0
      DO kpt = 1, np_out
        IF (ALL(fr_bats(:,kpt) /= fr_mis) .AND. &
            ABS(1. - SUM(fr_bats(:,kpt))) < eps_fr) THEN
          DO klu = 1,9
            lu_chi(kpt,klu) = SUM (fr_bats(:,kpt) * agg_bats(:,klu)) 
          ENDDO 
          IF (lu_chi(kpt,5) > frurb_bats) THEN
            lu_chi(kpt,1:9) = (/0.,0.,0.,0.,1.,0.,0.,0.,0./)
            cntf = cntf + 1
          ENDIF
        ELSE
          ok = .FALSE.
        ENDIF
      ENDDO
    ENDIF

    IF (.NOT. ok) THEN
      WRITE (*,*) "Warning, dati mancanti nell' attrib. fisiog by BATS"
    ELSE
      WRITE (*,'(2x,a)') "Attribuzione fisiog. by BATS completata"
      WRITE (*,'(4x,a,i4,a)') "forzate urbane ",cntf," celle"
    ENDIF
  ENDIF

!--------------------------------------------------------------------------
! 4.2 Land-use USGS (by EROS)

  IF (req_lu .AND. lusgs /= 0) THEN

!   Calcolo paramteri fisiogrfici Calmet
    IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
      ok = .TRUE.
      cntf = 0
      DO kpt = 1, np_out
      DO kfis = 1,7
        IF (.NOT. ANY(fr_usgs(:,kpt) /= 0. .AND. fis_usgs(:,kfis) == rmis) &
            .AND. ALL( fr_usgs(:,kpt) /= fr_mis ) .AND. &
            ABS(1. - SUM(fr_usgs(:,kpt))) < eps_fr) THEN
  
          IF (kfis == 1) THEN     ! LAND USE
            DO kcl = 1,ncl_calmet
              fr_lu_calmet(kcl) = &
                SUM(fr_usgs(:,kpt), MASK = fis_usgs(:,1) == idcl_calmet(kcl))
            ENDDO
            clprev = MAXLOC(fr_lu_calmet(:))
  
            IF (fr_lu_calmet(2) > frurb_usgs) THEN    ! forzo urbano
              IF (clprev(1) /= 2) cntf=cntf + 1
              fisiog_cal(kpt,1) = idcl_calmet(2)
            ELSE                                      ! uso classe prevalente
              fisiog_cal(kpt,1) = idcl_calmet(clprev(1))
            ENDIF
  
          ELSE                    ! ALTRI PARAMETRI (media pesata o urb.)
            IF (fisiog_cal(kpt,1) == idcl_calmet(2) .AND. lurb_fis == 1) THEN
              fisiog_cal(kpt,kfis) = fis_usgs(urb_cl_usgs,kfis)
            ELSE
              fisiog_cal(kpt,kfis) = SUM (fr_usgs(:,kpt) * fis_usgs(:,kfis)) 
            ENDIF
  
          ENDIF
        ELSE
          ok = .FALSE. 
        ENDIF
      ENDDO
      ENDDO
    ENDIF

!   Calcolo frazioni di Land-Use Chimere
    IF (lchidat /= 0 .OR. lchigrb /= 0) THEN
      ok = .TRUE.
      cntf = 0
      DO kpt = 1, np_out
        IF (ALL(fr_usgs(:,kpt) /= fr_mis) .AND. &
            ABS(1. - SUM(fr_usgs(:,kpt))) < eps_fr) THEN
          DO klu = 1,9
            lu_chi(kpt,klu) = SUM (fr_usgs(:,kpt) * agg_usgs(:,klu)) 
          ENDDO 
          IF (lu_chi(kpt,5) > frurb_usgs) THEN
            lu_chi(kpt,1:9) = (/0.,0.,0.,0.,1.,0.,0.,0.,0./)
            cntf = cntf + 1
          ENDIF
        ELSE
          ok = .FALSE.
        ENDIF
      ENDDO
    ENDIF
  
    IF (.NOT. ok) THEN
      WRITE (*,*) "Warning, dati mancanti nell' attrib. fisiog by USGS"
    ELSE
      WRITE (*,'(2x,a)') "Attribuzione fisiog. by USGS completata "
      WRITE (*,'(4x,a,i4,a)') "forzate urbane ",cntf," celle"
    ENDIF
  ENDIF

!--------------------------------------------------------------------------
! 4.3 Land-use Corine

  IF (req_lu .AND. lcori /= 0) THEN

!   Calcolo paramteri fisiogrfici Calmet
    IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
      nok = 0
      cntf = 0
      DO kpt = 1, np_out
        ok = .TRUE.
        DO kfis = 1,7
          IF (.NOT. ANY(fr_cori(:,kpt) /= 0. .AND. fis_cori(:,kfis) == rmis) &
              .AND. ALL( fr_cori(:,kpt) /= fr_mis ) .AND. &
              ABS(1. - SUM(fr_cori(:,kpt))) < eps_fr) THEN

            IF (kfis == 1) THEN     ! LAND USE
              DO kcl = 1,ncl_calmet
                fr_lu_calmet(kcl) = &
                  SUM(fr_cori(:,kpt), MASK = fis_cori(:,1) == idcl_calmet(kcl))
              ENDDO
              clprev = MAXLOC(fr_lu_calmet(:))
                               
              IF (fr_lu_calmet(2) > frurb_cori) THEN  ! forzo urbano
                IF (clprev(1) /= 2) cntf=cntf + 1
                fisiog_cal(kpt,1) = idcl_calmet(2)
              ELSE                                    ! uso classe prevalente
                fisiog_cal(kpt,1) = idcl_calmet(clprev(1))
              ENDIF
  
            ELSE                    ! ALTRI PARAMETRI (media pesata o urb.)
              IF (fisiog_cal(kpt,1) == idcl_calmet(2) .AND. lurb_fis == 1) THEN
                fisiog_cal(kpt,kfis) = fis_cori(urb_cl_cori,kfis)
              ELSE
                fisiog_cal(kpt,kfis) = SUM (fr_cori(:,kpt) * fis_cori(:,kfis)) 
              ENDIF
  
            ENDIF
          ELSE
            ok = .FALSE. 
          ENDIF
        ENDDO
        IF (ok) nok = nok + 1
      ENDDO
    ENDIF

!   Calcolo frazioni di Land-Use Chimere
    IF (lchidat /= 0 .OR. lchigrb /= 0) THEN
      nok = 0
      cntf = 0
      DO kpt = 1, np_out
        IF (ALL(fr_cori(:,kpt) /= fr_mis) .AND. &
            ABS(1. - SUM(fr_cori(:,kpt))) < eps_fr) THEN
          DO klu = 1,9
            lu_chi(kpt,klu) = SUM (fr_cori(:,kpt) * agg_cori(:,klu)) 
          ENDDO 
          IF (lu_chi(kpt,5) > frurb_cori) THEN
            lu_chi(kpt,1:9) = (/0.,0.,0.,0.,1.,0.,0.,0.,0./)
            cntf = cntf + 1
          ENDIF
          nok = nok + 1
        ENDIF
      ENDDO
    ENDIF
 
    WRITE (*,'(2x,a)') "Attribuzione fisiog. by Corine completata"
    WRITE (*,'(4x,a,i5,a,f6.2,a,i5)') "attribuite ",nok, &
      " celle (",100.*REAL(nok)/REAL(np_out),"%), forzate urbane ",cntf
  ENDIF

!--------------------------------------------------------------------------
! 4.4 Orografia USGS
  IF (ldem_usgs /= 0) THEN
    IF (lcaldat /= 0 .OR. lcalgrb /= 0) fisiog_cal(:,8) = orog_usgs(:)
    IF (lorogrb /= 0) orog(:) = orog_usgs(:)
  ENDIF

!--------------------------------------------------------------------------
! 4.5 Orografia NASA
  IF (ldem_nasa /= 0) THEN
    IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
      WHERE (orog_nasa(:) /= rmis)
        fisiog_cal(:,8) = orog_nasa(:)
      ENDWHERE
    ENDIF
    IF (lorogrb /= 0) THEN
      WHERE (orog_nasa(:) /= rmis)
        orog(:) = orog_nasa(:)
      ENDWHERE
    ENDIF
  ENDIF

!--------------------------------------------------------------------------
! 4.6 Scrivo statisitche di attribuzione LU 

  IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
    WRITE (95,*) "Numero di punti per ciascun LU Calmet:"
    DO k = 1, ncl_calmet
      natt(k) = COUNT(fisiog_cal(:,1) == idcl_calmet(k))
      WRITE (95,*) idcl_calmet(k),natt(k)
    ENDDO
    WRITE (95,*) "Altre (errori)",np_out - SUM(natt(:))
  ENDIF

  IF (lchidat /= 0 .OR. lchigrb /= 0) THEN
    cl_prev_chi(1:kpt) = MAXLOC(lu_chi(1:kpt,1:9),DIM=2)
    WRITE (95,*) "Celle in cui e' prevalente ciascuna classe LU Chimere:"
    DO k = 1,9
      WRITE (95,*),idcl_chimere(k),COUNT(cl_prev_chi(:)==k)
    ENDDO
  ENDIF

  IF (lcaldat /= 0 .OR. lcalgrb /= 0 .OR. lchidat /= 0 .OR. lchigrb /= 0) &
    WRITE (*,'(2x,a)') "Calcolati paramteri fisiografici su griglia output"

!--------------------------------------------------------------------------
! 4.7 Scrivo statisitche di attribuzione DEM 

  IF (lcaldat /= 0 .OR. lcalgrb /= 0) THEN
    cnt_dem_mis = COUNT(fisiog_cal(:,8) == rmis)
    WRITE (*,'(a,i8)') "Orografia: celle indefinite: ",cnt_dem_mis
  ELSE IF (lorogrb /= 0) THEN
    cnt_dem_mis = COUNT(orog(:) == rmis)
    WRITE (*,'(a,i8)') "Orografia: celle indefinite: ",cnt_dem_mis
  ENDIF

  IF (ldem_nasa /= 0) THEN
    cnt_dem_nasa = COUNT(orog_nasa(:) /= rmis)
    WRITE (*,*) "  celle definite con DEM NASA: ",cnt_dem_nasa
  ENDIF

  IF (ldem_usgs /= 0) THEN
    cnt_dem_usgs = COUNT(orog_usgs(:) /= rmis)
    IF (ldem_nasa /= 0) THEN
      WRITE (*,*) "  celle definite con DEM USGS: ", &
        cnt_dem_usgs - cnt_dem_nasa
    ELSE
      WRITE (*,*) "  celle definite con DEM USGS: ",cnt_dem_usgs
    ENDIF
  ENDIF

!==========================================================================
! 5) Scrivo gli output.

!--------------------------------------------------------------------------
! 5.1 Scrivo output per Calmet in formato GEO.DAT
!    - geo.dat:        LU, Orog, Z0, Alb, Bowen, Soil HF, Antr.HF, LAI
!    - array fisiog:   LU, Z0, Alb, Bowen, Soil HF, Antr.HF, LAI, Orog
!    - array fis_????: LU, Z0, Alb, Bowen, Soil HF, Antr.HF, LAI

  IF (lcaldat /= 0) THEN
    WRITE (nfile,'(5a)') "geo_",TRIM(area_out),"_",id_stag(istag),".dat"
    OPEN (UNIT=50, FILE=TRIM(nfile), STATUS="REPLACE", ACTION="WRITE")

! Header
    WRITE (50,'(4a)') "Area: ",TRIM(area_out)," Stagione: ",id_stag(istag)
    WRITE (50,'(2i5,3f10.3,i4)') ni_out,nj_out,dx_out,x1_out,y1_out, &
      utmz_out

! Land use
    WRITE (50,'(i1)') 0
    DO j = nj_out,1,-1
      k1 = (j-1) * ni_out + 1
      k2 = j * ni_out
      WRITE (50,chfmt1) NINT(fisiog_cal(k1:k2,1))
    ENDDO

! Orografia
    WRITE (50,'(f3.1)') 1.
    DO j = nj_out,1,-1
      k1 = (j-1) * ni_out + 1
      k2 = j * ni_out
      WRITE (50,chfmt2) fisiog_cal(k1:k2,8)
    ENDDO

! Altri campi
    DO kfis = 2,7
      WRITE (50,'(i1)') 2
      DO j = nj_out,1,-1
        k1 = (j-1) * ni_out + 1
        k2 = j * ni_out
        WRITE (50,chfmt2) fisiog_cal(k1:k2,kfis)
      ENDDO
    ENDDO

    CLOSE(50)
    WRITE (*,'(a)') "Scritto output per Calmet"

  ENDIF

!--------------------------------------------------------------------------
! 5.2 Scrivo output per Calmet in formato grib

  IF (lcalgrb /= 0) THEN
    WRITE (nfile,'(5a)') "geo_",TRIM(area_out),"_",id_stag(istag),".grb"
    CALL PBOPEN(iu_grb,TRIM(nfile),'W',kret)

    DO kfis = 1,8
      CALL write_grib(fisiog_cal(:,kfis),code_grib_calmet(kfis),iu_grb,grb_dir_inc)
    ENDDO

    CALL PBCLOSE(iu_grb,kret)
    WRITE (*,'(a)') "Scritto output Calmet, formato grib"
  ENDIF

!--------------------------------------------------------------------------
! 5.3 Scrivo output per Chimere in formato LANDUSE

  IF (lchidat /= 0) THEN
    WRITE (nfile,'(2a)') "LANDUSE_",TRIM(area_out)
    OPEN (UNIT=51,FILE=nfile, STATUS="REPLACE", FORM="FORMATTED", ACTION="WRITE")
    DO kpt = 1,np_out
      WRITE (51,'(9f7.3)') lu_chi(kpt,1:9)
    ENDDO
    CLOSE(51)
  ENDIF

!--------------------------------------------------------------------------
! 5.4 Scrivo output per Chimere in formato GRIB

  IF (lchigrb /= 0) THEN
    WRITE (nfile,'(3a)') "LANDUSE_",TRIM(area_out),".grb"
    CALL PBOPEN(iu_grb,TRIM(nfile),'W',kret)

    CALL write_grib(REAL(cl_prev_chi(:)),121,iu_grb,grb_dir_inc)
    DO klu = 1,9
      CALL write_grib(lu_chi(:,klu),130+klu,iu_grb,grb_dir_inc)
    ENDDO

    CALL PBCLOSE(iu_grb,kret)
    WRITE (*,'(a)') "Scritto output Chimere, formato grib"
  ENDIF

ENDDO stagioni
! chiuso il ciclo sulle stagioni

!--------------------------------------------------------------------------
! 5.5 Scrivo output orografia in formato GRIB
  
IF (lorogrb /= 0) THEN

  WRITE (nfile,'(5a)') "orog_",TRIM(area_out),".grb"
  CALL PBOPEN(iu_grb,TRIM(nfile),'W',kret)

  CALL write_grib(orog,8,iu_grb,grb_dir_inc)

  CALL PBCLOSE(iu_grb,kret)
  WRITE (*,'(a)') "Scritta orografia, formato grib"

ENDIF


STOP

!==========================================================================
! 7) Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "Errore leggendo proc_fisiog.inp"
STOP

9998 CONTINUE
WRITE (*,*) "Parametri illegali in proc_fisiog.inp"
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)
STOP

END PROGRAM proc_fisiog

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_bats(fract)

!--------------------------------------------------------------------------
! Dato un grigliato UTM/GEO/ROT, accede al dataset di land cover BATS e 
! ritorna la frazione di ciascun uso del suolo in ciascuna cella del 
! grigliato.
!
! Input:  parametri grigliato di output e datset BATS (modulo local)
! Ouptut: array fract
! Metodo: assegna i valori di land use al punto centrale delle celle BATS,
!         e semplicemente conta i punti che cadono in ciascuna cella UTM.
!         Funziona solo se la risoluzione BATS e' molto piu' fine del 
!         grigliato richiesto.
!
! Note:
! - Formato dataset BATS: e' scritto a BYTES; parte dall'angolo NW, ogni 
!   record corrisponde a una latitudine (ordine 12/34); 
!   la CONVERT="BIG_ENDIAN" nella OPEN non e' necessaria perche' le parole
!   sono di un solo byte.
!
! - Questa subroutine potrebbe non essere traspotrabile su altre macchine,
!   in quanto usa INTEGER(KIND=1) per dichiarare una variabile intera di un
!   byte (con altri compilatori potrebbe essere necessario usare un codice
!   KIND diverso)
!
! - Per quel che riguarda la lunghezza dei record nel file BATS, deve 
!   essere messa: ni_bats/4 se compilo normalmente, ni_bats se compilo con
!   l'opzione -assume byterecl, come potrebbe essere richiesto dalle PBIO.
!--------------------------------------------------------------------------

USE local
IMPLICIT NONE

REAL, INTENT(OUT) :: fract(ncl_bats, np_out)

INTEGER(KIND=1) arr(ni_bats)

REAL::  xout,yout,lat,lon,np_ave,bats_cell,out_cell,np_exp
INTEGER :: cnt(ncl_bats,np_out),nok(np_out)
INTEGER :: r1,r2,c1,c2
INTEGER :: i,j,k,kr,kc,kcl,idum,i_out,j_out,k_out,np_max,np_min,nwrong
INTEGER :: npts(ncl_bats)
CHARACTER (LEN=100) :: nfile
CHARACTER (LEN=40) :: ch40
!--------------------------------------------------------------------------

! Trovo righe e colonne estreme da leggere nel dataset
r1 = INT ((323985. - lat2_out*3600.) / 30.) + 1
r2 = INT ((323985. - lat1_out*3600.) / 30.) + 2
c1 = INT ((lon1_out*3600. + 647985.) / 30.) + 1
c2 = INT ((lon2_out*3600. + 647985.) / 30.) + 2

! Scorro i punti del dataset e li attribuisco alle celle UTM
! NOTA: (kr,kc) sono gli indici di riga/colonna nel dataset, e scorrono a 
!       partire dall'angolo NW (mentre i,j scorrono da SW)

CALL GETENV(bats_env,ch40)
WRITE (nfile,'(5a)') TRIM(ch40),"/",TRIM(bats_path),"/",TRIM(bats_name)
OPEN(UNIT=30, FILE=nfile, FORM='UNFORMATTED', STATUS='OLD',ACCESS='DIRECT', &
  RECL=ni_bats, ERR=9999) 
! RECL=ni_bats/4, ERR=9999) 

cnt = 0
DO kr = r1, r2
  READ (30,REC=kr,ERR=9999) arr
  DO kc = c1, c2

!   trovo la cella del grigliato di output in cui cade il punto BATS
    lat = (90. - 1./240.) - REAL(kr-1)/120.
    lon = (-180. + 1./240.) + REAL(kc-1)/120.
    IF (proj_out == "U") THEN
      CALL ll2utm(lat,lon,utmz_out,xout,yout,idum)
    ELSE IF (proj_out == "R") THEN
      CALL tll(lon,lat,xrot_out,yrot_out,xout,yout)
    ELSE IF (proj_out == "G") THEN
      xout = lon
      yout = lat
    ENDIF
    i_out = NINT((xout - (x1_out + dx_out/2.))/dx_out) + 1
    j_out = NINT((yout - (y1_out + dy_out/2.))/dy_out) + 1

!   se sono dentro al grigliato output, registro la classe LU che ho letto
    IF (i_out > 0 .AND. j_out > 0 .AND. & 
        i_out <= ni_out .AND. j_out <= nj_out) THEN
      k_out = i_out + (j_out-1) * ni_out

      DO kcl = 1, ncl_bats
        IF (arr(kc) == idcl_bats(kcl)) EXIT 
      ENDDO
      IF (kcl > ncl_bats) THEN
        WRITE (*,*) "Trovata classe BATS non gestita: ",arr(kc)
      ELSE
        cnt(kcl,k_out) = cnt(kcl,k_out) + 1
      ENDIF
    ENDIF

  ENDDO
ENDDO
CLOSE (30)

! Per ciascuna cella, passo dal n.ro di punti alla frazione
nok(:) = SUM(cnt(:,:),1)

DO k = 1, np_out
  IF (nok(k) > 0) THEN
    fract(:,k) = REAL(cnt(:,k)) / REAL(nok(k))
  ELSE 
    fract(:,k) = fr_mis
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! Statistiche dell'attribuzione punti

! dati in ciacuna cella
! NB: bats_cell = area media di una cella usgs (Km^2) 
!     out_cell = area media di una cella nel grigliato di output (Km^2)
np_max = MAXVAL(nok(:))
np_min = MINVAL(nok(:))
np_ave = SUM(nok(:)) / REAL(np_out)

bats_cell = 0.926 * 0.926 * cos((lat2_out+lat1_out)*3.1415/360.)
IF (proj_out == "U") THEN
  out_cell = dx_out * dy_out
ELSE IF (proj_out == "G") THEN  
  out_cell = (dx_out * dy_out * 111.111 * 111.111 * &
    cos((lat2_out+lat1_out)*3.1415/360.))
ELSE IF (proj_out == "R") THEN
  out_cell = (dx_out * dy_out * 111.111 * 111.111 * &
    cos((lat2_out+lat1_out-2*yrot_out)*3.1415/360.))
ENDIF
np_exp = out_cell / bats_cell

WRITE (*,'(a)') "Letto dataset BATS. Dati per ciasuna cella: "
WRITE (*,'(2x,2(a,i4),2(a,f7.2))') "max ",np_max," min ",np_min, &
  " ave ",np_ave," attesi in media ",np_exp

! dati per ciascuna classe
npts(:) = SUM(cnt(:,:),2)
WRITE (*,*) "Numero di dati validi per ciascuna classe BATS:"
WRITE (*,'(6(i10,2x))') npts(1:ncl_bats)

! somma delle frazioni di copertura in ciascuna cella
nwrong = COUNT(ABS(1. - SUM(fract(:,:),1)) > eps_fr)
IF (nwrong > 0) WRITE (*,*) &
  "Warning leggendo BATS: ",nwrong," punti (parzialmente) scoperti "

RETURN

9999 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)
STOP

END SUBROUTINE read_bats

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_usgs(fract)

!--------------------------------------------------------------------------
! Dato un grigliato UTM/GEO/ROT, accede al dataset di land cover BATS e 
! ritorna la frazione di ciascun uso del suolo in ciascuna cella del 
! grigliato.
!
! Input:  parametri grigliato di output e datset USGS (modulo local)
! Ouptut: array fract
! Metodo: assegna i valori di land use al punto centrale delle celle USGS,
!         e semplicemente conta i punti che cadono in ciascuna cella UTM.
!         Funziona solo se la risoluzione USGS e' molto piu' fine del 
!         grigliato richiesto.
!
! Note:
! - Subr. derivata dalla read_bats_utm. A differenza di questa, il codice 
!   delle classi non coincide con la loro posizione nell'array fract, 
!   perche' esistono (almeno in teoria) le classi "0" e "100". 
!
! - Formato dataset USGS: e' scritto a partire dall'angolo NW, ogni record
!   corrisponde a una latitudine (ordine 12/34).
!   Ogni dato occupa 1 byte, a seconda del compilatore, mettere la 
!   lunghezza dei record nella OPEN a ni_usgs o ni_usgs/4.
!
! - Questa subroutine potrebbe non essere traspotrabile su altre macchine,
!   in quanto usa INTEGER(KIND=1) per dichiarare una variabile intera di un
!   byte (con altri compilatori potrebbe essere necessario usare un codice
!   KIND diverso)
!
! - Per quel che riguarda la lunghezza dei record nel file USGS, deve 
!   essere messa: ni_usgs/4 se compilo normalmente, ni_usgs se compilo con
!   l'opzione -assume byterecl, come potrebbe essere richiesto dalle PBIO.
!--------------------------------------------------------------------------

USE local
IMPLICIT NONE

REAL, INTENT(OUT) :: fract(ncl_usgs, np_out)

INTEGER(KIND=1) :: arr(ni_usgs)

REAL::  xout,yout,lat,lon,np_ave,usgs_cell,out_cell,np_exp
INTEGER :: cnt(ncl_usgs, np_out), nok(np_out)
INTEGER :: r1,r2,c1,c2
INTEGER :: i,j,k,kr,kc,kcl,idum,i_out,j_out,k_out,np_max,np_min,nwrong
INTEGER :: npts(ncl_usgs)
CHARACTER (LEN=100) :: nfile
CHARACTER (LEN=40) :: ch40
!--------------------------------------------------------------------------

! Trovo righe e colonne estreme da leggere nel dataset
r1 = INT ((323985. - lat2_out*3600.) / 30.) + 1
r2 = INT ((323985. - lat1_out*3600.) / 30.) + 2
c1 = INT ((lon1_out*3600. + 647985.) / 30.) + 1
c2 = INT ((lon2_out*3600. + 647985.) / 30.) + 2

! Scorro i punti del dataset e li attribuisco alle celle OUT
! NOTA: (kr,kc) sono gli indici di riga/colonna nel dataset, e scorrono a 
!       partire dall'angolo NW (mentre i,j scorrono da SW)

CALL GETENV(usgs_env,ch40)
WRITE (nfile,'(5a)') TRIM(ch40),"/",TRIM(usgs_path),"/",TRIM(usgs_name)
OPEN(UNIT=30, FILE=nfile, FORM='UNFORMATTED', STATUS='OLD',ACCESS='DIRECT', &
  RECL=ni_usgs, ERR=9999) 
! RECL=ni_usgs/4, ERR=9999) 

cnt = 0
DO kr = r1, r2
  READ (30,REC=kr,ERR=9999) arr
  DO kc = c1, c2

!   Trovo la cella del grigliato di output in cui cade il punto USGS
    lat = (90. - 1./240.) - REAL(kr-1)/120.
    lon = (-180. + 1./240.) + REAL(kc-1)/120.
    IF (proj_out == "U") THEN
      CALL ll2utm(lat,lon,utmz_out,xout,yout,idum)
    ELSE IF (proj_out == "R") THEN
      CALL tll(lon,lat,xrot_out,yrot_out,xout,yout)
    ELSE IF (proj_out == "G") THEN
      xout = lon
      yout = lat
    ENDIF
    i_out = NINT((xout - (x1_out + dx_out/2.))/dx_out) + 1
    j_out = NINT((yout - (y1_out + dy_out/2.))/dy_out) + 1

!   Se sono dentro al grigliato output, registro la classe LU che ho letto
    IF (i_out > 0 .AND. j_out > 0 .AND. & 
        i_out <= ni_out .AND. j_out <= nj_out) THEN
      k_out = i_out + (j_out-1) * ni_out

      DO kcl = 1, ncl_usgs
        IF (arr(kc) == idcl_usgs(kcl)) EXIT 
      ENDDO
      IF (kcl > ncl_usgs) THEN
        WRITE (*,*) "Trovata classe USGS non gestita: ",arr(kc)
      ELSE
        cnt(kcl, k_out) = cnt(kcl, k_out) + 1
      ENDIF
    ENDIF

  ENDDO
ENDDO
CLOSE (30)

! Per ciascuna cella, passo dal n.ro di punti alla frazione
nok(:) = SUM(cnt(:,:),1)

DO k = 1, np_out
  IF (nok(k) > 0) THEN
    fract(:,k) = REAL(cnt(:,k)) / REAL(nok(k))
  ELSE 
    fract(:,k) = fr_mis
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! Statistiche dell'attribuzione punti

! Dati in ciacuna cella. 
! NB: usgs_cell = area media di una cella usgs (Km^2) 
!     out_cell = area media di una cella nel grigliato di output (Km^2)

np_max = MAXVAL(nok(:))
np_min = MINVAL(nok(:))
np_ave = SUM(nok(:)) / REAL(np_out)

usgs_cell = 0.926 * 0.926 * cos((lat2_out+lat1_out)*3.1415/360.)
IF (proj_out == "U") THEN
  out_cell = dx_out * dy_out
ELSE IF (proj_out == "G") THEN  
  out_cell = (dx_out * dy_out * 111.111 * 111.111 * &
    cos((lat2_out+lat1_out)*3.1415/360.))
ELSE IF (proj_out == "R") THEN
  out_cell = (dx_out * dy_out * 111.111 * 111.111 * &
    cos((lat2_out+lat1_out-2*yrot_out)*3.1415/360.))
ENDIF
np_exp = out_cell / usgs_cell

WRITE (*,'(a)') "Letto dataset USGS (EROS). Dati per ciasuna cella: "
WRITE (*,'(2x,2(a,i4),2(a,f7.2))') "max ",np_max," min ",np_min, &
  " ave ",np_ave," attesi in media ",np_exp

! Dati per ciascuna classe
npts(:) = SUM(cnt(:,:),2)
DO k=1,ncl_usgs
!  WRITE (*,*) k,npts(k)
ENDDO

! Somma delle frazioni di copertura in ciascuna cella
nwrong = COUNT(ABS(1. - SUM(fract(:,:),1)) > eps_fr)
IF (nwrong > 0) WRITE (*,*) &
  "Warning leggendo USGS: ",nwrong," punti (parzialmente) scoperti "

RETURN

9999 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)
STOP

END SUBROUTINE read_usgs

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_corine(fract)

!--------------------------------------------------------------------------
! Dato un grigliato UTM, geografico o ruotato, accede al dataset di land 
! cover CORINE 2000 (su area Italia), e ritorna la frazione di ciascun uso 
! del suolo (42 classi) in ciascuna cella.
!
! Input:  dataset Corine 2000, ritagliato sull'Italia e riscritto in 
!         formato compatto (file LAND_CLC00ita)
! Ouptut: array fract. 
! Metodo: per ciscuna cella, conta la % dei punti Corine per ciascuna 
!         classe di uso del suolo che cadono al suo interno.
!
! Formato del dataset:
! - Griglia: 14472x14472 punti, passo 100 m, x1=3743325., y1=1379546.
! - proiezione: Lambert, x0=4321000., y0=3210000., lon0=10., lat0=52.
! - Ordine dei punti: standard (a partire dall'angolo SW)
! - I dati mancanti (Svizzera) sono codificati come "0"
!
! Note:
! - nelle celle completamente scoperte, fract vale fr_mis in tutte le 
!   classi, mentre in quelle parz. scoperte le classi indefinite valgono 0.
! - Il dataset Corine contine un errore nelle coordinate Lambert, che 
!   darebbe luogo a uno spostamento nel land use fino a 2 km. E' stata 
!   introdotta una correzione empirica che dovrebbe ridurre l'errore sull'
!   Italia a circa 200 m.
!   [Metodo: confrontando orografia e LU, e' stato calcolato l'errore in
!    LAT e LON delle cooridnate Lambert, in 3 punti (TS, CA, Leuca). 
!    Gli errori vengono poi interpolati linearmente in X e Y in ciascu 
!    punto Corine.]
!--------------------------------------------------------------------------

USE local
IMPLICIT NONE

REAL, INTENT(OUT) :: fract(ncl_cori,np_out)

! Parametri griglia Corine Italia
INTEGER, PARAMETER :: nx_clci = 14472, ny_clci = 14472
REAL (KIND=8), PARAMETER :: x1_clci = 3743325., y1_clci = 1379546.
REAL (KIND=8), PARAMETER :: dx_clci = 100., dy_clci = 100.
REAL (KIND=8), PARAMETER :: x0 = 4321000., y0 = 3210000.
REAL( KIND=8), PARAMETER :: lon0 = 10., lat0 = 52.

! Coefficenti per correzione coordinate punti dataset
! lat_corr = lat + a*lat + b*lon + c
! lon_corr = lon + d*lat + e*lon + f
REAL (KIND=8), PARAMETER :: a =-0.00152, b = 0.00001, c = 0.01544
REAL (KIND=8), PARAMETER :: d = 0.00019, e = 0.00205, f =-0.10203

! Altre variabili locali
INTEGER, PARAMETER :: iz = 32
REAL (KIND=8) :: xgeod,ygeod,xgeod2,ygeod2,xlamb,ylamb,xoutd,youtd
REAL :: np_ave,np_exp,area_cori
INTEGER (kind=1) :: i1lu(nx_clci)
INTEGER :: cnt(ncl_cori,np_out),nok(np_out),npts(ncl_cori)
INTEGER :: i_clci,j_clci,il,i_out,j_out,k_out,kcl
INTEGER :: np_max,np_min,np_att,cnt_mis
INTEGER :: eof,eor,ios,k,idum
CHARACTER (LEN=100) :: nfile
CHARACTER (LEN=40) :: ch40

!--------------------------------------------------------------------------
! 1) Leggo i dati e li attribuisco al grigliato di output

! Trovo codice per EOF
CALL get_eof_eor(eof, eor)
fract (:,:) = fr_mis
cnt(:,:) = 0

! Scorro il dataset e sommo le frazioni di LU nelle varie celle 
CALL GETENV(corine_env,ch40)
WRITE (nfile,'(7a)') TRIM(ch40),"/",TRIM(corine_path),"/","LAND_CLC00ita"
OPEN(UNIT=30, FILE=nfile, STATUS='OLD', ACTION="READ", &
  FORM="UNFORMATTED", ERR=9999)

DO j_clci = 1, ny_clci
  READ (30,IOSTAT=ios) i1lu(1:nx_clci)
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998
  IF (MOD(j_clci,100) == 0) WRITE(*,*) "Ds Corine Ita, letta riga ", &
    j_clci," su ",ny_clci

  DO i_clci = 1, nx_clci

    il = INT(i1lu(i_clci))

    xlamb = x1_clci + dx_clci * DBLE(i_clci-1)
    ylamb = y1_clci + dy_clci * DBLE(j_clci-1)
    CALL laea2ll(xlamb,ylamb,x0,y0,lon0,lat0,xgeod2,ygeod2)
    
!   Correzione empirica coordinate Lambert
    xgeod = xgeod2 + a * xgeod2 + b * ygeod2 + c
    ygeod = ygeod2 + d * xgeod2 + e * ygeod2 + f

    IF (proj_out == "U") THEN
      CALL ll2utm_dble(ygeod,xgeod,iz,xoutd,youtd,idum)
    ELSE IF (proj_out == "R") THEN
      CALL tll_dble(xgeod,ygeod,DBLE(xrot_out),DBLE(yrot_out),xoutd,youtd)
    ELSE
      xoutd = xgeod
      youtd = ygeod
    ENDIF
    i_out = NINT((REAL(xoutd) - (x1_out + dx_out/2.))/dx_out) + 1
    j_out = NINT((REAL(youtd) - (y1_out + dy_out/2.))/dy_out) + 1

!   Se sono dentro al grigliato output, registro la classe LU che ho letto
    IF (i_out>0 .AND. j_out>0 .AND. i_out<=ni_out .AND. j_out<=nj_out) THEN
      k_out = i_out + (j_out-1) * ni_out
  
      DO kcl = 1, ncl_cori
        IF (il == idcl_cori(kcl)) EXIT 
      ENDDO
      IF (kcl > ncl_cori) THEN
        IF (il == 0) THEN
          cnt_mis = cnt_mis + 1
        ELSE
          WRITE (*,'(a,i3,2(a,f7.3))') "Skip classe Corine illegale: ", &
            il," lon ",xgeod," lat ",ygeod
        ENDIF
      ELSE
        cnt(kcl,k_out) = cnt(kcl,k_out) + 1
      ENDIF
    ENDIF
  ENDDO
ENDDO
CLOSE(30)

! Per ciascuna cella, passo dal n.ro di punti alla frazione
nok(1:np_out) = SUM(cnt(:,1:np_out),1)

DO k = 1, np_out
  IF (nok(k) > 0) THEN
    fract(:,k) = REAL(cnt(:,k)) / REAL(nok(k))
  ELSE 
    fract(:,k) = fr_mis
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 2) Statistiche dell'attribuzione punti

! dati in ciacuna cella
np_max = MAXVAL(nok(1:np_out))
np_min = MINVAL(nok(1:np_out))
np_att = COUNT(nok(1:np_out) /= 0)
np_ave = SUM(nok(1:np_out)) / REAL(np_out)
area_cori = 0.1 * 0.1
IF (proj_out == "U") THEN
  np_exp = (dx_out * dy_out) / area_cori
ELSE IF (proj_out == "R" .OR. proj_out == "G") THEN
  np_exp = COS((lat2_out+lat1_out)*3.1415/360.) * 111.1**2 * dx_out * dy_out / area_cori
ENDIF

WRITE (*,'(a)') " Letto dataset Corine 2000 Italia "
WRITE (*,'(2(a,i6))') "Celle attribuite: ",np_att," su ",np_out
WRITE (*,'(a)') "Dati per ciascuna cella"
WRITE (*,'(2(a,i6),2(a,f9.2))') "  max ",np_max," min ",np_min, &
  " ave ",np_ave," attesi in media ",np_exp

! dati per ciascuna classe
npts(:) = SUM(cnt(:,1:np_out),2)
WRITE (*,*)
WRITE (*,'(a)') "Numero di dati validi per ciascuna delle 42 classi di terra:"
WRITE (*,'(6(i10,2x))') npts(1:42)
WRITE (*,'(2(a,i10))') "Punti di mare ",npts(43)," Punti mancanti ",cnt_mis

RETURN

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(nfile)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)," record ",k
STOP

END SUBROUTINE read_corine

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_orog_usgs(orog)
!--------------------------------------------------------------------------
! Dato un grigliato UTM/GEO/ROT, accede al dataset di orograsfia USGS 
! (EROS) e ritorna la quota media di ciascuna cella.
!
! Metodo: il programma fa un ciclo su tutti i punti del dataset, cercando
! quelli che che cadono in una cella del grigliato di output; alla fine 
! calcola la quota media in ciascuna cella. Vengono comunque aperti solo i
! pezzi del dataset che intersecano l'area di output.
!
! NOTE:
! Nel file USGS, ciascun dato occupa 2 byte; a seconda del compilatore, 
! mettere la lunghezza dei record nella OPEN a nlont*2 o nlont/2
!--------------------------------------------------------------------------

USE local
IMPLICIT NONE

REAL, INTENT(OUT) :: orog(np_out)

! Parametri del dataset
!INTEGER :: selected_real_kind
REAL(KIND=kdble), PARAMETER :: rd = 1/120., dd=120.  ! risoluzione dset
INTEGER(KIND=2), PARAMETER :: dem_miss = -9999  ! dato mancante
INTEGER, PARAMETER :: ndset = 33                ! n.ro pezzi dataset
INTEGER, PARAMETER :: maxcol = 7200             ! colonne in un pezzo dset

! Parametri relativi ai pezzi del dataset
CHARACTER(LEN=7), PARAMETER :: nomed(ndset) = (/ &
  'W180N90','W140N90','W100N90','W060N90','W020N90','E020N90', &
  'E060N90','E100N90','E140N90','W180N40','W140N40','W100N40', &
  'W060N40','W020N40','E020N40','E060N40','E100N40','E140N40', &
  'W140S10','W100S10','W060S10','W020S10','W180S10','E020S10', &
  'E060S10','E100S10','E140S10','W180S60','W120S60','W060S60', &
  'W000S60','E060S60','E120S60'/)

REAL(KIND=kdble), PARAMETER :: lat1i(ndset) = (/ &
   40., 40., 40., 40., 40., 40., 40., 40., 40., &
  -10.,-10.,-10.,-10.,-10.,-10.,-10.,-10.,-10., &
  -60.,-60.,-60.,-60.,-60.,-60.,-60.,-60.,-60., &
  -90.,-90.,-90.,-90.,-90.,-90. /)

REAL(KIND=kdble), PARAMETER :: lat2i(ndset) = (/ &
   90., 90., 90., 90., 90., 90., 90., 90., 90., &
   40., 40., 40., 40., 40., 40., 40., 40., 40., &
  -10.,-10.,-10.,-10.,-10.,-10.,-10.,-10.,-10., &
  -60.,-60.,-60.,-60.,-60.,-60. /)

REAL(KIND=kdble), PARAMETER :: lon1i(ndset) = (/ &
  -180.,-140.,-100.,-60.,-20.,20.,60.,100.,140., &
  -180.,-140.,-100.,-60.,-20.,20.,60.,100.,140., &
  -180.,-140.,-100.,-60.,-20.,20.,60.,100.,140., &
  -180.,-120., -60.,0., 60.,120./)

REAL(KIND=kdble), PARAMETER :: lon2i(ndset) = (/ &
  -140.,-100.,-60.,-20.,20.,60.,100.,140.,180., &
  -140.,-100.,-60.,-20.,20.,60.,100.,140.,180., &
  -140.,-100.,-60.,-20.,20.,60.,100.,140.,180., &
  -120.,-60.,0.,60.,120.,180./)

! Variabili interne
REAL(KIND=kdble) :: lonminz,latminz,lonmaxz,latmaxz,dlonz,dlatz,xdset,ydset
REAL(KIND=kdble) :: lat1(ndset),lon1(ndset),lat2(ndset),lon2(ndset)
REAL(KIND=kdble) :: lon_out(np_out),lat_out(np_out)
REAL(KIND=kdble) :: xgeo_dset,ygeo_dset,ddx,ddy
INTEGER (KIND=2) :: rig(maxcol)
REAL :: xp(np_out),yp(np_out)
REAL :: z1(np_out),z2(np_out),z3(np_out),z4(np_out)
REAL :: lonmax,lonmin,latmax,latmin,avg,or_ave,sea_fr,np_ave,np_exp
REAL :: xx_dset,yy_dset,xx,yy,rlat,rlon,usgs_cell,out_cell
INTEGER :: cnt(np_out),np_max,np_min,nsea,ntot,nok
INTEGER :: i,j,k,kd,kr,kc,nlont,nlatt,nfirla,nlasla,idum,i_out,j_out,k_out
CHARACTER(LEN=100) :: nfile
CHARACTER(LEN=40) :: ch40

!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Se e' richiesta l'interpolazione, calcolo le coordinate georgrafiche 
!     dei punti di output
IF (int_orog == 1) THEN
  DO i = 1,ni_out
  DO j = 1,nj_out
    k = i + (j-1) * ni_out
    xx = x1_out + (i-1) * dx_out
    yy = y1_out + (j-1) * dy_out
    IF (proj_out == "U") THEN
      CALL utm2ll(xx,yy,utmz_out,.FALSE.,rlat,rlon)
    ELSE IF (proj_out == "R") THEN
      CALL rtll(xx,yy,xrot_out,yrot_out,rlon,rlat)
    ELSE IF (proj_out == "G") THEN
      rlon = xx
      rlat = yy
    ENDIF
    lon_out(k) = DBLE(xx)
    lat_out(k) = DBLE(yy)
  ENDDO
  ENDDO
  z1(:) = rmis
  z2(:) = rmis
  z3(:) = rmis
  z4(:) = rmis
  xp(:) = rmis
  yp(:) = rmis
ENDIF

! 1.2 Coordinate estreme dei pezzi dataset, relative ai centri delle celle
lat1(:) = lat1i(:) + 0.5 * rd
lon1(:) = lon1i(:) + 0.5 * rd
lat2(:) = lat2i(:) - 0.5 * rd
lon2(:) = lon2i(:) - 0.5 * rd

orog(:) = 0.
cnt(:) = 0.

!--------------------------------------------------------------------------
! 2) Lettura (ciclo sui pezzi del dateset USGS)

DO kd = 1,ndset

! Se il pezzo non si sovrappone all'area richiesta, lo skippo
  IF (lon2(kd) < lon1_out .OR. lon1(kd) > lon2_out .OR. &
      lat2(kd) < lat1_out .OR. lat1(kd) > lat2_out) CYCLE

! Dimensioni del pezzo di dataset & righe da leggere (aggiungo una corince 
! per sicurezza)
  nlont = NINT((lon2(kd) - lon1(kd))*dd) + 1
  nlatt = NINT((lat2(kd) - lat1(kd))*dd) + 1
  nfirla = INT((lat2(kd) - lat2_out)*dd) + 2
  nlasla = INT((lat2(kd) - lat1_out)*dd) + 1

  nfirla = nfirla - 1
  nlasla = nlasla + 1

  nfirla = MAX(nfirla,1)
  nlasla = MIN(nlasla,nlatt)

! apro file dati (ATTENZIONE: ISTRUZIONE NON STANDARD!)
  CALL GETENV(usgs_orog_env,ch40)
  WRITE (nfile,'(6a)') TRIM(ch40),"/",TRIM(usgs_orog_path),"/",nomed(kd),'.DEM'
  OPEN(31, FILE=nfile, FORM='UNFORMATTED', STATUS='OLD', ERR=9999, &
       ACCESS='DIRECT',CONVERT='BIG_ENDIAN',RECL=nlont*2)
!      ACCESS='DIRECT',CONVERT='BIG_ENDIAN',RECL=nlont/2)
!      ACCESS='DIRECT',RECL=nlont*2) !Sun Unix

! Leggo il dataset. Se e' richiesto il calcolo medio, sommo i dati che cadono in
! ciascuna cella di ouput, altrimenti memorizzo per ciscuna cella i 4 valori 
! che serviranno per interpolare.

  DO kr = nfirla, nlasla
    READ(31,REC=kr) (rig(k),k=1,nlont)
    ygeo_dset = lat2(kd) - (kr-1)*rd

    DO kc = 1, nlont
      xgeo_dset = lon1(kd)+(kc-1)*rd

      IF (int_orog == 0) THEN
        IF (proj_out == "U") THEN
          CALL ll2utm(REAL(ygeo_dset),REAL(xgeo_dset),utmz_out,xx_dset,yy_dset,idum)
        ELSE IF (proj_out == "R") THEN
          CALL tll(REAL(xgeo_dset),REAL(ygeo_dset),xrot_out,yrot_out,xx_dset,yy_dset)
        ELSE IF (proj_out == "G") THEN
          xx_dset = REAL(xgeo_dset)
          yy_dset = REAL(ygeo_dset)
        ENDIF

        i_out = NINT((xx_dset - (x1_out+dx_out/2.)) / dx_out) + 1
        j_out = NINT((yy_dset - (y1_out+dy_out/2.)) / dy_out) + 1
        IF (i_out <= 0 .OR. i_out > ni_out .OR. &
            j_out <= 0 .OR. j_out > nj_out) CYCLE
        IF (rig(kc) == dem_miss) THEN
          nsea=nsea+1
          rig(kc) = 0           ! metto a 0 i punti di mare
        ENDIF
        k_out = i_out + (j_out-1) * ni_out
        cnt(k_out) = cnt(k_out) + 1
        orog(k_out) = orog(k_out) + FLOAT(rig(kc))

      ELSE
        DO k = 1,np_out
          IF (rig(kc) == dem_miss) rig(kc) = 0   ! metto a 0 i punti di mare
          ddx = lon_out(k) - xgeo_dset
          ddy = lat_out(k) - ygeo_dset

          IF (ddx >= 0. .AND. ddx < rd .AND. ddy >= 0. .AND. ddy < rd) THEN
            z1(k) = FLOAT(rig(kc))
            xp(k) = ddx * 120.
            yp(k) = ddy * 120.
          ELSE IF (ddx < 0. .AND. ddx >= -rd .AND. ddy >= 0. .AND. ddy < rd) THEN
            z2(k) = FLOAT(rig(kc))
          ELSE IF (ddx < 0. .AND. ddx >= -rd .AND. ddy < 0. .AND. ddy >= -rd) THEN
            z3(k) = FLOAT(rig(kc))
          ELSE IF (ddx >= 0. .AND. ddx < rd .AND. ddy < 0. .AND. ddy >= -rd) THEN
            z4(k) = FLOAT(rig(kc))
          ENDIF
        ENDDO

      ENDIF
    ENDDO
  ENDDO

  CLOSE(31)

ENDDO

!--------------------------------------------------------------------------
! 3) Calcolo orografia

! 3.1) Richiesta media
IF (int_orog == 0) THEN
  WHERE (cnt(:) > 0)
    orog(:) = orog(:) / FLOAT(cnt(:))
  ELSEWHERE
    orog(:) = rmis
  ENDWHERE

! 3.2 Richiesta interpolazione
ELSE
  DO k = 1,np_out
    IF (z1(k)==rmis .OR. z2(k)==rmis .OR. z3(k)==rmis .OR. z4(k)==rmis .or. &
        xp(k)==rmis .OR. yp(k)==rmis) THEN
      orog(k) = rmis
    ELSE
      CALL hbilin (z1(k),z2(k),z3(k),z4(k),0.,0.,1.,1.,xp(k),yp(k),orog(k))
    ENDIF
  ENDDO

ENDIF

! 3.3 Statistiche
usgs_cell = 0.926 * 0.926 * cos((lat2_out+lat1_out)*3.1415/360.)
IF (proj_out == "U") THEN
  out_cell = dx_out * dy_out
ELSE IF (proj_out == "G") THEN  
  out_cell = (dx_out * dy_out * 111.111 * 111.111 * &
    cos((lat2_out+lat1_out)*3.1415/360.))
ELSE IF (proj_out == "R") THEN
  out_cell = (dx_out * dy_out * 111.111 * 111.111 * &
    cos((lat2_out+lat1_out-2*yrot_out)*3.1415/360.))
ENDIF
np_exp = out_cell / usgs_cell
WRITE (*,'(a)') "Letto dataset USGS (EROS). Dati per ciasuna cella: "
WRITE (*,'(2x,2(a,i4),2(a,f7.2))') "max ",np_max," min ",np_min, &
  " ave ",np_ave," attesi in media ",np_exp

nok = COUNT(orog(:) /= rmis)
or_ave = SUM(orog(:), MASK=orog(:)/=rmis) / REAL(nok)
WRITE (*,'(2(a,i7),a,f8.2)') "Celle richieste ",np_out, &
  "  Non attribuite: ",np_out-nok,"  Orog media: ",or_ave

IF (int_orog == 0) THEN
  ntot = SUM(cnt(:))
  np_max = MAXVAL(cnt(:))
  np_min = MINVAL(cnt(:))
  np_ave = REAL(ntot) / REAL(np_out)
  IF (ntot > 0) THEN
    sea_fr = REAL(nsea) / REAL(ntot)
  ELSE
    sea_fr = rmis
  ENDIF
  WRITE (*,'(2x,2(a,i5),2(a,f8.2))') "dati in ciasuna cella: max ", &
    np_max," min ",np_min," ave ",np_ave
  WRITE (*,'(a,f7.2)')  " Frazione di mare: ",sea_fr
ENDIF

!do k=1,np_out
!  if (z1(k)==rmis.OR.z2(k)==rmis.OR.z3(k)==rmis.OR.z4(k)==rmis)then
!  print *,"-------------------"
!  print *,"k,lon,lat,xp,yp",k,lon_out(k),lat_out(k),xp(k),yp(k)
!  print *,"z1234,orog ",z1(k),z2(k),z3(k),z4(k),orog(k)
!  endif
!enddo

RETURN

9999 CONTINUE
WRITE (*,*) "Errore leggendo ",nfile
STOP

END SUBROUTINE read_orog_usgs

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_orog_nasa(orog)
!--------------------------------------------------------------------------
! Dato un grigliato UTM, GEO o ROT, accede al dataset di orograsfia NASA 3"
! e ritorna la quota di ciascuna cella.
!
! Formato del dataset:
! - ciascun file copere un'area di 1 grado x 1 grado (1201 x 1201 punti); il 
!   nome del file contiene le coordinate dell'angolo SW
! - i dati sono scritti come Signed Integer da 16 bit, big endian
! - l'ordine dei punti parte da NW e prosegue lungo le righe
!
! Note:
! - Vengono aperti solo i files necessari, partendo da SW; per evitare 
!   sovrapposizioni, ignoro la prima riga (N) e l'ultima colonna (E) di
!   ciascun file.
! - La lunghezza dei record nella OPEN potrebbe dipendere dal compilatore o
!   anche dalle opzioni di compilazione (es: -assume byterecl)
!--------------------------------------------------------------------------

USE local
IMPLICIT NONE

REAL, INTENT(OUT) :: orog(np_out)

! Parametri del dataset
INTEGER, PARAMETER :: npds = 1201
INTEGER (KIND=2), PARAMETER :: imis_nasa = -32768

! Elenco di files mancanti sul sito USGS (mare aperto)
INTEGER, PARAMETER :: nos = 95
CHARACTER(LEN=7) :: tiles_open_sea(nos) = (/ &
 "N31E018","N32E016","N32E017","N32E018","N32E025","N33E012","N33E013", &
 "N33E014","N33E015","N33E016","N33E017","N33E018","N33E019","N33E020", &
 "N33E021","N33E022","N33E023","N33E024","N33E025","N34E012","N34E013", &
 "N34E014","N34E015","N34E016","N34E017","N34E018","N34E019","N34E020", &
 "N34E021","N34E022","N35E013","N35E015","N35E016","N35E017","N35E018", &
 "N35E019","N35E020","N35E021","N35E021","N35E022","N36W001","N36E013", &
 "N36E016","N36E017","N36E018","N36E019","N36E020","N37E000","N37E001", &
 "N37E002","N37E003","N37E004","N37E005","N37E017","N37E018","N37E019", &
 "N38E002","N38E003","N38E004","N38E005","N38E006","N38E007","N38E010", &
 "N38E011","N38E018","N38E019","N39E005","N39E006","N39E007","N39E010", &
 "N39E011","N39E012","N39E013","N39E014","N40E001","N40E002","N40E005", &
 "N40E006","N40E007","N40E010","N40E011","N41E004","N41E005","N41E006", &
 "N41E007","N41E010","N41E018","N42E004","N42E005","N42E007","N43E014", &
 "N44W003","N45W003","N52E002","N52E003"/)

! Variabili locali
REAL (KIND=kdble) :: lon_out(np_out),lat_out(np_out),xp(np_out),yp(np_out)
REAL (KIND=kdble) :: xgeo_dset,ygeo_dset,xx_dset,yy_dset,ddx,ddy,rd
REAL (KIND=kdble) :: xxd,yyd,ylat,xlon
REAL :: z1(np_out),z2(np_out),z3(np_out),z4(np_out)
REAL :: xx,yy,area_nasa,or_ave,np_ave,np_exp
INTEGER :: i,j,k,ids,jds,cnt(np_out),kr,kc,idum,i_out,j_out,k_out,kos
INTEGER :: nok,np_max,np_min,ntot,ios
INTEGER (KIND=2) :: rig(npds-1)
CHARACTER(LEN=100) :: nfile
CHARACTER(LEN=40) :: ch40
CHARACTER(LEN=4) :: ch4
CHARACTER(LEN=3) :: ch3
LOGICAL :: los

!--------------------------------------------------------------------------
! 1) Preliminari

rd = 1./ DBLE(npds-1)

! 1.1 Se e' richiesta l'interpolazione, calcolo le coordinate georgrafiche 
!     dei punti di output
IF (int_orog == 1) THEN
  DO i = 1,ni_out
  DO j = 1,nj_out
    xx = x1_out + (i-1) * dx_out
    yy = y1_out + (j-1) * dy_out
    k = i + (j-1) * ni_out

    xxd = DBLE(xx)
    yyd = DBLE(yy)
    IF (proj_out == "U") THEN
      CALL utm2ll_dble(xxd,yyd,utmz_out,.FALSE.,ylat,xlon)
    ELSE IF (proj_out == "R") THEN
      CALL rtll_dble(xxd,yyd,DBLE(xrot_out),DBLE(yrot_out),xlon,ylat)
    ELSE IF (proj_out == "G") THEN
      ylat = yyd
      xlon = xxd
    ENDIF
    lat_out(k) = ylat
    lon_out(k) = xlon

  ENDDO
  ENDDO
  z1(:) = rmis
  z2(:) = rmis
  z3(:) = rmis
  z4(:) = rmis
  xp(:) = rmis
  yp(:) = rmis
ENDIF

orog(:) = 0.
cnt(:) = 0.

!--------------------------------------------------------------------------
! 2) Lettura (ciclo sui pezzi del dateset NASA)

DO jds = INT(lat1_out),INT(lat2_out)
DO ids = INT(lon1_out),INT(lon2_out)

! Costruisco il nome del pezzo di dataset richiesto
  IF (jds < 0) THEN
    WRITE (ch3,'(a1,i2.2)') "S",-jds
  ELSE
    WRITE (ch3,'(a1,i2.2)') "N",jds
  ENDIF
  IF (ids < 0) THEN
    WRITE (ch4,'(a1,i3.3)') "W",-ids
  ELSE
    WRITE (ch4,'(a1,i3.3)') "E",ids
  ENDIF

! Se il file richiesto non e' presente sul server NASA (mare aperto), 
! prendo nota
  los = .FALSE.
  DO kos = 1,nos
    IF (ch3//ch4 == tiles_open_sea(kos)) THEN
      los = .TRUE.
      EXIT
    ENDIF
  ENDDO

! apro il file dati
  IF (.NOT. los) THEN
    CALL GETENV(nasa_env,ch40)
    WRITE (nfile,'(7a)') TRIM(ch40),"/",TRIM(nasa_path),"/",ch3,ch4,".hgt"
    WRITE (*,*) "Leggo file ",TRIM(nfile)
    OPEN(31, FILE=nfile, FORM='UNFORMATTED', STATUS='OLD', IOSTAT=ios, &
         ACCESS='DIRECT',CONVERT='BIG_ENDIAN',RECL=npds*2)
    IF (ios /= 0) THEN
      WRITE (*,*) "Errore aprendo ",TRIM(nfile)," scaricare da ftp NASA"
      CYCLE
    ENDIF
  ELSE
    WRITE (*,*) "Richiesto file in mare aperto ",ch3//ch4//".hgt"
  ENDIF
!--------------------------------------------------------------------------
! Leggo il dataset. Se e' richiesto il calcolo medio, sommo i dati che 
! cadono in ciascuna cella di ouput, altrimenti memorizzo per ciscuna cella
! i 4 valori che serviranno per interpolare.
! Se sto elaborando un file mancante (mare aperto) metto tutto a 0.

  DO kr = 2, npds
    IF (.NOT. los) THEN
      READ(31,REC=kr,IOSTAT=ios) rig(1:npds-1)
      IF (ios /= 0) GOTO 9998
    ELSE
      rig(1:npds-1) = 0
    ENDIF
  
    ygeo_dset = DBLE(jds) + DBLE(npds-kr) / DBLE(npds-1)

    DO kc = 1, npds-1
      xgeo_dset = DBLE(ids) + DBLE(kc-1) / DBLE(npds-1)

      IF (int_orog == 0) THEN
 
!       Calcolo la posizione del punto Dset nel grigliato di output
        IF (proj_out == "U") THEN
          CALL ll2utm_dble(ygeo_dset,xgeo_dset,utmz_out,xx_dset,yy_dset,idum)
        ELSE IF (proj_out == "R") THEN
          CALL tll_dble(xgeo_dset,ygeo_dset,DBLE(xrot_out),DBLE(yrot_out),&
            xx_dset,yy_dset)
        ELSE
          xx_dset = xgeo_dset
          yy_dset = ygeo_dset
        ENDIF

        i_out = NINT((REAL(xx_dset) - (x1_out+dx_out/2.)) / dx_out) + 1
        j_out = NINT((REAL(yy_dset) - (y1_out+dy_out/2.)) / dy_out) + 1
        IF (i_out <= 0 .OR. i_out > ni_out .OR. &
            j_out <= 0 .OR. j_out > nj_out) CYCLE

        k_out = i_out + (j_out-1) * ni_out
        IF (rig(kc) /= imis_nasa) THEN
          cnt(k_out) = cnt(k_out) + 1
          orog(k_out) = orog(k_out) + REAL(rig(kc))
        ENDIF

      ELSE
        DO k = 1,np_out
          ddx = lon_out(k) - xgeo_dset
          ddy = lat_out(k) - ygeo_dset

          IF (rig(kc) /= imis_nasa) THEN
            IF (ddx >= 0. .AND. ddx < rd .AND. ddy >= 0. .AND. ddy < rd) THEN
              z1(k) = REAL(rig(kc))
              xp(k) = ddx * DBLE(npds-1)
              yp(k) = ddy * DBLE(npds-1)
            ELSE IF (ddx < 0. .AND. ddx >= -rd .AND. ddy >= 0. .AND. ddy < rd) THEN
              z2(k) = REAL(rig(kc))
            ELSE IF (ddx < 0. .AND. ddx >= -rd .AND. ddy < 0. .AND. ddy >= -rd) THEN
              z3(k) = REAL(rig(kc))
            ELSE IF (ddx >= 0. .AND. ddx < rd .AND. ddy < 0. .AND. ddy >= -rd) THEN
              z4(k) = REAL(rig(kc))
            ENDIF
          ENDIF
        ENDDO

      ENDIF
    ENDDO
  ENDDO

  CLOSE(31)

ENDDO
ENDDO

!--------------------------------------------------------------------------
! 3) Calcolo orografia

! 3.1) Richiesta media
IF (int_orog == 0) THEN
  WHERE (cnt(:) > 0)
    orog(:) = orog(:) / REAL(cnt(:))
  ELSEWHERE
    orog(:) = rmis
  ENDWHERE

! 3.2 Richiesta interpolazione
ELSE
  DO k = 1,np_out
    IF (z1(k)==rmis .OR. z2(k)==rmis .OR. z3(k)==rmis .OR. z4(k)==rmis .or. &
        xp(k)==REAL(rmis) .OR. yp(k)==REAL(rmis)) THEN
      orog(k) = rmis
    ELSE
      CALL hbilin (z1(k),z2(k),z3(k),z4(k),0.,0.,1.,1., &
        REAL(xp(k)),REAL(yp(k)),orog(k))
    ENDIF
  ENDDO

ENDIF

! 3.3 Statistiche
area_nasa = COS((lat2_out+lat1_out)*3.1415/360.) * (111.1 / REAL(npds-1))**2
IF (proj_out == "U") THEN
  np_exp = (dx_out * dy_out) / area_nasa
ELSE IF (proj_out == "R" .OR. proj_out == "G") THEN
  np_exp = COS((lat2_out+lat1_out)*3.1415/360.) * 111.1**2 * dx_out * dy_out / area_nasa
ENDIF

WRITE (*,*) "Letta orografia NASA. Dati attesi per ciscuna cella: ",np_exp

nok = COUNT(orog(:) /= rmis)
or_ave = SUM(orog(:), MASK=orog(:)/=rmis) / REAL(nok)
WRITE (*,'(2(a,i7),a,f8.2)') " Celle richieste ",np_out, &
  "  Non attribuite: ",np_out - nok,"  Orog media: ",or_ave

IF (int_orog == 0) THEN
  ntot = SUM(cnt(:))
  np_max = MAXVAL(cnt(:))
  np_min = MINVAL(cnt(:))
  np_ave = REAL(ntot) / REAL(np_out)
!  IF (ntot > 0) THEN
!    sea_fr = REAL(nsea) / REAL(ntot)
!  ELSE
!    sea_fr = rmis
!  ENDIF
  WRITE (*,'(1x,2(a,i5),2(a,f8.2))') "dati in ciasuna cella: max ", &
    np_max," min ",np_min," ave ",np_ave
!  WRITE (*,'(a,f7.2)')  " Frazione di mare: ",sea_fr
ENDIF

!do k=1,np_out
!  if (z1(k)==rmis.OR.z2(k)==rmis.OR.z3(k)==rmis.OR.z4(k)==rmis)then
!  print *,"-------------------"
!  print *,"k,lon,lat,xp,yp",k,lon_out(k),lat_out(k),xp(k),yp(k)
!  print *,"z1234,orog ",z1(k),z2(k),z3(k),z4(k),orog(k)
!  endif
!enddo

RETURN

!--------------------------------------------------------------------------

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(nfile)," record ",kr
STOP

END SUBROUTINE read_orog_nasa

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_grib(field,id_var,iunit,grb_dir_inc)
!--------------------------------------------------------------------------
! Srcive il campo field in formato GRIB sull'unita iunit (gia' aperta), 
! con codice parametro id_var
!--------------------------------------------------------------------------

USE local
IMPLICIT NONE

REAL, INTENT(IN) :: field(np_out)
INTEGER, INTENT(IN) :: id_var,iunit,grb_dir_inc

! Dichiarazioni per GRIBEX.
INTEGER, PARAMETER :: maxdim = 1000000      ! dimensione massima dei GRIB
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: gribex_field(maxdim)

IF (np_out > maxdim) THEN
  WRITE (*,*) "Dimensinomento GRIB insufficiente, aumentare maxdim"
  RETURN
ENDIF

! Definisco sezione 1
ksec1(1)=200
ksec1(2)=201
ksec1(3)=1
ksec1(4)=255
ksec1(5)=192
ksec1(6)=id_var
ksec1(7)=1
ksec1(8)=0
ksec1(9)=0
ksec1(10)=0
ksec1(11)=1
ksec1(12)=1
ksec1(13)=0
ksec1(14)=0
ksec1(15)=1
ksec1(16)=0
ksec1(17)=0
ksec1(18)=1
ksec1(19)=0
ksec1(20)=0
ksec1(21)=21
ksec1(22:)=0

! Definisco sezione 2
ksec2(2) = ni_out
ksec2(3) = nj_out
ksec2(4) = NINT((y1_out + dy_out/2.) * 1000.)
ksec2(5) = NINT((x1_out + dx_out/2.) * 1000.)
ksec2(7) = NINT((y2_out - dy_out/2.) * 1000.)
ksec2(8) = NINT((x2_out - dx_out/2.) * 1000.)
ksec2(11)=64
ksec2(12)=0
ksec2(15:)=0

IF (proj_out == "U" .OR. proj_out == "G") THEN
  ksec2(1) = 0
  ksec2(13)=-90000
  ksec2(14)=0
ELSE IF (proj_out == "R") THEN
  ksec2(1) = 10
  ksec2(13)=NINT(yrot_out*1000.) - 90000
  ksec2(14)=NINT(xrot_out*1000.)
ENDIF

IF (grb_dir_inc == 0) THEN
  ksec2(6) = 0
  ksec2(9) = 0
  ksec2(10) = 0
ELSE
  ksec2(6) = 128
  ksec2(9) = NINT(dx_out * 1000.)
  ksec2(10) = NINT(dy_out * 1000.)
ENDIF

! Altre sezioni
psec2(:)=0.
ksec3(1) = 0
psec3(2) = rmis
ksec4(1) = ni_out * nj_out
ksec4(2) = 24
ksec4(3:) = 0.
gribex_field(1:np_out) = field(1:np_out)
gribex_field(np_out+1:) = 0.

! Scrivo grib
CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             gribex_field,maxdim,kbuffer,maxdim,klen,'C',kret)
!CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
!  field,np_out,kbuffer,np_out,kword,'C',kret)

CALL PBWRITE(iunit,kbuffer,ksec0(1),kret)

RETURN
END SUBROUTINE write_grib

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
      subroutine utm2ll_dble(x,y,iz,lsohem,rlat,rlon)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET,
! SCRITTA IN DOPPIA PRECISONE
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

IMPLICIT NONE
INTEGER, PARAMETER :: kdble = SELECTED_REAL_KIND(10,9)
REAL (KIND=kdble),PARAMETER :: rtd=180.0/3.141592654

REAL (KIND=kdble), INTENT(IN) :: x,y
INTEGER, INTENT(IN) :: iz
LOGICAL, INTENT(IN) :: lsohem
REAL (KIND=kdble), INTENT(OUT) :: rlat,rlon

! --- Parameter definitions
!      k0        -  scale on central meridian
!      a         -  Clarke 1866 equatorial radius
!      e2        -  squared Clarke 1866 eccentricity
!      ep2       -  (e2/(1.0-e2)
!      false_e   -  false easting
!      rtd       -  radians to degrees conversion

REAL (KIND=kdble), PARAMETER :: k0=0.9996, a=6378206.4, e2=0.00676866     
REAL (KIND=kdble), PARAMETER :: ep2=0.0068148, false_e=500000.0
REAL (KIND=kdble), PARAMETER :: dtr=3.141592654/180.0
REAL (KIND=kdble), PARAMETER :: e1=0.001697916
REAL (KIND=kdble), PARAMETER :: e11=3.0*e1/2.0 - 27.0*e1*e1*e1/32.0
REAL (KIND=kdble), PARAMETER :: e12=21.0*e1*e1/16.0 - 55.0*e1*e1*e1*e1/32.0
REAL (KIND=kdble), PARAMETER :: e13=151.0*e1*e1*e1/96.0
REAL (KIND=kdble), PARAMETER :: e14=1097.0*e1*e1*e1*e1/512.0
REAL (KIND=kdble), PARAMETER :: e4=e2*e2, e6=e2*e4

REAL (KIND=kdble) :: M,N1,l
REAL (KIND=kdble) :: rlon0,xm,u,p1,cosp1,c1,c2,tanp1,t1,t2,sinp1,sin2p1,r0,r1
REAL (KIND=kdble) :: d,d2,d3,d4,d5,d6,p,ym

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

RETURN
END SUBROUTINE utm2ll_dble

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

!----------------------------------------------------------------------
SUBROUTINE ll2utm_dble(rlat,rlon,iz0,x,y,iz)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET,
! SCRITTA IN DOPPIA PRECISONE
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
!               RLAT - Real*8      - N Latitude in decimal degrees
!                                    (use negative for southern hemisphere)
!               RLON - Real*8      - E Longitude in decimal degrees
!                                    (use negative for western hemisphere)
!                IZ0 - Integer     - UTM zone override (used only if
!                                    IZ0 .ne. zero).
!
! --- OUTPUT:
!                  X - Real*8      - UTM easting in km
!                  Y - Real*8      - UTM northing in km
!                 IZ - Integer     - UTM zone
!
! --- LL2UTM called by:  READCF
! --- LL2UTM calls:      none
!----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, PARAMETER :: kdble = SELECTED_REAL_KIND(10,9)

REAL (KIND=kdble), INTENT(IN) :: rlat,rlon
INTEGER, INTENT(IN) :: iz0
REAL (KIND=kdble), INTENT(OUT) :: x,y
INTEGER, INTENT(OUT) :: iz

REAL (KIND=kdble), PARAMETER :: k0=0.9996, a=6378206.4, e2=0.00676866     
REAL (KIND=kdble), PARAMETER :: ep2=0.0068148, false_e=500000.0
REAL (KIND=kdble), PARAMETER :: dtr=3.141592654/180.0
REAL (KIND=kdble) :: n,m,dl,p,sinp,tanp,t,cosp,c,a1,a2,a3,a4,a5,a6,t2,false_n

!----------------------------------------------------------------------

IF (iz0 == 0) THEN
  iz = INT((180.0+REAL(rlon))/6.0) + 1
ELSE
  iz = iz0
ENDIF

! Compute delta longitude in radians
dl = dtr*(rlon - (6.0*DBLE(iz)-183.0))

! Convert phi (latitude) to radians
p = dtr*rlat

sinp = SIN(p)
N = a/SQRT(1.0-e2*sinp*sinp)
tanp = TAN(p)
T = tanp*tanp
cosp = COS(p)
C = ep2*cosp*cosp
A1 = dl*cosp
M = 111132.0894*rlat - 16216.94*SIN(2.0*p) + 17.21*SIN(4.0*p) - 0.02*SIN(6.0*p)

A2 = A1**2
A3 = A2*A1
A4 = A2**2
A5 = A4*A1
A6 = A4*A2
T2 = T**2

! Compute UTM x and y (km)
x = 0.001*(k0*N*(A1+(1.0-T+C)*A3/6.0          &     
  + (5.0-18.0*T+T2+72.0*C-58.0*ep2)*A5/120.0) &
  + false_e)
y = (M+N*tanp * (A2/2.0 + (5.0-T+9.0*C+4.0*C*C)*A4/24.0 &
  + (61.0-58.0*T+T2+600.0*C-330.0*ep2)*A6/720.0))
false_n = 0.

IF (rlat < 0.) THEN
! in km, unlike false_e
  false_n = 10000.
ENDIF
y = 0.001*k0*y + false_n

RETURN
END SUBROUTINE ll2utm_dble

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!-------------------------------------------------------------------------
      SUBROUTINE tll_dble(ALMD,APHD,TLM0D,TPH0D,TLMD,TPHD)        
!-------------------------------------------------------------------------
! trasforma le coordinate geografiche ordinarie (ALMD,APHD) in
! coordinte ruotate (TLMD,TPHD). I/O in gradi e decimi.
! TLM0D, TPH0D: lon e lat del centro di rotazione, in gradi e decimi.
! 
IMPLICIT NONE
INTEGER, PARAMETER :: kdble = SELECTED_REAL_KIND(10,9)
REAL (KIND=kdble),INTENT(IN) :: almd,aphd,tlm0d,tph0d
REAL (KIND=kdble),INTENT(OUT) :: tlmd,tphd

REAL (KIND=kdble), PARAMETER :: dtr=3.141592654/180.0
REAL (KIND=kdble) :: ctph0,stph0,relm,srlm,crlm,aph,sph,cph,cc,anum,denom

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
END SUBROUTINE tll_dble

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!-------------------------------------------------------------------------
      SUBROUTINE rtll(TLMD,TPHD,TLM0D,TPH0D,ALMD,APHD)        
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

!-------------------------------------------------------------------------
SUBROUTINE rtll_dble(tlmd,tphd,tlm0d,tph0d,almd,aphd)        
!-------------------------------------------------------------------------
! trasforma le coordinate ruotate (TLMD,TPHD) in coordinate geografiche
! ordinarie (ALMD,APHD). I/O in gradi e decimi
! Versione f90, doppia precisione.
! TLM0D, TPH0D: lon e lat del centro di rotazione, in gradi e decimi.
! 
IMPLICIT NONE
INTEGER, PARAMETER :: kdble = SELECTED_REAL_KIND(10,9)
REAL (KIND=kdble),INTENT(IN) :: tlmd,tphd,tlm0d,tph0d
REAL (KIND=kdble),INTENT(OUT) :: almd,aphd

REAL (KIND=kdble), PARAMETER :: dtr=3.141592654/180.0
REAL (KIND=kdble) :: ctph0,stph0,stph,ctph,ctlm,stlm,aph,cph

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
END SUBROUTINE rtll_dble  

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE laea2ll (x,y,x0,y0,lon0,lat0,lon,lat)

! Converts Lambert Azimuthal Equal-Area Projection coordinates
! to Lat/Lon coordinates

  IMPLICIT NONE

  REAL(KIND=8), PARAMETER :: radius = 6378137., pi = 3.141592653
  REAL(KIND=8), INTENT(IN) ::  &
       x,              &    ! Lambert x-coordinate
       y,              &    ! Lambert y-coordinate
       x0,             &    ! False easting
       y0,             &    ! False northing
       lon0,           &    ! Central meridian
       lat0                 ! Latitude of origin
  REAL(KIND=8), INTENT(OUT) :: &      
       lon,            &    ! Longitude
       lat                  ! Latitude
  REAL(KIND=8) rho,c,x1,y1,phi,lambda,phi0,lambda0

  y1      = (y - y0) / radius
  x1      = (x - x0) / radius
  rho     = sqrt(x1**2 + y1**2)
  c       = 2 * asin(0.5 * rho)
  lambda0 = lon0 / 180. * pi
  phi0    = lat0 / 180. * pi
  phi     = asin(cos(c) * sin(phi0) + (y1 * sin(c) * cos(phi0)) / rho)
  lambda  = lambda0 + atan((x1 * sin(c)) /   &
           (rho * cos(phi0) * cos(c) - y1 * sin(phi0) * sin(c)))
  lon     = lambda / pi * 180.
  lat     = phi    / pi * 180.

  RETURN
END SUBROUTINE laea2ll

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

	SUBROUTINE hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp,zp)
! **************************************************************
! effettua interpolazione bilineare dati i valori nei punti
! 1,2,3,4 e le coordinate dei punti 1 e 3 oltre a quelle
! del punto p dove viene valutato il campo.
! I punti sono disposti nell'ordine:
! 4 3
! 1 2
!
      REAL :: z1,z2,z3,z4,x1,y1,x3,y3,xp,yp,zp
      REAL :: fc,z5,z6
!
      fc=((yp-y1)/(y3-y1))

      z5=(z4-z1)*fc+z1
      z6=(z3-z2)*fc+z2

      zp=(z6-z5)*((xp-x1)/(x3-x1))+z5

      RETURN
      END SUBROUTINE hbilin


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

SUBROUTINE scrive_help
!
! Visualizza a schermo l'help del programma
!
IMPLICIT NONE

WRITE (*,*) 
WRITE (*,*) "proc_fisiog.exe [-h] [-c]"
WRITE (*,*) " -h      : visualizza questo help"
WRITE (*,*) " -c      : crea file proc_fisiog.inp di esempio"
WRITE (*,*) 

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_esempio
!
! Scrive un file crea_geo_calmet.inp di esempio
!
IMPLICIT NONE

OPEN (UNIT=20, FILE="proc_fisiog.inp", STATUS="REPLACE", &
  FORM="FORMATTED")

!                            1234567890123456789012345678901234567890
WRITE (20,'(a)')            "!Parametri griglia richiesta"
WRITE (20,'(a)')            "UTM           ! Proiezione (UTM o GEO)"
WRITE (20,'(a)')            "90            ! Nx"
WRITE (20,'(a)')            "52            ! Ny"
WRITE (20,'(2a)')           "402.500       ! X1 (km): centro della ce", &
                            "lla (1,1) [SW]" 
WRITE (20,'(2a)')           "4852.500      ! Y1 (km): centro della ce", &
                            "lla (1,1) [SW]" 
WRITE (20,'(2a)')           "847.500       ! X2 (km): centro della ce", &
                            "lla (NX,NY) [NE]" 
WRITE (20,'(2a)')           "5107.500      ! Y2 (km): centro della ce", &
                            "lla (NX,NY) [NE]" 
WRITE (20,'(2a)')           "32            ! UTMZ (solo se proiezione", &
                            " UTM)"
WRITE (20,'(2a)')           "0.0           ! Xrot (=/ 0. solo se coor", &
                            "dinate geografiche ruotate)" 
WRITE (20,'(2a)')           "0.0           ! Yrot (=/ 0. solo se coor", &
                            "dinate geografiche ruotate)" 
WRITE (20,'(2a)')           "citydelta     ! nome area (max 10 char, ", &
                            "solo per nomi files di output)"
WRITE (20,*)
WRITE (20,'(2a)')           "!Dataset da utilizzare (0 = NO, /0 = SI;", &
                            "gia' ordinati per priorita' crescente)"
WRITE (20,'(3a)')           "0             ! Orografia USGS (30",CHAR(34),")"
WRITE (20,'(3a)')           "1             ! Orografia NASA (3",CHAR(34),")"
WRITE (20,'(3a)')           "0             ! Land-use BATS (30",CHAR(34),") NON ATTIVO"
WRITE (20,'(3a)')           "0             ! Land-use USGS (30",CHAR(34),")"
WRITE (20,'(3a)')           "0             ! Land-use JRC. NON ATTIVO"
WRITE (20,'(2a)')           "1             ! Land-use Corine 2000 (10", &
                            "0 m,  solo Italia)"
WRITE (20,*)
WRITE (20,'(2a)')           "!Frazione minima che forza l'assegnazion", &
                            "e urbana di una cella (per ogni Dataset)"
WRITE (20,'(a)')            "0.1           ! Land-use BATS" 
WRITE (20,'(a)')            "0.1           ! Land-use USGS"
WRITE (20,'(a)')            "0.1           ! Land-use JRC"
WRITE (20,'(a)')            "0.35          ! Land-use Corine 2000"
WRITE (20,'(2a)')           "1             ! [Calmet] 0: modifico sol", &
                            "o LU; 1: valori urb. per tutti i param."
WRITE (20,*)
WRITE (20,'(a)')            "!Output (0 = NO, /0 = SI)"
WRITE (20,'(2a)')           "0             ! calcolo orografia (0: me", &
                            "dia, 1: interpolazione lineare)"
WRITE (20,'(2a)')           "1             ! [Calmet] calcolo param. ", &
                            "fisiog. (1: val. annuali, 2: stagionali)"
WRITE (20,'(a)')            "1             ! Grib orografia"
WRITE (20,'(a)')            "1             ! Output calmet (geo.dat)"
WRITE (20,'(a)')            "1             ! Ouptut calmet (grib)"
WRITE (20,'(a)')            "0             ! Ouptut chimere (LANDUSE)"
WRITE (20,'(a)')            "0             ! Output chimere (grib)"
WRITE (20,'(2a)')           "1             ! Output grib con passo gr", &
                            "iglia esplicito (0 = NO, /0 = SI)"
CLOSE (20)
RETURN

END SUBROUTINE scrive_esempio

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

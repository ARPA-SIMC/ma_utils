PROGRAM calc_tpi
!--------------------------------------------------------------------------
! Programma per classificare dal punto di vista orografico una serie di 
! punti sparsi o su griglia. 
! Utilizza il "Topographic Position Index" calcolato con due raggi 
! d'influenza diversi, come da documentazione dell'estensione TPI di 
! ArcView
!
! Accede al datest di orografia NASA: 
!   globale, lat-lon, risoluzione 3" (90 m), diviso in elementi di 1 grado
!   x 1 grado (nomi files: N??E???.hgt). Formato: INTEGR*2, big-endian, 
!   punti ordinati dall'angolo NW, il nome file individua l'angolo SE;
!   dati disponibli su: ftp://e0srp01u.ecs.nasa.gov/srtm/version2/SRTM3
!
! Note:
! Con gli attuali parametri di default, il programma sembra funzionare 
! abbastanza bene, con due avvertenze:
! - i punti in pianura ai piedi delle colline sono classificati 
!   "collina incassata"
! - i valichi sono spesso classificati "montagna aperta" (questo punto
!   potrewbbe essere risolto solo calcolando TPI direzionale, il che 
!   implica una sostanziale riscrittura del programma)
!
!                                         Versione 1.3.2, Enrico 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Costanti
INTEGER, PARAMETER :: max_pts = 1000
INTEGER, PARAMETER :: max_grb = 2000000
REAL, PARAMETER :: dtr = 3.1415926 / 180.
REAL, PARAMETER :: rmis = -9999.

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(max_grb), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(max_grb)

! Parametri per calcolo TPI e classificazione orografica
REAL :: r_small,r_big,arw_tpi,arw_std,trs_calc,ref_lat
REAL :: sim_avep,sim_stdp,sim_avec,sim_stdc
REAL :: sim_trs_hs,sim_trs_hb,sim_trs_ms,sim_trs_mb

! Parametri griglia richiesta
REAL :: x1_grd,y1_grd,x2_grd,y2_grd,xrot_grd,yrot_grd,dx_grd,dy_grd
INTEGER :: ni_grd,nj_grd,utmz_grd
CHARACTER (LEN=1) :: proj_grd

! Altre variabili del programma
REAL, ALLOCATABLE :: ave_small(:),std_small(:),tpi_small(:)
REAL, ALLOCATABLE :: ave_big(:),std_big(:),tpi_big(:)
REAL, ALLOCATABLE :: tpi_min(:)
INTEGER, ALLOCATABLE :: class_arw(:), class_sim(:)
REAL :: orog_2x2(2401,2401)
REAL :: lon_stz(max_pts),lat_stz(max_pts)
REAL :: lon_dum,lat_dum,lon_req,lat_req,quota_req
REAL :: sum_small,sum2_small,sum_big,sum2_big
REAL :: km_dot_lon,km_dot_lat,r_small2,r_big2,dd2,std2
INTEGER :: net_stz(max_pts),usr_stz(max_pts),quota_stz(max_pts)
INTEGER :: ir_small,jr_small,ir_big,jr_big
INTEGER :: np_req,cnt_par,cnt_print,cnt_small,cnt_big,nrq_big,nrq_small
INTEGER :: ios,eof,eor,net_dum,usr_dum,quota_dum,iuin,iuout,iuext
INTEGER :: kp,kr,i,j,k,ii_nasa,jj_nasa,ii_req,jj_req,i_req,j_req
CHARACTER (LEN=200) :: charg,file_out,file_in,file_ext,chrec
CHARACTER (LEN=20) :: nome_stz(max_pts),nome_dum,sim_txt(0:4)
CHARACTER (LEN=3) :: inp
CHARACTER (LEN=1) :: next_arg
LOGICAL, ALLOCATABLE :: mask_small(:,:),mask_big(:,:)
LOGICAL :: mask_2x2_small(2401,2401),mask_2x2_big(2401,2401)

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 parametri da riga comandi

inp="nil"
next_arg = ""
cnt_par = 0

DO kp = 1,HUGE(0)
  CALL getarg(kp,charg)

  IF (charg == "") THEN
    EXIT
 ELSE IF (TRIM(charg) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(charg) == "-c") THEN
    CALL scrive_esempio
    STOP
  ELSE IF (TRIM(charg) == "-grd") THEN
    inp = "grd"
  ELSE IF (TRIM(charg) == "-stz") THEN
    inp = "stz"
  ELSE
    cnt_par = cnt_par + 1
    IF (cnt_par == 1) THEN
      file_in = charg
    ELSE IF (cnt_par == 2) THEN
      file_out = charg
    ENDIF
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 1.2 Leggo calc_tpi.inp

OPEN (UNIT=21, FILE="calc_tpi.inp", STATUS="OLD", &
  ACTION="READ", IOSTAT=ios)

  IF (ios /= 0) THEN
    WRITE (*,*) "File calc_tpi.inp non trovato, uso default"
    r_small = 1.
    r_big = 8.
    trs_calc = 0.5
    ref_lat = 45.
    arw_tpi = 1.
    arw_std = 20.
    sim_avep = 300.
    sim_stdp = 50.
    sim_avec = 500.
    sim_stdc = 250.
    sim_trs_hs = -0.2
    sim_trs_hb = -0.2
    sim_trs_ms = 0.
    sim_trs_mb = 0.

  ELSE
    kr = 0
    DO
      READ (21,'(a)',IOSTAT=ios) chrec      
      IF (ios /= 0) EXIT
      IF (TRIM(chrec) == "" .OR. INDEX(chrec,"!") == 1) CYCLE
      kr = kr + 1
      SELECT CASE (kr)
      CASE(1)
        READ (chrec,*,ERR=9999) r_small
      CASE(2)
        READ (chrec,*,ERR=9999) r_big
      CASE(3)
        READ (chrec,*,ERR=9999) trs_calc
      CASE(4)
        READ (chrec,*,ERR=9999) ref_lat
      CASE(5)
        READ (chrec,*,ERR=9999) arw_tpi
      CASE(6)
        READ (chrec,*,ERR=9999) arw_std
      CASE(7)
        READ (chrec,*,ERR=9999) sim_avep
      CASE(8)
        READ (chrec,*,ERR=9999) sim_stdp
      CASE(9)
        READ (chrec,*,ERR=9999) sim_avec
      CASE(10)
        READ (chrec,*,ERR=9999) sim_stdc
      CASE(11)
        READ (chrec,*,ERR=9999) sim_trs_hs
      CASE(12)
        READ (chrec,*,ERR=9999) sim_trs_hb
      CASE(13)
        READ (chrec,*,ERR=9999) sim_trs_ms
      CASE(14)
        READ (chrec,*,ERR=9999) sim_trs_mb
      END SELECT
    ENDDO
    IF (kr /= 14) WRITE (*,*) "Warning: file calc_tpi.inp incompleto"

  ENDIF

CLOSE(21)

IF (r_small <= 0. .OR. r_big <= 0. .OR. &
    trs_calc < 0. .OR. trs_calc > 1. .OR. &
    ref_lat < 0. .OR. ref_lat >= 90. .OR. &
    arw_tpi < 0. .OR. arw_std < 0.) GOTO 9992

!--------------------------------------------------------------------------
! 1.3 Con parametro -stz, leggo l'elenco dei punti richiesti

IF (inp == "stz") THEN

  CALL get_eof_eor(eof,eor)
  
  OPEN (UNIT=40, FILE=file_in, STATUS="OLD",ACTION="READ", ERR=9998)
  
  np_req = 0
  DO 
    READ (40,'(a)',IOSTAT=ios) chrec
    IF (ios == eof) EXIT
  
    READ (chrec,'(i4,1x,i6,8x,f8.3,1x,f7.3,1x,i5,1x,a20)',IOSTAT=ios) &
      net_dum,usr_dum,lon_dum,lat_dum,quota_dum,nome_dum
    IF (ios /= 0) THEN
      WRITE (*,*) "Skip record in ",TRIM(file_in),": ",chrec(1:20)
      CYCLE
    ENDIF  

    np_req = np_req + 1
    IF (np_req > max_pts) GOTO 9996
   
    net_stz(np_req) = net_dum
    usr_stz(np_req) = usr_dum
    lon_stz(np_req) = lon_dum
    lat_stz(np_req) = lat_dum
    quota_stz(np_req) = REAL(quota_dum)
    nome_stz(np_req) = nome_dum
  ENDDO

  WRITE (*,'(a,i6)') "Richiesto calcolo TPI su punti sparsi: ", np_req
  
!--------------------------------------------------------------------------
! 1.4 Con parametro -grd leggo il campo di orografia

ELSE IF (inp == "grd") THEN

  CALL grsvck(0)
  CALL PBOPEN (iuin,file_in,'R',kret)
  IF (kret /= 0) GOTO 9998
  
  CALL PBGRIB(iuin,kbuffer,max_grb*4,klen,kret)
  IF (kret < 0) GOTO 9997
  
  psec3(2) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,max_grb,kbuffer,max_grb,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
  IF (ksec2(11) /= 64) GOTO 9995
  IF (ksec1(6) /= 8) GOTO 9994
 
  ni_grd = ksec2(2)
  nj_grd = ksec2(3)
  x1_grd = REAL(ksec2(5)) / 1000.
  x2_grd = REAL(ksec2(8)) / 1000.
  y1_grd = REAL(ksec2(4)) / 1000.
  y2_grd = REAL(ksec2(7)) / 1000.
  xrot_grd = REAL(ksec2(14)) / 1000.
  yrot_grd = 45. + (REAL(ksec2(13))/1000.)
  
  IF (ksec2(1) == 0 .AND. y1_grd > 100.) THEN
    proj_grd = "U"
    utmz_grd = 32
  ELSE IF (ksec2(1) == 0) THEN
    proj_grd = "G"
  ELSE IF (ksec2(1) == 10) THEN
    proj_grd = "R"
  ELSE 
    GOTO 9993
  ENDIF
  
  dx_grd = (x2_grd - x1_grd) / REAL (ni_grd - 1)
  dy_grd = (y2_grd - y1_grd) / REAL (nj_grd - 1)
  np_req = ni_grd * nj_grd

  WRITE (*,'(4a,i6)') "Richiesto calcolo TPI su griglia: ",&
    " proiezione ",proj_grd," punti",np_req

ENDIF

!--------------------------------------------------------------------------
! 1.5 Calcolo le maschere relative all'area d'influenza locale ed estesa
!
! - mask_small e mask_big sono centrate nel punto da esaminare, e sono .T.
!   nei punti la cui distanza dal punto centrale e' minore del raggio
!   d'influenza.
! - controllo che il raggio d'influenza sia < 1° (altrimenti non e'
!   sufficiente leggere 4 tiles del dataset NASA)

km_dot_lat = (40000/360.) / 1200.
km_dot_lon = COS(ref_lat * dtr) * km_dot_lat

ir_small = NINT(r_small/km_dot_lon)
jr_small = NINT(r_small/km_dot_lat)
ir_big = NINT(r_big/km_dot_lon)
jr_big = NINT(r_big/km_dot_lat)
IF (ir_big > 1200) THEN
  WRITE (*,*) "Errore, raggio d'influenza esteso troppo grande"
  WRITE (*,*) "Valore max ammesso (con ref_lat = ",ref_lat," ): ",&
    1200*km_dot_lon
  STOP
ENDIF

ALLOCATE (mask_small(-ir_small:ir_small,-jr_small:jr_small))
ALLOCATE (mask_big(-ir_big:ir_big,-jr_big:jr_big))

mask_small(:,:) = .FALSE.
r_small2 = r_small**2
DO i = -ir_small, ir_small
DO j = -jr_small, jr_small
  dd2 = (REAL(i)*km_dot_lon)**2 + (REAL(j)*km_dot_lon)**2
  IF (dd2 < r_small2) mask_small(i,j) = .TRUE.
ENDDO
ENDDO

mask_big(:,:) = .FALSE.
r_big2 = r_big**2
DO i = -ir_big, ir_big
DO j = -jr_big, jr_big
  dd2 = (REAL(i)*km_dot_lon)**2 + (REAL(j)*km_dot_lon)**2
  IF (dd2 < r_big2) mask_big(i,j) = .TRUE.
ENDDO
ENDDO

WRITE (*,'(a,i6,a,i6,a)') "Punti nelle aree d'influenza: locale ", &
  COUNT(mask_small)," estesa ",COUNT(mask_big)

nrq_big = NINT(trs_calc * REAL(COUNT(mask_big)))
nrq_small = NINT(trs_calc * REAL(COUNT(mask_small)))

!==========================================================================
! 2) Elaborazioni (ciclo sui punti richiesti)
!
! L'array orog contiene i dati di orografia relativi all'area 2° x 2° a
!   attorno al punto richiesto (i.e. i dati contenuti in 4 files del 
!   dataset)
! ii_nasa e jj_nasa sono le coordinate geografiche dell'angolo SW dell'area
!   2°x2° attualmente in memoria
! i_req e j_req sono gli indici del punto del dataset NASA piu' vicino alle
!   coordinate richieste, relativi alla sottogriglia 2°x2°

ALLOCATE (ave_small(np_req),std_small(np_req),tpi_small(np_req))
ALLOCATE (ave_big(np_req),std_big(np_req),tpi_big(np_req))
ALLOCATE (tpi_min(np_req))
ALLOCATE (class_arw(np_req),class_sim(np_req))

ii_nasa = -999
jj_nasa = -999

cnt_print = 1
DO k = 1, np_req
!DO k = 1479, 1479

!--------------------------------------------------------------------------
! 2.1 Trovo le coordinate geografiche e la quota del punto richiesto

  IF (inp == "grd") THEN
    j = (k-1)/ni_grd + 1
    i = MOD(k-1,ni_grd) + 1
    lon_dum = x1_grd + REAL(i-1)*dx_grd
    lat_dum = y1_grd + REAL(j-1)*dy_grd
    IF (proj_grd == "G") THEN
      lon_req = lon_dum
      lat_req = lat_dum
    ELSE IF (proj_grd == "R") THEN
      CALL rtll(lon_dum,lat_dum,xrot_grd,yrot_grd,lon_req,lat_req)
    ELSE IF (proj_grd == "U") THEN
      CALL utm2ll(lon_dum,lat_dum,utmz_grd,.FALSE.,lat_req,lon_req)
    ENDIF
    quota_req = field(k)

  ELSE IF (inp == "stz") THEN
    lon_req = lon_stz(k)
    lat_req = lat_stz(k)
    quota_req = quota_stz(k)

  ENDIF

!--------------------------------------------------------------------------
! 2.2 Leggo i dati di orografia che cadono nell'area di influenza
!
  ii_req = NINT(lon_req) - 1
  jj_req = NINT(lat_req) - 1
  IF (ii_req /= ii_nasa .OR. jj_req /=jj_nasa) THEN
    CALL read_nasa(ii_req,jj_req,rmis,orog_2x2)    
    ii_nasa = ii_req
    jj_nasa = jj_req
  ENDIF

!--------------------------------------------------------------------------
! 2.3 Calcolo TPI e STD
!

! Punto piu' vicino nel dataset NASA
  i_req = NINT((lon_req - REAL(ii_req)) * 1200.) + 1
  j_req = NINT((lat_req - REAL(jj_req)) * 1200.) + 1

! Punti dell'area 2x2 negli intorni del punto richiesto
  mask_2x2_small(:,:) = .FALSE.
  mask_2x2_small(i_req-ir_small:i_req+ir_small, j_req-jr_small:j_req+jr_small) = &
    mask_small(:,:)
  mask_2x2_big(:,:) = .FALSE.
  mask_2x2_big(i_req-ir_big:i_req+ir_big, j_req-jr_big:j_req+jr_big) = &
    mask_big(:,:)

! TPI e STD, intorno locale
  sum_small = SUM(orog_2x2(:,:), MASK = (mask_2x2_small .AND. orog_2x2(:,:)/=rmis))
  sum2_small = SUM(orog_2x2(:,:)**2, MASK = (mask_2x2_small .AND. orog_2x2(:,:)/=rmis))
  cnt_small = COUNT(mask_2x2_small .AND. orog_2x2(:,:)/=rmis)

  IF (cnt_small >= nrq_small) THEN
    ave_small(k) = REAL(sum_small)/REAL(cnt_small)
    std2 = REAL(sum2_small)/REAL(cnt_small) - ave_small(k)**2
    std_small(k) = SQRT(MAX(std2,0.))
    IF (std_small(k) > 0.) THEN
      tpi_small(k) = (quota_req - ave_small(k)) / std_small(k)
    ELSE
      tpi_small(k) = 0.
    ENDIF
  ELSE    
    ave_small(k) = rmis
    std_small(k) = rmis
    tpi_small(k) = rmis
  ENDIF

! TPI e STD, intorno esteso
  sum_big = SUM(orog_2x2(:,:), MASK = (mask_2x2_big .AND. orog_2x2(:,:)/=rmis))
  sum2_big = SUM(orog_2x2(:,:)**2, MASK = (mask_2x2_big .AND. orog_2x2(:,:)/=rmis))
  cnt_big = COUNT(mask_2x2_big .AND. orog_2x2(:,:)/=rmis)

  IF (cnt_big > nrq_big) THEN
    ave_big(k) = REAL(sum_big)/REAL(cnt_big)
    std2 = REAL(sum2_big)/REAL(cnt_big) - ave_big(k)**2
    std_big(k) = SQRT(MAX(std2,0.))
    IF (std_big(k) > 0.) THEN
      tpi_big(k) = (quota_req - ave_big(k)) / std_big(k)
    ELSE
      tpi_big(k) = 0.
    ENDIF
  ELSE    
    ave_big(k) = rmis
    std_big(k) = rmis
    tpi_big(k) = rmis
  ENDIF

! chiudo ciclo sui punti richiesti
  IF (np_req < 2000) THEN
    IF (k > cnt_print*(np_req/20)) THEN
      WRITE (*,*) "Elaborato ",NINT(100.*REAL(k)/REAl(np_req))  ," % dei punti"
      cnt_print = cnt_print + 1
    ENDIF
  ELSE 
    IF (MOD(k,100) == 0) WRITE (*,'(a,f7.3,a)') &
      "Elaborato ",100.*REAL(k)/REAl(np_req)  ," % dei punti"
  ENDIF

ENDDO

!==========================================================================
! 3) Classificazione punti secondo ArcView e secondo SIM
!
! Clacolo la classificazione orografica dei punti richiesti in due modi:
! - Arcview (10 categorie):
!   0=pianura, 1=Canyon, 2=shallow valley, 3=testata valle, 4=valle a U,
!   5=pendio aperto, 6=alpopiano, 7=collina in valle, 
!   8=collina in pianura/pendio, 9=cresta/vetta
!
! - SIM (5 categorie)
!   0=pianura, 1=collina incassata, 2=collina aperta, 
!   3=montagna incassata, 4=montagna aperta
!   Metodo: uso AVE e STD raggio esteso per classificare il punto come 
!     pianura/collina/montagna (e' pianura se entrambe sono < soglia pian.;
!     in caso contrario, e' collina se entrambe sono < soglia collina)
!   Defionisco "aperti" i punti di montagna e collina in cui TPI su 
!     entrambi i  raggi e' < della corrispondente soglia.
!
! L'indicatore tpi_min e' il minimo tra tpi_small e tpi_big, forzato a 0 
! nei punti che risultano "pianura" nella classificazione SIM

sim_txt(0) = "Pianura"
sim_txt(1) = "Collina incassata"
sim_txt(2) = "Collina aperta"
sim_txt(3) = "Montagna incassata"
sim_txt(4) = "Montagna aperta"

class_arw(:) = -9

DO k = 1,np_req
  IF (tpi_small(k) < -arw_tpi) THEN 
    IF (tpi_big(k) < -arw_tpi) THEN
      class_arw(k) = 1                      ! Canyon
    ELSE IF (tpi_big(k) <= arw_tpi) THEN
      class_arw(k) = 2                      ! Shallow valley, midslope drainages
    ELSE
      class_arw(k) = 3                      ! Headwaters, upland drainages
    ENDIF
  ELSE IF (tpi_small(k) <= arw_tpi) THEN 
    IF (tpi_big(k) < -arw_tpi) THEN
      class_arw(k) = 4                      ! U-shaped valley
    ELSE IF (tpi_big(k) <= arw_tpi) THEN
      IF (std_big(k) > arw_std) THEN
        class_arw(k) = 5                    ! Open slope
      ELSE
        class_arw(k) = 0                    ! Plain
      ENDIF
    ELSE
      class_arw(k) = 6                      ! Upper slope, mesas
    ENDIF
  ELSE
    IF (tpi_big(k) < -arw_tpi) THEN      
      class_arw(k) = 7                      ! Hills in valleys
    ELSE IF (tpi_big(k) <= arw_tpi) THEN
      class_arw(k) = 8                      ! Midslope ridge, hills in plain
    ELSE
      class_arw(k) = 9                      ! Mountain top, high ridges
    ENDIF
  ENDIF
ENDDO

class_sim(:) = -9
DO k = 1,np_req
  IF (ave_big(k) < sim_avep .AND. std_big(k) < sim_stdp) THEN
    class_sim(k) = 0                        ! Pianura

  ELSE IF (ave_big(k) < sim_avec .AND. std_big(k) < sim_stdc) THEN
    IF (tpi_big(k) > sim_trs_hb .AND. tpi_small(k) > sim_trs_hs) THEN
      class_sim(k) = 2                      ! Collina aperta
    ELSE
      class_sim(k) = 1                      ! Collina incassata
    ENDIF

  ELSE
    IF (tpi_big(k) > sim_trs_mb .AND. tpi_small(k) > sim_trs_ms) THEN
      class_sim(k) = 4                      ! Montagna aperta
    ELSE
      class_sim(k) = 3                      ! Montagna incassata
    ENDIF
  ENDIF
ENDDO

WHERE (class_sim(:) == 0)
  tpi_min(:) = 0.
ELSEWHERE
  tpi_min(:) = MIN(tpi_small(:),tpi_big(:)) 
ENDWHERE

!==========================================================================
! 4) Scrittura output
! - Scrivo su file_out la classificazione secondo SIM; 
! - Scrivo su calc_tpi_extra.* la classificazione Arcview oltre a media, 
!   STD e TPI relativi ai due raggi d'influenza.

! 4.1) Richiesti dati su punti sparsi
IF (inp == "stz") THEN
  file_ext = "calc_tpi_ext.dat"
  OPEN (UNIT=50, FILE=file_ext, STATUS="REPLACE", FORM="FORMATTED")
  WRITE (50,'(a,5x,a)') "Nt IDusr Nome                quota", &
    "CLA CLS    ave_big std_big tpi_big ave_sma std_sma tpi_sma tpi_min"
  DO k = 1,np_req
    WRITE (50,'(i2.2,1x,i5.5,1x,a20,1x,i4,5x,i1,3x,i1,5x,7(1x,f7.2))') &
      net_stz(k),usr_stz(k),nome_stz(k),quota_stz(k), &
      class_arw(k),class_sim(k),ave_big(k),std_big(k),tpi_big(k), &
      ave_small(k),std_small(k),tpi_small(k),tpi_min(k)
  ENDDO
  CLOSE (50)

  OPEN (UNIT=51, FILE=file_out, STATUS="REPLACE", FORM="FORMATTED")
  WRITE (51,'(a,5x,a)') "Nt IDusr Nome                quota","CLS"
  DO k = 1,np_req
    WRITE (51,'(i2.2,1x,i5.5,1x,a20,1x,i4,5x,i1,3x,a)') &
      net_stz(k),usr_stz(k),nome_stz(k),quota_stz(k),class_sim(k),sim_txt(class_sim(k))
  ENDDO
  CLOSE (51)

! 4.1) Richiesti dati su griglia
ELSE IF (inp == "grd") THEN

! Campi base e classificazione ArcView
  file_ext = "calc_tpi_ext.grb"
  CALL PBOPEN(iuext,file_ext,'W',kret)
  DO kp = 1,8
    ksec1(1) = 200
    ksec1(6) = 130+kp

    SELECT CASE (kp)
    CASE (1) 
      field(1:np_req) = ave_big(1:np_req)
    CASE (2) 
      field(1:np_req) = std_big(1:np_req)
    CASE (3) 
      field(1:np_req) = tpi_big(1:np_req)
    CASE (4) 
      field(1:np_req) = ave_small(1:np_req)
    CASE (5) 
      field(1:np_req) = std_small(1:np_req)
    CASE (6) 
      field(1:np_req) = tpi_small(1:np_req)
    CASE (7) 
      field(1:np_req) = REAL(class_arw(1:np_req))
    CASE (8) 
      field(1:np_req) = tpi_min(1:np_req)
    END SELECT

    IF (ANY(field(1:np_req) == rmis)) THEN
      psec3(2) = rmis
      ksec1(5) = 192
    ENDIF

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,max_grb,kbuffer,max_grb,klen,'C',kret)
    CALL PBWRITE(iuext,kbuffer,ksec0(1),kret)
  ENDDO  

! Classificazione SIM
  CALL PBOPEN(iuout,file_out,'W',kret)
  ksec1(1) = 200
  ksec1(6) = 130+kp
  field(1:np_req) = REAL(class_sim(1:np_req))
  IF (ANY(field(1:np_req) == rmis)) THEN
    psec3(2) = rmis
    ksec1(5) = 192
  ENDIF

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,max_grb,kbuffer,max_grb,klen,'C',kret)
  CALL PBWRITE(iuout,kbuffer,ksec0(1),kret)

ENDIF
STOP

!==========================================================================
! 4) Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "Errore leggendo calc_tpi.inp"
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_in)
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_in)
STOP

9996 CONTINUE
WRITE (*,*) "Troppi punti in ",TRIM(file_in)," aumentare max_pts"
STOP

9995 CONTINUE
WRITE (*,*) "Scanning mode non gestito: ",ksec2(11)
STOP

9994 CONTINUE
WRITE (*,*) TRIM(file_in)," non contiene un grib di orografia"
STOP

9993 CONTINUE
WRITE (*,*) "Proiezione geografica non gestita: ksec2(1) = ",ksec2(1)
STOP

9992 CONTINUE
WRITE (*,*) "Parametri illegali in calc_tpi.inp"

END PROGRAM calc_tpi

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_nasa(ii_req,jj_req,rmis,orog_2x2)
!--------------------------------------------------------------------------
! Accede al dataset di orograsfia NASA 3" e ritorna i valori di un quadrato
! 2° x 2°, il cui angolo SW ha coordinate: lon=ii_req, lat=jj_req
!
! Formato del dataset:
! - ciascun file copere un'area di 1 grado x 1 grado (1201 x 1201 punti); il 
!   nome del file contiene le coordinate dell'angolo SW
! - i dati sono scritti come Signed Integer da 16 bit, big endian
! - l'ordine dei punti parte da NW e prosegue lungo le righe
!
! NB
! - La lunghezza dei record nella OPEN potrebbe dipendere dal compilatore o
!   anche dalle opzioni di compilazione (es: -assume byterecl)
!--------------------------------------------------------------------------

IMPLICIT NONE

INTEGER, INTENT(IN) :: ii_req,jj_req
REAL, INTENT(IN) :: rmis
REAL, INTENT(OUT) :: orog_2x2(2401,2401)

! Parametri relativi al dataset NASA
INTEGER, PARAMETER :: npds = 1201
INTEGER (KIND=2), PARAMETER :: imis_nasa = -32768
CHARACTER (LEN=40), PARAMETER :: nasa_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: nasa_path = "fisiog/dataset/dem_nasa"

! Variabili locali
INTEGER (KIND=2) :: rig(npds)
INTEGER :: i,j,ids,jds,ios,kr,npdsm1
CHARACTER (LEN=120) :: nfile
CHARACTER (LEN=40) :: ch40
CHARACTER (LEN=4) :: ch4
CHARACTER (LEN=3) :: ch3

!--------------------------------------------------------------------------

npdsm1 = npds - 1
orog_2x2(:,:) = rmis
DO i = 0,1
DO j = 0,1

! Apro il file dati
  ids = ii_req + i
  jds = jj_req + j

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

  CALL GETENV(nasa_env,ch40)
  WRITE (nfile,'(7a)') TRIM(ch40),"/",TRIM(nasa_path),"/",ch3,ch4,".hgt"
  OPEN(31, FILE=nfile, FORM='UNFORMATTED', STATUS='OLD', IOSTAT=ios, &
       ACCESS='DIRECT',CONVERT='BIG_ENDIAN',RECL=npds*2)
  IF (ios /= 0) THEN
    WRITE (*,*) "Errore aprendo ",TRIM(nfile)
    CYCLE
  ENDIF

! Leggo il dataset
  DO kr = 1, npds
    READ(31,REC=kr,IOSTAT=ios) rig(1:npds-1)
    IF (ios /= 0) GOTO 9998

    WHERE (rig(:) /= imis_nasa)
      orog_2x2(npdsm1*i+1:npdsm1*(i+1)+1, j*(npds-1)+npds-kr+1) = REAL(rig(:))
    ELSEWHERE
      orog_2x2(npdsm1*i+1:npdsm1*(i+1)+1, j*(npds-1)+npds-kr+1) = rmis
    ENDWHERE
  ENDDO

ENDDO
ENDDO

RETURN

!--------------------------------------------------------------------------

9998 CONTINUE
WRITE (*,*) "Errore leggegndo ",TRIM(nfile)," record ",kr
STOP

END SUBROUTINE read_nasa

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
WRITE (*,*) "calc_tpi.exe [-h] [-c] -stz/-grd file_in file_out"
WRITE (*,*) " -h      : visualizza questo help"
WRITE (*,*) " -c      : crea file calc_tpi.inp di esempio (consente di modifciare"
WRITE (*,*) "           i parametri della classficazione dei punti)"
WRITE (*,*) " file_in : con opzione -stz, lista di punti formato db_anagrafica,"
WRITE (*,*) "           con opzione -grd, campo di orografia in formato GRIB"
WRITE (*,*) " file_out: con opzione -stz in formato ASCII, altrimenti GRIB"
WRITE (*,*) ""

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_esempio
!
! Scrive un file calc_tpi.inp di esempio
!
IMPLICIT NONE

OPEN (UNIT=20, FILE="calc_tpi.inp", STATUS="REPLACE", &
  FORM="FORMATTED")

!                            1234567890123456789012345678901234567890
WRITE (20,'(2a)')           "1.            ! Raggio d'influenza local", &
                            "e (km)"
WRITE (20,'(2a)')           "8.            ! Raggio d'influenza estes", &
                            "o (grandi valli; km)"
WRITE (20,'(2a)')           "0.5           ! Frazione minima di dati ", &
                            "validi nell'intorno per calcolare TPI"
WRITE (20,'(2a)')           "45.           ! Latitudine di riferiment", &
                            "o (dove l'area d'influenza e' circolare)"
WRITE (20,*)
WRITE (20,'(a)')            "! Parametri per calcolo classificazione Arcview"
WRITE (20,'(2a)')           "1.            ! Valore assoluto soglie p", &
                            "er TPI normalizzato (DLT/STD)"
WRITE (20,'(2a)')           "20.           ! Soglia STD raggio esteso", &
                            " per distingure pianura e pendii (m)"
WRITE (20,*)
WRITE (20,'(a)')            "! Parametri per calcolo classificazione SIM"
WRITE (20,'(2a)')           "300.          ! Soglia max AVE raggio es", &
                            "teso per definire pianura (m)"
WRITE (20,'(2a)')           "50.           ! Soglia max STD raggio es", &
                            "teso per definire pianura (m)"
WRITE (20,'(2a)')           "500.          ! Soglia max AVE raggio es", &
                            "teso per definire collina (m)"
WRITE (20,'(2a)')           "250.          ! Soglia max STD raggio es", &
                            "teso per definire collina (m)"
WRITE (20,'(2a)')           "-0.2          ! Soglia min TPI raggio lo", &
                            "cale per definire collina aperta"
WRITE (20,'(2a)')           "-0.2          ! Soglia min TPI raggio es", &
                            "teso per definire collina aperta"
WRITE (20,'(2a)')           "0.            ! Soglia min TPI raggio lo", &
                            "cale per definire montagna aperta"
WRITE (20,'(2a)')           "0.            ! Soglia min TPI raggio es", &
                            "teso per definire montagna aperta"
CLOSE (20)
RETURN

END SUBROUTINE scrive_esempio

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

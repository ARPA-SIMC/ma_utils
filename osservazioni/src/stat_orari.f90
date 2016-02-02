PROGRAM stat_orari
!--------------------------------------------------------------------------
! Programma per calcolare una serie di statistiche a partire da una serie
! storica su un punto (stazione o modello).
! Scrive 4 coppie di files dat/ctl per grads (ggtyp, daily, month, hours) 
! e una serie di files in formato testo, ciascuno relativo a una statistica:
! - statistiche base  (stats): ndati,ave,max,min,std
! - distr.di frequenza(dfreq): n.ro dati con valori raggruppati in interv.
! - giorno tipo       (ggtyp): per ogni ora del giorno: ave, max, min, nok
! - anno tipo         (yrtyp): per ogni mese dell'anno: ave, max, min, nok
! - medie giornaliere (daily): per ogni giorno: ave, max, min, nok
! - medie mensili     (month): per ogni mese: ave, max, min, nok
! - medie stagionali  (SE???): per ogni stagione: ave, max, min, nok
! - rose dei venti    (wrose): dati per intensita' e quaxdrante
!
!
! Note:
! - Dalla versione 9, il tracciato dei files .sta con serie storiche
!   (daily, month, SE***) diventa uguale a quello di estraqa con dati
!   giornalieri. I nuovi files .sta sono la concatenazione di 5 estraqa-day
!   (ave, max, min, nox, extra), separati da 2 record vuoti; il record dei
!   livelli (se presente) e' il 2o di intestazione.
! - Dalla versione 10 e' stata implementata la possibiliita' di aggiungere 
!   ad alcuni files sta un 5o blocco con un'elaborazione extra, richiesta
!   da riga comando (es: rank; mdoficare anche split_sta.ksh)
! - Il programma ignora le stazioni, e media tutti i dati presenti nel file
!   relativi a ciascuna data-ora (appendendo opportunamente i files di 
!   input e' quindi possibile calcolare medie di settore, ecc.).
! - Nel file di input, i parametri possono essere in qualsiasi ordine;
! - Le stagioni invernali vengono calcolate in 2 modi: win/djf si 
!   riferiscono all'inverno "continuo" a cavallo di due anni 
!   (oct-mar/dec-feb), wi2/jfd sono divise in due segmenti e restano all'
!   interno dell'anno solare (jan-mar+oct-dec/jan,feb,dec)
! - Le stagioni invenrali continue (djf e win) sono attribuite all'anno
!   in cui iniziano.
! - Usa il modulo per la gestione date date_hander.f90 (obsoleto)
!
!                                                V10.3.0, Enrico 27/01/2016
!--------------------------------------------------------------------------

USE file_utilities
USE date_handler
IMPLICIT NONE

!--------------------------------------------------------------------------
! 0) Dichiarazioni - costanti

!--------------------------------------------------------------------------
! 0.1 Parametri costanti / opzioni
REAL, PARAMETER :: rmis_hhr = -9999. ! dato mancante, files estra_orari
REAL, PARAMETER :: rmis_ser = -9999. ! dato mancante, files trasp_seriet
REAL, PARAMETER :: rmis_sex = -1.E30 ! dato mancante, files trasp_seriet exp
REAL, PARAMETER :: rmis_ddy = -9999. ! dato mancante, files estra_qa giorn.
REAL, PARAMETER :: rmis_tem = -9999. ! dato mancante, files trasp_temp
INTEGER, PARAMETER :: fw = 10        ! ampiezza dei campi nei files I/O
INTEGER, PARAMETER :: mxbin = 20     ! n.ro max di intervalli (istogrammi)
INTEGER, PARAMETER :: mxpar = 500    ! n.ro max di parametri in un file

!--------------------------------------------------------------------------
! 0.2.1 Stringhe descrittive dei parametri che richiedono un trattamento 
!       speciale (componenti del vento + parametri per cui sono dfiniti 
!       valori specifici per l'istogramma), usate da estra_orari, 
!       trasp_seriet, trasp_temp

INTEGER, PARAMETER :: np_uu = 1                           ! 1
CHARACTER (LEN=fw), PARAMETER :: str_par_uu(np_uu) = &
  (/"    U-wind"/)
INTEGER, PARAMETER :: np_vv = 1                           ! 2
CHARACTER (LEN=fw), PARAMETER :: str_par_vv(np_vv) = &
  (/"    V-wind"/)
INTEGER, PARAMETER :: np_ff = 3                           ! 3
CHARACTER (LEN=fw), PARAMETER :: str_par_ff(np_ff) = &
  (/"    FF ist","  Mod-wind","    Modulo"/)
INTEGER, PARAMETER :: np_dd = 3                           ! 4
CHARACTER (LEN=fw), PARAMETER :: str_par_dd(np_dd) = &
  (/"    DD ist","  Dir-wind","    Direz."/)
INTEGER, PARAMETER :: np_tt = 2                           ! 5
CHARACTER (LEN=fw), PARAMETER :: str_par_tt(np_tt) = &
  (/"     T ist","     Temp."/)
INTEGER, PARAMETER :: np_prc = 4                          ! 6
CHARACTER (LEN=fw), PARAMETER :: str_par_prc(np_prc) = &
  (/"   Prc 1hr","   Prc day","  Prc 06hr","  Prc 12hr"/)
INTEGER, PARAMETER :: np_rh = 1                           ! 7
CHARACTER (LEN=fw), PARAMETER :: str_par_rh(np_rh) = &
  (/"    RH ist"/)
INTEGER, PARAMETER :: np_clo = 1                          ! 8
CHARACTER (LEN=fw), PARAMETER :: str_par_clo(np_clo) = &
  (/"     Cloud"/)
INTEGER, PARAMETER :: np_zi = 1                           ! 9
CHARACTER (LEN=fw), PARAMETER :: str_par_zi(np_zi) = &
  (/"  Mixing-H"/)
INTEGER, PARAMETER :: np_mo = 3                           ! 10
CHARACTER (LEN=fw), PARAMETER :: str_par_mo(np_mo) = &
  (/"        mo","      molm","     modia"/)

! 0.2.2 Stringhe descrittive delle stagioni
CHARACTER (LEN=3), PARAMETER :: labsea(9) = (/ &
  "mam","jja","son","djf","sum","win","yea","wi2","jfd"/)

! 0.2.3 Stringhe descrittive dei prodotti
CHARACTER (LEN=5), PARAMETER :: labprod(9) = (/ &
  "stats","dfreq","ggtyp","yrtyp","daily","month","seaso","wrose","hours"/)

! 0.2.4 Ordine delle statistiche nei files .sta (relative agli array dei
!       contatori statistici)
INTEGER, PARAMETER :: idx_stat(5) = (/2,3,4,1,5/)

!--------------------------------------------------------------------------
! 0.3 Contatori statistici
REAL :: stats(6,mxpar)           ! statistiche (nok/sum/max/min/sum2/ave)
REAL :: dfreq(mxpar,mxbin)       ! distribuz. di frequenza
REAL,ALLOCATABLE :: ggtyp(:,:,:) ! nok/med/max/min/(ext) giorno tipo (5,npar,0:23)
REAL,ALLOCATABLE :: yrtyp(:,:,:) ! nok/med/max/min/(ext) anno tipo (5,npar,1:12)
REAL,ALLOCATABLE :: daily(:,:,:) ! nok/med/max/min/(ext) giornalieri (5,npar,ndays)
REAL,ALLOCATABLE :: month(:,:,:) ! nok/med/max/min/(ext) mensili (5,npar,nmonths)
REAL,ALLOCATABLE :: season(:,:,:,:)!nok/med/max/min/(ext) stagionali (5,9,npar,nyears)
                                 ! Stagioni: MAM,JJA,SON,DJF,SUM,WIN,YEA,WI2,JFD
REAL,ALLOCATABLE :: yeatv(:,:,:) ! valori piu' alti nell'anno (req_rank,npar,nyears)
REAL,ALLOCATABLE :: hours(:,:)   ! valori orari (npar,nrep)
REAL :: wrose(mxbin,mxbin)       ! intervallo (<=); settore (N,NE,E...)

!--------------------------------------------------------------------------
! 0.4 Intervalli per i istogrammi / wind rose 
!     - l'indice mxpar si riferisce al n.ro di colonna nel file di input
!     - il valore estremo appartiene all'int. inferiore ( <= )
INTEGER :: nbin(mxpar)             ! n.ro di intervalli
REAL :: idbin(mxpar,mxbin)         ! sup. degli intervalli

!--------------------------------------------------------------------------
! 0.5 Altre variabili del programma
TYPE (csv_record) :: csvline
TYPE(date) :: data_dum,data1,data2
REAL :: rval(mxpar),rmis,ff_calm
INTEGER :: npar,nrep,ndays,nmonths,nyears,id_par(mxpar),ival(mxpar)
INTEGER :: kp,kpr,kpar_dd,kpar_ff,fint,dsect,ncalm,nsect,ndec_out,req_rank
INTEGER :: cnt_miss,cnt_nodd,cnt_noff,cnt_ok
INTEGER :: k,kk,k2,kv,kr,kpar,khour,kbin,khr,kmm
INTEGER :: kyear3,kyear6,kyear12,kyear,nsea,ksea3,ksea6,kmonth,kday,year1
INTEGER :: eof,eor,ios,idum,hrdum,p1,p2,irec,month_tot,lline,out_grp
CHARACTER (LEN=mxpar*(fw+1)+20) :: chdum,chdum2,head_par,head_liv
CHARACTER (LEN=500) :: chfmt0,chfmt1,chfmt2,chfmt3,chfmt4,chfmt5,chfmth
CHARACTER (LEN=100) :: chpar,file_in,file_root,file_out,file_out2
CHARACTER (LEN=fw) :: str_par(mxpar),str_par2(mxpar),str_liv(mxpar)
CHARACTER (LEN=fw) :: chval(mxpar),str_par_dum,str_par_dum2
CHARACTER (LEN=17) :: str_data_ser
CHARACTER (LEN=13) :: str_data_csv
CHARACTER (LEN=12) :: title(5)
CHARACTER (LEN=10) :: ch10
CHARACTER (LEN=8) :: ch_id_staz
CHARACTER (LEN=4) :: ch4
CHARACTER (LEN=3) :: inp_data,out_fmt,next_arg
LOGICAL :: fmt_ser_xls,out_liv,lrank,lstd,miss0,req_prod(9)

!--------------------------------------------------------------------------
! 1) Parametri da riga comandi

out_fmt = "txt"
inp_data = "hhr"
out_liv = .FALSE.
lrank = .FALSE.
lstd = .FALSE.
miss0 = .FALSE.
req_prod(:) = .TRUE.
file_in = ""
ndec_out = 1

next_arg = ""
ios = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL scrive_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-o") THEN
    inp_data = "hhr"
  ELSE IF (TRIM(chdum) == "-s") THEN
    inp_data = "ser"
  ELSE IF (TRIM(chdum) == "-sx") THEN
    inp_data = "sex"
  ELSE IF (TRIM(chdum) == "-d") THEN
    inp_data = "ddy"
  ELSE IF (TRIM(chdum) == "-q") THEN
    inp_data = "ddy"
  ELSE IF (TRIM(chdum) == "-t") THEN
    inp_data = "tem"
  ELSE IF (TRIM(chdum) == "-rank") THEN
    lrank = .TRUE.
    next_arg = "rnk"
  ELSE IF (TRIM(chdum) == "-std") THEN
    lstd = .TRUE.
  ELSE IF (TRIM(chdum) == "-miss0") THEN
    miss0 = .TRUE.
  ELSE IF (chdum(1:5) == "-prod") THEN
    req_prod(:) = .FALSE.
    DO kpr = 1,9
      IF (INDEX(chdum,labprod(kpr)) /= 0) req_prod(kpr) = .TRUE.
    ENDDO
  ELSE IF (TRIM(chdum) == "-liv") THEN
    out_liv = .TRUE.
  ELSE IF (TRIM(chdum) == "-csv") THEN
    out_fmt = "csv"
  ELSE IF (TRIM(chdum) == "-ndec") THEN
    next_arg = "ndc"
  ELSE IF (next_arg == "rnk") THEN
    READ (chdum,*,IOSTAT=ios) req_rank
    next_arg = ""
  ELSE IF (next_arg == "ndc") THEN
    READ (chdum,*,IOSTAT=ios) ndec_out
    next_arg = ""
  ELSE
    file_in = TRIM(chdum)
  ENDIF
ENDDO

IF (out_liv .AND. inp_data /= "ser" .AND. inp_data /= "sex") THEN
  WRITE (*,*) "Opzione -liv possibile solo con formato seriet"
  out_liv = .FALSE.
ENDIF
IF (TRIM(file_in) == "") THEN
  WRITE (*,*) "Errore nei parametri (specificare filein)"
  STOP 1
ENDIF
IF (ios /= 0 .OR. ndec_out > fw-3) THEN
  WRITE (*,*) "Errore nei parametri (ndec illegale o troppo alto)"
  STOP 1
ENDIF
IF (lrank .AND. lstd) THEN
  WRITE (*,*) "Le opzioni -rank e -std sono incompatibili"
  STOP 1
ENDIF

title(1) = "Medie"
title(2) = "Massimi"
title(3) = "Minimi"
title(4) = "Dati_validi"
IF (lrank) THEN
  WRITE (title(5),'(a5,i3)') "Rank ",req_rank
ELSE IF (lstd) THEN
  title(5) = "Dev_standard"
ENDIF

!--------------------------------------------------------------------------
! 2) Elaborazioni preliminari sul file input: lista parametri, date estreme

!--------------------------------------------------------------------------
! 2.1 Radice del nome del file
CALL get_eof_eor(eof,eor)
p1 = INDEX(file_in,"/",BACK=.TRUE.)
p2 = INDEX(file_in,".",BACK=.TRUE.)
IF (p2 < p1 .OR. p2 == 0) p2 = LEN(TRIM(file_in)) + 1
file_root = file_in(p1+1:p2-1)

!--------------------------------------------------------------------------
! 2.2 Apro file, skip header, leggo lista parametri, trovo gli (eventuali)
!     parametri speciali

! Apro file
OPEN (UNIT=20, FILE=file_in, STATUS= "OLD", ACTION="READ", ERR=9999)

! Skip header
head_par = ""
IF (inp_data == "hhr") THEN
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,'(13x,a)', ERR=9997) head_par

ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,'(a)',ERR=9998) chdum2
  READ (20,*,ERR=9998)
  READ (20,'(a)',ERR=9998) chdum
  str_data_ser = chdum(1:17) 
  head_par = chdum(18:)
  IF (out_liv) head_liv = chdum2(18:)
  IF (str_data_ser == "gg/mm/aaaa hh sca") THEN
    fmt_ser_xls = .TRUE.
  ELSE
    fmt_ser_xls = .FALSE.
  ENDIF

ELSE IF (inp_data == "ddy") THEN
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,'(10x,a)', ERR=9997) head_par

ELSE IF (inp_data == "tem") THEN
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,'(19x,a)', ERR=9997) head_par

ENDIF

! Leggo la lista dei parametri
IF (inp_data == "hhr" .OR. inp_data == "ser"  .OR. inp_data == "sex" .OR. &
    inp_data == "ddy" .OR. inp_data == "tem") THEN

  WRITE (chfmt0,'(a,i2,a)'), "(1x,a",fw,")"
  DO k = 1,mxpar
    p1 = (k-1) * (fw+1) + 1
    p2 = k * (fw+1)
    READ (head_par(p1:p2),chfmt0,IOSTAT=ios) str_par_dum
    IF (out_liv) READ (head_liv(p1:p2),chfmt0,IOSTAT=ios) str_liv(k)
    IF (ios /= 0) GOTO 9997
    IF (str_par_dum == "") EXIT

    id_par(k) = 0
    str_par(k) = str_par_dum
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_uu(1:np_uu)) )) &
      id_par(k) = 1
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_vv(1:np_vv)) )) &
      id_par(k) = 2
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_ff(1:np_ff)) )) &
      id_par(k) = 3
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_dd(1:np_dd)) )) &
      id_par(k) = 4
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_tt(1:np_tt)) )) &
      id_par(k) = 5
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_prc(1:np_prc)) )) &
      id_par(k) = 6
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_rh(1:np_rh)) )) &
      id_par(k) = 7
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_clo(1:np_clo)) )) &
      id_par(k) = 8
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_zi(1:np_zi)) )) &
      id_par(k) = 9
    IF (ANY( ADJUSTL(str_par_dum) == ADJUSTL(str_par_mo(1:np_mo)) )) &
      id_par(k) = 10

  ENDDO
  npar = k - 1

ENDIF
CLOSE (20)

!--------------------------------------------------------------------------
! 2.3 Numero di giorni, mesi e anni contenuti nel file (per statistiche 
!   daily, month, season). 
!   Il calcolo degli anni tiene conto delle stagioni di 3-6 mesi: se i dati
!   partono tra gennaio e marzo bisogna considerare anche l'anno precedente

data1 = date(1,1,9999)
data2 = date(1,1,0)

OPEN (UNIT=20, FILE=file_in, STATUS= "OLD", ACTION="READ", ERR=9999)
IF (inp_data == "hhr" .OR. inp_data == "ddy" .OR. inp_data == "tem") THEN
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
ENDIF

DO k = 1,HUGE(k)
  IF (inp_data == "hhr" .OR. inp_data == "ddy" .OR. inp_data == "tem") THEN
    READ (20,'(i4,2i3)',IOSTAT=ios) data_dum%yy,data_dum%mm,data_dum%dd

  ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
    IF (fmt_ser_xls .eqv. .FALSE.) THEN
      READ (20,'(i4,2i3)',IOSTAT=ios) data_dum%yy,data_dum%mm,data_dum%dd
    ELSE
      READ (20,'(i2,1x,i2,1x,i4)',IOSTAT=ios) data_dum%dd,data_dum%mm, &
        data_dum%yy
    ENDIF

  ENDIF
  IF (ios == eof) EXIT 
  IF (ios /= 0) GOTO 9996

  IF (data_dum < data1) data1 = data_dum
  IF (data_dum > data2) data2 = data_dum
ENDDO

nrep = k - 1
ndays = data2 - data1 + 1
nmonths = (data2%yy*12 + data2%mm) - (data1%yy*12 + data1%mm) + 1
IF (data1%mm <= 3) THEN
  year1 = data1%yy - 1
ELSE
  year1 = data1%yy
ENDIF
nyears = data2%yy - year1 + 1

CLOSE(20)

!--------------------------------------------------------------------------
! 2.4 Costruisco formato input
IF (inp_data == "hhr") THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i4,3i3,",npar,"(1x,a",fw,"))"
ELSE IF (inp_data == "ddy") THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i4,2i3,",npar,"(1x,a",fw,"))"
ELSE IF (inp_data == "tem") THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i4,3i3,6x",npar,"(1x,a",fw,"))"
ELSE IF ((inp_data == "ser" .OR. inp_data == "sex") .AND. &
         fmt_ser_xls .eqv. .FALSE.) THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i4,3i3,4x,",npar,"(1x,a",fw,"))"
ELSE IF ((inp_data == "ser" .OR. inp_data == "sex") .AND. &
         fmt_ser_xls .eqv. .TRUE.) THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i2,1x,i2,1x,i4,i3,4x,",npar,"(1x,a",fw,"))"
ENDIF

!--------------------------------------------------------------------------
! 2.5 Selezione valore per dati mancanti
IF (inp_data == "hhr") THEN
  rmis = rmis_hhr
ELSE IF (inp_data == "ser") THEN
  rmis = rmis_ser
ELSE IF (inp_data == "sex") THEN
  rmis = rmis_sex
ELSE IF (inp_data == "ddy") THEN
  rmis = rmis_ddy
ELSE IF (inp_data == "tem") THEN
  rmis = rmis_tem
ENDIF

!--------------------------------------------------------------------------
! 2.6 Alloco arrays
ALLOCATE (ggtyp(5,npar,0:23))
ALLOCATE (yrtyp(5,npar,1:12))
ALLOCATE (daily(5,npar,ndays))
ALLOCATE (month(5,npar,nmonths))
ALLOCATE (season(5,9,npar,nyears))
ALLOCATE(hours(npar,nrep))
IF (lrank) ALLOCATE(yeatv(req_rank,npar,nyears))

!--------------------------------------------------------------------------
! 2.7 Definisco gli intervalli per istogrammi (relativi a ciascun parametro)

!==========================================================================
!                           USER MODIFICATION

ff_calm = 1.  ! Soglia delle calme (inclusiva)
nsect = 8     ! N.ro di settori per direzione del vento (solo 4,8,16)

idbin(:,:) = rmis
DO k = 1,npar
  SELECT CASE (id_par(k))
  CASE (1,2,3)                                              ! Vel. vento
!   NB: La prima soglia deve rimanere = ff_calm
    nbin(k) = 7
    idbin(k,1:nbin(k)) = (/ff_calm,2.,4.,7.,10.,20.,100./)

  CASE (4)                                                  ! Dir. vento
    IF (nsect == 4) THEN
      nbin(k) = 5
      idbin(k,1:nbin(k)) = (/45.,135.,225.,315.,360./)
    ELSE IF (nsect == 8) THEN
      nbin(k) = 9
      idbin(k,1:nbin(k)) = & 
        (/22.5,67.5,112.5,157.5,202.5,247.5,292.5,337.5,360./)
    ELSE IF (nsect == 16) THEN
      nbin(k) = 17
      idbin(k,1:nbin(k)) = & 
        (/ 11.25, 33.75, 56.25, 78.75,101.25,123.75,146.25,168.75, &
          191.25,213.75,236.25,258.75,281.25,303.75,326.25,348.75,360./)
    ELSE 
      WRITE (*,*) "N.ro di settori illegale, modificare nsect nel sorgente"
      STOP 99
    ENDIF

  CASE (5)                                                  !Temperatura C
    nbin(k) = 10
    idbin(k,1:nbin(k)) = (/-10.,-5.,0.,5.,10.,15.,20.,25.,30.,50./)

  CASE (6)                                                  ! Precip.
    nbin(k) = 6
    idbin(k,1:nbin(k)) = (/0.,1.,2.,5.,10.,100./)

  CASE (7)                                                  ! Umidita' %
    nbin(k) = 6
    idbin(k,1:nbin(k)) = (/30.,50.,75.,90.,95.,100./)

  CASE (8)                                                  ! Cloud cover 
    nbin(k) = 6
    idbin(k,1:nbin(k)) = (/0.,2.,4.,6.,8.,9./)

  CASE (9)                                                  ! Mix.height 
    nbin(k) = 9
    idbin(k,1:nbin(k)) = (/50.,100.,200.,400.,800.,1200.,1600.,2000.,5000./)

  CASE (10)                                                 ! Monoin-Obukov
    nbin(k) = 9
    idbin(k,1:nbin(k)) = (/-999.,-100.,-30.,-10.,0.,10.,30.,100.,999./)

  CASE DEFAULT
    nbin(k) = 11
    idbin(k,1:nbin(k)) = (/-10000.,-1000.,-100.,-10.,-1.,0.,1.,10.,100.,1000.,10000./)

  END SELECT
ENDDO

!==========================================================================

!--------------------------------------------------------------------------
! 2.8 Trovo i parametri relativi al vento (se ci sono)
kpar_dd = -99
kpar_ff = -99
DO k = 1,npar
  IF (id_par(k) == 4) kpar_dd = k
  IF (id_par(k) == 3) kpar_ff = k
ENDDO

!--------------------------------------------------------------------------
! 2.9 Inizializzo i contatori statistici
stats(1,:) = 0.
stats(2,:) = 0.
stats(3,:) = -HUGE(0.)
stats(4,:) = HUGE(0.)
stats(5,:) = 0.
stats(6,:) = 0.
dfreq(:,:) = 0.
ggtyp(1:2,:,:) = 0.
ggtyp(3,:,:) = -HUGE(0.)
ggtyp(4,:,:) = HUGE(0.)
yrtyp(1:2,:,:) = 0.
yrtyp(3,:,:) = -HUGE(0.)
yrtyp(4,:,:) = HUGE(0.)
yrtyp(5,:,:) = 0.
daily(1:2,:,:) = 0.
daily(3,:,:) = -HUGE(0.)
daily(4,:,:) = HUGE(0.)
daily(5,:,:) = rmis
month(1:2,:,:) = 0.
month(3,:,:) = -HUGE(0.)
month(4,:,:) = HUGE(0.)
month(5,:,:) = rmis
season(1:2,:,:,:) = 0.
season(3,:,:,:) = -HUGE(0.)
season(4,:,:,:) = HUGE(0.)
season(5,:,:,:) = rmis
wrose(:,:) = 0.
ncalm = 0
cnt_ok = 0
cnt_miss = 0
cnt_nodd = 0
cnt_noff = 0
IF (lrank) yeatv(:,:,:) = -HUGE(0.)

WRITE (*,*) "Contenuto file di input:"
WRITE (*,*) "  - parametri:   ",npar
WRITE (*,*) "  - n.ro report: ",nrep
WRITE (*,*) "  - giornate:    ",ndays
WRITE (*,*) "  - mesi:        ",nmonths
WRITE (*,*) "  - anni:        ",nyears
IF (kpar_dd /= -99 .AND. kpar_ff /= -99) &
  WRITE (*,*) "  - sono presenti dati di vento"
WRITE (*,*)

!--------------------------------------------------------------------------
! 3) Ciclo sui record di input e aggiorno statistiche

OPEN (UNIT=20, FILE=file_in, STATUS= "OLD", ACTION="READ", ERR=9999)

! 3.1 Skippo header
IF (inp_data == "hhr" .OR. inp_data == "ddy" .OR. inp_data == "tem") THEN
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
ENDIF

DO k = 1,nrep

! 3.2 Leggo un report
  IF (inp_data == "hhr") THEN
    READ (20,chfmt1,IOSTAT=ios) data_dum%yy,data_dum%mm,data_dum%dd,hrdum,chval(1:npar)
    IF (ios /= 0) GOTO 9995
    DO kv = 1,npar
      READ (chval(kv),*) rval(kv)
    ENDDO

  ELSE IF (inp_data == "ddy") THEN
    READ (20,chfmt1,IOSTAT=ios) data_dum%yy,data_dum%mm,data_dum%dd,chval(1:npar)
    hrdum = 0
    IF (ios /= 0) GOTO 9995
    DO kv = 1,npar
      READ (chval(kv),*) rval(kv)
    ENDDO

  ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
    IF (fmt_ser_xls .eqv. .FALSE.) THEN
      READ (20,chfmt1,IOSTAT=ios) data_dum%yy,data_dum%mm,data_dum%dd,hrdum,chval(1:npar)
      IF (ios /= 0) GOTO 9995
    ELSE
      READ (20,chfmt1,IOSTAT=ios) data_dum%dd,data_dum%mm,data_dum%yy,hrdum,chval(1:npar)
      IF (ios /= 0) GOTO 9995
    ENDIF
    DO kv = 1,npar
      READ (chval(kv),*) rval(kv)
    ENDDO

  ELSE IF (inp_data == "tem") THEN
    READ (20,chfmt1,IOSTAT=ios) data_dum%yy,data_dum%mm,data_dum%dd,hrdum,chval(1:npar)
    IF (ios /= 0) GOTO 9995
    DO kv = 1,npar
      READ (chval(kv),*) rval(kv)
    ENDDO

  ENDIF

! Tappo la dir. vento 370 dei Metar, per far tornare i conti del n.ro dati 
! in wrose (dovrebbe essere gia' stata tappata da estra_orari)
  IF (kpar_dd /= -99) THEN
    IF (rval(kpar_dd) > 360.) rval(kpar_dd) = rmis
  ENDIF

! Se richiesto, metto a 0 i dati mancanti
  IF (miss0) THEN
    WHERE (rval(1:npar) == rmis)
      rval(1:npar) = 0.
    ENDWHERE
  ENDIF
 
! 3.3 Calcolo il numero progressivo di giorno, mese e anno correnti
! - gli indici ksea* rappresentano la stagione a cui appartiene la data
!   corrente
! - gli indici kyear* (statistiche stagionali) rappresentano l'anno della
!   stagione a cui appartiene la data corrente; sono contati a partire da 
!   year1; kyear12 e' l'anno solare (Jan-Dec), kyear3 e' relativo alle
!   stagioni di 3 mesi (Mar-Feb), kyear6 e' relativo alle stagioni di 6 
!   mesi (Apr-Mar); per l'attribuizone dell'anno fa fede il giorno iniziale
!   della stagione (la stagione 1 e' MAM)

  kday = data_dum - data1 + 1
  kmonth = (data_dum%yy*12 + data_dum%mm) - (data1%yy*12 + data1%mm) + 1
  kyear12 = data_dum%yy - year1 + 1
  IF (data_dum%mm <= 2) THEN
    kyear3 = kyear12 - 1
    ksea3 = 4
  ELSE
    kyear3 = kyear12
    ksea3 = data_dum%mm / 3
  ENDIF
  IF (data_dum%mm <= 3) THEN
    kyear6 = kyear12 - 1
    ksea6 = 2
  ELSE
    kyear6 = kyear12
    ksea6 = (data_dum%mm+2) / 6
  ENDIF

  IF (hrdum < 0 .OR. hrdum > 23 .OR. kday < 1 .OR. kday > ndays .OR. &
      kmonth < 1 .OR. kmonth > nmonths .OR. &
      ksea3 < 1 .OR. ksea3 > 4 .OR. ksea6 < 1 .OR. ksea6 > 2 .OR. &
      kyear12 < 1 .OR. kyear3 < 1 .OR. kyear6 < 1 .OR. &
      kyear12 > nyears .OR. kyear3 > nyears .OR. kyear6 > nyears) GOTO 9994

! 3.4 Aggiorno stats, ggtyp, yrtyp, daily, month, season
  WHERE (rval(1:npar) /= rmis)
    stats(1,1:npar) = stats(1,1:npar) + 1
    stats(2,1:npar) = stats(2,1:npar) + rval(1:npar)
    stats(3,1:npar) = MAX (stats(3,1:npar), rval(1:npar))
    stats(4,1:npar) = MIN (stats(4,1:npar), rval(1:npar))
    stats(5,1:npar) = stats(5,1:npar) + rval(1:npar)**2

    ggtyp(1,1:npar,hrdum) = ggtyp(1,1:npar,hrdum) + 1
    ggtyp(2,1:npar,hrdum) = ggtyp(2,1:npar,hrdum) + rval(1:npar)
    ggtyp(3,1:npar,hrdum) = MAX (ggtyp(3,1:npar,hrdum), rval(1:npar))
    ggtyp(4,1:npar,hrdum) = MIN (ggtyp(4,1:npar,hrdum), rval(1:npar))

    yrtyp(1,1:npar,data_dum%mm) = yrtyp(1,1:npar,data_dum%mm) + 1
    yrtyp(2,1:npar,data_dum%mm) = yrtyp(2,1:npar,data_dum%mm) + rval(1:npar)
    yrtyp(3,1:npar,data_dum%mm) = MAX (yrtyp(3,1:npar,data_dum%mm), rval(1:npar))
    yrtyp(4,1:npar,data_dum%mm) = MIN (yrtyp(4,1:npar,data_dum%mm), rval(1:npar))

    daily(1,1:npar,kday) = daily(1,1:npar,kday) + 1
    daily(2,1:npar,kday) = daily(2,1:npar,kday) + rval(1:npar)
    daily(3,1:npar,kday) = MAX (daily(3,1:npar,kday), rval(1:npar))
    daily(4,1:npar,kday) = MIN (daily(4,1:npar,kday), rval(1:npar))

    month(1,1:npar,kmonth) = month(1,1:npar,kmonth) + 1
    month(2,1:npar,kmonth) = month(2,1:npar,kmonth) + rval(1:npar)
    month(3,1:npar,kmonth) = MAX (month(3,1:npar,kmonth), rval(1:npar))
    month(4,1:npar,kmonth) = MIN (month(4,1:npar,kmonth), rval(1:npar))

!   Stagioni trimestrali continue
    season(1,ksea3,1:npar,kyear3) = season(1,ksea3,1:npar,kyear3) + 1
    season(2,ksea3,1:npar,kyear3) = season(2,ksea3,1:npar,kyear3) + rval(1:npar)
    season(3,ksea3,1:npar,kyear3) = MAX (season(3,ksea3,1:npar,kyear3), rval(1:npar))
    season(4,ksea3,1:npar,kyear3) = MIN (season(4,ksea3,1:npar,kyear3), rval(1:npar))

!   Semestri estivo/invernale continui
    season(1,ksea6+4,1:npar,kyear6) = &
      season(1,ksea6+4,1:npar,kyear6) + 1
    season(2,ksea6+4,1:npar,kyear6) = &
      season(2,ksea6+4,1:npar,kyear6) + rval(1:npar)
    season(3,ksea6+4,1:npar,kyear6) = &
      MAX (season(3,ksea6+4,1:npar,kyear6), rval(1:npar))
    season(4,ksea6+4,1:npar,kyear6) = &
      MIN (season(4,ksea6+4,1:npar,kyear6), rval(1:npar))

!   Anno solare
    season(1,7,1:npar,kyear12) = season(1,7,1:npar,kyear12) +1
    season(2,7,1:npar,kyear12) = season(2,7,1:npar,kyear12) + rval(1:npar)
    season(3,7,1:npar,kyear12) = &
      MAX(season(3,7,1:npar,kyear12), rval(1:npar))
    season(4,7,1:npar,kyear12) = &
      MIN (season(4,7,1:npar,kyear12), rval(1:npar))
  ENDWHERE

! Semestre invernale spezzato
  IF (ksea6 == 2) THEN
    WHERE (rval(1:npar) /= rmis)
      season(1,8,1:npar,kyear12) = season(1,8,1:npar,kyear12) +1
      season(2,8,1:npar,kyear12) = season(2,8,1:npar,kyear12) + rval(1:npar)
      season(3,8,1:npar,kyear12) = &
        MAX(season(3,8,1:npar,kyear12), rval(1:npar))
      season(4,8,1:npar,kyear12) = &
        MIN (season(4,8,1:npar,kyear12), rval(1:npar))
    ENDWHERE
  ENDIF  
 
! Trimestre invernale spezzato
  IF (ksea3 == 4) THEN
    WHERE (rval(1:npar) /= rmis)
      season(1,9,1:npar,kyear12) = season(1,9,1:npar,kyear12) +1
      season(2,9,1:npar,kyear12) = season(2,9,1:npar,kyear12) + rval(1:npar)
      season(3,9,1:npar,kyear12) = &
        MAX(season(3,9,1:npar,kyear12), rval(1:npar))
      season(4,9,1:npar,kyear12) = &
        MIN (season(4,9,1:npar,kyear12), rval(1:npar))
    ENDWHERE
  ENDIF  
 
! Rank N (SEyea)
  IF (lrank) THEN
    DO kv = 1,npar
      IF (rval(kv) == rmis .OR. rval(kv) <= yeatv(req_rank,kv,kyear12)) CYCLE
      DO kr = 1,req_rank
        IF (rval(kv) > yeatv(kr,kv,kyear12)) EXIT
      ENDDO
      IF (kr > req_rank) GOTO 9993
      yeatv(kr+1:req_rank,kv,kyear12) = yeatv(kr:req_rank-1,kv,kyear12)
      yeatv(kr,kv,kyear12) = rval(kv)
    ENDDO
  ENDIF

! Sum2 (per deviazione standard; yrtyp)
  IF (lstd) THEN
    WHERE (rval(1:npar) /= rmis)
      yrtyp(5,1:npar,data_dum%mm) = yrtyp(5,1:npar,data_dum%mm) + rval(1:npar)**2
    ENDWHERE
  ENDIF

! 3.5 Aggiorno dfreq
  DO kpar = 1,npar
    IF (rval(kpar) == rmis) CYCLE

    DO kbin = 1,nbin(kpar)
      IF (rval(kpar) <= idbin(kpar,kbin)) EXIT
    ENDDO
    IF (kbin <= nbin(kpar)) THEN
      dfreq(kpar,kbin) = dfreq(kpar,kbin) + 1
    ENDIF      

  ENDDO

! 3.6 Salvo la serie dei dati orari
  hours(k,1:kpar) = rval(1:npar)

! 3.7 Aggiorno wrose (questo deve rimanere l'ultimo blocco!!)
  IF (kpar_dd == -99 .OR. kpar_ff == -99) CYCLE

  write (44,*) data_dum,rval(kpar_dd),rval(kpar_ff),rmis

  IF (rval(kpar_dd) == rmis .AND. rval(kpar_ff) == rmis) THEN
    cnt_miss = cnt_miss + 1
  
  ELSE IF (rval(kpar_ff) == rmis) THEN
    cnt_noff = cnt_noff + 1

  ELSE IF (rval(kpar_dd) == rmis .AND. rval(kpar_ff) /= 0.) THEN
    cnt_nodd = cnt_nodd + 1

  ELSE
    cnt_ok = cnt_ok + 1
    DO kbin = 1,nbin(kpar_ff)
      IF (rval(kpar_ff) <= idbin(kpar_ff,kbin)) EXIT
    ENDDO
    IF (kbin > nbin(kpar_ff)) CYCLE
    fint = kbin
  
    DO kbin = 1,nbin(kpar_dd)
      IF (rval(kpar_dd) <= idbin(kpar_dd,kbin)) EXIT
    ENDDO
    IF (kbin > nbin(kpar_dd)) CYCLE
    dsect = kbin

    wrose(fint,dsect) = wrose(fint,dsect) + 1
    IF (rval(kpar_ff) <= ff_calm) THEN           ! aggiorno contatore calme
      ncalm = ncalm + 1
    ENDIF  
 
  ENDIF



ENDDO
CLOSE(20)

WRITE (*,*) "Lettura terminata"

!--------------------------------------------------------------------------
! 4) Calcolo statistiche (medie e altre elaborazioni)

! stats
WHERE (stats(1,1:npar) > 0)
  stats(6,1:npar) = stats(2,1:npar) /stats(1,1:npar) 
  stats(5,1:npar) = &
    SQRT(MAX(0., stats(5,1:npar)/stats(1,1:npar) - stats(6,1:npar)**2 ))
ELSEWHERE
  stats(2,1:npar) = rmis
  stats(3,1:npar) = rmis
  stats(4,1:npar) = rmis
  stats(5,1:npar) = rmis
  stats(6,1:npar) = rmis
ENDWHERE

! ggtyp
WHERE (ggtyp(1,1:npar,0:23) > 0)
  ggtyp(2,1:npar,0:23) = ggtyp(2,1:npar,0:23) / ggtyp(1,1:npar,0:23) 
ELSEWHERE
  ggtyp(2,1:npar,0:23) = rmis
  ggtyp(3,1:npar,0:23) = rmis
  ggtyp(4,1:npar,0:23) = rmis
ENDWHERE

! yrtyp
WHERE (yrtyp(1,1:npar,1:12) > 0)
  yrtyp(2,1:npar,1:12) = yrtyp(2,1:npar,1:12) / yrtyp(1,1:npar,1:12) 
ELSEWHERE
  yrtyp(2,1:npar,1:12) = rmis
  yrtyp(3,1:npar,1:12) = rmis
  yrtyp(4,1:npar,1:12) = rmis
ENDWHERE
IF (lstd) THEN
  WHERE (yrtyp(1,1:npar,1:12) > 0)
    yrtyp(5,1:npar,1:12) = SQRT(MAX(0., &
      yrtyp(5,1:npar,1:12)/yrtyp(1,1:npar,1:12) - yrtyp(2,1:npar,1:12)**2 ))
  ELSEWHERE
    yrtyp(2,1:npar,1:12) = rmis
  ENDWHERE
ENDIF

! daily
WHERE (daily(1,1:npar,1:ndays) > 0)
  daily(2,1:npar,1:ndays) = daily(2,1:npar,1:ndays) / &
    daily(1,1:npar,1:ndays) 
ELSEWHERE
  daily(2,1:npar,1:ndays) = rmis
  daily(3,1:npar,1:ndays) = rmis
  daily(4,1:npar,1:ndays) = rmis
ENDWHERE

! month
WHERE (month(1,1:npar,1:nmonths) > 0)
  month(2,1:npar,1:nmonths) = month(2,1:npar,1:nmonths) / &
    month(1,1:npar,1:nmonths) 
ELSEWHERE
  month(2,1:npar,1:nmonths) = rmis
  month(3,1:npar,1:nmonths) = rmis
  month(4,1:npar,1:nmonths) = rmis
ENDWHERE

! season
WHERE (season(1,1:9,1:npar,1:nyears) > 0)
  season(2,1:9,1:npar,1:nyears) = season(2,1:9,1:npar,1:nyears) / &
    season(1,1:9,1:npar,1:nyears)
ELSEWHERE
  season(2,1:9,1:npar,1:nyears) = rmis
  season(3,1:9,1:npar,1:nyears) = rmis
  season(4,1:9,1:npar,1:nyears) = rmis
ENDWHERE

! dfreq: metto a rmis la frequenza dei bin non utilizzati
WHERE (idbin(1:npar,1:mxbin) == rmis)
  dfreq(1:npar,1:mxbin) = rmis
ENDWHERE

! wrose: se ci sono dati di vento, raggruppo la prima e l'ultima classe 
! (entrambe Nord)
IF (kpar_dd /= -99 .AND. kpar_ff /= -99) THEN
  wrose(1:nbin(kpar_ff),1) = wrose(1:nbin(kpar_ff),1) + & 
    wrose(1:nbin(kpar_ff),nbin(kpar_dd))
ENDIF

! rank: metto a rmis i valori negli anni che non hanno abbastanza dati validi
IF (lrank) THEN
  IF (ANY(season(1,7,1:npar,1:nyears) < req_rank .AND. &
      yeatv(req_rank,1:npar,1:nyears) /= -HUGE(0.)) .OR. &
      ANY(season(1,7,1:npar,1:nyears) >= req_rank .AND. &
      yeatv(req_rank,1:npar,1:nyears) == -HUGE(0.))) GOTO 9993
  
  WHERE (season(1,7,1:npar,1:nyears) < req_rank)
    season(5,7,1:npar,1:nyears) = rmis
  ELSEWHERE
    season(5,7,1:npar,1:nyears) = yeatv(req_rank,1:npar,1:nyears)
  ENDWHERE
ENDIF

!--------------------------------------------------------------------------
! 5) Scrittura output e conclusione

!--------------------------------------------------------------------------
! 5.0 Operazioni preliminari

! 5.0.1 Costruisco le stringhe identificative dei caratteri per excel e 
!       GRADS (senza spazi e caratteri speciali, il 1o carattere non numerico)

str_par2(:) = ""

DO kpar = 1,npar
  str_par_dum2 = ADJUSTL(str_par(kpar))
  IF (INDEX("0123456789",str_par_dum2(1:1)) == 0) THEN
    str_par_dum = str_par_dum2(1:fw-1)
  ELSE
    str_par_dum = "v" // str_par_dum2(1:fw-2)
  ENDIF

  k2 = 0
  DO k = 1,LEN(TRIM(str_par_dum))
    IF (str_par_dum(k:k) == " " .OR. str_par_dum(k:k) == "_" .OR. &
        str_par_dum(k:k) == "." .OR. str_par_dum(k:k) == "-") CYCLE
    k2 = k2 + 1 
    str_par2(kpar)(k2:k2) = str_par_dum(k:k)
  ENDDO
  str_par2(kpar) = ADJUSTR(str_par2(kpar))

ENDDO

! 5.0.2 Scelgo il formato per i files .sta

IF (ndec_out >= 0) THEN
  WRITE (chfmt4,'(2(a,i2))') "f",fw,".",ndec_out
ELSE
  WRITE (chfmt4,'(2(a,i2))') "e",fw,".",fw-7
ENDIF

!--------------------------------------------------------------------------
! 5.1 File stats (ASCII)

IF (req_prod(1)) THEN

IF (out_fmt == "txt") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_stats.sta"
ELSE IF (out_fmt == "csv") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_stats.csv"
ENDIF
WRITE (chfmth,'(a,i3,a,i2,a)') "(13x,",npar,"(1x,a",fw,"))"
WRITE (chfmt2,'(a,i3,3a)') "(a13,",npar,"(1x,",TRIM(chfmt4),"))"
WRITE (chfmt5,'(a,i3,a,i2,a)') "(a13,",npar,"(1x,i",fw,"))"

OPEN (UNIT=31, FILE=file_out, STATUS="REPLACE", FORM="FORMATTED")
IF (out_fmt == "txt") THEN
  WRITE (31,chfmth) (str_par(kpar), kpar=1,npar)
  IF (out_liv) WRITE (31,chfmth) (str_liv(kpar), kpar=1,npar)
  IF (out_liv) WRITE (31,*)
  WRITE (31,chfmt5) "Tot. report: ",(nrep,k=1,npar)
  WRITE (31,chfmt5) "Dati buoni:  ",NINT(stats(1,1:npar))
  WRITE (31,chfmt2) "Media:       ",stats(6,1:npar)
  WRITE (31,chfmt2) "Massimo:     ",stats(3,1:npar)
  WRITE (31,chfmt2) "Minimo:      ",stats(4,1:npar)
  WRITE (31,chfmt2) "Std. dev.    ",stats(5,1:npar)
  WRITE (31,chfmt2) "Somma totale:",stats(2,1:npar)

ELSE IF (out_fmt == "csv") THEN

! header: parametri
  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Parametro")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_par(kpar))))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

! header: livelli
  IF (out_liv) THEN
    CALL init(csvline)
    CALL csv_record_addfield(csvline,"Livello")
    DO kpar = 1,npar
      CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_liv(kpar))))
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)
  ENDIF

! dati
  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Totale_reports")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,REAL(nrep))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Dati_validi")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,stats(1,kpar))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Media")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,stats(6,kpar))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Massimo")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,stats(3,kpar))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Minimo")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,stats(4,kpar))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Deviazione_standard")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,stats(5,kpar))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Somma_totale")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,stats(2,kpar))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

ENDIF
CLOSE(31)

ENDIF

!--------------------------------------------------------------------------
! 5.2 File dfreq (ASCII)

IF (req_prod(2)) THEN

IF (out_fmt == "txt") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_dfreq.sta"
ELSE IF (out_fmt == "csv") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_dfreq.csv"
ENDIF
WRITE (chfmth,'(a,i3,a,i2,a)') "(13x,",npar,"(1x,a",fw,"))"
WRITE (chfmt2,'(a,i3,3a)') "(i3,7x,a2,1x,",npar,"(1x,",TRIM(chfmt4),"))"
WRITE (chfmt5,'(a,i3,a,i2,a)') "(i3,7x,a2,1x,",npar,"(1x,i",fw,"))"

OPEN (UNIT=31, FILE=file_out, STATUS="REPLACE", FORM="FORMATTED")
IF (out_fmt == "txt") THEN
  WRITE (31,'(a)') "Dati validi"
  WRITE (31,*)
  WRITE (31,chfmth) (str_par(kpar), kpar=1,npar)
  IF (out_liv) WRITE (31,chfmth) (str_liv(kpar), kpar=1,npar)
  IF (out_liv) WRITE (31,*)
  
  DO kbin = 1,mxbin
    WRITE (31,chfmt5) kbin,"  ",NINT(dfreq(1:npar,kbin))
  ENDDO
  
  WRITE (31,*)
  WRITE (chfmt3,'(a,i3,a,i2,a)') "(a6,7x,",npar,"(1x,i",fw,"))"
  WRITE (31,chfmt3) "Totale",NINT(SUM(dfreq(1:npar,1:mxbin), DIM=2, &
    MASK = dfreq(1:npar,1:mxbin)/=rmis))
  
  WRITE (31,*)
  WRITE (31,*)
  WRITE (31,'(a)') "Estremo superiore (inclusivo) di ciascun intervallo:"
  WRITE (31,*)
  WRITE (31,chfmth) (str_par(kpar),kpar=1,npar)
  IF (out_liv) WRITE (31,chfmth) (str_liv(kpar), kpar=1,npar)
  IF (out_liv) WRITE (31,*)
  DO kbin = 1,mxbin
    WRITE (31,chfmt2) kbin,"<=",idbin(1:npar,kbin)
  ENDDO

ELSE IF (out_fmt == "csv") THEN

! Valori
! header: parametri
  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Valori")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_par(kpar))))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

! header: livelli
  IF (out_liv) THEN
    CALL init(csvline)
    CALL csv_record_addfield(csvline," ")
    DO kpar = 1,npar
      CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_liv(kpar))))
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)
  ENDIF

! Numero di dati
  DO kbin = 1,mxbin
    CALL init(csvline)
    CALL csv_record_addfield(csvline,kbin)
    DO kpar = 1,npar
      CALL csv_record_addfield(csvline,dfreq(kpar,kbin))
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)
   ENDDO

! Estremi dei bin
! header: parametri
  CALL init(csvline)
  CALL csv_record_addfield(csvline,"Estremo_sup_inclusivo_intervalli")
  DO kpar = 1,npar
    CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_par(kpar))))
  ENDDO
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

! header: livelli
  IF (out_liv) THEN
    CALL init(csvline)
    CALL csv_record_addfield(csvline," ")
    DO kpar = 1,npar
      CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_liv(kpar))))
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)
  ENDIF

! Numero di dati
  DO kbin = 1,mxbin
    CALL init(csvline)
    CALL csv_record_addfield(csvline,kbin)
    DO kpar = 1,npar
      CALL csv_record_addfield(csvline,idbin(kpar,kbin))
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)
   ENDDO

ENDIF
CLOSE(31)

ENDIF

!--------------------------------------------------------------------------
! 5.3.1 File ggtyp (ASCII)

IF (req_prod(3)) THEN

IF (out_fmt == "txt") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_ggtyp.sta"
ELSE IF (out_fmt == "csv") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_ggtyp.csv"
ENDIF
WRITE (chfmth,'(a,i3,a,i2,a)') "(a10,",npar,"(1x,a",fw,"))"
WRITE (chfmt2,'(a,i3,3a)') "(i2.2,8x,",npar,"(1x,",TRIM(chfmt4),"))"
WRITE (chfmt5,'(a,i3,a,i2,a)') "(i2.2,8x,",npar,"(1x,i",fw,"))"

OPEN (UNIT=31, FILE=file_out, STATUS="REPLACE", FORM="FORMATTED")
DO kk = 1,4
  IF (out_fmt == "txt") THEN
    WRITE (31,'(a)') TRIM(title(kk))                    ! header 1 (metric)
    IF (out_liv) THEN                                   ! header 2 (liv)
      WRITE (31,chfmth) "          ",(str_liv(kpar), kpar=1,npar)
    ELSE
      WRITE (31,*)
    ENDIF
    WRITE (31,chfmth) "hh        ",(str_par(kpar), kpar=1,npar) ! head 3 (par)
    DO khr = 0,23                                       ! dati
      IF (kk == 4) THEN
        WRITE (31,chfmt5) khr,NINT(ggtyp(idx_stat(kk),1:npar,khr))
      ELSE
        WRITE (31,chfmt2) khr,ggtyp(idx_stat(kk),1:npar,khr)
      ENDIF
    ENDDO
    WRITE (31,*)
    WRITE (31,*)

  ELSE IF (out_fmt == "csv") THEN

!   header 1 (metric)
    WRITE (31,'(a)') TRIM(title(kk))

!   header 2 (liv)
    CALL init(csvline)
    CALL csv_record_addfield(csvline,"")
    DO kpar = 1,npar
      IF (out_liv) THEN
        CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_liv(kpar))))
      ELSE
        CALL csv_record_addfield(csvline,"")
      ENDIF
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)

!   header 3 (par)
    CALL init(csvline)
    CALL csv_record_addfield(csvline,24)
    DO kpar = 1,npar
      CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_par(kpar))))
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)

!   dati
    DO khr = 0,23
      CALL init(csvline)
      WRITE (str_data_csv,'(i4.4,2(a1,i2.2),1x,i2.2)') 0,"-",0,"-",0,khr
      CALL csv_record_addfield(csvline,str_data_csv)
      DO kpar = 1,npar
        CALL csv_record_addfield(csvline,ggtyp(idx_stat(kk),kpar,khr))
      ENDDO
      WRITE (31,'(a)') csv_record_getrecord(csvline)
      CALL delete(csvline)
    ENDDO
 
  ENDIF
ENDDO
CLOSE(31)

ENDIF

!--------------------------------------------------------------------------
! 5.3.2 File ggtyp (GRADS)

IF (req_prod(3)) THEN

! File dat
WRITE (file_out,'(2a)') TRIM(file_root),"_ggtyp.dat"
OPEN (32, FILE=file_out, FORM='UNFORMATTED', STATUS="REPLACE", &
  ACCESS='DIRECT', RECL=4)

irec = 1
DO khr = 0,23
DO kpar = 1,npar
  WRITE (32, REC=irec) ggtyp(2,kpar,khr)          ! valori
  irec = irec + 1
ENDDO
DO kpar = 1,npar
  WRITE (32, REC=irec) ggtyp(1,kpar,khr)          ! n.ro di dati
  irec = irec + 1
ENDDO
ENDDO

CLOSE(32)

! File ctl
WRITE (file_out2,'(2a)') TRIM(file_root),"_ggtyp.ctl"
OPEN (UNIT=33, FILE=file_out2, STATUS="REPLACE", FORM="FORMATTED")

WRITE (33,'(3a)')                "DSET   ","^",TRIM(file_out)
WRITE (33,'(a,2(a,2i2.2,i4.4))') "TITLE  ","stats from ",data1,"to",data2
IF (ABS(rmis) < 1.E5) THEN
  WRITE (33,'(a,f10.3)')         "UNDEF  ",rmis   
ELSE
  WRITE (33,'(a,e10.3)')         "UNDEF  ",rmis   
ENDIF
WRITE (33,'(2a)')                "XDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "YDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "ZDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "TDEF   ","24 linear 00Z01Jan1900 01hr"
WRITE (33,'(a,i3)')              "VARS   ",npar*2
DO kpar = 1,npar
  WRITE (33,'(a,1x,2i4,1x,2a)') ADJUSTL(str_par2(kpar)), &
    0,99,"giorno tipo",ADJUSTL(str_par(kpar))
ENDDO
DO kpar = 1,npar
  WRITE (33,'(2a,1x,2i4,1x,2a)') "nr_",ADJUSTL(str_par2(kpar)), &
    0,99,"n.ro dati valdi: ",ADJUSTL(str_par(kpar))
ENDDO
WRITE (33,'(a)')                 "ENDVARS"

CLOSE(33)

ENDIF

!--------------------------------------------------------------------------
! 5.4.1 File yrtyp (ASCII)

IF (req_prod(4)) THEN

IF (out_fmt == "txt") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_yrtyp.sta"
ELSE IF (out_fmt == "csv") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_yrtyp.csv"
ENDIF
WRITE (chfmth,'(a,i3,a,i2,a)') "(a10,",npar,"(1x,a",fw,"))"
WRITE (chfmt2,'(a,i3,3a)') "(i2.2,8x,",npar,"(1x,",TRIM(chfmt4),"))"
WRITE (chfmt5,'(a,i3,a,i2,a)') "(i2.2,8x,",npar,"(1x,i",fw,"))"

OPEN (UNIT=31, FILE=file_out, STATUS="REPLACE", FORM="FORMATTED")
out_grp = 4
IF (lstd) out_grp = 5
DO kk = 1,out_grp
  IF (out_fmt == "txt") THEN
    WRITE (31,'(a)') TRIM(title(kk))                    ! header 1 (metric)
    IF (out_liv) THEN                                   ! header 2 (liv)
      WRITE (31,chfmth) "          ",(str_liv(kpar), kpar=1,npar)
    ELSE
      WRITE (31,*)
    ENDIF
    WRITE (31,chfmth) "mm        ",(str_par(kpar), kpar=1,npar) ! head 3 (par)
    DO kmm = 1,12                                       ! dati
      IF (kk == 4) THEN
        WRITE (31,chfmt5) kmm,NINT(yrtyp(idx_stat(kk),1:npar,kmm))
      ELSE
        WRITE (31,chfmt2) kmm,yrtyp(idx_stat(kk),1:npar,kmm)
      ENDIF
    ENDDO
    WRITE (31,*)
    WRITE (31,*)

  ELSE IF (out_fmt == "csv") THEN

!   header 1 (metric)
    WRITE (31,'(a)') TRIM(title(kk))

!   header 2 (liv)
    CALL init(csvline)
    CALL csv_record_addfield(csvline,"")
    DO kpar = 1,npar
      IF (out_liv) THEN
        CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_liv(kpar))))
      ELSE
        CALL csv_record_addfield(csvline,"")
      ENDIF
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)

!   header 3 (par)
    CALL init(csvline)
    CALL csv_record_addfield(csvline,24)
    DO kpar = 1,npar
      CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_par(kpar))))
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)

!   dati
    DO kmm = 1,12
      CALL init(csvline)
      WRITE (str_data_csv,'(i4.4,2(a1,i2.2),1x,i2.2)') 0,"-",0,"-",0,kmm
      CALL csv_record_addfield(csvline,str_data_csv)
      DO kpar = 1,npar
        CALL csv_record_addfield(csvline,yrtyp(idx_stat(kk),kpar,kmm))
      ENDDO
      WRITE (31,'(a)') csv_record_getrecord(csvline)
      CALL delete(csvline)
    ENDDO
 
  ENDIF
ENDDO
CLOSE(31)

ENDIF

!--------------------------------------------------------------------------
! 5.4.2 File yrtyp (GRADS)

IF (req_prod(4)) THEN

! File dat
WRITE (file_out,'(2a)') TRIM(file_root),"_yrtyp.dat"
OPEN (32, FILE=file_out, FORM='UNFORMATTED', STATUS="REPLACE", &
  ACCESS='DIRECT', RECL=4)

irec = 1
DO kmm = 1,12
  DO kpar = 1,npar
    WRITE (32, REC=irec) yrtyp(2,kpar,kmm)          ! valori
    irec = irec + 1
  ENDDO
  DO kpar = 1,npar
    WRITE (32, REC=irec) yrtyp(1,kpar,kmm)          ! n.ro di dati
    irec = irec + 1
  ENDDO
  IF (lstd) THEN
    DO kpar = 1,npar
      WRITE (32, REC=irec) yrtyp(5,kpar,kmm)        ! dev. standard
      irec = irec + 1
    ENDDO
  ENDIF
ENDDO

CLOSE(32)

! File ctl
WRITE (file_out2,'(2a)') TRIM(file_root),"_yrtyp.ctl"
OPEN (UNIT=33, FILE=file_out2, STATUS="REPLACE", FORM="FORMATTED")

WRITE (33,'(3a)')                "DSET   ","^",TRIM(file_out)
WRITE (33,'(a,2(a,2i2.2,i4.4))') "TITLE  ","stats from ",data1,"to",data2
IF (ABS(rmis) < 1.E5) THEN
  WRITE (33,'(a,f10.3)')         "UNDEF  ",rmis   
ELSE
  WRITE (33,'(a,e10.3)')         "UNDEF  ",rmis   
ENDIF
WRITE (33,'(2a)')                "XDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "YDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "ZDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "TDEF   ","12 linear 00Z01Jan1900 01mo"
IF (lstd) THEN
  WRITE (33,'(a,i3)')              "VARS   ",npar*3
ELSE
  WRITE (33,'(a,i3)')              "VARS   ",npar*2
ENDIF
DO kpar = 1,npar
  WRITE (33,'(a,1x,2i4,1x,2a)') ADJUSTL(str_par2(kpar)), &
    0,99,"anno tipo",ADJUSTL(str_par(kpar))
ENDDO
DO kpar = 1,npar
  WRITE (33,'(2a,1x,2i4,1x,2a)') "nr_",ADJUSTL(str_par2(kpar)), &
    0,99,"n.ro dati valdi: ",ADJUSTL(str_par(kpar))
ENDDO
IF (lstd) THEN
  DO kpar = 1,npar
    WRITE (33,'(2a,1x,2i4,1x,2a)') "std_",ADJUSTL(str_par2(kpar)), &
      0,99,"n.ro dati valdi: ",ADJUSTL(str_par(kpar))
  ENDDO
ENDIF
WRITE (33,'(a)')                 "ENDVARS"

CLOSE(33)

ENDIF

!--------------------------------------------------------------------------
! 5.5.1 File daily (ASCII)

IF (req_prod(5)) THEN

IF (out_fmt == "txt") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_daily.sta"
ELSE IF (out_fmt == "csv") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_daily.csv"
ENDIF
WRITE (chfmth,'(a,i3,a,i2,a)') "(a10,",npar,"(1x,a",fw,"))"
WRITE (chfmt2,'(a,i3,3a)') &
  "(i4.4,2(1x,i2.2),",npar,"(1x,",TRIM(chfmt4),"))"
WRITE (chfmt5,'(a,i3,a,i2,a)') &
  "(i4.4,2(1x,i2.2),",npar,"(1x,i",fw,"))"

OPEN (UNIT=31, FILE=file_out, STATUS="REPLACE", FORM="FORMATTED")
DO kk = 1, 4
  IF (out_fmt == "txt") THEN
    WRITE (31,'(a)') TRIM(title(kk))                    ! header 1 (metric)
    IF (out_liv) THEN                                   ! header 2 (liv)
      WRITE (31,chfmth) "          ",(str_liv(kpar), kpar=1,npar)
    ELSE
      WRITE (31,*)
    ENDIF
    WRITE (31,chfmth) "aaaa mm gg",(str_par(kpar), kpar=1,npar) ! head 3 (par)
    DO kday = 1,ndays                                   ! dati
      data_dum = data1 + kday - 1
      IF (kk == 4) THEN
        WRITE (31,chfmt5) data_dum%yy,data_dum%mm,data_dum%dd, &
          NINT(daily(idx_stat(kk),1:npar,kday))
      ELSE
        WRITE (31,chfmt2) data_dum%yy,data_dum%mm,data_dum%dd, &
          daily(idx_stat(kk),1:npar,kday)
      ENDIF
    ENDDO
    WRITE (31,*)
    WRITE (31,*)

  ELSE IF (out_fmt == "csv") THEN

!   header 1 (metric)
    WRITE (31,'(a)') TRIM(title(kk))

!   header 2 (liv)
    CALL init(csvline)
    CALL csv_record_addfield(csvline,"")
    DO kpar = 1,npar
      IF (out_liv) THEN
        CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_liv(kpar))))
      ELSE
        CALL csv_record_addfield(csvline,"")
      ENDIF
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)

!   header 3 (par)
    CALL init(csvline)
    CALL csv_record_addfield(csvline,ndays)
    DO kpar = 1,npar
      CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_par(kpar))))
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)

!   dati
    DO kday = 1,ndays
      data_dum = data1 + kday - 1
      CALL init(csvline)
      WRITE (str_data_csv,'(i4.4,2(a1,i2.2),1x,i2.2)') &
        data_dum%yy,"-",data_dum%mm,"-",data_dum%dd,0
      CALL csv_record_addfield(csvline,str_data_csv)
!     WRITE (ch10,'(2(i2.2,a1),i4.4)') &
!        data_dum%dd,"/",data_dum%mm,"/",data_dum%yy
!     CALL csv_record_addfield(csvline,ch10)
!     CALL csv_record_addfield(csvline,0)
      DO kpar = 1,npar
        CALL csv_record_addfield(csvline,daily(idx_stat(kk),kpar,kday))
      ENDDO
      WRITE (31,'(a)') csv_record_getrecord(csvline)
      CALL delete(csvline)
    ENDDO
 
  ENDIF
ENDDO
CLOSE(31)

ENDIF

!--------------------------------------------------------------------------
! 5.5.2 File daily (GRADS)

IF (req_prod(5)) THEN

! File dat
WRITE (file_out,'(2a)') TRIM(file_root),"_daily.dat"
OPEN (32, FILE=file_out, FORM='UNFORMATTED', STATUS="REPLACE", &
  ACCESS='DIRECT', RECL=4)

irec = 1
DO kday = 1,ndays
DO kpar = 1,npar
  WRITE (32, REC=irec) daily(2,kpar,kday)          ! media gg
  irec = irec + 1
ENDDO
DO kpar = 1,npar
  WRITE (32, REC=irec) daily(3,kpar,kday)          ! max gg
  irec = irec + 1
ENDDO
DO kpar = 1,npar
  WRITE (32, REC=irec) daily(4,kpar,kday)          ! min gg
  irec = irec + 1
ENDDO
DO kpar = 1,npar
  WRITE (32, REC=irec) daily(1,kpar,kday)          ! n.ro di dati
  irec = irec + 1
ENDDO
ENDDO

CLOSE(32)

! File ctl
WRITE (file_out2,'(2a)') TRIM(file_root),"_daily.ctl"
OPEN (UNIT=33, FILE=file_out2, STATUS="REPLACE", FORM="FORMATTED")

WRITE (33,'(3a)')                "DSET   ","^",TRIM(file_out)
WRITE (33,'(a,2(a,2i2.2,i4.4))') "TITLE  ","stats from ",data1,"to",data2
IF (ABS(rmis) < 1.E5) THEN
  WRITE (33,'(a,f10.3)')         "UNDEF  ",rmis   
ELSE
  WRITE (33,'(a,e10.3)')         "UNDEF  ",rmis   
ENDIF
WRITE (33,'(2a)')                "XDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "YDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "ZDEF   ","1 linear 1 1"
WRITE (33,'(a,i4,3a)')           "TDEF   ",ndays, &
  " linear 00Z",grads_date(data1)," 1dy"
WRITE (33,'(a,i3)')              "VARS   ",npar*4
DO kpar = 1,npar
  WRITE (33,'(a,1x,2i4,1x,2a)') ADJUSTL(str_par2(kpar)), &
    0,99,"media giornaliera: ",ADJUSTL(str_par(kpar))
ENDDO
DO kpar = 1,npar
  WRITE (33,'(2a,1x,2i4,1x,2a)') "mx",ADJUSTL(str_par2(kpar)), &
    0,99,"massimo giornaliero: ",ADJUSTL(str_par(kpar))
ENDDO
DO kpar = 1,npar
  WRITE (33,'(2a,1x,2i4,1x,2a)') "mn",ADJUSTL(str_par2(kpar)), &
    0,99,"minimo giornaliero: ",ADJUSTL(str_par(kpar))
ENDDO
DO kpar = 1,npar
  WRITE (33,'(2a,1x,2i4,1x,2a)') "nr",ADJUSTL(str_par2(kpar)), &
    0,99,"n.ro dati valdi: ",ADJUSTL(str_par(kpar))
ENDDO
WRITE (33,'(a)')                 "ENDVARS"

CLOSE(33)

ENDIF

!--------------------------------------------------------------------------
! 5.6.1 File month (ASCII)

IF (req_prod(6)) THEN

IF (out_fmt == "txt") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_month.sta"
ELSE IF (out_fmt == "csv") THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_month.csv"
ENDIF
WRITE (chfmth,'(a,i3,a,i2,a)') "(a10,",npar,"(1x,a",fw,"))"
WRITE (chfmt2,'(a,i3,3a)') &
  "(i4.4,2(1x,i2.2),",npar,"(1x,",TRIM(chfmt4),"))"
WRITE (chfmt5,'(a,i3,a,i2,a)') &
  "(i4.4,2(1x,i2.2),",npar,"(1x,i",fw,"))"

OPEN (UNIT=31, FILE=file_out, STATUS="REPLACE", FORM="FORMATTED")
DO kk = 1,4
  IF (out_fmt == "txt") THEN
    WRITE (31,'(a)') TRIM(title(kk))                    ! header 1 (metric)
    IF (out_liv) THEN                                   ! header 2 (liv)
      WRITE (31,chfmth) "          ",(str_liv(kpar), kpar=1,npar)
    ELSE
      WRITE (31,*)
    ENDIF
    WRITE (31,chfmth) "aaaa mm gg",(str_par(kpar), kpar=1,npar) ! head 3 (par)
    DO kmonth = 1,nmonths
      month_tot = data1%mm + kmonth -1
      data_dum%yy = data1%yy + (month_tot - 1) / 12
      data_dum%mm = MOD(month_tot - 1, 12) + 1
      data_dum%dd = 1
      IF (kk == 4) THEN
        WRITE (31,chfmt5) data_dum%yy,data_dum%mm,data_dum%dd, &
          NINT(month(idx_stat(kk),1:npar,kmonth))
      ELSE
        WRITE (31,chfmt2) data_dum%yy,data_dum%mm,data_dum%dd, &
          month(idx_stat(kk),1:npar,kmonth)
      ENDIF
    ENDDO
    WRITE (31,*)
    WRITE (31,*)

  ELSE IF (out_fmt == "csv") THEN

!   header 1 (metric)
    WRITE (31,'(a)') TRIM(title(kk))

!   header 2 (liv)
    CALL init(csvline)
    CALL csv_record_addfield(csvline,"")
    DO kpar = 1,npar
      IF (out_liv) THEN
        CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_liv(kpar))))
      ELSE
        CALL csv_record_addfield(csvline,"")
      ENDIF
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)

!   header 3 (par)
    CALL init(csvline)
    CALL csv_record_addfield(csvline,nmonths)
    DO kpar = 1,npar
      CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_par(kpar))))
    ENDDO
    WRITE (31,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)

!   dati
    DO kmonth = 1,nmonths
      month_tot = data1%mm + kmonth -1
      data_dum%yy = data1%yy + (month_tot - 1) / 12
      data_dum%mm = MOD(month_tot - 1, 12) + 1
      data_dum%dd = 1
      CALL init(csvline)
      WRITE (str_data_csv,'(i4.4,2(a1,i2.2),1x,i2.2)') &
        data_dum%yy,"-",data_dum%mm,"-",data_dum%dd,0
      CALL csv_record_addfield(csvline,str_data_csv)
!     WRITE (ch10,'(2(i2.2,a1),i4.4)') &
!        data_dum%dd,"/",data_dum%mm,"/",data_dum%yy
!     CALL csv_record_addfield(csvline,ch10)
!     CALL csv_record_addfield(csvline,0)
      DO kpar = 1,npar
        CALL csv_record_addfield(csvline,month(idx_stat(kk),kpar,kmonth))
      ENDDO
      WRITE (31,'(a)') csv_record_getrecord(csvline)
      CALL delete(csvline)
    ENDDO
 
  ENDIF
ENDDO
CLOSE(31)

ENDIF

!--------------------------------------------------------------------------
! 5.6.2 File month (GRADS)

IF (req_prod(6)) THEN

! File dat
WRITE (file_out,'(2a)') TRIM(file_root),"_month.dat"
OPEN (32, FILE=file_out, FORM='UNFORMATTED', STATUS="REPLACE", &
  ACCESS='DIRECT', RECL=4)

irec = 1
DO kmonth = 1,nmonths
DO kpar = 1,npar
  WRITE (32, REC=irec) month(2,kpar,kmonth)        ! media gg
  irec = irec + 1
ENDDO
DO kpar = 1,npar
  WRITE (32, REC=irec) month(3,kpar,kmonth)        ! max gg
  irec = irec + 1
ENDDO
DO kpar = 1,npar
  WRITE (32, REC=irec) month(4,kpar,kmonth)        ! min gg
  irec = irec + 1
ENDDO
DO kpar = 1,npar
  WRITE (32, REC=irec) month(1,kpar,kmonth)        ! n.ro di dati
  irec = irec + 1
ENDDO
ENDDO

CLOSE(32)

! File ctl
WRITE (file_out2,'(2a)') TRIM(file_root),"_month.ctl"
OPEN (UNIT=33, FILE=file_out2, STATUS="REPLACE", FORM="FORMATTED")

WRITE (33,'(3a)')                "DSET   ","^",TRIM(file_out)
WRITE (33,'(a,2(a,2i2.2,i4.4))') "TITLE  ","stats from ",data1,"to",data2
IF (ABS(rmis) < 1.E5) THEN
  WRITE (33,'(a,f10.3)')         "UNDEF  ",rmis   
ELSE
  WRITE (33,'(a,e10.3)')         "UNDEF  ",rmis   
ENDIF
WRITE (33,'(2a)')                "XDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "YDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "ZDEF   ","1 linear 1 1"
WRITE (33,'(a,i4,3a)')           "TDEF   ",nmonths, &
  " linear 00Z",grads_date( date(15,data1%mm,data1%yy) )," 1mo"
WRITE (33,'(a,i3)')              "VARS   ",npar*4
DO kpar = 1,npar
  WRITE (33,'(a,1x,2i4,1x,2a)') ADJUSTL(str_par2(kpar)), &
    0,99,"media mensile: ",ADJUSTL(str_par(kpar))
ENDDO
DO kpar = 1,npar
  WRITE (33,'(2a,1x,2i4,1x,2a)') "mx",ADJUSTL(str_par2(kpar)), &
    0,99,"massimo mensile: ",ADJUSTL(str_par(kpar))
ENDDO
DO kpar = 1,npar
  WRITE (33,'(2a,1x,2i4,1x,2a)') "mn",ADJUSTL(str_par2(kpar)), &
    0,99,"minimo mensile: ",ADJUSTL(str_par(kpar))
ENDDO
DO kpar = 1,npar
  WRITE (33,'(2a,1x,2i4,1x,2a)') "nr",ADJUSTL(str_par2(kpar)), &
    0,99,"n.ro dati valdi: ",ADJUSTL(str_par(kpar))
ENDDO
WRITE (33,'(a)')                 "ENDVARS"

CLOSE(33)

ENDIF

!--------------------------------------------------------------------------
! 5.7 files season (ASCII)

IF (req_prod(7)) THEN

DO nsea = 1,9

  IF (out_fmt == "txt") THEN
    WRITE (file_out,'(4a)') TRIM(file_root),"_SE",labsea(nsea),".sta"
  ELSE IF (out_fmt == "csv") THEN
    WRITE (file_out,'(4a)') TRIM(file_root),"_SE",labsea(nsea),".csv"
  ENDIF

  WRITE (chfmth,'(a,i3,a,i2,a)') "(a10,",npar,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,i3,3a)') &
    "(i4.4,2(1x,i2.2),",npar,"(1x,",TRIM(chfmt4),"))"
  WRITE (chfmt5,'(a,i3,a,i2,a)') &
    "(i4.4,2(1x,i2.2),",npar,"(1x,i",fw,"))"

  data_dum%dd = 1
  SELECT CASE (nsea)
  CASE(1,2,3,4)
    data_dum%mm = nsea * 3
  CASE(5)
    data_dum%mm = 4
  CASE(6)
    data_dum%mm = 10
  CASE(7)
    data_dum%mm = 1
  END SELECT

  OPEN (UNIT=31, FILE=file_out, STATUS="REPLACE", FORM="FORMATTED")
  out_grp = 4
  IF (nsea == 7 .AND. lrank) out_grp = 5
  DO kk = 1,out_grp
    IF (out_fmt == "txt") THEN
      WRITE (31,'(a)') TRIM(title(kk))                  ! header 1 (metric)
      IF (out_liv) THEN                                 ! header 2 (liv)
        WRITE (31,chfmth) "          ",(str_liv(kpar), kpar=1,npar)
      ELSE
        WRITE (31,*)
      ENDIF
      WRITE (31,chfmth) "aaaa mm gg",(str_par(kpar), kpar=1,npar) ! head 3 (par)
      DO kyear = 1,nyears
        data_dum%yy = kyear + year1 - 1
        IF (kk == 4) THEN
          WRITE (31,chfmt5) data_dum%yy,data_dum%mm,data_dum%dd, &
            NINT(season(idx_stat(kk),nsea,1:npar,kyear))
        ELSE
          WRITE (31,chfmt2) data_dum%yy,data_dum%mm,data_dum%dd, &
            season(idx_stat(kk),nsea,1:npar,kyear)
        ENDIF
      ENDDO
      WRITE (31,*)
      WRITE (31,*)

    ELSE IF (out_fmt == "csv") THEN

!     header 1 (metric)
      WRITE (31,'(a)') TRIM(title(kk))

!     header 2 (liv)
      CALL init(csvline)
      CALL csv_record_addfield(csvline,"")
      DO kpar = 1,npar
        IF (out_liv) THEN
          CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_liv(kpar))))
        ELSE
          CALL csv_record_addfield(csvline,"")
        ENDIF
      ENDDO
      WRITE (31,'(a)') csv_record_getrecord(csvline)
      CALL delete(csvline)

!     header 3 (par)
      CALL init(csvline)
      CALL csv_record_addfield(csvline,nyears)
      DO kpar = 1,npar
        CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_par(kpar))))
      ENDDO
      WRITE (31,'(a)') csv_record_getrecord(csvline)
      CALL delete(csvline)

!     dati
      DO kyear = 1,nyears
        data_dum%yy = kyear + year1 - 1
        CALL init(csvline)
        WRITE (str_data_csv,'(i4.4,2(a1,i2.2),1x,i2.2)') &
          data_dum%yy,"-",data_dum%mm,"-",data_dum%dd,0
        CALL csv_record_addfield(csvline,str_data_csv)
!       WRITE (ch10,'(2(i2.2,a1),i4.4)') &
!         data_dum%dd,"/",data_dum%mm,"/",data_dum%yy
!       CALL csv_record_addfield(csvline,ch10)
!       CALL csv_record_addfield(csvline,0)
        DO kpar = 1,npar
          CALL csv_record_addfield(csvline,season(idx_stat(kk),nsea, &
            kpar,kyear))
        ENDDO
        WRITE (31,'(a)') csv_record_getrecord(csvline)
        CALL delete(csvline)
      ENDDO
 
    ENDIF
  ENDDO
  CLOSE(31)

ENDDO

ENDIF

!--------------------------------------------------------------------------
! 5.8 file wrose (ASCII; solo se c'e' il vento!)

IF (req_prod(8)) THEN

IF (kpar_dd /= -99 .AND. kpar_ff /= -99) THEN
  WRITE (file_out,'(2a)') TRIM(file_root),"_wrose.sta"
  WRITE (chfmth,'(a,i2,a,i2,a,i2,a)') &
    "(a6,7x,",nbin(kpar_dd)-1,"(1x,",fw-3,"x,a3),1x,",fw-6,"x,a6))"
  WRITE (chfmt2,'(a,i2,3a)') &
    "(1x,a2,f7.1,3x,",nbin(kpar_dd),"(1x,",TRIM(chfmt4),"))"

  OPEN (UNIT=31, FILE=file_out, STATUS="REPLACE", FORM="FORMATTED")

  IF (nsect == 4) THEN
    WRITE (31,chfmth) "Modulo","N","E","S","W","Totale"
  ELSE IF (nsect == 8) THEN
    WRITE (31,chfmth) "Modulo","N","NE","E","SE","S","SW","W","NW","Totale"
  ELSE IF (nsect == 16) THEN
    WRITE (31,chfmth) "Modulo","N","NNE","NE","ENE","E","ESE","SE","SSE", &
                    "S","SSW","SW","WSW","W","WNW","NW","NNW","Totale"
  ENDIF

  DO kbin = 1,nbin(kpar_ff)
    WRITE (31,chfmt2) "<=",idbin(kpar_ff,kbin), &
      wrose(kbin,1:nbin(kpar_dd)-1), SUM(wrose(kbin,1:nbin(kpar_dd)-1))
  ENDDO

  WRITE (31,*)
  WRITE (chfmt2,'(a,i2,3a)') "(a13,",nbin(kpar_dd),"(1x,",TRIM(chfmt4),"))"
  WRITE (31,chfmt2) "Tot (esc.cal)", & 
    SUM(wrose(2:kbin,1:nbin(kpar_dd)-1),DIM=1), &
    SUM(wrose(2:kbin,1:nbin(kpar_dd)-1))
  WRITE (31,'(a9,f4.1,1x,f10.1)') "Calme: <=",ff_calm,REAL(ncalm)

  WRITE (31,*)
  WRITE (31,'(a13,1x,f10.1)') "Dati validi: ",REAL(cnt_ok)
  WRITE (31,'(a13,1x,f10.1)') "Dati solo dd:",REAL(cnt_noff)
  WRITE (31,'(a13,1x,f10.1)') "Dati solo ff:",REAL(cnt_nodd)
  WRITE (31,'(a13,1x,f10.1)') "Manca dd+ff: ",REAL(cnt_miss)

  CLOSE(31)
ENDIF

ENDIF

!--------------------------------------------------------------------------
! 5.9 file hour (GRADS)

IF (req_prod(9)) THEN

! File dat
WRITE (file_out,'(2a)') TRIM(file_root),"_hours.dat"
OPEN (32, FILE=file_out, FORM='UNFORMATTED', STATUS="REPLACE", &
  ACCESS='DIRECT', RECL=4)

irec = 1
DO khour = 1,nrep
DO kpar = 1,npar
  WRITE (32, REC=irec) hours(kpar,khour)          ! valori orari
  irec = irec + 1
ENDDO
ENDDO

CLOSE(32)

! File ctl
WRITE (file_out2,'(2a)') TRIM(file_root),"_hours.ctl"
OPEN (UNIT=33, FILE=file_out2, STATUS="REPLACE", FORM="FORMATTED")

WRITE (33,'(3a)')                "DSET   ","^",TRIM(file_out)
WRITE (33,'(a,2(a,2i2.2,i4.4))') "TITLE  ","time series from ",data1,"to",data2
IF (ABS(rmis) < 1.E5) THEN
  WRITE (33,'(a,f10.3)')         "UNDEF  ",rmis   
ELSE
  WRITE (33,'(a,e10.3)')         "UNDEF  ",rmis   
ENDIF
WRITE (33,'(2a)')                "XDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "YDEF   ","1 linear 1 1"
WRITE (33,'(2a)')                "ZDEF   ","1 linear 1 1"
WRITE (33,'(a,i4,3a)')           "TDEF   ",nrep, &
  " linear 00Z",grads_date(data1)," 1dy"
WRITE (33,'(a,i3)')              "VARS   ",npar
DO kpar = 1,npar
  WRITE (33,'(a,1x,2i4,1x,2a)') ADJUSTL(str_par2(kpar)), &
    0,99,ADJUSTL(str_par(kpar))
ENDDO
WRITE (33,'(a)')                 "ENDVARS"

CLOSE(33)

ENDIF

WRITE (*,*) "Scritte statistiche"

STOP

!--------------------------------------------------------------------------
! 6) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_in)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore leggendo headers ",TRIM(file_in)
STOP 3

9997 CONTINUE
WRITE (*,*) "Errore leggendo lista parametri (ADVANCE=NO) ",TRIM(file_in)
STOP 4

9996 CONTINUE
IF (inp_data == "hhr" .OR. inp_data == "ddy" .OR. inp_data == "tem") THEN
  lline = k + 3
ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
  lline = k + 6
ENDIF
WRITE (*,*) "Errore leggendo colonna date ",TRIM(file_in)," record ",lline
STOP 5

9995 CONTINUE
IF (inp_data == "hhr" .OR. inp_data == "ddy" .OR. inp_data == "tem") THEN
  lline = k + 3
ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
  lline = k + 6
ENDIF
WRITE (*,*) "Errore leggendo i dati ",TRIM(file_in),"record ",lline
STOP 6

9994 CONTINUE
WRITE (*,*) "Trovata data/ora illegale, mi fermo ",data_dum,hrdum
IF (hrdum < 0 .OR. hrdum > 23) WRITE (*,*) "hrdum ",hrdum," (0-23)"
IF (kday < 1 .OR. kday > ndays) WRITE (*,*) "kday ",kday," (1-",ndays,")"
IF (kmonth < 1 .OR. kmonth > nmonths) &
  WRITE (*,*) "kmonth ",kmonth," (1-",nmonths,")"
IF (ksea3 < 1 .OR. ksea3 > 4) WRITE (*,*) "ksea3 ",ksea3," (1-4)"
IF (ksea6 < 1 .OR. ksea6 > 2) WRITE (*,*) "ksea6 ",ksea6," (1-2)"
IF (kyear3 < 1 .OR. kyear3 > nyears) &
  WRITE (*,*) "kyear3 ",kyear3," (1-",nyears,")"
IF (kyear6 < 1 .OR. kyear6 > nyears) &
  WRITE (*,*) "kyear6 ",kyear6," (1-",nyears,")"
IF (kyear12 < 1 .OR. kyear12 > nyears) &
  WRITE (*,*) "kyear12 ",kyear12," (1-",nyears,")"
STOP 7

9993 CONTINUE
WRITE (*,*) "Erroraccio nel calcolo del rank"
STOP 98

END PROGRAM stat_orari

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE
INTEGER :: mxstaz

!            1234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) 
WRITE (*,*) "stat_orari.exe [-h] [-o/-s/-sx/-d/-t] filein"
WRITE (*,*) "           [-rank N] [-std] [-miss0]  [-prod=LIST] [-csv] [-liv] [-ndec N]"
WRITE (*,*)
WRITE (*,*) "filein   : file con i dati. Il par. successivo determina il suo formato:"
WRITE (*,*) " -o      : estra_orari o estra_qaria con dati orari (default)"
WRITE (*,*) " -s      : seriet con notazione decimale"
WRITE (*,*) " -sx     : seriet con notazione esponenziale"
WRITE (*,*) " -d      : estra_qaria con dati giornalieri o segmento di file .sta "
WRITE (*,*) " -t      : input nel formato prodotto da trasp_temp"
WRITE (*,*) " -q      : come -d (per compatibilita' con vecchie procedure)"
WRITE (*,*) ""
WRITE (*,*) " -rank N : aggiunge al file SEyea.sta un gruppo relativo all'N-mo"
WRITE (*,*) "           valore piu' alto"
WRITE (*,*) " -std    : aggiunge al file yrtyp.sta un gruppo relativo alla deviazione"
WRITE (*,*) "           standard dei valori"
WRITE (*,*) ""
WRITE (*,*) " -prod   : scrive solo i files relativi alle elaborazioni specificate;"
WRITE (*,*) "           LIST e' un elenco di campi (stats,dfreq,ggtyp,yrtyp,daily,"
WRITE (*,*) "           month,seaso,wrose,hours) separati da virgole e senza spazi."
WRITE (*,*) "           Default: li scrive tutti"
WRITE (*,*) " -csv    : scrive i files ASCII in formato csv (def: sep. da spazi)"
WRITE (*,*) " -liv    : aggiunge header con le quote dei livelli (solo fmt seriet)"
WRITE (*,*) " -ndec N : numero di decimali nei files .sta (-1 per notazione exp)"
WRITE (*,*) " -h      : visualizza questo help"
WRITE (*,*) 
WRITE (*,*) "Calcola alcune statistiche sui dati osservati presenti in filein."
WRITE (*,*) "Il programma ignora le stazioni, e media tutti i dati relativi a "
WRITE (*,*) "ciascuna data-ora (appendendo opportunamente i files di input e'"
WRITE (*,*) "quindi ossibile calcolare medie di settore, ecc.)"
WRITE (*,*) 

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

PROGRAM proc_st1a
!--------------------------------------------------------------------------
! Programma della catena Calmet.
! Legge le osservazioni al suolo estratte da estra_surf_calmet (files 
! *.st1a) e le riscrive (files *.st2)
! Legge l'elenco di stazioni e parametri da surf_req_db.lst e le date da 
! date_calmet.inp
!
! Operazioni compiute:
! - converte unita' di misura (da Oracle a Calmet)
! - calcola ceiling height e codice di precipitazione
! - se richiesto, interpola nel tempo i dati mancanti (NON IMPLEMENTATO)
! - se presenti, toglie le scadenze orarie esterne al run di Calmet
!   (code per interpolazione)
!
! Metodo:
! 1) Legge tutti i dati, calcola ceil. e code precip., interpola nel tempo 
!    usando l'intervallo specificato dall'utente (ciclo staz1)
! 2) Se ci sono delle ore senza dati di cloud o ceil, le riempie come puo'
! 3) converte le unita' di misura, toglie i dati esterni all'intervallo di
!    integrazione, scrive output e log. (ciclo staz2)
!
! Compilazione:
! Usa il modulo per la gestione date date_hander.f90
!
! Uso:
! proc_st1a.exe filedate filesurf hh_int1 hh_int2 [-force]
!
! Note:
! - Tracciato files .st1a (dati grezzi da archivio Oracle): 
!   1 record header, poi (6iN, 13f8.1): staz,net,anno,mese,giorno,ora 
!    ff, dd,tipo_nubi(h,m,l),tcc,lcc, tt, rh,prs,tpres,prc6h,prc1h
!  [m/s,grd      WMO WMO WMO  8i  8i   C   % hPa   WMO    mm    mm]
!
! - Tracciato files .st2 (come surf.dat per una sola stazione): 
!   1 record header, poi (6iN, 8f8.1): staz,net,anno,mese,giorno,ora,
!    ff, dd, ceil,  tcc, tt, rh, prs,cod_prc 
!   [m/s grd ft*100 10i  K   %   mb  Calmet ]
!
! - I files .st1a (input) devono contenere tutte le scadenze specificate in
!   date_calmet.inp (intervallo di estrazione), in ordine cronologico.
!
!                         V2.0.1, Enrico 10/09/2012, da tappabuchi.f (Manu)
!--------------------------------------------------------------------------
USE date_handler

IMPLICIT NONE

! Parametri costanti
CHARACTER(LEN=80), PARAMETER :: file_log_int = "interp_st1a.log"

REAL, PARAMETER :: rmis =  9999.        ! in & out, non modificare (calmet)
REAL, PARAMETER :: dtr=3.14159265/180.0 ! conversione gradi -> radianti
INTEGER, PARAMETER :: imis_par = -99    ! param. non richiesto in filesurf

! Arrays per dati di input, di lavoro, di output
REAL :: rpar_dum(13)
REAL, ALLOCATABLE :: rpar_in(:,:),code_prc_in(:),ceil_in(:)
REAL, ALLOCATABLE :: rpar_w1(:,:,:),code_prc_w1(:,:),ceil_w1(:,:)
REAL, ALLOCATABLE :: rpar_w2(:,:,:),code_prc_w2(:,:),ceil_w2(:,:)
REAL, ALLOCATABLE :: rpar_w3(:,:),rpar_out(:,:)
INTEGER, ALLOCATABLE :: flag_int(:,:,:),nok(:),id_rete(:),id_user(:)

! Altre variabili del programma
TYPE(date) :: data1r,data2r,data1e,data2e,data_rq,data_fil
REAL :: prc,lcc,val1,val2,val
REAL :: ff_mod,ff_comp,ff1,ff2,dd,dd1,dd2,uu,uu1,uu2,vv,vv1,vv2
REAL :: delta1,delta2,delta_dd
INTEGER :: id_db,id_par(15),flag_par(13)
INTEGER :: hh1r,hh2r,hh1e,hh2e,hr_fil,hr_rq,hh
INTEGER :: ios,ios1,ios2,eof,eor,idum,idum2,khr_next_prc06,nok2,nint,nmis
INTEGER :: kstaz,khr,k,kpar,p1,p2,cut1,cut2
INTEGER :: nhh_run,nhh_est,ndays,nstaz
INTEGER :: hh_int1,hh_int2,hh_int,cnt(0:5),cnt_hh_inv,cnt_par,dh,dhmin
CHARACTER(LEN=200) :: chrec
CHARACTER(LEN=80) :: filein,fileout,filedate,filesurf,chpar,chfmt
CHARACTER (LEN=6) :: str_rete
LOGICAL :: force

!==========================================================================
! 1) Preliminari

! 1.1) Parametri da riga comandi

! Valori di default delle opzioni
force = .FALSE.
hh_int1 = 0
hh_int2 = 0

! Leggo e interpreto i parametri
filedate = ""
filesurf = ""
cnt_par = 0

DO kpar = 1,5
  CALL getarg(kpar,chpar)

  IF (TRIM(chpar) == "-force") THEN
    force = .TRUE.

  ELSE IF (TRIM(chpar) /= "") THEN
    cnt_par = cnt_par + 1

    SELECT CASE (cnt_par)
    CASE(1)
      filedate = TRIM(chpar)
    CASE(2)
      filesurf = TRIM(chpar)
    CASE(3)
      IF (TRIM(chpar) /= "") READ (chpar,*,IOSTAT=ios1) hh_int1
    CASE(4)
      IF (TRIM(chpar) /= "") READ (chpar,*,IOSTAT=ios2) hh_int2
    END SELECT

  ENDIF
ENDDO

IF (TRIM(filedate) == "" .OR. TRIM(filedate) == "-h"  .OR. &
    TRIM(filesurf) == "" .OR. ios1/= 0 .OR. hh_int1 < 0 .OR. &
    ios2 /= 0 .OR. hh_int2 < 0) THEN
  CALL scrive_help
  STOP
ENDIF

! 1.2) Lettura date richieste e calcoli relativi
OPEN (UNIT=30, FILE=filedate, STATUS="OLD", ACTION="READ",ERR=9999)
READ (30,*,ERR=9999)
READ (30,*,ERR=9999)
READ (30,*,ERR=9999)
READ (30,'(i4,3i2)',ERR=9999) data1r%yy,data1r%mm,data1r%dd,hh1r
READ (30,'(i4,3i2)',ERR=9999) data2r%yy,data2r%mm,data2r%dd,hh2r
READ (30,'(i4,3i2)',ERR=9999) data1e%yy,data1e%mm,data1e%dd,hh1e
READ (30,'(i4,3i2)',ERR=9999) data2e%yy,data2e%mm,data2e%dd,hh2e
CLOSE(30)

! ampiezza in ore dell'intervallo dati e run
nhh_run = 24 * (data2r-data1r) + hh2r - hh1r + 1
nhh_est = 24 * (data2e-data1e) + hh2e - hh1e + 1

! posizione dell'intervallo run nell'intervallo dati
cut1 = (data1r - data1e)*24 + hh1r - hh1e + 1
cut2 = (data2r - data1e)*24 + hh2r - hh1e + 1

IF (cut1 <= 0 .OR. cut1 > nhh_est .OR. cut2 <= 0 .OR. cut2 > nhh_est) THEN
  WRITE (*,*) "Errore proc_st1a: l'intervallo del run non e' compreso ", &
              "in quello dei dati"
  STOP
ELSE IF (cut1 > cut2 .OR. cut2-cut1+1 /= nhh_run) THEN
  WRITE (*,*) "Errore proc_st1a: internal error 1"
  STOP
ENDIF

! 1.3) Leggo una prima volta filesurf per contare le stazioni richieste
CALL get_eof_eor(eof,eor)
OPEN (UNIT=31, FILE=filesurf, STATUS="OLD", ACTION="READ",ERR=9998)
READ (31,*,ERR=9998)
READ (31,*,ERR=9998)

nstaz = 0
DO kstaz = 1,HUGE(kstaz)
  READ (31,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (chrec == "") CYCLE
  nstaz = nstaz + 1
ENDDO
CLOSE (31)

! 1.4) Alloco array di lavoro e di output
! Parametri letti dai files .st1a
ALLOCATE (rpar_in(13,nhh_est))    ! parametri misurati
ALLOCATE (ceil_in(nhh_est))       ! ceiling stimata
ALLOCATE (code_prc_in(nhh_est))   ! code precip. stimato

! Parametri dopo l'interpolazione temporale
ALLOCATE (rpar_w1(13,nhh_est,nstaz))     ! parametri misurati
ALLOCATE (ceil_w1(nhh_est,nstaz))        ! ceiling 
ALLOCATE (code_prc_w1(nhh_est,nstaz))    ! code precip. 

! Parametri dopo il recupero delle ore interamente mancanti
ALLOCATE (rpar_w2(13,nhh_est,nstaz))     ! parametri misurati
ALLOCATE (ceil_w2(nhh_est,nstaz))        ! ceiling 
ALLOCATE (code_prc_w2(nhh_est,nstaz))    ! code precip. 

! Parametri in output (comprese ceiling e code precip.)
ALLOCATE (rpar_w3(8,nhh_est))     ! dopo conversione unita' di misura
ALLOCATE (rpar_out(8,nhh_run))    ! dopo ritaglio interv. temporale

! Altri arrays
ALLOCATE (flag_int(13+2,nhh_est,nstaz))  ! flag dell'interpolazione
ALLOCATE (nok(nstaz))                    ! n.ro osservaziooni valide
ALLOCATE (id_rete(nstaz),id_user(kstaz)) ! identificativi stazioni


WRITE (*,'(3(a,i3))') "proc_st1a: scadenze orarie dati ",nhh_est, &
  ", run ",nhh_run,", stazioni ",nstaz


!==========================================================================
! 2) Primo ciclo sulle stazioni (lettura e interp. temporale ordinaria)

WRITE (chfmt,'(a,i2,a)') "(i3,1x,a6,2(1x,i5),",15,"(1x,i3))"

OPEN (UNIT=31, FILE=filesurf, STATUS="OLD", ACTION="READ",ERR=9998)
READ (31,*,ERR=9998)
READ (31,*,ERR=9998)

staz1: DO kstaz = 1,nstaz

!--------------------------------------------------------------------------
! 2.0) ri-inizializzo a manca dato gli array di lettura (forse superfluo)
  rpar_in(:,:) = rmis
  ceil_in(:) = rmis
  code_prc_in(:) = rmis

!--------------------------------------------------------------------------
! 2.1) leggo da filesurf i parametri estratti (codici oracle di 15 param.),
!      e definisco quali dei 13 parametri "condensati" (i.e. tolti Td e 
!      Prc12) devono essere inclusi nell'output

  READ (31,'(a)') chrec

  IF (chrec == "") CYCLE
  READ (chrec,chfmt,IOSTAT=ios) id_rete(kstaz),str_rete,id_db, &
    id_user(kstaz),id_par(1:15)
  IF (ios /= 0) GOTO 9998

  flag_par(:) = 0

  DO kpar = 1,13
    SELECT CASE (kpar)
    CASE (1:8)    ! da FF a TT
      IF (id_par(kpar) /= imis_par) flag_par(kpar) = 1

    CASE (9)      ! RH
      IF ( (id_par(8) /= imis_par .AND. id_par(9) /= imis_par) .OR. &
        id_par(10) /= imis_par) flag_par(kpar) = 1

    CASE (10:11)  ! Prs, TPres
      IF (id_par(kpar+1) /= imis_par) flag_par(kpar) = 1

    CASE (12)     ! Prc 06h
      IF (id_par(13) /= imis_par .AND. id_par(14) /= imis_par) &
        flag_par(kpar) = 1

    END SELECT
  ENDDO

!--------------------------------------------------------------------------
! 2.2) Leggo tutti i report orari richiesti e salvo i dati (senza nessuna 
!      modifica) sull'array di ingresso rpar_in

  WRITE (filein,'(i3.3,a1,i5.5,a5)') &
    id_rete(kstaz),"_",id_user(kstaz),".st1a"
  OPEN (UNIT=32, FILE=filein, STATUS="OLD", ACTION="READ",ERR=9997)
  READ (32,*)

  DO khr = 1,nhh_est
    ndays = (hh1e + khr - 1) / 24
    data_rq = data1e + ndays
    hr_rq = MOD(hh1e + khr - 1, 24)

!    READ (32,'(i5,1x,i3.3,1x,i4.4,3(1x,i2.2),13f8.1)',IOSTAT=ios) &
    READ (32,*,IOSTAT=ios) &
      idum,idum2,data_fil%yy,data_fil%mm,data_fil%dd,hr_fil, &
      rpar_dum(1:13)

!   Se la stazione o l'ora non corrispondono, passo alla staz. successiva
    IF (ios /= 0) THEN
      WRITE (*,*) "Errore leggendo ",TRIM(filein)," scad: ",khr
      CYCLE staz1
    ELSE IF (idum /= id_user(kstaz) .OR. idum2 /= id_rete(kstaz) .OR. &
        data_fil /= data_rq .OR. hr_fil /= hr_rq) THEN
      WRITE (*,*) "Dati disallineati in ",TRIM(filein)," scad: ",khr
      IF (idum /= id_user(kstaz)) WRITE (*,*) "id_usr: ",idum,id_user(kstaz)
      IF (idum2 /= id_rete(kstaz)) WRITE (*,*) "id_rete: ", &
        idum2,id_rete(kstaz)
      IF (data_fil /= data_rq) WRITE (*,'(a,6i5)') "data: ",data_fil,data_rq
      IF (hr_fil /= hr_rq) WRITE (*,*) "ora: ",hr_fil,hr_rq
      CYCLE staz1
    ENDIF

!   Salvo i dati nell'array di input, filtrando quelli non richiesti
    WHERE (rpar_dum(1:13) /= rmis .AND. flag_par(1:13) == 1)
      rpar_in(1:13,khr) = rpar_dum(1:13)
    ELSEWHERE
      rpar_in(1:13,khr) = rmis
    ENDWHERE

  ENDDO                                              ! scadenze (ore)
  CLOSE(32)

!--------------------------------------------------------------------------
! 2.3) Calcolo i parametri derivati (codice di precipit. e ceiling height),
!      per tutte le scadenze estratte

  DO khr = 1,nhh_est

!   Ceiling. La copertura bassa non puo' essere maggiore di quella totale!
    IF (rpar_in(7,khr) > rpar_in(6,khr)) THEN
      lcc = rpar_in(6,khr)
    ELSE
      lcc = rpar_in(7,khr)
    ENDIF

    CALL ceilh(rpar_in(6,khr),lcc,rpar_in(3,khr), &
      rpar_in(4,khr),rpar_in(5,khr),ceil_in(khr),rmis)

!   Codice di precipitazione
    khr_next_prc06 = khr + 5 - MOD(khr+hh1e+4, 6)
    IF (rpar_in(13,khr) /= rmis) THEN
      prc = rpar_in(13,khr)
    ELSE IF (rpar_in(12,khr_next_prc06) /= rmis) THEN
      prc = rpar_in(12,khr_next_prc06) / 6. 
    ELSE
      prc = rmis
    ENDIF

    CALL encode_prc(rpar_in(11,khr),prc,code_prc_in(khr),rmis)

  ENDDO

!--------------------------------------------------------------------------
! 2.4) Interpolo nel tempo i dati mancanti: leggo i dati dagli array *in, 
!      e li ricopio sugli array *w1 aggiungendo i dati interpolati, usando
!      la finestra temporale specificata dall'utente

  rpar_w1(:,:,kstaz) = rpar_in(:,:)
  ceil_w1(:,kstaz) = ceil_in(:)
  code_prc_w1(:,kstaz) = code_prc_in(:)
  flag_int(:,:,kstaz) = 0

!--------------------------------------------------------------------------
! 2.4.1) Parametri ordinari (tutti tranne: vento, ceiling, codice di prec.)

  DO kpar = 3,13            
    IF (kpar == 6) THEN
      hh_int = hh_int2
    ELSE
      hh_int = hh_int1
    ENDIF

    DO khr = 1,nhh_est

      IF (rpar_in(kpar,khr) /= rmis) CYCLE

!     cerco il dato buono precedente e quello successivo
      p1 = 0
      DO k = khr-1, 1, -1
        IF (rpar_in(kpar,k) /= rmis) THEN
          p1 = k
          EXIT
        ENDIF
      ENDDO

      p2 = 0
      DO k = khr+1, nhh_est
        IF (rpar_in(kpar,k) /= rmis) THEN
          p2 = k
          EXIT
        ENDIF
      ENDDO

!     se ci sono dati buoni sufficientemente vicini, interpolo
      IF (p1 == 0 .OR. p2 == 0 .OR. p2-p1 > hh_int) CYCLE

      val1 = rpar_in(kpar,p1)
      val2 = rpar_in(kpar,p2)
      rpar_w1(kpar,khr,kstaz) = val1 + (val2-val1)/REAL(p2-p1) * (khr-p1)
      flag_int(kpar,khr,kstaz) = 1

    ENDDO
  ENDDO

!--------------------------------------------------------------------------
! 2.4.2) Vento. Sono possibili vari algoritimi (rivedere!!)
!        - direzione: prendo la direzione data dall'interp. per componenti
!        - modulo:
!          * se la differenza tra le direzioni delle 2 osservazioni 
!            utilizzate e' <= 120, oppure se non ci sono 2 osservazioni di 
!            direzione, interpolo i moduli;
!          * se la differenza e' > 120, interpolo per componenti.

  DO khr = 1,nhh_est
    IF (rpar_in(1,khr) /= rmis .AND. rpar_in(2,khr) /= rmis) CYCLE

!   Interpolazione del modulo: cerco il dato buono prec. e succ.
    p1 = 0
    DO k = khr-1, 1, -1
      IF (rpar_in(1,k) /= rmis) THEN
        p1 = k
        EXIT
      ENDIF
    ENDDO

    p2 = 0
    DO k = khr+1, nhh_est
      IF (rpar_in(1,k) /= rmis) THEN
        p2 = k
        EXIT
      ENDIF
    ENDDO

!   Interpolazione del modulo: se ho trovato dati vicini, interpolo
    IF (p1 /= 0 .AND. p2 /= 0 .AND. p2-p1 <= hh_int1) THEN
      val1 = rpar_in(1,p1)
      val2 = rpar_in(1,p2)
      ff_mod = val1 + (val2-val1)/REAL(p2-p1) * (khr-p1)
    ELSE
      ff_mod = rmis
    ENDIF

!   Interpolazione per componenti: cerco il dato buono prec. e succ.
    p1 = 0
    DO k = khr-1, 1, -1
      IF (rpar_in(1,k) /= rmis .AND. rpar_in(2,k) /= rmis) THEN
        p1 = k
        EXIT
      ENDIF
    ENDDO

    p2 = 0
    DO k = khr+1, nhh_est
      IF (rpar_in(1,k) /= rmis .AND. rpar_in(2,k) /= rmis) THEN
        p2 = k
        EXIT
      ENDIF
    ENDDO

!   Interpolazione per componenti: se ho trovato dati vicini, interpolo
    IF (p1 /= 0 .AND. p2 /= 0 .AND. p2-p1 <= hh_int1) THEN
      ff1 = rpar_in(1,p1)
      ff2 = rpar_in(1,p2)
      dd1 = rpar_in(2,p1)
      dd2 = rpar_in(2,p2)

      delta1 = ABS(dd2-dd1)
      delta2 = ABS( MIN(dd2,dd1)+360. - MAX(dd2,dd1) )
      delta_dd = MIN (delta1, delta2)

      uu1 = -ff1 * SIN(dd1*dtr)
      uu2 = -ff2 * SIN(dd2*dtr)
      vv1 = -ff1 * COS(dd1*dtr)
      vv2 = -ff2 * COS(dd2*dtr)

      uu = uu1 + (uu2-uu1)/REAL(p2-p1) * (khr-p1)
      vv = vv1 + (vv2-vv1)/REAL(p2-p1) * (khr-p1)
      CALL uv2dirint_scalar(uu,vv,dd,ff_comp,1,rmis)
    ELSE
      delta_dd = rmis
      dd = rmis
      ff_comp = rmis
    ENDIF

!   Se e'possibile, assegno il nuovo valore alla direzione
    IF (rpar_in(2,khr) /= rmis) THEN               !ho la dd osservata
      CONTINUE

    ELSE IF (dd /= rmis) THEN
      rpar_w1(2,khr,kstaz) = dd
      flag_int(2,khr,kstaz) = 1

    ENDIF

!   Se e'possibile, assegno il nuovo valore al modulo
    IF (rpar_in(1,khr) /= rmis) THEN               !ho la ff osservata
      CONTINUE

    ELSE IF (delta_dd /= rmis .AND. ff_mod /= rmis .AND. &
      ff_comp /= rmis .AND. delta_dd <= 120.) THEN !ho tutto & Delta < 120
      rpar_w1(1,khr,kstaz) = ff_mod
      flag_int(1,khr,kstaz) = 2

    ELSE IF (delta_dd /= rmis .AND. ff_mod /= rmis .AND. &
      ff_comp /= rmis .AND. delta_dd > 120.) THEN  !ho tutto & Dalta > 120
      rpar_w1(1,khr,kstaz) = ff_comp
      flag_int(1,khr,kstaz) = 3

    ELSE IF (ff_mod /= rmis) THEN                  ! ho solo il modulo
      rpar_w1(1,khr,kstaz) = ff_mod
      flag_int(1,khr,kstaz) = 2

    ENDIF

  ENDDO

!--------------------------------------------------------------------------
! 2.4.3) Ceiling height e codice di precipitazione. Questi parametri assu-
!        mono valori discreti; vengono messi al valore del dato presente 
!        piu' vicino, a condizione che la distanza nel tempo non superi
!        hh_int/2

! ceiling height
  DO khr = 1,nhh_est

    IF (ceil_in(khr) /= rmis) CYCLE

!   cerco il dato buono piu' vicino
    val = rmis
    DO k = 1, hh_int2/2
      IF ((khr-k) >= 1) THEN
        IF (ceil_in(khr-k) /= rmis) THEN
          val = ceil_in(khr-k)
          EXIT
        ENDIF
      ENDIF
      IF ((khr+k) <= nhh_est) THEN
        IF (ceil_in(khr+k) /= rmis) THEN
          val = ceil_in(khr+k)
          EXIT
        ENDIF
      ENDIF
    ENDDO

!   se ho trovato un dato buono sufficientemente vicino, lo attribuisco
    IF (val /= rmis) THEN
      ceil_w1(khr,kstaz) = val
      flag_int(14,khr,kstaz) = 1
    ENDIF

  ENDDO

! codice di precipitazione
  DO khr = 1,nhh_est

    IF (code_prc_in(khr) /= rmis) CYCLE

!   cerco il dato buono piu' vicino
    val = rmis
    DO k = 1, hh_int1/2
      IF ((khr-k) >= 1) THEN
        IF (code_prc_in(khr-k) /= rmis) THEN
          val = code_prc_in(khr-k)
          EXIT
        ENDIF
      ENDIF
      IF ((khr+k) <= nhh_est) THEN
        IF (code_prc_in(khr+k) /= rmis) THEN
          val = code_prc_in(khr+k)
          EXIT
        ENDIF
      ENDIF
    ENDDO

!   se ho trovato un dato buono sufficientemente vicino, lo attribuisco
    IF (val /= rmis) THEN
      code_prc_w1(khr,kstaz) = val
      flag_int(15,khr,kstaz) = 1
    ENDIF

  ENDDO

!--------------------------------------------------------------------------
! 2.5) Salvo il n.ro totale di osservazioni presenti nell'intervallo di 
!      simulazione (solo per log)

  nok(kstaz) =  COUNT(rpar_in(1:13,cut1:cut2) /= rmis) +    &
    COUNT(ceil_in(cut1:cut2) /= rmis) +         &
    COUNT(code_prc_in(cut1:cut2) /= rmis)

ENDDO staz1                                      ! stazioni (primo loop)
CLOSE(31)


!==========================================================================
! 3) Elaborazioni simultanee su tutte le stazioni (tappo buchi e apro log)
!    - Se ci sono delle ore di simulazione senza nessun dato di cloud o 
!      ceiling, ci metto qualcosa perche' calmet possa comunque girare. 
!    - Apro log e calcolo statistiche complessive
!

! Inizializzo gli array w2 come w1
rpar_w2(:,:,:) = rpar_w1(:,:,:)
ceil_w2(:,:) = ceil_w1(:,:)
code_prc_w2(:,:) = code_prc_w1(:,:)

! Ciclo sulle ore
cnt_hh_inv = 0
hours: DO khr = cut1,cut2

  IF (ANY(rpar_w2(6,khr,:) /= rmis) .AND. ANY(ceil_w2(khr,:) /= rmis)) THEN
    CYCLE
  ELSE
    cnt_hh_inv = cnt_hh_inv + 1
  ENDIF
 
!--------------------------------------------------------------------------
! 3.1) Recupero Cloud Cover  

! 3.1.1) Provo interpolazione lineare estesa a tutti i dati    
  IF (ALL(rpar_w2(6,khr,:) == rmis)) THEN
    DO kstaz = 1, nstaz
!     cerco il dato buono precedente e quello successivo
      p1 = 0
      DO k = khr-1, 1, -1
        IF (rpar_w1(6,k,kstaz) /= rmis) THEN
          p1 = k
          EXIT
        ENDIF
      ENDDO

      p2 = 0
      DO k = khr+1, nhh_est
        IF (rpar_w1(6,k,kstaz) /= rmis) THEN
          p2 = k
          EXIT
        ENDIF
      ENDDO

!     se ci sono un dato buono prima e uno dopo, interpolo
      IF (p1 == 0 .OR. p2 == 0) CYCLE

      val1 = rpar_w1(6,p1,kstaz)
      val2 = rpar_w1(6,p2,kstaz)
      rpar_w2(6,khr,kstaz) = val1 + (val2-val1)/REAL(p2-p1) * (khr-p1)
      flag_int(6,khr,kstaz) = 4

    ENDDO
  ENDIF

! 3.1.2) Se non ho trovato nulla, provo a prendere il dato piu' vicino
  IF (ALL(rpar_w2(6,khr,:) == rmis)) THEN
    DO kstaz = 1, nstaz

!     cerco il dato valido piu' vicino
      val = rmis
      dhmin = HUGE(1)
      DO k = 1, nhh_est
        dh = ABS(k-khr)
        IF (rpar_w1(6,k,kstaz) /= rmis .AND. dh < dhmin) THEN
          val = rpar_w1(6,k,kstaz)
          dhmin = dh
        ENDIF
      ENDDO

!     sostituisco
      IF (val /= rmis) THEN
        rpar_w2(6,khr,kstaz) = val
        flag_int(6,khr,kstaz) = 5
      ENDIF

    ENDDO
  ENDIF

! 3.1.3) Da ultimo, mi invento i dati
  IF (ALL(rpar_w2(6,khr,:) == rmis)) THEN
    DO kstaz = 1, nstaz
      rpar_w2(6,khr,1:nstaz) = 4.
      flag_int(6,khr,kstaz) = 6
    ENDDO
  ENDIF

!--------------------------------------------------------------------------
! 3.2) Recupero Ceiling height

! 3.2.2) Provo a prendere il dato piu' vicino
  IF (ALL(ceil_w2(khr,:) == rmis)) THEN
    DO kstaz = 1, nstaz

!     cerco il dato valido piu' vicino
      val = rmis
      dhmin = HUGE(1)
      DO k = 1, nhh_est
        IF (ceil_w1(k,kstaz) /= rmis .AND. dh < dhmin) THEN
          val = ceil_w1(k,kstaz)
          dh = ABS(k-khr)
        ENDIF
      ENDDO

!     sostituisco
      IF (val /= rmis) THEN
        ceil_w2(khr,kstaz) = val
        flag_int(14,khr,kstaz) = 4
      ENDIF

    ENDDO
  ENDIF

! 3.2.2) Da ultimo, mi invento i dati
  IF (ALL(ceil_w2(khr,:) == rmis)) THEN
    DO kstaz = 1, nstaz
      ceil_w2(khr,1:nstaz) = 4.
      flag_int(14,khr,kstaz) = 5
    ENDDO
  ENDIF

ENDDO hours

!--------------------------------------------------------------------------
! 3.2) Apro log dell'interpolazione temporale e scrivo headers

DO k = 0,5
  cnt(k) = COUNT(flag_int(:,cut1:cut2,:) == k)
ENDDO

OPEN (UNIT=70, FILE=file_log_int, STATUS="REPLACE", ACTION="WRITE")
WRITE (70,'(i3,a)') cnt_hh_inv," ! n.ro di ore con dati inventati"
WRITE (70,*)
WRITE (70,'(3a)')    "Flag ","  totale"," (descrizione)"
WRITE (70,'(a,i8,a)') "0    ",cnt(0)," (dato osservato)"
WRITE (70,'(a,i8,a)') "1    ",cnt(1)," (interp. ordinaria)"
WRITE (70,'(a,i8,a)') "2    ",cnt(2)," (ff, interp. come dd/ff)"
WRITE (70,'(a,i8,a)') "3    ",cnt(3)," (ff, interp. per componenti)"
WRITE (70,'(a,i8,a)') "4    ",cnt(4)," (interp. estesa)"
WRITE (70,'(a,i8,a)') "5    ",cnt(5)," (dato inventato)"
WRITE (70,*)


!==========================================================================
! 4) Secondo ciclo sulle stazioni (conversione unita', output, log)

staz2: DO kstaz = 1,nstaz

  rpar_w3(:,:) = rmis
  rpar_out(:,:) = rmis

!--------------------------------------------------------------------------
! 4.1) Passo ai parametri di calmet (travaso su rpar_w3 convertendo le 
!      unita' di misura e gestendo i casi speciali)

!1 Forza del vento: invariata (m/s)
  WHERE (rpar_w2(1,:,kstaz) /= rmis) 
    rpar_w3(1,:) = rpar_w2(1,:,kstaz)
  ELSEWHERE
    rpar_w3(1,:) = rmis
  ENDWHERE

!2 Direzione del vento: invariata
  rpar_w3(2,:) = rpar_w2(2,:,kstaz)

!3 Ceiling height: invariata
  rpar_w3(3,:) = ceil_w2(:,kstaz)

!4 Copertura totale: da ottavi a decimi;
!  - se "cielo invisibile", scrivo come "cielo coperto"; 
  DO khr = 1,nhh_est
  IF (rpar_w2(6,khr,kstaz) == rmis) THEN
    rpar_w3(4,khr) = rmis
  ELSE IF (rpar_w2(6,khr,kstaz) == 9.) THEN
    rpar_w3(4,khr) = 10.
  ELSE
    rpar_w3(4,khr) = rpar_w2(6,khr,kstaz) * 10./8.
  ENDIF
  ENDDO

!5 Temperatura: da gradi C a gradi K
  WHERE (rpar_w2(8,:,kstaz) /= rmis) 
    rpar_w3(5,:) = rpar_w2(8,:,kstaz) + 273.16
  ELSEWHERE
    rpar_w3(5,:) = rmis
  ENDWHERE

!6 Umidita' relativa: invariata
  rpar_w3(6,:) = rpar_w2(9,:,kstaz)

!7 Pressione: invariata
  rpar_w3(7,:) = rpar_w2(10,:,kstaz)

!8 Codice di precipitazione: invariato
  rpar_w3(8,:) = code_prc_w2(:,kstaz)

!--------------------------------------------------------------------------
! 4.2) Tolgo le scadenze esterne all'intervallo del run

  rpar_out(1:8,1:nhh_run) = rpar_w3(1:8,cut1:cut2)

!--------------------------------------------------------------------------
! 4.3) Scrivo l'output

  WRITE (fileout,'(i3.3,a1,i5.5,a4)') &
    id_rete(kstaz),"_",id_user(kstaz),".st2"
  OPEN (UNIT=33, FILE=fileout, STATUS="REPLACE", ACTION="WRITE")
  WRITE (33,'(3a)') " staz net aaaa mm gg hh", &
    "      ff      dd    ceil     tcc      tt", &
    "      rh     prs cod_prc"

  DO khr = 1,nhh_run
    ndays = (hh1r + khr - 1) / 24
    data_rq = data1r + ndays
    hr_rq = MOD(hh1r + khr - 1, 24)
    WRITE (33,'(i5.5,1x,i3.3,1x,i4.4,3(1x,i2.2),8f8.1)') &
      id_user(kstaz),id_rete(kstaz),data_rq%yy,data_rq%mm,data_rq%dd, &
      hr_rq,rpar_out(1:8,khr)
  ENDDO
  CLOSE(33)

!--------------------------------------------------------------------------
! 4.4) scrivo log interpolazione relativo a questa stazione
  nok2 = COUNT(rpar_w2(1:13,cut1:cut2,kstaz) /= rmis .AND. &
               flag_int(1:13,cut1:cut2,kstaz) == 0) +      &
         COUNT(ceil_w2(cut1:cut2,kstaz) /= rmis .AND.      & 
               flag_int(14,cut1:cut2,kstaz) == 0) +        &
         COUNT(code_prc_w2(cut1:cut2,kstaz) /= rmis .AND.  &
               flag_int(15,cut1:cut2,kstaz) == 0)

  nint = COUNT(rpar_w2(1:13,cut1:cut2,kstaz) /= rmis .AND. &
               flag_int(1:13,cut1:cut2,kstaz) /= 0) +      &
         COUNT(ceil_w2(cut1:cut2,kstaz) /= rmis .AND.      & 
               flag_int(14,cut1:cut2,kstaz) /= 0) +        &
         COUNT(code_prc_w2(cut1:cut2,kstaz) /= rmis .AND.  &
               flag_int(15,cut1:cut2,kstaz) /= 0)

  nmis = COUNT(rpar_w2(1:13,cut1:cut2,kstaz) == rmis) +    &
         COUNT(ceil_w2(cut1:cut2,kstaz) == rmis) +         &
         COUNT(code_prc_w2(cut1:cut2,kstaz) == rmis)

  WRITE (70,*)
  WRITE (70,'(a,i2,1x,i5)') "Stazione: ",id_rete(kstaz),id_user(kstaz)
  IF (nok(kstaz) /= nok2 .OR. nok(kstaz)+nint+nmis /= 15*nhh_run) &
    WRITE (70,*) "I torni non contano!! ",nok,nok2
  WRITE (70,'(3(a,i4))') "Dati buoni: ",nok(kstaz)," ricostruiti ",nint, &
    " mancanti ",nmis
  WRITE (70,*)
  WRITE (70,'(2a)') "hh:     ff     dd   hclo   mclo   lclo    tcc    ", &
    "lcc     tt     rh    prs  tpres prc6h   prc1h   ceil   cprc    flag"

  DO k = 1,nhh_est
    hh = MOD(hh1e + k-1, 24)
    WRITE (70,'(i2.2,1x,15(1x,f6.1),3x,15(1x,i1))') &
      hh,rpar_w2(1:13,k,kstaz),ceil_w2(k,kstaz),code_prc_w2(k,kstaz), &
      flag_int(1:15,k,kstaz)
  ENDDO

ENDDO staz2                                      ! stazioni (secondo loop)

CLOSE(70)

STOP


!==========================================================================
! 5) Gestione errori

9999 CONTINUE
WRITE (*,*) "proc_st1a: errore leggendo ",TRIM(filedate)
STOP

9998 CONTINUE
WRITE (*,*) "proc_st1a: errore leggendo ",TRIM(filesurf)
STOP

9997 CONTINUE
WRITE (*,*) "proc_st1a: errore aprendo ",TRIM(filein)
STOP

END PROGRAM proc_st1a

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE encode_prc(tp,prc,rpcode,rmis)
!--------------------------------------------------------------------------
! Calcola il codice di precipitazione (reale), a partire da precipitazione
! o tempo presente.
!
! Codici di precipitazione usati (vedi anche par. 2.3.3)
! - 0:  non piove
! - 1:  pioggia leggera
! - 7:  pioggia
! - 19: neve
!--------------------------------------------------------------------------
  
IMPLICIT NONE

REAL, INTENT(IN) :: tp,prc,rmis
REAL, INTENT(OUT) :: rpcode
!--------------------------------------------------------------------------
    
IF (tp == rmis) THEN
         
! Ricavo il codice di precipitazione in base alla precipitazione
  IF (prc == rmis .OR. prc < 0.) THEN
    rpcode = rmis
  ELSE IF (prc == 0.) THEN
    rpcode = 0.
  ELSE IF (prc < 1.) THEN
    rpcode = 1.
  ELSE
    rpcode = 7.
  ENDIF

ELSE

! Lo stimo a partire dal tempo presente
  SELECT CASE (NINT(tp))
  CASE(0:49)
    rpcode = 0.                               ! non piove
  CASE(50:59)
    rpcode = 1.                               ! pioggia leggera
  CASE(60:69,80:99)
    rpcode = 7.                               ! pioggia
  CASE(70:79)
    rpcode = 19.                              ! neve
  CASE DEFAULT
    rpcode = rmis                             ! mancante
  END SELECT

ENDIF

RETURN
END SUBROUTINE encode_prc

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE ceilh(tcc,lcc,tb,tm,ta,ceil,rmis)
!--------------------------------------------------------------------------
! Calcola la ceiling height (in centinaia di piedi) a partire dalla
! copertura totale e bassa e dal tipo di nubi alte, medie e basse.
!
! Metodo:
! Secondo me andrebbe rivisto pesantemente...
!
! Note:
! Valori convenzionali per l'altezza dei tipi di nubi (vedi anche 
! par. 2.3.3)
! nubi basse: 20
! nubi medie: 120
! nubi alte: 170
! cielo sereno: 999
!--------------------------------------------------------------------------
IMPLICIT NONE

REAL, INTENT(IN) :: tcc,lcc,tb,tm,ta,rmis
REAL, INTENT(OUT) :: ceil
!
REAL, PARAMETER :: hlow = 20.
REAL, PARAMETER :: hmed = 120.
REAL, PARAMETER :: hhig = 170.
REAL, PARAMETER :: hclear = 999.

!--------------------------------------------------------------------------
! 1) copertura totale mancante o cielo invisibile
IF (tcc == 9 .OR. tcc == rmis) THEN

  IF (lcc == rmis ) THEN
    ceil = rmis
  ELSE IF (lcc >= 5. ) THEN
    ceil = hlow
  ELSE
    IF (tm /= rmis .AND. tm /= 99.) THEN
      ceil = hmed
      IF (ta /= rmis .AND. ta /= 99.) THEN
        ceil = hhig
      ELSE
        ceil = rmis
      ENDIF
    ENDIF
  ENDIF

!--------------------------------------------------------------------------
! 2) copertura totale < 50%
ELSE IF (tcc <= 4. ) THEN

  ceil = hclear

!--------------------------------------------------------------------------
! 3) copertura totale > 50%
ELSE

  IF (lcc == rmis ) THEN

!   ceil = quota della nube piu' bassa
    IF (tb /= 99. .AND. tb /= rmis) THEN
      ceil = hlow
    ELSE 
      IF (tm /= 99. .AND. tm /= rmis) THEN
        ceil = hmed
      ELSE 
        IF (ta /= 99. .AND. ta /= rmis) THEN
          ceil = hhig
        ELSE 
          ceil = rmis
        ENDIF
      ENDIF
    ENDIF

  ELSE IF (lcc >= 5. ) THEN

    ceil = hlow

  ELSE

    IF (tm /= rmis .AND. tm /= 99.) THEN
      ceil = hmed
      IF (ta /= rmis .AND. ta /= 99.) THEN
        ceil = hhig
      ELSE
        ceil = rmis
      ENDIF
    ENDIF

  ENDIF

ENDIF

RETURN
END SUBROUTINE ceilh

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE uv2dirint(u,v,dir,mod,nval,rmis)
!
! Dati i vettori di componenti u e v, ritona quelli di direzione e modulo
! Se una componente e' mancante dir e mod sono mancanti. 
! Se u=v=0, mette mod=dir=0
!

IMPLICIT NONE

INTEGER, INTENT(IN):: nval
REAL, INTENT(IN) :: u(nval),v(nval),rmis
REAL, INTENT(OUT) :: dir(nval),mod(nval)

REAL, PARAMETER :: dtr = 180./3.141592654
INTEGER k

!-------------------------------------------------------------------------
DO k = 1,nval

  IF (u(k) == rmis .OR. v(k) == rmis) THEN
    dir(k) = rmis
    mod(k) = rmis
    CYCLE
  ENDIF

  IF      (u(k) <= 0. .AND. v(k) < 0.) THEN    ! 0 - 90
    dir(k) = dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) < 0. .AND. v(k) >= 0.) THEN    ! 90 - 180
    dir(k) = 90. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) >= 0. .AND. v(k) > 0.) THEN    ! 180 - 270
    dir(k) = 180. + dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) > 0. .AND. v(k) <= 0.) THEN    ! 270 - 360
    dir(k) = 270. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) == 0. .AND. v(k) == 0.) THEN
    dir(k) = 0.
  ENDIF
 
  mod(k) = SQRT (u(k)*u(k) + v(k)*v(k))

ENDDO

RETURN
END SUBROUTINE uv2dirint

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE uv2dirint_scalar(u,v,dd,ff,nval,rmis)
!
! Dati i vettori di componenti u e v, ritona quelli di direzione e modulo
! Se una componente e' mancante dd e ff sono mancanti.
! Se u=v=0, mette ff=dd=0
!

IMPLICIT NONE

INTEGER, INTENT(IN):: nval
REAL, INTENT(IN) :: u,v,rmis
REAL, INTENT(OUT) :: dd,ff

REAL, PARAMETER :: dtr = 180./3.141592654
INTEGER k
!-------------------------------------------------------------------------

DO k = 1,nval

  IF (u == rmis .OR. v == rmis) THEN
    dd = rmis
    ff = rmis
    CYCLE
  ENDIF

  IF	  (u <= 0. .AND. v < 0.) THEN    ! 0 - 90
    dd = dtr*atan( u/v )
  ELSE IF (u < 0. .AND. v >= 0.) THEN    ! 90 - 180
    dd = 90. + dtr*atan( -v/u )
  ELSE IF (u >= 0. .AND. v > 0.) THEN    ! 180 - 270
    dd = 180. + dtr*atan( u/v )
  ELSE IF (u > 0. .AND. v <= 0.) THEN    ! 270 - 360
    dd = 270. + dtr*atan( -v/u )
  ELSE IF (u == 0. .AND. v == 0.) THEN
    dd = 0.
  ENDIF

  ff = SQRT (u*u + v*v)

ENDDO

RETURN
END SUBROUTINE uv2dirint_scalar

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!                 1234567890123456789012345678901234567890
WRITE (*,'(2a)') "Uso: proc_st1a.exe filedate filesurf [hh", &
                 "_int1 hh_int2] [-force]"
WRITE (*,'(a)')  "  filedate: date_calmet.inp"
WRITE (*,'(a)')  "  filesurf: surf_req_db.dat"
WRITE (*,'(2a)') "  hh_int1: ampiezza int. di interp dati ", &
                 "mancanti, parametri ordinari(default: 0)"
WRITE (*,'(2a)') "  hh_int2: ampiezza int. di interp dati ", &
                 "mancanti, TCC e Ceil (default: 0)"
WRITE (*,'(2a)') "  -force: forza la ricostruzione di TCC ", &
                 "e Ceil perche' Calmet giri sempre"

RETURN

END SUBROUTINE scrive_help

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


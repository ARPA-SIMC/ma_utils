PROGRAM test_inp_calmet
!
! Programma che legge i files di input di Calmet e ritorna una serie di
! informazioni sul numero di dati presente.
!
! INPUT:
! surf.dat, up_*.dat, calmet.inp
!
! OUTPUT:
! datical.dat  : dettaglio dei dati in quota e al suolo presente
! datical.str  : sintesi di datical.dat per statistiche + codice d'errore
!                (0: dati sufficienti; >0 calmet non puo' girare)
!
! USO: 
! lanciare il programma (senza parametri) nella dir in cui gira calmet
! Compilare con il modulo date_handler (dir: ~ma_aria/util/f90/src/)
!
! NOTE:
! In questa versione (pensata per la catena Calmet su ADA) l'intervallo
! tra due temp e' parametrizzato (stept)
! L'Output dovrebbe gestire run di calmet di durata fino a mxday giorni
!
! SVILUPPI:
! Aggiungere codice tipo stazione (SYNOP, EMR, AGRO, LOCALE...)
! Leggere i nomi dei files dei temp da calmet.inp
!
!                                           Versione 4.0, Enrico 27/03/2008
!--------------------------------------------------------------------------

USE date_handler

IMPLICIT NONE

!--------------------------------------------------------------------------
! 0.1 Parametri costanti
INTEGER, PARAMETER :: imis = 9999
REAL, PARAMETER :: rmis = 9999.

INTEGER, PARAMETER :: mxstaz = 100 ! n.ro max staz al suolo in surf.dat
INTEGER, PARAMETER :: mxftemp = 200! n.ro max stazioni temp richieste
INTEGER, PARAMETER :: mxday  = 5   ! durata max di un run (giorni)
INTEGER, PARAMETER :: nvar   = 7   ! par. sup. esaminati (escludo code prc)

CHARACTER (LEN=15), PARAMETER :: idvar(nvar) = &
  (/"WIND SPEED     ","WIND DIR       ","CEILING HEIGHT ", &
    "CLOUD COVER    ","TEMPERATURE    ","REL. HUMIDITY  ", &
    "SURF. PRESSURE "/)

INTEGER, PARAMETER :: mxhr = mxday*24 + 1

!--------------------------------------------------------------------------
! 0.2 Altre variabili del programma

! Funzioni
CHARACTER (LEN=100) :: exinp

! Variabili
TYPE(date) :: data1,data2
REAL ::  ws(mxstaz),wd(mxstaz),temp(mxstaz),irh(mxstaz),pres(mxstaz)
INTEGER, ALLOCATABLE :: temp_levels(:,:),flag_lam(:,:),tot_temp(:),fok(:)

INTEGER :: idstaz(mxstaz),iceil(mxstaz),icc(mxstaz),ipcode(mxstaz)
INTEGER :: staz_loc(mxstaz)
INTEGER :: presente(mxhr,mxstaz,nvar)
INTEGER :: dathr(mxhr,nvar),dat_staz(mxstaz)

INTEGER :: yy1,jj1,hh1,yy2,jj2,hh2,hh,dd,mm,yy,yy4,idummy,stept
INTEGER :: lastbl,hhtot,ibtz,imm4only,iprog
INTEGER :: nhr,nlev,nstaz,nstaz_dati,nrec,nstaz_loc,nblock
INTEGER :: k,k2,khr,krs,kvar,kst,p1,p2,st012
INTEGER :: runl,nist_req,nftemp,hno
INTEGER :: eof,eor,ios,ios1,ios2,ios3,errc,ier

CHARACTER (LEN=200) :: chrec
CHARACTER (LEN=100) :: chfmt1,chfmt2,str_dat,temp_file(mxftemp),fname
CHARACTER (LEN=80) :: str1,str2,str3,charg
CHARACTER (LEN=5) :: idtemp(mxftemp)
CHARACTER (LEN=3) :: hno_str

LOGICAL :: read_ok

!--------------------------------------------------------------------------
! 1) Preliminari
!

CALL get_eof_eor(eof, eor)

errc = 0 !codice d'errore finale (0 se i dati sono suff. a girare calmet)

presente(:,:,:) = 0
idtemp = "-----"
str1 = ""
str2 = ""
str3 = ""

OPEN (UNIT=30, FILE="datical.dat", STATUS="REPLACE", ACTION="WRITE", &
      FORM="FORMATTED")

OPEN (UNIT=31, FILE="datical.str", STATUS="REPLACE", ACTION="WRITE", &
      FORM="FORMATTED")

! Assegno stept
CALL getarg(1,charg)
IF (charg == "") THEN
  stept = 6
ELSE 
  READ (charg,*,IOSTAT=ios) stept
  IF (ios /= 0 .OR. (stept /= 1 .AND. stept /= 6 .AND.stept /= 12 .AND. &
  stept /= 24)) THEN
    WRITE (*,*) "Uso: test_inp_calmet.exe [step_temp]"
    WRITE (*,*) "step_temp: intervallo (ore) tra 2 dati di profilo."
    WRITE (*,*) "   valori ammessi: 1,6,12,24; default: 6"
    STOP
  ENDIF
ENDIF

!--------------------------------------------------------------------------
! Leggo dal file calmet.inp (se esiste)

OPEN (UNIT=22, FILE="calmet.inp", STATUS="OLD", ACTION="READ", IOSTAT=ios)
IF (ios /= 0) THEN
  errc = 1

ELSE
  nftemp = 0
  read_ok = .TRUE.
  DO 
    READ (22,'(a)',IOSTAT=ios) chrec
    IF (ios /= 0) EXIT

    str_dat = exinp(chrec)
    p1 = INDEX(str_dat,"=")
    IF (p1 == 0 .OR. p1 == LEN(TRIM(str_dat)) ) CYCLE

!   nomi dei files dei dati TEMP 
    IF (INDEX(str_dat,"UPDAT") /= 0) THEN
      IF (nftemp == mxftemp) THEN
        WRITE (*,*) "Troppe stazioni TEMP"
        ios = 99
      ELSE
        nftemp = nftemp + 1
        temp_file(nftemp) = str_dat(p1+1:)
      ENDIF

!   anno iniziale del run (gestisco anno a 2 o 4 cifre)
    ELSE IF (INDEX(str_dat,"IBYR") /= 0) THEN
      READ (str_dat(p1+1:),*,IOSTAT=ios) yy
      IF (ios == 0 .AND. yy < 50) THEN
        yy4 = yy + 2000
      ELSE IF (ios == 0 .AND. yy < 100) THEN
        yy4 = yy + 1900
      ELSE IF (ios == 0) THEN
        yy4 = yy
      ELSE
        read_ok = .FALSE.
      ENDIF

!   mese iniziale del run
    ELSE IF (INDEX(str_dat,"IBMO") /= 0) THEN
      READ (str_dat(p1+1:),*,IOSTAT=ios) mm
      IF (ios /= 0) read_ok = .FALSE.

!   giorno iniziale del run
    ELSE IF (INDEX(str_dat,"IBDY") /= 0) THEN
      READ (str_dat(p1+1:),*,IOSTAT=ios) dd
      IF (ios /= 0) read_ok = .FALSE.

!   ora iniziale del run
    ELSE IF (INDEX(str_dat,"IBHR") /= 0) THEN
      READ (str_dat(p1+1:),*,IOSTAT=ios) hh
      IF (ios /= 0) read_ok = .FALSE.

!   time zone
    ELSE IF (INDEX(str_dat,"IBTZ") /= 0) THEN
      READ (str_dat(p1+1:),*,IOSTAT=ios) ibtz
      IF (ios /= 0) read_ok = .FALSE.

!   lunghezza del run
    ELSE IF (INDEX(str_dat,"IRLG") /= 0) THEN
      READ (str_dat(p1+1:),*,IOSTAT=ios) runl
      IF (ios == 0 .AND. MOD(runl,stept) == 0) THEN
        nist_req = runl/stept + 1
      ELSE IF (ios == 0 .AND. MOD(runl,stept) /= 0) THEN
        WRITE (*,*) "irlg non divisibile per ",stept
        ios = 99
      ELSE
        read_ok = .FALSE.
      ENDIF

!   flag mm4only
    ELSE IF (INDEX(str_dat,"MM4ONLY") /= 0) THEN
      READ (str_dat(p1+1:),*,IOSTAT=ios) imm4only
      IF (ios /= 0) read_ok = .FALSE.

!   uso dati LAM
    ELSE IF (INDEX(str_dat,"IPROG") /= 0) THEN
      READ (str_dat(p1+1:),*,IOSTAT=ios) iprog
      IF (ios /= 0) read_ok = .FALSE.

    ENDIF

    IF (.NOT. read_ok) THEN
      errc = 1
      EXIT
    ENDIF

  ENDDO
  CLOSE (22)

ENDIF

! calmet.inp: mancante o errore in lettura, salto analisi Temp
IF (errc == 1) THEN
  WRITE (30,'(70a)') "RADIOSONDAGGI: ASSENTI"
  WRITE (30,*)
  WRITE (str2,'(i3,3(1x,i3))') 0,0,0,0
  errc = 1001

! nessun Temp richiesto, salto analisi Temp (c'e' errore)
ELSE IF (nftemp == 0 .AND. imm4only == 0) THEN  
  WRITE (30,'(70a)') "RADIOSONDAGGI: ASSENTI"
  WRITE (30,*)
  WRITE (str2,'(i3,3(1x,i3))') 0,0,0,0
  errc = 1000

! nessun Temp richiesto, ma uso mm4only: salto analisi Temp (nessun errore)
ELSE IF (nftemp == 0 .AND. imm4only == 1) THEN  
  WRITE (30,'(70a)') "RADIOSONDAGGI: ASSENTI"
  WRITE (30,*)
  WRITE (str2,'(i3,3(1x,i3))') 0,0,0,0

ELSE                        ! analizzo i Temp

!==========================================================================
! 2) Esamino i radiosondaggi
!

!--------------------------------------------------------------------------
! 2.1 Calcoli

  WRITE (*,*) "Temp: richeste ",nftemp," stazioni per ",nist_req," scadenze"

! Calcolo le date estreme del run (GMT)
  data1 = date(dd,mm,yy4)
  hh1 = hh + ibtz
  IF (hh1 < 0) THEN
    hh1 = hh1 + 24
    data1 = data1 - 1 
  ENDIF

  hhtot = hh + runl
  data2 = data1 + hhtot/24
  hh2 = MOD(hhtot,24)

! Alloco gli array per n.ro totale di temp in ogni file e n.ro livelli dei
! Temp agli istanti richiesti
  ALLOCATE (temp_levels(nist_req,nftemp),flag_lam(nist_req,nftemp))
  ALLOCATE (tot_temp(nftemp))
  ALLOCATE (fok(nftemp))
  temp_levels(:,:) = 0
  tot_temp(:) = 0
  fok(:) = 0

! Controllo i singoli files
  DO krs = 1,nftemp
    CALL test_temp2(temp_file(krs),data1,hh1,data2,hh2,stept,nist_req, &
      temp_levels(1:nist_req,krs),tot_temp(krs),idtemp(krs), &
      flag_lam(1:nist_req,krs),ier)
    WRITE (*,'(3a,i2)') "   stazione ",idtemp(krs)," err. code ",ier

    IF (ier == 0) THEN
      fok(krs) = 1
    ELSE
      fok(krs) = 0
      temp_levels(1:nist_req,krs) = 0
      tot_temp(krs) = 0
    ENDIF
  ENDDO

!--------------------------------------------------------------------------
! 2.2 Scrittura output

! Costruisco i formati
  WRITE (chfmt1,'(a,i2,a)') "(a5,4(2x,i3),2x,",nist_req,"i3)"

! Scrivo su datical.dat
  WRITE (30,'(70a)') ("-",k=1,70)
  WRITE (30,'(70a)') "RADIOSONDAGGI"
  WRITE (30,'(a)') "id_us  ok?  tot  012  lam  livelli"

  IF (stept == 6) THEN
    st012 = 2
  ELSE
    st012 = 1
  ENDIF

  DO krs = 1,nftemp
    WRITE (30,chfmt1) idtemp(krs),fok(krs), &          ! il file e' buono?
      COUNT(temp_levels(1:nist_req,krs) /= 0), &       ! temp a ist. req.
      COUNT(temp_levels(1:nist_req:st012,krs) /= 0), & ! temp a ist. 0012
      COUNT(flag_lam(1:nist_req,krs) == 1), &          ! temp sostituiti
      temp_levels(1:nist_req,krs)                      ! livelli 
  ENDDO

  WRITE (30,*)
  WRITE (30,chfmt1) "TOT: ",SUM(fok(1:nftemp)), &
    COUNT(temp_levels(1:nist_req,1:nftemp) /= 0), &
    COUNT(temp_levels(1:nist_req:st012,1:nftemp) /= 0), &
    COUNT(flag_lam(1:nist_req,1:nftemp) == 1), &
    (COUNT(temp_levels(k,1:nftemp) /= 0),k = 1,nist_req)
  
! Preparo output per datical.str
  WRITE (str2,'(i3,3(1x,i3))') &
    SUM(fok(1:nftemp)), &                                ! staz. usabili
    COUNT(temp_levels(1:nist_req,1:nftemp) /= 0), &      ! temp a ist. req.
    COUNT(temp_levels(1:nist_req:st012,1:nftemp) /= 0), &! temp a ist. 0012
    COUNT(flag_lam(1:nist_req,1:nftemp) == 1)            ! temp sostituiti

  IF (COUNT(fok(1:nftemp) /= 0) == 0) errc = errc + 1000
       
ENDIF

!==========================================================================
! 3) Esamino i dati al suolo
!

!--------------------------------------------------------------------------
! 3.1) Se c'e', leggo da interp_st1a.log il numero di ore in cui sono stati
!      inventati i dati di CC e Ceil
OPEN (UNIT=23, FILE="interp_st1a.log", STATUS="OLD", IOSTAT=ios1)
READ (23,*,IOSTAT=ios2) hno
IF (ios1 /= 0 .OR. ios2 /= 0) THEN
  hno_str = "???"
ELSE
  WRITE (hno_str,'(i3)') hno
ENDIF
CLOSE (23)

!--------------------------------------------------------------------------
! 3.2) Apro surf.dat e calcolo le date
!

OPEN (UNIT=20, FILE="surf.dat", STATUS="OLD", ACTION="READ", ERR=9999)

READ (20,*,ERR=9999) yy1,jj1,hh1, yy2,jj2,hh2, idummy, nstaz
READ (20,*,ERR=9999) (idstaz(k),k=1,nstaz)


! Calcolo data (giorno mese anno) iniziale e finale dei dati
data1 = greg(jj1,yy1)
IF (yy1 >= 50) THEN
  data1%yy = yy1 + 1900  
ELSE
  data1%yy = yy1 + 2000
ENDIF

data2 = greg(jj2,yy2)
IF (yy2 >= 50) THEN
  data2%yy = yy2 + 1900  
ELSE
  data2%yy = yy2 + 2000
ENDIF

!--------------------------------------------------------------------------
! 3.3) Per ciascuna scadenza oraria leggo i dati e metto a "1" gli 
!      elementi dell'array "presente" corrispondenti ai dati validi.

ore: DO khr = 1, mxhr
  READ (20,*,IOSTAT=ios) (idummy,k=1,3), (ws(k),wd(k),iceil(k),icc(k), &
    temp(k),irh(k),pres(k),ipcode(k),k=1,nstaz)
  IF (ios == eof) EXIT 
  IF (ios /= 0) GOTO 9999

  WHERE (ws(1:nstaz)    /= rmis) presente(khr,1:nstaz,1) = 1
  WHERE (wd(1:nstaz)    /= rmis) presente(khr,1:nstaz,2) = 1
  WHERE (iceil(1:nstaz) /= imis) presente(khr,1:nstaz,3) = 1
  WHERE (icc(1:nstaz)   /= imis) presente(khr,1:nstaz,4) = 1
  WHERE (temp(1:nstaz)  /= rmis) presente(khr,1:nstaz,5) = 1
  WHERE (irh(1:nstaz)   /= imis) presente(khr,1:nstaz,6) = 1
  WHERE (pres(1:nstaz)  /= rmis) presente(khr,1:nstaz,7) = 1

ENDDO ore

nhr = khr - 1
WRITE (*,*) "Letto surf.dat: staz, scad ",nstaz,nhr

! Controllo se ci sono altri dati in surf.dat
IF (nhr == mxhr) THEN
  READ (20,*,IOSTAT=ios) idummy
  IF (ios /= eof) WRITE (*,*) "Warning, ci sono altri dati in surf.dat"
ENDIF

CLOSE (20)

! conto i dati presente per ogni ora e ogni variabile
dathr(1:nhr,:) = SUM(presente(1:nhr,1:nstaz,:), DIM=2)

! conto i dati presenti per ogni stazione
DO kst = 1, nstaz
  dat_staz(kst) = SUM(presente(1:nhr,kst,:))
ENDDO

!--------------------------------------------------------------------------
! 3.4 Scrivo output

! Costruisco i formati
nblock = nhr/6
lastbl = MOD(nhr,6)
IF (lastbl == 0) THEN
  WRITE (chfmt1,'(a,i2,a)') "(i5,3x,",nblock,"(6(1x,i1),1x),2x,i3)"
  WRITE (chfmt2,'(a,i2,a)') "(a5,3x,",nblock,"(6i2,1x),2x,i5)"
ELSE
  WRITE (chfmt1,'(a,i2,a,i1,a)') &
    "(i5,3x,",nblock,"(6(1x,i1),1x),",lastbl,"(1x,i1),3x,i3)"
  WRITE (chfmt2,'(a,i2,a,i1,a)') &
    "(a5,3x,",nblock,"(6i2,1x),",lastbl,"i2,1x,i5)"
ENDIF

! Scrivo su datical.dat

WRITE (30,'(70a)') ("-",k=1,70)
WRITE (30,'(///70a)') "DATI SUPERFICIALI"
WRITE (30,'(2(a,i3,i3,i5,i3,5x))') &
  "Primo dato : ",data1,hh1,"Ultimo dato: ",data2,hh2
WRITE (30,'(2a)') "Ore con dati TCC e Ceil inventati: ",hno_str

DO kvar = 1,nvar

  WRITE (30,'(//2a/)') "XXX ",idvar(kvar)

  DO kst = 1, nstaz
    WRITE (30,chfmt1) idstaz(kst), presente(1:nhr,kst,kvar), &
      SUM(presente(1:nhr,kst,kvar))
  ENDDO
  WRITE (30,*)
  WRITE (30,chfmt2) "TOT: ",dathr(1:nhr,kvar), SUM(dathr(1:nhr,kvar))

ENDDO

! Preparo output per datical.str: e' ammesso che in una data ora non ci 
! sia nessun dato di vento e prc, per gli altri paramteri ci deve sempre
! essere alemno un dato

IF (MINVAL(dathr(1:nhr,3:7)) <= 0) errc = errc + 100

WRITE (str3, '(i3,1x,i3,1x,a3,2x,8i3,2x,8i3)') &
  nstaz, &                                            ! staz richieste
  COUNT(dat_staz(1:nstaz) > 0), &                     ! staz con dati
  hno_str, &                                          ! ore dati inventati
  NINT( SUM(dathr(1:nhr,:),DIM=1) /FLOAT(nhr)), 0, &  ! n.ro medio dati 
  MINVAL(dathr(1:nhr,:),DIM=1), 0                     ! n.ro minimo dati

!==========================================================================
! 4) Esamino mm5.dat 
!
IF (iprog /= 0) THEN
  CALL test_mm5dat(24,ier)
  IF (ier /= 0) errc = errc + 10000  
ENDIF

!==========================================================================
! 5) Scrivo file datical.str
!
WRITE (str1,'(i5.5,1x,i4.4,2(a1,i2.2),1x,i2.2,1x,i2)') &
  errc,data1%yy,"/",data1%mm,"/",data1%dd,hh1,nhr

WRITE (31,'(3(a,5x))') TRIM(str1),TRIM(str2),TRIM(str3)

WRITE (31,'(/a)') "Documentazione campi"
WRITE (31,'(a)') "Error code: composto da 5 cifre (abcde)"
WRITE (31,'(a)') "  00000: tutto ok"
WRITE (31,'(a)') "  a=1 : file mm5.dat mancante"
WRITE (31,'(a)') "  b=1 : dati TEMP insufficienti"
WRITE (31,'(a)') "  c=1 : dati superficiali insufficienti"
WRITE (31,'(a)') "  d=1 : surf.dat: errore in lettura o file mancante"
WRITE (31,'(a)') "  e=1 : calmet.inp: errore in lettura o file mancante"
WRITE (31,*)
WRITE (31,'(a)') &
 "Gruppo DATE: giorno/ora del 1o dato al suolo, n.ro istanti dati al suolo)"
WRITE (31,'(a)') &
 "Gruppo TEMP: n.ro stazioni utili, temp a ist. richiesti, temp a ist. 00-12"
WRITE (31,'(a)') &
 "Gruppo SURF: stazioni richieste, stazioni con almeno un dato; "
WRITE (31,'(a)') &
 "             n.ro medio di dati per ogni param. (dir,spe,cei,clo,tem,rh,prs)"
WRITE (31,'(a)') &
 "             n.ro min. di dati in una qualsiasi ora per ogni parametro"

CLOSE (30)
CLOSE (31)
STOP

!--------------------------------------------------------------------------
! 5) Gestisco errore in lettura da surf.dat
9999 CONTINUE

WRITE (*,*) "surf.dat: file mancante o errore in lettura"
WRITE (str1,'(i4.4,2x,i2.2,a1,i2.2,a1,i4.4,1x,i2.2,1x,i2)') &
  errc+10,0,"/",0,"/",0,0,0
WRITE (31,'(2(a,5x))') TRIM(str1),TRIM(str2)

CLOSE (20)
CLOSE (30)
CLOSE (31)

STOP
END PROGRAM test_inp_calmet

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION exinp(rec) RESULT (str)
!
! Funzione che, dato un record letto da calmet.inp, ritrorna la prima 
! stringa compresa tra ! che questo contiene.
! Se il record non contine coppie di !! ritorna una stringa vuota
!
IMPLICIT NONE

CHARACTER (LEN=200) :: rec
CHARACTER (LEN=100) :: str

INTEGER :: p1,p2,pp
!
str = ""

p1 = INDEX(rec,"!")
IF (p1 == 0) RETURN

pp = INDEX(rec(p1+1:),"!")
IF (pp == 0) RETURN
p2 = pp + p1

str = rec(p1+1:p2-1)

RETURN

END FUNCTION exinp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE test_mm5dat(iu,ier)
!-------------------------------------------------------------------------------
! Apre sull'unita iunit il file mm5.dat e verifica se contiene tutti i dati. 
! Se non esiste o non e' completo, ritorna un codice d'errore > 0. Per ora, e 
! per fare prima, si limita a contare i record
!-------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: iu
INTEGER, INTENT(OUT) :: ier
!
INTEGER :: nt_lam,ni_lam,nj_lam,nlev_lam,kp,kz,kt

!-------------------------------------------------------------------------------

ier = 0

! Apro il file mm5.dat
OPEN (UNIT=iu, FILE="mm5.dat", STATUS="OLD", ERR=9999)

! Scorro l'header
READ (iu,*,ERR=9998,END=9998)
READ (iu,*,ERR=9998,END=9998)
READ (iu,*,ERR=9998,END=9998)
READ (iu,*,ERR=9998,END=9998)
READ (iu,'(9x,4i4)',ERR=9998,END=9998) nt_lam,ni_lam,nj_lam,nlev_lam
READ (iu,*,ERR=9998,END=9998)
DO kz = 1,nlev_lam
  READ (iu,*,ERR=9998,END=9998)
ENDDO
DO kp = 1,ni_lam*nj_lam
  READ (iu,*,ERR=9998,END=9998)
ENDDO

! Scorro i dati
DO kt = 1,nt_lam
  DO kp = 1,ni_lam*nj_lam
    READ(iu,*,ERR=9997,END=9997)
    DO kz = 1,nlev_lam
      READ (iu,*,ERR=9997,END=9997)
    ENDDO
  ENDDO
ENDDO

RETURN

!-------------------------------------------------------------------------------
! Gestione errori
9999 CONTINUE
ier = 1           ! file mm5.dat mancante
RETURN

9998 CONTINUE
ier = 2           ! header mm5.dat incompleto
RETURN

9997 CONTINUE
ier = 3           ! dati mm5.dat incompleti
RETURN

END SUBROUTINE test_mm5dat

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE test_temp2(file,data1,hh1,data2,hh2,stept_req,nist_req, &
  nlev,tot_temp,idtemp,flag_lam,ier)
!-------------------------------------------------------------------------------
!
!Dato un file in formato up*.dat, e le date estreme del run calmet, ritorna:
! - n.ro di livelli dei sondaggi richiesti (i.e. ore interne a lrun e multiple 
!   di stept) che sono presenti nel file.
! - n.ro totale di TEMP nel file (anche fuori dai limiti temporali richiesti)
! - codice stazione del TEMP
! - codice d'errore (= 0 se il file puo' essere usato per il run). 
!   NOTA: questo programma fa solo i controlli temporali (date-ore), i controlli
!   sul singolo TEMP sono gia' stati fatti in estra_temp_calmet
!
!-------------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

! Parametri della subroutine
CHARACTER (LEN=100),INTENT(IN) :: file   ! nome file up*.dat
TYPE (date), INTENT(IN) :: data1,data2   ! data iniziale/finale del run
INTEGER, INTENT(IN) :: hh1,hh2           ! ora iniziale/finale del run
INTEGER, INTENT(IN) :: stept_req         ! intervallo tra 2 temp richiesti
INTEGER, INTENT(IN) :: nist_req          ! n.ro di Temp (i.e. scadenze) richesti

INTEGER, INTENT(OUT) :: nlev(nist_req)   ! livelli nei temp a istanti richiesti
INTEGER, INTENT(OUT) :: tot_temp         ! n.ro totale di Temp nel file
CHARACTER (LEN=5),INTENT(OUT) :: idtemp  ! codice stazione
INTEGER, INTENT(OUT) :: flag_lam(nist_req)! 0: TEMP osservato; 1: TEMP da LAM
INTEGER, INTENT(OUT) :: ier              ! codice d'errore (0 = OK)

! Variabili interne
INTEGER, PARAMETER :: stept_cal = 12  ! intervallo tra 2 temp ammesso da Calmet
INTEGER, PARAMETER :: max_ntemp = 100 ! n.ro max di Temp (i.e. scadenze)

TYPE (date) :: data_temp(max_ntemp),data_req
INTEGER :: hh_temp(max_ntemp),nnl(max_ntemp),fl(max_ntemp)
INTEGER :: ios(3),iios,ik,icn,nl,k,cnt,kread,kreq,hhtot
INTEGER :: yy,yy4,mm,dd,hh,hh_req,delta_hh
CHARACTER (LEN=200) :: str
!-------------------------------------------------------------------------------

nlev(:) = 0
flag_lam(:) = 0
fl(:) = 0
tot_temp = 0

!-------------------------------------------------------------------------------
! 1) Leggo dal file numero e data dei Temp disponibili

! Header del file
OPEN (UNIT=62, FILE=file, STATUS="OLD", ACTION="READ", IOSTAT=ios(1))
READ (62,*,IOSTAT=ios(2))
READ (62,*,IOSTAT=ios(3))
IF (ANY(ios(1:3) /= 0)) THEN
  ier = 1
  RETURN
ENDIF

cnt = 0
DO 
! Header del Temp
  READ (62,'(a)',IOSTAT=iios) str
  IF (iios /= 0) EXIT
  READ (str,'(3x,i4,5x,a5,5x,4i2,5x,i2,t69,i2)',IOSTAT=iios) &
    ik,idtemp,yy,mm,dd,hh,icn,nl
  IF (iios /= 0) EXIT
  IF (nl <= 0) THEN
    CLOSE(62)
    ier = 2
    RETURN
  ENDIF

! Dati del Temp
  DO k = 1, (nl-1)/4 + 1
    READ (62,*,IOSTAT=iios)
    IF (iios /= 0) THEN
      CLOSE(62)
      ier = 3
      RETURN
    ENDIF
  ENDDO

! Il Temp e' giusto
  cnt = cnt + 1
  IF (yy < 50) THEN
    yy4 = yy + 2000
  ELSE
    yy4 = yy + 1900
  ENDIF
  data_temp(cnt) = date(dd,mm,yy4)
  hh_temp(cnt) = hh
  nnl(cnt) = nl
  IF (INDEX(str,"*") /= 0) fl(cnt) = 1
  
ENDDO
CLOSE (62)

! Nessun sodaggio buono
IF (cnt == 0) THEN
  ier = 10
  RETURN
ELSE
  tot_temp = cnt
ENDIF

!-------------------------------------------------------------------------------
! 2) Cerco tra i Temp letti quelli che coincidono con gli istanti richiesti

data_req = data1
hh_req = hh1
kreq = 1
kread = 1

DO
! Il temp letto e' precedente al prossimo richiesto
  IF (data_temp(kread) < data_req .OR. &
      (data_temp(kread) == data_req .AND. hh_temp(kread) < hh_req)) THEN

    IF (kread >= cnt) EXIT
    kread = kread + 1
    CYCLE

! Il temp letto corrisponde al prossimo richiesto
  ELSE IF (data_temp(kread) == data_req .AND. hh_temp(kread) == hh_req) THEN
    nlev(kreq) = nnl(kread)
    flag_lam(kreq) = fl(kread)

    IF (kreq >= nist_req) EXIT
    kreq = kreq + 1
    hhtot = hh1 + (kreq-1) * stept_req
    data_req = data1 + hhtot/24
    hh_req = MOD(hhtot,24)

    IF (kread >= cnt) EXIT
    kread = kread + 1

! Il temp letto e' successivo al prossimo richiesto
  ELSE 

    IF (kreq >= nist_req) EXIT
    kreq = kreq + 1
    hhtot = hh1 + (kreq-1) * stept_req
    data_req = data1 + hhtot/24
    hh_req = MOD(hhtot,24)

  ENDIF 

ENDDO

!-------------------------------------------------------------------------------
! 3) Controllo se le date rispondono ai requisiti per Calmet

! Data iniziale del run non contenuta in dati Temp
IF (data1 < data_temp(1) .OR. & 
    (data1 == data_temp(1) .AND. hh1 < hh_temp(1))) THEN
  ier = 11
  RETURN
ENDIF

! Data finale del run non contenuta in dati Temp
IF (data2 > data_temp(cnt) .OR. & 
    (data2 == data_temp(cnt) .AND. hh2 < hh_temp(cnt))) THEN
  ier = 12
  RETURN
ENDIF

! Il passo tra due temp e' troppo lungo
DO k = 2,cnt
  delta_hh = (data_temp(k) - data_temp(k-1)) * 24 + &
    (hh_temp(k) - hh_temp(k-1))
  IF (delta_hh > stept_cal) THEN
    ier = 13
    RETURN
  ENDIF
ENDDO

! Se ho passato tutti i controlli, il file e' utilizzabile
ier = 0
RETURN

END SUBROUTINE test_temp2

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



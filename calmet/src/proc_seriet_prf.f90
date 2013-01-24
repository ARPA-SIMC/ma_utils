PROGRAM proc_seriet_prf
!--------------------------------------------------------------------------
! Programma della catena Calmet
! Legge un file in formato seriet e lo riscrive in formato up.dat
! Uso: proc_seriet_prf.exe filein fileout stzid orog [-test] [-h] 
!
! Note:
! - Tracciato file di input (seriet): 
!   . deve contenete i parametri TT,DD,FF,PP (con parametro -calmet, PP
!     non e' richiesta, e viene sostituita con valori inventati)
!   . L'intestazione deve contenere le quote dalla superficie
! - Tracciato file di output (up.dat, esclusi headers): 
!   pp(mb), zz(m), tt(K), dd(grd), ff(m/s)
!
! Todo: gestire la presenza di un livello formato da V10m,T2m,Psup (utile 
!   per run con dati ECMWF o LAMA ante 2006)
!
!                                         Versione 2.0.2, Enrico 02/01/2013
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

REAL, PARAMETER :: rdry = 287.
REAL, PARAMETER :: g = 9.8
REAL, PARAMETER :: rmis = -9999.   ! dati mancanti nei files seriet
INTEGER, PARAMETER :: fw = 10      ! ampiezza dei campi nei files seriet
INTEGER, PARAMETER :: maxlev = 50  ! numero max di livelli nei files seriet
CHARACTER(LEN=fw) :: reqlab(4) = &
  (/"Temp      ","Dir-wind  ","Mod-wind  ","pr        "/)

TYPE(date) :: data1,data2,datac
REAL, ALLOCATABLE :: field(:),tt(:),dd(:),ff(:),pp(:),zz(:)
REAL :: orog,psup,ptop
INTEGER :: nlev_in(4),iz(4,maxlev),idf_in(4,maxlev)
INTEGER :: iz_out(maxlev),idf_out(4,maxlev),idf2(4)
INTEGER :: tiz_sorted(maxlev),idx_sorted(maxlev)
INTEGER :: npar_tot,nlev_out,hh1,hh2,hhc,l1out,npar_req
INTEGER :: cnt_par,k,k1,k2,krec,kpar,kp,kl
INTEGER :: eof,eor,ios
CHARACTER(LEN=10000) :: header_lev,header_par,chrec
CHARACTER(LEN=120) :: filein,fileout,chpar,chfmt
CHARACTER(LEN=5) :: stzid,next_arg
CHARACTER(LEN=fw) :: lev_lab,par_lab

LOGICAL :: ltest,lcalmet,unsorted(maxlev)

!REAL :: dd10,ff10,tt02,psup

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comandi etc.

ltest = .FALSE.
lcalmet = .FALSE.
next_arg = ""
cnt_par = 0
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP 1
  ELSE IF (TRIM(chpar) == "-test") THEN
    ltest = .TRUE.
  ELSE IF (TRIM(chpar) == "-calmet") THEN
    lcalmet = .TRUE.
    next_arg = "prsup"
  ELSE IF (next_arg == "prsup") THEN
    next_arg = ""
    READ (chpar,*,IOSTAT=ios) psup
    IF (ios /= 0) THEN
      CALL scrive_help
      STOP 1
    ENDIF
    IF (psup < 500. .OR. psup > 1100.) THEN
      GOTO 9991
    ELSE
      psup = psup * 100.
    ENDIF
  ELSE IF (cnt_par == 0) THEN
    cnt_par = 1
    filein = chpar
  ELSE IF (cnt_par == 1) THEN
    cnt_par = 2
    fileout = chpar
  ELSE IF (cnt_par == 2) THEN
    cnt_par = 3
    stzid = chpar(1:5)
  ELSE IF (cnt_par == 3) THEN
    cnt_par = 4
    READ (chpar,*,IOSTAT=ios) orog
  ELSE
    CALL scrive_help
    STOP 1
  ENDIF
ENDDO

IF (cnt_par /= 4 .OR. ios /= 0) THEN
  CALL scrive_help
  STOP 1
ENDIF  

IF (lcalmet) THEN
  npar_req = 3
  ptop = 700.
ELSE
  npar_req = 4
  ptop = 500.
ENDIF

! Codice EOF
CALL get_eof_eor(eof,eor)

!==========================================================================
! 2) Leggo una prima volta filein: interpreto gli header per trovare i 
!    livelli con i dati necessari, trovo le date estreme

!--------------------------------------------------------------------------
! 2.1 Leggo gli header
WRITE (chfmt,'(a,i2,a)'), "(1x,a",fw,")"

OPEN (UNIT=30, FILE=filein, STATUS="OLD", ACTION="READ", IOSTAT=ios)
IF (ios /= 0) GOTO 9999
READ (30,*,ERR=9998)
READ (30,*,ERR=9998)
READ (30,*,ERR=9998)
READ (30,'(17x,a)',ERR=9998) header_lev
READ (30,*,ERR=9998)
READ (30,'(17x,a)',ERR=9998) header_par

IF (MOD(LEN(TRIM(header_lev)), (fw+1)) /= 0) THEN
  GOTO 9997
ELSE
  npar_tot = LEN(TRIM(header_lev)) / (fw+1)
ENDIF

!--------------------------------------------------------------------------
! 2.2 Cerco negli header i campi relativi a TT,DD,FF,PP
! - nlev_in(1:4)   numero di livelli trovati per T,DD,FF,Pr
! - idf_in(1:4,:)  posizione di ciascun parametro nei record in input
! - iz(1:4,:)      quota relativa a ciascun parametro

iz(:,:) = -999
idf_in(:,:) = 0
nlev_in(:) = 0

DO kp = 1,npar_tot
  lev_lab = ADJUSTL(header_lev((kp-1)*(fw+1)+1:kp*(fw+1)))
  par_lab = ADJUSTL(header_par((kp-1)*(fw+1)+1:kp*(fw+1)))

  DO k2 = 1,4
    IF (TRIM(par_lab) == TRIM(reqlab(k2))) THEN
      nlev_in(k2) = nlev_in(k2) + 1
      IF (nlev_in(k2) > maxlev) GOTO 9996
      idf_in(k2,nlev_in(k2)) = kp
      READ (lev_lab,*,IOSTAT=ios) iz(k2,nlev_in(k2))  
      IF (ios /= 0) GOTO 9995
    ENDIF
  ENDDO
ENDDO

!print *,nlev_in
!do k = 1,4
!  print *,idf_in(k,:)
!  print *,iz(k,:)
!enddo

!--------------------------------------------------------------------------
! 2.3 Costruisco la lista ordinata dei livelli di T presenti in input

tiz_sorted(1:nlev_in(1)) = 0
idx_sorted(1:nlev_in(1)) = 0
unsorted(1:nlev_in(1)) = .TRUE.
DO kl = 1,nlev_in(1)
  tiz_sorted(kl) = MINVAL(iz(1,1:nlev_in(1)), MASK=unsorted(1:nlev_in(1)))  
  idx_sorted(kl) = MINLOC(iz(1,1:nlev_in(1)), DIM=1, &
    MASK=unsorted(1:nlev_in(1)))  
  unsorted(idx_sorted(kl)) =.FALSE.
ENDDO

!--------------------------------------------------------------------------
! 2.4 Cerco i livelli con tutti i dati necessari: scorro dal basso i 
!     livelli (ordinati) di TT e cerco gli altri parametri

nlev_out = 0
iz_out(:) = 0
idf_out(:,:) = 0

DO kl = 1,nlev_in(1)

! Per ciascun parametro, cerco il campo con il livello richeisto
  idf2(:) = 0
  DO kp = 1,4
    DO k2 = 1,nlev_in(kp)
      IF (iz(kp,k2) == tiz_sorted(kl)) THEN
        idf2(kp) = idf_in(kp,k2)
        EXIT
      ENDIF
    ENDDO
  ENDDO

! Se ho trovato un livello completo, lo salvo
  IF (ALL(idf2(1:npar_req) /= 0)) THEN
    nlev_out = nlev_out + 1
    iz_out(nlev_out) = iz(1,kl)
    idf_out(:,nlev_out) = idf2(:)
  ENDIF

ENDDO
IF (nlev_out == 0) GOTO 9992

!--------------------------------------------------------------------------
! 2.5 Trovo le date estreme contenute in filein

DO k = 1,HUGE(0)
  IF (k == 1) THEN
    READ (30,'(i2,1x,i2,1x,i4,1x,i2)',IOSTAT=ios) &
      data1%dd,data1%mm,data1%yy,hh1
  ELSE
    READ (30,'(i2,1x,i2,1x,i4,1x,i2)',IOSTAT=ios) &
      data2%dd,data2%mm,data2%yy,hh2
  ENDIF
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9994
ENDDO
CLOSE (30)

!--------------------------------------------------------------------------
! 2.6 Log a schermo

WRITE (chfmt,'(a,i5,a)') "(a,",nlev_out,"(1x,i4))" 
WRITE (*,*) "Analisi del file di input"
WRITE (*,*) "campi totali ",npar_tot," livelli completi ",nlev_out
WRITE (*,chfmt) "Quota ",iz_out(1:nlev_out)
WRITE (*,chfmt) "idx TT",idf_out(1,1:nlev_out)
WRITE (*,chfmt) "idx DD",idf_out(2,1:nlev_out)
WRITE (*,chfmt) "idx FF",idf_out(3,1:nlev_out)
IF (.NOT. lcalmet) WRITE (*,chfmt) "idx PP",idf_out(4,1:nlev_out)
WRITE (*,'(a,2(i4.4,1x,3(i2.2,1x),3x),a,i8)') "Date estreme: ",&
  data1%yy,data1%mm,data1%dd,hh1,data2%yy,data2%mm,data2%dd,hh2, &
  "   Istanti ",k-1

!==========================================================================
! 3) Lettura - scrittura (ciclo sugli istanti)

!--------------------------------------------------------------------------
! 3.1 Preliminari

! Apro fileout e scrivo header
OPEN (UNIT=31, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (31,'(1x,6i5,f5.0)') MOD(data1%yy,100),jul(data1),hh1, &
  MOD(data2%yy,100),jul(data2),hh2,ptop
WRITE (31,'(a)') "     F    F    F    F"

! Ri-apro filein e skip header
OPEN (UNIT=30, FILE=filein, STATUS="OLD", ACTION="READ", IOSTAT=ios)
DO k=1,6
  READ (30,*)
ENDDO

! Alloco arrays
ALLOCATE (field(npar_tot))
ALLOCATE (tt(nlev_out),dd(nlev_out),ff(nlev_out),pp(nlev_out),zz(nlev_out))

!--------------------------------------------------------------------------
! 3.2 Lettura e scrittura

DO k = 1,HUGE(0)

! Leggo un record di dati da filein
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
 
  READ (chrec,'(i2,1x,i2,1x,i4,1x,i2)',IOSTAT=ios) &
    datac%dd,datac%mm,datac%yy,hhc
  IF (ios /= 0) GOTO 9994
  READ (chrec(18:),*,IOSTAT=ios) field(1:npar_tot)
  IF (ios /= 0) GOTO 9993

! Assegno i valori di TT,DD,FF,PP
  DO kl = 1,nlev_out
    tt(kl) = field(idf_out(1,kl))
    dd(kl) = field(idf_out(2,kl))
    ff(kl) = field(idf_out(3,kl))
    IF (lcalmet .AND. kl==1) THEN
      pp(kl) = psup * (1. - g*iz_out(kl)/(rdry*tt(kl)))
    ELSE IF (lcalmet .AND. kl>1) THEN
      pp(kl) = pp(kl-1) - pp(kl-1) * &
        g * (iz_out(kl)-iz_out(kl-1)) / (rdry * (tt(kl)+tt(kl-1))/2)
    ELSE
      pp(kl) = field(idf_out(4,kl))
    ENDIF
    zz(kl) = REAL(iz_out(kl)) + orog
  ENDDO

!!!! COMPLETARE !!!!!
! Se c'e' il livello a 10m, ricalcolo la pressione
!  IF (iz_sorted(1) == 10) &
!    pp_sorted(1) = pp_sorted(1) - pp_sorted(1)*10.*g/(rdry*tt_sorted(1))

! Converto unita' di misura e dati mancanti
  WHERE (pp(1:nlev_out) == rmis)
    pp(1:nlev_out) = -99.9
  ELSEWHERE
    pp(1:nlev_out) = pp(1:nlev_out) / 100.
  ENDWHERE
  WHERE (tt(1:nlev_out) == rmis)
    tt(1:nlev_out) = 999.9
  ENDWHERE
  WHERE (dd(1:nlev_out) == rmis)
    dd(1:nlev_out) = 999
  ENDWHERE
  WHERE (ff(1:nlev_out) == rmis)
    ff(1:nlev_out) = 999
  ENDWHERE

! Se richiesta l'opzione -test, sovrascrivo alcuni dati (USER MODIFICATION)
  IF (ltest) THEN
    ff(1:nlev_out) = 3.
    dd(1:nlev_out) = 270.
  ENDIF

! Scrivo su fileout
  WRITE (31,'(3x,i4,5x,a5,5x,4i2,5x,i2,t69,i2)') & 
    9999,stzid,MOD(datac%yy,100),datac%mm,datac%dd,hhc, &
    nlev_out,nlev_out

  l1out = 1
  DO krec = 1, nlev_out/4
    k1 = (krec-1)*4 + 1
    k2 = krec*4
    WRITE (31,'(4(3x,f6.1,a1,f5.0,a1,f5.1,a1,i3,a1,i3))') &
      (pp(kl),"/",zz(kl),"/",tt(kl),"/",NINT(dd(kl)),"/",NINT(ff(kl)), & 
       kl = k1 + l1out-1, k2 + l1out-1)
  ENDDO

  IF (MOD(nlev_out,4) /= 0) THEN
    k1 = (nlev_out/4) * 4 + 1
    k2 = nlev_out
    WRITE (31,'(4(3x,f6.1,a1,f5.0,a1,f5.1,a1,i3,a1,i3))') &
      (pp(kl),"/",zz(kl),"/",tt(kl),"/",NINT(dd(kl)),"/",NINT(ff(kl)), &
       kl = k1 + l1out-1, k2 + l1out-1)
  ENDIF

ENDDO

WRITE (*,*) "proc_seriet_prf: elaborati ",k-1," istanti"
STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore lettura header ",TRIM(filein)
STOP 3

9997 CONTINUE
WRITE (*,*) "Errore parsing header ",TRIM(filein)
WRITE (*,*) "Numero di campi non intero"
STOP 3

9996 CONTINUE
WRITE (*,*) "Errore: troppi livelli in ",TRIM(filein)
WRITE (*,*) "Aumentare parametro maxpar"
STOP 3

9995 CONTINUE
WRITE (*,*) "Errore: lettura livelli nell'header di ",TRIM(filein)
WRITE (*,*) "Campo ",kp
STOP 3

9994 CONTINUE
WRITE (*,*) "Errore lettura data ",TRIM(filein)
WRITE (*,*) "Riga ",k+6
STOP 4

9993 CONTINUE
WRITE (*,*) "Errore lettura dati ",TRIM(filein)
WRITE (*,*) "Riga ",k+6
STOP 4

9992 CONTINUE
WRITE (*,*) "Il file di input non contine nessun livello completo"
STOP 5

9991 CONTINUE
WRITE (*,*) "Psup deve essere espressa in hPa"
STOP 1

END PROGRAM proc_seriet_prf

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo un breve help
!
IMPLICIT NONE

WRITE (*,*) "Uso: proc_seriet_prf.exe filein fileout stzid orog"
WRITE (*,*) "  [-calmet PSUP][-test]"
WRITE (*,*) "Filein:  in formato seriet"
WRITE (*,*) "Fileout: in formato UP*.DAT"
WRITE (*,*) "stzid:   codice stazione (CH*5; per headers fileout)"
WRITE (*,*) "orog:    quota stazione (i.e. quota punto LM)"
WRITE (*,*) "-calmet: non legge i dati di pressione, ma li calcola a partire dalla"
WRITE (*,*) "         pressione alla superficie (PSUP, hPa) e dal profilo di T"
WRITE (*,*) "-test:   sostituisce alcuni dati con valori fittizi (per debug calmet)"

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

RETURN

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine get_eof_eor, usero' valori di default"
eof = -1
eor = -2
RETURN

END SUBROUTINE get_eof_eor

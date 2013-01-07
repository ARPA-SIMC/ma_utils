PROGRAM mvl_create
!-------------------------------------------------------------------------------
! Legge un file in formato short summary e lo riscrive in formato .mvl,accedendo
! alle tabelle seriet. Programma della catena ak_seriet.
!
!                                               Versione 1.0, Enrico, 13/12/2010
!-------------------------------------------------------------------------------

IMPLICIT NONE

INTEGER, PARAMETER :: iu_in = 20, iu_out = 30, iu_tab = 40

REAL :: vmin,vmax
INTEGER :: ios,eof,eor,ier,ios1,ios2,nok,nskip,ndef,kpar,kc
INTEGER :: ps,p1,p2,p3,p4,pv1,pv2,var(3),liv(3),cnt_par,ndec,cp2
CHARACTER (LEN=100) :: filein,fileout,chpar,chrec
CHARACTER (LEN=20) :: str_model,str_var
CHARACTER (LEN=10) :: ch10(2)
CHARACTER (LEN=3) :: ch3(2)
CHARACTER (LEN=1) :: next_arg

!-------------------------------------------------------------------------------
! Parametri da riga comandi

cnt_par = 0
filein = ""
fileout = ""
next_arg = ""
str_model = "xxx"

DO kpar = 1,HUGE(kpar)
  CALL getarg(kpar,chpar)

  IF (TRIM(chpar) == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(chpar) == "-m") THEN
    next_arg="m"
  ELSE IF (next_arg == "m") THEN
    next_arg=""
    str_model = chpar
  ELSE
    cnt_par = cnt_par + 1
    IF (cnt_par == 1) THEN
      filein = chpar
    ELSE IF (cnt_par == 2) THEN
      fileout = chpar
    ENDIF

  ENDIF
ENDDO

! Controlli
IF (filein == "" .OR. filein == "") THEN
    CALL scrive_help
    STOP
ENDIF  
DO kc = 1,LEN(TRIM(str_model))
  IF (str_model(kc:kc) == " " .OR. str_model(kc:kc) == ",") &
    str_model(kc:kc) = "_"
ENDDO

! Codici EOF/EOR
CALL get_eof_eor(eof,eor)

!-------------------------------------------------------------------------------
! Elaborazioni

OPEN (UNIT=iu_in, FILE=filein, STATUS="OLD", ACTION="READ", ERR=9999)
OPEN (UNIT=iu_out, FILE=fileout, STATUS="REPLACE", ACTION="WRITE")
WRITE (iu_out,'(a)') "cem,tab,var,cp2,liv1,liv2,liv3,ndec,vmin,vmax,model,str_var"

nskip = 0
nok = 0
ndef = 0
DO
  READ (iu_in,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998
  
! Parsing  
  ps = INDEX(chrec,";")
  p1 = INDEX(chrec(:ps),"(")
  p2 = INDEX(chrec(p1+1:ps),")") + p1
  p3 = INDEX(chrec(ps+1:),"(") + ps
  p4 = INDEX(chrec(p3+1:),")") + p3
  IF (ps==0 .OR. p1==0 .OR. p2==0 .OR. p3==0 .OR. p4==0 .OR. &
      p1>=p2 .OR. p3>=p4) THEN
    nskip = nskip + 1
    CYCLE
  ENDIF

  var(:) = 0
  READ (chrec(p1+1:p2-1),*,IOSTAT=ios1) var(1:3)

  liv(:) = 0
  pv1 = INDEX(chrec(p3+1:p4-1),",")
  pv2 = INDEX(chrec(p3+1:p4-1),",",BACK=.TRUE.)
  IF (pv1 == 0 .AND. pv2 == 0) THEN
    READ (chrec(p3+1:p4-1),*,IOSTAT=ios2) liv(1)
  ELSE IF (pv1 == pv2) THEN
    READ (chrec(p3+1:p4-1),*,IOSTAT=ios2) liv(1:2) 
  ELSE
    READ (chrec(p3+1:p4-1),*,IOSTAT=ios2) liv(1:3)
  ENDIF

  IF (ios1 /= 0 .OR. ios2 /= 0) THEN
    nskip = nskip + 1
    CYCLE
  ENDIF

  CALL var2spec(var,iu_tab,ndec,vmin,vmax,str_var,cp2,ier)
  WRITE(ch3(1),'(i3)') cp2
  WRITE(ch3(2),'(i3)') ndec
  WRITE(ch10(1),'(f10.2)') vmin
  WRITE(ch10(2),'(f10.2)') vmax

  WRITE (iu_out,'(i3.3,2(a1,i3.3),a1,a,3(a1,i3.3),5(a1,a))') &
    var(1),",",var(2),",",var(3),",",TRIM(ADJUSTL(ch3(1))),",", &
    liv(1),",",liv(2),",",liv(3),",",TRIM(ADJUSTL(ch3(2))),",", &
    TRIM(ADJUSTL(ch10(1))),",",TRIM(ADJUSTL(ch10(2))),",", &
    TRIM(ADJUSTL(str_model)),",",TRIM(ADJUSTL(str_var))

  IF (ier == 0) THEN
    nok = nok + 1
  ELSE
    ndef = ndef + 1
  ENDIF

ENDDO

CLOSE (iu_in)
CLOSE (iu_out)
IF (nskip > 0) WRITE (*,'(a,i3)') "  record skippati in input: ",nskip
WRITE (*,'(a,i3,a,i3,a)') "  var-liv scrite ",nok+ndef ," di cui ",ndef, &
  " con valori di default "

STOP

!===============================================================================
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP

9997 CONTINUE
WRITE (*,*) "Troppi files in input, aumentare paramero mxfiles"
STOP

END PROGRAM mvl_create

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE var2spec(var,iu_tab,ndec,vmin,vmax,str,cp2,ier)
!--------------------------------------------------------------------------
! Data la codifica gribarch di una variabile (terna cem,tab,var), cerca nel
! file tabella_xxx_ser.txt corrispondente una serie di informazioni:
!
! - il n.ro di decimali piu' appropriato per rappresentarla (-1 indica
!   notazione esponenziale)
! - i valori minimo e massimo accettabili (quality control)
! - una stringa identificativa della variabile
! - un codice del tipo di varibile (comp2), che vale:
!     se var e' comp. vento  = il codice dell'altra componente
!     se e' temperatura      = -1
!     se e' Monin-Obukov     = -2
!     se e' SW Budget        = -3
!     se e' cloud cover      = -4
!     se e' flusso di calore = -5
!     negli altri casi       =  0
!
! Codici d'errore (se /= 0 la subrotunie ritorna dei valori di dafault):
! 0 = tutto ok
! -1 = parametro non presente in tabella
! -2 = file tabella_xxx_ser.txt non trovato
! -3 = errore lettura dal file tabella_xxx_ser.txt 
!--------------------------------------------------------------------------
IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: var(3),iu_tab
REAL, INTENT(OUT) :: vmin,vmax
INTEGER, INTENT(OUT) :: ndec,cp2,ier
CHARACTER (*), INTENT(OUT) :: str

! Parametri relativi ai files "tabella_xxx_ser.txt"
CHARACTER (LEN=40), PARAMETER :: tab_env = "HOME_MINGUZZI"
CHARACTER (LEN=40), PARAMETER :: tab_path = "util/grib/dat"

! Variabili locali
REAL :: vmin_t,vmax_t
INTEGER :: ndec_t,cp2_t,var_t
INTEGER :: ios,ios2
CHARACTER (LEN=80) :: nfile,ch80
CHARACTER (LEN=40) :: ch40
CHARACTER (LEN=8) :: ch8
CHARACTER (LEN=3) :: ch3
!--------------------------------------------------------------------------

! Assegno valori di default
ndec = 3
vmin = -1.e03
vmax = +1.e04
WRITE (ch8,'(1x,a4,i3.3)') "var_",var(3)
str = ADJUSTR(ch8)
cp2 = 0

! Apro la tabella richiesta
CALL GETENV(tab_env,ch40)
WRITE (nfile,'(5a,i3.3,a)') &
  TRIM(ch40),"/",TRIM(tab_path),"/","tabella_",var(2),"_ser.txt"

OPEN (UNIT=iu_tab, FILE=nfile, STATUS="OLD", ACTION="READ", IOSTAT=ios)
IF (ios /= 0) THEN
  ier = -2
  RETURN
ENDIF

! Cerco il parametro richiesto
DO
  READ (iu_tab,'(a)',IOSTAT=ios) ch80  
  IF (ios /= 0) EXIT
  IF (TRIM(ch80) == "" .OR. ch80(1:1) == "!") CYCLE

  READ (ch80,'(1x,i3,2x,a8,2x,a3,2(2x,f10.2),2x,i3)',IOSTAT=ios) &
    var_t,ch8,ch3,vmin_t,vmax_t,cp2_t

  IF (ch3 == "exp") THEN
    ndec_t = 999
  ELSE
    READ (ch3,*,IOSTAT=ios2) ndec_t
  ENDIF

  IF (ios /= 0 .OR. ios2 /= 0) THEN
    ier = -3
    RETURN
  ENDIF
  
  IF (var_t /= var(3)) CYCLE

  str = ADJUSTR(ch8)
  ndec = ndec_t
  vmin = vmin_t
  vmax = vmax_t
  cp2 = cp2_t
  ier = 0
  CLOSE (iu_tab)
  RETURN

ENDDO

ier = -1
RETURN

END SUBROUTINE var2spec

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE

!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "uso: mvl_create.exe filein fileout [-h] [-m model]"
WRITE (*,*) "filein:  in formato short summary (anche piu' file appesi)"
WRITE (*,*) "fileout: in formato .mvl"
WRITE (*,*) "model:   stringa identificativa del modello (1o campo di fileout)"

RETURN
END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

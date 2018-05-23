PROGRAM orari2seriet
!--------------------------------------------------------------------------
! Legge un file con serie storica su punto in formato ASCII separato da 
! spazi e lo riscrive in formato seriet.
!
! Uso: 
! orari2seriet.exe [-h] -o/-d [-ndec N] filein fileout
!
! Note:
!
! Todo:
! - aggiungere l'opzione [-fill], per scrivere in ogni caso un record per 
!   ogni ora di calendario
! - completare gestione del formato trasp_temp
! - opzione per output csv. In questo caso, attivare l'input in formato 
!   ser/sex (gia' presente nel codice)
!
!                                                 V1.0.0, Enrico 11/02/2013
!--------------------------------------------------------------------------

USE file_utilities
IMPLICIT NONE

!==========================================================================
! 0) Dichiarazioni e parametri costanti

!--------------------------------------------------------------------------
! 0.1 Parametri costanti / opzioni

REAL, PARAMETER :: rmis_hhr = -9999. ! dato mancante, files estra_orari
REAL, PARAMETER :: rmis_ddy = -9999. ! dato mancante, files estra_qa giorn.
REAL, PARAMETER :: rmis_tem = -9999. ! dato mancante, files trasp_temp
REAL, PARAMETER :: rmis_ser = -9999. ! dato mancante, files trasp_seriet
REAL, PARAMETER :: rmis_sex = -1.E30 ! dato mancante, files trasp_seriet exp
INTEGER, PARAMETER :: fw = 10        ! ampiezza dei campi nei files I/O
INTEGER, PARAMETER :: mxpar = 500    ! n.ro max di parametri in un file

INTEGER, PARAMETER :: hh_def = 0     ! ora per i dati che ne sono privi
INTEGER, PARAMETER :: sca_def = 0    ! scadenza per i dati che ne sono privi

!--------------------------------------------------------------------------
! 0.5 Altre variabili del programma

REAL :: rmis_inp,rmis_out,rval(mxpar)
INTEGER :: ndec_out,mand_par,npar,lline
INTEGER :: k,kp,kv,p1,p2,ios,eof,eor,yy,mm,dd,hh,sca
CHARACTER (LEN=mxpar*(fw+1)+20) :: chdum,head_par,head_liv,head_stz
CHARACTER (LEN=500) :: chfmt0,chfmt1,chfmt2
CHARACTER (LEN=250) :: filein,fileout,head
CHARACTER (LEN=fw) :: str_par(mxpar),str_par_dum,chval(mxpar)
CHARACTER (LEN=3) :: inp_data,next_arg

!==========================================================================
! 1) Preliminari 

! 1.1) Parametri da riga comando

inp_data = ""
filein = ""
fileout = ""
ndec_out = 1

mand_par = 0
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
  ELSE IF (TRIM(chdum) == "-d") THEN
    inp_data = "ddy"
  ELSE IF (TRIM(chdum) == "-t") THEN
    inp_data = "tem"
  ELSE IF (TRIM(chdum) == "-s") THEN
    inp_data = "ser"
  ELSE IF (TRIM(chdum) == "-sx") THEN
    inp_data = "sex"
  ELSE IF (TRIM(chdum) == "-ndec") THEN
    next_arg = "ndc"
  ELSE IF (next_arg == "ndc") THEN
    READ (chdum,*,IOSTAT=ios) ndec_out
    next_arg = ""
  ELSE IF (mand_par == 0) THEN
    filein = TRIM(chdum)
    mand_par = 1
  ELSE IF (mand_par == 1) THEN
    fileout = TRIM(chdum)
    mand_par = 2
  ENDIF
ENDDO

IF (TRIM(filein) == "" .OR. inp_data == "") THEN
  CALL scrive_help
  STOP 1
ENDIF
IF (inp_data == "ser" .OR. (inp_data == "sex")) THEN
  WRITE (*,*) "Input in formato seriet non ancora gestito"
  STOP 1
ENDIF
IF (ios /= 0 .OR. ndec_out > fw-3 .OR. (ndec_out == -1 .AND. fw < 7)) THEN
  WRITE (*,*) "Errore nei parametri (ndec illegale o troppo alto rispetto fw)"
  STOP 1
ENDIF

CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 1.2 Apro input file e leggo header

OPEN (UNIT=20, FILE=filein, STATUS= "OLD", ACTION="READ", ERR=9999)

IF (inp_data == "hhr") THEN
  READ (20,'(a)',ERR=9998) head_stz
  READ (20,'(a)', ERR=9998) chdum
  IF (LEN(TRIM(chdum)) > 13) THEN
    head_liv = chdum(14:)
  ELSE
    head_liv = ""
  ENDIF
  READ (20,'(13x,a)', ERR=9998) head_par

ELSE IF (inp_data == "ddy") THEN
  READ (20,'(a)',ERR=9998) head_stz
  READ (20,'(a)', ERR=9998) chdum
  IF (LEN(TRIM(chdum)) > 10) THEN
    head_liv = chdum(11:)
  ELSE
    head_liv = ""
  ENDIF
  READ (20,'(10x,a)', ERR=9998) head_par

ELSE IF (inp_data == "tem") THEN
  READ (20,'(19x,a)', ERR=9998) head_par

ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998) head_stz
  READ (20,*,ERR=9998)
  READ (20,'(17x,a)',ERR=9998) head_par
  READ (20,'(17x,a)',ERR=9998) head_liv
  READ (20,*,ERR=9998)

ENDIF

!--------------------------------------------------------------------------
! 1.3 Costruisco i formati per lettura e scrittura

! Leggo numero e elenco dei parametri
WRITE (chfmt0,'(a,i2,a)') "(1x,a",fw,")"
DO k = 1,mxpar
  p1 = (k-1) * (fw+1) + 1
  p2 = k * (fw+1)
  READ (head_par(p1:p2),chfmt0,IOSTAT=ios) str_par_dum
  IF (ios /= 0) GOTO 9997
  IF (str_par_dum == "") EXIT
  str_par(k) = str_par_dum
ENDDO
npar = k - 1

! Formato di lettura per i record dati
IF (inp_data == "hhr") THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i4,3i3,",npar,"(1x,a",fw,"))"
ELSE IF (inp_data == "ddy") THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i4,2i3,",npar,"(1x,a",fw,"))"
  rmis_inp = rmis_ddy
ELSE IF (inp_data == "tem") THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i4,3i3,6x",npar,"(1x,a",fw,"))"
  rmis_inp = rmis_tem
ELSE IF (inp_data == "ser") THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i2,1x,i2,1x,i4,i3,1x,i3,",npar,"(1x,a",fw,"))"
  rmis_inp = rmis_ser
ELSE IF (inp_data == "sex") THEN
  WRITE (chfmt1,'(a,i3,a,i2,a)') "(i2,1x,i2,1x,i4,i3,1x,i3,",npar,"(1x,a",fw,"))"
  rmis_inp = rmis_sex
ENDIF

! Formato di scrittura per i record dati
IF (ndec_out == -1) THEN
  rmis_out = rmis_sex
  WRITE (chfmt2,'(a,i3,2(a,i2),a)') &
    "(2(i2.2,a1),i4.4,1x,i2.2,1x,i3.3,",npar,"(1x,e",fw,".",fw-7,"))"
ELSE
  rmis_out = rmis_ser
  WRITE (chfmt2,'(a,i3,2(a,i2),a)') &
    "(2(i2.2,a1),i4.4,1x,i2.2,1x,i3.3,",npar,"(1x,f",fw,".",ndec_out,"))"
ENDIF

!--------------------------------------------------------------------------
! 1.4 Apro il file di ouput e scrivo l'header

OPEN (UNIT=30, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (30,'(a)') TRIM(head_stz)
WRITE (30,*)
WRITE (30,'(a)')  "Modello          "
WRITE (30,'(2a)') "Livello          ",TRIM(head_liv)
WRITE (30,*)
WRITE (30,'(2a)') "gg/mm/aaaa hh sca",TRIM(head_par)

!==========================================================================
! 2) Leggo e riscrivo (ciclo sui record di input)

DO k = 1,HUGE(0)

! 3.1 Leggo un record dati
  IF (inp_data == "hhr") THEN
    READ (20,chfmt1,IOSTAT=ios) yy,mm,dd,hh,chval(1:npar)
    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9996
    sca = sca_def
    DO kv = 1,npar
      READ (chval(kv),*) rval(kv)
    ENDDO

  ELSE IF (inp_data == "ddy") THEN
    READ (20,chfmt1,IOSTAT=ios) yy,mm,dd,chval(1:npar)
    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9996
    hh = hh_def
    sca = sca_def 
    DO kv = 1,npar
      READ (chval(kv),*) rval(kv)
    ENDDO

  ELSE IF (inp_data == "tem") THEN
    READ (20,chfmt1,IOSTAT=ios) yy,mm,dd,hh,chval(1:npar)
    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9996
    sca = sca_def
    DO kv = 1,npar
      READ (chval(kv),*) rval(kv)
    ENDDO

  ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
    READ (20,chfmt1,IOSTAT=ios) dd,mm,yy,hh,sca,chval(1:npar)
    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9996
    DO kv = 1,npar
      READ (chval(kv),*) rval(kv)
    ENDDO

  ENDIF

! 3.2 Lo scrivo in output
  WHERE (rval(1:npar) == rmis_inp)
    rval(1:npar) = rmis_out
  ENDWHERE
  WRITE (30,chfmt2) dd,"/",mm,"/",yy,hh,sca,rval(1:npar)

ENDDO

WRITE (*,*) "Operazioni completate, scritti ",k-1," record"

STOP

!--------------------------------------------------------------------------
! 6) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore leggendo headers ",TRIM(filein)
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore leggendo lista parametri ",TRIM(filein)
STOP 2

9996 CONTINUE
IF (inp_data == "hhr" .OR. inp_data == "ddy" .OR. inp_data == "tem") THEN
  lline = k + 3
ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
  lline = k + 6
ELSE IF (inp_data == "reg") THEN
  lline = k
ENDIF
WRITE (*,*) "Errore leggendo i dati ",TRIM(filein),"record ",lline
STOP 5

END PROGRAM orari2seriet

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

WRITE (*,*) 
WRITE (*,*) "orari2seriet.exe [-h] -o/-d [-ndec N] filein fileout"

WRITE (*,*) "Legge un file con serie storica su punto in formato ASCII separato da"
WRITE (*,*) "  spazi e lo riscrive in formato seriet"
WRITE (*,*) "filein   : file di input"
WRITE (*,*) "          -o = estra_orari o estra_qaria con dati orari"
WRITE (*,*) "          -d = estra_qaria con dati giornalieri o segmento di file .sta"
! WRITE (*,*) "          -t = trasp_temp"
WRITE (*,*) "filein   : file di output (in formato seriet)"
WRITE (*,*) " -ndec N : numero di decimali in ouptut (-1 per notazione exp, def. 1)"
WRITE (*,*) " -h      : visualizza questo help"
WRITE (*,*) 

RETURN

END SUBROUTINE scrive_help


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


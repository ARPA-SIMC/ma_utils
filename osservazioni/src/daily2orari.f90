PROGRAM daily2orari
!--------------------------------------------------------------------------
! Legge un file estra_qaria con dati giornalieri o un segmento di file .sta
! e lo riscrive in formato estra_orari standard (ie. una riga per ogni ora)
! Al momento, si limita a scrivere per tutte le ore di ogni giorno il
! valore giornaliero in input.
! Utile per elaborare dati PM10 giornalieri con tool per dati orari.
!
!                                         Versione 1.0.0, Enrico 28/11/2017
!--------------------------------------------------------------------------

USE file_utilities
USE datetime_class
IMPLICIT NONE

!--------------------------------------------------------------------------
! Costanti

REAL, PARAMETER :: rmis_hhr = -9999. ! dato mancante, files estra_orari
REAL, PARAMETER :: rmis_ddy = -9999. ! dato mancante, files estra_qa giorn.
INTEGER, PARAMETER :: fw = 10        ! ampiezza dei campi nei files I/O
INTEGER, PARAMETER :: mxpar = 500    ! n.ro max di parametri in un file

! Dichiarazioni
TYPE(datetime) :: datac,datacp1
INTEGER :: idp,kp,kd,kh,yy,mm,dd,yyp1,mmp1,ddp1
INTEGER :: eof,eor,ios
CHARACTER (LEN=100) :: filein,fileout,chdum
CHARACTER (LEN=mxpar*(fw+1)+20) :: header(3),chrec
CHARACTER (LEN=3) :: next_arg

!--------------------------------------------------------------------------
! 1) Parametri da riga comandi e altri preliminari

next_arg = ""
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL scrive_help
    STOP 1
  ELSE
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE DEFAULT
      CALL scrive_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

IF (idp /= 2) THEN
  CALL scrive_help
  STOP 1
ENDIF

CALL get_eof_eor(eof, eor)

!--------------------------------------------------------------------------
! 2) Elaborazioni 

! Apro files
OPEN (UNIT=20, FILE=filein, STATUS= "OLD", ACTION="READ", ERR=9999)
OPEN (UNIT=21, FILE=fileout, STATUS= "REPLACE", FORM="FORMATTED")

! Leggo e riscrivo gli header
READ (20,'(a)',ERR=9998) header(1)
READ (20,'(a)',ERR=9998) header(2)
READ (20,'(a)',ERR=9998) header(3)
IF (header(3)(1:10) /= "aaaa mm gg") GOTO 9997

WRITE (21,'(a)') TRIM(header(1))
WRITE (21,'(a)') TRIM(header(2))
WRITE (21,'(2a)') "aaaa mm gg hh",TRIM(header(3)(11:))

! Leggo e riscrivo i dati
DO kd = 1,HUGE(0)
  READ (20,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9996
  READ (chrec,'(i4,2(1x,i2))',ERR=9996) yy,mm,dd
  datac = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=00)  
  datacp1 = datac + timedelta_new(DAY=1)
  CALL getval(datacp1, YEAR=yyp1, MONTH=mmp1, DAY=ddp1)
  
  DO kh = 1,23
    WRITE (21,'(i4.4,3(1x,i2.2),a)') yy,mm,dd,kh,TRIM(chrec(11:))
  ENDDO
  WRITE (21,'(i4.4,3(1x,i2.2),a)') yyp1,mmp1,ddp1,0,TRIM(chrec(11:))
ENDDO

WRITE (*,*) "Elaborazione completate, elaborate ",kd-1," giornate"
STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore leggendo headers ",TRIM(filein)
STOP 2

9997 CONTINUE
WRITE (*,*) "Header inatteso: ",TRIM(filein), &
  " non nel formato estra_orari giornaliero"
STOP 2

9996 CONTINUE
WRITE (*,*) "Errore leggendo i dati ",TRIM(filein)," record dati ",kd
STOP 2

END PROGRAM daily2orari

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

!            1234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "dailyorari.exe [-h] filein fileout"
RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

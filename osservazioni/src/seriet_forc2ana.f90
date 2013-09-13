PROGRAM seriet_forc2ana
!--------------------------------------------------------------------------
! Legge un file seriet e lo riscrive convertendo le scadenze di previsione
! in analisi
!
! Note:
! Programma scritto per gestire run misti analisi-previsioni di Lapmod
! Se gli istanti in output non sono sequenziali (ie. se non lo sono i 
! verification times in input) si ferma con errore.
!
!                                         Versione 1.0.0, Enrico 22/08/2013
!--------------------------------------------------------------------------

USE datetime_class
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.

! Variabili locali
TYPE(datetime) :: datah_in,datah_out
INTEGER :: idp,kp,eof,eor,ios,dd,mm,yyyy,hh,sca,k
CHARACTER (LEN=5000) :: chrec
CHARACTER (LEN=250) :: filein,fileout,chdum

!--------------------------------------------------------------------------

! Parametri da riga comando
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

! Codice EOF
CALL get_eof_eor(eof,eor)

! Apro files
OPEN (UNIT=20, FILE=filein, STATUS= "OLD", ACTION="READ", ERR=9999)
OPEN (UNIT=21, FILE=fileout, STATUS= "REPLACE")

! Leggo e riscrivo gli header
DO k = 1,6
  READ (20,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0 ) GOTO 9998
  WRITE (21,'(a)') TRIM(chrec)
ENDDO

! Leggo e riscrivo i dati
DO k = 1, HUGE(0)
  READ (20,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (ios /= 0 ) GOTO 9997
  READ (chrec,'(i2,1x,i2,1x,i4,1x,i2,1x,i3)',IOSTAT=ios) dd,mm,yyyy,hh,sca
  IF (ios /= 0 ) GOTO 9997
  datah_in = datetime_new(YEAR=yyyy, MONTH=mm, DAY=dd, HOUR=hh)
  datah_out = datah_in + timedelta_new(HOUR=sca)
  CALL getval(datah_out, YEAR=yyyy, MONTH=mm, DAY=dd, HOUR=hh)
  WRITE (21,'(i2.2,a1,i2.2,a1,i4.4,1x,i2.2,1x,i3.3,a)') &
    dd,"/",mm,"/",yyyy,hh,0,TRIM(chrec(18:))
ENDDO

! Chiudo, log e termino
CLOSE(20)
CLOSE(21)

WRITE (*,*) "Programma terminato, elaborati ",k-1," istanti"

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*)  "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*)  "Errore leggendo headers ",TRIM(filein)
STOP

9997 CONTINUE
WRITE (*,*)  "Errore leggendo dati ",TRIM(filein)," riga ",k+6
STOP

END PROGRAM seriet_forc2ana

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: seriet_forc2ana.exe ]-h] filein fileout" 
WRITE (*,*) ""
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

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

PROGRAM split_output
!--------------------------------------------------------------------------
! Legge l'output di Chimere e lo riscrive, mettendo in files separati le
! scadenze relative a ciascuna giornata.
! I files prodotti si chiamano semplicemente YYYYMMDD.dat
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: maxdim = 10000000

! Altre variabili del programma
TYPE(date) :: datac
INTEGER :: ios,eof,eor,kscad,hhc,idata,ntry,n1,n2,nd,cntr,idum
REAL :: field(maxdim)
CHARACTER (LEN=200) :: filein,fileout

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

! Parametri da riga comando
CALL getarg(1,filein)

IF (filein == "" .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: split_output.exe filein"
  WRITE (*,*) "  Legge l'output Chimere e mette in files separati"
  WRITE (*,*) "  le scadenze relative a ciascuna giornata."
  STOP
ENDIF

! Codice per EOF
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 2) Trovo il numero di dati in ciascun record

! Trovo l'ordine di grandezza
ntry = 1
DO 
  IF (ntry > maxdim/10) EXIT

  ntry = ntry * 10
  OPEN (UNIT=30, FILE=filein, FORM="UNFORMATTED", STATUS="OLD", ERR=9999)
  READ (30,IOSTAT=ios) field(1:ntry)
WRITE (*,*) "Ordine di grandezza: =>",ios,ntry/10
  CLOSE(30)

  IF (ios /= 0) EXIT

ENDDO
WRITE (*,*) "Ordine di grandezza: =>",ntry/10

! Ricerca binaria del numero esatto
n1 = ntry/10             ! n.ro piu' alto letto
n2 = ntry                ! n.ro piu' basso non letto

DO
  ntry = (n2+n1)/2

  OPEN (UNIT=30,FILE=filein,FORM="UNFORMATTED",STATUS="OLD",ACTION="READ")
  READ (30,IOSTAT=ios) field(1:ntry)
  CLOSE(30)
  IF (ios == 0) THEN
    n1 = ntry
  ELSE
    n2 = ntry
  ENDIF

  IF (n2-n1 == 1) THEN
    EXIT
  ELSE IF (n2-n1 < 1) THEN
    WRITE (*,*) "Errore ricerca binaria"
    STOP
  ENDIF

ENDDO

nd = n1 - 1
WRITE (*,*) "Numero di dati in un record: ",nd

!--------------------------------------------------------------------------
! 3) Leggo il file e riscrivo in file separti per ciascuna giornata

OPEN (UNIT=30, FILE=filein, STATUS="OLD", FORM="UNFORMATTED", ERR=9999)

cntr = 0
DO kscad = 1, HUGE(kscad)

  READ (30,IOSTAT=ios) idata,field(1:nd)
  IF (ios == eof) THEN
    EXIT
  ELSE IF (ios /= 0) THEN
    GOTO 9998
  ENDIF
!   write(6,*)'idatas ',idata
  hhc = MOD(idata,100)
  datac%dd = MOD(idata/100,100)
  datac%mm = MOD(idata/10000,100)
  datac%yy = idata/1000000

! Se e' il primo record, apro il file di output
  IF (kscad == 1) THEN

    WRITE (fileout,'(i4.4,2i2.2,a)') datac%yy,datac%mm,datac%dd,".dat"
!    WRITE (*,'(3a,i2)') "aproo file ",TRIM(fileout)
    OPEN (UNIT=40, FILE=fileout,  FORM="UNFORMATTED")
    WRITE (40) idata,field(1:nd)
    cntr = cntr + 1
  ENDIF

! Continuo a scrivere sul file gia' aperto
  IF (kscad > 1) THEN
    WRITE (40) idata,field(1:nd)
    cntr = cntr + 1
  ENDIF

! Se sono arrivato alle ore 24, chiudo il file e ne apro uno nuovo, 
! riscrivendo il record corrente
  IF (kscad > 1 .AND. hhc == 0) THEN
    CLOSE (40)
    WRITE (*,'(3a,i2)') "Chiuso file ",TRIM(fileout)," record scritti ",cntr
    cntr = 0

    WRITE (fileout,'(i4.4,2i2.2,a)') datac%yy,datac%mm,datac%dd,".dat"
!    WRITE (*,'(3a,i2)') "aproo file ",TRIM(fileout)
    OPEN (UNIT=40, FILE=fileout,  FORM="UNFORMATTED")
    WRITE (40) idata,field(1:nd)
    cntr = cntr + 1
  ENDIF

ENDDO                    ! scadenze


 

! Conclusione
CLOSE (30)
CLOSE (40)
WRITE (*,'(3a,i2)') "Chiuso file ",TRIM(fileout)," record scritti ",cntr
WRITE (*,*) "Input file terminato, elaborate ",kscad-1," scadenze"

STOP

!--------------------------------------------------------------------------
! 4) Gestione erori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," scadenza ",idata
STOP

END PROGRAM split_output

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







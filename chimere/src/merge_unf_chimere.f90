PROGRAM merge_unf_chimere
!--------------------------------------------------------------------------
! Dati 2 files unformatted chimere con 25 istanti (anche vuoti o incompleti),
! ne scrive uno che contiene:
! - finche' ci sono, i dati del primo
! - quando il primo file termina, i dati del secondo
! - se termina anche il secondo, ripete l'ultimo record scritto
!
! NOTE:
! - I record in output contengono una data specificata dall'utente, che puo'
!   essere diversa da quelle dei files di input
! - Tutti i files devono avere record della stessa lunghezza, specificata 
!   dall'utente
! - Programma pensato per riempire i buchi negli input per un run lungo
! - Se entrambi i files di input contengono meno di 25 record, i record
!   non presenti sono uguali all'ultimo letto. Se entrambi i files di input
!  sono vuoti, tutti i valori in output sono messi a 0
!
! Uso: merge_unf_chimere.exe filein1 filein2 fileout nptot data [-h]
!
!                                           Versione 1.0, Enrico 05/03/2007
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

REAL, ALLOCATABLE :: f(:),f1(:),f2(:)
TYPE(date) :: data,datap1
INTEGER :: nptot,kh,idata,idatap1,idata1,idata2,nr1,nr2,nr3,hh1,hh2
INTEGER :: ios,ios1,ios2,eof,eor
CHARACTER (LEN=80) :: filein1,filein2,fileout,chdum
LOGICAL :: lf1,lf2

!--------------------------------------------------------------------------
! 1)  Preliminari

! Parametri da riga comando
CALL getarg(1,filein1)
CALL getarg(2,filein2)
CALL getarg(3,fileout)
CALL getarg(4,chdum)
READ (chdum,*,IOSTAT=ios1) nptot
CALL getarg(5,chdum)
READ (chdum,'(i4,2i2)',IOSTAT=ios2) data%yy,data%mm,data%dd

IF (TRIM(filein1) == "-h" .OR. TRIM(filein1) == "" .OR. &
    TRIM(filein2) == "" .OR. TRIM(fileout) == "" .OR. &
    nptot <= 0 .OR. ios1 /= 0 .OR. ios2 /= 0) THEN
  WRITE (*,*) "Uso: merge_unf_chimere.exe filein1 filein2 fileout nptot data [-h]"
  WRITE (*,*) "Scrive su fileout 25 record orari relativi alla data richiesta, prendendoli"
  WRITE (*,*) "se ci sono da file1, altrimenti da file2"
  STOP
ENDIF

! Apro files
OPEN (21, FILE=filein1, STATUS="OLD", FORM="UNFORMATTED",IOSTAT=ios1)
IF (ios == 0) THEN
  lf1 = .TRUE.
ELSE
  lf1 = .FALSE.
ENDIF

OPEN (22, FILE=filein2, STATUS="OLD", FORM="UNFORMATTED",IOSTAT=ios2)
IF (ios == 0) THEN
  lf2 = .TRUE.
ELSE
  lf2 = .FALSE.
ENDIF

OPEN (30, FILE=fileout, STATUS="REPLACE", FORM="UNFORMATTED")

! Altre operazioni
CALL get_eof_eor(eof, eor)

ALLOCATE (f(nptot),f1(nptot),f2(nptot))
f(:) = 0.

datap1 = data + 1
idata = 1000000*data%yy + 10000*data%mm + 100*data%dd
idatap1 = 1000000*datap1%yy + 10000*datap1%mm + 100*datap1%dd

!--------------------------------------------------------------------------
! 2) Leggo e scrivo

DO kh = 0,24

  IF (lf1) READ (21,IOSTAT=ios1) idata1,f1(1:nptot)
  IF (lf2) READ (22,IOSTAT=ios2) idata2,f2(1:nptot)
  hh1 = MOD(idata1,100)
  hh2 = MOD(idata2,100)

  IF (lf1 .AND. ios1 == 0) THEN
    f(1:nptot) = f1(1:nptot)
    IF ((kh<24 .AND. hh1/=kh) .OR. kh==24 .AND. hh1/=0) GOTO 9999
    nr1 = nr1 + 1

  ELSE IF (lf2 .AND. ios2 == 0) THEN
    lf1 = .FALSE.
    f(1:nptot) = f2(1:nptot)
    IF ((kh<24 .AND. hh2/=kh) .OR. kh==24 .AND. hh2/=0) GOTO 9998
    nr2 = nr2 + 1

  ELSE
    lf1 = .FALSE.
    lf2 = .FALSE.
    nr3 = nr3 + 1

  ENDIF    

  IF (kh < 24) THEN
    WRITE (30) idata+kh, f(1:nptot)
  ELSE
    WRITE (30) idatap1, f(1:nptot)
  ENDIF

ENDDO

CLOSE(21)
CLOSE(22)
CLOSE(30)

WRITE (*,'(a,3i3)') "Record da file1,file2,inventati: ",nr1,nr2,nr3

STOP

!--------------------------------------------------------------------------
! 3) Gestione Errori

9999 CONTINUE
WRITE (*,'(3a,i3,a,i3)') "Ora in input disallineata in ",TRIM(filein1), &
  " : ",MOD(idata1,100)," attesa",kh
STOP

9998 CONTINUE
WRITE (*,'(3a,i3,a,i3)') "Ora in input disallineata in ",TRIM(filein2), &
  " : ",MOD(idata2,100)," attesa",kh
STOP

END PROGRAM merge_unf_chimere

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

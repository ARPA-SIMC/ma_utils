PROGRAM sum_unf_chimere
!--------------------------------------------------------------------------
! Dati 2 files unformatted chimere, ne scrive uno che contiene la loro 
! somma
!
! Uso: sum_unf_chimere.exe filein1 filein2 fileout nptot [-h]
!
!                                         Versione 1.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------
IMPLICIT NONE

REAL, ALLOCATABLE :: f(:),f1(:),f2(:)
INTEGER :: nptot,k,idata1,idata2
INTEGER :: ios,eof,eor
CHARACTER (LEN=200) :: filein1,filein2,fileout,chdum

!--------------------------------------------------------------------------
! 1)  Preliminari

! Parametri da riga comando
CALL getarg(1,filein1)
CALL getarg(2,filein2)
CALL getarg(3,fileout)
CALL getarg(4,chdum)
READ (chdum,*,IOSTAT=ios) nptot

IF (TRIM(filein1) == "-h" .OR. TRIM(filein1) == "" .OR. &
    TRIM(filein2) == "" .OR. TRIM(fileout) == "" .OR. &
    nptot <= 0 .OR. ios /= 0 ) THEN

!              123456789012345678901234567890123456789012345678901234567890123456789012345
  WRITE (*,*) "Uso: sum_unf_chimere.exe filein1 filein2 fileout nptot"
  WRITE (*,*) "Dati 2 files unformatted chimere, ne scrive uno che contiene la loro somma" 
  STOP

ENDIF

! Altre operazioni
CALL get_eof_eor(eof, eor)
ALLOCATE (f(nptot),f1(nptot),f2(nptot))

OPEN (21, FILE=filein1, STATUS="OLD", FORM="UNFORMATTED",ERR=9999)
OPEN (22, FILE=filein2, STATUS="OLD", FORM="UNFORMATTED",ERR=9998)
OPEN (30, FILE=fileout, STATUS="REPLACE", FORM="UNFORMATTED")

!--------------------------------------------------------------------------
! 2) Leggo, sommo, scrivo

DO k = 1,HUGE(k)

  READ (21,IOSTAT=ios) idata1,f1(1:nptot)
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9997

  READ (22,IOSTAT=ios) idata2,f2(1:nptot)
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9996

  IF (idata1 /= idata2) GOTO 9995

  f(1:nptot) = f1(1:nptot) + f2(1:nptot)

  WRITE (30) idata1,f(1:nptot)

ENDDO
CLOSE(20)

WRITE (*,*) "Sum_unf_chimere.exe: letti e riscritti ",k-1," record"

STOP

!--------------------------------------------------------------------------
! 3) Gestione Errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein1)
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein2)
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein1)
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein2)
STOP

9995 CONTINUE
WRITE (*,*) "Date in input disallineate: ",idata1," ",idata2
STOP

END PROGRAM sum_unf_chimere

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

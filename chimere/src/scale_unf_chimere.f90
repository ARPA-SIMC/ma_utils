PROGRAM scale_unf_chimere
!--------------------------------------------------------------------------
! Dato un file unformatted chimere, lo riscrive riscalando linearmente 
! (ax+b) tutti i valori.
!
! Uso: scale_unf_chimere.exe filein fileout nptot a b [-h]
!
!                                           Versione 1.0, Enrico 03/03/2004
!--------------------------------------------------------------------------
IMPLICIT NONE

REAL, ALLOCATABLE :: f(:)
REAL :: aa,bb
INTEGER :: nptot,k,idata
INTEGER :: ios,ios1,ios2,ios3,eof,eor
CHARACTER (LEN=80) :: filein,fileout,chdum

!--------------------------------------------------------------------------
! 1)  Preliminari

! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,chdum)
READ (chdum,*,IOSTAT=ios1) nptot
CALL getarg(4,chdum)
READ (chdum,*,IOSTAT=ios2) aa
CALL getarg(5,chdum)
READ (chdum,*,IOSTAT=ios3) bb

IF (TRIM(filein) == "-h" .OR. TRIM(filein) == "" .OR. &
    TRIM(fileout) == "" .OR. nptot <= 0 .OR. &
    ios1 /= 0 .OR. ios2 /= 0 .OR.ios3 /= 0) THEN

  WRITE (*,*) "Uso: scale_unf_chimere.exe filein fileout nptot a b"
  WRITE (*,*)
  WRITE (*,*) " Dato un file unformatted chimere, lo riscrive riscalando linearmente"
  WRITE (*,*) "(ax+b) tutti i valori."
  STOP

ENDIF

! Altre operazioni
CALL get_eof_eor(eof, eor)
ALLOCATE (f(nptot))

OPEN (20, FILE=filein, STATUS="OLD", FORM="UNFORMATTED",ERR=9999)
OPEN (30, FILE=fileout, STATUS="REPLACE", FORM="UNFORMATTED")


!--------------------------------------------------------------------------
! 2) Leggo, riscalo, scrivo

DO k = 1,HUGE(k)

  READ (20,IOSTAT=ios) idata,f(1:nptot)
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998

  f(1:nptot) = aa * f(1:nptot) + bb

  WRITE (30) idata,f(1:nptot)

ENDDO
CLOSE(20)

WRITE (*,*) "Scale_unf_chimere.exe: letti e riscritti ",k-1," record"

STOP

!--------------------------------------------------------------------------
! 3) Gestione Errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo  scale ",TRIM(filein)
STOP

END PROGRAM scale_unf_chimere

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

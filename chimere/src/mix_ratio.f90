PROGRAM mix_ratio
!--------------------------------------------------------------------------
! Dati i campi di Umidita' specifica, T e P, calcola il mixing ratio.
!
! Uso: mix_ratio.exe [-h] np nz
!
! Input (files intermedi Chimere, unformatted)
! SPHU_3D: umidita' specifica
!
! Output:
! MIXR_3D: mixing ratio
!
!                                         Versione 1.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------
IMPLICIT NONE

INTEGER :: np,nz,nt
INTEGER :: iarg(2),ios(2),eof,eor,kp,kz,k,idata,idata2,idata3,ier,cnt
REAL, ALLOCATABLE :: q(:,:),mr(:,:)
REAL :: pl0,pl1,alt,tm
CHARACTER (LEN=200) :: fileq,filemr,chdum

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

! Nomi files I/O
fileq = "SPHU_3D"
filemr = "MIXR_3D"

! Parametri da riga comando
DO kp = 1,2
  CALL getarg(kp,chdum)

  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE
    READ(chdum,*,IOSTAT=ios(kp)) iarg(kp)
  ENDIF
ENDDO

IF (ANY(ios(:) /= 0) .OR. ANY(iarg(:) <= 0)) THEN
  CALL write_help
  STOP
ENDIF

np = iarg(1)
nz = iarg(2)

! Alloco variabili
ALLOCATE (q(np,nz),mr(np,nz))

! Apro i files
OPEN (UNIT=20, FILE=fileq, STATUS="OLD", FORM="UNFORMATTED", ERR=9999)
OPEN (UNIT=30, FILE=filemr, STATUS="REPLACE", FORM="UNFORMATTED")

CALL get_eof_eor(eof, eor)
 
!--------------------------------------------------------------------------
! 2) Lettura, calcolo, scrittura (ciclo sulle scadenze)

cnt = 0
DO k = 1,HUGE(k)

! Leggo
  READ (20,IOSTAT=ios(1)) idata,((q(kp,kz),kp=1,np),kz=1,nz)

  IF (ios(1) == eof) EXIT  
  IF (ios(1) /= 0) GOTO 9996  
 
! Calcolo mixr
  DO kz = 1,nz
  DO kp = 1,np 
    IF (q(kp,kz) < 0.) THEN
      mr(kp,kz) = 0.
      cnt = cnt + 1
    ELSE IF (q(kp,kz) > 1.) THEN
      mr(kp,kz) = 1.
      cnt = cnt + 1
    ELSE
      mr(kp,kz) = q(kp,kz) / (1.-q(kp,kz))
    ENDIF
  ENDDO
  ENDDO

! Scrivo
  WRITE (30) idata,((mr(kp,kz),kp=1,np),kz=1,nz)

ENDDO
nt = k-1

WRITE (*,'(a,3i6)') "Calcolato mixing ratio. np,nz,nt: ",np,nz,nt
WRITE (*,'(a,2i8)') "Valori tappati: ",cnt

STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileq)
STOP

9996 CONTINUE
WRITE (*,*) "Errore lettura file input ",ios(1)
STOP

END PROGRAM mix_ratio

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
WRITE (*,*) "Uso: mix_ratio.exe [-h] np nz"

RETURN

END SUBROUTINE write_help

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

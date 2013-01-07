PROGRAM show_unf_chimere
!--------------------------------------------------------------------------
! Programma per vedere cosa c'e' in un file unformatted chimere
! Uso: show_unf_chimere.exe filein np nz [-h] [-a fileasc]
!
!                                           Versione 2.0, Enrico 06/12/2012
!--------------------------------------------------------------------------
IMPLICIT NONE

!REAL (KIND=8), ALLOCATABLE :: f(:,:)
!REAL (KIND=8) :: f(5220,8)
!DOUBLE PRECISION, ALLOCATABLE :: f(:,:)
REAL, ALLOCATABLE :: f(:,:)
INTEGER :: np,nz,nt,idpts
INTEGER :: ios,ios1,ios2,idp,eof,eor,k,kp,kz,idata
CHARACTER (LEN=80) :: filein,fileasc,chdum
CHARACTER (LEN=1) :: next_arg
LOGICAL :: out_asc,out_pts

!--------------------------------------------------------------------------
! 1) Parametri da riga comando

out_asc = .FALSE.
out_pts = .FALSE.
next_arg = ""
ios = 0
idp = 0

DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-a") THEN
    next_arg = "a"
    out_asc = .TRUE. 
  ELSE IF (TRIM(chdum) == "-p") THEN
    next_arg = "p"
    out_pts = .TRUE. 
  ELSE IF (next_arg == "a") THEN
    next_arg = ""
    fileasc = chdum
  ELSE IF (next_arg == "p") THEN
    next_arg = ""
    READ (chdum,*,IOSTAT=ios) idpts

  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      READ (chdum,*,IOSTAT=ios1) np
    CASE (3)
      READ (chdum,*,IOSTAT=ios2) nz
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

IF (filein=="" .OR. idp/=3 .OR. np<=0 .OR. nz<=0 .OR. ios/=0 .OR. &
  ios1/=0 .OR. ios2/=0) THEN
  CALL write_help
  STOP
ENDIF

CALL get_eof_eor(eof, eor)
ALLOCATE (f(np,nz))

!--------------------------------------------------------------------------
! 2) Esamino il file

IF (out_asc) &
  OPEN (UNIT=30, FILE=fileasc, STATUS="REPLACE", FORM="FORMATTED")

OPEN (UNIT=20, FILE=filein, STATUS="OLD", FORM="UNFORMATTED")

DO k = 1,HUGE(k)

  READ (20,IOSTAT=ios) idata,((f(kp,kz),kp=1,np),kz=1,nz) 
  IF (ios == eof) EXIT
  IF (ios /= 0) THEN
    WRITE (*,*) "Errore di lettura"
    STOP
  ENDIF

  IF (out_asc) WRITE (30,*) idata,((f(kp,kz),kp=1,np),kz=1,nz)  

  WRITE (*,*) 
  WRITE (*,*) "Record: ",idata
  DO kz = 1,nz
    IF (.NOT. out_pts) THEN
      WRITE (*,*) "Liv,max,min,med: ",kz,MAXVAL(f(:,kz)), &
        MINVAL(f(:,kz)),SUM(f(:,kz))/REAL(np)
    ELSE
      WRITE (*,*) "Liv,max,min,med: ",kz,MAXVAL(f(:,kz)), &
        MINVAL(f(:,kz)),SUM(f(:,kz))/REAL(np)," point ",idpts,f(idpts,kz)

    ENDIF
  ENDDO

ENDDO
CLOSE(20)

END PROGRAM show_unf_chimere

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a schermo l'help del programma
!
WRITE (*,*) "Uso: show_unf_chimere.exe filein np nz [-h] [-a fileasc] [-p NPT]"
WRITE (*,*) "  Con opzione -p visualizza anche i valori in un punto specificato"
WRITE (*,*) "  Con opzione -a riscrive filein in foramto ASCII su fileasc"
WRITE (*,*) "  Per ora gestisce solo il formato standard Chimere:"
WRITE (*,*) "  READ (iu) idata,field(1:np,1:nz)"
!
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

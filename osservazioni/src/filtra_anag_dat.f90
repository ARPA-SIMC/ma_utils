PROGRAM filtra_anag_dat
!--------------------------------------------------------------------------
! filtra il file db_anagrafica.dat, tenedo solo le stazioni che appartengono
! a un elenco specificato.
! Uso: filtra_anag.exe filein fileout
!--------------------------------------------------------------------------
IMPLICIT NONE

INTEGER, PARAMETER :: maxstz = 1000
CHARACTER (LEN=40), PARAMETER :: anag_name = "db_anagrafica.dat"

INTEGER :: req_net(maxstz),req_usr(maxstz)
INTEGER :: anag_net,anag_usr,req_stz,idr,idu,nok
INTEGER :: eof,eor,ios,k
CHARACTER (LEN=200) :: anag_path,filein,fileout,home_bonafe,chrec

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: filtra_anag_dat.exe filein fileout"
  WRITE (*,*) " filein: elenco stazioni richieste (una per riga): id-rete, id-utente"
  WRITE (*,*) " fileout: formato anagrafica"
  STOP
ENDIF

! Trovo codice eof
CALL get_eof_eor(eof,eor)

! Leggo lista stazioni richieste
OPEN (UNIT=25, FILE=filein, STATUS="OLD",IOSTAT=ios)
IF (ios /= 0) GOTO 9999
DO k = 1,HUGE(0)
  READ (25,*,IOSTAT=ios) idr,idu
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998
  IF (k > maxstz) GOTO 9997
  req_net(k) = idr
  req_usr(k) = idu
ENDDO
CLOSE (25)
req_stz = k - 1

! Apro il file di anagrafica
CALL GETENV('HOME_BONAFE',HOME_BONAFE)
anag_path = TRIM(HOME_BONAFE)//"/osservazioni/dat/"
OPEN (UNIT=30, FILE=TRIM(anag_path)//TRIM(anag_name), STATUS= "OLD", &
  ACTION="READ")
READ (30,'(a)')
READ (30,'(a)') chrec
READ (30,'(a)')

! Apro il file di output
OPEN (UNIT=24, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (24,*)
WRITE (24,'(a)') TRIM(chrec)
WRITE (24,*)

!--------------------------------------------------------------------------
! 2) Leggo il file di anagrafica e riscrivo solo le stazioni richieste

anag: DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (chrec == "") THEN
!   WRITE (*,*) "skip blank line"
    CYCLE anag
  ENDIF
 
  READ (chrec,*,IOSTAT=ios) anag_net,anag_usr
  IF (ios /= 0) THEN
!   WRITE (*,*) "Warning, skip record illegale nel file di anagrafica"
!   WRITE (*,'(a)') TRIM(chrec)
    CYCLE anag
  ENDIF

  DO k = 1,req_stz
    IF (anag_net == req_net(k) .AND. anag_usr == req_usr(k)) THEN
      WRITE (24,'(a)') TRIM(chrec)
      nok = nok + 1
      CYCLE anag
    ENDIF
  ENDDO

ENDDO anag
CLOSE(30)

WRITE (*,*) "Stazioni richieste ",req_stz," trovate ",nok

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," record ",k
STOP

9997 CONTINUE
WRITE (*,*) "Richeste troppe stazioni, aumentare parametro maxstz (",maxstz,")"
STOP

END PROGRAM filtra_anag_dat

!=========================================================================

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


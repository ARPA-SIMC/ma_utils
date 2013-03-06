PROGRAM tfill_seriet
!--------------------------------------------------------------------------
! Legge un file seriet e lo riscrive sostituendo i record che hanno valori
! mancanti con l'ultimo record con tutti dati validi relativo alla stessa 
! ora
!
! Note:
! Per ora gestisce solo files di analisi (sca = 0)
!
!                                         Versione 1.0.1, Enrico 01/03/2013
!--------------------------------------------------------------------------

USE datetime_class
IMPLICIT NONE

INTEGER, PARAMETER :: nhead = 6
INTEGER, PARAMETER :: chhead = 17
INTEGER, PARAMETER :: fw = 10
REAL, PARAMETER :: rmis_ser = -9999.
REAL, PARAMETER :: rmis_sex = -1.E30

! Variabili locali
TYPE (datetime) :: datahc
REAL, ALLOCATABLE :: val(:)
INTEGER :: idp,kp,eof,eor,ios,k,yy,mm,dd,hh,sca,ncol,cnt_ok,cnt_mod
CHARACTER (LEN=10000) :: rec,rec_stored(0:23)
CHARACTER (LEN=250) :: filein,fileout,chdum

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
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

! 1.2 apro files, ricopio headers
OPEN (UNIT=30, FILE=filein, STATUS="OLD",ERR=9999)
OPEN (UNIT=31, FILE=fileout, STATUS="REPLACE")

DO k = 1,nhead
  READ (30,'(a)') rec
  WRITE (31,'(a)') TRIM(rec)
ENDDO
ncol = (LEN(TRIM(rec))-chhead) / (fw+1)
IF (MOD(LEN(TRIM(rec))-chhead,fw+1) /= 0) GOTO 9998
ALLOCATE (val(ncol))

! 1.3 codice per eof
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 2) Lettura/scrittura

rec_stored(:) = ""
cnt_ok = 0
cnt_mod = 0

DO k = 1,HUGE(0)
  READ (30,'(a)',IOSTAT=ios) rec
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9997
  READ (rec,'(i2,1x,i2,1x,i4,1x,i2,1x,i3)') dd,mm,yy,hh,sca
  READ (rec(chhead+1:),*) val(1:ncol)
  IF (sca /= 0 ) GOTO 9995

  IF (ANY(val(1:ncol) == rmis_ser) .OR. ANY(val(1:ncol) == rmis_sex)) THEN
    IF (TRIM(rec_stored(hh)) == "") GOTO 9996
    WRITE (31,'(i2.2,a1,i2.2,a1,i4.4,1x,i2.2,1x,i3.3,a)') &
      dd,"/",mm,"/",yy,hh,0,TRIM(rec_stored(hh))
    cnt_mod = cnt_mod + 1

  ELSE
    WRITE (31,'(a)') TRIM(rec)
    rec_stored(hh) = rec(chhead+1:)       
    cnt_ok = cnt_ok + 1

  ENDIF
ENDDO

WRITE (*,*) "Record scritti ",k-1
WRITE (*,*) "di cui: invariati ",cnt_ok," ricopiati ",cnt_mod
STOP

!--------------------------------------------------------------------------

9999 CONTINUE
WRITE (*,*)  "Errore aprendo ",TRIM(filein)
STOP 1

9998 CONTINUE
WRITE (*,*)  "Errore nel calcolo del numero di colonne in input"
STOP 2

9997 CONTINUE
WRITE (*,*)  "Errore leggendo ",TRIM(filein)
STOP 3

9996 CONTINUE
WRITE (*,*)  "Errore: presenti dati mancanti nel primo record relativo alle ore ",hh
STOP 4

9995 CONTINUE
WRITE (*,*) "Trovata scadenza diversa da 0 (",sca,")"
WRITE (*,*) "Questo programma gestisce solo le analisi"
STOP 5

END PROGRAM tfill_seriet

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*)
WRITE (*,*) "Uso: tfill_sereit.exe [-h] filein fileout" 
WRITE (*,*) "Legge un file seriet di analisi (sca = 0) e lo riscrive sostituendo "
WRITE (*,*) "  i record che hanno valori mancanti con l'ultimo record con tutti dati "
WRITE (*,*) "  validi relativo alla stessa ora"
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


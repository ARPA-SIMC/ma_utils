PROGRAM intercala_grib
!--------------------------------------------------------------------------
! Legge molti files con GRIB e li riscrive in un unico file, prendendo
! ciclicamente un grib da ciascun file. Non fa nessun controllo (non 
! decodifica i messaggi), e si ferma al primo EOF
!
!                                                   V1.0, Enrico 16/07/2009
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

!--------------------------------------------------------------------------
! 0) Dichiarazioni

! Parametri costanti
INTEGER, PARAMETER :: mxfiles = 200

! Altre varibaili del programma
INTEGER :: if(mxfiles),ig(mxfiles),ifo,nfiles,cntout
INTEGER :: k,k2,ios,eof,eor,iret
CHARACTER (LEN=120) :: filelst,fileout,filein(mxfiles),error_message

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comandi
CALL getarg(1,filelst)
CALL getarg(2,fileout)

IF (TRIM(filelst) == "" .OR. TRIM(fileout) == "" .OR. &
    TRIM(filelst) == "-h") THEN
  CALL scrive_help
  STOP
ENDIF

! Codice EOF
CALL get_eof_eor(eof,eor)

! Leggo elenco files da elaborare
OPEN (UNIT=30, FILE=filelst, STATUS="OLD", ACTION="READ", ERR=9999)
DO k = 1,mxfiles
  READ (30,'(a)',IOSTAT=ios) filein(k)
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998
ENDDO
CLOSE (30)

nfiles = k - 1
IF (nfiles == mxfiles) THEN
  WRITE (*,*) "Richiesti tropppi files, elaboro i primi ",mxfiles
  WRITE (*,*) "Aumentare parametro mxfiles"
ENDIF

! Apro files
DO k = 1, nfiles
  CALL grib_open_file(if(k),filein(k),"r",iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9997
ENDDO
CALL grib_open_file(ifo,fileout,"w",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9996

!--------------------------------------------------------------------------
! 2) Legggo/scrivo

cntout = 0
outer: DO k2 = 1,HUGE(0)

! Leggo il prossimo grib da ciscuno dei files in input
  ig(:) = -1
  DO k = 1, nfiles
    CALL grib_new_from_file(if(k),ig(k),iret)
    IF (iret == GRIB_END_OF_FILE) EXIT outer
    IF (iret /= GRIB_SUCCESS) GOTO 9995
  ENDDO

! Se ho trovato un grib per ciascun file input, li scrivo tutti su fileout
  DO k = 1, nfiles
    CALL grib_write(ig(k),ifo,iret)
    IF (iret /= GRIB_SUCCESS) GOTO 9994
    CALL grib_release(ig(k))
    cntout = cntout + 1
  ENDDO
ENDDO outer

WRITE (*,*) "Esecuzione terminata, elaborati ",k2-1," campi per ",nfiles," files"
WRITE (*,*) "EOF trovato su ",TRIM(filein(k))," tot. campi scritti ",cntout
STOP

!==========================================================================
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filelst)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filelst)
STOP

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein(k))
CALL grib_get_error_string(iret,error_message)
WRITE (*,*) error_message
STOP

9996 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileout)
CALL grib_get_error_string(iret,error_message)
WRITE (*,*) error_message
STOP

9995 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein(k))," grib numero ",k2
CALL grib_get_error_string(iret,error_message)
WRITE (*,*) error_message
STOP

9994 CONTINUE
WRITE (*,*) "Errore scrivendo su ",TRIM(fileout)," campo ",cntout+1
CALL grib_get_error_string(iret,error_message)
WRITE (*,*) error_message
STOP

END PROGRAM intercala_grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo l'help del programma
!
IMPLICIT NONE

!                12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*)
WRITE (*,'(a)') "Uso: intercala_grib.exe filelst fileout"
WRITE (*,'(a)') ""
WRITE (*,'(a)') "Legge molti files con GRIB e li riscrive in un unico file, prendendo "
WRITE (*,'(a)') "ciclicamente un grib da ciascun file"
WRITE (*,'(a)') "Filelst contiene la lista dei files da elaborare (un file per riga)"
WRITE (*,'(a)') ""

RETURN
END SUBROUTINE scrive_help

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


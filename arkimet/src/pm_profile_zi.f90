PROGRAM pm_profile_zi
!--------------------------------------------------------------------------
! Dati due files seriet, uno estratto da archivi Chimere (con i profili di 
! Z e PM2.5) e uno estratto da LAMAZ (con Zi), calcola il profilo medio di
! PM2.5 al di sotto dell'altezza di rimescolamento. Vengono calcolati 
! profili medi indipendenti, a partire dai dati in cui Zi cade in un dato 
! livello Chimere.
!
! Programma scritto per test prodotto QM1 Quitsat, specifico e poco 
! flessibile.
!
!                                         Versione 1.0.2, Enrico 13/01/2014
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.
INTEGER, PARAMETER :: nz = 8      ! numero livelli file Ninfa
INTEGER, PARAMETER :: iu1 = 31
INTEGER, PARAMETER :: iu2 = 32
INTEGER, PARAMETER :: iuo = 40

! Contatori
REAL :: prf(nz,nz)                ! livelli, ultimo livello < zi
REAL :: zave(nz)
INTEGER :: nok(nz)

! Altre variabili del programma
TYPE (date) :: data1,data2
CHARACTER (LEN=200) :: file1,file2,fileout,chfmt1,chfmt2,chfmt3
REAL :: zi,val(nz*2)              ! val(1:nz): Z ; val(nz+1:2*nz): PM2.5
INTEGER :: nf1,nf2,fmt1,fmt2
INTEGER :: hh1,hh2,sca1,sca2
INTEGER :: k,kl,idpr,istok,eof,eor,ist_nomis,ist_ok,ios

!##########################################################################
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1) Codice per EOF

CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 1.2) Parametri da riga comandi

CALL getarg(1,file1)
CALL getarg(2,file2)
CALL getarg(3,fileout)

IF (file1 == "-h" .OR. file1 == "" .OR. file2 == "" .OR. fileout == "") THEN
  WRITE (*,*) "Uso: pm_profile_z1.exe file_zi file_chimere file_out"
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.3) Apro files, controlli

CALL inq_file_smr(file1,fmt1,nf1)
CALL inq_file_smr(file2,fmt2,nf2)
IF (nf1 /= 1 .OR. nf2 /= nz*2) GOTO 9999
IF (fmt1 /= 1 .OR. fmt1 /= 1) GOTO 9998

OPEN (UNIT=iu1, FILE=file1, STATUS="OLD", FORM="FORMATTED",IOSTAT=ios)
IF (ios /= 0) GOTO 9997
OPEN (UNIT=iu2, FILE=file2, STATUS="OLD", FORM="FORMATTED",IOSTAT=ios)
IF (ios /= 0) GOTO 9996
DO k = 1,6
  READ (iu1,*)
  READ (iu2,*)
ENDDO

!--------------------------------------------------------------------------
! 2) Elaborazioni

prf(:,:) = 0.
zave(:) = 0.
nok(:) = 0
ist_nomis = 0
ist_ok = 0

DO k=1,HUGE(0)

! leggo un istante
  val(:) = rmis
  zi = rmis

  CALL read_rec_smr_scalar(iu1,fmt1,nf1,data1,hh1,sca1,zi,ios)
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9995
  CALL read_rec_smr(iu2,fmt2,nf2,data2,hh2,sca2,val(1:2*nz),ios)
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9994
  IF (data1 /= data2 .OR. hh1 /= hh2 .OR. sca1 /= sca2) GOTO 9993

! trovo ultimo livello < Zi
  DO kl = 1,nz
    IF (val(kl) > zi) EXIT
  ENDDO
  idpr = kl - 1

! Skip se dati mancanti o Zi < 1o livello
  IF (ANY(val(1:2*nz)==rmis) .OR. zi == rmis) CYCLE
  zave(1:nz) = zave(1:nz) + val(1:nz)
  ist_nomis = ist_nomis + 1
  IF (idpr == 0) CYCLE
  ist_ok = ist_ok + 1
  
! Salvo il profilo PM2.5
  prf(1:idpr,idpr) = prf(1:idpr,idpr) + val(nz+1:nz+idpr)
  nok(idpr) = nok(idpr) + 1

ENDDO

WRITE (*,'(a,3i8)') "Istanti nei files, con dati, con Zi > lay1: ", &
  k,ist_nomis,ist_ok
IF (ist_ok == 0) STOP

!--------------------------------------------------------------------------
! 3) Output

zave(1:nz) = zave(1:nz) / REAL(ist_nomis)

DO kl = 1,nz
IF (nok(kl) >0) THEN
  prf(1:kl,kl) = prf(1:kl,kl) / REAL(nok(kl))
ENDIF
ENDDO

WRITE (chfmt1,'(a,i2,a)') '(25x,',nz,'(1x,f5.0))'
WRITE (chfmt2,'(a,i2,a)') '(f5.0,a7,f5.0,2x,i6,',nz,'(1x,f5.2))'
WRITE (chfmt3,'(a,i2,a)') '(f5.0,a7,7x,i6,',nz,'(1x,f5.2))'

OPEN (UNIT=iuo, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (iuo,chfmt1) zave(1:nz)
DO k = 1,nz-1
  WRITE (iuo,chfmt2) zave(k)," < Zi <",zave(k+1),nok(k),prf(1:k,k)
ENDDO
WRITE (iuo,chfmt3) zave(nz)," < Zi  ",nok(nz),prf(1:nz,nz)

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Campi inattesi nei files di input "
WRITE (*,*) "attesi ",1,nz*2,"trovati",nf1,nf2
STOP

9998 CONTINUE
WRITE (*,*) "Formato inatteso nei files di input"
STOP

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file1)
STOP

9996 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file2)
STOP

9995 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file1)
STOP

9994 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file2)
STOP

9993 CONTINUE
WRITE (*,*) "Data-scad disallineate in input, istante ",k
STOP

END PROGRAM pm_profile_zi

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE inq_file_smr(file,idfmt,nf)
!-------------------------------------------------------------------------
! Dato un file SMR, ritorna il suo formato e il numero di campi
! formati:
! 0 = sconosciuto
! 1 = seriet per XLS
! 2 = seriet per fortran
! 3 = estra_orari
! 4 = trasp_temp
!-------------------------------------------------------------------------
IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: mxpar = 1000

! argomenti della subroutine
CHARACTER (LEN=200), INTENT(IN) :: file
INTEGER, INTENT(OUT) :: idfmt,nf

! Variabili locali
INTEGER :: iun,k,chead,eof,eor,ios
CHARACTER (LEN=80) :: str
CHARACTER (LEN=10) :: ch10
CHARACTER (LEN=1) :: ch1
LOGICAL :: l1

!-------------------------------------------------------------------------
! Preliminari
CALL get_eof_eor(eof,eor)

idfmt = 0
nf = 0

!-------------------------------------------------------------------------
! Cerco un'unita' libera 
DO k = 10,99
  INQUIRE (UNIT=k, OPENED=l1, IOSTAT=ios)
  IF (.NOT. l1 .AND. ios==0) THEN
    iun = k
    EXIT
  ENDIF
ENDDO
IF (iun == 0) GOTO 9999   ! non ho torvato nessuna unita' libera

!-------------------------------------------------------------------------
! Trovo il formato

OPEN (UNIT=iun, FILE=file, STATUS="OLD", ERR=9998)
READ (iun,*,END=9997,ERR=9997)
READ (iun,*,END=9997,ERR=9997)
READ (iun,'(a)',END=9997,ERR=9997) str

IF (str(1:19) == "aaaa mm gg hh staz.") THEN        ! trasp_temp
  idfmt = 4
  chead = 19

ELSE IF (str(1:13) == "aaaa mm gg hh") THEN         ! estra_orari
  idfmt = 3
  chead = 13

ELSE                                                ! seriet
  READ (iun,*,END=9997,ERR=9997)
  READ (iun,*,END=9997,ERR=9997)
  READ (iun,'(a)',END=9997,ERR=9997) str
  IF (str(1:17) == "gg/mm/aaaa hh sca") THEN 
    idfmt = 1
    chead = 17
  ELSE IF (str(1:17) == "aaaa mm gg hh sca") THEN 
    idfmt = 2
    chead = 17
  ELSE
    RETURN    
  ENDIF

ENDIF

!-------------------------------------------------------------------------
! Conto i campi

DO k = 1,chead
  READ (iun,'(a1)',ADVANCE="NO",IOSTAT=ios) ch1
  IF (ios /= 0) GOTO 9996
ENDDO

DO k = 1,mxpar
  READ (iun,'(1x,a10)',ADVANCE="NO",IOSTAT=ios) ch10
  IF (ios == eor) EXIT
  IF (ios /= 0) GOTO 9996
ENDDO

nf = k -1
CLOSE (iun)

RETURN

!-------------------------------------------------------------------------
! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine inq_file_smr"
RETURN

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file)
RETURN

9997 CONTINUE
WRITE (*,*) "Errore leggendo headers ",TRIM(file)
RETURN

9996 CONTINUE
WRITE (*,*) "Errore contando i campi ",TRIM(file)
RETURN

END SUBROUTINE inq_file_smr

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_rec_smr(iu,fmt,nf,data,hh,sca,val,ios)
!-------------------------------------------------------------------------
! Legge un record dati da un file SMR gia' aperto
!-------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: iu,fmt,nf
!
TYPE(date), INTENT(OUT) :: data
INTEGER, INTENT(OUT) :: hh,sca,ios
REAL, INTENT(OUT) :: val(nf)
!
CHARACTER(LEN=80) :: chfmt
!-------------------------------------------------------------------------

SELECT CASE (fmt)

CASE (1)                                                  ! seriet xls
  WRITE (chfmt,'(a,i3,a)') "(2(i2,1x),i4,1x,i2,1x,i3,",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%dd,data%mm,data%yy,hh,sca,val(1:nf)

CASE (2)                                                  ! seriet for

CASE (3)                                                  ! estra_orari
  WRITE (chfmt,'(a,i3,a)') "(i4,3(1x,i2),",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%yy,data%mm,data%dd,hh,val(1:nf)
  sca = 0

CASE (4)                                                  ! trasp_temp
  WRITE (chfmt,'(a,i3,a)') "(i4,3(1x,i2),6x,",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%yy,data%mm,data%dd,hh,val(1:nf)
  sca = 0
END SELECT

RETURN
END SUBROUTINE read_rec_smr

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_rec_smr_scalar(iu,fmt,nf,data,hh,sca,val,ios)
!-------------------------------------------------------------------------
! Legge un record dati da un file SMR gia' aperto
!-------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: iu,fmt,nf
!
TYPE(date), INTENT(OUT) :: data
INTEGER, INTENT(OUT) :: hh,sca,ios
REAL, INTENT(OUT) :: val
!
CHARACTER(LEN=80) :: chfmt
!-------------------------------------------------------------------------

SELECT CASE (fmt)

CASE (1)                                                  ! seriet xls
  WRITE (chfmt,'(a,i3,a)') "(2(i2,1x),i4,1x,i2,1x,i3,",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%dd,data%mm,data%yy,hh,sca,val

CASE (2)                                                  ! seriet for

CASE (3)                                                  ! estra_orari
  WRITE (chfmt,'(a,i3,a)') "(i4,3(1x,i2),",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%yy,data%mm,data%dd,hh,val
  sca = 0

CASE (4)                                                  ! trasp_temp
  WRITE (chfmt,'(a,i3,a)') "(i4,3(1x,i2),6x,",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%yy,data%mm,data%dd,hh,val
  sca = 0
END SELECT

RETURN
END SUBROUTINE read_rec_smr_scalar

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

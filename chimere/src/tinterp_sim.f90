PROGRAM tinterp_sim
!--------------------------------------------------------------------------
! Interpola nel tempo un file intermedio Chimere (un parametro), producendo
! un file con scadenze orarie.
!
! Uso: tinterp_sim.exe filein fileout np nz [-cum] [-h]
!
!                                         Versione 1.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

TYPE(date) :: data1,data2,datac
INTEGER :: np,nz,lcum,idata,hh1,hh2,hhc,hht,dt
INTEGER :: idp,kp,kz,kh,ios,ios1,ios2,eof,eor
REAL, ALLOCATABLE :: x1(:,:),x2(:,:),x(:,:)
REAL :: w1,w2,rdum
CHARACTER (LEN=200) :: filein,fileout,arg(4),chdum
CHARACTER (LEN=10) :: ch10

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

! Parametri da riga comando

! default
lcum = 0

idp = 0
DO kp = 1,5
  CALL getarg(kp,chdum)

  IF (chdum == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-cum") THEN
    lcum = 1
    CYCLE
  ENDIF

  idp = idp + 1
  arg(idp) = chdum

ENDDO

filein = arg(1)
fileout = arg(2)
READ (arg(3),*,IOSTAT=ios1) np
READ (arg(4),*,IOSTAT=ios2) nz

IF (ios1 /= 0 .OR. ios2 /= 0 .OR. filein == "" .OR. fileout == "") THEN
  CALL write_help
  STOP
ENDIF

! Alloco variabili
ALLOCATE (x(np,nz),x1(np,nz),x2(np,nz))

! Verifico che il file di input contenga il numero giusto di dati
OPEN (UNIT=20, FILE=filein, STATUS="OLD", FORM="UNFORMATTED", ERR=9999)
READ (20,IOSTAT=ios) idata,((x2(kp,kz),kp=1,np),kz=1,nz)
IF (ios /= 0) GOTO 9998
CLOSE (20)

OPEN (UNIT=20, FILE=filein, STATUS="OLD", FORM="UNFORMATTED", ERR=9999)
READ (20,IOSTAT=ios) idata,((x2(kp,kz),kp=1,np),kz=1,nz),rdum
IF (ios == 0) WRITE (*,*) "Warning, il file di input contiene altri dati!"
CLOSE (20)

! Apro i files
OPEN (UNIT=20, FILE=filein, STATUS="OLD", FORM="UNFORMATTED", ERR=9999)
OPEN (UNIT=30, FILE=fileout, STATUS="REPLACE", FORM="UNFORMATTED")

CALL get_eof_eor(eof, eor)
WRITE (*,*)
WRITE (*,*) "tinterp_sim: elaboro ",TRIM(filein)
 
!--------------------------------------------------------------------------
! 2) Lettura, calcolo, scrittura (ciclo sulle scadenze)

! 2.1) Leggo la prima scadenza in input 
READ (20,IOSTAT=ios) idata,((x1(kp,kz),kp=1,np),kz=1,nz)
IF (ios /= 0) GOTO 9998
WRITE (ch10,'(i10.10)') idata
READ (ch10,'(i4,3i2)',ERR=9997) data1%yy,data1%mm,data1%dd,hh1

! 2.2) ciclo sulle scadenze in input (leggo scadenze t1 e t2, scrivo le ore 
!      comprese tra t1 e t2-1)
DO

  READ (20,IOSTAT=ios) idata,((x2(kp,kz),kp=1,np),kz=1,nz)
  IF (ios == eof) EXIT  
  IF (ios /= 0) GOTO 9998  

  WRITE (ch10,'(i10.10)') idata
  READ (ch10,'(i4,3i2)',ERR=9997) data2%yy,data2%mm,data2%dd,hh2
  IF (data2 < data1 .OR. (data2 == data1 .AND. hh2 <= hh1)) GOTO 9996

  dt = (data2-data1) * 24 + (hh2-hh1)
  WRITE (*,'(a,i2,2(a,i4.4,3i2.2))') "Elaboro ",dt," istanti: da ", &
    data1%yy,data1%mm,data1%dd,hh1," a ",data2%yy,data2%mm,data2%dd,hh2

! Ciclo sulle ore comprese tra due scadenze del file di input
  DO kh = 0,dt-1
    hht = hh1 + kh
    datac = data1 + hht/24
    hhc = MOD(hht,24)
    idata = hhc + datac%dd*100 + datac%mm*10000 + datac%yy*1000000

    IF (lcum == 0) THEN
      w2 = REAL(kh) / REAL(dt)
      w1 = 1. - w2
    ELSE
      w2 = 1. / REAL(dt)
      w1 = 0. 
    ENDIF

    x(:,:) = w1 * x1(:,:) + w2 * x2(:,:)
    WRITE (30) idata,((x(kp,kz),kp=1,np),kz=1,nz)
!   print *,idata,w1,w2

  ENDDO

  x1(:,:) = x2(:,:)
  data1 = data2
  hh1 = hh2
ENDDO

! 2.3) Scrivo l'ultima scadenza
WRITE (*,'(a,i4.4,3i2.2)') "Elaboro istante finale: ", &
  data1%yy,data1%mm,data1%dd,hh1

IF (lcum == 0) THEN
  w1 = 1.
ELSE
  w1 = 1. / REAL(dt)
ENDIF
idata = hh1 + data1%dd*100 + data1%mm*10000 + data1%yy*1000000
x(:,:) = w1 * x1(:,:)
WRITE (30) idata,((x(kp,kz),kp=1,np),kz=1,nz)

STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP

9997 CONTINUE
WRITE (*,*) "idata in input illegale: ",idata
STOP

9996 CONTINUE
WRITE (*,*) "Date non crescono in ",TRIM(filein)
STOP

END PROGRAM tinterp_sim

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
WRITE (*,*) "Uso: tinterp_sim.exe filein fileout np nz [-cum] [-h]"
WRITE (*,*) "  con l'opzione -cum spalama i dati sulle ore precedenti, altrimenti"
WRITE (*,*) "  interpola linearmente"

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

program altipres_sim
!--------------------------------------------------------------------------
! Dati i campi di T3d e PS, calcola Z su un insieme di livelli di cui e'
! nota la pressione.
!
! Uso: altipres_sim.exe [-h] np nz
!
! Input (files intermedi Chimere; gia' interpolati in X,Y,t; unformatted)
! TEMP_3D: temperatura (K)
! SURP_2D: pressione superifciale (Pa)
! PRES_3D: pressione sui livelli richiesti (Pa)
!
! Output:
! ALTI_3D: altezza dalla superficie dei livelli richiesti (m)
!
!                                         Versione 1.0.1, Enrico 26/09/2012
!--------------------------------------------------------------------------
IMPLICIT NONE

INTEGER :: np,nz,nt
INTEGER :: iarg(2),ios(3),eof,eor,kp,kz,k,idata,idata2,idata3
REAL, ALLOCATABLE :: t(:,:),ps(:),p(:,:),z(:,:)
REAL :: pl0,pl1,alt,tm
CHARACTER (LEN=80) :: filet,filep,fileps,filez,chdum

REAL, PARAMETER :: rsurg = 29.27

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

! Nomi files I/O
filet = "TEMP_3D"
fileps = "SURP_2D"
filep = "PRES_3D"
filez = "ALTI_3D"
ios=0
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
ALLOCATE (t(np,nz),ps(np),p(np,nz),z(np,nz))

! Apro i files
OPEN (UNIT=20, FILE=filet, STATUS="OLD", FORM="UNFORMATTED", ERR=9999)
OPEN (UNIT=21, FILE=fileps, STATUS="OLD", FORM="UNFORMATTED", ERR=9998)
OPEN (UNIT=22, FILE=filep, STATUS="OLD", FORM="UNFORMATTED", ERR=9997)
OPEN (UNIT=30, FILE=filez, STATUS="REPLACE", FORM="UNFORMATTED")

CALL get_eof_eor(eof, eor)
 
!--------------------------------------------------------------------------
! 2) Lettura, calcolo, scrittura (ciclo sulle scadenze)

DO k = 1,HUGE(k)

! Leggo
  READ (20,IOSTAT=ios(1)) idata,((t(kp,kz),kp=1,np),kz=1,nz)
  READ (21,IOSTAT=ios(2)) idata2,(ps(kp),kp=1,np)
  READ (22,IOSTAT=ios(3)) idata3,((p(kp,kz),kp=1,np),kz=1,nz)
  IF (ios(1) == eof) EXIT  
  IF (ANY(ios(:) /= 0)) GOTO 9996  
  IF (idata /= idata2 .OR. idata /= idata3)  GOTO 9995
 
! Calcolo Z
  DO kp = 1,np 
    pl0 = LOG(ps(kp))
    pl1 = LOG(p(kp,1))
    alt = rsurg * t(kp,1) * (pl0 - pl1)
    z(kp,1) = alt

    DO kz = 2,nz
      pl0 = pl1
      pl1 = LOG(p(kp,kz))
      tm = 0.5*(t(kp,kz-1) + t(kp,kz))
      alt = alt + rsurg * tm * (pl0 - pl1)
      z(kp,kz) = alt
    ENDDO

  ENDDO

! Scrivo
  WRITE (30) idata,((z(kp,kz),kp=1,np),kz=1,nz)

ENDDO
nt = k-1

WRITE (*,'(a,3i6)') "Calcolata altezza livelli. np,nz,nt: ",np,nz,nt

STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filet)
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileps)
STOP

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filep)
STOP

9996 CONTINUE
WRITE (*,*) "Errore lettura files input ",ios(:)
STOP

9995 CONTINUE
WRITE (*,*) "Date disallineate in files input: ",idata,idata2,idata3
STOP


END PROGRAM altipres_sim

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
WRITE (*,*) "Uso: altipres_sim.exe [-h] np nz"

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

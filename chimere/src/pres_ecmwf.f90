PROGRAM pres_ecmwf
!--------------------------------------------------------------------------
! Dato il campo di PS e un grib da cui leggere i "vertical coordinate 
! parameters", calcola P sui primi nz model levels di ECMWF
!
! Uso: pres_ecmwf.exe [-h] np nz filegrib
!
! Input 
! SURP_2D:  pressione superifciale (Pa; file intermedio Chimere; gia' 
!           interpolato in orizzontale e nel tempo; unformatted)
! filegrib: usa solo per leggere psec2 dal primo grib
!
! Output:
! PRES_3D:  pressione sui livelli richiesti (Pa)
!
!                                         Versione 1.0.2, Enrico 13/01/2014
!--------------------------------------------------------------------------
IMPLICIT NONE

! Dichiarazioni per GRIBEX.
INTEGER, PARAMETER :: maxdim = 100000  ! dimensione massima dei GRIB
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
REAL, ALLOCATABLE :: ps(:),p(:,:)
REAL :: ca(256),cb(256)
INTEGER :: np,nz,nt,nzecm,idata
INTEGER :: ios1,ios2,ios,eof,eor,idp,iu,kp,kz,k
CHARACTER (LEN=200) :: filep,fileps,filegrib,chdum,arg(3)

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

! Nomi files I/O
fileps = "SURP_2D"
filep = "PRES_3D"

! Parametri da riga comando
DO kp = 1,3
  CALL getarg(kp,chdum)

  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ENDIF

  idp = idp + 1
  arg(idp) = chdum

ENDDO

READ (arg(1),*,IOSTAT=ios1) np
READ (arg(2),*,IOSTAT=ios2) nz
filegrib = arg(3)

IF (ios1 /= 0 .OR. ios2 /= 0 .OR. filegrib == "") THEN
  CALL write_help
  STOP
ENDIF

! Alloco variabili
ALLOCATE (ps(np),p(np,nz))

! Apro i files unformatted
OPEN (UNIT=20, FILE=fileps, STATUS="OLD", FORM="UNFORMATTED", ERR=9999)
OPEN (UNIT=30, FILE=filep, STATUS="REPLACE", FORM="UNFORMATTED")

! Trovo codice per EOF
CALL get_eof_eor(eof, eor)

!--------------------------------------------------------------------------
! 2) Leggo psec2 da filegrib

CALL grsvck(0)

CALL PBOPEN (iu,filegrib,'R',kret)
IF (kret /= 0) GOTO 9997 

CALL PBGRIB(iu,kbuffer,maxdim*4,klen,kret)
IF (kret /= 0) GOTO 9996

CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             field,maxdim,kbuffer,maxdim,klen,'D',kret)
IF (kret > 0) GOTO 9996

CALL PBCLOSE(iu)

! Calcolo i coefficenti ca e cb sui livelli richiesti (primi nz layers, 
!   contati dal basso)
nzecm = ksec2(12)/2

IF (MOD(ksec2(12),2) /= 0) THEN
  WRITE (*,*) "Errore, vert.coord.param. dispari: ",ksec2(12)
  STOP
ELSE IF (nz >= nzecm) THEN
  WRITE (*,*) "Errore, richiesti piu' livelli di quelli disponibili"
  STOP
ELSE
  WRITE (*,*) "pres_ecmwf.exe. N.ro livelli ECMWF originali: ",nzecm
ENDIF

DO kz = 1,nz
  ca(kz) = (psec2(nzecm-kz+10) + psec2(nzecm-kz+11)) * 0.5
  cb(kz) = (psec2(2*nzecm-kz+10) + psec2(2*nzecm-kz+11)) * 0.5
ENDDO

!--------------------------------------------------------------------------
! 3) Lettura, calcolo, scrittura (ciclo sulle scadenze)

DO k = 1,HUGE(k)

! Leggo Ps
  READ (20,IOSTAT=ios) idata,(ps(kp),kp=1,np)
  IF (ios == eof) EXIT  
  IF (ios /= 0) GOTO 9998  
 
! Calcolo P
  DO kz = 1,nz
    p(1:np,kz) = ca(kz) + cb(kz)*ps(1:np)
  ENDDO

! Scrivo P3d
  WRITE (30) idata,((p(kp,kz),kp=1,np),kz=1,nz)

ENDDO
nt = k-1
WRITE (*,'(a,3i6)') "Calcolata pressione 3d. np,nz,nt: ",np,nz,nt

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileps)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(fileps)
STOP

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filegrib)
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filegrib)
STOP

END PROGRAM pres_ecmwf

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

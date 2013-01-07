PROGRAM crea_surf_dat
!--------------------------------------------------------------------------
!  Programma della catena Calmet.
!  Legge le osservazioni al suolo preparate dai programmi proc_st1*.exe
!  (files *.st2) e scrive il surf.dat per il run di Calmet.
!  Legge l'elenco delle stazioni da file_lst e le date da filedate
!
!  Metodo:
!  - legge le date estreme del run da date_calmet.inp
!  - legge da srq_surf_PROJ.lst l'elenco delle stazioni da elaborare: un
!    file per stazione, in formato .st2. Questi contengono tutte e sole
!    le scadenze necessarie, con le unita' di misura richieste da Calmet.
!
!  Operazioni compiute:
!  - trasforma le date in formato calmet (gg giuliano, ore LST)
!  - costruisce gli headers
!
!  Note:
!  - se si volessero sostituire i codici utente con i codici Oracle: 
!    la sostituzione deve riguardare anche calmet.inp (programma 
!    crea_calmet_inp.f90), entrambi i codici dovrebbero leggere 
!    db_anagrafica.dat. Il codice Oracle e' contenuto anche in srq_surf.dat,
!    ma preferisco che questo file sia letto solo da estra_surf_calmet.f
!
!  - Tracciato files .st2 (come surf.dat per una sola stazione): 
!    1 record header, poi (6iN, 8f8.1): staz,net,anno,mese,giorno,ora,
!     ff, dd, ceil,  tcc, tt, rh, prs,cod_prc 
!    [m/s grd ft*100 10i  K   %   mb  Calmet ]
!
!                         V1, Enrico 01/01/2003, da cut_tosurf_sim.f (Manu)
!--------------------------------------------------------------------------
USE date_handler

IMPLICIT NONE

! Time zone (in futuro potrebbe essere inclusa in pre_calmet.inp)
INTEGER, PARAMETER :: ibtz = -1

! Files costanti
CHARACTER(LEN=80), PARAMETER :: file_lst  = "surf_req.dat"
CHARACTER(LEN=80), PARAMETER :: file_out  = "surf.dat"

! Dati mancanti
REAL, PARAMETER :: rmis = 9999.  ! non modificare (input, calmet)

! Dati di input/output; allocato (nstaz,8,nhr)
REAL, ALLOCATABLE :: par(:,:,:)
INTEGER, ALLOCATABLE :: id_out(:)  ! codici stazione scritti in surf.dat
 
! Altre variabili del programma
TYPE(date) :: data1,data2,data_cal1,data_cal2,data_req,data_fil,data_out
REAL :: rdum(8)
INTEGER :: ibyr,ibjul,ibhr,ieyr,iejul,iehr
INTEGER :: k,kstaz,khr,kpar,ios,ios2,eof,eor,idum,idum2
INTEGER :: hr1,hr2,hr_req,hr_fil,hr_out,jul_out,yea_out,dh1,dh2
INTEGER :: id_rete,id_user,ndays,nhr,nstaz
CHARACTER (LEN=200) :: chrec
CHARACTER(LEN=80) :: filein,chpar1,chpar2,filedate,filesurf

!--------------------------------------------------------------------------
! 1: Preliminari

! 1.0 IOSTAT per eof,eor
CALL get_eof_eor(eof,eor)

! 1.1 Parametri da riga comandi
CALL getarg(1,chpar1)
CALL getarg(2,chpar2)

IF (TRIM(chpar1)=="" .OR. TRIM(chpar2)=="" .OR. TRIM(chpar1)=="-h" ) THEN
  WRITE (*,*) "Uso: crea_surf_dat.exe filedate filesurf"
  STOP
ENDIF

filedate = TRIM(chpar1)
filesurf = TRIM(chpar2)

! 1.2 Leggo da filedate le date richieste
OPEN (UNIT=30, FILE=filedate, STATUS="OLD", ACTION="READ",ERR=9999)
READ (30,*,ERR=9999)
READ (30,*,ERR=9999)
READ (30,*,ERR=9999)
READ (30,'(i4,3i2)',ERR=9999) data1%yy,data1%mm,data1%dd,hr1
READ (30,'(i4,3i2)',ERR=9999) data2%yy,data2%mm,data2%dd,hr2
CLOSE(30)

nhr = 24 * (data2-data1) + hr2 - hr1 + 1

! 1.3 Leggo filesurf per contare le stazioni richieste
OPEN (UNIT=31, FILE=filesurf, STATUS="OLD", ACTION="READ",ERR=9998)
READ (31,*,ERR=9998)

nstaz = 0
DO 
  READ (31,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT
  IF (chrec == "") CYCLE

  READ (chrec,'(i2,i5)',IOSTAT=ios) id_rete,id_user
  IF (ios /= 0) THEN
    WRITE (*,*) "record illegale in filesurf, skippo: ",TRIM(chrec)
    CYCLE
  ENDIF

  nstaz = nstaz + 1
ENDDO
CLOSE(31)

! 1.4 Alloco arrays per i dati e per i codici stazione
ALLOCATE (par(nstaz,8,nhr), id_out(nstaz))
par(:,:,:) = rmis

WRITE (*,'(a,i3,a,i3,a)') "crea_surf_dat: richieste ", &
  nstaz," stazioni e",nhr," scad."

!--------------------------------------------------------------------------
! 2: Leggo tutti i report e travaso i dati sull'array di lavoro

OPEN (UNIT=31, FILE=filesurf, STATUS="OLD", ACTION="READ",ERR=9998)
READ (31,*,ERR=9998)

kstaz = 0
staz: DO

! Leggo da filesurf il codice della stazione corrente
  READ (31,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT
  IF (chrec == "") CYCLE

  READ (chrec,'(i2,i5)',IOSTAT=ios) id_rete,id_user
  IF (ios /= 0) CYCLE

  kstaz = kstaz + 1
  id_out(kstaz) = id_user

! Apro il file .st2 corrispondente
  WRITE (filein,'(i3.3,a1,i5.5,a4)') id_rete,"_",id_user,".st2"
  OPEN (UNIT=32, FILE=filein, STATUS="OLD", ACTION="READ",IOSTAT=ios)
  READ (32,*,IOSTAT=ios2)
  IF (ios /= 0 .OR. ios2 /= 0) THEN
    WRITE (*,'(3a)') "Errore aprendo ",TRIM(filein)," skippo la stazione"
    CYCLE staz
  ENDIF

! Leggo i report orari
  DO khr = 1,nhr
!   Data/ora che mi aspetto di leggere
    ndays = (hr1 + khr - 1) / 24
    data_req = data1 + ndays
    hr_req = MOD(hr1 + khr - 1, 24)

!    READ (32,'(i5.5,1x,i3.3,1x,i4.4,3(1x,i2.2),8f8.1)',IOSTAT=ios) &
    READ (32,*,IOSTAT=ios) &
      idum,idum2,data_fil%yy,data_fil%mm,data_fil%dd,hr_fil,rdum(1:8)

!   In caso di errore, passo alla stazione successiva (verranno scritti 
!   tutti dati mancanti)
    IF (idum /= id_user .OR. idum2 /= id_rete .OR. ios /= 0 .OR. &
      data_fil /= data_req .OR. hr_fil /= hr_req) THEN
      WRITE (*,'(3a,i3,a)') "Errore o disallineamento in ", &
        TRIM(filein)," scad. ",khr,", skippo i dati successivi"
      CYCLE staz
    ELSE
      par(kstaz,:,khr) = rdum(:)
    ENDIF

  ENDDO                                              ! scadenze (ore)
  CLOSE(32)

ENDDO staz                                           ! stazioni

WRITE (*,'(a)') "  letti i dati"

!--------------------------------------------------------------------------
! 3: Scrivo output

OPEN (UNIT=40, FILE=file_out, STATUS="REPLACE", ACTION="WRITE")

! Calcolo le date estreme del run in ore LST (i.e. GMT - ibtz)
dh1 = hr1 - ibtz
IF (dh1 < 0 .AND. dh1 >= -24) THEN
  ibhr = dh1 + 24
  data_cal1 = data1 - 1
ELSE IF (dh1 >= 0 .AND. dh1 < 24) THEN
  ibhr = dh1
  data_cal1 = data1
ELSE IF (dh1 >= 24 .AND. dh1 < 48) THEN
  ibhr = dh1 - 24
  data_cal1 = data1 + 1
ELSE
  WRITE (*,*) "ibtz illegale: ",ibtz
  STOP
ENDIF

dh2 = hr2 - ibtz
IF (dh2 < 0 .AND. dh2 >= -24) THEN
  iehr = dh2 + 24
  data_cal2 = data2 - 1
ELSE IF (dh2 >= 0 .AND. dh2 < 24) THEN
  iehr = dh2
  data_cal2 = data2
ELSE IF (dh2 >= 24 .AND. dh2 < 48) THEN
  iehr = dh2 - 24
  data_cal2 = data2 + 1
ELSE
  WRITE (*,*) "ibtz illegale: ",ibtz
  STOP
ENDIF

! Scrivo header records
ibyr = MOD(data_cal1%yy, 100)
ibjul = jul(data_cal1)
ieyr = MOD(data_cal2%yy, 100)
iejul = jul(data_cal2)

WRITE (40,'(2(i2.2,1x,i3.3,1x,i2.2,5x),i3,5x,i5)') &
  ibyr,ibjul,ibhr,ieyr,iejul,iehr,ibtz,nstaz
WRITE (40,'(10(i5.5,1x))') id_out(1:nstaz)

! Scrivo i dati
DO khr = 1,nhr

  ndays = (ibhr + khr - 1) / 24
  data_out = data_cal1 + ndays
  hr_out = MOD(ibhr + khr - 1, 24)
  yea_out = MOD(data_out%yy,100)
  jul_out = jul(data_out)

  WRITE (40,'(i2.2,1x,i3.3,1x,i2.2)') yea_out,jul_out,hr_out
  DO kstaz = 1,nstaz 
    WRITE (40,'(2(f7.1,1x),2(i7,1x),f7.1,1x,i7,1x,f7.1,1x,i7)') &
      par(kstaz,1,khr),par(kstaz,2,khr),NINT(par(kstaz,3,khr)), &
      NINT(par(kstaz,4,khr)),par(kstaz,5,khr),NINT(par(kstaz,6,khr)), &
      par(kstaz,7,khr),NINT(par(kstaz,8,khr))
  ENDDO

ENDDO

CLOSE(40)
WRITE (*,'(2a)') "  scritto ",TRIM(file_out)

STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filedate)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_lst)
STOP

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

END PROGRAM crea_surf_dat

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

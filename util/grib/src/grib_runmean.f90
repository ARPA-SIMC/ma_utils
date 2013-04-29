PROGRAM grib_runmean
!--------------------------------------------------------------------------
! Legge un file GRIB con istanti orari e ne scrive uno che, per ciascun 
! istante, contiene la media mobile su un certo intervallo.
!
! Il file in input deve:
! - contenere al massimo un campo per ogni verification time
! - contenere campi con verification time crescente (ammessi i buchi)
! - contenere campi definiti sulla stesa griglia
!
! Note:
! - Programma usato dalla catena Chimere
! - Il programma scrive in output un campo per ogni ora compresa tra il
!   primo e l'ultimo verification time in input, calcolando le medie sugli
!   intervalli che terminano in ciascuno di questi.
! - I dati in input non istantanei sono considerati istantanei all'istante
!   finale del periodo di elaborazione.
! - I dati in output hanno timerange "analisi istantantea"; se viene 
!   specificata la flag -forc e i dati di input hanno tutti lo stesso 
!   reftime, il timerange e' "previsione istantanea"
! - In linea di principio, sarebbe possibile scrivere i timerange giusti
!   (ie. "media tre le .. e le .."), ma e' molto complesso, soprattutto se
!   i dati in input non sono istantanei. Bisognerebbe gestire tutte le 
!   combinazioni di grib1/grib2, analisi/forecast, istantanei/elaborati, 
!   inizio/fine intervallo ...
! - Per avere la massima flessibilita', il programma non controlla se 
!   parametro, livello etc cambiano all'interno di un file
! - Il programma usa la logica grib-api (tiene in memoria  solo l'handler
!   dei grib che elabora), e potrebbe quindi essere molto lento. 
!   Eventualmente si potrebbero tenere in memoria i campi corripondenti al 
!   vettore igin (in modo da decodificare i grib una volta sola)
!
!                                                 V3.0.3, Enrico 17/04/2013
!--------------------------------------------------------------------------

USE grib_api
USE datetime_class
USE grib2_utilities
USE missing_values

IMPLICIT NONE

!==========================================================================
! 0) Dichiarazioni

INTEGER, PARAMETER :: igmiss = 0              ! Handler di un grib mancante

! Variabili locali
REAL, ALLOCATABLE :: values(:,:),mean(:),val_deb(:)
INTEGER, ALLOCATABLE :: igin(:),nok(:)
INTEGER :: ifin,ifout,ig_read,ig_first,ig_write
INTEGER :: nhr,nreq,hrout,nhincr,nx,ny,np,ibm,en,fstep,kdeb
INTEGER :: ios(3),cnt_par,cnt_out,kpar,kg,ksk,iret,ier,idata,ihr,clret(0:5)
INTEGER :: gnov,nom,nocv
CHARACTER(LEN=80) :: filein,fileout,chpar
CHARACTER (LEN=12) :: ch12,ch12b
CHARACTER (LEN=10) :: ch10
CHARACTER (LEN=3) :: next_arg
LOGICAL :: rq_forc,ldeb

TYPE(datetime) :: vtime_read,vtime_first,vtime_curr,vtime_out
TYPE(datetime) :: rtime_read,rtime_first

!==========================================================================
! 1) Preliminari

ldeb = .FALSE.
kdeb = 10095

! 1.1 Parametri da riga comando
next_arg = ""
cnt_par = 0
ios(:) = 0
nreq = -999
hrout = -HUGE(0)
rq_forc = .FALSE.

DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(chpar) == "-forc") THEN
    rq_forc = .TRUE.
  ELSE IF (TRIM(chpar) == "-nval") THEN
    next_arg = "nvl"
  ELSE IF (TRIM(chpar) == "-istout") THEN
    next_arg = "ist"
  ELSE IF (next_arg == "nvl") THEN
    READ(chpar,*,IOSTAT=ios(2)) nreq
    next_arg = ""
  ELSE IF (next_arg == "ist") THEN
    READ(chpar,*,IOSTAT=ios(3)) hrout
    next_arg = ""
    
  ELSE IF (cnt_par == 0) THEN
    cnt_par = 1
    filein = chpar
  ELSE IF (cnt_par == 1) THEN
    cnt_par = 2
    fileout = chpar
  ELSE IF (cnt_par == 2) THEN
    cnt_par = 3
    READ(chpar,*,IOSTAT=ios(1)) nhr
  ENDIF
ENDDO

! 1.2 Controlli e operazioni dipendenti dai parametri
IF (ANY(ios(:) /= 0) .OR. nhr < 1) THEN
    CALL scrive_help
    STOP
ENDIF  

IF (nreq == -999) THEN
  nreq = nhr
ELSE IF (nreq < 1) THEN
  nreq = 1
ENDIF

IF (hrout == -HUGE(0)) hrout = nhr
ALLOCATE (igin(nhr))

! 1.3 Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w",iret)

!==========================================================================
! 2) Elaborazioni (ciclo sui grib in input)
!
! vtime_curr : istante finale dell'intervallo di nhr ore che sto elaborando
! vtime_read : istante di validita' dell'ultimo grib letto
! igin(1:nhr) : contiene gli handler del campo all'istante corrente e alle 
!               nhr-1 ore precedenti. Gli handler corrispondenti a istanti 
!               non presenti in filein sono settati a igmiss
! values(np,nhr) : valori dei grib relativi agli handler igin

cnt_out = 0
DO kg = 1,HUGE(0)

!--------------------------------------------------------------------------
! 2.1 Preliminari. Questa sezione ritorna: 
!   ig_read   : handler dell'ultimo grib letto
!   vtime_read: istante letto
!   vtime_curr: ultimo istante elaborato
!   nhincr    : ore tra l'ultimo ist. letto e il precedente ist. elaborato

! 2.1.1 Leggo il prossimo grib, calcolo verification time, set missing values
  CALL grib_new_from_file(ifin,ig_read,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

  CALL get_grib_time(ig_read, RTIME=rtime_read, VTIME=vtime_read, IRET=ier)

  CALL grib_get(ig_read,"bitmapPresent",ibm)

! 2.1.2 Primo grib
  IF (kg == 1) THEN

!   Trovo numero di punti, alloco il buffer dei dati, salvo date e handler
    CALL grib_get(ig_read,"numberOfPointsAlongAParallel",nx)
    CALL grib_get(ig_read,"numberOfPointsAlongAMeridian",ny)
    np = nx * ny
    ALLOCATE (values(np,nhr),mean(np),nok(np))
    IF (ldeb) ALLOCATE(val_deb(np))

    ig_first = ig_read
    rtime_first = rtime_read
    vtime_first = vtime_read

!   Inizializzo buffer di handler e dati, contatori, etc.
    igin(:) = igmiss
    values(:,:) = rmiss
    vtime_curr = vtime_read - timedelta_new(HOUR=1)
    nhincr = 1

! 2.2.3 Altro grib: 
  ELSE 

    ! Controllo che l'area non sia cambiata 
    CALL check_consistency(ig_read,ig_first, &
       .TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE., &
       .FALSE.,clret,ier)
    IF (clret(1) /= 0) GOTO 9997

    ! Se sono richiesti in ouput timerange previsti, controllo che il 
    !   reference time non sia cambiato 
    IF (rq_forc .AND. rtime_read /= rtime_first) GOTO 9995

    ! Conto gli eventuali istanti mancanti
    CALL getval(vtime_read - vtime_curr, AHOUR=nhincr)
    IF (nhincr < 1) GOTO 9996

  ENDIF

  IF (ldeb) THEN
    CALL getval(vtime_read, SIMPLEDATE=ch12)
    CALL grib_get(ig_read,"values",val_deb(1:np))
    WRITE (*,'(a,i4,3a,f12.5)') "kg ",kg," vtime_read ",ch12, &
      " valore ",val_deb(kdeb)
  ENDIF
 
!--------------------------------------------------------------------------
! 2.2 Calcolo le medie e scrivo.
! - Il ciclo su ksk e' un ciclo sui verification times in output;
! - vtime_curr e' l'istante finale del periodo di media che sto elabroando;
! - nhr e' la lunghezza dei periodi di media
! - hrout e' l'ora all'interno del periodo di media a cui attribuire il 
!   dato (tra 1 e nhr)

  DO ksk = 1,nhincr

!   2.2.1 Aggiorno data corrente e buffers
    vtime_curr = vtime_curr + timedelta_new(HOUR=1) 
    igin(1:nhr-1) = igin(2:nhr)
    values(1:np,1:nhr-1) = values(1:np,2:nhr)
    IF (ksk == nhincr) THEN
      igin(nhr) = ig_read

      CALL grib_get(ig_read,"getNumberOfValues",gnov)    ! totale di punti nel grib
      CALL grib_get(ig_read,"numberOfMissing",nom)       ! n.ro dati mancanti
      CALL grib_get(ig_read,"numberOfCodedValues",nocv)  ! n.ro dati validi
      IF (nocv == 0) THEN
        values(1:np,nhr) = rmiss
      ELSE
        CALL grib_set(ig_read,"missingValue",rmiss)
        CALL grib_get(ig_read,"values",values(1:np,nhr))
      ENDIF
      IF (nom + nocv /= gnov .OR. &
        (nocv /= 0 .AND. nocv /= COUNT(values(1:np,nhr) /= rmiss))) GOTO 9994

    ELSE
      igin(nhr) = igmiss
      values(1:np,nhr) = rmiss

    ENDIF

!   2.2.2 Calcolo la media, mascherando i punti che non hanno abbastanza dati
    nok(1:np) = COUNT(values(1:np,1:nhr) /= rmiss, DIM=2)
    WHERE (nok(1:np) >= nreq)
      mean(1:np) = SUM(values(1:np,1:nhr), DIM=2, MASK=values(1:np,1:nhr) /= rmiss) / REAL(nok(1:np))
    ELSEWHERE
      mean(1:np) = rmiss
    ENDWHERE

!   2.2.3 Codifico il nuovo grib e lo scrivo sul file di output
    vtime_out = vtime_curr - timedelta_new(HOUR = nhr-hrout)

    IF (ldeb) THEN
      CALL getval(vtime_curr, SIMPLEDATE=ch12)
      CALL getval(vtime_out, SIMPLEDATE=ch12b)
      WRITE (*,'(5a,i6,a,f12.5,a,i6)') "  VT fine periodo media: ",ch12,"   VT output: ", &
        ch12b,"    nok ",COUNT(mean(1:np) /= rmiss)," valore ",mean(kdeb)," dati usati ",nok(kdeb)
    ENDIF

    CALL grib_clone(ig_read,ig_write)
    CALL grib_get(ig_read,"editionNumber",en)

!   Output con timerange = forecast istantaneo (con grib1, le keys startStep 
!      e endStep apparenemtne non funzionano...
    IF (rq_forc) THEN 
      CALL getval(rtime_first,SIMPLEDATE=ch12)
      READ (ch12(1:8),*) idata
      READ (ch12(9:10),*) ihr
      CALL grib_set(ig_write,"dataDate",idata)
      CALL grib_set(ig_write,"dataTime",ihr*100)

      CALL getval(vtime_out - rtime_first, AHOUR=fstep)
      IF (en == 1) THEN
        CALL grib_set(ig_write,"timeRangeIndicator",0)
        CALL grib_set(ig_write,"P1",fstep)
        CALL grib_set(ig_write,"P2",0)
      ELSE IF (en == 2) THEN
        CALL grib_set(ig_write,"significanceOfReferenceTime",1)
        CALL grib_set(ig_write,"typeOfProcessedData",1)
        CALL grib_set(ig_write,"productDefinitionTemplateNumber",0)
        CALL grib_set(ig_write,"typeOfGeneratingProcess",2)
        CALL grib_set(ig_write,"forecastTime",fstep)
      ENDIF

      CALL grib_set(ig_write,"bitmapPresent",1)
      CALL grib_set(ig_write,"missingValue",rmiss)
      CALL grib_set(ig_write,"values",mean(1:np))

!   Output con timerange = analisi istantanea
    ELSE
      CALL getval(vtime_out,SIMPLEDATE=ch12)
      READ (ch12(1:8),*) idata
      READ (ch12(9:10),*) ihr
      CALL grib_set(ig_write,"dataDate",idata)
      CALL grib_set(ig_write,"dataTime",ihr*100)

      IF (en == 1) THEN
        CALL grib_set(ig_write,"timeRangeIndicator",0)
        CALL grib_set(ig_write,"P1",0)
        CALL grib_set(ig_write,"P2",0)
      ELSE IF (en == 2) THEN
        CALL grib_set(ig_write,"significanceOfReferenceTime",0)
        CALL grib_set(ig_write,"typeOfProcessedData",0)
        CALL grib_set(ig_write,"productDefinitionTemplateNumber",0)
        CALL grib_set(ig_write,"typeOfGeneratingProcess",0)
        CALL grib_set(ig_write,"forecastTime",0)
      ENDIF

      CALL grib_set(ig_write,"bitmapPresent",1)
      CALL grib_set(ig_write,"missingValue",rmiss)
      CALL grib_set(ig_write,"values",mean(1:np))

    ENDIF

    CALL grib_write(ig_write,ifout)
    cnt_out = cnt_out + 1

  ENDDO

ENDDO

! 2.3 Chiudo il file
CALL grib_close_file(ifout,iret)
WRITE (*,*) "Grib letti ",kg-1," scritti ",cnt_out

STOP

!==========================================================================
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," campo ",kg
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore campo con griglia diversa ",kg
STOP 3

9996 CONTINUE
WRITE (*,*) "Errore: istanti non progressivi in ",TRIM(filein)
CALL getval(vtime_read,SIMPLEDATE=ch10)
WRITE (*,*) "Grib ",kg," verification time ",ch10
CALL getval(vtime_curr,SIMPLEDATE=ch10)
WRITE (*,*) "Data attesa: ",ch10
STOP 4

9995 CONTINUE
WRITE (*,*) "Torvati campi con reftime diversi: l'output con timerange previsti e' impossibile"
CALL getval(rtime_read,SIMPLEDATE=ch10)
WRITE (*,*) "Grib ",kg," reference time ",ch10
CALL getval(rtime_first,SIMPLEDATE=ch10)
WRITE (*,*) "Grib ",1," reference time ",ch10
STOP 5

9994 CONTINUE
WRITE (*,*) "Errore nelle chiavi realtive ai dati mancanti"
WRITE (*,*) "Campo ",kg
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(values(1:np,nhr) /= rmiss)
STOP 6

END PROGRAM grib_runmean

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo l'help del programma
!
IMPLICIT NONE

!                12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*)
WRITE (*,'(a)') "Uso: grib_runmean filein fileout nhr [-nval nreq] [-istout hrout] [-forc]"
WRITE (*,'(a)') "filein   file grib in input (stessa griglia, verif.time crescente e univoco)"
WRITE (*,'(a)') "fileout  file grib in output (timerange istantaneo)"
WRITE (*,'(a)') "nhr      numero di ore su cui calcolare la media mobile"
WRITE (*,'(a)') "nreq     n.ro minimo di dati per considerare valida una media mobile [def: tutti]"
WRITE (*,'(a)') "hrout    istante a cui viene attribuita la media mobile in output;"
WRITE (*,'(a)') "         tra 1 (=istante iniziale) e nhr (=istante finale; default)"
WRITE (*,'(a)') "-forc    scrive i dati in output come previsioni istantanee (def: analisi)"
WRITE (*,'(a)') "         i dati in input devono essere previsioni con lo stesso reference time"

RETURN
END SUBROUTINE scrive_help



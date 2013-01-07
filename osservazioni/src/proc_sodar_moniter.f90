PROGRAM proc_sodar_moniter
!--------------------------------------------------------------------------
! Elabora i dati Sodar della campagna Moniter:
! - legge il file .csv complessivo (prodotto da proc_sodar.ksh)
! - scrive un file orario in formato seriet e un file semiorario in
!   foramto GRADS
! 
! Uso: proc_sodar.exe filein
!--------------------------------------------------------------------------

USE datetime_class
IMPLICIT NONE

! Parametri relativi al file di input
INTEGER, PARAMETER :: nlev = 27        ! N.ro livelli input
REAL, PARAMETER :: hlev1 = 25.         ! Quota primo livello
REAL, PARAMETER :: delta_lev = 50.     ! Distanza tra 2 livelli (m)
INTEGER, PARAMETER :: step_t = 30      ! Distanza tra 2 istanti (min)

! Altri parametri costanti
REAL, PARAMETER :: rmis = -9999.       ! Codice dati mancanti
CHARACTER (LEN=80), PARAMETER :: fileser = "sodar_ser.txt"
CHARACTER (LEN=80), PARAMETER :: filegrads = "sodar.dat"
CHARACTER (LEN=80), PARAMETER :: filectl = "sodar.ctl"

! Dichiarazioni
TYPE(datetime) :: data1,datac,datap
TYPE(timedelta) :: deltatc,step_t2
INTEGER :: ios,eof,eor,k,kk,i,ii,kz
INTEGER :: year,month,day,hh,min,delta_min,delta_step,irec,nt
INTEGER :: out_ser,out_grads,out_ser_mis,out_grads_mis,expected_rec
REAL :: dd(nlev),ff(nlev),ww(nlev),zi
CHARACTER (LEN=400) :: chfmt
CHARACTER (LEN=80) :: filein
CHARACTER (LEN=16) :: ch16,ch16a,ch16b
CHARACTER (LEN=12) :: grads_date

!--------------------------------------------------------------------------
! Preliminari
CALL getarg(1,filein)

IF (TRIM(filein) == "-h" .OR. TRIM(filein) == "") THEN
  WRITE (*,*) "Uso: proc_sodar.exe filein"
  STOP
ENDIF

! Apro files
OPEN (UNIT=30, FILE=filein, STATUS="OLD", ACTION="READ", ERR=9999)
OPEN (UNIT=40, FILE=fileser, STATUS="REPLACE", FORM="FORMATTED")
OPEN (UNIT=41, FILE=filegrads, FORM='UNFORMATTED', STATUS="REPLACE", &
  ACCESS='DIRECT', RECL=4)
OPEN (UNIT=42, FILE=filectl, STATUS="REPLACE", FORM="FORMATTED")

! Codice EOF EOR
CALL get_eof_eor(eof,eor)

! Passo temporale file input
CALL init(step_t2, MINUTE=step_t)

! Scrivo header Seriet
WRITE (40,'(a)') "Sodar "
WRITE (40,*)

WRITE (chfmt,'(a,i3,a)') "(a17,",nlev*3+1,"(1x,a10))"
WRITE (40,chfmt) "Modello ->       ",("sodar_____", i=1,nlev*3+1)

WRITE (chfmt,'(a,i3,a)') "(a17,",nlev*3+1,"(1x,i10))"
WRITE (40,chfmt) "Livello (m) ->   ",0, &
  ((NINT(hlev1+i*delta_lev), i=0,nlev-1), ii=1,3)

WRITE (40,*)
WRITE (chfmt,'(a,i3,a)') "(a17,",nlev*3+1,"(1x,a10))"
WRITE (40,chfmt) "gg/mm/aaaa hh sca","      Hmix",  &
  ("  Dir-wind", i=1,nlev),("  Mod-wind", i=1,nlev),("    W-wind", i=1,nlev)

! Costruisco formato dati seriet
WRITE (chfmt,'(a,3(i3,a))') &
  "(i2.2,a1,i2.2,a1,i4.4,1x,i2.2,1x,i3.3,1x,f10.0,",nlev, &
  "(1x,f10.0),",nlev,"(1x,f10.1),",nlev,"(1x,f10.4))"


!--------------------------------------------------------------------------
! Lettura/scrittura dati (riempiendo le date mancanti)

datap = datetime_miss
out_ser = 0
out_ser_mis = 0
out_grads = 0
out_grads_mis = 0
irec = 1

DO k = 1,HUGE(0)

! Leggo il prossimo record, calcolo ora GMT dei dati, converto unita' di misura
  READ (30,*,IOSTAT=ios) year,month,day,hh,min, &
    zi,dd(1:nlev),ff(1:nlev),ww(1:nlev)
  IF (ios == eof) EXIT
  
  CALL init(datac, YEAR=year, MONTH=month, DAY=day, HOUR=hh, MINUTE=min)
  datac = datac - timedelta_new(HOUR=1)

  WHERE (ff(1:nlev) /= rmis)
    ff(1:nlev) = ff(1:nlev) / 100.
  ENDWHERE
  WHERE (ww(1:nlev) /= rmis)
    ww(1:nlev) = ww(1:nlev) / 100.
  ENDWHERE

! Se e' il primo istante memorizzo data e ora, altrimenti controllo se ci
! sono istanti mancanti
  IF (k == 1) THEN
    data1 = datac
  ELSE
    deltatc = datac - datap
    delta_step = deltatc / step_t2
  ENDIF

! Se non ho trovato un buco, scrivo i dati (in formato GRADS sempre; in 
! formato seriet solo se ho letto i dati di un'ora intera)

  IF (k == 1 .OR. delta_step == 1) THEN
    IF (min == 0) THEN
      WRITE (40,chfmt) day,"/",month,"/",year,hh,0, &
        zi,dd(1:nlev),ff(1:nlev),ww(1:nlev)
      out_ser = out_ser + 1
    ENDIF

    WRITE (41, REC=irec) zi
    irec = irec + 1
    DO kz = 1,nlev
      WRITE (41, REC=irec) dd(kz)
      irec = irec + 1
    ENDDO
    DO kz = 1,nlev
      WRITE (41, REC=irec) ff(kz)
      irec = irec + 1
    ENDDO
    DO kz = 1,nlev
      WRITE (41, REC=irec) ww(kz)
      irec = irec + 1
    ENDDO
    out_grads = out_grads + 1 

! Se ho trovato un buco, inserisco record di dati mancanti e da ultimo 
! scrivo i dati letti; datap e datac sono gli istanti con dati immediatamente
! prima e dopo il buco.

  ELSE IF (delta_step > 1) THEN
    CALL getval(datap, ISODATE=ch16)
    WRITE (*,'(a,i5,a,1x,a)') &
      "Inserisco ",delta_step," istanti mancanti, a partire da ",ch16

    DO kk = 1,delta_step-1
      datac = datap + kk * step_t2
      CALL getval(datac, YEAR=year, MONTH=month, DAY=day, HOUR=hh, MINUTE=min)

      IF (min == 0) THEN
        WRITE (40,chfmt) day,"/",month,"/",year,hh,0,(rmis,i=1,nlev*3+1)
        out_ser_mis = out_ser_mis + 1
      ENDIF

      DO kz = 1,nlev*3+1
        WRITE (41, REC=irec) rmis
        irec = irec + 1
      ENDDO
      out_grads_mis = out_grads_mis + 1
  
    ENDDO

    datac = datap + delta_step*step_t2
    CALL getval(datac, YEAR=year, MONTH=month, DAY=day, HOUR=hh, MINUTE=min)
    IF (min == 0) THEN
      WRITE (40,chfmt) day,"/",month,"/",year,hh,0, &
        zi,dd(1:nlev),ff(1:nlev),ww(1:nlev)
      out_ser = out_ser + 1
    ENDIF

    WRITE (41, REC=irec) zi
    irec = irec + 1
    DO kz = 1,nlev
      WRITE (41, REC=irec) dd(kz)
      WRITE (41, REC=irec+1) ff(kz)
      WRITE (41, REC=irec+2) ww(kz)
      irec = irec +3
    ENDDO
    out_grads = out_grads + 1

! Se le ore tornano indietro, esco con errore
  ELSE
    GOTO 9998

  ENDIF
  
! Salvo data corrente
  datap = datac

ENDDO

! Scrivo il file .ctl
nt = (datap - data1) / step_t2 + 1

WRITE (42,'(3a)')                "DSET   ","^",TRIM(filegrads)
WRITE (42,'(a,2(a,2i2.2,i4.4))') "TITLE  ","Sodar data"
WRITE (42,'(a,f10.3)')           "UNDEF  ",rmis   
WRITE (42,'(2a)')                "XDEF   ","1 linear 1 1"
WRITE (42,'(2a)')                "YDEF   ","1 linear 1 1"
WRITE (42,'(a,i3,a,2f6.0)')      "ZDEF   ",nlev," linear ",hlev1,delta_lev
WRITE (42,'(a,i4,3a)')           "TDEF   ",nt," linear ",grads_date(data1)," 30mn"
WRITE (42,'(a,i3)')              "VARS   ",4
WRITE (42,'(a,1x,2i4,1x,a)')     "zi",0,99,"sodar mixingh height (m)"
WRITE (42,'(a,1x,2i4,1x,a)')     "dd",nlev,99,"wind direction (deg)"
WRITE (42,'(a,1x,2i4,1x,a)')     "ff",nlev,99,"wind speed (m/s)"
WRITE (42,'(a,1x,2i4,1x,a)')     "w ",nlev,99,"vertical velocity (m/s)"
WRITE (42,'(a)')                 "ENDVARS"

! Log riepilogo
WRITE (*,*)
CALL getval(data1, ISODATE=ch16a)
CALL getval(datap, ISODATE=ch16b)
WRITE (*,'(5a,i6)') "data_ini (GMT) ",ch16a, &
  ", data_end (GMT) ",ch16b
WRITE (*,'(2(a,i6))') "record richiesti ",nt," record letti ",k-1
WRITE (*,'(3(a,i6),a)') "Output seriet: ist. scritti ",out_ser+out_ser_mis, &
  " (validi ",out_ser," aggiunti mancanti ",out_ser_mis,")"
WRITE (*,'(3(a,i6),a)') "Output GRADS : ist. scritti ",out_grads+out_grads_mis, &
  " (validi ",out_grads," aggiunti mancanti ",out_grads_mis,")"

expected_rec = nt * (3*nlev+1)
IF (irec-1 /= expected_rec) THEN
  WRITE (*,*) "Warning: numero di valori scritto sul file GRADS inatteso"
  WRITE (*,*) "Scritti",irec-1,"Attesi",expected_rec
ENDIF

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore ordine date "
CALL getval(datap, ISODATE=ch16)
WRITE (*,*) "Record ",k-1,ch16
CALL getval(datac, ISODATE=ch16)
WRITE (*,*) "Record ",k,ch16

STOP

END PROGRAM proc_sodar_moniter

!==========================================================================
FUNCTION grads_date (cdate) RESULT (ch12)
!
! function to return a date as a ch*12 string with the format required in a 
! GRADS ctl file (hhZddmmmyyyy)
!
USE datetime_class
IMPLICIT NONE
!
TYPE (datetime), INTENT(IN) :: cdate
INTEGER :: yy,mm,dd,hh,min
CHARACTER(LEN=12) :: ch12

CHARACTER(LEN=3), PARAMETER :: ch3_month(12) = &
  (/'jan','feb','mar','apr','may','jun', &
    'jul','aug','sep','oct','nov','dec'/)

CALL getval(cdate, YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh, MINUTE=min)
WRITE (ch12,'(i2.2,a1,i2.2,a3,i4.4)') hh,"Z",dd,ch3_month(mm),yy

RETURN
END FUNCTION grads_date

!==========================================================================
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


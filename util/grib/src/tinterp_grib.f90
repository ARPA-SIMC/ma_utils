PROGRAM tinterp_grib
!--------------------------------------------------------------------------
! Dato un file GRIB relativo a un parametro, lo riscrive con passo 
! temporale costante (verification time).
!
! Note:
! - i campi in input devono avere:
!   *) stessa griglia, parametro e livello;
!   *) verification time progressivi
!   *) timerange istantanei
! - il programma gestisce GRIB1 e GRIB2, ma con i GRIB2 non e' testato (e 
!   potrebbe non funzionare con qualsiasi GRIB2)
! - il programma gestisce sia analisi sia previsioni, ma in output e' ga-
!   rantita la sequenza dei verification time, non quella dei reftime/trange
!   Ad esempio, se l'input e' una sequenza di previsioni +00/+23, un run
!   interamente mancante sara' scritto come previsione +24/+47 del run
!   precedente. Per ovviare a questo, l'utente dovrebbe imporre lo step 
!   dei reftime (nuovo parametro da riga comando)
! - il programma dovrebbe funzionare anche per sfoltire un file, ma in 
!   questa modalita' potrebbe non essere ottimizzato.
!
!                                         Versione 1.0.2, Enrico 13/01/2014
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
USE datetime_class
USE grib2_utilities
IMPLICIT NONE

! Variabili locali
TYPE(datetime) :: vtime0,vtime1,vtime2,vtime_rq,rtime1,rtime2,rtime_out
REAL, ALLOCATABLE :: values1(:),values2(:),valuesout(:)
REAL :: w1,w2
INTEGER :: scad1(4),scad2(4)
INTEGER :: ifin,ifout,igi0=0,igi1=0,igi2=0,igout=0
INTEGER :: step,delta_2r,delta_r1,delta_21,ni,nj,yy,mm,dd,hh
INTEGER :: ft,en,gnov,nocv,nom
INTEGER :: kp,cntg,k,cnt_eqt,cnt_int,cnt_unc
INTEGER :: idp,ios,iret,clret(0:5)
CHARACTER (LEN=200) :: chdum,filein,fileout
CHARACTER (LEN=13) :: ch13(3)
CHARACTER (LEN=10) :: ch10
LOGICAL :: verbose

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

verbose = .FALSE.
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-verbose") THEN
    verbose = .TRUE.
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE (3)
      READ (chdum,*,IOSTAT=ios) step
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 1.2 Controlli e inizializzazioni

IF (ios /= 0) THEN
  CALL write_help
  STOP 1
ENDIF

CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w",iret)
cnt_eqt = 0
cnt_int = 0
cnt_unc = 0

!--------------------------------------------------------------------------
! 1.3 Elaboro il primo campo

! Leggo il grib
CALL grib_new_from_file(ifin,igi1,iret)
IF (iret == GRIB_SUCCESS) THEN
  cntg = 1
ELSE
  GOTO 9998
ENDIF

! Calcolo reference time, verification time, scadenza
CALL get_grib_time(igi1,rtime=rtime1,vtime=vtime1,iret=iret)
IF (iret /= 0) GOTO 9996
CALL get_grib1_header(igi1,scad=scad1)
IF (scad1(4) /= 0) GOTO 9991

! Alloco gli arrays di lavoro
CALL grib_get(igi1,"numberOfPointsAlongAParallel",ni)
CALL grib_get(igi1,"numberOfPointsAlongAMeridian",nj)
ALLOCATE (values1(ni*nj),values2(ni*nj),valuesout(ni*nj))

! Scrivo il campo
CALL grib_write(igi1,ifout)
cnt_unc = 1
IF (verbose) THEN
  ch13(1) = to_char(vtime1)
  WRITE (*,*) "unchanged: VTime ",ch13(1)
ENDIF

! Estraggo i valori e salvo i dati che serviranno in seguito
CALL grib_get(igi1,"getNumberOfValues",gnov)    ! totale di punti nel grib
CALL grib_get(igi1,"numberOfMissing",nom)       ! n.ro dati mancanti
CALL grib_get(igi1,"numberOfCodedValues",nocv)  ! n.ro dati validi
IF (nocv == 0) THEN
  values1(:) = rmiss
ELSE
  CALL grib_set(igi1,"missingValue",rmiss)
  CALL grib_get(igi1,"values",values1(:))
ENDIF
IF (nom + nocv /= gnov .OR. &
  (nocv /= 0 .AND. nocv /= COUNT(values1(:) /= rmiss))) GOTO 9988

CALL grib_clone(igi1,igi0)
vtime0 = vtime1

!--------------------------------------------------------------------------
! 1.4 Elaboro il secondo campo

! Leggo il grib
CALL grib_new_from_file(ifin,igi2,iret)
IF (iret == GRIB_END_OF_FILE) THEN
  WRITE (*,*) TRIM(filein)," contiene un solo campo"
  STOP
ELSE IF (iret == GRIB_SUCCESS) THEN
  cntg = 2
ELSE
  GOTO 9997
ENDIF

! Controllo griglia, parametro, livello
CALL check_consistency(igi2,igi0,.TRUE.,.FALSE.,.FALSE.,.TRUE.,.TRUE., &
  .TRUE.,clret,iret)
IF (ANY(clret(:) == 1)) GOTO 9995

! Calcolo reference time, verification time, scadenza
CALL get_grib_time(igi2,rtime=rtime2,vtime=vtime2,iret=iret)
IF (iret /= 0) GOTO 9996
IF (vtime2 < vtime1) GOTO 9994
CALL get_grib1_header(igi2,scad=scad2)
IF (scad2(4) /= 0) GOTO 9990
IF (vtime2 == vtime1) cnt_eqt = cnt_eqt + 1

! Estraggo i valori 
CALL grib_get(igi2,"getNumberOfValues",gnov)    ! totale di punti nel grib
CALL grib_get(igi2,"numberOfMissing",nom)       ! n.ro dati mancanti
CALL grib_get(igi2,"numberOfCodedValues",nocv)  ! n.ro dati validi
IF (nocv == 0) THEN
  values2(:) = rmiss
ELSE
  CALL grib_set(igi2,"missingValue",rmiss)
  CALL grib_get(igi2,"values",values2(:))
ENDIF
IF (nom + nocv /= gnov .OR. &
  (nocv /= 0 .AND. nocv /= COUNT(values2(:) /= rmiss))) GOTO 9987

!==========================================================================
! 2) Ciclo principale (istanti in output)

WRITE (*,*) "Preliminari completati, inizio ciclo principale"

main: DO k = 1,HUGE(0)
  vtime_rq = vtime0 + timedelta_new(hour=k*step)
  IF (vtime_rq < vtime1) GOTO 9993

!--------------------------------------------------------------------------
! 2.1) Se l'istante richiesto non e' compreso tra i due istanti in memoria, 
!      scorro filein fino a raggiungere vtime_rq

  IF (vtime_rq > vtime2) THEN
    DO
      igi1 = igi2
      vtime1 = vtime2
      values1(:) = values2(:)  

!     Leggo il prossimo campo
      CALL grib_release(igi2)
      vtime2 = datetime_miss
      values2(:) = rmiss

      CALL grib_new_from_file(ifin,igi2,iret)
      IF (iret == GRIB_END_OF_FILE) THEN
        EXIT main
      ELSE IF (iret /= GRIB_SUCCESS) THEN
        GOTO 9997
      ENDIF
      cntg = cntg + 1

!     Controllo griglia, parametro, livello
      CALL check_consistency(igi2,igi0,.TRUE.,.FALSE.,.FALSE.,.TRUE., &
        .TRUE.,.TRUE.,clret,iret)
      IF (ANY(clret(:) == 1)) GOTO 9995
      
!     Calcolo verifcation time, reference time, scadenza
      CALL get_grib_time(igi2,rtime=rtime1,vtime=vtime2,iret=iret)
      IF (iret /= 0) GOTO 9996
      IF (vtime2 < vtime1) GOTO 9994
      CALL get_grib1_header(igi2,scad=scad2)
      IF (scad2(4) /= 0) GOTO 9990
      IF (vtime2 == vtime1) cnt_eqt = cnt_eqt + 1

!     Estraggo i valori 
!     NB: se sto sfoltendo un file, il programa potrebbe essere reso piu'
!     efficiente eseguendo questa operazione solo quando necessario...
      CALL grib_get(igi2,"getNumberOfValues",gnov)    ! totale di punti nel grib
      CALL grib_get(igi2,"numberOfMissing",nom)       ! n.ro dati mancanti
      CALL grib_get(igi2,"numberOfCodedValues",nocv)  ! n.ro dati validi
      IF (nocv == 0) THEN
        values2(:) = rmiss
      ELSE
        CALL grib_set(igi2,"missingValue",rmiss)
        CALL grib_get(igi2,"values",values2(:))
      ENDIF
      IF (nom + nocv /= gnov .OR. &
        (nocv /= 0 .AND. nocv /= COUNT(values2(:) /= rmiss))) GOTO 9987

!     Se ho raggiunto l'istante richiesto, mi fermo
      IF (vtime2 >= vtime_rq) EXIT

    ENDDO
  ENDIF

!--------------------------------------------------------------------------
! 2.2) Interpolo i dati all'istante richiesto e scrivo il campo risultante

  IF (verbose) THEN
    ch13(1) = to_char(vtime1)
    ch13(2) = to_char(vtime2)
    ch13(3) = to_char(vtime_rq)
  ENDIF

! 2.2.1 Istante coincidente con un dato in input
  IF (vtime_rq == vtime1) THEN
    CALL grib_write(igi1,ifout)
    cnt_unc = cnt_unc + 1
    IF (verbose) WRITE (*,*) "unchanged: VTime ",ch13(3)

  ELSE IF (vtime_rq == vtime2) THEN
    CALL grib_write(igi2,ifout)
    cnt_unc = cnt_unc + 1
    IF (verbose) WRITE (*,*) "unchanged: VTime ",ch13(3)

! 2.2.2 Istante da interpolare
  ELSE
    CALL grib_release(igout)
    CALL grib_clone(igi1,igout)

!   Costruisco gli header del grib in output: se sto interpolando tra due 
!   analisi, cambio solo reference time, negli alti casi interpolo il 
!   forecast time
    IF (vtime1 == rtime1 .AND. vtime2 == rtime2) THEN 
      CALL getval(vtime_rq,year=yy,month=mm,day=dd,hour=hh)
      CALL grib_set(igout,"year",yy)
      CALL grib_set(igout,"month",mm)
      CALL grib_set(igout,"day",dd)
      CALL grib_set(igout,"hour",hh)
    ELSE
      CALL getval(vtime_rq - rtime1, ahour=ft)
      CALL grib_get(igi1,"editionNumber",en)
      IF (en == 1) THEN
        CALL grib_set(igout,"P1",ft)
      ELSE
        CALL grib_set(igout,"forecastTime",ft)
      ENDIF
    ENDIF

!   Calcolo i valori in output
    CALL getval(vtime2 - vtime_rq, ahour=delta_2r)
    CALL getval(vtime_rq - vtime1, ahour=delta_r1)
    CALL getval(vtime2 - vtime1, ahour=delta_21)
    w1 = REAL(delta_2r) / REAL(delta_21)
    w2 = REAL(delta_r1) / REAL(delta_21)
    IF (w1 <= 0. .OR. w1 >= 1. .OR. w2 <= 0. .OR. w2 >= 1.) GOTO 9992

    WHERE (values1(:) /= rmiss .AND. values2(:) /=  rmiss)
      valuesout(:) = w1 * values1(:) + w2 * values2(:)
    ELSEWHERE
      valuesout(:) = rmiss
    ENDWHERE
    CALL grib_set(igout,"values",valuesout(:))

!   Lo scrivo
    CALL grib_write(igout,ifout)
    cnt_int = cnt_int + 1
    IF (verbose) WRITE (*,'(7a,2(1x,f5.3))') "VTime ",ch13(3), &
      "   interp. from ",ch13(1)," and ",ch13(2),"   weights",w1,w2

  ENDIF

ENDDO main

!==========================================================================
! 3) Conclusione

! Log a schermo
WRITE (*,'(4(a,i6),a)') "Campi letti: ",cntg," scritti ",k, &
  " (uguali ",cnt_unc," interpolati ",cnt_int,")"
IF (k /= cnt_int + cnt_unc) WRITE (*,*) "Errore nei conteggi!!"
IF (cnt_eqt > 0) WRITE (*,*) &
  "Trovati ",cnt_eqt," campi con lo stesso verification time"

STOP

!--------------------------------------------------------------------------

9999 CONTINUE
WRITE (*,*) "File non trovato ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Il file ",TRIM(filein)," non contine nessun campo"
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," campo ",cntg
STOP 2

9996 CONTINUE
WRITE (*,*) "Errore nel calcolo del verification time, campo ",cntg
STOP 3

9995 CONTINUE
WRITE (*,*) "Grib inconsistenti, campo ",cntg
DO k = 0,5
  IF (clret(k) == 1)  WRITE (*,*) cllab(k),": test non passato"
ENDDO
STOP 3

9994 CONTINUE
WRITE (*,*) "Trovati verification times non consecutivi:"
CALL getval(vtime1,simpledate=ch10)
WRITE (*,*) " Campo ",cntg-1," vtime ",ch10
CALL getval(vtime2,simpledate=ch10)
WRITE (*,*) " Campo ",cntg," vtime ",ch10
STOP 3

9993 CONTINUE
WRITE (*,*) "Errore inatteso nel calcolo della sequenza temporale, informare il code owner"
STOP 4

9992 CONTINUE
WRITE (*,*) "Errore inatteso nel calcolo dei pesi, informare il code owner"
STOP 4

9991 CONTINUE
WRITE (*,*) "Il campo ",cntg," ha un timerange non istantaneo: ",scad2(4)
STOP 3

9990 CONTINUE
WRITE (*,*) "Il campo ",cntg," ha un timerange non istantaneo: ",scad1(4)
STOP 3

9988 CONTINUE
WRITE (*,*) "Errore nelle chiavi realtive ai dati mancanti, campo ",cntg
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(values1(1:ni*nj) /= rmiss)
STOP 5

9987 CONTINUE
WRITE (*,*) "Errore nelle chiavi realtive ai dati mancanti, campo ",cntg
WRITE (*,*) "Dati totali (getNumberOfValues):   ",gnov
WRITE (*,*) "Dati validi (numberOfCodedValues): ",nocv
WRITE (*,*) "Dati mancanti (numberOfMissing):   ",nom
WRITE (*,*) "Dati mancanti (matrice grib):      ",COUNT(values2(1:ni*nj) /= rmiss)
STOP 5

END PROGRAM tinterp_grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: tinterp_grib.exe [-h] [-verbose] filein fileout step"
WRITE (*,*) ""
WRITE (*,*) "Dato un file GRIB relativo a un parametro, lo riscrive con passo temporale "
WRITE (*,*) "  costante (verification time)"
WRITE (*,*) "filein:   file di input (con un'unica griglia, var, liv)"
WRITE (*,*) "fileout:  file di output"
WRITE (*,*) "step:     passo temporale richiesto in output (verification time; ore)"
WRITE (*,*) "-verbose: vsulizza le date di tutti i campi elaborati"
WRITE (*,*) "-h:       visualizza questo help"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

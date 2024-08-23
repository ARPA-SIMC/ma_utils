PROGRAM grib_daily_stat
!--------------------------------------------------------------------------
! Programma che legge un file con molti grib e scrive su files separati 
! alcune elaborazioni relative a ciascuna giornata di dati (un campo per 
! ogni giorno). Le elaborazioni gestite sono: massimo, media, AOT, massimo
! della media mobile, superamento di una soglia.
! Scrive inoltre un file con il giorno tipo (24 campi)
!
! Dalla versione 6, il programma usa grib_api / eccodes.
! - Il test delle varie funzionalita' e' parziale (possibili bug!)
! - Non c'e' piu' il controllo sull'uguaglianza della griglia tra i campi
!   in input, ma solo sul numero di punti, oltre a grib edition, parametro
!   e livello (par 2.1.2 + label 9997)
!
! Note 
! - I grib devono essere definiti sulla stessa area, riferirsi a un unico
!   parametro e livello e essere ordinati per istante di validita'.
! - Il programma ragiona solo sull'istante di validita': le statistiche
!   reltive ai campi previsti saranno scritte come analisi relative alla 
!   data di validita', alle ore 12
! - Se per una data giornata non ci sono dati in input o se nessuno dei 
!   punti ha un numero di dati sufficienti, non scrive nulla.
!   Con nval=0, scrive un campo interamente mancante nelle giornate con dati
!   in input, ma in cui nessun punto ha un numero di dati sufficienti.
!   I campi "giorno medio" vengono scritti sempre (ev. tutti mancanti)
!   Si potrebbe mettere un'opzione per scrivere sempre un campo per 
!   ciascuna giornata di calendario, ma l'implementazione e' complessa.
!   I campi interamente mancanti danno comunque problemi con grib_api!!
! - Per rendere piu' leggibile il codice, il programma fa comunque tutti i
!   calcoli, ma scrive solo gli output richiesti
!
!                                         Versione 6.0.0, Enrico 19/03/2020
!--------------------------------------------------------------------------

USE grib_api
USE datetime_class
USE missing_values
USE grib2_utilities
IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: hh_aot_min = 8     ! prima ora per calcolo AOT (GMT)
INTEGER, PARAMETER :: hh_aot_max = 19    ! ultima ora per calcolo AOT (GMT)
INTEGER, PARAMETER :: nval_aot = hh_aot_max - hh_aot_min + 1

! Parametri da riga comando
REAL :: thr_aot,thr_exc,thr_mxrm1,thr_mxrm2
INTEGER :: nrm,nval,nval_rm,nbit
LOGICAL :: lave,lmax,ldty,ldtylc,laot,lmxrm1,lmxrm2,lexc
LOGICAL :: l_mod_scad

! Altre variabili del programma
REAL, ALLOCATABLE :: field(:),fout(:)
REAL, ALLOCATABLE :: field_ave(:),field_max(:),field_aot(:),field_mxrm(:),field_exc(:)
REAL, ALLOCATABLE :: field_dty(:,:),field_rm(:,:),field_mxrm1(:),field_mxrm2(:)
INTEGER,ALLOCATABLE :: cnt_ok(:),cnt_ok_mxrm(:),cnt_ok_aot(:)
INTEGER, ALLOCATABLE :: cnt_ok_dty(:,:),cnt_ok_rm(:,:)
TYPE(datetime) :: datav,datav_sav,datav_ini,datav_inip1
REAL :: fave
INTEGER :: ngribin,ngribout
INTEGER :: ifin,ifout_ave,ifout_max,ifout_dty,ifout_aot,ifout_rm1,ifout_rm2,ifout_exc
INTEGER :: igin=0,igin_first=0,igout=0,iret
INTEGER :: ni,nj,ni_first,nj_first,np,par(3),par_first(3),lev(3),lev_first(3),sca(4),en,en_first
INTEGER :: nok,hhv,dlth,ier,kh,ios,kpar,cnt_par,kg,pdb
INTEGER :: data_out
CHARACTER (LEN=200) :: filein,chpar
CHARACTER (LEN=10) :: ch10,ch10_dv,ch10_dvs
CHARACTER (LEN=8) :: ch8
CHARACTER (LEN=3) :: next_arg
LOGICAL :: deb,deb2

!##########################################################################
! 1) Preliminari

! 1.1 Parametri da riga comando

! 1.1.1 Valori di defualt
next_arg = ""
cnt_par = 0
ios = 0
deb = .FALSE.
deb2 = .FALSE.
lave = .FALSE.
lmax = .FALSE.
ldty = .FALSE.
ldtylc = .FALSE.
laot = .FALSE.
lmxrm1 = .FALSE.
lmxrm2 = .FALSE.
lexc = .FALSE.
pdb = 0
nbit = 16
thr_aot = rmiss
thr_mxrm1 = rmiss
thr_mxrm2 = rmiss
thr_exc = rmiss

nval = 18                ! per calcolo AOD: 18
nrm = 8                  ! per calcoli O3:   8
nval_rm = 6              ! per calcolo SOMO: 6

! 1.1.2 Parsing riga comando
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP 1
  ELSE IF (TRIM(chpar) == "-pts") THEN
    deb = .TRUE.
    next_arg = "pts"
  ELSE IF (TRIM(chpar) == "-nbit") THEN
    next_arg = "nbt"

  ELSE IF (TRIM(chpar) == "-nval") THEN
    next_arg = "nvl"
  ELSE IF (TRIM(chpar) == "-nrm") THEN
    next_arg = "nrm"
  ELSE IF (TRIM(chpar) == "-nvalrm") THEN
    next_arg = "nvr"

  ELSE IF (TRIM(chpar) == "-ave") THEN
    lave = .TRUE.
  ELSE IF (TRIM(chpar) == "-max") THEN
    lmax = .TRUE.
  ELSE IF (TRIM(chpar) == "-dty") THEN
    ldty = .TRUE.
  ELSE IF (TRIM(chpar) == "-dtylc") THEN
    ldtylc = .TRUE.
  ELSE IF (TRIM(chpar) == "-aot") THEN
    laot = .TRUE.
    next_arg = "aot"
  ELSE IF (TRIM(chpar) == "-mxrm1") THEN
    lmxrm1 = .TRUE.
    next_arg = "rm1"
  ELSE IF (TRIM(chpar) == "-mxrm2") THEN
    lmxrm2 = .TRUE.
    next_arg = "rm2"
  ELSE IF (TRIM(chpar) == "-exc") THEN
    lexc = .TRUE.
    next_arg = "exc"

  ELSE IF (next_arg == "pts") THEN
    READ(chpar,*,IOSTAT=ios) pdb
    next_arg = ""
  ELSE IF (next_arg == "nbt") THEN
    READ(chpar,*,IOSTAT=ios) nbit
    next_arg = ""
  ELSE IF (next_arg == "nvl") THEN
    READ(chpar,*,IOSTAT=ios) nval
    next_arg = ""
  ELSE IF (next_arg == "nrm") THEN
    READ(chpar,*,IOSTAT=ios) nrm
    next_arg = ""
  ELSE IF (next_arg == "nvr") THEN
    READ(chpar,*,IOSTAT=ios) nval_rm
    next_arg = ""
  ELSE IF (next_arg == "aot") THEN
    READ(chpar,*,IOSTAT=ios) thr_aot
    next_arg = ""
  ELSE IF (next_arg == "rm1") THEN
    READ(chpar,*,IOSTAT=ios) thr_mxrm1
    next_arg = ""
  ELSE IF (next_arg == "rm2") THEN
    READ(chpar,*,IOSTAT=ios) thr_mxrm2
    next_arg = ""
  ELSE IF (next_arg == "exc") THEN
    READ(chpar,*,IOSTAT=ios) thr_exc
    next_arg = ""
    
  ELSE IF (cnt_par == 0) THEN
    cnt_par = 1
    filein = chpar
  ELSE
    CALL scrive_help
    STOP 1
  ENDIF

  IF (ios /= 0) THEN
    CALL scrive_help
    STOP 1
  ENDIF
    
ENDDO

! 1.1.3 Controlli sui parametri
IF ((ldty.AND.ldtylc) .OR. &
    ((lmxrm1.OR.lmxrm2) .AND. nrm<1) .OR. &
    (nval<0 .OR. nval>24) .OR. & 
    (laot.AND.thr_aot==rmiss) .OR. (lmxrm1.AND.thr_mxrm1==rmiss) .OR. &
    (lexc.AND.thr_exc==rmiss) .OR. (lmxrm2.AND.thr_mxrm2==rmiss) .OR. &
    (nbit<1 .OR. nbit>24) .OR. pdb<0 .OR. cnt_par/=1) THEN
  CALL scrive_help
  STOP 1
ENDIF  

! 1.2 Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

IF (lave)             CALL grib_open_file (ifout_ave,"ave.grb",'w',iret)
IF (lmax)             CALL grib_open_file (ifout_max,"max.grb",'w',iret)
IF (ldty.OR.ldtylc)   CALL grib_open_file (ifout_dty,"dty.grb",'w',iret)
IF (laot)             CALL grib_open_file (ifout_aot,"aot.grb",'w',iret)
IF (lmxrm1)           CALL grib_open_file (ifout_rm1,"rm1.grb",'w',iret)
IF (lmxrm2)           CALL grib_open_file (ifout_rm2,"rm2.grb",'w',iret)
IF (lexc)             CALL grib_open_file (ifout_exc,"exc.grb",'w',iret)

OPEN (UNIT=96, FILE="grib_daily_stat.log", STATUS="REPLACE", ACTION="WRITE")
OPEN (UNIT=97, FILE="grib_daily_stat_pts.log", STATUS="REPLACE", ACTION="WRITE")

!##########################################################################
! 2) Lettura - Scrittura (ciclo sui grib)
!
! Calcolo del massimo della media mobile:
! - Ciascuna media mobile viene attribuita al giorno in cui cade il suo 
!   istante finale (ie. la media tra 18 e 24 cade nel giorno successivo)
! - nrm: numero di ore su cui devono essere calcolate le medie mobili
! - nval_rm: numero di dati validi richiesti per considerare valida una
!   media mobile
! - nval: numero di medie mobili richiesto per considerare valido il loro
!   massimo giornaliero
! - Gli array field_rm(np,nrm) e cnt_ok_rm(np,nrm) servono per 
!   calcolare le medie mobili a cui appartiene l'ora corrente (il 2o indice
!   e' la posizione dell'ora corrente all'interno di ciscuna media mobile)
!   Contengono somma e dati validi relativi alle 1:nrm ore precedenti
! - Gli array field_mxrm(np) e cnt_ok_mxrm(np) contengono il massimo della
!   media mobile trovato per il giorno corrente e il numero di medie mobili
!   usato per calcolarlo
! - Gli array field_mxrm1(np) e field_mxrm2(np) contengono i prodotti 
!   finali relativi al massimo giornaliero della media mobile (SOMO e EXC)

ngribin = 0
ngribout = 0
l_mod_scad = .FALSE.

grib: DO kg = 1,HUGE(0)

!==========================================================================
! 2.1) Elaborazioni preliminari sul grib appena letto

! 2.1.1 Leggo e decodifico il prossimo grib
  IF (kg > 1) CALL grib_release(igin)
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT grib
  IF (iret /= GRIB_SUCCESS) GOTO 9990

! 2.1.2 Se e' la prima data, salvo il puntatore al grib e alloco gli arrays; 
!       altrimenti controllo che area e parametro non siano cambiati.
  IF (kg == 1) THEN
    CALL grib_get(igin,"editionNumber",en)
    CALL grib_get(igin,"Ni",ni)
    CALL grib_get(igin,"Nj",nj)
    CALL get_grib1_header(igin, PAR=par, LEV=lev, SCAD=sca)
    np = ni * nj

    en_first = en
    ni_first = ni
    nj_first = nj
    par_first = par
    lev_first = lev
    CALL grib_clone(igin,igin_first)
    
    ALLOCATE (field(np),fout(np))
    ALLOCATE (field_ave(np),field_max(np),field_aot(np),field_exc(np))
    ALLOCATE (field_mxrm(np),field_mxrm1(np),field_mxrm2(np))
    ALLOCATE (field_dty(np,0:23),field_rm(np,nrm))
    ALLOCATE (cnt_ok(np),cnt_ok_mxrm(np),cnt_ok_aot(np))
    ALLOCATE (cnt_ok_rm(np,nrm),cnt_ok_dty(np,0:23))

  ELSE
    CALL grib_get(igin,"editionNumber",en)
    CALL grib_get(igin,"Ni",ni)
    CALL grib_get(igin,"Nj",nj)
    CALL get_grib1_header(igin, PAR=par, LEV=lev, SCAD=sca)
    IF (ni /= ni_first .OR. nj /= nj_first) GOTO 9996
    IF (en /= en_first) GOTO 9989
    IF (ANY(par /= par_first) .OR. ANY(lev /= lev_first)) GOTO 9998
  ENDIF

  CALL grib_set(igin,"missingValue",rmiss)
  CALL grib_get(igin,"values",field(1:np))
  
  IF (deb .AND. pdb > np) THEN
    WRITE (*,*) "I grib hanno ",np," punti, log sul punto ",pdb," impossibile"
    deb= .FALSE.
  ENDIF
 
! 2.1.3 Operazioni su data e scadenza:
! - calcolo la data di validita';
! - controllo che sia successiva a quella del grib precedente;
! - calcolo la differenza oraria tra il grib appena letto e quello precedente;
! - se la scadenza non e' relativa a un'analisi prendo nota.

  CALL get_grib_time(igin, VTIME=datav, IRET=ier) 
  IF (ier /= 0) GOTO 9995
  CALL getval(datav, HOUR=hhv)
  
  IF (kg == 1) THEN
    dlth = 0
  ELSE
    CALL getval(datav - datav_sav, AHOUR=dlth)
    IF (dlth < 1) GOTO 9994
  ENDIF
  IF (sca(2) /= 0 .OR. sca(3) /= 0) l_mod_scad = .TRUE.

! 2.1.4 Log relativi al grib appena letto
  ngribin = ngribin +1
  nok = COUNT(field(1:np) /= rmiss)
  fave = rmiss
  IF (nok>0) fave = SUM(field(1:np), MASK = field(1:np)/=rmiss) / REAL(nok)
  CALL getval(datav, SIMPLEDATE=ch10_dv)
  CALL getval(datav_sav, SIMPLEDATE=ch10_dvs)
  WRITE (96,'(3a,1x,a,4(1x,i3.3), 3x, a,i6,1x,e12.5)') &
   "Letto grb: data, ora ",ch10_dv(1:8),ch10_dv(9:10)," scad ",sca(:), &
   " nok,ave: ",nok,fave

! debug
  IF (deb2) WRITE (96,'(4a,i3)') &
    " precedente: data, ora ",ch10_dvs(1:8),ch10_dvs(9:10)," dlth ",dlth

!==========================================================================
! Switch principale. Il campo appena letto puo' essere:
! - il primo campo del file (par 2.2)
! - un campo della stessa giornata (par. 2.3)
! - il primo campo di una nuova giornata (par. 2.4)

! 2.2) Se e' il primo campo, inizializzo i contatori. 

  IF (kg == 1) THEN
    IF (deb) WRITE (97,*) "(grib 1) input: ",ch10_dv(1:8)," ",ch10_dv(9:10),field(pdb)
    datav_sav = datav
    datav_ini = datav
    datav_inip1 = datav + timedelta_new(HOUR=1)

    WHERE (field(1:np) /= rmiss)
      field_ave(1:np) = field(1:np)
      field_max(1:np) = field(1:np)
      field_dty(1:np,hhv) = field(1:np)
      field_mxrm(1:np) = -HUGE(0.)
      field_mxrm1(1:np) = -HUGE(0.)
      field_mxrm2(1:np) = -HUGE(0.)
      cnt_ok(1:np) = 1
      cnt_ok_dty(1:np,hhv) = 1
      cnt_ok_mxrm(1:np) = 0
    ELSEWHERE
      field_ave(1:np) = 0.
      field_max(1:np) = -HUGE(0.)
      field_dty(1:np,hhv) = 0.
      field_mxrm(1:np) = -HUGE(0.)
      field_mxrm1(1:np) = -HUGE(0.)
      field_mxrm2(1:np) = -HUGE(0.)
      cnt_ok(1:np) = 0
      cnt_ok_dty(1:np,hhv) = 0
      cnt_ok_mxrm(1:np) = 0
    ENDWHERE

    field_aot(1:np) = 0.
    cnt_ok_aot(1:np) = 0
    IF (hhv >= hh_aot_min .AND. hhv <= hh_aot_max) THEN
      WHERE (field(1:np) /= rmiss)
        field_aot(1:np) = MAX(field(1:np)-thr_aot, 0.)
        cnt_ok_aot(1:np) = 1
      ENDWHERE
    ENDIF

    WHERE (SPREAD(field(1:np), DIM=2, NCOPIES=nrm) /= rmiss)
      field_rm(1:np,1:nrm) = SPREAD(field(1:np), DIM=2, NCOPIES=nrm)
      cnt_ok_rm(1:np,1:nrm) = 1
    ELSEWHERE
      field_rm(1:np,1:nrm) = 0.
      cnt_ok_rm(1:np,1:nrm) = 0
    ENDWHERE

!  Debug
   IF (deb2) WRITE (97,*) "init: ",hhv,field(pdb),field_ave(pdb),cnt_ok(pdb)
   IF (deb2) WRITE (97,'(a,i3,3x,8(1x,f6.2),3x,8(1x,i2),3x,f6.2,1x,i2)') &
     "init:             ",hhv,field_rm(pdb,1:nrm),cnt_ok_rm(pdb,1:nrm), &
     field_mxrm(pdb),cnt_ok_mxrm(pdb)

!==========================================================================
! 2.3) Se la data non e'cambiata, aggiorno i contatori

  ELSE IF (ch10_dv(1:8) == ch10_dvs(1:8)) THEN
    IF (deb) WRITE (97,*) "(same day) input: ",ch10_dv(1:8)," ",ch10_dv(9:10),field(pdb)
           
!   Salvo la data corrente per quando elaborero' la prossima
    datav_sav = datav

!   Shift medie mobili
    DO kh = nrm, dlth+1, -1
      field_rm(1:np,kh) = field_rm(1:np,kh-dlth)
      cnt_ok_rm(1:np,kh) = cnt_ok_rm(1:np,kh-dlth)
    ENDDO
    field_rm(1:np,1:MIN(dlth,nrm)) = 0.
    cnt_ok_rm(1:np,1:MIN(dlth,nrm)) = 0.

!   Elaboro statisitiche
    WHERE (field(1:np) /= rmiss)
      field_ave(1:np) = field(1:np) + field_ave(1:np)
      field_max(1:np) = MAX(field(1:np),field_max(1:np))
      field_dty(1:np,hhv) = field(1:np) + field_dty(1:np,hhv)
      cnt_ok(1:np) = cnt_ok(1:np) + 1
      cnt_ok_dty(1:np,hhv) = cnt_ok_dty(1:np,hhv) + 1
    ENDWHERE

!   AOT
    IF (hhv >= hh_aot_min .AND. hhv <= hh_aot_max) THEN
      WHERE (field(1:np) /= rmiss)
        field_aot(1:np) = field_aot(1:np) + MAX(field(1:np)-thr_aot, 0.)
        cnt_ok_aot(1:np) = cnt_ok_aot(1:np) + 1
      ENDWHERE
    ENDIF

!   RM: incremento cumulate mobili
    WHERE (SPREAD(field(1:np), DIM=2, NCOPIES=nrm) /= rmiss)
      field_rm(1:np,1:nrm) =  SPREAD(field(1:np), DIM=2, NCOPIES=nrm) + &
        field_rm(1:np,1:nrm)
      cnt_ok_rm(1:np,1:nrm) = cnt_ok_rm(1:np,1:nrm) + 1
    ENDWHERE

!   RM: calcolo la media mobile che si conclude nell'ora che sto elaborando
    WHERE (cnt_ok_rm(1:np,nrm) >= nval_rm)
      field_rm(1:np,nrm) = field_rm(1:np,nrm) / &
        REAL(cnt_ok_rm(1:np,nrm))
    ELSEWHERE
      field_rm(1:np,nrm) = rmiss
    ENDWHERE

!   RM: aggiorno il max delle medie mobili relative alla giornata corrente
    WHERE (field_rm(1:np,nrm) /= rmiss)
      field_mxrm(1:np) = MAX(field_mxrm(1:np),field_rm(1:np,nrm))
      cnt_ok_mxrm(1:np) = cnt_ok_mxrm(1:np) + 1
    ENDWHERE

!   Debug
    IF (deb .AND. (lmxrm1 .OR. lmxrm2)) WRITE (97,*) &
      "Media mobile ore prec.: ",field_rm(pdb,nrm)

   IF (deb2) WRITE (97,*) "proc stessa data: ",hhv,field(pdb),field_ave(pdb),cnt_ok(pdb)
   IF (deb2) WRITE (97,'(a,i3,3x,8(1x,f6.2),3x,8(1x,i2),3x,f6.2,1x,i2)') &
     "proc stessa data: ",hhv,field_rm(pdb,1:nrm),cnt_ok_rm(pdb,1:nrm), &
     field_mxrm(pdb),cnt_ok_mxrm(pdb)

!==========================================================================
! 2.4) Se ho iniziato una nuova giornata, elaboro le statistiche del giorno
!      precedente, scrivo i dati del giorno precedente, aggiorno o 
!      re-iniziliazzo i contatori

  ELSE
    IF (deb) WRITE (97,*) "(new day) input: ",ch10_dv(1:8)," ",ch10_dv(9:10),field(pdb)

!--------------------------------------------------------------------------
!   2.4.1 Calcolo le statistiche relative al giorno precedente

!   Ave, Max
    WHERE (cnt_ok(1:np) >= nval)
      field_ave(1:np) = field_ave(1:np) / REAL(cnt_ok(1:np))
    ELSEWHERE
      field_ave(1:np) = rmiss
      field_max(1:np) = rmiss
    ENDWHERE

!   AOT
    WHERE (cnt_ok_aot(1:np) < nval_aot)
      field_aot(1:np) = rmiss
    ENDWHERE

!   RM
    IF (deb) WRITE (97,*) "cnt_ok_mxrm ",cnt_ok_mxrm(pdb)
    WHERE (cnt_ok_mxrm(1:np) < nval)
      field_mxrm1(1:np) = rmiss
      field_mxrm2(1:np) = rmiss
    ELSEWHERE
      field_mxrm1(1:np) = MAX(field_mxrm(1:np) - thr_mxrm1, 0.)
      WHERE (field_mxrm(1:np) > thr_mxrm2)
        field_mxrm2(1:np) = 1.
      ELSEWHERE
        field_mxrm2(1:np) = 0.
      ENDWHERE
    ENDWHERE

!   Exc
    WHERE (field_ave(1:np) /= rmiss)
      WHERE (field_ave(1:np) > thr_exc)
        field_exc(1:np) = 1.
      ELSEWHERE
        field_exc(1:np) = 0.
      ENDWHERE
    ELSEWHERE
        field_exc(1:np) = rmiss
    ENDWHERE

!   Debug
    CALL getval(datav_sav, SIMPLEDATE=ch10)
    WRITE (96,'(2a)') "Calcolate statistiche del ",ch10(1:8)
    IF (deb) WRITE (97,*) "Completate elaborazioni del ", &
      ch10(1:8),field_ave(pdb),cnt_ok(pdb)

!--------------------------------------------------------------------------
!   2.4.2 Scrivo le elaborazioni relative al giorno precedente

!   Codifico data e ora per scrivere i grib relativi al giorno precedente
    CALL grib_clone(igin,igout)
    CALL grib_set(igout,"missingValue",rmiss)
    
    CALL getval(datav_sav, SIMPLEDATE=ch8)
    READ (ch8,'(i8)') data_out
    CALL grib_set(igout,"dataDate",data_out)
    CALL grib_set(igout,"hour",12)                              
    CALL grib_set(igout,"minute",0)                            
    CALL grib_set(igout,"second",0)                            
    CALL grib_get(igout,"bitsPerValue",nbit)
    IF (l_mod_scad) THEN
      IF (en == 1) THEN
        CALL grib_set(igout,"timeRangeIndicator",0)
        CALL grib_set(igout,"P1",0)
        CALL grib_set(igout,"P2",0)
      ELSE IF (en == 2) THEN                             
        CALL grib_set(igout,"significanceOfReferenceTime",0)
        CALL grib_set(igout,"typeOfProcessedData",0)
        CALL grib_set(igout,"typeOfGeneratingProcess",0)
        CALL grib_set(igout,"forecastTime",0)
      ENDIF
    ENDIF
      
!   Scrivo il campo medio del giorno precedente
    nok = COUNT(field_ave(1:np) /= rmiss)
    IF (lave .AND. nok>0) THEN
      fave = SUM(field_ave(1:np), MASK = field_ave(1:np)/=rmiss) / REAL(nok)
      WRITE (96,*) "Scrivo media del ",ch8,": dati ok, media ",nok,fave
      IF (deb) WRITE (97,*) "ave  : ",field_ave(pdb)
  
      CALL grib_set(igout,"values",field_ave(1:np))
      CALL grib_write (igout,ifout_ave)
      ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo massimo del giorno precedente
    nok = COUNT(field_max(1:np) /= rmiss)
    IF (lmax .AND. nok>0) THEN
      fave = rmiss
      IF (nok>0) fave = SUM(field_max(1:np), MASK = field_max(1:np)/=rmiss) / REAL(nok)
      WRITE (96,*) "Scrivo max del",ch8,": dati ok, media ",nok,fave
      IF (deb) WRITE (97,*) "max  : ",field_max(pdb)
  
      CALL grib_set(igout,"values",field_max(1:np))
      CALL grib_write (igout,ifout_max)
      ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo AOT del giorno precedente
    nok = COUNT(field_aot(1:np) /= rmiss)
    IF (laot .AND. nok>0) THEN
      fave = rmiss
      IF (nok>0) fave = SUM(field_aot(1:np), MASK = field_aot(1:np)/=rmiss) / REAL(nok)
      WRITE (96,*) "Scrivo AOT del",ch8,": dati ok, media ",nok,fave
      IF (deb) WRITE (97,*) "AOT  : ",field_aot(pdb)
  
      CALL grib_set(igout,"values",field_aot(1:np))
      CALL grib_write (igout,ifout_aot)
      ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo "eccedenza del max media mobile" del giorno precedente
    IF (deb .AND. (lmxrm1 .OR. lmxrm2)) WRITE (97,*) "MxRM : ",field_mxrm(pdb)

    nok = COUNT(field_mxrm1(1:np) /= rmiss)
    IF (lmxrm1 .AND. nok>0) THEN
      fave = rmiss
      IF (nok>0) fave = SUM(field_mxrm1(1:np), MASK = field_mxrm1(1:np)/=rmiss) / REAL(nok)
      WRITE (96,*) "Scrivo MxRMH1 del",ch8,": dati ok, media ",nok,fave
      IF (deb) WRITE (97,*) "MxRM1: ",field_mxrm1(pdb)
  
      CALL grib_set(igout,"values",field_mxrm1(1:np))
      CALL grib_write (igout,ifout_rm1)
      ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo "superamenti del max media mobile" del giorno precedente
    nok = COUNT(field_mxrm2(1:np) /= rmiss)
    IF (lmxrm2 .AND. nok>0) THEN
      fave = rmiss
      IF (nok>0) fave = SUM(field_mxrm2(1:np), MASK = field_mxrm2(1:np)/=rmiss) / REAL(nok)
      WRITE (96,*) "Scrivo MxRMH2 del",ch8,": dati ok, media ",nok,fave
      IF (deb) WRITE (97,*) "MxRM2: ",field_mxrm2(pdb)

      CALL grib_set(igout,"values",field_mxrm2(1:np))
      CALL grib_write (igout,ifout_rm2)
      ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo superamenti del giorno precedente
    nok = COUNT(field_exc(1:np) /= rmiss)
    IF (lexc .AND. nok>0) THEN
      fave = rmiss
      IF (nok>0) fave = SUM(field_exc(1:np), MASK = field_exc(1:np)/=rmiss) / REAL(nok)
      WRITE (96,*) "Scrivo EXC del",ch8," : dati ok, media ",nok,fave
      IF (deb) WRITE (97,*) "EXC  : ",field_exc(pdb)
  
      CALL grib_set(igout,"values",field_exc(1:np))
      CALL grib_write (igout,ifout_exc)
      ngribout = ngribout + 1
    ENDIF
  
    CALL grib_release(igout)
    CALL getval(datav_sav, SIMPLEDATE=ch10)
    WRITE (*,'(2a)') "Elaborata data ",ch10(1:8)
    WRITE (96,*)

!--------------------------------------------------------------------------
!   2.4.3 Aggiorno o re-inizializzo i contatori

!   Salvo la data corrente per quando elaborero' la prossima
    datav_sav = datav

!   Shift medie mobili
    DO kh = nrm, dlth+1, -1
      field_rm(1:np,kh) = field_rm(1:np,kh-dlth)
      cnt_ok_rm(1:np,kh) = cnt_ok_rm(1:np,kh-dlth)
    ENDDO
    field_rm(1:np,1:MIN(dlth,nrm)) = 0.
    cnt_ok_rm(1:np,1:MIN(dlth,nrm)) = 0.

!   Re-inizializzo i contatori (dty e' aggiornato e non re-inizializzato)
    WHERE (field(1:np) /= rmiss)
      field_ave(1:np) = field(1:np)
      field_max(1:np) = field(1:np)
      field_dty(1:np,hhv) = field(1:np) + field_dty(1:np,hhv)
      field_mxrm(1:np) = -HUGE(0.)
      field_mxrm1(1:np) = -HUGE(0.)
      field_mxrm2(1:np) = -HUGE(0.)
      cnt_ok(1:np) = 1
      cnt_ok_dty(1:np,hhv) = cnt_ok_dty(1:np,hhv) + 1
      cnt_ok_mxrm(1:np) = 0
    ELSEWHERE
      field_ave(1:np) = 0.
      field_max(1:np) = -HUGE(0.)
      field_mxrm(1:np) = -HUGE(0.)
      field_mxrm1(1:np) = -HUGE(0.)
      field_mxrm2(1:np) = -HUGE(0.)
      cnt_ok(1:np) = 0 
      cnt_ok_mxrm(1:np) = 0
    ENDWHERE

    field_aot(1:np) = 0.
    cnt_ok_aot(1:np) = 0
    IF (hhv >= hh_aot_min .AND. hhv <= hh_aot_max) THEN
      WHERE (field(1:np) /= rmiss)
        field_aot(1:np) = MAX(field(1:np)-thr_aot, 0.)
        cnt_ok_aot(1:np) = 1
      ENDWHERE
    ENDIF

!   RM: incremento cumulate mobili
    WHERE (SPREAD(field(1:np), DIM=2, NCOPIES=nrm) /= rmiss)
      field_rm(1:np,1:nrm) =  SPREAD(field(1:np), DIM=2, NCOPIES=nrm) + &
        field_rm(1:np,1:nrm)
      cnt_ok_rm(1:np,1:nrm) = cnt_ok_rm(1:np,1:nrm) +1
    ENDWHERE

!   RM: calcolo la media mobile che si conclude nell'ora che sto elaborando
    WHERE (cnt_ok_rm(1:np,nrm) >= nval_rm)
      field_rm(1:np,nrm) = field_rm(1:np,nrm) / &
        REAL(cnt_ok_rm(1:np,nrm))
    ELSEWHERE
      field_rm(1:np,nrm) = rmiss
    ENDWHERE

!   RMH: aggiorno il max delle medie mobili relative alla giornata corrente
    WHERE (field_rm(1:np,nrm) /= rmiss)
      field_mxrm(1:np) = MAX(field_mxrm(1:np),field_rm(1:np,nrm))
      cnt_ok_mxrm(1:np) = cnt_ok_mxrm(1:np) + 1
    ENDWHERE

!   Debug
   IF (deb2) WRITE (97,*) "proc nuova data: ",hhv,field(pdb),field_ave(pdb),cnt_ok(pdb)
   IF (deb2) WRITE (97,'(a,i3,3x,8(1x,f6.2),3x,8(1x,i2),3x,f6.2,1x,i2)') &
     "proc nuova data:  ",hhv,field_rm(pdb,1:nrm),cnt_ok_rm(pdb,1:nrm), &
     field_mxrm(pdb),cnt_ok_mxrm(pdb)

  ENDIF
! Chiusura dello switch principale (fra i paragrafi 2.2, 2.3 e 2.4)
  
ENDDO grib

!##########################################################################
! 3) Se il file di input non e' vuoto, scrivo le statistiche relative all'
!    ultima giornata, oltre ai 24 campi del giorno tipo

IF (ngribin > 0) THEN
 
!--------------------------------------------------------------------------
! 3.1 Calcolo le statistiche relative all'ultima giornata

! Ave, Max
  WHERE (cnt_ok(1:np) >= nval)
    field_ave(1:np) = field_ave(1:np) / REAL(cnt_ok(1:np))
  ELSEWHERE
    field_ave(1:np) = rmiss
    field_max(1:np) = rmiss
  ENDWHERE

! AOT
  WHERE (cnt_ok_aot(1:np) < nval_aot)
    field_aot(1:np) = rmiss
  ENDWHERE

! RM
  IF (deb) WRITE (97,*)  "cnt_ok_mxrm",cnt_ok_mxrm(pdb)
  WHERE (cnt_ok_mxrm(1:np) < nval)
    field_mxrm1(1:np) = rmiss
    field_mxrm2(1:np) = rmiss
  ELSEWHERE
    field_mxrm1(1:np) = MAX(field_mxrm(1:np) - thr_mxrm1, 0.)
    WHERE (field_mxrm(1:np) > thr_mxrm2)
      field_mxrm2(1:np) = 1.
    ELSEWHERE
      field_mxrm2(1:np) = 0.
    ENDWHERE
  ENDWHERE

! Exc
  WHERE (field_ave(1:np) /= rmiss)
    WHERE (field_ave(1:np) > thr_exc)
      field_exc = 1.
    ELSEWHERE
      field_exc = 0.
    ENDWHERE
  ELSEWHERE
      field_exc = rmiss
  ENDWHERE

! Giorno tipo
  WHERE (cnt_ok_dty(1:np,0:23) > 0)
    field_dty(1:np,0:23) = field_dty(1:np,0:23) / &
      REAL(cnt_ok_dty(1:np,0:23))
  ELSEWHERE
    field_dty(1:np,0:23) = rmiss
  ENDWHERE

! Debug
  CALL getval(datav_sav, SIMPLEDATE=ch10)
  WRITE (96,'(3a)') "Calcolate statistiche del ",ch10(1:8)," (last)"
  IF (deb) WRITE (97,*) "Completata proc. ultima giornata ",field_ave(pdb),cnt_ok(pdb)

!--------------------------------------------------------------------------
! 3.2 Scrivo le elaborazioni relative all'ultima giornata

! Codifico la data da scrivere
  CALL grib_clone(igin_first,igout)
  CALL grib_set(igout,"missingValue",rmiss)

  CALL getval(datav_sav, SIMPLEDATE=ch8)
  READ (ch8,'(i8)') data_out
  CALL grib_set(igout,"dataDate",data_out)
  CALL grib_set(igout,"hour",12)                              
  CALL grib_set(igout,"minute",0)                            
  CALL grib_set(igout,"second",0)                            
  CALL grib_get(igout,"bitsPerValue",nbit)
  IF (l_mod_scad) THEN
    IF (en == 1) THEN
      CALL grib_set(igout,"timeRangeIndicator",0)
      CALL grib_set(igout,"P1",0)
      CALL grib_set(igout,"P2",0)
    ELSE IF (en == 2) THEN                             
      CALL grib_set(igout,"significanceOfReferenceTime",0)
      CALL grib_set(igout,"typeOfProcessedData",0)
      CALL grib_set(igout,"typeOfGeneratingProcess",0)
      CALL grib_set(igout,"forecastTime",0)
    ENDIF
  ENDIF

! Scrivo il campo medio dell'ultima giornata
  nok = COUNT(field_ave(1:np) /= rmiss)
  IF (lave .AND. nok>0) THEN
    fave = rmiss
    IF (nok>0) fave = SUM(field_ave(1:np), MASK = field_ave(1:np)/=rmiss) / REAL(nok)
    WRITE (96,*) "Scrivo media del",ch8," (last): dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "ave  : ",field_ave(pdb)

    CALL grib_set(igout,"values",field_ave(1:np))
    CALL grib_write (igout,ifout_ave)
    ngribout = ngribout + 1
  ENDIF

! Scrivo il campo massimo dell'ultima giornata
  nok = COUNT(field_max(1:np) /= rmiss)
  IF (lmax .AND. nok>0) THEN
    fave = rmiss
    IF (nok>0) fave = SUM(field_max(1:np), MASK = field_max(1:np)/=rmiss) / REAL(nok)
    WRITE (96,*) "Scrivo max del",ch8," (last): dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "max  : ",field_max(pdb)
  
    CALL grib_set(igout,"values",field_max(1:np))
    CALL grib_write (igout,ifout_max)
    ngribout = ngribout + 1
  ENDIF

! Scrivo il campo AOT dell'ultima giornata
  nok = COUNT(field_aot(1:np) /= rmiss)
  IF (laot .AND. nok>0) THEN
    fave = rmiss
    IF (nok>0) fave = SUM(field_aot(1:np), MASK = field_aot(1:np)/=rmiss) / REAL(nok)
    WRITE (96,*) "Scrivo AOT del",ch8," (last): dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "AOT  : ",field_aot(pdb)

    CALL grib_set(igout,"values",field_aot(1:np))
    CALL grib_write (igout,ifout_aot)
    ngribout = ngribout + 1
  ENDIF

! Scrivo il campo "eccedenza del max media mobile" dell'ultima giornata
  IF (deb .AND. (lmxrm1 .OR. lmxrm2)) WRITE (97,*) "MxRM : ",field_mxrm(pdb)

  nok = COUNT(field_mxrm1(1:np) /= rmiss)
  IF (lmxrm1 .AND. nok>0) THEN
    fave = rmiss
    IF (nok>0) fave = SUM(field_mxrm1(1:np), MASK = field_mxrm1(1:np)/=rmiss) / REAL(nok)
    WRITE (96,*) "Scrivo MxRM1 del ",ch8," (last): dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "MxRM1: ",field_mxrm1(pdb)
  
    CALL grib_set(igout,"values",field_mxrm1(1:np))
    CALL grib_write (igout,ifout_rm1)
    ngribout = ngribout + 1
  ENDIF

! Scrivo il campo "superamenti del max media mobile" dell'ultima giornata
  nok = COUNT(field_mxrm2(1:np) /= rmiss)
  IF (lmxrm2 .AND. nok>0) THEN
    fave = rmiss
    IF (nok>0) fave = SUM(field_mxrm2(1:np), MASK = field_mxrm2(1:np)/=rmiss) / REAL(nok)
    WRITE (96,*) "Scrivo MxRM2 del",ch8," (last): dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "MxRM2: ",field_mxrm2(pdb)
  
    CALL grib_set(igout,"values",field_mxrm2(1:np))
    CALL grib_write (igout,ifout_rm2)
    ngribout = ngribout + 1
  ENDIF

! Scrivo il campo superamenti dell'ultima giornata
  nok = COUNT(field_exc(1:np) /= rmiss)
  IF (lexc .AND. nok>0) THEN
    fave = rmiss
    IF (nok>0) fave = SUM(field_exc(1:np), MASK = field_exc(1:np)/=rmiss) / REAL(nok)
    WRITE (96,*) "Scrivo EXC del",ch8," (last): dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "EXC  : ",field_exc(pdb)
  
    CALL grib_set(igout,"values",field_exc(1:np))
    CALL grib_write (igout,ifout_exc)
    ngribout = ngribout + 1
  ENDIF

  CALL getval(datav_sav, SIMPLEDATE=ch10)
  WRITE (*,'(2a)') "Elaborata data ",ch10(1:8)

  WRITE (96,*)

! Scrivo i campi del giorno tipo. 

! Codifico la data del primo grib
  CALL grib_clone(igin_first,igout)
  CALL grib_set(igout,"missingValue",rmiss)

  CALL getval(datav_ini, SIMPLEDATE=ch8)
  READ (ch8,'(i8)') data_out
  CALL grib_set(igout,"dataDate",data_out)
  CALL grib_set(igout,"minute",0)                            
  CALL grib_set(igout,"second",0)                            
  CALL grib_get(igout,"bitsPerValue",nbit)
  IF (l_mod_scad) THEN
    IF (en == 1) THEN
      CALL grib_set(igout,"timeRangeIndicator",0)
      CALL grib_set(igout,"P1",0)
      CALL grib_set(igout,"P2",0)
    ELSE IF (en == 2) THEN                             
      CALL grib_set(igout,"significanceOfReferenceTime",0)
      CALL grib_set(igout,"typeOfProcessedData",0)
      CALL grib_set(igout,"typeOfGeneratingProcess",0)
      CALL grib_set(igout,"forecastTime",0)
    ENDIF
  ENDIF

! Campo ordinario: attribuisco le medie alle ore 00-23 del giorno di
! validita' del primo campo
  IF (ldty) THEN
    DO kh = 0,23
      fout(1:np) = field_dty(1:np,kh)
      nok = COUNT(field(1:np) /= rmiss)
      fave = rmiss
      IF (nok>0) fave = SUM(fout(1:np), MASK = field(1:np)/=rmiss) / REAL(nok)
      WRITE (96,'(a,i3,i6,1x,e13.6)') &
        "Scrivo dty : hh, nok, media: ",kh,nok,fave
      IF (deb) WRITE (97,*) "gty: ",kh,field_dty(pdb,kh)

      CALL grib_set(igout,"hour",kh)                              
      CALL grib_set(igout,"values",fout(1:np))
      CALL grib_write (igout,ifout_dty)
      ngribout = ngribout + 1
    ENDDO

! Campo LAMI da de-cumulare: devo attribuire l'ora 00 al giorno successivo
  ELSE IF (ldtylc) THEN
    DO kh = 1,23
      fout(1:np) = field_dty(1:np,kh)
      nok = COUNT(field(1:np) /= rmiss)
      fave = rmiss
      IF (nok>0) fave = SUM(fout(1:np), MASK = field(1:np)/=rmiss) / REAL(nok)
      WRITE (96,'(a,i3,1x,e13.6)') &
        "Scrivo dty : hh, nok, media: ",kh,nok,fave
      IF (deb) WRITE (97,*) "gty_lc: ",kh,fout(pdb)

      CALL grib_set(igout,"hour",kh)                              
      CALL grib_set(igout,"values",fout(1:np))
      CALL grib_write (igout,ifout_dty)
      ngribout = ngribout + 1
    ENDDO

    kh = 0
    fout(1:np) = field_dty(1:np,kh)
    nok = COUNT(field(1:np) /= rmiss)
    fave = rmiss
    IF (nok>0) fave = SUM(field(1:np), MASK = field(1:np)/=rmiss) / REAL(nok)
    WRITE (96,'(a,i3,i3,1x,e13.6)') &
      "Scrivo dty : hh, nok, media: ",kh,nok,fave
    IF (deb) WRITE (97,*) "gty_lc: ",kh,field_dty(pdb,kh)

    CALL getval(datav_inip1, SIMPLEDATE=ch8)
    READ (ch8,'(i8)') data_out
    CALL grib_set(igout,"dataDate",data_out)
    CALL grib_set(igout,"hour",kh)                              
    CALL grib_set(igout,"values",fout(1:np))
    CALL grib_write (igout,ifout_dty)
    ngribout = ngribout + 1
  ENDIF

ENDIF

WRITE (*,*)
IF (l_mod_scad) THEN 
  WRITE (*,*) "Warning: i dati di input comprendono scadenze non di analisi,"
  WRITE (*,*) "che sono stati scritti come analisi alle ore 12 di ciascun giorno"
ENDIF

WRITE (*,*) "Grib letti, scritti ",ngribin,ngribout

STOP

!##########################################################################
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)," iret ",iret
STOP 2

9998 CONTINUE
WRITE (*,*) "Parametro/livello diverso in ",TRIM(filein)," grib ",kg
WRITE (*,'(2(a,3i4))') "Atteso:  par ",par_first(:)," liv ",lev_first(:)
WRITE (*,'(2(a,3i4))') "Atteso:  par ",par(:)," liv ",lev(:)
STOP 3

! 9997 CONTINUE
! WRITE (*,*) "Griglia diversa in ",TRIM(filein)," grib ",kg
! WRITE (*,'(a,10i8)') "Atteso:  ",ksec2((/1,2,3,4,5,7,8,11,13,14/))
! WRITE (*,'(a,10i8)') "Trovato: ",ksec2((/1,2,3,4,5,7,8,11,13,14/))
! STOP 4

9996 CONTINUE
WRITE (*,*) "Numero punti diverso in ",TRIM(filein)," grib ",kg
WRITE (*,*) "Attesi ",np," trovati ",ni*nj
STOP 5

9995 CONTINUE
WRITE (*,*) "Errore calcolo data validita, ",TRIM(filein)," grib ",kg
STOP 6

9994 CONTINUE
WRITE (*,*) "Istanti di validita' non sequenziali in ",TRIM(filein), &
  " grib ",kg
CALL getval(datav, SIMPLEDATE=ch10)
WRITE (*,'(2a)') "Attuale    ",ch10(1:8)
CALL getval(datav_sav, SIMPLEDATE=ch10)
WRITE (*,'(2a)') "Precedente ",ch10(1:8)
STOP 7

9990 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," iret ",iret," kg ",kg
STOP 2

9989 CONTINUE
WRITE (*,*) "Il file di input mescola grib1 e grib2"
STOP 8

END PROGRAM grib_daily_stat

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo l'help del programma
!
IMPLICIT NONE

!            123456789012345678901234567890123456789012345678901234567890123456789012345678
WRITE (*,*)
WRITE (*,*) "Uso: grib_daily_stat.exe filein [-h] [-pts N] [-nbit N] "
WRITE (*,*) " [-nval NVAL] [-nrm NRM] [-nvalrm NVALRM]"
WRITE (*,*) " [-ave] [-max] [-dty/dtylc] [-aot X] [-mxrm1 X] [-mxrm2 X] [-exc X]"
WRITE (*,*)
WRITE (*,*) "filein   file in input, in formato GRIB1"
WRITE (*,*) "-h       visualizza questo help"
WRITE (*,*) "-pts N   scrive un file di log relativo al punto N"
WRITE (*,*) "-nbit N  forza il numero di bit nei grib in uscita [1-24, def: 16]"
WRITE (*,*) ""
WRITE (*,*) "-nval NVAL: numero minimo di dati orari per considerare valide le statistiche"
WRITE (*,*) "         giornaliere; si applica a: media, massimo,  superamenti, max media"
WRITE (*,*) "         media mobile [1-24, def. 18]"
WRITE (*,*) "-nrm NRM: numero di ore su cui calcolare le medie mobili [def. 8]"
WRITE (*,*) "-nvalrm NVALRM numero minimo di dati orari per considerare valida una media"
WRITE (*,*) "         mobile [0-nrm, def. 6]"
WRITE (*,*) ""
WRITE (*,*) "-ave     (ave.grb) scrive la media giornaliera"
WRITE (*,*) "-max     (max.grb) scrive il massimo giornaliero"
WRITE (*,*) "-dty     (dty.grb) scrive il giorno medio"
WRITE (*,*) "-dtylc   (dtylc.grb) scrive il giorno medio, attribuendo il dato delle ore 00"
WRITE (*,*) "         al giorno successivo (utile per campi Cosmo da decumulare)"
WRITE (*,*) "-aot X   (aot.grb) calcola AOT con soglia X (somma della parte dei dati"
WRITE (*,*) "         eccedente X, calcolata tra le 8 e le 19 GMT e solo se tutti i 12 "
WRITE (*,*) "         dati orari sono presenti)"
WRITE (*,*) "-mxrm1 X (rm1.grb) calcola il massimo giornaliero della media mobile nelle NRM"
WRITE (*,*) "         ore precedenti, purche' abbiano almeno NVALRM dati validi, e sottrae"
WRITE (*,*) "         X dal risultato [per calcolo SOMO35: NRM=8, NVALRM=6, X=35., NVAL=???]"
WRITE (*,*) "-mxrm2 X (rm2.grb) calcola il massimo giornaliero della media mobile nelle NRM"
WRITE (*,*) "         ore precedenti, purche' abbiano almeno NVALRM dati validi; scrive 1."
WRITE (*,*) "         nei punti in cui questa e' maggiore di X, 0. negli altri punti"
WRITE (*,*) "         [per calcoli di legge: NRM=8, NVALRM=6, X=120., NVAL=???]"
WRITE (*,*) "-exc X   (exc.grb) scrive 1. nei punti in cui la media giornaliera supera la"
WRITE (*,*) "         soglia X, 0., nei punti in cui non la supera"
WRITE (*,*)
WRITE (*,*) "NB: i dati non istantanei sono sempre attribuiti al giorno in cui termina"
WRITE (*,*) "    l'intervallo di elaborazione!"
!            123456789012345678901234567890123456789012345678901234567890123456789012345678

RETURN
END SUBROUTINE scrive_help


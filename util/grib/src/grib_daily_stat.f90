PROGRAM grib_daily_stat
!--------------------------------------------------------------------------
! Programma che legge un file con molti grib e scrive su files separati 
! alcune elaborazioni relative a ciascuna giornata di dati (un campo per 
! ogni giorno). Le elaborazioni gestite sono: massimo, media, AOT, massimo
! della media mobile, superamento di una soglia.
! Scrive inoltre un file con il giorno tipo (24 campi)
!
! Note 
! - I grib devono essere definiti sulla stessa area, riferirsi a un unico
!   parametro e livello e essere ordinati per istante di validita'.
! - Il programma ragiona solo sull'istante di validita': le statistiche
!   reltive a i campi previsti saranno scritte come analisi relative alla 
!   data di validita'
! - Per rendere piu' leggibile il codice, il programma fa comunque tutti i
!   calcoli, ma scrive solo gli output richiesti
!
!                                           Versione 5.0, Enrico 28/05/2012
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.         ! valore per dati mancanti REAL
!REAL, PARAMETER :: rmis = -HUGE(0.)     ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000    ! dimensione massima dei GRIB
INTEGER, PARAMETER :: hh_aot_min = 8     ! prima ora per calcolo AOT (GMT)
INTEGER, PARAMETER :: hh_aot_max = 19    ! ultima ora per calcolo AOT (GMT)
INTEGER, PARAMETER :: nval_aot = hh_aot_max - hh_aot_min + 1

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1_sav(1024),ksec2_sav(1024),ksec3_sav(2),ksec4_sav(512)
INTEGER :: ksec1_first(1024),ksec2_first(1024)
INTEGER :: ksec1_out(1024),ksec4_out(512)

INTEGER :: kbuffer(maxdim),klen,kret
REAL :: psec2(512),psec3(2),psec2_sav(512)
REAL :: field(maxdim),fout(maxdim)

! Parametri da riga comando
REAL :: thr_aot,thr_exc,thr_mxrm1,thr_mxrm2
INTEGER :: nrm,nval,nval_rm,nbit
LOGICAL :: lave,lmax,ldty,ldtylc,laot,lmxrm1,lmxrm2,lexc,l_mod_scad

! Altre variabili del programma
REAL, ALLOCATABLE :: field_ave(:),field_max(:),field_aot(:),field_mxrm(:),field_exc(:)
REAL, ALLOCATABLE :: field_dty(:,:),field_rm(:,:),field_mxrm1(:),field_mxrm2(:)
INTEGER,ALLOCATABLE :: cnt_ok(:),cnt_ok_mxrm(:),cnt_ok_aot(:)
INTEGER, ALLOCATABLE :: cnt_ok_dty(:,:),cnt_ok_rm(:,:)
TYPE(date) :: datav,datav_sav,datav_ini,datav_inip1
REAL :: fave
INTEGER :: ngribin,ngribout
INTEGER :: iuin,iuout_ave,iuout_max,iuout_dty,iuout_aot,iuout_rm1,iuout_rm2,iuout_exc
INTEGER :: np,nok,hhv,hhv_sav,dlth,ier,kh,k,ios,kpar,cnt_par,kg,pdb
CHARACTER (LEN=80) :: filein,fileout,chpar
CHARACTER (LEN=3) :: next_arg
LOGICAL :: deb

!##########################################################################
! 1) Preliminari

! 1.1 Parametri da riga comando

! 1.1.1 Valori di defualt
next_arg = ""
cnt_par = 0
ios = 0
deb = .FALSE.
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
thr_aot = rmis
thr_mxrm1 = rmis
thr_mxrm2 = rmis
thr_exc = rmis

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
    STOP
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
  ENDIF

  IF (ios /= 0) THEN
    CALL scrive_help
    STOP
  ENDIF
    
ENDDO

! 1.1.3 Controlli sui parametri
IF ((ldty.AND.ldtylc) .OR. &
    ((lmxrm1.OR.lmxrm2) .AND. nrm<1) .OR. &
    (nval<1 .OR. nval>24) .OR. & 
    (laot.AND.thr_aot==rmis) .OR. (lmxrm1.AND.thr_mxrm1==rmis) .OR. &
    (lexc.AND.thr_exc==rmis) .OR. (lmxrm2.AND.thr_mxrm2==rmis) .OR. &
    (nbit<1 .OR. nbit>24) .OR. pdb<0) THEN
  CALL scrive_help
  STOP
ENDIF  

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
IF (kret /= 0) GOTO 9999

IF (lave)             CALL PBOPEN (iuout_ave,"ave.grb",'W',kret)
IF (lmax)             CALL PBOPEN (iuout_max,"max.grb",'W',kret)
IF (ldty.OR.ldtylc)   CALL PBOPEN (iuout_dty,"dty.grb",'W',kret)
IF (laot)             CALL PBOPEN (iuout_aot,"aot.grb",'W',kret)
IF (lmxrm1)           CALL PBOPEN (iuout_rm1,"rm1.grb",'W',kret)
IF (lmxrm2)           CALL PBOPEN (iuout_rm2,"rm2.grb",'W',kret)
IF (lexc)             CALL PBOPEN (iuout_exc,"exc.grb",'W',kret)

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
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT grib
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3(2) = rmis                                    ! dati mancanti = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.1.2 Se e' la prima data, salvo gli header grib e alloco gli arrays; 
!       altirmenti controllo che area e parametro non siano cambiati.
  IF (kg == 1) THEN
    ksec1_first(:) = ksec1(:)
    ksec2_first(:) = ksec2(:)
    np = ksec4(1) 
    ALLOCATE (field_ave(np),field_max(np),field_aot(np),field_exc(np))
    ALLOCATE (field_mxrm(np),field_mxrm1(np),field_mxrm2(np))
    ALLOCATE (field_dty(np,0:23),field_rm(np,nrm))
    ALLOCATE (cnt_ok(np),cnt_ok_mxrm(np),cnt_ok_aot(np))
    ALLOCATE (cnt_ok_rm(np,nrm),cnt_ok_dty(np,0:23))
  ELSE
    IF (ANY( ksec1((/1,6,7,8,9/)) /= ksec1_first((/1,6,7,8,9/)) )) GOTO 9998
    IF (ANY( ksec2((/1,2,3,4,5,7,8,11,13,14/)) /= &
       ksec2_first((/1,2,3,4,5,7,8,11,13,14/)) )) GOTO 9997
    IF (ksec4(1) /= np) GOTO 9996
  ENDIF

  IF (deb .AND. pdb > np) THEN
    WRITE (*,*) "I grib hanno ",np," punti, log sul punto ",pdb," impossibile"
    deb= .FALSE.
  ENDIF
 
! 2.1.3 Operazioni su data e scadenza:
! - calcolo la data di validita';
! - controllo che sia successiva a quella del grib precedente;
! - calcolo la differnza oraria tra il grib appena letto e quello precedente;
! - se la scadenza non e' relativa a un'analisi prendo nota.

  hhv_sav = hhv
  CALL ksec1_valid(ksec1,datav,hhv,ier)
  IF (ier /= 0) GOTO 9995
  IF (kg == 1) THEN
    dlth = 0
  ELSE
    dlth = (datav - datav_sav) * 24 + (hhv - hhv_sav)
    IF (dlth < 1) GOTO 9994
  ENDIF
  IF (ksec1(16) /= 0 .OR. ksec1(17) /= 0) l_mod_scad = .TRUE.

! 2.1.4 Log relativi al grib appena letto
  ngribin = ngribin +1
  nok = COUNT(field(1:np) /= rmis)
  fave = rmis
  IF (nok>0) fave = SUM(field(1:np), MASK = field(1:np)/=rmis) / REAL(nok)
  WRITE (96,'(a,i4.4,4(1x,i2.2), 3x, a,4(1x,i3.3), 3x, a,i6,1x,e12.5)') &
    "Letto grb: data, ora ",(ksec1(21)-1) * 100+ksec1(10), ksec1(11:14), &
    "scad ",ksec1(15:18)," nok,ave: ",nok,fave
  IF (deb) WRITE (97,*) "input: ",hhv,field(pdb)

!==========================================================================
! 2.2) Se e' il primo campo, inizializzo i contatori. 

  IF (kg == 1) THEN

    ksec1_sav(:) = ksec1(:)
    ksec2_sav(:) = ksec2(:)
    ksec3_sav(:) = ksec3(:)
    ksec4_sav(:) = ksec4(:)
    psec2_sav(:) = psec2(:)

    datav_sav = datav
    datav_ini = datav
    datav_inip1 = datav + 1

    WHERE (field(1:np) /= rmis)
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
      WHERE (field(1:np) /= rmis)
        field_aot(1:np) = MAX(field(1:np)-thr_aot, 0.)
        cnt_ok_aot(1:np) = 1
      ENDWHERE
    ENDIF

    WHERE (SPREAD(field(1:np), DIM=2, NCOPIES=nrm) /= rmis)
      field_rm(1:np,1:nrm) = SPREAD(field(1:np), DIM=2, NCOPIES=nrm)
      cnt_ok_rm(1:np,1:nrm) = 1
    ELSEWHERE
      field_rm(1:np,1:nrm) = 0.
      cnt_ok_rm(1:np,1:nrm) = 0
    ENDWHERE

!   Debug
!   IF (deb) WRITE (97,*) "init: ",hhv,field(pdb),field_ave(pdb),cnt_ok(pdb)
!   IF (deb) WRITE (97,'(a,i3,3x,8(1x,f6.2),3x,8(1x,i2),3x,f6.2,1x,i2)') &
!     "init:             ",hhv,field_rm(pdb,1:nrm),cnt_ok_rm(pdb,1:nrm), &
!     field_mxrm(pdb),cnt_ok_mxrm(pdb)

!==========================================================================
! 2.3) Se la data non e'cambiata, aggiorno i contatori
!
!
  ELSE IF (datav == datav_sav) THEN
           
!   Shift medie mobili
    DO kh = nrm, dlth+1, -1
      field_rm(1:np,kh) = field_rm(1:np,kh-dlth)
      cnt_ok_rm(1:np,kh) = cnt_ok_rm(1:np,kh-dlth)
    ENDDO
    field_rm(1:np,1:MIN(dlth,nrm)) = 0.
    cnt_ok_rm(1:np,1:MIN(dlth,nrm)) = 0.

!   Elaboro statisitiche
    WHERE (field(1:np) /= rmis)
      field_ave(1:np) = field(1:np) + field_ave(1:np)
      field_max(1:np) = MAX(field(1:np),field_max(1:np))
      field_dty(1:np,hhv) = field(1:np) + field_dty(1:np,hhv)
      cnt_ok(1:np) = cnt_ok(1:np) + 1
      cnt_ok_dty(1:np,hhv) = cnt_ok_dty(1:np,hhv) + 1
    ENDWHERE

!   AOT
    IF (hhv >= hh_aot_min .AND. hhv <= hh_aot_max) THEN
      WHERE (field(1:np) /= rmis)
        field_aot(1:np) = field_aot(1:np) + MAX(field(1:np)-thr_aot, 0.)
        cnt_ok_aot(1:np) = cnt_ok_aot(1:np) + 1
      ENDWHERE
    ENDIF

!   RM: incremento cumulate mobili
    WHERE (SPREAD(field(1:np), DIM=2, NCOPIES=nrm) /= rmis)
      field_rm(1:np,1:nrm) =  SPREAD(field(1:np), DIM=2, NCOPIES=nrm) + &
        field_rm(1:np,1:nrm)
      cnt_ok_rm(1:np,1:nrm) = cnt_ok_rm(1:np,1:nrm) + 1
    ENDWHERE

!   RM: calcolo la media mobile che si conclude nell'ora che sto elaborando
    WHERE (cnt_ok_rm(1:np,nrm) >= nval_rm)
      field_rm(1:np,nrm) = field_rm(1:np,nrm) / &
        REAL(cnt_ok_rm(1:np,nrm))
    ELSEWHERE
      field_rm(1:np,nrm) = rmis
    ENDWHERE

!   RM: aggiorno il max delle medie mobili relative alla giornata corrente
    WHERE (field_rm(1:np,nrm) /= rmis)
      field_mxrm(1:np) = MAX(field_mxrm(1:np),field_rm(1:np,nrm))
      cnt_ok_mxrm(1:np) = cnt_ok_mxrm(1:np) + 1
    ENDWHERE

!   Debug
    IF (deb .AND. (lmxrm1 .OR. lmxrm2)) WRITE (97,*) &
      "Media mobile ore prec.: ",field_rm(pdb,nrm)

!   IF (deb) WRITE (97,*) "proc stessa data: ",hhv,field(pdb),field_ave(pdb),cnt_ok(pdb)
!   IF (deb) WRITE (97,'(a,i3,3x,8(1x,f6.2),3x,8(1x,i2),3x,f6.2,1x,i2)') &
!     "proc stessa data: ",hhv,field_rm(pdb,1:nrm),cnt_ok_rm(pdb,1:nrm), &
!     field_mxrm(pdb),cnt_ok_mxrm(pdb)

!==========================================================================
! 2.4) Se ho iniziato una nuova giornata, elaboro le statistiche del giorno
!      precedente, scrivo i dati del giorno precedente, aggiorno o 
!      re-iniziliazzo i contatori

  ELSE

!--------------------------------------------------------------------------
!   2.4.1 Calcolo le statistiche relative al giorno precedente

!   Ave, Max
    WHERE (cnt_ok(1:np) >= nval)
      field_ave(1:np) = field_ave(1:np) / REAL(cnt_ok(1:np))
    ELSEWHERE
      field_ave(1:np) = rmis
      field_max(1:np) = rmis
    ENDWHERE

!   AOT
    WHERE (cnt_ok_aot(1:np) < nval_aot)
      field_aot(1:np) = rmis
    ENDWHERE

!   RM
    IF (deb) WRITE (97,*) "cnt_ok_mxrm ",cnt_ok_mxrm(pdb)
    WHERE (cnt_ok_mxrm(1:np) < nval)
      field_mxrm1(1:np) = rmis
      field_mxrm2(1:np) = rmis
    ELSEWHERE
      field_mxrm1(1:np) = MAX(field_mxrm(1:np) - thr_mxrm1, 0.)
      WHERE (field_mxrm(1:np) > thr_mxrm2)
        field_mxrm2(1:np) = 1.
      ELSEWHERE
        field_mxrm2(1:np) = 0.
      ENDWHERE
    ENDWHERE

!   Exc
    WHERE (field_ave(1:np) /= rmis)
      WHERE (field_ave(1:np) > thr_exc)
        field_exc(1:np) = 1.
      ELSEWHERE
        field_exc(1:np) = 0.
      ENDWHERE
    ELSEWHERE
        field_exc(1:np) = rmis
    ENDWHERE

!   Debug
    WRITE (96,'(a,i4.4,2(1x,i2.2))') "Calcolate statistiche del ", &
      datav_sav%yy,datav_sav%mm,datav_sav%dd
!   IF (deb) WRITE (97,*) "Completata proc. giornata ",field_ave(pdb),cnt_ok(pdb)

!--------------------------------------------------------------------------
!   2.4.2 Scrivo le elaborazioni relative al giorno precedente

!   Codifico data e ora per scrivere grib relativi al giorno precedente
    ksec1_sav(10) = MOD(datav_sav%yy-1,100) + 1
    ksec1_sav(21) = (datav_sav%yy-1) / 100 + 1
    ksec1_sav(11) = datav_sav%mm
    ksec1_sav(12) = datav_sav%dd
    ksec1_sav(13) = 12

!   Scrivo il campo medio del giorno precedente
    IF (lave) THEN
    nok = COUNT(field_ave(1:np) /= rmis)
    fave = rmis
    IF (nok>0) fave = SUM(field_ave(1:np), MASK = field_ave(1:np)/=rmis) / REAL(nok)
    WRITE (96,*) "Scrivo media: dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "ave  : ",field_ave(pdb)

    fout(1:np) = field_ave(1:np)
    ksec1_out(:) = ksec1_sav(:)
    ksec4_out(:) = ksec4_sav(:)
    IF (ANY(fout(1:np) == rmis)) THEN
      IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
        ksec1_out(5) = ksec1_out(5) + 64
      psec3(2) = rmis 
    ENDIF
    IF (l_mod_scad) ksec1_out(16:17) = 0
    ksec4_out(2) = nbit    

    CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
                 ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout_ave,kbuffer,ksec0(1),kret)
    ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo massimo del giorno precedente
    IF (lmax) THEN
    nok = COUNT(field_max(1:np) /= rmis)
    fave = rmis
    IF (nok>0) fave = SUM(field_max(1:np), MASK = field_max(1:np)/=rmis) / REAL(nok)
    WRITE (96,*) "Scrivo max  : dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "max  : ",field_max(pdb)

    fout(1:np) = field_max(1:np)
    ksec1_out(:) = ksec1_sav(:)
    ksec4_out(:) = ksec4_sav(:)
    IF (ANY(fout(1:np) == rmis)) THEN
      IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
        ksec1_out(5) = ksec1_out(5) + 64
      psec3(2) = rmis 
    ENDIF
    IF (l_mod_scad) ksec1_out(16:17) = 0
    ksec4_out(2) = nbit    

    CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
                 ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout_max,kbuffer,ksec0(1),kret)
    ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo AOT del giorno precedente
    IF (laot) THEN
    nok = COUNT(field_aot(1:np) /= rmis)
    fave = rmis
    IF (nok>0) fave = SUM(field_aot(1:np), MASK = field_aot(1:np)/=rmis) / REAL(nok)
    WRITE (96,*) "Scrivo AOT: dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "AOT  : ",field_aot(pdb)

    fout(1:np) = field_aot(1:np)
    ksec1_out(:) = ksec1_sav(:)
    ksec4_out(:) = ksec4_sav(:)
    IF (ANY(fout(1:np) == rmis)) THEN
      IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
        ksec1_out(5) = ksec1_out(5) + 64
      psec3(2) = rmis 
    ENDIF
    IF (l_mod_scad) ksec1_out(16:17) = 0
    ksec4_out(2) = nbit    

    CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
                 ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout_aot,kbuffer,ksec0(1),kret)
    ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo "eccedenza del max media mobile" del giorno precedente
    IF (deb .AND. (lmxrm1 .OR. lmxrm2)) WRITE (97,*) "MxRM : ",field_mxrm(pdb)

    IF (lmxrm1) THEN
    nok = COUNT(field_mxrm1(1:np) /= rmis)
    fave = rmis
    IF (nok>0) fave = SUM(field_mxrm1(1:np), MASK = field_mxrm1(1:np)/=rmis) / REAL(nok)
    WRITE (96,*) "Scrivo MxRMH1: dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "MxRM1: ",field_mxrm1(pdb)

    fout(1:np) = field_mxrm1(1:np)
    ksec1_out(:) = ksec1_sav(:)
    ksec4_out(:) = ksec4_sav(:)
    IF (ANY(fout(1:np) == rmis)) THEN
      IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
        ksec1_out(5) = ksec1_out(5) + 64
      psec3(2) = rmis 
    ENDIF
    IF (l_mod_scad) ksec1_out(16:17) = 0
    ksec4_out(2) = nbit    

    CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
                 ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout_rm1,kbuffer,ksec0(1),kret)
    ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo "superamenti del max media mobile" del giorno precedente
    IF (lmxrm2) THEN
    nok = COUNT(field_mxrm2(1:np) /= rmis)
    fave = rmis
    IF (nok>0) fave = SUM(field_mxrm2(1:np), MASK = field_mxrm2(1:np)/=rmis) / REAL(nok)
    WRITE (96,*) "Scrivo MxRMH2: dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "MxRM2: ",field_mxrm2(pdb)

    fout(1:np) = field_mxrm2(1:np)
    ksec1_out(:) = ksec1_sav(:)
    ksec4_out(:) = ksec4_sav(:)
    IF (ANY(fout(1:np) == rmis)) THEN
      IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
        ksec1_out(5) = ksec1_out(5) + 64
      psec3(2) = rmis 
    ENDIF
    IF (l_mod_scad) ksec1_out(16:17) = 0
    ksec4_out(2) = nbit    

    CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
                 ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout_rm2,kbuffer,ksec0(1),kret)
    ngribout = ngribout + 1
    ENDIF

!   Scrivo il campo superamenti del giorno precedente
    IF (lexc) THEN
    nok = COUNT(field_exc(1:np) /= rmis)
    fave = rmis
    IF (nok>0) fave = SUM(field_exc(1:np), MASK = field_exc(1:np)/=rmis) / REAL(nok)
    WRITE (96,*) "Scrivo EXC  : dati ok, media ",nok,fave
    IF (deb) WRITE (97,*) "EXC  : ",field_exc(pdb)

    fout(1:np) = field_exc(1:np)
    ksec1_out(:) = ksec1_sav(:)
    ksec4_out(:) = ksec4_sav(:)
    IF (ANY(fout(1:np) == rmis)) THEN
      IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
        ksec1_out(5) = ksec1_out(5) + 64
      psec3(2) = rmis 
    ENDIF
    IF (l_mod_scad) ksec1_out(16:17) = 0
    ksec4_out(2) = nbit    

    CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
                 ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout_exc,kbuffer,ksec0(1),kret)
    ngribout = ngribout + 1
    ENDIF
  
    WRITE (*,'(a,i4.4,2(1x,i2.2))') "Elaborata data ", &
      datav_sav%yy,datav_sav%mm,datav_sav%dd
    WRITE (96,*)

!--------------------------------------------------------------------------
!   2.4.3 Aggiorno o re-inizializzo i contatori

!   Aggiorno data precedente
    datav_sav = datav

!   Shift medie mobili
    DO kh = nrm, dlth+1, -1
      field_rm(1:np,kh) = field_rm(1:np,kh-dlth)
      cnt_ok_rm(1:np,kh) = cnt_ok_rm(1:np,kh-dlth)
    ENDDO
    field_rm(1:np,1:MIN(dlth,nrm)) = 0.
    cnt_ok_rm(1:np,1:MIN(dlth,nrm)) = 0.

!   Re-inizializzo i contatori (dty e' aggiornato e non re-inizializzato)
    WHERE (field(1:np) /= rmis)
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
      WHERE (field(1:np) /= rmis)
        field_aot(1:np) = MAX(field(1:np)-thr_aot, 0.)
        cnt_ok_aot(1:np) = 1
      ENDWHERE
    ENDIF

!   RM: incremento cumulate mobili
    WHERE (SPREAD(field(1:np), DIM=2, NCOPIES=nrm) /= rmis)
      field_rm(1:np,1:nrm) =  SPREAD(field(1:np), DIM=2, NCOPIES=nrm) + &
        field_rm(1:np,1:nrm)
      cnt_ok_rm(1:np,1:nrm) = cnt_ok_rm(1:np,1:nrm) +1
    ENDWHERE

!   RM: calcolo la media mobile che si conclude nell'ora che sto elaborando
    WHERE (cnt_ok_rm(1:np,nrm) >= nval_rm)
      field_rm(1:np,nrm) = field_rm(1:np,nrm) / &
        REAL(cnt_ok_rm(1:np,nrm))
    ELSEWHERE
      field_rm(1:np,nrm) = rmis
    ENDWHERE

!   RMH: aggiorno il max delle medie mobili relative alla giornata corrente
    WHERE (field_rm(1:np,nrm) /= rmis)
      field_mxrm(1:np) = MAX(field_mxrm(1:np),field_rm(1:np,nrm))
      cnt_ok_mxrm(1:np) = cnt_ok_mxrm(1:np) + 1
    ENDWHERE

!   Debug
!   IF (deb) WRITE (97,*) "proc nuova data: ",hhv,field(pdb),field_ave(pdb),cnt_ok(pdb)
!   IF (deb) WRITE (97,'(a,i3,3x,8(1x,f6.2),3x,8(1x,i2),3x,f6.2,1x,i2)') &
!     "proc nuova data:  ",hhv,field_rm(pdb,1:nrm),cnt_ok_rm(pdb,1:nrm), &
!     field_mxrm(pdb),cnt_ok_mxrm(pdb)

  ENDIF

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
    field_ave(1:np) = rmis
    field_max(1:np) = rmis
  ENDWHERE

! AOT
  WHERE (cnt_ok_aot(1:np) < nval_aot)
    field_aot(1:np) = rmis
  ENDWHERE

! RM
  IF (deb) WRITE (97,*)  "cnt_ok_mxrm",cnt_ok_mxrm(pdb)
  WHERE (cnt_ok_mxrm(1:np) < nval)
    field_mxrm1(1:np) = rmis
    field_mxrm2(1:np) = rmis
  ELSEWHERE
    field_mxrm1(1:np) = MAX(field_mxrm(1:np) - thr_mxrm1, 0.)
    WHERE (field_mxrm(1:np) > thr_mxrm2)
      field_mxrm2(1:np) = 1.
    ELSEWHERE
      field_mxrm2(1:np) = 0.
    ENDWHERE
  ENDWHERE

! Exc
  WHERE (field_ave(1:np) /= rmis)
    WHERE (field_ave(1:np) > thr_exc)
      field_exc = 1.
    ELSEWHERE
      field_exc = 0.
    ENDWHERE
  ELSEWHERE
      field_exc = rmis
  ENDWHERE

! Giorno tipo
  WHERE (cnt_ok_dty(1:np,0:23) > 0)
    field_dty(1:np,0:23) = field_dty(1:np,0:23) / &
      REAL(cnt_ok_dty(1:np,0:23))
  ELSEWHERE
    field_dty(1:np,0:23) = rmis
  ENDWHERE

! Debug
    WRITE (96,'(a,i4.4,2(1x,i2.2))') "Calcolate statistiche del ", &
      datav_sav%yy,datav_sav%mm,datav_sav%dd
! IF (deb) WRITE (97,*) "Completata proc. ultima giornata ",field_ave(pdb),cnt_ok(pdb)

!--------------------------------------------------------------------------
! 3.2 Scrivo le elaborazioni relative all'ultima giornata

! Codifico la data da scrivere
  ksec1_sav(10) = MOD(datav_sav%yy-1,100) + 1
  ksec1_sav(21) = (datav_sav%yy-1) / 100 + 1
  ksec1_sav(11) = datav_sav%mm
  ksec1_sav(12) = datav_sav%dd
  ksec1_sav(13) = 12

! Scrivo il campo medio dell'ultima giornata
  IF (lave) THEN
  nok = COUNT(field_ave(1:np) /= rmis)
  fave = rmis
  IF (nok>0) fave = SUM(field_ave(1:np), MASK = field_ave(1:np)/=rmis) / REAL(nok)
  WRITE (96,*) "Scrivo media: dati ok, media ",nok,fave
  IF (deb) WRITE (97,*) "ave  : ",field_ave(pdb)

  fout(1:np) = field_ave(1:np)
  ksec1_out(:) = ksec1_sav(:)
  ksec4_out(:) = ksec4_sav(:)
  IF (ANY(fout(1:np) == rmis)) THEN
    IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
      ksec1_out(5) = ksec1_out(5) + 64
    psec3(2) = rmis 
  ENDIF
  IF (l_mod_scad) ksec1_out(16:17) = 0
  ksec4_out(2) = nbit    

  CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
               ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout_ave,kbuffer,ksec0(1),kret)
  ngribout = ngribout + 1
  ENDIF

! Scrivo il campo massimo dell'ultima giornata
  IF (lmax) THEN
  nok = COUNT(field_max(1:np) /= rmis)
  fave = rmis
  IF (nok>0) fave = SUM(field_max(1:np), MASK = field_max(1:np)/=rmis) / REAL(nok)
  WRITE (96,*) "Scrivo max  : dati ok, media ",nok,fave
  IF (deb) WRITE (97,*) "max  : ",field_max(pdb)

  fout(1:np) = field_max(1:np)
  ksec1_out(:) = ksec1_sav(:)
  ksec4_out(:) = ksec4_sav(:)
  IF (ANY(fout(1:np) == rmis)) THEN
    IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
      ksec1_out(5) = ksec1_out(5) + 64
    psec3(2) = rmis 
  ENDIF
  IF (l_mod_scad) ksec1_out(16:17) = 0
  ksec4_out(2) = nbit    

  CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
               ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout_max,kbuffer,ksec0(1),kret)
  ngribout = ngribout + 1
  ENDIF

! Scrivo il campo AOT dell'ultima giornata
  IF (laot) THEN
  nok = COUNT(field_aot(1:np) /= rmis)
  fave = rmis
  IF (nok>0) fave = SUM(field_aot(1:np), MASK = field_aot(1:np)/=rmis) / REAL(nok)
  WRITE (96,*) "Scrivo AOT  : dati ok, media ",nok,fave
  IF (deb) WRITE (97,*) "AOT  : ",field_aot(pdb)

  fout(1:np) = field_aot(1:np)
  ksec1_out(:) = ksec1_sav(:)
  ksec4_out(:) = ksec4_sav(:)
  IF (ANY(fout(1:np) == rmis)) THEN
    IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
      ksec1_out(5) = ksec1_out(5) + 64
    psec3(2) = rmis 
  ENDIF
  IF (l_mod_scad) ksec1_out(16:17) = 0
  ksec4_out(2) = nbit    

  CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
               ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout_aot,kbuffer,ksec0(1),kret)
  ngribout = ngribout + 1
  ENDIF

! Scrivo il campo "eccedenza del max media mobile" dell'ultima giornata
  IF (deb .AND. (lmxrm1 .OR. lmxrm2)) WRITE (97,*) "MxRM : ",field_mxrm(pdb)

  IF (lmxrm1) THEN
  nok = COUNT(field_mxrm1(1:np) /= rmis)
  fave = rmis
  IF (nok>0) fave = SUM(field_mxrm1(1:np), MASK = field_mxrm1(1:np)/=rmis) / REAL(nok)
  WRITE (96,*) "Scrivo MxRM1: dati ok, media ",nok,fave
  IF (deb) WRITE (97,*) "MxRM1: ",field_mxrm1(pdb)

  fout(1:np) = field_mxrm1(1:np)
  ksec1_out(:) = ksec1_sav(:)
  ksec4_out(:) = ksec4_sav(:)
  IF (ANY(fout(1:np) == rmis)) THEN
    IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
      ksec1_out(5) = ksec1_out(5) + 64
    psec3(2) = rmis 
  ENDIF
  IF (l_mod_scad) ksec1_out(16:17) = 0
  ksec4_out(2) = nbit    

  CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
               ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout_rm1,kbuffer,ksec0(1),kret)
  ngribout = ngribout + 1
  ENDIF

! Scrivo il campo "sueperamenti del max media mobile" dell'ultima giornata
  IF (lmxrm2) THEN
  nok = COUNT(field_mxrm2(1:np) /= rmis)
  fave = rmis
  IF (nok>0) fave = SUM(field_mxrm2(1:np), MASK = field_mxrm2(1:np)/=rmis) / REAL(nok)
  WRITE (96,*) "Scrivo MxRM2: dati ok, media ",nok,fave
  IF (deb) WRITE (97,*) "MxRM2: ",field_mxrm2(pdb)

  fout(1:np) = field_mxrm2(1:np)
  ksec1_out(:) = ksec1_sav(:)
  ksec4_out(:) = ksec4_sav(:)
  IF (ANY(fout(1:np) == rmis)) THEN
    IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
      ksec1_out(5) = ksec1_out(5) + 64
    psec3(2) = rmis 
  ENDIF
  IF (l_mod_scad) ksec1_out(16:17) = 0
  ksec4_out(2) = nbit    

  CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
               ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout_rm2,kbuffer,ksec0(1),kret)
  ngribout = ngribout + 1
  ENDIF

! Scrivo il campo superamenti dell'ultima giornata
  IF (lexc) THEN
  nok = COUNT(field_exc(1:np) /= rmis)
  fave = rmis
  IF (nok>0) fave = SUM(field_exc(1:np), MASK = field_exc(1:np)/=rmis) / REAL(nok)
  WRITE (96,*) "Scrivo EXC  : dati ok, media ",nok,fave
  IF (deb) WRITE (97,*) "EXC  : ",field_exc(pdb)

  fout(1:np) = field_exc(1:np)
  ksec1_out(:) = ksec1_sav(:)
  ksec4_out(:) = ksec4_sav(:)
  IF (ANY(fout(1:np) == rmis)) THEN
    IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
      ksec1_out(5) = ksec1_out(5) + 64
    psec3(2) = rmis 
  ENDIF
  IF (l_mod_scad) ksec1_out(16:17) = 0
  ksec4_out(2) = nbit    

  CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
               ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout_exc,kbuffer,ksec0(1),kret)
  ngribout = ngribout + 1

  WRITE (*,'(a,i4.4,2(1x,i2.2))') "Elaborata data ", &
    datav_sav%yy,datav_sav%mm,datav_sav%dd
  WRITE (96,*)
  ENDIF

! Scrivo i campi del giorno tipo. 

! Codifico in ksec1_sav la data del primo grib
  ksec1_sav(10) = MOD(datav_ini%yy-1,100) + 1
  ksec1_sav(21) = (datav_ini%yy-1) / 100 + 1
  ksec1_sav(11) = datav_ini%mm
  ksec1_sav(12) = datav_ini%dd

! Campo ordinario: attribuisco le medie alle ore 00-23 del giorno di
! validita' del primo campo
  IF (ldty) THEN
    DO kh = 0,23
      fout(1:np) = field_dty(1:np,kh)
      nok = COUNT(field(1:np) /= rmis)
      fave = rmis
      IF (nok>0) fave = SUM(fout(1:np), MASK = field(1:np)/=rmis) / REAL(nok)
      WRITE (96,'(a,i3,i6,1x,e13.6)') &
        "Scrivo dty : hh, nok, media: ",kh,nok,fave
      IF (deb) WRITE (97,*) "gty: ",kh,field_dty(pdb,kh)

      ksec1_out(:) = ksec1_sav(:)
      ksec4_out(:) = ksec4_sav(:)
      IF (ANY(fout(1:np) == rmis)) THEN
        IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
          ksec1_out(5) = ksec1_out(5) + 64
        psec3(2) = rmis 
      ENDIF
      IF (l_mod_scad) ksec1_out(16:17) = 0
      ksec1_out(13) = kh
      ksec4_out(2) = nbit    

      CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
                   ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
      IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
      CALL PBWRITE (iuout_dty,kbuffer,ksec0(1),kret)
      ngribout = ngribout + 1
    ENDDO

! Campo LAMI da de-cumulare: devo attribuire l'ora 00 al giorno successivo
  ELSE IF (ldtylc) THEN
    DO kh = 1,23
      fout(1:np) = field_dty(1:np,kh)
      nok = COUNT(field(1:np) /= rmis)
      fave = rmis
      IF (nok>0) fave = SUM(fout(1:np), MASK = field(1:np)/=rmis) / REAL(nok)
      WRITE (96,'(a,i3,1x,e13.6)') &
        "Scrivo dty : hh, nok, media: ",kh,nok,fave
      IF (deb) WRITE (97,*) "gty_lc: ",kh,fout(pdb)

      ksec1_out(:) = ksec1_sav(:)
      ksec4_out(:) = ksec4_sav(:)
      IF (ANY(fout(1:np) == rmis)) THEN
        IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
          ksec1_out(5) = ksec1_out(5) + 64
        psec3(2) = rmis 
      ENDIF
      ksec1_out(13) = kh
      IF (l_mod_scad) ksec1_out(16:17) = 0
      ksec4_out(2) = nbit    

      CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
                   ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
      IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
      CALL PBWRITE (iuout_dty,kbuffer,ksec0(1),kret)
      ngribout = ngribout + 1
    ENDDO

    kh = 0
    fout(1:np) = field_dty(1:np,kh)
    nok = COUNT(field(1:np) /= rmis)
    fave = rmis
    IF (nok>0) fave = SUM(field(1:np), MASK = field(1:np)/=rmis) / REAL(nok)
    WRITE (96,'(a,i3,i3,1x,e13.6)') &
      "Scrivo dty : hh, nok, media: ",kh,nok,fave
    IF (deb) WRITE (97,*) "gty_lc: ",kh,field_dty(pdb,kh)

    ksec1_out(:) = ksec1_sav(:)
    ksec4_out(:) = ksec4_sav(:)
    IF (ANY(fout(1:np) == rmis)) THEN
      IF (ksec1_out(5) == 0 .OR. ksec1_out(5) == 128) &
        ksec1_out(5) = ksec1_out(5) + 64
      psec3(2) = rmis 
    ENDIF
    ksec1_out(10) = MOD(datav_inip1%yy-1,100) + 1
    ksec1_out(11) = datav_inip1%mm
    ksec1_out(12) = datav_inip1%dd
    ksec1_out(13) = kh
    IF (l_mod_scad) ksec1_out(16:17) = 0
    ksec4_out(2) = nbit    

    CALL GRIBEX (ksec0,ksec1_out,ksec2_sav,psec2_sav,ksec3_sav,psec3, &
                 ksec4_out,fout,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout_dty,kbuffer,ksec0(1),kret)
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
WRITE (*,*) "Errore aprendo ",TRIM(filein)," kret ",kret
STOP

9998 CONTINUE
WRITE (*,*) "Parametro/livello diverso in ",TRIM(filein)," grib ",kg
WRITE (*,'(a,2i4,a,3i4)') "Atteso:  par ",ksec1_first(1),ksec1_first(6), &
  "   liv ",ksec1_first(7:9)
WRITE (*,'(a,2i4,a,3i4)') "Trovato: par ",ksec1(1),ksec1(6), &
  "   liv ",ksec1(7:9)
STOP

9997 CONTINUE
WRITE (*,*) "Griglia diversa in ",TRIM(filein)," grib ",kg
WRITE (*,'(a,10i8)') "Atteso:  ",ksec2((/1,2,3,4,5,7,8,11,13,14/))
WRITE (*,'(a,10i8)') "Trovato: ",ksec2((/1,2,3,4,5,7,8,11,13,14/))
STOP

9996 CONTINUE
WRITE (*,*) "Numero punti diverso in ",TRIM(filein)," grib ",kg
WRITE (*,*) "Attesi ",np," trovati ",ksec4(1)

9995 CONTINUE
WRITE (*,*) "Errore calcolo data validita, ",TRIM(filein)," grib ",kg
STOP

9994 CONTINUE
WRITE (*,*) "Istanti di validita' non sequenziali in ",TRIM(filein), &
  " grib ",kg
WRITE (*,'(a,i4.4,3(1x,i2.2))') "Attuale:    ", &
  datav%yy,datav%mm,datav%dd,hhv
WRITE (*,'(a,i4.4,3(1x,i2.2))') "Precedente: ", &
  datav_sav%yy,datav_sav%mm,datav_sav%dd,hhv_sav
STOP

END PROGRAM grib_daily_stat

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE ksec1_valid(ksec1,data,hh,ier)
!--------------------------------------------------------------------------
! Data la sezione 1 di un GRIB, ritorna la data e l'ora di validita'
!                                             Versione 2, Enrico 10/07/2007
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: ksec1(*)
TYPE(date), INTENT(OUT) :: data
INTEGER, INTENT(OUT) :: hh,ier
!
TYPE(date) :: datar
INTEGER :: hhr,hh_tot,sca

!--------------------------------------------------------------------------

datar%dd = ksec1(12)
datar%mm = ksec1(11)
datar%yy = ksec1(10) + 100 * (ksec1(21) - 1)
hhr = ksec1(13)

! Gestione time range & time unit
SELECT CASE(ksec1(18))
CASE(0,10)
  IF (ksec1(16) == 0) THEN      ! unintialised analysis
    sca=0
  ELSE                          ! forecast
    IF (ksec1(15) == 1) THEN
      sca = ksec1(16)
    ELSE
      GOTO 9999
    ENDIF
  ENDIF

CASE(1)                         ! initialised analysis
  sca = 0

CASE(2:5)                       ! prodotto riferito a un intervallo
  IF (ksec1(15) == 1) THEN
    sca = ksec1(17)
  ELSE
    GOTO 9999
  ENDIF

CASE(13)                        ! analisi LAMA
  IF (ksec1(15) == 1 .AND. ksec1(16) == 0) THEN
    sca = 0
  ELSE
    GOTO 9999
  ENDIF

CASE DEFAULT                    ! time range non gestio
  GOTO 9998

END SELECT

! Calcolo data-ora di validita'
hh_tot = hhr + sca
data = datar + (hh_tot/24)
hh = MOD(hh_tot,24)
ier = 0

RETURN

! Messaggi d'errore
9999 CONTINUE
WRITE (*,*) "Errore ksec1_valid: time unit indicator non gestito ", &
  ksec1(15)
ier = 1
RETURN

9998 CONTINUE
WRITE (*,*) "Errore ksec1_valid: time range indicator non gestito ", &
  ksec1(18)
ier = 2
RETURN

END SUBROUTINE ksec1_valid

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
!            123456789012345678901234567890123456789012345678901234567890123456789012345678

RETURN
END SUBROUTINE scrive_help


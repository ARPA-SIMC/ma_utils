PROGRAM gacsv2seriet
!--------------------------------------------------------------------------
! Legge uno o piu' file grib_api_csv col tracciato per estazioni arkimet su
! punto (senza header). Per ciascuno dei punti richiesti in filepts, scrive 
! un file con:
! - una riga per ciascuna combinazione reftime-scad (P1) richiesta in 
!   filerow
! - una colonna per ciascuna combinazione var-liv richiesta in filecol
!
! Note:
! - vg6d_getpoint puo' sicuramente ritornare il valore della stessa cella 
!   del modello per due punti distinti, ma se vegono richieste due volte le 
!   stesse coordinate le restituisce una volta sola. 
! - Modifiche ai dati fatte dal passagio in Libsim (vg6d_getpoint gacsv):
!   . codifica dei livelli secondo le tabelle GRIB2
!   . unita' di misura standard Dballe (QA in Kg/m3, Nubi in %)
!
! TODO:
! - Gestione formati ADMS e ISC: write_out_header, write_out_rec; parametri
!   aggiuntivi
! - Verificare che il programma funzioni davvero se viene richiesto due 
!   volte lo stesso punto
! - EOR DOS: con formato csv e dos=F non dovrebbero esserci spazi a fine 
!   record
!
! Altri sviluppi eventuali:
! - discriminare eof da errore di lettura (get_eof_eor o analogo LibSIM)
! - rivedere i messaggi a schermo in modo che il programma possa funzionare 
!   in modalita' STDIO (-> log4fortran su STDERR? livelli info/warning?)
! - verificare il modo migliore per scrivere le date nel formato CSV
! - carattere opzionale DOS=.TRUE. in csv_record_getrecord
! - opzione per verification time invece di reference time (i dati 
!   sarebbero comunque ordinati per reftime e trange, ie. non e' garantito
!   che i verification times siano consecutivi)
!
!                                          Versione 1.7.2 Enrico 31/03/2016
!--------------------------------------------------------------------------

USE file_utilities
USE datetime_class
USE missing_values
USE seriet_utilities
IMPLICIT NONE

! Input namelist e valori di default. 
! Questi valori influenzano vari programmi, non cambiarli !!!
CHARACTER (LEN=20), PARAMETER :: file_nml = "gacsv2seriet.nml"
INTEGER :: out_form = 1
INTEGER :: out_ndec = -2
INTEGER :: qcont = 2
LOGICAL :: libsim = .TRUE.
LOGICAL :: step_yy_mm = .TRUE.
INTEGER :: lab3d = 2
INTEGER :: lab3ddec = 1
CHARACTER (LEN=1) :: xls_dec_sep = "."
LOGICAL :: add_albedo = .FALSE.
LOGICAL :: dir_int =  .TRUE.
LOGICAL :: tem_cel = .FALSE.
LOGICAL :: mo_rec = .FALSE.
LOGICAL :: sw_down = .FALSE.
LOGICAL :: flx_rev = .FALSE.
LOGICAL :: cc_fract = .FALSE.
LOGICAL :: sca_ini = .FALSE.
LOGICAL :: dos = .FALSE.
NAMELIST /param/out_form,out_ndec,qcont,libsim,step_yy_mm,lab3d,lab3ddec, &
  xls_dec_sep,add_albedo,dir_int,tem_cel,mo_rec,sw_down,flx_rev,cc_fract,sca_ini,dos

! File di log
INTEGER, PARAMETER :: iu_qcnt = 10
CHARACTER(LEN=40), PARAMETER :: file_qcnt = "qcnt.log"

! Variabili locali
TYPE(gacsv_report), ALLOCATABLE :: record_in(:)
REAL, ALLOCATABLE :: val_in(:,:),alb_fis(:),z0_fis(:),ff(:),dir(:)
REAL, ALLOCATABLE :: zlev(:,:),zlay(:,:),orog(:)
INTEGER, ALLOCATABLE :: pts_map(:,:),id_vl_uu(:),id_vl_vv(:)
LOGICAL, ALLOCATABLE :: leof(:)

TYPE (csv_record) :: csvline
TYPE (datetime) :: reftime_max(maxqry),reftime_min(maxqry),reftime_req
TYPE (datetime) :: greftime_max,greftime_min
TYPE (timedelta) :: gtd
TYPE(datascad) :: datascad_req,datascad_sav

DOUBLE PRECISION :: req_lon(maxpt),req_lat(maxpt)
REAL :: vmin(maxvl),vmax(maxvl)
REAL :: pct_ok
INTEGER :: varliv_req(6,maxvl),cp2(maxvl),ndec(maxvl)
INTEGER :: tdh(maxqry),p1_min(maxqry),p1_max(maxqry),p1_step(maxqry)
INTEGER :: id_vl_alb,id_vl_z0,p2_sav
INTEGER :: k,k2,k3,krt,ksc,kp,kpt,kf,kvl,kuv,nht,nargs,nfilein
INTEGER :: ios,iret,ier(12),iu,nvl_out
INTEGER :: cnt_read,cnt_xyz_ok,cnt_ok,nok,cnt_qc(5),cnt_head
INTEGER :: nf,npt,nvl,nrrow,nrt,nsc,nuv
INTEGER :: gtdh,gp1_min,gp1_max,gp1_step,p1_req,special_gtd,mm1,mm2,yy1,yy2
CHARACTER (LEN=50+maxvl*9) :: chfmt_seriet
CHARACTER (LEN=500) :: filein(maxqry)
CHARACTER (LEN=500) :: chrec,filepts,filerow,filecol,filefis,chdum
CHARACTER (LEN=80) :: req_label(maxpt)
CHARACTER (LEN=10) :: model(maxvl),varname(maxvl),ch10(3)
CHARACTER (LEN=1) :: next_arg,cheor
LOGICAL :: tvar_alb,tvar_z0,convert

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

next_arg = ""
nargs = 0
nfilein = 0
filein(:) = ""
filepts = ""
filecol = ""
filerow = ""
filefis = ""
convert = .TRUE.

DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-c") THEN
    CALL crea_namelist(file_nml)
    STOP
  ELSE IF (TRIM(chdum) == "-f") THEN
    next_arg = "f"
  ELSE IF (TRIM(chdum) == "-noconv") THEN
    convert = .FALSE.
  ELSE IF (next_arg == "f") THEN
    filefis = chdum
    next_arg = ""
  ELSE 
    IF (nargs == 0) THEN
      filepts = chdum
    ELSE IF (nargs == 1) THEN
      filecol = chdum
    ELSE IF (nargs == 2) THEN
      filerow = chdum
    ELSE 
      nfilein = nfilein + 1
      IF (nfilein > maxqry) GOTO 9989
      filein(nfilein) = chdum
    ENDIF
    nargs = nargs + 1
  ENDIF
ENDDO

IF (nargs < 4 .OR. nfilein < 1) GOTO 9999

!--------------------------------------------------------------------------
! 1.2 Leggo namelist
!

OPEN (UNIT=20, FILE=file_nml, STATUS="OLD", ACTION="READ", IOSTAT=ios)

IF (ios /= 0) THEN
  WRITE (*,'(2x,3a)') "File ",TRIM(file_nml)," non trovato, uso defaults"

ELSE
  READ (20, NML=param, IOSTAT = ios)
! IF (ios /= 0 .OR. out_form < 1 .OR. out_form > 6 .OR. &
  IF (ios /= 0 .OR. out_form < 1 .OR. out_form > 2 .OR. &
    qcont < 0 .OR. qcont > 3) GOTO 9984

  IF (.NOT. libsim) THEN
    WRITE (*,*) "I dati non sono stati prodotti da libsim (opzione libsim=.FALSE.):"
    WRITE (*,*) "disabilito la ri-conversione delle unita' di misura"
    convert = .FALSE.
  ENDIF

  IF (out_form == 3) THEN        ! ADMS meteo
    dir_int     = .TRUE.
    tem_cel     = .TRUE.
    mo_rec      = .TRUE.
    sw_down     = .TRUE.
    flx_rev     = .TRUE.
    cc_fract    = .FALSE.
    dos         = .TRUE.
  ELSE IF (out_form == 4) THEN   ! ADMS background
    dos         = .TRUE.
  ELSE IF (out_form == 5) THEN   ! ISC
    dir_int     = .TRUE.
    tem_cel     = .FALSE.
  ENDIF

! IF (add_albedo .AND. out_form /= 3) THEN
!   WRITE (*,*) "Aggiunta campo albedo gestita solo per ADMS meteo"
  IF (add_albedo) THEN
    WRITE (*,*) "Aggiunta campo albedo non (ancora) gestita"
    add_albedo = .FALSE.
  ENDIF
ENDIF

CLOSE (20)

! Fisso il carattere EOR (per output DOS)
IF (dos) THEN
  cheor = CHAR(13)
ELSE
  cheor = ""
ENDIF

p2_sav = imiss

!--------------------------------------------------------------------------
! 1.3 Leggo da filepts l'elenco dei punti richiesti

OPEN (UNIT=30, FILE=filepts, STATUS="OLD", ERR=9998)
READ (30,'(a)',IOSTAT=ios) chrec
CALL test_header("lspts",chrec,iret)
IF (iret > 0) GOTO 9997

npt = 0
DO
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT
  IF (TRIM(chrec) == "") CYCLE
  npt = npt + 1
  IF (npt > maxpt) GOTO 9987

  CALL init(csvline,RECORD=chrec,NFIELD=nf)
  IF (nf < 2) GOTO 9996
  CALL csv_record_getfield(csvline,FIELD=req_lon(npt),IER=ier(1))
  CALL csv_record_getfield(csvline,FIELD=req_lat(npt),IER=ier(2))
  IF (ANY(ier(1:2) /= 0)) GOTO 9996
  IF (nf >= 3) THEN
    CALL csv_record_getfield(csvline,FIELD=req_label(npt),IER=ier(3))
    IF (ier(3) /= 0) GOTO 9996
  ELSE
    WRITE (req_label(npt),*) npt
  ENDIF
  CALL delete(csvline)
ENDDO
CLOSE (30)

!--------------------------------------------------------------------------
! 1.4 Leggo da filerow l'elenco dei reftime-trange richiesti

OPEN (UNIT=31, FILE=filerow, STATUS="OLD",ERR=9992)
READ (31,'(a)',IOSTAT=ios) chrec
CALL test_header("lsrow",chrec,iret)
IF (iret > 0) GOTO 9991

nrrow = 0
DO
  READ (31,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT
  IF (TRIM(chrec) == "") CYCLE
  nrrow = nrrow + 1
  IF (nrrow > maxqry) GOTO 9986

  CALL init(csvline,RECORD=chrec,NFIELD=nf)
  IF (nf < 6) GOTO 9990
  CALL csv_record_getfield(csvline,FIELD=ch10(1),IER=ier(1))
  reftime_min(nrrow) = datetime_new(SIMPLEDATE=ch10(1))
  CALL csv_record_getfield(csvline,FIELD=ch10(1),IER=ier(2))
  reftime_max(nrrow) = datetime_new(SIMPLEDATE=ch10(1))
  CALL csv_record_getfield(csvline,FIELD=tdh(nrrow),IER=ier(3))
  CALL csv_record_getfield(csvline,FIELD=p1_min(nrrow),IER=ier(4))
  CALL csv_record_getfield(csvline,FIELD=p1_max(nrrow),IER=ier(5))
  CALL csv_record_getfield(csvline,FIELD=p1_step(nrrow),IER=ier(6))
  IF (ANY(ier(1:6)/=0)) GOTO 9990
  IF (tdh(nrrow) < 0 .OR. reftime_max(nrrow) < reftime_min(nrrow) .OR. &
      (reftime_max(nrrow)==reftime_min(nrrow) .AND. tdh(nrrow)/=0) .OR. &
      (reftime_max(nrrow)>reftime_min(nrrow) .AND. tdh(nrrow)==0) .OR. &
      p1_step(nrrow) < 0 .OR. p1_max(nrrow) < p1_min(nrrow) .OR. &
      (p1_max(nrrow)==p1_min(nrrow) .AND. p1_step(nrrow)/=0) .OR. &
      (p1_max(nrrow)>p1_min(nrrow) .AND. p1_step(nrrow)==0) .OR. &
      p1_max(nrrow) < 0 .OR. p1_min(nrrow) < 0) &
      GOTO 9981

  CALL getval(reftime_max(nrrow)-reftime_min(nrrow), AHOUR=nht)

! Controllo che gli intervalli dei valori siano divisibili per gli step
  IF (tdh(nrrow) /= 0) THEN
    IF (MOD(nht,tdh(nrrow)) /= 0) THEN
      IF (tdh(nrrow) == 365*24 .AND. step_yy_mm) THEN
        WRITE (*,'(2x,a,i3)') "Trovato step annuale, record ",nrrow
      ELSE IF ((tdh(nrrow) == 30*24 .OR. tdh(nrrow) == 31*24) .AND. &
               step_yy_mm) THEN
        WRITE (*,'(2x,a,i3)') "Trovato step mensile, record ",nrrow
      ELSE
        WRITE (*,'(2x,a,i3)') "Warning: l'intervallo " // &
        "dei reftime richiesti non e' divisibile per lo step, record ", &
        nrrow
      ENDIF
    ENDIF
  ENDIF

  IF (p1_max(nrrow) /= p1_min(nrrow)) THEN
    IF (MOD(p1_max(nrrow) - p1_min(nrrow), p1_step(nrrow)) /= 0) &
      WRITE (*,*) "Warning: l'intervallo delle scad richieste " // &
      "non e' divisibile per lo step, record ",nrrow
  ENDIF

  CALL delete(csvline)
ENDDO
CLOSE (31)

!--------------------------------------------------------------------------
! 1.5 Calcolo reftime e tranges complessivi

IF (nrrow > 0) THEN
! Calcolo l'unione degli intervalli di reftime indicati nei vari record, 
! e il numero totale di reftime in output
  greftime_max = reftime_max(1)
  greftime_min = reftime_min(1)
  DO k = 1,nrrow
    IF (reftime_max(k) > greftime_max) greftime_max = reftime_max(k)
    IF (reftime_min(k) < greftime_min) greftime_min = reftime_min(k)
  ENDDO
  IF (greftime_max == greftime_min) THEN
    gtdh = 0
  ELSE IF (ALL(tdh(1:nrrow) == 0)) THEN
    GOTO 9979
  ELSE
    gtdh = MINVAL(tdh(1:nrrow), MASK=tdh(1:nrrow)>0)
  ENDIF
  CALL init(gtd, HOUR=gtdh)
  
  special_gtd = 0
  IF (greftime_max == greftime_min) THEN
    nrt = 1
  ELSE
    CALL getval(greftime_max-greftime_min, AHOUR=nht)
    IF (MOD(nht,gtdh) == 0) THEN
      nrt = nht / gtdh + 1
    ELSE
      CALL getval(greftime_min, YEAR=yy1, MONTH=mm1)
      CALL getval(greftime_max, YEAR=yy2, MONTH=mm2)
      IF (step_yy_mm .AND. gtdh == 365*24) THEN
        nrt = yy2-yy1 + 1
        special_gtd = 2
        WRITE (*,'(2x,a)') "Scrivero' output con step di un anno"
      ELSE IF ((gtdh == 30*24 .OR. gtdh == 31*24) .AND. step_yy_mm) THEN
        nrt = (yy2-yy1)*12 + mm2-mm1 + 1
        special_gtd = 1
        WRITE (*,'(2x,a)') "Scrivero' output con step di un mese"
      ELSE 
        nrt = nht / gtdh + 1
        WRITE (*,*) "Warning: l'intervallo " // &
        "complessivo dei reftime richiesti non e' divisibile per lo step"
      ENDIF
    ENDIF
  ENDIF
  
! Calcolo l'unione degli intervalli di timerange indicati nei vari record, 
! e il numero totale di timerange in output
! Lo step in ouput e' il minimo tra gli step in input diversi da zero.
  gp1_max = MAXVAL(p1_max(1:nrrow))
  gp1_min = MINVAL(p1_min(1:nrrow))
  IF (ALL(p1_step(1:nrrow) == 0)) THEN
    gp1_step = 0
  ELSE
    gp1_step = MINVAL(p1_step(1:nrrow), MASK=p1_step(1:nrrow) /=0)
  ENDIF

  IF (gp1_max == gp1_min) THEN
    nsc = 1
  ELSE
    IF (MOD(gp1_max - gp1_min, gp1_step) /= 0) WRITE (*,*) &
      "Warning: l'intervallo complessivo delle scad richieste " // &
      "non e' divisibile per lo step"
    nsc = (gp1_max - gp1_min) / gp1_step + 1
  ENDIF

ELSE ! filerow non contiene record validi
  nrt = 0
  nsc = 0

ENDIF
!--------------------------------------------------------------------------
! 1.6 Leggo da filecol l'elenco dei var-liv richiesti

varliv_req(:,:) = imiss
OPEN (UNIT=32, FILE=filecol, STATUS="OLD", ERR=9995)
READ (32,'(a)',IOSTAT=ios) chrec
CALL test_header("lscol",chrec,iret)
IF (iret > 0) GOTO 9994

nvl = 0
DO
  READ (32,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT
  IF (TRIM(chrec) == "") CYCLE
  nvl = nvl + 1

  IF (nvl > maxvl) GOTO 9988

  CALL init(csvline,RECORD=chrec,NFIELD=nf)
  IF (nf < 12) GOTO 9993
  CALL csv_record_getfield(csvline,FIELD=varliv_req(1,nvl), IER=ier(1))
  CALL csv_record_getfield(csvline,FIELD=varliv_req(2,nvl), IER=ier(2))    
  CALL csv_record_getfield(csvline,FIELD=varliv_req(3,nvl), IER=ier(3))    
  CALL csv_record_getfield(csvline,FIELD=cp2(nvl),          IER=ier(4))
  CALL csv_record_getfield(csvline,FIELD=varliv_req(4,nvl), IER=ier(5))    
  CALL csv_record_getfield(csvline,FIELD=varliv_req(5,nvl), IER=ier(6))    
  CALL csv_record_getfield(csvline,FIELD=varliv_req(6,nvl), IER=ier(7))    
  CALL csv_record_getfield(csvline,FIELD=ndec(nvl),         IER=ier(8))
  CALL csv_record_getfield(csvline,FIELD=vmin(nvl),         IER=ier(9))
  CALL csv_record_getfield(csvline,FIELD=vmax(nvl),         IER=ier(10))
  CALL csv_record_getfield(csvline,FIELD=model(nvl),        IER=ier(11))
  CALL csv_record_getfield(csvline,FIELD=varname(nvl),      IER=ier(12))
  IF (ANY(ier(1:12)/=0)) GOTO 9993
  CALL delete(csvline)
ENDDO
CLOSE (32)

!--------------------------------------------------------------------------
! 1.7 Elaborazioni sull'elenco dei var-liv richiesti

! 1.7.1 Se e' richiesto un output con tracciato rigido (ISC, ADMS) 
!       controllo che i var-liv corrispondano
IF (out_form == 3 .OR. out_form == 4 .OR. out_form == 5) THEN
  CALL check_list_varliv(nvl,varliv_req(1:6,1:nvl),out_form,nvl_out)
  IF (nvl_out /= nvl) THEN
    WRITE (*,*) "Warning: trovati dati incompatibili col formato richiesto, non saranno scritti"
    nvl = nvl_out
  ENDIF
ENDIF

! 1.7.2 Se necessario, cerco tra i parametri richiesti albedo e roughness 
tvar_alb = .FALSE.
IF (sw_down .OR. add_albedo) THEN
  var: DO k = 1,nvl
    alb: DO k2 = 1,nvl_alb

      key: DO k3 = 1,4 ! -999 = wildcard
        IF (varliv_req(k3,k) /= -999 .AND. &
            varliv_alb(k3,k2) /= -999 .AND. &
            varliv_req(k3,k) /= varliv_alb(k3,k2)) CYCLE alb
      ENDDO key
      tvar_alb = .TRUE.
      id_vl_alb = k2
      EXIT var

    ENDDO alb
  ENDDO var
ENDIF

tvar_z0 = .FALSE.
IF (out_form == 5) THEN
  DO k = 1,nvl
  DO k2 = 1,nvl_alb
    IF (ALL(varliv_req(1:4,k) == varliv_z0(1:4,k2))) THEN
      tvar_z0 = .TRUE.
      id_vl_z0 = k2
      EXIT
    ENDIF
  ENDDO
  ENDDO
ENDIF

! 1.7.3 Se e' richiesto il vento scritto come dir/int, trovo le coppie 
!       di var/liv corrispondenti
IF (dir_int) THEN
  ALLOCATE (id_vl_uu(nvl),id_vl_vv(nvl))
  CALL find_uvcomp(nvl,varliv_req(1:6,1:nvl),cp2(1:nvl), &
    nuv,id_vl_uu,id_vl_vv)
ENDIF

! 1.7.4 Se sono richieste delle elaborazioni, aggiusto i parametri relativi
!       a header, formati e controllo di qualita'
DO k = 1,nvl
  IF (dir_int .AND. cp2(k) > 0 .AND. cp2(k) < 500) THEN ! Dir. vento
    varname(k) = "Dir-wind"
    ndec(k) = 0
    vmax(k) = 360.
    vmin(k) = 0.
 
  ELSE IF (dir_int .AND. cp2(k) > 500) THEN             ! Velocita' vento
    varname(k) = "Mod-wind"
    ndec(k) = 1
    vmax(k) = 70.
    vmin(k) = 0.

  ELSE IF (tem_cel .AND. cp2(k) == -1) THEN             ! Temperatura
    varname(k) = "Temp (C)" 
    ndec(k) = 1
    vmax(k) = +50.
    vmin(k) = -80.
  
  ELSE IF (mo_rec .AND. cp2(k) == -2) THEN              ! Monin-Obukov
    varname(k) = "1/MO"
    ndec(k) = 3
    vmax(k) = 1000.
    vmin(k) = -1000.
                                                        ! flussi cal. ADMS
  ELSE IF (flx_rev .AND. out_form == 3 .AND. cp2(k) == -5) THEN
    vmax(k) = 60.
    vmin(k) = -500.

  ELSE IF (sw_down .AND. cp2(k) == -3) THEN             ! SW Budget
    varname(k) = "SW_down"
    ndec(k) = 1
    vmax(k) = 1300.
    vmin(k) = 0.

  ELSE IF (cc_fract .AND. cp2(k) == -4) THEN            ! Cloud Cover fr.
    ndec(k) = 2
    vmax(k) = 1.
    vmin(k) = 0.

  ELSE IF (out_form == 3 .AND. cp2(k) == -4) THEN       ! Cloud Cover oct.
    ndec(k) = 0
    vmax(k) = 8.
    vmin(k) = 0.

  ENDIF
ENDDO

! 1.7.5 Aggiustamenti finali al numero di decimali in output: 
! - se sono richieste elaborazioni su alcuni parametri, fisso un minimo 
! - se sono richiesti piu' di 8 decimali, passo a notazione scientifica
! - se presente, impongo il numero di decimali richiesto dall'utente
IF (mo_rec) THEN
  WHERE (cp2(1:nvl) == -2)
    ndec(1:nvl) = MAX(ndec(1:nvl),3)
  ENDWHERE
ENDIF
WHERE (ndec(1:nvl) > 8)
  ndec(1:nvl) = -1
ENDWHERE
IF (out_ndec == -1 .OR. out_ndec > 8) THEN
  ndec(1:nvl) = -1
ELSE IF (out_ndec > 0) THEN
  ndec(1:nvl) = out_ndec
ENDIF

!--------------------------------------------------------------------------
! 1.8 Se richiesto e se disponibili leggo i campi statici da filefis.
!     Le quote dei livelli possono essere scritte in output solo se le ho 
!     trovate nel file fisiografico.

ALLOCATE (alb_fis(npt),z0_fis(npt),orog(npt))
ALLOCATE (zlev(npt,maxaklev),zlay(npt,maxaklay))

IF(((sw_down .OR. add_albedo) .AND. .NOT. tvar_alb) .OR. &
   (out_form == 5 .AND. .NOT. tvar_z0) .OR. &
   (.NOT. libsim .AND. ANY(varliv_req(4,1:nvl) == 109)) .OR. &
   (.NOT. libsim .AND. ANY(varliv_req(4,1:nvl) == 110)) .OR. &
   (libsim .AND. ANY(varliv_req(4,1:nvl) == 105)) ) &
  CALL read_fisiog_gacsv(filefis,npt,req_lat(1:npt),req_lon(1:npt),libsim,&
    alb_fis,z0_fis,zlev,zlay,orog)

IF ((lab3d == 1 .OR. lab3d == 2) .AND. &
    ((.NOT. libsim .AND. ANY(varliv_req(4,1:nvl) == 109)) .OR. &
     (.NOT. libsim .AND. ANY(varliv_req(4,1:nvl) == 110)) .OR. &
     (libsim .AND. ANY(varliv_req(4,1:nvl) == 105))) .AND. &
    (ALL(zlev(:,:) == rmiss) .AND. ALL(zlay(:,:) == rmiss)) ) THEN
  WRITE (*,'(a)') "Quote dei livelli non disponibili, uso gli indici"
  lab3d = 0
ENDIF

!--------------------------------------------------------------------------
! 1.9 Log a schermo

WRITE (*,'(2x,a)') "gascsv2seriet; richiesti in output: "
WRITE (*,'(2x,i10,1x,a)') npt,"files (punti)"
WRITE (*,'(2x,i10,1x,a)') nvl,"colonne (var-liv)"
WRITE (ch10(1),'(i10)') nrt
WRITE (ch10(2),'(i10)') nsc
WRITE (*,'(2x,i10,1x,5a)') nrt*nsc,"righe (",TRIM(ADJUSTL(ch10(1))),&
  " reftime * ",TRIM(ADJUSTL(ch10(2)))," timerange)"
IF (dir_int) WRITE(*,'(10x,i2,a)') nuv," coppie di componenti del vento"

!==========================================================================
! 2) Leggo ed elaboro filein(s)

!--------------------------------------------------------------------------
! 2.1) Preliminari

! 2.1.1 Alloco gli array di lavoro

! Dati relativi alla data-scad richiesta letti dai files gacsv
ALLOCATE (val_in(npt,nvl))

! Contenuto dell'ultimo record letto (per ciascuno dei files di input)
ALLOCATE (record_in(nfilein))

! Per ciascuno dei punti presenti in ciascuno dei files di input, pts_map 
! contiene il numero del corrispondente punto richiesto (ie. numero di riga
! in filepts). Inoltre vale:
!   0  se per quel punto non sono ancora stati trovati dati 
!  -1  se quel punto non e' richiesto in output
!
! NB: Il primo indice dell'array corrisponde al campo "npoint" in ciascuno
!     dei files gacsv di input.
ALLOCATE (pts_map(maxpt,nfilein))

! Array logico, vale .T. se il file corrispondente e' terminato
ALLOCATE (leof(nfilein))

! Arrayi di lavoro per le elaborazioni che coinvolgono diversi parametri
IF (dir_int) ALLOCATE (ff(npt),dir(npt))

! 2.1.2 Apro i files di INPUT
DO kf = 1,nfilein
  OPEN (UNIT=10+kf, FILE=filein(kf), STATUS="OLD", ERR=9985)
ENDDO

! 2.1.3 Apro il file per il log del quality control
OPEN (UNIT=iu_qcnt, FILE=file_qcnt, STATUS="REPLACE")
CALL init(csvline)
CALL csv_record_addfield(csvline,"point")
CALL csv_record_addfield(csvline,"table")
CALL csv_record_addfield(csvline,"var")
CALL csv_record_addfield(csvline,"val_bad")
CALL csv_record_addfield(csvline,"val_ok")
WRITE (iu_qcnt,'(a)') csv_record_getrecord(csvline)
CALL delete(csvline)

! 2.1.4 Apro i files di OUTPUT e scrivo il loro header
DO k = 1,npt
  iu = 10+maxqry+k
  WRITE (chdum,'(a,i3.3,a)') "seriet_",k,".txt"
  OPEN (UNIT=iu, FILE=chdum, STATUS="REPLACE")
  CALL write_out_header(npt,nvl,k,varliv_req(1:6,1:nvl),model(1:nvl), &
    varname(1:nvl),req_lat(1:npt),req_lon(1:npt),req_label(1:npt), &
    zlev,zlay,orog,iu,out_form,lab3d,lab3ddec,cheor)
ENDDO

! 2.1.5 Costruisco il formato per i record di dati in output
IF (out_form == 1) THEN
  CALL build_out_fmt(nvl,ndec(1:nvl),out_form,chfmt_seriet)
ELSE
  chfmt_seriet = ""
ENDIF

! 2.1.6 Inizializzo le variabili usate nel ciclo principale
DO kf = 1, nfilein
  CALL gacsv_rep_setmiss(record_in(kf), reft="min")
ENDDO
leof(:) = .FALSE.
pts_map(:,:) = 0
cnt_read = 0
cnt_xyz_ok = 0
cnt_ok = 0
cnt_qc(:) = 0
cnt_head = 0

!--------------------------------------------------------------------------
! 2.2 Apro il ciclo principale su reftime/timerange richiesti

DO krt = 1, nrt
DO ksc = 1, nsc

  IF (special_gtd == 1) THEN
    reftime_req = greftime_min + (krt-1) * timedelta_new(MONTH=1)
  ELSE IF (special_gtd == 2) THEN
    reftime_req = greftime_min + (krt-1) * timedelta_new(YEAR=1)
  ELSE 
    reftime_req = greftime_min + (krt-1) * gtd 
  ENDIF

  p1_req = gp1_min + (ksc-1) * gp1_step
  datascad_req = datascad_new(reftime_req,p1_req,imiss,imiss)
  val_in(:,:) = rmiss

!--------------------------------------------------------------------------
! 2.3) Ciclo interno sui files di input
!     Analizzo la data dell'ultimo record letto:
! .1) se e' successiva a quella richiesta, o se sono gia' arrivato alla 
!     fine di questo file, passo al file successivo
! .2) se e' precedente a quella richiesta, leggo record successivi fino a 
!     trovare una data-scad >= a quella richiesta, poi agico come sotto.
! .3) se e' uguale a quella richiesta, cerco di piazzare il dato al posto 
!     giusto dell'array val_in (devo controllare l'indice del punto nel 
!     file gacsv: se e' un punto non richiesto lo salto, se e' la prima 
!     volta che lo trovo confronto le coordinate con quelle dei punti 
!     richiesti in filepts, se e' gia' associato a un punto richiesto salvo
!     il dato)
!     Leggo record successivi finche' non trovo una data-scad diversa.

  files_in: DO kf = 1, nfilein

! 2.3.1 L'ultima data/scad letta e' successiva a quella richiesta, oppure
!       sono gia' arrivato alla fine di questo file
    IF (leof(kf) .OR. record_in(kf)%datascad > datascad_req) CYCLE files_in

! 2.3.2 L'ultima data/scad letta e' precedente a quella richiesta, oppure
!       non ho ancora letto nulla da questo file
    IF (record_in(kf)%datascad < datascad_req) THEN
      DO
        READ (10+kf, '(a)', IOSTAT=ios) chrec
        IF (ios /= 0) THEN    ! dovrebbe essere EOF; gestire errore lettura
          CALL gacsv_rep_setmiss(record_in(kf), reft="max")
          leof(kf) = .TRUE.
          CYCLE files_in
        ELSE IF (INDEX(chrec,"date") /= 0) THEN
          cnt_head = cnt_head + 1
          CYCLE files_in
        ENDIF
        cnt_read = cnt_read + 1
        datascad_sav = record_in(kf)%datascad
        CALL gacsv_rep_read(chrec,record_in(kf),iret)
        IF (iret /= 0) GOTO 9982
        IF (record_in(kf)%datascad < datascad_sav) GOTO 9983

!       Se i dati elaborati devono essere riferiti all'inizio dell'intervallo
!       di elaborazione, controllo che la sua lunghezza non cambi
        IF (sca_ini) THEN
          IF (c_e(p2_sav)) THEN
            IF (record_in(kf)%datascad%p2 /= p2_sav) GOTO 9978
          ELSE
            p2_sav = record_in(kf)%datascad%p2 
          ENDIF
        ENDIF

        IF (record_in(kf)%datascad == datascad_req) EXIT
        IF (record_in(kf)%datascad > datascad_req) CYCLE files_in
      ENDDO
    ENDIF

! 2.3.3 L'ultima data/scad letta corrisponde a quella richiesta

    DO
!     A) Se l'ultimo record letto si riferisce a un punto che compare per 
!     la prima volta in questo file, scorro i punti richiesti finche' non
!     ne trovo uno con le stesse coordinate (pts_map non e' piu' 0).
      IF (pts_map(record_in(kf)%np,kf) == 0) THEN
        DO kpt = 1,npt

!         Escludo che due diversi punti di questo file siano associati al
!         medesimo punto richiesto
          IF (ANY(pts_map(:,kf) == kpt)) CYCLE
          IF (ABS(record_in(kf)%lon - req_lon(kpt)) < coo_eps .AND. &
              ABS(record_in(kf)%lat - req_lat(kpt)) < coo_eps) THEN
            pts_map(record_in(kf)%np,kf) = kpt
            EXIT
          ENDIF
        ENDDO
        IF (kpt > npt) pts_map(record_in(kf)%np,kf) = -1
      ENDIF

!     B) Se l'ultimo record letto si riferisce a un punto richiesto, salvo 
!     il dato, altrimenti passo oltre
      IF (pts_map(record_in(kf)%np,kf) > 0) THEN
        DO kvl = 1,nvl
          IF (ALL(record_in(kf)%varliv(1:6) == varliv_req(1:6,kvl))) THEN
            val_in(pts_map(record_in(kf)%np,kf),kvl) = record_in(kf)%value
            cnt_xyz_ok = cnt_xyz_ok + 1
            EXIT
          ENDIF
        ENDDO
      ENDIF

!     Leggo il record successivo
      READ (10+kf, '(a)', IOSTAT=ios) chrec
      IF (ios /= 0) THEN    ! dovrebbe essere EOF; gestire errore di lettura
        CALL gacsv_rep_setmiss(record_in(kf), reft="max")
        leof(kf) = .TRUE.
        CYCLE files_in
      ELSE IF (INDEX(chrec,"date") /= 0) THEN
        cnt_head = cnt_head + 1
        CYCLE files_in
      ENDIF
      cnt_read = cnt_read + 1
      datascad_sav = record_in(kf)%datascad
      CALL gacsv_rep_read(chrec,record_in(kf),iret)
      IF (iret /= 0) GOTO 9982
      IF (record_in(kf)%datascad < datascad_req .OR. &
          record_in(kf)%datascad < datascad_sav) GOTO 9983
      IF (record_in(kf)%datascad > datascad_req) EXIT

    ENDDO

  ENDDO files_in

!--------------------------------------------------------------------------
! 2.4) Elaboro i dati trovati per la data-scad richiesta


! 2.4.1 Compensazione dei cambiamenti delle unita' di misura fatti da LibSIM
  DO kvl = 1,nvl
    IF (convert .AND. cp2(kvl) == -6) THEN        ! AQ: Kg/m3 -> ug/m3
      WHERE (val_in(:,kvl) /= rmiss)
        val_in(:,kvl) = val_in(:,kvl) * 10**9
      ENDWHERE
    ENDIF
  ENDDO

! 2.4.2 Elaborazioni che dipendono da un solo parametro
  DO kvl = 1,nvl
    IF (tem_cel .AND. cp2(kvl) == -1) THEN       ! Temperatura: K -> C
      WHERE (val_in(:,kvl) /= rmiss)
        val_in(:,kvl) = val_in(:,kvl) - 273.16
      ENDWHERE

    ELSE IF (mo_rec .AND. cp2(kvl) == -2) THEN   ! MO -> 1/MO
      WHERE (val_in(:,kvl) /= rmiss .AND. val_in(:,kvl) /=0)
        val_in(:,kvl) = 1. / val_in(:,kvl)
      ENDWHERE
      WHERE (val_in(:,kvl) == 0.)
        val_in(:,kvl) = rmiss
      ENDWHERE
  
    ELSE IF (flx_rev .AND. cp2(kvl) == -5) THEN  ! SHF, LHF -> -SHF, -LHF
      WHERE (val_in(:,kvl) /= rmiss)
        val_in(:,kvl) = -val_in(:,kvl)
      ENDWHERE
                                                 ! SW budget -> SW incoming
    ELSE IF (sw_down .AND. .NOT. tvar_alb .AND. cp2(kvl) == -3) THEN  
      WHERE (val_in(:,kvl) /= rmiss .AND. alb_fis(:) /= rmiss)
        val_in(:,kvl) = val_in(:,kvl) / &
          (1. - MIN(MAX(0.01 * alb_fis(:),0.),1.))
      ENDWHERE
      WHERE (val_in(:,kvl) /= rmiss .AND. alb_fis(:) == rmiss)
        val_in(:,kvl) = rmiss
      ENDWHERE
                                                 ! CC: % -> frazione
    ELSE IF (cc_fract .AND. cp2(kvl) == -4) THEN
      WHERE (val_in(:,kvl) /= rmiss)
        val_in(:,kvl) = val_in(:,kvl) * 0.01
      ENDWHERE
                                                 ! TCC: % -> ottavi (ADMS)
    ELSE IF (out_form == 3 .AND. cp2(kvl) == -4) THEN
      WHERE (val_in(:,kvl) /= rmiss)
        val_in(:,kvl) = REAL(NINT(val_in(:,kvl) * 0.08))
      ENDWHERE

    ENDIF
  ENDDO

! 2.4.3 Elaborazioni che dipendono da piu' di un paramtero
  IF (dir_int) THEN                              ! U,V -> DD,FF
    DO kuv = 1,nuv
      CALL uv2dirint(npt,val_in(:,id_vl_uu(kuv)),val_in(:,id_vl_vv(kuv)), &
        dir(:),ff(:))
      val_in(:,id_vl_uu(kuv)) = dir(:)
      val_in(:,id_vl_vv(kuv)) = ff(:)
    ENDDO
  ENDIF

  IF (sw_down .AND. tvar_alb) THEN               ! SW budget -> SW incoming
    DO kvl = 1,nvl            
    IF (cp2(kvl) == -3) THEN
      IF (tvar_alb) THEN
        WHERE (val_in(:,kvl) /= rmiss .AND. val_in(:,id_vl_alb) /= rmiss)
          val_in(:,kvl) = val_in(:,kvl) / &
            (1. - MIN(MAX(0.01 * val_in(:,id_vl_alb),0.),1.))
        ELSEWHERE
          val_in(:,kvl) = rmiss    
        ENDWHERE
      ELSE 
        WHERE (val_in(:,kvl) /= rmiss .AND. alb_fis(:) /= rmiss)
          val_in(:,kvl) = val_in(:,kvl) / &
            (1. - MIN(MAX(0.01 * alb_fis(:),0.),1.))
        ELSEWHERE
          val_in(:,kvl) = rmiss
        ENDWHERE
      ENDIF
    ENDIF
    ENDDO
  ENDIF

! NO -> NOx (ADMSb)

! 2.4.4 Controllo di qualita'
  CALL qcontrol(npt,nvl,varliv_req(1:3,1:nvl),cp2(1:nvl),vmin(1:nvl), &
    vmax(1:nvl),qcont,iu_qcnt,val_in,cnt_qc)

!--------------------------------------------------------------------------
! 2.5) Scrivo sui files di output

  DO k = 1,npt
    iu = 10+maxqry+k
    CALL write_out_rec(nvl,val_in(k,1:nvl),datascad_req,iu,out_form, &
      chfmt_seriet,ndec(1:nvl),xls_dec_sep,sca_ini,p2_sav,cheor,nok)
    cnt_ok = cnt_ok + nok
  ENDDO

ENDDO   ! scadenze (ksc)
ENDDO   ! reference times (krt)

!--------------------------------------------------------------------------
! 3) Conclusione

! 3.1 Log a schermo
WRITE (*,'(2x,a)') "Sommario dati elaborati:"
WRITE (ch10(1),'(i10)') cnt_xyz_ok
WRITE (*,'(2x,i10,3a)') cnt_read," dati letti (di cui ", &
  TRIM(ADJUSTL(ch10(1)))," relativi a punti/varliv/datascad richiesti)"
WRITE (ch10(1),'(i10)') npt*nvl*nrt*nsc - cnt_ok
WRITE (ch10(2),'(i10)') cnt_qc(5)
IF (npt*nvl*nrt*nsc /= 0) THEN
  pct_ok = 100. * REAL(cnt_ok) / REAL(npt*nvl*nrt*nsc)
ELSE
  pct_ok = 0.
ENDIF
WRITE (*,'(2x,i10,a,f6.2,5a)') cnt_ok," validi in output (",pct_ok,"%; ", &
  TRIM(ADJUSTL(ch10(1)))," mancanti in output, ", &
  TRIM(ADJUSTL(ch10(2)))," mancanti in input)"
WRITE (ch10(1),'(i10)') cnt_qc(1)
WRITE (ch10(2),'(i10)') cnt_qc(2)
WRITE (ch10(3),'(i10)') cnt_qc(3)
WRITE (*,'(2x,i10,7a)') cnt_qc(4)," dati sospetti (", &
  TRIM(ADJUSTL(ch10(1)))," invariati, ",TRIM(ADJUSTL(ch10(2))), &
  " corretti, ",TRIM(ADJUSTL(ch10(3)))," messi mancanti)"
WRITE (*,'(2x,i10,a)') cnt_head," segmenti (headers)"

IF (cnt_ok+cnt_qc(5)+cnt_qc(3) /= npt*nvl*nrt*nsc .OR. &
    cnt_xyz_ok > cnt_read .OR. SUM(cnt_qc(1:3)) /= cnt_qc(4)) GOTO 9980

! 3.2 Codice di uscita del programma

! Tutti i dati richiesti sono mancanti
IF (cnt_ok ==0) THEN
  STOP 103

! Alcuni dei dati richiesti sono mancanti
ELSE IF (cnt_ok < npt*nvl*nrt*nsc) THEN
  STOP 102

! Trovati tutti i dati richiesti, ma letti anche dati non richiesti
ELSE IF (cnt_read > cnt_ok) THEN
  STOP 101 

! Letti tutti e soli i dati richiesti
ELSE
  STOP

ENDIF

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*)  "Errore nei parametri"
CALL write_help
STOP 1

9998 CONTINUE
WRITE (*,*)  "Errore aprendo ",TRIM(filepts)
STOP 2
9997 CONTINUE
WRITE (*,*)  "Header inatteso in ",TRIM(filepts)
STOP 3
9996 CONTINUE
WRITE (*,'(3a,i3)')  "Errore leggendo ",TRIM(filepts)," Record ",npt+1
WRITE (*,'(a)') TRIM(chrec)
STOP 4

9995 CONTINUE
WRITE (*,*)  "Errore aprendo ",TRIM(filecol)
STOP 5
9994 CONTINUE
WRITE (*,*)  "Header inatteso in ",TRIM(filecol)
STOP 6
9993 CONTINUE
WRITE (*,'(3a,i3)')  "Errore leggendo ",TRIM(filecol)," Record ",nvl+1
WRITE (*,'(a)') TRIM(chrec)
STOP 7

9992 CONTINUE
WRITE (*,*)  "Errore aprendo ",TRIM(filerow)
STOP 8
9991 CONTINUE
WRITE (*,*)  "Header inatteso in ",TRIM(filerow)
STOP 9
9990 CONTINUE
WRITE (*,'(3a,i3)')  "Errore leggendo ",TRIM(filerow)," Record ",nrrow+1
WRITE (*,'(a)') TRIM(chrec)
STOP 10

9989 CONTINUE
WRITE (*,*) &
  "Richiesti troppi input files: aumentare maxqry (ora ",maxqry,")"
STOP 11
9988 CONTINUE
WRITE (*,*) &
  "Richieste troppe combinazioni var-liv: aumentare maxvl (ora ",maxvl,")"
STOP 12
9987 CONTINUE
WRITE (*,*) "Richiesti troppi punti: aumentare maxpt (ora ",maxpt,")"
STOP 13
9986 CONTINUE
WRITE (*,*) "Troppi record in filerow, aumentare maxqry (ora ",maxqry,")"
STOP 14

9985 CONTINUE
WRITE (*,*) "Errore apertura o file vuoto, file gacsv n.ro ",kf," ",TRIM(filein(kf))
STOP 15

9984 CONTINUE
WRITE (*,*) "Parametri namelist illegali o non gestiti"
STOP 16

9983 CONTINUE
WRITE (*,*) "datascad non progressiva, file n.ro ",kf," ",TRIM(filein(kf))
CALL getval(datascad_req%reftime,SIMPLEDATE=ch10(1))
WRITE (*,'(2a,2x,a,i3.3)') "Datascad attesa:  ", &
  ch10(1),"+",datascad_req%p1
CALL getval(record_in(kf)%datascad%reftime,SIMPLEDATE=ch10(1))
WRITE (*,'(2a,2x,a,i3.3)') "Datascad trovata: ", &
  ch10(1),"+",record_in(kf)%datascad%p1
STOP 17

9982 CONTINUE
CALL getval(reftime_req,SIMPLEDATE=ch10(1))
WRITE (*,'(2a)') "Errore leggendo file ",TRIM(filein(kf))
WRITE (*,'(3a,i3.3)') " datascad richiesta: ",ch10(1)," +",p1_req
WRITE (*,'(a)') "Record letto: >-",TRIM(chrec),"-<"
STOP 18

9981 CONTINUE
WRITE (*,*) "Record illegale in ",TRIM(filerow)," numero ",nrrow
WRITE (*,*) TRIM(chrec)
IF (tdh(nrrow) < 0) WRITE (*,*) "tdh <0"
IF (reftime_max(nrrow) < reftime_min(nrrow)) &
  WRITE (*,*) "reftime_max < reftime_min"
IF (reftime_max(nrrow)==reftime_min(nrrow) .AND. tdh(nrrow)/=0) THEN
  WRITE (*,*) "reftime_max = reftime_min e step /= 0"
  WRITE (*,'(5a,i4)') "max ",to_char(reftime_max(nrrow)), &
    " min ",to_char(reftime_min(nrrow)), &
    " tdh ",tdh(nrrow)
ENDIF
IF (reftime_max(nrrow)>reftime_min(nrrow) .AND. tdh(nrrow)==0) &
  WRITE (*,*) "reftime_max /= reftime_min e step = 0"
IF (p1_step(nrrow) < 0) WRITE (*,*) "p1_step < 0"
IF (p1_max(nrrow) < p1_min(nrrow)) WRITE (*,*) "p1_max < p1_min"
IF (p1_max(nrrow)==p1_min(nrrow) .AND. p1_step(nrrow)/=0) &
  WRITE (*,*) "p1_max = p1_min e p1_step /= 0"
IF (p1_max(nrrow)>p1_min(nrrow) .AND. p1_step(nrrow)==0) &
  WRITE (*,*) "p1_max /= p1_min e p1_step = 0"
IF (p1_max(nrrow) < 0) WRITE (*,*) "p1_max < 0"
IF (p1_min(nrrow) < 0) WRITE (*,*) "p1_min < 0"
STOP 19

9980 CONTINUE
WRITE (*,*) "Warning: Errore nel bilancio del numero di dati elaborati"
STOP 20

9979 CONTINUE
WRITE (*,*) "Impossibile calcolare lo step dei reference time in ouput:"
WRITE (*,*) "i segmenti di dati richiesti si riferiscono a istanti sparsi"
STOP 21

9978 CONTINUE
WRITE (*,*) "L'opzione sca_ini richiede che tutti i dati abbiano la stesso periodo"
WRITE (*,*) "di elaborazione. Trovati: ",p2_sav,record_in(kf)%datascad%p2
STOP 22

END PROGRAM gacsv2seriet

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE check_list_varliv(nvl_in,varliv,out_form,nvl_out) 
!
! Modifica la lista dei varliv, togliendo i parametri non gestiti per il 
! formato richiesto. Deve essere chiamata prima delle subroutines find_*
!
USE missing_values
IMPLICIT NONE

INTEGER, INTENT(IN) :: nvl_in,out_form
INTEGER, INTENT(INOUT) :: varliv(6,nvl_in)
INTEGER, INTENT(OUT) :: nvl_out

INTEGER :: varliv_work(6,nvl_in),k,k2

!--------------------------------------------------------------------------
! Paramtri gestiti per gli output con tracciato rigido

INTEGER, PARAMETER :: nok_isc = 7
INTEGER, PARAMETER :: vlok_isc(6,nok_isc) = RESHAPE((/ &
   -1,200, 33,105, 10, 0, &
   -1,200, 34,105, 10, 0, &
   -1,200, 11,105,  2, 0, &
   -1,200,100,  1,  0, 0, &
   -1,200,101,  1,  0, 0, &
   -1,200,102,  1,  0, 0, &
   -1,200,103,  1,  0, 0/), &
   (/6,nok_isc/))

INTEGER, PARAMETER :: nok_adm = 21
INTEGER, PARAMETER :: vlok_adm(6,nok_adm) = RESHAPE((/ &
   -1,200, 33,105, 10, 0, &
   -1,200, 34,105, 10, 0, &
   -1,200, 11,105,  2, 0, &
   -1,200,102,  1,  0, 0, &
   -1,200,103,  1,  0, 0, &
   -1,200,110,  1,  0, 0, &
   -1,200,113,  1,  0, 0, &
   -1,200,114,  1,  0, 0, &
   -1,  2, 33,105, 10, 0, &
   -1,  2, 34,105, 10, 0, &
   -1,  2, 11,105,  2, 0, &
   -1,  2,212,105, 10, 0, &
   -1,  2,200,105, 10, 0, &
   -1,  2,210,105, 10, 0, &
   -1,  2,111,105, 10, 0, &
   -1,  2,122,105, 10, 0, &
   -1,  2, 71,105, 10, 0, &
   -1,  2, 84,105, 10, 0, &
   -1,  2, 61,105, 10, 0, &
   -1,  2, 33,105, 10, 0, &
   -1,  2, 33,105, 10, 0/), &
   (/6,nok_adm/))

INTEGER, PARAMETER :: nok_adb = 8
INTEGER, PARAMETER :: vlok_adb(6,nok_adb) = RESHAPE((/ &
   -1,200,151,109,  1, 0, &
   -1,200,152,109,  1, 0, &
   -1,200,209,109,  1, 0, &
   -1,200,153,109,  1, 0, &
   -1,200,154,109,  1, 0, &
   -1,200,155,109,  1, 0, &
   -1,200,220,109,  1, 0, &
   -1,200,221,109,  1, 0/), &
   (/6,nok_adb/))

!--------------------------------------------------------------------------
! Scorro i var-liv in input, travaso nell'array varliv_work solo quelli 
! che sono gestiti per il formato richiesto.

nvl_out = 0
DO k = 1,nvl_in
  IF (out_form == 3) THEN
    DO k2 = 1,nok_adm
      IF (ALL(vlok_adm(1:6,k2) == varliv(1:6,k) .OR. &
              vlok_adm(1:6,k2) == -1)) THEN
        nvl_out = nvl_out + 1
        varliv_work(1:6,nvl_out) = varliv(1:6,k)
        EXIT
      ENDIF
    ENDDO

  ELSE IF (out_form == 4) THEN
    DO k2 = 1,nok_adb
      IF (ALL(vlok_adb(1:6,k2) == varliv(1:6,k) .OR. &
              vlok_adb(1:6,k2) == -1)) THEN
        nvl_out = nvl_out + 1
        varliv_work(1:6,nvl_out) = varliv(1:6,k)
        EXIT
      ENDIF
    ENDDO

  ELSE IF (out_form == 5) THEN
    DO k2 = 1,nok_isc
      IF (ALL(vlok_isc(1:6,k2) == varliv(1:6,k) .OR. &
              vlok_isc(1:6,k2) == -1)) THEN
        nvl_out = nvl_out + 1
        varliv_work(1:6,nvl_out) = varliv(1:6,k)
        EXIT
      ENDIF
    ENDDO

  ENDIF
ENDDO

varliv(:,:) = imiss
varliv(1:6,1:nvl_out) = varliv_work(1:6,1:nvl_out)

RETURN
END SUBROUTINE check_list_varliv

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE find_uvcomp(nvl,varliv,cp2,nuv,id_vl_uu,id_vl_vv)
!
! Trova tra i parametri richiesti le coppie di componenti del vento
!
IMPLICIT NONE

! Parametri della subroutine
INTEGER, INTENT(IN) :: nvl,varliv(6,nvl),cp2(nvl)
INTEGER, INTENT(OUT) :: nuv,id_vl_uu(nvl),id_vl_vv(nvl)

! Variabili locali
INTEGER :: k,k2

!--------------------------------------------------------------------------

nuv = 0
ucomp: DO k = 1,nvl
  IF (cp2(k) <= 0 .OR. cp2(k) >= 500) CYCLE

  DO k2 = 1,nvl
    IF (ALL(varliv((/1,2,4,5,6/),k2) == varliv((/1,2,4,5,6/),k)) .AND. &
        varliv(3,k2) == cp2(k) ) THEN
      nuv = nuv + 1
      id_vl_uu(nuv) = k
      id_vl_vv(nuv) = k2
      CYCLE ucomp
    ENDIF
  ENDDO

ENDDO ucomp

RETURN
END SUBROUTINE find_uvcomp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_out_header(npt,nvl,kpt,varliv,model,varname,req_lat, &
  req_lon,req_label,zlev,zlay,orog,iu,out_form,lab3d,lab3ddec,cheor)
!
! Scrive l'header su un file di output gia' aperto
!
USE file_utilities
USE missing_values
USE seriet_utilities
IMPLICIT NONE

! Parametri della subroutine
INTEGER, INTENT(IN) :: npt,nvl,kpt,varliv(6,nvl),iu,out_form,lab3d,lab3ddec
CHARACTER (LEN=10), INTENT(IN) :: model(nvl),varname(nvl)
DOUBLE PRECISION, INTENT(IN) :: req_lon(npt),req_lat(npt)
REAL, INTENT(IN) :: zlev(npt,maxaklev),zlay(npt,maxaklay),orog(npt)
CHARACTER (LEN=80), INTENT(IN) :: req_label(npt)
CHARACTER (LEN=1), INTENT(IN) :: cheor

! Variabili locali
TYPE(csv_record) :: csvline
REAL :: zid
INTEGER :: k
CHARACTER (LEN=250) :: chfmt
CHARACTER (LEN=17) :: ch17
CHARACTER (LEN=10) :: str_lev(nvl)
CHARACTER (LEN=3) :: tipo_lev
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! 1) Costruisco le stringhe richieste da diversi formati

! 1.2) Descrittori relativi al livelli
IF (out_form == 1 .OR. out_form == 2) THEN
  IF (lab3d == 0) THEN
    ch17 = "Id livello       "
  ELSE IF (lab3d == 1) THEN
    ch17 = "Livello (m SLM)  "
  ELSE IF (lab3d == 2) THEN
    ch17 = "Livello (m Sup.) "
  ENDIF

  DO k = 1,nvl
    IF (varliv(4,k) == 1) THEN
      tipo_lev = "sup"                     ! Surface
      zid = 0.

    ELSE IF (varliv(4,k) == 256) THEN
      tipo_lev = "cld"                     ! Cloud groups
      zid = varliv(6,k)

    ELSE IF (varliv(4,k) == 103) THEN
      tipo_lev = "sup"                     ! Specified height above ground
      zid = REAL(varliv(5,k)) / 1000.

    ELSE IF (varliv(4,k) == 101) THEN
      tipo_lev = "msl"                     ! Mean Sea Level
      zid = 0.

    ELSE IF (varliv(4,k) == 105 .AND. &
      (varliv(5,k) == varliv(6,k) .OR. .NOT. c_e(varliv(6,k))) ) THEN 
      tipo_lev = "lev"                     ! Hybrid level
      IF (varliv(5,k) > 0 .AND. varliv(5,k) <= maxaklev .AND. &
          lab3d == 1) THEN
        zid = zlev(kpt,varliv(5,k))
      ELSE IF (varliv(5,k) > 0 .AND. varliv(5,k) <= maxaklev .AND. &
          lab3d == 2 .AND. orog(kpt) /= rmiss) THEN
        zid = zlev(kpt,varliv(5,k)) - orog(kpt)
      ELSE
        zid = REAL(varliv(5,k))
      ENDIF

    ELSE IF (varliv(4,k) == 105 .AND. varliv(5,k) /= varliv(6,k)) THEN 
      tipo_lev = "lay"                     ! Hybrid layer
      IF (varliv(5,k) > 0 .AND. varliv(5,k) <= maxaklev .AND. &
          lab3d == 1) THEN
        zid = zlay(kpt,varliv(5,k))
      ELSE IF (varliv(5,k) > 0 .AND. varliv(5,k) <= maxaklev .AND. &
          lab3d == 2 .AND. orog(kpt) /= rmiss) THEN
        zid = zlay(kpt,varliv(5,k)) - orog(kpt)
      ELSE
        zid = REAL(varliv(5,k))
      ENDIF

    ELSE IF (varliv(4,k) == 106) THEN
      tipo_lev = "grd"                     ! Layer in the ground
      IF (c_e(varliv(6,k))) THEN
        zid = -REAL(varliv(6,k)) / 1000.
      ELSE
        zid = -REAL(varliv(5,k)) / 1000.
      ENDIF

    ELSE IF (varliv(4,k) == 100) THEN
      tipo_lev = "prs"                     ! Isobaric surface
      zid = REAL(varliv(5,k))

    ELSE
      tipo_lev = "xxx"
      zid = REAL(varliv(5,k))

    ENDIF

    IF (lab3d == 0) THEN
!     IF (NINT(zid) < 100 .AND. ABS(REAL(NINT(zid))-zid) > 0.001) THEN 
      IF (lab3ddec == 2 .OR. &
          (lab3ddec == 1 .AND. zid < 1. .AND. ABS(zid) > 0.001)) THEN
        WRITE (str_lev(k),'(a3,a1,f6.3)') tipo_lev,"_",zid
      ELSE
        IF (zid >= 0) THEN
          WRITE (str_lev(k),'(a3,a1,i6.6)') tipo_lev,"_",NINT(zid)
        ELSE
          WRITE (str_lev(k),'(a3,a1,i6.6)') tipo_lev,"_",NINT(-zid)
        ENDIF 
      ENDIF

    ELSE IF (lab3d == 1 .OR. lab3d == 2) THEN
!     IF (NINT(zid) < 100 .AND. ABS(REAL(NINT(zid))-zid) > 0.001) THEN 
      IF (lab3ddec == 2 .OR. &
          (lab3ddec == 1 .AND. zid < 1. .AND. ABS(zid) > 0.001)) THEN
        WRITE (str_lev(k),'(f10.3)') zid
      ELSE
        IF (zid >= 0) THEN
          WRITE (str_lev(k),'(i10)') NINT(zid)
        ELSE
          WRITE (str_lev(k),'(i10)') -NINT(-zid)
        ENDIF
      ENDIF

    ENDIF

  ENDDO
ENDIF

!--------------------------------------------------------------------------
! 2) Scrivo gli header nel formato richiesto

! 2.1 Formato seriet
IF (out_form == 1) THEN
  WRITE (chfmt,'(4(a,i2),a)') "(a,f",coo_ndec+5,".",coo_ndec,",1x,f", &
    coo_ndec+5,".",coo_ndec,",1x,a,1x,2a)"
  WRITE (iu,chfmt) "Punto: ", &                                          !1
    req_lon(kpt),req_lat(kpt),"!",TRIM(req_label(kpt)),TRIM(cheor)
  WRITE (iu,'(a)') TRIM(cheor)                                           !2

  IF (nvl > 0) THEN
    WRITE (chfmt,'(a,i3,a)') "(a17,",nvl,"(1x,a10),a)"
  ELSE
    chfmt = "(a17,a)"
  ENDIF
  WRITE (iu,chfmt) "Modello          ",ADJUSTR(model(1:nvl)),TRIM(cheor) !3
  WRITE (iu,chfmt) ch17,str_lev(1:nvl),TRIM(cheor)                       !4
  WRITE (iu,'(a)') TRIM(cheor)                                           !5
  WRITE (iu,chfmt) "gg/mm/aaaa hh sca",ADJUSTR(varname(1:nvl)),TRIM(cheor)!6

! 2.2 Formato CSV
ELSE IF (out_form == 2) THEN
  CALL init(csvline)                                             ! record 1
  CALL csv_record_addfield(csvline,req_lon(kpt))
  CALL csv_record_addfield(csvline,req_lat(kpt))
  CALL csv_record_addfield(csvline,TRIM(req_label(kpt)))
  WRITE (iu,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

  CALL init(csvline)                                             ! record 2
  CALL csv_record_addfield(csvline,"Model")
  CALL csv_record_addfield(csvline,"")
  CALL csv_record_addfield(csvline,"")
  DO k = 1,nvl
    CALL csv_record_addfield(csvline,TRIM(ADJUSTL(model(k))))
  ENDDO
  WRITE (iu,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

  CALL init(csvline)                                             ! record 3
  CALL csv_record_addfield(csvline,"Level")
  CALL csv_record_addfield(csvline,"")
  CALL csv_record_addfield(csvline,"")
  DO k = 1,nvl
    CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_lev(k))))
  ENDDO
  WRITE (iu,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)          !

  CALL init(csvline)                                             ! record 4
  CALL csv_record_addfield(csvline,"Simpledate")
  CALL csv_record_addfield(csvline,"Hour")
  CALL csv_record_addfield(csvline,"P1")
  DO k = 1,nvl
    CALL csv_record_addfield(csvline,TRIM(ADJUSTL(varname(k))))
  ENDDO
  WRITE (iu,'(2a)') csv_record_getrecord(csvline),cheor
  CALL delete(csvline)

ENDIF

RETURN
END SUBROUTINE write_out_header

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE build_out_fmt(nvl,ndec,out_form,chfmt)
!
! Costruisce il formato per i record dati in output
!
USE seriet_utilities
IMPLICIT NONE

INTEGER, INTENT(IN) :: nvl,ndec(nvl),out_form
CHARACTER (LEN=50+maxvl*9), INTENT(OUT) :: chfmt

! Varibili locali
INTEGER :: k
CHARACTER(LEN=8) :: ch8

!--------------------------------------------------------------------------

! 1) Formato Seriet
IF (out_form == 1) THEN
  chfmt = "(2(i2.2,a1),i4.4,a1,i2.2,a1,i3.3,"
  DO k = 1,nvl
    IF (ndec(k) >= 0) THEN
      WRITE (ch8,'(a7,i1)') "1x,f10.",ndec(k)
    ELSE
      ch8 = "1x,e10.3"
    ENDIF
    chfmt = TRIM(chfmt) // ch8 // ","
  ENDDO
  chfmt = TRIM(chfmt) // "a)"
ENDIF

RETURN
END SUBROUTINE build_out_fmt

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_out_rec(nvl,values,ds,iu,out_form,chfmt,ndec, &
  xls_dec_sep,sca_ini,p2_sav,cheor,nok)
!
! Scrive su uno dei files di output un record (ie. dati relativi a una 
! coppia reference time - timerange)
!
USE file_utilities
USE datetime_class
USE missing_values
USE seriet_utilities
IMPLICIT NONE

INTEGER, INTENT(IN) :: nvl,iu,out_form
REAL, INTENT(IN) :: values(nvl)
TYPE (datascad), INTENT(IN) :: ds
CHARACTER (LEN=50+maxvl*9), INTENT(IN) :: chfmt
INTEGER, INTENT(IN) :: ndec(nvl),p2_sav
CHARACTER (LEN=1), INTENT(IN) :: xls_dec_sep,cheor
LOGICAL, INTENT(IN) :: sca_ini
INTEGER, INTENT(OUT) :: nok

! Variabili locali
TYPE(csv_record) :: csvline
TYPE (datascad) :: ds_out
TYPE (timedelta) :: td
REAL :: values_out(nvl)
INTEGER :: yy,mm,dd,hh,p1,p2,k
CHARACTER (LEN=14+maxvl*11) :: out_rec
CHARACTER (LEN=10) :: ch10

!--------------------------------------------------------------------------

nok = COUNT(c_e(values(1:nvl)))
WHERE (c_e(values(1:nvl)))
  values_out(1:nvl) = values(1:nvl)
ELSEWHERE
  WHERE(ndec(1:nvl) >= 0)
    values_out(1:nvl) = rmis_ser
  ELSEWHERE
    values_out(1:nvl) = rmis_sex
  ENDWHERE
ENDWHERE

ds_out = ds
IF (sca_ini) THEN
! Analisi non istantanee
  IF (c_e(p2_sav) .AND. ds_out%p1 == 0 .AND. p2_sav > 0) THEN
    td = timedelta_new(HOUR=p2_sav)
    ds_out%reftime = ds_out%reftime - td
! Previsioni non istantanee
  ELSE IF (c_e(p2_sav) .AND. ds_out%p1 > 0 .AND. p2_sav <= ds_out%p1) THEN
    ds_out%p1 = ds_out%p1 - p2_sav
  ENDIF
ENDIF

! 1) Formato Seriet
IF (out_form == 1) THEN
  CALL getval(ds_out%reftime, YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
  WRITE (out_rec,chfmt) dd,"/",mm,"/",yy," ",hh," ",ds_out%p1, &
    values_out(1:nvl),TRIM(cheor)

! Se richesto, sostiuisco il separatore decimale
  IF (xls_dec_sep /= ".") THEN
    p1 = 1
    DO
      p2 = INDEX(out_rec(p1:),".")
      IF (p2 == 0) EXIT
      out_rec(p2+p1-1:p2+p1-1) = xls_dec_sep
      p1 = p2 + p1
    ENDDO
  ENDIF  

  WRITE (iu,'(a)') TRIM(out_rec)

! 2) Formtato CSV

ELSE IF (out_form == 2) THEN
  CALL getval(ds_out%reftime, SIMPLEDATE=ch10)
  CALL init(csvline)
  CALL csv_record_addfield(csvline,ch10(1:8))
  CALL csv_record_addfield(csvline,ch10(9:10))
  CALL csv_record_addfield(csvline,ds_out%p1)
  DO k = 1,nvl
    CALL csv_record_addfield(csvline,values_out(k))
  ENDDO
  WRITE (iu,'(2a)') csv_record_getrecord(csvline),cheor
  CALL delete(csvline)

ENDIF

RETURN
END SUBROUTINE write_out_rec

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE qcontrol(npt,nvl,id_var,cp2,vmin,vmax,qcont,iu,val_in,cnt_qc)
!--------------------------------------------------------------------------
! Controlla se i dati estratti rientrano tra i limiti min e max accettabili
! I dati che escono dai limiti sono comunque segnalati sul file di log,
! quello che viene scritto nei files di output dipende dal valore di qcont:
! 0: lascia invariati tutti i dati
! 1: mette mancanti tutti i dati
! 2: corregge i dati (presumibilmente) errati per troncamento grib, mette
!    mancanti gli altri
! 3: corregge tutti i dati, mettendo il valore massimo (minimo) consentito
!
! Il vettore cnt_qc contiene:
! 1: dati sospetti e invariati
! 2: dati sospetti e modificati
! 3: dati sospetti e messi mancanti
! 4: totale dati sospetti
! 5: dati mancanti in input
!--------------------------------------------------------------------------

USE file_utilities
USE missing_values
IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: npt,nvl,id_var(3,nvl),cp2(nvl),qcont,iu
INTEGER, INTENT(INOUT) :: cnt_qc(5)
REAL, INTENT(IN) :: vmin(nvl),vmax(nvl)
REAL, INTENT(INOUT) :: val_in(npt,nvl)

! Variabili locali
TYPE (csv_record) :: csvline
REAL :: val_ok
INTEGER :: kpt,kvl,kv
LOGICAL :: correct

!--------------------------------------------------------------------------
! Elenco dei parametri per cui i valori sospetti sono (probabilmente) dovuti
! a errori di troncamento grib. 
! La flag qc2_check determina quali estremi controllare:
! "l" solo il limite inferiore, "h" solo quello superiore, "b" entrambi

INTEGER, PARAMETER :: qc2_npar = 24
INTEGER, PARAMETER :: qc2_tab(qc2_npar) = (/ &
  002,002,002,002,002,201,201, &                    ! precipitazioni
  201,201,201,201,002,002, &                        ! cluod water
  200,002,002,002,002, &                            ! clouds
  002,201,201, &                                    ! radiazione visibile
  002,200,002/)                                     ! Mod-wind, RH
INTEGER, PARAMETER :: qc2_var(qc2_npar) = (/ &
  061,078,079,152,153,102,113, &                    ! precipitazioni
  031,033,035,036,150,151, &                        ! cluod water
  114,071,073,074,075, &                            ! clouds
  111,022,023, &                                    ! radiazione visibile
  033,033,052/)                                     ! Mod-wind, RH
CHARACTER(LEN=1), PARAMETER :: qc2_check(qc2_npar) = (/ &
  "l","l","l","l","l","l","l", &                    ! precipitazioni
  "l","l","l","l","l","l", &                        ! cluod water
  "b","b","b","b","b", &                            ! clouds
  "l","l","l", &                                    ! radiazione visibile
  "l","l","b"/)                                     ! Mod-wind, RH

!--------------------------------------------------------------------------

DO kvl = 1, nvl
DO kpt = 1, npt

! Dato mancante
  IF (.NOT. c_e(val_in(kpt,kvl))) THEN
    val_in(kpt,kvl) = rmiss
    cnt_qc(5) = cnt_qc(5) + 1

! Dato sospetto
  ELSE IF (val_in(kpt,kvl)<vmin(kvl) .OR. val_in(kpt,kvl)>vmax(kvl)) THEN
    cnt_qc(4) = cnt_qc(4) + 1

!   Lascio tutto invariato
    IF (qcont == 0) THEN
      val_ok = val_in(kpt,kvl)
      cnt_qc(1) = cnt_qc(1) + 1

!   metto tutto mancante
    ELSE IF (qcont == 1) THEN
      val_ok = rmiss
      cnt_qc(3) = cnt_qc(3) + 1

!   correggo alcuni parametri, metto gli altri mancanti
    ELSE IF (qcont == 2) THEN
      correct = .FALSE.
      DO kv = 1,qc2_npar
        IF (id_var(2,kvl) == qc2_tab(kv) .AND. &
            id_var(3,kvl) == qc2_var(kv)) THEN
          IF (val_in(kpt,kvl) < vmin(kvl) .AND. &
              (qc2_check(kv) == "l" .OR. qc2_check(kv) == "b")) THEN
            val_ok = vmin(kvl)
            cnt_qc(2) = cnt_qc(2) + 1
            correct = .TRUE.
          ELSE IF (val_in(kpt,kvl) > vmax(kvl) .AND. &
              (qc2_check(kv) == "h" .OR. qc2_check(kv) == "b")) THEN
            val_ok = vmax(kvl)
            cnt_qc(2) = cnt_qc(2) + 1
            correct = .TRUE.
          ENDIF
          EXIT
        ENDIF
      ENDDO
      IF (.NOT. correct) THEN
        val_ok = rmiss
        cnt_qc(3) = cnt_qc(3) + 1
      ENDIF

!   correggo tutto
    ELSE IF (qcont == 3) THEN
      IF (val_in(kpt,kvl) < vmin(kvl)) THEN
        val_ok = vmin(kvl)
        cnt_qc(2) = cnt_qc(2) + 1
      ELSE IF (val_in(kpt,kvl) > vmax(kvl)) THEN
        val_ok = vmax(kvl)
        cnt_qc(2) = cnt_qc(2) + 1
      ENDIF

    ENDIF

!   scrivo sul file di log
    CALL init(csvline)
    CALL csv_record_addfield(csvline,kpt)
    CALL csv_record_addfield(csvline,id_var(2,kvl))
    CALL csv_record_addfield(csvline,id_var(3,kvl))
    CALL csv_record_addfield(csvline,val_in(kpt,kvl))
    CALL csv_record_addfield(csvline,val_ok)
    WRITE (iu,'(a)') csv_record_getrecord(csvline)
    CALL delete(csvline)

!   Modifico il dato in output
    val_in(kpt,kvl) = val_ok

  ENDIF
ENDDO
ENDDO

RETURN
END SUBROUTINE qcontrol

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE uv2dirint(nval,u,v,dir,mod)
!
! Dati i vettori di componenti u e v, ritona quelli di direzione e modulo
! Se una componente e' mancante dir e mod sono mancanti. 
! Se u=v=0, mette mod=dir=0
!
USE missing_values
IMPLICIT NONE

INTEGER, INTENT(IN):: nval
REAL, INTENT(IN) :: u(nval),v(nval)
REAL, INTENT(OUT) :: dir(nval),mod(nval)

REAL, PARAMETER :: dtr = 180./3.141592654
INTEGER k

!-------------------------------------------------------------------------
DO k = 1,nval

  IF (u(k) == rmiss .OR. v(k) == rmiss .OR. &
      u(k) == -1.e30 .OR. v(k) == -1.e30 .OR. &
      u(k) == -1.5e15 .OR. v(k) == -1.5e15) THEN
    dir(k) = rmiss
    mod(k) = rmiss
    CYCLE
  ENDIF

  IF      (u(k) <= 0. .AND. v(k) < 0.) THEN    ! 0 - 90
    dir(k) = dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) < 0. .AND. v(k) >= 0.) THEN    ! 90 - 180
    dir(k) = 90. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) >= 0. .AND. v(k) > 0.) THEN    ! 180 - 270
    dir(k) = 180. + dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) > 0. .AND. v(k) <= 0.) THEN    ! 270 - 360
    dir(k) = 270. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) == 0. .AND. v(k) == 0.) THEN
    dir(k) = 0.
  ENDIF
 
  mod(k) = SQRT (u(k)*u(k) + v(k)*v(k))

ENDDO

RETURN
END SUBROUTINE uv2dirint

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: gacsv2seriet.exe [-h] [-c] [-f filefis] [-noconv]"
WRITE (*,*) "     filepts filecol filerow filein1 [filein...]" 
WRITE (*,*)
WRITE (*,*) "Legge 1+ file grib_api_csv con tracciato per estrazioni su punto (no header)"
WRITE (*,*) "Per ciascuno dei punti richiesti in filepts, scrive un file con:"
WRITE (*,*) "- una colonna per ciascuna combinazione var-liv richiesta in filecol       "
WRITE (*,*) "- una riga per ciascuna combinazione reftime-scad (P1) richiesta in filerow"
WRITE (*,*) "  (se filerow contine piu' righe, cosidera l'unione dei dati richiesti)"
WRITE (*,*) ""
WRITE (*,*) "-f fielfis: legge da filefis i parametri fisiogrfici relativi ai punti "
WRITE (*,*) "  richiesti; filefis e' in formato gacsv e puo' contenere i campi "
WRITE (*,*) "  albedo, z0, orog, zlay, zlev"
WRITE (*,*) "-noconv: non compensa le conversioni di unita' di misura fatte da Libsim;"
WRITE (*,*) "  usare in concomitanza con l'opzione --noconvert di vg6d_getpoint"
WRITE (*,*) ""
WRITE (*,*) "Codice di uscita del programma (echo $?):"
WRITE (*,*) "    0: letti tutti e soli i dati richiesti"
WRITE (*,*) "  101: trovati tutti i dati richiesti, ma letti anche dati non richiesti"
WRITE (*,*) "  102: alcuni dei dati richiesti sono mancanti"
WRITE (*,*) "  103: tutti i dati richiesti sono mancanti "
WRITE (*,*) ""
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE crea_namelist(file_nml)
!
! Crea un file gacsv2seriet.naml di esempio (con documentazione inclusa)
!

IMPLICIT NONE
!-------------------------------------------------------------------------
CHARACTER (LEN=20), INTENT(IN) :: file_nml

OPEN (UNIT=20, FILE=file_nml, STATUS="REPLACE", ACTION="WRITE")
9 FORMAT (a)

WRITE (20,9) "&param"
WRITE (20,9) "out_form     = 1,"
WRITE (20,9) "out_ndec     = -2"
WRITE (20,9) "qcont        = 2,"
WRITE (20,9) "libsim       = T,"
WRITE (20,9) "step_yy_mm   = T,"
WRITE (20,9) "lab3d        = 2,"
WRITE (20,9) "lab3ddec     = 1,"
WRITE (20,9) "xls_dec_sep  = '.',"
!WRITE (20,9) "add_albedo   = F,"
WRITE (20,9) "dir_int      = T,"
WRITE (20,9) "tem_cel      = F,"
WRITE (20,9) "mo_rec       = F,"
WRITE (20,9) "sw_down      = F,"
WRITE (20,9) "flx_rev      = F,"
WRITE (20,9) "cc_fract     = F,"
WRITE (20,9) "sca_ini      = F,"
WRITE (20,9) "dos          = F"
WRITE (20,9) "/"
WRITE (20,9) ""
WRITE (20,9) ""
!  123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (20,9) &
  "Opzioni generali:"
!WRITE (20,9) &
!  "out_form    : 1 = ASCII del. spazi (seriet); 2 = CSV; 3 = ADMS meteo"
!WRITE (20,9) &
!  "              4 = ADMS background;  5 = ISC"
WRITE (20,9) &
  "out_form    : 1 = ASCII del. spazi (seriet); 2 = CSV"
WRITE (20,9) &
  "out_ndec    : numero di decimali in output (solo con out_form = 1)"
WRITE (20,9) &
  "               -1 notazione esponenziale; -2 dipendenti dal parametro (def)"
WRITE (20,9) &
  "qcont       : trattamento dati sospetti (0: lascia invariati, 1: mette"
WRITE (20,9) &
  "              tutti mancanti; 2: corregge solo errori presumibilmente"
WRITE (20,9) &
  "              dovuti a troncamento grib, 3: corregge tutti)"
WRITE (20,9) &
  "libsim      : se T tiene conto delle modifiche che LibSim fa ai dati (mettere"
WRITE (20,9) &
  "              F se il file gacsv e' stato prodotto senza passare da LibSim)"
WRITE (20,9) &
  "step_yy_mm  : se T riconosce step di un mese o un anno nei reference time"
WRITE (20,9)
WRITE (20,9) &
  "lab3d       : label dei livelli 3D (0: identificativo liv.; 1: quota SLM;"
WRITE (20,9) &
  "              2: quota da superficie). 1 e 2 solo per alcuni archivi"
WRITE (20,9) &
  "lab3ddec    : presenza dei decimali nelle label dei livelli (solo con "
WRITE (20,9) &
  "              lab3d = 1 o 2): 0 mai, 1 solo se necessario, 2 sempre"
WRITE (20,9) &
  "xls_dec_sep : imposta il separtore decimale in output (solo formato seriet)"
!WRITE (20,9) &
!  "add_albedo  : aggiunge all'output il campo albedo (solo formato ADMS e solo"
!WRITE (20,9) &
!  "              se l'albedo non e' tra i parametri estatti)"
WRITE (20,9)
WRITE (20,9) &
  "Opzioni che vengono sovrascritte se richiedo ISC o ADMS:"
WRITE (20,9) &
  "dir_int     : se T scrivo i venti come dir. e int."
WRITE (20,9) &
  "tem_cel     : se T scrivo le temperature in Celsius"
WRITE (20,9) &
  "mo_rec      : se T scrivo il reciproco della lughezza di Monin-Obukov"
WRITE (20,9) &
  "sw_down     : se T converto da SW budget a SW incoming"
WRITE (20,9) &
  "flx_rev     : se T cambio segno ai flussi di calore (>0 se sup.si raffredda)"
WRITE (20,9) &
  "cc_fract    : se T esprimo la copertura nuvolosa in frazione (default: %)"
WRITE (20,9) &
  "sca_ini     : se T scrivo data e scad dei parametri non istantei relative all'"
WRITE (20,9) &
  "                inizio del periodo di elaborazione (default: alla fine)."
WRITE (20,9) &
  "                Tutti i parametri devono avere lo stesso periodo di elaborazione!"
WRITE (20,9) &
  "dos         : se T scrivo con record DOS (i.e. aggiungo CR a ogni riga)"
WRITE (20,9) ""

CLOSE (20)

STOP
END SUBROUTINE crea_namelist

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

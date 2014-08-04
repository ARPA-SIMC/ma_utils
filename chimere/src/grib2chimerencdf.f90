PROGRAM grib2chimerencdf
!--------------------------------------------------------------------------
! Ricostruisce l'output di un run Chimere (NetCDF) a partire dai GRIB
! corrispondenti, scritti in un unico file.
! Versione 1 come grb_chimere2netcdf.f90
!
! Uso: grib2chimere.exe filein fileout fileaero vcoord DOMAIN 
! [-ro OROG]/[-roz OROG]
!
! TODO:
! Opzione per campi mancanti (0/missing?)
! Controllo dell'ordine temporale e salti di ora gia' alla prima scansione di filein
! Indagare la reale utilita' di:
! - fattori di conversione (fspec; xmass letto da LAT_SPEC, ma sempre = 0)
! - presenza di hlay e airm nel file NetCdf
! - presenza di lat e lon nel file NetCdf
!
! NOTE:
! - I dati di input devono essere:
!   * ordinati per verification time 
!   * definiti sulla stessa griglia, con scanning flag 64 (010)
!   * definiti su livelli sigma (109,k,0)
! - Il dati in output hanno sempre frequenza oraria, e non contengono 
!   informazioni sul timerange (i files NetCDF di chimere non distinguono 
!   tra analisi e previsione)
! - Il programma scrive in ouptut tutti i parametri e i livelli che 
!   compaiono almeno una volta nel file di input; eventuali dati mancanti 
!   per alcuni istanti vengono messi a 0.!
! - Il programma cerca le label NetCDF corrispondenti alle specie in input in
!   una lista di files statici (tabella_200.txt ...)
!
!                                         Versione 2.0.0, Enrico 04/08/2014
!--------------------------------------------------------------------------

USE grib_api
use netcdf
USE datetime_class
USE char_utilities
USE missing_values
USE grib2_utilities
IMPLICIT NONE

! 0.1 Parametri costanti
INTEGER, PARAMETER :: maxpar = 200     ! n.ro max specie
INTEGER, PARAMETER :: maxlev = 20      ! n.ro max livelli
REAL, PARAMETER :: fc = 287*7.2868e16 / 9.81

! 0.2 Tabelle con la codifica GRIB delle specie

! Path di default delle tabelle seriet
! PKGDATAROOTDIR viene sostituito in fase di compilazione con il path delle
! tabelle seriet (di solito /usr/share/ma_utils). La sostituzione sfrutta 
! il comando gfortran -D; vedi Makefile.am nelle singole dir.
CHARACTER (LEN=40), PARAMETER :: tab_path_def = PKGDATAROOTDIR
CHARACTER (LEN=40), PARAMETER :: tab_env = "MA_UTILS_DAT"
INTEGER, PARAMETER :: ntab = 4
INTEGER :: id_tab(ntab) = (/195,196,199,200/) 

! 0.4 Profilo standard dell'atmosfera (da Holton); valori ogni km
REAL, PARAMETER :: pstd(0:10) = (/101325.,89874.,79495.,70108.,61640., &
  54019.,47181.,41060.,35599.,30742.,26436./)

! 0.5 Indicatori dei parametri NetCDF
REAL, ALLOCATABLE :: fspec(:),val_bin(:)
INTEGER, ALLOCATABLE :: varid_par(:)
INTEGER :: dimid_time,dimid_date,dimid_x,dimid_y,dimid_z,dimid_verti,dimid_bins
INTEGER :: varid_lon,varid_lat,varid_avcoord,varid_bvcoord,varid_cutoff,varid_times
INTEGER :: varid_hlay,varid_airm,ncstat
CHARACTER (LEN=19) :: cdf_time
CHARACTER (LEN=16), ALLOCATABLE :: unit(:)
CHARACTER (LEN=8), ALLOCATABLE :: nclab(:)

! 0.4 Altre variabili del programma
TYPE(datetime) :: datah1,datah2,datahc,datahp,datah_dum
REAL, ALLOCATABLE :: conc(:,:,:,:),zz(:,:,:),ro(:,:,:),lon(:,:),lat(:,:),orog(:,:)
REAL, ALLOCATABLE :: field(:),ac(:),bc(:),pp(:)
REAL :: lnpstd(0:10),w1,w2,lnps,ps,lnpp,rmiss_out,xmass
INTEGER :: npar,inp_par(maxpar,3),inp_lev(maxlev,3),nx,ny,nz,nt,nbin
INTEGER :: igin=0,igfirst=0,iggeo=0,ifin,ifgeo,sm,ncid
INTEGER :: i,j,kt,kg,kp,kz,idp,iret,ios,l1,l2,dum_var,nfok
INTEGER :: id_hlay,invent_roz,idz
INTEGER :: clret(0:5),par(3),lev(3)
CHARACTER (LEN=200) :: filein,fileout,fileaero,filecoord,filevcoord,filegeo
CHARACTER (LEN=200) :: tab_path,file_tab(ntab),chdum,domain
CHARACTER (LEN=23) :: ch23
CHARACTER (LEN=8) :: dum_nclab
CHARACTER (LEN=2) :: next_arg
LOGICAL :: new

!==========================================================================
! 1) Elaborazioni preliminari

!--------------------------------------------------------------------------
! 1.0 Variabili d'ambiente
tab_path = ""
CALL GETENV(tab_env,tab_path)
IF (TRIM(tab_path) == "") tab_path = tab_path_def

DO kt = 1,ntab
  WRITE (file_tab(kt),'(2a,i3.3,a4)') TRIM(tab_path), &
    "/tabella_",id_tab(kt),".txt"
ENDDO

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

rmiss_out = 0.
idp = 0
id_hlay = 0
invent_roz = 0
domain = ""
next_arg = ""
filein = ""
fileout = ""
fileaero = ""
filevcoord = ""
filegeo = ""

DO kp = 1,HUGE(kp)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "") THEN  
    EXIT
  ELSE IF (TRIM(chdum) == "-ro") THEN  
    invent_roz = 1
    next_arg = "or"
  ELSE IF (TRIM(chdum) == "-roz") THEN  
    id_hlay = -1
    invent_roz = 2
    next_arg = "or"
  ELSE IF (next_arg == "or") THEN
    filegeo = chdum
    next_arg = ""
  ELSE 
    IF (idp == 0) filein = chdum
    IF (idp == 1) fileout = chdum
    IF (idp == 2) fileaero = chdum
    IF (idp == 3) filecoord = chdum
    IF (idp == 4) filevcoord = chdum
    IF (idp == 5) domain = chdum
    idp = idp + 1
  ENDIF
ENDDO

IF (idp /= 6 .OR. (invent_roz > 0 .AND. filegeo == "")) THEN
  CALL write_help
  STOP 1
ENDIF 

!--------------------------------------------------------------------------
! 1.2) Scorro una prima volta i GRIB per trovare: date estreme, dimensioni 
!      griglia, numero e lista parametri, numero e lista livelli. 
!      Verifico che tutti i campi siano definiti sulla stessa griglia, e 
!      che i dati siano ordinati per verification time.
!
! Note: 
! - id_hlay contiene l'indice dell'array inp_par corrispondente alla quota
!   dei model layers (-1: non presente in input)
! - invent_roz vale: 0: non invento nulla; 1: invento Ro; 2: invento Ro e Z

WRITe (*,*) "Scansione dei dati in input"
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

npar = 0
inp_par(:,:) = 0
nz = 0
inp_lev(:,:) = 0

DO kg = 1,HUGE(0)
  
! Leggo il prossimo GRIB
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998
  CALL get_grib1_header(igin, PAR=par, LEV=lev, IRET=iret)
  CALL get_grib_time(igin, VTIME=datahc)

! Controllo che la griglia sia la stessa del primo campo
  IF (kg == 1) THEN
    igfirst = igin
    CALL grib_get(igin,"scanningMode",sm)
    IF (sm /= 64) GOTO 9997
  ELSE
    CALL check_consistency(igin,igfirst,.TRUE.,.FALSE.,.FALSE.,.FALSE., &
      .FALSE.,.FALSE.,clret,iret)
    IF (iret /= 0) GOTO 9996
  ENDIF

! Verifico se ho trovato un nuovo parametro 
  new = .TRUE.
  DO kp = 1, npar
    IF (ALL(par(1:3) == inp_par(kp,1:3))) THEN
      new = .FALSE.
      EXIT
    ENDIF
  ENDDO  
  IF (new) THEN
    npar = npar + 1
    inp_par(npar,1:3) = par(1:3)
    IF (par(2) == 200 .AND. par(3) == 8) THEN
      IF (invent_roz == 2) GOTO 9995
      id_hlay = npar
    ELSE IF (par(2) == 200 .AND. par(3) == 89) THEN
      IF (invent_roz > 0) GOTO 9994
    ENDIF
  ENDIF

! Verifico se ho trovato un nuovo livello
  new = .TRUE.
  DO kz = 1, nz
    IF (ALL(lev(1:3) == inp_lev(kz,1:3))) THEN
      new = .FALSE.
      EXIT
    ENDIF
  ENDDO  
  IF (new) THEN
    nz = nz + 1
    inp_lev(nz,1:3) = lev(1:3)
  ENDIF

! Verifico che i campi siano ordinati per verification time; salvo gli 
! istanti estremi.
  IF (kg == 1) THEN
    datah1 = datahc
    datahp = datahc
  ELSE
    IF (datahc < datahp) GOTO 9986
    IF (datahc - datahp > timedelta_new(HOUR=1)) THEN
      WRITE (*,*) "Warning: trovati in input campi con differenza > 1 ora"
      WRITE (*,*) "  campo ",kg," istante ",to_char(datahc)
      WRITE (*,*) "  campo ",kg-1," istante ",to_char(datahp)
    ENDIF
    
    datah2 = datahc
    datahp = datahc
  ENDIF

  IF (kg > 1) CALL grib_release(igin)

ENDDO

CALL grib_close_file(ifin)
CALL getval(datah2-datah1, AHOUR=nt)

! Elaborazioni dipendenti dal contenuto di filein
CALL grib_get(igfirst,"numberOfPointsAlongAParallel",nx)
CALL grib_get(igfirst,"numberOfPointsAlongAMeridian",ny)
nt = nt + 1

ALLOCATE (field(nx*ny))
ALLOCATE (lon(nx,ny),lat(nx,ny))
ALLOCATE (ac(nz),bc(nz),pp(nz))
ALLOCATE (nclab(npar),unit(npar),fspec(npar),varid_par(npar))
ALLOCATE (conc(nx,ny,nz,npar))

!--------------------------------------------------------------------------
! 1.3) Trovo gli identificativi NetCDF relativi ai parametri in input

WRITE (*,*) "Altre operazioni preliminari"
nclab(1:kp) = ""

DO kt = 1,ntab

  OPEN (UNIT=20, FILE=file_tab(kt), STATUS="OLD", ACTION="READ", ERR=9993)

  DO
    READ (20,'(a)',IOSTAT=ios) chdum
    IF (ios /= 0) EXIT
    IF (TRIM(chdum) == "" .OR. chdum(1:1) == "!") CYCLE
    READ (chdum,'(1x,i3,2x,a8)',IOSTAT=ios) dum_var,dum_nclab
    IF (ios /= 0) GOTO 9992

!   Se il parametro letto e' tra quelli presenti in input, salvo nome e unita'
    DO kp = 1,npar
      IF (inp_par(kp,2) == id_tab(kt) .AND. inp_par(kp,3) == dum_var) THEN
        IF (inp_par(kp,2) == 200 .AND. inp_par(kp,3) == 8) THEN
          nclab(kp) = "hlay"
        ELSE IF (inp_par(kp,2) == 200 .AND. inp_par(kp,3) == 89) THEN
          nclab(kp) = "airm"
        ELSE IF (inp_par(kp,2) == 200) THEN
          nclab(kp) = uppercase(dum_nclab)
        ELSE
          nclab(kp) = dum_nclab
        ENDIF

        IF (inp_par(kp,2) == 200 .AND. inp_par(kp,3) == 8) THEN
          unit(kp) = "meter"
        ELSE IF (inp_par(kp,2) == 200 .AND. inp_par(kp,3) == 89) THEN
          unit(kp) = "molecule/cm**3"
        ELSE IF ((inp_par(kp,2) == 195 .AND. inp_par(kp,3) < 200) .OR. &
            (inp_par(kp,2) == 200 .AND. inp_par(kp,3) >= 220)) THEN
          unit(kp) = "ug/m3"
        ELSE IF (inp_par(kp,2) == 196 .OR. inp_par(kp,2) == 199 .OR. &
                 (inp_par(kp,2) == 200 .AND. &
                  inp_par(kp,3) > 150 .AND. inp_par(kp,3) < 220)) THEN
          unit(kp) = "ppb vol"
        ELSE
          unit(kp) = "unknown"
        ENDIF

        xmass = 0.
        fspec(kp) = xmass*1.6603e-12
        
        EXIT
      ENDIF
    ENDDO

  ENDDO
  CLOSE (20)

ENDDO

! Verifico se e' stata trovata la label NetCDF per tutti i parametri in input
DO kp = 1,npar
  IF (nclab(kp) == "") THEN 
    WRITE (*,'(a)') "Parametro non trovato nelle tabelle grib: ",inp_par(kp,2:3)
    WRITE (nclab(kp),'(i3.3,a1,i3.3)') inp_par(kp,2),"_",inp_par(kp,3)
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 1.4) Leggo dal file AEROSOL gli estremi dei bin aerosol

OPEN (UNIT=21, FILE=fileaero, STATUS="OLD", FORM="FORMATTED", ERR=9985)
READ (21,*)
READ (21,*) nbin
ALLOCATE (val_bin(nbin+1))
READ (21,*) val_bin(1:nbin+1)
CLOSE (21)

!--------------------------------------------------------------------------
! 1.5) Leggo dal file COORD le coordinate della griglia Chimere

OPEN (UNIT=22, FILE=filecoord, STATUS="OLD", FORM="FORMATTED", ERR=9984)
DO j = 1,ny
DO i = 1,nx
  READ (22,*) lon(i,j),lat(i,j)
ENDDO
ENDDO
CLOSE (22)

!--------------------------------------------------------------------------
! 1.6 Leggo dal file VCOORD i coefficienti dei livelli verticali Chimere

OPEN (UNIT=23, FILE=filevcoord, STATUS="OLD", ACTION="READ", ERR=9991)
DO kz = 1,nz 
  READ (23,*,IOSTAT=ios) ac(kz),bc(kz)
  IF (ios /= 0) GOTO 9990
ENDDO
CLOSE (23)

!--------------------------------------------------------------------------
! 1.7) Se richiesto, invento valori fittizi per le quote dei model layers e
!      la densita' dell'aria

IF (invent_roz > 0) THEN

  ALLOCATE (zz(nx,ny,nz),orog(nx,ny),ro(nx,ny,nz))

! Leggo orografia
  CALL grib_open_file(ifgeo,filegeo,"r",iret)
  IF (iret /= 0) GOTO 9989
  CALL grib_new_from_file(ifgeo,iggeo,iret)

  CALL check_consistency(iggeo,igfirst,.TRUE.,.FALSE.,.FALSE.,.FALSE., &
      .FALSE.,.FALSE.,clret,iret)
  IF (iret /= 0) GOTO 9988

  CALL get_grib1_header(iggeo, PAR=par, IRET=iret)
  IF (par(3) /= 8) WRITE (*,*) &
    "Warning, parametro inatteso in grib orografia: ",par(:)

  CALL grib_get(iggeo,"values",field(:))
  DO j = 1,ny
    orog(1:nx,j) = field((j-1)*nx+1:j*nx)
  ENDDO
  orog(:,:) = MAX(MIN(orog(:,:),10000.),0.)

  CALL grib_release(iggeo)
  CALL grib_close_file(ifgeo)

! Calcolo una volta per tutte il log. del profilo standard di pressione
  lnpstd(:) = LOG(pstd(:))

  DO i = 1,nx
  DO j = 1,ny

!   Calcolo Ps (interpolo il profilo standard alla quota dell'orografia)
    l1 = INT(orog(i,j)/1000.)
    l2 = l1 + 1
    w2 = orog(i,j)/1000. - INT(orog(i,j)/1000.)
    w1 = 1. - w2
    lnps = w1*lnpstd(l1) + w2*lnpstd(l2)
    ps = EXP(lnps)

!   Calcolo P sui livelli Chimere
    DO kz = 1,nz
      pp(kz) = 1e5*ac(kz) + ps*bc(kz)
    ENDDO

!   Calcolo Z al top dei layers e Ro media dei layers (costanti)
    IF (invent_roz == 2) THEN
      DO kz = 1,nz
        lnpp = LOG(pp(kz))
        DO l1 = 0,nz-1
          IF (lnpp <= lnpstd(l1)) EXIT
        ENDDO
        IF (lnpp > lnpstd(0)) l1 = 0
        l2 = l1 + 1
        w1 = (lnpstd(l2)-lnpp) / (lnpstd(l2)-lnpstd(l1))
        w2 = (lnpp-lnpstd(l1)) / (lnpstd(l2)-lnpstd(l1))
        zz(i,j,kz) = 1000. * (w1*REAL(l1) + w2*REAL(l2))
      ENDDO

      ro(i,j,1) = -fc * (pp(1)-ps) / (zz(i,j,1)-orog(i,j))
      DO kz = 2,nz
        ro(i,j,kz) = -fc * (pp(kz)-pp(kz-1)) / (zz(i,j,kz)-zz(i,j,kz-1))
      ENDDO
    ENDIF

  ENDDO
  ENDDO

ENDIF

!--------------------------------------------------------------------------
! 1.8) Apro il file di ouptut (NetCDF) e scrivo gli header

ncstat = nf90_create(fileout, NF90_CLOBBER, ncid)

! Assegno le dimensioni
ncstat = nf90_def_dim(ncid,'Time',        NF90_UNLIMITED,dimid_time)
ncstat = nf90_def_dim(ncid,'DateStrLen',  19,            dimid_date)
ncstat = nf90_def_dim(ncid,'west_east',   nx,            dimid_x)
ncstat = nf90_def_dim(ncid,'south_north', ny,            dimid_y)
ncstat = nf90_def_dim(ncid,'bottom_top',  nz,            dimid_z)
ncstat = nf90_def_dim(ncid,'vcoord_dim',  nz,            dimid_verti)
ncstat = nf90_def_dim(ncid,'number_of_cut_off_diameters',nbin+1,dimid_bins)

! Assegno gli attributi globali
ncstat = nf90_put_att(ncid,NF90_GLOBAL,'Title','CHIMERE SUITE')
ncstat = nf90_put_att(ncid,NF90_GLOBAL,'Sub-title', &
  'Hourly output concentrations file')
ncstat = nf90_put_att(ncid,NF90_GLOBAL,'Chimere_type','out')
ncstat = nf90_put_att(ncid,NF90_GLOBAL,'Generating_process', &
  'Generated by grib2chimerencdf')
ncstat = nf90_put_att(ncid,NF90_GLOBAL,'Conventions','None')
ncstat = nf90_put_att(ncid,NF90_GLOBAL,'Domain',domain)
ncstat = nf90_put_att(ncid,NF90_GLOBAL,'history','unknown')

! Assegno le varibili fisse
ncstat = nf90_def_var(ncid,'lon',NF90_FLOAT,(/dimid_x,dimid_y/),varid_lon)
ncstat = nf90_put_att(ncid,varid_lon,'units','degrees_east')
ncstat = nf90_put_att(ncid,varid_lon,'long_name','Longitude')

ncstat = nf90_def_var(ncid,'lat',NF90_FLOAT,(/dimid_x,dimid_y/),varid_lat)
ncstat = nf90_put_att(ncid,varid_lat,'units','degrees_north')
ncstat = nf90_put_att(ncid,varid_lat,'long_name','Latitude')

ncstat = nf90_def_var(ncid,'a_vcoord',NF90_FLOAT,(/dimid_verti/),varid_avcoord)
ncstat = nf90_put_att(ncid,varid_avcoord,'units','no_units')
ncstat = nf90_put_att(ncid,varid_avcoord,'long_name','A_sigma_coefficient')

ncstat = nf90_def_var(ncid,'b_vcoord',NF90_FLOAT,(/dimid_verti/),varid_bvcoord)
ncstat = nf90_put_att(ncid,varid_bvcoord,'units','no_units')
ncstat = nf90_put_att(ncid,varid_bvcoord,'long_name','B_sigma_coefficient')

ncstat = nf90_def_var(ncid,'cut_off_diameters',NF90_FLOAT,(/dimid_bins/),varid_cutoff)
ncstat = nf90_put_att(ncid,varid_cutoff,'units','meters')
ncstat = nf90_put_att(ncid,varid_cutoff,'long_name','cut_off_diameters')

ncstat = nf90_def_var(ncid,'Times',NF90_CHAR,(/dimid_date,dimid_time/),varid_times)

! Assegno le variabili relative ai parametri in input
DO kp = 1, npar
  ncstat = nf90_def_var(ncid,nclab(kp),NF90_FLOAT,&
    (/dimid_x,dimid_y,dimid_z,dimid_time/),varid_par(kp))
  ncstat = nf90_put_att(ncid,varid_par(kp),'units',unit(kp))
  ncstat = nf90_put_att(ncid,varid_par(kp),'long_name',TRIM(ADJUSTL(nclab(kp)))//" Concentration")
ENDDO

! Se sono richieste in output e non sono presenti in input, assegno i 
! parametri "quota livelli" e "densita' dell'aria"

IF (invent_roz > 0) THEN
  ncstat = nf90_def_var(ncid,'airm',NF90_FLOAT, &
    (/dimid_x,dimid_y,dimid_z,dimid_time/),varid_airm)
  ncstat = nf90_put_att(ncid,varid_airm,'units','molecule/cm**3')
  ncstat = nf90_put_att(ncid,varid_airm,'long_name','Air density')

  IF (invent_roz == 2) THEN
    ncstat = nf90_def_var(ncid,'hlay',NF90_FLOAT, &
      (/dimid_x,dimid_y,dimid_z,dimid_time/),varid_hlay)
    ncstat = nf90_put_att(ncid,varid_hlay,'units','meters')
    ncstat = nf90_put_att(ncid,varid_hlay,'long_name','Layer top altitude')
  ENDIF
ENDIF

ncstat = nf90_enddef(ncid)

!--------------------------------------------------------------------------
! 1.9) Scrivo sul file NetCDF i campi costanti

ncstat = nf90_put_var(ncid,varid_lon,lon)
ncstat = nf90_put_var(ncid,varid_lat,lat)
ncstat = nf90_put_var(ncid,varid_avcoord,ac)
ncstat = nf90_put_var(ncid,varid_bvcoord,bc)
ncstat = nf90_put_var(ncid,varid_cutoff,val_bin(1:nbin+1))

!--------------------------------------------------------------------------
! 1.10) Log a schermo delle operazioni preliminari

WRITE (*,'(a,i6)') "Specie in input:      ",npar
WRITE (*,'(2(a,i4))') "Dimensioni griglia :  ",nx," * ",ny
WRITE (*,'(a,i6)') "Livelli verticali :   ",nz
WRITE (*,'(a,i6)') "N.ro istanti (orari): ",nt
WRITE (*,'(a,a13)') "Istante iniziale: ",to_char(datah1)
WRITE (*,'(a,a13)') "Istante iniziale: ",to_char(datah2)
WRITE (*,*)

IF (invent_roz == 1) &
  WRITE (*,'(a)') "Verra' aggiunto il campo (inventato) Ro"
IF (invent_roz == 2) &
  WRITE (*,'(a)') "Verranno aggiunti i campi (inventati) Z e Ro"

!==========================================================================
! 2) Leggo i GRIB e scrivo il file Chimere (ciclo sugli istanti richiesti)

CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

DO kt = 1,nt
  datahc = datah1 + timedelta_new(HOUR=kt-1)
  ch23 = to_char(datahc)
  WRITE (*,'(2a)') "Elaboro istante ",ch23(1:13)

  cdf_time = ch23(1:19)
  cdf_time(11:11) = "_"
  ncstat = nf90_put_var(ncid,varid_times,cdf_time,(/1,kt/),(/19,1/))

  conc(:,:,:,:) = rmiss_out
  nfok = 0

!--------------------------------------------------------------------------
! 2.1 Leggo da filein tutti i dati relativi all'istante corrente

  DO kg = 1,HUGE(0)

!   Se non e' il primo campo di un nuovo istante, leggo il prossimo GRIB
    IF (kg /= 1 .OR. kt == 1) THEN
      CALL grib_new_from_file(ifin,igin,iret)
      IF (iret == GRIB_END_OF_FILE) EXIT
      IF (iret /= GRIB_SUCCESS) GOTO 9998
      CALL get_grib1_header(igin, PAR=par, LEV=lev, IRET=iret)
      CALL get_grib_time(igin, VTIME=datah_dum)
    ENDIF

    IF (datah_dum < datahc) GOTO 9983
    IF (datah_dum > datahc) EXIT

!   Trovo gli indici di livello e parametro del campo appena letto
    idz = -1
    DO kz = 1,nz
      IF (ALL(inp_lev(kz,1:3) == lev(1:3))) THEN
        idz = kz
        EXIT
      ENDIF
    ENDDO
    IF (idz == -1) GOTO 9987

    idp = -1
    DO kp = 1,npar
      IF (ALL(inp_par(kp,1:3) == par(1:3))) THEN
        idp = kp
        EXIT
      ENDIF
    ENDDO
    IF (idp == -1) GOTO 9987

!   Salvo i valori nell'array conc
    CALL grib_get(igin,"values",field(:))
    DO j = 1,ny
      conc(1:nx,j,idz,idp) = field((j-1)*nx+1:j*nx)
    ENDDO

    CALL grib_release (igin)
    nfok = nfok + 1

  ENDDO

  IF (nfok /= nz*npar) WRITE (*,*) "Warning: trovati ",nfok," campi su ",nz*npar

!--------------------------------------------------------------------------
! 2.2 Scrivo i dati relativi all'istante appena elaborato

  DO kp = 1, npar
    ncstat = nf90_put_var(ncid,varid_par(kp),conc(1:nx,1:ny,1:nz,kp), &
      (/1,1,1,kt/),(/nx,ny,nz,1/))
  ENDDO

! Se richiesto, stimo la densita' dell'aria a partire dalle quote dei 
! livelli in input (variabili nel tempo)
  IF (invent_roz == 1) THEN
    DO i = 1,nx
    DO j = 1,ny
      zz(i,j,1:nz) = conc(i,j,1:nz,id_hlay)
      ro(i,j,1) = -fc * (pp(1)-ps) / (zz(i,j,1)-orog(i,j))
      DO kz = 2,nz
        ro(i,j,kz) = -fc * (pp(kz)-pp(kz-1)) / (zz(i,j,kz)-zz(i,j,kz-1))
      ENDDO
    ENDDO
    ENDDO
  ENDIF

! Se richiesto, scrivo quote livelli e densita' dell'aria
  IF (invent_roz > 0) THEN
    ncstat = nf90_put_var(ncid,varid_airm,ro(1:nx,1:ny,1:nz), &
      (/1,1,1,kt/),(/nx,ny,nz,1/))
    IF (invent_roz == 2) THEN
      ncstat = nf90_put_var(ncid,varid_hlay,zz(1:nx,1:ny,1:nz), &
        (/1,1,1,kt/),(/nx,ny,nz,1/))
    ENDIF
  ENDIF

ENDDO

ncstat = nf90_close(ncid)
STOP 0

!==========================================================================
! 3) Gestione erori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," campo ",kg
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore in ",TRIM(filein),": scanning mode non gestito ",sm
STOP 3

9996 CONTINUE
WRITE (*,*) "Errore in ",TRIM(filein),": trovato campo con griglia diversa ",kg
STOP 3

9995 CONTINUE
WRITE (*,*) TRIM(filein)," contiene gia' le quote dei model layers"
WRITE (*,*) "L'opzione -roz e' inapplicabile"
STOP 4

9994 CONTINUE
WRITE (*,*) TRIM(filein)," contiene gia' la densita' dell'aria"
WRITE (*,*) "Le opzioni -ro e -roz sono inapplicabili"
STOP 4

9993 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_tab(kt))
STOP 2

9992 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_tab(kt))
STOP 2

9991 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filevcoord)
STOP 2

9990 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filevcoord)
STOP 2

9989 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filegeo)
STOP 2

9988 CONTINUE
WRITE (*,*) "Il file di orografia ",TRIM(filegeo)," ha una griglia diversa da quello dei dati ",TRIM(filein)
STOP 3

9987 CONTINUE
WRITE (*,*) "Errore ricerca livelli/parametri in ",TRIM(filein)
STOP 5

9986 CONTINUE
WRITE (*,*) TRIM(filein)," non e' ordinato per verificaiton time"
WRITE (*,*) "Campo ",kg-1," istante ",to_char(datahp)
WRITE (*,*) "Campo ",kg," istante ",to_char(datahc)
STOP 3

9985 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileaero)
STOP 2

9984 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filecoord)
STOP 2

9983 CONTINUE
WRITE (*,*) "Errore sequenza date"
STOP 5

END PROGRAM grib2chimerencdf

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "Uso: grib2chimere.exe filein fileout aero coord vcoord DOMAIN "
WRITE (*,*) "     [-ro OROG]/[-roz OROG]"
WRITE (*,*) "filein:   in formato grib"
WRITE (*,*) "fileout:  in formato NetCDF - Chimere"
WRITE (*,*) "aero:     file con estremi dei bin aerosol (formato AEROSOL)"
WRITE (*,*) "coord:    file con le coordinate di punti griglia Chimere (formato COORD)"
WRITE (*,*) "vcoord:   file con i coefficienti dei livelli verticali Chimere (formato VCOORD)"
WRITE (*,*) "DOMAIN:   valore dell'attibuto ""Domain"" in fileout (stringa)"
WRITE (*,*) "-ro OROG: aggiunge all'output la densita' dell' aria (inventata)."
WRITE (*,*) "          La quota di model layers deve essere compresa tra i parametri in input"
WRITE (*,*) "          OROG e' un file grib con l'orografia relativa alla griglia di filein"
WRITE (*,*) "-roz OROG: aggiunge all'ouptut quota dei livelli e densita' dell' aria (inventati"
WRITE (*,*) "          e costanti)."
WRITE (*,*) "          OROG e' un file grib con l'orografia relativa alla griglia di filein"
RETURN

END SUBROUTINE write_help


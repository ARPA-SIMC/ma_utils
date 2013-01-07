!--------------------------------------------------------------------------
! Programma della catena Calmet.
! Legge da file i grib necessari e li riscrive nel formato richiesto da 
! Calmet nel file mm5.dat
!
! Operazioni compiute:
! - interpola i dati nel tempo
! - calcola le coordinate x,y,z di ogni punto per ogni istante
!
! Metodo:
! legge le date richieste da filedate              (date_calmet.inp)
! legge descrizione dei grib e opzioni da filelam  (grib2mm5_PROJ.inp)
! legge orog(m) e parametri griglia da fileorog    (mm5orog_PROJ.grb)
! legge U,V,Geo (ed event. T,Q) da filegrib        (mm5.grb)
!
! L'interpolazione temporale richiede che l'ora iniziale e finale del run
! calmet siano istanti del modello meteo, e che non ci siano buchi nei dati
! LAM.
!
! La compilazione richiede:
! - il modulo per la gestione date date_hander.f90
! - le funzioni di libreria Gribex
!
! Uso:
! grib2mm5.exe fileorog filegrib filedate filelam
!
! Note:
! Le analisi ECMWF sono scritte come analisi non iniz. fino a una certa 
! data (<= 1999) e come analisi inizializzate oltre (<= 2003)
!
! Possibili sviluppi:
! - verificare che il programma sia blindato in caso orografia e dati siano
!   su griglie diverse o semplicemnte abbiano Scanning Flag diverse.
! - esguire interpolazione sui livelli Calmet (richiede lettura calmet.inp)
! - manca formato mm4
! - manca interpolazione vento SMR
!
!                                                 V1.2.1, Enrico 10/09/2012
!--------------------------------------------------------------------------

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

MODULE local
!
! Modulo generico per il trasferimento dei dati all'interno del programma
!
IMPLICIT NONE

! Numero di livelli e di punti dei dati LAM
INTEGER :: nlevp, nlevm, np

! 2: sono richiesti dati di T e Q; 1: non lo sono
INTEGER :: par_ext

! Dati LAM all'istante precendete
REAL, ALLOCATABLE :: pl_geop1(:,:),pl_uu1(:,:),pl_vv1(:,:),pl_zmsl1(:,:)
REAL, ALLOCATABLE :: pl_tt1(:,:),pl_qq1(:,:)
REAL, ALLOCATABLE :: ml_pp1(:,:),ml_uu1(:,:),ml_vv1(:,:),ml_zmsl1(:,:)
REAL, ALLOCATABLE :: ml_tt1(:,:),ml_qq1(:,:)

! Dati LAM all'istante corrente (i.e. appena letti)
REAL, ALLOCATABLE :: pl_geop2(:,:),pl_uu2(:,:),pl_vv2(:,:),pl_zmsl2(:,:)
REAL, ALLOCATABLE :: pl_tt2(:,:),pl_qq2(:,:)
REAL, ALLOCATABLE :: ml_pp2(:,:),ml_uu2(:,:),ml_vv2(:,:),ml_zmsl2(:,:)
REAL, ALLOCATABLE :: ml_tt2(:,:),ml_qq2(:,:)

! Dati LAM interpolati nel tempo (i.e. all'ora richiesta per output)
REAL, ALLOCATABLE :: pl_geop0(:,:),pl_uu0(:,:),pl_vv0(:,:),pl_zmsl0(:,:)
REAL, ALLOCATABLE :: pl_tt0(:,:),pl_qq0(:,:)
REAL, ALLOCATABLE :: ml_pp0(:,:),ml_uu0(:,:),ml_vv0(:,:),ml_zmsl0(:,:)
REAL, ALLOCATABLE :: ml_tt0(:,:),ml_qq0(:,:)

! Contatore per dati "out of range" di Rh
INTEGER :: rh_out

END MODULE local

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

PROGRAM grib2mm5

USE local
USE date_handler
IMPLICIT NONE

!--------------------------------------------------------------------------
! 0) Dichiarazioni

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000      ! max n.ro di punti dei GRIB
INTEGER, PARAMETER :: iuout = 30           ! unita' su cui scrivo fileout
INTEGER, PARAMETER :: iulog = 90           ! unita' su cui scrivo log

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL :: psec2(512),psec3(2)
REAL :: field(maxdim)

INTEGER :: ksec1_orog(1024),ksec2_orog(1024)
REAL    :: orog(maxdim)

! Dichiarazioni per FINDGRIB90
INTEGER :: data(3),ora(2),scad(4),level(3),var(3),ier

! Altre variabili del programma
TYPE (date) :: data1,data2,datac,data_rec

REAL, ALLOCATABLE :: lat_lam(:),lon_lam(:),virt_orog(:,:)
REAL, ALLOCATABLE :: vor_max(:),vor_min(:),vor_ave(:),vor_var(:), &
  vor_diff(:)
REAL :: latr,lonr,x1,x2,y1,y2,dx,dy,xrot,yrot,qq

INTEGER, ALLOCATABLE :: list_levp(:),list_levm(:)
INTEGER :: out_form,model,step_ist,tipo_scad,spin_up,calc_mlz,int_wind
INTEGER :: nist_lam,nhr,nx,ny
INTEGER :: i,j,k,klev,klevp,klevm,kp,kist,khr
INTEGER :: hh1,hh2,hhc,hh_rec,hh_tot,days_back,iuin,iugrb,scan_flag(3)
INTEGER :: pmxv(1),pmnv(1),pmxd(1),pmnd(1)

INTEGER :: idum

CHARACTER (LEN=80) :: fileorog,filegrib,filedate,filelam,fileout,out_head
CHARACTER (LEN=10) :: str_tipo_scad
CHARACTER (LEN=6) :: str_model
!--------------------------------------------------------------------------
! 1) Input: definisco quali sono i dati richiesti

! 1.1 Parametri da riga comandi
CALL getarg(1,filedate)
CALL getarg(2,filelam)
CALL getarg(3,fileorog)
CALL getarg(4,filegrib)

IF (TRIM(fileorog) == "-c") THEN
  CALL scrive_esempio
  STOP

ELSE IF (fileorog == "" .OR. filegrib == "" .OR. filedate == "" .OR. &
    filelam == "" .OR. TRIM(fileorog) == "-h") THEN
  CALL scrive_help
  STOP

ENDIF

! 1.2 Leggo da filedate le date estreme del run
OPEN (UNIT=20, FILE=filedate, STATUS="OLD", ACTION="READ", ERR=9999)
READ (20,*, ERR=9999)
READ (20,*, ERR=9999)
READ (20,*, ERR=9999)
READ (20,'(i4,3i2)', ERR=9999) data1%yy,data1%mm,data1%dd,hh1
READ (20,'(i4,3i2)', ERR=9999) data2%yy,data2%mm,data2%dd,hh2
CLOSE (20)

nhr = (data2 - data1)*24 + (hh2 - hh1) + 1

! 1.3 Leggo le opzioni da filelam
OPEN (UNIT=21, FILE=filelam, STATUS="OLD", ACTION="READ", ERR=9998)
READ (21,*, ERR=9998)
READ (21,*, ERR=9998)
READ (21,*, ERR=9998)
READ (21,*, ERR=9998)
READ (21,*, ERR=9998) out_form
READ (21,*, ERR=9998) model
READ (21,*, ERR=9998) par_ext
READ (21,*, ERR=9998) tipo_scad
READ (21,*, ERR=9998) step_ist
READ (21,*, ERR=9998) spin_up

READ (21,*, ERR=9998) nlevp
ALLOCATE (list_levp(nlevp))
DO klevp = 1,nlevp
  READ (21,*, ERR=9998) list_levp(klevp)
ENDDO

READ (21,*, ERR=9998) nlevm
ALLOCATE (list_levm(nlevm))
DO klevm = 1,nlevm
  READ (21,*, ERR=9998) list_levm(klevm)
ENDDO

READ (21,*, ERR=9998) int_wind
READ (21,*, ERR=9998) calc_mlz
CLOSE (21)

! 1.4 Controllo che le opzioni di filelam siano legali 
IF ((out_form /= 0 .AND. out_form /= 1) .OR. &
    (model /= 1 .AND. model /= 2 .AND. model /= 3) .OR. &
    (par_ext /= 1 .AND. par_ext /= 2 .AND. par_ext /= 3 ) .OR. &
    (tipo_scad /= 1 .AND. tipo_scad /= 2 .AND. &
     tipo_scad /= 3 .AND. tipo_scad /= 4) .OR. &
    step_ist <= 0 .OR. MOD(nhr-1,step_ist) /= 0 .OR. &
    spin_up < 0 .OR. nlevp < 0 .OR. nlevm < 0 .OR. &
    (int_wind /= 1 .AND. int_wind /= 2) .OR. &
    (calc_mlz /= 1 .AND. calc_mlz /= 2)) THEN
  WRITE (*,*) "Parametri illegali in ",TRIM(filelam)
  STOP

ELSE IF (nlevm + nlevp == 0) THEN
  WRITE (*,*) "Non e' stato richiesto nessun livello!"
  STOP

ELSE IF (calc_mlz == 2 ) THEN
  IF (model /= 3) THEN
    WRITE (*,*) "Calcolo di Z sui model lev. usando geop su Pres. lev", &
      " gestito solo per Aladin"
    STOP
  ELSE IF (nlevm < 2 .OR. nlevp == 0 ) THEN
    WRITE (*,*) "Ci devono essere almeno 2 model lev. e 1 pres. lev. (4.3.3)"
    STOP
  ENDIF

ELSE IF (ANY(list_levp(1:nlevp-1) < list_levp(2:nlevp)) .OR. &
         ANY(list_levm(1:nlevm-1) > list_levm(2:nlevm)) ) THEN
  WRITE (*,*) "I livelli richiesti devono essere ordinati dal basso"
  STOP

ELSE IF (int_wind == 2) THEN
  WRITE (*,*)  "algoritmo SMR per int. temp. del vento non ancora gestito"
  STOP

ENDIF

! 1.5 Calcolo i parametri dipendenti dalle opzioni di filelam
nist_lam = nhr / step_ist + 1

IF (model == 1) THEN
  str_model = "ECMWF "        
ELSE IF (model == 2) THEN
  str_model = "LOKAL "        
ELSE IF (model == 3) THEN
  str_model = "ALADIN"        
ENDIF

IF (tipo_scad == 1) THEN
  str_tipo_scad = "Analisi NI"
ELSE IF (tipo_scad == 2) THEN
  str_tipo_scad = "Forc. 00Z "
ELSE IF (tipo_scad == 3) THEN
  str_tipo_scad = "Forc.00/12"
ELSE IF (tipo_scad == 4) THEN
  str_tipo_scad = "Analisi In"
ENDIF

!--------------------------------------------------------------------------
! 2) Leggo l'orografia e calcolo i parametri che restano uguali per tutti 
!    i grib (area ...)

! 2.1 Leggo l'orografia
CALL PBOPEN (iuin,fileorog,'R',kret)
IF (kret /= 0) GOTO 9997

CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
CALL GRIBEX (ksec0,ksec1_orog,ksec2_orog,psec2,ksec3,psec3,ksec4, &
             orog,maxdim,kbuffer,maxdim,klen,'D',kret)
IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
CALL PBCLOSE (iuin,kret)

nx = ksec2_orog(2)
ny = ksec2_orog(3)
np = nx * ny

! 2.2 Alloco le variabili che dipendono dal numero di punti e livelli LAM

! Arrays dei dati (modulo local)
CALL allocate_local

! Altri arrays
ALLOCATE (lat_lam(np),lon_lam(np))

IF (nlevm > 0 .AND. nlevp > 0 .AND. model == 3) THEN
  ALLOCATE (virt_orog(np,nist_lam))
  ALLOCATE (vor_max(np),vor_min(np),vor_var(np),vor_ave(np),vor_diff(np))
ENDIF

! 2.3 Calcolo le coordinate geografiche dei punti di griglia

! Trovo scanning flags e verifico che siano legali
CALL bin2scan(ksec2(11),scan_flag)

IF (scan_flag(1) /= 0 .OR. scan_flag(3) /= 0 .OR. &
    (scan_flag(2) /= 0 .AND. scan_flag(2) /= 1) ) THEN
  WRITE (*,'(a,3i2)') "Scanning flag GRIB non gestiti: ",scan_flag(1:3)
  STOP
ELSE IF (ksec2_orog(1) /= 0) THEN
  WRITE (*,*) "Proiezione geografica GRIB non gestita, ksec2(1): ", &
    ksec2_orog(1)
  STOP
ENDIF

! Trovo i parametri della griglia
x1 = REAL(ksec2_orog(5)) / 1000.
x2 = REAL(ksec2_orog(8)) / 1000.

IF (scan_flag(2) == 0) THEN
  y1 = REAL(ksec2_orog(7)) / 1000.
  y2 = REAL(ksec2_orog(4)) / 1000.
ELSE  
  y1 = REAL(ksec2_orog(4)) / 1000.
  y2 = REAL(ksec2_orog(7)) / 1000.
ENDIF

dx = (x2-x1) / REAL(nx-1)
dy = (y2-y1) / REAL(ny-1)

xrot = REAL(ksec2_orog(14)) / 1000.
yrot = 90. + REAL(ksec2_orog(13)) / 1000.

! Trovo le coordinate dei punti, ed eventualmente le antiruoto
DO kp = 1, np
  i = MOD(kp-1, nx) + 1
  lonr = x1 + (i-1) * dx

  IF (scan_flag(2) == 0) THEN
    j = ny - (kp-1) / nx                                    ! ECMWF, Aladin
  ELSE
    j = (kp-1) / nx + 1                                     ! Lokal, Lambo
  ENDIF
  latr = y1 + (j-1) * dy

  IF (xrot /= 0 .OR. yrot /= 90.) THEN
    CALL rtll(lonr,latr,xrot,yrot,lon_lam(kp),lat_lam(kp))
  ELSE
    lat_lam(kp) = latr 
    lon_lam(kp) = lonr 
  ENDIF

ENDDO

!--------------------------------------------------------------------------
! 3) Apro i files e scrivo l'header sul file di output

! 3.1 Apro il file dei dati grib
CALL PBOPEN (iuin,filegrib,'R',kret)
IF (kret /= 0) GOTO 9996

! 3.2 Apro il file di log
OPEN (UNIT=iulog, FILE="grib2mm5.log", STATUS="REPLACE", ACTION="WRITE")

! 3.3 Apro il file di output
IF (out_form == 1) THEN
  fileout = "mm5.dat"
ELSE IF (out_form == 2) THEN
  fileout = "mm4.dat"
ENDIF

OPEN (UNIT=iuout, FILE=fileout, STATUS="REPLACE", ACTION="WRITE")

! 3.4 Scrivo l'header del file di output
WRITE (out_head,'(4a)') "modello: ",str_model," scadenze: ",str_tipo_scad

IF (out_form == 1) THEN
  WRITE (iuout,'(a)') TRIM(out_head)                                !hr 1
  IF (par_ext == 1 .OR. par_ext == 2) THEN                                    
    WRITE (iuout,'(5i2)') 0,0,0,0,0                                 !hr 2
  ELSE IF (par_ext == 3) THEN
    WRITE (iuout,'(5i2)') 0,1,0,0,0                                 !hr 2
  ENDIF
  WRITE (iuout,'(a)') ""                                            !hr 3
  WRITE (iuout,'(8i3)') (-9, k=1,8)                                 !hr 4
  WRITE (iuout,'(4i2.2,i5,3i4)') &
    MOD(data1%yy,100),data1%mm,data1%dd,hh1,nhr,np,1,nlevp+nlevm    !hr 5
  WRITE (iuout,'(4i4,4(f8.2))') 1,1,np,1,(-999.99, k=1,4)           !hr 6

  DO klev = 1,nlevp+nlevm  
    WRITE (iuout,'(f6.3)') 9.999                                    !hr Z
  ENDDO

  DO kp = 1,np
    WRITE (iuout,'(i7,i2,f7.3,f8.3,i5)') &
      kp,1,lat_lam(kp),lon_lam(kp),NINT(orog(kp))                   !hr Pts
  ENDDO

ELSE IF (out_form == 2) THEN

! header records mm4 [...]

ENDIF

WRITE (*,'(a)') "grib2mm5: preliminari completi, inizio ciclo scadenze LAM"

!--------------------------------------------------------------------------
! 4) Ciclo sugli istanti LAM: leggo i dati, li elaboro e li scrivo

scad_lam: DO kist = 1,nist_lam

! Calcolo data/ora dell'istante attuale
  hh_tot = hh1 + (kist-1) * step_ist
  hhc = MOD(hh_tot,24)
  datac = data1 + hh_tot/24

! Seleziono il run del modello da usare per l'istante corrente
  CALL select_run(datac,hhc,tipo_scad,spin_up,iulog,data,ora,scad)

! Inizializzo contetore per valori sospetti di rh
  rh_out = 0

!--------------------------------------------------------------------------
! 4.1 Se richiesto, leggo pressure levels
  DO klevp = 1,nlevp

    CALL encode_lev(model,"P",list_levp(klevp),level)

!   Geo
    CALL encode_var(model,"Z",var)
    CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
    IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
    IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
      CALL fatal_error(data,ora,scad,level,var,ier,2)

    pl_geop2(1:np,klevp) = field(1:np)

!   Vento U
    CALL encode_var(model,"U",var)
    CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
    IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
    IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
      CALL fatal_error(data,ora,scad,level,var,ier,2)

    pl_uu2(1:np,klevp) = field(1:np)

!   Vento V
    CALL encode_var(model,"V",var)
    CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
    IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
    IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
      CALL fatal_error(data,ora,scad,level,var,ier,2)

    pl_vv2(1:np,klevp) = field(1:np)

    IF (par_ext == 2 .OR. par_ext == 3) THEN
!     Temperatura
      CALL encode_var(model,"T",var)
      CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
      IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   field,maxdim,kbuffer,maxdim,klen,'D',kret)
      IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
      IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
        CALL fatal_error(data,ora,scad,level,var,ier,2)

      pl_tt2(1:np,klevp) = field(1:np)
    ENDIF

    IF (par_ext == 3) THEN
!     Umidita' specifica
      CALL encode_var(model,"Q",var)
      CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
      IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   field,maxdim,kbuffer,maxdim,klen,'D',kret)
      IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
      IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
        CALL fatal_error(data,ora,scad,level,var,ier,2)

!     Solo per Aladin, converto l'umidita' relativa in specifica
      IF (model == 3) THEN
        DO kp = 1,np
          CALL relhumtoq(field(kp),REAL(list_levp(klevp)), &
            pl_tt2(kp,klevp),qq)
          field(kp) = qq
        ENDDO
      ENDIF

      pl_qq2(1:np,klevp) = field(1:np)
    ENDIF

  ENDDO

!--------------------------------------------------------------------------
! 4.2 Se richiesto, leggo model levels

  DO klevm = 1,nlevm

    CALL encode_lev(model,"M",list_levm(klevm),level)
  
!   Pressione
    CALL encode_var(model,"P",var)
    CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
    IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
    IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
      CALL fatal_error(data,ora,scad,level,var,ier,2)

    IF (model == 3) THEN
      ml_pp2(1:np,klevm) = field(1:np) / 100.  ! aladin: press. e' in Pa
    ELSE
      ml_pp2(1:np,klevm) = field(1:np)         ! altri modelli: in hPa (??)
    ENDIF

!   Vento U
    CALL encode_var(model,"U",var)
    CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
    IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
    IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
      CALL fatal_error(data,ora,scad,level,var,ier,2)

    ml_uu2(1:np,klevm) = field(1:np)
 
!   Vento V
    CALL encode_var(model,"V",var)
    CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
    IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
    IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
      CALL fatal_error(data,ora,scad,level,var,ier,2)

    ml_vv2(1:np,klevm) = field(1:np)

    IF (par_ext == 2 .OR. par_ext == 3) THEN
!     Temperatura
      CALL encode_var(model,"T",var)
      CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
      IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   field,maxdim,kbuffer,maxdim,klen,'D',kret)
      IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
      IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
        CALL fatal_error(data,ora,scad,level,var,ier,2)

      ml_tt2(1:np,klevm) = field(1:np)
    ENDIF

    IF (par_ext == 3) THEN
!     Umidita' specifica 
      CALL encode_var(model,"Q",var)
      CALL findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
      IF (ier /= 0) CALL fatal_error(data,ora,scad,level,var,ier,1)

      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   field,maxdim,kbuffer,maxdim,klen,'D',kret)
      IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
      IF (ANY( ksec2(:) /= ksec2_orog(:) )) &
        CALL fatal_error(data,ora,scad,level,var,ier,2)

!     Solo per Aladin, converto l'umidita' relativa in specifica
      IF (model == 3) THEN
        DO kp = 1,np
          CALL relhumtoq(field(kp),ml_pp2(kp,klevm),ml_tt2(kp,klevm),qq)
          field(kp) = qq
        ENDDO
      ENDIF

      ml_qq2(1:np,klevm) = field(1:np)
    ENDIF

  ENDDO

!--------------------------------------------------------------------------
! 4.3 Calcolo l'altezza MSL dei vari livelli, per tutti i punti
!
! Metodo:
! - per i pressure levels prendo semplicemente il geopotenziale. L'orog.
!   e' quella del modello (oppure una costruita a partire da EROS), ma 
!   cmnq non ha un'importanza fondamentale (i liv.P sono +- "di libera 
!   atmosfera" anche nei punti in cui cadono vicino al suolo)
! - per i model levels devo calcolare l'altezza esatta dalla superficie
! - se si usano entrmabi, e' possibile calcolare un' "orografia variabile",
!   a partire dal geop sul 1o livello P che cade all'interno della fascia
!   coperta dai model levels (gestito solo per aladin)

! 4.3.1 Model levels (da orografia e valore del livello)
  IF (nlevm > 0 .AND. (model /= 3 .OR. calc_mlz == 1)) THEN
    IF (model == 1) THEN                     !ECMWF: hybrid, variable
!     CALL calc_zmodel_ecmwf()    

    ELSE IF (model == 2) THEN                !Lokal: hybrid, fixed
!     CALL calc_zmodel_lokal()    

    ELSE IF (model == 3) THEN                !Aladin: terr-follow, fixed
      DO kp = 1,np
      DO klevm = 1,nlevm
        ml_zmsl2(kp,klevm) = REAL(orog(kp) + list_levm(klevm))
      ENDDO
      ENDDO

    ENDIF

! 4.3.2 Model levels (da geop. sui press.lev. e pressione sui model lev.)
  ELSE IF (nlevm > 0 .AND. model == 3 .AND. calc_mlz == 2) THEN
    CALL calc_zmodel_aladin(list_levp,list_levm,virt_orog(1:np,kist),kist)

  ENDIF

! 4.3.3 Pressure levels (dal geopotenziale)
  IF (nlevp > 0) THEN
    pl_zmsl2(1:np,1:nlevp) = pl_geop2(1:np,1:nlevp) / 9.8
  ENDIF

!--------------------------------------------------------------------------
! 4.4 Interpolazione temporale e output
!
! Metodo: al primo istante scrivo i dati per l'ora corrente e salvo quelli
! per interpolare le ore successive; agli istanti successivi, scrivo anche 
! i record delle ore comprese tra gli utlimi 2 istanti LAM.

! 4.4.1 Se non sono al primo istante, calcolo e scrivo i dati relativi alle
!       ore precedenti

  IF (kist > 1) THEN
    DO khr = 1, step_ist - 1

!     calcolo i dati all'ora corrente, e li salvo negli array di lavoro
      CALL time_interp(khr,step_ist,int_wind)

!     calcolo data/ora del record corrente
      hh_tot = hhc - step_ist + khr
      IF (hh_tot >= 0) THEN
        hh_rec = MOD(hh_tot,24)
        data_rec = datac + hh_tot/24
      ELSE
        days_back = (-1 - hh_tot) / 24 + 1
        data_rec = datac - days_back
        hh_rec = hh_tot + 24 * days_back
      ENDIF

      DO kp = 1,np
!       scrivo l'header dei data record
        WRITE (iuout,'(4(i2.2,1x),i6,1x,i1,1x,2(f6.0,1x),i2)') &
          MOD(data_rec%yy,100),data_rec%mm,data_rec%dd,hh_rec, &
          kp,1,-9999.,-9999.,-9
    
!       scrivo i data record
        CALL write_rec_ml(kp,iuout,iulog)
        CALL write_rec_pl(kp,iuout,iulog,list_levp)

      ENDDO

    ENDDO
  ENDIF

! 4.4.2 In tutti i casi, scrivo i record dell'ora corrispndente ai dati 
!       LAM appena letti.

! copio i dati dell'ultimo istante (*l_*2) sugli array di lavoro (*l_*0)
  CALL ass_dati_0_2

  DO kp = 1,np
!   scrivo l'header dei data record
    WRITE (iuout,'(4(i2.2,1x),i6,1x,i1,1x,2(f6.0,1x),i2)') &
      MOD(datac%yy,100),datac%mm,datac%dd,hhc,kp,1,-9999.,-9999.,-9
    
!   scrivo i data record
    CALL write_rec_ml(kp,iuout,iulog)
    CALL write_rec_pl(kp,iuout,iulog,list_levp)

  ENDDO

! 4.4.3 Copio i dati dell'ultimo istante (*l_*2) sugli array relativi 
!       all'istante precedente (*l_*1), per poter interpolare nel tempo al
!       prossimo istante LAM
  CALL ass_dati_1_2

  WRITE (*,'(a,i3)') "  Elaborata scadenza ",kist
  WRITE (iulog,*) "  Scadenza completata, RH >100 o <0: ",rh_out

ENDDO scad_lam


!--------------------------------------------------------------------------
! 5) Conclusione (log statistiche, chiudo i files  ...)

! Log statistiche dell'orografia virtuale Aladin
IF (nlevm > 0 .AND. model == 3 .AND. calc_mlz == 2) THEN
  vor_max(1:np) = MAXVAL(virt_orog(1:np,1:nist_lam), DIM=2)
  vor_min(1:np) = MINVAL(virt_orog(1:np,1:nist_lam), DIM=2)
  vor_var(1:np) = vor_max(1:np) - vor_min(1:np) 
  vor_ave(1:np) = SUM(virt_orog(1:np,1:nist_lam), DIM=2) / REAL(nist_lam)
  pmxv = MAXLOC(vor_var(1:np))
  pmnv = MINLOC(vor_var(1:np))

  WRITE (iulog,*)
  WRITE (iulog,'(a)') "Ororgrafia stimata Aladin: variabilita' temporale"
  WRITE (iulog,'(a,f7.1)') "Media: ",SUM(vor_var(1:np)) / REAL(np)
  WRITE (iulog,'(a,f7.1,a,2f8.2,1x,f5.0)') "Max: ",vor_var(pmxv), &
    " al pto (x,y,z) ",lon_lam(pmxv),lat_lam(pmxv),vor_ave(pmxv)

  vor_diff(1:np) = vor_ave(1:np) - orog(1:np) 
  pmxd = MAXLOC(vor_diff(1:np))
  pmnd = MINLOC(vor_diff(1:np))

  WRITE (iulog,'(a)') "differenza: (org. stimata media) - (orog grib)"
  WRITE (iulog,'(a,f7.1)') "Media: ",SUM(vor_diff(1:np)) / REAL(np)
  WRITE (iulog,'(a,f7.1,a,2f8.2,1x,f5.0)') "Max: ",vor_diff(pmxd), &
    " al pto (x,y,z) ",lon_lam(pmxd),lat_lam(pmxd),orog(pmxd)
  WRITE (iulog,'(a,f7.1,a,2f8.2,1x,f5.0)') "Min: ",vor_diff(pmnd), &
    " al pto (x,y,z) ",lon_lam(pmnd),lat_lam(pmnd),orog(pmnd)

! Output grib orografia virtuale Aladin
  CALL PBOPEN (iugrb,"vorog_alad.grb",'W',kret)
  field(1:np) = vor_ave(1:np)
  CALL GRIBEX (ksec0,ksec1_orog,ksec2_orog,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  CALL PBWRITE (iugrb,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
  CALL PBCLOSE (iugrb,kret)

ENDIF

! Chiudo i files
CALL PBCLOSE (iuin,kret)
CLOSE (iuout)
CLOSE (iulog)

STOP

!--------------------------------------------------------------------------
! 6) Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "grib2mm5: errore leggendo ",TRIM(filedate)
STOP

9998 CONTINUE
WRITE (*,*) "grib2mm5: errore leggendo ",TRIM(filelam)
STOP

9997 CONTINUE
WRITE(*,'(3a,i4)') "grib2mm5: errore aprendo ",TRIM(fileorog)," kret ",kret
STOP

9996 CONTINUE
WRITE(*,'(3a,i4)') "grib2mm5: errore aprendo ",TRIM(filegrib)," kret ",kret
STOP

END PROGRAM grib2mm5

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE allocate_local
!
! alloca gli array del modulo local (contenitori per i dati LAM letti e 
! interpolati nel tempo
!
USE local
IMPLICIT NONE
!--------------------------------------------------------------------------

! Arrays relativi ai pressure levels
IF (nlevp > 1) THEN

  ALLOCATE (pl_zmsl0(np,nlevp),pl_zmsl1(np,nlevp),pl_zmsl2(np,nlevp))

  ALLOCATE (pl_geop0(np,nlevp),pl_uu0(np,nlevp),pl_vv0(np,nlevp))
  ALLOCATE (pl_geop1(np,nlevp),pl_uu1(np,nlevp),pl_vv1(np,nlevp))
  ALLOCATE (pl_geop2(np,nlevp),pl_uu2(np,nlevp),pl_vv2(np,nlevp))

  IF (par_ext == 2 .OR. par_ext == 3) &
    ALLOCATE (pl_tt0(np,nlevp),pl_tt1(np,nlevp),pl_tt2(np,nlevp))

  IF (par_ext == 3) &
    ALLOCATE (pl_qq0(np,nlevp),pl_qq1(np,nlevp),pl_qq2(np,nlevp))

ENDIF

! Arrays relativi ai model levels
IF (nlevm > 1) THEN

  ALLOCATE (ml_zmsl0(np,nlevm),ml_zmsl1(np,nlevm),ml_zmsl2(np,nlevm))

  ALLOCATE (ml_pp0(np,nlevm),ml_uu0(np,nlevm),ml_vv0(np,nlevm))
  ALLOCATE (ml_pp1(np,nlevm),ml_uu1(np,nlevm),ml_vv1(np,nlevm))
  ALLOCATE (ml_pp2(np,nlevm),ml_uu2(np,nlevm),ml_vv2(np,nlevm))

  IF (par_ext == 2 .OR. par_ext == 3) &
    ALLOCATE (ml_tt0(np,nlevp),ml_tt1(np,nlevp),ml_tt2(np,nlevp))

  IF (par_ext == 3) &
    ALLOCATE (ml_qq0(np,nlevp),ml_qq1(np,nlevp),ml_qq2(np,nlevp))

ENDIF

RETURN
END SUBROUTINE allocate_local

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE ass_dati_0_2
!
! Agisce sul modulo local, assegnado agli arrays *l_*0 il valore degli 
! arrays *l_*2
!
USE local
IMPLICIT NONE
!
IF (nlevm > 0) THEN
  ml_pp0(1:np,1:nlevm) = ml_pp2(1:np,1:nlevm)
  ml_uu0(1:np,1:nlevm) = ml_uu2(1:np,1:nlevm)
  ml_vv0(1:np,1:nlevm) = ml_vv2(1:np,1:nlevm)
  ml_zmsl0(1:np,1:nlevm) = ml_zmsl2(1:np,1:nlevm)

  IF (par_ext == 2 .OR. par_ext == 3) &
    ml_tt0(1:np,1:nlevm) = ml_tt2(1:np,1:nlevm)
  IF (par_ext == 3) &
    ml_qq0(1:np,1:nlevm) = ml_qq2(1:np,1:nlevm)
ENDIF

IF (nlevp > 0) THEN
  pl_geop0(1:np,1:nlevp) = pl_geop2(1:np,1:nlevp)
  pl_uu0(1:np,1:nlevp) = pl_uu2(1:np,1:nlevp)
  pl_vv0(1:np,1:nlevp) = pl_vv2(1:np,1:nlevp)
  pl_zmsl0(1:np,1:nlevp) = pl_zmsl2(1:np,1:nlevp)

  IF (par_ext == 2 .OR. par_ext == 3) &
    pl_tt0(1:np,1:nlevp) = pl_tt2(1:np,1:nlevp)
  IF (par_ext == 3) &
    pl_qq0(1:np,1:nlevp) = pl_qq2(1:np,1:nlevp)
ENDIF

RETURN
END SUBROUTINE ass_dati_0_2

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE ass_dati_1_2
!
! Agisce sul modulo local, assegnado agli arrays *l_*1 il valore degli 
! arrays *l_*2
!
USE local
IMPLICIT NONE
!
IF (nlevm > 0) THEN
  ml_pp1(1:np,1:nlevm) = ml_pp2(1:np,1:nlevm)
  ml_uu1(1:np,1:nlevm) = ml_uu2(1:np,1:nlevm)
  ml_vv1(1:np,1:nlevm) = ml_vv2(1:np,1:nlevm)
  ml_zmsl1(1:np,1:nlevm) = ml_zmsl2(1:np,1:nlevm)

  IF (par_ext == 2 .OR. par_ext == 3) &
    ml_tt1(1:np,1:nlevm) = ml_tt2(1:np,1:nlevm)
  IF (par_ext == 3) &
    ml_qq1(1:np,1:nlevm) = ml_qq2(1:np,1:nlevm)
ENDIF

IF (nlevp > 0) THEN
  pl_geop1(1:np,1:nlevp) = pl_geop2(1:np,1:nlevp)
  pl_uu1(1:np,1:nlevp) = pl_uu2(1:np,1:nlevp)
  pl_vv1(1:np,1:nlevp) = pl_vv2(1:np,1:nlevp)
  pl_zmsl1(1:np,1:nlevp) = pl_zmsl2(1:np,1:nlevp)

  IF (par_ext == 2 .OR. par_ext == 3) &
    pl_tt1(1:np,1:nlevp) = pl_tt2(1:np,1:nlevp)
  IF (par_ext == 3) &
    pl_qq1(1:np,1:nlevp) = pl_qq2(1:np,1:nlevp)
ENDIF

RETURN
END SUBROUTINE ass_dati_1_2

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE bin2scan (bin,scan)
!
! given the value of ksec2(11), returns the 3 "scanning flag"
! da grid_handler.f90
!
IMPLICIT NONE

INTEGER, INTENT(IN) :: bin
INTEGER, INTENT(OUT) :: scan(3)
INTEGER :: dum
!

IF (bin < 8) THEN        ! scanning flag written in reverse order (MM5)
                         ! WARNING: scan(1) might really be scan(3) !!!
  dum = bin
  scan(3) = MOD (dum,2)
  dum = dum / 2
  scan(2) = MOD (dum,2)
  dum = dum / 2
  scan(1) = MOD (dum,2)
  
ELSE                     ! scanning flags written in standard (ECMWF) way

  IF (MOD(bin,32) /= 0) WRITE (*,*) 'warning, strano scan ',bin

  dum = bin / 32
  scan(1) = MOD (dum,2)
  dum = dum / 2
  scan(2) = MOD (dum,2)
  dum = dum / 2
  scan(3) = MOD (dum,2)

ENDIF

RETURN
END SUBROUTINE bin2scan

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE calc_zmodel_aladin(list_levp,list_levm,virt_orog,kist)
!
! Calcola l'altezza slm dei model levels Aladin, in base al geopotenziale 
! sui pressure levels.
! Agisce sugli array *l_*2, contenuti nel modulo local
!
USE local
IMPLICIT NONE

INTEGER, INTENT(IN) :: list_levp(nlevp),list_levm(nlevm),kist
REAL, INTENT(OUT) :: virt_orog(np)

REAL (KIND=8) :: ml_lnp(nlevm)
REAL (KIND=8) :: lnpp,lnp1,lnp2
REAL :: z1,z2,zsup_pl1
INTEGER :: cnt_extrp,kp,klevp,klevm,pl1,ml1
!--------------------------------------------------------------------------

cnt_extrp = 0
DO kp = 1,np

  ml_lnp(1:nlevm) = LOG(ml_pp2(kp,1:nlevm))

! pl1: pressure level di riferimento ,i.e. 1o livello P che cade all'
!      interno della fascia coperta dai model levels.
! lnpp: ln della sua pressione

  DO klevp = 1,nlevp
    lnpp = LOG(REAL(list_levp(klevp)))
    IF (lnpp < ml_lnp(1)) EXIT
  ENDDO
  pl1 = klevp

  IF (pl1 > nlevp) THEN       
!   se tutti i livelli P sotto terra, avverto e mi fermo
    WRITE (*,*) "Tutti i livelli P sottoterra, punto: ",kp
    STOP
  ENDIF

! ml1 = model level sottostante
  DO klevm = nlevm,1,-1
    IF (lnpp < ml_lnp(klevm)) EXIT
  ENDDO

  IF (klevm == 0) THEN
    STOP "Erroraccio calc_zmodel_aladin"
  ELSE IF (klevm == nlevm) THEN
    ml1 = klevm - 1
    cnt_extrp = cnt_extrp + 1
  ELSE
    ml1 = klevm
  ENDIF

! interpolo (o estrapolo) la quota dalla superifcie del livello P di
! riferimento
  z1 = list_levm(ml1)
  z2 = list_levm(ml1+1)
  lnp1 = ml_lnp(ml1)
  lnp2 = ml_lnp(ml1+1)
  zsup_pl1 = REAL(z1 + (lnpp - lnp1) * ((z2-z1) / (lnp2-lnp1)))

! aggiungo ai model levels l'orog. stimata a partire dal liv.P di rif.
  virt_orog(kp) = pl_geop2(kp,pl1) / 9.8 - zsup_pl1
  ml_zmsl2(kp,1:nlevm) = list_levm(1:nlevm) + virt_orog(kp)

ENDDO

IF (cnt_extrp > 0) WRITE (*,'(a,i2,a,i4,a)') "Istante ",kist, &
  " dati Aladin estrapolati in ",cnt_extrp," punti"
!
RETURN
END SUBROUTINE calc_zmodel_aladin

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE encode_lev(model,tipo_lev,val_lev,level)
!
! Codifica (per findgrib90) il livello richiesto, per un dato modello e 
! tipo di livello (p o model)
!
IMPLICIT NONE

INTEGER, INTENT(IN) :: model,val_lev
CHARACTER (LEN=1), INTENT(IN) :: tipo_lev
INTEGER, INTENT(OUT) :: level(3)
!

! Pressure levels
IF (tipo_lev == "P") THEN
  level(1:3) = (/100,val_lev,0/)

! Model levels
ELSE IF (tipo_lev == "M") THEN
  IF (model == 1) THEN                ! ECMWF hybrid levels

  ELSE IF (model == 2) THEN           ! Lokal hybrid levels

  ELSE IF (model == 3) THEN           ! Aladin terrain-following levels
    level(1:3) = (/105,val_lev,0/)

  ENDIF   

ENDIF

RETURN
END SUBROUTINE encode_lev

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE encode_var(model,param,var)
!
! Codifica (per findgrib90) la variabile richiesta, per un dato modello
!
IMPLICIT NONE

INTEGER, INTENT(IN) :: model
CHARACTER (LEN=1), INTENT(IN) :: param
INTEGER, INTENT(OUT) :: var(3)
!

! ECMWF
IF (model == 1) THEN

  IF (param == "Z") THEN
    var = (/98,128,129/)
  ELSE IF (param == "T") THEN
    var = (/98,128,130/)
  ELSE IF (param == "U") THEN
    var = (/98,128,131/)
  ELSE IF (param == "V") THEN
    var = (/98,128,132/)
  ELSE IF (param == "Q") THEN
    var = (/98,128,133/)
  ENDIF

! Lokal
ELSE IF (model == 2) THEN

  IF (param == "Z") THEN
    var = (/200,2,6/)
  ELSE IF (param == "T") THEN
    var = (/200,2,11/)
  ELSE IF (param == "U") THEN
    var = (/200,2,33/)
  ELSE IF (param == "V") THEN
    var = (/200,2,34/)
  ELSE IF (param == "Q") THEN
    var = (/200,2,51/)
  ELSE IF (param == "P") THEN
    var = (/200,2,1/)
  ENDIF

! Aladin
ELSE IF (model == 3) THEN

  IF (param == "Z") THEN
    var = (/85,2,6/)
  ELSE IF (param == "T") THEN
    var = (/85,2,11/)
  ELSE IF (param == "U") THEN
    var = (/85,2,33/)
  ELSE IF (param == "V") THEN
    var = (/85,2,34/)
  ELSE IF (param == "Q") THEN
    var = (/85,2,52/)
  ELSE IF (param == "P") THEN
    var = (/85,2,1/)
  ENDIF

ENDIF   

RETURN
END SUBROUTINE encode_var

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE fatal_error(data,ora,scad,level,var,ier,id_msg)
!
! Stampo messaggio e termino esecuzione in caso di errore sui grib
!
IMPLICIT NONE

INTEGER, INTENT(IN) :: data(3),ora(2),scad(4),level(3),var(3),ier,id_msg

IF (id_msg == 1) THEN
  WRITE (*,'(a,i3,a)') "  Grib mancante, STOP (Error code findgrib90: ", &
    ier,")"
ELSE IF (id_msg == 2) THEN
  WRITE (*,'(a,i3)') "  Area grib diversa da area orografia, STOP"
ENDIF

WRITE (*,'(a,2i3,i5,2i3)') "    data/ora:  ",data(:),ora(:)
WRITE (*,'(a,4i4)')        "    scadenza:  ",scad(:)
WRITE (*,'(a,3i4)')        "    livello:   ",level(:)
WRITE (*,'(a,3i4)')        "    variabile: ",var(:)

STOP

END SUBROUTINE fatal_error

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE findgrib90(iuin,iulog,kbuffer,maxdim,data,ora,scad,level,var,ier)
!--------------------------------------------------------------------------
! Traduzione f90 della patrunesca findgrib.f (f77)
!
! Modifiche:
! - maxdim (dimensionamento del buffer kbuffer) diventa un argomento intero
!   e solo di input
! - non c'e' piu' la chiamata a setpar per avere il n.ro di bytes di un
!   intero (la subr. potrebbe essere meno portabile...)
!
!                                                 V 1.0,  Enrico 27/03/2003
!--------------------------------------------------------------------------
!
!	Ricerca all'interno di un file aperto con PBOPEN i grib
!	specificati dalla chiave data,ora,scad,level,var.
!       Tratta i  valori negativi nelle chiavi come wildcards
!
!	input:
!
!	iuin		I	unita` restituita da PBOPEN
!       maxdim          I       dimensionamento del buffer kbuffer
!
!	data(1)		I	giorno		)	
!	data(2)		I	mese		)
!	data(3)		I	anno			)  emissione
!	ora(1)		I	ora		)
!	ora(2)		I	minuti		)
!	scad(1)		I	indicator of unit of time range	(table 4)
!	scad(2)		I	periodo di tempo 1
!	scad(3)		I	periodo di tempo 2
!				(nel caso sia definita solo p1 o p2
!				viene testato solo il max tra p1 e p2)
!	scad(4)		I	time range indicator 		(table 5)
!	level(1)	I	indicator of type of level	(table 3)
!	level(2)	I	height, pressure etc. of levels
!	level(3)	I	height, pressure etc. of levels
!	var(1)		I	identification of originating/generating
!				 centre  (table 0)
!	var(2)		I	table 2 version number
!	var(3)		I	parameter			(table 2)
!
!	output:
!
!	kbuffer(maxdim)	I	buffer di debosito del grib estratto	
!	ier		I	codice errore
!				=0 tutto o.k.
!				=-1 grib not found
!				altri > vedi errori pbgrib
!--------------------------------------------------------------------------

IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: iuin,iulog,maxdim
INTEGER, INTENT(IN) :: data(3),ora(2),scad(4),level(3),var(3)
INTEGER, INTENT(OUT):: kbuffer(maxdim),ier

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: klen,kret
REAL :: psec2(512),psec3(2)
REAL :: field(maxdim)

! altre variabili della subroutine

INTEGER :: datag(3),orag(2),scadg(4),levelg(3),varg(3)
INTEGER :: igiro

!--------------------------------------------------------------------------

igiro = 0

DO 
! Leggo il prossimo grib
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)

! Gestisco errore ed EOF
  IF (kret == -1 .AND. igiro == 0) THEN       ! EOF 1a volta, rewind
    CALL pbseek(iuin,0,0,kret)
    igiro = 1
    WRITE (iulog,'(a)') "Findgrib90: rewind"
    CYCLE

  ELSE IF (kret == -1 .AND. igiro == 1) THEN  ! EOF 2a volta, mi arrendo
    ier = -1
    RETURN

  ELSE IF (kret /= 0) THEN                    ! erore di lettura, termino
    ier = kret
    RETURN

  ENDIF

! Decodifico l'header del Grib
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'I',kret)

! Se ho trovato il Grib giusto, termino
  datag  = (/ksec1(12),ksec1(11),ksec1(10)+(ksec1(21)-1)*100/)
  orag   = (/ksec1(13),ksec1(14)/)
  scadg  = (/ksec1(15),ksec1(16),ksec1(17),ksec1(18)/)
  levelg = (/ksec1(7), ksec1(8), ksec1(9)/)
  varg   = (/ksec1(2), ksec1(1), ksec1(6)/)

  IF (ALL( datag == data   .OR. data < 0  ) .AND. &
      ALL( orag == ora     .OR. ora < 0   ) .AND. &
      ALL( scadg == scad   .OR. scad < 0  ) .AND. &
      ALL( levelg == level .OR. level < 0 ) .AND. &
      ALL( varg == var     .OR. var < 0   )) THEN

    ier = 0
    RETURN

  ENDIF

ENDDO

END SUBROUTINE findgrib90

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE rtll(TLMD,TPHD,TLM0D,TPH0D,ALMD,APHD)        
!-------------------------------------------------------------------------
! trasforma le coordinate ruotate (TLMD,TPHD) in coordinate geografiche
! ordinarie (ALMD,APHD). I/O in gradi e decimi
! TLM0D, TPH0D: lon e lat del centro di rotazione, in gradi e decimi.
!-------------------------------------------------------------------------

REAL, PARAMETER :: DTR=3.141592654/180.

CTPH0=COS(TPH0D*DTR)
STPH0=SIN(TPH0D*DTR)

STPH=SIN(TPHD*DTR)
CTPH=COS(TPHD*DTR)
CTLM=COS(TLMD*DTR)
STLM=SIN(TLMD*DTR)

APH=ASIN(STPH0*CTPH*CTLM+CTPH0*STPH)                            
CPH=COS(APH)                                                    

ALMD=TLM0D+ASIN(STLM*CTPH/CPH)/DTR                               
APHD=APH/DTR                                                    

RETURN                                                          
END   

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_esempio
!
! Scrive un file grib2mm5.inp di esempio
!
IMPLICIT NONE

OPEN (UNIT=20, FILE="grib2mm5.inp", STATUS="REPLACE", FORM="FORMATTED")

!                            1234567890123456789012345678901234567890
WRITE (20,'(2a)')           "1        ! formato di output (1: mm5; 2:", &
                            " mm4)"
WRITE (20,'(2a)')           "1        ! modello (1: ECMWF; 2: LOKAL; ",& 
                            "3: ALADIN"
WRITE (20,'(2a)')           "1        ! parametri richiesti (1: solo ",& 
                            "vento; 2: T per sondaggi; 3: anche T, Q)"
WRITE (20,'(2a)')           "1        ! tipo scadenza (1:an non ini; ", & 
                            "2:forc00; 3:forc00+12; 4:an.ini)"
WRITE (20,'(2a)')           "6        ! time step tra gli istanti LAM", &
                            " (analisi o forecast; ore)"
WRITE (20,'(2a)')           "0        ! spin-up modello (prima scad. ", &
                            "di previsione da usare; ore)"
WRITE (20,'(a)')            "5        ! numero di pressure levels"
WRITE (20,'(2a)')           "800.     ! elenco dei pressure levels (u", &
                            "no per riga, in hPa, dal basso)"
WRITE (20,'(a)')            "700."
WRITE (20,'(a)')            "600."
WRITE (20,'(a)')            "500."
WRITE (20,'(a)')            "400."
WRITE (20,'(a)')            "9        ! numero di model levels"
WRITE (20,'(2a)')           "20.      ! elenco dei model levels (uno ", &
                            "per riga, dal basso)"
WRITE (20,'(a)')            "50."
WRITE (20,'(a)')            "100."
WRITE (20,'(a)')            "250."
WRITE (20,'(a)')            "500."
WRITE (20,'(a)')            "750."
WRITE (20,'(a)')            "1000."
WRITE (20,'(a)')            "1250."
WRITE (20,'(a)')            "1500."
WRITE (20,'(2a)')           "1        ! interpolazione temporale vent", &
                            "o (1: componenti; 2: algoritmo SMR)"
WRITE (20,'(2a)')           "1        ! calcolo di Z sui model lev. (", &
                            "1: uso orog; 2: uso geop su pres.lev.)"
CLOSE (20)

END SUBROUTINE scrive_esempio

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo l'help del programma
!
IMPLICIT NONE

WRITE (*,*)
WRITE (*,'(2a)') "Uso: grib2mm5.exe [-h] [-c] filedate filelam", & 
                 " fileorog filegrib" 
WRITE (*,'(2a)') "grib2mm5.exe -c: costruisce un file grib2mm5.inp di e", &
                 "sempio"
WRITE (*,'(a)')  "grib2mm5.exe -h: visualizza questo help"
WRITE (*,*)
WRITE (*,'(a)')  "  filedate: date estreme run calmet (date_calmet.inp)"
WRITE (*,'(2a)') "  filelam : descrizione dati e opzioni di elaborazione", &
                 "(grib2mm5.inp)"
WRITE (*,'(2a)') "  fileorog: orografia su area modello meteo ", & 
                 "(mm5orog_PROJ.grb)"
WRITE (*,'(a)')  "  filegrib: dati del modello meteo (mm5.grb)"
WRITE (*,*)

RETURN
END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE select_run(datac,hhc,tipo_scad,spin_up,iulog,data,ora,scad)
!--------------------------------------------------------------------------
! Seleziona il run del modello da cui devono essere presi i dati relativi
! alla data-ora corrente (datac,hhc), e ne restituisce (come richieste da 
! findgrib90): data di inizio, ora di inizio, scadenza.
! 
! NB: la codifica potrebbe dipendere anche dal modello (in questo caso, 
!     occorerra' passare come parametro anche model).
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

! Parametri della subroutine
TYPE(date), INTENT(IN) :: datac
INTEGER, INTENT(IN) :: hhc,tipo_scad,spin_up,iulog
INTEGER, INTENT(OUT) :: data(3),ora(2),scad(4)

! Altre variabili
TYPE(date) :: data_run
INTEGER :: hh_tot,hh_run,sca_run,h1,delta_day

!--------------------------------------------------------------------------
! 1) Calcolo data-ora-scadenza del run richiesto
SELECT CASE (tipo_scad)

! 1.1: I dati LAM sono analisi
CASE(1,4)
  data_run = datac
  hh_run = hhc

! 1.2: I dati LAM sono previsioni prese dal run delle 00Z
CASE(2)

  h1 = hhc - spin_up
  IF (h1 < 0) THEN                ! serve corsa di un giorno precedente
    delta_day = (-h1-1) / 24 + 1
    sca_run = hhc + delta_day * 24
  ELSE                            ! serve corsa del giorno corrente
    delta_day = 0
    sca_run = hhc
  ENDIF

  data_run = datac - delta_day
  hh_run = 0

! 1.3: I dati LAM sono previsioni prese dai run delle 00Z e delle 12Z 
CASE(3)

  h1 = hhc - spin_up
  IF (h1 < 0) THEN                ! serve corsa di un giorno precedente
    delta_day = (-h1-1) / 24 + 1
    hh_run = (h1 + delta_day*24)/12 *12
    sca_run = MOD(h1 + delta_day*24, 12)
  ELSE                            ! serve corsa del giorno corrente
    delta_day = 0
    hh_run = (h1/12) * 12
    sca_run = MOD(h1, 12)
  ENDIF

  data_run = datac - delta_day

! 1.4: Tipo_scad illegale
CASE DEFAULT
  WRITE (*,*) "Errore, tipo_scad illegale: ",tipo_scad
  STOP

END SELECT

!--------------------------------------------------------------------------
! 2) Output e debug

data = (/data_run%dd, data_run%mm, data_run%yy/)
ora  = (/hh_run, 0/)

IF (tipo_scad == 1) THEN
  scad = (/1,0,0,0/)
ELSE IF (tipo_scad == 4) THEN
  scad = (/1,0,0,1/)
ELSE
  scad = (/1,sca_run,0,0/)
ENDIF

WRITE (iulog,*)
WRITE (iulog,'(a,2i3,i5,i3)')      "datac,hhc:         ",datac,hhc
WRITE (iulog,'(a,2i3,i5,i3,a,i3)') "data/ora/scad run: ", &
  data_run,hh_run," +",sca_run

RETURN
END SUBROUTINE select_run

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE time_interp(khr,step_ist,int_wind)
!
! A partire dai dati relativi a due istanti LAM (*l_*1 e *l_*2), calcola i
! dati relativi a un'ora compresa tra questi e li salva negli arrays *l_*0
!
! khr e' l'ora richiesta, all'interno dell'intervallo tra i due istanti 
!   LAM: khr = 0 si riferisce al primo istante (*l_*1), khr = step_ist al 
!   secondo istante (*l_*2)
! step_ist e' il passo temporale tra due istanit LAM
! int_wind indica il tipo di interpolazione richiesta per il vento (1: per
!   componenti; 2: algoritmo SMR)
!
USE local
IMPLICIT NONE

INTEGER, INTENT(IN) :: khr,step_ist,int_wind 

!--------------------------------------------------------------------------
! 1) Interpolo pressure levels: 
!
! Considerato che i dati sui pr.lev. sono in realta' interpolati (i.e. non
! sono rappresentativi di uno strato preciso), non mi preoccupo del fatto 
! che la quota dei livelli cambia nel tempo. Interpolo come se i livelli
! fossero orizzontali, e attribuisco ai dati interpolati una quota 
! interpolata (sempre linearmente)

IF (nlevp > 0) THEN

  pl_zmsl0(:,:) = pl_zmsl1(:,:) + &
    (pl_zmsl2(:,:) - pl_zmsl1(:,:)) * REAL(khr) / REAL(step_ist)
  pl_geop0(:,:) = pl_geop1(:,:) + &
    (pl_geop2(:,:) - pl_geop1(:,:)) * REAL(khr) / REAL(step_ist)

  IF (par_ext == 2 .OR. par_ext == 3) pl_tt0(:,:) = pl_tt1(:,:) + &
      (pl_tt2(:,:) - pl_tt1(:,:)) * REAL(khr) / REAL(step_ist)
  IF (par_ext == 3) pl_qq0(:,:) = pl_qq1(:,:) + &
      (pl_qq2(:,:) - pl_qq1(:,:)) * REAL(khr) / REAL(step_ist)

  IF (int_wind == 1) THEN
    pl_uu0(:,:) = pl_uu1(:,:) + &
      (pl_uu2(:,:) - pl_uu1(:,:)) * REAL(khr) / REAL(step_ist)
    pl_vv0(:,:) = pl_vv1(:,:) + &
      (pl_vv2(:,:) - pl_vv1(:,:)) * REAL(khr) / REAL(step_ist)
  ENDIF

ENDIF
!--------------------------------------------------------------------------
! 2) Interpolo model levels: 
!
! Aladin e Lokal hanno livelli con altezza fissa -> interpolo linearmente
! senza problemi; se si volessero usare i model levels ECMWF o LAMBO, 
! bisognerebbe probabilmente studiare qualcosa di piu' astuto...

IF (nlevm > 0) THEN

  ml_zmsl0(:,:) = ml_zmsl1(:,:) + &
    (ml_zmsl2(:,:) - ml_zmsl1(:,:)) * REAL(khr) / REAL(step_ist)
  ml_pp0(:,:) = ml_pp1(:,:) + &
    (ml_pp2(:,:) - ml_pp1(:,:)) * REAL(khr) / REAL(step_ist)

  IF (par_ext == 2 .OR. par_ext == 3) ml_tt0(:,:) = ml_tt1(:,:) + &
      (ml_tt2(:,:) - ml_tt1(:,:)) * REAL(khr) / REAL(step_ist)
  IF (par_ext == 3) ml_qq0(:,:) = ml_qq1(:,:) + &
      (ml_qq2(:,:) - ml_qq1(:,:)) * REAL(khr) / REAL(step_ist)

  IF (int_wind == 1) THEN
    ml_uu0(:,:) = ml_uu1(:,:) + &
      (ml_uu2(:,:) - ml_uu1(:,:)) * REAL(khr) / REAL(step_ist)
    ml_vv0(:,:) = ml_vv1(:,:) + &
      (ml_vv2(:,:) - ml_vv1(:,:)) * REAL(khr) / REAL(step_ist)
  ENDIF

ENDIF

RETURN
END SUBROUTINE time_interp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE uv2dirint(u,v,dd,ff,nval,rmis)
!
! Dati i vettori di componenti u e v, ritona quelli di direzione e modulo
! Se una componente e' mancante dd e ff sono mancanti.
! Se u=v=0, mette ff=dd=0
!

IMPLICIT NONE

INTEGER, INTENT(IN):: nval
REAL, INTENT(IN) :: u(nval),v(nval),rmis
REAL, INTENT(OUT) :: dd(nval),ff(nval)

REAL, PARAMETER :: dtr = 180./3.141592654
INTEGER k
!-------------------------------------------------------------------------

DO k = 1,nval

  IF (u(k) == rmis .OR. v(k) == rmis) THEN
    dd(k) = rmis
    ff(k) = rmis
    CYCLE
  ENDIF

  IF	  (u(k) <= 0. .AND. v(k) < 0.) THEN    ! 0 - 90
    dd(k) = dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) < 0. .AND. v(k) >= 0.) THEN    ! 90 - 180
    dd(k) = 90. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) >= 0. .AND. v(k) > 0.) THEN    ! 180 - 270
    dd(k) = 180. + dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) > 0. .AND. v(k) <= 0.) THEN    ! 270 - 360
    dd(k) = 270. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) == 0. .AND. v(k) == 0.) THEN
    dd(k) = 0.
  ENDIF

  ff(k) = SQRT (u(k)*u(k) + v(k)*v(k))

ENDDO

RETURN
END SUBROUTINE uv2dirint

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE uv2dirint_scalar(u,v,dd,ff,nval,rmis)
!
! Dati i vettori di componenti u e v, ritona quelli di direzione e modulo
! Se una componente e' mancante dd e ff sono mancanti.
! Se u=v=0, mette ff=dd=0
!

IMPLICIT NONE

INTEGER, INTENT(IN):: nval
REAL, INTENT(IN) :: u,v,rmis
REAL, INTENT(OUT) :: dd,ff

REAL, PARAMETER :: dtr = 180./3.141592654
INTEGER k
!-------------------------------------------------------------------------

DO k = 1,nval

  IF (u == rmis .OR. v == rmis) THEN
    dd = rmis
    ff = rmis
    CYCLE
  ENDIF

  IF	  (u <= 0. .AND. v < 0.) THEN    ! 0 - 90
    dd = dtr*atan( u/v )
  ELSE IF (u < 0. .AND. v >= 0.) THEN    ! 90 - 180
    dd = 90. + dtr*atan( -v/u )
  ELSE IF (u >= 0. .AND. v > 0.) THEN    ! 180 - 270
    dd = 180. + dtr*atan( u/v )
  ELSE IF (u > 0. .AND. v <= 0.) THEN    ! 270 - 360
    dd = 270. + dtr*atan( -v/u )
  ELSE IF (u == 0. .AND. v == 0.) THEN
    dd = 0.
  ENDIF

  ff = SQRT (u*u + v*v)

ENDDO

RETURN
END SUBROUTINE uv2dirint_scalar

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_rec_ml(kp,iuout,iulog)
!
! Per un dato punto, scrive sul file di output i record relativi ai model 
! levels. I dati sono presi dagli arrarys ml_* del modulo local.
!
USE local
IMPLICIT NONE
!
INTEGER , INTENT(IN) :: kp,iuout,iulog
REAL :: dd(nlevm),ff(nlevm),rh
INTEGER :: klevm
!-------------------------------------------------------------------------

IF (nlevm <= 0) RETURN

! Calcolo dir e modulo del vento sulla colonna
CALL uv2dirint(ml_uu0(kp,1:nlevm),ml_vv0(kp,1:nlevm), &
               dd(1:nlevm),ff(1:nlevm),nlevm,-9999.)
!
IF (par_ext == 1) THEN
  DO klevm = 1,nlevm
    WRITE (iuout,'(i4,i6,f6.1,i4,f5.1)') &
      NINT(ml_pp0(kp,klevm)),NINT(ml_zmsl0(kp,klevm)),-99.9, &
      NINT(dd(klevm)),ff(klevm)
  ENDDO

ELSE IF (par_ext == 2) THEN
  DO klevm = 1,nlevm
    WRITE (iuout,'(i4,i6,f6.1,i4,f5.1)') &
      NINT(ml_pp0(kp,klevm)),NINT(ml_zmsl0(kp,klevm)),ml_tt0(kp,klevm), &
      NINT(dd(klevm)),ff(klevm)
  ENDDO


ELSE IF (par_ext == 3) THEN
  DO klevm = 1,nlevm
    CALL qtorelhum(ml_qq0(kp,klevm),ml_pp0(kp,klevm),ml_tt0(kp,klevm),rh)
    IF (rh < 0 .OR. rh > 100) rh_out = rh_out + 1
    IF (rh < 0 .OR. rh > 120) WRITE (iulog,*) "RH out of range: ",rh

    WRITE (iuout,'(i4,i6,f6.1,i4,f5.1,i4,f8.4)') &
      NINT(ml_pp0(kp,klevm)),NINT(ml_zmsl0(kp,klevm)),ml_tt0(kp,klevm), &
      NINT(dd(klevm)),ff(klevm),NINT(rh),ml_qq0(kp,klevm)
  ENDDO

ENDIF

!
RETURN
END SUBROUTINE write_rec_ml

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_rec_pl(kp,iuout,iulog,list_levp)
!
! Per un dato punto, scrive sul file di output i record relativi ai 
! pressure levels. I dati sono presi dagli arrarys pl_* del modulo local.
!
! Se uso anche i model levels, i pressure levels che cadono nella fascia 
! dei model levels vengono eliminati e sostituiti con copie dell'ultimo 
! model level (il n.ro di livelli in output deve essere sempre lo stesso)
!
! NB: Calcolo anche RH (che non viene usata da calmet) per monitorare i 
! valori non fisici che potrebbero comparire interpolando nel tempo 
! separatamente T e Q.
!
USE local
IMPLICIT NONE
!
INTEGER , INTENT(IN) :: kp,iuout,iulog,list_levp(nlevp)
REAL :: dd(nlevp),ff(nlevp),dd_rep,ff_rep,rh
INTEGER :: klevp
!-------------------------------------------------------------------------

IF (nlevp <= 0) RETURN

! Ri-calcolo dd e ff dell'ultimo model level
IF (nlevm > 0) THEN
  CALL uv2dirint_scalar(ml_uu0(kp,nlevm),ml_vv0(kp,nlevm),dd_rep,ff_rep, &
    1,-9999.)
ENDIF

! Calcolo dir e modulo del vento sulla colonna
CALL uv2dirint(pl_uu0(kp,1:nlevp),pl_vv0(kp,1:nlevp), &
               dd(1:nlevp),ff(1:nlevp),nlevp,-9999.)

! Richiesto solo il vento
IF (par_ext == 1) THEN

  DO klevp = 1,nlevp
    IF (nlevm > 0 .AND. pl_zmsl0(kp,klevp) < ml_zmsl0(kp,nlevm)) THEN
      WRITE (iuout,'(i4,i6,f6.1,i4,f5.1)') &
        NINT(ml_pp0(kp,nlevm)),NINT(ml_zmsl0(kp,nlevm)),-99.9, &
        NINT(dd_rep),ff_rep

    ELSE
      WRITE (iuout,'(i4,i6,f6.1,i4,f5.1)') &
        list_levp(klevp),NINT(pl_zmsl0(kp,klevp)),-99.9, &
        NINT(dd(klevp)),ff(klevp)

    ENDIF
  ENDDO

! Richiesta anche T (solo per sondaggi)
ELSE IF (par_ext == 2) THEN

  DO klevp = 1,nlevp
    IF (nlevm > 0 .AND. pl_zmsl0(kp,klevp) < ml_zmsl0(kp,nlevm)) THEN
      WRITE (iuout,'(i4,i6,f6.1,i4,f5.1)') &
        NINT(ml_pp0(kp,nlevm)),NINT(ml_zmsl0(kp,nlevm)),ml_tt0(kp,nlevm), &
        NINT(dd_rep),ff_rep

    ELSE
      WRITE (iuout,'(i4,i6,f6.1,i4,f5.1)') &
        list_levp(klevp),NINT(pl_zmsl0(kp,klevp)),pl_tt0(kp,klevp), &
        NINT(dd(klevp)),ff(klevp)

    ENDIF
  ENDDO

! Rihieste anche t e q per Calmet
ELSE IF (par_ext == 3) THEN

  DO klevp = 1,nlevp
    IF (nlevm > 0 .AND. pl_zmsl0(kp,klevp) < ml_zmsl0(kp,nlevm)) THEN

      CALL qtorelhum(ml_qq0(kp,nlevm),ml_pp0(kp,nlevm),ml_tt0(kp,nlevm),rh)
      IF (rh < 0 .OR. rh > 100) rh_out = rh_out + 1
      IF (rh < 0 .OR. rh > 120) WRITE (iulog,*) "RH out of range: ",rh

      WRITE (iuout,'(i4,i6,f6.1,i4,f5.1,i4,f8.4)') &
        NINT(ml_pp0(kp,nlevm)),NINT(ml_zmsl0(kp,nlevm)),ml_tt0(kp,nlevm), &
        NINT(dd_rep),ff_rep,NINT(rh),ml_qq0(kp,nlevm)

    ELSE

      CALL qtorelhum(pl_qq0(kp,klevp),REAL(list_levp(klevp)), &
        pl_tt0(kp,klevp),rh)
      IF (rh < 0 .OR. rh > 100) rh_out = rh_out + 1
      IF (rh < 0 .OR. rh > 120) WRITE (iulog,*) "RH out of range: ",rh

      WRITE (iuout,'(i4,i6,f6.1,i4,f5.1,i4,f8.4)') &
        list_levp(klevp),NINT(pl_zmsl0(kp,klevp)),pl_tt0(kp,klevp), &
        NINT(dd(klevp)),ff(klevp),NINT(rh),pl_qq0(kp,klevp)

    ENDIF
  ENDDO

ENDIF

!
RETURN
END SUBROUTINE write_rec_pl

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!                       SUBROUTINES DA TERMOLIB
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!-------------------------------------------------------------------------
SUBROUTINE relhumtoq(rh,p,t,q)
!
! calcola l'umidita specifica a partire dalla umidita relativa
!

q = rh*(0.622*sat(t))/(p-0.378*sat(t))
IF (q.lt.0.) q=0.

RETURN
END 

!-------------------------------------------------------------------------
SUBROUTINE qtorelhum(q,p,t,rh)
! CALCOLA L'UMIDITA RELATIVA A PARTIRE DALLA UMIDITA SPECIFICA
! REF.: BAKER, MON.WEA.REV.,1983,111,PAG.328 E SEG.

rh = q*(p-0.378*sat(t))/(0.622*sat(t))
IF (RH.LT.0.) RH=0.

RETURN
END 

!-------------------------------------------------------------------------
FUNCTION sat(t)
!
! CALCOLA LA TENSIONE DI VAPORE SATURO A PARTIRE DALLA TEMPERATURA
! REF.: BAKER, MON.WEA.REV.,1983,111,PAG.328 E SEG.
! T in Kelvin, SAT(T) in mb (?)

IF(t.gt.273.16)THEN
  a=17.269
  b=35.86
ELSE
  a=21.874
  b=7.66
ENDIF

sat=6.11*EXP(a*(t-273.16)/(t-b))

RETURN
END 
!-------------------------------------------------------------------------

PROGRAM exdom_grib
!--------------------------------------------------------------------------
! Legge un pacco grib relativo a una variabile (scalare) e lo riscrive,
! interpolato in orizzontale (gestendo lo scanning flag), nel formato 
! richiesto da diagmet (o da interp).
! L'output ha gli stessi livelli e scadenze dell'input, e l'ordine delle 
! martici segue le convenzioni di Chimere (scanning 010 e livelli 
! ordinati dal basso)
!
! Uso: exdom_grib.exe filein fileout filecoord/-noint [-ln] [-r rule]
!      [-lsmask filelsm]
!
! Note: 
! I grib nel file di input dovrebbero essere in ordine e raggruppati per 
!   scadenza, altrimenti il programma diventa molto piu' lento.
!
! Deduce direttamente dal grib: data iniziale e finale, n.ro e valore dei
!   livelli, n.ro e valore delle scadenze, parametri della griglia.
!
! Richiede che i grib del file differiscano solo per data/scad e livello
!
!                                         Versione 3.2.1, Enrico 07/12/2012
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: maxdim = 100000  ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxist = 1000    ! n.ro max istanti nel grib
INTEGER, PARAMETER :: maxlev = 60      ! n.ro max livelli
INTEGER, PARAMETER :: maxpts = 100000  ! n.ro max punti griglia output

INTEGER, PARAMETER :: imis = 9999
INTEGER, PARAMETER :: iulog = 90

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim),lsm(maxdim)

! Altre variabili del programma
REAL :: values(maxpts,maxlev)
TYPE(date) :: data1,data2,datac,datap
TYPE(date) :: data_list_org(maxist),data_list_sort(maxist)
REAL :: lon_out(maxpts),lat_out(maxpts)
REAL :: w1,w2,lat_dum,lon_dum,lsm_min,lsm_mis
INTEGER :: ksec1_first(1024),ksec2_first(1024)
INTEGER :: lev_list_org(3,maxlev),idx_lev(maxlev)
INTEGER :: hh_list_org(maxist),hh_list_sort(maxist)
INTEGER :: data(3),ora(2),level(3),var(3)
INTEGER :: np_in,np_out,nx,ny,nt,nz,dt
INTEGER :: hh1,hh2,hhc,hhp,hh_tot,scanning,delta,nt_calc,idp,idata
INTEGER :: k,kk,kl,kt,kz,kp,ier,ios,eof,eor,j,j2,iu,iu1,iu2
CHARACTER (LEN=80) :: filein,fileout,filecoord,filelsm,chdum,arg(3)
CHARACTER (LEN=1) :: rule,next_arg
LOGICAL :: log_int,hor_int,lsm_int,first,newlev,newist

!==========================================================================
! 1) Elaborazioni preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

! default
hor_int=.TRUE.
log_int=.FALSE.
lsm_int=.FALSE.
rule="l"

idp = 0
next_arg = ""
DO kp = 1,HUGE(kp)
  CALL getarg(kp,chdum)

  IF (chdum == "") THEN
    EXIT
  ELSE IF (next_arg == "r") THEN
    rule = chdum(1:1)
    next_arg = ""
    CYCLE
  ELSE IF (next_arg == "m") THEN
    filelsm = chdum
    next_arg = ""
    CYCLE
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-ln") THEN    
    log_int= .TRUE.
    CYCLE
  ELSE IF (TRIM(chdum) == "-noint") THEN    
    hor_int= .FALSE.
    CYCLE
  ELSE IF (TRIM(chdum) == "-r") THEN    
    next_arg = "r"
    CYCLE
  ELSE IF (TRIM(chdum) == "-lsmask") THEN    
    lsm_int = .TRUE.
    next_arg = "m"
    CYCLE
  ENDIF

  idp = idp + 1
  arg(idp) = chdum
ENDDO

filein = arg(1)
fileout = arg(2)
filecoord = arg(3)

IF (filein == "" .OR. fileout == "" .OR. &
    (hor_int .AND. filecoord == "") .OR. &
    (lsm_int .AND. filelsm == "") .OR. &
    (rule /= "l" .AND. rule /= "n") ) THEN
  CALL write_help
  STOP
ENDIF

OPEN (UNIT=iulog,FILE="exdom_grib.log",STATUS="REPLACE",FORM="FORMATTED")

!--------------------------------------------------------------------------
! 1.2) Scorro una prima volta i GRIB per trovare: date estreme, numero e 
!      valore dei livelli, passo temporale, parametri griglia.
!      Controllo anche che tutti gli altri parametri dei GRIB non cambino.

CALL grsvck(0)
CALL PBOPEN (iu1,filein,'R',kret)
IF (kret.ne.0) GOTO 9999 

data1 = date(31,12,9999)
hh1 = 24
data2 = date(1,1,0)
hh2 = 0
dt = imis
first = .TRUE.
nz = 0
nt = 0

DO kk=1,HUGE(0)
  
! Leggo il prossimo GRIB
  CALL PBGRIB(iu1,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

! Lo decodifico
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! Controllo che i parametri del GRIB non cambino all'interno del file
  IF (first) THEN
    ksec1_first(:) = ksec1(:)
    ksec2_first(:) = ksec2(:)
    nx = ksec2(2)
    ny = ksec2(3)
    np_in = nx * ny
    scanning = ksec2(11)

  ELSE
    IF (ksec1(5) == 0 .OR. ksec1(5) == 64) GOTO 9987
    IF (ksec1(1) /= ksec1_first(1) .OR. ksec1(6) /= ksec1_first(6) .OR. &
        ANY(ksec2(1:11) /= ksec2_first(1:11)) .OR. &
        ANY(ksec2(13:14) /= ksec2_first(13:14)) ) GOTO 9998
    IF (ANY(ksec1(2:4) /= ksec1_first(2:4))) THEN
      WRITE (*,*) "WARNING: ci sono differenze nella sez.1, elementi 2-4"
      WRITE (*,'(a10,3(1x,i6))') "1o grib: ",ksec1_first(2:4)
      WRITE (*,'(a4,i4,a2,3(1x,i6))') "grib",kk,": ",ksec1(2:4)
    ENDIF
    IF (ANY(ksec2(15:) /= ksec2_first(15:)) ) WRITE (*,*) &
      "WARNING: Ci sono differenze in ksec2 (indici > 14): campo ",kk

  ENDIF

! Calcolo le date estreme (solo per log)
  CALL ksec1_valid(ksec1,datac,hhc,ier)

  IF (datac < data1 .OR. (datac == data1 .AND. hhc < hh1) ) THEN
    data1 = datac
    hh1 = hhc
  ELSE IF (datac > data2 .OR. (datac == data2 .AND. hhc > hh2) ) THEN
    data2 = datac
    hh2 = hhc
  ENDIF

! Calcolo il passo temporale (solo per log)
  IF (.NOT. first) THEN
    IF (datac < datap .OR. (datac == datap .AND. hhc <= hhp) ) THEN
      delta = (datap - datac) * 24 + (hhp - hhc)
    ELSE IF (datac > datap .OR. (datac == datap .AND. hhc > hhp) ) THEN
      delta = (datac - datap) * 24 + (hhc - hhp)
    ENDIF    
    IF (delta > 0) dt = MIN(dt,delta)
  ENDIF

  datap = datac
  hhp = hhc

! Se ho trovato un nuovo livello, lo memorizzo
  level = (/ksec1(7),ksec1(8),ksec1(9)/)

  newlev = .TRUE.
  DO kl = 1,nz
  IF (ALL(level(1:3) == lev_list_org(1:3,kl))) THEN
    newlev = .FALSE.
    EXIT
  ENDIF
  ENDDO 
  IF (newlev) THEN
    IF (nz >= maxlev) GOTO 9997
    nz = nz + 1
    lev_list_org(1:3,nz) = level(1:3)
  ENDIF

! Se ho trovato un nuovo istante, lo memorizzo
  newist = .TRUE.
  DO kt = 1,nt
  IF (datac == data_list_org(kt) .AND. hhc == hh_list_org(kt)) THEN
    newist = .FALSE.
    EXIT
  ENDIF
  ENDDO 
  IF (newist) THEN
    IF (nt >= maxist) GOTO 9996
    nt = nt + 1
    data_list_org(nt) = datac
    hh_list_org(nt) = hhc
  ENDIF

  first = .FALSE.
ENDDO
! NB: per qualche motivo sconosciuto, PBCLOSE da' "segmentation fault", per
!     cui sono stato costretto a commentare l'istruzione.
!CALL PBCLOSE(iu1)

!--------------------------------------------------------------------------
! 1.3) Leggo da fileccord le coordinate dei punti di output

IF (hor_int) THEN
  CALL get_eof_eor(eof, eor)
  OPEN (UNIT=25, FILE=filecoord, STATUS="OLD", ACTION="READ", ERR=9993)
  
  np_out = 0
  DO
    READ (25,*,IOSTAT=ios) lon_dum,lat_dum
    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9992
    IF (np_out >= maxpts) GOTO 9991
  
    np_out = np_out + 1
    lon_out(np_out) = lon_dum
    lat_out(np_out) = lat_dum
  
  ENDDO
  CLOSE (25)

ELSE
  np_out = np_in
  IF (np_out >= maxpts) GOTO 9991

ENDIF

WRITE (*,'(a,i6,a)') "exdom_grib.f90: richiesti ",np_out," punti"

!--------------------------------------------------------------------------
! 1.4) Se richiesto, leggo la land-sea mask e verifico che abbia la stessa
!      griglia dei dati

IF (lsm_int) THEN

! Leggo LSM
  CALL PBOPEN(iu2,filelsm,'R',kret)
  CALL PBGRIB(iu2,kbuffer,maxdim*4,klen,kret)
  IF (kret < -1) GOTO 9990
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               lsm,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
  
! Controlli:sul grib LSM (stessa griglia dei dati, deve contenre una LSM)
! NB: i "direction increments" possono essere definiti in uno solo dei due 
!     files, e se non sono definiti possono risultare diversi
!     non controllo i "vertical coordinate parameters"

  IF (ksec1(1) /= 2 .OR. ksec1(6) /= 81) WRITE (*,*) & 
    "Warning, parametro inatteso nel file LSM: ",ksec1(1),ksec1(6)
  IF (MAXVAL(lsm(1:np_in)) > 1. .OR. MINVAL(lsm(1:np_in)) < 0.) &
    WRITE (*,*) "Warning, valori sospetti in LSM: min, max ", &
    MINVAL(lsm(1:np_in)),MAXVAL(lsm(1:np_in))
  
  IF (ANY(ksec2(1:5) /= ksec2_first(1:5)) .OR. &
      ksec2(7) /= ksec2_first(7) .OR. &
      ksec2(8) /= ksec2_first(8) .OR. &
      ksec2(11) /= ksec2_first(11) .OR. &
      ANY(ksec2(13:20) /= ksec2_first(13:20)) ) GOTO 9989
  IF (ksec2(6) == 128 .AND. ksec2_first(6) == 128 .AND. &
      (ksec2(9) /= ksec2_first(9) .OR. ksec2(10) /= ksec2_first(10)) ) &
     GOTO 9989

! Stabilisco il valore minimo della LSM perche' un dato sia utilizzato
  IF ((ksec1_first(1) == 2 .AND. ksec1_first(6) == 86) .OR. &
      (ksec1_first(1) == 201 .AND. ksec1_first(6) == 198) ) THEN
!   Soil moisture LM (1o livello di suolo = 10 cm): nelle celle con LSM<0.5
!   vale sempre 0.; altrove si considera la cella interamente di terra.
!   L'unita' di misura e' mmH2O, da cui il valore di saturazione 100.
    lsm_min = 0.5
    lsm_mis = 100.
  ELSE
    GOTO 9988

  ENDIF

! NB: per qualche motivo sconosciuto, PBCLOSE da' "segmentation fault", per
!     cui sono stato costretto a commentare l'istruzione.
!  CALL PBCLOSE(iu2)

ELSE
  lsm(:) = 1.

ENDIF

!--------------------------------------------------------------------------
! 1.5) Ordino i livelli (dal basso verso l'alto) e gli istanti (dal primo 
!      all'ultimo)

CALL sort_levels(lev_list_org,maxlev,nz,idx_lev,ier,iulog)
IF (ier > 0 .OR. (ier < 0 .AND. nz /= 1) ) &
  WRITE (*,*) "Warning sort_levels: ier ",ier

CALL sort_ist(data_list_org,hh_list_org,maxist,nt, &
              data_list_sort,hh_list_sort,ier,iulog)
IF (ier /= 0) WRITE (*,*) "Warning sort_ist: ier ",ier

!--------------------------------------------------------------------------
! 1.6) Controlli e log a schermo del contenuto dei GRIB
IF (data2 == data1 .AND. hh2 == hh1) THEN
  nt_calc = 1
ELSE IF (dt <= 0 .OR. dt == imis .OR. data2 < data1 .OR. &
  (data2 == data1 .AND. hh2 < hh1)) THEN
  WRITE (*,*) "Internal error in dates"
  STOP
ELSE
  hh_tot = (data2 - data1) * 24 + (hh2 - hh1 + 1)
  IF (MOD(hh_tot - 1, dt) /= 0) THEN
    nt_calc = -999
  ELSE
    nt_calc = (hh_tot - 1) / dt + 1
  ENDIF
ENDIF

IF (scanning == 0) THEN
  WRITE (*,'(a)') "Necessaria inversione asse Y "
ELSE IF (scanning /= 64) THEN
  GOTO 9995
ENDIF

WRITE (*,'(2a)')       "Contenuto di ",TRIM(filein)
WRITE (*,'(a,3i6)')    "Nx, Ny, Nz     :",nx,ny,nz
WRITE (*,'(a,i5,3i3)') "Data iniziale  :",data1%yy,data1%mm,data1%dd,hh1
WRITE (*,'(a,i5,3i3)') "Data finale    :",data2%yy,data2%mm,data2%dd,hh2
WRITE (*,'(a,i6)')     "N.ro istanti   :",nt
IF (nt_calc == nt) THEN
  WRITE (*,'(a,i6)')   "Passo temporale:",dt
ELSE
  WRITE (*,'(a,i6)')   "Passo temporale non calcolabile"
ENDIF
IF (hor_int) THEN
  IF (log_int) WRITE (*,'(a)') "Richiesta interpolazione logaritimica"
  IF (rule == "n") WRITE (*,'(a)') "Richiesta interpolazione punto piu vicino"
ELSE
  WRITE (*,'(a)') "Interpolazione non richiesta"
ENDIF

!==========================================================================
! 2) Leggo i GRIB, interpolo nello spazio e scrivo il file Chimere. 

! Preliminari
var = (/ksec1_first(2),ksec1_first(1),ksec1_first(6)/)

OPEN (UNIT=40, FILE=fileout, STATUS="REPLACE", FORM="UNFORMATTED")
CALL PBOPEN (iu,filein,'R',kret)

! Apro il ciclo sugli istanti
DO kt = 1,nt

! Data corrente (input) e parametri derivati
  datac = data_list_sort(kt)
  hhc = hh_list_sort(kt)
  data = (/datac%dd,datac%mm,datac%yy/)
  ora = (/hhc,0/)
  idata = hhc + datac%dd*100 + datac%mm*10000 + datac%yy*1000000

! Apro il ciclo sui livelli
  DO kz = 1,nz

    level = lev_list_org(1:3,kz)

!   Cerco il GRIB e lo decodifico
    CALL findgrib90_val(iu,iulog,kbuffer,maxdim,data,ora,level,var,ier)
    IF (ier /= 0) GOTO 9994
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

!   Se richiesto, interpolo nello spazio
    IF (hor_int) THEN
      IF (log_int) field(1:np_in) = LOG(field(1:np_in))
  
      CALL interp_sp(np_in,np_out,ksec2_first,field(1:np_in), &
        lat_out(1:np_out),lon_out(1:np_out),rule, &
        lsm_int,lsm_min,lsm_mis,lsm(1:np_in), &
        values(1:np_out,idx_lev(kz)),ier)
      IF (ier /= 0) CONTINUE
  
      IF (log_int) values(1:np_out,idx_lev(kz)) = &
        EXP(values(1:np_out,idx_lev(kz)))

     ELSE
       values(1:np_in,idx_lev(kz)) = field(1:np_in)

     ENDIF
  ENDDO

! Scrivo sul file di output
  WRITE (40) idata,((values(k,kz),k=1,np_out),kz=1,nz)
  IF (kt == 1) WRITE (*,'(2(a,i5),3i3,2(a,i4))')  "Elaborato primo istante:  ", &
    kt,": ",datac%yy,datac%mm,datac%dd,hhc
  IF (kt == nt) WRITE (*,'(2(a,i5),3i3,2(a,i4))') "Elaborato ultimo istante: ", &
    kt,": ",datac%yy,datac%mm,datac%dd,hhc

ENDDO

STOP

!==========================================================================
! 3) Gestione errori

9999 CONTINUE
WRITE(*,*) "Errore pbopen: kret ",kret," ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,'(a,i5)') "Errore, area o parametro diversi nel grib ",kk
WRITE (*,'(a,2i6)') "Primo grib, ksec1(1,6): ",ksec1_first(1),ksec1_first(6)
WRITE (*,'(a,2i6)') "Curr. grib, ksec1(1,6): ",ksec1(1),ksec1(6)
WRITE (*,'(a,14(1x,i6))') "Primo grib, ksec2(1:14):",ksec2_first(1:14)
WRITE (*,'(a,14(1x,i6))') "Curr. grib, ksec2(1:14):",ksec2(1:14)
STOP

9997 CONTINUE
WRITE (*,*) "Richiesti troppi livelli, modificare il parametro maxlev"
STOP

9996 CONTINUE
WRITE (*,*) "Richiesti troppi istanti, modificare il parametro maxist"
STOP

9995 CONTINUE
WRITE (*,*) "Scanning flag non gestito: ",scanning
STOP

9994 CONTINUE
WRITE (*,*) "Grib non trovato o errore findgrib:"
WRITE (*,*) "ier: ",ier
WRITE (*,*) "Istante: ",data,ora(1)
WRITE (*,*) "Livello: ",level
STOP

9993 CONTINUE
WRITE(*,*) "Errore aprendo ",TRIM(filecoord)
STOP

9992 CONTINUE
WRITE(*,*) "Errore leggendo ",TRIM(filecoord)
STOP

9991 CONTINUE
WRITE (*,'(a,i6,a,i6,a)') "Richiesti troppi punti in output (",np_out, &
  "), modificare il parametro maxpts (ora ",maxpts,")"
STOP

9990 CONTINUE
WRITE (*,*) "Errore leggendo file Land Sea Mask: ",TRIM(filelsm)
STOP

9989 CONTINUE
WRITE (*,*) "Griglia LSM diversa da griglia dati"
WRITE (*,'(a/,2(10i8/))') "Dati, ksec2(1:20): ",ksec2_first(1:20)
WRITE (*,'(a/,2(10i8/))') "LSM,  ksec2(1:20): ",ksec2(1:20)
STOP

9988 CONTINUE
WRITE (*,*) "Interpolazione lsmask non gestita per il parametro ", &
  ksec1_first(1),ksec1_first(6)
WRITE (*,*) "(impossibile calcolare lsm_min e lsm_mis)"
STOP

9987 CONTINUE
WRITE (*,*) "Grib senza sezione 2, interpolazione impossibile"
STOP

END PROGRAM exdom_grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE interp_sp(np_in,np_out,ksec2,field,ygeo,xgeo,rule, &
  lsm_int,lsm_min,lsm_mis,lsm,values2d,ier)
!--------------------------------------------------------------------------
! Interpola orizzonalmente il campo field, definito sulla griglia descritta
! in ksec2, sui punti sparsi di coordinate geografiche xgeo,ygeo.
!
! Gestisce 2 tipi di interpolazione: lineare (rule = "l"), nearest point 
! (rule = "n")
!--------------------------------------------------------------------------

USE grid_handler
IMPLICIT NONE

! Argomenti della subroutine
REAL, INTENT(IN) :: field(np_in),ygeo(np_out),xgeo(np_out)
REAL, INTENT(IN) :: lsm_min,lsm_mis,lsm(np_in)
INTEGER, INTENT(IN) :: np_in,np_out,ksec2(1024)
CHARACTER (LEN=1), INTENT(IN) :: rule
LOGICAL, INTENT(IN) :: lsm_int
REAL, INTENT(OUT) :: values2d(np_out)
INTEGER, INTENT(OUT) :: ier

! Parametro eps: mettere >0. per evitere "fuori area" dovuti a troncamento
! numerico quando le griglie di input e output coincidono. 
! Se un punto cade fuori area ma entro eps passi griglia da una cella della
! griglia input, viene spostato dentro la griglia, a eps passi dal bordo.
REAL, PARAMETER :: eps = 0.001

! Variabili locali
TYPE (grid) :: grid_in
REAL :: cyrot,syrot,xgrid,ygrid,rigrid,rjgrid
REAL :: z1,z2,z3,z4,xp,yp,zp,wtot,val,w
INTEGER :: k,iz,igrid,jgrid,kgrid,ib,jb
INTEGER :: kg(4)
LOGICAL :: out
!--------------------------------------------------------------------------

!WRITE (*,*) "ORG: Min,max,med ",MAXVAL(field(1:np_in)),MINVAL(field(1:np_in)), &
!  SUM(field(1:np_in))/REAL(np_in)
!print *,"rule ",rule

!--------------------------------------------------------------------------
! 1) Definisco la griglia di ingresso

CALL build_grid(ksec2,field,grid_in)

IF (grid_in%proj == "GEO") THEN
  cyrot = cos( grid_in%yrot/dtr )
  syrot = sin( grid_in%yrot/dtr )
ENDIF

!--------------------------------------------------------------------------
! 2) Ciclo sui punti richiesti. Se un punto cade fuori dal dominio, assegno
!    comunque il valore del punto di bordo piu' vicino.

out = .FALSE.
DO k = 1,np_out

! Coordinate del punto richiesto nella griglia di input
  IF (grid_in%proj == "GEO") THEN
    CALL tlld(xgeo(k),ygeo(k),grid_in%xrot,cyrot,syrot,xgrid,ygrid)
  ELSE IF (grid_in%proj == "UTM") THEN
    CALL ll2utm(ygeo(k),xgeo(k),grid_in%utmz,xgrid,ygrid,iz)
  ENDIF

  rigrid = (xgrid-grid_in%x1) / grid_in%dx
  rjgrid = (ygrid-grid_in%y1) / grid_in%dy


! Se il punto richiesto e' fuori area ma nell'intorno di un punto della 
! griglia input, lo sposto (per evitare messaggi di "fuori area" dovuti a
! errori di troncamento quando le griglie input e output sono uguali)
  IF (rigrid > -eps .AND. rigrid <= 0. .AND. &
      ABS(rjgrid-NINT(rjgrid)) <= eps) THEN
    rigrid = eps
  ELSE IF (rigrid >= REAL(grid_in%nx-1) .AND. rigrid < REAL(grid_in%nx-1)+eps .AND. &
      ABS(rjgrid-NINT(rjgrid)) <= eps) THEN
    rigrid = REAL(grid_in%nx-1) - eps
  ENDIF
  IF (rjgrid > -eps .AND. rjgrid <= 0. .AND. &
      ABS(rigrid-NINT(rigrid)) <= eps) THEN
    rjgrid = eps
  ELSE IF (rjgrid >= REAL(grid_in%ny-1) .AND. rjgrid < REAL(grid_in%ny-1)+eps .AND. &
      ABS(rigrid-NINT(rigrid)) <= eps) THEN
    rjgrid = REAL(grid_in%ny-1) - eps
  ENDIF

! Punto di bordo piu' vicino
  ib = MIN(MAX(1,NINT(rigrid)),grid_in%nx)
  jb = MIN(MAX(1,NINT(rjgrid)),grid_in%ny)

  SELECT CASE (rule)

! 2.1) Interpolazione con punto piu' vicino; 
  CASE ('n')
    igrid = NINT(rigrid) + 1
    jgrid = NINT(rjgrid) + 1

    IF (igrid < 1 .OR. igrid > grid_in%nx .OR. &
        jgrid < 1 .OR. jgrid > grid_in%ny ) THEN
      kgrid = (jb-1)*grid_in%nx + ib
      out = .TRUE.
    ELSE
      kgrid = (jgrid-1)*grid_in%nx + igrid
    ENDIF

    IF (.NOT. lsm_int .OR. lsm(kgrid) > lsm_min) THEN
      values2d(k) = grid_in%field(kgrid)
    ELSE
      values2d(k) = lsm_mis
    ENDIF
   
! 2.2) Interpolazione lineare
  CASE ('l') 

!   trovo il punto in basso a sn della cella in cui cade il punto
    igrid = INT(rigrid) + 1
    jgrid = INT(rjgrid) + 1

!   Se sono fuori griglia assegno il valore del bordo piu' vicino
    IF (igrid < 1 .OR. igrid+1 > grid_in%nx .OR. &
        jgrid < 1 .OR. jgrid+1 > grid_in%ny ) THEN
      kgrid = (jb-1)*grid_in%nx + ib
      values2d(k) = grid_in%field(kgrid)
      out = .TRUE.
      CYCLE      
    ENDIF

!   Trovo indici e valori dei 4 vertici della cella 
    kg(1) = (jgrid-1)*grid_in%nx + igrid
    kg(2) = (jgrid-1)*grid_in%nx + igrid + 1
    kg(3) = jgrid*grid_in%nx + igrid + 1
    kg(4) = jgrid*grid_in%nx + igrid

    z1 = grid_in%field(kg(1))
    z2 = grid_in%field(kg(2))
    z3 = grid_in%field(kg(3))
    z4 = grid_in%field(kg(4))

    xp = rigrid - INT(rigrid)
    yp = rjgrid - INT(rjgrid)

    IF (.NOT. lsm_int .OR. ALL(lsm(kg(1:4))>lsm_min)) THEN
!     Interpolo linearmente
      CALL hbilin (z1,z2,z3,z4, 0.,0.,1.,1., xp,yp,zp)
      values2d(k) = zp

    ELSE
!     Interpolo 1/R2 a partire dai soli punti di terra
      wtot = 0.
      val = 0.
      IF (lsm(kg(1)) > lsm_min) THEN
          w = 1./(xp**2 + yp**2)
          val = val + w * z1
          wtot = wtot + w
      ENDIF
      IF (lsm(kg(2)) > lsm_min) THEN
          w = 1./((1.-xp)**2 + yp**2)
          val = val + w * z2
          wtot = wtot + w
      ENDIF
      IF (lsm(kg(3)) > lsm_min) THEN
          w = 1./((1.-xp)**2 + (1.-yp)**2)
          val = val + w * z3
          wtot = wtot + w
      ENDIF
      IF (lsm(kg(4)) > lsm_min) THEN
          w = 1./(xp**2 + (1.-yp)**2)
          val = val + w * z4
          wtot = wtot + w
      ENDIF

      IF (wtot > 0.) THEN
        values2d(k) = val / wtot
      ELSE
        values2d(k) = lsm_mis
      ENDIF

    ENDIF

  END SELECT 
ENDDO

IF (out) THEN 
  WRITE (*,*) "Warning, assegnato valore del bordo a punti fuori dominio"
  ier = 1
ELSE
  ier = 0
ENDIF

!WRITE (*,*) "INT: Min,max,med ",MAXVAL(values2d(1:np_out)), &
!  MINVAL(values2d(1:np_out)),SUM(values2d(1:np_out))/REAL(np_out)
!READ *

RETURN
END SUBROUTINE interp_sp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE findgrib90_val(iuin,iulog,kbuffer,maxdim,data,ora,level,var,ier)
!--------------------------------------------------------------------------
! Traduzione f90 della patrunesca findgrib.f (f77).
! Versione ulteriormente modificata per cercare il grib in base alla 
!   data/ora di validita', invece di data/ora di emissione + scadenza.
!
! Modifiche (rispetto a findgrib):
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
!	data(3)		I	anno		)  validita'
!	ora(1)		I	ora		)
!	ora(2)		I	minuti		)
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
!	kbuffer(maxdim)	I	buffer di deposito del grib estratto	
!	ier		I	codice errore
!				=0 tutto o.k.
!				=-1 grib not found
!				altri > vedi errori pbgrib
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: iuin,iulog,maxdim
INTEGER, INTENT(IN) :: data(3),ora(2),level(3),var(3)
INTEGER, INTENT(OUT):: kbuffer(maxdim),ier

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: klen,kret
REAL :: psec2(512),psec3(2)
REAL :: field(maxdim)

! altre variabili della subroutine
TYPE (date) :: data_valg
INTEGER :: datag(3),orag(2),scadg(4),levelg(3),varg(3),hh_valg
INTEGER :: igiro,ier2

!--------------------------------------------------------------------------

igiro = 0

DO 
! Leggo il prossimo grib
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)

! Gestisco errore ed EOF
  IF (kret == -1 .AND. igiro == 0) THEN       ! EOF 1a volta, rewind
    CALL pbseek(iuin,0,0,kret)
    igiro = 1
    WRITE (iulog,'(a)') "Findgrib90_val: rewind"
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
  CALL ksec1_valid(ksec1,data_valg,hh_valg,ier2)
  IF (ier2 /= 0) THEN
    ier = -2
    RETURN
  ENDIF

  datag  = (/data_valg%dd,data_valg%mm,data_valg%yy/)
  orag   = (/hh_valg,ksec1(14)/)
  levelg = (/ksec1(7), ksec1(8), ksec1(9)/)
  varg   = (/ksec1(2), ksec1(1), ksec1(6)/)

  IF (ALL(datag == data) .AND. ALL(orag == ora) .AND. &
      ALL(levelg == level) .AND. ALL(varg == var) ) THEN
    ier = 0
    RETURN
  ENDIF

ENDDO

END SUBROUTINE findgrib90_val

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE ksec1_valid(ksec1,data,hh,ier)
!--------------------------------------------------------------------------
! Data la sezione 1 di un GRIB, ritorna la data e l'ora di validita'
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
CASE(0)
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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE sort_levels(lev_in,maxlev,nz,idx_lev,ier,iulog)
!
! Dato un elenco di terne descrittive di livelli GRIB, ritorna un vettore
! con l'indice di ciascun livello contato dal basso (convenzione Chimere)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: lev_in(3,maxlev),maxlev,nz,iulog
INTEGER, INTENT(OUT) :: idx_lev(maxlev),ier
!
REAL :: mxl,mnl
INTEGER :: k,k2,idx,levtype
LOGICAL :: left(nz)
!--------------------------------------------------------------------------

levtype = lev_in(1,1)

! Controllo che i livelli siano dello stesso tipo
IF (ANY(lev_in(1,1:nz) /= levtype)) THEN
  DO k = 1,nz
    idx_lev(k) = k
  ENDDO
  ier = 1
  RETURN
ENDIF

! Se c'e' un solo livello o i livelli sono alla superficie termino
IF (nz <= 1 .OR. levtype == 1) THEN
  DO k = 1,nz
    idx_lev(k) = k
  ENDDO
  ier = -1
  RETURN
ENDIF

! Ordino i livelli
SELECT CASE (levtype)

! livelli P o ibridi, 1o livello al top (LM, ECMWF)
CASE (100,101,107:110) 
  left(:) = .TRUE.

  DO k = 1,nz
!   idx = MAXLOC(lev_in(2,1:nz),DIM=1,MASK=left(1:nz))
    idx = 0
    mxl = -HUGE(0.)
    DO k2 = 1,nz
      IF (lev_in(2,k2) > mxl .AND. left(k2)) THEN
        idx = k2
        mxl = lev_in(2,k2)
      ENDIF
    ENDDO
    IF (idx == 0) STOP "Errore ricerca livello massimo"

    idx_lev(idx) = k
    left(idx) = .FALSE.
  ENDDO

! livelli terrain-following, 1o livello alla superficie (Calmet)
CASE (105,106)
  left(:) = .TRUE.
  DO k = 1,nz
!   idx = MINLOC(lev_in(2,1:nz),DIM=1,MASK=left(1:nz))
    idx = 0
    mnl = HUGE(0.)
    DO k2 = 1,nz
      IF (lev_in(2,k2) < mnl .AND. left(k2)) THEN
        idx = k2
        mnl = lev_in(2,k2)
      ENDIF
    ENDDO
    IF (idx == 0) STOP "Errore ricerca livello minimo"

    idx_lev(idx) = k
    left(idx) = .FALSE.
  ENDDO

! livelli non gestiti, non cambio nulla e avviso
CASE DEFAULT
  DO k = 1,nz
    idx_lev(k) = k
  ENDDO
  ier = 2
  RETURN

END SELECT

ier = 0

WRITE (iulog,*) "Lista livelli"
WRITE (iulog,*) idx_lev(1:nz)

RETURN
END SUBROUTINE sort_levels

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE sort_ist(data_in,hh_in,maxist,nt,data_out,hh_out,ier,iulog)
!
! Dato un elenco di date/ore, le ritorna ordinate in senso crescente
!
USE date_handler
IMPLICIT NONE
!
TYPE(date), INTENT(IN) :: data_in(maxist)
INTEGER, INTENT(IN) :: hh_in(maxist),maxist,nt,iulog
TYPE(date), INTENT(OUT) :: data_out(maxist)
INTEGER, INTENT(OUT) :: hh_out(maxist),ier
!
INTEGER :: k,k2,idx,jday,julh(maxist),mnd
LOGICAL :: left(nt)
!--------------------------------------------------------------------------

! Controllo di non superare il massimo intero rappresentabile
IF (HUGE(1) < 210000000) THEN
  data_out(1:nt) = data_in(1:nt)
  hh_out(1:nt) = hh_in(1:nt)
  ier = 1
  RETURN
ENDIF

! Trasformo le date in YYYYJJJHH (julh)
DO k = 1,nt
  jday = jul(data_in(k))
  julh(k) = data_in(k)%yy*100000 + jday*100 + hh_in(k)
ENDDO

! Ordino le date
left(:) = .TRUE.
DO k = 1,nt
!  idx = MINLOC(julh(1:nt),1,MASK=left(1:nt))
  idx = 0
  mnd = HUGE(0)
  DO k2 = 1,nt
    IF (julh(k2) < mnd .AND. left(k2)) THEN
      idx = k2
      mnd = julh(k2)
    ENDIF
  ENDDO
  IF (idx == 0) STOP "Errore ricerca prima data"

  data_out(k) = data_in(idx)
  hh_out(k) = hh_in(idx)
  left(idx) = .FALSE.
ENDDO

ier = 0

WRITE (iulog,*) "Lista istanti"
DO k=1,nt
  WRITE (iulog,*) data_out(k),hh_out(k)
ENDDO

RETURN
END SUBROUTINE sort_ist

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: exdom_grib.exe filein fileout filecoord/-noint [-ln] [-r rule]"
WRITE (*,*) "                    [-lsmask filelsm]"
WRITE (*,*) "  filecoord: elenco coordinate griglia output (file COORD)"
WRITE (*,*) "  -noint:    non interpola (output sulla griglia di input)"
WRITE (*,*) "  -ln:       interpola il logaritmo del parametro (usare per"
WRITE (*,*) "             interpolare la pressione su livelli inclinati)"
WRITE (*,*) "  rule:      algoritmo per interplazione orizzontale:"
WRITE (*,*) "               l  (int. lineare, default) "
WRITE (*,*) "               n  (punto piu vicino)"
WRITE (*,*) "  -lsmask:   interpola usando solo i punti di terra i.e. il cui valore nel"
WRITE (*,*) "             grib filelsm e' < 0.5"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

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

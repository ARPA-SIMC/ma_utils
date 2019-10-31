PROGRAM grib23ddat
!--------------------------------------------------------------------------
! Reads the ouptut of a NWP model (GRIB format), and writes it in 3D.DAT 
! format, version 2.1 or 3.
! Use: grib3ddat.exe file_static file_dat file_out
!
! Input:
! - file_static: topography and height of model layers
! - file_dat:    meteorolgical data, GRIB1 format
! - grib23dddat.inp: program namelist
!
! NOTES: 
! - GRIB data in file_dat:
!   * must be defined on the same grid as file_static
!   * can contain analysis, or forecast from a single model run (only 
!     one reference time)
!   * muts be defined on model layers (indicatorOfTypeOfLevel=110)
!   * must be sorted for increasing verfication time 
!
! - Management of 3D water variables: apparently (subroutine RDMM5),
!   calmet never uses the variables W, Qr, Qi, Qs e Qg; it uses Qc (if
!   availbale), to calculate Ceiling Height. Therefore, this program
!   allows 4 options for input fields related with water (keyword inp_h2o):
!   0) None
!   1) Only specific humidity (the program writes RH and mix.ratio)
!   2) Specific humidity + cloud water
!   3) Specific humidity + cloud water + cloud ice (the program writes
!      cloud water + cloud ice as cloud water, cloud ice is not written) 
!
! - How Calmet reads 3D.DAT file. This involves subroutines:
!     - rdhd5 (version of 3D.DAT format)
!     - rdhd52/rdhd53 (headers)
!     - rdmm5 (data record)
!   Calmet deduce the version of 3D.DAT file from the content of the first
!   2 records, and save it in the variable imm53d (sub. rdhd5), that can be:
!     0: MM5.DAT   - Calmet V?.?, sub. rdhd51 (presently not implemented)
!     1: 3D.DAT V1 - Calmet V5.2, sub. rdhd52 (presently not implemented)
!     2: 3D.DAT V2 - Calmet V5.5/5.8 sub rdhd53
!     3: 3D.DAT V3 - Calmet V6.*
!   All Calmet versions continue to read older versions of 3D.DAT file. 
!   The variable ioutmm5 (sub. rdhd5*) encode the list of parameters 
!   available in input.
! 
! - According to documnetation (version 2.1 of outputs), missing values in 
!   surface fields (in the header of data records) should be set to 0., 
!   instead of -9999. (subroutine write_dat)
!
! - The height of model layers should be considered from MSL, and not from
!   the surface (CALPUFF manual, table 8.21)
!
! FUTURE DEVELOPEMENTS:
! - allow no-hourly input
! - allow input on pressure levels
! - add more fields to file_static:
!   * land fraction: to discriminate points of land and sea (keyword
!     ilu, used in subr. RDHD5 e RDHD53 when itwprog=2
!   * albedo: to try to evaluate the presence of snow on the ground 
! - Add to file_dat the surface fields that are read but not used 
!   (sub. RDMM5, line 20175)
!
!                                                 V2.0.0, Enrico 25/10/2019
!--------------------------------------------------------------------------

USE grib_api
USE date_handler
IMPLICIT NONE

!==========================================================================
! 0) Dichiarazioni

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.         ! valore per dati mancanti REAL
INTEGER, PARAMETER :: imis = -9999       ! valore per dati mancanti INT
INTEGER, PARAMETER :: iunml = 20         ! unita' su cui scrivo fileout
INTEGER, PARAMETER :: iuout = 30         ! unita' su cui scrivo fileout
INTEGER, PARAMETER :: iulog = 90         ! unita' su cui scrivo log
CHARACTER (LEN=80), PARAMETER :: file_nml = "grib23ddat.inp"
CHARACTER (LEN=80), PARAMETER :: file_log = "grib23ddat.log"

! Funzioni
LOGICAL :: samegrid

! Altre varibaili del programma
REAL, ALLOCATABLE :: orog(:),zlev3d(:,:),xgeo(:,:),ygeo(:,:)
REAL, ALLOCATABLE :: values3d(:,:,:),values2d(:,:)
INTEGER, ALLOCATABLE :: id_lev3d(:),rq_par3d(:,:),rq_lev3d(:,:)
INTEGER, ALLOCATABLE :: rq_lev2d(:,:),rq_par2d(:,:)
LOGICAL, ALLOCATABLE :: found3d(:,:),found2d(:)

TYPE(date) :: data_ini,datac,datag,datav
REAL :: x1,y1,dx,dy,xx,yy,xrot,yrot,rdum
INTEGER :: nx,ny,nlev3d,nscad,npar3d,npar2d
INTEGER :: hh_step,nht,hh_ini,hhc,hhg,hhv,hht,scad(3),lev(3),par(3)
INTEGER :: tipo_scad,inp_h2o,out_fmt,dlth,utmz
INTEGER :: if_static,if_dat,ig_orog,ig,idxl,idxp
INTEGER :: iret,ier,idum(3),k,kpar,cnt_par,kl,kp,i,j,ilu,cnt_sea,cnt_out
CHARACTER (LEN=200) :: file_static,file_dat,file_out,error_message,chpar
CHARACTER (LEN=20) :: str_model,str_tipo_scad
CHARACTER (LEN=15) :: hr2
CHARACTER (LEN=3) :: next_arg
LOGICAL :: lmodif

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comandi

next_arg = ""
lmodif = .FALSE.
cnt_par = 0

DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(chpar) == "-c") THEN
    CALL scrive_esempio(file_nml)
    STOP
  ELSE IF (TRIM(chpar) == "-modif") THEN
    lmodif = .TRUE.
  ELSE IF (cnt_par == 0) THEN
    cnt_par = 1
    file_static = chpar
  ELSE IF (cnt_par == 1) THEN
    cnt_par = 2
    file_dat = chpar
  ELSE IF (cnt_par == 2) THEN
    cnt_par = 3
    file_out = chpar
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 1.2 Leggo le opzioni da grib23dddat.inp

OPEN (UNIT=iunml, FILE=file_nml, STATUS="OLD", ACTION="READ", ERR=9996)
READ (iunml,*, ERR=9996) tipo_scad
READ (iunml,*, ERR=9996) nscad
READ (iunml,*, ERR=9996) nlev3d
ALLOCATE (id_lev3d(1:nlev3d))
id_lev3d(:) = imis
READ (iunml,*, ERR=9996) id_lev3d(1:nlev3d)
READ (iunml,*, ERR=9996) inp_h2o
READ (iunml,*, ERR=9996) utmz
READ (iunml,*, ERR=9996) out_fmt
CLOSE (iunml)

!--------------------------------------------------------------------------
! 1.3 Controllo che le opzioni di file_nml siano legali 

IF ((tipo_scad/=1 .AND. tipo_scad/=2) .OR. &
    nscad < 1 .OR. &
    nlev3d < 1 .OR. &
    ANY(id_lev3d(1:nlev3d)==imis) .OR. &
    (inp_h2o/=1 .AND. inp_h2o/=2 .AND.inp_h2o/=3) .OR. &
    (out_fmt/=2 .AND. out_fmt/=3) &
   ) THEN      

  WRITE (*,*) "Parametri illegali in ",TRIM(file_nml)
  WRITE (*,*) nscad,nlev3d
  WRITE (*,*) id_lev3d(:)
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.4 Calcolo i parametri dipendenti dalle opzioni di file_nml

! 1.4.1 Step e durata totale del file (in ore)
hh_step = 1 
nht = 1+(nscad-1)*hh_step

! 1.4.2 Numero e codifica parametri e livelli richiesti in input: COSMO

! Numero e codifica grib dei parametri 3D letti nel file di input
IF (inp_h2o == 0) THEN               ! P,T,U,V; ioutmm5 = 81
  npar3d = 4
  ALLOCATE (rq_par3d(3,npar3d))
  rq_par3d(:,:) = RESHAPE((/200,2,1, 200,2,11, 200,2,33, 200,2,34/), &
    (/3,npar3d/))
  WRITE (hr2,'(5i3)') 0,0,0,0,0

ELSE IF (inp_h2o == 1) THEN          ! P,T,U,V,Q; ioutmm5 = 82
  npar3d = 5
  ALLOCATE (rq_par3d(3,npar3d))
  rq_par3d(:,:) = RESHAPE((/200,2,1, 200,2,11, 200,2,33, 200,2,34, &
    200,2,51/),(/3,npar3d/))
  WRITE (hr2,'(5i3)') 0,1,0,0,0

ELSE IF (inp_h2o == 2) THEN          ! T,P,U,V,Q,Qcr; ioutmm5 = 83
  npar3d = 6
  ALLOCATE (rq_par3d(3,npar3d))
  rq_par3d(:,:) = RESHAPE((/200,2,1, 200,2,11, 200,2,33, 200,2,34, &
    200,2,51, 200,2,150/),(/3,npar3d/))
  WRITE (hr2,'(5i3)') 0,1,1,0,0

ELSE IF (inp_h2o == 3) THEN          ! T,P,U,V,Q,Qcr,Qis; ioutmm5 = 83
  npar3d = 7
  ALLOCATE (rq_par3d(3,npar3d))
  rq_par3d(:,:) = RESHAPE((/200,2,1, 200,2,11, 200,2,33, 200,2,34, &
    200,2,51, 200,2,150, 200,2,151/),(/3,npar3d/))
  WRITE (hr2,'(5i3)') 0,1,1,0,0
ENDIF

! Codifica grib dei livelli 3d richiesti
ALLOCATE (rq_lev3d(3,nlev3d))
rq_lev3d(1,:) = 110
rq_lev3d(2,:) = id_lev3d(1:nlev3d)
rq_lev3d(3,:) = id_lev3d(1:nlev3d) + 1

! Numero e codifica grib dei parametri 2D letti nel file di input: 
! (Temperatura superficie e Preciptazione)
npar2d = 2
ALLOCATE (rq_par2d(3,npar2d))
rq_par2d(:,:) = RESHAPE((/200,2,11, 200,2,61/),(/3,npar2d/))

! Codifica grib dei livelli superficiali richiesti
ALLOCATE (rq_lev2d(3,npar2d))
rq_lev2d(:,:) = RESHAPE((/1,0,0, 1,0,0/),(/3,npar2d/))


! 1.4.4 Stringhe per log
str_model = "COSMO "        
IF (tipo_scad == 1) THEN
  str_tipo_scad = "Analisi"
ELSE IF (tipo_scad == 2) THEN
  str_tipo_scad = "Previsioni"
ENDIF

!--------------------------------------------------------------------------
! 1.5 Apro file log

OPEN (UNIT=iulog, FILE=file_log, STATUS="REPLACE", ACTION="WRITE")

!==========================================================================
! 2) Leggo file_static: parametri griglia, orografia, quota livelli

! Leggo grib orografia
CALL grib_open_file(if_static,file_static,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

ig_orog = -1
CALL grib_new_from_file(if_static,ig_orog,iret)
IF (iret /= GRIB_SUCCESS) GOTO 9998
CALL grib_get(ig_orog,"indicatorOfParameter",idum(1))
CALL grib_get(ig_orog,"indicatorOfTypeOfLevel",idum(2))
IF (idum(1) /= 8 .OR. idum(2) /= 1) GOTO 9997
CALL grib_get(ig_orog,"iScansNegatively",idum(1))
CALL grib_get(ig_orog,"jScansPositively",idum(2))
CALL grib_get(ig_orog,"jPointsAreConsecutive",idum(3))
IF (ANY(idum(1:3) /= (/0,1,0/))) GOTO 9992

! Leggo parametri griglia 
CALL grib_get(ig_orog,"numberOfPointsAlongAParallel",nx)
CALL grib_get(ig_orog,"numberOfPointsAlongAMeridian",ny)
CALL grib_get(ig_orog,"longitudeOfFirstGridPointInDegrees",x1)
CALL grib_get(ig_orog,"latitudeOfFirstGridPointInDegrees",y1)
CALL grib_get(ig_orog,"iDirectionIncrementInDegrees",dx)
CALL grib_get(ig_orog,"jDirectionIncrementInDegrees",dy)
CALL grib_get(ig_orog,"longitudeOfSouthernPoleInDegrees",xrot)
CALL grib_get(ig_orog,"latitudeOfSouthernPoleInDegrees",rdum)
yrot = rdum + 90.

! Alloco arrays dipendenti dal numero di punti
ALLOCATE (xgeo(nx,ny),ygeo(nx,ny),orog(nx*ny),zlev3d(nx*ny,nlev3d)) 
zlev3d(:,:) = rmis

! Salvo orografia
CALL grib_get(ig_orog,"values",orog)

! Se devo elaborare model layers COSMO, ne leggo le quote
DO
  ig = -1
  CALL grib_new_from_file(if_static,ig,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT   

!   Controlli
  IF (.NOT. samegrid(ig_orog,ig)) GOTO 9995
  CALL grib_get(ig,"indicatorOfTypeOfLevel",idum(1))
  CALL grib_get(ig,"topLevel",idum(2))
  CALL grib_get(ig,"bottomLevel",idum(3))
  IF (idum(1) /= 110 .OR. idum(3) /= idum(2)+1) GOTO 9994

!   Se e' uno dei livelli richiesti, salvo le quote
  DO k = 1,nlev3d
    IF (idum(2) == id_lev3d(k)) THEN
      CALL grib_get(ig,"values",zlev3d(:,k))
      EXIT
    ENDIF
  ENDDO

  CALL grib_release(ig)
ENDDO

IF (ANY(zlev3d(:,:) == rmis)) GOTO 9993

!==========================================================================
! 3) Scrivo gli header di file_out

! Se i grib sono ordinati, leggo la data iniziale dal primo grib
CALL grib_open_file(if_dat,file_dat,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9991
ig = -1
CALL grib_new_from_file(if_dat,ig,iret)
IF (iret /= GRIB_SUCCESS) GOTO 9990
CALL grib_get(ig,"dataDate",idum(1))
CALL grib_get(ig,"dataTime",idum(2))

data_ini%yy = idum(1)/10000
data_ini%mm = MOD(idum(1)/100,100)
data_ini%dd = MOD(idum(1),100)
hh_ini = idum(2) / 100

CALL grib_release(ig)
CALL grib_close_file(if_dat)

! Se richiesto, trasformo la data iniziale da GMT a LST
IF (utmz /= 0) THEN
  hht = hh_ini + utmz
  IF (hht < 0) THEN
    hh_ini = hht + 24
    data_ini = data_ini - 1
  ELSE IF (hht > 24) THEN
    hh_ini = hht - 24
    data_ini = data_ini + 1
  ELSE
    hh_ini = hht
  ENDIF
ENDIF

! Calcolo le coordinate geografiche di tutti i punti
DO j = 1,ny
DO i = 1,nx
  k = (j-1)*nx + i
  xx = x1 + (i-1) * dx
  yy = y1 + (j-1) * dy
  CALL rtll (xx,yy,xrot,yrot,xgeo(i,j),ygeo(i,j))
ENDDO
ENDDO

! Header records fissi
OPEN (UNIT=iuout, FILE=file_out, STATUS="REPLACE", ACTION="WRITE")

IF (out_fmt == 2) THEN
!                      12345678901234561234567890123456
  WRITE (iuout,'(a)') "3D.DAT          2.1             Analisi operative COSMO-I7" ! hr 1
  WRITE (iuout,'(a)') "0"                                       ! n.ro record commento
  WRITE (iuout,'(a15)') hr2                                     ! hr 2
  WRITE (iuout,'(a)') ""                                        ! hr 3
  WRITE (iuout,'(21i3)') (0,k=1,21)                             ! hr 4
  WRITE (iuout,'(i4.4,3i2.2,i5,3i4)') &                         ! hr 5
    data_ini%yy,data_ini%mm,data_ini%dd,hh_ini,nht,nx,ny,nlev3d
  WRITE (iuout,'(6i4,2f10.4,2f9.4)') &                          ! hr 6
    1,1,nx,ny,1,nlev3d,MINVAL(xgeo),MAXVAL(xgeo),MINVAL(ygeo),MAXVAL(ygeo)
  DO k = 1,nlev3d                                               ! sigma coeff.
    WRITE (iuout,'(f6.3)') 0.
  ENDDO

ELSE IF (out_fmt == 3) THEN 
  WRITE (iuout,'(a)') "3D.DAT            3             Analisi operative COSMO-I7" ! hr 1
  WRITE (iuout,'(a)') "0"                                       ! n.ro record commento
  WRITE (iuout,'(a)') hr2                                       ! hr 2
  WRITE (iuout,'(a)') ""                                        ! hr 3
  WRITE (iuout,'(21i3)') (0,k=1,21)                             ! hr 4
  WRITE (iuout,'(i4.4,3i2.2,i4,i5,i5,3i4)') &                   ! hr 5
    data_ini%yy,data_ini%mm,data_ini%dd,hh_ini, 0,nht,0, nx,ny,nlev3d
  WRITE (iuout,'(6i4,2f10.4,2f9.4)') &                          ! hr 6
    1,1,nx,ny,1,nlev3d,MINVAL(xgeo),MAXVAL(xgeo),MINVAL(ygeo),MAXVAL(ygeo)
  DO k = 1,nlev3d                                               ! sigma coeff.
    WRITE (iuout,'(f6.3)') 0.
  ENDDO

ENDIF

! Coordinate, quota e land-sea mask di ciascun punto. Il Land-Use (ilu) 
! serve solo per discriminare tra mare (16) e terra (/=16)
cnt_sea = 0
DO j = 1,ny
DO i = 1,nx
  k = (j-1)*nx + i
  IF (orog(k) < 0.015 .AND. orog(k) > 0.01) THEN
    ilu = 16
    cnt_sea = cnt_sea + 1
  ELSE
    ilu = 0
  ENDIF

  IF (out_fmt == 2) THEN
    WRITE (iuout,'(2i4,f9.4,f10.4,i5,i3,1x,f9.4,f10.4,i5)') &
      i,j,ygeo(i,j),xgeo(i,j),NINT(orog(k)),ilu,ygeo(i,j),xgeo(i,j),NINT(orog(k))
  ELSE IF (out_fmt == 3) THEN
    WRITE (iuout,'(2i4,f9.4,f10.4,i5,i3)') i,j,ygeo(i,j),xgeo(i,j),NINT(orog(k)),ilu
  ENDIF

ENDDO
ENDDO

! Log a schermo delle operazioni preliminari
WRITE (*,'(a)') "***   grib23ddat: inizio elaborazioni   ***"
WRITE (*,'(4a)') "Modello ",TRIM(str_model), &
  " scadenze di ",TRIM(str_tipo_scad)
WRITE (*,'(a,2i4,a,4(1x,f9.4))') &
  "Area grib: nx,ny ",nx,ny,"    x1,y1,dx,dy",x1,y1,dx,dy
IF (utmz == 0) THEN
  WRITE (*,'(a,i4.4,3(1x,i2.2),a,i4)') &
    "Data iniziale (GMT)",data_ini%yy,data_ini%mm,data_ini%dd,hh_ini, &
    ", durata attesa del input file (ore) ",nht
ELSE
  WRITE (*,'(a,i3,a,i4.4,3(1x,i2.2),a,i4)') &
    "Data iniziale (LST, utmz =",utmz,") ",data_ini%yy,data_ini%mm, &
    data_ini%dd,hh_ini,", durata attesa del input file (ore) ",nht
ENDIF
WRITE (*,'(2a,i2,a,i2)') "Parametri richiesti in input: ", &
  "model layers " ,npar3d,", superficiali ",npar2d
WRITE (*,'(a,3f7.0,i5,a,i5)') "Orografia (m): ave, max, min, p.ti mare ", &
  SUM(orog(:))/REAL(nx*ny), MAXVAL(orog(:)), MINVAL(orog(:)), cnt_sea,"/",nx*ny
WRITE (*,*)

!==========================================================================
! 4) Leggo i dati e scrivo data records: files di input ordinati per 
!    verification time

!--------------------------------------------------------------------------
! 4.1) Alloco arrays, apro file

ALLOCATE (values3d(nx*ny,nlev3d,npar3d),values2d(nx*ny,npar2d))
ALLOCATE (found3d(nlev3d,npar3d),found2d(npar2d))

CALL grib_open_file(if_dat,file_dat,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9991
ig = -1

cnt_out = 0
DO k = 1,HUGE(0)

!--------------------------------------------------------------------------
! 4.2 Leggo il prossimo grib, trovo data, scad, par, liv
  CALL grib_new_from_file(if_dat,ig,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9990
  IF (.NOT. samegrid(ig_orog,ig)) GOTO 9986

! data
  CALL grib_get(ig,"dataDate",idum(1))
  CALL grib_get(ig,"dataTime",idum(2))
  datag%yy = idum(1)/10000
  datag%mm = MOD(idum(1)/100,100)
  datag%dd = MOD(idum(1),100)
  hhg = idum(2) / 100

! Se richiesto, trasformo la data corrente da GMT a LST
  IF (utmz /= 0) THEN
    hht = hhg + utmz
    IF (hht < 0) THEN
      hhg = hht + 24
      datag = datag - 1
    ELSE IF (hht > 24) THEN
      hhg = hht - 24
      datag = datag + 1
    ELSE
      hhg = hht
    ENDIF
  ENDIF
  
! scad
  CALL grib_get(ig,"timeRangeIndicator",scad(1))
  CALL grib_get(ig,"startStep",scad(2))
  CALL grib_get(ig,"endStep",scad(3))
  
! liv
  CALL grib_get(ig,"indicatorOfTypeOfLevel",lev(1))
  CALL grib_get(ig,"topLevel",lev(2))
  CALL grib_get(ig,"bottomLevel",lev(3))
  
! par
  CALL grib_get(ig,"centre",par(1))
  CALL grib_get(ig,"gribTablesVersionNo",par(2))
  CALL grib_get(ig,"indicatorOfParameter",par(3))

!--------------------------------------------------------------------------
! 4.3 Gestisco i cambiamenti di istante
!
! datag,hhg: data di emissione dell'ultimo grib letto
! datav,hhv: data di validita' dell'ultimo grib letto
! datac,hhc: data cui si riferiscono i dati in memeoria (arrays values*)

  CALL scad2val(datag,hhg,scad,datav,hhv,ier)
  IF (ier /=0) GOTO 9988
  IF (ier == -1) WRITE (*,*) &
    "Warning, trovati dati non istantanei e non riferiti all'ora precedente"

! Primo grib del file: salvo data di validita', azzero gli arrays
  IF (k == 1) THEN
    datac = datav
    hhc = hhv    
    values3d(:,:,:) = rmis
    values2d(:,:) = rmis
    found3d(:,:) = .FALSE.
    found2d(:) = .FALSE.
  
  ELSE
    dlth = (datav - datac) * 24 + (hhv - hhc)

!   Nuovo istante: scrivo output istante precedente, azzero gli arrays
    IF (dlth == hh_step) THEN
      IF (COUNT(found3d(:,:)) /= nlev3d*npar3d .OR. &
          COUNT(found2d(:)) /= npar2d) GOTO 9987

!     Se richiesto, sovrascrivo i dati per test/debug
      IF (lmodif) THEN
        idum(1) = hhc-utmz
        IF (hhc-utmz < 0) THEN
          rdum = REAL(hhc-utmz+24)
        ELSE IF (hhc-utmz > 24) THEN
          rdum = REAL(hhc-utmz-24)
        ELSE
          rdum = REAL(hhc-utmz)
        ENDIF
        values3d(:,:,3) = rdum  ! U = ora GMT
        values3d(:,:,4) = 0.    ! V
      ENDIF

      CALL write_dat(values3d,zlev3d,values2d,nx,ny,nlev3d, &
        npar3d,npar2d,datac,hhc,iuout,inp_h2o,rmis,out_fmt)
!     CALL write_log(found3d,found2d)
      cnt_out = cnt_out + 1

      IF (utmz == 0) THEN
        WRITE (*,'(a,i4.4,3(1x,i2.2))') &
          "Elaborato istante (GMT): ",datac%yy,datac%mm,datac%dd,hhc
      ELSE
        WRITE (*,'(a,i3,a,i4.4,3(1x,i2.2))') "Elaborato istante (LST, utmz=", &
          utmz,"): ",datac%yy,datac%mm,datac%dd,hhc
      ENDIF
 
      datac = datav
      hhc = hhv

      values3d(:,:,:) = rmis
      values2d(:,:) = rmis
      found3d(:,:) = .FALSE.
      found2d(:) = .FALSE.

!   Stesso istante del grib precedente
    ELSE IF (dlth == 0) THEN
      CONTINUE

!   Istante inatteso
    ELSE
      GOTO 9989

    ENDIF
  ENDIF

!--------------------------------------------------------------------------
! 4.4 Se ho trovato un campo richiesto, lo salvo negli arrays

! 4.4.1 Elaboro un campo 3D
  IF (lev(1) == 110) THEN

    idxl = 0
    DO kl = 1,nlev3d
      IF (ALL(rq_lev3d(1:3,kl) == lev(1:3))) idxl = kl
    ENDDO
    idxp = 0
    DO kp = 1,npar3d
      IF (ALL(rq_par3d(1:3,kp) == par(1:3))) idxp = kp
    ENDDO

    IF (idxl /= 0 .AND. idxp /= 0) THEN
      found3d(idxl,idxp) = .TRUE.
      CALL grib_get(ig,"values",values3d(:,idxl,idxp))
    ENDIF

! 4.4.1 Elaboro un campo 2D
  ELSE IF (lev(1) == 1) THEN
    idxp = 0
    DO kp = 1,npar2d
      IF (ALL(rq_lev2d(1:3,kp) == lev(1:3)) .AND. &
          ALL(rq_par2d(1:3,kp) == par(1:3))) idxp = kp
    ENDDO

    IF (idxp /= 0) THEN
      found2d(idxp) = .TRUE.
      CALL grib_get(ig,"values",values2d(:,idxp))
    ENDIF

  ENDIF

! Chiudo ciclo lettura grib
  CALL grib_release(ig)
ENDDO

! 4.5 Scrivo l'ultimo istante e chiudo il file

! Se richiesto, sovrascrivo i dati per test/debug
IF (lmodif) THEN
  idum(1) = hhc-utmz
  IF (hhc-utmz < 0) THEN
    rdum = REAL(hhc-utmz+24)
  ELSE IF (hhc-utmz > 24) THEN
    rdum = REAL(hhc-utmz-24)
  ELSE
    rdum = REAL(hhc-utmz)
  ENDIF
  values3d(:,:,3) = rdum  ! U = ora GMT
  values3d(:,:,4) = 0.    ! V
ENDIF

CALL write_dat(values3d,zlev3d,values2d,nx,ny,nlev3d, &
  npar3d,npar2d,datac,hhc,iuout,inp_h2o,rmis,out_fmt)
!CALL write_log(found3d,found2d)
cnt_out = cnt_out + 1

IF (utmz == 0) THEN
  WRITE (*,'(a,i4.4,3(1x,i2.2))') &
    "Elaborato istante (GMT): ",datac%yy,datac%mm,datac%dd,hhc
ELSE
  WRITE (*,'(a,i3,a,i4.4,3(1x,i2.2))') "Elaborato istante (LST, utmz=", &
    utmz,"): ",datac%yy,datac%mm,datac%dd,hhc
ENDIF
WRITE (*,*) "Totale scadenze scritte: ",cnt_out

CALL grib_close_file(if_dat)


!==========================================================================
! 5) Conclusione

STOP

!==========================================================================
! 7) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_static)
CALL grib_get_error_string(iret,error_message)
WRITE (*,*) error_message
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo orografia "
CALL grib_get_error_string(iret,error_message)
WRITE (*,*) error_message
STOP

9997 CONTINUE
WRITE (*,*) "Il primo grib in ",TRIM(file_static)," non e' orografia"
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_nml)
STOP

9995 CONTINUE
WRITE (*,*) "Model layer definito su griglia diversa da orografia"
WRITE (*,*) TRIM(file_static)
STOP

9994 CONTINUE
WRITE (*,*) "Model layer incompatibile con modello richiesto"
WRITE (*,*) TRIM(file_static)
STOP

9993 CONTINUE
WRITE (*,*) "Quota dei model layers richiesti non trovata in ", &
  TRIM(file_static)
STOP

9992 CONTINUE
WRITE (*,*) "Scanning mode orografia non gestito: ",idum(1:3)
STOP

9991 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_dat)
CALL grib_get_error_string(iret,error_message)
WRITE (*,*) error_message
STOP

9990 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_dat)
CALL grib_get_error_string(iret,error_message)
WRITE (*,*) error_message
STOP

9989 CONTINUE
IF (utmz == 0) THEN
  WRITE (*,*) "Errore, data di validita' (GMT) non ordinata in ",TRIM(file_dat)
ELSE
  WRITE (*,'(a,i3,2a)') "Errore, data di validita' (LST, utmz =",utmz, &
    ") non ordinata in ",TRIM(file_dat)
ENDIF

WRITE (*,'(a,1x,i4.4,3(1x,i2.2))') &
  "Data precedente ",datac%yy,datac%mm,datac%dd,hhc
WRITE (*,'(a,1x,i4.4,3(1x,i2.2))') &
  "Data trovata    ",datav%yy,datav%mm,datav%dd,hhv
STOP

9988 CONTINUE
WRITE (*,'(a,3i4)') "Errore, scadenza non gestita ",scad(:)
STOP

9987 CONTINUE
WRITE (*,'(a,i4.4,3(1x,i2.2))') &
  "Campi incompleti elaborando l'istante: ",datac%yy,datac%mm,datac%dd,hhc
WRITE (*,'(2(a,i3),2(a,i2))') "Trovati: 3D ", &
  COUNT(found3d(:,:)),"/",nlev3d*npar3d,", 2D ",COUNT(found2d(:)),"/",npar2d
WRITE (*,'(2a)') "Verificare la consitenza tra grib23ddat.inp e ",TRIM(file_dat)
STOP

9986 CONTINUE
WRITE (*,*) "Campo in input definito su griglia diversa da orografia"
WRITE (*,*) "Campo ",k," file ",TRIM(file_dat)
STOP

END PROGRAM grib23ddat

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_esempio(file_nml)
!
! Scrive un file grib23ddat.inp di esempio
!
IMPLICIT NONE
CHARACTER (LEN=80),INTENT(IN) :: file_nml

OPEN (UNIT=20, FILE=file_nml, STATUS="REPLACE", FORM="FORMATTED")

!                            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (20,'(2a)')           "1        ! Tipo di scadenze (1: analisi, 2: previsioni)"
WRITE (20,'(2a)')           "24       ! Numero di scadenze da elaborare"
WRITE (20,'(2a)')           "24       ! Numero e lista dei livelli verticali (chiave ""topLevel""; ordinati dal basso)"
WRITE (20,'(a)')            "45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17"
WRITE (20,'(2a)')           "1        ! input H2O 3d (0:none, 1:Q, 2:Q+cloud water, 3:Q+cloud water+cloud ice"
WRITE (20,'(2a)')           "0        ! Time zone degli orari in output (0: ore GMT; /=0: LST; 1: LST Italia)"
WRITE (20,'(2a)')           "3        ! formato in output (2: versione 2.1; 3: versione 3)"
CLOSE (20)

! Deafult per LAMAZ 35 layers
!WRITE (20,'(2a)')           "21       ! Numero e lista dei livelli ve", &
!                            "rticali (ksec1(8); ordinati dal basso)"
!WRITE (20,'(a)')            "35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15"

END SUBROUTINE scrive_esempio

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo l'help del programma
!
IMPLICIT NONE

!                12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*)
WRITE (*,'(a)') "Use: grib3ddat.exe file_static file_dat file_out [-h] [-c] [-modif]"
WRITE (*,'(a)') ""
WRITE (*,'(a)') "file_static: grib format; contains topography (1st field) and height of model layers"
WRITE (*,'(a)') "file_dat:    meteorological input, grib format; mandatory fields: P,T,U,V,Prc,Tsup;"
WRITE (*,'(a)') "             optional fields: Q,Qcr,Qis "
WRITE (*,'(a)') "file_out:    ouptut file, 3D.DAT format"
WRITE (*,'(a)') "-h:          print this help"
WRITE (*,'(a)') "-c:          build a template for file grib23ddat.inp and terminates"
WRITE (*,'(a)') "-modif:      write in output ""fake"" values for test/debug"
WRITE (*,'(a)') ""

RETURN
END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION samegrid(ig1,ig2) RESULT (same)
!
! Dati 2 puntatori a messaggi grib, verifica se sono definiti sulla stessa 
! griglia
!
USE grib_api
IMPLICIT NONE

INTEGER :: ig1,ig2
LOGICAL :: same
REAL :: rval(2,6)
INTEGER :: ival(2,5)

CALL grib_get(ig1,"numberOfPointsAlongAParallel",      ival(1,1))
CALL grib_get(ig1,"numberOfPointsAlongAMeridian",      ival(1,2))
CALL grib_get(ig1,"iScansNegatively",                  ival(1,3))
CALL grib_get(ig1,"jScansPositively",                  ival(1,4))
CALL grib_get(ig1,"jPointsAreConsecutive",             ival(1,5))
CALL grib_get(ig1,"longitudeOfFirstGridPointInDegrees",rval(1,1))
CALL grib_get(ig1,"latitudeOfFirstGridPointInDegrees", rval(1,2))
CALL grib_get(ig1,"longitudeOfLastGridPointInDegrees", rval(1,3))
CALL grib_get(ig1,"latitudeOfLastGridPointInDegrees",  rval(1,4))
CALL grib_get(ig1,"iDirectionIncrementInDegrees",      rval(1,5))
CALL grib_get(ig1,"jDirectionIncrementInDegrees",      rval(1,6))

CALL grib_get(ig2,"numberOfPointsAlongAParallel",      ival(2,1))
CALL grib_get(ig2,"numberOfPointsAlongAMeridian",      ival(2,2))
CALL grib_get(ig2,"iScansNegatively",                  ival(2,3))
CALL grib_get(ig2,"jScansPositively",                  ival(2,4))
CALL grib_get(ig2,"jPointsAreConsecutive",             ival(2,5))
CALL grib_get(ig2,"longitudeOfFirstGridPointInDegrees",rval(2,1))
CALL grib_get(ig2,"latitudeOfFirstGridPointInDegrees", rval(2,2))
CALL grib_get(ig2,"longitudeOfLastGridPointInDegrees", rval(2,3))
CALL grib_get(ig2,"latitudeOfLastGridPointInDegrees",  rval(2,4))
CALL grib_get(ig2,"iDirectionIncrementInDegrees",      rval(2,5))
CALL grib_get(ig2,"jDirectionIncrementInDegrees",      rval(2,6))

IF (ALL(ival(1,1:5)==ival(2,1:5)) .AND. ALL(rval(1,1:6)==rval(2,1:6))) THEN
  same = .TRUE.
ELSE
  same= .FALSE.
ENDIF

END FUNCTION samegrid

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scad2val(datag,hhg,scad,datav,hhv,ier)
!
! Calcola la data di validita', a partire da data di emissione e scadenza
! scad(1:3) = time range, start step, end step
!
USE date_handler
IMPLICIT NONE

TYPE(date), INTENT(IN) :: datag
INTEGER, INTENT(IN) :: hhg,scad(3)
TYPE(date), INTENT(OUT) :: datav
INTEGER, INTENT(OUT) :: hhv,ier
INTEGER :: sca,hh_tot

!--------------------------------------------------------------------------
! gestisco time range

SELECT CASE(scad(1))
CASE(0,10)
  IF (scad(2) == 0) THEN        ! unintialised analysis
    sca = 0
  ELSE                          ! forecast
    sca = scad(2)
  ENDIF

CASE(1)                         ! initialised analysis
  sca = 0

CASE(2:5)                       ! prodotto riferito a un intervallo
  sca = scad(3)

CASE(13)                        ! analisi LAMA
  sca = 0

CASE DEFAULT                    ! time range non gestito
  ier = 1
  RETURN

END SELECT

! Calcolo data-ora di validita'
hh_tot = hhg + sca
datav = datag + (hh_tot/24)
hhv = MOD(hh_tot,24)

IF (scad(1)>=2 .AND. scad(1)<=5 .AND. scad(3)-scad(2)/=1) THEN
  ier = -1
ELSE
  ier = 0
ENDIF

RETURN
END SUBROUTINE scad2val

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_dat(values3d,zlev3d,values2d,nx,ny,nlev3d, &
  npar3d,npar2d,datav,hhv,iuout,inp_h2o,rmis,out_fmt)
!
! Scrive su iuout una scadenza di dati superficiali e 3d
!

USE date_handler
IMPLICIT NONE

! Argomenti subroutine
TYPE(date), INTENT(IN) :: datav
INTEGER, INTENT(IN) :: nx,ny,nlev3d,npar3d,npar2d,hhv,iuout,inp_h2o,out_fmt
REAL, INTENT(IN) :: values3d(nx*ny,nlev3d,npar3d),values2d(nx*ny,npar2d)
REAL, INTENT(IN) :: zlev3d(nx*ny,nlev3d),rmis

! Variabili locali
REAL :: pmsl,rain,rads,radl,t2,qq2,wd10,ws10,sstp2
REAL :: dd(nlev3d),ff(nlev3d),rh(nlev3d),mixr(nlev3d)
INTEGER :: myrb,mmob,mdayb,mhrb,msecb,myr,mmo,mday,mhr,msec,isnow
INTEGER :: i,j,k,iz
CHARACTER (LEN=80) :: chfmt3d

!--------------------------------------------------------------------------
! Data di validita'. 
! Nei dati MM5, (myrb,mmob,mdayb,mhrb,msecb) e' l'istante iniziale del 
! periodo di validita' dei dati (startStep), mentre (myr,mmo,mday,mhr,msec)
! e' l'istante finale (endStep). Calmet richiede che i due istanti coincidano.

myrb = datav%yy
mmob = datav%mm
mdayb = datav%dd
mhrb = hhv
msecb = 0
myr = datav%yy
mmo = datav%mm
mday = datav%dd
mhr = hhv
msec = 0

! Parametri attualmente non gestiti (Calmet non li usa)
pmsl = rmis
isnow = -9
rads = rmis
radl = rmis
t2 = rmis
qq2 = rmis
wd10 = rmis
ws10 = rmis

! Formato per record dati 3D
IF (inp_h2o == 0) THEN
  chfmt3d = "(i4,i6,f6.1,i4,f5.1)"
ELSE IF (inp_h2o == 1) THEN
  chfmt3d = "(i4,i6,f6.1,i4,f5.1,i3,f5.2)"
ELSE IF (inp_h2o == 2 .OR. inp_h2o == 3) THEN
  IF (out_fmt == 2) THEN
    chfmt3d = "(i4,i6,f6.1,i4,f5.1,i3,f5.2,2f6.3)"
  ELSE IF (out_fmt == 3) THEN
    chfmt3d = "(i4,i6,f6.1,i4,f5.1,i3,3f5.2)"
  ENDIF
ENDIF

! Ciclo sui punti griglia
DO j = 1,ny
DO i = 1,nx
  k = (j-1)*nx + i

! Elaboro e scrivo i parametri superficiali
! NB: lettura Calmet V5.2: sub. rdmm5, line 20175)

  rain = MIN(values2d(k,2) / 10., 99.99)
  sstp2 = values2d(k,1)

  IF (out_fmt == 2) THEN
    WRITE (iuout,'(i4.4,3i2.2, 2i3, f7.1,f5.2,i2,3f8.1,f8.2,3f8.1)') &
      myrb,mmob,mdayb,mhrb, i,j, pmsl,rain,isnow,rads,radl,t2,qq2,wd10,ws10,sstp2
  ELSE IF (out_fmt == 3) THEN
    WRITE (iuout,'(i4.4,3i2.2,i4,1x,i4.4,3i2.2,i4, i4,i3, f7.1,f5.2,i2,3f8.1,f8.2,3f8.1)') &
      myrb,mmob,mdayb,mhrb,msecb, myr,mmo,mday,mhr,msec, i,j, &
      pmsl,rain,isnow,rads,radl,t2,qq2,wd10,ws10,sstp2
  ENDIF

! Elaboro e scrivo i parametri in quota 
! NB: lettura Calmet: sub. rdmm5, line 20333 e segg.

  CALL uv2dirint(values3d(k,1:nlev3d,3),values3d(k,1:nlev3d,4),nlev3d,dd,ff)
  IF (inp_h2o >= 1) CALL calcq(values3d(k,1:nlev3d,1), &
    values3d(k,1:nlev3d,2),values3d(k,1:nlev3d,5),nlev3d,rh,mixr)
  
  DO iz = 1, nlev3d
    SELECT  CASE (inp_h2o)
    CASE(0)                        ! P,T,U,V; ioutmm5 = 81
       WRITE (iuout,chfmt3d) &
        NINT(values3d(k,iz,1)/100.), NINT(zlev3d(k,iz)),values3d(k,iz,2), &
         NINT(dd(iz)),ff(iz)

    CASE(1)                        ! P,T,U,V,Q; ioutmm5 = 82
      WRITE (iuout,chfmt3d) &
        NINT(values3d(k,iz,1)/100.), NINT(zlev3d(k,iz)),values3d(k,iz,2), &
        NINT(dd(iz)),ff(iz),NINT(rh(iz)),1000.*mixr(iz)

    CASE(2)                        ! P,T,U,V,Q,Qcr; ioutmm5 = 83
      WRITE (iuout,chfmt3d) &
        NINT(values3d(k,iz,1)/100.), NINT(zlev3d(k,iz)),values3d(k,iz,2), &
        NINT(dd(iz)),ff(iz),NINT(rh(iz)),1000.*mixr(iz), &
        1000.*values3d(k,iz,6),rmis

    CASE(3)                        ! P,T,U,V,Q,Qcr,Qis; ioutmm5 = 83
      WRITE (iuout,chfmt3d) &
        NINT(values3d(k,iz,1)/100.), NINT(zlev3d(k,iz)),values3d(k,iz,2), &
        NINT(dd(iz)),ff(iz),NINT(rh(iz)),1000.*mixr(iz), &
        1000.*(values3d(k,iz,6)+values3d(k,iz,7)),rmis

    END SELECT
  ENDDO
ENDDO
ENDDO

END SUBROUTINE write_dat

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_log(found3d,found2d,nlev3d,npar3d,npar2d,iulog)
IMPLICIT NONE

LOGICAL, INTENT(IN) :: found3d(nlev3d,npar3d),found2d(npar2d)
INTEGER, INTENT(IN) :: nlev3d,npar3d,npar2d,iulog
!
OPEN (UNIT=iulog, FILE="grib23ddat.log", STATUS="REPLACE")
WRITE (iulog,*) found3d
WRITE (iulog,*) found2d
CLOSE(iulog)

!
RETURN
END SUBROUTINE write_log

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

SUBROUTINE uv2dirint(uu,vv,n,dd,ff)

IMPLICIT NONE
REAL, PARAMETER :: rtd = 180./3.14159

INTEGER, INTENT(IN) :: n
REAL, INTENT(IN) :: uu(n),vv(n)
REAL, INTENT(OUT) :: dd(n),ff(n)

! Calcolo Modulo
ff(1:n) = SQRT(uu(1:n)*uu(1:n) + vv(1:n)*vv(1:n))

! Calcolo direzione: 0-90
WHERE (uu(:)<=0. .AND. vv(:)< 0.)
  dd(:) = rtd * ATAN(uu(:)/vv(:))
ENDWHERE

! Calcolo direzione: 90-180
WHERE (uu(:)<0. .AND. vv(:)>=0.)
  dd(:) = 90. + rtd * ATAN(-vv(:)/uu(:))
ENDWHERE

! Calcolo direzione: 180-270
WHERE (uu(:)>=0. .AND. vv(:)>0.)
  dd(:) = 180. + rtd * ATAN(uu(:)/vv(:))
ENDWHERE

! Calcolo direzione: 270-360
WHERE (uu(:)>0. .AND. vv(:)<=0.)
  dd(:) = 270. + rtd * ATAN(-vv(:)/uu(:))
ENDWHERE

WHERE (uu(:)==0. .AND. vv(:)==0.)
  dd(:) = 0.
ENDWHERE

RETURN
END SUBROUTINE uv2dirint

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE calcq(pp,tt,qq,n,rh,mixr)

IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(IN) :: pp(n),tt(n),qq(n)
REAL, INTENT(OUT) :: rh(n),mixr(n)
REAL :: sat(n)

! Saturation vapour pressure
WHERE (tt(:) > 273.16)
  sat(:) = 6.11 * EXP(17.269*(tt(:)-273.16)/(tt(:)-35.86))
ELSEWHERE
  sat(:) = 6.11 * EXP(21.874*(tt(:)-273.16)/(tt(:)-7.66))
ENDWHERE

! Relative humidity
rh(:) = qq(:) * (pp(:)-0.378*sat(:))/(0.622*sat(:))
WHERE (rh(:) < 0.)
  rh(:) = 0.
ENDWHERE
WHERE (rh(:) > 100.)
  rh(:) = 100.
ENDWHERE

! Mixing ratio
mixr(:) = qq(:) / (1.-qq(:))

RETURN
END SUBROUTINE calcq

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! SUBROUTINES ORIGINALI DA TERMOLIB (non usate)
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

! !-------------------------------------------------------------------------
! function dir(u,v)
!   IMPLICIT NONE
!   REAL, INTENT(IN) :: u,v
!   REAL dir
!   REAL, PARAMETER :: rtd = 180./3.141592654
! 
!   IF      (u <= 0. .AND. v < 0.) THEN    ! 0 - 90
!      dir = rtd*atan( u/v )
!   ELSE IF (u < 0. .AND. v >= 0.) THEN    ! 90 - 180
!      dir = 90. + rtd*atan( -v/u )
!   ELSE IF (u >= 0. .AND. v > 0.) THEN    ! 180 - 270
!      dir = 180. + rtd*atan( u/v )
!   ELSE IF (u > 0. .AND. v <= 0.) THEN    ! 270 - 360
!      dir = 270. + rtd*atan( -v/u )
!   ELSE IF (u == 0. .AND. v == 0.) THEN
!      dir = 0.
!   ENDIF
! 
!   RETURN
! end function dir
! 
! !-------------------------------------------------------------------------
! SUBROUTINE relhumtoq(rh,p,t,q)
! !
! ! calcola l'umidita specifica a partire dalla umidita relativa
! !
! 
! q = rh*(0.622*sat(t))/(p-0.378*sat(t))
! IF (q.lt.0.) q=0.
! 
! RETURN
! END 
! 
! !-------------------------------------------------------------------------
! SUBROUTINE qtorelhum(q,p,t,rh)
! ! CALCOLA L'UMIDITA RELATIVA A PARTIRE DALLA UMIDITA SPECIFICA
! ! REF.: BAKER, MON.WEA.REV.,1983,111,PAG.328 E SEG.
! 
! rh = q*(p-0.378*sat(t))/(0.622*sat(t))
! IF (RH.LT.0.) RH=0.
! 
! RETURN
! END 
! 
! !-------------------------------------------------------------------------
! FUNCTION sat(t)
! !
! ! CALCOLA LA TENSIONE DI VAPORE SATURO A PARTIRE DALLA TEMPERATURA
! ! REF.: BAKER, MON.WEA.REV.,1983,111,PAG.328 E SEG.
! ! T in Kelvin, SAT(T) in mb (?)
! 
! IF(t.gt.273.16)THEN
!   a=17.269
!   b=35.86
! ELSE
!   a=21.874
!   b=7.66
! ENDIF
! 
! sat=6.11*EXP(a*(t-273.16)/(t-b))
! 
! RETURN
! END 
! !-------------------------------------------------------------------------

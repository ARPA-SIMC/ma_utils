PROGRAM chimerencdfmet2grib
!--------------------------------------------------------------------------
! Legge un file di netcdf di Chimere e lo scrive in formato GRIB
! Uso: chimerencdf2grib.exe filein fileout filetmpl fileinfo
!
! Programma derivato da una radicale semplificazione di chimerencdf2gribex.f90.
! Testato solo per convertire i campi di turbolenza prodotti da diagmet.
!
!                               Versione 4.0.0, Michele & Enrico 06/02/2025
!--------------------------------------------------------------------------
USE grib_api
USE missing_values
USE calendar 
USE netcdf
IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: maxvar = 200     ! n.ro max var. in output Chimere
INTEGER, PARAMETER :: maxlev = 100     ! n.ro max livelli nel file di input .nc

! Length of a date buffer in CHIMERE
INTEGER, PARAMETER :: dlen=19

TYPE :: varmeta
  REAL :: min
  REAL :: max
  INTEGER :: id3
  INTEGER :: ndims
  LOGICAL :: is3d
  LOGICAL :: isreal
  CHARACTER(LEN=16) :: varname
END TYPE varmeta

TYPE(varmeta), ALLOCATABLE, DIMENSION(:) :: varids1

! NetCdf
REAL, ALLOCATABLE :: buf3d(:,:,:),buf2d(:,:)
INTEGER :: mm5date2numeric
INTEGER :: ncid,ncstat,ncstat1,ncstat2,ncstat3,dimid1,dimid2
INTEGER :: times1,times1_varid,dstrdimid,nvarin,nvarout,dstrlen,idata,idata_ini
INTEGER :: nzonal,nmerid,nlev
INTEGER :: ivar,ivarid,vartype1
CHARACTER(LEN=dlen) :: datebuf

! GRIB
REAL, ALLOCATABLE :: values_out(:,:,:),values2(:,:),values1(:)
INTEGER :: ift,igt,ifo,igo,iret,en
INTEGER :: ni,nj,np,yy,mm,dd,hh,yoc,cortod,levt,lev1,lev2,p1
INTEGER :: toffs,sfoffs,svoffs
CHARACTER(LEN=40) :: gt

! Altre variabili del programma
INTEGER :: lev_out(maxlev),code_var(maxvar),tab_var(maxvar)
INTEGER :: nhead,ios,cem,scad_ini,nbit
INTEGER :: k,kscad,klev,ntrov,ntrovs,nscri,nscris,nl,cnt_grb,kp,idp
CHARACTER (LEN=250) :: chrec
CHARACTER (LEN=120) :: filein,fileout,filetmpl,fileinfo,chdum,arg(4)
CHARACTER(LEN=20):: namevar(maxvar)

!==========================================================================
! 1) Elaborazioni preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

idp = 0

DO kp = 1,HUGE(kp)
   CALL getarg(kp,chdum)
   IF (TRIM(chdum) == "-h") THEN
     CALL write_help
     STOP 1
   ELSE IF (TRIM(chdum) == "") THEN  
     EXIT
   ELSE IF (idp >= 4) THEN
     CALL write_help
     STOP 1
   ELSE   
      idp = idp + 1
      arg(idp) = chdum
   ENDIF
ENDDO

filein = arg(1)
fileout = arg(2)
filetmpl = arg(3)
fileinfo = arg(4)

IF (filein == "" .OR. fileout == "" .OR. filetmpl == "" .OR. &
    fileinfo == "" .OR. TRIM(filein) == "-h") THEN
   CALL write_help
   STOP 1
ENDIF

!--------------------------------------------------------------------------
! 1.2 Leggo dal file NetCDF le dimensioni della matrice dei dati (x,y,z,t,var)

! Apro il file
ncstat=nf90_open(filein,NF90_NOWRITE,ncid)

! Numero istanti
ncstat1 = nf90_inq_dimid(ncid,'Time',dimid1)
ncstat2 = nf90_inq_dimid(ncid,'time',dimid2)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=times1)
ELSE IF (ncstat2 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid2,len=times1)
ELSE
  WRITE (*,*) "N.ro istanti temporali non presente nel file NetCDF"
ENDIF

! Valore istanti
ncstat1=nf90_inq_varid(ncid,'Times',times1_varid)
ncstat2=nf90_inq_dimid(ncid,'DateStrLen',dstrdimid)
ncstat3=nf90_inquire_dimension(ncid,dstrdimid,len=dstrlen)
IF (ncstat1/=NF90_NOERR .OR. ncstat2/=NF90_NOERR .OR. &
    ncstat3/=NF90_NOERR) GOTO 9989

! Nx
ncstat1 = nf90_inq_dimid(ncid,'west_east',dimid1)
ncstat2 = nf90_inq_dimid(ncid,'longitude',dimid2)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=nzonal)
ELSE IF (ncstat2 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid2,len=nzonal)
ELSE
  WRITE (*,*) "N.ro celle in direzione X non presente nel file NetCDF"
ENDIF

! Ny
ncstat1 = nf90_inq_dimid(ncid,'south_north',dimid1)
ncstat2 = nf90_inq_dimid(ncid,'latitude',dimid2)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=nmerid)
ELSE IF (ncstat2 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid2,len=nmerid)
ELSE
  WRITE (*,*) "N.ro celle in direzione Y non presente nel file NetCDF"
ENDIF

! Nlev
ncstat1 = nf90_inq_dimid(ncid,'bottom_top',dimid1)
ncstat2 = nf90_inq_dimid(ncid,'level',dimid2)
IF (ncstat1 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid1,len=nlev)
ELSE IF (ncstat2 == NF90_NOERR) THEN
  ncstat=nf90_inquire_dimension(ncid,dimid2,len=nlev)
ELSE
  WRITE (*,*) "N.ro livelli non presente nel file NetCDF, assumo un solo livello"
  nlev = 1
ENDIF

! Nvar
ncstat=nf90_inquire(ncid, nVariables=nvarin)

!--------------------------------------------------------------------------
! 1.3 Apro il template grib e verifico che la griglia sia compatibile

CALL grib_open_file(ift,filetmpl,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_new_from_file(ift,igt,iret)

CALL grib_get(igt,"editionNumber",en)
IF (en == 1) THEN
  CALL grib_get(igt,"Ni",ni)
  CALL grib_get(igt,"Nj",nj)
  np = ni*nj
ELSE IF (en == 2) THEN
  CALL grib_get(igt,"gridType",gt)
  IF (gt == "regular_ll" .OR. gt == "rotated_ll") THEN
    CALL grib_get(igt,"numberOfPointsAlongAParallel",ni)
    CALL grib_get(igt,"numberOfPointsAlongAMeridian",nj)
    np = ni*nj
  ELSE IF (gt == "unstructured_grid") THEN
    CALL grib_get(igt,"numberOfDataPoints",np)
    ni = imiss
    nj = imiss
  ELSE
    CALL grib_get(igt,"Ni",ni)
    CALL grib_get(igt,"Nj",nj)
    np = ni*nj
  ENDIF
ENDIF

IF (ni /= nzonal .OR. nj /= nmerid) GOTO 9991

!--------------------------------------------------------------------------
! 1.4 leggo da fileinfo livelli e variabili richiesti in output

OPEN (UNIT=30, FILE=fileinfo, STATUS="OLD", ERR=9999)

nhead = 5
k=0
DO
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT

! Salto righe vuote e di commento
  chrec = ADJUSTL(chrec)
  IF (chrec(1:1) == "!" .OR. TRIM(chrec) == "") CYCLE
  k = k+1

! Interpreto la riga letta: formato V2013 (CHIMERE_NCINFO.DAT)
  IF (k==1) THEN
     CONTINUE
  ELSE IF (k==2) THEN
     READ (chrec,*,IOSTAT=ios) cem
  ELSE IF (k==3) THEN
     READ (chrec,*,IOSTAT=ios) nbit
  ELSE IF (k==4) THEN
     READ (chrec,*,IOSTAT=ios) scad_ini
  ELSE IF (k==5) THEN
     READ (chrec,*,IOSTAT=ios) lev_out(1:nlev)
  ELSE IF (k>=nhead+1 .AND. k<=maxvar+nhead) THEN
     READ (chrec,'(i3,1x,i3,1x,a)',IOSTAT=ios) &
       tab_var(k-nhead),code_var(k-nhead),namevar(k-nhead)
     namevar(k-nhead) = ADJUSTL(TRIM(namevar(k-nhead)))
  ELSE
     WRITE (*,*) "Troppe specie, elaboro le prime ",maxvar,&
          " (aumentare param. maxvar)"
     k = k-1
     EXIT
  ENDIF
  IF (ios /= 0) GOTO 9998

ENDDO
CLOSE(30)

nvarout = k - nhead

!--------------------------------------------------------------------------
! 1.5 Log a schermo dei dati richiesti

WRITE (*,*) "N step temporali: ",times1
WRITE (*,*) "Parametri griglia:"
WRITE (*,*) "  nx,ny,nz     : ",ni,nj,nlev
WRITE (*,*) ""
WRITE (*,*) "Numero di parametri:"
WRITE (*,*) "variabili totali in input:  ",nvarin
WRITE (*,*) "variabili richieste in out: ",nvarout
WRITE (*,*) "livelli in outuput:         ",COUNT(lev_out(1:nlev) /= 0)
WRITE (*,*) ""
WRITE (*,*) "Parametri scrittura grib:"
WRITE (*,*) "scadenza iniziale: ",scad_ini
WRITE (*,*) "numero bit:        ",nbit

!--------------------------------------------------------------------------
! 1.7 Altre operazioni

! Alloco le varaibili
ALLOCATE (values_out(np,nlev,nvarout),values2(np,nlev),values1(np))
ALLOCATE (buf2d(ni,nj),buf3d(ni,nj,nlev))
ALLOCATE (varids1(nvarin))

!==========================================================================
! 2) Lettura e scrittura

WRITE (*,*)
WRITE (*,*) "Inizio elaborazioni"
CALL grib_open_file(ifo,fileout,"w",iret)

cnt_grb = 0
ntrov = 0
ntrovs = 0
nscri = 0
nscris = 0

ist: DO kscad = 1, times1
  ncstat = nf90_get_var(ncid,times1_varid,datebuf,(/1,kscad/),(/dstrlen, 1/))
  idata = mm5date2numeric(datebuf)

  IF (kscad == 1) idata_ini = idata
  WRITE (*,*) "Elaboro scadenza ",idata

  CALL chdata2ksec(idata,idata_ini,scad_ini,.FALSE.,yoc,mm,dd,hh,p1,cortod)
  yy = (cortod-1)*100 + yoc
  
  var: DO ivar=1,nvarout
    ncstat=nf90_inq_varid(ncid,namevar(ivar),ivarid)
    IF (ncstat /= NF90_NOERR) THEN
      IF (kscad ==1) WRITE(*,'(2a,1x,4i6)') "Variabile non trovata     ", &
        namevar(ivar),ivar,ivarid,tab_var(ivar),code_var(ivar)
      CYCLE var
    ENDIF

   ncstat=nf90_inquire_variable(ncid,ivarid,varids1(ivarid)%varname, &
     vartype1,varids1(ivarid)%ndims)

   IF (varids1(ivarid)%ndims==3) THEN           ! 2D field
     IF (kscad == 1) ntrovs = ntrovs + 1
     ncstat = nf90_get_var(ncid,ivarid,buf2d,(/1,1,kscad/), &   ! start vector
       (/nzonal,nmerid,1/))                                      ! count vector
      nl = 1
      values1 = RESHAPE(buf2d,(/nzonal*nmerid/))
      values_out(:,1,ivar) = values1(:)

   ELSE IF (varids1(ivarid)%ndims==4) THEN      ! 3D field
     IF (kscad == 1) ntrov = ntrov + 1
     ncstat = nf90_get_var(ncid,ivarid,buf3d,(/1,1,1,kscad/), & ! start vector
       (/nzonal,nmerid,nlev,1/))                                 ! count vector
      values2 = RESHAPE(buf3d,(/nzonal*nmerid,nlev/))
      values_out(:,:,ivar) = values2(:,:)
      nl = nlev

   ENDIF

   IF (code_var(ivar) <= 0) CYCLE
   IF (kscad ==1) THEN
     IF (nl == 1) THEN
       WRITE (*,'(3a,1x,2i6)') "Trovata la variabile (2D) ", &
         namevar(ivar)," (tab, var):",tab_var(ivar),code_var(ivar)
       nscris = nscris + 1
     ELSE
       WRITE (*,'(3a,1x,2i6)') "Trovata la variabile (3D) ", &
         namevar(ivar)," (tab, var):",tab_var(ivar),code_var(ivar)
       nscri = nscri + 1
     ENDIF
   ENDIF

   lev: DO klev = 1,nl
     IF (lev_out(klev) == 0) CYCLE

!     Codifica livelli - grib1
      IF (en == 1) THEN
        IF (varids1(ivarid)%ndims==4) then
          levt = 109
          lev1 = klev
        ELSE IF (namevar(ivar) == "tem2" .OR. namevar(ivar) == "sreh" ) THEN
          levt = 105
          lev1 = 2
        ELSE IF (namevar(ivar) == "w10m" .OR. namevar(ivar) == "w10s" .OR. &
                 namevar(ivar) == "u10m" .OR. namevar(ivar) == "v10m") THEN
          levt = 105
          lev1 = 10
        ELSE
          levt = 1
          lev1 = 0
        ENDIF

!     Codifica livelli - grib2
      ELSE IF (en == 2) THEN
        IF (varids1(ivarid)%ndims==4) then
          toffs = 150
          sfoffs = 0
          svoffs = klev
        ELSE IF (namevar(ivar) == "tem2" .OR. namevar(ivar) == "sreh" ) THEN
          toffs = 103
          sfoffs = 3
          svoffs = 2000
        ELSE IF (namevar(ivar) == "w10m" .OR. namevar(ivar) == "w10s" .OR. &
                 namevar(ivar) == "u10m" .OR. namevar(ivar) == "v10m") THEN
          toffs = 103
          sfoffs = 3
          svoffs = 10000
        ELSE
          toffs = 1
          sfoffs = 0
          svoffs = 0
        ENDIF

      ENDIF
     
!     Correggo unita' di misura (da Chimere a GRIB). Verificate Tp!!
      IF (namevar(ivar) == "alb") THEN
        values_out(:,1,ivar) = values_out(:,1,ivar)/100.
      ENDIF

!     Scrivo GRIB
      CALL grib_clone(igt,igo)

!     Reftime
      CALL grib_set(igo,"month",mm)
      CALL grib_set(igo,"day",dd)
      CALL grib_set(igo,"hour",hh)

      IF (en == 1) THEN           ! anno, var, lev, trange
        CALL grib_set(igo,"yearOfCentury",yoc)
        CALL grib_set(igo,"centuryOfReferenceTimeOfData",cortod)

        CALL grib_set(igo,"table2Version",tab_var(ivar))
        CALL grib_set(igo,"indicatorOfParameter",code_var(ivar))

        CALL grib_set(igo,"indicatorOfTypeOfLevel",levt)
        CALL grib_set(igo,"level",lev1)

        CALL grib_set(igo,"unitOfTimeRange",1)
        CALL grib_set(igo,"P1",p1)
        CALL grib_set(igo,"P2",0)
        CALL grib_set(igo,"timeRangeIndicator",0)
           
     ELSE IF (en == 2) THEN       ! anno, var, lev, trange
        CALL grib_set(igo,"year",yy)

        CALL grib_set(igo,"discipline",0)
        CALL grib_set(igo,"parameterCategory",tab_var(ivar))
        CALL grib_set(igo,"parameterNumber",code_var(ivar))

        CALL grib_set(igo,"typeOfFirstFixedSurface",toffs)
        CALL grib_set(igo,"scaleFactorOfFirstFixedSurface",sfoffs)
        CALL grib_set(igo,"scaledValueOfFirstFixedSurface",svoffs)

        CALL grib_set(igo,"productDefinitionTemplateNumber",1)
        CALL grib_set(igo,"indicatorOfUnitOfTimeRange",1)
        CALL grib_set(igo,"forecastTime",p1)

     ENDIF

      CALL grib_set(igo,"values",values_out(1:np,klev,ivar))
      CALL grib_write(igo,ifo)
      CALL grib_release(igo)

      cnt_grb = cnt_grb + 1
    ENDDO lev              ! livelli
  ENDDO var                ! parametri
ENDDO ist                  ! istanti

ncstat=nf90_close(ncid)
WRITE (*,*) "Variabili (3D): input ",ntrov," output ",nscri
WRITE (*,*) "Variabili (2D): input ",ntrovs," output ",nscris
WRITE (*,*) "Scadenze elaborate ",kscad-1," grib scritti ",cnt_grb

STOP 0

!==========================================================================
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filetmpl)
STOP 1

9998 CONTINUE
WRITE (*,*) "Record illegale o mal posizionato in ",TRIM(fileinfo)
WRITE (*,*) "Numero record (esclusi commenti) ",k
WRITE (*,*) TRIM(chrec)
STOP 2

9991 CONTINUE
WRITE (*,*) "Dati inconsistenti in ",TRIM(filetmpl)," e ",TRIM(filein)
IF (ni /= nzonal) WRITE (*,*) "Nx: ",ni,nzonal
IF (nj /= nmerid) WRITE (*,*) "Nx: ",nj,nmerid
STOP 4

9989 CONTINUE
WRITE (*,*) "Riferimenti temporali non trovati in ",TRIM(filein)
STOP 6

END PROGRAM chimerencdfmet2grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE chdata2ksec(idata,idata_ini,scad_ini,verbose, &
  ksec10,ksec11,ksec12,ksec13,ksec16,ksec21)
!--------------------------------------------------------------------------
! Data una data nel formato intero di chimere (YYYYMMDDHH), ritorna gli 
! elementi relativi a data e scadenza nella sezione 1 del grib (edition 1)
!--------------------------------------------------------------------------
USE date_handler

IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: idata       ! data/ora letta dal record corrente
INTEGER, INTENT(IN) :: idata_ini   ! data/ora del 1o record del file
INTEGER, INTENT(IN) :: scad_ini
LOGICAL, INTENT(IN) :: verbose
INTEGER, INTENT(OUT) :: ksec10,ksec11,ksec12,ksec13,ksec16,ksec21

! Variabli locali
TYPE(date) :: datac,data_ini,data_emi
INTEGER :: hhc,hh_ini,hh_emi,scadc,bck_day
CHARACTER (LEN=10) :: chdata

!--------------------------------------------------------------------------

! Leggo data/ora corrente
WRITE (chdata,'(i10.10)') idata
READ (chdata,'(i4,3i2)',ERR=9999) datac%yy,datac%mm,datac%dd,hhc

IF (verbose) WRITE (*,'(a,i5,3i3)') "Elaboro istante: ", &
  datac%yy,datac%mm,datac%dd,hhc

! Richiedo tutte analisi
IF (scad_ini < 0) THEN

  ksec10 = 1 + MOD(datac%yy-1,100)
  ksec21 = 1 + (datac%yy-1)/100
  ksec11 = datac%mm
  ksec12 = datac%dd
  ksec13 = hhc
  ksec16 = 0

! Richiedo previsioni
ELSE

! Calcolo la data di emissione
  WRITE (chdata,'(i10.10)') idata_ini
  READ (chdata,'(i4,3i2)',ERR=9999) &
    data_ini%yy,data_ini%mm,data_ini%dd,hh_ini
  hh_emi = hh_ini - scad_ini
  IF (hh_emi >= 0) THEN
    data_emi = data_ini
  ELSE
    bck_day = (-1-hh_emi)/24 + 1
    data_emi = data_ini - bck_day
    hh_emi = hh_emi + bck_day * 24
  ENDIF

! Calcolo la scadenza di previsione
  scadc = hhc - hh_emi + 24 * (datac - data_emi)
  IF (verbose) WRITE (*,'(a,i5,3i3,a,i3)') "Codifico scadenza: ", &
    data_emi%yy,data_emi%mm,data_emi%dd,hh_emi," +",scadc

! Definisco la sezione1 GRIB
  ksec10 = 1 + MOD(data_emi%yy-1,100)
  ksec21 = 1 + (data_emi%yy-1)/100
  ksec11 = data_emi%mm
  ksec12 = data_emi%dd
  ksec13 = hh_emi
  ksec16 = scadc

ENDIF

RETURN

9999 CONTINUE
WRITE (*,*) "Data illegale in file input: ",idata
STOP

END SUBROUTINE chdata2ksec

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "Uso: chimerencdfmet2grib.exe filein fileout template fileinfo"
WRITE (*,*)
WRITE (*,*) "Legge un file di netcdf ""METEO.nc""di Chimere e lo scrive in formato GRIB"
WRITE (*,*) "filein:   file di input (NetCDF)"
WRITE (*,*) "fileout:  file di output (GRIB)"
WRITE (*,*) "template: template da usare per costruire i grib in output; GRIB1 o GRIB2, deve"
WRITE (*,*) "          essere un campo superficiale istantaneo, usato per dfinire tutte le"
WRITE (*,*) "          chiavi grib tranne il parametro"
WRITE (*,*) "fileinfo: namelist (formato CHIMERE_INFO.DAT o CHIMERE_NCINFO.DAT)"
!            12345678901234567890123456789012345678901234567890123456789012345678901234567890

RETURN

END SUBROUTINE write_help

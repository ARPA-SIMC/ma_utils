PROGRAM grb_chimere2netcdf
!--------------------------------------------------------------------------
! Legge un file grib prodotto da Chimere e lo riscrive in formato NetCDF
!   Uso: chimere_grib2netcdf.exe [-h] filein fileout [-ctn]
!
! NOTE:
! Tutti i grib devono essere definiti sulla stessa area
!
! Gestiti soli i livelli del tipo 109,N,0 (il NetCDF conterra' in ogni caso
!   tutti i livelli da 1 al livello piu' alto presente nel file)
!   
! I grib devono riferirsi tutti a:
!   - analisi media oraria (1,0,0,0)
!   - analisi media giornaliera (scritto come istanteneo ore 12)
!   - previsioni a scadenza oraria provenienti dallo stesso run (1,N,0,0)
!     (previsioni relative a run diversi richiederebbero un NetCDF a 5 dim)
!
! Sviluppi:
!   - nome e unita' di misura letta da tabelle (sez. 2.2)
!
!                                         Versione 1.3.1, Enrico 10/01/2014
!--------------------------------------------------------------------------

USE netcdf
USE date_handler
USE seriet_utilities
IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: maxdim = 100000  ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxvar = 100     ! n.ro max di parametri 

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1_first(1024),ksec2_first(1024)
INTEGER :: kbuffer(maxdim),kword,klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Dichiarazioni per NetCDF
INCLUDE "netcdf.inc"
INTEGER :: dimid_x,dimid_y,dimid_z,dimid_time
INTEGER :: varid(maxvar),varid_x,varid_y,varid_z,varid_time
CHARACTER (LEN=10) :: unit(maxvar)

! Altre variabili del programma
TYPE(date) :: data1,data2,datac
REAL (kind=8) :: t1
REAL (kind=8), ALLOCATABLE :: arr_1d_dble(:)
REAL, ALLOCATABLE :: arr_2d(:,:),arr_1d(:)
REAL :: dx,dy,vmin,vmax
INTEGER :: nx,ny,nz,nt,nscad_forc,nvar
INTEGER :: var_grb(3,maxvar),hh1,hh2,hhc,ndec,cp2
INTEGER :: iu,ncid,idp,k,kp,kv,kg,kl,ist,ier
CHARACTER (LEN=80) :: filein,fileout,chdum
CHARACTER (LEN=40) :: var_name(maxvar),strcdf_dt,strcdf_t0
CHARACTER (LEN=4) :: scad_type
CHARACTER (LEN=3) :: proj,conventions
LOGICAL :: lforc,ldaily,lini,l3d,ksec2_diff

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando
idp = 1
conventions="def"
filein = ""
fileout = ""

DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "") THEN  
    EXIT
  ELSE IF (TRIM(chdum) == "-ctn") THEN  
    conventions="ctn"
  ELSE IF (idp == 1) THEN
    filein = chdum
    idp = idp + 1
  ELSE IF (idp == 2) THEN
    fileout = chdum
    idp = idp + 1
  ENDIF
ENDDO

IF (filein == "" .OR. fileout == "") THEN
  CALL write_help
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.2 Altre operazioni

! Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

!--------------------------------------------------------------------------
! 2) Scorro una prima volta il GRIB per trovare: dimensioni griglia, numero
!    di livelli, elenco variabili, tipo di scadenze (analisi o previsioni)

! 2.0) Inizializzazioni
nz = 0
nvar = 0
data1 = date(1,1,9999)
hh1 = 25
data2 = date(1,1,0)
hh2 = -1
nscad_forc = -1
lforc = .FALSE. ! .T. se ci sono scadenze di previsione
ldaily = .TRUE. ! .T. se tutti i dati hanno ora 12 (i.e. medie giornaliere)
lini = .TRUE.   ! .T. se i dati hanno la stessa data di inizializzazione

CALL PBOPEN (iu,filein,'R',kret)
DO kg = 1,HUGE(0)

  CALL PBGRIB(iu,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    GOTO 9996
  ENDIF

  CALL GRIBEX(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
              field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.1) Gestione griglia: controllo che non cambi, trovo dim. e proiez.
  IF (kg == 1) THEN
    ksec1_first(:) = ksec1(:)
    ksec2_first(:) = ksec2(:)
    IF (ksec2(11) /= 64) GOTO 9994

    IF (ksec2(1) == 0) THEN
      IF (MAXVAL( ksec2((/4,5,7,8/)) ) > 360000) THEN
        proj = "utm"
      ELSE
        proj = "geo"
      ENDIF
    ELSE IF (ksec2(1) == 10) THEN
      proj = "rot"
    ELSE
      GOTO 9993
    ENDIF
    nx = ksec2(2)
    ny = ksec2(3)
    dx = REAL(ksec2(8) - ksec2(5)) / (1000. * REAL(nx-1))
    dy = REAL(ksec2(7) - ksec2(4)) / (1000. * REAL(ny-1))

  ELSE
    IF (ksec2_diff(ksec2(1:14),ksec2_first(1:14))) GOTO 9995

  ENDIF

! 2.2) Memorizzo/controllo variabili (chck bounds: maxvar)
  DO kv = 1,nvar
    IF (ALL(ksec1((/2,1,6/)) == var_grb(1:3,kv))) EXIT
  ENDDO
  IF (kv > nvar) THEN
    nvar = kv
    var_grb(1:3,nvar) = ksec1((/2,1,6/))

!   Nome
    CALL var2spec(var_grb(1:3,kv),ndec,vmin,vmax,var_name(kv),cp2,ier)

!   Unita' di misura
    IF (ksec1(1)==195 .OR. &
        (ksec1(1)==200 .AND. (ksec1(6)==220 .OR. ksec1(6)==221 .OR. ksec1(6)>=225))&
        ) THEN
      unit(kv) = "ug/m3"
    ELSE IF (ksec1(1)==199 .OR. ksec1(1)==196 .OR. &
        (ksec1(1)==200 .AND. ksec1(6)>=151 .AND. ksec1(6)<=221)) THEN
      unit(kv) = "ppb"
    ELSE
      unit(kv) = "unknown"
    ENDIF
!   unit(kv) = "ug/m3"

  ENDIF

! 2.3) Cerco il livello piu' alto presente nel file
  IF (ksec1(7) /= 109 .OR. ksec1(9) /= 0) GOTO 9989
  nz = MAX(nz,ksec1(8))
  
! 2.4) Trovo istante iniziale, finale, step, tipo scadenze
  CALL ksec1_valid(ksec1,datac,hhc,ier)
  IF (ier /= 0) GOTO 9992
  IF (datac < data1 .OR. (datac == data1 .AND. hhc < hh1)) THEN
    data1 = datac
    hh1 = hhc
  ENDIF
  IF (datac > data2 .OR. (datac == data2 .AND. hhc > hh2)) THEN
    data2 = datac
    hh2 = hhc
  ENDIF
  IF (ANY(ksec1((/15,17,18/)) /= (/1,0,0/))) GOTO 9991
  IF (ksec1(16) /= 0) THEN
    lforc = .TRUE.
    nscad_forc = MAX(nscad_forc,ksec1(16))
  ENDIF
  IF (ksec1(13) /= 12) ldaily = .FALSE.
  IF (ANY(ksec1(10:14) /= ksec1_first(10:14))) lini = .FALSE.
  
ENDDO
CALL PBCLOSE (iu,kret)

! 2.5) Verifico se date e scadenze sono gestibili e calcolo n.ro istanti
IF (.NOT. lforc) THEN
  IF (ldaily) THEN
    scad_type = "dyan"
    strcdf_dt = "0000-00-01 00:00:00.00 +00:00"
    strcdf_t0 = "days since 1-1-1 00:00:0.0"
    nt = data2-data1 + 1
  ELSE
    scad_type = "hran"
    strcdf_dt = "0000-00-00 01:00:00.00 +00:00"
    strcdf_t0 = "hours since 1-1-1 00:00:0.0"
    nt = 24 * (data2-data1) + hh2 - hh1 + 1
  ENDIF
ELSE
  IF (lini) THEN
    scad_type = "forc"
    strcdf_dt = "0000-00-00 01:00:00.00 +00:00"
    strcdf_t0 = "hours since 1-1-1 00:00:0.0"
    nt = nscad_forc + 1
  ELSE
    GOTO 9990
  ENDIF
ENDIF

! 2.6) Scelgo NetCDF 2d o 3d
IF (nz == 1) THEN
  l3d = .FALSE.
ELSE
  l3d = .TRUE.
ENDIF

! 2.7) Visualizzo a schermo il contenuto del GRIB
WRITE (*,'(2a)') "Contenuto file grib ",TRIM(filein)
WRITE (*,'(a,2i4,2x,f9.3,1x,f9.3,2x,a3)') "Griglia (nx,ny,dx,dy,proj): ", &
  nx,ny,dx,dy,proj
WRITE (*,'(a,i5,3i3)') "Primo istante : ",data1%yy,data1%mm,data1%dd,hh1
WRITE (*,'(a,i5,3i3)') "Ultimo istante: ",data2%yy,data2%mm,data2%dd,hh2
WRITE (*,'(3a,i5)') "Tipo scadenze: ",scad_type,"; numero istanti ",nt
IF (l3d) THEN
  WRITE (*,'(a,i2)') "Livelli: ",nz," campi 3d"
ELSE
  WRITE (*,'(a,i2)') "Livelli: ",nz," campi 2d"
ENDIF
WRITE (*,'(a,i3)') "Variabili: ",nvar
DO kv=1,nvar
  WRITE (*,'(2x,3i4)') var_grb(1:3,kv)
ENDDO
WRITE (*,*)

!--------------------------------------------------------------------------
! 3) Apro e imposto il file NetCDF

! Apro il file
ier = NF90_CREATE(fileout,0,ncid)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)

! Assegno attributi globali
ier = NF90_PUT_ATT(ncid,NF90_GLOBAL,"Conventions","COARDS")
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)

! Definisco le dimensioni
ier = NF90_DEF_DIM(ncid,"x",nx,dimid_x)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
ier = NF90_DEF_DIM(ncid,"y",ny,dimid_y)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
IF (l3d) THEN
  ier = NF90_DEF_DIM(ncid,"z",nz,dimid_z)
  IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
ENDIF
ier = NF90_DEF_DIM(ncid,"time",NF90_UNLIMITED,dimid_time)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)

! Definisco le variabili statiche (x,y,z,time)
ier = NF90_DEF_VAR(ncid,"x",NF90_FLOAT,dimid_x,varid_x)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
ier = NF90_DEF_VAR(ncid,"y",NF90_FLOAT,dimid_y,varid_y)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
IF (l3d) THEN
  ier = NF90_DEF_VAR(ncid,"z",NF90_FLOAT,dimid_z,varid_z)
  IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
ENDIF
ier = NF90_DEF_VAR(ncid,"time",NF90_DOUBLE,dimid_time,varid_time)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)

! Assegno attributi alle variabili statiche
ier = NF90_PUT_ATT(ncid,varid_x,"units","km")
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
ier = NF90_PUT_ATT(ncid,varid_y,"units","km")
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
IF (l3d) THEN
  ier = NF90_PUT_ATT(ncid,varid_z,"units","hybrid level")
  IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
ENDIF
ier = NF90_PUT_ATT(ncid,varid_time,"units",TRIM(strcdf_t0))
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
ier = NF90_PUT_ATT(ncid,varid_time,"delta_t",TRIM(strcdf_dt))
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)

! Definisco le variabili contenute nel grib
DO kv = 1,nvar
  IF (l3d) THEN
    ier = NF90_DEF_VAR(ncid,var_name(kv),NF90_FLOAT, &
      (/dimid_x,dimid_y,dimid_z,dimid_time/),varid(kv))
  ELSE
    ier = NF90_DEF_VAR(ncid,var_name(kv),NF90_FLOAT, &
      (/dimid_x,dimid_y,dimid_time/),varid(kv))
  ENDIF
  IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
  ier = NF90_PUT_ATT(ncid,varid(kv),"units",unit(kv))
  IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
ENDDO

print *,"6"
ier = NF90_ENDDEF(ncid)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)

!--------------------------------------------------------------------------
! 4) Scrittura dati NetCDF

WRITE (*,'(a)') "Inizio scrittura NetCDF"

! 4.1) Scrivo varibili statiche (x,y,z,time)
! NB: tolgo 13 giorni per tener conto del calendario giuliano

ALLOCATE (arr_1d(nx))
DO k = 1,nx
  arr_1d(k) = REAL(ksec2_first(5))/1000. + REAL(k-1) * dx
ENDDO
ier = NF90_PUT_VAR(ncid,varid_x,arr_1d)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
DEALLOCATE (arr_1d)

ALLOCATE (arr_1d(ny))
DO k = 1,ny
  arr_1d(k) = REAL(ksec2_first(4))/1000. + REAL(k-1) * dy
ENDDO
ier = NF90_PUT_VAR(ncid,varid_y,arr_1d)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
DEALLOCATE (arr_1d)

IF (l3d) THEN
  ALLOCATE (arr_1d(nz))
  DO k = 1,nz
    arr_1d(k) = REAL(k)
  ENDDO
  ier = NF90_PUT_VAR(ncid,varid_z,arr_1d)
  IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
  DEALLOCATE (arr_1d)
ENDIF

ALLOCATE (arr_1d_dble(nt))
IF (scad_type == "dyan") THEN
  t1 = DBLE(data1 - date(1,1,1) - 13)
ELSE
  t1 = DBLE(24 * (data1 - date(1,1,1) - 13) + hh1)
ENDIF
DO k = 1,nt
  arr_1d_dble(k) = t1 + k-1
ENDDO
ier = NF90_PUT_VAR(ncid,varid_time,arr_1d_dble)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
DEALLOCATE (arr_1d_dble)

! 4.2) Scrivo le variabili contenuti nel grib (ciclo principale GRIB)
ALLOCATE (arr_2d(nx,ny))
CALL PBOPEN (iu,filein,'R',kret)
DO kg = 1,HUGE(0)

  CALL PBGRIB(iu,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    GOTO 9996
  ENDIF

  CALL GRIBEX(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
              field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! Trovo a quale variabile si riferisce il grib appena letto
  DO kv = 1,nvar
    IF (ALL(var_grb(1:3,kv) == (/ksec1(2),ksec1(1),ksec1(6)/))) EXIT
  ENDDO
  IF (kv > nvar) GOTO 9999

! Trovo a quale livello si riferisce il grib appena letto
  kl = ksec1(8)
  IF (kl > nz .OR. kl < 1) GOTO 9998
  IF (l3d .AND. kl /= 1) GOTO 9988

! Trovo a quale istante si riferisce il grib appena letto
  CALL ksec1_valid(ksec1,datac,hhc,ier)
  IF (ier /= 0) GOTO 9992
  IF (scad_type == "dyan") THEN
    ist = datac - data1 + 1
  ELSE
    ist = (datac - data1) * 24 + hhc - hh1 + 1
  ENDIF
  IF (ist < 1 .OR. ist > nt) GOTO 9997

! Travaso i dati in un array con le dimensioni giuste
  arr_2d(:,:) = RESHAPE(field(1:nx*ny),(/nx,ny/))

! Scrivo i dati
  IF (l3d) THEN
    ier = NF90_PUT_VAR(ncid,varid(kv),arr_2d,START=(/1,1,kl,ist/))
  ELSE
    ier = NF90_PUT_VAR(ncid,varid(kv),arr_2d,START=(/1,1,ist/))
  ENDIF

  IF (ier /= NF90_NOERR) THEN
    WRITE (*,'(a,5i5)') "grib ",kg
    WRITE (*,'(a,i5,a,i3,2i4,a)') " var ",varid(kv)," (",var_grb(1:3,kv),")"
    WRITE (*,'(a,i5)') " liv ",kl
    WRITE (*,'(a,i5,a,i4,3i3,a)') " ist ",ist," (",datac%yy,datac%mm, &
      datac%dd,hhc,")"
    CALL netcdf_err(ier)
  ENDIF

ENDDO

!--------------------------------------------------------------------------
! 5) Chiudo files e termino

WRITE (*,'(a,i6)') "Scrittura terminata; campi elaborati: ",kg-1
ier = NF90_CLOSE(ncid)
IF (ier /= NF90_NOERR) CALL netcdf_err(ier)
CALL PBCLOSE (iu,kret)

STOP

!--------------------------------------------------------------------------
! Gestione errori (esclusi errori libreria NetCDF)

9999 CONTINUE
WRITE (*,'(a,3i4,a,i7)') "var grib non trovata: ", &
  ksec1(2),ksec1(1),ksec1(6)," grib n.ro ",kg
STOP

9998 CONTINUE
WRITE (*,*) "Errore gestione grib (livello illegale in seconda lettura)"
STOP

9997 CONTINUE
WRITE (*,'(a,i5,a,4i5,a,i7)') "Istante fuori dai limiti in file NetCDF: ist ", &
  ist," data ",datac,hhc," grib n.ro ",kg
STOP

9996 CONTINUE
WRITE(*,*) "Error pbgrib: kret ",kret
STOP

9995 CONTINUE
WRITE(*,*) "Trovato grib con griglia diversa, grib n.ro: ",kg
STOP

9994 CONTINUE
WRITE(*,*) "Scanning mode flag non getito: ",ksec2(11)
STOP

9993 CONTINUE
WRITE(*,*) "Proiezione grib non gestita: ksec2(1) = ",ksec2(1)
STOP

9992 CONTINUE
WRITE (*,*) "Errore ksec1_valid, grib n.ro: ",kg
STOP

9991 CONTINUE
WRITE (*,'(a,4i4,a,i6)') "Errore scadenza non gestita ",ksec1(15:18), &
  " grib n.ro: ",kg
STOP

9990 CONTINUE
WRITE (*,*) "Errore, i grib contengono previsioni con data emissione diversa"
STOP

9989 CONTINUE
WRITE (*,'(a,3i4,a,i6)') "Errore livello non gestito ",ksec1(7:9), &
  " grib n.ro: ",kg
STOP

9988 CONTINUE
WRITE (*,'(a,3i4,a,i6)') "Errore livello /= 1 con output 2d ",ksec1(7:9), &
  " grib n.ro: ",kg
STOP

END PROGRAM grb_chimere2netcdf

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

SUBROUTINE netcdf_err(ier)
!
! Scrive a schermo il messaggio d'errore NetCDF e termina il programma
! 
USE netcdf
IMPLICIT NONE
INTEGER, INTENT(IN) :: ier
!
WRITE (*,'(a)') "Errore libreria NetCDF:"
WRITE (*,'(a)') NF90_STRERROR(ier)

STOP
END SUBROUTINE netcdf_err

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
WRITE (*,*) "Uso: chimere_grib2netcdf.exe filein fileout"

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION ksec2_diff(ksec2a,ksec2b) RESULT(is_diff)
!
! Controlla se due array ksec2 scritti da Gribex corrispondono alla stessa 
! griglia
!
IMPLICIT NONE
LOGICAL :: is_diff
INTEGER, INTENT(IN) :: ksec2a(14),ksec2b(14)

IF (ANY(ksec2a((/1,2,3,4,5,6,7,8,11/)) /= ksec2b((/1,2,3,4,5,6,7,8,11/)))) THEN
  is_diff = .TRUE.

ELSE IF (ksec2a(6) == 128 .AND. &
  (ksec2a(9) /= ksec2b(9) .OR. ksec2a(10) /= ksec2b(10))) THEN
  is_diff = .TRUE.

ELSE IF (ksec2a(1) == 10 .AND. &
  (ksec2a(13) /= ksec2b(13) .OR. ksec2a(14) /= ksec2b(14))) THEN
  is_diff = .TRUE.

ELSE 
  is_diff = .FALSE.

ENDIF

END FUNCTION ksec2_diff

PROGRAM temp_csv2asc
!--------------------------------------------------------------------------
! Programma che legge un file di radiosondaggi nel formato csv prodotto da
! v7d_transform e scrive ciascun sondaggio in un file in formato estra_temp
! Uso: temp_csv2asc.exe [-h] filein [-tlaunch] [-zlsm] [-interp] [-zidro]
!
! Note:
! - Programma chiamato da temp_bufr2asc.ksh, e usato dalle catene Calmet e 
!   Ginepro. 
! - Il tracciato record del file di input deve essere: data,lon,lat,
!    B01001,B01002,B07030,B10004,B12101,B12103,B11001,B11002,B10008
!   (WMOblk,WMOsta,  hsta, Press,  Temp,    Td,   Dir, Speed,  Geop) 
!   Sono ammessi header ripetuti in mezzo al file.
!
! TODO:
! - attualmente, lancindo estra_temp.ksh con opzione -split, i sondaggi con
!   geoptenziale intermante mancanti si perdono: v7d_transform elabora un
!   TEMP alla volta, e uno dei parmetri richiesti non ha dati validi, non 
!   lo scrive neanche nell'header; questo programma trova un header illegale
!   e salta il segmento. 
!   Volendo si potrebbe fare in modo che i TEMP a cui manca solo il 
!   geopotenziale siano scritti comunque...
!
! - raffinare l'interpolazione verticale dei dati mancanti: 
!   a) calcolo delle quote con idrostatica, invece di interpolare il 
!      geop dai livelli standard (sub. zidro_rsd)
!   b) interpolazione "circolare" della direzione del vento (sub. interp_rsd)
! - verificare che succede se mancano in input dei dati di anagrafica
! - opzioni per output: RH invece di Td, componenti del vento
! - gestione di record dati non ordinati per livello discendente (mettere 
!   almeno un controllo)
!
!                                         Versione 1.2.0, Enrico 12/09/2013
!--------------------------------------------------------------------------

USE file_utilities
USE datetime_class
USE missing_values

IMPLICIT NONE

! Parametri e variabili relative al tracciato del file .csv in input
REAL, PARAMETER :: eps = 1.e-5        ! tolleranza per uguaglianza tra reali
INTEGER, PARAMETER :: nf_req = 12     ! n.ro di campi nel file csv
INTEGER, PARAMETER :: max_lev = 200   ! n.ro max di livelli di un sondaggio
CHARACTER (LEN=86), PARAMETER :: head2_req = & ! header del file .csv
  "Date,Longitude,Latitude,B01001,B01002,B07030,B10004,B12101,B12103,B11001,B11002,B10008"
REAL :: val(6),val_rsd(6,max_lev)     ! Valori misurati (pp,tt,td,dd,ff,gz)

CHARACTER(LEN=19) :: ch_isodate
REAL :: lat,lon,quo
INTEGER :: gts_blo,gts_sta

! Altre variabili
TYPE (datetime) :: data,data_rsd
TYPE (csv_record) :: csvline
REAL :: lat_rsd,lon_rsd,quo_rsd
INTEGER :: gts_blo_rsd,gts_sta_rsd,nlev_rsd
INTEGER :: idp,kp,k,k2,eof,eor,ios,nf,ier(nf_req)
INTEGER :: cnt_head1,cnt_head2,cnt_dat,cnt_rsd
CHARACTER (LEN=250) :: filein,chdum,chrec
CHARACTER (LEN=3) :: out_time,out_quo
LOGICAL :: linterp,lzidro

!==========================================================================
! 1) Preliminari

! 1.1 Parametri da riga comando
out_time="syn"
out_quo="sup"
linterp = .FALSE.
lzidro = .FALSE.

idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-tlaunch") THEN
    out_time="lau"
  ELSE IF (TRIM(chdum) == "-zslm") THEN
    out_quo="slm"
  ELSE IF (TRIM(chdum) == "-interp") THEN
    linterp = .TRUE.
  ELSE IF (TRIM(chdum) == "-zidro") THEN
    lzidro = .TRUE.
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

! 1.2 Apro il file di input
OPEN (UNIT=20, FILE=filein, STATUS="OLD", FORM="FORMATTED", ERR=9999)

! 1.3 Trovo codice EOF
CALL get_eof_eor(eof,eor)

!==========================================================================
! 2) Elaborazioni (ciclo sui dati in input)

cnt_head1 = 0
cnt_head2 = 0
cnt_dat = 0
cnt_rsd = 0
nlev_rsd = 0

k = 0
main: DO

!--------------------------------------------------------------------------
! 2.1 Leggo il prossimo record 

  READ (20,'(a)',IOSTAT=ios) chrec
  k = k + 1
  IF (ios == eof) EXIT main

  IF (INDEX(chrec,"v7d_transform") /= 0) THEN
    cnt_head1 = cnt_head1 + 1
    CYCLE main

  ELSE IF (INDEX(chrec,"D")/=0 .OR. INDEX(chrec,"L")/=0 .OR. &
           INDEX(chrec,"B")/=0) THEN
    IF (TRIM(chrec) == head2_req) THEN
      cnt_head2 = cnt_head2 + 1
      CYCLE main
    ELSE
      WRITE (*,*) "Header inatteso in ",TRIM(filein), " riga ",k
      cnt_head1 = cnt_head1 - 1
      DO k2 = 1,HUGE(0)
        READ (20,'(a)',IOSTAT=ios) chrec
        k = k + 1
        IF (ios == eof) EXIT main
        IF (INDEX(chrec,"v7d_transform") /= 0) THEN
          cnt_head1 = cnt_head1 + 1
          WRITE (*,*) "Salto ",k2-1," righe e proseguo"
          CYCLE main
        ENDIF
      ENDDO
    ENDIF

  ELSE
    CALL init(csvline,RECORD=chrec,NFIELD=nf)
    IF (nf /= nf_req) GOTO 9997
    ier(:) = 0
    val(:) = rmiss
    CALL csv_record_getfield(csvline,FIELD=ch_isodate,IER=ier(1))
    CALL csv_record_getfield(csvline,FIELD=lon,IER=ier(2))
    CALL csv_record_getfield(csvline,FIELD=lat,IER=ier(3))
    CALL csv_record_getfield(csvline,FIELD=gts_blo,IER=ier(4))
    CALL csv_record_getfield(csvline,FIELD=gts_sta,IER=ier(5))
    CALL csv_record_getfield(csvline,FIELD=quo,IER=ier(6))
    CALL csv_record_getfield(csvline,FIELD=val(1),IER=ier(7))
    CALL csv_record_getfield(csvline,FIELD=val(2),IER=ier(8))
    CALL csv_record_getfield(csvline,FIELD=val(3),IER=ier(9))
    CALL csv_record_getfield(csvline,FIELD=val(4),IER=ier(10))
    CALL csv_record_getfield(csvline,FIELD=val(5),IER=ier(11))
    CALL csv_record_getfield(csvline,FIELD=val(6),IER=ier(12))
    IF (ANY(ier(1:nf_req) /= 0)) GOTO 9996

    CALL init(data,ISODATE=ch_isodate)
    CALL delete(csvline)
    cnt_dat = cnt_dat + 1
  ENDIF

!--------------------------------------------------------------------------
! 2.2 Elaboro i dati
 
! 2.2.1 Primo record con dati
  IF (cnt_dat == 1) THEN
    data_rsd = data
    lon_rsd = lon
    lat_rsd = lat
    gts_blo_rsd = gts_blo
    gts_sta_rsd = gts_sta
    quo_rsd = quo

    nlev_rsd = 1
    val_rsd(:,:) = rmiss
    val_rsd(1:6,nlev_rsd) = val(1:6)

! 2.2.2 Nuovo livello del radiosondaggio corrente
  ELSE IF (gts_blo_rsd == gts_blo .AND. gts_sta_rsd == gts_sta .AND. &
           data_rsd == data) THEN

    IF (nlev_rsd >= max_lev) GOTO 9995
    IF (ABS(lon_rsd - lon) > eps .OR. ABS(lat_rsd - lat) > eps .OR. &
        ABS(quo_rsd - quo) > eps) GOTO 9994

    nlev_rsd = nlev_rsd + 1
    val_rsd(1:6,nlev_rsd) = val(1:6)

! 2.2.3 Inizia un nuovo radiosondaggio
  ELSE
    CALL check_rsd(nlev_rsd,val_rsd(1:6,1:nlev_rsd))
    IF (linterp) CALL interp_rsd(nlev_rsd,val_rsd(1:6,1:nlev_rsd))
    IF (lzidro) CALL zidro_rsd(nlev_rsd,val_rsd(1:6,1:nlev_rsd),quo)

    CALL write_rsd(out_time,out_quo,nlev_rsd,val_rsd(1:6,1:nlev_rsd), &
      data_rsd,lon_rsd,lat_rsd,gts_blo_rsd,gts_sta_rsd,quo_rsd)
    cnt_rsd = cnt_rsd + 1

    data_rsd = data
    lon_rsd = lon
    lat_rsd = lat
    gts_blo_rsd = gts_blo
    gts_sta_rsd = gts_sta
    quo_rsd = quo

    nlev_rsd = 1
    val_rsd(:,:) = rmiss
    val_rsd(1:6,nlev_rsd) = val(1:6)
  ENDIF

ENDDO main

! 2.2.4 Scrivo l'ultimo sondaggio
IF (nlev_rsd > 0) THEN
  CALL check_rsd(nlev_rsd,val_rsd(1:6,1:nlev_rsd))
  IF (linterp) CALL interp_rsd(nlev_rsd,val_rsd(1:6,1:nlev_rsd))
  IF (lzidro) CALL zidro_rsd(nlev_rsd,val_rsd(1:6,1:nlev_rsd),quo)
  
  CALL write_rsd(out_time,out_quo,nlev_rsd,val_rsd(1:6,1:nlev_rsd), &
    data_rsd,lon_rsd,lat_rsd,gts_blo_rsd,gts_sta_rsd,quo_rsd)
  cnt_rsd = cnt_rsd + 1
ENDIF

!==========================================================================
! 3) Conclusione

IF (cnt_head1 /= cnt_head2) THEN
  WRITE (*,*) "Warning: trovato un numero diverso di occorrenze dei 2 header"
  WRITE (*,*) "1o header: ",cnt_head1,", 2o header: ",cnt_head2
ENDIF

WRITE (*,*) "Elaborazioni terminate: "
WRITE (*,*) "Input: ",cnt_head1," segmenti ",cnt_dat," record di dati"
WRITE (*,*) "Ouput: ",cnt_rsd," sondaggi"

STOP

!==========================================================================
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9997 CONTINUE
WRITE (*,*) "Numero di campi inattteso in ",TRIM(filein), " riga ",k
WRITE (*,*) "Attesi ",nf_req," trovati ",nf
STOP 3

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein), " riga ",k
WRITE (*,*) TRIM(chrec)
DO k2 = 1,nf_req
  IF (ier(k2) /=0) WRITE (*,*) "  errore nel campo ",k2
ENDDO
STOP 4

9995 CONTINUE
WRITE (*,*) TRIM(filein), " riga ",k
WRITE (*,*) "Trovati troppi livelli nel sondaggio ",cnt_rsd+1
WRITE (*,*) "Aumentare parametro max_lev, attualmente ",max_lev
STOP 5

9994 CONTINUE
WRITE (*,*) TRIM(filein), " riga ",k
WRITE (*,*) "Trovati dati con coordinate diverse nello stesso sondaggio"
IF (ABS(lon_rsd - lon) > eps) WRITE (*,*) &
  "lon: torvata ",lon," attesa ",lon_rsd
IF (ABS(lat_rsd - lat) > eps) WRITE (*,*) &
  "lat: torvata ",lat," attesa ",lat_rsd
IF (ABS(quo_rsd - quo) > eps) WRITE (*,*) &
  "quo: torvata ",quo," attesa ",quo_rsd
STOP 6

END PROGRAM temp_csv2asc

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE check_rsd(nlev_rsd,val_rsd)
!-------------------------------------------------------------------------
! Mette a dato mancante i valori palesemente errati
! Verifica che i livelli sino ordinati dall'alto (ie. con P crescente)
!
! Todo: ordinare i livelli, nel caso non lo siano (usare la funzione sort
!   di libsim:array_utilities)
!-------------------------------------------------------------------------

USE missing_values
IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: nlev_rsd
REAL, INTENT(INOUT) :: val_rsd(6,nlev_rsd)

! Variabili locali
REAL, PARAMETER :: g = 9.80665
REAL, PARAMETER :: t0 = 273.15

INTEGER :: kl

!-------------------------------------------------------------------------
! 1) Controllo di qualita'

! Pressione
WHERE (val_rsd(1,1:nlev_rsd) <= 0.)
  val_rsd(1,1:nlev_rsd) = rmiss
ENDWHERE

! Temperatura
WHERE (val_rsd(2,1:nlev_rsd) < t0-100. .OR. val_rsd(2,1:nlev_rsd) > t0+50.)
  val_rsd(2,1:nlev_rsd) = rmiss
ENDWHERE

! Temperatura di rugiada
WHERE (val_rsd(3,1:nlev_rsd) < 0. .OR. val_rsd(3,1:nlev_rsd) > t0+50.)
  val_rsd(3,1:nlev_rsd) = rmiss
ENDWHERE

! Direzione del vento
WHERE (val_rsd(4,1:nlev_rsd) < 0. .OR. val_rsd(4,1:nlev_rsd) > 360.)
  val_rsd(4,1:nlev_rsd) = rmiss
ENDWHERE

! Velocita' del vento
WHERE (val_rsd(5,1:nlev_rsd) < 0. .OR. val_rsd(5,1:nlev_rsd) > 150.)
  val_rsd(5,1:nlev_rsd) = rmiss
ENDWHERE

!-------------------------------------------------------------------------
! 2) Controllo l'ordine dei livelli

DO kl = 2,nlev_rsd
  IF (val_rsd(1,kl) < val_rsd(1,kl-1)) THEN
    WRITE (*,*) "[temp_csv2asc.ex, sub. sort_rsd] Trovato TEMP con P decerescente"
    STOP
  ENDIF
ENDDO

RETURN
END SUBROUTINE check_rsd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE interp_rsd(nlev_rsd,val_rsd)
!-------------------------------------------------------------------------
! Interpola in verticale i dati mancanti di T, Td, DD, FF
!-------------------------------------------------------------------------

USE missing_values
IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: nlev_rsd
REAL, INTENT(INOUT) :: val_rsd(6,nlev_rsd)

! Variabili locali
REAL(KIND=8) :: lnp1,lnp2,lnp,slope
INTEGER :: kp,kl,l1,l2,prev_ok(6,nlev_rsd),next_ok(6,nlev_rsd)
INTEGER :: cnt_int(2:6),cnt_mis(2:6)

!-------------------------------------------------------------------------
! 1) Costruisco gli array prev_ok e next_ok: per ciascun valore, contengono 
!    il livello  con dati validi immediatamente precedente e successivo.
!    Per i dati validi, i due valori sono uguali al livello del dato.
!    Se un dato non ha livelli validi sopra/sotto, gli array valgono imiss

next_ok(:,:) = imiss
DO kl = 1, nlev_rsd
  IF (val_rsd(1,kl) /= rmiss) THEN
    DO kp = 2,6
      IF (val_rsd(kp,kl) /= rmiss) next_ok(kp,kl:nlev_rsd) = kl
    ENDDO
  ENDIF
ENDDO

prev_ok(:,:) = imiss
DO kl = nlev_rsd, 1, -1
  IF (val_rsd(1,kl) /= rmiss) THEN
    DO kp = 2,6
      IF (val_rsd(kp,kl) /= rmiss) prev_ok(kp,1:kl) = kl
    ENDDO
  ENDIF
ENDDO

!-------------------------------------------------------------------------
! 2) Ove possibile, interpolo (in ln(P)) i dati mancanti

cnt_int(:) = 0
cnt_mis(:) = 0

DO kl = 1, nlev_rsd
DO kp = 2,6
  l1 = prev_ok(kp,kl)
  l2 = next_ok(kp,kl)
  IF (val_rsd(kp,kl) == rmiss) THEN
    IF (val_rsd(1,kl) /= rmiss .AND. l1 /= imiss .AND. l2 /= imiss) THEN
      lnp1 = LOG(val_rsd(1,l1))
      lnp2 = LOG(val_rsd(1,l2))
      lnp = LOG(val_rsd(1,kl))
      slope = (val_rsd(kp,l2) - val_rsd(kp,l1)) / (lnp2 - lnp1)
      val_rsd(kp,kl) =  REAL(DBLE(val_rsd(kp,l1)) + slope * (lnp - lnp1))
      cnt_int(kp) = cnt_int(kp) + 1
    ELSE
      cnt_mis(kp) = cnt_mis(kp) + 1
    ENDIF
  ENDIF
ENDDO
ENDDO

WRITE (*,'(2(a,5i4))') "Valori interpolati (T,Td,DD,FF,Z): ",cnt_int(2:6), &
  " rimasti mancanti: ",cnt_mis(2:6)

RETURN
END SUBROUTINE interp_rsd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE zidro_rsd(nlev_rsd,val_rsd,quo)
!-------------------------------------------------------------------------
! Ricalcola le quote (var_rsd(6,:)) in base all'idrostatica:
! d(Phi) = -RT d(lnP)
!
! Note:
! Il calcolo sembra banale, ma nasconde varie trappole: 
! - nel TEMP vengono mescolati livelli standard e significativi
! - il geop. di norma e' riportato solo per i livelli standard
! - non sono riuscito a individuare in modo univoco nel TEMP la Psup
! Quindi per il calcolo delle quote dei livelli:
! - se geop al primo livello e' presente (di solito e' il livello 1000 hPa
!   sottoterra), parto da questo
! - se e' mancante, suppongo che sia la superficie e gli attribuisco la
!   quota della stazione
! - all'atto pratico, le quote cosi' ricostruite possono differire di
!   qualche decina di m da quelle del sondaggio...
!-------------------------------------------------------------------------

USE missing_values
IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: nlev_rsd
REAL, INTENT(INOUT) :: val_rsd(6,nlev_rsd)
REAL, INTENT(IN) :: quo

! Variabili locali
REAL, PARAMETER :: eps = 0.622
REAL, PARAMETER :: t0 = 273.15
REAL, PARAMETER :: g = 9.80665
REAL, PARAMETER :: r = 287.          ! Costante gas perfetti [J/(K.Kg)]
REAL(KIND=8) :: dlnp,tdc,pmb,esattd,q,tv,tmed
INTEGER :: kp,kl

!-------------------------------------------------------------------------

IF (val_rsd(6,nlev_rsd) == rmiss) val_rsd(6,nlev_rsd) = quo * g

DO kl = nlev_rsd-1, 1, -1
  IF (val_rsd(1,kl) == rmiss .OR. val_rsd(2,kl) == rmiss) GOTO 9999
  IF (val_rsd(3,kl) /= rmiss) THEN
    tdc = DBLE(val_rsd(3,kl) - t0)
  ELSE
    tdc = -t0
  ENDIF
  pmb = DBLE(val_rsd(1,kl) / 100.)
  esattd = 6.1078 * EXP((17.2693882*tdc) / (tdc+237.3))
  q = eps * esattd / (pmb - (1-eps)*esattd)
  tv = (1 + 0.61 * q) * DBLE(val_rsd(2,kl))
  dlnp = LOG(DBLE(val_rsd(1,kl))) -  LOG(DBLE(val_rsd(1,kl+1)))
  tmed = 0.5 * DBLE(val_rsd(2,kl) + val_rsd(2,kl+1))
  val_rsd(6,kl) = val_rsd(6,kl+1) - r * tmed * dlnp
ENDDO

RETURN

9999 CONTINUE
WRITE (*,*) "Dati mancanti al livello ",nlev_rsd-kl+1," P=",val_rsd(1,kl), &
  " calcolo idrostatico delle quote incompleto"
RETURN

END SUBROUTINE zidro_rsd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_rsd(out_time,out_quo,nlev_rsd,val_rsd, &
  data_rsd,lon_rsd,lat_rsd,gts_blo_rsd,gts_sta_rsd,quo_rsd)
!-------------------------------------------------------------------------
! Scrive su file i dati di un radiosondaggio, nel formato di estra_temp
!-------------------------------------------------------------------------

USE datetime_class
USE missing_values

IMPLICIT NONE

! Argomenti della subroutine
TYPE (datetime), INTENT(IN) :: data_rsd
REAL, INTENT(IN) :: val_rsd(6,nlev_rsd),lon_rsd,lat_rsd,quo_rsd
INTEGER, INTENT(IN) :: nlev_rsd,gts_blo_rsd,gts_sta_rsd
CHARACTER (LEN=3), INTENT(IN) :: out_time,out_quo

! Nomi delle stazioni TEMP
INTEGER, PARAMETER :: nsta_ok = 8
INTEGER, PARAMETER :: blo_ok(nsta_ok) = (/ &
  16,16,16,16,16,16,16,16 /)
INTEGER, PARAMETER :: sta_ok(nsta_ok) = (/ &
  044,080,113,144,245,320,429,560 /)
CHARACTER(LEN=24), PARAMETER :: nome_sta_ok(nsta_ok) = (/ &
  "UDINE/CAMPOFORMIDO      ","MILANO/LINATE           ", &
  "CUNEO-LEVALDIGI         ","S. PIETRO CAPOFIUME     ", &
  "PRATICA DI MARE         ","BRINDISI                ", &
  "TRAPANI BIRGI           ","CAGLIARI ELMAS          " /)

! Variabili locali
REAL, PARAMETER :: g = 9.80665
REAL, PARAMETER :: t0 = 273.15
REAL, PARAMETER :: rmiss_tem = -9999.
REAL :: val_out(6,nlev_rsd)
TYPE (datetime) :: data_out
INTEGER :: yy,mm,dd,hh,mnt,kl,kst
CHARACTER (LEN=250) :: fileout
CHARACTER (LEN=24) :: nome_sta

!-------------------------------------------------------------------------

! Calcolo la data in ouptut
data_out = data_rsd
CALL getval (data_rsd, HOUR=hh, MINUTE=mnt)

IF (out_time == "syn") THEN 
  mnt = 0
  IF (hh <= 1) THEN
    hh = 0
  ELSE IF (hh <= 7) THEN
    hh = 6
  ELSE IF (hh <= 13) THEN
    hh = 12
  ELSE IF (hh <= 19) THEN
    hh = 18
  ELSE
    data_out = data_rsd + timedelta_new(DAY=1)
    hh = 00
  ENDIF
ENDIF

CALL getval (data_out, YEAR=yy, MONTH=mm, DAY=dd)

! Apro il file di ouptut
WRITE (fileout,'(i2.2,i3.3,a1,i4.4,3i2.2,a4)') &
  gts_blo_rsd,gts_sta_rsd,"_",yy,mm,dd,hh,".dat"
OPEN (UNIT=30, FILE=fileout, STATUS="REPLACE")

! Scrivo gli headers
nome_sta = ""
DO kst = 1,nsta_ok
  IF (gts_blo_rsd == blo_ok(kst) .AND. gts_sta_rsd == sta_ok(kst)) &
    nome_sta = nome_sta_ok(kst)
ENDDO

WRITE (30,'(1x,i2.2,i3.3,1x,a24,1x,i4.4,2(i2.2),1x,2(i2.2),1x,i3.3,1x,i4,2(1x,f10.5))') &
  gts_blo_rsd,gts_sta_rsd,nome_sta,yy,mm,dd,hh,mnt,nlev_rsd,NINT(quo_rsd),lon_rsd,lat_rsd
IF (out_quo == "sup") THEN
  WRITE (30,'(a)') "     Press.      Temp.     T.rug.     Direz.     Modulo      Z-sup"
ELSE IF (out_quo == "slm") THEN
  WRITE (30,'(a)') "     Press.      Temp.     T.rug.     Direz.     Modulo      Z-slm"
ENDIF

! Converto unita' di misura
val_out(:,:) = rmiss_tem
DO kl = 1,nlev_rsd
  IF (val_rsd(1,kl) /= rmiss) val_out(1,kl) = val_rsd(1,kl) / 100.
  IF (val_rsd(2,kl) /= rmiss) val_out(2,kl) = val_rsd(2,kl) - t0
  IF (val_rsd(3,kl) /= rmiss) val_out(3,kl) = val_rsd(3,kl) - t0
  IF (val_rsd(4,kl) /= rmiss) val_out(4,kl) = val_rsd(4,kl)
  IF (val_rsd(5,kl) /= rmiss) val_out(5,kl) = val_rsd(5,kl)
  IF (val_rsd(6,kl) /= rmiss .AND. out_quo == "sup") &
     val_out(6,kl) = val_rsd(6,kl) / g - quo_rsd
  IF (val_rsd(6,kl) /= rmiss .AND. out_quo == "slm") &
     val_out(6,kl) = (val_rsd(6,kl) / g)
ENDDO

! Scrivo i dati
DO kl = nlev_rsd,1,-1
  WRITE (30,'(6(1x,f10.1))') val_out(1:6,kl)
ENDDO

CLOSE (30)

RETURN
END SUBROUTINE write_rsd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: temp_csv2asc.exe [-h] filein [-tlaunch] [-zlsm] [-interp] [-zidro]" 
WRITE (*,*) ""
WRITE (*,*) "filein:   prodotto da v7d_transform --output-format=csv"
WRITE (*,*) "-tlaunch: scrive in output l'effettiva ora di lancio del sondaggio (default:"
WRITE (*,*) "          scrive la corrispondente ora sinottica)"
WRITE (*,*) "-zslm     scrive in output le quote SLM (default: dalla superficie)"
WRITE (*,*) "-h:       visualizza questo help"
WRITE (*,*) "-interp   interpola in verticale i valori mancanti (default: scrive solo i"
WRITE (*,*) "          dati effettivamente presenti nel TEMP; utile per dati vecchi)"
WRITE (*,*) "-zidro    ricalcola le quote con l'idrostatica, in base al profilo di T."
WRITE (*,*) "          In pratica, le quote ricalcolate sono diverse dal geopotenziale"
WRITE (*,*) "          ai livelli standard scritto nei Temp: usare questa opzione solo"
WRITE (*,*) "          se servono quote di livelli che con -interp sarebbero mancanti"
WRITE (*,*) ""
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

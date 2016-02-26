! Tipi derivati
MODULE local
TYPE varlivsca
  INTEGER :: l1,lt1,p1,p2,tr
  CHARACTER (LEN=6) :: bcode
  CHARACTER (LEN=200) :: short_name
END TYPE varlivsca
TYPE point_id
  INTEGER :: lat5,lon5
  CHARACTER (LEN=40) :: report
END TYPE point_id
END MODULE local

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

PROGRAM bufr_csv_int2orari
!--------------------------------------------------------------------------
! Legge un segmento relativo a una varibaile estratto da un file csv 
! prodotto con "dbamsg dump --interpeted --csv" e lo riscrive nei vecchi
! formati "estra_orari" o "estra_qa day".
!
! Uso: bufr_csv_int2orari.exe filein filepar data_ini data_fin fileout"
!      [-dy] [-offset H] [-ndec N] [-tc] [-phpa] [-deb] [-h]"
!
! Note:
! - Programma chiamato da estra_oss_int.sh
! - Programma derivato da bufr_csv2orari.f90 (elabora i files prodotti da
!   "dbamsg dump --csv"); rispetto a questo, la semplificazione principale
!   e' che elabora un parametro alla volta, cosa possbile perche' con l'
!   opzione --interpeted la data finisce nella stessa riga del valore.
!
! - Nel file di input gli istanti devono essere in ordine cronologico
! - Per i dati non istantanei, il programm considera sempre la fine 
!   dell'intervallo di elaborazione (convenzione Dballe/BUFR)
! - Il programma elabora solo i record relativi al par/liv/scad richiesto,
!   ma il file di input ne puo' contenere altri.
!
!                                         Versione 1.0.0, Enrico 25/02/2016
!--------------------------------------------------------------------------

USE file_utilities
USE datetime_class
USE missing_values
USE char_utilities
USE local

IMPLICIT NONE

REAL, PARAMETER :: rmiss_out = -9999. ! Valore per dati mancanti in output
INTEGER, PARAMETER :: iu_in = 20      ! Unita' per lettura filein
INTEGER, PARAMETER :: iu_par = 21     ! Unita' per lettura filepar
INTEGER, PARAMETER :: iu_out = 22     ! Unita' per scrittura fileout
INTEGER, PARAMETER :: iu_log = 23     ! Unita' per scrittura filelog

! Altre variabili locali
TYPE (csv_record) :: csvline
TYPE (varlivsca) :: vls_req,vls_read
TYPE(point_id) :: idp_read,idp_first
TYPE (datetime) :: datah_req1,datah_req2,datah_req,datah_in1,datah_in2
TYPE (datetime) :: vtime_read,vtime_last

REAL :: val_read,val_out
INTEGER :: idp,ndec,hoff,nh_day,ndays,nrec_out,nerr,nnrq
INTEGER :: yy,mm,dd,hh,mn
INTEGER :: cnt_err,cnt_didp,cnt_nrq,cnt_dble,cnt_rq,cnt_ok
INTEGER :: eof,eor,ios,ier(10),iret,k,kp,kd,kh,idum,nf
CHARACTER (LEN=200) :: filein,fileout,filelog,filepar,chfmt1,chfmt2,chrec,chdum
CHARACTER (LEN=12) :: ch12a,ch12b
CHARACTER (LEN=3) :: next_arg
CHARACTER (LEN=2) :: out_fmt
CHARACTER (LEN=1) :: var_kind
LOGICAL :: ldeb,ltc,lphpa,end_inp

!==========================================================================
! 1) Preliminari

filelog = "bufr_csv_int2orari.log"

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

out_fmt = "hr"
next_arg = ""
idp = 0
ndec = 1 
hoff = 0
ldeb = .FALSE.
ltc = .FALSE.
lphpa = .FALSE.

DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (next_arg == "nde" ) THEN
    READ (chdum,*,IOSTAT=ios) ndec
    IF (ios /= 0) GOTO 9999
    next_arg = ""
  ELSE IF (next_arg == "ofs" ) THEN
    READ (chdum,*,IOSTAT=ios) hoff
    IF (ios /= 0) GOTO 9999
    next_arg = ""
  ELSE IF (TRIM(chdum) == "-dy") THEN
    out_fmt = "dy"
  ELSE IF (TRIM(chdum) == "-deb") THEN
    ldeb = .TRUE.
  ELSE IF (TRIM(chdum) == "-tc") THEN
    ltc = .TRUE.
  ELSE IF (TRIM(chdum) == "-phpa") THEN
    lphpa = .TRUE.
  ELSE IF (TRIM(chdum) == "-ndec") THEN
    next_arg = "nde"
  ELSE IF (TRIM(chdum) == "-offset") THEN
    next_arg = "ofs"
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      filepar = chdum
    CASE (3)
      READ (chdum,'(i4,2i2)',IOSTAT=ios) yy,mm,dd
      IF (ios /= 0) GOTO 9999
      datah_req1 = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=00)
    CASE (4)
      READ (chdum,'(i4,2i2)',IOSTAT=ios) yy,mm,dd
      IF (ios /= 0) GOTO 9999
      datah_req2 = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=23)
    CASE (5)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

IF (idp /= 5) THEN
  CALL write_help
  STOP 1
ENDIF

CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 1.2 Leggo la definizione del parametro richiesto

OPEN (UNIT=iu_par, FILE=filepar, STATUS="OLD", FORM="FORMATTED", ERR=9998)
READ (iu_par,'(a)',IOSTAT=ios) chrec  
IF (ios /= 0) GOTO 9997
CLOSE(iu_par)

CALL init(csvline,RECORD=chrec,NFIELD=nf)
IF (nf < 10) GOTO 9997
CALL csv_record_getfield(csvline,FIELD=idum,IER=ier(1))
CALL csv_record_getfield(csvline,FIELD=vls_req%bcode,IER=ier(2))
CALL csv_record_getfield(csvline,FIELD=chdum,IER=ier(3))
CALL csv_record_getfield(csvline,FIELD=vls_req%l1,IER=ier(4))
CALL csv_record_getfield(csvline,FIELD=vls_req%lt1,IER=ier(5))
CALL csv_record_getfield(csvline,FIELD=vls_req%p1,IER=ier(6))
CALL csv_record_getfield(csvline,FIELD=vls_req%p2,IER=ier(7))
CALL csv_record_getfield(csvline,FIELD=vls_req%tr,IER=ier(8))
CALL csv_record_getfield(csvline,FIELD=chdum,IER=ier(9))
CALL csv_record_getfield(csvline,FIELD=vls_req%short_name,IER=ier(10))
IF (ANY(ier(1:10) /= 0)) GOTO 9997

IF (vls_req%bcode == "B12101" .OR. vls_req%bcode == "B12103") THEN
  var_kind = "T"     ! Temperatura
ELSE IF (vls_req%bcode == "B10004" .OR. vls_req%bcode == "B10051") THEN
  var_kind = "P"     ! Pressione
ELSE IF (vls_req%bcode == "B15195") THEN
  var_kind = "Q"     ! Qualita' dell'aria
ELSE
  var_kind = "x"     ! altro
ENDIF

!--------------------------------------------------------------------------
! 1.3 Inizializzazioni dipendenti dai parametri da riga comando e da quelli 
!     richiesti in output

CALL getval (datah_req2 - datah_req1, DAY=idum)
ndays = idum + 1

IF (out_fmt == "hr") THEN
  nh_day = 24
  chfmt1 = "(a13,1x,a10)"
  IF (ndec >= 0 .AND. ndec <= 7) THEN
    WRITE (chfmt2,'(a,2(i3,a))') "(i4.4,3(1x,i2.2),1x,f10.",ndec,")"
  ELSE
    WRITE (chfmt2,'(a,i3,a)') "(i4.4,3(1x,i2.2),1x,e10.3)"
  ENDIF

ELSE IF (out_fmt == "dy") THEN
  nh_day = 1
  chfmt1 = "(a10,1x,a10)"
  IF (ndec >= 0 .AND. ndec <= 7) THEN
    WRITE (chfmt2,'(a,i3,a)') "(i4.4,2(1x,i2.2),1x,f10.",ndec,")"
  ELSE
    WRITE (chfmt2,'(a,i3,a)') "(i4.4,2(1x,i2.2),1x,e10.3)"
  ENDIF

ENDIF

IF (ldeb) &
  OPEN (UNIT=iu_log, FILE=filelog, STATUS="REPLACE", FORM="FORMATTED")

!--------------------------------------------------------------------------
! 1.4 Apro filein e leggo il primo dato utile

OPEN (UNIT=iu_in, FILE=filein, STATUS="OLD", FORM="FORMATTED", ERR=9996)
cnt_err = 0
cnt_didp = 0
cnt_nrq = 0
cnt_dble = 0
cnt_rq = 0
cnt_ok = 0

CALL read_next_req(iu_in,vls_req,hoff, &
  idp_read,vtime_read,val_read,iret,nerr,nnrq,ldeb,iu_log)
cnt_err = cnt_err + nerr
cnt_nrq = cnt_nrq + nnrq
idp_first = idp_read
datah_in1 = vtime_read
datah_in2 = vtime_read

IF (iret /= 0) THEN
  WRITE (*,'(a)') "Nessun record relativo al par/var/liv richiesto"
  WRITE (*,'(a,i6)') "  non interpretabili ",nerr
  WRITE (*,'(a,i6)') "  non richiesti      ",nnrq
  STOP 1
ELSE
  cnt_rq = cnt_rq + 1
ENDIF

!--------------------------------------------------------------------------
! 1.5 Apro fileout e scrivo gli headers

OPEN (UNIT=iu_out, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (iu_out,'(2(f10.5,1x),a)') REAL(idp_read%lon5)/100000, &
  REAL(idp_read%lat5)/100000.,TRIM(idp_read%report)
WRITE (iu_out,*)
IF (out_fmt == "hr") THEN
  WRITE (iu_out,chfmt1) "aaaa mm gg hh",ADJUSTR(vls_req%short_name(1:10))
ELSE IF (out_fmt == "dy") THEN
  WRITE (iu_out,chfmt1) "aaaa mm gg",ADJUSTR(vls_req%short_name(1:10))
ENDIF

!==========================================================================
! 2) Elaborazioni (ciclo sulle ore richieste in output)

end_inp = .FALSE.
DO kd = 1,ndays
DO kh = 0,nh_day-1
  datah_req = datah_req1 + timedelta_new(DAY=kd-1, HOUR=kh)

! Scorro i dati finche' non raggiungo (o supero) l'istante richiesto
  IF (vtime_read < datah_req) THEN
!  IF (.NOT. end_inp .AND. vtime_read < datah_req) THEN
    DO

      vtime_last = vtime_read
      CALL read_next_req(iu_in,vls_req,hoff, &
        idp_read,vtime_read,val_read,iret,nerr,nnrq,ldeb,iu_log)
      cnt_err = cnt_err + nerr
      cnt_nrq = cnt_nrq + nnrq
      IF (iret /= 0) THEN
        end_inp = .TRUE. 
        EXIT
      ELSE IF (vtime_read < vtime_last) THEN
        GOTO 9995
      ENDIF

      IF (idp_read%lon5 /= idp_first%lon5 .OR. idp_read%lat5 /= idp_first%lat5) &
        WRITE (*,'(a,2(a,2f10.5))') "Warning, ci sono coordinate diverse:",&
        "trovato: ",REAL(idp_read%lon5)/10000., REAL(idp_read%lat5)/10000., &
        "atteso:  ",REAL(idp_first%lon5)/10000.,REAL(idp_first%lat5)/10000.

      IF (vtime_read == vtime_last) THEN
        cnt_dble = cnt_dble + 1
      ELSE IF (vtime_read >= datah_req) THEN
        cnt_rq = cnt_rq + 1
        datah_in2 = vtime_read
        EXIT
      ENDIF

    ENDDO
  ENDIF

! Dato macante (fine file, oppure ho superato l'istante richiesto)
  IF (end_inp .OR. vtime_read > datah_req) THEN
    val_out = rmiss_out

! Dato valido: converto untia' di misura
  ELSE
    IF (val_read == rmiss) THEN
      val_out = rmiss_out
    ELSE IF (var_kind == "T" .AND. ltc) THEN
      val_out = val_read - 273.5
    ELSE IF (var_kind == "P" .AND. lphpa) THEN
      val_out = val_read * 0.01
    ELSE IF (var_kind == "Q") THEN
      val_out = val_read * 1.e9
    ELSE
      val_out = val_read
    ENDIF

  ENDIF

! Scrivo su fileout il record relativo all'istante richiesto (in tutti i casi)
  IF (val_out /= rmiss_out) cnt_ok = cnt_ok + 1
  CALL getval(datah_req, YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
  IF (out_fmt == "hr") THEN
    WRITE (iu_out,chfmt2) yy,mm,dd,hh,val_out
  ELSE IF (out_fmt == "dy") THEN
    WRITE (iu_out,chfmt2) yy,mm,dd,val_out
  ENDIF

ENDDO
ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione

! Log a schermo
IF (out_fmt == "hr") THEN
  nrec_out = ndays*24
ELSE IF (out_fmt == "dy") THEN
  nrec_out = ndays
ENDIF
CALL getval(datah_in1, SIMPLEDATE=ch12a)
CALL getval(datah_in2, SIMPLEDATE=ch12b)
WRITE (*,*)
WRITE (*,'(3a)')   "Input (",TRIM(filein),"):"
WRITE (*,'(15a)')   "  date estreme: ", &
  ch12a(1:4),"-",ch12a(5:6),"-",ch12a(7:8)," ",ch12a(9:12), &
  ch12b(1:4),"-",ch12b(5:6),"-",ch12b(7:8)," ",ch12b(9:12)
WRITE (*,'(a,i6)') "  record letti:         ",cnt_err + cnt_nrq + cnt_dble + cnt_rq
WRITE (*,'(a,i6)') "    non interpretabili: ",cnt_err
WRITE (*,'(a,i6)') "    non richiesti:      ",cnt_nrq
WRITE (*,'(a,i6)') "    date ripetute:      ",cnt_dble
WRITE (*,'(a,i6)') "    dati utilizzabili:  ",cnt_rq
WRITE (*,*)
WRITE (*,'(3a)')   "Output (",TRIM(fileout),"):"
WRITE (*,'(a,i6)') "  istanti richiesti ",nrec_out
WRITE (*,'(a,i6,a,f6.2,a)') "  dati validi:      ",cnt_ok, &
   " (",REAL(cnt_ok)/REAL(nrec_out)*100.," %)"

! Chiudo files
CLOSE(iu_in)
CLOSE(iu_out)

! Codice d'errore finale
IF (cnt_ok == 0) THEN
  STOP 103                        ! tutti i dati richiesti sono mancanti
ELSE IF (cnt_ok < nrec_out) THEN
  STOP 102                        ! alcuni dei dati richiesti sono mancanti
ELSE                               
  STOP 0                          ! trovati tutti i dati richiesti
ENDIF

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore nei parametri"
WRITE (*,*) ""
CALL write_help
STOP 1

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filepar)
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filepar)," (prima riga)"
IF (ios /= 0) THEN
  WRITE (*,*) "ios ",ios
ELSE
  WRITE (*,*) TRIM(chrec)
  IF (nf <10) THEN
    WRITE (*,*) "nf ",nf
  ELSE
    DO k = 1,10
      IF (ier(k) /= 0) WRITE (*,*) "Errore lettura campo ",k
    ENDDO
  ENDIF
ENDIF
STOP 2

9996 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9995 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein),": date non sequenziali"
CALL getval(vtime_last, SIMPLEDATE=ch12a)
CALL getval(vtime_read, SIMPLEDATE=ch12b)
WRITE (*,'(a,1x,a)') ch12a,ch12b
STOP 3

END PROGRAM bufr_csv_int2orari

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_next_req(iu,vls_req,hoff,idp_read,vtime_read,val_read, &
  iret,nerr,nnrq,ldeb,iu_log)
!
! Legge dall'unita' iu gia' aperta il prossimo record corrisopndente al 
! parametro richiesto.
! Di questo ritorna: identificativo punto, verification time e valore 
! iret: 0=dato trovato; 1=dato non torvato
! nnrq: numero di record letti non corrispondenti al parametro richiesto
! nerr: numero di record che non e' stato possibile leggere o interpretare
!
USE file_utilities
USE datetime_class
USE missing_values
USE char_utilities
USE local

IMPLICIT NONE

! Parametri della subroutine
INTEGER, INTENT(IN) :: iu,iu_log,hoff
TYPE(varlivsca), INTENT(IN) :: vls_req
LOGICAL, INTENT(IN) :: ldeb
TYPE(point_id), INTENT(OUT) :: idp_read
TYPE(datetime), INTENT(OUT) :: vtime_read
REAL, INTENT(OUT) :: val_read
INTEGER, INTENT(OUT) :: iret,nerr,nnrq

! Variabili locali
TYPE (csv_record) :: csvline
TYPE (timedelta) :: offset
REAL :: rlon,rlat,val
INTEGER :: yy,mm,dd,hh,min,ss
INTEGER :: ios,ier(13),l1,lt1,l2,lt2,tr,p1,p2,nf
CHARACTER (LEN=200) :: chrec
CHARACTER (LEN=40) :: rep,chdum
CHARACTER (LEN=19) :: str_date
CHARACTER (LEN=6) :: bcode

!--------------------------------------------------------------------------

nerr = 0
nnrq = 0
offset = timedelta_new(HOUR=hoff)

DO
! Metto mancati i parametri in ouptut
  idp_read = point_id(lat5=imiss, lon5=imiss, report="")
  vtime_read = datetime_miss
  val_read = rmiss

! Leggo il prossimo record da filein
  READ (iu,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) THEN
    iret = 1
    RETURN
  ENDIF
  
! Lo interpreto
  CALL init(csvline,RECORD=chrec,NFIELD=nf)
  ier (:) = 0
  CALL csv_record_getfield(csvline,FIELD=rlon,IER=ier(1))
  CALL csv_record_getfield(csvline,FIELD=rlat,IER=ier(2))
  CALL csv_record_getfield(csvline,FIELD=rep,IER=ier(3))
  CALL csv_record_getfield(csvline,FIELD=str_date,IER=ier(4))
  CALL csv_record_getfield(csvline,FIELD=lt1,IER=ier(5))
  CALL csv_record_getfield(csvline,FIELD=l1,IER=ier(6))
  CALL csv_record_getfield(csvline,FIELD=lt2,IER=ier(7))
  CALL csv_record_getfield(csvline,FIELD=l2,IER=ier(8))
  CALL csv_record_getfield(csvline,FIELD=tr,IER=ier(9))
  CALL csv_record_getfield(csvline,FIELD=p1,IER=ier(10))
  CALL csv_record_getfield(csvline,FIELD=p2,IER=ier(11))
  CALL csv_record_getfield(csvline,FIELD=bcode,IER=ier(12))
  CALL csv_record_getfield(csvline,FIELD=chdum,IER=ier(13))
  IF (ANY(ier(:) /= 0)) THEN
    IF (ldeb) WRITE (iu_log,'(a,13i5)') "Errore parsing csv ",ier(:)
    nerr = nerr + 1
    CYCLE
  ENDIF
  READ(chdum,*,IOSTAT=ios) val
  IF (ios /= 0) THEN
    IF (ldeb) WRITE (iu_log,'(2a)') "Chiave non numerica: ",TRIM(chdum)
    nerr = nerr + 1
    CYCLE
  ENDIF

! Calcolo verification time (scarto i dati non alle ore esatte)
! Conicide sempre con il reference time: sto elaborando dati osservati (forecast
! time = 0), e per i dati non istantanei la convenzione e' che il reftime sia la 
! fine dell'intervallo di elaborazione (quando viene fatta la misura)

  yy = imiss
  mm = imiss
  dd = imiss
  hh = imiss
  min = imiss
  ss = imiss
  READ (str_date,'(i4,5(1x,i2))',IOSTAT=ios) yy,mm,dd,hh,min,ss
  IF (ios /= 0) THEN
    IF (ldeb) WRITE (iu_log,'(a,6i5)') "Errore lettura data ",yy,mm,dd,hh,min,ss
    nerr = nerr + 1
    CYCLE
  ELSE IF (min /= 0 .OR. ss /= 0) THEN
    IF (ldeb) WRITE (iu_log,'(a,6i5)') "Data non e' ora esatta ",yy,mm,dd,hh,min,ss
    nnrq = nnrq + 1
    CYCLE
  ENDIF
  vtime_read = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh) + offset

! Controllo se si riferisce al var/liv/scad richiesto
  IF (l1 /= vls_req%l1 .OR. lt1 /= vls_req%lt1 .OR. tr /= vls_req%tr .OR. &
      p1 /= vls_req%p1 .OR. p2 /= vls_req%p2 .OR. bcode /= vls_req%bcode) THEN

    IF (ldeb) THEN
      chdum = ""
      IF (l1 /= vls_req%l1)  chdum = TRIM(chdum) // " l1"
      IF (lt1 /= vls_req%lt1) chdum = TRIM(chdum) // " lt1"
      IF (tr /= vls_req%tr) chdum = TRIM(chdum) // " tr"
      IF (p1 /= vls_req%p1) chdum = TRIM(chdum) // " p1"
      IF (p2 /= vls_req%p2) chdum = TRIM(chdum) // " p2"
      IF (bcode /= vls_req%bcode) chdum = TRIM(chdum) // " bcode"
      WRITE (iu_log,'(a,i5,3i3,2a)') "Var/liv/sca non richiesto; data:", &
        yy,mm,dd,hh," differenze: ",TRIM(chdum)
    ENDIF

    nnrq = nnrq + 1
    CYCLE
  ENDIF

! Se e' tutto ok ritorno il valore appena letto
  idp_read%lon5 = NINT(rlon*1.e5)
  idp_read%lat5 = NINT(rlat*1.e5)
  idp_read%report = rep
  val_read = val
  iret =0
  IF (ldeb) WRITE (iu_log,'(a,4i5)') "Record ok ",yy,mm,dd,hh
  RETURN

ENDDO

END SUBROUTINE read_next_req

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

!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "Uso: bufr_csv_int2orari.exe filein filepar data_ini data_fin fileout"
WRITE (*,*) "     [-dy] [-offset H] [-tc] [-phpa] [-ndec N] [-deb] [-h]"
WRITE (*,*)
WRITE (*,*) "filein:   BUFR riscritto csv, prodotto da ""dbamsg dump --interpreted --csv"""
WRITE (*,*) "filepar:  definizione del parametro richiesto in output. Una riga; formato "
WRITE (*,*) "          param_arkioss.csv"
WRITE (*,*) "data_ini: prima data richiesta in output (AAAAMMGG)"
WRITE (*,*) "data_fin: ultima data richiesta in output (AAAAMMGG)"
WRITE (*,*) "fileout:  file di ouput (formato estra_orari senza header)"
WRITE (*,*)
WRITE (*,*) "-dy       per ogni giorno scrive il dato alle ore 00, in formato estra_qa day"
WRITE (*,*) "-offset H aggiunge H ore all'istante di tutti i dati (puo' essere negativo)"
WRITE (*,*) "-deb:     scrive sul file bufr_csv_int2orari.log un report di tutti i record"
WRITE (*,*) "          letti dal file di input (una riga per ogni record)"
WRITE (*,*) "-h:       visualizza questo help"
WRITE (*,*)
WRITE (*,*) "Codici d'errore:"
WRITE (*,*) "0:     trovati tutti i dati richiesti"
WRITE (*,*) "102:   alcuni dei dati richiesti sono mancanti"
WRITE (*,*) "103:   tutti i dati richiesti sono mancanti"
WRITE (*,*) "altro: fatal error"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

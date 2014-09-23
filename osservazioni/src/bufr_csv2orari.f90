! Modulo per tipo derivato "parametro meteo"
MODULE local
  TYPE metpar
    INTEGER :: idvar,l1,lt1,p1,p2,tr
    CHARACTER (LEN=200) :: long_name,short_name,unit
    CHARACTER (LEN=6) :: bcode
  END TYPE metpar
END MODULE local

PROGRAM bufr_csv2orari
!--------------------------------------------------------------------------
! Legge un file csv prodotto da "dbamsg dump -csv", relativo a una stazione
! superficiale, e lo riscrive nel vecchio formato "estra_orari".
!
! Uso: bufr_csv2orari.exe [-syn] filein data_ini data_fin filepar idsta 
!   [-tc] [-phpa] [-ndec N ] [-deb] [-h]
!
! Note:
! Programma chiamato da estra_oss.ksh
!
! Todo:
! - Fare in modo che se ci sono due messaggi synop consecutivi relativi 
!   alla stessa ora prenda i dati del secondo.
! - Aggiungere informazioni all'header (nome staz, nome rete,coordinate); 
!   Metodi possibili: passare da  riga comando, leggere B01194, accedre 
!   ad anagrafica ufficiale info.
! - Aggiungere opzione -uv per scrivere le componenti del vento invece di
!   direzione e modulo. 
!   Metodo: alla lettura dei parametri richiesti, bisogna abbinare le 
!   componenti con livello e timerange corrispondente; modificare l'header
!   (shortname o nuove Btable); ricalcolare i valori prima di scrivere; 
!   aggiungere l'opzione a estra_oss.ksh)
! ? controllo ridondante sull'inizo dei messaggi (key: "edition")
! 
!                                         Versione 1.0.0, Enrico 16/09/2014
!--------------------------------------------------------------------------

USE file_utilities
USE datetime_class
USE missing_values
USE local

IMPLICIT NONE

REAL, PARAMETER :: rmiss_out = -9999. ! Valore per dati mancanti in output
INTEGER, PARAMETER :: maxpar = 40     ! Max numero di parametri in ouptut
INTEGER, PARAMETER :: iu_in = 20      ! Unita' per lettura filein
INTEGER, PARAMETER :: iu_par = 21     ! Unita' per lettura filepar
INTEGER, PARAMETER :: iu_out = 22     ! Unita' per scrittura fileout
INTEGER, PARAMETER :: iu_log = 23     ! Unita' per scrittura filelog

! Altre variabili locali
TYPE (metpar) :: req_par(maxpar)
TYPE (csv_record) :: csvline
TYPE (datetime) :: datah_req1,datah_req2,datah_in1,datah_in2
TYPE (datetime) :: datah_req,datah_next,datah_dum
REAL, ALLOCATABLE :: values(:)
INTEGER :: idum,ios,eof,eor,ier(10),iret,k,kp,kskip,kd,kh
INTEGER :: idp,idsta,yy,mm,dd,hh,nf,pp,npar,ndays,nok,ndec
INTEGER :: cnt_date_in,cnt_date_miss,cnt_valok_out,cnt_istok_out
CHARACTER (LEN=200) :: filein,fileout,filepar,filelog,chdum,chrec,chfmt1,chfmt2
CHARACTER (LEN=10) :: ch10a,ch10b
CHARACTER (LEN=3) :: inp_type,next_arg
LOGICAL, ALLOCATABLE :: par_ok(:)
LOGICAL :: end_inp,ldeb,ltc,lphpa,is_temp(maxpar),is_pres(maxpar)

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
next_arg = ""
inp_type="hfr"
idp = 0
ndec = 1 
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
  ELSE IF (TRIM(chdum) == "-deb") THEN
    ldeb = .TRUE.
  ELSE IF (TRIM(chdum) == "-tc") THEN
    ltc = .TRUE.
  ELSE IF (TRIM(chdum) == "-phpa") THEN
    lphpa = .TRUE.
  ELSE IF (TRIM(chdum) == "-syn") THEN
    inp_type="syn"
  ELSE IF (TRIM(chdum) == "-ndec") THEN
    next_arg = "nde"
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      READ (chdum,'(i4,2i2)',IOSTAT=ios) yy,mm,dd
      IF (ios /= 0) GOTO 9999
      datah_req1 = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=00)
    CASE (3)
      READ (chdum,'(i4,2i2)',IOSTAT=ios) yy,mm,dd
      IF (ios /= 0) GOTO 9999
      datah_req2 = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=23)
    CASE (4)
      filepar = chdum
    CASE (5)
      READ (chdum,*,IOSTAT=ios) idsta
      IF (ios /= 0) GOTO 9999
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

! 1.2 Leggo la lista dei parametri richiesti
is_temp(:) = .FALSE.
is_pres(:) = .FALSE.

OPEN (UNIT=iu_par, FILE=filepar, STATUS="OLD", FORM="FORMATTED", ERR=9998)
DO kp = 1,HUGE(0)
  READ (iu_par,'(a)',IOSTAT=ios) chrec  
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9997
  IF (kp > maxpar) GOTO 9996

  CALL init(csvline,RECORD=chrec,NFIELD=nf)
  IF (nf < 10) GOTO 9997
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%idvar,IER=ier(1))
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%bcode,IER=ier(2))
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%long_name,IER=ier(3))
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%l1,IER=ier(4))
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%lt1,IER=ier(5))
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%p1,IER=ier(6))
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%p2,IER=ier(7))
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%tr,IER=ier(8))
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%unit,IER=ier(9))
  CALL csv_record_getfield(csvline,FIELD=req_par(kp)%short_name,IER=ier(10))
  IF (ANY(ier(1:10) /= 0)) GOTO 9997
  IF (req_par(kp)%bcode == "B12101" .OR. req_par(kp)%bcode == "B12103") & 
    is_temp(kp) = .TRUE.
  IF (req_par(kp)%bcode == "B10004" .OR. req_par(kp)%bcode == "B10051") &
    is_pres(kp) = .TRUE.

ENDDO
CLOSE(iu_par)
npar = kp-1

! Inizializzazioni dipendenti dai parametri da riga comando e da quelli 
! richiesti in output

CALL getval (datah_req2 - datah_req1, DAY=idum)
ndays = idum + 1

ALLOCATE (values(npar),par_ok(npar))

WRITE (chfmt1,'(a,i3,a)') "(a13,",npar,"(1x,a10))"
IF (ndec >= 0 .AND. ndec <= 7) THEN
  WRITE (chfmt2,'(a,2(i3,a))') "(i4.4,3(1x,i2.2),",npar,"(1x,f10.",ndec,"))"
ELSE
  WRITE (chfmt2,'(a,i3,a)') "(i4.4,3(1x,i2.2),",npar,"(1x,e10.3))"
ENDIF

IF (ldeb) THEN
  WRITE (filelog,'(a3,i5.5,a4)') "eo_",idsta,".log"
  OPEN (UNIT=iu_log, FILE=filelog, STATUS="REPLACE", FORM="FORMATTED")
  WRITE (iu_log,'(a25,1x,a6,5(1x,a5))') "Parameteri richiesti     ",&
    "Bcode ","l1","lt1","p1","p2","tr"
  DO kp = 1,npar
    WRITE (iu_log,'(a25,1x,a6,5(1x,i5))') "                         ", &
      req_par(kp)%bcode,req_par(kp)%l1,req_par(kp)%lt1, &
      req_par(kp)%p1,req_par(kp)%p2,req_par(kp)%tr
  ENDDO  
  WRITE (iu_log,*)
  WRITE (iu_log,'(a25,1x,a6,5(1x,a5),1x,a10)') "Messaggi in input        ",&
    "Bcode ","l1","lt1","p1","p2","tr","value"
ENDIF

! 1.3 Apro filein e leggo la prima data che contiene
cnt_date_in = 0
cnt_date_miss = 0
cnt_valok_out = 0
cnt_istok_out = 0
par_ok(:) = .FALSE.
end_inp = .FALSE.

OPEN (UNIT=iu_in, FILE=filein, STATUS="OLD", FORM="FORMATTED", ERR=9995)
DO k = 1,HUGE(0)
  READ (iu_in,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) GOTO 9994
  IF (ios /= 0) GOTO 9993
  pp = INDEX(chrec,",")
  IF (pp == 0) CYCLE
  IF (chrec(1:pp-1) == "date") THEN
    READ (chrec(pp+1:pp+13),'(i4,3(1x,i2))',IOSTAT=ios) yy,mm,dd,hh
    IF (ios /= 0) GOTO 9993
    datah_next =  datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
    cnt_date_in = cnt_date_in + 1
    EXIT
  ENDIF
ENDDO
datah_in1 = datah_next

! 1.4 Apro fileout e scrivo gli header
WRITE (fileout,'(a3,i5.5,a4)') "eo_",idsta,".dat"
OPEN (UNIT=iu_out, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (iu_out,'(i5.5)') idsta
WRITE (iu_out,*)
WRITE (iu_out,chfmt1) "aaaa mm gg hh",ADJUSTR(req_par(1:npar)%short_name(1:10))

!--------------------------------------------------------------------------
! 2) Elaborazioni (ciclo sulle ore richieste in output)

DO kd = 1,ndays
DO kh = 0,23
  datah_req = datah_req1 + timedelta_new(DAY=kd-1, HOUR=kh)

! Se i dati di input hanno frequenza temporale maggiore di un'ora, li 
! scorro finche' non raggiungo (o supero) l'istante richiesto
  DO kskip = 1,HUGE(0)
    IF (end_inp .OR. datah_req <= datah_next) EXIT
    CALL skip_input_time(iu_in,eof,datah_next,datah_dum,iret)

    IF (iret == 1) THEN
      end_inp = .TRUE.
    ELSE IF (iret == 2) THEN
      GOTO 9992
    ELSE IF (iret == 3) THEN
      GOTO 9991
    ELSE
      datah_in2 = datah_dum
      datah_next = datah_dum
      cnt_date_in = cnt_date_in + 1
    ENDIF
  ENDDO

! Assegno i valori relativi all'istante richiesto (leggo o metto mancanti)
  values(:) = rmiss

  IF (.NOT. end_inp .AND. datah_req > datah_next) THEN
    GOTO 9990

  ELSE IF (end_inp .OR. datah_req < datah_next) THEN
    cnt_date_miss = cnt_date_miss + 1

  ELSE
    IF (ldeb) THEN
      CALL getval(datah_req, SIMPLEDATE=ch10a)
      WRITE (iu_log,*)
      WRITE (iu_log,'(2a)') "*** Prossima data: ",ch10a
    ENDIF

    IF (inp_type == "hfr") THEN
      CALL get_msg_hfr(iu_in,iu_log,ldeb,eof,datah_req,npar,req_par(1:npar), &
         values,datah_dum,iret,nok)
    ELSE IF (inp_type == "syn") THEN
      CALL get_msg_syn(iu_in,eof,datah_req,npar,req_par(1:npar), &
         values,datah_dum,iret,nok)
    ENDIF

    IF (iret == 1) THEN
      end_inp = .TRUE.
    ELSE IF (iret == 2) THEN
      GOTO 9993
    ELSE IF (iret == 3) THEN
      GOTO 9991
    ELSE
      datah_in2 = datah_dum
      datah_next = datah_dum
      cnt_date_in = cnt_date_in + 1
    ENDIF

  ENDIF

! Se richiesto, correggo le unita' di misura
  IF (ltc) THEN
    WHERE (is_temp(1:npar) .AND. values(1:npar) /= rmiss)
      values(1:npar) = values(1:npar) - 273.15
    ENDWHERE
  ENDIF

  IF (lphpa) THEN
    WHERE (is_pres(1:npar) .AND. values(1:npar) /= rmiss)
      values(1:npar) = values(1:npar) * 0.01
    ENDWHERE
  ENDIF

! Scrivo su fileout il record relativo all'istante richiesto
  cnt_valok_out = cnt_valok_out + COUNT(values(1:npar) /= rmiss)
  IF (ANY(values(1:npar) /= rmiss)) cnt_istok_out = cnt_istok_out + 1
  WHERE (values(1:npar) /= rmiss)
    par_ok(1:npar) = .TRUE.
  ENDWHERE
  WHERE (values(1:npar) == rmiss) values(1:npar) = rmiss_out
  CALL getval(datah_req, YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
  WRITE (iu_out,chfmt2) yy,mm,dd,hh,values(1:npar)

ENDDO
ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione

! Log a schermo
CALL getval(datah_in1, SIMPLEDATE=ch10a)
CALL getval(datah_in2, SIMPLEDATE=ch10b)
WRITE (*,*)
WRITE (*,'(3a)') "Input (",TRIM(filein),"):"
WRITE (*,'(a,i3)') "  istanti nel file: ",cnt_date_in
WRITE (*,'(a,i3)') "  istanti mancanti: ",cnt_date_miss
WRITE (*,'(4a)')   "  date estreme: ",ch10a," ",ch10b
WRITE (*,*)
WRITE (*,'(3a)') "Output (",TRIM(fileout),"):"
WRITE (*,'(2(a,i4))') "  richiesti: istanti ",ndays*24," parametri ",npar
WRITE (*,'(2(a,i6),a,f6.2,a)') "  dati validi:        ",cnt_valok_out, &
  " su ",ndays*24*npar," (",REAL(cnt_valok_out)/REAL(ndays*24*npar)*100.," %)"
WRITE (*,'(2(a,i6),a,f6.2,a)') "  istanti con dati:   ",cnt_istok_out, &
  " su ",ndays*24," (",REAL(cnt_istok_out)/REAL(ndays*24)*100.," %)"
WRITE (*,'(2(a,i6))')          "  parametri con dati: ",COUNT(par_ok(1:npar)), &
  " su ",npar

! Chiudo files
CLOSE(iu_in)
CLOSE(iu_out)
CLOSE(iu_log)

STOP 0

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
WRITE (*,*) "Errore leggendo ",TRIM(filepar)," record ",k
IF (ios /= 0) THEN
  IF (ios /= 0) WRITE (*,*) "ios ",ios
ELSE
  WRITE (*,*) chrec
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
WRITE (*,*) "Richiesti troppi parametri: aumentare maxpar (attuale: ",maxpar,")"
STOP 3

9995 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9994 CONTINUE
WRITE (*,*) "Errore: ",TRIM(filein)," non contiene nessun record ""date"""
STOP 3

9993 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein),": record illegale"
WRITE (*,*) TRIM(chrec)
STOP 3

9992 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein),": date non sequenziali"
CALL getval(datah_next, SIMPLEDATE=ch10a)
CALL getval(datah_dum, SIMPLEDATE=ch10b)
WRITE (*,'(a,1x,a)') ch10a,ch10b
STOP 3

9991 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein),": date non sequenziali"
CALL getval(datah_req, SIMPLEDATE=ch10a)
CALL getval(datah_dum, SIMPLEDATE=ch10b)
WRITE (*,'(a,1x,a)') ch10a,ch10b
STOP 3

9990 CONTINUE
WRITE (*,*) "Erroraccio gestione date (chiamare l'assistenza)"
STOP 4

END PROGRAM bufr_csv2orari

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_msg_hfr(iu,iu_log,ldeb,eof,datah_req,npar,req_par,values,datah_next,ier,nok)
!
! Legge dall'unita' iu (gia aperta) i dati relativi a un'istante temporale
! Ritorna i valori relativi ai parametri richiesti e la data successiva
! presente nel file. 
!
! Codici d'errore:
! 1: raggiunta la fine di filein (values definiti, datah_next mancante)
! 2: errore di lettura in filein (fatal)
! 3: date non sequenziali in filein (fatal)
!
! Chiavi utili nei messaggi BUFR-SIMC scritti csv (dati ad alta frequenza)
!   edition: inizio messaggio
!   B07192 [SIM] First level type (257: introduce anagrafica ???)
!   B07193 [SIM] Level L1
!   B04192 [SIM] Time range type
!   B04193 [SIM] Time range P1
!   B04194 [SIM] Time range P2
!   B04001 YEAR
!   B04002 MONTH
!   B04003 DAY
!   B04004 HOUR
!   B01194 [SIM] Report mnemonic (rete)
!   B05001 LATITUDE
!   B06001 LONGITUDE
!   B07030 HEIGHT OF STATION GROUND ABOVE MSL

USE datetime_class
USE missing_values
USE local

IMPLICIT NONE

! Parametri della subroutine
TYPE (datetime), INTENT(IN) :: datah_req
TYPE (metpar), INTENT(IN) :: req_par(npar)
INTEGER, INTENT(IN) :: iu,iu_log,eof,npar
LOGICAL, INTENT(IN) :: ldeb

TYPE (datetime), INTENT(OUT) :: datah_next
REAL, INTENT(OUT) :: values(npar)
INTEGER, INTENT(OUT) :: ier,nok

! Variabili locali
TYPE (datetime) :: datah_dum
TYPE (metpar) :: par_dum
REAL :: val_dum
INTEGER :: ios,k,kp,yy,mm,dd,hh,pp
CHARACTER (LEN=200) :: chrec,key

!--------------------------------------------------------------------------

values(:) = rmiss
par_dum = metpar(0,imiss,imiss,imiss,imiss,imiss,"","","","")
val_dum = rmiss
nok = 0

DO k = 1,HUGE(0)
  READ (iu,'(a)',IOSTAT=ios) chrec

! Fine file: salvo l'ultimo valore letto e termino
  IF (ios == eof) THEN
    IF (ldeb) CALL write_log_hfr(iu_log,npar,par_dum,val_dum,req_par(1:npar))
    DO kp = 1,npar
      IF (par_dum%bcode==req_par(kp)%bcode .AND. &
          par_dum%lt1==req_par(kp)%lt1 .AND. &
          par_dum%l1==req_par(kp)%l1 .AND. par_dum%tr==req_par(kp)%tr .AND. &
          par_dum%p1==req_par(kp)%p1 .AND. par_dum%p2==req_par(kp)%p2) THEN
        values(kp) = val_dum
        nok = nok + 1
        EXIT
      ENDIF
    ENDDO
    datah_next = datetime_miss
    ier = 1
    RETURN

  ELSE IF (ios /= 0) THEN
    GOTO 9999

  ENDIF

  pp = INDEX(chrec,",")
  IF (pp == 0) CYCLE
  key = chrec(1:pp-1)

! Inizia un nuovo messaggio
  IF (key == "date") THEN
    READ (chrec(pp+1:pp+13),'(i4,3(1x,i2))',IOSTAT=ios) yy,mm,dd,hh
    IF (ios /= 0) GOTO 9999
    datah_dum = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)

!   Data precedente a quella richiesta: fatal error
    IF (datah_dum < datah_req) THEN
      datah_next = datah_dum
      ier = 3
      RETURN 

!   Data successiva a quella richiesta: salvo i valori letti e concludo
    ELSE IF (datah_dum > datah_req) THEN
      IF (ldeb) CALL write_log_hfr(iu_log,npar,par_dum,val_dum,req_par(1:npar))
      DO kp = 1,npar
        IF (par_dum%bcode==req_par(kp)%bcode .AND. &
            par_dum%lt1==req_par(kp)%lt1 .AND. &
            par_dum%l1==req_par(kp)%l1 .AND. par_dum%tr==req_par(kp)%tr .AND. &
            par_dum%p1==req_par(kp)%p1 .AND. par_dum%p2==req_par(kp)%p2) THEN
          values(kp) = val_dum
          nok = nok + 1
          EXIT
        ENDIF
      ENDDO
      datah_next = datah_dum
      ier = 0
      RETURN

!   Data uguale a quella richiesta: salvo il valore del parametro appena letto
!   (se e' uno di quelli richiesti)
    ELSE
      IF (ldeb) CALL write_log_hfr(iu_log,npar,par_dum,val_dum,req_par(1:npar))
      DO kp = 1,npar
        IF (par_dum%bcode==req_par(kp)%bcode .AND. &
            par_dum%lt1==req_par(kp)%lt1 .AND. &
            par_dum%l1==req_par(kp)%l1 .AND. par_dum%tr==req_par(kp)%tr .AND. &
            par_dum%p1==req_par(kp)%p1 .AND. par_dum%p2==req_par(kp)%p2) THEN
          values(kp) = val_dum
          nok = nok + 1
          EXIT
        ENDIF
      ENDDO
      par_dum = metpar(0,imiss,imiss,imiss,imiss,imiss,"","","","")
      val_dum = rmiss
      nok = 0
    ENDIF

! Prosegue la lettura del messaggio in corso

! key fa parte della codifica par/liv
  ELSE IF (key == "B07192") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%lt1
    IF (ios /= 0) goto 9999
  ELSE IF (key == "B07193") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%l1
    IF (ios /= 0) goto 9999
  ELSE IF (key == "B04192") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%tr
    IF (ios /= 0) goto 9999
  ELSE IF (key == "B04193") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%p1
    IF (ios /= 0) goto 9999
  ELSE IF (key == "B04194") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%p2
    IF (ios /= 0) goto 9999
  ELSE

!   key corrisponde a una delle variabili fisiche richieste in output
    DO kp = 1, npar
      IF (key == req_par(kp)%bcode) THEN
        READ (chrec(pp+1:),*,IOSTAT=ios) val_dum
        par_dum%bcode = key(1:6)
        EXIT
      ENDIF
    ENDDO
  ENDIF

ENDDO

RETURN

9999 CONTINUE
WRITE (*,*) TRIM(chrec)
ier = 2
RETURN

END SUBROUTINE get_msg_hfr

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_msg_syn(iu,eof,datah_req,npar,req_par,values,datah_next,ier,nok)
!
! Legge dall'unita' iu (gia aperta) i dati relativi a un messaggio synop.
! Ritorna i valori relativi ai parametri richiesti e la data successiva
! presente nel file. 
!
! NOTE:
! - Nei BUFR-WMO tutti i parametri sono inclusi nello stesso messaggio: 
!   questa subr. si limita quindi a leggere un solo messaggio, e ritorna i 
!   valori corrispondenti ai parametri richiesti.
! - Non vengono controllati livello e timerange (suppongo che le variabili
!   fisiche compaiano una sola volta nel messaggio)  
! - La subroutine gestisce la presenza di chiavi senza valore (frequenti)
! - In alcuni casi, il file bufr contiene messaggi ripetuti, che (in 
!   apparenza) differiscono solo per "centre" e "master table" (di solito
!   alle ore non sinottiche; approfondire)
!
! Chiavi utili nei messaggi BUFR-WMO scritti csv:
!   edition: inizio messaggio
!   B01001 WMO BLOCK NUMBER
!   B01002 WMO STATION NUMBER
!   B01015 Station name ?
!   B05001 LATITUDE
!   B06001 LONGITUDE
!   B07030 HEIGHT OF STATION GROUND ABOVE MSL
!   B07031 HEIGHT OF BAROMETER ABOVE MEAN SEA LEVEL 
!   B04001 YEAR
!   B04002 MONTH
!   B04003 DAY
!   B04004 HOUR
!   B10004 PRESSURE
!   B12101 TEMPERATURE/DRY-BULB TEMPERATURE
!   B12103 DEW-POINT TEMPERATURE 
!   B11001 WIND DIRECTION 
!   B11002 WIND SPEED
!   B20010 CLOUD COVER (TOTAL) 
!   B13003 RELATIVE HUMIDITY
!   B10051 SLP

USE datetime_class
USE missing_values
USE local

IMPLICIT NONE

! Parametri della subroutine
TYPE (datetime), INTENT(IN) :: datah_req
TYPE (metpar), INTENT(IN) :: req_par(npar)
INTEGER, INTENT(IN) :: iu,eof,npar

TYPE (datetime), INTENT(OUT) :: datah_next
REAL, INTENT(OUT) :: values(npar)
INTEGER, INTENT(OUT) :: ier,nok

! Variabili locali
INTEGER :: ios,k,kp,yy,mm,dd,hh,pp
CHARACTER (LEN=200) :: chrec,key

!--------------------------------------------------------------------------

values(:) = rmiss
nok = 0

DO k = 1,HUGE(0)
  READ (iu,'(a)',IOSTAT=ios) chrec

! Fine file: termino
  IF (ios == eof) THEN
    datah_next = datetime_miss
    ier = 1
    RETURN
  ELSE IF (ios /= 0) THEN
    GOTO 9999
  ENDIF

  pp = INDEX(chrec,",")
  IF (pp == 0) CYCLE
  key = chrec(1:pp-1)

! Trovo una nuova data: termino (il messaggio e' concluso)
! Se la nuova data e' precedente a quella in input, lo segnalo (fatal error)
  IF (key == "date") THEN
    READ (chrec(pp+1:pp+13),'(i4,3(1x,i2))',IOSTAT=ios) yy,mm,dd,hh
    IF (ios /= 0) GOTO 9999
    datah_next = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
    IF (datah_next < datah_req) THEN
      ier = 3
    ELSE
      ier = 0
    ENDIF
    RETURN

! Cerco i parametri richiesti
  ELSE
    DO kp = 1,npar
      IF (key(1:6) == req_par(kp)%bcode .AND. LEN(TRIM(chrec)) > pp+1) THEN
        READ (chrec(pp+1:),*,IOSTAT=ios) values(kp)      
        IF (ios /= 0) GOTO 9999
        EXIT
      ENDIF
    ENDDO

  ENDIF
ENDDO

RETURN

9999 CONTINUE
WRITE (*,*) TRIM(chrec)
ier = 2
RETURN

END SUBROUTINE get_msg_syn

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE skip_input_time(iu,eof,datah_next,datah_dum,ier)
!
! Scorre il file aperto su iu fino al prossimo record "data" 
! Necessaria per gestire files di input con frequenza piu' che oraria.
!
! Codici d'errore:
! 1: raggiunta la fine di filein (values definiti, datah_next mancante)
! 2: errore di lettura in filein (fatal)
! 3: date non sequenziali in filein (fatal)

USE datetime_class

IMPLICIT NONE

! Parametri della subroutine
INTEGER, INTENT(IN) :: iu,eof
TYPE (datetime), INTENT(IN) :: datah_next

TYPE (datetime), INTENT(OUT) :: datah_dum
INTEGER, INTENT(OUT) :: ier

! Variabili locali
INTEGER :: k,pp,yy,mm,dd,hh,ios
CHARACTER (LEN=200) :: chrec

!-------------------------------------------------------------------------

DO k = 1,HUGE(0)

  READ (iu,'(a)',IOSTAT=ios) chrec

  IF (ios == eof) THEN
    datah_dum = datetime_miss
    ier = 1
    RETURN

  ELSE IF (ios /= 0) THEN
    GOTO 9999

  ELSE
    pp = INDEX(chrec,",")
    IF (pp == 0) CYCLE
    IF (chrec(1:pp-1) == "date") THEN
      READ (chrec(pp+1:pp+13),'(i4,3(1x,i2))',IOSTAT=ios) yy,mm,dd,hh
      IF (ios /= 0) GOTO 9999
      datah_dum =  datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
      IF (datah_dum < datah_next) THEN
        ier = 3
      ELSE
        ier = 0
      ENDIF
      RETURN
    ENDIF

  ENDIF
ENDDO

RETURN

9999 CONTINUE
WRITE (*,*) TRIM(chrec)
ier = 2
RETURN

END SUBROUTINE skip_input_time

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_log_hfr(iu,npar,par_dum,val_dum,req_par)
!
! Scrive su iu le informazioni relative all'ultimo messaggio letto
! Replica i controlli eseguti nella sub. chiamante
!

USE local
IMPLICIT NONE

TYPE (metpar), INTENT(IN) :: req_par(npar),par_dum
REAL, INTENT(IN) :: val_dum
INTEGER, INTENT(IN) :: iu,npar

INTEGER :: kp,idp
CHARACTER(LEN=200) :: chrec

1000 FORMAT(a25,1x,a6,5(1x,i5),1x,f10.1)

!

DO kp = 1,npar
  IF (par_dum%bcode==req_par(kp)%bcode .AND. &
      par_dum%lt1==req_par(kp)%lt1 .AND. &
      par_dum%l1==req_par(kp)%l1 .AND. par_dum%tr==req_par(kp)%tr .AND. &
      par_dum%p1==req_par(kp)%p1 .AND. par_dum%p2==req_par(kp)%p2) THEN
    WRITE (iu,1000) "msg richiesto            ", &
      par_dum%bcode,par_dum%l1,par_dum%lt1,par_dum%p1,par_dum%p2,par_dum%tr,val_dum
    RETURN
  ELSE IF (par_dum%bcode==req_par(kp)%bcode) THEN
    idp = kp
  ENDIF  
ENDDO

IF (par_dum%bcode=="") THEN
  WRITE (iu,1000) "par non richiesto        ", &
    "B-----",par_dum%l1,par_dum%lt1,par_dum%p1,par_dum%p2,par_dum%tr,val_dum

ELSE
  WRITE (chrec,1000) "lev/trange non richiesto ", &
    par_dum%bcode,par_dum%l1,par_dum%lt1,par_dum%p1,par_dum%p2,par_dum%tr,val_dum
  IF (par_dum%l1/=req_par(idp)%l1) chrec = TRIM(chrec) // " l1"
  IF (par_dum%lt1/=req_par(idp)%lt1) chrec = TRIM(chrec) // " lt1"
  IF (par_dum%p1/=req_par(idp)%p1) chrec = TRIM(chrec) // " p1"
  IF (par_dum%p2/=req_par(idp)%p2) chrec = TRIM(chrec) // " p2"
  IF (par_dum%tr/=req_par(idp)%tr) chrec = TRIM(chrec) // " tr"
  WRITE (iu,'(a)') TRIM(chrec)

ENDIF

RETURN
END SUBROUTINE write_log_hfr

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
WRITE (*,*) "Uso: bufr_csv2orari.exe [-syn] filein data_ini data_fin filepar idsta"
WRITE (*,*) "     [-tc] [-phpa] [-ndec N] [-deb] [-h]"
WRITE (*,*) "-syn:    elabora i dati di una stazione Synop (def: stazione ad alta frequenza)"
WRITE (*,*) "filein:  BUFR riscritto csv, prodotto da ""dbamsg dump -csv"""
WRITE (*,*) "data_ini: prima data richiesta in output (AAAAMMGG)"
WRITE (*,*) "data_fin: ultima data richiesta in output (AAAAMMGG)"
WRITE (*,*) "filepar: lista dei parametri richiesti in output (formato param_arkioss.csv)"
WRITE (*,*) "idsta:   codice stazione (per header e nome file output)"
WRITE (*,*) "-ndec N: forza a N il numero di decimali in output (-1 per notazione exp)"
WRITE (*,*) "-uv      scrive le temperature in gradi centigradi (def: K)"
WRITE (*,*) "-tc      scrive le temperature in gradi centigradi (def: K)"
WRITE (*,*) "-phpa    scrive le presisoni in hPa (def: Pa)"
WRITE (*,*) "-deb:    scrive su file .log l'elenco di tutti i messaggi in input"
WRITE (*,*) "-h:      visualizza questo help"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

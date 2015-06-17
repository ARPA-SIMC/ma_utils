! Modulo per tipi derivati
MODULE local

! 1.1) Tipo derivato "parametro meteo"
  TYPE metpar
    INTEGER :: idvar,l1,lt1,p1,p2,tr
    CHARACTER (LEN=200) :: long_name,short_name,unit
    CHARACTER (LEN=6) :: bcode
  END TYPE metpar

! 1.2) Tipo derivato "angrafica stazione"
  TYPE anag
    REAL :: lon,lat,quo
    CHARACTER (LEN=100) :: rete,nome
  END TYPE anag

  CONTAINS

! 2.1) Copia in anag2 i campi validi di anag1; se trova valori validi e 
!      diversi, manda un warning
  SUBROUTINE copy_anag(anag1,anag2)
  USE missing_values
  IMPLICIT NONE
  TYPE(anag), INTENT(IN) :: anag1
  TYPE(anag), INTENT(INOUT) :: anag2

  IF (c_e(anag1%lon)) THEN
    IF (c_e(anag2%lon) .AND. anag1%lon /= anag2%lon) WRITE (*,*) &
       "Warning anagrafica: lon passa da ",anag2%lon," a ",anag1%lon
    anag2%lon = anag1%lon
  ENDIF

  IF (c_e(anag1%lat)) THEN
    IF (c_e(anag2%lat) .AND. anag1%lat /= anag2%lat) WRITE (*,*) &
       "Warning anagrafica: lat passa da ",anag2%lat," a ",anag1%lat
    anag2%lat = anag1%lat
  ENDIF

  IF (c_e(anag1%quo)) THEN
    IF (c_e(anag2%quo) .AND. anag1%quo /= anag2%quo) WRITE (*,*) &
       "Warning anagrafica: quota passa da ",anag2%quo," a ",anag1%quo
    anag2%quo = anag1%quo
  ENDIF

  IF (anag1%rete /= "") THEN
    IF (anag2%rete /= "" .AND. anag1%rete /= anag2%rete) WRITE (*,*) &
       "Warning anagrafica: rete passa da ",TRIM(anag2%rete)," a ",TRIM(anag1%rete)
    anag2%rete = anag1%rete
  ENDIF

  IF (anag1%nome /= "") THEN
    IF (anag2%nome /= "" .AND. anag1%nome /= anag2%nome) WRITE (*,*) &
       "Warning anagrafica: nome passa da ",TRIM(anag2%nome)," a ",TRIM(anag1%nome)
    anag2%nome = anag1%nome
  ENDIF

  RETURN
  END SUBROUTINE copy_anag

!!!! Vecchia versione !!!!
!! 2.1.2) Copia in anag2 i campi validi di anag1; se trova valori validi e 
!!        diversi, non li copia e manda un warning
!  SUBROUTINE copy_anag(anag1,anag2)
!  USE missing_values
!  IMPLICIT NONE
!  TYPE(anag), INTENT(IN) :: anag1
!  TYPE(anag), INTENT(INOUT) :: anag2
!
!  IF (c_e(anag1%lon)) THEN
!    IF (c_e(anag2%lon) .AND. anag1%lon /= anag2%lon) WRITE (*,*) &
!       "Warning anagrafica: lon passa da ",anag2%lon," a ",anag1%lon
!    anag2%lon = anag1%lon
!  ENDIF
!
!
!
!  IF (c_e(anag1%lon)) THEN
!    IF (c_e(anag2%lon)) THEN
!      IF (anag1%lon /= anag2%lon) WRITE (*,*) &
!       "Warning anagrafica: lon trovata ",anag1%lon," attesa ",anag2%lon
!    ELSE
!      anag2%lon = anag1%lon
!    ENDIF
!  ENDIF
!
!  IF (c_e(anag1%lat)) THEN
!    IF (c_e(anag2%lat)) THEN
!      IF (anag1%lat /= anag2%lat) WRITE (*,*) &
!       "Warning anagrafica: lat trovata ",anag1%lat," attesa ",anag2%lat
!    ELSE
!      anag2%lat = anag1%lat
!    ENDIF
!  ENDIF
!
!  IF (c_e(anag1%quo)) THEN
!    IF (c_e(anag2%quo)) THEN
!      IF (anag1%quo /= anag2%quo) WRITE (*,*) &
!       "Warning anagrafica: quo trovata ",anag1%quo," attesa ",anag2%quo
!    ELSE
!      anag2%quo = anag1%quo
!    ENDIF
!  ENDIF
!  IF (anag1%rete /= "") THEN
!    IF (anag2%rete /= "") THEN
!      IF (anag1%rete /= anag2%rete) WRITE (*,*) &
!       "Warning anagrafica: rete trovata ",TRIM(anag1%rete)," attesa ",TRIM(anag2%rete)
!    ELSE
!      anag2%rete = anag1%rete
!    ENDIF
!  ENDIF
!
!  IF (anag1%nome /= "") THEN
!    IF (anag2%nome /= "") THEN
!      IF (anag1%nome /= anag2%nome) WRITE (*,*) &
!       "Warning anagrafica: nome trovato ",TRIM(anag1%nome)," atteso ",TRIM(anag2%nome)
!    ELSE
!      anag2%nome = anag1%nome
!    ENDIF
!  ENDIF
!
!  RETURN
!  END SUBROUTINE copy_anag

! 2.2) Funzione per verificare se due variabili metpar hanno stess Bcode, 
!      livello e timerange
  FUNCTION metpar_equal(par_dum,par_req) RESULT(leq)
  USE missing_values
  IMPLICIT NONE
  TYPE(metpar), INTENT(IN) :: par_dum,par_req
  LOGICAL leq

  IF (par_dum%bcode==par_req%bcode .AND. &
      (par_req%l1==imiss  .OR. par_dum%l1==par_req%l1) .AND. &
      (par_req%lt1==imiss .OR. par_dum%lt1==par_req%lt1) .AND. &
      (par_req%p1==imiss  .OR. par_dum%p1==par_req%p1) .AND. &
      (par_req%p2==imiss  .OR. par_dum%p2==par_req%p2) .AND. &
      (par_req%tr==imiss  .OR. par_dum%tr==par_req%tr)) THEN
    leq = .TRUE.
  ELSE
    leq = .FALSE.
  ENDIF

  RETURN  
  END FUNCTION metpar_equal

END MODULE local

!==========================================================================

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
! - Eventualmente agigungere opzione per scrivere cloud cover Synop in 
!   ottavi (adesso esce in %; assicurarsi che alameno sia consistente nella
!   serie storica dei synop)
! 
!                                         Versione 2.0.4, Enrico 17/06/2015
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
INTEGER, PARAMETER :: iu_ana = 23     ! Unita' per scrittura anagrafica
INTEGER, PARAMETER :: iu_log = 24     ! Unita' per scrittura filelog

! Altre variabili locali
TYPE (metpar) :: req_par(maxpar)
TYPE (anag) :: anag_dum,anag_out
TYPE (csv_record) :: csvline
TYPE (datetime) :: datah_req1,datah_req2,datah_in1,datah_in2
TYPE (datetime) :: datah_req,datah_next,datah_dum
REAL, ALLOCATABLE :: values(:)
INTEGER :: idum,ios,eof,eor,ier(10),iret,k,kp,kskip,kd,kh
INTEGER :: idp,yy,mm,dd,hh,mn,nf,pp,npar,ndays,ndec,ndiff
INTEGER :: cnt_date_in,cnt_date_miss,cnt_date_skip,cnt_valok_out
INTEGER :: cnt_istok_out,cnt_msg_skip,cnt_diff,cnt_ist_diff
CHARACTER (LEN=200) :: filein,fileout,filepar,filelog,fileana
CHARACTER (LEN=200) :: chdum,chrec,chfmt1,chfmt2
CHARACTER (LEN=20) :: idsta
CHARACTER (LEN=12) :: ch12a,ch12b
CHARACTER (LEN=10) :: ch10a,ch10b
CHARACTER (LEN=6) :: ch6
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
      READ (chdum,'(a)',IOSTAT=ios) idsta
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

WRITE (fileana,'(a3,a,a4)') "eo_",TRIM(idsta),".ana"
OPEN (UNIT=iu_ana, FILE=fileana, STATUS="REPLACE", FORM="FORMATTED")

IF (ldeb) THEN
  WRITE (filelog,'(a3,a,a4)') "eo_",TRIM(idsta),".log"
  OPEN (UNIT=iu_log, FILE=filelog, STATUS="REPLACE", FORM="FORMATTED")
  WRITE (iu_log,'(a25,1x,a6,5(1x,a5))') "Parametri richiesti      ",&
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
anag_out = anag(rmiss,rmiss,rmiss,"","")
cnt_date_in = 0
cnt_date_miss = 0
cnt_date_skip = 0
cnt_valok_out = 0
cnt_istok_out = 0
cnt_msg_skip = 0
cnt_diff = 0
cnt_ist_diff = 0
par_ok(:) = .FALSE.
end_inp = .FALSE.

yy = imiss
mm = imiss
dd = imiss
hh = imiss
mn = imiss

OPEN (UNIT=iu_in, FILE=filein, STATUS="OLD", FORM="FORMATTED", ERR=9995)
DO k = 1,HUGE(0)
  READ (iu_in,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) GOTO 9994
  IF (ios /= 0) GOTO 9993
  pp = INDEX(chrec,",")
  IF (pp == 0) CYCLE
  ios = 0
  IF (chrec(1:pp-1) == "B04001") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) yy
  ELSE IF (chrec(1:pp-1) == "B04002") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) mm
  ELSE IF (chrec(1:pp-1) == "B04003") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) dd
  ELSE IF (chrec(1:pp-1) == "B04004") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) hh
  ELSE IF (chrec(1:pp-1) == "B04005") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) mn
  ENDIF
  IF (ios /= 0) GOTO 9993
  IF (c_e(yy) .AND. c_e(mm) .AND. c_e(dd) .AND. c_e(hh) .AND. c_e(mn)) THEN
    datah_next =  datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh, MINUTE=mn)
    cnt_date_in = cnt_date_in + 1
    EXIT
  ENDIF
ENDDO
datah_in1 = datah_next

! 1.4 Apro fileout e scrivo gli header
WRITE (fileout,'(a3,a,a4)') "eo_",TRIM(idsta),".dat"
OPEN (UNIT=iu_out, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (iu_out,'(a)') TRIM(idsta)
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
      IF (datah_next == datah_dum) THEN
        cnt_msg_skip = cnt_msg_skip + 1
        CYCLE
      ELSE
        IF (ldeb) THEN
          CALL getval(datah_next, SIMPLEDATE=ch12a)
          WRITE (iu_log,'(3a,i5)') "Skip data: ",ch12a," prog. ",cnt_date_in
        ENDIF
        datah_in2 = datah_dum
        datah_next = datah_dum
        cnt_date_in = cnt_date_in + 1
        cnt_date_skip = cnt_date_skip + 1
        cnt_msg_skip = cnt_msg_skip + 1
      ENDIF

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
      CALL getval(datah_req, SIMPLEDATE=ch12a)
      WRITE (iu_log,*)
      WRITE (iu_log,'(3a,i5)') "*** Prossima data: ",ch12a," prog. ",cnt_date_in
    ENDIF

    CALL get_msg(iu_in,iu_log,ldeb,eof,datah_req,npar,req_par(1:npar),inp_type, &
       values,datah_dum,anag_dum,ndiff,iret)

    IF (ndiff > 0) THEN
      cnt_diff = cnt_diff + ndiff
      cnt_ist_diff = cnt_ist_diff + 1
    ENDIF
    CALL copy_anag(anag_dum,anag_out)

    IF (iret == 1) THEN
      end_inp = .TRUE.
    ELSE IF (iret == 2) THEN
      GOTO 9993
    ELSE IF (iret == 3) THEN
      GOTO 9991
    ELSE IF (iret == 4) THEN
      GOTO 9989
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

! Output anagrafica
ch10a = ""
ch10b = ""
ch6 = ""
IF (c_e(anag_out%lat)) WRITE (ch10a,'(f10.5)') anag_out%lat
IF (c_e(anag_out%lon)) WRITE (ch10b,'(f10.5)') anag_out%lon
IF (c_e(anag_out%quo)) WRITE (ch6,'(f6.1)') anag_out%quo
WRITE (iu_ana,'(a,2(a1,a10),a1,a6,2(a1,a))') TRIM(idsta),",",ch10a,",",ch10b,",", &
  ch6,",",TRIM(anag_out%rete),",",TRIM(anag_out%nome)

! Log a schermo
CALL getval(datah_in1, SIMPLEDATE=ch12a)
CALL getval(datah_in2, SIMPLEDATE=ch12b)
WRITE (*,*)
WRITE (*,'(3a)') "Input (",TRIM(filein),"):"
WRITE (*,'(a,i6)') "  istanti letti:    ",cnt_date_in
WRITE (*,'(a,i6)') "  istanti mancanti: ",cnt_date_miss
WRITE (*,'(4a)')   "  date estreme lette: ",ch12a," ",ch12b
WRITE (*,*)
WRITE (*,'(3a)') "Output (",TRIM(fileout),"):"
WRITE (*,'(2(a,i6))') "  richiesti: istanti ",ndays*24," parametri ",npar
WRITE (*,'(2(a,i6),a,f6.2,a)') "  dati validi:        ",cnt_valok_out, &
  " su ",ndays*24*npar," (",REAL(cnt_valok_out)/REAL(ndays*24*npar)*100.," %)"
WRITE (*,'(2(a,i6),a,f6.2,a)') "  istanti con dati:   ",cnt_istok_out, &
  " su ",ndays*24," (",REAL(cnt_istok_out)/REAL(ndays*24)*100.," %)"
WRITE (*,'(2(a,i6))')          "  parametri con dati: ",COUNT(par_ok(1:npar)), &
  " su ",npar
WRITE (*,'(a,2(i6,a))') "  saltati ",cnt_date_skip," istanti, ", &
  cnt_msg_skip," messaggi"
IF (cnt_diff > 0) WRITE (*,'(2(a,i5))') &
  "Dati diversi relativi allo stesso istante: dati ",cnt_diff, &
  " istanti ",cnt_ist_diff

! Chiudo files
CLOSE(iu_in)
CLOSE(iu_out)
CLOSE(iu_ana)
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
WRITE (*,*) "Errore: ",TRIM(filein)," non contiene nessuna data"
STOP 3

9993 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein),": record illegale"
WRITE (*,'(a)') TRIM(chrec)
STOP 3

9992 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein),": date non sequenziali"
CALL getval(datah_next, SIMPLEDATE=ch12a)
CALL getval(datah_dum, SIMPLEDATE=ch12b)
WRITE (*,'(a,1x,a)') ch12a,ch12b
STOP 3

9991 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein),": date non sequenziali"
CALL getval(datah_req, SIMPLEDATE=ch12a)
CALL getval(datah_dum, SIMPLEDATE=ch12b)
WRITE (*,'(a,1x,a)') ch12a,ch12b
STOP 3

9990 CONTINUE
WRITE (*,*) "Erroraccio gestione date (chiamare l'assistenza)"
STOP 4

9989 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein),": date intercalate ai parametri"
CALL getval(datah_req, SIMPLEDATE=ch12a)
WRITE (*,'(a)') ch12a
STOP 3

END PROGRAM bufr_csv2orari

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_msg(iu,iu_log,ldeb,eof,datah_req,npar,req_par,inp_type, &
  values,datah_next,anag_dum,ndiff,ier)

! Legge dall'unita' iu (gia aperta) i dati relativi a un'istante temporale
! (uno o piu' messaggi). Ritorna i valori relativi ai parametri richiesti 
! e la data successiva presente nel file.
!
! NOTE: 
! - La subr. presuppone che le chiavi dei BUFR siano sempre ordinate per 
!   gruppi: prima quelle relative alla data, poi quelle relative a 
!   var-liv-anag, e da ultimo il valore della varibile fisica e le 
!   (eventuali) flag di qualita'.
!
! - Di norma, i BUFR proveninti da dati ad altra frequenza, contengono un 
!   messaggio separato per ogni parametro, mentre i BUFR Synop radunano 
!   tutti i parametri in un unico messaggio.
!
! - Spesso nei dati Synop sono presenti due messaggi consecutivi che 
!   differiscono solo per "centre" e "master table": in questo caso prendo
!   tutti i dati disponibili, verificando che i messaggi non contengano
!   valori diversi.
!
! - Per i dati Synop, attualmente livello e timerange richiesti sono 
!   sempre mancanti (vedi estra_oss.sh), per cui i dati sono selezionati solo
!   in base alla varibile fisica (Bcode). Se si volessero implementare i
!   controlli (costruendo un file param_synop.csv con lo stesso tracciato di
!   param_arkioss.csv), considerare che:
!   * il file deve essere prodotto a mano (i synop vengono estratti solo 
!     come messaggio completo)
!   * nei synop, il timerange e' dato dalla B04024 (time significance), ed
!     e' implicitamente istantaneo se la chiave non e' presente
!   * analogamente, il livello e' e' dato dalla B07032, ed e' implicitamete
!     "superficie" se la chiave non e' presente.
!
! Codici d'errore:
! 1: raggiunta la fine di filein (values definiti, datah_next mancante)
! 2: errore di lettura in filein (fatal)
! 3: date non sequenziali in filein (fatal)
! 4: intreccio di date e valori in filein (fatal)

USE datetime_class
USE missing_values
USE local

IMPLICIT NONE

! Parametri della subroutine
TYPE (datetime), INTENT(IN) :: datah_req
TYPE (metpar), INTENT(IN) :: req_par(npar)
INTEGER, INTENT(IN) :: iu,iu_log,eof,npar
LOGICAL, INTENT(IN) :: ldeb
CHARACTER (LEN=3), INTENT(IN) :: inp_type

TYPE (anag), INTENT(OUT) :: anag_dum
TYPE (datetime), INTENT(OUT) :: datah_next
REAL, INTENT(OUT) :: values(npar)
INTEGER, INTENT(OUT) :: ndiff,ier

! Variabili locali
TYPE (datetime) :: datah_dum
TYPE (metpar) :: par_dum
REAL :: val_dum
INTEGER :: ios,iosr,k,kp,yy,mm,dd,hh,mn,pp,kp_qc_pending
INTEGER :: cnt_null,nok,ndble,nqcmiss,nmsg,qcf
CHARACTER (LEN=200) :: chrec,key
CHARACTER (LEN=12) :: ch12
CHARACTER (LEN=4) :: kty

!--------------------------------------------------------------------------

datah_dum = datah_req
values(:) = rmiss
par_dum = metpar(0,imiss,imiss,imiss,imiss,imiss,"","","","")
val_dum = rmiss
yy = imiss
mm = imiss
dd = imiss
hh = imiss
mn = imiss
qcf = imiss
kp_qc_pending = imiss

IF (inp_type == "syn") THEN
  anag_dum = anag(rmiss,rmiss,rmiss,"synop","")
ELSE
  anag_dum = anag(rmiss,rmiss,rmiss,"","")
ENDIF

nok = 0
ndble = 0
ndiff = 0
nmsg = 0
cnt_null = 0
nqcmiss = 0

DO k = 1,HUGE(0)

!--------------------------------------------------------------------------
! 1) Leggo il prossimo record

  READ (iu,'(a)',IOSTAT=iosr) chrec

! Fine file: salvo l'ultimo valore letto e mando il segnale di fine programma
  IF (iosr == eof) THEN
    DO kp = 1,npar
      IF (metpar_equal(par_dum,req_par(kp))) THEN
        IF (values(kp) == rmiss) THEN
          values(kp) = val_dum
          nok = nok + 1
        ELSE IF (values(kp) == val_dum) THEN
          ndble = ndble + 1
        ELSE
          ndiff = ndiff + 1
          IF (ldeb) THEN
            CALL getval(datah_req, SIMPLEDATE=ch12)
            WRITE (*,'(a10,2(2x,f10.3),i3,1x,a)') &
              ch12,values(kp),val_dum,kp,TRIM(req_par(kp)%short_name)
          ENDIF
        ENDIF

        EXIT
      ENDIF
    ENDDO

    IF (ldeb) THEN
      IF (ANY(par_dum%bcode==req_par(1:npar)%bcode)) &
        CALL write_log(iu_log,npar,par_dum,val_dum,req_par(1:npar))
      CALL getval(datah_req, SIMPLEDATE=ch12)
      WRITE (iu_log,'(2a,5(1x,a,1x,i2))') "Terminata lettura istante ",ch12, &
        " messaggi",nmsg,"dati validi",nok,"doppi",ndble,"inconsistenti",ndiff, &
        " mancanti per qc",nqcmiss
     ENDIF

    datah_next = datetime_miss
    ier = 1
    RETURN

  ELSE IF (iosr /= 0) THEN
    GOTO 9999

  ENDIF

!--------------------------------------------------------------------------
! 2) Interpreto la key appena letta

  pp = INDEX(chrec,",")
  IF (pp == 0) CYCLE
  key = chrec(1:pp-1)
  kty = "othe"
  ios = 0

! 2.0 "key" a cui non e' associato alcun valore
  IF (pp == LEN(TRIM(chrec))) THEN
    kty = "null"

! 2.1 Inizio di un nuovo messaggio
  ELSE IF (key == "edition") THEN
    kty = "head"

! 2.2 "key" fa parte della codifica della data
  ELSE IF (key == "B04001") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) yy
    kty = "date"
  ELSE IF (key == "B04002") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) mm
    kty = "date"
  ELSE IF (key == "B04003") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) dd
    kty = "date"
  ELSE IF (key == "B04004") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) hh
    kty = "date"
  ELSE IF (key == "B04005") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) mn
    kty = "date"

! 2.3 "key" contiene informazioni di anagrafica
  ELSE IF (key == "B05001") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) anag_dum%lat
    kty = "anag"
  ELSE IF (key == "B06001") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) anag_dum%lon
    kty = "anag"
  ELSE IF (key == "B07030") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) anag_dum%quo
    kty = "anag"
  ELSE IF (key == "B01019" .OR. key == "B01015") THEN
    READ (chrec(pp+1:),'(a)',IOSTAT=ios) anag_dum%nome
    kty = "anag"
  ELSE IF (key == "B01194") THEN
    READ (chrec(pp+1:),'(a)',IOSTAT=ios) anag_dum%rete
    kty = "anag"

! 2.4 "key" fa parte della codifica di par/liv
  ELSE IF (key == "B07192") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%lt1
    kty = "varl"
  ELSE IF (key == "B07193") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%l1
    kty = "varl"
  ELSE IF (key == "B04192") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%tr
    kty = "varl"
  ELSE IF (key == "B04193") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%p1
    kty = "varl"
  ELSE IF (key == "B04194") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) par_dum%p2
    kty = "varl"

! 2.5 "key" contiene informazioni sul controllo di qualita'
!     NB chiavi di futura implementazione: 33192, 33193, 33194, 33197
  ELSE IF (key == "B33196") THEN
    READ (chrec(pp+1:),*,IOSTAT=ios) qcf
    kty = "qflg"

! 2.6 "key" corrisponde a una delle variabili fisiche richieste in output
  ELSE
    DO kp = 1, npar
      IF (key == req_par(kp)%bcode) THEN
        READ (chrec(pp+1:),*,IOSTAT=ios) val_dum
        par_dum%bcode = key(1:6)
        kty = "valu"
        EXIT
      ENDIF
    ENDDO
  ENDIF

  IF (ios /= 0) GOTO 9999

!--------------------------------------------------------------------------
! 3) Elaborazioni e controlli

! Iniziato un nuovo messaggio
  IF (kty == "head") THEN 
    datah_dum = datetime_miss
    yy = imiss
    mm = imiss
    dd = imiss
    hh = imiss
    mn = imiss
    qcf = imiss
    kp_qc_pending = imiss

! Trovata chiave relativa ai dati mentre la data e' indefinita
  IF (datah_dum == datetime_miss .AND. &
    (kty=="varl" .OR. kty=="valu" .OR. kty=="anag" .OR. kty=="valu")) GOTO 9998

! Completata la lettura della data
  ELSE IF (kty == "date" .AND. &
           c_e(yy).AND.c_e(mm).AND.c_e(dd).AND.c_e(hh).AND.c_e(mn)) THEN
    datah_dum = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh, MINUTE=mn)
    nmsg = nmsg + 1
    yy = imiss
    mm = imiss
    dd = imiss
    hh = imiss
    mn = imiss

!   data precedente a quella richiesta: fatal error
    IF (datah_dum < datah_req) THEN
      datah_next = datah_dum
      ier = 3
      RETURN 

!   data successiva a quella richiesta: salvo l'ultimo valore letto e concludo
    ELSE IF (datah_dum > datah_req) THEN
      DO kp = 1,npar
        IF (metpar_equal(par_dum,req_par(kp))) THEN
          IF (values(kp) == rmiss) THEN
            values(kp) = val_dum
            nok = nok + 1
          ELSE IF (values(kp) == val_dum) THEN
            ndble = ndble + 1
          ELSE
            ndiff = ndiff + 1
            IF (ldeb) THEN
              CALL getval(datah_req, SIMPLEDATE=ch12)
              WRITE (*,'(a12,2(2x,f10.3),i3,1x,a)') &
                ch12,values(kp),val_dum,kp,TRIM(req_par(kp)%short_name)
            ENDIF
          ENDIF

          EXIT
        ENDIF
      ENDDO

      IF (ldeb) THEN
        IF (ANY(par_dum%bcode==req_par(1:npar)%bcode)) &
          CALL write_log(iu_log,npar,par_dum,val_dum,req_par(1:npar))
        CALL getval(datah_req, SIMPLEDATE=ch12)
        WRITE (iu_log,'(2a,5(1x,a,1x,i2))') "Terminata lettura istante ",ch12, &
          " messaggi",nmsg,"dati validi",nok,"doppi",ndble,"inconsistenti",ndiff, &
          " mancanti per qc",nqcmiss
      ENDIF

      datah_next = datah_dum
      ier = 0
      RETURN

!   data uguale a quella richiesta, ie. ci sono messaggi consecutivi con la
!   stassa data: proseguo senza colpo ferire
    ELSE
      CONTINUE

    ENDIF

! Se e' uno dei parametri richiesti, lo salvo
  ELSE IF (kty == "valu") THEN
    kp_qc_pending = imiss
    DO kp = 1,npar
      IF (metpar_equal(par_dum,req_par(kp))) THEN
        IF (values(kp) == rmiss) THEN
          values(kp) = val_dum
          nok = nok + 1
          kp_qc_pending = kp
        ELSE IF (values(kp) == val_dum) THEN
          ndble = ndble + 1
        ELSE
          ndiff = ndiff + 1
          IF (ldeb) THEN
            CALL getval(datah_req, SIMPLEDATE=ch12)
            WRITE (*,'(a12,2(2x,f10.3),i3,1x,a)') &
              ch12,values(kp),val_dum,kp,TRIM(req_par(kp)%short_name)
          ENDIF
        ENDIF

        EXIT

      ENDIF
    ENDDO

    IF (ldeb) CALL write_log(iu_log,npar,par_dum,val_dum,req_par(1:npar))
    par_dum = metpar(0,imiss,imiss,imiss,imiss,imiss,"","","","")
    val_dum = rmiss

! Se e' una flag di qualita', annullo l'ultimo dato letto
  ELSE IF (kty == "qflg") THEN
    IF (qcf == 1 .AND. c_e(kp_qc_pending)) THEN
      IF (kp_qc_pending < 1 .OR. kp_qc_pending > npar) GOTO 9997
      IF (ldeb) THEN
        CALL getval(datah_dum, SIMPLEDATE=ch12)
        WRITE (iu_log,'(4a,f10.1,3a)') "### Dato messo mancante: ",ch12," ", &
          req_par(kp_qc_pending)%bcode, values(kp_qc_pending), &
          " (",TRIM(req_par(kp_qc_pending)%short_name),")"
      ENDIF

      values(kp_qc_pending) = rmiss
      nqcmiss = nqcmiss + 1
      kp_qc_pending = imiss
      qcf = imiss
    ENDIF

! Altrimenti proseguo senza colpo ferire  
  ELSE IF (kty=="anag" .OR. kty=="varl" .OR. kty=="othe" .OR. kty=="null") THEN
    CONTINUE

  ENDIF

ENDDO

!--------------------------------------------------------------------------

WRITE (*,*) "Non dovrei mai passare di qui!!!"
STOP 100

9999 CONTINUE
WRITE (*,*) TRIM(chrec)
ier = 2
RETURN

9998 CONTINUE
WRITE (*,*) TRIM(chrec)
ier = 4
RETURN

9997 CONTINUE
WRITE (*,*) "Errore gestione qc ",kp_qc_pending
RETURN

END SUBROUTINE get_msg

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE skip_input_time(iu,eof,datah_in,datah_out,ier)
!
! Scorre il file aperto sull'unita' "iu" fino al prossimo messaggio
! Necessaria per gestire files di input con frequenza piu' che oraria.
!
! datah_ini: ultima data letta prima della chiamata a questa subr.
! datah_out: data letta dalla subr. (messaggio successivo a datah_ini)
!
! Codici d'errore:
! 1: raggiunta la fine di filein (datah_next mancante)
! 2: errore di lettura in filein (fatal)
! 3: date non sequenziali in filein (fatal)
! 4: intreccio di date e valori in filein (fatal)

USE datetime_class
USE missing_values

IMPLICIT NONE

! Parametri della subroutine
INTEGER, INTENT(IN) :: iu,eof
TYPE (datetime), INTENT(IN) :: datah_in

TYPE (datetime), INTENT(OUT) :: datah_out
INTEGER, INTENT(OUT) :: ier

! Variabili locali
INTEGER :: k,pp,yy,mm,dd,hh,mn,ios
CHARACTER (LEN=200) :: chrec

!-------------------------------------------------------------------------

yy = imiss
mm = imiss
dd = imiss
hh = imiss
mn = imiss

DO k = 1,HUGE(0)

  READ (iu,'(a)',IOSTAT=ios) chrec

  IF (ios == eof) THEN
    datah_out = datetime_miss
    ier = 1
    RETURN

  ELSE IF (ios /= 0) THEN
    GOTO 9999

  ELSE
    pp = INDEX(chrec,",")
    IF (pp == 0) CYCLE
    ios = 0

    IF (chrec(1:pp-1) == "B04001") THEN
      READ (chrec(pp+1:),*,IOSTAT=ios) yy
    ELSE IF (chrec(1:pp-1) == "B04002") THEN
      READ (chrec(pp+1:),*,IOSTAT=ios) mm
    ELSE IF (chrec(1:pp-1) == "B04003") THEN
      READ (chrec(pp+1:),*,IOSTAT=ios) dd
    ELSE IF (chrec(1:pp-1) == "B04004") THEN
      READ (chrec(pp+1:),*,IOSTAT=ios) hh
    ELSE IF (chrec(1:pp-1) == "B04005") THEN
      READ (chrec(pp+1:),*,IOSTAT=ios) mn
    ENDIF

    IF (ios /= 0) GOTO 9999
    IF (c_e(yy) .AND. c_e(mm) .AND. c_e(dd) .AND. c_e(hh) .AND. c_e(mn)) THEN
      datah_out =  datetime_new(YEAR=yy,MONTH=mm,DAY=dd,HOUR=hh,MINUTE=mn)
      IF (datah_out < datah_in) THEN
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

SUBROUTINE write_log(iu,npar,par_dum,val_dum,req_par)
!-------------------------------------------------------------------------
! Scrive su iu le informazioni relative all'ultimo dato letto. Viene
! chiamata ogni volta che viene trovata una chiave corrispondente a una
! variabile fisica richiesta.
! 
! NOTE:
! - E' impossibile chiamare questa routine alla fine di ogni messaggio: nel
!   caso dei dati Synop, il messaggio contiene molti valori, e le 
!   informazioni relative ai ver-liv non sono conservate
! - E' necessario passare a questa routine il vettore dei dati richiesti e 
!   ripetere i controlli fatti nel main program, per poter distinguere i 
!   casi in cui la varibiale fisica e' giusta, ma ha var-liv diversi da 
!   quelli richiesti.
!-------------------------------------------------------------------------

USE local
IMPLICIT NONE

TYPE (metpar), INTENT(IN) :: req_par(npar),par_dum
REAL, INTENT(IN) :: val_dum
INTEGER, INTENT(IN) :: iu,npar

INTEGER :: kp,idp
CHARACTER(LEN=200) :: chrec

1000 FORMAT(a35,1x,a6,5(1x,i5),1x,f10.1,1x,a)

!-------------------------------------------------------------------------

DO kp = 1,npar
  IF (metpar_equal(par_dum,req_par(kp))) THEN
    WRITE (iu,1000) "dato ok                           ", &
      par_dum%bcode,par_dum%l1,par_dum%lt1,par_dum%p1,par_dum%p2, &
      par_dum%tr,val_dum,TRIM(req_par(kp)%short_name)
    RETURN

  ELSE IF (par_dum%bcode==req_par(kp)%bcode) THEN
    WRITE (iu,1000) "dato con liv-trange non richiesto ", &
      par_dum%bcode,par_dum%l1,par_dum%lt1,par_dum%p1,par_dum%p2, &
      par_dum%tr,val_dum,""

  ENDIF  
ENDDO

RETURN
END SUBROUTINE write_log

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
WRITE (*,*) "idsta:   codice stazione (per header e nome file output; char, len<=20)"
WRITE (*,*) "-ndec N: forza a N il numero di decimali in output (-1 per notazione exp)"
WRITE (*,*) "-uv      scrive le temperature in gradi centigradi (def: K)"
WRITE (*,*) "-tc      scrive le temperature in gradi centigradi (def: K)"
WRITE (*,*) "-phpa    scrive le presisoni in hPa (def: Pa)"
WRITE (*,*) "-deb:    scrive su file .log la sequenza dei dati trovati in input"
WRITE (*,*) "-h:      visualizza questo help"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

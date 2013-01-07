PROGRAM orari_daily_stat
!--------------------------------------------------------------------------
! Dato un file in formato seriet o estra_orari, conta quante volte un dato 
! orario o una statistica giornaliera soddisfano una condizione specificata.
! Scrive output a schermo
! 
!                                                   V2.0, Enrico 17/01/2007
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

!--------------------------------------------------------------------------
! 0) Dichiarazioni - costanti

REAL, PARAMETER :: rmis_hhr = -9999. ! dato mancante, files estra_orari
REAL, PARAMETER :: rmis_ser = -9999. ! dato mancante, files trasp_seriet
INTEGER, PARAMETER :: fw = 10        ! ampiezza dei campi formato seriet
LOGICAL, PARAMETER :: ldeb = .TRUE.  ! Attivo stampe per debug (su fort.90)

TYPE(date) :: data_dum,datac
REAL :: soglia,rval,rmis,cum_val,calc_val,val24(0:23)
INTEGER :: campo,nhead,cnt_ok,cnt_ver,nv_cum,ndeb
INTEGER :: karg,id_par,eof,eor,ios,k,hh_dum,kh
CHARACTER (LEN=80) :: filein,ch80,label,chfmt
CHARACTER (LEN=fw) :: chval,idpar
CHARACTER (LEN=4) :: oper
CHARACTER (LEN=3) :: inp_data
CHARACTER (LEN=2) :: cond
LOGICAL :: leof,new_int

!--------------------------------------------------------------------------
! 1)  Preliminari

! Parametri da riga comandi
inp_data = "hhr"
id_par = 1

DO karg = 1,HUGE(0)
  CALL getarg(karg,ch80)
  IF (TRIM(ch80) == "") THEN  
    EXIT

  ELSE IF (TRIM(ch80) == "-h") THEN
    CALL write_help
    STOP

  ELSE IF (TRIM(ch80) == "-s") THEN
    inp_data = "ser"

  ELSE IF (id_par == 1) THEN
    filein = ch80
    id_par = id_par + 1

  ELSE IF (id_par == 2) THEN
    READ (ch80,*,IOSTAT=ios) campo
    IF (ios /= 0) THEN
      CALL write_help
      STOP
    ENDIF 
    id_par = id_par + 1

  ELSE IF (id_par == 3) THEN
    oper = ch80(1:4)
    id_par = id_par + 1

  ELSE IF (id_par == 4) THEN
    cond = ch80(1:2)
    id_par = id_par + 1

  ELSE IF (id_par == 5) THEN
    READ (ch80,*,IOSTAT=ios) soglia
    IF (ios /= 0) THEN
      CALL write_help
      STOP
    ENDIF 
    id_par = id_par + 1
  ENDIF
ENDDO

IF (id_par < 6 .OR. TRIM(filein) == "" .OR. campo < 1 .OR. &
    (oper/="hval" .AND. oper/="dyav" .AND. oper/="8hav" .AND. oper/="dymx") .OR. &
    (cond/="gt" .AND. cond/="ge" .AND. cond/="lt" .AND. & 
     cond/="le" .AND. cond/="eq" .AND. cond/="ne")) THEN
  CALL write_help
  STOP
ENDIF

! Valore per dati mancanti
IF (inp_data == "hhr") THEN
  rmis = rmis_hhr
ELSE IF (inp_data == "ser") THEN
  rmis = rmis_ser
ENDIF

! Costruisco formato input
IF (inp_data == "hhr") THEN
  WRITE (chfmt,'(a,i3,a,i2,a)') "(i4,3i3,",(campo-1)*(fw+1)+1,"x,a",fw,")"
ELSE IF (inp_data == "ser") THEN
  WRITE (chfmt,'(a,i3,a,i2,a)') &
    "(i2,1x,i2,1x,i4,i3,4x",(campo-1)*(fw+1)+1,"x,a",fw,")"
ENDIF

! Apro file input
OPEN (UNIT=20, FILE=filein, STATUS= "OLD", ACTION="READ", ERR=9999)

! Leggo header
IF (inp_data == "hhr") THEN
  READ (20,'(a)',ERR=9998) ch80
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  label = ADJUSTL(ch80)
  nhead = 3

ELSE IF (inp_data == "ser") THEN
  READ (20,'(a)',ERR=9998) ch80
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  READ (20,*,ERR=9998)
  label = ADJUSTL(ch80(7:))
  nhead = 6

ENDIF

! Codice EOF
CALL get_eof_eor(eof, eor)
IF (ldeb) WRITE (90,'(a)'),chfmt

!--------------------------------------------------------------------------
! 2) Lettura e calcolo condizione

! Inizializzazioni
datac = date(0,0,0)
cnt_ok = 0
cnt_ver = 0
leof = .FALSE.

DO k = 1,HUGE(0)

! Leggo un record
  IF (inp_data == "hhr") THEN
    READ (20,chfmt,IOSTAT=ios) data_dum%yy,data_dum%mm,data_dum%dd,hh_dum,chval
    IF (ios == eof) leof = .TRUE.
    IF (.NOT. leof .AND. ios /= 0) GOTO 9998

  ELSE IF (inp_data == "ser") THEN
    READ (20,chfmt,IOSTAT=ios) data_dum%dd,data_dum%mm,data_dum%yy,hh_dum,chval
    IF (ios == eof) leof = .TRUE.
    IF (.NOT. leof .AND. ios /= 0) GOTO 9998
  ENDIF

  IF (.NOT. leof) THEN
    READ (chval,*,IOSTAT=ios) rval
    IF (ios /= 0) GOTO 9998

  IF (ldeb) WRITE (90,'(a,i4.4,3i2.2,2x,2x,a,3x,a,e16.3,1x,e16.3)') &
    "Letta: ",data_dum%yy,data_dum%mm,data_dum%dd,hh_dum,chval, &
    "Rval, cum_val: ",rval,cum_val

  ENDIF

! Verifico se sono arrivato al termine di un intervallo di media
  IF (leof) THEN
    new_int = .TRUE.
  ELSE
    new_int = .FALSE.
    SELECT CASE (oper)
    CASE ("dyav","8hav","dymx")
      IF (data_dum /= datac) new_int = .TRUE.
    CASE ("hval")
      new_int = .TRUE.
    END SELECT
  ENDIF
  IF (new_int) datac = data_dum

! Se non ho terminato l'intervallo di media, incremento il valore progressivo
! e passo direttamente a leggere il prossimo record
  IF (.NOT. new_int) THEN

    SELECT CASE(oper)
    CASE ("dyav")
      IF (rval /= rmis) THEN
        cum_val = cum_val + rval
        nv_cum = nv_cum + 1
      ENDIF
    CASE ("dymx")
      IF (rval /= rmis) THEN
        cum_val = MAX(cum_val,rval)
        nv_cum = nv_cum + 1
      ENDIF
    CASE ("8hav")
      val24(hh_dum) = rval
    END SELECT

    CYCLE

! Ho terminato un intervallo di media:
! - se ho letto il primo record, mi limito a inizializzare i contatori
! - altrimenti: calcolo la media, ri-inizializzo i contatori con il valore 
!   appena letto, verifico se l'intervallo completato rispetta la condizione.
!
! rval:     valore appena letto
! cum_val:  valore progressivo relativo all'intevallo di media in corso
! calc_val: valore da confrontare con la soglia, relativo all'intervallo di
!           media (o alla scadenza oraria) gia' conclusa (i.e. quando lo 
!           definisco ho gia' letto un record successivo)
!
! NB: la condizione vine controllata sempre dopo la lettura del record successivo
!     alla fine dell'intervallo di media.

  ELSE

!   Calcolo la media
    SELECT CASE(oper)
    CASE ("dyav")
      IF (k == 1) THEN
      ELSE IF (nv_cum > 0) THEN
        calc_val = cum_val / REAL(nv_cum)
      ELSE
        calc_val = rmis
      ENDIF

    CASE ("dymx")
      IF (k == 1) THEN
      ELSE IF (nv_cum > 0) THEN
        calc_val = cum_val
      ELSE
        calc_val = rmis
      ENDIF

    CASE ("hval")
      IF (k == 1) THEN
      ELSE 
        calc_val = cum_val
      ENDIF

    CASE ("8hav")
      IF (k == 1) THEN
      ELSE 
        calc_val = -999.
        DO kh = 0,16
          IF (ANY(val24(kh:kh+7) == rmis)) THEN
          ELSE
            calc_val = MAX( calc_val, SUM(val24(kh:kh+7))/8. )
          ENDIF
        ENDDO
        IF (calc_val == -999.) calc_val = rmis
      ENDIF

    END SELECT

!   (Ri-)inizializzo i contatori
    IF (.NOT. leof) THEN
      SELECT CASE(oper)
      CASE ("dyav")
        IF (rval /= rmis) THEN
          cum_val = rval
          nv_cum = 1
        ELSE
          cum_val = 0.
          nv_cum = 0
        ENDIF
  
      CASE ("dymx")
        IF (rval /= rmis) THEN
          cum_val = rval
          nv_cum = 1
        ELSE
          cum_val = -HUGE(0.)
          nv_cum = 0
        ENDIF

      CASE ("hval")
        cum_val = rval

      CASE ("8hav")
        val24(0:23) = rmis
  
      END SELECT
    ENDIF

!   Verifico la condizione
    IF (k > 1 .AND. calc_val /= rmis) THEN
      ndeb = cnt_ver
      cnt_ok = cnt_ok + 1
      SELECT CASE(cond)
      CASE("gt")      
        IF (calc_val > soglia) cnt_ver = cnt_ver + 1
      CASE("ge")      
        IF (calc_val >= soglia) cnt_ver = cnt_ver + 1
      CASE("lt")      
        IF (calc_val < soglia) cnt_ver = cnt_ver + 1
      CASE("le")      
        IF (calc_val >= soglia) cnt_ver = cnt_ver + 1
      CASE("eq")      
        IF (calc_val == soglia) cnt_ver = cnt_ver + 1
      CASE("ne")      
        IF (calc_val /= soglia) cnt_ver = cnt_ver + 1
      END SELECT

      IF (ldeb) THEN
        IF (cnt_ver > ndeb) THEN
          WRITE (90,'(a,e13.6)') "++ Cond. verificata ",calc_val
        ELSE
          WRITE (90,'(a,e13.6)') "-- Cond. non verif. ",calc_val
        ENDIF  
      ENDIF
    ENDIF

  ENDIF

  IF (leof) EXIT

ENDDO

!--------------------------------------------------------------------------
! 3) Ouptut e conclusione

WRITE (*,'(a)') TRIM(label)
WRITE (*,'(a)') TRIM(idpar)
WRITE (*,'(a2,1x,e12.5)') cond,soglia
WRITE (ch80,*) cnt_ok
WRITE (*,'(a)') TRIM(ADJUSTL(ch80))
WRITE (ch80,*) cnt_ver
WRITE (*,'(a)') TRIM(ADJUSTL(ch80))

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," riga ",k+nhead
STOP

END PROGRAM orari_daily_stat

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

SUBROUTINE write_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE

WRITE (*,*) "orari_daily_stat.exe [-h] [-s] filein campo oper cond soglia"
WRITE (*,*) "Dato un file in formato seriet o estra_orari, conta quante volte"
WRITE (*,*) "  un dato orario o una statistica giornaliera soddisfano una"
WRITE (*,*) "  condizione specificata. Scrive output a schermo"
WRITE (*,*) ""
WRITE (*,*) "filein: file con i dati (default: in formato estra_orari)"
WRITE (*,*) "campo:  numero del campo da analizzare"
WRITE (*,*) "oper:   tipo di media (hval, dyav, 8hav, dymx)"
WRITE (*,*) "cond:   condizione da valutare (gt,lt,eq,ge,le,ne)"
WRITE (*,*) "soglia: Valore limite (realtivo alla condizione)"
WRITE (*,*) ""
WRITE (*,*) " -s:    input nel formato prodotto da trasp_seriet"
WRITE (*,*) " -h:    visualizza questo help"

RETURN

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

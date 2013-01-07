PROGRAM cong2
!--------------------------------------------------------------------------
! Legge un file con molti grib e riscrive solo quelli che soddisfano tutte
! le condizioni richieste dall'utente. Se richiesto, output in formato AFA.
!
! Opzioni gestite (completano le funzionalita' di cong)
! - selezione dei grib compresi entro due date
! - selezione di una lista di livelli
! - scrittura afa di grib con "Direction increments not given"
!
! Uso: cong2.exe [-h] filein fileout [-afa] [-nbit N] [-novcp]
!                [-t datah1 datah2] [-llist file] [-cnt N] 
!                [-isec n idx val] [-uv2h]
!
! Note:
! - Risulta impossibile scrivere in modo semplice i dati di un file AFA con
!   un formato intelligente: bisogna usare Fi.j (per gesite valori molto 
!   alti), e il calcolo dei decimali significativi in base a nbit va in 
!   crisi per campi come la pressione.
!
!                                           Versione 1.7, Enrico 10/04/2008
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.       ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000  ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxll = 100      ! max n.ro di livelli in lev list
INTEGER, PARAMETER :: maxfh = 10       ! max n.ro elementi header modificabili

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
TYPE(date) :: data1,data2,datac
REAL :: field2(maxdim),dx,dy
INTEGER :: iuin,iuout,kp,idp,hh1,hh2,hhc,lev(3,maxll)
INTEGER :: ios,ier,eof,eor,cnt_in,cnt_out,nll,kll,cnt_max,nbit,cnt_fnb
INTEGER :: ni,nj,i,j,k
INTEGER :: force_head,fh(3,maxfh),kfh
CHARACTER (LEN=80) :: filein,fileout,filell,chdum,arg(2)
CHARACTER (LEN=2) :: next_arg
LOGICAL :: afa,data_range,lev_list,lrq,cnt_limit,force_nbit,novcp,uv2h,encode

!--------------------------------------------------------------------------
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

! default
afa = .FALSE.
data_range = .FALSE.
lev_list = .FALSE.
cnt_limit = .FALSE.
novcp=.FALSE.
force_head = 0
uv2h=.FALSE.
arg(:) = ""

idp = 0
next_arg = ""
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)

  IF (chdum == "") THEN
    EXIT

  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP

  ELSE IF (next_arg == "t1") THEN
    READ (chdum,'(i4,3i2)',IOSTAT=ios) data1%yy,data1%mm,data1%dd,hh1
    IF (ios /= 0) THEN
      CALL write_help
      STOP
    ENDIF
    next_arg = "t2"
    CYCLE

  ELSE IF (next_arg == "t2") THEN
    READ (chdum,'(i4,3i2)',IOSTAT=ios) data2%yy,data2%mm,data2%dd,hh2
    IF (ios /= 0) THEN
      CALL write_help
      STOP
    ENDIF
    next_arg = ""
    CYCLE

  ELSE IF (next_arg == "ll") THEN
    filell = chdum    
    CYCLE

  ELSE IF (next_arg == "cn") THEN
    READ (chdum,*,IOSTAT=ios) cnt_max
    IF (ios /= 0) THEN
      CALL write_help
      STOP
    ENDIF
    next_arg = ""
    CYCLE

  ELSE IF (next_arg == "nb") THEN
    READ (chdum,*,IOSTAT=ios) nbit
    IF (ios /= 0 .OR. nbit < 1 .OR. nbit > 24) THEN
      CALL write_help
      STOP
    ENDIF
    next_arg = ""
    CYCLE

  ELSE IF (next_arg == "h1") THEN
    READ (chdum,*,IOSTAT=ios) fh(1,force_head)
    IF (ios /= 0 .OR. fh(1,force_head) < 1 .OR. fh(1,force_head) > 4) THEN
      CALL write_help
      STOP
    ENDIF
    next_arg = "h2"
    CYCLE

  ELSE IF (next_arg == "h2") THEN
    READ (chdum,*,IOSTAT=ios) fh(2,force_head)
    IF (ios /= 0 .OR. fh(2,force_head) < 1) THEN
      CALL write_help
      STOP
    ENDIF
    next_arg = "h3"
    CYCLE

  ELSE IF (next_arg == "h3") THEN
    READ (chdum,*,IOSTAT=ios) fh(3,force_head)
    IF (ios /= 0) THEN
      CALL write_help
      STOP
    ENDIF
    next_arg = ""
    CYCLE

  ELSE IF (TRIM(chdum) == "-t") THEN    
    data_range = .TRUE.
    next_arg = "t1"
    CYCLE

  ELSE IF (TRIM(chdum) == "-llist") THEN    
    lev_list = .TRUE.
    next_arg = "ll"
    CYCLE

  ELSE IF (TRIM(chdum) == "-afa") THEN    
    afa = .TRUE.
    CYCLE

  ELSE IF (TRIM(chdum) == "-novcp") THEN    
    novcp = .TRUE.
    CYCLE

 ELSE IF (TRIM(chdum) == "-uv2h") THEN    
    uv2h = .TRUE.
    CYCLE

  ELSE IF (TRIM(chdum) == "-nbit") THEN    
    force_nbit = .TRUE.
    next_arg = "nb"
    CYCLE

 ELSE IF (TRIM(chdum) == "-cnt") THEN    
    cnt_limit = .TRUE.
    next_arg = "cn"
    CYCLE

 ELSE IF (TRIM(chdum) == "-isec") THEN    
    force_head = force_head + 1
    IF (force_head > maxfh) GOTO 9997
    next_arg = "h1"
    CYCLE

  ENDIF

  idp = idp + 1
  arg(idp) = chdum
ENDDO

filein = arg(1)
fileout = arg(2)

IF (filein == "" .OR. fileout == "" .OR. next_arg /= "") THEN
  CALL write_help
  STOP
ENDIF

! 1.2 Se richiesta l'opzione -llist, leggo i codici dei livelli richiesti
IF (lev_list) THEN
  CALL get_eof_eor(eof,eor)
  OPEN (UNIT=40, FILE=filell, STATUS="OLD", FORM="FORMATTED",ERR=9999)
  DO kll = 1,maxll
    READ (40,*,IOSTAT=ios) lev(1:3,kll)
    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9998 
  ENDDO

  IF (kll > maxll) THEN
    READ (40,*,IOSTAT=ios)
    IF (ios /= eof) WRITE (*,*) &
      "Cong2: richiesti troppi livelli, considero i primi ",maxll
  ELSE IF (kll == 1) THEN
    WRITE (*,*) "Cong2: nessun  livello richiesto!"
  ENDIF

  nll = kll - 1
ENDIF

! 1.3 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.4 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
IF (kret /= 0) GOTO 9996

IF (afa) THEN
  OPEN (UNIT=20, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
ELSE
  CALL PBOPEN (iuout,fileout,'W',kret)
ENDIF

!--------------------------------------------------------------------------
! 2) Leggo / Scrivo (ciclo sui grib)

cnt_in = 0
cnt_out = 0
cnt_fnb = 0
DO

!--------------------------------------------------------------------------
! 2.1 Leggo il prossimo grib 
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
  cnt_in = cnt_in + 1

!--------------------------------------------------------------------------
! 2.2 Verifico se il grib letto rispetta le condizioni richieste dall'utente

! 2.2.1) Data/ora compresa tra estremi
  IF (data_range) THEN
    CALL ksec1_valid(ksec1,datac,hhc,ier)
    IF (datac < data1 .OR. (datac == data1 .AND. hhc < hh1)) CYCLE
    IF (datac > data2 .OR. (datac == data2 .AND. hhc > hh2)) CYCLE
  ENDIF

! 2.2.2) Livello incluso in lista
  IF (lev_list) THEN

    lrq = .FALSE.
    DO kll = 1,nll
    IF (ALL(ksec1(7:9) == lev(1:3,kll))) THEN
      lrq = .TRUE.
      EXIT
    ENDIF
    ENDDO

    IF (.NOT. lrq) CYCLE

  ENDIF

! 2.2.3) Ho superato il numero massimo di grib da esaminare
  IF (cnt_limit .AND. cnt_in > cnt_max) THEN
    WRITE (*,*) "N.ro di grib richiesti superato (il file ne contiene altri!)"
    EXIT
  ENDIF

!--------------------------------------------------------------------------
! 2.3 Se e' necessario, ricodifico il grib

  encode = .FALSE.

! 2.3.1 Se richiesto, interpolo il vento da griglia U/V a griglia H
  IF (uv2h) THEN
    IF ((ksec1(1)==2 .AND. ksec1(6)==33 .AND. ksec1(7)==110) .OR. &
        (ksec1(1)==2 .AND. ksec1(6)==124)) THEN
      encode = .TRUE.
      ni = ksec2(2)
      nj = ksec2(3)
      DO k = 1,ni*nj
        j = (k-1)/ni+1
        i = k - ni*(j-1)
        IF (i == 1) THEN                       ! 1a colonna: estrapolo
          field2(k) = 1.5*field(k) - 0.5*field(k+1)
        ELSE                                   ! altre colonne: interpolo
          field2(k) = 0.5*field(k) + 0.5*field(k-1)
        ENDIF
      ENDDO
      field(1:ni*nj) = field2(1:ni*nj)

    ELSE IF ((ksec1(1)==2 .AND. ksec1(6)==34 .AND. ksec1(7)==110) .OR. &
        (ksec1(1)==2 .AND. ksec1(6)==125)) THEN
      encode = .TRUE.
      ni = ksec2(2)
      nj = ksec2(3)
      DO k = 1,ni*nj
        j = (k-1)/ni+1
        i = k - ni*(j-1)
        IF (j == 1) THEN                       ! 1a riga: estrapolo
          field2(k) = 1.5*field(k) - 0.5*field(k+ni)
        ELSE                                   ! altre righe: interpolo
          field2(k) = 0.5*field(k) + 0.5*field(k-ni)
        ENDIF
      ENDDO
      field(1:ni*nj) = field2(1:ni*nj)

    ENDIF
  ENDIF

! 2.3.1 Se richiesto, azzero i Vertical Coordinate Parameters
  IF (novcp) THEN
    ksec2(12) = 0
    psec2(:) = 0.
    encode = .TRUE.
  ENDIF

! 2.3.2 Se e' un grib SOP, forzo nbit > 8
  IF (ksec4(4) == 64 .AND. ksec4(2) < 8) THEN
    ksec4(2) = 8
    encode = .TRUE.
    cnt_fnb = cnt_fnb + 1 
  ENDIF

! 2.3.3 Se richiesto, forzo il numero di bit
  IF (force_nbit) THEN
    ksec4(2) = nbit
    encode = .TRUE.
  ENDIF

! 2.3.4 Se richiesto, forzo alcuni valori degli header
  IF (force_head > 0) THEN
    encode = .TRUE.
    DO kfh = 1,force_head 
      SELECT CASE (fh(1,kfh))
      CASE (1)
        ksec1(fh(2,kfh)) = fh(3,kfh)
      CASE (2)
        ksec2(fh(2,kfh)) = fh(3,kfh)
      CASE (3)
        ksec3(fh(2,kfh)) = fh(3,kfh)
      CASE (4)
        ksec4(fh(2,kfh)) = fh(3,kfh)
      END SELECT
    ENDDO
  ENDIF

! Ricodifico
  IF (encode) THEN
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  ENDIF

!--------------------------------------------------------------------------
! 2.4 Scrivo il grib, ed eventualmente converto in AFA (per ora solo aree 
!     geo non ruotate)

  IF (afa) THEN

!   Dataset
    WRITE (20,'(a)') filein

!   Data
    WRITE (20,'(i4.4 ,4i3.2)') 100*(ksec1(21)-1)+ksec1(10),ksec1(11:14)

!   Scadenza
    WRITE (20,'(i3.3,i6.5,2i4.3)') ksec1(15:18)

!   Livello
    WRITE (20,'(i3.3,i6.5,i6.3)') ksec1(7:9)

!   Parametro
    IF (ksec1(2).ne.98.or.ksec1(24).eq.0) THEN
      WRITE (20,'(i3.3,5i4.3)') ksec1(2),ksec1(1),ksec1(6),0,0,0
    ELSE
      WRITE (20,'(i3.3,5i4.3)') &
        ksec1(2),ksec1(1),ksec1(6),ksec1(37),ksec1(39),ksec1(42)
    ENDIF

!   Griglia
    IF (ksec2(1) /= 0 .AND. ksec2(1) /= 10) THEN
      WRITE (*,*) "Tipo di griglia non gestito: ",ksec2(1)
      STOP
    ENDIF 

    WRITE (20,'(i3)') ksec2(1)

    IF (ksec2(1) == 10) THEN
      WRITE (20,'(3f9.3)') REAL(ksec2(13))/1000.+90., & 
        REAL(ksec2(14))/1000., 0.
    ENDIF

    IF (ksec2(6) == 128) THEN
      dx = REAL(ksec2(9))/1000.
      dy = REAL(ksec2(10))/1000.
    ELSE
      dx = REAL(ABS(ksec2(5)-ksec2(8))) / (1000. * REAL(ksec2(2)-1))
      dy = REAL(ABS(ksec2(4)-ksec2(7))) / (1000. * REAL(ksec2(3)-1))
    ENDIF

    WRITE (20,'(2f9.3,f10.5,i9)') REAL(ksec2(4))/1000., & 
      REAL(ksec2(7))/1000., dy, ksec2(3) 
    WRITE (20,'(2f9.3,f10.5,i9)') REAL(ksec2(5))/1000., & 
      REAL(ksec2(8))/1000., dx, ksec2(2)   

!   Altre info
    WRITE (20,'(3i8,1x,e12.3,1x,2i8)') &
      ksec4(1),ksec2(11),ksec4(5),-1.5e21,32767,0

!   Dati
    WRITE (20,*) field(1:ksec4(1))

    cnt_out = cnt_out + 1

  ELSE

    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
    cnt_out = cnt_out + 1

  ENDIF

ENDDO

!--------------------------------------------------------------------------
! 3) Chiudo i files e termino

CALL PBCLOSE (iuin,kret)
IF (afa) THEN
  CLOSE (20)
ELSE
  CALL PBCLOSE (iuout,kret)
ENDIF

WRITE (*,*) "Grib letti: ",cnt_in," scritti ",cnt_out
IF (cnt_fnb /= 0) WRITE (*,*) "nbit forzato a 8 in ",cnt_fnb," campi SOP"

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filell)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filell)
STOP

9997 CONTINUE
WRITE (*,*) "Richiesta la modifica di troppi elementi header. Max: ",maxfh
STOP

9996 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

END PROGRAM cong2

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE ksec1_valid(ksec1,data,hh,ier)
!--------------------------------------------------------------------------
! Data la sezione 1 di un GRIB, ritorna la data e l'ora di validita'
!                                             Versione 2, Enrico 10/07/2007
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
CASE(0,10)
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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a schermo l'hlep del programma
!
!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Legge un file con molti grib e riscrive solo quelli che soddisfano tutte"
WRITE (*,*) " le condizioni richieste dall'utente"
WRITE (*,*)
WRITE (*,*) "Uso: cong2.exe [-h] filein fileout [-afa] [-nbit N] [-novcp]"
WRITE (*,*) "               [-t datah1 datah2] [-llist filelst] [-cnt N]"
WRITE (*,*) "               [-isec n idx val] [-uv2h]"
WRITE (*,*)
WRITE (*,*) "  filein:  file di input (grib)"
WRITE (*,*) "  fileout: file di output (grib o afa)"
WRITE (*,*) "  -afa:    output in formato afa"
WRITE (*,*) "  -nbit N: output in formato grib con precisione di N bits"
WRITE (*,*) "  -novcp:  elimina i Vertical Coordinate Parametrers (ksec2(12) e psec2(:))"
WRITE (*,*) "  -t       scrive solo i campi con data di validita' >= datah1 e <= datah2"
WRITE (*,*) "           datah1 e datah2 nel formato YYYYMMDDHH"
WRITE (*,*) "  -llist   scrive soli i campi con livello compreso in un elenco. Questo"
WRITE (*,*) "           e' scritto in filelst come terna di interi (un liv per riga)"
WRITE (*,*) "  -cnt N:  esamina solo i primi N grib contenuti in filein"
WRITE (*,*) "  -isec N IDX VAL: forza a VAL l'elemento IDX della sezione N dell'header" 
WRITE (*,*) "  -uv2h    interpola su grilgia H i dati di vento LM sui model layers "
WRITE (*,*) "           estratti dall'archivio. La griglia del grib resta la stessa ->"
WRITE (*,*) "           prestare attenzione a passare il programma una volta sola!!"
WRITE (*,*)
RETURN
END SUBROUTINE write_help

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

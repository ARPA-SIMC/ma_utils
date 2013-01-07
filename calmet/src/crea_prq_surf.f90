PROGRAM crea_prq_surf
!--------------------------------------------------------------------------
! Programma di appoggio alla catena calmet.
! Costruisce un file prq_surf_PROJ.lst di prima scelta, relativo alle 
! stazioni contenute in un'anagrafica e in cui tutti i parametri 
! disponibili sarebbero estratti.
! Programma mutuato da crea_surf_req.f90 (V3), con piccole modifiche
!
! Uso (manuale)
! crea_prq_surf.exe [-h] filein fileout [-linux/-unix]
! -h      : visualizza help
! filein  : elenco stazioni nel formato della condivisione regionale: una
!           riga per stazione, primi campi id_rete id_user (i2,i5)
!           (srq_surf_PROJ.lst)
! fileout : tabella dei parametri richiesti da calmet presenti in archivio
!           per ciasuna delle stazioni richieste (prq_surf_PROJ.lst). Il
!           file dovra' poi essere modificato manualmente.
! -unix/-linux : fissa i path costanti per Ada (default) o Annapurna"
!
! Compilazione:
! f90 (non usa moduli ne' librerie)
!
! Note:
! Legge i codici Oracle dei parametri meteo (per ciascuna rete) da 
! code_parametri.dat. 
! Legge l'id Oracle delle stazioni da db_anagrafica.dat
! Legge le stringhe descrittive delle reti da db_reti.dat
!
! Tracciato fileout (prq_surf_PROJ.lst): 
! 1 record header, poi (i3,1x,a6,2(1x,i5),15(1x,i3));
! id_net,str_net,id_db,id_user,flag(15) (flag/=0 per i param. da estrarre)
! ordine parametri come in surf_req.dat: 
! vel dir khc kmc klc tcc lcc t2m td2 q2m prs tpr p12 p6h p1h
!
!                                                   V1.2, Enrico 11/05/2004 
!--------------------------------------------------------------------------
IMPLICIT NONE

! 0.1 Costanti e vettori di appoggio relativi al file "code_parametri.dat"
INTEGER, PARAMETER :: cp_nreti = 25   ! n.ro di reti gestite (colonne)
INTEGER, PARAMETER :: cp_npar  = 46   ! n.ro di parametri gestiti (righe)
INTEGER, PARAMETER :: imis_cp  = -99  ! valore per parametri non presenti

CHARACTER (LEN=40) :: cp_path
CHARACTER (LEN=40), PARAMETER :: cp_name = "code_parametri.dat"
INTEGER :: cp_par(cp_nreti,cp_npar),cp_id_rete(cp_nreti)

! 0.2 Costanti ecc. relativi al file "db_reti.dat"
INTEGER, PARAMETER :: dr_mxreti = 100 ! n.ro max di reti in archivio (righe)

CHARACTER (LEN=40) :: dr_path
CHARACTER (LEN=40), PARAMETER :: dr_name = "db_reti.dat"
INTEGER :: dr_id_rete(dr_mxreti)
CHARACTER (LEN=6) :: dr_str_rete(dr_mxreti)

! 0.3 Costanti ecc. relativi al file "db_anagrafica.dat"
CHARACTER (LEN=40) :: anag_path
CHARACTER (LEN=40), PARAMETER :: anag_name = "db_anagrafica.dat"

! 0.4 Gestione parametri e assegnazione path espliciti.
INTEGER, PARAMETER :: req_par = 2
INTEGER :: kpar,cnt_par
CHARACTER (LEN=100) :: chpar

! 0.5 Altre variabili del programma
INTEGER, ALLOCATABLE :: anag_net(:),anag_usr(:),anag_db(:)
INTEGER :: calmet_par(cp_nreti,15),calmet_flag(cp_nreti,15)
INTEGER :: id_net,id_usr,id_db,db_nreti,cp_prog
INTEGER :: ieof,ieor,k,kanag,ios,dk_prog,cnt_staz,idum(3)
INTEGER :: nanag,cnt_not_anag
CHARACTER (LEN=200) :: filein,fileout,chfmt_cp
CHARACTER (LEN=7) :: ch7
CHARACTER (LEN=6) :: str_rete

! 0.6 Variabili d'ambiente
CHARACTER (LEN=30) :: HOME_BONAFE
CALL GETENV('HOME_BONAFE',HOME_BONAFE)
cp_path = &
     TRIM(HOME_BONAFE)//"/osservazioni/dat/"
anag_path = &
     TRIM(HOME_BONAFE)//"/osservazioni/dat/"
dr_path = &
     TRIM(HOME_BONAFE)//"/osservazioni/dat/"

!--------------------------------------------------------------------------
! 1) Input: dati richiesti dall'utente e lettura da files costanti

! 1.1 parametri da riga comandi
cnt_par = 0
DO kpar = 1,HUGE(kpar)
  CALL getarg(kpar,chpar)

  SELECT CASE (TRIM(chpar))
  CASE ("")
    EXIT
  CASE ("-h")
  WRITE (*,*) "Uso: crea_prq_surf.exe [-h] [-unix/-linux] filein fileout"
    STOP
  CASE DEFAULT
    cnt_par = cnt_par + 1
    SELECT CASE(cnt_par)
    CASE(1)
      filein = TRIM(chpar)
    CASE(2)
      fileout = TRIM(chpar)
    END SELECT

  END SELECT
ENDDO
IF (cnt_par /= req_par) STOP "Errore nei parametri "

! 1.2 Leggo dal file code_parametri.dat l'elenco dei codici ORACLE dei vari 
!     parametri, per tutte le reti gestite

WRITE (chfmt_cp,'(a,i2,a)') "(36x,",cp_nreti,"i5)"
OPEN (UNIT=23, FILE=TRIM(cp_path)//TRIM(cp_name), STATUS= "OLD", &
  ACTION="READ", ERR=9999)
READ (23,*, ERR=9999)
READ (23,chfmt_cp, ERR=9999) cp_id_rete(1:cp_nreti)
READ (23,*, ERR=9999)
DO k = 1, cp_npar
  READ (23,chfmt_cp, ERR=9999) cp_par(1:cp_nreti,k)
ENDDO
CLOSE (23)

! 1.3 Leggo dal file db_reti.dat le stringhe descrittive delle reti
CALL get_eof_eor(ieof, ieor)

OPEN (UNIT=24, FILE=TRIM(dr_path)//TRIM(dr_name), STATUS= "OLD", &
  ACTION="READ", ERR=9998)
READ (24,*, ERR=9998)
READ (24,*, ERR=9998)
READ (24,*, ERR=9998)
DO k = 1,dr_mxreti
  READ (24,'(i4,1x,a6)', IOSTAT = ios) dr_id_rete(k),dr_str_rete(k) 
  IF (ios /= 0) EXIT
ENDDO

IF (k > dr_mxreti) THEN
  WRITE (*,*) "Warning: troppi dati nel file db_reti.dat, potrebbe mancare"
  WRITE (*,*) "  il nome di rete per alcune stazioni (aumentare dr_mxreti)"
  db_nreti = dr_mxreti
ELSE
  db_nreti = k
ENDIF

CLOSE (24)

! 1.4 Leggo l'anagrafica: la scorro una prima volta per vedere quanti
!     record contiene, poi alloco gli arrays e la rileggo

OPEN (UNIT=25, FILE=TRIM(anag_path)//TRIM(anag_name), STATUS= "OLD", &
  ACTION="READ", ERR=9996)
READ (25,*,ERR=9996)
READ (25,*,ERR=9996)
READ (25,*,ERR=9996)
DO kanag = 1,HUGE(kanag)
  READ (25,*,IOSTAT=ios) idum(1:3)
  IF (ios /= 0) EXIT
ENDDO
CLOSE (25)

nanag = kanag - 1
ALLOCATE (anag_net(nanag),anag_usr(nanag),anag_db(nanag))

OPEN (UNIT=25, FILE=TRIM(anag_path)//TRIM(anag_name), STATUS= "OLD", &
  ACTION="READ", ERR=9996)
READ (25,*,ERR=9996)
READ (25,*,ERR=9996)
READ (25,*,ERR=9996)
DO kanag = 1,nanag
  READ (25,*) anag_net(kanag),anag_usr(kanag),anag_db(kanag)
ENDDO
CLOSE (25)

!--------------------------------------------------------------------------
! 2) Per ciascuna rete, ordino i parametri come richiesto da 
!    estra_surf_calmet e seleziono quelli che sono disponibili in archivio

calmet_par(:,1:15) = cp_par(:,(/3,2,11,12,13,10,14,1,8,4,6,15,17,18,5/))

WHERE (calmet_par(:,:) == -99) 
  calmet_flag(:,:) = 0
ELSEWHERE
  calmet_flag(:,:) = 1
ENDWHERE

!--------------------------------------------------------------------------
! 3) Leggo l'elenco stazioni lo riscrivo aggiungendo le flag di presenza in
!    archivio dei parametri

! 3.1 Apro files e gestisco header
OPEN (UNIT=30, FILE=filein, STATUS= "OLD", ACTION="READ", ERR=9997)
READ (30,*,ERR=9997)

OPEN (UNIT=31, FILE=fileout, STATUS= "REPLACE", ACTION="WRITE")
WRITE (31,'(2a)') " nr rete   id_db id_us vel dir khc kmc klc tcc ", & 
                  "lcc t2m td2 q2m prs tpr p12 p6h p1h"
WRITE (31,*)

! 3.2 Ciclo sulle stazioni
cnt_staz = 0 
cnt_not_anag = 0 
DO
  READ (30,'(a7)',IOSTAT=ios) ch7
  IF (ios /= 0) EXIT
  
  READ (ch7,'(i2,i5)',IOSTAT=ios) id_net,id_usr
  IF (ios /= 0) THEN
    WRITE (*,*) "Codice rete/stazione illegale, skippo: ",ch7
    CYCLE
  ENDIF
  cnt_staz = cnt_staz + 1

! trovo il codice oracle della stazione
  DO kanag = 1, nanag
    IF (id_net == anag_net(kanag) .AND. id_usr == anag_usr(kanag)) THEN
      id_db = anag_db(kanag)
      EXIT
    ENDIF
  ENDDO

  IF (kanag > nanag) THEN
    WRITE (*,*) "Stazione non in anagrafica, skippo: ",id_net,id_usr
    cnt_not_anag = cnt_not_anag + 1
    CYCLE
  ENDIF

! trovo stringa identificativa rete
  IF (ANY( id_net == dr_id_rete(1:db_nreti) )) THEN
    DO k=1,db_nreti
      IF (id_net == dr_id_rete(k)) EXIT
    ENDDO
    str_rete = dr_str_rete(k)
  ELSE
    str_rete = ""
  ENDIF

! trovo n.ro progressivo della rete richiesta in code_parametri
  IF (ANY( id_net == cp_id_rete(1:cp_nreti) )) THEN
    DO k=1,cp_nreti
      IF (id_net == cp_id_rete(k)) EXIT
    ENDDO
    cp_prog = k
  ELSE
    WRITE (*,*) "Rete non gestita in code_parametri.dat, skippo ",id_net
    CYCLE
  ENDIF

! scrivo il record relativo alla stazione corrente. 
  WRITE (31,'(i3,1x,a6,2(1x,i5),15(3x,i1))') id_net,str_rete,id_db, &
    id_usr,calmet_flag(cp_prog,1:15)

ENDDO

CLOSE(30)
CLOSE(31)

WRITE (*,'(a,i5,a)') "crea_prq_surf: selezionate ",cnt_staz," stazioni"
IF (cnt_not_anag > 0) WRITE (*,*) & 
  "Trovate ",cnt_not_anag," stazioni fuori anagrafica"

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "Errore leggendo code_parametri.dat"
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo db_reti.dat"
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(anag_path)//TRIM(anag_name)
STOP

END PROGRAM crea_prq_surf

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

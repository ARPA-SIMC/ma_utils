PROGRAM sum_pm_species
!--------------------------------------------------------------------------
! Programma che legge un insieme di files con i campi delle singole specie 
! del PM (in ppb) e scrive un file che contiene la loro somma (in ppb o 
! ug/m3)
!
! Note:
! Derivato da somma_grib.f90; puo' essere usato anche per calcolare 
! combinazioni lineari di grib.
!
!                                           Versione 2.1, Enrico 10/11/2006
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
REAL, PARAMETER :: avgd = 6.02e23          ! Numero di Avogadro
REAL, PARAMETER :: mair = 28.96            ! Peso molecolare aria secca
INTEGER, PARAMETER :: maxdim = 100000      ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxfil = 100         ! n.ro massimo files input

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1a(1024),ksec2a(1024),ksec3a(2),ksec4a(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL :: psec2a(512),psec3a(2),psec2(512),psec3(2)
REAL :: field(maxdim)

! Altre variabili del programma
REAL :: fout(maxdim),coeff(maxfil)
REAL :: fk1,fk2,tt_ro
INTEGER :: iuin(maxfil),par_out(3),opt_ro,ngrib
INTEGER :: nfin,np,ios,ios0,ios1,ios2,ios3,eof,eor,kf,kg,kp,idp,iuout,iuro
CHARACTER (LEN=80) :: filein(maxfil),filelst,fileout,filero,chdum,next_arg
LOGICAL :: ld_par,ld_lev,ld_date,ld_sca,ld_ksec1,lpar_out

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Inizializzazioni
CALL grsvck(0)
CALL get_eof_eor(eof,eor) 
ld_par = .FALSE.
ld_lev = .FALSE.
ld_date = .FALSE.
ld_sca = .FALSE.
ld_ksec1 = .FALSE.

! 1.2 Parametri da riga comando
filelst=""
filero=""
lpar_out = .FALSE.
opt_ro = 0
next_arg = ""
idp = 1
DO kp = 1,HUGE(0)

  CALL getarg(kp,chdum)

  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "") THEN
    EXIT

  ELSE IF (next_arg == "rof") THEN
    filero = chdum
    next_arg = ""
  ELSE IF (next_arg == "rot") THEN
    READ (chdum,*,IOSTAT=ios0) tt_ro
    next_arg = ""
  ELSE IF (next_arg == "cem") THEN
    READ (chdum,*,IOSTAT=ios1) par_out(1)
    next_arg = "tab"
  ELSE IF (next_arg == "tab") THEN
    READ (chdum,*,IOSTAT=ios2) par_out(2)
    next_arg = "var"
  ELSE IF (next_arg == "var") THEN
    READ (chdum,*,IOSTAT=ios3) par_out(3)
    next_arg = ""

  ELSE IF (TRIM(chdum) == "-ro") THEN
    opt_ro = 1
    next_arg="rof"
  ELSE IF (TRIM(chdum) == "-rost") THEN
    opt_ro = 2
    next_arg="rot"
  ELSE IF (TRIM(chdum) == "-p") THEN
    lpar_out = .TRUE.
    next_arg="cem"

  ELSE IF (idp == 1) THEN
    idp = 2
    filelst = chdum
  ELSE IF (idp == 2) THEN
    idp = 3
    fileout = chdum
  ENDIF

ENDDO

IF (TRIM(filelst) == "" .OR. TRIM(fileout) == "" .OR. &
    (opt_ro == 1 .AND. TRIM(filero) == "") .OR. &
    (opt_ro == 2 .AND. (ios0 /= 0 .OR. tt_ro < 0 .OR. tt_ro > 20)) .OR. &
    (lpar_out .AND. (ios1 /= 0 .OR. ios2 /= 0 .OR. ios3 /= 0 .OR. & 
       ANY(par_out(1:3) < 1) .OR. ANY(par_out(1:3) > 255))) &
   ) THEN
  CALL write_help
  STOP
ENDIF

! 1.3 Leggo da namelist nomi files input e coefficenti
OPEN (UNIT=20, FILE=filelst, STATUS="OLD",FORM="FORMATTED", ERR=9999)
nfin = 0
DO WHILE (.TRUE.)
  READ (20,'(a)',IOSTAT=ios) chdum
  IF (ios == eof) EXIT
  IF (TRIM(chdum) == "" .OR. chdum(1:1) == "!") CYCLE
  nfin = nfin + 1
  IF (nfin > maxfil) GOTO 9997

  READ (chdum,*,IOSTAT=ios) filein(nfin),coeff(nfin)
  IF (ios /= 0) GOTO 9998
ENDDO
CLOSE(20)
WRITE (*,*) "Richiesta elaborazione di ",nfin," files"

! 1.4 Apro i files grib
DO kf = 1,nfin
  CALL PBOPEN (iuin(kf),filein(kf),'R',kret)
  IF (kret /= 0) GOTO 9996
ENDDO

IF (opt_ro == 1) THEN
  CALL PBOPEN (iuro,filero,'R',kret)
  IF (kret /= 0) GOTO 9995
ENDIF

CALL PBOPEN (iuout,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura

ngrib = 0

grib: DO kg = 1,HUGE(0)
  files: DO kf = 1,nfin

!   2.1) Leggo e decodifico il grib
    CALL PBGRIB(iuin(kf),kbuffer,maxdim*4,klen,kret)
    IF (kret.eq.-1) THEN 
      EXIT grib
    ELSE IF (kret < -1) THEN
      WRITE(*,*) "Error pbgrib: kret ",kret
      STOP
    ENDIF

    psec3(2) = rmis
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret > 0) WRITE(*,*) "Warning gribex: kret ",kret

!   2.2) Controllo la compatibilita' degli header
    IF (kf == 1) THEN     
      ksec1a(:) = ksec1(:)
      ksec2a(:) = ksec2(:)
      ksec3a(:) = ksec3(:)
      ksec4a(:) = ksec4(:)
      psec2a(:) = psec2(:)
      psec3a(:) = psec3(:)
    ELSE
      IF (ANY(ksec2(:) /= ksec2a(:)) .OR. (ksec4(1) /= ksec4a(1))) THEN
        GOTO 9993
      ELSE IF (ANY(ksec1(1:2) /= ksec1a(1:2)) .OR. &
               ksec1(6) /= ksec1a(6)) THEN
        ld_par = .TRUE.
      ELSE IF (ANY(ksec1(7:9) /= ksec1a(7:9))) THEN
        ld_lev = .TRUE.
      ELSE IF (ANY(ksec1(10:14) /= ksec1a(10:14))) THEN
        ld_date = .TRUE.
      ELSE IF (ANY(ksec1(15:18) /= ksec1a(15:18))) THEN
        ld_sca = .TRUE.
      ELSE IF (ANY(ksec1(3:5) /= ksec1a(3:5)) .OR. &
               ANY(ksec1(19:) /= ksec1a(19:))) THEN
        ld_ksec1 = .TRUE.
      ENDIF
    ENDIF

!   2.3) Incremento la sommatoria
    IF (kf == 1) THEN     
      np = ksec4a(1)
      fout(1:np) = 0.
    ENDIF

    WHERE (fout(1:np) /= rmis .AND. field(1:np) /= rmis)
      fout(1:np) = fout(1:np) + coeff(kf) * field(1:np)
    ELSEWHERE
      fout(1:np) = rmis
    ENDWHERE
    ngrib = ngrib + 1

  ENDDO files

! 2.4) Se richiesto, leggo (o calcolo) la densita' dell'aria (mle/cm3) e 
!      converto da ppb a ug/m3
!      
  IF (opt_ro == 1) THEN
    CALL PBGRIB(iuro,kbuffer,maxdim*4,klen,kret)
    IF (kret <= -1) GOTO 9994

    psec3(2) = rmis
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'D',kret)
    IF (kret > 0) WRITE(*,*) "Warning gribex: kret ",kret

    IF (ANY(ksec2(:) /= ksec2a(:)) .OR. (ksec4(1) /= ksec4a(1))) GOTO 9992
    IF (ksec1(6) /= 89 .OR. ksec1(1) /= 200) &
      WRITE (*,'(3a,3i4)') "Warning: ",TRIM(filero), &
      " contiene parametro sospetto: ",ksec1(2),ksec1(1),ksec1(6)

    WHERE (fout(1:np) /= rmis .AND. field(1:np) /= rmis)
      fout(1:np) = fout(1:np) * field(1:np) * 1.e3 / avgd
    ELSEWHERE
      fout(1:np) = rmis
    ENDWHERE
    ngrib = ngrib + 1

  ELSE IF (opt_ro == 2) THEN
    field(1:np) = 101325. * avgd / (287. * (tt_ro + 273.15) * mair * 1000.)
    WHERE (fout(1:np) /= rmis)
      fout(1:np) = fout(1:np) * field(1:np) * 1.e3 / avgd
    ELSEWHERE
      fout(1:np) = rmis
    ENDWHERE

  ENDIF

! 2.5) Scrivo il risultato
  ksec1(:) = ksec1a(:)
  IF (lpar_out) THEN 
    ksec1(1) = par_out(2)
    ksec1(2) = par_out(1)
    ksec1(6) = par_out(3)
  ENDIF

  ksec2(:) = ksec2a(:)
  psec2(:) = psec2a(:)
  ksec3(:) = ksec3a(:)
  psec3(2) = rmis
  ksec4(:) = ksec4a(:)

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               fout,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)

ENDDO grib

!--------------------------------------------------------------------------
! 3) Conclusione

IF (ld_par)   WRITE (*,*) "Ci sono parametri diversi"
IF (ld_lev)   WRITE (*,*) "Ci sono livelli diversi"
IF (ld_date)  WRITE (*,*) "Ci sono date diverse"
IF (ld_sca)   WRITE (*,*) "Ci sono scadenze diverse"
IF (ld_ksec1) WRITE (*,*) "Ci sono campi di sezione 1 diversi"
IF (opt_ro == 1) THEN
  WRITE (*,*) "Totale campi elaborati (incluso ro): ",ngrib
ELSE
  WRITE (*,*) "Totale campi elaborati: ",ngrib
ENDIF

DO kf = 1,nfin
  CALL PBCLOSE (iuin(kf),kret)
ENDDO
CALL PBCLOSE (iuout,kret)
CLOSE (96)

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filelst)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo somma_grib_multi.lst, record ",kf
WRITE (*,'(a)') chdum
STOP

9997 CONTINUE
WRITE (*,*) "Richiesti troppi files in input, aumentare parametro maxfil"
STOP

9996 CONTINUE
WRITE(*,*) "Errore aprendo file grib n.ro ",kf,": ",TRIM(filein(kf)), &
  " kret ",kret
STOP

9995 CONTINUE
WRITE(*,*) "Errore aprendo il file densita': ",TRIM(filero)," kret ",kret
STOP

9994 CONTINUE
WRITE(*,*) "Errore o EOF leggendo il file densita': ",TRIM(filero), &
  " kret ",kret
STOP

9993 CONTINUE
WRITE (*,*) "Campo con griglia diversa: file ",TRIM(filein(kf)), &
  " campo ",kg
STOP

9992 CONTINUE
WRITE (*,*) "Campo con griglia diversa: file ",TRIM(filero), &
  " campo ",kg
STOP

END PROGRAM sum_pm_species

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a schermo l'hlep del programma
!

!            123456789012345678901234567890123456789012345678901234567890123456789012345

! Programma che legge un insieme di files con i campi delle singole specie 
! del PM (in ppb) e scrive un file che contiene la loro somma (in ppb o 
! ug/m3)
!
! Note:
! Derivato da somma_grib.f90; puo' essere usato anche per calcolare 
! combinazioni lineari di grib.


!            123456789012345678901234567890123456789012345678901234567890123456789012345678
WRITE (*,*) "Uso: sum_pm_species.exe [-h] filelst fileout [-p cem tab var]"
WRITE (*,*) "                        [-ro filero / -rost tt]"
WRITE (*,*)
WRITE (*,*) "Operazione compiuta: output = SUM(pmol_N * input_N) [ * Ro * e3/Avgd ]"
WRITE (*,*)
WRITE (*,*) "Serve a:"
WRITE (*,*) "- leggere molte specie di PM (in ppb) e scriverne la somma (ev. in ug/m3)"
WRITE (*,*) "- convertire una specie da ppb a ug/m3"
WRITE (*,*) "- compiere un'operazione lineare su files grib (non attivare -ro e -rost)"
WRITE (*,*)
WRITE (*,*) "filelst:        elenco dei nomi files con le specie PM e dei relativi pesi"
WRITE (*,*) "                molecolari (un file per record, formato libero)"
WRITE (*,*) "fileout:        nome file ouptut"
WRITE (*,*) "-h              visualizza questo help"
WRITE (*,*) "-p cem,tab,var: modifica codici descrittori parametro in output (altrimenti "
WRITE (*,*) "                usa quelli del primo file)"
WRITE (*,*) "-ro filero:     converte in ug/m3, prendendo densita' aria da filero."
WRITE (*,*) "-rost tt:       converte in ug/m3, usando P=1013.25 e T=tt (0<=tt<=20)"
WRITE (*,*)

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

PROGRAM llgribex
!--------------------------------------------------------------------------
! Legge un file con molti grib e scrive a schermo alcune informazioni sulle
! intestazioni, una riga per ogni grib
! Fa lo stesso lavoro di wgrib e lgrib, ma funziona anche con grib SOP
!
!                                           Versione 2.6, Enrico 02/03/2011
!--------------------------------------------------------------------------

!USE date_handler
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 1000000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
INTEGER :: iuin,iuout,cnt,np,kpar,pshow,ios
CHARACTER (LEN=200) :: header,chrec,chrec1,chrec2
CHARACTER (LEN=80) :: chfmt0,chfmt1,chfmt2
CHARACTER (LEN=80) :: filein,chpar
CHARACTER (LEN=1) :: next_arg,pfmt,hoper
LOGICAL :: ls4

! Variabili temporanee (definite solo per un particolare uso del programma)

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
ls4 = .FALSE.
pshow = 0
next_arg = ""

DO kpar = 1,HUGE(kpar)
  CALL getarg(kpar,chpar)

  IF (next_arg == "p" .OR. next_arg == "q") THEN
    READ (chpar,*,IOSTAT=ios) pshow
    IF (ios /= 0 .OR. pshow <= 0) THEN
      CALL write_help
      STOP
    ENDIF
    next_arg = ""

  ELSE IF (TRIM(chpar) == "")THEN
    EXIT

  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL write_help
    STOP

  ELSE IF (TRIM(chpar) == "-s4") THEN
    ls4 = .TRUE.

  ELSE IF (TRIM(chpar) == "-p") THEN
    next_arg = "p"
    pfmt="e"

  ELSE IF (TRIM(chpar) == "-pi") THEN
    next_arg = "q"
    pfmt="i"

  ELSE
    filein = TRIM(chpar)

  ENDIF
ENDDO

IF (TRIM(filein) == "") THEN
  CALL write_help
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Scelgo cosa deve decodificare la Gribex
IF (pshow == 0) THEN
  hoper = "I"
ELSE
  hoper = "D"
ENDIF

! 1.3 Apro il file
CALL PBOPEN (iuin,filein,'R',kret)
IF (kret < 0) THEN
  WRITE (*,*) "Errore aprendo ",TRIM(filein)
  STOP
ENDIF

! 1.4 Assegno i formati
chfmt0 = "(i6.6,3x, i4.4,3i2.2,2x, 3(1x,i3.3),3x, i3.3,1x,i4.4,1x,i3.3,2x, 4(1x,i3.3))"
chfmt1 = "(i6.6,a1,i6.6,1x,i2.2,2x,i2.2)"
IF (pfmt == "e") THEN
  chfmt2 = "(e10.3)"
ELSE IF (pfmt == "i") THEN
  chfmt2 = "(i8)"
ENDIF

! 1.5 Costruisco l'header e lo scrivo

!         123456---1234121212---123-123-123---123-1234-123---123-123-123-123
header = "Progr.   data emis.   <parametro>   <--livello->   <---scadenza-->"

!                                            ---12345-12345-12--12
IF (ls4) WRITE (header,'(2a)') TRIM(header),"   <- nok/ntot > nb sop"

!                                                     ---1234567890
IF (pshow /= 0) WRITE (header,'(2a,i6)') TRIM(header),"   pto:",pshow

WRITE (*,'(a)') TRIM(header)

!--------------------------------------------------------------------------
! 2) Leggo / Scrivo (ciclo sui grib)

DO cnt = 1,HUGE(cnt)

! 2.1 Leggo il grib 
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3(2) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,hoper,kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
  np = ksec2(2)*ksec2(3)

! 2.2 Costruisco il record di output con le informazioni richieste
  WRITE (chrec,chfmt0) &
    cnt, &                                                ! progressivo
    ksec1(10)+100*(ksec1(21)-1),ksec1(11:13), &           ! data emi
    ksec1(2),ksec1(1),ksec1(6), &                         ! parametro
    ksec1(7:9), &                                         ! livello
    ksec1(15:18)                                          ! scadenza

  IF (ls4) THEN
    WRITE (chrec1,chfmt1) &
      COUNT(field(1:np) /= rmis),"/",np, &                ! nok/np
      ksec4(2), &                                         ! nbits                  
      ksec4(4)                                            ! SOP?
    chrec = TRIM(chrec) // "   " // chrec1
  ENDIF

  IF (pshow /= 0) THEN
    IF (pfmt == "e") THEN
      WRITE (chrec2,chfmt2) field(pshow)                  ! value
    ELSE IF (pfmt == "i") THEN
      WRITE (chrec2,chfmt2) NINT(field(pshow))            ! value
    ENDIF
    chrec = TRIM(chrec) // "   " // chrec2
  ENDIF

! 2.3 Scrivo a schermo i dati 
  WRITE (*,'(a)') TRIM(chrec)

ENDDO
!--------------------------------------------------------------------------
! 3) Chiudo i files e termino

CALL PBCLOSE (iuin,kret)
STOP

END PROGRAM llgribex

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help

WRITE (*,*) "Uso: llgrib [-h] [-s4] [-p n] [-pi n] filein"
WRITE (*,*) "  Visualizza il contenuto di un file grib, una riga per ogni campo"
WRITE (*,*) "  default  visualizza: progressivo, data, livello, parametro, scadenza"
WRITE (*,*) "  -s4      aggiunge: dati validi, nbits, compressione SOP"
WRITE (*,*) "  -p n     aggiunge: valori nel punto n (formato e10.3)"
WRITE (*,*) "  -pi n    aggiunge: valori nel punto n (formato i8)"
WRITE (*,*)
RETURN

END SUBROUTINE write_help

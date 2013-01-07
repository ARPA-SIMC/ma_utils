PROGRAM llgrib
!--------------------------------------------------------------------------
! Legge un file con molti grib e scrive a schermo alcune informazioni sulle
! intestazioni, una riga per ogni grib. Gestisce grib1 e (alcuni) grib2.
! Le versioni < 3 chiamano la Gribex e gestiscono solo grib1.
!
!                                         Versione 3.1.0, Enrico 06/11/2012
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
USE grib2_utilities
IMPLICIT NONE

REAL, ALLOCATABLE :: field(:)
REAL :: pval
INTEGER :: date,hh,en,nodp,nocv,df,drtn,bpv,sop,par(3),lev(3),scad(4)
INTEGER :: ifin,igin,kpar,pshow,ios,iret,kg,ier,ni,nj
CHARACTER (LEN=200) :: header,chrec,chrec1,chrec2
CHARACTER (LEN=80) :: chfmt0,chfmt1,chfmt2
CHARACTER (LEN=80) :: filein,chpar
CHARACTER (LEN=1) :: next_arg,pfmt
LOGICAL :: ls4

INTEGER, PARAMETER :: imiss_out = 999
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
  ELSE IF (TRIM(chpar) == "-px") THEN
    next_arg = "q"
    pfmt="x"
  ELSE
    filein = TRIM(chpar)
  ENDIF

ENDDO

IF (TRIM(filein) == "") THEN
  CALL write_help
  STOP
ENDIF

! 1.2 Apro il file
ifin = 0
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999

! 1.3 Assegno i formati
chfmt0 = "(i6.6,3x, i8.8,i2.2,2x, 3(1x,i3.3),3x, i3.3,1x,i4.4,1x,i3.3,2x, 4(1x,i3.3))"
chfmt1 = "(i6.6,a1,i6.6,1x,i2.2,2x,i2.2)"
IF (pfmt == "e") THEN
  chfmt2 = "(e10.3)"
ELSE IF (pfmt == "x") THEN
  chfmt2 = "(e14.7)"
ELSE IF (pfmt == "i") THEN
  chfmt2 = "(i8)"
ENDIF

! 1.4 Costruisco l'header e lo scrivo

!         123456---1234121212---123-123-123---123-1234-123---123-123-123-123
header = "Progr.   data emis.   <parametro>   <--livello->   <---scadenza-->"

!                                            ---12345-12345-12--12
IF (ls4) WRITE (header,'(2a)') TRIM(header),"   <- nok/ntot > nb sop"

!                                                     ---1234567890
IF (pshow /= 0) WRITE (header,'(2a,i6)') TRIM(header),"   pto:",pshow

WRITE (*,'(a)') TRIM(header)

!--------------------------------------------------------------------------
! 2) Leggo / Scrivo (ciclo sui grib)

igin = 0
DO kg = 1,HUGE(kg)

! 2.1 Leggo il prossimo campo
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998

! 2.2 Leggo le chiavi richieste, in stile "GRIB1"
  CALL get_grib1_header(igin, PAR=par, LEV=lev, SCAD=scad, IRET=ier)

  CALL grib_get(igin,"editionNumber",en)
  CALL grib_get(igin,"dataDate",date)
  CALL grib_get(igin,"hour",hh)
  CALL grib_get(igin,"numberOfDataPoints",nodp)
  CALL grib_get(igin,"numberOfCodedValues",nocv)

! 16/01/2012: grib_get_element ritorna un valore simile ma diverso!!!
! IF (pshow /= 0) CALL grib_get_element(igin,"values",pshow,pval)
  IF (pshow /= 0) THEN
    CALL grib_get(igin,"Ni",ni)
    CALL grib_get(igin,"Nj",nj)
    ALLOCATE(field(ni*nj))
    CALL grib_get(igin,"values",field)
    pval = field(pshow)
    DEALLOCATE(field)
  ENDIF

  IF (en == 1) THEN                       ! Chiavi specifiche del GRIB1
    CALL grib_get(igin,"bitsPerValue",bpv)
    CALL grib_get(igin,"dataFlag",df)
    sop = MOD(df/2,2)

  ELSE IF (en == 2) THEN                  ! Chiavi specifiche del GRIB2
    CALL grib_get(igin,"dataRepresentationTemplateNumber",drtn)
    IF (drtn == 0) THEN
      CALL grib_get(igin,"bitsPerValue",bpv)
    ELSE
      bpv = -9
    ENDIF
    sop = drtn

  ENDIF

! 2.3 Scrivo a schermo le informazioni richieste
  WHERE (.NOT. c_e(par(:)))
    par(:) = imiss_out
  ENDWHERE
  WHERE (.NOT. c_e(lev(:)))
    lev(:) = imiss_out
  ENDWHERE
  WHERE (.NOT. c_e(scad(:)))
    scad(:) = imiss_out
  ENDWHERE

  WRITE (chrec,chfmt0) kg,date,hh,par(1:3),lev(1:3),scad(1:4)
  IF (ls4) THEN
    WRITE (chrec1,chfmt1) nocv,"/",nodp,bpv,sop
    chrec = TRIM(chrec) // "   " // chrec1
  ENDIF

  IF (pshow /= 0) THEN
    IF (pfmt == "e" .OR. pfmt == "x") THEN
      WRITE (chrec2,chfmt2) pval
    ELSE IF (pfmt == "i") THEN
      WRITE (chrec2,chfmt2) NINT(pval)
    ENDIF
    chrec = TRIM(chrec) // "   " // TRIM(chrec2)
  ENDIF

  WRITE (*,'(a)') TRIM(chrec)

  CALL grib_release(igin,iret)  
ENDDO

!--------------------------------------------------------------------------
! 3) Chiudo il file e termino

CALL grib_close_file(ifin,iret)
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

END PROGRAM llgrib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help

WRITE (*,*) "Uso: llgrib [-h] [-s4] [-p n / -pi n / -px n] filein"
WRITE (*,*) "  Visualizza il contenuto di un file grib, una riga per ogni campo"
WRITE (*,*) "  default  visualizza: progressivo, data, livello, parametro, scadenza"
WRITE (*,*) "  -s4      aggiunge: dati validi, nbits, compressione SOP"
WRITE (*,*) "  -p n     aggiunge: valori nel punto n (formato e10.3)"
WRITE (*,*) "  -pi n    aggiunge: valori nel punto n (formato i8)"
WRITE (*,*) "  -px n    aggiunge: valori nel punto n (formato e14.7)"
WRITE (*,*)
RETURN

END SUBROUTINE write_help

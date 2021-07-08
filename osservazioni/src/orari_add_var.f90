PROGRAM orari_add_var
!--------------------------------------------------------------------------
! Legge 1 file relativo a una serie temporale su punto; lo riscrive
! aggiungendo un parametro calcolato in base ai dati in input.
! Programma laboratorio, da modificare ogni volta per appplicazioni
! specifiche
!
! Uso: 
! orari_add_var.exe [-h] filein fileout newvar par [-inpf FMT]
! Programma laboratorio, da modificare ogni volta
!
!                                                 V1.0.0, Enrico 20/04/2021
!--------------------------------------------------------------------------

IMPLICIT NONE

!==========================================================================
! 0) Dichiarazioni

! Parametri costanti
REAL, PARAMETER :: pi = 3.1415926
REAL, PARAMETER :: rmis_hhr = -9999. ! dato mancante, files estra_orari
REAL, PARAMETER :: rmis_ser = -9999. ! dato mancante, files trasp_seriet
REAL, PARAMETER :: rmis_sex = -1.E30 ! dato mancante, files trasp_seriet exp
REAL, PARAMETER :: rmis_qad = -9999. ! dato mancante, files estra_qa giorn.
REAL, PARAMETER :: rmis_tem = -9999. ! dato mancante, files trasp_temp
INTEGER, PARAMETER :: fw = 10        ! ampiezza dei campi nei files I/O
INTEGER, PARAMETER :: mxpar = 500    ! n.ro max di parametri in un file
INTEGER, PARAMETER :: out_dec = 1    ! n.ro di decimali in output

! Altre variabili del programma
REAL, ALLOCATABLE :: val_inp(:),buffer(:)
REAL :: rmis_inp,val_out,nv_par(10),wd,ws,br,pctmin

INTEGER :: head_len,npar_inp,head_offset_inp,nhead_inp,mand_par,nok_out
INTEGER :: yy,mm,dd,hh,sca,eof,eor
INTEGER :: k,kp,kr,ier
INTEGER :: nh,col,nrq

CHARACTER (LEN=fw), ALLOCATABLE :: chval_inp(:)
CHARACTER (LEN=mxpar*(fw+1)+20) :: chrec_inp,head_par
CHARACTER (LEN=fw) :: newvar
CHARACTER (LEN=250) :: chdum,fileinp,fileout,chfmt1,chfmt2
CHARACTER (LEN=4) :: next_arg
CHARACTER (LEN=3) :: inp_fmt
CHARACTER (LEN=1) :: inp_step

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

! Default
inp_fmt = "ser"
mand_par = 0
next_arg = ""

! Parsing
DO kp = 1,HUGE(0)-1
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(chdum) == "-inpf") THEN
    next_arg = "inpf"
  ELSE IF (next_arg == "inpf") THEN
    inp_fmt = TRIM(chdum)
    next_arg = ""
  ELSE
    IF (mand_par == 0) THEN
      fileinp = TRIM(chdum)
    ELSE IF (mand_par == 1) THEN
      fileout = TRIM(chdum)
    ELSE IF (mand_par == 2) THEN
      newvar = TRIM(chdum)
    ELSE IF (mand_par >= 3) THEN
      READ (chdum,*) nv_par(mand_par-2)
    ENDIF
    mand_par = mand_par + 1
  ENDIF
ENDDO

! Calcolo step temporale
IF (inp_fmt/="ser" .OR. inp_fmt/="hhr" .OR. inp_fmt/="sex") THEN
  inp_step = "h"
ELSE IF (inp_fmt/="qad") THEN
  inp_step = "d"
ELSE IF (inp_fmt/="tem") THEN
  inp_step = "t"
ENDIF  

! Controlli
IF (inp_fmt/="ser" .AND. inp_fmt/="hhr" .AND. inp_fmt/="sex" .AND. &
     inp_fmt/="qad" .AND. inp_fmt/="tem") THEN
  CALL scrive_help
  STOP
ENDIF

IF ((newvar == "FFbr" .AND. mand_par /= 4) .OR. &
    (newvar == "rmean" .AND. mand_par /= 6) .OR. &
    (newvar /= "FFbr" .AND. newvar /= "rmean")) THEN
  CALL scrive_help
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.2 Parametri dipendenti dal formato dei dati

! Tracciato file di input
IF (inp_fmt == "hhr") THEN
  head_offset_inp = 13
  nhead_inp = 3
  rmis_inp = rmis_hhr
ELSE IF (inp_fmt == "ser") THEN
  head_offset_inp = 17
  nhead_inp = 6
  rmis_inp = rmis_ser
ELSE IF (inp_fmt == "sex") THEN
  head_offset_inp = 17
  nhead_inp = 6
  rmis_inp = rmis_sex
ELSE IF (inp_fmt == "qad") THEN
  head_offset_inp = 10
  nhead_inp = 3
  rmis_inp = rmis_qad
ELSE IF (inp_fmt == "tem") THEN
  head_offset_inp = 19
  nhead_inp = 3
  rmis_inp = rmis_tem
ENDIF

!--------------------------------------------------------------------------
! 1.3 Apro il file di input ed elaboro l'header

! Apro files
OPEN (UNIT=20, FILE=fileinp, STATUS="OLD", FORM="FORMATTED", ERR=9999)
OPEN (UNIT=40, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")

print *,nhead_inp

! Leggo e riscrivo gli header di fileinp
DO k = 1, nhead_inp - 1
  READ (20,'(a)',IOSTAT=ier) chrec_inp
  IF (ier /= 0) GOTO 9997
  WRITE (40,'(a)') TRIM(chrec_inp)
ENDDO
READ (20,'(a)',IOSTAT=ier) head_par
IF (ier /= 0) GOTO 9997
WRITE (40,'(3a)') TRIM(head_par)," ",ADJUSTR(newvar)

! Calcolo il numero dei parametri in fileinp
head_len = LEN(TRIM(head_par))
IF (MOD(head_len - head_offset_inp, fw + 1) /= 0) THEN
  GOTO 9996
ELSE
  npar_inp = (head_len - head_offset_inp) / (fw + 1)
ENDIF

!--------------------------------------------------------------------------
! 1.4 Costruisco i formati per i dati in input (chftm1) e output (chfmt2)

IF (inp_fmt == "hhr") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i4,3i3,",npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(i4.4,3(1x,i2.2),", &
    npar_inp+1,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_fmt == "qad") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i4,2i3,",npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(i4.4,2(1x,i2.2),", &
    npar_inp+1,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_fmt == "tem") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i4,3i3,6x,",npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(i4.4,3(1x,i2.2),6x,", &
    npar_inp+1,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_fmt == "ser") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i2,1x,i2,1x,i4,1x,i2,1x,i3,", &
    npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(2(i2.2,a1),i4.4,1x,i2.2,1x,i3.3,", &
    npar_inp+1,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_fmt == "sex") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i2,1x,i2,1x,i4,1x,i2,1x,i3,", &
    npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(2(i2.2,a1),i4.4,1x,i2.2,1x,i3.3,", &
    npar_inp+1,"(1x,e",fw,".",3,"))"

ENDIF

!--------------------------------------------------------------------------
! 1.5 Alloco gli arrays; codice per EOF

ALLOCATE (val_inp(npar_inp))
ALLOCATE (chval_inp(npar_inp))
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 1.6 Assegnazioni dipendenti dai parametri

IF (newvar == "rmean") THEN
  nh = NINT(nv_par(1))
  col = NINT(nv_par(2))
  pctmin = nv_par(3)
  nrq = CEILING(pctmin * nv_par(1))
  ALLOCATE(buffer(nh))
  buffer(:) = rmis_inp 
  IF (col > npar_inp) GOTO 9980
  WRITE (*,*) "Media su ",nh," ore, richiesti almeno ",nrq," dati"
ENDIF

WRITE (*,'(a,2(i4,a))') "Numero di parametri in filein: ",npar_inp

!==========================================================================
! 2) Leggo/scrivo (ciclo sui record in input)

nok_out = 0

main: DO kr = 1,HUGE(0)-1

!--------------------------------------------------------------------------
! 2.1 Leggo il prossimo record dal file di input

  val_inp(:) = rmis_inp
  READ (20,'(a)',IOSTAT=ier) chrec_inp
  IF (ier == eof) EXIT
  IF (ier /= 0) GOTO 9993

  IF (inp_fmt == "hhr") THEN
    READ (chrec_inp,chfmt1,IOSTAT=ier) yy,mm,dd,hh,chval_inp(1:npar_inp)
    IF (ier /= 0) GOTO 9993
    DO kp = 1,npar_inp
      READ (chval_inp(kp),*,ERR=9993) val_inp(kp)
    ENDDO

  ELSE IF (inp_fmt == "qad") THEN
    READ (chrec_inp,chfmt1,IOSTAT=ier) yy,mm,dd,chval_inp(1:npar_inp)
    hh = 0
    IF (ier /= 0) GOTO 9993
    DO kp = 1,npar_inp
      READ (chval_inp(kp),*,ERR=9993) val_inp(kp)
    ENDDO

  ELSE IF (inp_fmt == "ser" .OR. inp_fmt == "sex") THEN
     chval_inp(:) = ""
     READ (chrec_inp,chfmt1,IOSTAT=ier) dd,mm,yy,hh,sca,chval_inp(1:npar_inp)
     IF (ier /= 0) GOTO 9993
     DO kp = 1,npar_inp
       READ (chval_inp(kp),*,ERR=9993) val_inp(kp)
     ENDDO

  ELSE IF (inp_fmt == "tem") THEN
    READ (chrec_inp,chfmt1,IOSTAT=ier) yy,mm,dd,hh,chval_inp(1:npar_inp)
    IF (ier /= 0) GOTO 9993
    DO kp = 1,npar_inp
      READ (chval_inp(kp),*,ERR=9993) val_inp(kp)
    ENDDO

  ENDIF

!--------------------------------------------------------------------------
! 2.3 Calcolo il nuovo parametro 

  IF (newvar == "FFbr") THEN
    wd = val_inp(1)
    ws = val_inp(2)
    br = nv_par(1)     ! dir. lungo cui calcolare la componente positiva della brezza
    IF (wd /= rmis_inp .AND. ws /= rmis_inp) THEN
      val_out = ws * COS(pi/180.*(wd-br))
      nok_out = nok_out + 1
    ELSE
      val_out = rmis_inp
    ENDIF

  ELSE IF (newvar == "rmean") THEN
    buffer(1:nh-1) = buffer(2:nh)
    buffer(nh) = val_inp(col)   ! buffer(nh) e' il dato piu' recente
    IF (COUNT(buffer(:) /= rmis_inp) >= nrq) THEN
      val_out = SUM(buffer(:), MASK=buffer(:) /= rmis_inp) / REAL(nh)
      nok_out = nok_out + 1
    ELSE
      val_out = rmis_inp
    ENDIF
   
  ELSE
    val_out = rmis_inp

 ENDIF
  
!--------------------------------------------------------------------------
! 2.4 Scrivo un record sul file di output

  IF (inp_fmt == "hhr") THEN
    WRITE (40,chfmt2,IOSTAT=ier) yy,mm,dd,hh,val_inp(1:npar_inp),val_out

  ELSE IF (inp_fmt == "qad") THEN
    WRITE (40,chfmt2,IOSTAT=ier) yy,mm,dd,val_inp(1:npar_inp),val_out

  ELSE IF (inp_fmt == "ser" .OR. inp_fmt == "sex") THEN
    WRITE (40,chfmt2,IOSTAT=ier) dd,"/",mm,"/",yy,hh,sca,val_inp(1:npar_inp),val_out

  ELSE IF (inp_fmt == "tem") THEN
    WRITE (40,chfmt2,IOSTAT=ier) yy,mm,dd,hh,val_inp(1:npar_inp),val_out

  ENDIF
  IF (ier /= 0) GOTO 9991

ENDDO main

!==========================================================================
! 3) Conclusione

WRITE (*,'(2(a,i6))') "Elaborati ",kr-1," istanti, dati calcolati ",nok_out

STOP

!==========================================================================
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileinp)
STOP 1

9997 CONTINUE
WRITE (*,*) "Errore leggendo header ",TRIM(fileinp)
STOP 2

9996 CONTINUE
WRITE (*,*) "L'header parametri di filein e' troncato o ha campi con ampiezza /= ",fw
WRITE (*,*) "lughezza header: ",head_len,", offset: ",head_offset_inp,", fw: ",fw
STOP 3

9993 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(fileinp)," linea ",kr + nhead_inp
STOP 4

9991 CONTINUE
WRITE (*,*) "Errore scrittura output, riga ",kr+nhead_inp
STOP

9980 CONTINUE
WRITE (*,*) "Errore, filein non ha ",col," colonne"
STOP

END PROGRAM orari_add_var

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

SUBROUTINE scrive_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE
!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "maskout_orari.exe [-h] filein fileout newvar par1, par2, ... [-inpf FMT]"
WRITE (*,*) "Legge 1 file relativo a una serie temporale su punto; lo riscrive"
WRITE (*,*) "aggiungendo un parametro calcolato in base ai dati in input."
WRITE (*,*) ""
WRITE (*,*) "filein    : file da leggere e riscrivere"
WRITE (*,*) "fileout   : file di ouptut (stesso tracciato di filein)"
WRITE (*,*) "newvar    : flag variabile da aggiungere (FFbr, rmean)"
WRITE (*,*) "[par1, par2 ...]: parametri relativi alla variabile da calcolare"
WRITE (*,*) "  con FFbr: direzione lungo cui calcolare la componente di brezza"
WRITE (*,*) "  con rmean: numero di ore, colonna da elaborare, frazione min dati validi (da 0.a 1.) "
WRITE (*,*) "             Il dato e' attribuito all'ultima ora dell'intervallo"
WRITE (*,*) "-inpf FMT : formato del file di input: ser = seriet (default)"
WRITE (*,*) "            hhr = estra_orari / estraqa orario, sex = seriet con notazione exp"
WRITE (*,*) "            qad = estra_qaria giornaliero, tem = trasp_temp"
WRITE (*,*) "-h        : visualizza questo help"
WRITE (*,*) 

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

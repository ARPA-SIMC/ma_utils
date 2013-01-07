PROGRAM maskout_orari
!--------------------------------------------------------------------------
! Legge 2 files relativi a serie temporali su punto; riscrive il primo 
! (input) mettendo mancanti i dati relativi ad alcuni istanti, scelti in 
! base al valore di un campo del secondo file (mask)
! Gestisce i formati seriet (decimale ed exp), estra_orari, estra_qa 
! (orario e giornaliero), trasp_temp.
! Input e mask possono essere in formato diverso, ma devono avere lo stesso
! step temporale
!
! Todo:
! - gestire input e maschera con step temporale diverso (ora/giorno)
!
! Uso: 
! maskout_orari.exe [-h] filein filemask fileout
!   [-inpf FMT] [-mskf FMT] [-mskn N] [-outm N]
!
!                                                 V1.1.0, Enrico 07/09/2012
!--------------------------------------------------------------------------

IMPLICIT NONE

!==========================================================================
! 0) Dichiarazioni

! Parametri costanti
REAL, PARAMETER :: rmis_hhr = -9999. ! dato mancante, files estra_orari
REAL, PARAMETER :: rmis_ser = -9999. ! dato mancante, files trasp_seriet
REAL, PARAMETER :: rmis_sex = -1.E30 ! dato mancante, files trasp_seriet exp
REAL, PARAMETER :: rmis_qad = -9999. ! dato mancante, files estra_qa giorn.
REAL, PARAMETER :: rmis_tem = -9999. ! dato mancante, files trasp_temp
INTEGER, PARAMETER :: fw = 10        ! ampiezza dei campi nei files I/O
INTEGER, PARAMETER :: mxpar = 500    ! n.ro max di parametri in un file
INTEGER, PARAMETER :: out_dec = 1    ! n.ro di decimali in output

! Altre variabili del programma
REAL, ALLOCATABLE :: val_inp(:),val_msk(:),val_out(:)
REAL :: rmis_inp,rmis_msk,thr_low,thr_high
INTEGER :: head_offset_inp,nhead_inp,head_offset_msk,nhead_msk,mand_par
INTEGER :: msk_col,mis_col,head_len,npar_inp,npar_msk
INTEGER :: cnt_mis_val,cnt_mis_rec
INTEGER :: yy,mm,dd,hh,sca,k,kp,kr,ios(4),ier,eof,eor
CHARACTER (LEN=mxpar*(fw+1)+20) :: chrec_inp,chrec_msk,head_par
CHARACTER (LEN=fw), ALLOCATABLE :: chval_inp(:),chval_msk(:)
CHARACTER (LEN=250) :: chdum,fileinp,fileout,filemsk,chfmt1,chfmt2,chfmt3
CHARACTER (LEN=4) :: next_arg
CHARACTER (LEN=3) :: inp_fmt,msk_fmt
CHARACTER (LEN=1) :: inp_step,msk_step
LOGICAL :: lmiss,lhigh,llow

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

! Default
inp_fmt = "ser"
msk_fmt = "ser"
msk_col = 1
mis_col = 0
mand_par = 0
ios(:) = 0
lmiss = .FALSE.
lhigh = .FALSE.
llow = .FALSE.

! Parsing
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (next_arg == "inpf") THEN
    inp_fmt = TRIM(chdum)
    next_arg = ""
  ELSE IF (next_arg == "mskf") THEN
    msk_fmt = TRIM(chdum)
    next_arg = ""
  ELSE IF (next_arg == "mskn") THEN
    READ (chdum,*,IOSTAT=ios(1)) msk_col
    next_arg = ""
  ELSE IF (next_arg == "outm") THEN
    READ (chdum,*,IOSTAT=ios(2)) mis_col
    next_arg = ""
  ELSE IF (next_arg == "high") THEN
    READ (chdum,*,IOSTAT=ios(3)) thr_high
    next_arg = ""
  ELSE IF (next_arg == "low") THEN
    READ (chdum,*,IOSTAT=ios(4)) thr_low
    next_arg = ""
  ELSE IF (TRIM(chdum) == "-inpf") THEN
    next_arg = "inpf"
  ELSE IF (TRIM(chdum) == "-mskf") THEN
    next_arg = "mskf"
  ELSE IF (TRIM(chdum) == "-mskn") THEN
    next_arg = "mskn"
  ELSE IF (TRIM(chdum) == "-outm") THEN
    next_arg = "outm"
  ELSE IF (TRIM(chdum) == "-miss") THEN
    lmiss = .TRUE.
  ELSE IF (TRIM(chdum) == "-high") THEN
    lhigh = .TRUE.
    next_arg = "high"
  ELSE IF (TRIM(chdum) == "-low") THEN
    llow = .TRUE.
    next_arg = "low"
  ELSE
    IF (mand_par == 0) THEN
      fileinp = TRIM(chdum)
    ELSE IF (mand_par == 1) THEN
      filemsk = TRIM(chdum)
    ELSE IF (mand_par == 2) THEN
      fileout = TRIM(chdum)
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

IF (msk_fmt/="ser" .OR. msk_fmt/="hhr" .OR. msk_fmt/="sex") THEN
  msk_step = "h"
ELSE IF (msk_fmt/="qad") THEN
  msk_step = "d"
ELSE IF (msk_fmt/="tem") THEN
  msk_step = "t"
ENDIF  

! Controlli
IF ((inp_fmt/="ser" .AND. inp_fmt/="hhr" .AND. inp_fmt/="sex" .AND. &
     inp_fmt/="qad" .AND. inp_fmt/="tem") .OR. &
    (msk_fmt/="ser" .AND. msk_fmt/="hhr" .AND. msk_fmt/="sex" .AND. &
     msk_fmt/="qad" .AND. msk_fmt/="tem") .OR. &
     mand_par /= 3 .OR. ANY(ios(:)/=0) .OR. msk_col <= 0 .OR. mis_col < 0) THEN
  CALL scrive_help
  STOP
ENDIF
IF (.NOT. lmiss .AND. .NOT. lhigh .AND. .NOT. llow) THEN
  WRITE (*,*) "Non e' stato specificato nessun criterio per assegnare i dati mancanti"
  WRITE (*,*) " (opzioni: -miss, -high, -low)"
  STOP
ENDIF

IF (inp_step /= msk_step) GOTO 9000

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

! Tracciato file mask
IF (msk_fmt == "hhr") THEN
  head_offset_msk = 13
  nhead_msk = 3
  rmis_msk = rmis_hhr
ELSE IF (msk_fmt == "ser") THEN
  head_offset_msk = 17
  nhead_msk = 6
  rmis_msk = rmis_ser
ELSE IF (msk_fmt == "sex") THEN
  head_offset_msk = 17
  nhead_msk = 6
  rmis_msk = rmis_sex
ELSE IF (msk_fmt == "qad") THEN
  head_offset_msk = 10
  nhead_msk = 3
  rmis_msk = rmis_qad
ELSE IF (msk_fmt == "tem") THEN
  head_offset_msk = 19
  nhead_msk = 3
  rmis_msk = rmis_tem
ENDIF

!--------------------------------------------------------------------------
! 1.3 Apro i files ed elaboro gli header

! Apro files
OPEN (UNIT=20, FILE=fileinp, STATUS="OLD", FORM="FORMATTED", ERR=9999)
OPEN (UNIT=30, FILE=filemsk, STATUS="OLD", FORM="FORMATTED", ERR=9998)
OPEN (UNIT=40, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")

! Leggo e riscrico gli header di fileinp
DO k = 1, nhead_inp - 1
  READ (20,'(a)',IOSTAT=ier) chrec_inp
  IF (ier /= 0) GOTO 9997
  WRITE (40,'(a)') TRIM(chrec_inp)
ENDDO
READ (20,'(a)',IOSTAT=ier) head_par
IF (ier /= 0) GOTO 9997
WRITE (40,'(a)') TRIM(head_par)

! Calcolo il numero dei parametri in fileinp
head_len = LEN(TRIM(head_par))
IF (MOD(head_len - head_offset_inp, fw + 1) /= 0) THEN
  GOTO 9996
ELSE
  npar_inp = (head_len - head_offset_inp) / (fw + 1)
ENDIF
IF (mis_col > npar_inp) GOTO 9001

! Leggo header di filemsk
DO k = 1, nhead_msk - 1
  READ (30,*,IOSTAT=ier)
  IF (ier /= 0) GOTO 9995
ENDDO
READ (30,'(a)',IOSTAT=ier) head_par
IF (ier /= 0) GOTO 9995

! Calcolo il numero dei parametri in filemsk
head_len = LEN(TRIM(head_par))
IF (MOD(head_len - head_offset_msk, fw + 1) /= 0) THEN
  GOTO 9994
ELSE
  npar_msk = (head_len - head_offset_msk) / (fw + 1)
ENDIF
IF (msk_col > npar_msk) GOTO 9002

!--------------------------------------------------------------------------
! 1.4 Costruisco i formati per i dati in input (chftm1) e output (chfmt2)

IF (inp_fmt == "hhr") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i4,3i3,",npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(i4.4,3(1x,i2.2),", &
    npar_inp,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_fmt == "qad") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i4,2i3,",npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(i4.4,2(1x,i2.2),", &
    npar_inp,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_fmt == "tem") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i4,3i3,6x,",npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(i4.4,3(1x,i2.2),6x,", &
    npar_inp,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_fmt == "ser") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i2,1x,i2,1x,i4,1x,i2,1x,i3,", &
    npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(2(i2.2,a1),i4.4,1x,i2.2,1x,i3.3,", &
    npar_inp,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_fmt == "sex") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i2,1x,i2,1x,i4,1x,i2,1x,i3,", &
    npar_inp,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(2(i2.2,a1),i4.4,1x,i2.2,1x,i3.3,", &
    npar_inp,"(1x,e",fw,".",3,"))"

ENDIF

!--------------------------------------------------------------------------
! 1.5 Costruisco il formato per leggere il file maschera

IF (msk_fmt == "hhr") THEN
  WRITE (chfmt3,'(a,2(i3.3,a))') "(i4,3i3,",npar_msk,"(1x,a",fw,"))"

ELSE IF (msk_fmt == "qad") THEN
  WRITE (chfmt3,'(a,2(i3.3,a))') "(i4,2i3,",npar_msk,"(1x,a",fw,"))"

ELSE IF (msk_fmt == "tem") THEN
  WRITE (chfmt3,'(a,2(i3.3,a))') "(i4,3i3,6x,",npar_msk,"(1x,a",fw,"))"

ELSE IF (msk_fmt == "ser") THEN
  WRITE (chfmt3,'(a,2(i3.3,a))') "(i2,1x,i2,1x,i4,1x,i2,1x,i3,", &
    npar_msk,"(1x,a",fw,"))"

ELSE IF (msk_fmt == "sex") THEN
  WRITE (chfmt3,'(a,2(i3.3,a))') "(i2,1x,i2,1x,i4,i3,4x,",npar_msk,"(1x,a",fw,"))"

ENDIF

!--------------------------------------------------------------------------
! 1.6 Alloco gli arrays; codice per EOF

ALLOCATE (val_inp(npar_inp),val_out(npar_inp),val_msk(npar_msk))
ALLOCATE (chval_inp(npar_inp),chval_msk(npar_msk))
CALL get_eof_eor(eof,eor)

WRITE (*,'(a,2(i4,a))') "Numero di parametri: ", &
  npar_inp," in filein ",npar_msk," in filemsk"

!==========================================================================
! 2) Leggo/scrivo (ciclo sui record in input)

cnt_mis_val = 0
cnt_mis_rec = 0

DO kr = 1,HUGE(0)

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
      READ (chval_inp(kp),*,ERR=9992) val_inp(kp)
    ENDDO

  ELSE IF (inp_fmt == "qad") THEN
    READ (chrec_inp,chfmt1,IOSTAT=ier) yy,mm,dd,chval_inp(1:npar_inp)
    hh = 0
    IF (ier /= 0) GOTO 9993
    DO kp = 1,npar_inp
      READ (chval_inp(kp),*,ERR=9992) val_inp(kp)
    ENDDO

  ELSE IF (inp_fmt == "ser" .OR. inp_fmt == "sex") THEN
    READ (chrec_inp,chfmt1,IOSTAT=ier) dd,mm,yy,hh,sca,chval_inp(1:npar_inp)
    IF (ier /= 0) GOTO 9993
    DO kp = 1,npar_inp
      READ (chval_inp(kp),*,ERR=9992) val_inp(kp)
    ENDDO

  ELSE IF (inp_fmt == "tem") THEN
    READ (chrec_inp,chfmt1,IOSTAT=ier) yy,mm,dd,hh,chval_inp(1:npar_inp)
    IF (ier /= 0) GOTO 9993
    DO kp = 1,npar_inp
      READ (chval_inp(kp),*,ERR=9992) val_inp(kp)
    ENDDO

  ENDIF

!--------------------------------------------------------------------------
! 2.2 Leggo il prossimo record dal file di maschera

  val_msk(:) = rmis_msk
  READ (30,'(a)',IOSTAT=ier) chrec_msk
  IF (ier == eof) EXIT
  IF (ier /= 0) GOTO 9992

  IF (msk_fmt == "hhr") THEN
    READ (chrec_msk,chfmt3,IOSTAT=ier) yy,mm,dd,hh,chval_msk(1:npar_msk)
    IF (ier /= 0) GOTO 9992
    DO kp = 1,npar_msk
      READ (chval_msk(kp),*,ERR=9992) val_msk(kp)
    ENDDO

  ELSE IF (msk_fmt == "qad") THEN
    READ (chrec_msk,chfmt3,IOSTAT=ier) yy,mm,dd,chval_msk(1:npar_msk)
    hh = 0
    IF (ier /= 0) GOTO 9992
    DO kp = 1,npar_msk
      READ (chval_msk(kp),*,ERR=9992) val_msk(kp)
    ENDDO

  ELSE IF (msk_fmt == "ser" .OR. msk_fmt == "sex") THEN
    READ (chrec_msk,chfmt3,IOSTAT=ier) dd,mm,yy,hh,sca,chval_msk(1:npar_msk)
    IF (ier /= 0) GOTO 9992
    DO kp = 1,npar_msk
      READ (chval_msk(kp),*,ERR=9992) val_msk(kp)
    ENDDO

  ELSE IF (msk_fmt == "tem") THEN
    READ (chrec_msk,chfmt3,IOSTAT=ier) yy,mm,dd,hh,chval_msk(1:npar_msk)
    IF (ier /= 0) GOTO 9992
    DO kp = 1,npar_msk
      READ (chval_msk(kp),*,ERR=9992) val_msk(kp)
    ENDDO

  ENDIF

!--------------------------------------------------------------------------
! 2.3 Metto mancanti i dati necessari

  val_out(1:npar_inp) = val_inp(1:npar_inp)

  IF ((lmiss .AND. val_msk(msk_col) == rmis_msk) .OR. &
      (lhigh .AND. val_msk(msk_col) > thr_high) .OR. &
      (llow .AND. val_msk(msk_col) < thr_low)) THEN
    cnt_mis_rec = cnt_mis_rec + 1
    IF (mis_col == 0) THEN
      val_out(1:npar_inp) = rmis_inp
      cnt_mis_val = cnt_mis_val + npar_inp
    ELSE
      val_out(mis_col) = rmis_inp
      cnt_mis_val = cnt_mis_val + 1
    ENDIF
  ENDIF    

!--------------------------------------------------------------------------
! 2.4 Scrivo un record sul file di output

  IF (inp_fmt == "hhr") THEN
    WRITE (40,chfmt2,IOSTAT=ier) yy,mm,dd,hh,val_out(1:npar_inp)

  ELSE IF (inp_fmt == "qad") THEN
    WRITE (40,chfmt2,IOSTAT=ier) yy,mm,dd,val_out(1:npar_inp)

  ELSE IF (inp_fmt == "ser" .OR. inp_fmt == "sex") THEN
    WRITE (40,chfmt2,IOSTAT=ier) dd,"/",mm,"/",yy,hh,sca,val_out(1:npar_inp)

  ELSE IF (inp_fmt == "tem") THEN
    WRITE (40,chfmt2,IOSTAT=ier) yy,mm,dd,hh,val_out(1:npar_inp)

  ENDIF
  IF (ier /= 0) GOTO 9991

ENDDO

!==========================================================================
! 3) Conclusione

WRITE (*,'(a,i4,a,i6,a)') "Elaborati ",npar_inp," parametri ",kr-1," istanti"
WRITE (*,'(a,2(i8,a),f6.2,a)') "Messi mancanti ", &
  cnt_mis_val," dati in ",cnt_mis_rec," istanti (", &
  100.*REAL(cnt_mis_rec)/REAL(kr-1),"%)"

STOP

!==========================================================================
! 4) Gestione errori

9000 CONTINUE
WRITE (*,*) "Step temporale diverso in filein e filemsk"
STOP 5

9001 CONTINUE
WRITE (*,*) "Richiesto di modificare il campo ",mis_col
WRITE (*,*) TRIM(fileinp)," ha solo ",npar_inp," campi"
STOP 5

9002 CONTINUE
WRITE (*,*) "Richiesto di usare come maschera il campo ",msk_col
WRITE (*,*) TRIM(filemsk)," ha solo ",npar_msk," campi"
STOP 5

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileinp)
STOP 1

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filemsk)
STOP 1

9997 CONTINUE
WRITE (*,*) "Errore leggendo header ",TRIM(fileinp)
STOP 2

9996 CONTINUE
WRITE (*,*) "L'header parametri di filein e' troncato o ha campi con ampiezza /= ",fw
WRITE (*,*) "lughezza header: ",head_len,", offset: ",head_offset_inp,", fw: ",fw
STOP 3

9995 CONTINUE
WRITE (*,*) "Errore leggendo header ",TRIM(filemsk)
STOP 2

9994 CONTINUE
WRITE (*,*) "L'header parametri di filemsk e' troncato o ha campi con ampiezza /= ",fw
WRITE (*,*) "lughezza header: ",head_len,", offset: ",head_offset_msk,", fw: ",fw
STOP 3

9993 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(fileinp)," linea ",kr + nhead_inp
STOP 4

9992 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filemsk)," linea ",kr + nhead_msk
STOP 4

9991 CONTINUE
WRITE (*,*) "Errore scrittura output, riga ",kr+nhead_inp

END PROGRAM maskout_orari

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
INTEGER :: mxstaz

!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "maskout_orari.exe [-h] filein filemask fileout -miss -high X -low X"
WRITE (*,*) "  [-inpf FMT] [-mskf FMT] [-mskn N] [-outm N]"
WRITE (*,*) "Legge 2 files relativi a serie temporali su punto; riscrive il primo (input)"
WRITE (*,*) "mettendo mancanti i dati relativi ad alcuni istanti, scelti in base al valore di "
WRITE (*,*) "  un campo del secondo file (mask)"
WRITE (*,*) ""
WRITE (*,*) "filein    : file da leggere e riscrivere"
WRITE (*,*) "filemask  : file da cui leggere la lista dei dati mancanti"
WRITE (*,*) "fileout   : file di ouptut (stesso tracciato di filein)"
WRITE (*,*) ""
WRITE (*,*) "-miss     : mette mancanti (anche) i dati che sono mancanti nella maschera"
WRITE (*,*) "-high X   : mette mancanti (anche) i dati che nella maschera sono > X"
WRITE (*,*) "-low X    : mette mancanti (anche) i dati che nella maschera sono < X"
WRITE (*,*) ""
WRITE (*,*) "-inpf FMT : formato del file di input: ser = seriet (default)"
WRITE (*,*) "            hhr = estra_orari / estraqa orario, sex = seriet con notazione exp"
WRITE (*,*) "            qad = estra_qaria giornaliero, tem = trasp_temp"
WRITE (*,*) "-mskf FMT : formato del file di maschera (vedi sopra)"
WRITE (*,*) "-mskn N   : usa come maschera il campo N di filemask (defualt: il primo)"
WRITE (*,*) "-outm N   : mette mancante solo il campo N di filein (default: tutti i campi)"
WRITE (*,*) "-h        : visualizza questo help"
WRITE (*,*) 

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

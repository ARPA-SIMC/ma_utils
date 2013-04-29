PROGRAM proc_multi_orari
!--------------------------------------------------------------------------
! Data una lista di files relativi a serie storiche su punto, scrive un
! file che per ogni data/parametro contiene il risultato di una statistica
! sui files di input.
! Gestisce i formati seriet (decimale ed exp), estra_orari, estra_qa 
! (orario e giornaliero), trasp_temp
!
! Per aggiungere una nuova elaborazione "custom", modificare:
! - controlli sui parametri (par 1.1 in fondo)
! - se richiesta, gestione del file con i dati di supporto
! - controlli sui dati in input (par 1.5)
! - calcoli (par 2.2)
! - help (subr. scrive_help)
!
! Note:
! - Tutti i files di input devono essere nello stesso formato, contenere gli
!   stessi istanti e gli stessi parametri (nello stesso ordine e con lo 
!   stesso nome)
! - Il programma elabora ogni dato in modo indipendente (ad esempio, non fa 
!   nessun tipo di controllo sulla sequenzialita' delle date)
!
! TODO:
! - aggiungere diagnostica complessiva (es. dati validi totali in/out)
!
!                                                 V2.2.0, Enrico 10/04/2013
!--------------------------------------------------------------------------

USE array_utilities
IMPLICIT NONE

!==========================================================================
! 0) Dichiarazioni

! Parametri costanti
REAL, PARAMETER :: rmis_hhr = -9999. ! dato mancante, files estra_orari
REAL, PARAMETER :: rmis_ser = -9999. ! dato mancante, files trasp_seriet
REAL, PARAMETER :: rmis_sex = -1.E30 ! dato mancante, files trasp_seriet exp
REAL, PARAMETER :: rmis_ddy = -9999. ! dato mancante, files estra_qa giorn.
REAL, PARAMETER :: rmis_tem = -9999. ! dato mancante, files trasp_temp
INTEGER, PARAMETER :: fw = 10        ! ampiezza dei campi nei files I/O
INTEGER, PARAMETER :: mxpar = 500    ! n.ro max di parametri in un file
INTEGER, PARAMETER :: iulst = 20     ! unita' per filelst
INTEGER, PARAMETER :: iuout = 25     ! unit' per fileout
INTEGER, PARAMETER :: iusta = 26     ! unit' per filesta
INTEGER, PARAMETER :: iulog = 27     ! unit' per filelog
INTEGER, PARAMETER :: iu0 = 30       ! prima unita' per i files di input

! Altre variabili del programma
REAL, ALLOCATABLE :: val_in(:,:),val_out(:),val_ok(:),ave(:),ave2(:)
REAL, ALLOCATABLE :: yrtyp(:,:),yrtypstd(:,:),cli_max_diff(:,:)
REAL :: rmis,qcx,qcy,qcz,sp_sum,sp_sum2,sp_ave,sp_std,sp_max_diff
INTEGER, ALLOCATABLE :: nok(:),prvidx(:)
INTEGER :: mand_par,eof,eor,ios,nf,npar,nprv,head_len,head_offset,nhead,nld
INTEGER :: yy,mm,dd,hh,sca,idx,out_dec,req_par,sp_nok
INTEGER :: kp,kpr,kr,kf,p1,p2,k2,km,ksk

CHARACTER (LEN=mxpar*(fw+1)+20) :: head_par,head_liv,head_par1,head_liv1
CHARACTER (LEN=mxpar*(fw+1)+20) :: head_par_out,head_liv_out,head_par_sta
CHARACTER (LEN=mxpar*(fw+1)+20) :: head_staz,head_model,head_staz_out,head_model_out
CHARACTER (LEN=fw), ALLOCATABLE :: chval(:)
CHARACTER (LEN=fw) :: idpar(mxpar)
CHARACTER (LEN=250) :: filelst,chdum,filein,fileout,filesta,filelog
CHARACTER (LEN=250) :: chfmt1,chfmt2,chfmt3,chfmt4,chfmt5
CHARACTER (LEN=10), ALLOCATABLE :: prvlab(:)
CHARACTER (LEN=10) :: ch10
CHARACTER (LEN=3) :: stat,inp_data,next_arg
LOGICAL,ALLOCATABLE :: pexc(:,:)
LOGICAL :: ld,miss0

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando

inp_data = "nil"
mand_par = 0
filelst = ""
fileout = ""
filesta = ""
out_dec = 2
ld = .FALSE.
miss0 = .FALSE.
qcx = 3.
qcy = 1.
qcz = 0.

next_arg = ""
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (next_arg == "ndc" ) THEN
    next_arg = ""
    READ (chdum,*) out_dec
  ELSE IF (next_arg == "qcx" ) THEN
    next_arg = "qcy"
    READ (chdum,*) qcx
  ELSE IF (next_arg == "qcy" ) THEN
    next_arg = "qcz"
    READ (chdum,*) qcy
  ELSE IF (next_arg == "qcz" ) THEN
    next_arg = ""
    READ (chdum,*) qcz
  ELSE IF (TRIM(chdum) == "-o") THEN
    inp_data = "hhr"
  ELSE IF (TRIM(chdum) == "-s") THEN
    inp_data = "ser"
  ELSE IF (TRIM(chdum) == "-sx") THEN
    inp_data = "sex"
  ELSE IF (TRIM(chdum) == "-d") THEN
    inp_data = "ddy"
  ELSE IF (TRIM(chdum) == "-q") THEN
    inp_data = "ddy"
  ELSE IF (TRIM(chdum) == "-t") THEN
    inp_data = "tem"
  ELSE IF (TRIM(chdum) == "-ndec") THEN
    next_arg = "ndc"
  ELSE IF (TRIM(chdum) == "-qcpar") THEN
    next_arg = "qcx"
  ELSE IF (TRIM(chdum) == "-ld") THEN
    ld = .TRUE.
  ELSE IF (TRIM(chdum) == "-miss0") THEN
    miss0 = .TRUE.
  ELSE
    IF (mand_par == 0) THEN
      filelst = TRIM(chdum)
    ELSE IF (mand_par == 1) THEN
      stat = TRIM(chdum)
    ELSE IF (mand_par == 2) THEN
      fileout = TRIM(chdum)
    ELSE IF (mand_par == 3) THEN
      filesta = TRIM(chdum)
    ENDIF
    mand_par = mand_par + 1
  ENDIF
ENDDO

IF (stat=="pex" .OR. stat=="dst" .OR. stat=="qcc") THEN
  req_par = 4
ELSE IF (stat=="ave" .OR. stat=="max" .OR. stat/="min" .OR. &
  stat=="std" .OR. stat=="mdn" .OR. stat=="tke" .OR. stat=="qcs") THEN
  req_par = 3
ELSE
  CALL scrive_help
  STOP 1
ENDIF
IF (mand_par /= req_par) THEN
  CALL scrive_help
  STOP 1
ENDIF
IF (qcx<0. .OR. qcy<0. .OR. qcz<0.) THEN
  CALL scrive_help
  STOP 1
ENDIF

!--------------------------------------------------------------------------
! 1.2 Parametri dipendenti dal formato dei dati; codice per EOF

IF (inp_data == "hhr") THEN
  head_offset = 13
  nhead = 3
  rmis = rmis_hhr
ELSE IF (inp_data == "ser") THEN
  head_offset = 17
  nhead = 6
  rmis = rmis_ser
ELSE IF (inp_data == "sex") THEN
  head_offset = 17
  nhead = 6
  rmis = rmis_sex
ELSE IF (inp_data == "ddy") THEN
  head_offset = 10
  nhead = 3
  rmis = rmis_ddy
ELSE IF (inp_data == "tem") THEN
  head_offset = 19
  nhead = 3
  rmis = rmis_tem
ENDIF

CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 1.3 Apro i files di input

OPEN (UNIT=iulst, FILE=filelst, STATUS="OLD", FORM="FORMATTED", ERR=9999)
DO nf = 1,HUGE(0)
  READ (iulst,'(a)',IOSTAT=ios) filein
  IF (ios == eof) EXIT
  OPEN (UNIT=iu0+nf, FILE=filein, STATUS="OLD", FORM="FORMATTED", ERR=9998)
ENDDO  
nf = nf - 1
CLOSE (iulst)
IF (nf < 1) GOTO 9989

!--------------------------------------------------------------------------
! 1.4 Leggo gli header (parametri e livelli) dei files in input e controllo
!     che siano uguali per tutti i files

nld = 0
DO kf = 1,nf

! Leggo gli header
  IF (inp_data == "hhr") THEN
    READ (iu0+kf,'(a)',ERR=9997) head_staz
    READ (iu0+kf,*,ERR=9997)
    READ (iu0+kf,'(a)', ERR=9997) head_par
    head_liv = ""
  
  ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
    READ (iu0+kf,'(a)',ERR=9997) head_staz
    READ (iu0+kf,*,ERR=9997)
    READ (iu0+kf,'(a)',ERR=9997) head_model
    READ (iu0+kf,'(a)',ERR=9997) head_liv
    READ (iu0+kf,*,ERR=9997)
    READ (iu0+kf,'(a)',ERR=9997) head_par
  
  
  ELSE IF (inp_data == "ddy") THEN
    READ (iu0+kf,'(a)',ERR=9997) head_staz
    READ (iu0+kf,*,ERR=9997)
    READ (iu0+kf,'(a)', ERR=9997) head_par
    head_liv = ""
  
  ELSE IF (inp_data == "tem") THEN
    READ (iu0+kf,'(a)',ERR=9997) head_liv
    READ (iu0+kf,*,ERR=9997)
    READ (iu0+kf,'(a)', ERR=9997) head_par
  
  ENDIF

! Controllo che siano uguali a quelli del primo file
  IF (kf == 1) THEN
    head_liv1 = head_liv
    head_par1 = head_par
  ELSE
    IF (TRIM(head_liv1) /= TRIM(head_liv)) THEN
      IF (ld) THEN
        nld = nld + 1
      ELSE
        GOTO 9996
      ENDIF
    ENDIF
    IF (TRIM(head_par1) /= TRIM(head_par)) GOTO 9995
  ENDIF

! Se ha senso farlo, salvo gli header stazione e modello del primo file,
! per poterli scrivere nel file di ouput
  IF (kf == 1) THEN
    head_staz_out = ""
    head_model_out = ""
    IF (nf == 1 .OR. stat=="qcs") head_staz_out = head_staz
    IF (nf == 1 .AND. (inp_data == "ser" .OR. inp_data == "sex")) &
      head_model_out = head_model
  ENDIF

ENDDO

! Calcolo il numero dei parametri
head_len = LEN(TRIM(head_par1))
IF (MOD(head_len - head_offset, fw + 1) /= 0) THEN
  GOTO 9994
ELSE
  npar = (head_len - head_offset) / (fw + 1)
ENDIF

! Leggo le stringhe identificative dei parametri
idpar(:) = ""
DO kp = 1, npar
  p1 = head_offset + (fw+1)*(kp-1) + 2
  p2 = head_offset + (fw+1)*kp
  READ (head_par1(p1:p2),'(a)') idpar(kp)
ENDDO

!--------------------------------------------------------------------------
! 1.5 Controllo che i dati in input siano compatibili con l'elaborazione
!     richiesta; se necessario, modifico gli header in output

head_par_out = head_par1
head_liv_out = head_liv1

IF ((stat=="tke" .OR. stat=="dst" .OR. stat=="qcc") .AND. nf/=1) THEN
  WRITE (*,'(3a)') "La statistica ",stat," richiede un solo file in input"
  GOTO 9991
ENDIF

IF (stat=="qcs" .AND. nf<2) THEN
  WRITE (*,'(3a)') "La statistica ",stat," richiede almeno 2 files in input"
  GOTO 9991
ENDIF

IF (stat=="tke") THEN
  DO kp = 1,npar
    IF (INDEX(idpar(kp),"tke") == 0) THEN
      WRITE (*,*) "Trovato parametro non tke: ",kp
      GOTO 9991
    ENDIF
  ENDDO

  DO kp = 1, npar
    p1 = head_offset + (fw+1)*(kp-1) + 2
    p2 = head_offset + (fw+1)*kp
    WRITE (head_par_out(p1:p2),'(a)') "  real-tke"
  ENDDO
ENDIF

!--------------------------------------------------------------------------
! 1.6 Se necessario, leggo il file con i dati di supporto per la statistica 
!     richiesta

! 1.6.1 Frazione di province con superamenti (pex)
IF (stat == "pex") THEN
  ALLOCATE (prvidx(nf),prvlab(nf))
  OPEN (UNIT=iulst, FILE=filesta, STATUS="OLD", FORM="FORMATTED", ERR=9988)

  nprv = 0
  prvidx(:) = 0
  prvlab(:) = ""
  DO kp = 1,nf
    READ (iulst,'(a)',IOSTAT=ios) ch10
    IF (ios /= 0) GOTO 9987
    IF (TRIM(ch10) == "") GOTO 9985
    DO k2 = 1,nprv
      IF (TRIM(ADJUSTL(ch10)) == prvlab(k2)) THEN
        prvidx(kp) = k2
        EXIT
      ENDIF
    ENDDO
    IF (k2 == nprv + 1) THEN
      nprv = nprv + 1
      prvidx(kp) = nprv
      prvlab(nprv) = ADJUSTL(ch10)
    ENDIF
  ENDDO
  READ (iulst,'(a)',IOSTAT=ios) ch10
  IF (ios /= eof) GOTO 9986
  CLOSE (iulst)
  ALLOCATE (pexc(nprv,npar))

  WRITE (*,*) "Completata la lettura di ",TRIM(filesta)
  WRITE (*,*) "Punti: ",nf,"; province diverse: ",nprv
  DO kpr = 1,nprv
    WRITE (*,'(3x,a10,1x,i3)') prvlab(kpr),COUNT(prvidx(1:nf)==kpr)
  ENDDO
ENDIF

! 1.6.2 Andamento de-stagionalizzato
IF (stat == "dst") THEN
  ALLOCATE (yrtyp(12,npar))  

! Per ogni parametro, leggo da filesta le medie relative a ciascun mese
  WRITE (chfmt3,'(a,2(i3.3,a))') "(10x,",npar,"(1x,f",fw,".1))"
  OPEN (UNIT=iulst, FILE=filesta, STATUS="OLD", FORM="FORMATTED", ERR=9988)
  READ (iulst,*)
  READ (iulst,*)
  READ (iulst,'(a)') head_par_sta
  IF (head_par_sta(11:10+fw*npar) /= &
      head_par(head_offset+1:head_offset+fw*npar)) GOTO 9984
  DO km = 1,12
    READ (iulst,chfmt3) yrtyp(km,1:npar)
  ENDDO
  CLOSE (iulst)

!! Per ogni parametro, calcolo le anomalie mensili (differenze tra la media
!! di ciascun mese e la media totale) 
!  DO kp = 1,npar
!    IF (ALL(yrtyp(1:12,kp) /= rmis)) THEN
!      yrtyp(1:12,kp) = yrtyp(1:12,kp) - SUM(yrtyp(1:12,kp))/12.
!    ELSE
!      yrtyp(1:12,kp) = rmis
!    ENDIF
!  ENDDO

ENDIF

! 1.6.3 Controllo di qualita' climatologico
IF (stat == "qcc") THEN
  ALLOCATE (yrtyp(12,npar),yrtypstd(12,npar),cli_max_diff(12,npar))  

! Per ogni parametro, leggo da filesta media e std relative a ciascun mese
  WRITE (chfmt3,'(a,2(i3.3,a))') "(10x,",npar,"(1x,f",fw,".1))"
  OPEN (UNIT=iulst, FILE=filesta, STATUS="OLD", FORM="FORMATTED", ERR=9988)
  READ (iulst,*)
  READ (iulst,*)
  READ (iulst,'(a)') head_par_sta
  IF (head_par_sta(11:10+fw*npar) /= &
      head_par(head_offset+1:head_offset+fw*npar)) GOTO 9984

  DO km = 1,12    ! medie mensili
    READ (iulst,chfmt3) yrtyp(km,1:npar)
  ENDDO
  DO ksk = 1,56   ! skip max min nok
    READ (iulst,*)
  ENDDO
  DO km = 1,12    ! std mensili
    READ (iulst,chfmt3) yrtypstd(km,1:npar)
  ENDDO

! Per ogni mese e per ogni parametro, calcolo la massima differenza 
! ammissibile ripsetto al clima
  WHERE (yrtyp(1:12,1:npar) /= rmis .AND. yrtypstd(1:12,1:npar) /= rmis)
    cli_max_diff(1:12,1:npar) = &
      MAX(qcx*yrtypstd(1:12,1:npar), qcy*yrtyp(1:12,1:npar), qcz)
  ELSEWHERE
    cli_max_diff(1:12,1:npar) = rmis
  ENDWHERE

  CLOSE (iulst)
ENDIF

!--------------------------------------------------------------------------
! 1.7 Costruisco i formati per i dati in input (chftm1) e output (chfmt2)

IF (inp_data == "hhr") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i4,3i3,",npar,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(i4.4,3(1x,i2.2),", &
    npar,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_data == "ddy") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i4,2i3,",npar,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(i4.4,2(1x,i2.2),", &
    npar,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_data == "tem") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i4,3i3,6x,",npar,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(i4.4,3(1x,i2.2),6x,", &
    npar,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_data == "ser") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i2,1x,i2,1x,i4,1x,i2,1x,i3,", &
    npar,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(2(i2.2,a1),i4.4,1x,i2.2,1x,i3.3,", &
    npar,"(1x,f",fw,".",out_dec,"))"

ELSE IF (inp_data == "sex") THEN
  WRITE (chfmt1,'(a,2(i3.3,a))') "(i2,1x,i2,1x,i4,1x,i2,1x,i3,", &
    npar,"(1x,a",fw,"))"
  WRITE (chfmt2,'(a,3(i3.3,a))') "(2(i2.2,a1),i4.4,1x,i2.2,1x,i3.3,", &
    npar,"(1x,e",fw,".",3,"))"

ENDIF

!--------------------------------------------------------------------------
! 1.8 Alloco gli arrays (esclusi quelli relativi a filesta - se richiesti)

ALLOCATE (val_in(nf,npar),val_ok(nf))
ALLOCATE (val_out(npar),chval(npar),nok(npar),ave(npar),ave2(npar))

!--------------------------------------------------------------------------
! 1.9 Apro il file di output e scrivo gli header

OPEN (UNIT=iuout, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")

IF (inp_data == "hhr") THEN
  WRITE (iuout,'(a)') TRIM(head_staz_out)
  WRITE (iuout,*)
  WRITE (iuout,'(a)') TRIM(head_par_out)
  head_liv = ""

ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
  WRITE (iuout,'(a)') TRIM(head_staz_out)
  WRITE (iuout,*)
  WRITE (iuout,'(a)') TRIM(head_model_out)
  WRITE (iuout,'(a)')  TRIM(head_liv_out)
  WRITE (iuout,*)
  WRITE (iuout,'(a)')  TRIM(head_par_out)

ELSE IF (inp_data == "ddy") THEN
  WRITE (iuout,'(a)') TRIM(head_staz_out)
  WRITE (iuout,*)
  WRITE (iuout,'(a)')  TRIM(head_par_out)
  head_liv = ""

ELSE IF (inp_data == "tem") THEN
  WRITE (iuout,'(a)')  TRIM(head_liv_out)
  WRITE (iuout,*)
  WRITE (iuout,'(a)') TRIM(head_par_out)

ENDIF

!--------------------------------------------------------------------------
! 1.10 Se previsto, apro il file di log

IF (stat == "qcc" .OR. stat == "qcs") THEN
  OPEN (UNIT=iulog, FILE="proc_multi_orari.log", STATUS="REPLACE")
  WRITE (chfmt4,'(3(a,i2),a)') &
    "(i4,1x,a",fw,",1x,i4.4,3i2.2,5(1x,f",fw,".",out_dec,"))"
  WRITE (chfmt5,'(2(a,i2),a)') "(a4,1x,a",fw,",1x,a10,5(1x,a",fw,"))"
  WRITE (iulog,chfmt5) &
    "npar","idpar","yyyymmddhh","valore","riferim.","std","tolleranza","delta"
ENDIF

!==========================================================================
! 2) Leggo/scrivo (ciclo sui record in input)

record: DO kr = 1,HUGE(0)

!--------------------------------------------------------------------------
! 2.1 Leggo il prossimo record da tutti i files

  val_in(:,:) = rmis
  DO kf = 1,nf
    IF (inp_data == "hhr") THEN
      READ (iu0+kf,chfmt1,IOSTAT=ios) yy,mm,dd,hh,chval(1:npar)
      IF (ios == eof) EXIT record
      IF (ios /= 0) GOTO 9993
      DO kp = 1,npar
        READ (chval(kp),*,ERR=9992) val_in(kf,kp)
      ENDDO
  
    ELSE IF (inp_data == "ddy") THEN
      READ (iu0+kf,chfmt1,IOSTAT=ios) yy,mm,dd,chval(1:npar)
      IF (ios == eof) EXIT record
      hh = 0
      IF (ios /= 0) GOTO 9993
      DO kp = 1,npar
        READ (chval(kp),*,ERR=9992) val_in(kf,kp)
      ENDDO
  
    ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
      READ (iu0+kf,chfmt1,IOSTAT=ios) dd,mm,yy,hh,sca,chval(1:npar)
      IF (ios == eof) EXIT record
      IF (ios /= 0) GOTO 9993
      DO kp = 1,npar
        READ (chval(kp),*,ERR=9992) val_in(kf,kp)
      ENDDO

    ELSE IF (inp_data == "tem") THEN
      READ (iu0+kf,chfmt1,IOSTAT=ios) yy,mm,dd,hh,chval(1:npar)
      IF (ios == eof) EXIT record
      IF (ios /= 0) GOTO 9993
      DO kp = 1,npar
        READ (chval(kp),*,ERR=9992) val_in(kf,kp)
      ENDDO

    ENDIF
  ENDDO

!--------------------------------------------------------------------------
! 2.2 Eseguo l'eleborazione statistica richiesta

! 2.2.0 Se richiesto, metto a 0 i dati mancanti
  IF (miss0) THEN
    WHERE (val_in(1:nf,1:npar) == rmis)   
      val_in(1:nf,1:npar) = 0.
    ENDWHERE
  ENDIF

  nok(1:npar) = COUNT(val_in(1:nf,1:npar) /= rmis, DIM=1)

! 2.2.1 Elaborazioni standard

  IF (stat == "ave") THEN
    WHERE (nok(1:npar) > 0)
      val_out(1:npar) = SUM(val_in(1:nf,1:npar), DIM=1, &
        MASK=val_in(1:nf,1:npar)/= rmis) / REAL(nok(1:npar))
    ELSEWHERE
      val_out(1:npar) = rmis
    ENDWHERE

  ELSE IF (stat == "max") THEN
    WHERE (nok(1:npar) > 0)
      val_out(1:npar) = MAXVAL(val_in(1:nf,1:npar), DIM=1, &
        MASK=val_in(1:nf,1:npar)/= rmis)
    ELSEWHERE
      val_out(1:npar) = rmis
    ENDWHERE

  ELSE IF (stat == "min") THEN
    WHERE (nok(1:npar) > 0)
      val_out(1:npar) = MINVAL(val_in(1:nf,1:npar), DIM=1, &
        MASK=val_in(1:nf,1:npar)/= rmis)
    ELSEWHERE
      val_out(1:npar) = rmis
    ENDWHERE

  ELSE IF (stat == "std") THEN
    WHERE (nok(1:npar) > 0)
      ave(1:npar) = SUM(val_in(1:nf,1:npar), DIM=1, &
        MASK=val_in(1:nf,1:npar)/= rmis) / REAL(nok(1:npar))
      ave2(1:npar) = SUM(val_in(1:nf,1:npar)**2, DIM=1, &
        MASK=val_in(1:nf,1:npar)/= rmis) / REAL(nok(1:npar))
      val_out(1:npar) = SQRT(MAX(0.,ave(1:npar) - ave2(1:npar)))
    ELSEWHERE
      val_out(1:npar) = rmis
    ENDWHERE

  ELSE IF (stat == "mdn") THEN
    DO kp = 1,npar
      IF (nok(kp) > 0) THEN

!       Salvo i dati validi nel vettore val_ok
        idx = 0
        DO kf = 1,nf
          IF (val_in(kf,kp) /= rmis) THEN
            idx = idx + 1
            val_ok(idx) = val_in(kf,kp)
          ENDIF
        ENDDO
        IF (idx /= nok(kp)) WRITE (*,*) &
          "Errore sorting valori: kp,nok1,nok2 ",kp,nok(kp),idx
   
!       Ordino i valori
        CALL sort(val_ok(1:idx))

!       Prendo la mediana
        IF (MOD(idx,2) == 0) THEN
          val_out(kp) = (val_ok(idx/2) + val_ok(idx/2+1)) / 2.
        ELSE
          val_out(kp) = val_ok(idx/2+1)
        ENDIF

      ELSE
        val_out(kp) = rmis

      ENDIF
    ENDDO

! 2.2.2 elaborazioni custom

  ELSE IF (stat == "tke") THEN
    WHERE (nok(1:npar) > 0)
      val_out(1:npar) = 0.5 * val_in(1,1:npar) * val_in(1,1:npar)
    ELSEWHERE
      val_out(1:npar) = rmis
    ENDWHERE

  ELSE IF (stat == "dst") THEN
    WHERE (nok(1:npar) > 0 .AND. yrtyp(mm,1:npar) /= rmis)
      val_out(1:npar) = val_in(1,1:npar) - yrtyp(mm,1:npar)
    ELSEWHERE
      val_out(1:npar) = rmis
    ENDWHERE

  ELSE IF (stat == "qcc") THEN
    val_out(1:npar) = val_in(1,1:npar)
    DO kp = 1,npar
      IF (val_in(1,kp) /= rmis .AND. cli_max_diff(mm,kp) /= rmis .AND. &
           ABS(val_in(1,kp)-yrtyp(mm,kp)) > cli_max_diff(mm,kp)) THEN
        val_out(kp) = rmis
        WRITE (iulog,chfmt4) kp,idpar(kp),yy,mm,dd,hh,val_in(1,kp), &
          yrtyp(mm,kp),yrtypstd(mm,kp),cli_max_diff(mm,kp), &
          val_in(1,kp)-yrtyp(mm,kp)
      ENDIF
    ENDDO

  ELSE IF (stat == "qcs") THEN
    val_out(1:npar) = val_in(1,1:npar)
    DO kp = 1,npar
      sp_nok = COUNT(val_in(2:nf,kp) /= rmis)
      IF (val_in(1,kp) /= rmis .AND. sp_nok > 0) THEN
        sp_sum = SUM(val_in(2:nf,kp), MASK = val_in(2:nf,kp)/=rmis)
        sp_sum2 = SUM(val_in(2:nf,kp)**2, MASK = val_in(2:nf,kp)/=rmis)
        sp_ave = sp_sum / REAL(sp_nok)
        sp_std =  SQRT(MAX(0., sp_sum2/REAL(sp_nok) - sp_ave**2))
        sp_max_diff = MAX(qcx*sp_std, qcy*sp_ave, qcz)
        IF (ABS(val_in(1,kp)-sp_ave) > sp_max_diff) THEN
          val_out(kp) = rmis
          WRITE (iulog,chfmt4) kp,idpar(kp),yy,mm,dd,hh, &
            val_in(1,kp),sp_ave,sp_std,sp_max_diff,val_in(1,kp)-sp_ave
        ENDIF
      ENDIF
    ENDDO

! Elaborazione pex: il valore in output e' mancante solo se sono mancanti 
! tutti i dati in input e non e' stata specificata la flag -miss0. In tutti
! i casi, il dato viene diviso per il numero totale di province, anche se
! alcune hanno tutti i dati mancanti.

  ELSE IF (stat == "pex") THEN
    pexc(:,:) = .FALSE.
    DO kf = 1,nf
      WHERE (val_in(kf,1:npar) /= rmis .AND. val_in(kf,1:npar) > 50)
        pexc(prvidx(kf),1:npar) = .TRUE.
      ENDWHERE
    ENDDO

    WHERE (nok(1:npar) > 0)
      val_out(1:npar) = REAL(COUNT(pexc(1:nprv,1:npar),DIM=1)) / REAL(nprv)
    ELSEWHERE
      val_out(1:npar) = rmis
    ENDWHERE

  ENDIF

!--------------------------------------------------------------------------
! 2.3 Scrivo un record sul file di output

  IF (inp_data == "hhr") THEN
    WRITE (iuout,chfmt2,IOSTAT=ios) yy,mm,dd,hh,val_out(1:npar)

  ELSE IF (inp_data == "ddy") THEN
    WRITE (iuout,chfmt2,IOSTAT=ios) yy,mm,dd,val_out(1:npar)

  ELSE IF (inp_data == "ser" .OR. inp_data == "sex") THEN
    WRITE (iuout,chfmt2,IOSTAT=ios) dd,"/",mm,"/",yy,hh,sca,val_out(1:npar)

  ELSE IF (inp_data == "tem") THEN
    WRITE (iuout,chfmt2,IOSTAT=ios) yy,mm,dd,hh,val_out(1:npar)

  ENDIF
  IF (ios /= 0) GOTO 9990

ENDDO record

!==========================================================================
! 3) Conclusione

WRITE (*,'(a,2(i4,a),i6,a)') "Elaborati ",nf," files, ", &
  npar," parametri ",kr-1," istanti"
IF (ld .AND. nld > 0) WRITE (*,'(a,i3,a)') &
  "Warning: le elaborazioni mescolano dati su livelli diversi (in ", &
  nld," files)"

STOP

!==========================================================================
! 4) Gestione errori

9000 CONTINUE
WRITE (*,*) "Il file di input e quello di maschera devono avere lo stesso step temporale"
STOP 10

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filelst)
STOP 1

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore leggendo header, file ",kf
STOP 3

9996 CONTINUE
WRITE (*,*) "Header livelli nel file ",kf," diverso da quello del primo file"
WRITE (*,*) "Header file ",kf,":"
WRITE (*,'(a)') TRIM(head_liv)
WRITE (*,*) "Header primo file:"
WRITE (*,'(a)') TRIM(head_liv1)
STOP 4

9995 CONTINUE
WRITE (*,*) "Header parametri nel file ",kf," diverso da quello del primo file"
STOP 5

9994 CONTINUE
WRITE (*,*) "L'header parametri del primo file e' troncato o ha campi con ampiezza /= ",fw
WRITE (*,*) "lughezza header: ",head_len,", offset: ",head_offset,", fw: ",fw
STOP 6

9993 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," linea ",kr+nhead
STOP 7

9992 CONTINUE
WRITE (*,*) "Errore nella conversione CHAR->REAL ",TRIM(filein), &
  " linea ",kr+nhead
STOP 8

9991 CONTINUE
WRITE (*,*) "Dati di input incompatibili con l'elaborazione richiesta"
STOP 9

9990 CONTINUE
WRITE (*,*) "Errore di scrittura dati, record ",kr

9989 CONTINUE
WRITE (*,*) "Errore, ",TRIM(filein)," e' vuoto"
STOP 2

9988 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filesta)
STOP 1

9987 CONTINUE
WRITE (*,*) "Errore: ",TRIM(filesta)," contiene meno punti di ",TRIM(filelst)
STOP 10

9986 CONTINUE
WRITE (*,*) "Errore: ",TRIM(filesta)," contiene piu' punti di ",TRIM(filelst)
STOP 10

9985 CONTINUE
WRITE (*,*) "Errore: riga vuota in ",TRIM(filesta)
STOP 10

9984 CONTINUE
WRITE (*,*) "Errore: header parametri diversi in filein e filesta"
WRITE (*,'(a)') head_par(head_offset+1:head_offset+fw*npar)
WRITE (*,'(a)') head_par_sta(11:10+fw*npar)
STOP 11

END PROGRAM proc_multi_orari

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
WRITE (*,*) "proc_multi_orari.exe [-h] -o/-s/-sx/-d/-t filelst stat fileout [filesta]"
WRITE (*,*) "   [-nedc N] [-ld] [-miss0] [-qcpar X Y Z]"
WRITE (*,*) "Legge un insieme di files relativi a serie storiche su punto"
WRITE (*,*) "Scrive un file con lo stesso formato, che per ogni data/parametro contiene"
WRITE (*,*) "  il risultato di una statistica sui files di input."
WRITE (*,*) 
WRITE (*,*) "filelst  : lista dei files da elaborare. Il parametro successivo determina"
WRITE (*,*) "           il loro formato:"
WRITE (*,*) " -o      : estra_orari o estra_qaria con dati orari"
WRITE (*,*) " -s      : seriet con notazione decimale"
WRITE (*,*) " -sx     : seriet con notazione esponenziale"
WRITE (*,*) " -d      : estra_qaria con dati giornalieri o segmento di file .sta "
WRITE (*,*) " -t      : input nel formato prodotto da trasp_temp"
WRITE (*,*) " -q      : come -d (per compatibilita' con vecchie procedure)"
WRITE (*,*) ""
WRITE (*,*) "stat     : elaborazione da compiere sui dati di input:"
WRITE (*,*) "           standard: ave, max, min, std, mdn"
WRITE (*,*) "           ""custom"": "
WRITE (*,*) "           - tke: un solo file in input, passa da SQRT(2*TKE) a TKE"
WRITE (*,*) "           - pex: calcola la frazione di province in cui almeno un punto e' >50"
WRITE (*,*) "               filesta ha le stesse righe di filelst, e contiene l'id provincia"
WRITE (*,*) "               (stringa, max 10 char) di ciascuno dei punti"
WRITE (*,*) "           - dst: un solo file di input, toglie a tutti i dati validi un valore"
WRITE (*,*) "               climatologico mensile; filesta e' in formato yrtyp.sta, e ha le "
WRITE (*,*) "               stesse colonne del file di input"
WRITE (*,*) "           - qcc: un solo file di input, lo riscrive mettendo mancanti i dati che "
WRITE (*,*) "               differiscono dal clima del mese corrente per piu' di"
WRITE (*,*) "               max(X*stdev, Y*|media|, Z); X,Y,Z>=0; default: X=3., Y=1., Z=0."
WRITE (*,*) "               filesta e' un file yrtyp.sta prodotto con l'opzione -std"
WRITE (*,*) "           - qcs: riscrive i dati del primo file, mettendo mancanti i dati che"
WRITE (*,*) "               differiscono dalla media degli altri per piu' di"
WRITE (*,*) "               max(X*stdev, Y*|media|, Z); X,Y,Z>=0; default: X=3., Y=1., Z=0."
WRITE (*,*) ""
WRITE (*,*) "filesta  : file di appoggio per l'elaborazione statstica (se richiesto)"
WRITE (*,*) "fileout  : file di output"
WRITE (*,*) "-nedc N  : numero di decimali in output (def: 2; con input sex sempre 3)"
WRITE (*,*) "-ld      : non controlla la corrispondenza dei livelli nei files di input"
WRITE (*,*) "           (utile per elaborare dati sui model layers COSMO)"
WRITE (*,*) "-miss0   : Considera i dati mancanti come se fossero 0 (utile per il calcolo"
WRITE (*,*) "           dei superamenti PM10)"
WRITE (*,*) "-qcpar X Y Z: modifica i parametri del controllo di qualita' (stat = qcc o qcs)"
WRITE (*,*) "-h       : visualizza questo help"
WRITE (*,*) 

RETURN

END SUBROUTINE scrive_help


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

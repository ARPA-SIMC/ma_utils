PROGRAM gacsv_scan
!--------------------------------------------------------------------------
! Legge un file grib_api_csv col tracciato per estrazioni arkimet su punto
! (vedi ak_seriet.ksh) e scrive:
! - filepts (*.pts.csv): un record per ogni punto presente nel file
! - filecol (*.varliv.csv): un record per ogni combinazione 
!   par-liv presente nel file di input, con tutte le inforamzioni 
!   necessarie per scrivere i dati in formato seriet
! - filerow (*.datascad.csv): massimo, minimo e step di reference times e 
!   timerange presenti nel file.
!
! Note:
! - per scrivere filecol cerca i files tabella_xxx_ser.txt; se non li 
!   trova usa dei default.
! - per eleborare i timerange, considero solo P1 (termine del periodo di 
!   validita' del dato); quindi:
!   a) se sono presenti previsioni cumulate/mediate su intervalli temporali 
!      diversi (es. cumulata +6/+12 e +00/+12) il programma da' errore
!   b) se per un dato parametro/livello sono presenti piu' dati con lo 
!      stesso valore di P1 e diversi valori di P2 o timerange, il 
!      programma  da' un warning, e non sara' possibile convertire i dati
!      in formato seriet.
!
!                                         Versione 1.3.2, Enrico 17/10/2013
!--------------------------------------------------------------------------

USE file_utilities
USE datetime_class
USE seriet_utilities

IMPLICIT NONE

INTERFACE
SUBROUTINE sort_tab_i(array)
  INTEGER, INTENT(INOUT) :: array(:,:)
END SUBROUTINE sort_tab_i
END INTERFACE

!--------------------------------------------------------------------------
! Variabili locali
TYPE (csv_record) :: colcsv,ptscsv,rowcsv
TYPE (datetime) :: reftime_c,reftime_max,reftime_min,reftime_p
TYPE (timedelta) :: tdelta_c,tdelta_min
TYPE (gacsv_report) :: report
REAL :: vmin,vmax,pts_lon(maxpt),pts_lat(maxpt)
INTEGER :: idp,k,kp,kpt,kvl,ktr,kvltr,ios,ier(14),iret,idum,ndef
INTEGER :: yy,mm,dd,hh,minute,tdh,ndec,cp2,p1_max,p1_min,p1_step
INTEGER :: cnt_seg,cnt_dat_seg,cnt_dat_ok,ntr,nvl,nvltr,npt
INTEGER :: tr(3),vl(6),trange(3,maxtr),varliv(6,maxvl),dltr(maxtr)
INTEGER :: varlivtr(9,maxvltr),pts_list(maxpt)
CHARACTER (LEN=500) :: chrec,filein,filepts,filecol,filerow,chdum
CHARACTER (LEN=10) :: str_model,str_var,chfmt
CHARACTER (LEN=20) :: ch20
CHARACTER (LEN=10) :: ch10
CHARACTER (LEN=1) :: next_arg
LOGICAL :: out_pts,out_col,out_row,lseriet,ltdelta,verbose

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
idp = 0
next_arg = ""
out_pts = .FALSE.
out_col = .FALSE.
out_row = .FALSE.
verbose=.FALSE.
filein = ""
filepts = ""
filecol = ""
filerow = ""
str_model = "arkimet"

DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-verbose") THEN
    verbose = .TRUE.
  ELSE IF (TRIM(chdum) == "-p") THEN
    next_arg = "p"
    CYCLE
  ELSE IF (TRIM(chdum) == "-c") THEN
    next_arg = "c"
    CYCLE
  ELSE IF (TRIM(chdum) == "-r") THEN
    next_arg = "r"
    CYCLE
  ELSE IF (TRIM(chdum) == "-m") THEN
    next_arg = "m"
    CYCLE
  ELSE IF (next_arg == "p") THEN
    filepts = chdum
    out_pts = .TRUE.
    next_arg = ""
  ELSE IF (next_arg == "c") THEN
    filecol = chdum
    out_col = .TRUE.
    next_arg = ""
  ELSE IF (next_arg == "r") THEN
    filerow = chdum
    out_row = .TRUE.
    next_arg = ""
  ELSE IF (next_arg == "m") THEN
    str_model = ADJUSTL(chdum)
    next_arg = ""
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE DEFAULT
      GOTO 9999
    END SELECT
  ENDIF
ENDDO

!--------------------------------------------------------------------------
! 2) Leggo filein e salvo le inforamzioni relative al suo contenuto

! Inizializzio i contatori
nvl = 0
ntr = 0
npt = 0
nvltr = 0
varliv(:,:) = 0
trange(:,:) = 0
cnt_seg = 0
cnt_dat_seg = 0
cnt_dat_ok = 0
reftime_min = datetime_max
reftime_max = datetime_min
tdelta_min = timedelta_max
ltdelta = .FALSE.

! Ciclo sui record in input
OPEN (UNIT=20, FILE=filein, STATUS="OLD", ERR=9998)
DO k = 1,HUGE(0)
  READ (20,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT
  IF (TRIM(chrec) == "") CYCLE
  IF (cnt_seg == 0) cnt_seg = 1    ! filein contiene almeno un record
  IF (verbose .AND. MOD(k,100000) == 0) WRITE (*,*) "Elaboro record ",k

  idum = INDEX(chrec,"date")
  IF (idum /= 0) THEN                        ! Header di segmento
    cnt_seg = cnt_seg + 1
    cnt_dat_seg = 0
    CALL test_header("gacsv",chrec,iret)
    IF (iret > 0) GOTO 9997

  ELSE                                       ! Record di dati
    cnt_dat_seg = cnt_dat_seg + 1
    CALL gacsv_rep_read(chrec,report,iret)
    IF (iret /= 0) GOTO 9996
    cnt_dat_ok = cnt_dat_ok + 1
    tr(1:3) = (/report%datascad%p1,report%p2,report%timerange/)
    vl(1:6) = report%varliv(1:6)

!   Controllo se ho trovato un nuovo punto
    IF (ALL(report%np /= pts_list(1:npt))) THEN
      npt = npt + 1
      IF (npt > maxpt) GOTO 9995
      pts_list(npt) = report%np
      pts_lon(npt) = REAL(report%lon)
      pts_lat(npt) = REAL(report%lat)
    ENDIF

!   Controllo se ho trovato un nuovo var-liv
    DO kvl = 1,nvl
      IF (ALL(vl(1:6) == varliv(1:6,kvl))) EXIT
    ENDDO
    IF (kvl == nvl + 1) THEN
      nvl = nvl + 1
      IF (nvl > maxvl) GOTO 9995
      varliv(1:6,nvl) = vl(1:6)
    ENDIF

!   Controllo se ho trovato un nuovo timerange
    DO ktr = 1,ntr
      IF (ALL(tr(1:3) == trange(1:3,ktr))) EXIT
    ENDDO
    IF (ktr == ntr + 1) THEN
      ntr = ntr + 1
      IF (ntr > maxtr) GOTO 9995
      trange(1:3,ktr) = tr(1:3)
    ENDIF

!   Controllo se ho trovato un nuovo var-liv-timerange
    DO kvltr = 1,nvltr
      IF (ALL(vl(1:6) == varlivtr(1:6,kvltr)) .AND. &
          ALL(tr(1:3) == varlivtr(7:9,kvltr))) EXIT
    ENDDO
    IF (kvltr == nvltr + 1) THEN
      IF (nvltr > maxvltr) GOTO 9995
      nvltr = nvltr + 1
      varlivtr(1:6,nvltr) = vl(1:6)
      varlivtr(7:9,nvltr) = tr(1:3)
    ENDIF

!   Cerco max, min e minima differenza del reference time
    reftime_c = report%datascad%reftime
    IF (reftime_c > reftime_max) reftime_max = reftime_c
    IF (reftime_c < reftime_min) reftime_min = reftime_c

    IF (k > 1) THEN
      tdelta_c = reftime_c - reftime_p
      IF (tdelta_c > timedelta_0 .AND. tdelta_c < tdelta_min) THEN
        tdelta_min = tdelta_c    
        ltdelta = .TRUE.
      ENDIF
    ENDIF

    reftime_p = reftime_c
  ENDIF

ENDDO

!--------------------------------------------------------------------------
! 3) Elaborazioni relative a timerange e reference time

! Se c'e' un solo reference time, metto tdelta a 0
IF (ltdelta) THEN
  CALL getval(tdelta_min,AHOUR=tdh)
ELSE
  tdh = 0
ENDIF

! Se il file non e' vuoto, elaboro i timerange
IF (ntr > 0) THEN

! Verifico se e' possibile scrivere l'output in formato seriet (bisogna che
! le n-ple Var-Liv-P1 siano uniche)
  CALL sort_tab_i(varlivtr(:,1:nvltr))

  lseriet = .TRUE.
  DO kvltr = 1,nvltr-1
    IF (ALL(varlivtr(1:7,kvltr)==varlivtr(1:7,kvltr+1))) THEN
      WRITE (*,*) "Trovati dati con stesso par, liv e P1, ma " // &
        " diverso P2 o timerange" 
      WRITE (*,'(2a)') "   cem,   tab,   var,  level1,    l1,    l2,", &
        "      p1,    p2,trange"
      WRITE (*,'(3(i6,1x),2x,3(i6,1x),2x,3(i6,1x))') varlivtr(1:9,kvltr)
      WRITE (*,'(3(i6,1x),2x,3(i6,1x),2x,3(i6,1x))') varlivtr(1:9,kvltr+1)
      lseriet = .FALSE.
    ENDIF
  ENDDO

  IF (lseriet) THEN
!   Trov max, min, step di P1
    CALL sort_tab_i(trange(:,1:ntr))
    p1_min = MINVAL(trange(1,1:ntr))
    p1_max = MAXVAL(trange(1,1:ntr))
    IF (ntr == 1 .OR. ALL(trange(1,2:ntr) == trange(1,1))) THEN
      p1_step = 0
    ELSE
      p1_step = MINVAL(trange(1,2:ntr)-trange(1,1:ntr-1), &
         MASK = trange(1,2:ntr)/=trange(1,1:ntr-1))
    ENDIF
  ENDIF

ENDIF

!--------------------------------------------------------------------------
! 4) Scrittura output

! 4.1) Punti
IF (out_pts) THEN
  WRITE (chfmt,'(a,i2,a,i2,a)') "(f",coo_ndec+5,".",coo_ndec,")"
  OPEN (UNIT=30, FILE=filepts, STATUS="REPLACE")
  chrec = ""
  CALL build_header("lspts",chrec)
  WRITE (30,'(a)') TRIM(chrec)

  DO kpt = 1,npt
    CALL init(ptscsv)
    WRITE (ch20,chfmt) pts_lon(kpt)
    CALL csv_record_addfield(ptscsv,TRIM(ADJUSTL(ch20)))
    WRITE (ch20,chfmt) pts_lat(kpt)
    CALL csv_record_addfield(ptscsv,TRIM(ADJUSTL(ch20)))
    WRITE (ch10,'(a6,i3.3)') "punto_",kpt
    CALL csv_record_addfield(ptscsv,TRIM(ADJUSTL(ch10)))
    WRITE (30,'(a)') csv_record_getrecord(ptscsv)
    CALL delete(ptscsv)
  ENDDO
  CLOSE(30)
ENDIF

! 4.2) Variabili e livelli (colonne) 
IF (out_col) THEN
  CALL sort_tab_i(varliv(:,1:nvl))

  OPEN (UNIT=31, FILE=filecol, STATUS="REPLACE")
  chrec = ""
  CALL build_header("lscol",chrec)
  WRITE (31,'(a)') TRIM(chrec)

  ndef = 0
  DO kvl = 1,nvl

!   leggo dalle tabelle i parametri aggiuntivi per seriet.
    CALL var2spec(varliv(1:3,kvl),ndec,vmin,vmax,str_var,cp2,iret)
    IF (iret /= 0) ndef = ndef + 1
    

    CALL init(colcsv)
    CALL csv_record_addfield(colcsv,varliv(1,kvl))    
    CALL csv_record_addfield(colcsv,varliv(2,kvl))    
    CALL csv_record_addfield(colcsv,varliv(3,kvl))    
    CALL csv_record_addfield(colcsv,cp2)
    CALL csv_record_addfield(colcsv,varliv(4,kvl))    
    CALL csv_record_addfield(colcsv,varliv(5,kvl))    
    CALL csv_record_addfield(colcsv,varliv(6,kvl))    
    CALL csv_record_addfield(colcsv,ndec)
    CALL csv_record_addfield(colcsv,vmin)
    CALL csv_record_addfield(colcsv,vmax)    
    CALL csv_record_addfield(colcsv,TRIM(ADJUSTL(str_model)))
    CALL csv_record_addfield(colcsv,TRIM(ADJUSTL(str_var)))
    WRITE (31,'(a)') csv_record_getrecord(colcsv)
    CALL delete(colcsv)
  ENDDO
  CLOSE(31)

  IF (ndef >0) WRITE (*,'(a,i3,a)') "Uso default per ",ndef, &
    " parametri non trovati nelle tabelle"
ENDIF

! 4.3) Timerange e reference time (righe)
IF (out_row) THEN
  IF (ntr > 0 .AND. lseriet) THEN
    OPEN (UNIT=32, FILE=filerow, STATUS="REPLACE")
    chrec = ""
    CALL build_header("lsrow",chrec)
    WRITE (32,'(a)') TRIM(chrec)

    CALL init(rowcsv)
    CALL getval(reftime_min,SIMPLEDATE=ch10)
    CALL csv_record_addfield(rowcsv,ch10)
    CALL getval(reftime_max,SIMPLEDATE=ch10)
    CALL csv_record_addfield(rowcsv,ch10)
    CALL csv_record_addfield(rowcsv,tdh)
    CALL csv_record_addfield(rowcsv,p1_min)
    CALL csv_record_addfield(rowcsv,p1_max)
    CALL csv_record_addfield(rowcsv,p1_step)
    WRITE (32,'(a)') csv_record_getrecord(rowcsv)
    CALL delete(rowcsv)
    CLOSE(32)

  ELSE IF (ntr > 0 .AND. .NOT. lseriet) THEN
    WRITE (*,*) "I dati non possono essere scritti in formato seriet "
    WRITE (*,*) "file ",TRIM(filerow)," non scritto"

  ELSE IF (ntr == 0) THEN               ! sto elaborando un file vuoto
    OPEN (UNIT=32, FILE=filerow, STATUS="REPLACE")
    chrec = ""
    CALL build_header("lsrow",chrec)
    WRITE (32,'(a)') TRIM(chrec)
    CLOSE(32)

  ENDIF
ENDIF

! 3.4) Log a schermo
WRITE (*,'(a)') "Elaborazioni completate, trovati:"
WRITE (*,'(i10,1x,a)') cnt_seg,"segmenti"
WRITE (*,'(i10,1x,a)') cnt_dat_ok,"record di dati validi"
WRITE (*,'(i10,1x,a)') npt,"punti"
WRITE (*,'(i10,1x,a)') ntr,"timeranges"
WRITE (*,'(i10,1x,a)') nvl,"coppie var-liv"
WRITE (*,'(i10,1x,a)') nvltr,"combinazioni var-liv-timerange"

IF (ntr > 0) THEN
  CALL getval(reftime_min,SIMPLEDATE=ch10)
  WRITE (*,'(a10,1x,a)') ch10,"reftime min"
  CALL getval(reftime_max,SIMPLEDATE=ch10)
  WRITE (*,'(a10,1x,a)') ch10,"reftime max"
  IF (ltdelta) THEN
    CALL getval(tdelta_min,AHOUR=tdh)
    WRITE (*,'(i10,1x,a)') tdh,"reftime step minimo (ore)"
  ENDIF
  IF (lseriet) THEN
    WRITE (*,'(i10,1x,a)') p1_min,"P1 min"
    WRITE (*,'(i10,1x,a)') p1_max,"P1 max"
    WRITE (*,'(i10,1x,a)') p1_step,"P1 step"
  ENDIF
ENDIF

IF (lseriet) THEN
  STOP
ELSE
  STOP 10
ENDIF

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*)  "Errore nei parametri"
CALL write_help
STOP 1

9998 CONTINUE
WRITE (*,*)  "File di input gacsv non trovato ",TRIM(filein)
STOP 2

9997 CONTINUE
WRITE (*,*)  "Sequenza dei campi inattesa nel segmento ",cnt_seg
STOP 3

9996 CONTINUE
WRITE (*,*) "Errore leggendo il record dati ",cnt_dat_seg, &
  " del segmento ",cnt_seg
STOP 4

9995 CONTINUE
WRITE (*,*) "Dimensione massima array insufficiente:"
IF (nvl > maxvl) WRITE (*,'(a,i4,a)') "valiv (max = ",maxvl,")"
IF (ntr > maxtr) WRITE (*,'(a,i4,a)') "timerange (max = ",maxtr,")"
IF (nvltr > maxvltr) WRITE (*,'(a,i4,a)') "varlivtrange (max = ",maxvltr,")"
IF (npt > maxpt) WRITE (*,'(a,i4,a)') "punti (max = ",maxpt,")"
STOP 5

END PROGRAM gacsv_scan

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE sort_tab_i(array)
! 
! Ordina le righe di una tabella di interi.
! Considera l'array formato da un insieme di vettori = array(:,k)
! Cambia l'ordine di questi vettori (ie. ordina lungo il secondo indice),
! considerando in sequenza i valori che trova sul primo indice (ie. ordina
! prima per array(1,:), poi per array(2,:), ...
!
IMPLICIT NONE
INTEGER, INTENT(INOUT) :: array(:,:)

INTEGER, ALLOCATABLE :: darray(:,:)
LOGICAL, ALLOCATABLE :: used(:)
INTEGER :: shp(2),jin,jout,i,min_idx
LOGICAL :: is_small
!
shp = SHAPE(array)
ALLOCATE (darray(shp(1),shp(2)),used(shp(2)))

used(:) = .FALSE.

DO jout = 1,shp(2)
  min_idx = 0
  DO jin = 1,shp(2)
    IF (used(jin)) CYCLE
    IF (min_idx == 0) min_idx = jin

!   Guardo se il vettore array(:,jin) e' "minore" di array(:,max_idx)
    DO i = 1,shp(1)
      IF (array(i,jin) < array(i,min_idx)) THEN
        is_small = .TRUE.
        EXIT
      ELSE IF (array(i,jin) > array(i,min_idx)) THEN
        is_small = .FALSE.
        EXIT
      ENDIF
    ENDDO
    IF (i > shp(1)) is_small = .FALSE.
    IF (is_small) min_idx = jin
!   print '(a,2i6,l,i6)', "jin,i,is_small,min_idx ",jin,i,is_small,min_idx

  ENDDO

! Salvo su darray il vettore "minimo" tra quelli ancora da collocare
  darray(:,jout) = array(:,min_idx)
  used(min_idx) = .TRUE.

ENDDO

!print *,"original"
!print '(6i5)',array
!print *,"sorted"
!print '(6i5)',darray

array(:,:) = darray(:,:)

RETURN
END SUBROUTINE sort_tab_i

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: gacsv_scan.exe filein [-p filepts] [-c filecol] [-r filerow] "
WRITE (*,*) "    [-m model] [-h] [-verbose]" 
WRITE (*,*) "Analizza un file grib_api_cvs col tracciato per estazioni arkimet su punto"
WRITE (*,*) "-p filepts: scrive su filepts elenco e coordinate dei punti"
WRITE (*,*) "-c filecol: scrive su filecol min max e step di ref.time e timerange"
WRITE (*,*) "-r filerow: scrive su filerow le combianzioni par/lev"
WRITE (*,*)
WRITE (*,*) "-m model: stringa identificativa del modello (per header files seriet)"
WRITE (*,*) "-h: visualizza questo help" 
WRITE (*,*) "-verbose: visualizza informazioni aggiuntive per debug"
WRITE (*,*) "Cerca le tabelle seriet in $MA_UTILS_DAT (default: "//PKGDATAROOTDIR//")"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

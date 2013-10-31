PROGRAM scores_seriet_wind
!--------------------------------------------------------------------------
! Dati due files in uno dei formati SIM (estra_orari, trasp_seriet, 
! trasp_temp), calcola alcuni scores relativi al vento su un livello tra i 
! dati del primo (osservazioni) e del secondo (previsioni)
!
! Uso: scores_seriet_wind fileoss filepre fileout [-h] [-c]
! Lettura opzionale di scores_seriet_wind.naml
!
! Note:
! - Il programma gestisce il caso in cui il file previsto abbia meno 
!   istanti di quello osservato (es: verifica del vento previsto +24/+48 
!   in una sequenza di run), a condizione che:
!   * gli istanti devono essere in ordine 
!   * il 1o istante oss. non puo' essere successivo al 1o istante prev.
!   * deve esistere un record oss. per ogni record prev.
!
! - Il formato (obsoleto) "seriet fortran" potrebbe non essere gestito
!   correttamente
!
! - Contenuto arrays per calcolo statistiche:
!   * stat: SUM(ff_oss), SUM(ff_pre), SUM((ff_pre - ff_oss)^2),
!           SUM(ABS(dd_pre - dd_oss))
!   * cnt: Ntot; Nok; Ncalm: OS & PR; OS & noPR; noOS & PR; noOS & noPR
!
!                                           Versione 1.0, Enrico 18/01/2008
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.      ! valore per dati mancanti (I/O)
INTEGER, PARAMETER :: mxpar = 100     ! max. N. parametri nei files di input
INTEGER, PARAMETER :: nff_bin = 500   ! N. bin (passi di 0.1 m/s)per calcolo
                                      ! dist. freq. velocita' vento oss.

! Scores_seriet_wind.naml
REAL :: thr_calm
INTEGER :: col_ff_oss,col_dd_oss,col_ff_pre,col_dd_pre,ncff,ncdd
NAMELIST /param/col_ff_oss,col_dd_oss,col_ff_pre,col_dd_pre,thr_calm, &
  ncff,ncdd

! Altre variabili del programma
TYPE (date) :: datao,datap,data1,data2
REAL, ALLOCATABLE :: wr_cldd(:),wr_clff(:)
REAL :: val(mxpar),dfreq(0:nff_bin),stat(4)
REAL :: delta,step,ffo,ffp,ddo,ddp,cal_frq_cor,rnok
REAL :: wr_ave,wr_varo,wr_varp,wr_cov,wrcor,frco,frcp
REAL :: wr_ave2,wr_varo2,wr_varp2,wr_cov2,wrcor2
INTEGER, ALLOCATABLE :: wro(:,:),wrp(:,:),wro2(:,:),wrp2(:,:)
INTEGER, ALLOCATABLE :: ncalmo(:),ncalmp(:)
INTEGER :: nfreq(0:nff_bin),cnt(6)
INTEGER :: fmt_oss,fmt_pre,nf_oss,nf_pre,nrho,cff,cdd,nsum
INTEGER :: hho,hhp,scao,scap,hh1,hh2,clfo,cldo,clfp,cldp,quoz,resto
INTEGER :: k,k2,ios,eof,eor,ff_bin
CHARACTER (LEN=mxpar*11+30) :: chrec
CHARACTER (LEN=80) :: fileoss,filepre,fileout,chfmt
CHARACTER (LEN=10) :: ch10heado(3,mxpar),ch10headp(3,mxpar)
LOGICAL :: warn

!##########################################################################
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1) Codice per EOF
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 1.2) Parametri da riga comandi
CALL getarg(1,fileoss)
CALL getarg(2,filepre)
CALL getarg(3,fileout)

IF (fileoss == "-c") THEN
  CALL scrive_namelist
  STOP

ELSE IF (fileoss == "-h" .OR. fileoss == "" .OR. filepre == "" .OR. &
  fileout == "") THEN
!              123456789012345678901234567890123456789012345678901234567890123456789012345
  WRITE (*,*) "Dati due files in uno dei formati SMR (seriet, estra_orari o estra_temp),"
  WRITE (*,*) "  calcola alcuni scores relativi ai dati di vento su un livello"
  WRITE (*,*)
  WRITE (*,*) "Uso: scores_seriet_wind fileoss filepre fileout [-h] [-c]"
  WRITE (*,*) "-h: visualizza questo help"
  WRITE (*,*) "-c: crea un file scores_seriet_wind.naml di esmpio"
  WRITE (*,*)
  WRITE (*,*) "NOTE:"
  WRITE (*,*) "Mettere il file con il passo temporale piu lungo come file previsto"
  WRITE (*,*) "scores_seriet.naml consente di specificare:"
  WRITE (*,*) "- quali dati (colonne) elaborare nei due files"
  WRITE (*,*) "- la soglia le calme di vento"
  WRITE (*,*) "- il numero di classi da usare per la correlazione delle rose dei venti"
  WRITE (*,*)
  STOP

ENDIF

!--------------------------------------------------------------------------
! 1.3) Analizzo i file di input

CALL inq_file_smr(fileoss,fmt_oss,nf_oss,col_ff_oss,col_dd_oss)
CALL inq_file_smr(filepre,fmt_pre,nf_pre,col_ff_pre,col_dd_pre)

!--------------------------------------------------------------------------
! 1.4) Parametri da namelist

OPEN (UNIT=40, FILE="scores_seriet_wind.naml", STATUS="OLD", IOSTAT=ios)

IF (ios /= 0) THEN
  WRITE (*,*) "File scores_seriet_wind.naml non trovato, uso defaults"
  IF (col_ff_oss == 0 .OR. col_dd_oss == 0) THEN
    WRITE (*,*) "Dati di vento non presenti in ",TRIM(fileoss)
    STOP
  ELSE IF (col_ff_pre == 0 .OR. col_dd_pre == 0) THEN
    WRITE (*,*) "Dati di vento non presenti in ",TRIM(filepre)
    STOP
  ELSE
    thr_calm = 1.
    ncff = 5
    ncdd = 5
  ENDIF
ELSE
  READ (40, NML=param, ERR=9999)
ENDIF

IF (ncff <= 0 .OR. ncdd <= 0) THEN
  WRITE (*,*) "Numero classi FF,DD illegale: ",ncff,ncdd
  STOP
ELSE IF (col_ff_pre == 0 .OR. col_dd_pre == 0 .OR. &
         col_ff_oss == 0 .OR. col_dd_oss == 0) THEN
  WRITE (*,*) "Colonne non trovate (FP,DP,FO,DO) ",&
    col_ff_pre,col_dd_pre,col_ff_oss,col_dd_oss
  STOP
ENDIF

ALLOCATE(wro(ncff,ncdd),wrp(ncff,ncdd),wro2(ncff,ncdd),wrp2(ncff,ncdd))
ALLOCATE(wr_cldd(ncdd),wr_clff(ncff),ncalmo(ncff),ncalmp(ncff))
CLOSE (40)
 
!--------------------------------------------------------------------------
! 1.5.1) Osservazioni: apro il file, leggo headers, costruisco labels e 
!        formato dei record dati.

OPEN (UNIT=41, FILE=fileoss, STATUS="OLD")

IF (fmt_oss == 1) THEN
  nrho = 6
  WRITE (chfmt,'(a,i3,a)') "(17x,",nf_oss,"(1x,a10))"
  READ (41,*)
  READ (41,*)
  READ (41,chfmt) (ch10heado(1,k),k=1,nf_oss)
  READ (41,chfmt) (ch10heado(2,k),k=1,nf_oss)
  READ (41,*)
  READ (41,chfmt) (ch10heado(3,k),k=1,nf_oss)

ELSE IF (fmt_oss == 2) THEN
  nrho = 0

ELSE IF (fmt_oss == 3) THEN
  nrho = 3
  WRITE (chfmt,'(a,i3,a)') "(13x,",nf_oss,"(1x,a10))"
  ch10heado(1,1:nf_oss) = "Oss"
  ch10heado(2,1:nf_oss) = "Surf"
  READ (41,*)
  READ (41,*)
  READ (41,chfmt) (ch10heado(3,k),k=1,nf_oss)

ELSE IF (fmt_oss == 4) THEN
  nrho = 3
  WRITE (chfmt,'(a,i3,a)') "(19x,",nf_oss,"(1x,a10))"
  ch10heado(1,1:nf_oss) = "RSD"
  READ (41,chfmt) (ch10heado(2,k),k=1,nf_oss)
  READ (41,*)
  READ (41,chfmt) (ch10heado(3,k),k=1,nf_oss)

ENDIF

!--------------------------------------------------------------------------
! 1.5.2) Previsioni: apro il file, leggo headers, costruisco labels
OPEN (UNIT=42, FILE=filepre, STATUS="OLD")

IF (fmt_pre == 1) THEN
  WRITE (chfmt,'(a,i3,a)') "(17x,",nf_pre,"(1x,a10))"
  READ (42,*)
  READ (42,*)
  READ (42,chfmt) (ch10headp(1,k),k=1,nf_pre)
  READ (42,chfmt) (ch10headp(2,k),k=1,nf_pre)
  READ (42,*)
  READ (42,chfmt) (ch10headp(3,k),k=1,nf_pre)

ELSE IF (fmt_oss == 2) THEN
  nrho = 0

ELSE IF (fmt_pre == 3) THEN
  WRITE (chfmt,'(a,i3,a)') "(13x,",nf_pre,"(1x,a10))"
  ch10headp(1,1:nf_pre) = "Oss"
  ch10headp(2,1:nf_pre) = "Surf"
  READ (42,*)
  READ (42,*)
  READ (42,chfmt) (ch10headp(3,k),k=1,nf_pre)

ELSE IF (fmt_pre == 4) THEN
  WRITE (chfmt,'(a,i3,a)') "(19x,",nf_pre,"(1x,a10))"
  ch10headp(1,1:nf_pre) = "RSD"
  READ (42,chfmt) (ch10headp(2,k),k=1,nf_pre)
  READ (42,*)
  READ (42,chfmt) (ch10headp(3,k),k=1,nf_pre)

ENDIF

!--------------------------------------------------------------------------
! 1.6) Log a schermo dell'analisi files

SELECT CASE (fmt_oss)
CASE (1)
  chfmt = "seriet XLS" 
CASE (2)
  chfmt = "seriet for" 
CASE (3)
  chfmt = "estra_orari" 
CASE (4)
  chfmt = "trasp_temp" 
END SELECT
WRITE (*,'(3a,i3)') "Osservazioni: formato ",TRIM(chfmt),"; campi ",nf_oss
WRITE (*,'(4a)') "Parametri richiesti: vel ->",ch10heado(3,col_ff_oss), &
  ", dir ->",ch10heado(3,col_dd_oss)

SELECT CASE (fmt_pre)
CASE (1)
  chfmt = "seriet XLS" 
CASE (2)
  chfmt = "seriet for" 
CASE (3)
  chfmt = "estra_orari" 
CASE (4)
  chfmt = "trasp_temp" 
END SELECT
WRITE (*,'(3a,i3)') "Previsioni: formato ",TRIM(chfmt),"; campi ",nf_pre
WRITE (*,'(4a)') "Parametri richiesti: vel ->",ch10headp(3,col_ff_pre), &
  ", dir ->",ch10headp(3,col_dd_pre)

!##########################################################################
! 2) Leggo una prima volta le osservazioni, per costruire la distribuzione 
!    di frequenza della velocita' del vento. 
!
! - Gli elementi dei vettori nfreq e dfreq contengono i dati relativi
!   al valore esatto e agli 0.1 m/s superiori (elemento 0: 0<= FF < 0.1 m/s;
!   elemento 1: 0.1 <= FF < 0.2 m/s; elemento nff_bin: FF >= 10*nff_bin)
! - I vettori wr_cldd(ncdd) e wr_clff(ncff) contengono gli estremi 
!   inferiori delle classi di direzione e velocita' (il primo elemento di 
!   entrambi e' 0, l'ultima classe di velocita' e' aperta verso l'alto)

! Leggo i valori
nfreq(:) = 0
DO
  CALL read_rec_smr(41,fmt_oss,nf_oss,datao,hho,scao,val(1:nf_oss),ios)
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998
  IF (val(col_ff_oss) == rmis .OR. (val(col_dd_oss) == rmis)) CYCLE

  ff_bin = INT(val(col_ff_oss)*10.)
  IF (ff_bin > nff_bin) ff_bin = nff_bin
  nfreq(ff_bin) = nfreq(ff_bin) + 1
ENDDO
rnok = REAL(SUM(nfreq(:)))

! Passo alla distribuzione di frequenza cumulata
nsum = 0
DO k = 1,nff_bin
  dfreq(k) = REAL(nsum + nfreq(k)) / rnok
  nsum = nsum + nfreq(k)
ENDDO

! Calcolo i margini delle calssi della wind rose
wr_clff(1) = 0.
DO k = 2,ncff
DO k2 = 0,nff_bin
  IF (dfreq(k2) > REAL(k-1)/REAL(ncff)) THEN
    wr_clff(k) = REAL(k2)/10.
    EXIT
  ENDIF
ENDDO
ENDDO

! Elimino le eventuali classi degeneri
WRITE (chfmt,'(a,i3,a)') "(a,",ncff,"(1x,f5.2))"
WRITE (*,chfmt) "Velocita': ",wr_clff(:)
warn = .FALSE.
DO k = 2,ncff
  IF (wr_clff(k) <= wr_clff(k-1)) THEN
    wr_clff(k) = (NINT(wr_clff(k-1)*10)+1)/10.
    warn = .TRUE.
  ENDIF
ENDDO
IF (warn) THEN
  WRITE (*,*) "WARNING: Corrette classi di velocita' degeneri:"
  WRITE (*,*) "sarebbe consigliabile ridurre il numero di classi"
ENDIF

step = 360./REAL(ncdd)
DO k = 1,ncdd
  wr_cldd(k) = (k-1)*step
ENDDO

WRITE (*,'(a)') "Estremi classi per wind rose correlation:"
WRITE (chfmt,'(a,i3,a)') "(a,",ncff,"(1x,f5.2))"
WRITE (*,chfmt) "Velocita': ",wr_clff(:)
WRITE (chfmt,'(a,i3,a)') "(a,",ncdd,"(1x,f5.1))"
WRITE (*,chfmt) "Direzione: ",wr_cldd(:)

! Chiudo e riapro il file osservazioni
CLOSE (41)
OPEN (UNIT=41, FILE=fileoss, STATUS="OLD")
DO k = 1,nrho
  READ (41,*)
ENDDO

!##########################################################################
! 3) Leggo i dati e aggiorno i contatori

cnt(:) = 0
stat(:) = 0.
wro(:,:) = 0
wrp(:,:) = 0
wro2(:,:) = 0
wrp2(:,:) = 0
ncalmo(:) = 0
ncalmp(:) = 0

main: DO

!--------------------------------------------------------------------------
! 3.1 Leggo il prossimo record previsto

  CALL read_rec_smr(42,fmt_pre,nf_pre,datap,hhp,scap,val(1:nf_pre),ios)
  IF (ios /= 0) EXIT
  hh1 = MOD(hhp+scap,24)
  data1 = datap + (hhp+scap)/24
  ffp = val(col_ff_pre)
  ddp = val(col_dd_pre)

!--------------------------------------------------------------------------
! 3.2 Trovo il corrispondente record osservato

  DO
    CALL read_rec_smr(41,fmt_oss,nf_oss,datao,hho,scao,val(1:nf_oss),ios)
    IF (ios /= 0) EXIT main
    hh2 = MOD(hho+scao,24)
    data2 = datao + (hho+scao)/24

    IF (data2 < data1 .OR. (data2 == data1 .AND. hh2 < hh1)) THEN
      CYCLE
    ELSE IF (data2 > data1 .OR. (data2 == data1 .AND. hh2 > hh1)) THEN
      WRITE (*,'(a,2i3,i5,i3)') &
        "Errore, record oss. non trovato per l'istante: ",data1,hh1
      STOP
    ELSE
      ffo = val(col_ff_oss)
      ddo = val(col_dd_oss)

      EXIT
    ENDIF
  ENDDO

!--------------------------------------------------------------------------
! 3.3 Incremento i contatori statistici

! dati totali
  cnt(1) = cnt(1) + 1

! dati validi
  IF (ffo == rmis .OR. ffp == rmis .OR. ddo == rmis .OR. ddp == rmis) CYCLE
  cnt(2) = cnt(2) + 1
  delta = ddp - ddo
  IF (delta > 180.) delta = delta - 360.
  IF (delta < -180.) delta = delta + 360.

! ff Media e RMS
  stat(1) = stat(1) + ffo
  stat(2) = stat(2) + ffp
  stat(3) = stat(3) + (ffp-ffo)**2
      
! Tabella di ccontingenza delle calme di vento
  IF (ffo <= thr_calm .AND. ffp <= thr_calm) cnt(3) = cnt(3) + 1
  IF (ffo <= thr_calm .AND. ffp >  thr_calm) cnt(4) = cnt(4) + 1
  IF (ffo >  thr_calm .AND. ffp <= thr_calm) cnt(5) = cnt(5) + 1
  IF (ffo >  thr_calm .AND. ffp >  thr_calm) cnt(6) = cnt(6) + 1

! dd MAE (calme sempre escluse)
  IF (ffo > thr_calm .AND. ffp > thr_calm) THEN
    stat(4) = stat(4) + ABS(delta)
  ENDIF

! Wind Rose Correlation (dati originali)
  clfo = 1
  DO k = 2,ncff
    IF (ffo > wr_clff(k)) clfo = k
  ENDDO
  cldo = 1
  DO k = 2,ncdd
    IF (ddo > wr_cldd(k)) cldo = k
  ENDDO
  wro(clfo,cldo) = wro(clfo,cldo) + 1

  clfp = 1
  DO k = 2,ncff
    IF (ffp > wr_clff(k)) clfp = k
  ENDDO
  cldp = 1
  DO k = 2,ncdd
    IF (ddp > wr_cldd(k)) cldp = k
  ENDDO
  wrp(clfp,cldp) = wrp(clfp,cldp) + 1

! Wind Rose Correlation (calme ridistribuite)
  IF (ffo <= thr_calm) THEN
    ncalmo(clfo) = ncalmo(clfo) + 1  
  ELSE
    wro2(clfo,cldo) = wro2(clfo,cldo) + 1
  ENDIF
  IF (ffp <= thr_calm) THEN
    ncalmp(clfp) = ncalmp(clfp) + 1  
  ELSE
    wrp2(clfp,cldp) = wrp2(clfp,cldp) + 1
  ENDIF

ENDDO main

CLOSE (41)
CLOSE (42)

!##########################################################################
! 4) Calcolo scores e scrivo output

!--------------------------------------------------------------------------
! 4.1 Ridistribuisco le calme di vento nelle Windrose2

OPEN (UNIT=51, FILE="wind_roses.log", STATUS="REPLACE", FORM="FORMATTED")
WRITE (51,*) "Statistiche ridistribuzione calme:"

WRITE (51,*) "Osservato:"
DO k = 1,ncff
  quoz = ncalmo(k)/ncdd
  resto = MOD(ncalmo(k),ncdd)
  wro2(k,1:resto) = wro2(k,1:resto)+quoz+1
  wro2(k,resto+1:) = wro2(k,resto+1:)+quoz
  WRITE (51,*) k,ncalmo(k),quoz,resto
ENDDO

WRITE (51,*) "Previsto:"
DO k = 1,ncff
  quoz = ncalmp(k)/ncdd
  resto = MOD(ncalmp(k),ncdd)
  wrp2(k,1:resto) = wrp2(k,1:resto)+quoz+1
  wrp2(k,resto+1:) = wrp2(k,resto+1:)+quoz
  WRITE (51,*) k,ncalmp(k),quoz,resto
ENDDO
WRITE (51,*)

!--------------------------------------------------------------------------
! 4.2 Controlli

IF (cnt(3) + cnt(4) + cnt(5) + cnt(6) /= cnt(2)) WRITE (*,*) &
  "Warning, errore nei conteggi delle calme! ",cnt(:)

IF (SUM(wrp(:,:)) /= cnt(2) .OR. SUM(wro(:,:)) /= cnt(2)) WRITE (*,*) &
  "Warning, errore nel calcolo rose dei venti: dati attesi, oss, pre ", &
  cnt(6),SUM(wro(:,:)),SUM(wrp(:,:))

IF (SUM(wrp2(:,:)) /= cnt(2) .OR. SUM(wro2(:,:)) /= cnt(2)) WRITE (*,*) &
  "Warning, errore nella ridistribuzione delle calme: dati attesi, oss, pre ", &
  cnt(6),SUM(wro2(:,:)),SUM(wrp2(:,:))

!--------------------------------------------------------------------------
! 4.3 Calcolo gli scores

IF (cnt(2) > 0) THEN
  stat(1) = stat(1) / REAL(cnt(2))                ! AVE ff oss
  stat(2) = stat(2) / REAL(cnt(2))                ! MEDIA ff pre
  stat(3) = SQRT(stat(3) / REAL(cnt(2)))          ! RMSE ff
  cal_frq_cor = REAL(cnt(3)+cnt(6))/ REAL(cnt(2)) ! %Correct calme
ELSE
  stat(1:3) = rmis
  cal_frq_cor = rmis
ENDIF
IF (cnt(6) > 0) THEN
  stat(4) = stat(4) / REAL(cnt(6))                ! MAE dd oss
ELSE
  stat(4) = rmis
ENDIF

wr_ave = REAL(cnt(2)) / REAL(ncff*ncdd)
wr_varo = SUM(REAL((wro(:,:)) - wr_ave)*(REAL(wro(:,:)) - wr_ave))
wr_varp = SUM((REAL(wrp(:,:)) - wr_ave)*(REAL(wrp(:,:)) - wr_ave))
wr_cov = SUM(REAL((wro(:,:)) - wr_ave)*REAL((wrp(:,:)) - wr_ave))
IF (wr_varo > 0. .AND. wr_varp > 0.) THEN
  wrcor = wr_cov / SQRT((wr_varo * wr_varp))      ! WRC con dati originali
ELSE
  wrcor = rmis
ENDIF

wr_ave2 = REAL(cnt(2)) / REAL(ncff*ncdd)
wr_varo2 = SUM(REAL((wro2(:,:)) - wr_ave)*(REAL(wro2(:,:)) - wr_ave))
wr_varp2 = SUM((REAL(wrp2(:,:)) - wr_ave)*(REAL(wrp2(:,:)) - wr_ave))
wr_cov2 = SUM(REAL((wro2(:,:)) - wr_ave)*REAL((wrp2(:,:)) - wr_ave))
IF (wr_varo2 > 0. .AND. wr_varp2 > 0.) THEN
  wrcor2 = wr_cov2 / SQRT((wr_varo2 * wr_varp2))  ! WRC con calme ridistribuite
ELSE
  wrcor2 = rmis
ENDIF

!--------------------------------------------------------------------------
! 4.4 Scrivo output: 

IF (cnt(2) > 0) THEN
  frco = REAL(cnt(3)+cnt(4))/REAL(cnt(2))
  frcp = REAL(cnt(3)+cnt(5))/REAL(cnt(2))
ELSE
  frco = rmis
  frcp = rmis
ENDIF

! File principale (score e statistiche)
OPEN (UNIT=50, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
WRITE (50,'(i8,a)') cnt(1)," ! Dati totali"
WRITE (50,'(i8,a)') cnt(2)," ! Dati valdi"
WRITE (50,'(i8,a)') cnt(6)," ! Dati senza calma Oss ne' Pre"
WRITE (50,'(f8.2,a)') stat(1)," ! ff AVE oss"
WRITE (50,'(f8.2,a)') stat(2)," ! ff AVE pre"
WRITE (50,'(f8.2,a)') stat(2)-stat(1)," ! ff BIAS"
WRITE (50,'(f8.2,a)') stat(3)," ! ff RMSE"
WRITE (50,'(f8.2,a)') stat(4)," ! dd MAE (escluse calme)"
WRITE (50,'(f8.2,a)') wrcor," ! Windrose correlation (dati originali)"
WRITE (50,'(f8.2,a)') wrcor2," ! Windrose correlation (calme ridistrib.)"
WRITE (50,*)
WRITE (50,'(f8.2,a)') frco," ! Frequenza osservata calme"
WRITE (50,'(f8.2,a)') frcp," ! Frequenza prevista calme"
WRITE (50,'(f8.2,a)') cal_frq_cor," ! Frequenza previsioni corrette calme"
WRITE (50,*)
WRITE (50,'(f8.2,a)') thr_calm," ! Soglia calme"
WRITE (50,'(i8,a)') ncff," ! Classi velocita' per windrose"
WRITE (50,'(i8,a)') ncdd," ! Classi direzione per windrose"
CLOSE(50)

! Dettagli wind rose

WRITE (51,'(2a)') "Osservato (dati originali): ",TRIM(fileoss)
WRITE (chfmt,'(a,i3,a)') "(8x,",ncdd,"f6.1,2x,a6)"
WRITE (51,chfmt) wr_cldd(1:ncdd),"Totale"
WRITE (chfmt,'(a,i3,a)') "(a2,f4.1,2x,",ncdd,"i6,2x,i6)"
DO k = 1,ncff
  WRITE (51,chfmt) " >",wr_clff(k),wro(k,1:ncdd),SUM(wro(k,1:ncdd))
ENDDO
WRITE (chfmt,'(a,i3,a)') "(a6,2x,",ncdd,"i6,2x,i6)"
WRITE (51,chfmt) "Totale",(SUM(wro(1:ncff,k)),k=1,ncdd),SUM(wro(1:ncff,1:ncdd))

WRITE (51,*)
WRITE (51,'(2a)') "Previsto (dati originali): ",TRIM(fileoss)
WRITE (chfmt,'(a,i3,a)') "(8x,",ncdd,"f6.1,2x,a6)"
WRITE (51,chfmt) wr_cldd(1:ncdd),"Totale"
WRITE (chfmt,'(a,i3,a)') "(a2,f4.1,2x,",ncdd,"i6,2x,i6)"
DO k = 1,ncff
  WRITE (51,chfmt) " >",wr_clff(k),wrp(k,1:ncdd),SUM(wrp(k,1:ncdd))
ENDDO
WRITE (chfmt,'(a,i3,a)') "(a6,2x,",ncdd,"i6,2x,i6)"
WRITE (51,chfmt) "Totale",(SUM(wrp(1:ncff,k)),k=1,ncdd),SUM(wrp(1:ncff,1:ncdd))

WRITE (51,*)
WRITE (51,'(2a)') "Osservato (calme ridistribuite): ",TRIM(fileoss)
WRITE (chfmt,'(a,i3,a)') "(8x,",ncdd,"f6.1,2x,a6)"
WRITE (51,chfmt) wr_cldd(1:ncdd),"Totale"
WRITE (chfmt,'(a,i3,a)') "(a2,f4.1,2x,",ncdd,"i6,2x,i6)"
DO k = 1,ncff
  WRITE (51,chfmt) " >",wr_clff(k),wro2(k,1:ncdd),SUM(wro2(k,1:ncdd))
ENDDO
WRITE (chfmt,'(a,i3,a)') "(a6,2x,",ncdd,"i6,2x,i6)"
WRITE (51,chfmt) "Totale",(SUM(wro2(1:ncff,k)),k=1,ncdd),SUM(wro2(1:ncff,1:ncdd))

WRITE (51,*)
WRITE (51,'(2a)') "Previsto (calme ridistribuite): ",TRIM(fileoss)
WRITE (chfmt,'(a,i3,a)') "(8x,",ncdd,"f6.1,2x,a6)"
WRITE (51,chfmt) wr_cldd(1:ncdd),"Totale"
WRITE (chfmt,'(a,i3,a)') "(a2,f4.1,2x,",ncdd,"i6,2x,i6)"
DO k = 1,ncff
  WRITE (51,chfmt) " >",wr_clff(k),wrp2(k,1:ncdd),SUM(wrp2(k,1:ncdd))
ENDDO
WRITE (chfmt,'(a,i3,a)') "(a6,2x,",ncdd,"i6,2x,i6)"
WRITE (51,chfmt) "Totale",(SUM(wrp2(1:ncff,k)),k=1,ncdd),SUM(wrp2(1:ncff,1:ncdd))

CLOSE(51)

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore leggendo scores_seriet_wind.naml"
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(fileoss)
STOP

END PROGRAM scores_seriet_wind

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE inq_file_smr(file,idfmt,nf,col_ff,col_dd)
!-------------------------------------------------------------------------
! Dato un file SMR, ritorna il suo formato, il numero di campi, la 
! posizione del primo campo "velocita' vento" e quella del primo campo 
! "direzione vento"
!
! valori per idfmt:
! 0 = sconosciuto
! 1 = seriet per XLS
! 2 = seriet per fortran
! 3 = estra_orari
! 4 = trasp_temp
!-------------------------------------------------------------------------
IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: mxpar = 1000

! argomenti della subroutine
CHARACTER (LEN=80), INTENT(IN) :: file
INTEGER, INTENT(OUT) :: idfmt,nf,col_ff,col_dd

! Variabili locali
INTEGER :: iun,k,chead,eof,eor,ios,rhead
CHARACTER (LEN=80) :: str
CHARACTER (LEN=10) :: ch10
CHARACTER (LEN=1) :: ch1
LOGICAL :: l1

!-------------------------------------------------------------------------
! Preliminari
CALL get_eof_eor(eof,eor)

idfmt = 0
nf = 0
col_ff = 0
col_dd = 0

!-------------------------------------------------------------------------
! Cerco un'unita' libera 
DO k = 10,99
  INQUIRE (UNIT=k, OPENED=l1, IOSTAT=ios)
  IF (.NOT. l1 .AND. ios==0) THEN
    iun = k
    EXIT
  ENDIF
ENDDO
IF (iun == 0) GOTO 9999   ! non ho torvato nessuna unita' libera

!-------------------------------------------------------------------------
! Trovo il formato

OPEN (UNIT=iun, FILE=file, STATUS="OLD", ERR=9998)
READ (iun,*,END=9997,ERR=9997)
READ (iun,*,END=9997,ERR=9997)
READ (iun,'(a)',END=9997,ERR=9997) str

IF (str(1:19) == "aaaa mm gg hh staz.") THEN        ! trasp_temp
  idfmt = 4
  rhead = 3
  chead = 19

ELSE IF (str(1:13) == "aaaa mm gg hh") THEN         ! estra_orari
  idfmt = 3
  rhead = 3
  chead = 13

ELSE                                                ! seriet
  READ (iun,*,END=9997,ERR=9997)
  READ (iun,*,END=9997,ERR=9997)
  READ (iun,'(a)',END=9997,ERR=9997) str
  IF (str(1:17) == "gg/mm/aaaa hh sca") THEN 
    idfmt = 1
    rhead = 6
    chead = 17
  ELSE IF (str(1:17) == "aaaa mm gg hh sca") THEN 
    idfmt = 2
    rhead = 6
    chead = 17
  ELSE
    RETURN    
  ENDIF

ENDIF
CLOSE (iun)

!-------------------------------------------------------------------------
! Conto i campi e trovo le posizioni di velocita' e direzione vento

OPEN (UNIT=iun, FILE=file, STATUS="OLD", ERR=9998)
DO k = 1,rhead-1
  READ (iun,*)
ENDDO

DO k = 1,chead
  READ (iun,'(a1)',ADVANCE="NO",IOSTAT=ios) ch1
  IF (ios /= 0) GOTO 9996
ENDDO

DO k = 1,mxpar
  READ (iun,'(1x,a10)',ADVANCE="NO",IOSTAT=ios) ch10
  IF (ios == eor) EXIT
  IF (ios /= 0) GOTO 9996
  IF (ch10 == "  Mod-wind" .OR. ch10 == "    FF ist") col_ff = k
  IF (ch10 == "  Dir-wind" .OR. ch10 == "    DD ist") col_dd = k
ENDDO

nf = k -1
CLOSE (iun)
RETURN

!-------------------------------------------------------------------------
! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine inq_file_smr"
RETURN

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file)
RETURN

9997 CONTINUE
WRITE (*,*) "Errore leggendo headers ",TRIM(file)
RETURN

9996 CONTINUE
WRITE (*,*) "Errore contando i campi ",TRIM(file)
RETURN

END SUBROUTINE inq_file_smr

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_rec_smr(iu,fmt,nf,data,hh,sca,val,ios)
!-------------------------------------------------------------------------
! Legge un record dati da un file SMR gia' aperto
!-------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: iu,fmt,nf
!
TYPE(date), INTENT(OUT) :: data
INTEGER, INTENT(OUT) :: hh,sca,ios
REAL, INTENT(OUT) :: val(nf)
!
CHARACTER(LEN=80) :: chfmt
!-------------------------------------------------------------------------

SELECT CASE (fmt)

CASE (1)                                                  ! seriet xls
  WRITE (chfmt,'(a,i3,a)') "(2(i2,1x),i4,1x,i2,1x,i3,",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%dd,data%mm,data%yy,hh,sca,val(1:nf)

CASE (2)                                                  ! seriet for

CASE (3)                                                  ! estra_orari
  WRITE (chfmt,'(a,i3,a)') "(i4,3(1x,i2),",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%yy,data%mm,data%dd,hh,val(1:nf)
  sca = 0

CASE (4)                                                  ! trasp_temp
  WRITE (chfmt,'(a,i3,a)') "(i4,3(1x,i2),6x,",nf,"(1x,f10.1))"
  READ (iu,chfmt,IOSTAT=ios) data%yy,data%mm,data%dd,hh,val(1:nf)
  sca = 0
END SELECT

RETURN
END SUBROUTINE read_rec_smr

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_namelist
!
OPEN (UNIT=40, FILE="scores_seriet_wind.naml", STATUS="REPLACE", FORM="FORMATTED")
!                 123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (40,'(a)') "&param"
WRITE (40,'(a)') "col_ff_oss = 2"
WRITE (40,'(a)') "col_dd_oss = 1"
WRITE (40,'(a)') "col_ff_pre = 2"
WRITE (40,'(a)') "col_dd_pre = 1"
WRITE (40,'(a)') "thr_calm   = 1."
WRITE (40,'(a)') "ncff       = 5"
WRITE (40,'(a)') "ncdd       = 5"
WRITE (40,'(a)') "/"
WRITE (40,'(a)') ""
WRITE (40,'(a)') "col_ff_oss : colonna velocita' vento nel file osservazioni"
WRITE (40,'(a)') "col_dd_oss : colonna direzione vento nel file osservazioni"
WRITE (40,'(a)') "col_ff_pre : colonna velocita' vento nel file previsioni"
WRITE (40,'(a)') "col_dd_pre : colonna direzione vento nel file previsioni"
WRITE (40,'(a)') "thr_calm   : soglia per le calme di vento (m/s; calma se ff <= thr_calm)"
WRITE (40,'(a)') "wc_ncff    : numero di calssi di velocita' nelle rose dei venti"
WRITE (40,'(a)') "wc_ncdd    : numero di calssi di direzione nelle rose dei venti"
CLOSE (40)

END SUBROUTINE scrive_namelist

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

PROGRAM trasp_temp
!--------------------------------------------------------------------------
! Legge un insieme di files TEMP prodotti da estra_temp.f90 relativi a una 
! stazione e scrive in un unico file la serie temporale dei dati su un 
! elenco di livelli. Calcola inoltre: 
! - Quota (m) e intensita' (K) dell'inversione termica al suolo
! - Valor medio della velocita' del vento nello strato compreso tra 
!   superficie e zref_w (m/s)
! - Energia necessaria per portare la temperatura del PBL a un profilo 
!   adiabatico corrispondente alla Temp.Pot. alla quota zref_t (MJ/m2)
! - Radiazione solare incidente a cielo sereno realtiva al giorno e alle 
!   coordinate del sondaggio (MJ/m2)
!
! Input: 
!   - trasp_temp.lst: contiene l'elenco dei TEMP da analizzare
!
! Output:
!   - temp_SSSSS.txt                   dati intabellati
!   - trasp_temp.log                   statistiche di base sui dati
!   - temp_SSSSS.dat, temp_SSSSS.ctl   files per GRADS
!
! Uso: trasp_temp.exe [-h] [-c]
!   -h: help a schermo
!   -c: crea un file trasp_temp.inp di esempio
! 
! NOTE:
! - Il programma funziona correttamente solo se i TEMP in input hanno 
!   tutti i parametri su tutti i livelli (i TEMP vecchi, devono essere
!   stati estratti con le opzioni -interp e -zidro).
! - Se le quote richieste in ouptut sono in m, il programma usa il 
!   riferimento (sup o SLM) richiesto in output: gli array idlev_out, z_in,
!   z_invers usano il riferimetno specificato da tlev_out.
!   Se i dati in input usano l'altro riferimento, vengono convertiti al
!   momento della lettura (tlev_in)
!
!                                                 V6.0.0, Enrico 12/09/2013
!--------------------------------------------------------------------------

USE datetime_class
IMPLICIT NONE 

! Parametri relativi ai files di I/O
REAL, PARAMETER :: rmis = -9999.       ! dato mancante in output
INTEGER, PARAMETER :: mxlev_in = 1000  ! max n.ro di livelli in un temp
INTEGER, PARAMETER :: mxlev_out = 100  ! max n.ro di livelli in output
INTEGER, PARAMETER :: mxtemp = 5000    ! max n.ro di sondaggi
INTEGER, PARAMETER :: fw = 10          ! ampiezza campi file di output

! Costanti fisiche
REAL, PARAMETER :: t0k = -273.15       ! Temperatura zero assoluto (°C)
REAL, PARAMETER :: rr = 287.           ! R (J/K*kg)
REAL, PARAMETER :: cp = 1004.          ! Cp (J/K*Kg)
REAL, PARAMETER :: gg = 9.8            ! g (m/s^2)
REAL, PARAMETER :: ha1 = 990.          ! Costante solare (W/m2)
REAL, PARAMETER :: ha2 = -30           ! Assorbimento atmosferico (W/m2)

! Nomi files accessori
CHARACTER(LEN=15), PARAMETER :: filelst = "trasp_temp.lst"
CHARACTER(LEN=15), PARAMETER :: filenml = "trasp_temp.inp"
CHARACTER(LEN=15), PARAMETER :: filelog = "trasp_temp.log"

! Array per input/output
TYPE (datetime), ALLOCATABLE :: data_out(:)
REAL, ALLOCATABLE :: field_out(:,:,:),t_invers(:),z_invers(:)
REAL, ALLOCATABLE :: val_ii(:),ff_ave(:),rad_csky(:)
REAL :: p_in(mxlev_in),field_in(4,mxlev_in),z_in(mxlev_in)

! Contatori statistici
INTEGER :: fok(4,mxlev_out),invok(5)
REAL :: fsum(4,mxlev_out),fsum2(4,mxlev_out),invsum(5),invsum2(5)
REAL :: fmax(4,mxlev_out),fmin(4,mxlev_out),invmax(5),invmin(5)
REAL :: fave(4,mxlev_out),fstd(4,mxlev_out),invave(5),invstd(5)

! Namelist
INTEGER :: step_usr,tlev_out,nlev_out,zint,skip_sup,skip_ug,tint
REAL :: idlev_out(mxlev_out),z_reft,z_refw

! Altre variabili del programma
TYPE (datetime) :: data_ini,data_end,datan,datah
TYPE (timedelta) :: step
REAL :: teta_in(mxlev_in),ro_in(mxlev_in),z_interf(mxlev_in+1)
REAL :: teta_ref,tt_ref,p_ref,ro_ref,dlnp_ref,rhh
REAL :: lonh,lath,lon,lat,f1(4),f2(4),z1,z2,sinalp,rad_sum
REAL :: dz,dtdz,dtpdz,incr,hstaz,ff_sum,tmax,hsup
INTEGER :: ntemp_in,ntemp_out,hstep,nvars,tlev_in
INTEGER :: iret,ios,ios1,ios2,irec,kpar,klev,kskip
INTEGER :: lok,l1,l2,ll,idlev_sup,lmax
INTEGER :: idstaz,yy,mm,dd,hh,nlev_in,ihstaz
INTEGER :: idstazn,yyn,mmn,ddn,hhn
INTEGER :: idstazh,yyh,mmh,ddh,hhh,ihstazh
INTEGER :: kt_out,kt_in,cnt_temp_skip,cnt_temp_ok,cnt_int_fail
INTEGER :: k,k2,p1,ks,nstep,njul
CHARACTER(LEN=100) :: filein,fileasc,filesta,filectl,filedat
CHARACTER(LEN=100) :: chfmt1,chfmt2,chfmt3,chfmt4,chfmt5,chfmtgr
CHARACTER(LEN=24) :: nome_staz,nome_stazh
CHARACTER(LEN=10) :: idpar(4),idparh(4),labz,labzh
CHARACTER(LEN=10) :: chpar,ch10a,ch10b,ch10
CHARACTER(LEN=5) :: id_tlev
CHARACTER(LEN=3) :: grmm
LOGICAL :: lskip_sup,lskip_ug,ok

! Interfaccia per la funzione str_fill (e' di tipo: CHARACTER(*) )
INTERFACE
  FUNCTION str_fill(str_in) RESULT(str_out)
  CHARACTER(*) :: str_in
  CHARACTER(LEN(str_in)) :: str_out,str_dum
  END FUNCTION str_fill
END INTERFACE

! Funzione per calcolo giorno giuliano
INTEGER :: jul

!--------------------------------------------------------------------------
! 1: Preliminari

! 1.1 Eventuale parametro da riga comando
CALL getarg(1,chpar)
IF (TRIM(chpar) == '-h') THEN
  CALL scrive_help
  STOP 1
ELSE IF (TRIM(chpar) == '-c') THEN
  CALL scrive_esempio
  STOP
ENDIF

! 1.2 Lettura opzioni
OPEN (UNIT=21, FILE=filenml, STATUS="OLD", ACTION="READ", & 
  ERR=9999)
READ (21,*,ERR=9999) step_usr
READ (21,*,ERR=9999) tlev_out
READ (21,*,ERR=9999) nlev_out
DO k = 1,nlev_out
  READ (21,*,ERR=9999) idlev_out(k)
ENDDO
READ (21,*,ERR=9999) zint
READ (21,*,ERR=9999) skip_sup
READ (21,*,ERR=9999) skip_ug
READ (21,*,ERR=9999) tint
READ (21,*,ERR=9999) z_refw
READ (21,*,ERR=9999) z_reft
CLOSE(21)

IF ((tlev_out /= 1 .AND. tlev_out /= 2 .AND. tlev_out /= 3) .OR. &
    (zint /= 1 .AND. zint /= 2)) GOTO 9999

IF (skip_sup == 0) THEN
  lskip_sup = .FALSE.
ELSE
  lskip_sup = .TRUE.
ENDIF
IF (skip_ug == 0) THEN
  lskip_ug = .FALSE.
ELSE
  lskip_ug = .TRUE.
ENDIF
IF (tlev_out == 1) THEN
  id_tlev = "hPa  "
ELSE IF (tlev_out == 2) THEN
  id_tlev = "m-SLM"
ELSE IF (tlev_out == 3) THEN
  id_tlev = "m-sup"
ENDIF  

! 1.3 Elaborazioni e controlli sulla lista dei TEMP richiesti
CALL proc_lst(filelst,data_ini,data_end,step,ntemp_in,ntemp_out,iret)
IF (iret /= 0) GOTO 9998
IF (step_usr /= 0) step = timedelta_new(HOUR=step_usr)

! 1.4 Altre operazioni preliminari

!Costruisco i formati
WRITE (chfmt1,'(a,i3,a,i2,a)') "(a9,a5,a1,4x,", &
  nlev_out*4+5,"(1x,f",fw,".0))"
WRITE (chfmt2,'(a,i3,a,i2,a)') "(a13,1x,a5,",nlev_out*4+5,"(1x,a",fw,"))"
WRITE (chfmt3,'(a,i3,a,i2,a)') "(i4.4,3(1x,i2.2),1x,i5,", &
  nlev_out*4+5,"(1x,f",fw,".1))"
WRITE (chfmt4,'(a,i3,a,i2,a)') "(a13,6x,",nlev_out*4+5,"(1x,f",fw,".1))"
WRITE (chfmt5,'(a,i3,a,i2,a)') "(a13,6x,",nlev_out*4+5,"(1x,i",fw,"))"

WRITE (chfmtgr,'(a,i3,a)') "(a,i2,a,",nlev_out,"i7)"

! Apro files
OPEN (UNIT=30, FILE=filelst, STATUS="OLD", ACTION="READ")
OPEN (UNIT=90, FILE=filelog, STATUS="REPLACE", FORM="FORMATTED")

! Alloco arrays
ALLOCATE (data_out(ntemp_out))
ALLOCATE (val_ii(ntemp_out),ff_ave(ntemp_out),rad_csky(ntemp_out))
ALLOCATE (field_out(4,mxlev_out,ntemp_out),t_invers(ntemp_out),z_invers(ntemp_out))

!==========================================================================
! 2) Ciclo sui radiosondaggi richiesti in output

! 2.0 Inizializzazioni

! Arrays per output
field_out(:,:,:) = rmis
z_invers(:) = rmis
t_invers(:) = rmis
val_ii(:) = rmis
ff_ave(:) = rmis
rad_csky(:) = rmis

! Contatori statistici
fok(:,:) = 0
fsum(:,:) = 0.
fsum2(:,:) = 0.
fmax(:,:) = -HUGE(0.)
fmin(:,:) = HUGE(0.)
invok(:) = 0
invsum(:) = 0.
invsum2(:) = 0.
invmax(:) = -HUGE(0.)
invmin(:) = HUGE(0.)

! Contatori per log
cnt_temp_ok = 0
cnt_temp_skip = 0
cnt_int_fail = 0

! Varie
idpar(1:4) = ""

kt_out = 0
temp: DO kt_in = 1,ntemp_in

!--------------------------------------------------------------------------
! 2.1 Elaborazioni preliminari

! Leggo ed elaboro il nome del prosismo file richiesto
  kt_out = kt_out + 1
  READ (30,'(a)',IOSTAT=ios) filein
  IF (ios /= 0) EXIT temp

  p1 = INDEX(filein,"/",BACK=.TRUE.)
  READ(filein(p1+1:),'(i5,1x,i4,3i2)',IOSTAT=ios) idstazn,yyn,mmn,ddn,hhn
  IF (ios /= 0) GOTO 9997
  CALL init(datan, YEAR=yyn, MONTH=mmn, DAY=ddn, HOUR=hhn)

! Se ci sono buchi nella serie dei TEMP richiesti, lascio i dati mancanti
  DO kskip = 1,HUGE(0)
    data_out(kt_out) = data_ini + (kt_out - 1) * step

! call getval(data_out(kt_out),simpledate=ch10a)
! call getval(datan,simpledate=ch10b)
! print *,"kt_in,kt_out,datan,data_out",kt_in,kt_out,ch10b," ",ch10a

    IF (datan < data_out(kt_out)) GOTO 9996
    IF (datan == data_out(kt_out)) EXIT
    CALL getval(data_out(kt_out),SIMPLEDATE=ch10)
    WRITE (*,'(a,6x,2a)') "* ",ch10,": TEMP non presente in input"
    cnt_temp_skip = cnt_temp_skip + 1
    kt_out = kt_out + 1
  ENDDO

!--------------------------------------------------------------------------
! 2.2) Leggo il prossimo TEMP. 
!      Se non ci sono i due record di intestazione e almeno un record di 
!      dati, oppure se c'e' un errore nella lettura dati, mettero' tutto a 
!      "manca dato". Salto sempre i livelli con z mancante, e se richiesto 
!      anche quello alla superifice.

! 2.2.1 Apro file e leggo header
  OPEN (UNIT=31, FILE=filein, STATUS="OLD", ACTION="READ",IOSTAT=ios)
  READ (31,'(1x,i5,1x,a24,1x,i4.4,2(i2),1x,i2,3x,i3,1x,i4,2(1x,f10.5))', &
    IOSTAT=ios1) idstazh,nome_stazh,yyh,mmh,ddh,hhh,nlev_in,ihstazh,lonh,lath
  READ (31,'(11x,5(1x,a10))',IOSTAT=ios2) idparh(1:4),labzh
  CALL init(datah, YEAR=yyh, MONTH=mmh, DAY=ddh, HOUR=hhh)

! 2.2.2 Controlli sugli header
  IF (ios /= 0 .OR. ios1 /= 0 .OR. ios2 /= 0) THEN
    WRITE (*,*) "* ",TRIM(filein)," errore lettura header, metto mancante"
    CYCLE temp
  ELSE IF (idstazh/=idstazn .OR. datah/=datan) THEN
    WRITE (*,*) "* ",TRIM(filein), &
      " header incompatibile con nome file, metto mancante"
    CYCLE temp
  ELSE IF (nlev_in < 1) THEN
    WRITE (*,*) "* ",TRIM(filein)," nessun livello in input, metto mancante"
    CYCLE temp
  ELSE IF (nlev_in > mxlev_in) THEN
    GOTO 9995
  ENDIF

  IF (kt_in == 1) THEN
    idstaz = idstazh
    nome_staz = nome_stazh
    ihstaz = ihstazh
    hstaz = REAL(ihstaz)
    lat = lath
    lon = lonh
    idpar(1:4) = idparh(1:4)
    labz = labzh
    IF (TRIM(ADJUSTL(labz)) == "Z-sup") THEN
      tlev_in = 3
    ELSE IF (TRIM(ADJUSTL(labz)) == "Z-slm" .OR. &
             TRIM(ADJUSTL(labz)) == "Quota") THEN
      tlev_in = 2
    ELSE
      GOTO 9993
    ENDIF
    IF (tlev_out == 3) THEN
      hsup = 0.
    ELSE
      hsup = hstaz
    ENDIF

  ELSE IF (idstazh/=idstaz .OR. nome_stazh/=nome_staz .OR. ihstazh/=ihstaz .OR. &
           lonh/=lon .OR. lath/=lat .OR. ANY(idparh(1:4)/=idpar(1:4)) .OR. &
           labzh/=labz) THEN
    WRITE (*,*) "* ",TRIM(filein), &
      " WARNING: valori di anagrafica diversi rispetto al primo TEMP"
  ENDIF
  
! 2.2.3 Leggo i dati e salvo i valori nei rispettivi array:
!   p_in, z_in:     Pressione (mb), quota (m SLM)
!   field_in(1:4):  T(C), Td(C), DD(gradi), FF (m/s)

  lok = 1
  lev_in: DO k = 1,nlev_in
    READ(31,*,IOSTAT=ios) p_in(lok),field_in(1:4,lok),z_in(lok)
    IF (ios /= 0) THEN                              ! errore di lettura
      WRITE (*,*) "Errore leggendo ",TRIM(filein)," metto mancante"
      CYCLE temp
    ELSE IF (p_in(lok) == rmis .OR. z_in(lok) == rmis .OR. &
      (lskip_ug .AND. z_in(lok) < hsup) .OR. &
      (lskip_sup .AND. z_in(lok)-hsup < 1.)) THEN  ! salto il livello
      CYCLE lev_in
    ELSE                                            ! livello valido
      lok = lok + 1
    ENDIF  
  ENDDO lev_in
  IF (tlev_in == 3 .AND. tlev_out == 2) THEN
    WHERE (z_in(1:nlev_in) /= rmis)
      z_in(1:nlev_in) = z_in(1:nlev_in) + hstaz
    ENDWHERE
  ELSE IF (tlev_in == 2 .AND. tlev_out == 3) THEN
    WHERE (z_in(1:nlev_in) /= rmis)
      z_in(1:nlev_in) = z_in(1:nlev_in) - hstaz
    ENDWHERE
  ENDIF
  
  cnt_temp_ok = cnt_temp_ok + 1
  WRITE (*,'(3a,i4,a,i4)') ". ",TRIM(filein)," liv.file: ",nlev_in, &
    " liv.mantenuti: ",lok-1
  WRITE (90,*)
  WRITE (90,'(3a,i4,a,i4)') ". ",TRIM(filein)," liv.file: ",nlev_in, &
    " liv.mantenuti: ",lok-1
  nlev_in = lok - 1
  
  CLOSE(31)
  WRITE(90,*) "Elaboro rsd: ",TRIM(filein)

!--------------------------------------------------------------------------
! 2.3 Interplo sui livelli richiesti

  DO k = 1, nlev_out

! Trovo i livelli sotto e sopra a quello richiesto
    l2 = 1
    IF (tlev_out==1) THEN                            ! hPa
      DO WHILE ( p_in(l2) >= idlev_out(k) )
        l2 = l2 + 1
        IF (l2 > nlev_in) EXIT
      ENDDO
    ELSE                                             ! metri
      DO WHILE ( z_in(l2) <= idlev_out(k) )
        l2 = l2 + 1
        IF (l2 > nlev_in) EXIT
      ENDDO
    ENDIF

    IF (l2 <= nlev_in) THEN
      l1 = l2 - 1
    ELSE
      l1 = 0
    ENDIF

! Calcolo i dati sui livelli richiesti
    IF (l1 /= 0) THEN

!     interpolazione in ln(P)
      IF (zint==1 .AND. tlev_out==1 .AND. &
          p_in(l1) > 0. .AND. p_in(l2) > 0.) THEN 

        f1(:) = field_in(:,l1)
        f2(:) = field_in(:,l2)
        z1 = LOG(p_in(l1))
        z2 = LOG(p_in(l2))

        WHERE (f1(:) /= rmis .AND. f2(:) /= rmis)
          field_out(:,k,kt_out) = f1(:) + (f2(:)-f1(:))/(z2-z1) * &
            (LOG(idlev_out(k))-z1)
        ELSEWHERE
          field_out(:,k,kt_out) = rmis
        ENDWHERE

!     interpolazione in z
      ELSE IF (zint==1 .AND. (tlev_out==2 .OR. tlev_out==3)) THEN

        f1 = field_in(:,l1)
        f2 = field_in(:,l2)
        z1 = z_in(l1)
        z2 = z_in(l2)
        WHERE (f1 /= rmis .AND. f2 /= rmis)
          field_out(:,k,kt_out) = f1 + (f2-f1)/(z2-z1) * (idlev_out(k)-z1)
        ELSEWHERE
          field_out(:,k,kt_out) = rmis
        ENDWHERE

!     livello piu' vicino in ln(P)
      ELSE IF (zint==2 .AND. tlev_out==1 .AND. &
               p_in(l1) > 0. .AND. p_in(l2) > 0.) THEN 

        z1 = LOG(p_in(l1))
        z2 = LOG(p_in(l2))
        IF ( LOG(p_in(l1)) - LOG(idlev_out(k)) <  &
             LOG(idlev_out(k)) - LOG(p_in(l2)) ) THEN
          ll = l1
        ELSE
          ll = l2
        ENDIF

        WHERE (field_in(:,ll) /= rmis)
          field_out(:,k,kt_out) = field_in(:,ll)
        ELSEWHERE
          field_out(:,k,kt_out) = rmis
        ENDWHERE

!     livello piu' vicino in z
      ELSE IF (zint==2 .AND. (tlev_out==2 .OR. tlev_out==3)) THEN

        z1 = z_in(l1)
        z2 = z_in(l2)
        IF (idlev_out(k)-z1 < z2-idlev_out(k)) THEN
          ll = l1
        ELSE
          ll = l2
        ENDIF

        WHERE (field_in(:,ll) /= rmis)
          field_out(:,k,kt_out) = field_in(:,ll)
        ELSEWHERE
          field_out(:,k,kt_out) = rmis
        ENDWHERE
      ENDIF

!   interpolazione impossibile
    ELSE
      field_out(:,k,kt_out) = rmis

    ENDIF
  ENDDO

!--------------------------------------------------------------------------
! 2.4 Calcolo l'inversione al suolo

! Salto eventuali livelli sottoterra
  idlev_sup = nlev_in
  DO k2 = 1,nlev_in
    IF (z_in(k2) >= hsup) THEN
      idlev_sup = k2
      EXIT
    ENDIF
  ENDDO

  IF (idlev_sup == nlev_in .OR. field_in(1,idlev_sup) == rmis) THEN
    t_invers(kt_out) = rmis
    z_invers(kt_out) = rmis

  ELSE
    tmax = MAXVAL(field_in(1,idlev_sup+1:nlev_in), &
      MASK=field_in(1,idlev_sup+1:nlev_in)/=rmis)
    lmax = MAXLOC(field_in(1,idlev_sup+1:nlev_in), &
      MASK=field_in(1,idlev_sup+1:nlev_in)/=rmis, DIM=1) + idlev_sup
    IF (tmax > field_in(1,idlev_sup)) THEN
      t_invers(kt_out) = tmax - field_in(1,idlev_sup)
      z_invers(kt_out) = z_in(lmax) - hsup
    ELSE
      t_invers(kt_out) = 0.
      z_invers(kt_out) = 0.
    ENDIF

  ENDIF

!print *,idlev_sup,field_in(1,idlev_sup)
!print *,tmax,lmax,hstaz
!print *,"out: ",t_invers(kt_out),z_in(lmax)

!--------------------------------------------------------------------------
! 2.5 Calcolo la velocita' media del vento nel PBL
! Considero FF costante nello strato centrato su ciascun livello di misura 
! (i.e. interpolazione verticale usa il livello piu' vicino); calcolo la
! media di FF pesata per lo spessore di ciascuno strato.

! Salto eventuali livelli sottoterra
  idlev_sup = nlev_in
  DO k2 = 1,nlev_in
    IF (z_in(k2) >= hsup) THEN
      idlev_sup = k2
      EXIT
    ENDIF
  ENDDO

  IF (idlev_sup == nlev_in .OR. field_in(4,idlev_sup) == rmis) THEN
    ff_ave(kt_out) = rmis
  ELSE
    WRITE (90,'(a,i3)') "FF media PBL: idlev_sup" ,idlev_sup

!   Calcolo le quote delle interfacce tra gli strati
    z_interf(idlev_sup) = hsup
    DO klev = idlev_sup+1, nlev_in
      z_interf(klev) = 0.5 * (z_in(klev) + z_in(klev-1))
    ENDDO
  
    ff_sum = 0.
    ok = .FALSE.
    DO klev = idlev_sup,nlev_in-1
      IF (field_in(4,klev) == rmis) THEN
        EXIT
      ELSE IF (z_interf(klev+1) < z_refw) THEN
        ff_sum = ff_sum + field_in(4,klev) * (z_interf(klev+1) - z_interf(klev))
        WRITE (90,'(a,i4,f6.1,f6.0)') "livello, ff, spessore ", &
          klev, field_in(4,klev), z_interf(klev+1)-z_interf(klev)
      ELSE
        ff_sum = ff_sum + field_in(4,klev) * (z_refw - z_interf(klev))
        WRITE (90,'(a,i4,f6.1,f6.0)') "Ulimo livello, ff, spessore ", &
          klev, field_in(4,klev), z_refw-z_interf(klev)
        ok = .TRUE.
        EXIT
      ENDIF
    ENDDO
  
    IF (ok) THEN
      ff_ave(kt_out) = ff_sum / (z_refw - hsup)
    ELSE
      ff_ave(kt_out) = rmis
    ENDIF


  ENDIF

!--------------------------------------------------------------------------
! 2.6 Calcolo l'inversione integrale

! Calcolo  Temp.Pot. e densita' su tutti i livelli;
  teta_in(1:nlev_in) = (field_in(1,1:nlev_in) - t0k) * &
    (1000./p_in(1:nlev_in)) ** (rr/cp)
  ro_in(1:nlev_in) = (100.*p_in(1:nlev_in)) / &
    (rr*(field_in(1,1:nlev_in) - t0k))

!print *,field_in(1,1:nlev_in)
!print *,p_in(1:nlev_in)
!print *,teta_in(1:nlev_in)
!read *

! Trovo i livelli sopra e sotto rispetto a z_reft
  l2 = 1
  DO WHILE (z_in(l2) <= z_reft)
    l2 = l2 + 1
    IF (l2 > nlev_in) EXIT
  ENDDO

! Eseguo il calcolo solo se ho trovato un livello sopra e uno sotto a z_ref.

  IF (l2 <= nlev_in .AND. l2 > 1) THEN
    l1 = l2 - 1

!   Segnalo eventuali strati superadibatici al di sotto di Z_reft
    DO klev = 1, l1
      IF (teta_in(klev+1) < teta_in(klev)) THEN
        WRITE (90,'(a,i5,a,i5,a,f8.3)') "Warning: strato superadiabatico: z= ", &
          NINT(z_in(klev))," - ",NINT(z_in(klev+1)), &
          " dt = ",teta_in(klev+1)-teta_in(klev)
!       teta_in(klev+1) = teta_in(klev)
      ENDIF
    ENDDO
   
!   Calcolo i parametri atmosferici alla quota Z_reft
    dtdz = (field_in(1,l2) - field_in(1,l1)) / (z_in(l2) - z_in(l1))
    tt_ref = field_in(1,l1) + dtdz * (z_reft - z_in(l1))
    dtpdz = (teta_in(l2) - teta_in(l1)) / (z_in(l2) - z_in(l1))
    teta_ref = teta_in(l1) + dtpdz * (z_reft - z_in(l1))
   
    dlnp_ref = -gg * (z_reft - z_in(l1)) / (rr * (0.5*(tt_ref+field_in(1,l1))-t0k))
    IF (dlnp_ref < -10.*EPSILON(0.)) THEN
      p_ref = EXP(LOG(p_in(l1)) + dlnp_ref)
    ELSE IF (dlnp_ref < 10.*EPSILON(0.)) THEN
      p_ref = p_in(l1)
    ELSE
      p_ref = p_in(l1)
      WRITE (*,*) "Warning: calcolo P(z_reft) fallito !!!"
    ENDIF
    ro_ref = (100. * p_ref) / (rr*(tt_ref - t0k))
   
    WRITE (90,'(a)') "Inv.int: z_top,lay_top,teta_ref,tt_ref,p_ref,ro_ref "
    WRITE (90,'(8x,f6.1,i8,2f8.2,f6.0,f7.3)') &
      z_reft,l2,teta_ref,tt_ref,p_ref,ro_ref
    WRITE (90,*)
   
!   Calcolo inversione integrale

!   a) layer tra superficie e primo livello valido 
!      considero Tp costante (i.e. gradiente adiabatico)
    val_ii(kt_out) = (z_in(1) - hsup) * (teta_ref - teta_in(1)) * cp * ro_in(1)
   
    WRITE (90,'(a,i2,f5.0,f6.1,f6.0,f6.3,e10.3)') &
      "layer,TPlow,Dteta,Dz,Ro_ave,incr: ",&
      0,teta_in(1),teta_ref-teta_in(1),z_in(1)-hsup,ro_in(1),val_ii(kt_out) 
   
!   b) aggiungo il contributo dei layers interni
    DO klev = 1, l1-1
      dz = z_in(klev+1) - z_in(klev)
      incr = dz * (teta_ref - 0.5*(teta_in(klev+1)+teta_in(klev))) * &
        cp * 0.5*(ro_in(klev+1)+ro_in(klev))
      val_ii(kt_out) = val_ii(kt_out) + incr
   
      WRITE (90,'(a,i2,f5.0,f6.1,f6.0,f6.3,e10.3)') &
        "layer,TPlow,Dteta,Dz,Ro_ave,incr: ",&
        klev,teta_in(klev),teta_ref-0.5*(teta_in(klev+1)+teta_in(klev)), &
         dz,0.5*(ro_in(klev+1)+ro_in(klev)),incr
    ENDDO
   
!   c) aggiungo il contributo del layer che comprende z_ref
    incr = 0.5 * (z_reft-z_in(l1)) * (teta_ref - teta_in(l1)) * &
      cp * 0.5*(ro_in(l1)+ro_ref)
    val_ii(kt_out) = val_ii(kt_out) + incr
   
    WRITE (90,'(a,i2,f5.0,f6.1,f6.0,f6.3,e10.3)') &
      "layer,TPlow,Dteta,Dz,Ro_ave,incr: ",&
      l1, teta_in(l1), teta_ref-teta_in(l1), z_reft-z_in(l1), &
      0.5*(ro_in(l1)+ro_ref), incr

!   d) converto da J/m2 a Mj/m2
    val_ii(kt_out) = val_ii(kt_out) * 1.e-6
   

  ELSE
    val_ii(kt_out) = rmis

  ENDIF

!--------------------------------------------------------------------------
! 2.7 Calcolo la radiazione solare incidente a cielo sereno integrata
! nella giornata (Mj/m2)

  njul = jul(data_out(kt_out))
  nstep = 24*6                          ! calcolo ogni 10'
  rad_sum = 0.
  DO ks = 1,nstep
    rhh = REAL(ks) * 24. / REAL(nstep)
    CALL solar(njul,rhh,lat,lon,sinalp)                     
    rad_sum = rad_sum + (ha1 * sinalp + ha2)
  ENDDO
  rad_csky(kt_out) = rad_sum * (3600.*24)/REAL(nstep) * 1.e-6

!--------------------------------------------------------------------------
! 2.8 Log calcoli falliti

  IF (ANY(field_out(:,1:nlev_out,kt_out) == rmis) .OR. &
    t_invers(kt_out) == rmis .OR. z_invers(kt_out) == rmis .OR. & 
    ff_ave(kt_out) == rmis .OR. val_ii(kt_out) == rmis .OR. rad_csky(kt_out) == rmis) &
    cnt_int_fail = cnt_int_fail + 1

ENDDO temp

WRITE (*,'(a,i4,a)') "Lettura completata, letti ",cnt_temp_ok," sondaggi"

!==========================================================================
! 3) Interploazione temporale dei dati mancanti (se richiesta)

IF (tint == 0) THEN
  WRITE (*,*) "Eseguo interpolazione temporale"

  DO kpar = 1,4
  DO klev = 1,nlev_out
    CALL tinterp_scalar(ntemp_out,field_out(kpar,klev,1:ntemp_out),rmis)
  ENDDO
  ENDDO

  CALL tinterp_scalar(ntemp_out,t_invers(1:ntemp_out),rmis)
  CALL tinterp_scalar(ntemp_out,z_invers(1:ntemp_out),rmis)
  CALL tinterp_scalar(ntemp_out,ff_ave(1:ntemp_out),rmis)
  CALL tinterp_scalar(ntemp_out,val_ii(1:ntemp_out),rmis)
  CALL tinterp_scalar(ntemp_out,rad_csky(1:ntemp_out),rmis)
ENDIF

!==========================================================================
! 4) Output 

!--------------------------------------------------------------------------
! 4.1 Ouptut ASCII

WRITE (fileasc,'(a,i5.5,a)') "temp_",idstaz,".txt"
OPEN (UNIT=40, FILE=fileasc, STATUS="REPLACE", ACTION="WRITE")

! Scrivo i record d'intestazione
WRITE (40,chfmt1) "Livelli (",id_tlev,")",&
  ((idlev_out(k),k2=1,4),k=1,nlev_out),0.,0.,0.,0.,0.
WRITE (40,*)
WRITE (40,chfmt2) "aaaa mm gg hh","staz.", &
  ((idpar(k2),k2=1,4),k=1,nlev_out),&
  "T invers.","Z invers.","inv.inte.","rad.csky.","   ff_ave"

! Scrivo i record dei dati
DO kt_out = 1,ntemp_out
  CALL getval(data_out(kt_out), YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
  WRITE (40,chfmt3) yy,mm,dd,hh,idstaz,&
    (field_out(1:4,k,kt_out),k=1,nlev_out), &
    t_invers(kt_out),z_invers(kt_out),val_ii(kt_out),rad_csky(kt_out),ff_ave(kt_out)
ENDDO

CLOSE (40)

!--------------------------------------------------------------------------
! 4.2) Output statistico

! 4.2.1 Calcolo le statistiche
fok(:,:)  = COUNT(field_out(:,:,:) /= rmis, DIM=3)
fsum(:,:) = SUM(field_out(:,:,:),    DIM=3, MASK=field_out(:,:,:)/=rmis)
fsum2(:,:)= SUM(field_out(:,:,:)**2, DIM=3, MASK=field_out(:,:,:)/=rmis)
fmax(:,:) = MAXVAL(field_out(:,:,:), DIM=3, MASK=field_out(:,:,:)/=rmis)
fmin(:,:) = MINVAL(field_out(:,:,:), DIM=3, MASK=field_out(:,:,:)/=rmis)

invok(1)  = COUNT(t_invers(:) /= rmis)
invsum(1) = SUM(t_invers(:), MASK=t_invers(:)/=rmis)
invsum2(1)= SUM(t_invers(:)**2, MASK=t_invers(:)/=rmis)
invmax(1) = MAXVAL(t_invers(:), MASK=t_invers(:)/=rmis)
invmin(1) = MINVAL(t_invers(:), MASK=t_invers(:)/=rmis)

invok(2)  = COUNT(z_invers(:) /= rmis)
invsum(2) = SUM(z_invers(:), MASK=z_invers(:)/=rmis)
invsum2(2)= SUM(z_invers(:)**2, MASK=z_invers(:)/=rmis)
invmax(2) = MAXVAL(z_invers(:), MASK=z_invers(:)/=rmis)
invmin(2) = MINVAL(z_invers(:), MASK=z_invers(:)/=rmis)

invok(3)  = COUNT(val_ii(:) /= rmis)
invsum(3) = SUM(val_ii(:), MASK=val_ii(:)/=rmis)
invsum2(3)= SUM(val_ii(:)**2, MASK=val_ii(:)/=rmis)
invmax(3) = MAXVAL(val_ii(:), MASK=val_ii(:)/=rmis)
invmin(3) = MINVAL(val_ii(:), MASK=val_ii(:)/=rmis)

invok(4)  = COUNT(rad_csky(:) /= rmis)
invsum(4) = SUM(rad_csky(:), MASK=rad_csky(:)/=rmis)
invsum2(4)= SUM(rad_csky(:)**2, MASK=rad_csky(:)/=rmis)
invmax(4) = MAXVAL(rad_csky(:), MASK=rad_csky(:)/=rmis)
invmin(4) = MINVAL(rad_csky(:), MASK=rad_csky(:)/=rmis)

invok(5)  = COUNT(ff_ave(:) /= rmis)
invsum(5) = SUM(ff_ave(:), MASK=ff_ave(:)/=rmis)
invsum2(5)= SUM(ff_ave(:)**2, MASK=ff_ave(:)/=rmis)
invmax(5) = MAXVAL(ff_ave(:), MASK=ff_ave(:)/=rmis)
invmin(5) = MINVAL(ff_ave(:), MASK=ff_ave(:)/=rmis)

WHERE (fok(:,:) > 0)
  fave(:,:) = fsum(:,:) / fok(:,:)
  fstd(:,:) = SQRT(MAX( fsum2(:,:)/fok(:,:) - fave(:,:)**2, 0.))
ELSEWHERE
  fmax(:,:) = rmis
  fmin(:,:) = rmis
  fave(:,:) = rmis
  fstd(:,:) = rmis
ENDWHERE
WHERE (invok(:) > 0)
  invave(:) = invsum(:) / invok(:)
  invstd(:) = SQRT(MAX( invsum2(:)/invok(:) - invave(:)**2, 0.))
ELSEWHERE
  invmax(:) = rmis
  invmin(:) = rmis
  invave(:) = rmis
  invstd(:) = rmis
ENDWHERE

! 4.2.2 Le scrivo
WRITE (filesta,'(a,i5.5,a)') "temp_",idstaz,"_stat.txt"
OPEN (UNIT=41, FILE=filesta, STATUS="REPLACE", ACTION="WRITE")

WRITE (41,chfmt1) "Livelli (",id_tlev,")",&
  ((idlev_out(k),k2=1,4),k=1,nlev_out),0.,0.,0.,0.,0.
WRITE (41,*)
WRITE (41,chfmt2) "-------------","     ",((idpar(k2),k2=1,4),k=1,nlev_out), &
  "T invers.","Z invers.","inv.inte.","rad.csky.","   ff_ave"
WRITE (41,chfmt4) "Massimo      ",(fmax(:,k),k=1,nlev_out),invmax(:) 
WRITE (41,chfmt4) "Minimo       ",(fmin(:,k),k=1,nlev_out),invmin(:) 
WRITE (41,chfmt4) "Media        ",(fave(:,k),k=1,nlev_out),invave(:) 
WRITE (41,chfmt4) "Dev.standard ",(fstd(:,k),k=1,nlev_out),invstd(:) 
WRITE (41,chfmt5) "Dati validi  ",(fok(:,k),k=1,nlev_out),invok(:) 
WRITE (41,chfmt5) "Dati rich.   ",(ntemp_out,k=1,nlev_out*4+5)

CLOSE (41)

!--------------------------------------------------------------------------
! 4.3) Output GRADS: file .dat

WRITE (filedat,'(a,i5.5,a)') "temp_",idstaz,".dat"
OPEN (32, FILE=filedat, FORM='UNFORMATTED', STATUS="REPLACE", &
  ACCESS='DIRECT', RECL=4)

!print *,ntemp_out,nlev_out
!print *,field_out(1:4,1,1)
!print *,t_invers,z_invers,val_ii,rad_csky,ff_ave

irec = 1
DO kt_out = 1,ntemp_out
  DO kpar = 1,4
  DO klev = 1,nlev_out
    WRITE (32, REC=irec) field_out(kpar,klev,kt_out)
    irec = irec + 1
  ENDDO
  ENDDO

  WRITE (32, REC=irec) t_invers(kt_out)
  irec = irec + 1
  WRITE (32, REC=irec) z_invers(kt_out)
  irec = irec + 1
  WRITE (32, REC=irec) val_ii(kt_out)
  irec = irec + 1
  WRITE (32, REC=irec) rad_csky(kt_out)
  irec = irec + 1
  WRITE (32, REC=irec) ff_ave(kt_out)
  irec = irec + 1

ENDDO
CLOSE (32)

!--------------------------------------------------------------------------
! 4.4) Output GRADS: file .ctl

CALL getval(data_ini, YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
CALL getval(step, AHOUR=hstep)
SELECT CASE (mm)
CASE(1)
  grmm="Jan"
CASE(2)
  grmm="Feb"
CASE(3)
  grmm="Mar"
CASE(4)
  grmm="Apr"
CASE(5)
  grmm="May"
CASE(6)
  grmm="Jun"
CASE(7)
  grmm="Jul"
CASE(8)
  grmm="Aug"
CASE(9)
  grmm="Sep"
CASE(10)
  grmm="Oct"
CASE(11)
  grmm="Nov"
CASE(12)
  grmm="Dec"
END SELECT

WRITE (filectl,'(a,i5.5,a)') "temp_",idstaz,".ctl"
OPEN (UNIT=33, FILE=filectl, STATUS="REPLACE", FORM="FORMATTED")

WRITE (33,'(3a)')          "DSET   ","^",TRIM(filedat)
WRITE (33,'(3a)')          "TITLE  ","temp ",TRIM(nome_staz)
WRITE (33,'(a,f10.3)')     "UNDEF  ",rmis   
WRITE (33,'(2a)')          "XDEF   ","1 linear 1 1"
WRITE (33,'(2a)')          "YDEF   ","1 linear 1 1"

IF (nlev_out > 0) THEN
  WRITE (33,chfmtgr)       "ZDEF   ",nlev_out," levels ", &
    NINT(idlev_out(1:nlev_out))
ELSE
  WRITE (33,'(a)')         "ZDEF    1 levels 0"
ENDIF  

WRITE (33,'(a,i5,a,i2.2,a,i2.2,a3,i4.4,a,i3.3,a)') "TDEF   ", &
  ntemp_out," linear ",hh,"z",dd,grmm,yy," ",hstep,"hr"

nvars = 5
IF (nlev_out > 0) nvars = nvars + 4
WRITE (33,'(a,i2)')        "VARS   ",nvars

IF (nlev_out > 0) THEN
DO kpar = 1,4
  WRITE (33,'(a10,1x,i2,2a)') &
    str_fill(idpar(kpar)),nlev_out," 99 ", idpar(kpar)
ENDDO
ENDIF

WRITE (33,'(a)')         "Tinv 0 99 Intensita' inversione (gradi C)"
WRITE (33,'(a)')         "Zinv 0 99 Altezza inversione (m)"
WRITE (33,'(a)')         "intinv 0 99 inversione integrale (MJ/m2)"
WRITE (33,'(a)')         "radcsky 0 99 radiazione incidente clear sky (MJ/m2)"
WRITE (33,'(a)')         "ffpbl 0 99 velocita' vento nel PBL (m/s)"

WRITE (33,'(a)')           "ENDVARS"
CLOSE (33)

!==========================================================================
! 5) Conclusione

WRITE (*,'(2(a,i4))') "Elaborazioni completata: scritti ",ntemp_out," sondaggi"
IF (cnt_int_fail > 0) WRITE (*,'(a,i4,a)') &
  "Interpolazioni/calcoli impossibili in ",cnt_int_fail," sondaggi"

STOP

!==========================================================================
! 6) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore leggendo trasp_temp.inp"
STOP 1

9998 CONTINUE
IF (iret == 1) THEN
  WRITE (*,*) "Errore aprendo ",TRIM(filelst)
ELSE IF (iret == 2) THEN
  WRITE (*,*) TRIM(filelst)," e' un file vuoto"
ELSE IF (iret == 3) THEN
  WRITE (*,*) "I TEMP non si riferiscono alla stessa stazione"
ELSE IF (iret == 4) THEN
  WRITE (*,*) "Errore leggendo ",TRIM(filelst)
ELSE IF (iret == 5) THEN
  WRITE (*,*) "I TEMP non sono in ordine cronologico (riga ",k,"di ", &
    TRIM(filelst),")"
ENDIF
STOP 2

9997 CONTINUE
WRITE (*,*) "Nome file illegale in ",TRIM(filelst), "riga ",kt_in
WRITE (*,*) TRIM(filein)
STOP 2

9996 CONTINUE
WRITE (*,*) "Trovato un TEMP fuori dalla sequenza delle date: ",yy,mm,dd,hh
STOP 3

9995 CONTINUE
WRITE (*,*) "Troppi livelli in input(",nlev_in, &
  "), aumentare il parametro mxlev_in (",mxlev_in,")"
STOP 4

9994 CONTINUE
WRITE (*,*) "Errore, i temp contengono parametri diversi: "
WRITE (*,*) "rilanciare l'estrazione temp usando le stesse opzioni"
STOP 5

9993 CONTINUE
WRITE (*,*) "Tipo di livelli in input sconosciuto in ",TRIM(filein)
WRITE (*,*) labz
STOP 6

END PROGRAM trasp_temp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE solar(njul,hh,xlat,xlon,sinalp)
!--------------------------------------------------------------------------
! Da calmet
! Calcola l'angolo solare in un punto per una data ora (GMT)
!--------------------------------------------------------------------------
IMPLICIT NONE

! Argomenti
INTEGER, INTENT(IN) :: njul                    ! giorno giuliano richiesto
REAL, INTENT(IN) :: hh                         ! ora GMT richiesta
REAL, INTENT(IN) :: xlat,xlon                  ! coord. punto richiesto

REAL, INTENT(OUT) :: sinalp                    ! angolo solare per ogni ora

! Altre variabili della subroutine
REAL :: radd,xsind,xcosd,rad2d,cos2d,sin2d,em,sigma,sincd,capd,coscd
REAL :: radlat,sinlat,coslat,solha,d,mxlon

!--------------------------------------------------------------------------
! Calcoli preliminari

! La longitudine di calmet ha il segno invertito!!
mxlon = -xlon

d = (float(njul) -1.) * 360./365.242
radd = 0.0174533 * d
xsind = SIN(radd)
xcosd = COS(radd)
rad2d = 2. * radd
sin2d = SIN(rad2d)
cos2d = COS(rad2d)
em = 12. + 0.12357 * xsind - 0.004289 * xcosd + 0.153809 * sin2d + &
     0.060783 * cos2d
sigma = 279.9348 + d + 1.914827 * xsind - 0.079525 * xcosd + 0.019938 * &
        sin2d - 0.00162 * cos2d
sincd = 0.39784989 * SIN(0.0174533 * sigma)
capd = ASIN(sincd)
coscd = COS(capd)

! Calcolo angolo solare
radlat = 0.0174533 * xlat
sinlat = SIN(radlat)
coslat = COS(radlat)

solha = 15. * (hh - em) - mxlon
sinalp = sinlat * sincd + coslat * coscd * &
  COS(0.0174533 * solha)
IF (sinalp < 0.) sinalp = 0.

RETURN
END SUBROUTINE solar

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE proc_lst(filelst,data_ini,data_end,step,ntemp_in,ntemp_out,iret)
!--------------------------------------------------------------------------
! Controlla che il file trasp_temp.lst contenga files relativi a una sola 
! stazione, e che le date siano in ordine crescente. 
! Ritorna:
! - data iniziale, data finale, step temporale minimo.
! - n.ro sondaggi in input (= nel file), e in ouptut (= con step costante)
!
! Codici per iret:
! 0: tutto ok (output GRADS possibile)
! 1: file non trovato
! 2: file vuoto
! 3: i TEMP non si riferiscono alla stessa stazione
! 4: errore di lettura o parsing
! 5: i TEMP non sono in ordine cronologico
!--------------------------------------------------------------------------

USE datetime_class
IMPLICIT NONE

! Argomenti della subroutine
CHARACTER(LEN=*) :: filelst
TYPE(datetime), INTENT(OUT) :: data_ini,data_end
TYPE(timedelta), INTENT(OUT) :: step
INTEGER, INTENT(OUT) :: ntemp_in,ntemp_out,iret

! Variabili locali
TYPE(datetime) :: data_now,data_prev
TYPE(timedelta) :: step_dum
INTEGER :: ios,p1,k,yy,mm,dd,hh,idum,idstaz,istep
CHARACTER(LEN=100) :: rec
CHARACTER(LEN=13) :: ch13(2)

!--------------------------------------------------------------------------

OPEN (UNIT=50, FILE=filelst, STATUS="OLD", ACTION="READ", IOSTAT=ios)
IF (ios /= 0) THEN
  iret = 1
  RETURN
ENDIF

step = timedelta_max

DO k = 1,HUGE(0)
  READ (50,'(a)',IOSTAT=ios) rec
  IF (ios /= 0) EXIT

  p1 = INDEX(rec,"/",BACK=.TRUE.)
  READ(rec(p1+1:),'(i5,1x,i4,3i2)',IOSTAT=ios) idum,yy,mm,dd,hh
  IF (ios /= 0) THEN
    iret = 4
    RETURN
  ENDIF
  CALL init(data_now, YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)

  IF (k == 1) THEN
    idstaz = idum
    data_ini = data_now
  ELSE IF (idstaz /= idum) THEN
    iret = 3
    RETURN
  ELSE IF (data_now <= data_prev) THEN
    iret = 5
    RETURN
  ELSE
    step_dum = data_now - data_prev
    IF (step_dum < step) step = step_dum
  ENDIF

  data_prev = data_now
  CALL delete(data_now)
ENDDO

IF (k == 1) THEN
  iret = 2
  RETURN
ELSE IF (k == 2) THEN
  step = timedelta_new(HOUR=24)
ENDIF

data_end = data_prev
CALL getval(step, AHOUR=istep)
ntemp_in = k-1
ntemp_out = (data_end - data_ini) / step + 1

iret = 0
CLOSE(50)

CALL getval(data_ini,ISODATE=ch13(1))
CALL getval(data_end,ISODATE=ch13(2))
CALL getval(step, AHOUR=istep)

WRITE (*,'(a,i5,4a)') "TEMP richiesti: ",ntemp_in," da ",ch13(1)," a ",ch13(2)
WRITE (*,'(a,2(i5,a))') "Record in output: ",ntemp_out," step ",istep," ore"

RETURN
END SUBROUTINE proc_lst

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE tinterp_scalar(nt,f,rmis)
!
! Interpola linearmente i valori mancanti di un vettore
! Metodo: scorre il vettore di input; quando trova un dato valido ,scrive 
!         quel dato e tutti quelli che erano stati mancanti dopo il dato 
!         valido precedente.
!
IMPLICIT NONE
INTEGER, INTENT(IN) :: nt
REAL, INTENT(INOUT) :: f(nt)
REAL, INTENT(IN) :: rmis
!
INTEGER, PARAMETER :: imis = -999
REAL :: f2(nt),p1,p2
INTEGER :: k,k2,lastok
!--------------------------------------------------------------------------

f2(:) = rmis
lastok = imis

DO k =1,nt
  IF (f(k) == rmis) CYCLE
  f2(k) = f(k)

  IF (lastok /= imis) THEN
    DO k2 = lastok+1, k-1
      p1 = REAL(k-k2) / REAL(k-lastok)
      p2 = REAL(k2-lastok) / REAL(k-lastok)
      f2(k2) = p1*f(lastok) + p2*f(k)
    ENDDO
  ENDIF
  lastok = k

ENDDO

f(:) = f2(:)

RETURN
END SUBROUTINE tinterp_scalar

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION str_fill (str_in) RESULT(str_out)
!--------------------------------------------------------------------------
! Filtra alcuni caratteri (elencati in ch_not) da una stringa, 
! sostituendoli con "_". La stinga in output e' allineata a sinistra
! 
! Siccome il risultato e' un CHARACTER(*), richiede che nel programma 
! chiamante sia inserito il blocco INTERFACE (invece della dichiarazione
! del tipo della FUNCTION)
!
!INTERFACE
!  FUNCTION str_fill(str_in) RESULT(str_out)
!  CHARACTER(*) :: str_in
!  CHARACTER(LEN(str_in)) :: str_out,str_dum
!  END FUNCTION str_fill
!END INTERFACE
!--------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(*) :: str_in
CHARACTER(LEN(str_in)) :: str_out,str_dum

INTEGER :: k
CHARACTER(LEN=1) :: ch,chp1,chm1
CHARACTER(LEN=1), PARAMETER :: ch_not(2) = (/" ","."/)
!

str_dum = ADJUSTL(str_in)
str_out = ""

DO k = 1, LEN(TRIM(str_dum))
  IF ( ANY(str_dum(k:k) == ch_not) ) THEN
    str_out(k:k) = "_"
  ELSE
    str_out(k:k) = str_dum(k:k)
  ENDIF
ENDDO

RETURN
END FUNCTION str_fill

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION jul (data) RESULT(jul_day)
!
! Ritorna il giorno giuliano corriposndente a una data
!
USE datetime_class

TYPE(datetime) , INTENT(IN) :: data
INTEGER :: jul_day
INTEGER, PARAMETER :: dy_month(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
INTEGER :: yy,mm,dd,k
!
jul_day = 0
CALL getval(data, YEAR=yy, MONTH=mm, DAY=dd)
DO k = 1, mm-1
  jul_day = jul_day + dy_month(k)
  IF (k == 2 .AND. MOD(yy,4) == 0 .AND. &
    (MOD(yy,100) /= 0 .OR. MOD(yy/100,4) ==0)) jul_day = jul_day + 1
ENDDO
jul_day = jul_day + dd

RETURN
END FUNCTION jul

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE

WRITE (*,*) 
WRITE (*,*) "trasp_temp.exe [-h] [-c]"
WRITE (*,*) "parametri:"
WRITE (*,*) " -c      : crea un file trasp_temp.inp di esempio"
WRITE (*,*) " -h      : visualizza questo help"
WRITE (*,*) 
WRITE (*,*) "richiede in input:"
WRITE (*,*) "  - trasp_temp.inp: namelist opzioni"
WRITE (*,*) "  - trasp_temp.lst: elenco dei files da analizzare"
WRITE (*,*) "    (un file per riga, con orari sinottici)"
WRITE (*,*)

RETURN

END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_esempio
!
! Scrive un file trasp_temp.inp di esempio
!
IMPLICIT NONE

OPEN (UNIT=20, FILE="trasp_temp.inp", STATUS="REPLACE", FORM="FORMATTED")

!                            1234567890123456789012345678901234567890
WRITE (20,'(2a)')           "0             ! step temporale in output", &
                            "(0: lo stesso dei dati in input)"
WRITE (20,'(2a)')           "3             ! tipo livelli su cui inte", &
                            "rpolare (1: hPa; 2: m SLM; 3: m da sup.)"
WRITE (20,'(2a)')           "2             ! n.ro livelli su cui inte", &
                            "polare"
WRITE (20,'(2a)')           "500           ! lista livelli su cui int", &
                            "erpolare"
WRITE (20,'(a)')            "1000"
WRITE (20,'(2a)')           "1             ! interpolazione (1: linea", &
                            "re in z o ln(P); 2: liv. piu' vicino)"
WRITE (20,'(2a)')           "0             ! uso dati a livello super", &
                            "ficie (0: SI; 1: NO)"
WRITE (20,'(2a)')           "0             ! uso dati estrapolati sot", &
                            "toterra (0: SI; 1: NO)"
WRITE (20,'(2a)')           "1             ! interpolo nel tempo i da", &
                            "ti mancanti (0: SI; 1: NO)"
WRITE (20,'(2a)')           "1500.         ! altezza top per calcolo ", &
                            "velocita' media vento BPL (m, come tipo liv.)"
WRITE (20,'(2a)')           "1500.         ! altezza top per calcolo ", &
                            "inversione integrale (m, come tipo liv)"

RETURN
END SUBROUTINE scrive_esempio

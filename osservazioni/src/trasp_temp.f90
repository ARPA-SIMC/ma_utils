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
! Se la lista dei temp lo consente, scrive i dati anche in formato GRADS
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
!
! Se trova un file vuoto, il programma mette i valori a "dato mancante" e 
!   continua.
!
! Al momento, il programma gestisce solo TEMP estratti usando le opzioni 
!   di default di estra_temp: 
!   - T e Td in °C
!   - vento espresso come direzione e modulo
!   - formato xls
!   - dati interpolati su tutti i livelli
!
! L'output GRADS viene scritto solo se i temp richiesti (trasp_temp.lst):
!   - sono raggruppati per stazione 
!   - le date sono crescenti con lo stesso step per tutte le stazioni
!   - ci sono almeno due temp per ciascuna stazione
!
!                                                   V4.0, Enrico 17/04/2009 
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE 

REAL, PARAMETER :: rmis = -9999.       ! dato mancante in output
INTEGER, PARAMETER :: mxlev_in = 100   ! max n.ro di livelli in un temp
INTEGER, PARAMETER :: mxlev_out = 100  ! max n.ro di livelli in output
INTEGER, PARAMETER :: mxtemp = 5000    ! max n.ro di sondaggi
INTEGER, PARAMETER :: mxanag = 500     ! max stazioni TEMP in db_anagrafica
INTEGER, PARAMETER :: fw = 10          ! ampiezza campi file di output
REAL, PARAMETER :: t0k = -273.15       ! Temperatura zero assoluto (°C)
REAL, PARAMETER :: rr = 287.           ! R (J/K*kg)
REAL, PARAMETER :: cp = 1004.          ! Cp (J/K*Kg)
REAL, PARAMETER :: gg = 9.8            ! g (m/s^2)
REAL, PARAMETER :: ha1 = 990.          ! Costante solare (W/m2)
REAL, PARAMETER :: ha2 = -30           ! Assorbimento atmosferico (W/m2)

! file db_anagrafica.dat
CHARACTER (LEN=30) :: HOME_BONAFE
CHARACTER (LEN=40) :: anag_path 
CHARACTER (LEN=40), PARAMETER :: anag_name = "db_anagrafica.dat"

TYPE (date) :: data(mxtemp),dataf
REAL :: idlev_out(mxlev_out)
REAL :: p_in(mxlev_in),field_in(4,mxlev_in),z_in(mxlev_in)
REAL :: teta_in(mxlev_in),ro_in(mxlev_in),z_interf(mxlev_in+1)
REAL :: field_out(4,mxlev_out,mxtemp),t_invers(mxtemp),z_invers(mxtemp)
REAL :: val_ii(mxtemp),ff_ave(mxtemp),rad_csky(mxtemp)
REAL :: fsum(4,mxlev_out),fsum2(4,mxlev_out),invsum(5),invsum2(5)
REAL :: fmax(4,mxlev_out),fmin(4,mxlev_out),invmax(5),invmin(5)
REAL :: fave(4,mxlev_out),fstd(4,mxlev_out),invave(5),invstd(5)
REAL :: f1(4),f2(4),z1,z2,t_last
REAL :: teta_ref,tt_ref,p_ref,ro_ref,dlnp_ref,z_reft,z_refw
REAL :: dz,dtdz,dtpdz,incr,hstaz,ff_sum
REAL :: lon,lat,sinalp,rad_sum,hh
REAL :: anag_lon(mxanag),anag_lat(mxanag),anag_quo(mxanag)
INTEGER :: fok(4,mxlev_out),invok(5)
INTEGER :: tlev_out,nlev_in,nlev_out,nvars,ntemp,tint,zint,skip_liv1
INTEGER :: idstaz(mxtemp),ora(2,mxtemp),l1,l2,ll,idstazf,oraf
INTEGER :: ios,ios1,ios2,ios3,k,k2,kt,p1,lok,ks,nstep,njul
INTEGER :: cnt_temp_rq,cnt_temp_ok,cnt_int_fail
INTEGER :: staz,hr,hr1,ihstaz
INTEGER :: kret,step,irec,kpar,klev,ka
INTEGER :: net,usr,db,iquo,anag_id(mxanag),n_anag
CHARACTER(LEN=100) :: filein,fileout,filedat,fileasc,filestat,chrec
CHARACTER(LEN=100) :: chfmt1,chfmt2,chfmt3,chfmt4,chfmt5,chfmtgr
CHARACTER(LEN=24) :: nome_staz
CHARACTER(LEN=10) :: chpar
CHARACTER(LEN=10) :: idpar(4),idpar_first(4),idpar_fill(4)
CHARACTER(LEN=3) :: id_tlev
LOGICAL :: dati_ok,grads,ok

! Interfaccia per la funzione str_fill (e' di tipo: CHARACTER(*) )
INTERFACE
  FUNCTION str_fill(str_in) RESULT(str_out)
  CHARACTER(*) :: str_in
  CHARACTER(LEN(str_in)) :: str_out,str_dum
  END FUNCTION str_fill
END INTERFACE

!--------------------------------------------------------------------------
! 1: Preliminari

! 1.1 Eventuale parametro da riga comando
CALL getarg(1,chpar)
IF (TRIM(chpar) == '-h') THEN
  CALL scrive_help
  STOP
ELSE IF (TRIM(chpar) == '-c') THEN
  CALL scrive_esempio
  STOP
ENDIF

! 1.2 Lettura opzioni
OPEN (UNIT=21, FILE="trasp_temp.inp", STATUS="OLD", ACTION="READ", & 
  ERR=9999)
READ (21,*,ERR=9999) tlev_out
READ (21,*,ERR=9999) nlev_out
DO k = 1,nlev_out
  READ (21,*,ERR=9999) idlev_out(k)
ENDDO
READ (21,*,ERR=9999) zint
READ (21,*,ERR=9999) skip_liv1
READ (21,*,ERR=9999) tint
READ (21,*,ERR=9999) z_refw
READ (21,*,ERR=9999) z_reft
CLOSE(21)

IF ((tlev_out /= 1 .AND. tlev_out /= 2) .OR. &
    (zint /= 1 .AND. zint /= 2)) GOTO 9999

! 1.3 Controlli sulla lista dei TEMP richiesti
CALL verifica_lst(kret,step)
IF (kret == -4) THEN
  WRITE (*,*) "Errore leggendo o interpretando trasp_temp.lst:"
  WRITE (*,*) "  output ASCII imprevedibile, GRADS impossibile"
  grads = .FALSE.

ELSE IF (kret == -3) THEN
  WRITE (*,*) "I TEMP non si riferiscono alla stessa stazione:"
  WRITE (*,*) "  output ASCII imprevedibile, GRADS impossibile"
  grads = .FALSE.

ELSE IF (kret == -2) THEN
  WRITE (*,*) "Sono richiesti meno di due TEMP: output GRADS impossibile"
  grads = .FALSE.

ELSE IF (kret == -1) THEN
  WRITE (*,*) "Sequenza temporale dei TEMP illegale: output GRADS impossibile"
  grads = .FALSE.

ELSE
  WRITE (*,*) "Output GRADS possibile, step orario: ",step
  grads = .TRUE.

ENDIF  

IF (tint == 0) THEN
  IF (grads) THEN
    WRITE (*,'(a)') "Interplozione temporale possibile"
  ELSE
    WRITE (*,'(a)') "Interplozione temporale impossibile"
    tint = 1
  ENDIF
ENDIF

! 1.4 Inizializzazioni, costruzione formati, apertura files, 
idpar(1:4) = ""
cnt_temp_rq = 0
cnt_temp_ok = 0

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

IF (tlev_out == 1) THEN
  id_tlev = "hPa"
ELSE
  id_tlev = " m "
ENDIF  

WRITE (chfmt1,'(a,i3,a,i2,a)') "(a9,a3,a1,6x,", &
  nlev_out*4+5,"(1x,f",fw,".0))"
WRITE (chfmt2,'(a,i3,a,i2,a)') "(a13,1x,a5,",nlev_out*4+5,"(1x,a",fw,"))"
WRITE (chfmt3,'(a,i3,a,i2,a)') "(i4.4,3(1x,i2.2),1x,i5,", &
  nlev_out*4+5,"(1x,f",fw,".1))"
WRITE (chfmt4,'(a,i3,a,i2,a)') "(a13,6x,",nlev_out*4+5,"(1x,f",fw,".1))"
WRITE (chfmt5,'(a,i3,a,i2,a)') "(a13,6x,",nlev_out*4+5,"(1x,i",fw,"))"

WRITE (chfmtgr,'(a,i3,a)') "(a,i2,a,",nlev_out,"i7)"

OPEN (UNIT=30, FILE="trasp_temp.lst", STATUS="OLD", ACTION="READ", & 
  ERR=9998)
OPEN (UNIT=90, FILE="trasp_temp.log", STATUS="REPLACE", FORM="FORMATTED")

! 1.5 Leggo da db_anagrafica.dat le coordinate di tutte le stazioni di RSD
CALL GETENV('HOME_BONAFE',HOME_BONAFE)
anag_path = TRIM(HOME_BONAFE)//"/osservazioni/dat/"

OPEN (UNIT=23, FILE=TRIM(anag_path)//TRIM(anag_name), STATUS= "OLD", &
     ACTION="READ")
READ (23,*)
READ (23,*)
READ (23,*)

n_anag = 0
DO 
  READ (23,'(a)',IOSTAT=ios1) chrec
  IF (ios1 /= 0) EXIT
  READ (chrec,*,iostat=ios2) net,usr,db,lon,lat,iquo
  IF (ios2 /= 0) CYCLE
  IF (net /= 2) CYCLE
  IF (n_anag == mxanag) THEN
    WRITE (*,*) "Troppi temp in db_anagrafica, aumentare mxanag"
    EXIT
  ENDIF

  n_anag = n_anag + 1
  anag_id(n_anag) = usr
  anag_lon(n_anag) = lon
  anag_lat(n_anag) = lat
  anag_quo(n_anag) = REAL(iquo)
ENDDO

!==========================================================================
! 2) Ciclo sui radiosondaggi

field_out(:,:,:) = rmis
z_invers(:) = rmis
t_invers(:) = rmis
val_ii(:) = rmis
ff_ave(:) = rmis
rad_csky(:) = rmis

idstaz(:) = -9999
data(:) = date(0,0,0)
ora(:,:) = 0

cnt_temp_rq = 0

temp: DO kt = 1,mxtemp

!--------------------------------------------------------------------------
! 2.1) Leggo il TEMP. 
!      Se non ci sono i due record di intestazione e almeno un record di 
!      dati, oppure se c'e' un errore nella lettura dati, mettero' tutto a 
!      "manca dato". Salto sempre i livelli con z mancante, e se richiesto 
!      anche quello alla superifice.

! Apro file e leggo header
  READ (30,'(a)',IOSTAT=ios) filein
  IF (ios /= 0) EXIT temp
  IF (TRIM(filein) == "") CYCLE temp
  cnt_temp_rq = cnt_temp_rq + 1
  
  OPEN (UNIT=31, FILE=filein, STATUS="OLD", ACTION="READ",IOSTAT=ios)
  READ (31,'(1x,i5,1x,a24,1x,i4.4,2(i2),1x,2(i2),1x,i3,1x,i4)', &
    IOSTAT=ios1) idstaz(kt),nome_staz,data(kt)%yy,data(kt)%mm,data(kt)%dd, &
    ora(:,kt),nlev_in,ihstaz
  READ (31,'(11x,4(1x,a10))',IOSTAT=ios2) idpar(1:4)
  hstaz = REAL(ihstaz)

  p1 = INDEX(filein,"/",BACK=.TRUE.) + 1
  READ (filein(p1:),'(i5,1x,i4,3i2)',IOSTAT=ios3) &
    idstazf,dataf%yy,dataf%mm,dataf%dd,oraf
  
  IF (ios /= 0 .OR. ios1 /= 0 .OR. ios2 /= 0 .OR. nlev_in < 1) THEN
    dati_ok = .FALSE.

    WRITE (*,'(3a)') "* ",TRIM(filein), &
      ": nessun dato, metto mancanti (prendo datah da nome file)"
    idstaz(kt) = idstazf
    data(kt) = dataf
    ora(:,kt) = (/oraf,0/)

  ELSE

!   Controllo consistenza tra header e nome file
    IF (ios3 /= 0) THEN
      WRITE (*,'(3a)') "WARNING ",TRIM(filein), &
        ": parsing nome file fallito, risultati incerti"
    ELSE IF (idstaz(kt)/=idstazf .OR. data(kt)/=dataf .OR. &
      ora(1,kt)/=oraf) THEN
      WRITE (*,'(3a)') "WARNING ",TRIM(filein), &
        ": nome file inconsistente con header, risultati incerti"
    ENDIF
  
!   Leggo i dati e salvo i valori nelgi array relativi:
!   p_in, z_in:     Pressione (mb), quota (m SLM)
!   field_in(1:4):  T(C), Td(C), DD(gradi), FF (m/s)

    dati_ok = .TRUE.
    lok = 1
    DO k = 1,nlev_in
      READ(31,*,IOSTAT=ios) p_in(lok),field_in(1:4,lok),z_in(lok)
  
!     errore in lettura, metto tutto mancante
      IF (ios /= 0) THEN
        WRITE (*,*) "Errore leggendo ",TRIM(filein)," metto mancante"
        dati_ok = .FALSE.
        EXIT
  
!     skippo il livello
      ELSE IF ( &
        (p_in(lok)==rmis .OR. z_in(lok)==rmis .OR. z_in(lok)<hstaz) .OR. &
        (skip_liv1 == 1 .AND. z_in(lok)-hstaz < 1.)) THEN
        CYCLE
  
!     livello buono
      ELSE
        IF (z_in(lok) < hstaz) THEN
          z_in(lok) = hstaz
          WRITE (*,'(2a,2(f5.0,a))') "Quota misura < quota stazione,", &
            " modifico quota misura (da ",z_in(lok)," a ",hstaz,")"
        ENDIF 
        lok = lok + 1
  
      ENDIF  
    ENDDO
  
    WRITE (*,'(3a,i2,a,i2)') ". ",TRIM(filein)," liv.file: ",nlev_in, &
      " liv.mantenuti: ",lok-1
    WRITE (90,*)
    WRITE (90,'(3a,i2,a,i2)') ". ",TRIM(filein)," liv.file: ",nlev_in, &
      " liv.mantenuti: ",lok-1
    nlev_in = lok - 1
  
  ENDIF
  CLOSE(31)
  
  IF (.NOT. dati_ok) CYCLE temp

!--------------------------------------------------------------------------
! 2.2) Controllo allineamento files

  cnt_temp_ok = cnt_temp_ok + 1
  IF (cnt_temp_ok == 1) THEN
    idpar_first(:) = idpar
  ELSE IF (ANY(idpar(:) /= idpar_first(:))) THEN
    WRITE (*,*) "Errore, i temp contengono parametri diversi: "
    WRITE (*,*) "rilanciare l'estrazione temp usando le stesse opzioni"
    STOP
  ENDIF

!==========================================================================
! 2.3) Elaborazioni sul TEMP appena letto

  WRITE(90,*) "Elaboro rsd: ",TRIM(filein)

!--------------------------------------------------------------------------
! 2.3.1 Interplo sui livelli richiesti

  DO k = 1, nlev_out

! Trovo i livelli sotto e sopra a quello richiesto
    l2 = 1
    IF (tlev_out == 1) THEN                          ! hPa
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
      IF (zint == 1 .AND. tlev_out == 1 .AND. &
          p_in(l1) > 0. .AND. p_in(l2) > 0.) THEN 

        f1 = field_in(:,l1)
        f2 = field_in(:,l2)
        z1 = LOG(p_in(l1))
        z2 = LOG(p_in(l2))

        WHERE (f1 /= rmis .AND. f2 /= rmis)
          field_out(:,k,kt) = f1 + (f2-f1)/(z2-z1) * (LOG(idlev_out(k))-z1)
        ELSEWHERE
          field_out(:,k,kt) = rmis
        ENDWHERE

!     interpolazione in z
      ELSE IF (zint == 1 .AND. tlev_out == 2) THEN

        f1 = field_in(:,l1)
        f2 = field_in(:,l2)
        z1 = z_in(l1)
        z2 = z_in(l2)
        WHERE (f1 /= rmis .AND. f2 /= rmis)
          field_out(:,k,kt) = f1 + (f2-f1)/(z2-z1) * (idlev_out(k)-z1)
        ELSEWHERE
          field_out(:,k,kt) = rmis
        ENDWHERE

!     livello piu' vicino in ln(P)
      ELSE IF (zint == 2 .AND. tlev_out == 1 .AND. &
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
          field_out(:,k,kt) = field_in(:,ll)
        ELSEWHERE
          field_out(:,k,kt) = rmis
        ENDWHERE

!     livello piu' vicino in z
      ELSE IF (zint == 2 .AND. tlev_out == 2) THEN

        z1 = z_in(l1)
        z2 = z_in(l2)
        IF (idlev_out(k)-z1 < z2-idlev_out(k)) THEN
          ll = l1
        ELSE
          ll = l2
        ENDIF

        WHERE (field_in(:,ll) /= rmis)
          field_out(:,k,kt) = field_in(:,ll)
        ELSEWHERE
          field_out(:,k,kt) = rmis
        ENDWHERE
      ENDIF

!   interpolazione impossibile
    ELSE
      field_out(:,k,kt) = rmis

    ENDIF
  ENDDO

!--------------------------------------------------------------------------
! 2.3.2 Calcolo l'inversione al suolo

  IF (nlev_in >= 2 .AND. field_in(1,2) > field_in(1,1) .AND. &
      field_in(1,1) /= rmis .AND. field_in(1,2) /= rmis ) THEN

    t_last = field_in(1,2)
    t_invers(kt) = field_in(1,2) - field_in(1,1)
    IF (z_in(2) /= rmis) THEN 
      z_invers(kt) = z_in(2) - hstaz
    ELSE
      z_invers(kt) = rmis
    ENDIF

    DO k2 = 3, nlev_in
      IF (field_in(1,k2) < t_last) EXIT

      t_last = field_in(1,k2)
      t_invers(kt) = field_in(1,k2) - field_in(1,1)
      IF (z_in(k2) /= rmis) THEN 
        z_invers(kt) = z_in(k2) - hstaz
      ELSE
        z_invers(kt) = rmis
      ENDIF
    ENDDO

  ELSE IF (nlev_in >= 2 .AND. field_in(1,1) /= rmis) THEN
    t_invers(kt) = 0.
    z_invers(kt) = 0.
  ELSE
    t_invers(kt) = rmis
    z_invers(kt) = rmis
  ENDIF

!--------------------------------------------------------------------------
! 2.3.3 Calcolo la velocita' media del vento nel PBL
! Considero FF costante nello strato centrato su ciascun livello di misura 
! (i.e. interpolazione verticale usa il livello piu' vicino); calcolo la
! media di FF pesata per lo spessore di ciascuno strato.

  z_interf(1) = hstaz
  DO klev = 2, nlev_in
    z_interf(klev) = 0.5 * (z_in(klev) + z_in(klev-1))
  ENDDO

  ff_sum = 0.
  ok = .FALSE.
  DO klev = 1,nlev_in-1
    IF (field_in(4,klev) == rmis) THEN
      EXIT
    ELSE IF (z_interf(klev+1) < z_refw) THEN
      ff_sum = ff_sum + field_in(4,klev) * (z_interf(klev+1) - z_interf(klev))
    ELSE
      ff_sum = ff_sum + field_in(4,klev) * (z_refw - z_interf(klev))
      ok = .TRUE.
      EXIT
    ENDIF
  ENDDO

  IF (ok) THEN
    ff_ave(kt) = ff_sum / (z_refw - hstaz)
  ELSE
    ff_ave(kt) = rmis
  ENDIF

!--------------------------------------------------------------------------
! 2.3.4 Calcolo l'inversione integrale

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
  DO WHILE ( z_in(l2) <= z_reft)
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
    val_ii(kt) = (z_in(1) - hstaz) * (teta_ref - teta_in(1)) * cp * ro_in(1)
   
    WRITE (90,'(a,i2,f5.0,f6.1,f6.0,f6.3,e10.3)') &
      "layer,TPlow,Dteta,Dz,Ro_ave,incr: ",&
      0,teta_in(1),teta_ref-teta_in(1),z_in(1)-hstaz,ro_in(1),val_ii(kt) 
   
!   b) aggiungo il contributo dei layers interni
    DO klev = 1, l1-1
      dz = z_in(klev+1) - z_in(klev)
      incr = dz * (teta_ref - 0.5*(teta_in(klev+1)+teta_in(klev))) * &
        cp * 0.5*(ro_in(klev+1)+ro_in(klev))
      val_ii(kt) = val_ii(kt) + incr
   
      WRITE (90,'(a,i2,f5.0,f6.1,f6.0,f6.3,e10.3)') &
        "layer,TPlow,Dteta,Dz,Ro_ave,incr: ",&
        klev,teta_in(klev),teta_ref-0.5*(teta_in(klev+1)+teta_in(klev)), &
         dz,0.5*(ro_in(klev+1)+ro_in(klev)),incr
    ENDDO
   
!   c) aggiungo il contributo del layer che comprende z_ref
    incr = 0.5 * (z_reft-z_in(l1)) * (teta_ref - teta_in(l1)) * &
      cp * 0.5*(ro_in(l1)+ro_ref)
    val_ii(kt) = val_ii(kt) + incr
   
    WRITE (90,'(a,i2,f5.0,f6.1,f6.0,f6.3,e10.3)') &
      "layer,TPlow,Dteta,Dz,Ro_ave,incr: ",&
      l1, teta_in(l1), teta_ref-teta_in(l1), z_reft-z_in(l1), &
      0.5*(ro_in(l1)+ro_ref), incr

!   d) converto da J/m2 a Mj/m2
    val_ii(kt) = val_ii(kt) * 1.e-6
   

  ELSE
    val_ii(kt) = rmis

  ENDIF

!--------------------------------------------------------------------------
! 2.3.5 Calcolo la radiazione solare incidente a cielo sereno

! Trovo le coordinate della stazione
  ok = .FALSE.
  DO ka = 1,n_anag
    IF (anag_id(ka) == idstaz(kt)) THEN
      lat = anag_lat(ka)
      lon = anag_lon(ka)
      ok = .TRUE.
      EXIT
    ENDIF
  ENDDO

! Calcolo radiaz. solare incidente integrata nelle giornata (Mj/m2)
  IF (ok) THEN
    njul = jul(data(kt))
    nstep = 24*6                          ! calcolo ogni 10'
    rad_sum = 0.
    DO ks = 1,nstep
      hh = REAL(ks) * 24. / REAL(nstep)
      CALL solar(njul,hh,lat,lon,sinalp)                     
      rad_sum = rad_sum + (ha1 * sinalp + ha2)
    ENDDO
    rad_csky(kt) = rad_sum * (3600.*24)/REAL(nstep) * 1.e-6

  ELSE
    WRITE (*,*) "Stazione ",idstaz(kt)," non trovata in db_anagrafica.dat"
    rad_csky(kt) = rmis

  ENDIF

!--------------------------------------------------------------------------
! 2.3.6 Log calcoli falliti

  IF (ANY(field_out(:,1:nlev_out,kt) == rmis) .OR. &
    t_invers(kt) == rmis .OR. z_invers(kt) == rmis .OR. & 
    ff_ave(kt) == rmis .OR. val_ii(kt) == rmis .OR. rad_csky(kt) == rmis) &
    cnt_int_fail = cnt_int_fail + 1

ENDDO temp

WRITE (*,'(a,i4,a)') "Lettura compltata, elaborati ",cnt_temp_ok," sondaggi"
ntemp = cnt_temp_rq

!--------------------------------------------------------------------------
! 3) Interploazione temporale dei dati mancanti (se richiesta)

IF (tint == 0) THEN
  WRITE (*,'(a,i4,a)') "Eseguo interpolazione nel tempo di ", &
    cnt_temp_rq - cnt_temp_ok," sondaggi"

  DO kpar = 1,4
  DO klev = 1,nlev_out
    CALL tinterp_scalar(ntemp,field_out(kpar,klev,1:ntemp),rmis)
  ENDDO
  ENDDO

  CALL tinterp_scalar(ntemp,t_invers(1:ntemp),rmis)
  CALL tinterp_scalar(ntemp,z_invers(1:ntemp),rmis)
  CALL tinterp_scalar(ntemp,ff_ave(1:ntemp),rmis)
  CALL tinterp_scalar(ntemp,val_ii(1:ntemp),rmis)
  CALL tinterp_scalar(ntemp,rad_csky(1:ntemp),rmis)
ENDIF

!--------------------------------------------------------------------------
! 4) Output ASCII & statistico

WRITE (fileasc,'(a,i5.5,a)') "temp_",idstaz(1),".txt"
OPEN (UNIT=40, FILE=fileasc, STATUS="REPLACE", ACTION="WRITE")

! 4.1 Scrivo i record d'intestazione (otuput ASCII)
WRITE (40,chfmt1) "Livelli (",id_tlev,")",&
  ((idlev_out(k),k2=1,4),k=1,nlev_out),0.,0.,0.,0.,0.
WRITE (40,*)
WRITE (40,chfmt2) "aaaa mm gg hh","staz.", &
  ((idpar(k2),k2=1,4),k=1,nlev_out),&
  "T invers.","Z invers.","inv.inte.","rad.csky.","   ff_ave"

!4.2 Scrivo i record dei dati (output ASCII)
DO kt = 1,ntemp
  WRITE (40,chfmt3) data(kt)%yy,data(kt)%mm,data(kt)%dd,ora(1,kt), &
    idstaz(kt),(field_out(1:4,k,kt),k=1,nlev_out), &
    t_invers(kt),z_invers(kt),val_ii(kt),rad_csky(kt),ff_ave(kt)
ENDDO

CLOSE (40)

!--------------------------------------------------------------------------
! 5) Output statistico

! 5.1 Calcolo le statistiche
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

! 5.2 Le scrivo
WRITE (filestat,'(a,i5.5,a)') "temp_",idstaz(1),"_stat.txt"
OPEN (UNIT=41, FILE=filestat, STATUS="REPLACE", ACTION="WRITE")

WRITE (41,chfmt1) "Livelli (",id_tlev,")",&
  ((idlev_out(k),k2=1,4),k=1,nlev_out),0.,0.,0.,0.,0.
WRITE (41,*)
WRITE (41,chfmt2) "-------------","     ",((idpar(k2),k2=1,4),k=1,nlev_out), &
  "T invers.","Z invers.","inv.inte.","rad.csky.","   ff_ave"
WRITE (41,chfmt4) "Massimo      ",(fmax(:,k),k=1,nlev_out),invmax(:) 
WRITE (41,chfmt4) "Minimo       ",(fmin(:,k),k=1,nlev_out),invmin(:) 
WRITE (41,chfmt4) "Media        ",(fave(:,k),k=1,nlev_out),invave(:) 
WRITE (41,chfmt4) "Dev.standard ",(fstd(:,k),k=1,nlev_out),invstd(:) 
WRITE (41,chfmt5) "Dati buoni   ",(fok(:,k),k=1,nlev_out),invok(:) 
WRITE (41,chfmt5) "Dati rich.   ",(cnt_temp_rq,k=1,nlev_out*4+5)

CLOSE (41)

!--------------------------------------------------------------------------
! 6) Output GRADS

IF (grads) THEN

! 6.1) Scrivo il file .dat

  WRITE (fileout,'(a,i5.5,a)') "temp_",idstaz(1),".dat"
  filedat = fileout
  OPEN (32, FILE=fileout, FORM='UNFORMATTED', STATUS="REPLACE", &
    ACCESS='DIRECT', RECL=4)

  irec = 1
  DO kt = 1,ntemp
    DO kpar = 1,4
    DO klev = 1,nlev_out
      WRITE (32, REC=irec) field_out(kpar,klev,kt)
      irec = irec + 1
    ENDDO
    ENDDO
  
    WRITE (32, REC=irec) t_invers(kt)
    irec = irec + 1
    WRITE (32, REC=irec) z_invers(kt)
    irec = irec + 1
    WRITE (32, REC=irec) val_ii(kt)
    irec = irec + 1
    WRITE (32, REC=irec) rad_csky(kt)
    irec = irec + 1
    WRITE (32, REC=irec) ff_ave(kt)
    irec = irec + 1

  ENDDO
  CLOSE (32)

! 6.2 Scrivo il file .ctl

  WRITE (fileout,'(a,i5.5,a)') "temp_",idstaz(1),".ctl"
  OPEN (UNIT=33, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
  
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

  WRITE (33,'(a,i5,a,i2.2,a,a9,a,i3.3,a)') "TDEF   ", &
    ntemp," linear ",hr1,"z",grads_date(data(1))," ",step,"hr"

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

ENDIF

!--------------------------------------------------------------------------
! 7) Conclusione

WRITE (*,'(2(a,i4))') "Elaborazione completata: scritti ",ntemp," TEMP"
IF (cnt_int_fail > 0) WRITE (*,'(a,i4,a)') &
  "Interpolazioni/calcoli impossibili in ",cnt_int_fail," sondaggi"

STOP

!--------------------------------------------------------------------------
! 8) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore leggendo trasp_temp.inp"
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo trasp_temp.lst"
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",filein
STOP

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

SUBROUTINE verifica_lst(kret,step)
!--------------------------------------------------------------------------
! Verifica se il file trasp_temp.lst contiene files relativi a una sola 
! stazione, verifica se le date sono in ordine crescente e a intervalli 
! regolari
!
! Codici per kret:
! -4: errore leggendo o interpretando trasp_temp.lst (output imprevedibile)
! -3: i TEMP non si riferiscono alla stessa stazione (output imprevedibile)
! -2: sono richiesti meno di due TEMP (no output GRADS)
! -1: i TEMP non sono in ordine temporale con step costante (no output GRADS)
! 0 : tutto ok (output GRADS possibile)
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

INTEGER, INTENT(OUT) :: kret,step

TYPE(date) :: data1,data2,data,cdata,rqdata
INTEGER :: ios,p1
INTEGER :: hr1,hr2,hr,chr,rqhr,tothr,add_day
INTEGER :: staz1,cstaz
CHARACTER(LEN=100) :: rec
LOGICAL :: reg_step
!--------------------------------------------------------------------------

reg_step = .TRUE.
OPEN (UNIT=50, FILE="trasp_temp.lst", STATUS="OLD", ACTION="READ", & 
  IOSTAT=ios)
IF (ios /= 0) THEN
  kret = -4
  GOTO 9999
ENDIF

!--------------------------------------------------------------------------
! Leggo i primi 2 record, e da questi calcolo lo step del file 

READ (50,'(a)',IOSTAT=ios) rec
IF (ios /= 0) THEN
  kret = -2
  GOTO 9999
ENDIF
p1 = INDEX(rec,"/",BACK=.TRUE.)
READ(rec(p1+1:),'(i5,1x,i4,3i2)',IOSTAT=ios) staz1,data1%yy,data1%mm, &
  data1%dd,hr1
IF (ios /= 0) THEN
  kret = -4
  GOTO 9999
ENDIF

READ (50,'(a)',IOSTAT=ios) rec
IF (ios /= 0) THEN
  kret = -2
  GOTO 9999
ENDIF
p1 = INDEX(rec,"/",BACK=.TRUE.)
READ(rec(p1+1:),'(i5,1x,i4,3i2)',IOSTAT=ios) cstaz,data2%yy,data2%mm, &
  data2%dd,hr2
IF (ios /= 0)  THEN
  kret = -4
  GOTO 9999
ENDIF

IF (cstaz /= staz1) THEN
  kret = -3
  GOTO 9999
ENDIF

step = (data2 - data1) * 24 + (hr2 - hr1)
IF (step <= 0) reg_step = .FALSE.

!--------------------------------------------------------------------------
! Leggo gli altri record e verifico se il file e' regolare

cdata = data2
chr = hr2

DO
  READ (50,'(a)',IOSTAT=ios) rec
  IF (ios /= 0) EXIT
  IF (TRIM(rec) == "") CYCLE

  p1 = INDEX(rec,"/",BACK=.TRUE.)
  READ(rec(p1+1:),'(i5,1x,i4,3i2)',IOSTAT=ios) cstaz,data%yy,data%mm, &
    data%dd,hr
  IF (ios /= 0) THEN
    kret = -4
    GOTO 9999
  ELSE IF (cstaz /= staz1) THEN 
    kret = -3
    GOTO 9999
  ENDIF

  IF (reg_step) THEN
    tothr = chr + step
    add_day = tothr/24
    rqhr = MOD(tothr,24)
    rqdata = cdata + add_day
  
    IF (rqdata /= data .OR. rqhr /= hr) THEN
      reg_step = .FALSE.
    ELSE
      cdata = data
      chr = hr
    ENDIF
  ENDIF

ENDDO

IF (reg_step) THEN
  kret = 0
ELSE
  kret = -1
ENDIF

9999 CONTINUE
CLOSE(50)

RETURN
END SUBROUTINE verifica_lst

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE tinterp_scalar(nt,f,rmis)
!
! Interpola linearmetne i valori mancanti di un vettore
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
WRITE (*,*) "    (un file per riga)"
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
WRITE (20,'(2a)')           "2             ! tipo livelli su cui inte", &
                            "rpolare (1: hPa; 2: metri SLM)"
WRITE (20,'(2a)')           "2             ! n.ro livelli su cui inte", &
                            "polare"
WRITE (20,'(2a)')           "500           ! lista livelli su cui int", &
                            "erpolare"
WRITE (20,'(a)')            "1000"
WRITE (20,'(2a)')           "1             ! interpolazione (1: linea", &
                            "re in z o ln(P); 2: liv. piu' vicino)"
WRITE (20,'(2a)')           "1             ! uso dati a livello super", &
                            "ficie (0: SI; 1: NO) "
WRITE (20,'(2a)')           "1             ! interpolo nel tempo i da", &
                            "ti mancanti (0: SI; 1: NO)"
WRITE (20,'(2a)')           "1500.         ! altezza top per calcolo ", &
                            "velocita' media del vento BPL (m SLM)"
WRITE (20,'(2a)')           "1500.         ! altezza top per calcolo ", &
                            "inversione integrale (m SLM)"

RETURN
END SUBROUTINE scrive_esempio

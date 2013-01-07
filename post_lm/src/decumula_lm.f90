PROGRAM decumula_lm
!--------------------------------------------------------------------------
! De-cumula i parametri LM contenuti in un file in formato .ASC 
!   (otuput di seriet) oppure .grb
! Possono essere prodotti valori cumulati/mediati nell'ora precedente 
!   oppure valori istantanei (questi ultimi sono calcolati facendo la media
!   tra la media nell'ora precedente e quella nell'ora successiva)
! Unifica i programmi: decumula_lami.f90 e decumula_asc.f90
! 
! Metodo:
! Il programma esamina, per ciascun parametro, il dato corrente (c), 
!   quello immediatamente precedente (p) e quello successivo (n). Se da 
!   questi e' possibile calcolare il valore richiesto lo scrive, altrimenti
!   lo mette mancante.
!
! Note: input GRIB 
! Bug: se i campi in input sono su griglie diverse (es. UVH), i campi 
!   decumulati relativi all'ultima scadenza sono scritti tutti con la 
!   stessa griglia (quella dell'ultimo campo del file di input); per 
!   risolvere il problema bisognerebbe tenere in memoria per ciascun 
!   parametro la sez. 2 dell'ultimo campo letto.
!
! Il file puo' contenere diversi parametri; i campi non cumulati restano
!   invariati, quelli cumulati/mediati sono de-cumulati, a condizione che 
!   le scadenze abbiano passo orario e siano in ordine progressivo. Se
!   la de-cumulazione non e' possibile, viene scritto un GRIB interamente 
!   mancante.
! Se il file contiene piu' di un parametro, l'ordine dei grib in output 
!   viene cambiato (i grib cum/med sono spostati in avanti di una scadenza;
!   l'ultimo istante di tutti i parametri cum/med e' a fine file)
! Sono gestite sia le analisi LAMA/LAMAZ sia le previsioni LM 
!
! Note: input ASC
! Se il parametro in esame non e' cumulato/mediato, il programma termina
!  senza scrivere nulla.
! La de-cumulazione delle previsioni LM e' implementata ma non testata
!
!                                         Versione 3.8.0, Enrico 07/09/2012
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis_ser(2) = (/-1.E30,-1.5E15/)  ! dati mancanti input
REAL, PARAMETER :: rmis = -1.E30        ! dati mancanti output e grib
INTEGER, PARAMETER :: len_lab = 100     ! lun. max. label descrittive punti
INTEGER, PARAMETER :: maxdim = 100000   ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxpar = 30       ! n.ro max di parametri nel grib

CHARACTER (LEN=80),PARAMETER :: chfmt = &      ! formato per scrittura .asc
  '(i4.4,1x,i4.4,4(1x,i2.2),1x,i3.3,1x,i5.5,2(1x,i3.3),1x,i9,2(1x,a),a)'

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1f(1024),ksec2f(1024)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: fieldp(maxdim,maxpar),fieldc(maxdim,maxpar),fieldn(maxdim,maxpar)
REAL    :: field(maxdim),fieldw(maxdim)

! Altre variabili del programma
TYPE(date) :: datap(maxpar),datac(maxpar),datan(maxpar),dataw,datas
REAL :: valuep,valuec,valuen,valuew,values,fdp,fdn
INTEGER :: var_liv(6,maxpar),var_liv_dum(6)
INTEGER :: nbitp(maxpar),nbitc(maxpar),nbitn(maxpar)
INTEGER :: hhp(2,maxpar),hhc(2,maxpar),hhn(2,maxpar),hhw(2),hhs(2)
INTEGER :: scadp(4,maxpar),scadc(4,maxpar),scadn(4,maxpar),scadw(4),scads(4)
INTEGER :: dec,select_oper,npar,iop_fmt,oper(maxpar),oper_dum,srq
INTEGER :: prog_ptc,prog_ptn,id_ptc,id_ptn,idum,id_parc
INTEGER :: tot_len,path_len,ds_len,p1,p2,kdelim
INTEGER :: cnt_eq(maxpar),cnt_mis(maxpar),cnt_wg,cnt_wa
INTEGER :: cnt_mis_org(maxpar),cnt_dec(maxpar)
INTEGER :: kk,kp,kpt,karg,idp,ios,eof,eor
INTEGER :: iuin,iuout,npts,ptdeb
CHARACTER (LEN=len_lab+63) :: ch_record
CHARACTER (LEN=len_lab) :: labelc,labeln
CHARACTER (LEN=500) :: filein,fileout,chdum,nome_file,arg(2)
CHARACTER (LEN=20) :: ch_value
CHARACTER (LEN=10) :: str_oper(maxpar)
LOGICAL :: deb,end_asc,first_asc,newp,lstag

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1) Parametri da riga comando

deb = .FALSE.
iop_fmt = 0
srq = 0
idp = 0
filein = ""
fileout = ""

DO karg = 1,HUGE(karg)
  CALL getarg(karg,chdum)
  IF (TRIM(chdum) == "") THEN  
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-v") THEN
    deb = .TRUE.
  ELSE IF (TRIM(chdum) == "-asc") THEN  
    iop_fmt = 1
  ELSE IF (TRIM(chdum) == "-grb") THEN  
    iop_fmt = 2
  ELSE IF (TRIM(chdum) == "-ist") THEN  
    srq = 3
  ELSE IF (TRIM(chdum) == "-prh") THEN  
    srq = 2
  ELSE
    idp = idp + 1
    IF (idp == 1) THEN
      filein = chdum
    ELSE IF (idp == 2) THEN
      fileout = chdum
    ELSE
      CALL write_help
      STOP
    ENDIF
  ENDIF
ENDDO

IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  iop_fmt == 0 .OR. srq == 0) THEN
  CALL write_help
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.2) Memorizzo i dati che devono rimanere costanti all'interno del file

IF (iop_fmt == 1) THEN
! Formato .asc: li ricavo dal nome del file 
! (Il nome del dataset e' delimitato dal sestultimo "_", in modo da gestire 
! nomi DS di qualsiasi lunghezza)

  CALL get_eof_eor(eof,eor)
  tot_len = LEN(TRIM(filein))
  path_len = INDEX(filein,"/",BACK=.TRUE.)
  nome_file = filein(path_len+1:tot_len)

  p2 = LEN(TRIM(nome_file)) + 1
  DO kdelim = 1,6
    p2 = INDEX(nome_file(1:p2-1),"_",BACK=.TRUE.)
  ENDDO
  ds_len = p2 - 1
  IF (ds_len /= 10) WRITE (*,'(3a)') &
    "Warning, nome DS non ha 10 caratteri: $",nome_file(1:ds_len),"$"

  READ( nome_file(ds_len + 1:), '(4(1x,i3),1x,i5,1x,i3)', IOSTAT=ios ) &
    var_liv(1:6,1)
  IF (ios /= 0) GOTO 9998

ELSE
! Formato .grb: li ricavo dal primo grib contenuto nel file
  CALL grsvck(0)
  CALL PBOPEN(iuin,filein,'R',kret)
  IF (kret /= 0) GOTO 9999
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret /= 0) GOTO 9992

  CALL GRIBEX(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
              field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.200) GOTO 9991
  npts = ksec2(2)*ksec2(3)
  ksec1f(:) = ksec1(:)
  ksec2f(:) = ksec2(:)
  IF (npts > maxdim) GOTO 9995

  CALL PBCLOSE(iuin,kret)

ENDIF

!--------------------------------------------------------------------------
! 1.3) Apro i files
IF (iop_fmt == 1) THEN
  OPEN (UNIT=20, FILE=filein, STATUS="OLD",  FORM="FORMATTED", ERR=9999)
  OPEN (UNIT=30, FILE=fileout, STATUS="REPLACE",  FORM="FORMATTED")

ELSE IF (iop_fmt == 2) THEN
  CALL PBOPEN(iuin,filein,'R',kret)
  CALL PBOPEN(iuout,fileout,'W',kret)

ENDIF

IF (deb) OPEN (UNIT=90,FILE="decumula_lm.log",STATUS="REPLACE", &
  FORM="FORMATTED")

IF (iop_fmt == 1) THEN
!==========================================================================
! 2) Decumulo: formato .asc (ciclo sugli istanti)

!--------------------------------------------------------------------------
! 2.0) Preliminari e inizializzazioni

! Verifico se il parametro e' mediato/cumulato.
oper(1) = select_oper(var_liv(1:3,1))

IF (oper(1) == 0) THEN
  STOP
ELSE IF (oper(1) == 1) THEN
  WRITE (*,'(a,3i4,a)') "Decumula_lm, ASC: elaboro il parametro ", &
    var_liv(1:3,1)," tipo: cumulato"
ELSE IF (oper(1) == 2) THEN
  WRITE (*,'(a,3i4,a)') "Decumula_lm, ASC: elaboro il parametro ", &
    var_liv(1:3,1)," tipo: mediato"
ENDIF

! inizializzo i conatatori statistici
cnt_eq(1) = 0 
cnt_mis(1) = 0 
cnt_mis_org(1) = 0 
cnt_dec(1) = 0
cnt_wa = 0

! Inizializzo dati relativi a istante corrente e precedente
datac(1) = date(-999,-999,-999)
hhc(1:2,1) = (/-999,-999/)
scadc(1:4,1) = (/-999,-999,-999,-999/)
valuec = rmis

datan(1) = date(-999,-999,-999)
hhn(1:2,1) = (/-999,-999/)
scadn(1:4,1) = (/-999,-999,-999,-999/)
valuen = rmis

! inizializzazioni specifiche formato .ASC
prog_ptc = -999
prog_ptn = -999
id_ptc = -999
id_ptn = -999
labelc = ""
labeln = ""
end_asc = .FALSE.
first_asc = .TRUE.

DO kpt = 1,HUGE(kpt)

  DO kk = 1,HUGE(kk)

!--------------------------------------------------------------------------
! 2.1) Lettura
    READ (20,'(a)',IOSTAT=ios) ch_record
    IF (ios == eof) THEN
      end_asc = .TRUE.
      EXIT 
    ELSE IF (ios /= 0) THEN
      GOTO 9997
    ENDIF

!--------------------------------------------------------------------------
! 2.2) Aggiorno i dati relativi all'ora precedente, corrente successiva 
!      (i.e. quella appena letta), gestendo il cambio di punto.
!      Se ho letto il primo valore relativo a questo punto, devo aspettare 
!      il prossimo giro per elaborarlo

    prog_ptc = prog_ptn
    id_ptc = id_ptn
    labelc = labeln

    READ (ch_record,*,IOSTAT=ios) prog_ptn
    IF (ios /= 0) GOTO 9997

    IF (prog_ptn == prog_ptc) THEN            ! Il punto non e' cambiato
      newp = .FALSE.
    ELSE                                      ! Il punto e' cambiato
      newp = .TRUE.
    ENDIF

    datap(1) = datac(1)
    hhp(1:2,1) = hhc(1:2,1)
    scadp(1:4,1) = scadc(1:4,1)
    valuep = valuec

    datac(1) = datan(1)
    hhc(1:2,1) = hhn(1:2,1)
    scadc(1:4,1) = scadn(1:4,1)
    valuec = valuen

    READ (ch_record,*,IOSTAT=ios) prog_ptn,datan(1)%yy,datan(1)%mm, &
      datan(1)%dd,hhn(1,1),hhn(2,1),scadn(1:4,1),id_ptn,valuen

    IF (ios /= 0) GOTO 9997
    p1 = INDEX(ch_record,"!")
    IF (p1 > 0) THEN
      labeln = TRIM(ch_record(p1+1:))
    ELSE
      labeln = ""
    ENDIF

    IF (ANY(valuen == rmis_ser(:))) cnt_mis_org(1) = cnt_mis_org(1) + 1
  
    IF (first_asc) THEN             ! ho letto il 1o dato del file
      first_asc = .FALSE.
      CYCLE            
    ENDIF
    IF (newp) EXIT                  ! ho letto il 1o dato di un nuovo punto

!--------------------------------------------------------------------------
! 2.3) De-cumulazione

    CALL proc_scad(srq,datap(1),hhp(1:2,1),scadp(1:4,1), &
      datac(1),hhc(1:2,1),scadc(1:4,1),datan(1),hhn(1:2,1),scadn(1:4,1), &
      dataw,hhw,scadw,dec)
    
    IF (deb) THEN
      fdp = rmis
      fdn = rmis
    ENDIF
  
    IF (dec == 0) THEN                                ! Metto mancante
      valuew = rmis
      cnt_mis(1) = cnt_mis(1) + 1
  
    ELSE IF (dec == 1) THEN                           ! Lascio invariato
      valuew = valuec
      cnt_eq(1) = cnt_eq(1) + 1
      IF (valuew == rmis) cnt_mis(1) = cnt_mis(1) + 1
  
    ELSE IF (dec == 2) THEN                           ! Decumulo
      cnt_dec(1) = cnt_dec(1) + 1

      CALL decumula_scalar(scadp(1:4,1),scadc(1:4,1),scadn(1:4,1), &
        valuep,valuec,valuen,1,rmis,oper(1),srq,deb,1,valuew,fdp,fdn)
  
    ENDIF

!--------------------------------------------------------------------------
! 2.4) Scrittura (il punto non e' cambiato)

    cnt_wa = cnt_wa + 1
    IF (deb) THEN
      WRITE (90,*)
      WRITE (90,'(a,4i7)') "Scrivo asc : prog,prog_pt,srq,dec ", &
        cnt_wa,prog_ptc,srq,dec
      WRITE (90,'(a,5x,5i5,5x,4i5)') "datap,scadp:", &
        datap(1)%yy,datap(1)%mm,datap(1)%dd,hhp(:,1),scadp(:,1)
      WRITE (90,'(a,5x,5i5,5x,4i5)') "datac,scadc:", &
        datac(1)%yy,datac(1)%mm,datac(1)%dd,hhc(:,1),scadc(:,1)
      WRITE (90,'(a,5x,5i5,5x,4i5)') "datan,scadn:", &
        datan(1)%yy,datan(1)%mm,datan(1)%dd,hhn(:,1),scadn(:,1)
      WRITE (90,'(a,3(3x,e12.5))') "value p,c,n:         ", &
        valuep,valuec,valuen
      WRITE (90,'(a,3(3x,e12.5))') "value decp,decn,out: ", &
        fdp,fdn,valuew
    ENDIF
  
    WRITE (ch_value,*) valuew
    WRITE (30,chfmt) prog_ptc, dataw%yy,dataw%mm,dataw%dd, &
      hhw(1:2),scadw(1:4), id_ptc,TRIM(ch_value), "!",TRIM(labelc)

  ENDDO   ! scadenze

!==========================================================================
! 2.5) Cambio di punto o fine file: devo recuperarare l'ultimo valore
!
! Cambio di punto: metto valuen mancante ed elaboro valuec; poi metto 
!   mancanti valuep e valuec per prepararmi a elaborare il punto successivo
! Fine file: sposto valuen e valuec in valuec e valuep con valuen mancante,
!   elaboro valuec

!--------------------------------------------------------------------------
! 2.5.1) Assegno i dati relativi al punto appena completato

  IF (end_asc) THEN
    datap(1) = datac(1)
    hhp(1:2,1) = hhc(1:2,1)
    scadp(1:4,1) = scadc(1:4,1)
    valuep = valuec
    datac(1) = datan(1)
    hhc(1:2,1) = hhn(1:2,1)
    scadc(1:4,1) = scadn(1:4,1)
    valuec = valuen

  ELSE
    datas = datan(1)
    hhs(1:2) = hhn(1:2,1)
    scads(1:4) = scadn(1:4,1)
    values = valuen
  
  ENDIF

  datan(1) = date(-999,-999,-999)
  hhn(1:2,1) = (/-999,-999/)
  scadn(1:4,1) = (/-999,-999,-999,-999/)
  valuen = rmis

!--------------------------------------------------------------------------
! 2.5.2) Elaboro l'ultima scadenza del punto appena completato

  CALL proc_scad(srq,datap(1),hhp(1:2,1),scadp(1:4,1), &
    datac(1),hhc(1:2,1),scadc(1:4,1),datan(1),hhn(1:2,1),scadn(1:4,1), &
    dataw,hhw,scadw,dec)
  
  IF (deb) THEN
    fdp = rmis
    fdn = rmis
  ENDIF

  IF (dec == 0) THEN                                ! Metto mancante
    valuew = rmis
    cnt_mis(1) = cnt_mis(1) + 1

  ELSE IF (dec == 1) THEN                           ! Lascio invariato
    valuew = valuec
    cnt_eq(1) = cnt_eq(1) + 1
    IF (valuew == rmis) cnt_mis(1) = cnt_mis(1) + 1

  ELSE IF (dec == 2) THEN                           ! Decumulo
    cnt_dec(1) = cnt_dec(1) + 1

    CALL decumula_scalar(scadp(1:4,1),scadc(1:4,1),scadn(1:4,1), &
      valuep,valuec,valuen,1,rmis,oper(1),srq,deb,1,valuew,fdp,fdn)

  ENDIF

!--------------------------------------------------------------------------
! 2.5.3) Scrittura (cambio punto o fine file)

  cnt_wa = cnt_wa + 1
  IF (deb) THEN
    WRITE (90,*)
    WRITE (90,'(a,4i7)') "Scrivo asc : prog,prog_pt,srq,dec ", &
      cnt_wa,prog_ptc,srq,dec
    WRITE (90,'(a,5x,5i5,5x,4i5)') "datap,scadp:", &
      datap(1)%yy,datap(1)%mm,datap(1)%dd,hhp(:,1),scadp(:,1)
    WRITE (90,'(a,5x,5i5,5x,4i5)') "datac,scadc:", &
      datac(1)%yy,datac(1)%mm,datac(1)%dd,hhc(:,1),scadc(:,1)
    WRITE (90,'(a,5x,5i5,5x,4i5)') "datan,scadn:", &
      datan(1)%yy,datan(1)%mm,datan(1)%dd,hhn(:,1),scadn(:,1)
    WRITE (90,'(a,3(3x,e12.5))') "value p,c,n:         ", &
      valuep,valuec,valuen
    WRITE (90,'(a,3(3x,e12.5))') "value decp,decn,out: ", &
      fdp,fdn,valuew
  ENDIF

  WRITE (ch_value,*) valuew
  WRITE (30,chfmt) prog_ptc, dataw%yy,dataw%mm,dataw%dd, &
    hhw(1:2),scadw(1:4), id_ptc,TRIM(ch_value), "!",TRIM(labelc)

  IF (end_asc) EXIT

!--------------------------------------------------------------------------
! 2.5.4) Assegno i dati relativi al prossimo punto

  datap(1) = date(-999,-999,-999)
  hhp(1:2,1) = (/-999,-999/)
  scadp(1:4,1) = (/-999,-999,-999,-999/)
  valuep = rmis

  datac(1) = date(-999,-999,-999)
  hhc(1:2,1) = (/-999,-999/)
  scadc(1:4,1) = (/-999,-999,-999,-999/)
  valuec = rmis

  datan(1) = datas
  hhn(1:2,1) = hhs(1:2)
  scadn(1:4,1) = scads(1:4)
  valuen = values

ENDDO     ! punti

! Fine elaborazioni formato ASC
!--------------------------------------------------------------------------


ELSE IF (iop_fmt == 2) THEN

!==========================================================================
! 3) Decumulo: formato .grb (ciclo sugli istanti)
! NOTA: tutti gli array dimensionati (maxpar), ad esempio var_liv,data*,
!   hh*,scad*,filed*, si riferiscono solo ai parametri mediati/cumulati.

!--------------------------------------------------------------------------
! 3.0) Preliminari e inizializzazioni

! Scelgo il punto per debug
IF (deb) THEN
  IF (ksec2f(2)==234 .AND. ksec2f(3)==272) THEN  !griglia LAMI operativa
    ptdeb = 43149    ! in mezzo alla pianura
  ELSE
    ptdeb = INT(0.5*ksec2f(2)*ksec2f(3))
  ENDIF
  WRITE (90,'(a,i6)') "Punto visualizzato: ",ptdeb
ENDIF

! n.ro parametri
npar = 0
oper(:) = 0

! inizializzo i contatori statistici
cnt_eq(:) = 0 
cnt_mis(:) = 0 
cnt_mis_org(:) = 0 
cnt_dec(:) = 0
cnt_wg = 0
lstag = .FALSE.

! inizalizzo date e scadenze
datac(:) = date(-999,-999,-999)
hhc(1:2,:) = SPREAD((/-999,-999/),2,maxpar)
scadc(1:4,:) = SPREAD((/-999,-999,-999,-999/),2,maxpar)
fieldc(1:npts,:) = rmis

datan(:) = date(-999,-999,-999)
hhn(1:2,:) = SPREAD((/-999,-999/),2,maxpar)
scadn(1:4,:) = SPREAD((/-999,-999,-999,-999/),2,maxpar)
fieldn(1:npts,:) = rmis

DO kk = 1,HUGE(kk)

!--------------------------------------------------------------------------
! 3.1) Lettura

  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    GOTO 9992
  ENDIF

!--------------------------------------------------------------------------
! 3.2) Decodifica, controlli, scelta operazioni da compiere sui parametri

  psec3(2) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.200) GOTO 9991
  
  var_liv_dum(1:6) = (/ksec1(2),ksec1(1),ksec1(6:9)/)
  oper_dum = select_oper(var_liv_dum(1:3))
  
! Controllo che la griglia non sia cambiata, ma ammetto la presenza di
! griglie "compatibili" (ie h,u,v)
  IF (ANY(ksec2((/1,2,3,6,9,10,11,13,14/)) /= &
          ksec2f((/1,2,3,6,9,10,11,13,14/)))) GOTO 9994
  IF (ABS((ksec2(8)-ksec2(5)) - (ksec2f(8)-ksec2f(5))) > 2 .OR. &
      ABS((ksec2(7)-ksec2(4)) - (ksec2f(7)-ksec2f(4))) > 2) GOTO 9994
  IF (ANY(ksec2(4:8) /= ksec2f(4:8))) lstag = .TRUE.

! IF (ANY(ksec1(1:4) /= ksec1f(1:4))) THEN
!   WRITE (*,*) "WARNING: ci sono differenze nei primi 4 elementi della sez.1"
!   WRITE (*,'(a10,4(1x,i6))') "1o grib: ",ksec1f(1:4)
!   WRITE (*,'(a4,i4,a2,4(1x,i6))') "grib",kk,": ",ksec1(1:4)
! ENDIF
  IF (ksec1(5) == 0 .OR. ksec1(5) == 64) WRITE (*,*) &
    "WARNING: grib senza sez. 2, i risulati potrebbero essere imprevedibili"
  IF (ANY(ksec2(15:19) /= ksec2f(15:19)) ) WRITE (*,*) &
    "WARNING: Ci sono differenze in ksec2 (indici > 14): campo ",kk

! se non e' un parametro cumulato, lo scrivo tal quale e passo al campo 
! successivo
  IF (oper_dum == 0) THEN
    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
    IF (deb) WRITE (90,'(/2(a,3i4))') "Scrivo grib invariato   Var: ", &
      var_liv_dum(1:3)," Liv: ",var_liv_dum(4:6)
    CYCLE
  ENDIF

! verifico se ho letto un nuovo parametro
  id_parc = 0
  DO kp = 1,npar
    IF (ALL(var_liv(1:6,kp) == var_liv_dum(1:6))) THEN
      id_parc = kp
      EXIT
    ENDIF
  ENDDO
  
  IF (id_parc == 0) THEN
    npar = npar + 1
    IF (npar > maxpar) GOTO 9990
  
    var_liv(1:6,npar) = var_liv_dum(1:6)
    id_parc = npar
    oper(npar) = select_oper(var_liv_dum(1:3))
  
    IF (oper(npar) == 0) THEN
      str_oper(npar) = "invariato"
    ELSE IF (oper(npar) == 1) THEN
      str_oper(npar) = "cumulato"
    ELSE IF (oper(npar) == 2) THEN
      str_oper(npar) = "mediato"
    ENDIF
  
  ENDIF

!--------------------------------------------------------------------------
! 3.3) Per il parametro che ho appena letto, aggiorno i dati relativi all'
!      ora corrente, precedente e successiva

! Ora precedente
  datap(id_parc) = datac(id_parc)
  hhp(1:2,id_parc) = hhc(1:2,id_parc)
  scadp(1:4,id_parc) = scadc(1:4,id_parc)
  fieldp(1:npts,id_parc) = fieldc(1:npts,id_parc)
  nbitp(id_parc) = nbitc(id_parc)

! Ora corrente
  datac(id_parc) = datan(id_parc)
  hhc(1:2,id_parc) = hhn(1:2,id_parc)
  scadc(1:4,id_parc) = scadn(1:4,id_parc)
  fieldc(1:npts,id_parc) = fieldn(1:npts,id_parc)
  nbitc(id_parc) = nbitn(id_parc)

! Ora successiva (i.e. quella appena letta)
  datan(id_parc)%dd = ksec1(12)
  datan(id_parc)%mm = ksec1(11)
  datan(id_parc)%yy = ksec1(10) + 100 * (ksec1(21) - 1)
  hhn(1:2,id_parc) = ksec1(13:14)
  scadn(1:4,id_parc) = ksec1(15:18)
  fieldn(1:npts,id_parc) = field(1:npts)
  nbitn(id_parc) = ksec4(2)

! Se e' il primo campo di questo parametro, ho in memoria solo l'ora 
! successiva, quindi aspetto il prossimo giro per elaborarla
  IF (ALL(scadc(1:4,id_parc) == -999)) CYCLE

!--------------------------------------------------------------------------
! 3.4) De-cumulazione

  CALL proc_scad(srq, &
    datap(id_parc),hhp(1:2,id_parc),scadp(1:4,id_parc), &
    datac(id_parc),hhc(1:2,id_parc),scadc(1:4,id_parc), &
    datan(id_parc),hhn(1:2,id_parc),scadn(1:4,id_parc), &
    dataw,hhw,scadw,dec)
  
  IF (ALL(fieldc(1:npts,id_parc) == rmis)) &
    cnt_mis_org(id_parc) = cnt_mis_org(id_parc) + 1
  IF (deb) THEN
    fdp = rmis
    fdn = rmis
  ENDIF

  IF (dec == 0) THEN                                ! Metto mancante
    fieldw(1:npts) = rmis
    cnt_mis(id_parc) = cnt_mis(id_parc) + 1

  ELSE IF (dec == 1) THEN                           ! Lascio invariato
    fieldw(1:npts) = fieldc(1:npts,id_parc)
    cnt_eq(id_parc) = cnt_eq(id_parc) + 1
    IF (ALL(fieldw(1:npts) == rmis)) cnt_mis(id_parc) = cnt_mis(id_parc) + 1

  ELSE IF (dec == 2) THEN                           ! Decumulo
    cnt_dec(id_parc) = cnt_dec(id_parc) + 1
    CALL decumula(scadp(1:4,id_parc),scadc(1:4,id_parc),scadn(1:4,id_parc), &
      fieldp(1:npts,id_parc),fieldc(1:npts,id_parc),fieldn(1:npts,id_parc), &
      npts,rmis,oper(id_parc),srq,deb,ptdeb,fieldw(1:npts),fdp,fdn)

  ENDIF

!--------------------------------------------------------------------------
! 3.5) Scrittura

  cnt_wg = cnt_wg + 1
  IF (deb) THEN
    WRITE (90,*)
    WRITE (90,'(3a,5i5)') "Scrivo grib ",TRIM(str_oper(id_parc)), &
      ": prog,srq,id_parc,dec ",cnt_wg,srq,id_parc,dec
    WRITE (90,'(a,5x,5i5,5x,4i5)') "datap,scadp:", &
      datap(id_parc)%yy,datap(id_parc)%mm,datap(id_parc)%dd, &
      hhp(:,id_parc),scadp(:,id_parc)
    WRITE (90,'(a,5x,5i5,5x,4i5)') "datac,scadc:", &
      datac(id_parc)%yy,datac(id_parc)%mm,datac(id_parc)%dd, &
      hhc(:,id_parc),scadc(:,id_parc)
    WRITE (90,'(a,5x,5i5,5x,4i5)') "datan,scadn:", &
      datan(id_parc)%yy,datan(id_parc)%mm,datan(id_parc)%dd, &
      hhn(:,id_parc),scadn(:,id_parc)
    WRITE (90,'(a,3(3x,e12.5))') "field p,c,n:         ", &
      fieldp(ptdeb,id_parc),fieldc(ptdeb,id_parc),fieldn(ptdeb,id_parc)
    WRITE (90,'(a,3(3x,e12.5))') "field decp,decn,out: ", &
      fdp,fdn,fieldw(ptdeb)
  ENDIF
    
! Definisco sezioni
  ksec1((/2,1,6/)) = var_liv(1:3,id_parc)  ! var
  ksec1(7:9) = var_liv(4:6,id_parc)        ! liv
  ksec1(10) = MOD(dataw%yy-1,100) + 1      ! data
  ksec1(21) = (dataw%yy-1)/100 + 1
  ksec1(11) = dataw%mm
  ksec1(12) = dataw%dd
  ksec1(13:14) = hhw(1:2)                  ! ora
  ksec1(15:18) = scadw(1:4)                ! scad
  ksec1(2:4) = ksec1f(2:4)                 ! altro
  IF (ANY(fieldw(1:npts) == rmis)) THEN
    ksec1(5) = 192
  ELSE
    ksec1(5) = 128
  ENDIF
  psec3(2) = rmis
  ksec4(1) = npts
  IF (dec == 2 .AND. srq == 3) THEN
    ksec4(2) = MAX(nbitp(id_parc),nbitc(id_parc),nbitn(id_parc))
  ELSE IF (dec == 2 .AND. srq == 2) THEN
    ksec4(2) = MAX(nbitp(id_parc),nbitc(id_parc))
  ELSE
    ksec4(2) = nbitc(id_parc)
  ENDIF
  ksec4(3:) = 0

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               fieldw,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

ENDDO

!==========================================================================
! 4) Recupero l'ultimo campo di ogni parametro, che e' salvato in fieldn e
!    non e' ancora stato elaborato

!--------------------------------------------------------------------------
! 4.1) Aggiorno i dati

! Ora precedente
datap(:) = datac(:)
hhp(1:2,:) = hhc(1:2,:)
scadp(1:4,:) = scadc(1:4,:)
fieldp(1:npts,:) = fieldc(1:npts,:)
nbitp(:) = nbitc(:)

! Ora corrente
datac(:) = datan(:)
hhc(1:2,:) = hhn(1:2,:)
scadc(1:4,:) = scadn(1:4,:)
fieldc(1:npts,:) = fieldn(1:npts,:)
nbitc(:) = nbitn(:)

! Ora successiva (metto mancante)
datan(:) = date(-999,-999,-999)
hhn(1:2,:) = SPREAD((/-999,-999/),2,maxpar)
scadn(1:4,:) = SPREAD((/-999,-999,-999,-999/),2,maxpar)
fieldn(1:npts,:) = rmis
nbitn(:) = 0

DO id_parc = 1,npar

!--------------------------------------------------------------------------
! 4.2) De-cumulazione

  CALL proc_scad(srq, &
    datap(id_parc),hhp(1:2,id_parc),scadp(1:4,id_parc), &
    datac(id_parc),hhc(1:2,id_parc),scadc(1:4,id_parc), &
    datan(id_parc),hhn(1:2,id_parc),scadn(1:4,id_parc), &
    dataw,hhw,scadw,dec)

  IF (ALL(fieldc(1:npts,id_parc) == rmis)) &
    cnt_mis_org(id_parc) = cnt_mis_org(id_parc) + 1

  IF (dec == 0) THEN                                ! Metto mancante
    fieldw(1:npts) = rmis
    cnt_mis(id_parc) = cnt_mis(id_parc) + 1

  ELSE IF (dec == 1) THEN                           ! Lascio invariato
    fieldw(1:npts) = fieldc(1:npts,id_parc)
    cnt_eq(id_parc) = cnt_eq(id_parc) + 1
    IF (ALL(fieldw(1:npts) == rmis)) cnt_mis(id_parc) = cnt_mis(id_parc) + 1

  ELSE IF (dec == 2) THEN                           ! Decumulo
    cnt_dec(id_parc) = cnt_dec(id_parc) + 1
    CALL decumula(scadp(1:4,id_parc),scadc(1:4,id_parc),scadn(1:4,id_parc), &
      fieldp(1:npts,id_parc),fieldc(1:npts,id_parc),fieldn(1:npts,id_parc), &
      npts,rmis,oper(id_parc),srq,deb,ptdeb,fieldw(1:npts),fdp,fdn)

  ENDIF

!--------------------------------------------------------------------------
! 4.3) Scrittura
  cnt_wg = cnt_wg + 1
  IF (deb) THEN
    WRITE (90,*)
    WRITE (90,'(3a,5i5)') "Scrivo grib (last) ",TRIM(str_oper(id_parc)), &
      ": prog,srq,id_parc,dec ",cnt_wg,srq,id_parc,dec
    WRITE (90,'(a,5x,5i5,5x,4i5)') "datap,scadp:", &
      datap(id_parc)%yy,datap(id_parc)%mm,datap(id_parc)%dd, &
      hhp(:,id_parc),scadp(:,id_parc)
    WRITE (90,'(a,5x,5i5,5x,4i5)') "datac,scadc:", &
      datac(id_parc)%yy,datac(id_parc)%mm,datac(id_parc)%dd, &
      hhc(:,id_parc),scadc(:,id_parc)
    WRITE (90,'(a,5x,5i5,5x,4i5)') "datan,scadn:", &
      datan(id_parc)%yy,datan(id_parc)%mm,datan(id_parc)%dd, &
      hhn(:,id_parc),scadn(:,id_parc)
    WRITE (90,'(a,3(3x,e12.5))') "field p,c,n:         ", &
      fieldp(ptdeb,id_parc),fieldc(ptdeb,id_parc),fieldn(ptdeb,id_parc)
    WRITE (90,'(a,3(3x,e12.5))') "field decp,decn,out: ", &
      fdp,fdn,fieldw(ptdeb)
  ENDIF
    
  ksec1((/2,1,6/)) = var_liv(1:3,id_parc)  ! var
  ksec1(7:9) = var_liv(4:6,id_parc)        ! liv
  ksec1(10) = MOD(dataw%yy-1,100) + 1      ! data
  ksec1(21) = (dataw%yy-1)/100 + 1
  ksec1(11) = dataw%mm
  ksec1(12) = dataw%dd
  ksec1(13:14) = hhw(1:2)                  ! ora
  ksec1(15:18) = scadw(1:4)                ! scad
  ksec1(2:4) = ksec1f(2:4)                 ! altro
  IF (ANY(fieldw(1:npts) == rmis)) THEN
    ksec1(5) = 192
  ELSE
    ksec1(5) = 128
  ENDIF
  psec3(2) = rmis
  ksec4(1) = npts
  IF (dec == 2 .AND. srq == 3) THEN
    ksec4(2) = MAX(nbitp(id_parc),nbitc(id_parc),nbitn(id_parc))
  ELSE IF (dec == 2 .AND. srq == 2) THEN
    ksec4(2) = MAX(nbitp(id_parc),nbitc(id_parc))
  ELSE
    ksec4(2) = nbitc(id_parc)
  ENDIF
  ksec4(3:) = 0
  
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               fieldw,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

ENDDO

IF (lstag) WRITE (*,*) &
  "Warning, trovate griglie diverse ma compatibili (U,V,H)"

! Fine elaborazioni formato GRIB
!--------------------------------------------------------------------------

ENDIF

!==========================================================================
! 5) Conclusione

IF (iop_fmt == 1) THEN
  CLOSE(20)
  CLOSE(30)

  WRITE (*,'(a)') "Decumula_lm: riepilogo"
  WRITE (*,'(a,2i8)') "dati decumulati, invariati validi: ",&
    cnt_dec(1),cnt_eq(1)
  WRITE (*,'(a,2i8)') "dati mancanti in input, in output: ", &
    cnt_mis_org(1),cnt_mis(1)

ELSE IF (iop_fmt == 2) THEN
  CALL PBCLOSE(iuin,kret)
  CALL PBCLOSE(iuout,kret)
  IF (deb) CLOSE(90)

  IF (npar == 0) THEN
    WRITE (*,'(a)') "Decumula_lm: nessun parametro modificato"
  ELSE
    WRITE (*,'(a)') "Decumula_lm: riepilogo parametri modificati"
    DO kp = 1,npar
      WRITE (*,'(2(a,3i4),3x,2a)') "  Var: ",var_liv(1:3,kp), &
        " Liv: ",var_liv(4:6,kp)," Tipo: ",str_oper(kp)
      WRITE (*,'(a,2i8)') "  campi decumulati, invariati validi:          ",&
        cnt_dec(kp),cnt_eq(kp)
      WRITE (*,'(a,2i8)') "  campi interamente mancanti in input, output: ", &
        cnt_mis_org(kp),cnt_mis(kp)
    ENDDO
  ENDIF

ENDIF 
STOP

!==========================================================================
! 5) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Nome del file di input non riconoscibilie: ",TRIM(filein)
STOP

9997 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," record ",kk
STOP

9995 CONTINUE
WRITE (*,*) "Griglia troppo grande (valore, maxdim): ",npts,maxdim
STOP

9994 CONTINUE
WRITE (*,*) "Ci sono grib con sez.2 diversa: file ",TRIM(filein)
WRITE (*,'(a10,14(1x,i6))') "1o grib: ",ksec2f(1:14)
WRITE (*,'(a4,i4,a2,14(1x,i6))') "grib",kk,": ",ksec2(1:14)
STOP

9992 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib ",kk," kret ",kret
STOP

9991 CONTINUE
WRITE (*,*) "Errore decodificando ",TRIM(filein)," grib ",kk," kret ",kret
STOP

9990 CONTINUE
WRITE (*,'(a,i2,a)') "Errore, n.ro massimo di parametri diversi (", &
  maxpar,") superato"
STOP

END PROGRAM decumula_lm

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION select_oper(var) RESULT(oper)
!--------------------------------------------------------------------------
! Analizza il parametro definito da var(3), e ritorna:
!   2 se e' un parametro mediato
!   1 se e' un parametro cumulato
!   0 altrimenti
!--------------------------------------------------------------------------
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: var(3)
INTEGER :: oper
! 
INTEGER, PARAMETER :: ncum = 7        ! n.ro parametri cumulati gestiti
INTEGER, PARAMETER :: nmed = 8        ! n.ro parametri mediati gestiti
!
INTEGER :: var_cum(3,ncum),var_med(3,nmed),kp

! Descrittori dei parametri cumulati
var_cum(1:3,1) = (/-1,2,61/)             ! tp
var_cum(1:3,2) = (/-1,2,78/)             ! consnw
var_cum(1:3,3) = (/-1,2,79/)             ! gspsnw
var_cum(1:3,4) = (/-1,2,152/)            ! conp
var_cum(1:3,5) = (/-1,2,153/)            ! tsno
var_cum(1:3,6) = (/-1,201,102/)          ! gsprai
var_cum(1:3,7) = (/-1,201,113/)          ! conrai

! descrittori dei parametri mediati
var_med(1:3,1) = (/-1,2,111/)            ! swbudg
var_med(1:3,2) = (/-1,2,112/)            ! lwbudg
var_med(1:3,3) = (/-1,2,121/)            ! lhf
var_med(1:3,4) = (/-1,2,122/)            ! shf
var_med(1:3,5) = (/-1,2,124/)            ! umfl
var_med(1:3,6) = (/-1,2,125/)            ! vmfs
var_med(1:3,7) = (/-1,201,22/)           ! radir 22
var_med(1:3,8) = (/-1,201,23/)           ! radiff 23

!--------------------------------------------------------------------------

oper = 0

DO kp = 1,ncum
  IF ((var_cum(1,kp) == -1 .OR. var_cum(1,kp) == var(1)) .AND. &
      (var_cum(2,kp) == -1 .OR. var_cum(2,kp) == var(2)) .AND. &
      (var_cum(3,kp) == -1 .OR. var_cum(3,kp) == var(3)) ) THEN
    oper = 1
    RETURN
  ENDIF
ENDDO

DO kp = 1,nmed
  IF ((var_med(1,kp) == -1 .OR. var_med(1,kp) == var(1)) .AND. &
      (var_med(2,kp) == -1 .OR. var_med(2,kp) == var(2)) .AND. &
      (var_med(3,kp) == -1 .OR. var_med(3,kp) == var(3)) ) THEN
    oper = 2
    RETURN
  ENDIF
ENDDO

RETURN
END FUNCTION select_oper

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE proc_scad(srq,datap,hhp,scadp,datac,hhc,scadc, &
  datan,hhn,scadn,dataw,hhw,scadw,dec)

!--------------------------------------------------------------------------
! Date le data/scad corrente, precedente e successiva, controlla se e' 
! possibile decumulare i dati relativi. 
! Ritorna data e scadenza in output e il codice dell'operazione da compiere:
!   dec = 0: il dato deve essere messo mancante
!   dec = 1: il dato puo' essere scritto cosi' com'e'
!   dec = 2: il dato puo' essere decumulato
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE
!
TYPE(date), INTENT(IN) :: datap,datac,datan
INTEGER, INTENT(IN) :: srq,hhp(2),hhc(2),hhn(2),scadp(4),scadc(4),scadn(4)
TYPE(date), INTENT(OUT) :: dataw
INTEGER, INTENT(OUT) :: hhw(2),scadw(4),dec

INTEGER :: dhemip,dhemin

!--------------------------------------------------------------------------
! 1) Verifico quale operazione deve essere compiuta.

IF (srq /= 2 .AND. srq /= 3) GOTO 9999
dhemip = 24*(datac-datap) + (hhc(1)-hhp(1))
dhemin = 24*(datan-datac) + (hhn(1)-hhc(1))

! 1.1) Sono richiesti dati relativi all'ora precedente
IF (srq == 2) THEN

! Scrivo invariato
  IF (scadc(1) == 1 .AND. scadc(3) == scadc(2)+1 .AND. &
      (scadc(4)==3 .OR. scadc(4)==4 .OR. scadc(4)==13)) THEN 
    dec = 1

! Decumulo: LAMA
  ELSE IF (dhemip == 1 .AND. &
    ALL(scadc(1:4) == (/1,0,scadc(3),13/)) .AND. &
    ALL(scadp(1:4) == (/1,0,scadc(3)-1,13/)) ) THEN
    dec = 2

! Decumulo: previsioni LM
  ELSE IF (dhemip == 0 .AND. &
    ALL(scadc(1:4) == (/1,0,scadc(3),scadc(4)/)) .AND. &
    ALL(scadp(1:4) == (/1,0,scadc(3)-1,scadc(4)/)) ) THEN
    dec = 2

! Valore non calcolabile
  ELSE
    dec = 0

  ENDIF

! 1.2) Sono richiesti dati istantanei
ELSE IF (srq == 3) THEN

! Scrivo invariato
  IF (ALL(scadc(1:4) == (/1,0,scadc(3),0/)) .OR. &
      ALL(scadc(1:4) == (/1,0,0,13/)) ) THEN
    dec = 1

! Decumulo: LAMAZ
  ELSE IF (dhemin == 1 .AND. &
    ALL(scadc(1:4) == (/1,0,1,13/)) .AND. &
    ALL(scadn(1:4) == (/1,0,1,13/)) ) THEN
    dec = 2

! Decumulo: analisi COSMO
! Scadenze di tipo 1,0,N,13; per i valori di N gestisco 3 casi:
!
!    prev curr next   descrizione
! a) *    1    2      prima scadenza di un run, scadp e' irrilevante
! b) N-1  N    N+1    i 3 istanti appartengono allo stesso run
! c) N-1  N    1      passaggio da un run a quello successivo

  ELSE IF (dhemin == 1 .AND. &
    ALL(scadc(1:4) == (/1,0,1,13/)) .AND. &
    ALL(scadn(1:4) == (/1,0,2,13/)) ) THEN
    dec = 2

  ELSE IF (dhemip == 1 .AND. dhemin == 1 .AND. &
    ALL(scadp(1:4) == (/1,0,scadc(3)-1,13/)) .AND. &
    ALL(scadn(1:4) == (/1,0,scadc(3)+1,13/)) ) THEN
    dec = 2

  ELSE IF (dhemip == 1 .AND. dhemin == 1 .AND. &
    ALL(scadp(1:4) == (/1,0,scadc(3)-1,13/)) .AND. &
    ALL(scadn(1:4) == (/1,0,1,13/)) ) THEN
    dec = 2

! Decumulo: previsioni LM, scadenza 1
  ELSE IF (dhemin == 0 .AND. &
    ALL(scadc(1:4) == (/1,0,1,scadc(4)/)) .AND. &
    ALL(scadn(1:4) == (/1,0,2,scadc(4)/)) ) THEN
    dec = 2

! Decumulo: previsioni LM, scadenze > 1
  ELSE IF (dhemip == 0 .AND. dhemin == 0 .AND. &
    ALL(scadp(1:4) == (/1,0,scadc(3)-1,scadc(4)/)) .AND. &
    ALL(scadc(1:4) == (/1,0,scadc(3),scadc(4)/)) .AND. &
    ALL(scadn(1:4) == (/1,0,scadc(3)+1,scadc(4)/)) ) THEN
    dec = 2

! Valore non calcolabile
  ELSE
    dec = 0

  ENDIF

ENDIF

!--------------------------------------------------------------------------
! 2) Calcolo data e scadenza in output

dataw = datac
hhw = hhc

IF (srq == 2) THEN                                    ! dati ora prec.
  IF (scadc(4) == 13) THEN                            ! LAMA, LAMAZ
    scadw = (/1,0,1,13/)
  ELSE IF (scadc(4) == 3 .OR. scadc(4) == 4) THEN     ! LM, scadenze > 0
    scadw = (/1,scadc(3)-1,scadc(3),scadc(4)/)
  ELSE IF (ALL(scadc(1:4) == (/1,0,0,0/))) THEN       ! LM, analisi
    scadw = (/1,0,0,scadn(4)/)                  
    IF (dec /= 0) GOTO 9998
  ELSE
    GOTO 9998
  ENDIF

ELSE IF (srq == 3) THEN                               ! dati istantanei
  IF (scadc(4) == 13) THEN                            ! LAMA, LAMAZ
    scadw = (/1,0,0,13/)
  ELSE
    scadw = (/1,scadc(3),0,0/)                        ! LM
  ENDIF
ENDIF

RETURN

!--------------------------------------------------------------------------
! 3) Gestione errori (non dovrebbero mai verificarsi...)

9999 CONTINUE
WRITE (*,*) "Errore proc_scad, srq illegale: ",srq
dec = 0
RETURN

9998 CONTINUE
WRITE (*,*) "proc_scad, errore nel calcolo scadenza output: "
WRITE (*,*) "dec: ",dec
WRITE (*,*) "scadp: ",scadp
WRITE (*,*) "scadc: ",scadc
WRITE (*,*) "scadn: ",scadn
STOP

END SUBROUTINE proc_scad

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE decumula(scadp,scadc,scadn,fieldp,fieldc,fieldn,npts,rmis,oper, &
  srq,deb,ptdeb,fieldw,fdp,fdn)
!-------------------------------------------------------------------------
! Dati 3 campi consecutivi di uno stesso parametro, ritorna il cmapo 
! decumulato relativo all'istante centrale.
!
! Metodo: 
! calcolo in ogni caso la media nell'ora precedente; se sono richiesti 
! dati istantanei, calcolo anche quella nell'ora successiva e prendo la 
! media delle 2.
!
! Note:
! Viene chiamata solo se i 3 campi consentono effettivamente la 
!   de-cumulazione; le verifiche e il calcolo della scadenza in output
!   sono nella subroutine proc_scad.
!
! oper: vale 1 se il campo e' cumulato, 2 se e' mediato
! srq:  vale 2 se sono richiesti dati relativi all'ora precedente, 3 se 
!       sono richiesti dati istantanei
!-------------------------------------------------------------------------
IMPLICIT NONE

! Parametri della subrotine
INTEGER, INTENT(IN) :: scadp(4),scadc(4),scadn(4),npts,oper,srq,ptdeb
REAL, INTENT(IN) :: fieldp(npts),fieldc(npts),fieldn(npts),rmis
LOGICAL, INTENT(IN) :: deb
REAL, INTENT(OUT) :: fieldw(npts),fdp,fdn

! Variabili locali
REAL :: f_decp(npts),f_decn(npts)

!-------------------------------------------------------------------------
! 1) Calcolo campo cumulato/mediato nell'ora precedente

IF (scadc(3)==scadc(2)+1 .AND. (scadc(4)==3 .OR. scadc(4)==4 .OR. & 
    scadc(4)==13)) THEN                ! campo relativo all'ora precedente
  f_decp(1:npts) = fieldc(1:npts)

ELSE IF (oper == 1) THEN               ! campo cumulato
  WHERE (fieldc(1:npts) /= rmis .AND. fieldp(1:npts) /= rmis)
    f_decp(1:npts) = fieldc(1:npts) - fieldp(1:npts)
  ELSEWHERE
    f_decp(1:npts) = rmis
  ENDWHERE

ELSE IF (oper == 2) THEN               ! campo mediato
  WHERE (fieldc(1:npts) /= rmis .AND. fieldp(1:npts) /= rmis)
    f_decp(1:npts) = fieldc(1:npts) * REAL(scadc(3)) - &
      fieldp(1:npts) * REAL(scadp(3))
  ELSEWHERE
    f_decp(1:npts) = rmis
  ENDWHERE

ENDIF

!-------------------------------------------------------------------------
! 2) Calcolo campo cumulato/mediato nell'ora successiva

IF (scadn(3)==scadn(2)+1 .AND. (scadn(4)==3 .OR. scadn(4)==4 .OR. &
    scadn(4)==13)) THEN                ! campo relativo all'ora precedente
  f_decn(1:npts) = fieldn(1:npts)

ELSE IF (oper == 1) THEN               ! campo cumulato
  WHERE (fieldn(1:npts) /= rmis .AND. fieldc(1:npts) /= rmis)
    f_decn(1:npts) = fieldn(1:npts) - fieldc(1:npts)
  ELSEWHERE
    f_decn(1:npts) = rmis
  ENDWHERE

ELSE IF (oper == 2) THEN               ! campo mediato
  WHERE (fieldn(1:npts) /= rmis .AND. fieldc(1:npts) /= rmis)
    f_decn(1:npts) = fieldn(1:npts) * REAL(scadn(3)) - &
      fieldc(1:npts) * REAL(scadc(3))
  ELSEWHERE
    f_decn(1:npts) = rmis
  ENDWHERE

ENDIF

!-------------------------------------------------------------------------
! 3) Calcolo campo in output

IF (srq == 2) THEN
  fieldw(1:npts) = f_decp(1:npts)
ELSE IF (srq == 3) THEN
  WHERE (f_decp(1:npts) /= rmis .AND. f_decn(1:npts) /= rmis)
    fieldw(1:npts) = 0.5 * (f_decp(1:npts) + f_decn(1:npts))
  ELSEWHERE
    fieldw(1:npts) = rmis
  ENDWHERE
ENDIF

IF (deb) THEN
  fdp = f_decp(ptdeb)
  fdn = f_decn(ptdeb)
ENDIF
 
RETURN
END SUBROUTINE decumula

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE decumula_scalar(scadp,scadc,scadn,fieldp,fieldc,fieldn,npts, &
  rmis,oper,srq,deb,ptdeb,fieldw,fdp,fdn)
!-------------------------------------------------------------------------
! Dati 3 campi consecutivi di uno stesso parametro, ritorna il cmapo 
! decumulato relativo all'istante centrale.
!
! Metodo: 
! calcolo in ogni caso la media nell'ora precedente; se sono richiesti 
! dati istantanei, calcolo anche quella nell'ora successiva e prendo la 
! media delle 2.
!
! Note:
! Viene chiamata solo se i 3 campi consentono effettivamente la 
!   de-cumulazione; le verifiche e il calcolo della scadenza in output
!   sono nella subroutine proc_scad.
!
! oper: vale 1 se il campo e' cumulato, 2 se e' mediato
! srq:  vale 2 se sono richiesti dati relativi all'ora precedente, 3 se 
!       sono richiesti dati istantanei
!-------------------------------------------------------------------------
IMPLICIT NONE

! Parametri della subrotine
INTEGER, INTENT(IN) :: scadp(4),scadc(4),scadn(4),npts,oper,srq,ptdeb
REAL, INTENT(IN) :: fieldp,fieldc,fieldn,rmis
LOGICAL, INTENT(IN) :: deb
REAL, INTENT(OUT) :: fieldw,fdp,fdn

! Variabili locali
REAL :: f_decp,f_decn

!-------------------------------------------------------------------------
! 1) Calcolo campo cumulato/mediato nell'ora precedente

IF (scadc(3)==scadc(2)+1 .AND. (scadc(4)==3 .OR. scadc(4)==4 .OR. & 
    scadc(4)==13)) THEN                ! campo relativo all'ora precedente
  f_decp = fieldc

ELSE IF (oper == 1) THEN               ! campo cumulato
  IF (fieldc /= rmis .AND. fieldp /= rmis) THEN
    f_decp = fieldc - fieldp
  ELSE
    f_decp = rmis
  ENDIF

ELSE IF (oper == 2) THEN               ! campo mediato
  IF (fieldc /= rmis .AND. fieldp /= rmis) THEN
    f_decp = fieldc * REAL(scadc(3)) - &
      fieldp * REAL(scadp(3))
  ELSE
    f_decp = rmis
  ENDIF

ENDIF

!-------------------------------------------------------------------------
! 2) Calcolo campo cumulato/mediato nell'ora successiva

IF (scadn(3)==scadn(2)+1 .AND. (scadn(4)==3 .OR. scadn(4)==4 .OR. &
    scadn(4)==13)) THEN                ! campo relativo all'ora precedente
  f_decn = fieldn

ELSE IF (oper == 1) THEN               ! campo cumulato
  IF (fieldn /= rmis .AND. fieldc /= rmis) THEN
    f_decn = fieldn - fieldc
  ELSE
    f_decn = rmis
  ENDIF

ELSE IF (oper == 2) THEN               ! campo mediato
  IF (fieldn /= rmis .AND. fieldc /= rmis) THEN
    f_decn = fieldn * REAL(scadn(3)) - &
      fieldc * REAL(scadc(3))
  ELSE
    f_decn = rmis
  ENDIF

ENDIF

!-------------------------------------------------------------------------
! 3) Calcolo campo in output

IF (srq == 2) THEN
  fieldw = f_decp
ELSE IF (srq == 3) THEN
  IF (f_decp /= rmis .AND. f_decn /= rmis) THEN
    fieldw = 0.5 * (f_decp + f_decn)
  ELSE
    fieldw = rmis
  ENDIF
ENDIF

IF (deb) THEN
  fdp = f_decp
  fdn = f_decn
ENDIF
 
RETURN
END SUBROUTINE decumula_scalar

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: decumula_lm.exe filein fileout -ist/-prh -asc/-grb [-v] [-h]" 
WRITE (*,*) ""
WRITE (*,*) "De-cumula i parametri LM contenuti in un file in formato .ASC (otuput di "
WRITE (*,*) "  seriet) oppure .grb"
WRITE (*,*) ""
WRITE (*,*) "  -ist: scrive valori 'istantanei' (media tra le medie di 2 ore consecutive)"
WRITE (*,*) "  -prh: scrive valori relativi all'ora precedente"
WRITE (*,*) "  -asc: input ASCII (se il parametro non e' cum/med, non scrive nulla)"
WRITE (*,*) "  -grb: input GRIB"
WRITE (*,*) "  -v  : scrive log esteso (file decumula_lm.log)"
WRITE (*,*) "  -h  : scrive questo help"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

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


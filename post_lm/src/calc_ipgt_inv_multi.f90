PROGRAM calc_ipgt_inv_multi
!--------------------------------------------------------------------------
! Programma che stima la classe di stabilita' a partire dagli output LM
! Usa diversi metodi:
! - Reuter 1970 (COST 715): da velocita' vento e rad. globale
! - Bowen 1983 (SRDT): da velocita' vento, rad. solare, inv. termica 
! - Turner 1964 + Bowen: giorno come Bowen, notte da nuvolosita' e vel. vento
!
! Note:
! Legge e scrive da files grib diversi per ciascun parametro.
! Tutti i grib devono essere definiti sulla stessa area.
! Data e scadenza nei files di input devono corrispondere; i dati di 
!   radiazione visibile e IR devono riferirsi alla media sull'ora 
!   precedente (in analogia con la logica seguita da Calmet)
!
!                                         Versione 1.0.1, Enrico 07/09/2012
!--------------------------------------------------------------------------

IMPLICIT NONE

INTEGER, PARAMETER :: klog = 11288  ! punto per log (S.P.Cap., area LAMAZ)
REAL, PARAMETER :: rmis = -1.e20    ! valore per dati mancanti

! Dichiarazioni per GRIBEX.
INTEGER, PARAMETER :: maxdim = 300000  ! dimensione massima dei GRIB
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1_sav(1024),ksec2_sav(1024)
INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2(512),psec3(2)
REAL :: field(maxdim)

! Nomi files input/output
CHARACTER (LEN=15), PARAMETER :: file_in(8) = (/ &
  "10_u.grb       ","       10_v.grb","  srf_sosbs.grb","    srf_alb.grb", &
  " srf_thsbs.grb ","      prf_t.grb","       02_t.grb","    srf_tcc.grb"/)
CHARACTER (LEN=15), PARAMETER :: file_out(4) = & 
  (/"   ipgt_reu.grb","   ipgt_bow.grb","   ipgt_tur.grb","  inversion.grb"/)

! Tabelle per calcolo IPGT secondo Reuter 1970
REAL, PARAMETER :: reu_class_rad(0:7) = &
  (/-HUGE(0.),-70.,-6.5,54.5,109.5,244.5,450.,HUGE(0.)/)
! (/-HUGE(0.),-28.,-6.5,54.5,109.5,244.5,415.,HUGE(0.)/)
REAL, PARAMETER :: reu_class_ff(0:6) = &
  (/0.,2.,3.,4.,5.,7.,HUGE(0.)/)
INTEGER, PARAMETER :: reu_ipgt(7,6) = RESHAPE((/ &
  6,6,4,3,2,2,1, &
  6,5,4,4,3,2,2, &
  5,4,4,4,3,2,2, &
  5,4,4,4,3,3,2, &
  5,4,4,4,4,3,3, &
  4,4,4,4,4,4,4  &
  /),(/7,6/))  

! Tabelle per calcolo IPGT secondo Bowen 1983 e Turner 1964
! NB: per mantenere la simmetria con quelle di Reuter, le righe sono state 
!     ribaltate (i valori bassi del 1o indice in bow_ipgt_* e bow_calss_rad
!     corrispondono a situazioni stabili).
! soglia_inv = soglia per considerare significativa l'inversione termica
REAL, PARAMETER :: soglia_inv = 1.0    

REAL, PARAMETER :: bow_class_rad_day(0:4) = &
  (/-HUGE(0.),175.,675.,925.,HUGE(0.)/)
REAL, PARAMETER :: bow_class_ff_day(0:5) = &
  (/-HUGE(0.),2.,3.,5.,6.,HUGE(0.)/)
REAL, PARAMETER :: bow_class_ff_night(0:3) = &
  (/-HUGE(0.),2.,2.5,HUGE(0.)/)
INTEGER, PARAMETER :: bow_ipgt_day(4,5) = RESHAPE((/ &
  4,2,1,1, &
  4,3,2,1, &
  4,3,2,2, &
  4,4,3,3, &
  4,4,4,3  &
  /),(/4,5/))  
INTEGER, PARAMETER :: bow_ipgt_night(2,3) = RESHAPE((/ &
  6,5, &
  5,4, &
  4,4  &
  /),(/2,3/))  

! Altre variabili del programma
REAL, ALLOCATABLE :: par_in(:,:),sw_down(:),inversion(:),ff(:)
INTEGER, ALLOCATABLE :: ipgt(:,:),cl_rad(:,:),cl_ff(:,:),idlev(:,:)
LOGICAL, ALLOCATABLE :: day_time(:)

REAL :: tlog(50)
INTEGER :: np,nlevs,rql,iu_in(8),iu_out(4)
INTEGER :: kfin,kp,k,kc,kl,kist,ios
CHARACTER (LEN=80) :: chdum,next_arg
CHARACTER (LEN=1) :: inp
LOGICAL :: lreu,lbow,ltur,linv

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
lreu = .FALSE.
lbow = .FALSE.
ltur = .FALSE.
linv = .FALSE.
next_arg = ""

DO kp = 1,HUGE(kp)
  CALL getarg(kp,chdum) 

  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-reu") THEN
    lreu = .TRUE.
  ELSE IF (TRIM(chdum) == "-bow") THEN
    lbow = .TRUE.
  ELSE IF (TRIM(chdum) == "-tur") THEN
    ltur = .TRUE.
  ELSE IF (TRIM(chdum) == "-inv") THEN
    linv = .TRUE.
  ELSE IF (TRIM(chdum) == "-lev") THEN
    next_arg = "nlevs"
  ELSE IF (next_arg == "nlevs") THEN
    READ (chdum,*,IOSTAT=ios) nlevs
    IF (ios /= 0) THEN
      CALL write_help
      STOP
    ENDIF  
    next_arg = ""
  ENDIF
ENDDO

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
DO kfin = 1,8
  IF ((kfin==4 .AND. .NOT.(lbow.OR.ltur)) .OR. (kfin==5 .AND. .NOT.lreu) .OR. &
      (kfin==6 .AND. .NOT.(lbow.OR.linv)) .OR. &
      (kfin==7 .AND. .NOT.(lbow.OR.linv)) .OR. (kfin==8 .AND. .NOT.ltur)) CYCLE
  CALL PBOPEN (iu_in(kfin),file_in(kfin),'R',kret)
  IF (kret /= 0) GOTO 9999
ENDDO

IF (lreu) CALL PBOPEN (iu_out(1),file_out(1),'W',kret)
IF (lbow) CALL PBOPEN (iu_out(2),file_out(2),'W',kret)
IF (ltur) CALL PBOPEN (iu_out(3),file_out(3),'W',kret)
IF (linv) CALL PBOPEN (iu_out(4),file_out(4),'W',kret)
OPEN (UNIT = 95, FILE="calc_ipgt_inv.log", STATUS="REPLACE", FORM="FORMATTED")

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura (cicli su: istanti, files di input, livelli in input)

ist: DO kist = 1,HUGE(kist)

! Se non e' il primo istante, resetto le variabili di calcolo
  IF (kist > 1) THEN
    ipgt(:,:) = 0
    par_in(:,:) = rmis
    ff(:) = rmis
    sw_down(:) = rmis
    inversion(:) = rmis
  ENDIF

  DO kfin = 1,8

    IF ((kfin==4 .AND. .NOT.(lbow.OR.ltur)) .OR. (kfin==5 .AND. .NOT.lreu) .OR. &
        (kfin==6 .AND. .NOT.(lbow.OR.linv)) .OR. &
        (kfin==7 .AND. .NOT.(lbow.OR.linv)) .OR. (kfin==8 .AND. .NOT.ltur)) CYCLE

!   Ciclo sui livelli (per gestire T 3d, di cui mi serve il massimo)
    IF (kfin == 6) THEN
      rql = nlevs
    ELSE
      rql = 1
    ENDIF
    DO kl = 1,rql
   
!--------------------------------------------------------------------------
! 2.1) Lettura e controlli

! 2.1.1 Leggo e decodifico i grib di input
      CALL PBGRIB(iu_in(kfin),kbuffer,maxdim*4,klen,kret)
      IF (kret == -1 .AND. kfin == 1) THEN 
        EXIT ist
      ELSE IF (kret == -1) THEN
        GOTO 9998
      ELSE IF (kret < -1) THEN
        WRITE(*,*) "Error pbgrib: kret ",kret
        STOP
      ENDIF
   
      psec3(2) = rmis                           ! forzo dati mancanti = rmis
      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   field,maxdim,kbuffer,maxdim,klen,'D',kret)
      IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.1.2 Se e' il primo grib: alloco e inizializzo le variabili, salvo sezione 2,
!       capisco se sto elaborando analisi (LAMA) o previsioni (LM)
      IF (kfin == 1 .AND. kist == 1) THEN
        ksec2_sav(:) = ksec2(:)
        np = ksec4(1)
   
        ALLOCATE (par_in(8,np),ipgt(3,np))
        ALLOCATE (ff(np),cl_rad(3,1:np),cl_ff(3,1:np))
        IF (lbow .OR. ltur) ALLOCATE (sw_down(np),day_time(np))
        IF (lbow .OR. linv) ALLOCATE (idlev(nlevs,3),inversion(np))
   
        ipgt(:,:) = 0
        par_in(:,:) = rmis
        ff(:) = rmis
        sw_down(:) = rmis
        inversion(:) = rmis

        IF (ksec1(18) == 0) THEN
          WRITE (*,*) "Elaboro previsioni LM"
          inp = "F"
        ELSE IF (ksec1(18) == 13) THEN
          WRITE (*,*) "Elaboro analisi LAMA"
          inp = "A"
        ELSE
          GOTO 9996
        ENDIF
   
      ENDIF
   
! 2.1.3 Controlli su data, scadenza, livello e area
      IF (ANY(ksec2(:) /= ksec2_sav(:)) .OR. ksec4(1) /= np) GOTO 9997
   
      SELECT CASE (kfin)
      CASE (1)
        ksec1_sav(:) = ksec1(:)
        IF (inp == "F" .AND. ANY(ksec1(15:18)/=(/1,ksec1(16),0,0/))) GOTO 9995
        IF (inp == "A" .AND. ANY(ksec1(15:18)/=(/1,0,0,13/))) GOTO 9995

      CASE(2)
        IF (ANY(ksec1(7:18) /= ksec1_sav(7:18)) .OR. &
          ksec1(21) /= ksec1_sav(21)) GOTO 9994

      CASE(3,5)
        IF (ANY(ksec1(7:9) /= (/1,0,0/))) GOTO 9994
        IF (inp == "A" .AND. &
            (ANY(ksec1(10:16)/=ksec1_sav(10:16)) .OR. ksec1(17)/=1 .OR. &
             ksec1(18)/=ksec1_sav(18) .OR. ksec1(21)/=ksec1_sav(21)) ) GOTO 9994
        IF (inp == "F" .AND. &
            (ANY(ksec1(10:15)/=ksec1_sav(10:15)) .OR. &
             ksec1(16)/=ksec1_sav(16)-1 .OR. ksec1(17)/=ksec1_sav(16) .OR. &
             ksec1(18)/=3 .OR. ksec1(21)/=ksec1_sav(21)) ) GOTO 9994

      CASE(4,8)
        IF (ANY(ksec1(7:9) /= (/1,0,0/))) GOTO 9994
        IF (ANY(ksec1(10:18) /= ksec1_sav(10:18)) .OR. &
          ksec1(21) /= ksec1_sav(21)) GOTO 9994

      CASE (6)
        IF (ANY(ksec1(10:18) /= ksec1_sav(10:18)) .OR. &
          ksec1(21) /= ksec1_sav(21)) GOTO 9994
        IF (kist == 1) THEN
          idlev(kl,1:3) = ksec1(7:9)
        ELSE
          IF (ANY(ksec1(7:9) /= idlev(kl,1:3))) GOTO 9994
        ENDIF

      CASE(7)
        IF (ANY(ksec1(7:9) /= (/105,2,0/))) GOTO 9994
        IF (ANY(ksec1(10:18) /= ksec1_sav(10:18)) .OR. &
          ksec1(21) /= ksec1_sav(21)) GOTO 9994

      END SELECT
   
! 2.1.4 Controlli sul parametro
      SELECT CASE (kfin)
      CASE(1)                                          ! U
        IF (ksec1(1)/=2 .OR. ksec1(6)/=33) GOTO 9993
      CASE(2)                                          ! V
        IF (ksec1(1)/=2 .OR. ksec1(6)/=34) GOTO 9993
      CASE(3)                                          ! Rad. Vis.
        IF (ksec1(1)/=2 .OR. ksec1(6)/=111) GOTO 9993
      CASE(4)                                          ! Albedo
        IF (ksec1(1)/=2 .OR. ksec1(6)/=84) GOTO 9993
      CASE(5)                                          ! Rad. IR
        IF (ksec1(1)/=2 .OR. ksec1(6)/=112) GOTO 9993
      CASE(6)                                          ! T
        IF (ksec1(1)/=2 .OR. ksec1(6)/=11) GOTO 9993
      CASE(7)                                          ! T2m
        IF (ksec1(1)/=2 .OR. ksec1(6)/=11) GOTO 9993
      CASE(8)                                          ! TCC
        IF (ksec1(1)/=2 .OR. ksec1(6)/=71) GOTO 9993
      END SELECT      
   
! 2.1.5 Salvo il campo in par_in
      IF (kfin == 6) THEN
        par_in(kfin,1:np) = MAX(field(1:np),par_in(kfin,1:np))
        tlog(kl) = field(klog)
      ELSE
        par_in(kfin,1:np) = field(1:np)
      ENDIF

    ENDDO                                       ! livelli input
  ENDDO                                         ! files di input

! 2.1.6  Scrivo log
  WRITE (95,*)
  WRITE (95,'(a,5i3,a,4i3)') "Elaboro istante ",ksec1_sav(10:14), &
    " scad ",ksec1_sav(15:18)
  WRITE (95,'(a,6(2x,f8.3))') "Input: U,V,SWB,Alb,LWB,TCC   ", &
    par_in(1:5,klog),par_in(8,klog)
  WRITE (95,'(a,f6.2,5x,30(2x,f6.2))') "T2m, Tlay ", &
    par_in(7,klog)-273.15,tlog(1:nlevs)-273.15

! 2.1.7 Se ci sono dati mancanti, passo all'istante successivo
  IF (ANY(par_in(1:3,1:np) == rmis) .OR. &
      ((lbow.OR.ltur) .AND. ANY(par_in(4,1:np) == rmis)) .OR. &
      (lreu .AND. ANY(par_in(5,1:np) == rmis)) .OR. &
      ((lbow.OR.linv) .AND. ANY(par_in(6:7,1:np) == rmis)) .OR. &
      (ltur .AND. ANY(par_in(8,1:np) == rmis))) THEN
    WRITE (*,'(a,5i3,a,4i3,a)') "Istante: ",ksec1_sav(10:14), &
      " scad ",ksec1_sav(15:18),"   skippo per dati mancanti"
    WRITE (95,*) "Ci sono dati mancanti, passo all'istante successivo"
    CYCLE
  ENDIF

!--------------------------------------------------------------------------
! 2.2) Calcoli

  ff(1:np) = SQRT(par_in(1,1:np)*par_in(1,1:np)+par_in(2,1:np)*par_in(2,1:np))

! 2.2.1 Calcolo classe di stabilita', metodo Reuter 1970

  IF (lreu) THEN

    cl_rad(1,:) = 0
    DO kc = 1,7
      WHERE (par_in(3,1:np)+par_in(5,1:np) > reu_class_rad(kc-1) .AND. &
             par_in(3,1:np)+par_in(5,1:np) <= reu_class_rad(kc))
        cl_rad(1,1:np) = kc
      ENDWHERE
    ENDDO

    cl_ff(1,:) = 0
    DO kc = 1,6
      WHERE (ff(1:np) > reu_class_ff(kc-1) .AND. &
             ff(1:np) <= reu_class_ff(kc))
        cl_ff(1,1:np) = kc
      ENDWHERE
    ENDDO

    DO k = 1,np
      ipgt(1,k) = reu_ipgt(cl_rad(1,k),cl_ff(1,k))
    ENDDO

  ENDIF

! 2.2.2 Calcolo classe di stabilita' diurna, meteodi Bowen 1983 e Turner 1964. 
!       In questa sezione: definisco la maschere giorno/notte (day_time),
!       assegno il valore di cl_ff; nei punti in cui e' giorno, calcolo 
!       anche cl_rad e ipgt.

  IF (lbow .OR. ltur) THEN

!   Calcolo SW_down
    WHERE (par_in(4,1:np) >= 0. .AND. par_in(4,1:np) < 100.)
      sw_down(1:np) = par_in(3,1:np) / (1. - 0.01*par_in(4,1:np))
    ELSEWHERE
      sw_down(1:np) = 0.
    ENDWHERE

    WHERE (sw_down(1:np) > 5.)
      day_time(1:np) = .TRUE.
    ELSEWHERE
      day_time(1:np) = .FALSE.
    ENDWHERE

!   Calcolo classe di radiazione (diurna)
    cl_rad(2:3,:) = 0
    DO kc = 1,4
      WHERE (sw_down(1:np) > bow_class_rad_day(kc-1) .AND. &
             sw_down(1:np) <= bow_class_rad_day(kc) .AND. day_time(1:np))
        cl_rad(2,1:np) = kc
        cl_rad(3,1:np) = kc
      ENDWHERE
    ENDDO

!   Calcolo classe di vento (diurna e notturna)
    cl_ff(2:3,:) = 0
    DO kc = 1,5
      WHERE (ff(1:np) > bow_class_ff_day(kc-1) .AND. &
             ff(1:np) <= bow_class_ff_day(kc) .AND. day_time(1:np))
        cl_ff(2,1:np) = kc
        cl_ff(3,1:np) = kc
      ENDWHERE
    ENDDO
    DO kc = 1,3
      WHERE (ff(1:np) > bow_class_ff_night(kc-1) .AND. &
             ff(1:np) <= bow_class_ff_night(kc) .AND. .NOT. day_time(1:np))
        cl_ff(2,1:np) = kc
        cl_ff(3,1:np) = kc
      ENDWHERE
    ENDDO

!   Calcolo classe di stabilita' (diurna)
    DO k = 1,np
      IF (day_time(k)) THEN
        ipgt(2,k) = bow_ipgt_day(cl_rad(2,k),cl_ff(2,k))
        ipgt(3,k) = bow_ipgt_day(cl_rad(2,k),cl_ff(2,k))
      ENDIF
    ENDDO

  ENDIF

! 2.2.3 Calcolo inversione termica (metodo Bowen 1983 o richiesta in output)
  IF (lbow .OR. linv) THEN
    inversion(1:np) = par_in(6,1:np) - par_in(7,1:np)
  ENDIF

! 2.2.4 Calcolo classe di stabilita' notturna, metodo Bowen 1983
  IF (lbow) THEN
    WHERE (.NOT. day_time(1:np))
      WHERE (inversion(1:np) > soglia_inv)
        cl_rad(2,1:np) = 1
      ELSEWHERE
        cl_rad(2,1:np) = 2
      ENDWHERE
    ENDWHERE
    DO k = 1,np
      IF(.NOT. day_time(k)) &
        ipgt(2,k) = bow_ipgt_night(cl_rad(2,k),cl_ff(2,k))
    ENDDO
  ENDIF

! 2.2.5 Calcolo classe di stabilita' notturna, metodo Turner 1964
  IF (ltur) THEN
    WHERE (.NOT. day_time(1:np))
      WHERE (par_in(8,1:np) < 40.)
        cl_rad(3,1:np) = 1
      ELSEWHERE
        cl_rad(3,1:np) = 2
      ENDWHERE
    ENDWHERE

    DO k = 1,np
      IF(.NOT. day_time(k)) &
        ipgt(3,k) = bow_ipgt_night(cl_rad(3,k),cl_ff(3,k))
    ENDDO
  ENDIF

! 2.2.6 Log e controlli su calcolo IPGT
  WRITE (95,'(a,3(2x,f8.3),2x,l2,2x,3i2,2x,3i2)') &
    "Derivati: ff,sw_down,net rad,day,cl_rad,cl_ff ", &
    ff(klog),sw_down(klog),par_in(3,klog)+par_in(5,klog),day_time(klog), &
    cl_rad(1:3,klog),cl_ff(1:3,klog)
  WRITE (95,'(a,3i3,2x,f7.3)') "Output: Reu,Bow,Tur,Inv ", &
    ipgt(1:3,klog),inversion(klog)

  IF (lreu .AND. (ANY(cl_rad(1,1:np)==0) .OR. ANY(cl_ff(1,1:np)==0) .OR. &
      ANY(ipgt(1,1:np)==0))) GOTO 9992
  IF (lbow .AND. (ANY(cl_rad(2,1:np)==0) .OR. ANY(cl_ff(2,1:np)==0) .OR. &
      ANY(ipgt(2,1:np)==0))) GOTO 9991
  IF (ltur .AND. (ANY(cl_rad(3,1:np)==0) .OR. ANY(cl_ff(3,1:np)==0) .OR. &
      ANY(ipgt(3,1:np)==0))) GOTO 9990

!--------------------------------------------------------------------------
! 2.3) Scrittura output

  ksec1(1) = 2                                      ! tabella
  ksec1(6) = 202                                    ! parametro
  ksec1(7:9) = (/1,0,0/)                            ! livello
  ksec1(10:18) = ksec1_sav(10:18)                   ! data e scadenza
  ksec2(:) = ksec2_sav(:)                           ! griglia
  ksec4(2) = 10                                     ! nbit

  IF (lreu) THEN
    field(1:np) = ipgt(1,1:np)
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iu_out(1),kbuffer,ksec0(1),kret)
  ENDIF

  IF (lbow) THEN
    field(1:np) = ipgt(2,1:np)
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iu_out(2),kbuffer,ksec0(1),kret)
  ENDIF

  IF (ltur) THEN
    field(1:np) = ipgt(3,1:np)
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iu_out(3),kbuffer,ksec0(1),kret)
  ENDIF

  IF (linv) THEN
    ksec1(6) = 203
    field(1:np) = inversion(1:np)
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iu_out(4),kbuffer,ksec0(1),kret)
  ENDIF

  WRITE (*,'(a,5i3,a,4i3,a)') "Istante: ",ksec1_sav(10:14), &
    " scad ",ksec1_sav(15:18),"   elaborato"
  
ENDDO ist

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE(*,*) "Errore aprendo ",TRIM(file_in(kfin))," kret ",kret
STOP

9998 CONTINUE
WRITE(*,*) "Il file ",TRIM(file_in(kfin))," contiene meno istanti"
STOP

9997 CONTINUE
WRITE(*,*) "Area diversa in ",TRIM(file_in(kfin))," istante ",kist
IF (ksec4(1) /= np) WRITE (*,'(a,i10,a,i10)') &
  "ksec4(1) attesto ",np," trovato ",ksec4(1)
DO k = 1,1024
  IF (ksec2_sav(k) /= ksec2(k)) WRITE (*,'(a,i4,2(a,i10))') &
    "ksec2(",k,") attesto ",ksec2_sav(k)," trovato ",ksec2(k)
ENDDO
STOP

9996 CONTINUE
WRITE(*,'(2a,4i4)') "Scadenza non gestita in ",TRIM(file_in(kfin)), &
  ksec1(15:18)
STOP

9995 CONTINUE
WRITE(*,'(2a,4i4,a,i6)') "Scadenza inattesa in ",TRIM(file_in(kfin)), &
  ksec1(15:18)," istante ",kist
STOP

9994 CONTINUE
WRITE(*,'(3a,i5)') "Livello/data/scadenza disallineata in ", &
  TRIM(file_in(kfin))," istante ",kist
WRITE(*,'(a,3i4,4x,6i3,4x,4i3,2x,2a)') "Primo campo: ", &
  ksec1_sav(7:9),ksec1_sav(10:14),ksec1_sav(21),ksec1_sav(15:18),&
  " file: ",TRIM(file_in(1))
WRITE(*,'(a,3i4,4x,6i3,4x,4i3,2x,2a)') "Trovato:     ", &
  ksec1(7:9),ksec1(10:14),ksec1(21),ksec1(15:18), &
  " file: ",TRIM(file_in(kfin))
IF (kfin == 6) WRITE (*,'(3a,i3,a)') "*** Verificare che il file ", &
  TRIM(file_in(kfin))," contenga esattamente ",nlevs," livelli ***"
STOP

9993 CONTINUE
WRITE(*,'(3a,3i4)') "Parametro errato in ",TRIM(file_in(kfin)), &
  " file,tab,var ",kfin,ksec1(1),ksec1(6)
STOP

9992 CONTINUE
WRITE (*,*) "Errore attribuzione classi, metodo Reuter, istante ",kist
STOP

9991 CONTINUE
WRITE (*,*) "Errore attribuzione classi, metodo Bowen, istante ",kist
STOP

9990 CONTINUE
WRITE (*,*) "Errore attribuzione classi, metodo Turner, istante ",kist
STOP


END PROGRAM calc_ipgt_inv_multi

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: calc_ipgt_inv.exe [-reu] [-bow] [-tur] [-inv] [-lev nlev] [-h]" 
WRITE (*,*) "-reu: calcolo secondo Reuter 1970 (vento, rad.netta)"
WRITE (*,*) "-bow: calcolo secondo Bowen 1983  (vento, rad.solare, inv.termica)"
WRITE (*,*) "-tur: calcolo secondo Turner 1964 (vento, rad.solare, nuvolosita')"
WRITE (*,*) "-inv: calcolo inversione termica come MAX(Tlay)-T02m"
WRITE (*,*) "-lev nlev: numero di livelli contenuto nel file prf_t.grb. Obbligatorio con"
WRITE (*,*) "   opzioni -bow e -inv. Valori consigliati: 6 con 35 layers, 9 con 40 layers"
WRITE (*,*)
WRITE (*,*) "Nomi files di input:"
WRITE (*,*) "1) vento u (obbligatorio):      10_u.grb"
WRITE (*,*) "2) vento v (obbligatorio):      10_v.grb"
WRITE (*,*) "3) rad.visibile (obbligatorio): srf_sosbs.grb (relativi ora precedente)"
WRITE (*,*) "4) albedo (Bowen e Turner):     srf_alb.grb"
WRITE (*,*) "5) rad.IR (Reuter):             srf_thsbs.grb (relativi ora precedente)"
WRITE (*,*) "6) temperatura (Bowen,invers.): prf_t.grb"
WRITE (*,*) "7) Temp. a 2 m (Bowen,invers.): 02_t.grb"
WRITE (*,*) "8) nuvolsita (Turner)           srf_tcc.grb"
RETURN

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

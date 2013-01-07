PROGRAM calc_ustar_mo
!--------------------------------------------------------------------------
! Programma che calcola U* e MO a partire dall'output di LM
! Legge t,td/q,ps,umfl,vmfl,shf da files grib separati
! Scrive 2 files grib: ustar.grb e mo.grb
!
! Note:
! Tutti i grib devono essere definiti sulla stessa area.
! Tutti i campi di input devono avere la stessa data e scadenza 
!   (istantanea) i.e. umfl, vmfl e shf devono essere stati decumulati.
! Il programma calcola la T virtuale usando l'umidita' specifica, invece
!   del mixing ratio come sarebbe piu' corretto (le differenze sono del
!   tutto trascurabili)
!
!                                           Versione 3.2, Enrico 28/03/2012
!--------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

! Costanti fisiche
REAL, PARAMETER :: g = 9.8             ! Accelerazione di gravita' [m/s]
REAL, PARAMETER :: karm = 0.4          ! Costante di Von Karman
REAL, PARAMETER :: cp = 1004.67        ! Calore specifico aria [J/(K.Kg)]

! Altri parametri costanti (user modification)
REAL, PARAMETER :: rmis = -9999.       ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 300000  ! dimensione massima dei GRIB
REAL, PARAMETER :: momax = 1000.       ! valore max per ABS(MO)
REAL, PARAMETER :: usmin = 0.01        ! valore min per U* nel calcolo MO
INTEGER :: kptst = 22544               ! indice punto per log (S.P.Cap)

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1_sav(1024),ksec2_sav(1024)
INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2(512),psec3(2)
REAL :: field(maxdim)

! Altre variabili del programma
TYPE (date) :: data_dum,datac
REAL, ALLOCATABLE :: par_in(:,:),q(:),ro(:),tetav(:),upwp(:),vpwp(:)
REAL, ALLOCATABLE :: ustar(:),mo(:)
REAL :: fave1,fave2
INTEGER :: iuin(6),iuout(2),np,kfin,kist,nok1,nok2,hh_dum,hhc,ier,k
CHARACTER (LEN=80) :: filein(6),fileout(2)
CHARACTER (LEN=2) :: hum

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
DO kfin = 1,6
  CALL getarg(kfin,filein(kfin))
ENDDO

IF (ANY(filein(1:6) == "") .OR. TRIM(filein(1)) == "-h") THEN
  WRITE (*,*) "Uso: calc_ustar_mo.exe [-h] filein(6)" 
  WRITE (*,*) "Ordine dei files di input: T, Td/Q, Ps, umfl, vmfl, shf"
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
DO kfin = 1,6
  CALL PBOPEN (iuin(kfin),filein(kfin),'R',kret)
  IF (kret /= 0) GOTO 9999
ENDDO

fileout(1) = "ustar.grb"
fileout(2) = "mo.grb"

CALL PBOPEN (iuout(1),fileout(1),'W',kret)
CALL PBOPEN (iuout(2),fileout(2),'W',kret)
OPEN (UNIT=96,FILE="ustar_mo_ave.log",STATUS="REPLACE",FORM="FORMATTED")
OPEN (UNIT=97,FILE="ustar_mo_pts.log",STATUS="REPLACE",FORM="FORMATTED")
WRITE (96,*)
WRITE (97,*)

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura (ciclo sugli istanti)

ist: DO kist = 1,HUGE(kist)

!--------------------------------------------------------------------------
! 2.1) Lettura e controlli (ciclo sui file di input)

  DO kfin = 1,6

!   Leggo e decodifico i grib di input
    CALL PBGRIB(iuin(kfin),kbuffer,maxdim*4,klen,kret)
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

!   Se e' il primo grib, alloco le variabili e salvo sezione 2 
    IF (kfin == 1 .AND. kist == 1) THEN
      ksec2_sav(:) = ksec2(:)
      np = ksec4(1)
      ALLOCATE (par_in(6,np),q(np),ro(np),tetav(np),upwp(np),vpwp(np))
      ALLOCATE (ustar(np),mo(np))

      IF (kptst > np) THEN
        WRITE (*,*) "Punto test ",kptst," fuori dal grib, uso il punto 1"
        kptst = 1
      ENDIF
    ENDIF

!   Controlli su scadenza e area
    IF (ANY(ksec2(:) /= ksec2_sav(:)) .OR. ksec4(1) /= np) GOTO 9997

    IF (kfin == 1) THEN
      ksec1_sav(:) = ksec1(:)
      IF (ANY(ksec1(15:18)/=(/1,ksec1(16),0,0/)) .AND. &   ! LM
          ANY(ksec1(15:18)/=(/1,0,0,13/)) ) &              ! LAMA
        GOTO 9996
    ELSE
      IF (ANY(ksec1(10:18) /= ksec1_sav(10:18)) .OR. &
        ksec1(21) /= ksec1_sav(21)) GOTO 9995
    ENDIF

!   Controlli sul parametro
    SELECT CASE(kfin)
    CASE(1)                                          ! T
      IF (ksec1(1) /= 2 .OR. ksec1(6) /= 11) GOTO 9994
    CASE(2)                                          ! Q/Td
      IF (ksec1(1) == 2 .AND. ksec1(6) == 17) THEN
        hum = "td"
      ELSE IF (ksec1(1) == 2 .AND. ksec1(6) == 51) THEN
        hum = "qq"
      ELSE
        GOTO 9994
      ENDIF
    CASE(3)                                          ! P
      IF (ksec1(1) /= 2 .OR. ksec1(6) /= 1) GOTO 9994
    CASE(4)                                          ! UMFL
      IF (ksec1(1) /= 2 .OR. ksec1(6) /= 124) GOTO 9994
    CASE(5)                                          ! VMFL
      IF (ksec1(1) /= 2 .OR. ksec1(6) /= 125) GOTO 9994
    CASE(6)                                          ! SHF
      IF (ksec1(1) /= 2 .OR. ksec1(6) /= 122) GOTO 9994
    END SELECT      

!   Salvo il campo in par_in
    par_in(kfin,1:np) = field(1:np)

  ENDDO

!--------------------------------------------------------------------------
! 2.2) Calcoli

! Calcolo densita' aria
  IF (hum == "td") CALL td2q(par_in(1:6,1:np),np,rmis,q)
  par_in(2,1:np) = q(1:np)
  CALL airden(par_in(1:6,1:np),np,rmis,ro)
  CALL tpq2tetav(par_in(1:6,1:np),np,rmis,tetav)

! Calcolo USTAR
  WHERE (par_in(4,1:np) /= rmis .AND. par_in(5,1:np) /= rmis .AND. &
         ro(1:np) /= rmis)
    upwp(1:np) = - par_in(4,1:np) / ro(1:np)
    vpwp(1:np) = - par_in(5,1:np) / ro(1:np)
    ustar(1:np) = (upwp(1:np)*upwp(1:np) + vpwp(1:np)*vpwp(1:np)) ** 0.25

  ELSEWHERE
    ustar(1:np) = rmis

  ENDWHERE

! Calcolo Monin-Obukov
  WHERE (ro(1:np) /= rmis .AND. tetav(1:np) /= rmis .AND. &
         ustar(1:np) /= rmis .AND. par_in(6,1:np) /= rmis)
    WHERE (par_in(6,1:np) /= 0)
      mo(1:np) = cp * ro(1:np) * tetav(1:np) * (MAX(ustar(1:np),usmin)**3) / &
        (karm * g * par_in(6,1:np))
    ELSEWHERE
      mo(1:np) = momax
    ENDWHERE
    WHERE(mo(1:np) < -momax) mo(1:np) = -momax
    WHERE(mo(1:np) > momax) mo(1:np) = momax

  ELSEWHERE
    mo(1:np) = rmis

  ENDWHERE
!--------------------------------------------------------------------------
! 2.3) Scrittura output

! Scrivo grib USTAR
  ksec1(1) = 2                                      ! tabella
  IF (ANY(ustar(1:np) == rmis)) ksec1(5) = 192      ! forzo bitmap
  ksec1(6) = 201                                    ! parametro
  ksec1(7:9) = (/1,0,0/)                            ! livello superficie
  ksec1(10:18) = ksec1_sav(10:18)                   ! data e scadenza
  ksec2(:) = ksec2_sav(:)                           ! griglia
  psec3(2) = rmis                                   ! codice dati mancanti
  ksec4(2) = 16                                     ! nbit
  field(1:np) = ustar(1:np)

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout(1),kbuffer,ksec0(1),kret)
  
! Scrivo grib Monin-Obukov
  ksec1(1) = 2                                      ! tabella
  IF (ANY(mo(1:np) == rmis)) ksec1(5) = 192         ! forzo bitmap
  ksec1(6) = 200                                    ! parametro
  ksec1(7:9) = (/1,0,0/)                            ! livello superficie
  ksec1(10:18) = ksec1_sav(10:18)                   ! data e scadenza
  ksec2(:) = ksec2_sav(:)                           ! griglia
  psec3(2) = rmis                                   ! codice dati mancanti
  ksec4(2) = 24                                     ! nbit
  field(1:np) = mo(1:np)

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout(2),kbuffer,ksec0(1),kret)
  
! Scrivo log
  nok1 = COUNT(ustar(1:np) /= rmis)
  IF (nok1 > 0) THEN
    fave1 = SUM(ustar(1:np), MASK = ustar(1:np)/=rmis) / REAL(nok1)
  ELSE
    fave1 = rmis
  ENDIF

  nok2 = COUNT(mo(1:np) /= rmis)
  IF (nok2 > 0) THEN
    fave2 = SUM(mo(1:np), MASK = mo(1:np)/=rmis) / REAL(nok2)
  ELSE
    fave2 = rmis
  ENDIF

  datac%dd = ksec1_sav(12)
  datac%mm = ksec1_sav(11)
  datac%yy = ksec1_sav(10) + 100 * (ksec1_sav(21) - 1)
  hhc = ksec1_sav(13)
  WRITE (96,'(i4.4,3i2.2,3x, a,2e12.4,1x,2i7)') &
    datac%yy,datac%mm,datac%dd,hhc," ustar,mo,nok ",fave1,fave2,nok1,nok2
  WRITE (97,'(i4.4,3i2.2,3x, a,1x,f6.2,e10.3,f8.0,3f8.3,3x, a,f6.3,f6.1,f6.2,f10.3)') &
    datac%yy,datac%mm,datac%dd,hhc, &
    "T,Q,P,umf,vmf,shf: ",par_in(1:6,kptst), &
    "ro,tetav,u*,mo ",ro(kptst),tetav(kptst),ustar(kptst),mo(kptst)

ENDDO ist

!--------------------------------------------------------------------------
! 3) Conclusione

DO kfin = 1,5
  CALL PBCLOSE (iuin(kfin),kret)
ENDDO
CALL PBCLOSE (iuout,kret)
CLOSE (96)
CLOSE (97)
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9999 CONTINUE
WRITE(*,*) "Errore aprendo ",TRIM(filein(kfin))," kret ",kret
STOP

9998 CONTINUE
WRITE(*,*) "Il file ",TRIM(filein(kfin))," contiene meno istanti"
STOP

9997 CONTINUE
WRITE(*,*) "Area diversa in ",TRIM(filein(kfin))," istante ",kist
IF (ksec4(   1) /= np) WRITE (*,'(a,i10,a,i10)') &
  "ksec4(1) attesto ",np," trovato ",ksec4(1)
DO k = 1,1024
  IF (ksec2_sav(k) /= ksec2(k)) WRITE (*,'(a,i4,2(a,i10))') &
    "ksec2(",k,") attesto ",ksec2_sav(k)," trovato ",ksec2(k)
ENDDO

STOP

9996 CONTINUE
WRITE(*,'(2a,4i4)') "Scadenza non gestita in ",TRIM(filein(kfin)), &
  ksec1(15:18)
STOP

9995 CONTINUE
WRITE(*,*) "Data/scadenza disallineata in ",TRIM(filein(kfin)), &
  " istante ",kist
WRITE(*,'(a,6i5,5x,4i4)') "Richiesta: ",ksec1_sav(21),ksec1_sav(10:18)
WRITE(*,'(a,6i5,5x,4i4)') "Trovata:   ",ksec1(21),ksec1(10:18)
STOP

9994 CONTINUE
WRITE(*,'(3a,3i4)') "Parametro errato in ",TRIM(filein(kfin)), &
  " file,tab,var ",kfin,ksec1(1),ksec1(6)
STOP

END PROGRAM calc_ustar_mo

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE td2q(par_in,np,rmis,q)
!
! Calcola Q a partire da P e Td
! Unita' di misura: P -> Pa;  Td -> K;  Q -> kg/kg
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: par_in(6,np),rmis
INTEGER, INTENT(IN) :: np
REAL, INTENT(OUT) :: q(np)
!
REAL, PARAMETER :: eps = 0.622
REAL, PARAMETER :: abz = 273.16
!
REAL :: tc,pp,esattd
INTEGER :: k

!--------------------------------------------------------------------------

DO k = 1,np

  IF (par_in(2,k) <= 0. .OR. par_in(3,k) <= 0. .OR. &
      par_in(2,k) == rmis .OR. par_in(3,k) == rmis) THEN
    q(k) = rmis
  ELSE
    tc = par_in(2,k) - abz
    pp = par_in(3,k) / 100.
    esattd = 6.1078 * EXP((17.2693882*tc) / (tc+237.3))
    q(k) = eps * esattd / (pp - (1-eps)*esattd)
  ENDIF
 
ENDDO

RETURN
END SUBROUTINE td2q

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE tpq2tetav(par_in,np,rmis,tetav)
!
! Calcola la temperatura potenziale virtuale a partire da T, P e Q
! Unita' di misura: P -> Pa;  T -> K; Q -> kg/kg; Tetav -> K
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: par_in(6,np),rmis
INTEGER, INTENT(IN) :: np
REAL, INTENT(OUT) :: tetav(np)
!
REAL, PARAMETER :: pp0 = 100000.     ! Reference pressure [Pa]
REAL, PARAMETER :: r = 287.          ! Costante gas perfetti [J/(K.Kg)]
REAL, PARAMETER :: cp = 1004.67      ! Calore specifico aria [J/(K.Kg)]
!
REAL :: tt,qq,pp,teta
INTEGER :: k

!--------------------------------------------------------------------------

DO k = 1,np

  IF (par_in(1,k) <= 0. .OR. par_in(1,k) == rmis .OR. &
      par_in(2,k) <= 0. .OR. par_in(2,k) == rmis .OR. &
      par_in(3,k) <= 0. .OR. par_in(3,k) == rmis) THEN
    tetav(k) = rmis
  ELSE
    tt = par_in(1,k)
    qq = par_in(2,k)
    pp = par_in(3,k)
    teta = tt * ((pp0/pp)**(r/cp))
    tetav(k) = (1 + 0.61 * qq) * teta
  ENDIF
 
ENDDO

RETURN
END SUBROUTINE tpq2tetav

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE airden(par_in,np,rmis,ro)
!
! Calcola la densita' dell'aria a partire da T,P,Q
! Unita' di misura: P -> Pa;  T -> K;  Q -> kg/kg;  Ro -> kg/m3
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: par_in(6,np),rmis
INTEGER, INTENT(IN) :: np
REAL, INTENT(OUT) :: ro(np)
!
REAL, PARAMETER :: r0 = 287.
!
REAL :: tt,pp,qq,tvir
INTEGER :: k
!--------------------------------------------------------------------------

DO k = 1,np

  IF (par_in(1,k) <= 0. .OR. par_in(2,k) < 0. .OR. par_in(3,k) <= 0. .OR. &
      par_in(1,k)==rmis .OR. par_in(2,k)==rmis .OR. par_in(3,k)==rmis) &
      THEN
    ro(k) = rmis
  ELSE
    tt = par_in(1,k)
    qq = par_in(2,k)
    pp = par_in(3,k)
    tvir = (1 + 0.61 * qq) * tt
    ro(k) = pp / (r0 * tvir)
  ENDIF
 
ENDDO

RETURN
END SUBROUTINE airden

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE ksec1_valid(ksec1,data,hh,ier)
!--------------------------------------------------------------------------
! Data la sezione 1 di un GRIB, ritorna la data e l'ora di validita'
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: ksec1(*)
TYPE(date), INTENT(OUT) :: data
INTEGER, INTENT(OUT) :: hh,ier
!
TYPE(date) :: datar
INTEGER :: hhr,hh_tot,sca

!--------------------------------------------------------------------------

datar%dd = ksec1(12)
datar%mm = ksec1(11)
datar%yy = ksec1(10) + 100 * (ksec1(21) - 1)
hhr = ksec1(13)

! Gestione time range & time unit
SELECT CASE(ksec1(18))
CASE(0)
  IF (ksec1(16) == 0) THEN      ! unintialised analysis
    sca=0
  ELSE                          ! forecast
    IF (ksec1(15) == 1) THEN
      sca = ksec1(16)
    ELSE
      GOTO 9999
    ENDIF
  ENDIF

CASE(1)                         ! initialised analysis
  sca = 0

CASE(2:5)                       ! prodotto riferito a un intervallo
  IF (ksec1(15) == 1) THEN
    sca = ksec1(17)
  ELSE
    GOTO 9999
  ENDIF

CASE(13)                        ! analisi LAMA
  IF (ksec1(15) == 1 .AND. ksec1(16) == 0) THEN
    sca = 0
  ELSE
    GOTO 9999
  ENDIF

CASE DEFAULT                    ! time range non gestio
  GOTO 9998

END SELECT

! Calcolo data-ora di validita'
hh_tot = hhr + sca
data = datar + (hh_tot/24)
hh = MOD(hh_tot,24)
ier = 0

RETURN

! Messaggi d'errore
9999 CONTINUE
WRITE (*,*) "Errore ksec1_valid: time unit indicator non gestito ", &
  ksec1(15)
ier = 1
RETURN

9998 CONTINUE
WRITE (*,*) "Errore ksec1_valid: time range indicator non gestito ", &
  ksec1(18)
ier = 2
RETURN

END SUBROUTINE ksec1_valid

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

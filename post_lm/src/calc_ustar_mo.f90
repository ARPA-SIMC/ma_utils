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
!                                         Versione 4.0.0, Enrico 05/02/2025
!--------------------------------------------------------------------------

USE grib_api
USE missing_values
USE grib2_utilities
IMPLICIT NONE

! Costanti fisiche
REAL, PARAMETER :: g = 9.8             ! Accelerazione di gravita' [m/s]
REAL, PARAMETER :: karm = 0.4          ! Costante di Von Karman
REAL, PARAMETER :: cp = 1004.67        ! Calore specifico aria [J/(K.Kg)]

REAL, PARAMETER :: usmin = 0.01        ! valore min per U* nel calcolo MO
REAL, PARAMETER :: momax = 1000.       ! valore max per ABS(MO)

! Codifica dei parametri in input
INTEGER, PARAMETER :: &
!                 T   Td    P UMFL VMFL  SHF 
  rqt2v(6) = (/   2,   2,   2,   2,   2,   2/), &
  rqiop(6) = (/  11,  17,   1, 124, 125, 122/), &

  rqd(6)   = (/   0,   0,   0,   0,   0,   0/), &
  rqpc(6)  = (/   0,   0,   3,   2,   2,   0/), &
  rqpn(6)  = (/   0,   6,   0,  17,  18,  11/)

! Sostituire, per usare Q invece di Td
! rqiop(6) = (/  11,  51,   1, 124, 125, 122/), &   

! Variabili locali
REAL, ALLOCATABLE :: par_in(:,:),q(:),ro(:),tetav(:),upwp(:),vpwp(:)
REAL, ALLOCATABLE :: ustar(:),mo(:)
REAL :: fave1,fave2
INTEGER :: ifin(6),igin(6),ifout(2),igout(2),clret(0:6)
INTEGER :: ni,nj,np,ni_sav,nj_sav,np_sav
INTEGER :: d,pc,pn,t2v,iop,gnov,nom,nocv,en,en_sav
INTEGER :: kfin,kfout,kist,nok1,nok2,hh_dum,hhc,ier,iret,k
CHARACTER (LEN=200) :: filein(6),fileout(2)
CHARACTER(LEN=40) :: gt
CHARACTER (LEN=2) :: hum
LOGICAL :: cl_grid,cl_time,cl_vtime,cl_lev,cl_var,lverbose,lforce

!--------------------------------------------------------------------------
! Parametri da riga comando

DO kfin = 1,6
  CALL getarg(kfin,filein(kfin))
ENDDO

IF (ANY(filein(1:6) == "") .OR. TRIM(filein(1)) == "-h") THEN
  WRITE (*,*) "Uso: calc_ustar_mo.exe [-h] filein(6)" 
  WRITE (*,*) "Ordine dei files di input: T, Td, Ps, umfl, vmfl, shf"
  STOP 1
ENDIF

hum="td"
! hum="qq"  ! cambiare anche rqiop e 
fileout(1) = "ustar.grb"
fileout(2) = "mo.grb"
lverbose = .FALSE.
cl_grid = .TRUE.
cl_time = .TRUE.
cl_vtime = .TRUE.
cl_lev = .FALSE.
cl_var = .FALSE.

! Apro i files
ifin(:) = 0
igin(:) = 0
DO kfin = 1,6
  CALL grib_open_file(ifin(kfin),filein(kfin),"r",iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9999
ENDDO

ifout(:) = 0
igout(:) = 0
DO kfout = 1,2
  CALL grib_open_file(ifout(kfout),fileout(kfout),"w",iret)
ENDDO

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura (ciclo sugli istanti)

ist: DO kist = 1,HUGE(kist)

! 2.1) Lettura e controlli (ciclo sui file di input)

  DO kfin = 1,6
    CALL grib_new_from_file(ifin(kfin),igin(kfin),iret)
    IF (iret == GRIB_END_OF_FILE) EXIT ist
    IF (iret /= GRIB_SUCCESS) GOTO 9998
  ENDDO

  DO kfin = 2,6
    CALL check_consistency(igin(1),igin(kfin), &
      cl_grid,cl_time,cl_vtime,cl_lev,cl_var,lverbose,clret,ier)
    IF (ier /= 0) GOTO 9997

  ENDDO

! 2.2) Alloco gli array; controllo che il numero di punti non cambi
  CALL grib_get(igin(1),"gridType",gt)
  IF (gt == "regular_ll" .OR. gt == "rotated_ll") THEN
    CALL grib_get(igin(1),"numberOfPointsAlongAParallel",ni)
    CALL grib_get(igin(1),"numberOfPointsAlongAMeridian",nj)
    np = ni*nj
  ELSE IF (gt == "unstructured_grid") THEN
    CALL grib_get(igin(1),"numberOfDataPoints",np)
    ni = imiss
    nj = imiss
  ELSE
    CALL grib_get(igin(1),"Ni",ni)
    CALL grib_get(igin(1),"Nj",nj)
    np = ni*nj
  ENDIF

  IF (kist == 1) THEN
    ALLOCATE (par_in(6,np),q(np),ro(np),tetav(np),upwp(np),vpwp(np))
    ALLOCATE (ustar(np),mo(np))
    ni_sav = ni
    nj_sav = nj
    np_sav = np
  ELSE IF (ni /= ni_sav .OR. nj /= nj_sav .OR. np /= np_sav) THEN
    GOTO 9996
  ENDIF

! 2.3) Controllo sui parametri; leggo i dati
  DO kfin = 1,6

    CALL grib_get(igin(kfin),"editionNumber",en)         ! grib edition
    IF (kist == 1 .AND. kfin == 1) THEN
      en_sav = en
    ELSE IF (en /= en_sav) THEN
      GOTO 9993
    ENDIF

    IF (en == 1) THEN
      CALL grib_get(igin(kfin),"table2Version",t2v) 
      CALL grib_get(igin(kfin),"indicatorOfParameter",iop) 
      IF (t2v/=rqt2v(kfin) .OR. iop/=rqiop(kfin)) GOTO 9995
    ELSE
      CALL grib_get(igin(kfin),"discipline",d)
      CALL grib_get(igin(kfin),"parameterCategory",pc)
      CALL grib_get(igin(kfin),"parameterNumber",pn)
      IF (d/=rqd(kfin) .OR. pc/=rqpc(kfin) .OR. pn/=rqpn(kfin)) GOTO 9994
    ENDIF  
  
    CALL grib_get(igin(kfin),"getNumberOfValues",gnov)    ! totale di punti nel grib
    CALL grib_get(igin(kfin),"numberOfMissing",nom)       ! n.ro dati mancanti
    CALL grib_get(igin(kfin),"numberOfCodedValues",nocv)  ! n.ro dati validi
    IF (nocv == 0) THEN
      par_in(kfin,:) = rmiss
    ELSE
      CALL grib_set(igin(kfin),"missingValue",rmiss)
      CALL grib_get(igin(kfin),"values",par_in(kfin,:))
    ENDIF
    IF (nom + nocv /= gnov .OR. &
      (nocv /= 0 .AND. nocv /= COUNT(par_in(kfin,:) /= rmiss))) GOTO 9994

  ENDDO

! 2.4) Calcolo densita' aria
  IF (hum == "td") CALL td2q(par_in(1:6,1:np),np,rmiss,q)
  par_in(2,1:np) = q(1:np)
  CALL airden(par_in(1:6,1:np),np,rmiss,ro)
  CALL tpq2tetav(par_in(1:6,1:np),np,rmiss,tetav)

! 2.5) Calcolo USTAR
  WHERE (par_in(4,1:np) /= rmiss .AND. par_in(5,1:np) /= rmiss .AND. &
         ro(1:np) /= rmiss)
    upwp(1:np) = - par_in(4,1:np) / ro(1:np)
    vpwp(1:np) = - par_in(5,1:np) / ro(1:np)
    ustar(1:np) = (upwp(1:np)*upwp(1:np) + vpwp(1:np)*vpwp(1:np)) ** 0.25

  ELSEWHERE
    ustar(1:np) = rmiss

  ENDWHERE

! 2.6) Calcolo Monin-Obukov
  WHERE (ro(1:np) /= rmiss .AND. tetav(1:np) /= rmiss .AND. &
         ustar(1:np) /= rmiss .AND. par_in(6,1:np) /= rmiss)
    WHERE (par_in(6,1:np) /= 0)
      mo(1:np) = cp * ro(1:np) * tetav(1:np) * (MAX(ustar(1:np),usmin)**3) / &
        (karm * g * par_in(6,1:np))
    ELSEWHERE
      mo(1:np) = momax
    ENDWHERE
    WHERE(mo(1:np) < -momax) mo(1:np) = -momax
    WHERE(mo(1:np) > momax) mo(1:np) = momax

  ELSEWHERE
    mo(1:np) = rmiss

  ENDWHERE

! 2.7) Scrivo Ustar
  CALL grib_clone(igin(1),igout(1))
  IF (en_sav == 1) THEN
    CALL grib_set(igout(1),"table2Version",2) 
    CALL grib_set(igout(1),"indicatorOfParameter",201) 
  ELSE
    CALL grib_set(igout(1),"discipline",0)
    CALL grib_set(igout(1),"parameterCategory",2)
    CALL grib_set(igout(1),"parameterNumber",201)
  ENDIF

  IF (.NOT. ALL(c_e(ustar(:)))) THEN
    IF (en_sav == 1) THEN
      CALL grib_set(igout(1),"bitmapPresent",1)
    ELSE IF (en_sav == 2) THEN
      CALL grib_set(igout(1),"bitMapIndicator",0)
    ENDIF
    CALL grib_set(igout(1),"missingValue",rmiss)
  ENDIF
  CALL grib_set(igout(1),"values",ustar(:))
  CALL grib_write(igout(1),ifout(1))

! 2.8) Scrivo Obukov
  CALL grib_clone(igin(1),igout(2))
  IF (en_sav == 1) THEN
    CALL grib_set(igout(2),"table2Version",2) 
    CALL grib_set(igout(2),"indicatorOfParameter",200) 
  ELSE
    CALL grib_set(igout(2),"discipline",0)
    CALL grib_set(igout(2),"parameterCategory",2)
    CALL grib_set(igout(2),"parameterNumber",200)
  ENDIF

  IF (.NOT. ALL(c_e(mo(:)))) THEN
    IF (en_sav == 1) THEN
      CALL grib_set(igout(2),"bitmapPresent",1)
    ELSE IF (en_sav == 2) THEN
      CALL grib_set(igout(2),"bitMapIndicator",0)
    ENDIF
    CALL grib_set(igout(2),"missingValue",rmiss)
  ENDIF
  CALL grib_set(igout(2),"values",mo(:))
  CALL grib_write(igout(2),ifout(2))

! 2.9) Scrivo log
  nok1 = COUNT(ustar(1:np) /= rmiss)
  IF (nok1 > 0) THEN
    fave1 = SUM(ustar(1:np), MASK = ustar(1:np)/=rmiss) / REAL(nok1)
  ELSE
    fave1 = rmiss
  ENDIF

  nok2 = COUNT(mo(1:np) /= rmiss)
  IF (nok2 > 0) THEN
    fave2 = SUM(mo(1:np), MASK = mo(1:np)/=rmiss) / REAL(nok2)
  ELSE
    fave2 = rmiss
  ENDIF

  WRITE (96,'(a,2e12.4,1x,2i7)') &
    " ustar,mo,nok ",fave1,fave2,nok1,nok2

!  datac%dd = ksec1_sav(12)
!  datac%mm = ksec1_sav(11)
!  datac%yy = ksec1_sav(10) + 100 * (ksec1_sav(21) - 1)
!  hhc = ksec1_sav(13)
!  WRITE (96,'(i4.4,3i2.2,3x, a,2e12.4,1x,2i7)') &
!    datac%yy,datac%mm,datac%dd,hhc," ustar,mo,nok ",fave1,fave2,nok1,nok2
!  WRITE (97,'(i4.4,3i2.2,3x, a,1x,f6.2,e10.3,f8.0,3f8.3,3x, a,f6.3,f6.1,f6.2,f10.3)') &
!    datac%yy,datac%mm,datac%dd,hhc, &
!    "T,Q,P,umf,vmf,shf: ",par_in(1:6,kptst), &
!    "ro,tetav,u*,mo ",ro(kptst),tetav(kptst),ustar(kptst),mo(kptst)

! 2.10) Libero memoria
  DO kfin = 1,6
    CALL grib_release(igin(kfin))
  ENDDO
  DO kfout = 1,2
    CALL grib_release(igin(kfout))
  ENDDO

ENDDO ist

STOP

!--------------------------------------------------------------------------

9999 CONTINUE
WRITE (*,*)  "File non trovato ",TRIM(filein(kfin))
STOP 2

9998 CONTINUE
WRITE (*,*)  "Errore leggendo ",TRIM(filein(kfin))," campo ",kist
STOP 2

9997 CONTINUE
WRITE (*,*)  "Grib inconsistenti, campo ",kist
DO k = 0,5
  IF (clret(k) == -1) WRITE (*,*) cllab(k),": test non eseguito"
  IF (clret(k) == 0)  WRITE (*,*) cllab(k),": test superato"
  IF (clret(k) == 1)  WRITE (*,*) cllab(k),": test non passato"
ENDDO
STOP 3

9996 CONTINUE
WRITE (*,*) "Numero di punti diverso nell'istante ",kist," mi fermo"
WRITE (*,*) "Trovati (ni,nj,np) ",ni,nj,np
WRITE (*,*) "Attesi  (ni,nj,np) ",ni_sav,nj_sav,np_sav
STOP 4

9995 CONTINUE
WRITE(*,'(3a,3i4)') "Parametro errato in ",TRIM(filein(kfin)), &
  " file,en,tab,var ",kfin,en,t2v,iop
STOP 5

9994 CONTINUE
WRITE(*,'(3a,3i4)') "Parametro errato in ",TRIM(filein(kfin)), &
  " file,en,dis,cat,num ",kfin,en,d,pc,pn
STOP 5

9993 CONTINUE
WRITE (*,*) "grib con edition diversa nell'istante ",kist," mi fermo"

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


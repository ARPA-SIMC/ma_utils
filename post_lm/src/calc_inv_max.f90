PROGRAM calc_inv_max
!--------------------------------------------------------------------------
! Legge un file con campi di T 3d e calcola, per ciacun punto e ciscun
! istante, il valore massimo dell'inversione termica.
! Metodo: 
!
! I grib in ingrsso devono essere riferiti alla stessa area, essere 
! raggruppati  per data/scad, ed essere ordinati per livello (dal basso)
! E'ammessa la presenza di livelli diversi in istanti diversi.
!
!                                           Versione 2.0, Enrico 03/12/2008
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 1000000     ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxlev = 20          ! n.ro max. livelli

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec2_sav(1024)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
REAL, ALLOCATABLE :: temp(:,:),inv(:)
INTEGER :: data_scad(9),data_scadc(9),lev(1:3),levp(1:3),par(3),np
INTEGER :: iuin1,iuin2,iuout,k,kp,idp,cntl,cntt,cntg,cntsk
CHARACTER (LEN=80) :: file_t2m,file_t3d,fileout,charg,next_arg
LOGICAL :: levok1,levok2,lt2m,ksec2_diff

!==========================================================================
! 1) Preliminari

! 1.1 Parametri da riga comando
lt2m=.FALSE.
next_arg = ""
idp = 1
DO kp = 1,HUGE(0)
  CALL getarg(kp,charg)

  IF (charg == "") THEN
    EXIT
  ELSE IF (TRIM(charg) == "-h") THEN
    WRITE (*,*) "Uso: calc_inv_max.exe [-h] [-t2m file_t2m] file_t3d fileout" 
    STOP
  ELSE IF (TRIM(charg) == "-t2m") THEN
    lt2m=.TRUE.
    next_arg="t2"
  ELSE IF (next_arg == "t2") THEN
    file_t2m = charg
    next_arg=""
  ELSE IF (idp == 1) THEN
    file_t3d = charg
    idp = idp + 1
  ELSE IF (idp == 2) THEN
    fileout = charg
    idp = idp + 1
  ENDIF
ENDDO

IF (idp < 2) THEN
  WRITE (*,*) "Uso: calc_inv_max.exe [-h] [-t2m file_t2m] file_t3d fileout" 
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin1,file_t3d,'R',kret)
IF (kret /= 0) GOTO 9999
IF (lt2m) THEN
  CALL PBOPEN (iuin2,file_t2m,'R',kret)
  IF (kret /= 0) GOTO 9998
ENDIF

CALL PBOPEN (iuout,fileout,'W',kret)
OPEN (UNIT = 90, FILE ="calc_inv_max.log", STATUS= "REPLACE")
WRITE (90,'(a)') "Nlev      Data                Scad"

!==========================================================================
! 2) Cicli: data_scad, livello

cntg = 0
cntsk = 0
levok1 = .TRUE.
levok2 = .TRUE.
grib: DO k = 1,HUGE(0)

!--------------------------------------------------------------------------
! 2.1 Leggo il prossimo grib dal file 3D

  CALL PBGRIB(iuin1,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
  data_scad(1:9) = ksec1(10:18)
  par(1:3) = (/ksec1(2),ksec1(1),ksec1(6)/)
  lev(1:3) = ksec1(7:9)

!--------------------------------------------------------------------------
! 2.2) Controlli

! Se e' il 1o grib, alloco le variabili, altrimenti controllo che l'area
! non sia cambiata

  IF (k == 1) THEN
    ksec2_sav(:) = ksec2(:)
    np = ksec4(1)
    IF (lt2m) THEN
      ALLOCATE (temp(np,0:maxlev),inv(np))
    ELSE
      ALLOCATE (temp(np,maxlev),inv(np))
    ENDIF
    cntl = 0

  ELSE IF (ksec2_diff(ksec2(1:14),ksec2_sav(1:14))) THEN
    GOTO 9997

  ENDIF

! Se il grib non contiene un campo di temperatura, lo salto
  IF (ANY(par(1:3)/=(/200,2,11/))) THEN
    cntsk = cntsk + 1
    CYCLE
  ENDIF
  cntg = cntg + 1

!--------------------------------------------------------------------------
!   2.3) Se ho trovato una nuova data/scad, calcolo e scrivo l'inversione
!        massima relativa alla data/scad precedente. Se e' attiva l'opzione
!        t2m, aggiungo la T@2m come ulteriore livello (indice 0)

  IF (k == 1) THEN
    data_scadc(:) = data_scad(:)
    cntt = 1
    levp(:) = -999
    cntl = 0

  ELSE IF (ANY(data_scad(:) /= data_scadc(:))) THEN

!   Se richiesto, leggo la T@2m e la uso come livello aggiuntivo
    IF (lt2m) THEN
      CALL PBGRIB(iuin2,kbuffer,maxdim*4,klen,kret)
      IF (kret == -1) THEN
        EXIT
      ELSE IF (kret < -1) THEN
        WRITE(*,*) "Error pbgrib: kret ",kret
        STOP
      ENDIF
    
      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   field,maxdim,kbuffer,maxdim,klen,'D',kret)
      IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

!     Controllo che il grib si riferisca a una Temperatura sulla stessa
!       area e alla scadenza giusta
      IF (ANY(data_scadc(1:9) /= ksec1(10:18))) THEN
        WRITE (*,*) "Errore T2m , data-scad disallineate rispetto a T3d"
        STOP
      ENDIF
      IF (ANY(par(1:3) /= (/200,2,11/))) THEN
        WRITE (*,*) "Errore file T2m contiene par diverto da temperatura"
        STOP
      ENDIF

!     Salvo il dato come livello 0 e calcolo inversione
      temp (1:np,0) = field(1:np)
      WRITE (90,'(i4,3x,9i4)') cntl+1,data_scadc(1:9)
      CALL calc_inv(temp(1:np,0:cntl),inv,np,cntl+1,rmis)

!   Altrimenti calcolo l'inversione a partire solo dai model layers
    ELSE
      WRITE (90,'(i4,3x,9i4)') cntl,data_scadc(1:9)
      CALL calc_inv(temp(1:np,1:cntl),inv,np,cntl,rmis)

    ENDIF

!   Scrivo il campo "inversione termica" su fileout
    ksec1(1) = 2
    ksec1(2) = 200
    ksec1(6) = 11
    ksec1(7:9) = (/1,0,0/)
    ksec1(10:18) = data_scadc(1:9)
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 inv,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

    temp(:,:) = rmis
    inv(:) = rmis
    data_scadc(:) = data_scad(:)
    cntt = cntt + 1
    levp(:) = -999
    cntl = 0

  ENDIF

! Se non e' il 1o grib di questa data/scad, eseguo controlli sul livello
  cntl = cntl + 1
  IF (ANY(levp(:) /= -999)) THEN
    IF (lev(1) /= levp(1)) levok1 = .FALSE.
    IF (lev(1) == 110 .AND. levp(1) == 110) THEN
      IF (lev(2) >= levp(2)) GOTO 9996
    ELSE
      levok2 = .FALSE.
    ENDIF
  ENDIF

! Salvo l'ultimo campo letto nell'array 3d temp
  temp(1:np,cntl) = field(1:np)    
  levp(:) = lev(:)

ENDDO grib

!--------------------------------------------------------------------------
! 2.4 Calcolo e scrivo l'inversione relativa all'ultimo istante

IF (lt2m) THEN
  CALL PBGRIB(iuin2,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    WRITE(*,*) "Errore: campo non trovato (T2m, ultimo istante) "
    STOP
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF
 
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! Controllo che il grib si riferisca a una Temperatura sulla stessa
!   area e alla scadenza giusta
  IF (ANY(data_scadc(1:9) /= ksec1(10:18))) THEN
    WRITE (*,*) "Errore T2m , data-scad disallineate rispetto a T3d"
    STOP
  ENDIF
  IF (ANY(par(1:3) /= (/200,2,11/))) THEN
    WRITE (*,*) "Errore file T2m contiene par diverto da temperatura"
    STOP
  ENDIF

! Salvo il dato come livello 0 e calcolo inversione
  temp (1:np,0) = field(1:np)
  WRITE (90,'(i4,3x,9i4)') cntl+1,data_scadc(1:9)
  CALL calc_inv(temp(1:np,0:cntl),inv,np,cntl+1,rmis)

ELSE
  WRITE (90,'(i4,3x,9i4)') cntl,data_scadc(1:9)
  CALL calc_inv(temp(1:np,1:cntl),inv,np,cntl,rmis)

ENDIF

ksec1(1) = 2
ksec1(2) = 200
ksec1(6) = 11
ksec1(7:9) = (/1,0,0/)
ksec1(10:18) = data_scadc(1:9)
CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             inv,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

!--------------------------------------------------------------------------
! 3) Chiudo i files e termino

IF (.NOT. levok1) &
  WRITE (*,*) "Warning: sono presenti livelli di tipo diverso"
IF (.NOT. levok2) &
  WRITE (*,*) "Warning: trovati tipi di livello non gestiti"

WRITE (*,*) "Calc_invb_max: elaborazioni terminate, elaborati ",cntt," istanti"
WRITE (*,*) "Grib elaborati: ",cntg," saltati ",cntsk

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_t3d)
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_t2m)
STOP

9997 CONTINUE
WRITE (*,*) "Grib definito su area diversa in ",TRIM(file_t3d)," : ",k
STOP

9996 CONTINUE
WRITE (*,*) "Errore, trovati model levels non ordinati dal basso"
STOP

END PROGRAM calc_inv_max

!==========================================================================

SUBROUTINE calc_inv(temp,inv,np,nl,rmis)
IMPLICIT NONE
!--------------------------------------------------------------------------
! Dato un campo 3d di Temperatura (livelli ordinati dal basso), calcola per
! ciascun punto il valore massimo dell'inversione termica.
!
! Metodo: scorro i livelli dal basso; per ciascuno, cerco la temperatura 
! piu' altra tra i livelli sovrastanti, e salvo il max della differenza
!
! Note:
! L'array inv contiene valori positivi se c'e' qualche inversione, negativi
! se la temperatura diminuisce in modo monotono con la quota. 
! In ogni caso, i valori negativi non sono molto significativi (dpendono
! molto dalla risoluzione verticale...)
!--------------------------------------------------------------------------
INTEGER, INTENT(IN) :: np,nl
REAL, INTENT(IN) :: temp(np,nl),rmis
REAL, INTENT(OUT) :: inv(np)

REAL :: inv2(np)
INTEGER :: kl

!--------------------------------------------------------------------------

! Controlli preliminari
IF (np < 1) THEN
  WRITE (*,*) "Errore, numero punti = 0"
  STOP
ENDIF

IF (nl < 2) THEN
  WRITE (*,*) "Calcolo inversione impossibilie con ",nl," livelli"
  inv(:) = rmis
  RETURN
ENDIF

IF (ANY(temp(:,:) == rmis)) THEN
  WRITE (*,*) "Ci sono dati mancanti di temperatura, metto mancante l'inversione"
  inv(:) = rmis
  RETURN
ENDIF

! Calcolo inversione massima
inv(1:np) = MAXVAL(temp(1:np,2:nl),DIM=2) - temp(1:np,1)
DO kl = 2,nl-1
  inv2(1:np) = MAXVAL(temp(1:np,kl+1:nl),DIM=2) - temp(1:np,kl)
  inv(1:np) = MERGE(inv(:), inv2(:), inv(:) > inv2(:))
ENDDO

RETURN
END SUBROUTINE calc_inv

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION ksec2_diff(ksec2a,ksec2b) RESULT(is_diff)
!
! Controlla se due array ksec2 scritti da Gribex corrispondono alla stessa 
! griglia
!
IMPLICIT NONE
LOGICAL :: is_diff
INTEGER, INTENT(IN) :: ksec2a(14),ksec2b(14)

IF (ANY(ksec2a((/1,2,3,4,5,6,7,8,11/)) /= ksec2b((/1,2,3,4,5,6,7,8,11/)))) THEN
  is_diff = .TRUE.

ELSE IF (ksec2a(6) == 128 .AND. &
  (ksec2a(9) /= ksec2b(9) .OR. ksec2a(10) /= ksec2b(10))) THEN
  is_diff = .TRUE.

ELSE IF (ksec2a(1) == 10 .AND. &
  (ksec2a(13) /= ksec2b(13) .OR. ksec2a(14) /= ksec2b(14))) THEN
  is_diff = .TRUE.

ELSE 
  is_diff = .FALSE.

ENDIF

END FUNCTION ksec2_diff

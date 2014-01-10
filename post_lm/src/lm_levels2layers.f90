PROGRAM rw_grib
!--------------------------------------------------------------------------
! Legge un file grib con le quote dei levels LM e ne scrive uno con le 
! quote centrali dei layers (i.e. la media tra il level inf e sup.).
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 300000      ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxlev = 100         ! max n.ro di liveli LM

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
REAL :: zlev(maxdim,maxlev),zlay(maxdim,maxlev)
INTEGER :: iuin,iuout,kl,nlev,np,ksec2f(1024),idlev(maxlev)
CHARACTER (LEN=80) :: filein,fileout
LOGICAL :: ksec2_diff

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)

IF (filein == "" .OR. fileout == "" .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: lm_levels2layers.exe [-h] filein fileout"
  STOP
ENDIF

! 1.2 Inizializzazioni
CALL grsvck(0)
zlev(:,:) = rmis
zlay(:,:) = rmis

!--------------------------------------------------------------------------
! 2) Leggo i levels

CALL PBOPEN (iuin,filein,'R',kret)

DO kl = 1,maxlev

  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
  IF (kl == 1) THEN
    ksec2f(:) = ksec2(:)
    np = ksec4(1)
  ELSE
    IF (ksec2_diff(ksec2(1:14),ksec2f(1:14)) .OR. ksec4(1) /= np) THEN
      WRITE (*,*) "Errore, esistono grib con area diversa"
      STOP
    ELSE IF (ksec1(6) /= 8 .OR. ksec1(7) /= 109) THEN
      WRITE (*,*) "Errore, parametro o tipo livello errato"
      WRITE (*,*) "ksec1(6): ",ksec1(6)," (8); ksec1(7): ",ksec1(7)," (109)"
      STOP
    ENDIF
  ENDIF

  zlev(1:np,kl) = field(1:np)
  idlev(kl) = ksec1(8)

ENDDO

IF (kl > maxlev) THEN
  WRITE (*,*) "Errore troppi lilvelli"
  STOP
ELSE
  nlev = kl-1
  WRITE (*,*) "Letti levels: ",nlev
ENDIF

!--------------------------------------------------------------------------
! 3) Scrivo i layers

CALL PBOPEN (iuout,fileout,'W',kret)

DO kl = 1,nlev-1

  field(1:np) = (zlev(1:np,kl) + zlev(1:np,kl+1)) /2.
  ksec1(7) = 110
  ksec1(8) = MIN(idlev(kl),idlev(kl+1))
  ksec1(9) = MAX(idlev(kl),idlev(kl+1))

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret


ENDDO
!--------------------------------------------------------------------------
! 3) Chiudo i files e termino

CALL PBCLOSE (iuin,kret)
CALL PBCLOSE (iuout,kret)

WRITE (*,*) "Scritti layers: ",nlev-1

STOP

END PROGRAM rw_grib

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

PROGRAM grib_ave
!--------------------------------------------------------------------------
! Programma che legge un file con molti grib e scrive un solo grib con il
! campo medio.
!
! Note:
! - I grib devono essere definiti sulla stessa area (sez. 2 coincidente)
! - Le altre sezioni sono prese dal primo grib del file
!
!                                           Versione 2.0, Enrico 08/06/2009
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -1.E30           ! valore per dati mancanti
!REAL, PARAMETER :: rmis = -HUGE(0.)       ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 500000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1_sav(1024),ksec2_sav(1024),ksec3_sav(2),ksec4_sav(512)
INTEGER :: kbuffer(maxdim),klen,kret,field_nok(maxdim)
REAL    :: psec2(512),psec3(2),psec2_sav(512),psec3_sav(2)
REAL    :: field(maxdim),field_sum(maxdim),field_frok(maxdim)

! Altre variabili del programma
REAL :: fave,fave_sum,fract_req
INTEGER :: ngribin,iuin,iuout,iuout2,nok,np_sav,data_sav(5),idp,nreq,ios,kp,nskip
CHARACTER (LEN=80) :: filein,fileout,fileout2,chdum
CHARACTER (LEN=3) :: next_arg
LOGICAL :: first

!--------------------------------------------------------------------------
! 1) Preliminari

fileout2 = "grib_ave_fok.grb"

! 1.1 Parametri da riga comando
idp = 0
next_arg = ""
fract_req = 0.
filein = ""
fileout = ""
ios = 0

DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "-req") THEN
    next_arg = "fok"
  ELSE IF (next_arg == "fok") THEN
    READ (chdum,*,IOSTAT=ios) fract_req
    next_arg = ""
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

IF (filein == "" .OR. fileout == "" .OR. ios /= 0 .OR. &
    fract_req < 0. .OR. fract_req > 1.) THEN
  CALL write_help
  STOP
ENDIF  

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
IF (kret /= 0) THEN 
  WRITE(*,*) "Errore aprendo ",filein," kret ",kret
  STOP
ENDIF

OPEN (UNIT=96, FILE="grib_ave.log", STATUS="REPLACE", ACTION="WRITE")

!--------------------------------------------------------------------------
! 2) Lettura (ciclo sui grib)

field_sum(:) = 0.
field_nok(:) = 0
fave_sum = 0.
first = .TRUE.
ngribin = 0
nskip = 0

grib: DO

! Leggo il grib
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT grib
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

! Lo decodifico
  psec3(2) = rmis                !impongo di mettere i dati mancanti = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! Controllo che area e n.ro di punti siano giusti, e che i dati non siano 
! tutti mancanti
  IF (first) THEN
    first = .FALSE.
    ksec1_sav(:) = ksec1(:)
    ksec2_sav(:) = ksec2(:)
    ksec3_sav(:) = ksec3(:)
    ksec4_sav(:) = ksec4(:)
    psec2_sav(:) = psec2(:)
    psec3_sav(:) = psec3(:)
    np_sav = ksec4(1) 
  ELSE IF (ANY( ksec2(:) /= ksec2_sav(:) ) .OR. ksec4(1) /= np_sav) THEN
    WRITE (*,*) "Il grib ",ngribin," e' definito su un'area diversa, skippo"
    CYCLE grib
  ENDIF  

  nok = COUNT(field(1:np_sav) /= rmis)
  IF (nok > 0) THEN
    ngribin = ngribin +1
  ELSE
    nskip = nskip + 1
    CYCLE grib
  ENDIF

! Calcolo e memorizzo la media del campo
  fave = SUM(field(1:np_sav), MASK = field(1:np_sav)/=rmis) / REAL(nok)
  fave_sum = fave_sum + fave
  WRITE (96,'(a,i6,a,i4,i7,3(1x,e12.5))') &
    "Grib ",ngribin," Par,nok,ave,max,min ",ksec1(6),nok,fave, &
    MAXVAL(field(1:np_sav), MASK = field(1:np_sav)/=rmis), &
    MINVAL(field(1:np_sav), MASK = field(1:np_sav)/=rmis)

! Aggiorno il campo somma
  WHERE (field(1:np_sav) /= rmis)
    field_sum(1:np_sav) = field_sum(1:np_sav) + field(1:np_sav)
    field_nok(1:np_sav) = field_nok(1:np_sav) + 1
  ENDWHERE

ENDDO grib

CALL PBCLOSE (iuin,kret)

!--------------------------------------------------------------------------
! 3) Calcolo la media e la scrivo
nreq = MAX(NINT(fract_req * ngribin),1)
WHERE (field_nok(1:np_sav) >= nreq)
  field_sum(1:np_sav) = field_sum(1:np_sav) / REAL(field_nok(1:np_sav))
ELSEWHERE
  field_sum(1:np_sav) = rmis
ENDWHERE

field_frok(1:np_sav) = REAL(field_nok(1:np_sav)) / REAL(ngribin)
fave_sum = fave_sum / REAL(ngribin)

! Confronto la media del campo medio con la media delle medie dei campi
nok = COUNT(field_sum(1:np_sav) /= rmis)
IF (nok > 0) THEN
  fave = SUM(field_sum(1:np_sav), MASK = field_sum(1:np_sav)/=rmis) / &
    REAL(nok)
ELSE
  fave = rmis
ENDIF

WRITE (*,*)
WRITE (*,*) "Campi interamente mancanti:  ",nskip
WRITE (*,*) "Grib con dati buoni:         ",ngribin
WRITE (*,*) "Punti in un grib:            ",np_sav
WRITE (*,*) "Media delle medie dei campi: ",fave_sum
WRITE (*,*) "Media del campo medio:       ",fave
WRITE (*,*) "Max,Min,Nok del campo medio: ", &
  MAXVAL(field_sum(1:np_sav), MASK = field_sum(1:np_sav)/=rmis), &
  MINVAL(field_sum(1:np_sav), MASK = field_sum(1:np_sav)/=rmis), &
  COUNT(field_sum(1:np_sav)/=rmis)

! Definisco gli headers del grib con i valori del primo campo
ksec1(:) = ksec1_sav(:)
ksec2(:) = ksec2_sav(:)
ksec3(:) = ksec3_sav(:)
ksec4(:) = ksec4_sav(:)
psec2(:) = psec2_sav(:)
psec3(:) = psec3_sav(:)

! Se ci sono dati mancanti forzo la scrittura della bitmap
IF (nok /= np_sav) THEN
  psec3(2) = rmis
  IF (ksec1(5) == 0 .OR. ksec1(5) == 64) THEN
    ksec1(5) = 64
  ELSE 
    ksec1(5) = 192
  ENDIF
ENDIF

! Scrivo il grib medio
CALL GRIBEX (ksec0,ksec1,ksec2_sav,psec2,ksec3,psec3,ksec4, &
             field_sum,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

CALL PBOPEN (iuout,fileout,'W',kret)
CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
CALL PBCLOSE (iuout,kret)

! Scrivo il grib frazione dati validi
psec3(2) = rmis
CALL GRIBEX (ksec0,ksec1,ksec2_sav,psec2,ksec3,psec3,ksec4, &
             field_frok,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

CALL PBOPEN (iuout2,fileout2,'W',kret)
CALL PBWRITE (iuout2,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
CALL PBCLOSE (iuout2,kret)

CLOSE (96)

END PROGRAM grib_ave

!--------------------------------------------------------------------------

SUBROUTINE write_help

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib_ave.exe [-h] filein fileout [-req fraction]" 
WRITE (*,*)
WRITE (*,*) "Scrive su fileout il campo medio dei grib contenuti in filein, e su "
WRITE (*,*) "grib_ave_fok.grb le frazione di dati validi."
WRITE (*,*)
WRITE (*,*) "-req: mette mancanti i punti in cui la frazione dei dati validi"
WRITE (*,*) "      e' < fraction (default: solo i punti senza dati validi)"

END SUBROUTINE write_help

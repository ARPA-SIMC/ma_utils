PROGRAM grib_stat
!--------------------------------------------------------------------------
! Programma che legge un file con molti grib e scrive in 4 files separati
! i campi: massimo, minimo, media, varianza
!
! Note:
! - I grib devono essere definiti sulla stessa area (sez. 2 coincidente)
! - Le altre sezioni sono prese dal primo grib del file
! - I punti che risultano mancanti in un qualsiasi GRIB sono messi
!   mancanti; e' ammesso che ci sia un diverso numero di dati mancanti nei
!   diversi grib
! - Deriva da grib_ave.f90
!
!                                         Versione 1.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -1.E30           ! valore per dati mancanti
!REAL, PARAMETER :: rmis = -HUGE(0.)       ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec1_sav(1024),ksec2_sav(1024),ksec3_sav(2),ksec4_sav(512)
INTEGER :: kbuffer(maxdim), klen, kret
INTEGER :: field_nok(maxdim)
REAL    :: psec2(512),psec3(2),psec2_sav(512),psec3_sav(2)
REAL    :: field(maxdim),field_sum(maxdim),field_sum2(maxdim)
REAL    :: field_max(maxdim),field_min(maxdim)

! Altre variabili del programma
REAL :: fave,fave_sum
INTEGER :: ngribin,iuin,iuout,nok,np_sav,data_sav(5)
CHARACTER (LEN=200) :: filein, fileout
LOGICAL :: first

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)

IF (filein == "" .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: grib_stat.exe [-h] filein"
  WRITE (*,*) "Scrive i files: ave.grb, max.grb, min.grb, var.grb"
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

field_max(:) = -HUGE(0.)
field_min(:) = HUGE(0.)
field_sum(:) = 0.
field_sum2(:) = 0.
field_nok(:) = 0
fave_sum = 0.
first = .TRUE.
ngribin = 0

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
    WRITE (*,*) "Campo interamente mancante, skippo"
    CYCLE grib
  ENDIF

! Calcolo e memorizzo la media del campo
  fave = SUM(field(1:np_sav), MASK = field(1:np_sav)/=rmis) / REAL(nok)
  fave_sum = fave_sum + fave
  WRITE (96,'(a,i6,a,i4,i7,3(1x,e12.5))') &
    "Grib ",ngribin," Par,nok,ave,max,min ",ksec1(6),nok,fave, &
    MAXVAL(field(1:np_sav), MASK = field(1:np_sav)/=rmis), &
    MINVAL(field(1:np_sav), MASK = field(1:np_sav)/=rmis)

! Aggiorno i campi relativi alle statistiche
  WHERE (field(:) /= rmis .AND. field_max(:) /= rmis)
    WHERE (field(:) > field_max(:))
      field_max(:) = field(:)
    ENDWHERE
    WHERE (field(:) < field_min(:))
      field_min(:) = field(:)
    ENDWHERE
    field_sum(:) = field_sum(:) + field(:)
    field_sum2(:) = field_sum2(:) + field(:)*field(:)
    field_nok(:) = field_nok(:) + 1

  ELSEWHERE
    field_max(:) = rmis
    field_min(:) = rmis
    field_sum(:) = rmis
    field_sum2(:) = rmis
  ENDWHERE

ENDDO grib

CALL PBCLOSE (iuin,kret)

!--------------------------------------------------------------------------
! 3) Calcolo le statistiche e le scrivo

WHERE (field_sum(:) /= rmis)
  field_sum(:) = field_sum(:)/REAL(field_nok(:))
  field_sum2(:) = field_sum2(:)/REAL(field_nok(:)) - field_sum(:)*field_sum(:)
ENDWHERE
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
WRITE (*,*) "Grib con dati buoni: ",ngribin
WRITE (*,*) "Punti in un grib:    ",np_sav
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
  IF (ksec1(5) == 0 .OR. ksec1(5) == 64) THEN
    ksec1(5) = 64
  ELSE 
    ksec1(5) = 192
  ENDIF
ENDIF

! Scrivo il grib massimo
CALL GRIBEX (ksec0,ksec1,ksec2_sav,psec2,ksec3,psec3,ksec4, &
             field_max,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

CALL PBOPEN (iuout,"max.grb",'W',kret)
CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
CALL PBCLOSE (iuout,kret)

! Scrivo il grib minimo
CALL GRIBEX (ksec0,ksec1,ksec2_sav,psec2,ksec3,psec3,ksec4, &
             field_min,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

CALL PBOPEN (iuout,"min.grb",'W',kret)
CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
CALL PBCLOSE (iuout,kret)

! Scrivo il grib medio
CALL GRIBEX (ksec0,ksec1,ksec2_sav,psec2,ksec3,psec3,ksec4, &
             field_sum,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

CALL PBOPEN (iuout,"ave.grb",'W',kret)
CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
CALL PBCLOSE (iuout,kret)

! Scrivo il grib varianza
CALL GRIBEX (ksec0,ksec1,ksec2_sav,psec2,ksec3,psec3,ksec4, &
             field_sum2,maxdim,kbuffer,maxdim,klen,'C',kret)
IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

CALL PBOPEN (iuout,"var.grb",'W',kret)
CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
CALL PBCLOSE (iuout,kret)

CLOSE (96)

END PROGRAM grib_stat

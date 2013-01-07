PROGRAM post_zi_diagmet
!--------------------------------------------------------------------------
! Legge un file con molti grib Zi scritti da chimere2grib.exe e li riscrive
! con le modifiche necessaria per l'importazione nei dataset LAMA*.
!
!                                           Versione 1.0, Enrico 01/09/2006
!--------------------------------------------------------------------------

USE date_handler

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 1000000     ! dimensione massima dei GRIB
TYPE(date), PARAMETER :: data_igen30_ini = date(27,02,2004)
TYPE(date), PARAMETER :: data_igen30_end = date(19,07,2004)
TYPE(date), PARAMETER :: data_lama3_ini = date(26,01,2006)

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del progra+mma
TYPE(date) :: datac
INTEGER :: iuin,iuout,cnt,np
CHARACTER (LEN=80) :: filein,fileout

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)

IF (filein == "" .OR. fileout == "" .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: post_zi_diagmet filein fileout"
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
CALL PBOPEN (iuout,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2) Leggo / Scrivo (ciclo sui grib)

DO cnt = 1,HUGE(cnt)

! 2.1 Leggo il grib 
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

! 2.2 Lo modifico
  datac = date(ksec1(12), ksec1(11), ksec1(10)+100*(ksec1(21)-1))

! Codice modello archivio LAMA (30 o 32)
  IF (datac >= data_igen30_ini .AND. datac <= data_igen30_end) THEN
    ksec1(3) = 30
  ELSE
    ksec1(3) = 32
  ENDIF

! Time range: analisi LAMA istantanee
  ksec1(15:18) = (/1,0,0,13/)

! Sub centre identifier (???)
  ksec1(22) = 255

! Proiezione: geografica ruotata
  ksec2(1) = 10
  ksec2(13) = NINT((57.5-90.) * 1000.)
  ksec2(14) = NINT(10. * 1000.)

! Direction increments not given
  ksec2(6) = 0
  ksec2(9) = 0
  ksec2(10) = 0

! Vertical coordinate parameters (35 o 40)
  IF (datac < data_lama3_ini) THEN
    ksec2(12) = 40
    psec2(11:50) = (/100000., 288.149902343750, 42., 0.219999969006, 0.019999999553,  &
      0.039999999106, 0.064999997616, 0.092999994755, 0.122999966145, 0.153999984264, &
      0.185000002384, 0.215999960899, 0.247999966145, 0.280999958515, 0.314999997616, &
      0.351000010967, 0.388000011444, 0.425000011921, 0.462000012398, 0.499000012875, &
      0.536000013351, 0.573000013828, 0.610000014305, 0.647000014782, 0.683000028133, &
      0.717999994755, 0.751999974251, 0.783999979496, 0.813000023365, 0.838999986649, &
      0.861999988556, 0.883000016212, 0.902999997139, 0.921999990940, 0.939999997616, &
      0.955999970436, 0.970000028610, 0.981999993324, 0.991999983788, 1.000000000000/)

  ELSE
    ksec2(12) = 45
    psec2(11:55) = (/100000., 288.149902343750, 42., 0.219999969006, 0.019999999553, &
      0.039999999106, 0.060359999537, 0.081389963627, 0.103320002556, 0.126359999180, &
      0.150629997253, 0.176239967346, 0.203229963779, 0.231610000134, 0.261349976063, &
      0.292400002480, 0.324639976025, 0.357969999313, 0.392229974270, 0.427249968052, &
      0.462859988213, 0.498839974403, 0.534969985485, 0.571049988270, 0.606840014458, &
      0.642109990120, 0.676630020142, 0.710169970989, 0.742529988289, 0.773490011692, &
      0.802850008011, 0.830449998379, 0.856130003929, 0.879760026932, 0.901229977608, &
      0.920480012894, 0.937439978123, 0.952139973640, 0.964590013027, 0.974870026112, &
      0.983120024204, 0.989499986172, 0.994239985943, 0.997630000114, 1.000000000000/)
  ENDIF

! Mixing height maximum value
  np = ksec4(1)
  field(1:np) = MIN(field(1:np),2500.)

! Precisione (nbit)
  ksec4(2) = 12

! 2.3 Lo riscrivo
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

WRITE (*,*) "Letti-scritti ",cnt-1," grib"

STOP

END PROGRAM post_zi_diagmet

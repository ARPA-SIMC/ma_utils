PROGRAM dummy_grib_calmet
!--------------------------------------------------------------------------
! Programma che scrive un pacco di grib corrispondenti a 1 run di calmet,
! con tutti i dati mancanti.
! Legge da calmet.inp la data, i parametri dell'area e i livelli verticali
! NOTA: non gestisce (ancora?) l'ora del run, da' per scontato che il run
! duri 24 ore, a partire dalle 00Z del giorno richiesto.
!
! Uso: dummy_grib_calmet fileinp igen fileout
!
!                                         Versione 3.2.3, Enrico 14/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000      ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxlev = 100         ! max n.ro di livelli calmet
LOGICAL, PARAMETER :: ww = .FALSE.         ! se T scrive grib w
LOGICAL, PARAMETER :: xout = .TRUE.        ! se T scrive grib extra output

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Variabili dipendenti dal run
REAL :: dgridkm,xorigkm,yorigkm,x1,x2,y1,y2
REAL :: liv_uv(maxlev),liv_w(0:maxlev)
INTEGER :: ibyr,ibmo,ibdy
INTEGER :: nx,ny,nz

! Funzioni
CHARACTER (LEN=100) :: exinp,exdat

! Altre variabili del programma
INTEGER :: iuout,igen,cc,yy,kh,kl,ios
CHARACTER (LEN=200) :: chrec,fileout,fileinp
CHARACTER (LEN=100) :: chdat
CHARACTER (LEN=8) :: str_gen

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,fileinp)
CALL getarg(2,str_gen)
CALL getarg(3,fileout)

READ (str_gen,*,IOSTAT=ios) igen

IF (fileinp == "" .OR. fileout == "" .OR. igen == 0 .OR. &
    ios /= 0 .OR. TRIM(fileinp) == "-h") THEN
  WRITE (*,*) "Uso: dummy_grib_calmet.exe [-h] fileinp igen fileout" 
  WRITE (*,*) 
  WRITE (*,*) "Scrive su fileout un pacco di grib corrispondenti a 1 run di,"
  WRITE (*,*) "calmet, con tutti i dati mancanti."
  WRITE (*,*) "fileinp e' il calmet.inp del run richiesto"
  WRITE (*,*) 
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro il file di output
CALL PBOPEN (iuout,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2) Leggo da fileinp i parametri del run calmet

OPEN (UNIT=30, FILE=fileinp, STATUS="OLD", ACTION="READ", ERR=9999)

! 2.0.1 run title e gruppo 0a (nulla)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9998
  IF (INDEX(exinp(chrec),"END") /= 0) EXIT
ENDDO

! 2.0.2 gruppi 0b, 0c, 0d (nulla)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9998
  IF (INDEX(exinp(chrec),"END") /= 0) EXIT
ENDDO

! 2.1 gruppo 1 (data iniziale e lunghezza del run)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9998
  IF (INDEX(exinp(chrec),"END") /= 0) THEN
    EXIT

  ELSE IF (INDEX(exinp(chrec),"IBYR") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) ibyr

  ELSE IF (INDEX(exinp(chrec),"IBMO") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) ibmo

  ELSE IF (INDEX(exinp(chrec),"IBDY") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) ibdy

  ENDIF
ENDDO

! 2.2 gruppo 2 (parametri griglia e livelli)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9998
  IF (INDEX(exinp(chrec),"END") /= 0) THEN
    EXIT

  ELSE IF (INDEX(exinp(chrec),"NZ") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) nz
    IF (nz > maxlev) GOTO 9997

  ELSE IF (INDEX(exinp(chrec),"ZFACE") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) liv_w(0:nz)

  ELSE IF (INDEX(exinp(chrec),"NX") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) nx

  ELSE IF (INDEX(exinp(chrec),"NY") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) ny

  ELSE IF (INDEX(exinp(chrec),"DGRIDKM") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) dgridkm

  ELSE IF (INDEX(exinp(chrec),"XORIGKM") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) xorigkm

  ELSE IF (INDEX(exinp(chrec),"YORIGKM") /= 0) THEN
    chdat = exdat(exinp(chrec))
    READ(chdat,*,ERR=9998) yorigkm

  ENDIF
ENDDO

CLOSE(30)

! Parametri elaborati dipendenti da quelli letti
IF (ibyr < 50) THEN           ! anno a 2 cifre > 2000 (vecchio formato)
  cc = 21
  yy = ibyr
ELSE IF (ibyr < 100) THEN     ! anno a 2 cifre < 2000 (vecchio formato)
  cc = 20
  yy = ibyr
ELSE                          ! anno a 4 cifre (nuovo formato)
  cc = (ibyr-1)/100 + 1
  yy = MOD((ibyr-1),100) + 1
ENDIF

x1 = xorigkm + dgridkm / 2.
y1 = yorigkm + dgridkm / 2.
x2 = xorigkm + nx * dgridkm - dgridkm / 2.
y2 = yorigkm + ny * dgridkm - dgridkm / 2.

liv_uv(1:nz) = (liv_w(0:nz-1) + liv_w(1:nz))/2.

WRITE (*,'(a,3i4)') "dummy_grib_calmet, nx,ny,nz: ",nx,ny,nz
!WRITE (*,*) "Livelli UVT: ",NINT(liv_uv(1:nz))

!--------------------------------------------------------------------------
! 3) Fisso i dati costanti delle intestazioni grib

! sezione 1
ksec1(1) = 200
ksec1(2) = 200
ksec1(3) = igen
ksec1(4) = 255
ksec1(5) = 192
ksec1(9) = 0

ksec1(15) = 1
ksec1(16) = 0
ksec1(17) = 0
ksec1(18) = 10
ksec1(19) = 0
ksec1(20) = 0
ksec1(22:) = 0

! sezione 2
ksec2(1) = 0
ksec2(2) = nx
ksec2(3) = ny
ksec2(4) = NINT(y1*1000.)      ! 4852.500
ksec2(5) = NINT(x1*1000.)      ! 402.500
ksec2(6) = 128
ksec2(7) = NINT(y2*1000.)      ! 5107.500
ksec2(8) = NINT(x2*1000.)      ! 847.500
ksec2(9) = NINT(dgridkm*1000.)
ksec2(10) = NINT(dgridkm*1000.)
ksec2(11) = 64
ksec2(12:) = 0

psec2(:) = 0.

! sezione 3
ksec3(:) = 0
psec3(:) = rmis

! sezione 4
ksec4(1) = ksec2(2) * ksec2(3)
!ksec4(2) = 1
ksec4(2) = 24
ksec4(3:) = 0

!--------------------------------------------------------------------------
! 4) Scrivo i grib

DO kh = 0,23

  ksec1(10) = yy
  ksec1(11) = ibmo
  ksec1(12) = ibdy
  ksec1(13) = kh
  ksec1(14) = 0
  ksec1(21) = cc

  DO kl = 1,nz

!   u-wind
    ksec1(7) = 105
    ksec1(8) = NINT(liv_uv(kl))
    ksec1(6) = 33

    field(:) = rmis
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

!   v-wind
    ksec1(7) = 105
    ksec1(8) = NINT(liv_uv(kl))
    ksec1(6) = 34

    field(:) = rmis
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

!   Temperature
    ksec1(7) = 105
    ksec1(8) = NINT(liv_uv(kl))
    ksec1(6) = 11

    field(:) = rmis
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
    CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

    IF (ww) THEN
!     w-wind
      ksec1(7) = 105
      ksec1(8) = NINT(liv_w(kl))
      ksec1(6) = 40
   
      field(:) = rmis
      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                   field,maxdim,kbuffer,maxdim,klen,'C',kret)
      IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
      CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
      IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
    ENDIF

  ENDDO

! 2D fields
  ksec1(7) = 1
  ksec1(8) = 0

! ipgt
  ksec1(6) = 100
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

! ustar
  ksec1(6) = 101
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

! hmix
  ksec1(6) = 102
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

! el
  ksec1(6) = 103
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

! wstar
  ksec1(6) = 104
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

ENDDO

IF (xout) THEN
DO kh = 0,23
  ksec1(10) = yy
  ksec1(11) = ibmo
  ksec1(12) = ibdy
  ksec1(13) = kh
  ksec1(14) = 0
  ksec1(21) = cc

! SWB
  ksec1(6) = 110
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
 
! LWB
  ksec1(6) = 111
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
 
! LHF
  ksec1(6) = 112
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
 
! SHF
  ksec1(6) = 113
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
 
! CFR
  ksec1(6) = 114
  field(:) = rmis
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

ENDDO
ENDIF

!--------------------------------------------------------------------------
! 5) Chiudo il file grib e termino

CALL PBCLOSE (iuout,kret)
STOP

!--------------------------------------------------------------------------
! 6) Gestione errori I/O

9999 CONTINUE
WRITE (*,*) "dummy_grib_calmet: errore aprendo ",TRIM(fileinp)
STOP

9998 CONTINUE
WRITE (*,*) "dummy_grib_calmet: errore leggendo ",TRIM(fileinp)
WRITE (*,*) "ibyr,ibmo,ibdy", ibyr,ibmo,ibdy
WRITE (*,*) "nz,nx,ny,dX,x0,y0", nz,nx,ny,dgridkm,xorigkm,yorigkm
WRITE (*,*) "liv_w: ",liv_w(0:nz)
STOP

9997 CONTINUE
WRITE (*,*) "dummy_grib_calmet: troppi livelli, aumentare parametro maxlev"
STOP


END PROGRAM dummy_grib_calmet

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION exinp(rec) RESULT (str)
!
! Funzione che, dato un record letto da calmet.inp, ritrorna la prima 
! stringa compresa tra ! che questo contiene.
! Se il record non contine coppie di !! ritorna una stringa vuota
!
IMPLICIT NONE

CHARACTER (LEN=200) :: rec
CHARACTER (LEN=100) :: str

INTEGER :: p1,p2,pp
!
str = ""

p1 = INDEX(rec,"!")
IF (p1 == 0) RETURN

pp = INDEX(rec(p1+1:),"!")
IF (pp == 0) RETURN
p2 = pp + p1

str = rec(p1+1:p2-1)

RETURN

END FUNCTION exinp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION exdat(rec) RESULT (str)
!
! Funzione che, data una stringa (ritornata da exinp) ritorna la 
! sottostringa che segue l'ultimo carattere "="
! Se la stringa non contiene = ritorna la stringa stessa.
!
IMPLICIT NONE

CHARACTER (LEN=100) :: rec
CHARACTER (LEN=100) :: str

INTEGER :: p1
!
str = rec

p1 = INDEX(rec,"=")
IF (p1 == 0) THEN
  str = rec
  RETURN
ELSE
  str = rec(p1+1:)
ENDIF

RETURN

END FUNCTION exdat


PROGRAM grib_step_explicit
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive, scrivendo esplicitamente il 
! passo griglia nei campi che non lo contengono.
! Utile per importare in GRADS campi lat-lon con step non indicato.
!
!                                           Versione 1.0, Enrico 23/09/2010
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 1000000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
INTEGER :: iuin,iuout,cnt
CHARACTER (LEN=80) :: filein,fileout

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)

IF (filein == "" .OR. fileout == "" .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: grib_step_explicit [-h] filein fileout" 
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
  IF (ksec2(6) == 0) THEN
    ksec2(6) = 128
    ksec2(9) = NINT(REAL(ABS(ksec2(5)-ksec2(8))) / REAL(ksec2(2)-1))
    ksec2(10) = NINT(REAL(ABS(ksec2(4)-ksec2(7))) / REAL(ksec2(3)-1))
  ENDIF

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

END PROGRAM grib_step_explicit

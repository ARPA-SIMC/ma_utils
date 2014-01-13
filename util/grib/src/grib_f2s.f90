PROGRAM grib_f2s
!--------------------------------------------------------------------------
! Legge un file GRIB e lo ricodifica usando il second order packing con
! la codifica piu' semplice possibile (by ROW)
!
!                                         Versione 1.2.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
INTEGER :: iuin,iuout,k,lpack,cnt1
CHARACTER (LEN=200) :: filein,fileout,charg

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,charg)

lpack = 1
IF (TRIM(charg) == "-cw") lpack = 2
IF (TRIM(charg) == "-wmo") lpack = 3

IF (filein == "" .OR. fileout == "" .OR. TRIM(filein) == "-h" .OR. &
  filein(1:1) == "-" .OR. fileout(1:1) == "-" ) THEN
  WRITE (*,*) "Uso: grib_f2s [-h] filein fileout [-cw/-wmo]" 
  WRITE (*,*) "Legge un file GRIB e lo ricodifica attivando il 2nd order packing"
  WRITE (*,*) "  Default: row by row sop"
  WRITE (*,*) "  -cw:     constant width sop"
  WRITE (*,*) "  -wmo:    general WMO sop (apparently equal to row by row"
  STOP
ENDIF

IF (lpack == 1) THEN
  WRITE (*,*) "ROW by ROW packing"
ELSE IF (lpack == 2) THEN
  WRITE (*,*) "Constant width packing"
ELSE IF (lpack == 3) THEN
  WRITE (*,*) "General WMO packing"
ELSE
  WRITE (*,*) "Opzione non gestita"
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
CALL PBOPEN (iuout,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2) Leggo / Scrivo

cnt1 = 0
DO k = 1,HUGE(k)

! 2.1 Leggo il prossimo grib
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

! 2.2 Modifico la sezione 4 per attivare il second order packing
!     Se il grib e' codificato con pochi bit, lo lascio al primo ordine
!     (passando al sop grib con 1-4 bit ho visto differenze fino al 10%)
  IF (ksec4(2) < 6 ) THEN
    cnt1 = cnt1 + 1
  ELSE
    ksec4(3) = 0
    ksec4(4) = 64
    ksec4(6) = 16

    IF (lpack == 1) THEN         ! Row
      ksec4(9) = 16
      ksec4(10) = 16
    ELSE IF (lpack == 2) THEN    ! Constant width
      ksec4(9) = 32
      ksec4(10) = 0
      ksec4(11) = ksec4(2) - 1
    ELSE IF (lpack == 3) THEN    ! General WMO
      ksec4(9) = 32
      ksec4(10) = 16
    ENDIF
  ENDIF

! 2.3 Lo riscrivo
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret
 
  IF (MOD(k,100) == 0) WRITE (*,*) "Elaborato grib ",k
ENDDO

!--------------------------------------------------------------------------
! 3) Chiudo i files grib e termino

CALL PBCLOSE (iuin,kret)
CALL PBCLOSE (iuout,kret)

IF (cnt1 > 0) &
  WRITE (*,*) "Grib con 1-5 bit lasciati al primo ordine: ",cnt1
STOP

END PROGRAM grib_f2s


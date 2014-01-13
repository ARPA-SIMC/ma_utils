PROGRAM grib_s2f
!--------------------------------------------------------------------------
! Legge un file GRIB codificato con il second order packing (opzioni di
! default GRIBEX) e lo riscrive con la codifica normale (first order)
!
! Nota: pare che nbit si perda -> devo re-inventarlo (qui metto 16)
!
!                                         Versione 1.2.2, Enrico 13/01/2014
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
INTEGER :: iuin,iuout,k,nbits,ios,cnt1,cnt2
CHARACTER (LEN=200) :: filein,fileout,charg

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,charg)

ios = 0
IF (TRIM(charg) /= "") THEN
  READ (charg,*,IOSTAT = ios) nbits
ELSE
  nbits = 16
ENDIF

IF (filein == "" .OR. fileout == "" .OR. TRIM(filein) == "-h" .OR. &
  ios /= 0 .OR. nbits < 1 .OR. nbits > 24) THEN
  WRITE (*,*) "Uso: grib_s2f [-h] filein fileout [nbit]" 
  WRITE (*,*) "Legge un file GRIB codificato con il second order packing"
  WRITE (*,*) "  e lo riscrive con la codifica normale (first order)"
  WRITE (*,*) "nbit: n.ro di bit per riscrivere i grib 2o ordine (default 16)"
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
cnt2 = 0

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

! 2.2 Se e' un grib al secondo ordine, lo modifico
  IF (ksec4(4) == 64) THEN
    cnt2 = cnt2 + 1
    ksec4(2) = nbits
    ksec4(3:) = 0
  ELSE
    cnt1 = cnt1 + 1
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

WRITE (*,*) "Grib modificati, invariati: ",cnt2,cnt1

CALL PBCLOSE (iuin,kret)
CALL PBCLOSE (iuout,kret)
STOP

END PROGRAM grib_s2f


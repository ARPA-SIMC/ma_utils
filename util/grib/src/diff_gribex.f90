PROGRAM diff_grib
!--------------------------------------------------------------------------
! Programma che legge due files con molti grib e visualizza le differenze
! Pensato principalmente per confrontare due files che dovrebbero essere 
! uguali, ma non sono identici al bit
!                                           Versione 1.0, Enrico 05/01/2005
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -1.e20           ! valore per dati mancanti
!REAL, PARAMETER :: rmis = -HUGE(0.)       ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 1000000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0a(2),ksec1a(1024),ksec2a(1024),ksec3a(2),ksec4a(512)
INTEGER :: ksec0b(2),ksec1b(1024),ksec2b(1024),ksec3b(2),ksec4b(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2a(512),psec3a(2),psec2b(512),psec3b(2)
REAL    :: fielda(maxdim),fieldb(maxdim)

! Altre variabili del programma
INTEGER :: iua,iub,ngrib,npa,npb,noka,nokb,nok,kp
REAL :: avea,aveb,rms,mxdiff,rms_mx,mxdiff_absmx
CHARACTER (LEN=80) :: filea,fileb
CHARACTER (LEN=14) :: str_nok
CHARACTER (LEN=1) :: eq_ksec(4)
LOGICAL :: leq(4),lnok

!--------------------------------------------------------------------------
! 1) Preliminari

! Parametri da riga comando
CALL getarg(1,filea)
CALL getarg(2,fileb)

IF (filea == "" .OR. fileb == "" .OR. TRIM(filea) == "-h") THEN
  WRITE (*,*) "Uso: diff_grib.exe [-h] file1 file2" 
  STOP
ENDIF

! Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! Apro i files
CALL PBOPEN (iua,filea,'R',kret)
IF (kret /= 0) THEN 
  WRITE(*,*) "Errore aprendo ",filea," kret ",kret
  STOP
ENDIF

CALL PBOPEN (iub,fileb,'R',kret)
IF (kret /= 0) THEN 
  WRITE(*,*) "Errore aprendo ",fileb," kret ",kret
  STOP
ENDIF

!--------------------------------------------------------------------------
! 2) Elaborazioni (ciclo sui grib)

leq(:) = .TRUE.
lnok = .TRUE.
rms_mx = 0.
mxdiff_absmx = 0.

ngrib = 0
!                123456---1234567---123456-123456---1234567890-1234567890-1234567890-1234567890-(123456)
WRITE (*,'(a)') " ngrib   ksec ==     nok1   nok2         ave1       ave2        rms    mx diff    (nok)"

DO

! Leggo e decodifico un grib da filea
  CALL PBGRIB(iua,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3a(2) = rmis              !impongo di mettere i dati mancanti = rmis
  CALL GRIBEX (ksec0a,ksec1a,ksec2a,psec2a,ksec3a,psec3a,ksec4a, &
               fielda,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! Leggo e decodifico un grib da fileb
  CALL PBGRIB(iub,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3b(2) = rmis              !impongo di mettere i dati mancanti = rmis
  CALL GRIBEX (ksec0b,ksec1b,ksec2b,psec2b,ksec3b,psec3b,ksec4b, &
               fieldb,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! Confronto headers
  ngrib = ngrib + 1
  eq_ksec(1:4) = "Y"
  IF ( ANY(ksec1a(:) /= ksec1b(:)) ) eq_ksec(1) = "N"
  IF ( ANY(ksec2a(:) /= ksec2b(:)) ) eq_ksec(2) = "N"
  IF ( ANY(ksec3a(:) /= ksec3b(:)) ) eq_ksec(3) = "N"
  IF ( ANY(ksec4a(1:11) /= ksec4b(1:11)) ) eq_ksec(4) = "N"

! N.ro punti e valori medi
  npa = ksec4a(1)
  npb = ksec4b(1)
  noka = COUNT(fielda(1:npa) /= rmis)
  nokb = COUNT(fieldb(1:npb) /= rmis)

  IF (noka /= 0) THEN
    avea = SUM(fielda(1:npa), MASK = fielda(1:npa) /= rmis) / REAL(noka)
  ELSE
    avea = rmis
  ENDIF
  IF (nokb /= 0) THEN
    aveb = SUM(fieldb(1:npb), MASK = fieldb(1:npb) /= rmis) / REAL(nokb)
  ELSE
    aveb = rmis
  ENDIF

! RMS e max diff
  IF (npa == npb) THEN
    nok = 0
    rms = 0.
    mxdiff = 0.
    DO kp = 1,npa
      IF (fielda(kp) /= rmis .AND. fieldb(kp) /= rmis) THEN
        nok = nok + 1
        rms = rms + (fielda(kp)- fieldb(kp)) **2
        mxdiff = MAX(mxdiff,ABS(fielda(kp)- fieldb(kp)))
      ENDIF
    ENDDO

    IF (nok > 0) THEN
      rms = SQRT(rms / REAL(nok))
    ELSE
      rms = rmis
      mxdiff = rmis
    ENDIF

  ELSE
    rms = rmis
    mxdiff = rmis

  ENDIF

! Salvo i risultati con piu' differenze
  WHERE (eq_ksec(1:4) == "N") 
    leq(1:4) = .FALSE.
  ENDWHERE
  IF (noka /= nokb) lnok = .FALSE.
  IF (rms > rms_mx) rms_mx = rms
  IF (ABS(mxdiff) > mxdiff_absmx)  mxdiff_absmx = mxdiff

! Le visualizzo
  WRITE (*,'(i6,2x,4(1x,a1),2x,2(1x,i6),2x,4(1x,e10.3),1x,a1,i6,a1)') &
    ngrib,eq_ksec(1:4),noka,nokb,avea,aveb,rms,mxdiff,"(",nok,")"

ENDDO

WHERE (leq(1:4))
  eq_ksec(1:4) = "Y"
ELSEWHERE
  eq_ksec(1:4) = "N"
ENDWHERE
IF (lnok) THEN
  str_nok = "no_differences"
ELSE
  str_nok = "   differences"
ENDIF

WRITE (*,*)
WRITE (*,'(a)') "Caso peggiore:"
WRITE (*,'(6x,2x,4(1x,a1),2x,a14,2x,2(1x,10x),2(1x,e10.3))') &
  eq_ksec(1:4),str_nok,rms_mx,mxdiff_absmx

!--------------------------------------------------------------------------
! 3) Conclusione

CALL PBCLOSE (iua,kret)
CALL PBCLOSE (iub,kret)

STOP

END PROGRAM diff_grib

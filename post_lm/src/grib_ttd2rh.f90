PROGRAM grib_ttd2rh
!--------------------------------------------------------------------------
! Programma che legge da 2 file T e Td e scrive un file con RH.
! Usato dalla catena as2pm
!
!                                         Versione 1.1.1, Enrico 10/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0a(2),ksec1a(1024),ksec2a(1024),ksec3a(2),ksec4a(512)
INTEGER :: ksec0b(2),ksec1b(1024),ksec2b(1024),ksec3b(2),ksec4b(512)
INTEGER :: ksec0c(2),ksec1c(1024),ksec2c(1024),ksec3c(2),ksec4c(512)

INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2a(512),psec3a(2),psec2b(512),psec3b(2),psec2c(512),psec3c(2)
REAL :: tt(maxdim),td(maxdim),rh(maxdim)

! Altre variabili del programma
REAL :: ttd2rh
INTEGER :: ngrib,iuin1,iuin2,iuout,np,idp,k,kp
CHARACTER (LEN=80) :: file_tt,file_td,file_rh,chdum
LOGICAL :: ksec2_diff

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      file_tt = chdum
    CASE (2)
      file_td = chdum
    CASE (3)
      file_rh = chdum
    CASE DEFAULT
      CALL write_help
      STOP
    END SELECT
  ENDIF
ENDDO

IF (TRIM(file_tt) == "" .OR. TRIM(file_tt) == "" .OR. TRIM(file_rh) == "") THEN
  CALL write_help
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin1,file_tt,'R',kret)
IF (kret /= 0) GOTO 9999
CALL PBOPEN (iuin2,file_td,'R',kret)
IF (kret /= 0) GOTO 9998
CALL PBOPEN (iuout,file_rh,'W',kret)

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura (ciclo sui grib)

ngrib = 0
grib: DO

! 2.1) Leggo e decodifico T
  CALL PBGRIB(iuin1,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT grib
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3a(2) = rmis                                   ! dati mancanti = rmis
  CALL GRIBEX (ksec0a,ksec1a,ksec2a,psec2a,ksec3a,psec3a,ksec4a, &
               tt,maxdim,kbuffer,maxdim,klen,'D',kret)
  if (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.2) Leggo & decodifico Td
  CALL PBGRIB(iuin2,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    WRITE(*,*) "Errore, il file 2 continene meno grib del file 1"
    STOP
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3b(2) = rmis                                   ! dati mancanti = rmis
  CALL GRIBEX (ksec0b,ksec1b,ksec2b,psec2b,ksec3b,psec3b,ksec4b, &
               td,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.3) Diagnostica sulla compatibilita' dei campi

  IF (ksec2_diff(ksec2a(1:14),ksec2b(1:14)) .OR. (ksec4a(1) /= ksec4b(1))) THEN
    WRITE (*,*) "T e Td definiti su griglie diverse, skip (progr. ",ngrib + 1,")"
    DO k = 1,14 
      IF (ksec2a(k) /= ksec2b(k)) WRITE (*,'(a,i4,a,2i12)') &
        "    ksec2(",k,"): ",ksec2a(k),ksec2b(k)
    ENDDO
    CYCLE
  ELSE IF ((ksec1a(1) /= 2 .OR. ksec1a(6) /= 11) .AND. &   ! COSMO
           (ksec1a(1) /= 128 .OR. ksec1a(6) /= 167)) THEN  ! ECMWF
    WRITE (*,'(3a,i3,a)') "Campo non di temperatura in ",TRIM(file_tt), &
      " SKIP (progr.",ngrib+1,")"
    CYCLE
  ELSE IF ((ksec1b(1) /= 2 .OR. ksec1b(6) /= 17) .AND. &   ! COSMO
           (ksec1b(1) /= 128 .OR. ksec1b(6) /= 168)) THEN  ! ECMWF
    WRITE (*,'(3a,i3,a)') "Campo non di Td in ",TRIM(file_td), &
      " SKIP (progr.",ngrib+1,")"
    CYCLE
  ENDIF
  ngrib = ngrib +1

! 2.4) Calcolo RH e la scrivo
  np = ksec4a(1)
  DO k = 1,np
    IF (tt(k) /= rmis .AND. td(k) /= rmis) THEN
      rh(k) = ttd2rh(tt(k),td(k))
    ELSE
      rh(k) = rmis
    ENDIF
  ENDDO

  ksec1c(:) = ksec1a(:)
  ksec1c(1) = 2
  ksec1c(6) = 52
  ksec2c(:) = ksec2a(:)
  psec2c(:) = psec2a(:)
  ksec3c(:) = ksec3a(:)
  psec3c(2) = rmis                         ! dati mancanti = rmis
  ksec4c(:) = ksec4a(:)
  ksec4c(2) = MAX(ksec4a(2),ksec4b(2))     ! metto nibt = max tra T e Td

  CALL GRIBEX (ksec0c,ksec1c,ksec2c,psec2c,ksec3c,psec3c,ksec4c, &
               rh,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout,kbuffer,ksec0c(1),kret)
  
ENDDO grib

WRITE (*,*) "Scritti: ",ngrib," campi"

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE(*,*) "Errore aprendo ",TRIM(file_tt)
STOP

9998 CONTINUE
WRITE(*,*) "Errore aprendo ",TRIM(file_td)
STOP


END PROGRAM grib_ttd2rh

!==========================================================================

SUBROUTINE write_help

!              123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: ttd2rh.exe file_tt file_td file_rh"
WRITE (*,*) "Legge da due files T e Td, scrive su un terzo file RH"
RETURN

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION ttd2rh(tt,td) RESULT (rh)
!--------------------------------------------------------------------------
! Evaluates Reative Humidity from Temperature and Dew Point Temperature.
! tt = Air temperature (K)
! td = Dew Point Temperature (K)
! rh = Relative Humidity (%)
!--------------------------------------------------------------------------

IMPLICIT NONE
REAL, INTENT (IN) :: tt,td
REAL :: rh
REAL :: esat_tt,esat_td

REAL, PARAMETER :: abs_zero = 273.15
REAL, PARAMETER :: apos=17.269, aneg=21.874, bpos=35.86, bneg=7.66
REAL, PARAMETER :: c = 6.1078

IF (tt > abs_zero) THEN
  esat_tt = c * EXP(apos * (tt - abs_zero) / (tt - bpos))
ELSE
  esat_tt = c * EXP(aneg * (tt - abs_zero) / (tt - bneg))
ENDIF

IF (td > abs_zero) THEN
  esat_td = c * EXP(apos * (td - abs_zero) / (td - bpos))
ELSE
  esat_td = c * EXP(aneg * (td - abs_zero) / (td - bneg))
ENDIF

rh = esat_td / esat_tt * 100.
rh = MIN(MAX(rh,0.),100.)

RETURN
END FUNCTION ttd2rh

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

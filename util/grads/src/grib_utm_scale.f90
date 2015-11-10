PROGRAM grib_utm_scale
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive dividendo per 100. estremi e 
! passo di griglia, in modo da trasformare griglie UTM in griglie 
! geografiche con valori accettabili.
!
! - I campi su griglia non UTM non vengono modificati
! - Scrive nel file utm#grid.grb un campo di 0. definito sulla griglia 
!   originale del primo grib UTM
! - Se la normalizzazione porta a un passo griglia non intero, i grib in 
!   uscita avranno "direction increments not given"
!
! Note:
! Programma chiamato da grb2grads con opzione -scale. Scritto per aggirare
! i problemi che le nuove versioni di GRADS hanno nela gestione delle 
! griglie UTM ad alta risoluzone (se si compiono operazioni tra files 
! diversi spesso dice che le griglie sono incompatibili)
! 
!                                         Versione 2.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.         ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 1000000   ! dimensione massima dei GRIB
REAL, PARAMETER :: eps = 0.001           ! tolleranza per uguaglianza Real

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
REAL :: fnorm,offset
INTEGER :: iuin,iuout,cnt_in,cnt_utm,ksec2_sav(1024)
INTEGER :: new_dx,new_dy,ios1,ios2
CHARACTER (LEN=200) :: filein,fileout,chpar1,chpar2
LOGICAL :: wrn(3)

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
filein = ""
fileout = ""
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,chpar1)
READ (chpar1,*,IOSTAT=ios1) fnorm
CALL getarg(4,chpar2)
READ (chpar2,*,IOSTAT=ios2) offset

IF (filein=="" .OR. fileout=="" .OR. chpar1=="" .OR. chpar2=="" .OR. &
    ios1/=0 .OR. ios2/=0 .OR. TRIM(filein) == "-h") THEN
  WRITE (*,*) "Uso: grib_utm_scale.exe [-h] filein fileout fnorm offset" 
  WRITE (*,*) "Riscrive filein, modificando la griglia dei campi UTM-GRIB1:"
  WRITE (*,*) "- estremi e passo griglia sono moltiplicati per fnorm"
  WRITE (*,*) "- alle coordinate Y viene sommata la quantita' offset"
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
CALL PBOPEN (iuout,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2) Leggo / Scrivo (ciclo sui grib)

cnt_utm = 0
wrn(:) = .FALSE.

DO cnt_in = 1,HUGE(0)

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

! 2.2 Elaborazioni: se la griglia e' UTM riscalo il campo, altrimenti 
!     riscrivo senza modifiche
  IF (ksec2(1) == 0 .AND. (ksec2(4) > 90000 .OR. ksec2(7) > 90000 .OR. &
       ksec2(5) > 360000 .OR. ksec2(8) > 360000)) THEN
    cnt_utm = cnt_utm + 1

!   Controllo se e' cambiata la griglia
    IF (cnt_utm == 1) THEN
      ksec2_sav(:) = ksec2(:)
    ELSE IF (ANY(ksec2(1:11) /= ksec2_sav(1:11)))  THEN
      wrn(1) = .FALSE.
    ENDIF

!   Rinormalizzio il passo griglia; se il passo grigli risultante non e'
!   intero, forzo la flag "direction increments not given"
    IF (ksec2(6) == 128) THEN
      new_dx = NINT(REAL(ksec2(9)) * fnorm)
      new_dy = NINT(REAL(ksec2(10)) * fnorm)
      IF (ABS(REAL(new_dx)/fnorm - ksec2(9)) < eps .AND. &
          ABS(REAL(new_dy)/fnorm - ksec2(10)) < eps) THEN
        ksec2(9) = new_dx
        ksec2(10) = new_dy
      ELSE
        wrn(2) = .TRUE.
        ksec2(6) = 0
      ENDIF
    ENDIF

!   Rinormalizzo gli estremi della griglia
    ksec2(4) = NINT(REAL(ksec2(4)) * fnorm + offset)
    ksec2(5) = NINT(REAL(ksec2(5)) * fnorm)
    ksec2(7) = NINT(REAL(ksec2(7)) * fnorm + offset)
    ksec2(8) = NINT(REAL(ksec2(8)) * fnorm)

!   Ricodifico il grib
    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                 field,maxdim,kbuffer,maxdim,klen,'C',kret)
    IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret

  ENDIF

! 2.3 riscrivo il grib
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

ENDDO

!--------------------------------------------------------------------------
! 3) Operazioni conclusive 

! Chiudo i files, 
CALL PBCLOSE (iuin,kret)
CALL PBCLOSE (iuout,kret)

! Log complessivo
WRITE (*,*) "grib_utm_scale.exe: grib in input ",cnt_in-1, &
  " di cui UTM ",cnt_utm
IF (wrn(1)) WRITE (*,*) &
  "Warning: il file di input contiene grib UTM sugriglie diverse"
IF (wrn(2)) WRITE (*,*) &
  "Warning: forzati direction increments not givem"
!IF (wrn(3)) WRITE (*,*) &
!  "Warning: probabili errori di troncamento negli estremi della griglia "

STOP

END PROGRAM grib_utm_scale

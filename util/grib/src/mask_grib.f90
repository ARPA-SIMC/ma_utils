PROGRAM maskout_grib
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive, sostituendo i valori nei 
! punti definiti da una maschera.
!
!                                         Versione 1.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: maxdim = 1000000   ! dimensione massima dei GRIB
REAL, PARAMETER :: rmis = 1.e-20         ! valore interno per dati mancanti

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim), klen, kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
INTEGER :: kmask(maxdim)
INTEGER :: np,npmask,nx,ny
INTEGER :: iuin,iumask,iuout,cnt,ios,kp,kp2,ip,jp,ip2,jp2
REAL :: field2(maxdim),fmask(maxdim),mask_val
REAL :: dd,ddmin,dx,dy
CHARACTER (LEN=200) :: filein,fileout,filemask,opt,chpar

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,filemask)
CALL getarg(4,opt)
CALL getarg(5,chpar)

IF (opt == "-val") READ (chpar,*,IOSTAT=ios) mask_val

IF (TRIM(filein) == "-h" .OR. filein == "" .OR. fileout == "" .OR. &
    filemask == "" .OR. opt == "" .OR. &
    (opt /= "-miss" .AND. opt /= "-near" .AND. opt /= "-val") .OR. &
    (opt == "-val" .AND. ios /= 0) ) THEN
  CALL write_help
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin,filein,'R',kret)
IF (kret.ne.0) GOTO 9999
CALL PBOPEN (iumask,filemask,'R',kret)
IF (kret.ne.0) GOTO 9998
CALL PBOPEN (iuout,fileout,'W',kret)

!--------------------------------------------------------------------------
! 2) Leggo la maschera

CALL PBGRIB(iumask,kbuffer,maxdim*4,klen,kret)
CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
             fmask,maxdim,kbuffer,maxdim,klen,'D',kret)

np = ksec0(1)
WHERE (fmask(1:np) == psec3(2))
  fmask(1:np) = rmis
ENDWHERE
npmask = COUNT(fmask(1:np) == rmis)

IF (npmask == 0) THEN
  WRITE (*,*) "La maschera non contiene dati mancanti, fileout sara' uguale a filein"
  opt = ""
ELSE IF (npmask == np .AND. opt == "-near") THEN
  GOTO 9997
ELSE
  WRITE (*,*) "Letta maschera, punti da sostituire ",npmask," su ",np
ENDIF

!--------------------------------------------------------------------------
! 3) Con opzione "-near", trovo il punto valido piu' vicino a ciscun punto
!    mancante

IF (opt == "-near") THEN
 
  IF (ksec2(11) /= 0 .AND. ksec2(11) /= 64) GOTO 9996

  nx = ksec2(2)
  ny = ksec2(3)
  kmask(1:np) = -999

  DO kp = 1,np

!   Se il punto (kp) e' buono, terro' lo stesso valore
    IF (fmask(kp) /= rmis) THEN
      kmask(kp) = kp

!   Altrimenti cerco tra tutti i punti buoni (kp2) quello piu' vicino
    ELSE
      jp = (kp-1)/nx + 1
      ip = kp - (nx*(jp-1))
      ddmin = HUGE(0.)
      DO kp2 = 1,np
        IF (fmask(kp2) == rmis) CYCLE
        jp2 = (kp2-1)/nx + 1
        ip2 = kp2 - (nx*(jp2-1))
        dx = REAL(ip-ip2)
        dy = REAL(jp-jp2)
        dd = SQRT(dx*dx+dy*dy)
        IF (dd < ddmin) THEN
          ddmin = dd
          kmask(kp) = kp2
        ENDIF
      ENDDO

    ENDIF
  ENDDO

  IF (ANY(kmask(1:np) == -999)) GOTO 9995
  WRITE (*,*) "Calcolati punti per sostituzione"

ENDIF

!--------------------------------------------------------------------------
! 3) Leggo / Scrivo (ciclo sui grib)

DO cnt = 1,HUGE(0)

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

! 2.2 Controllo che l'area sia la stessa

! 2.3 Modifico i dati
  IF (opt == "-miss") THEN
    WHERE (fmask(1:np) == rmis)
      field(1:np) = rmis
    ENDWHERE

  ELSE IF (opt == "-near") THEN
    field2(1:kp) = field(kmask(1:kp))
    field(1:np) = field2(1:np)

  ELSE IF (opt == "-val") THEN
    WHERE (fmask(1:np) == rmis)
      field(1:np) = mask_val
    ENDWHERE

  ENDIF

! 2.3 Riscrivo il grib
  psec3(2) = rmis
  IF (npmask > 0 .AND. opt == "-miss") ksec1(5) = 192
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret


ENDDO
!--------------------------------------------------------------------------
! 4) Conclusione

CALL PBCLOSE (iuin,kret)
CALL PBCLOSE (iumask,kret)
CALL PBCLOSE (iuout,kret)

WRITE (*,*) "Letti-scritti ",cnt-1," grib"

STOP

!--------------------------------------------------------------------------
! 5) Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filemask)
STOP

9997 CONTINUE
WRITE (*,*) "Maschera interamente mancante, opzione -near impossibile"
STOP

9996 CONTINUE
WRITE (*,*) "Errore scanning mode non gestito ",ksec2(11)
STOP

9995 CONTINUE
WRITE (*,*) "Erroraccio mask: ",COUNT(kmask(1:np) == -999)
STOP

END PROGRAM maskout_grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a schermo l'help del programma
!
WRITE (*,*) "Uso: mask_grib.exe filein fileout filemask -miss/-near/-val [val]"
WRITE (*,*) ""
WRITE (*,*) "filemask: contiene un solo grib, definito sulla stessa area di filein"
WRITE (*,*) "fileout:  uguale a filein, tranne nei punti che sono mancanti in filemask"
WRITE (*,*) "  -miss:  mette mancanti"
WRITE (*,*) "  -near:  mette il punto piu' vicino (esclusi quelli mancanti in filemask)"
WRITE (*,*) "  -val X: mette al valore specificato"


END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


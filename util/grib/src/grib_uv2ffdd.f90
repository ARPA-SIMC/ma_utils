PROGRAM grib_uv2ffdd
!--------------------------------------------------------------------------
! Programma che legge un file con le componenti u e uno con le componenti
! v, e ne scrive due con direzione e modulo.
!
! Note:
! - Controlla che tutti i grib siano definiti sulla stessa area (sez.2)
! - Controlla che ciascuna coppia di grib si riferisca alla stessa scadenza
!   (sez 1)
! - Le altre sezioni sono prese dall'utlimo grib del file
!
!                                         Versione 1.3.3, Enrico 27/11/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.           ! valore per dati mancanti
!REAL, PARAMETER :: rmis = -HUGE(0.)       ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 100000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0u(2),ksec1u(1024),ksec2u(1024),ksec3u(2),ksec4u(512)
INTEGER :: ksec0v(2),ksec1v(1024),ksec2v(1024),ksec3v(2),ksec4v(512)
INTEGER :: ksec0d(2),ksec1d(1024),ksec2d(1024),ksec3d(2),ksec4d(512)
INTEGER :: ksec0f(2),ksec1f(1024),ksec2f(1024),ksec3f(2),ksec4f(512)

INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2u(512),psec3u(2),psec2v(512),psec3v(2)
REAL :: psec2d(512),psec3d(2),psec2f(512),psec3f(2)
REAL :: field_uu(maxdim),field_vv(maxdim)
REAL :: field_ff(maxdim),field_dd(maxdim)

! Altre variabili del programma
REAL :: fave
INTEGER :: ngrib,iuin1,iuin2,iuout1,iuout2,nok,np_sav,np
CHARACTER (LEN=200) :: filein1,filein2,fileout1,fileout2

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
CALL getarg(1,filein1)
CALL getarg(2,filein2)
CALL getarg(3,fileout1)
CALL getarg(4,fileout2)

IF (filein1 == "" .OR. filein2 == "" .OR. &
    fileout1 == "" .OR. fileout2 == "" .OR. &
    TRIM(filein1) == "-h") THEN
  WRITE (*,*) "Uso: grib_uv2ffdd.exe [-h] file_uu file_vv file_dd file_ff" 
  STOP
ENDIF

! 1.2 Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! 1.3 Apro i files
CALL PBOPEN (iuin1,filein1,'R',kret)
IF (kret /= 0) THEN 
  WRITE(*,*) "Errore aprendo ",filein1," kret ",kret
  STOP
ENDIF

CALL PBOPEN (iuin2,filein2,'R',kret)
IF (kret /= 0) THEN 
  WRITE(*,*) "Errore aprendo ",filein2," kret ",kret
  STOP
ENDIF

CALL PBOPEN (iuout1,fileout1,'W',kret)
CALL PBOPEN (iuout2,fileout2,'W',kret)

OPEN (UNIT=96, FILE="grib_uv2ddff.log", STATUS="REPLACE", ACTION="WRITE")

!--------------------------------------------------------------------------
! 2) Lettura - Scrittura (ciclo sui grib)

ngrib = 0

grib: DO

! 2.1) Leggo e decodifico il grib uu
  CALL PBGRIB(iuin1,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    EXIT grib
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3u(2) = rmis                                   ! dati mancanti = rmis
  CALL GRIBEX (ksec0u,ksec1u,ksec2u,psec2u,ksec3u,psec3u,ksec4u, &
               field_uu,maxdim,kbuffer,maxdim,klen,'D',kret)
  if (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.2) Leggo & decodifico il grib vv
  CALL PBGRIB(iuin2,kbuffer,maxdim*4,klen,kret)
  IF (kret.eq.-1) THEN 
    WRITE(*,*) "Errore, il file vv continene meno grib del file uu"
    STOP
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  psec3v(2) = rmis                                   ! dati mancanti = rmis
  CALL GRIBEX (ksec0v,ksec1v,ksec2v,psec2v,ksec3v,psec3v,ksec4v, &
               field_vv,maxdim,kbuffer,maxdim,klen,'D',kret)
  if (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! 2.3) Controllo che uu e vv siano compatibili: deve essere diverso solo il
!      codice parametro ksec1(6)
  IF (ANY(ksec1v(:5) /= ksec1u(:5)) .OR. (ksec1v(6) == ksec1u(6)) .OR. &
      ANY(ksec1v(7:24) /= ksec1u(7:24))) THEN
    WRITE (*,*) "Dati incompatibili nella sez. 1, grib ",ngrib + 1
    WRITE (*,'(a,24(1x,i3))') "file U, ksec1(1:24) " ,ksec1u(1:24)
    WRITE (*,'(a,24(1x,i3))') "file V, ksec1(1:24) " ,ksec1v(1:24)
    STOP
  ELSE IF (ANY(ksec2v(:) /= ksec2u(:))) THEN
    WRITE (*,*) "Dati incompatibili nella sez. 2, grib ",ngrib + 1
    WRITE (*,'(a,20i3)') "file U, ksec2(1:20) " ,ksec2u(1:20)
    WRITE (*,'(a,20i3)') "file V, ksec2(1:20) " ,ksec2v(1:20)
    STOP
  ENDIF

  ngrib = ngrib +1

! 2.4) Calcolo i campi direzione e modulo e li scrivo
  np = ksec4u(1)
  CALL uv2dirint(field_uu(1:np),field_vv(1:np),field_dd(1:np), &
    field_ff(1:np),np,rmis)

! Scrivo dd
  ksec1d(:) = ksec1u(:)
  ksec1d(6) = 35
  psec3d(2) = rmis                                   ! dati mancanti = rmis
  CALL GRIBEX (ksec0d,ksec1d,ksec2u,psec2u,ksec3u,psec3d,ksec4u, &
               field_dd,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout1,kbuffer,ksec0d(1),kret)
  
! Log ff
  nok = COUNT(field_ff(1:np) /= rmis)
  IF (nok > 0) THEN
    fave = SUM(field_ff(1:np), MASK = field_ff(1:np)/=rmis) / REAL(nok)
  ELSE
    fave = rmis
  ENDIF
  WRITE (96,'(a,i4,i6,f8.2)') "Scritti dd,ff ;prog, dati ok, ff media ", &
    ngrib,nok,fave

! Scrivo ff
  ksec1f(:) = ksec1u(:)
  ksec1f(6) = 36
  psec3f(2) = rmis                                   ! dati mancanti = rmis
  CALL GRIBEX (ksec0f,ksec1f,ksec2u,psec2u,ksec3u,psec3f,ksec4u, &
               field_ff,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout2,kbuffer,ksec0f(1),kret)

  WRITE (*,*) "Scritti dd,ff ",ngrib

ENDDO grib

!--------------------------------------------------------------------------
! 3) Conclusione

CALL PBCLOSE (iuin1,kret)
CALL PBCLOSE (iuin2,kret)
CALL PBCLOSE (iuout1,kret)
CALL PBCLOSE (iuout2,kret)
CLOSE (96)

END PROGRAM grib_uv2ffdd

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
SUBROUTINE uv2dirint(u,v,dd,ff,nval,rmis)
!
! Dati i vettori di componenti u e v, ritona quelli di direzione e modulo
! Se una componente e' mancante dd e ff sono mancanti. 
! Se u=v=0, mette ff=dd=0
!

IMPLICIT NONE

INTEGER, INTENT(IN):: nval
REAL, INTENT(IN) :: u(nval),v(nval),rmis
REAL, INTENT(OUT) :: dd(nval),ff(nval)

REAL, PARAMETER :: dtr = 180./3.141592654
INTEGER k

!-------------------------------------------------------------------------
DO k = 1,nval

  IF (u(k) == rmis .OR. v(k) == rmis) THEN
    dd(k) = rmis
    ff(k) = rmis
    CYCLE
  ENDIF

  IF      (u(k) <= 0. .AND. v(k) < 0.) THEN    ! 0 - 90
    dd(k) = dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) < 0. .AND. v(k) >= 0.) THEN    ! 90 - 180
    dd(k) = 90. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) >= 0. .AND. v(k) > 0.) THEN    ! 180 - 270
    dd(k) = 180. + dtr*atan( u(k)/v(k) )
  ELSE IF (u(k) > 0. .AND. v(k) <= 0.) THEN    ! 270 - 360
    dd(k) = 270. + dtr*atan( -v(k)/u(k) )
  ELSE IF (u(k) == 0. .AND. v(k) == 0.) THEN
    dd(k) = 0.
  ENDIF
 
  ff(k) = SQRT (u(k)*u(k) + v(k)*v(k))

ENDDO

RETURN
END SUBROUTINE uv2dirint


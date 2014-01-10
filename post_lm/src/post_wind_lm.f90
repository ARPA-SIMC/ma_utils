PROGRAM post_wind_lm
!--------------------------------------------------------------------------
! Legge 2 files con molti grib di componenti U e V dall'output di LM;
! li riscrive destaggherati e antiruotati.
!
! Note:
! - I grib corrispondenti nei due file di input devono avere la stessa area
!   e la stessa sezione 1 (tranne il parametro)
! - Con l'opzione -dest, suppone che i dati di input siano su griglia C
!   scritta come griglia H (i.e. spostati di mezza cella verso NE)
! - Con l'opzione -antir antiruota le componenti
!
!                                         Versione 1.1.4, Enrico 10/01/2014
!--------------------------------------------------------------------------

USE grid_handler
IMPLICIT NONE

! Parametri costanti:
! rmis (valore per dati mancanti) e' definito in grid_handler
! maxdim (dimensione massima dei GRIB) e' definito in grid_handler

LOGICAL, PARAMETER :: deb = .TRUE.         ! Abilita log esteso su fort.90
INTEGER, PARAMETER :: nxdeb=5, nydeb=5     ! dim. riquadro visualizzato

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0u(2),ksec1u(1024),ksec2u(1024),ksec3u(2),ksec4u(512)
INTEGER :: ksec0v(2),ksec1v(1024),ksec2v(1024),ksec3v(2),ksec4v(512)
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)

INTEGER :: kbuffer(maxdim), klen, kret
REAL :: psec2u(512),psec3u(2),psec2v(512),psec3v(2),psec2(512),psec3(2)
REAL :: field_uu(maxdim),field_vv(maxdim),field(maxdim)

! Altre variabili del programma
TYPE(grid) :: grin_uu,grin_vv,grout_uu,grout_vv
REAL :: fave,field1(maxdim),field2(maxdim)
INTEGER :: kf,kp,ni,nj,k,ii,jj,kk,k0
INTEGER :: ngrib,in_geo,iuin1,iuin2,iuout1,iuout2,nok,np_sav,np
CHARACTER (LEN=80) :: filein1,filein2,fileout1,fileout2,chpar,file(4)
LOGICAL :: dest,antir,ksec2_diff

!==========================================================================
! 1) Preliminari

! 1.1 Parametri da riga comando
dest = .FALSE.
antir = .FALSE.
kf = 0
DO kp = 1,6
  CALL getarg(kp,chpar)
  IF (TRIM(chpar) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chpar) == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-dest") THEN
    dest = .TRUE.
  ELSE IF (TRIM(chpar) == "-antir") THEN
    antir = .TRUE.
  ELSE
    kf = kf + 1
    file(kf) = chpar
  ENDIF
ENDDO

IF (ANY(file(1:4) == "")) THEN
  CALL write_help
  STOP
ENDIF

filein1 = file(1)
filein2 = file(2)
fileout1 = file(3)
fileout2 = file(4)

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

OPEN (UNIT=96, FILE="grib_wind_antir.log", STATUS="REPLACE", ACTION="WRITE")

!==========================================================================
! 2) Lettura - Scrittura (ciclo sui grib)

ngrib = 0
in_geo = 0

grib: DO

!--------------------------------------------------------------------------
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
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

  CALL build_grid(ksec2u,field_uu,grin_uu)

  IF (deb) THEN
    WRITE (90,*)
    WRITE (90,*) "grin_uu letta (angolo SW)"
    DO jj = nydeb,1,-1
      k0 = (jj-1)*grin_uu%nx
      WRITE (90,'(18(f7.3,1x))') grin_uu%field(k0+1:k0+nxdeb)
    ENDDO
  ENDIF

!--------------------------------------------------------------------------
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
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

  CALL build_grid(ksec2v,field_vv,grin_vv)

  IF (deb) THEN
    WRITE (90,*)
    WRITE (90,*) "grin_vv letta (angolo SW)"
    DO jj = nydeb,1,-1
      k0 = (jj-1)*grin_vv%nx
      WRITE (90,'(18(f7.3,1x))') grin_vv%field(k0+1:k0+nxdeb)
    ENDDO
  ENDIF

!--------------------------------------------------------------------------
! 2.3) Controlli sulle intestazioni dei grib

  IF (ANY(ksec1v(:5) /= ksec1u(:5)) .OR. ANY(ksec1v(7:21) /= ksec1u(7:21)) .OR. &
      ksec2_diff(ksec2u(1:14),ksec2v(1:14))) THEN
    WRITE (*,*) "Trovati grib con sezioni diverse, ngrib = ",ngrib+1
    DO kp = 1,5
      IF (ksec1v(kp) /= ksec1u(kp)) WRITE (*,*) &
        "sez 1 (k,u,v): ",kp,ksec1u(kp),ksec1v(kp)
    ENDDO
    DO kp = 7,21
      IF (ksec1v(kp) /= ksec1u(kp)) WRITE (*,*) &
        "sez 1 (k,u,v): ",kp,ksec1u(kp),ksec1v(kp)
    ENDDO
    DO kp = 1,14
      IF (ksec2v(kp) /= ksec2u(kp)) WRITE (*,*) &
        "sez 2 (k,u,v): ",kp,ksec2u(kp),ksec2v(kp)
    ENDDO
    STOP
  ENDIF

  IF (ksec1u(6) /= 33 .OR. ksec1v(6) /= 34) THEN
    WRITE (*,*) "Codici parametro inattesi nei files di input"
    WRITE (*,*) "file1: ",ksec1u(6)," file2: ",ksec1v(6)," ngrib ",ngrib+1
    STOP
  ENDIF

  IF (.NOT. (grin_uu .samegrid. grin_vv)) THEN
    WRITE (*,*) "Erroraccio gestione griglie"
    STOP
  ENDIF

  IF (ksec2u(19) == 0) in_geo = in_geo + 1
  ngrib = ngrib +1

!--------------------------------------------------------------------------
! 2.4) Se richiesto, destagghero da griglia C LM 

  IF (dest) THEN  

!   Ciclo  sui punti H reali
    ni = grin_uu%nx
    nj = grin_uu%ny
    field1(:) = rmis
    field2(:) = rmis

    DO kk = 1, ni*nj
      jj = (kk-1) / ni + 1
      ii = kk - (jj-1)*ni

!     Destag U
      IF (ii == 1) THEN
        field1(kk) = grin_uu%field(kk)
      ELSE
        field1(kk) = 0.5 * (grin_uu%field(kk-1) + grin_uu%field(kk))
      ENDIF

!     Destag V
      IF (jj == 1) THEN
        field2(kk) = grin_vv%field(kk)
      ELSE
        field2(kk) = 0.5 * (grin_vv%field(kk-ni) + grin_vv%field(kk))
      ENDIF
    ENDDO

    grin_uu%field(:) = field1
    grin_vv%field(:) = field2

  ENDIF

  IF (deb) THEN
    WRITE (90,*)
    WRITE (90,*) "grin_uu destag"
    DO jj = nydeb,1,-1
      k0 = (jj-1)*grin_uu%nx
      WRITE (90,'(18(f7.3,1x))') grin_uu%field(k0+1:k0+nxdeb)
    ENDDO
    WRITE (90,*)
    WRITE (90,*) "grin_vv destag"
    DO jj = nydeb,1,-1
      k0 = (jj-1)*grin_vv%nx
      WRITE (90,'(18(f7.3,1x))') grin_vv%field(k0+1:k0+nxdeb)
    ENDDO
  ENDIF

!--------------------------------------------------------------------------
! 2.5) Se richiesto antiruoto il vento

  IF (antir) THEN
    CALL antirot_wind(grin_uu,grin_vv,grout_uu,grout_vv)
  ELSE
    grout_uu = grin_uu
    grout_vv = grin_vv
  ENDIF

  IF (deb) THEN
    WRITE (90,*)
    WRITE (90,*) "grout_uu"
    DO jj = nydeb,1,-1
      k0 = (jj-1)*grin_uu%nx
      WRITE (90,'(18(f7.3,1x))') grin_uu%field(k0+1:k0+nxdeb)
    ENDDO
    DO jj = nydeb,1,-1
      k0 = (jj-1)*grin_vv%nx
      WRITE (90,'(18(f7.3,1x))') grin_vv%field(k0+1:k0+nxdeb)
    ENDDO
  ENDIF

!--------------------------------------------------------------------------
! 2.6) Scrivo le nuove componenti

! Scrivo uu
  psec3u(2) = rmis                                   ! dati mancanti = rmis
  field(:) = grout_uu%field(:)
  CALL GRIBEX (ksec0u,ksec1u,ksec2u,psec2u,ksec3u,psec3u,ksec4u, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)

  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout1,kbuffer,ksec0u(1),kret)

! Scrivo vv
  psec3v(2) = rmis                                   ! dati mancanti = rmis
  field(:) = grout_vv%field(:)
  CALL GRIBEX (ksec0v,ksec1v,ksec2v,psec2v,ksec3v,psec3v,ksec4v, &
               field,maxdim,kbuffer,maxdim,klen,'C',kret)

  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  CALL PBWRITE (iuout2,kbuffer,ksec0v(1),kret)

ENDDO grib

WRITE (*,*) "Post_wind_lm: coppie u,v elaborate: ",ngrib

!==========================================================================
! 3) Conclusione

IF (in_geo > 0) WRITE (*,*) & 
  "Warning: comp. gia' riferite al nord geografico in ",in_geo," campi"

CALL PBCLOSE (iuin1,kret)
CALL PBCLOSE (iuin2,kret)
CALL PBCLOSE (iuout1,kret)
CALL PBCLOSE (iuout2,kret)
CLOSE (96)

END PROGRAM post_wind_lm

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: post_wind_lm.exe fin_uu fin_vv fout_uu fout_vv [-dest] [-antir] [-h]"
WRITE (*,*) "Legge 2 files con molti grib di componenti U e V e li riscrive"
WRITE (*,*) "I grib di input devono essere definiti sulla stessa area."
WRITE (*,*) ""
WRITE (*,*) "-dest:  suppone che i dati di input siano su griglia C scritta come H"
WRITE (*,*) "        (i.e. spostati di mezza cella verso NE; archivi LAMI e LAMA), e li"
WRITE (*,*) "        interpola sulla griglia H reale."
WRITE (*,*) "-antir: suppone che i dati di input siano componenti relative al nord "
WRITE (*,*) "        ruotato, e le riscrive rispetto al nord geografico"
WRITE (*,*) ""

END SUBROUTINE write_help

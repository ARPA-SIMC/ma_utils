PROGRAM zoom_grib
!--------------------------------------------------------------------------
! Legge un file con molti grib e lo riscrive, ritagliando una sottoarea.
!
! Uso: zoom_grib [-h] filein fileout -pts/-ins/-cir West Sud Est Nord
!
! Sottoaree utili:
! 349 237 582 507 (LMSMR2 nei ds fisiografici DWD)
!  57  17 290 288 (LMSMR2 in LMSMR4)
!  34  47 202 227 (LAMAZ  in LMSMR2)
!  90  63 258 243 (LAMAZ  in LMSRM4)
!
!                                         Versione 2.0.1, Enrico 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rmis = -9999.            ! valore per dati mancanti
INTEGER, PARAMETER :: maxdim = 1000000      ! dimensione massima dei GRIB

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: ksec2_in(1024),ksec2_out(1024)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field_in(maxdim),field_out(maxdim)

! Altre variabili del programma
REAL (KIND=8) :: x1,y1,x2,y2,xr1,xr2,yr1,yr2,x1z,x2z,y1z,y2z,dx,dy,adj_lm
INTEGER, ALLOCATABLE :: idx(:)
INTEGER :: iz1,jz1,iz2,jz2,sel
INTEGER :: nx,ny,np,nxz,nyz,npz
INTEGER :: iz,jz,kz,ic,jc,kc
INTEGER :: iuin1,iuin2,iuout,idp,kp,cnt,ios(4)
CHARACTER (LEN=200) :: filein,fileout,chdum,arg(2)
CHARACTER (LEN=1) :: next_arg

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando
idp = 0
next_arg = ""

DO kp = 1,7
  CALL getarg(kp,chdum)

  IF (chdum == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP

  ELSE IF (TRIM(chdum) == "-pts") THEN
    sel = 1
    next_arg = "w"
    CYCLE
  ELSE IF (TRIM(chdum) == "-ins") THEN
    sel = 2
    next_arg = "w"
    CYCLE
  ELSE IF (TRIM(chdum) == "-cir") THEN
    sel = 3
    next_arg = "w"
    CYCLE

  ELSE IF (next_arg == "w") THEN
    IF (sel == 1) THEN
      READ (chdum,*,IOSTAT=ios(1)) iz1
    ELSE
      READ (chdum,*,IOSTAT=ios(1)) xr1
    ENDIF
    next_arg = "s"
    CYCLE
  ELSE IF (next_arg == "s") THEN
    IF (sel == 1) THEN
      READ (chdum,*,IOSTAT=ios(2)) jz1
    ELSE
      READ (chdum,*,IOSTAT=ios(2)) yr1
    ENDIF
    next_arg = "e"
    CYCLE
  ELSE IF (next_arg == "e") THEN
    IF (sel == 1) THEN
      READ (chdum,*,IOSTAT=ios(3)) iz2
    ELSE
      READ (chdum,*,IOSTAT=ios(3)) xr2
    ENDIF
    next_arg = "n"
    CYCLE
  ELSE IF (next_arg == "n") THEN
    IF (sel == 1) THEN
      READ (chdum,*,IOSTAT=ios(4)) jz2
    ELSE
      READ (chdum,*,IOSTAT=ios(4)) yr2
    ENDIF
    next_arg = ""
    CYCLE
  ENDIF

  idp = idp + 1
  arg(idp) = chdum

ENDDO

filein = arg(1)
fileout = arg(2)

IF (filein == "" .OR. fileout == "" .OR. ANY(ios(1:4) /= 0) .OR. &
    sel < 1 .OR. sel > 3) THEN
  CALL write_help
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.2) Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

!==========================================================================
! 2) Calcoli relativi alla sottoarea

!--------------------------------------------------------------------------
! 2.1) Apro filein una prima volta per leggere l'aera di input

! Leggo il grib
CALL PBOPEN (iuin1,filein,'R',kret)
IF (kret < 0) THEN
  WRITE (*,*) "Errore aprendo ",TRIM(filein)
  STOP
ENDIF
CALL PBGRIB(iuin1,kbuffer,maxdim*4,klen,kret)
IF (kret == -1) THEN
  WRITE (*,*) "Errore leggendo ",TRIM(filein)
  STOP
ELSE IF (kret < -1) THEN
  WRITE(*,*) "Error pbgrib: kret ",kret
  STOP
ENDIF
CALL GRIBEX (ksec0,ksec1,ksec2_in,psec2,ksec3,psec3,ksec4, &
             field_in,maxdim,kbuffer,maxdim,klen,'D',kret)
IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
!CALL PBCLOSE(iuin1)

! Calcolo passo ed estremi
IF (ksec2_in(11) == 64) THEN      ! scanning standard (LAM)
  y1 = DBLE(ksec2_in(4)) / 1000.
  y2 = DBLE(ksec2_in(7)) / 1000.
ELSE IF (ksec2_in(11) == 0) THEN  ! scanning rovesciato (ECMWF)
  y1 = DBLE(ksec2_in(7)) / 1000.
  y2 = DBLE(ksec2_in(4)) / 1000.
ELSE                                 ! Scanning non gestito (errore)
  WRITE (*,*) "Scanning mode non gestito ",ksec2_in(11)
  STOP
ENDIF

nx = ksec2_in(2)
ny = ksec2_in(3)
x1 = DBLE(ksec2_in(5)) / 1000.
x2 = DBLE(ksec2_in(8)) / 1000.
np = nx*ny

IF (ksec2_in(6) == 0) THEN
  dx = (x2-x1) / DBLE(nx-1)
  dy = (y2-y1) / DBLE(ny-1)

! Le aree operativa LM a 7 km hanno passo 0.0625, ma gli estremi vengono
! scritti negli header del GRIB con una precisione di 3 decimali. Questo
! porterebbe a errori di troncamento nel passo griglia e nelle coordinate
! estreme dell'area di output.
  IF (ksec2_in(13) == -32500 .AND. ksec2_in(14) == 10000 .AND. &
    ABS(dx-0.0625) < 0.005 .AND. ABS(dy-0.0625) < 0.005) THEN
    x1 = adj_lm(x1)
    x2 = adj_lm(x2)
    y1 = adj_lm(y1)
    y2 = adj_lm(y2)

    dx = (x2-x1) / DBLE(nx-1)
    dy = (y2-y1) / DBLE(ny-1)
  ENDIF

ELSE
  dx =  DBLE(ksec2_in(9)) / 1000.
  dy =  DBLE(ksec2_in(10)) / 1000.

ENDIF

! Visualizzo a schermo i parametri dell'aera input
WRITE (*,'(a)') "Area in input"
WRITE (*,'(a,2i5,2f10.5)') "nx,ny,dx,dy: ",nx,ny,dx,dy
WRITE (*,'(a,4f12.6)')     "W,S,E,N    : ",x1,y1,x2,y2
WRITE (*,*)

!--------------------------------------------------------------------------
! 2.2) Se la selezione avviane in base a coordinate geografiche, calcolo gli
!      indici estremi della sottoarea di output

IF (sel == 2 .OR. sel == 3) THEN
  WRITE (*,*) "Selezione area con coordinate estreme non implementata!"
  STOP
ENDIF

! Controlli
IF (iz1 < 0 .OR. iz2 < 0 .OR. jz2 < 0 .OR. jz2 < 0 .OR. &
    iz1 > nx .OR. iz2 > nx .OR. jz1 > ny .OR. jz2 > ny .OR. &
    iz1 > iz2 .OR. jz1 > jz2) THEN
  WRITE (*,'(a,4i5)') "Indici sottoarea illegali; (W,S,E,N): ", &
    iz1,jz1,iz2,jz2
  STOP
ENDIF

!--------------------------------------------------------------------------
! 2.3) Costruisco la nuova sezione 2

x1z = x1 + dx * (iz1-1)
y1z = y1 + dy * (jz1-1)
x2z = x1 + dx * (iz2-1)
y2z = y1 + dy * (jz2-1)
nxz = iz2 - iz1 + 1
nyz = jz2 - jz1 + 1
npz = nxz * nyz
ALLOCATE (idx(npz))

ksec2_out(:) = ksec2_in(:)
ksec2_out(2) = nxz
ksec2_out(3) = nyz
ksec2_out(5) = NINT(x1z * 1000.)
ksec2_out(8) = NINT(x2z * 1000.)

IF (ksec2_in(11) == 64) THEN      ! scanning standard (LAM)
  ksec2_out(4) = NINT(y1z * 1000.)
  ksec2_out(7) = NINT(y2z * 1000.)
ELSE IF (ksec2_in(11) == 0) THEN  ! scanning rovesciato (ECMWF)
  ksec2_out(7) = NINT(y1z * 1000.)
  ksec2_out(4) = NINT(y2z * 1000.)
ENDIF

! Tappo per fare in modo che lo zoom sull'area LAMAZ abbia gli stessi estremi
! che uscivano dalla versione vecchia del programma
!
!IF (nxz == 169 .AND. nyz == 181 .AND. &
!    ksec2_out(5) == -2938 .AND. ksec2_out(4) == -21125 .AND. &
!    ksec2_out(8) == 7563 .AND. ksec2_out(7) == -9875) THEN
!  WRITE (*,'(a)') "*** area LAMAZ, aggiusto estremi"
!  ksec2_out(5) = -2937
!ENDIF

! Visualizzo a schermo i parametri dell'aera output
WRITE (*,'(a)') "Area in output"
WRITE (*,'(a,2i5,2f10.5)') "nx,ny,dx,dy: ",nxz,nyz,dx,dy
WRITE (*,'(a,4f12.6)')     "W,S,E,N    : ",x1z,y1z,x2z,y2z
IF (ksec2_in(11) == 64) THEN
  WRITE (*,'(a,4(1x,i7))') "-> Header GRIB: ",&
    ksec2_out(5),ksec2_out(4),ksec2_out(8),ksec2_out(7)
ELSE IF (ksec2_in(11) == 0) THEN
  WRITE (*,'(a,4(1x,i7))') "-> Header GRIB: ",&
    ksec2_out(5),ksec2_out(4),ksec2_out(7),ksec2_out(8)
ENDIF

WRITE (*,'(a,4i5)') "Indici estremi sottoarea (W,S,E,N): ",iz1,jz1,iz2,jz2
WRITE (*,*)

!--------------------------------------------------------------------------
! 2.4) Calcolo gli indici nella vecchia griglia dei punti della sottoarea 

IF (ksec2_in(11) == 64) THEN      ! scanning standard (LAM)
  DO iz = 1,nxz
  DO jz = 1,nyz
    ic = iz + iz1 - 1
    jc = jz + jz1 - 1
    kc = ic + (jc-1)*nx
    kz = iz + (jz-1)*nxz
    idx(kz) = kc
  ENDDO
  ENDDO

ELSE IF (ksec2_in(11) == 0) THEN  ! scanning rovesciato (ECMWF)
  DO iz = 1,nxz
  DO jz = 1,nyz
    ic = iz + iz1 - 1
    jc = jz + jz1 - 1
    kc = ic + (ny-jc)*nx
    kz = iz + (nyz-jz)*nxz
    idx(kz) = kc
  ENDDO
  ENDDO

ENDIF

!==========================================================================
! 3) Leggo / Scrivo (ciclo sui grib)

CALL PBOPEN (iuin2,filein,'R',kret)
CALL PBOPEN (iuout,fileout,'W',kret)

DO cnt = 1,HUGE(cnt)

! 2.1 Leggo il grib orignale
  CALL PBGRIB(iuin2,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field_in,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

  IF (ANY(ksec2(1:19) /= ksec2_in(1:19))) THEN
    WRITE (*,*) "Errore, i grib in input hanno aree diverse"
    STOP
  ENDIF

! 2.2 Lo riscrivo sulla sottoaera
  field_out(1:npz) = field_in(idx(1:npz))
  ksec4(1) = npz
  CALL GRIBEX (ksec0,ksec1,ksec2_out,psec2,ksec3,psec3,ksec4, &
               field_out,maxdim,kbuffer,maxdim,klen,'C',kret)
  IF (kret > 0) WRITE (*,*) "Warning gribex: kret ",kret
  
  CALL PBWRITE (iuout,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) "Error pbwrite, kret ",kret

ENDDO
!--------------------------------------------------------------------------
! 3) Chiudo i files e termino

!CALL PBCLOSE (iuin2,kret)
!CALL PBCLOSE (iuout,kret)

WRITE (*,*) "Letti-scritti ",cnt-1," grib"

STOP

END PROGRAM zoom_grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION adj_lm(zold) RESULT(znew)
!
! Ripristina il valore corretto di una coordinata estrema di un'area 
! "LAMI 7km", annullando il troncamento a 3 decimali dell'header GRIB
!
IMPLICIT NONE
REAL (KIND=8) :: zold,znew
INTEGER :: dec3,sgn
!

! Lavoro con numeri positivo (altrimenti NINT e' un casino...)
IF (zold < 0.) THEN
  sgn = -1
  zold = -zold
ELSE
  sgn = 1
ENDIF

! Se le terza cifra decimale e' 2-3 o 7-8, la metto a 2.5 o 7.5
dec3 = MOD(NINT(zold*1000.),10)
IF (dec3 == 2 .OR. dec3 == 3) THEN
  znew = DBLE(100*INT(zold*100.)+25) / 10000.
ELSE IF (dec3 == 7 .OR. dec3 == 8) THEN
  znew = DBLE(100*INT(zold*100.)+75) / 10000.
ELSE
  znew = zold
ENDIF

zold = zold * DBLE(sgn)
znew = znew * DBLE(sgn)

! Se ho cambiato qualcosa, avverto
IF (zold /= znew) WRITE (*,'(2(a,f10.5))') &
  "Area LM, corretta coordinata estremo area: da ",zold," a ",znew

RETURN
END FUNCTION adj_lm

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a schermo l'help del programma
!
!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) ""
WRITE (*,*) "                 ***** ZOOM_GRIB  ***** "
WRITE (*,*) "Programma che legge un file contente molti grib e lo riscrive ritagliando"
WRITE (*,*) "  una sottoarea."
WRITE (*,*)
WRITE (*,*) "USO:"
WRITE (*,*) "zoom_grib [-h] filein fileout -pts/-ins/-cir  West Sud Est Nord"
WRITE (*,*) ""
WRITE (*,*) " -h     : visualizza questo help"
WRITE (*,*) "filein  : file da cui leggere i grib"
WRITE (*,*) "fileout : file su cui scrivere i grib"
WRITE (*,*) " -pts   : seleziona l'area in base agli indici dei punti: W,S,E,N sono gli"
WRITE (*,*) "          indici degli angoli della sottoarea (i,j relativi all'area input)"
WRITE (*,*) " -ins   : seleziona l'area in base alle coordiante esterne: scrive il"
WRITE (*,*) "          riquadro piu' grande interamente contenuto negli estremi W,S,E,N"
WRITE (*,*) " -cir   : seleziona l'area in base alle coordiante interne: scrive il"
WRITE (*,*) "          riquadro piu' piccolo che contiene gli estremi W,S,E,N"
WRITE (*,*) " W,S,E,N: estremi della sottoarea richiesta"
WRITE (*,*) ""

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

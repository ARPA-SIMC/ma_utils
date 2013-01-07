PROGRAM chimere2grib
!--------------------------------------------------------------------------
! Legge un file di output (oppure un file fisiografico) di Chimere e lo
!   scrive in formato GRIB
! Uso: chimere2grib.exe filein fileout fileinfo igen [-V2003] [-v]
!                       [-lu/-bio/-metg/-meta/-ini/-eminv/-latbc/-topbc]
!
! NOTE:
! I formati metg e meta differicono per il numero di campi 3d e 2d che 
!   contengono. Il tracciato del file e' fisso, ma fileinfo deve comunque
!   contenere l'elenco corretto dei parametri.
!
! I grib in quota sono codificati come livelli ibridi, ma senza includere i
!   vertical coordinate parameters. 
!
!                                           Versione 6.4, Enrico 22/05/2008
!--------------------------------------------------------------------------

IMPLICIT NONE

INTERFACE

  FUNCTION lowercase(chin) RESULT (chout)
  IMPLICIT NONE
  CHARACTER (*), INTENT(IN) :: chin
  CHARACTER (LEN=LEN_TRIM(chin)) :: chout
  END FUNCTION lowercase

  FUNCTION uppercase(chin) RESULT (chout)
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(IN) :: chin
  CHARACTER (LEN=LEN_TRIM(chin)) :: chout
  END FUNCTION uppercase

END INTERFACE

! Parametri costanti
INTEGER, PARAMETER :: maxdim = 100000  ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxvar = 200     ! n.ro max var. in output Chimere
INTEGER, PARAMETER :: maxlev = 16      ! n.ro max livelli in output Chimere
INTEGER, PARAMETER :: nhead = 9        ! record d'intestazione in fileinfo
INTEGER, PARAMETER :: nbit = 24        ! n.ro bit per codifica GRIB
REAL, PARAMETER :: rmis = -1.e9        ! codifica valore mancante

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),kword,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
DOUBLE PRECISION, ALLOCATABLE :: conc2p(:,:,:)
REAL, ALLOCATABLE :: conc(:,:,:),tot(:),conc_miss(:,:,:)
REAL :: eps(maxvar),x1,y1,x2,y2,dx,dy,xrot,yrot,rdum
INTEGER :: version
INTEGER :: nvar,nlev,nx,ny,np,nvar_out,nvar3d,nvar2d,nl,slen,mm,ii,nxi,nyi
INTEGER :: code_var(maxvar),tab_var(maxvar),lev_out(maxlev)
INTEGER :: cem,igen,idata,idata_ini,yy,scad_ini
INTEGER :: iu,k,kp,kvar,kscad,klev,kday,kh,ios,eof,eor,cnt_grb,idp,inp_fmt
CHARACTER (LEN=80) :: filein,fileout,fileinfo,chrec,chdum,arg(4)
CHARACTER (LEN=10) :: chdata
CHARACTER (LEN=3) :: proj
LOGICAL :: verbose

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando
idp = 0
inp_fmt = 1
version=2005

DO kp = 1,HUGE(kp)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "") THEN  
    EXIT
  ELSE IF (TRIM(chdum) == "-v") THEN  
    verbose = .TRUE.
  ELSE IF (TRIM(chdum) == "-V2003") THEN
    version=2003
  ELSE IF (TRIM(chdum) == "-V2004") THEN
    version=2004
  ELSE IF (TRIM(chdum) == "-lu") THEN
    inp_fmt = 2  
  ELSE IF (TRIM(chdum) == "-bio") THEN
    inp_fmt = 3  
  ELSE IF (TRIM(chdum) == "-metg") THEN
    inp_fmt = 4  
  ELSE IF (TRIM(chdum) == "-meta") THEN
    inp_fmt = 5  
  ELSE IF (TRIM(chdum) == "-ini") THEN
    inp_fmt = 6  
  ELSE IF (TRIM(chdum) == "-eminv") THEN
    inp_fmt = 7  
  ELSE IF (TRIM(chdum) == "-latbc") THEN
    inp_fmt = 8  
  ELSE IF (TRIM(chdum) == "-topbc") THEN
    inp_fmt = 9  
  ELSE
    idp = idp + 1
    arg(idp) = chdum
  ENDIF
ENDDO

! N.ro di varibiali 2D e 3D nel file METEO
IF (inp_fmt == 4) THEN
  IF (version == 2003) THEN
    nvar3d = 6
    nvar2d = 7
  ELSE IF (version == 2004) THEN  
    nvar3d = 7
    nvar2d = 7
  ELSE IF (version == 2005) THEN  
    nvar3d = 8
    nvar2d = 9
  ENDIF
ELSE IF (inp_fmt == 5) THEN
  IF (version == 2003) THEN
    nvar3d = 7
    nvar2d = 8
  ELSE IF (version == 2004) THEN  
    nvar3d = 8
    nvar2d = 8
  ELSE IF (version == 2005) THEN  
    nvar3d = 8
    nvar2d = 9
  ENDIF
ENDIF

filein = arg(1)
fileout = arg(2)
fileinfo = arg(3)
READ (arg(4),*,IOSTAT=ios) igen

IF (filein == "" .OR. fileout == "" .OR. fileinfo == "" .OR. &
    ios /= 0 .OR. TRIM(filein) == "-h") THEN
  CALL write_help
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.2 Leggo informazioni da fileinfo
OPEN (UNIT=30, FILE=fileinfo, STATUS="OLD", ERR=9999)

k = 0
DO
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT

! Skippo righe vuote e di commento
  chrec = ADJUSTL(chrec)
  IF (chrec(1:1) == "!" .OR. TRIM(chrec) == "") CYCLE
  k = k+1

! Interpreto la riga letta
  SELECT CASE (k)
  CASE (1)
    READ (chrec,*,IOSTAT=ios) cem
  CASE (2)
    READ (chrec,'(a)',IOSTAT=ios) proj
  CASE (3)
    READ (chrec,*,IOSTAT=ios) x1,y1
  CASE (4)
    READ (chrec,*,IOSTAT=ios) x2,y2
  CASE (5)
    READ (chrec,*,IOSTAT=ios) xrot,yrot
  CASE (6)
    READ (chrec,*,IOSTAT=ios) nx,ny
  CASE (7)
    READ (chrec,*,IOSTAT=ios) nlev
  CASE (8)
    READ (chrec,*,IOSTAT=ios) lev_out(1:nlev)
  CASE (9)
    READ (chrec,*,IOSTAT=ios) scad_ini

  CASE (10:maxvar+nhead)
    READ (chrec,*,IOSTAT=ios) code_var(k-nhead), tab_var(k-nhead)
    
  CASE DEFAULT
    WRITE (*,*) "Troppe specie, elaboro le prime ",maxvar,&
      " (aumentare param. maxvar)"
    k = k-1
    EXIT

  END SELECT

  IF (ios /= 0) GOTO 9998

ENDDO
CLOSE(30)

! Controlli e calcolo grandezze derivate
IF (cem <= 0 .OR. nlev < 0 .OR. & 
    x1 >= x2 .OR. y1 >= y2 .OR. nx <= 1 .OR. ny <= 1  .OR. &
    (nlev==0 .AND. inp_fmt/=2 .AND. inp_fmt/=3) .OR. &
    inp_fmt < 1 .OR. inp_fmt > 9 .OR. &
    inp_fmt == 9 .AND. nlev /= 1) GOTO 9997

SELECT CASE (inp_fmt)
CASE (1,4,5,6,7,8,9)          ! output,metg,meta,ini,eminv,latbc,topbc
  nvar = k - nhead
  nvar_out = COUNT(code_var(1:nvar) > 0)
CASE (2)                      ! LANDUSE
  nvar = 9
  nvar_out = 10
  nlev = 1
CASE (3)                      ! BIOGENIC
  nvar = 5
  nvar_out = 6
  nlev = 1
END SELECT
IF ((inp_fmt == 4 .OR. inp_fmt == 5) .AND. nvar /= nvar3d+nvar2d) GOTO 9993

dx = (x2-x1)/REAL(nx-1)
dy = (y2-y1)/REAL(ny-1)

! Se formato BC, stendo i 4 bordi su un vettore 1D ...
IF (inp_fmt == 8) THEN
  nxi = nx
  nyi = ny
  nx = (nxi+nyi)*2 + 4
  ny = 1
  x2 = x1 + (nx-1)*dx
  y2 = y1
ENDIF

np = nx*ny

WRITE (*,'(a,3i5,2f10.5)') "Parametri griglia (nx,ny,nz,dx,dy) ", &
  nx,ny,nlev,dx,dy
WRITE (*,'(2(a,i3))') "Specie da leggere: ",nvar," da scrivere: ",nvar_out
WRITE (*,'(2(a,i3))') "Livelli in output: ",COUNT(lev_out(1:nlev) /= 0)
WRITE (*,*) 

!--------------------------------------------------------------------------
! 1.3) Definisco la parte costante dell'header dei grib

! Sezione 1
ksec1(2) = cem
ksec1(3) = igen
ksec1(4) = 255
IF (inp_fmt == 8) THEN
  ksec1(5) = 192
ELSE
  ksec1(5) = 128
ENDIF

ksec1(15) = 1
ksec1(17) = 0
ksec1(18) = 0
ksec1(19) = 0
ksec1(20) = 0
ksec1(22:) = 0

! Sezione 2
ksec2(2) = nx
ksec2(3) = ny
ksec2(4) = NINT(y1 * 1000.)
ksec2(5) = NINT(x1 * 1000.)
ksec2(7) = NINT(y2 * 1000.)
ksec2(8) = NINT(x2 * 1000.)
ksec2(11)=64
ksec2(12)=0

IF (UPPERCASE(proj) == "UTM" .OR. (xrot == 0. .AND. yrot == 0.)) THEN
  ksec2(1) = 0
  ksec2(6) = 128
  ksec2(9) = NINT(dx * 1000.)
  ksec2(10) = NINT(dy * 1000.)
  ksec2(13) = 0
  ksec2(14) = 0

ELSE IF (UPPERCASE(proj) == "GEO" .AND. (xrot /= 0. .OR. yrot /= 0.)) THEN
  ksec2(1) = 10
  ksec2(6) = 0
  ksec2(9) = 0
  ksec2(10) = 0
  ksec2(13) = NINT((yrot-90.) * 1000.)
  ksec2(14) = NINT(xrot * 1000.)

ELSE
  WRITE (*,*) "Errore, proiezione non gestita ",proj
  STOP

ENDIF

DO k=15,22
  ksec2(k)=0
ENDDO

! Altre sezioni
DO k=1,11
  psec2(k)=0.
ENDDO
ksec3(1) = 0
psec3(2) = rmis
ksec4(1) = np
ksec4(2) = nbit
DO k=3,33
  ksec4(k) = 0.
ENDDO

!--------------------------------------------------------------------------
! 1.4 Altre operazioni

! Alloco le varaibili
SELECT CASE (inp_fmt)

CASE (1,4,5,9)
  ALLOCATE (conc(np,nlev,nvar))

CASE (2,3)
  ALLOCATE (conc(np,nlev,nvar))
  ALLOCATE (tot(np))

CASE (6) 
  ALLOCATE (conc2p(np,nlev,nvar))

CASE (7) 
  ALLOCATE (conc(np,nlev,24))

CASE (8)
  ALLOCATE (conc(np-4,nlev,nvar))
  ALLOCATE (conc_miss(np,nlev,nvar))

END SELECT

! Apro i files
SELECT CASE (inp_fmt)
CASE (1,4,5,6,8,9)
  OPEN (UNIT=31, FILE=filein, STATUS="OLD", FORM="UNFORMATTED", ERR=9996)
CASE (2,3,7)
  OPEN (UNIT=31, FILE=filein, STATUS="OLD", FORM="FORMATTED", ERR=9996)
END SELECT

CALL PBOPEN (iu,fileout,'W',kret)

! Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! Trovo codice per EOF
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 2) Lettura e scrittura: 

!--------------------------------------------------------------------------
! 2.1) Formato OUT.sim (ciclo sui record in input - scadenze orarie)

IF (inp_fmt == 1) THEN

  cnt_grb = 0
  DO kscad = 1, HUGE(kscad)

    READ (31,IOSTAT=ios) idata, &
      (((conc(k,klev,kvar),k=1,np),klev=1,nlev),kvar=1,nvar)
    IF (ios == eof) THEN
      EXIT
    ELSE IF (ios /= 0) THEN
      GOTO 9995
    ENDIF

    IF (kscad == 1) idata_ini = idata
    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    DO kvar = 1,nvar

      IF (code_var(kvar) <= 0) CYCLE
      ksec1(1) = tab_var(kvar)
      ksec1(6) = code_var(kvar)

      DO klev = 1,nlev
        IF (lev_out(klev) == 0) CYCLE
        ksec1(7) = 109
        ksec1(8) = klev
        ksec1(9) = 0

        CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
          conc(1:np,klev,kvar),np,kbuffer,maxdim,kword,'C',kret)

        CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
        IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

        cnt_grb = cnt_grb + 1

      ENDDO                ! livelli
    ENDDO                  ! specie
  ENDDO                    ! scadenze

  WRITE (*,'(a,i3,a,i6,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
    cnt_grb," grib"

!--------------------------------------------------------------------------
! 2.2) Formato LANDUSE

ELSE IF (inp_fmt == 2) THEN

  DO k = 1,np
    READ (31,*,IOSTAT=ios) conc(k,1,1:9)
    IF (ios /= 0) GOTO 9995
  ENDDO
  tot(1:np) = REAL( MAXLOC(conc(1:np,1,1:9),DIM=2) )

  ksec1(7) = 1
  ksec1(8) = 0
  ksec1(9) = 0

  ksec1(21) = 20
  ksec1(10) = 100
  ksec1(11) = 1
  ksec1(12) = 1
  ksec1(13) = 1

  ksec1(16) = 0

! Scrivo le frazioni dei vari tipi di LU
  DO kvar = 1,nvar
    ksec1(1) = 200
    ksec1(6) = 130 + kvar

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
      conc(1:np,1,kvar),np,kbuffer,maxdim,kword,'C',kret)
    CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
  ENDDO

! Scrivo il LU prevalente
  ksec1(1) = 200
  ksec1(6) = 121
  ksec4(2) = 24

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    tot(1:np),np,kbuffer,maxdim,kword,'C',kret)
  CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

  WRITE (*,*) "Scritti ",nvar+1," grib"

!--------------------------------------------------------------------------
! 2.3) Formato BIOGENIC

ELSE IF (inp_fmt == 3) THEN

  DO k = 1,np
    READ (31,*,IOSTAT=ios) conc(k,1,1:5)
    IF (ios /= 0) GOTO 9995
  ENDDO
  tot(1:np) = SUM(conc(1:np,1,1:5),DIM=2)

  ksec1(7) = 1
  ksec1(8) = 0
  ksec1(9) = 0

  ksec1(21) = 20
  ksec1(10) = 100
  ksec1(11) = 1
  ksec1(12) = 1
  ksec1(13) = 1

  ksec1(16) = 0

! Scrivo i potenziali di emissione biogenica
  DO kvar = 1,nvar
    ksec1(1) = 200
    ksec1(6) = 130 + kvar

    CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
      conc(1:np,1,kvar),np,kbuffer,maxdim,kword,'C',kret)
    CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
    IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret
  ENDDO

! Scrivo il potenziale di emissione totale
  ksec1(1) = 200
  ksec1(6) = 136
  ksec4(2) = 24

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    tot(1:np),np,kbuffer,maxdim,kword,'C',kret)
  CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

  WRITE (*,*) "Scritti ",nvar+1," grib"

!--------------------------------------------------------------------------
! 2.4 + 2.5) Formato METEO

ELSE IF (inp_fmt == 4 .OR. inp_fmt == 5) THEN

  cnt_grb = 0
  DO kscad = 1, HUGE(kscad)

    READ (31,IOSTAT=ios) idata, &
      (((conc(k,klev,kvar),k=1,np),klev=1,nlev),kvar=1,nvar3d), &
       ((conc(k,1,kvar),k=1,np),kvar=nvar3d+1,nvar)
    IF (ios == eof) THEN
      EXIT
    ELSE IF (ios /= 0) THEN
      GOTO 9995
    ENDIF

    IF (kscad == 1) idata_ini = idata
    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    DO kvar = 1,nvar

      IF (code_var(kvar) <= 0) CYCLE
      IF (kvar <= nvar3d) THEN
        nl = nlev
      ELSE
        nl = 1
      ENDIF
      ksec1(1) = tab_var(kvar)
      ksec1(6) = code_var(kvar)

      DO klev = 1,nl
        IF (lev_out(klev) == 0) CYCLE
        IF (kvar <= nvar3d) THEN
          ksec1(7) = 109
          ksec1(8) = klev
          ksec1(9) = 0
        ELSE
          ksec1(7) = 1
          ksec1(8) = 0
          ksec1(9) = 0
        ENDIF

        CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
          conc(1:np,klev,kvar),np,kbuffer,maxdim,kword,'C',kret)

        CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
        IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

        cnt_grb = cnt_grb + 1

      ENDDO                ! livelli
    ENDDO                  ! specie
  ENDDO                    ! scadenze

  WRITE (*,'(a,i3,a,i6,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
    cnt_grb," grib"

!--------------------------------------------------------------------------
! 2.6) Formato INI / END

ELSE IF (inp_fmt == 6) THEN

  ksec1(10) = 100
  ksec1(11) = 1
  ksec1(12) = 1
  ksec1(13) = 0
  ksec1(14) = 0
  ksec1(21) = 20

  ksec1(16) = 0

  cnt_grb = 0
  DO kvar = 1,nvar

    READ (31,IOSTAT=ios) ii,((conc2p(k,klev,kvar),k=1,np),klev=1,nlev)
    IF (ios /= 0) GOTO 9995

    IF (code_var(kvar) <= 0) CYCLE
    ksec1(1) = tab_var(kvar)
    ksec1(6) = code_var(kvar)

    DO klev = 1,nlev
      IF (lev_out(klev) == 0) CYCLE
      ksec1(7) = 109
      ksec1(8) = klev
      ksec1(9) = 0

      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
        conc(1:np,klev,kvar),np,kbuffer,maxdim,kword,'C',kret)

      CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
      IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

      cnt_grb = cnt_grb + 1

    ENDDO                ! livelli
  ENDDO                  ! specie

  WRITE (*,'(a,i6,a)') "Scritti ",cnt_grb," grib"

!--------------------------------------------------------------------------
! 2.7) Formato Emission Inventory

ELSE IF (inp_fmt == 7) THEN

  slen = LEN(TRIM(filein))
  READ (filein(slen-1:slen),'(i2)',IOSTAT=ios) mm
  IF (ios /= 0 .OR. mm < 1 .OR. mm > 12) mm = 1

  cnt_grb = 0
  DO kvar = 1,nvar

!   Skip specie non richieste
    IF (code_var(kvar) <= 0) THEN
      DO k = 1,np*3
      READ (31,*,ERR=9995,END=9995) 
      ENDDO
      CYCLE
    ELSE
      IF (verbose) WRITE (*,'(a,i3)') "Elaboro specie: ",kvar
      ksec1(1) = tab_var(kvar)
      ksec1(6) = code_var(kvar)
    ENDIF

!   Elaboro i grib relativi a ciascuna specie / tipo di giorno 
!   (clclo interno su ore e livelli)
    DO kday = 1,3

      DO k = 1,np
        READ (31,*,IOSTAT=ios) ((conc(k,klev,kh),kh=1,24),klev=1,nlev)
        IF (ios /= 0) GOTO 9995
      ENDDO

      ksec1(10) = 100
      ksec1(11) = mm
      ksec1(12) = kday
      ksec1(14) = 0
      ksec1(21) = 20

      ksec1(16) = 0

      DO klev = 1,nlev
        IF (lev_out(klev) == 0) CYCLE
        DO kh = 1,24

          ksec1(7) = 109
          ksec1(8) = klev
          ksec1(9) = 0
          ksec1(13) = kh

          CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
            conc(1:np,klev,kh),np,kbuffer,maxdim,kword,'C',kret)
          CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
          IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

          cnt_grb = cnt_grb + 1
        ENDDO                ! ore
      ENDDO                  ! livelli

    ENDDO                    ! tipi di giorno
  ENDDO                      ! specie

  WRITE (*,'(a,i6,a)') "Scritti ",cnt_grb," grib"

!--------------------------------------------------------------------------
! 2.8) Formato Lateral Boundary conditions

ELSE IF (inp_fmt == 8) THEN

  cnt_grb = 0
  DO kscad = 1, HUGE(kscad)

    READ (31,IOSTAT=ios) idata, &
      (((conc(k,klev,kvar),kvar=1,nvar),k=1,np-4),klev=1,nlev)
    IF (ios == eof) THEN
      EXIT
    ELSE IF (ios /= 0) THEN
      GOTO 9995
    ENDIF

!   Inserisco valori mancanti negli angoli della cornice delle BC
!   nxi,nyi
    conc_miss(1:nyi,:,:) = conc(1:nyi,:,:)                              ! W
    conc_miss(nyi+1,:,:) = rmis
    conc_miss(nyi+2:2*nyi+1,:,:) = conc(nyi+1:2*nyi,:,:)                ! E
    conc_miss(2*nyi+2,:,:) = rmis
    conc_miss(2*nyi+3:2*nyi+nxi+2,:,:) = conc(2*nyi+1:2*nyi+nxi,:,:)    ! S
    conc_miss(2*nyi+nxi+3,:,:) = rmis
    conc_miss(2*nyi+nxi+4:2*nyi+2*nxi+3,:,:) = &
      conc(2*nyi+nxi+1:2*nyi+2*nxi,:,:)                                 ! N
    conc_miss(2*nyi+2*nxi+4,:,:) = rmis

    IF (kscad == 1) idata_ini = idata
    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    DO kvar = 1,nvar

      IF (code_var(kvar) <= 0) CYCLE
  
      ksec1(1) = tab_var(kvar)
      ksec1(6) = code_var(kvar)

      DO klev = 1,nlev
        IF (lev_out(klev) == 0) CYCLE
        ksec1(7) = 109
        ksec1(8) = klev
        ksec1(9) = 0

        CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
          conc_miss(1:np,klev,kvar),np,kbuffer,maxdim,kword,'C',kret)

        CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
        IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

        cnt_grb = cnt_grb + 1

      ENDDO                ! livelli
    ENDDO                  ! specie
  ENDDO                    ! scadenze

  WRITE (*,'(a,i3,a,i6,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
    cnt_grb," grib"

!--------------------------------------------------------------------------
! 2.9) Formato Top BC

ELSE IF (inp_fmt == 9) THEN

  cnt_grb = 0
  DO kscad = 1, HUGE(kscad)

    READ (31,IOSTAT=ios) idata, &
      ((conc(k,1,kvar),kvar=1,nvar),k=1,np)
    IF (ios == eof) THEN
      EXIT
    ELSE IF (ios /= 0) THEN
      GOTO 9995
    ENDIF

    IF (kscad == 1) idata_ini = idata
    CALL chdata2ksec(idata,idata_ini,scad_ini,verbose, &
      ksec1(10),ksec1(11),ksec1(12),ksec1(13),ksec1(16),ksec1(21))

    DO kvar = 1,nvar

      IF (code_var(kvar) <= 0) CYCLE
  
      ksec1(1) = tab_var(kvar)
      ksec1(6) = code_var(kvar)

      DO klev = 1,nlev
        IF (lev_out(klev) == 0) CYCLE
        ksec1(7) = 109
        ksec1(8) = klev
        ksec1(9) = 0

        CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
          conc(1:np,klev,kvar),np,kbuffer,maxdim,kword,'C',kret)

        CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
        IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

        cnt_grb = cnt_grb + 1

      ENDDO                ! livelli
    ENDDO                  ! specie
  ENDDO                    ! scadenze

  WRITE (*,'(a,i3,a,i6,a)') "Elaborate ",kscad-1," scadenze, scritti ", &
    cnt_grb," grib"

ENDIF

CALL PBCLOSE (iu,kret)

!--------------------------------------------------------------------------
! 3) Rileggo il 1o record di filein per verificare se contiene altri dati

SELECT CASE (inp_fmt) 
CASE (1,8,9)
  CLOSE (31)
  OPEN (UNIT=31, FILE=filein, STATUS="OLD", FORM="UNFORMATTED")
  READ (31,IOSTAT=ios) idata,conc(:,:,:),rdum
  IF (ios == 0) WRITE (*,*) "Warning: il file ",TRIM(filein), &
    " contiene altri dati!"
  CLOSE(31)

CASE (2,3,7)
  READ (31,*,IOSTAT=ios) rdum
  IF (ios == 0) WRITE (*,*) "Warning: il file ",TRIM(filein), &
    " contiene altri dati!"
  CLOSE(31)

CASE (4,5)
  CLOSE (31)
  OPEN (UNIT=31, FILE=filein, STATUS="OLD", FORM="UNFORMATTED")
  READ (31,IOSTAT=ios) idata,conc(:,:,1:nvar3d),conc(:,1,nvar3d+1:),rdum
  IF (ios == 0) WRITE (*,*) "Warning: il file ",TRIM(filein), &
    " contiene altri dati!"
  CLOSE(31)
  
CASE (6)
  CLOSE (31)
  OPEN (UNIT=31, FILE=filein, STATUS="OLD", FORM="UNFORMATTED")
  READ (31,IOSTAT=ios) idata,conc2p(:,:,:),rdum
  IF (ios == 0) WRITE (*,*) "Warning: il file ",TRIM(filein), &
   " contiene altri dati!"
  CLOSE(31)

END SELECT

STOP

!--------------------------------------------------------------------------
! 4) Gestione erori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileinfo)
STOP

9998 CONTINUE
WRITE (*,*) "Record illegale o mal posizionato in ",TRIM(fileinfo)
WRITE (*,*) TRIM(chrec)
STOP

9997 CONTINUE
WRITE (*,*) "Parametri illegali leggendo ",TRIM(fileinfo)
STOP

9996 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9995 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP

9993 CONTINUE
WRITE (*,'(3a,i3,a)') "Numero specie illegale in ",TRIM(fileinfo), &
  " (attese ",nvar2d+nvar3d,")"
STOP

9992 CONTINUE
WRITE (*,*) "Date inconsistenti in ",TRIM(filein)
STOP

END PROGRAM chimere2grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE chdata2ksec(idata,idata_ini,scad_ini,verbose, &
  ksec10,ksec11,ksec12,ksec13,ksec16,ksec21)
!--------------------------------------------------------------------------
! Data una data nel formato intero di chimere (YYYYMMDDHH), ritorna gli 
! elementi relativi a data e scadenza nella sezione 1 del grib.
!--------------------------------------------------------------------------
USE date_handler

IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: idata       ! data/ora letta dal record corrente
INTEGER, INTENT(IN) :: idata_ini   ! data/ora del 1o record del file
INTEGER, INTENT(IN) :: scad_ini
LOGICAL, INTENT(IN) :: verbose
INTEGER, INTENT(OUT) :: ksec10,ksec11,ksec12,ksec13,ksec16,ksec21

! Variabli locali
TYPE(date) :: datac,data_ini,data_emi
INTEGER :: hhc,hh_ini,hh_emi,scadc,bck_day
CHARACTER (LEN=10) :: chdata

!--------------------------------------------------------------------------

! Leggo data/ora corrente
WRITE (chdata,'(i10.10)') idata
READ (chdata,'(i4,3i2)',ERR=9999) datac%yy,datac%mm,datac%dd,hhc

IF (verbose) WRITE (*,'(a,i5,3i3)') "Elaboro istante: ", &
  datac%yy,datac%mm,datac%dd,hhc

! Richiedo tutte analisi
IF (scad_ini < 0) THEN

  ksec10 = 1 + MOD(datac%yy-1,100)
  ksec21 = 1 + (datac%yy-1)/100
  ksec11 = datac%mm
  ksec12 = datac%dd
  ksec13 = hhc
  ksec16 = 0

! Richiedo previsioni
ELSE

! Calcolo la data di emissione
  WRITE (chdata,'(i10.10)') idata_ini
  READ (chdata,'(i4,3i2)',ERR=9999) &
    data_ini%yy,data_ini%mm,data_ini%dd,hh_ini
  hh_emi = hh_ini - scad_ini
  IF (hh_emi >= 0) THEN
    data_emi = data_ini
  ELSE
    bck_day = (-1-hh_emi)/24 + 1
    data_emi = data_ini - bck_day
    hh_emi = hh_emi + bck_day * 24
  ENDIF

! Calcolo la scadenza di previsione
  scadc = hhc - hh_emi + 24 * (datac - data_emi)
  IF (verbose) WRITE (*,'(a,i5,3i3,a,i3)') "Codifico scadenza: ", &
    data_emi%yy,data_emi%mm,data_emi%dd,hh_emi," +",scadc

! Definisco la sezione1 GRIB
  ksec10 = 1 + MOD(data_emi%yy-1,100)
  ksec21 = 1 + (data_emi%yy-1)/100
  ksec11 = data_emi%mm
  ksec12 = data_emi%dd
  ksec13 = hh_emi
  ksec16 = scadc

ENDIF

RETURN

9999 CONTINUE
WRITE (*,*) "Data illegale in file input: ",idata
STOP

END SUBROUTINE chdata2ksec

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: chimere2grib.exe filein fileout fileinfo igen [-V2003] [-v2004] [-v]"
WRITE (*,*) "     [-lu / -bio / -metg / -meta / -ini / -eminv / -latbc]"
WRITE (*,*)
WRITE (*,*) "  con 4 parametri analizza un file col tracciato standard chimere: "
WRITE (*,*) "    un record per ora, con data e matrice 4d (X,Y,Z,t,specie)." 
WRITE (*,*) "    Questo tracciato vale per i ifles: out.sim, par.sim, AEMISSIONS,"
WRITE (*,*) "    BEMISSIONS, LAT_CONCS, TOP_CONCS, par_2D, par_3D"
WRITE (*,*)
WRITE (*,*) "  -lu:   analizza un file LANDUSE (record senza data; calcola tipo prev.)"
WRITE (*,*) "  -bio:  analizza un file BIOFACS (calcola potenziale totale)"
WRITE (*,*) "  -metg: analizza un file METEO, modello gas (record con grandezze 3d e 2d)"
WRITE (*,*) "  -meta: analizza un file METEO, modello aerosol"
WRITE (*,*) "  -ini:  analizza un file ini.sim, end.sim (un record per ogni specie 3d)"
WRITE (*,*) "  -eminv:analizza un file EMISSIONS-area.mm (profili giornalieri di emiss.)"
WRITE (*,*) "  -latbc:analizza un file LAT_CONCS"
WRITE (*,*) "  -topbc:analizza un file TOP_CONCS (mettere nz = 1)"
WRITE (*,*) ""
WRITE (*,*) "  -V2003:legge i files della versione Chimere 200310F (solo per METEO)"
WRITE (*,*) "  -V2004:legge i files della versione Chimere 200410A (solo per METEO)"
WRITE (*,*) "  -v:    verbose (log a schermo esteso)"
WRITE (*,*) ""
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_eof_eor(eof, eor)
!-------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!-------------------------------------------------------------------------
IMPLICIT NONE

INTEGER, INTENT(OUT) :: eof,eor

INTEGER :: k, ios, idummy=0, iun=0
LOGICAL :: l1 = .TRUE.


! Cerco un'unita' libera per aprire il file di prova
DO k = 10,99
  INQUIRE (UNIT=k, OPENED=l1, IOSTAT=ios)
  IF (.NOT. l1 .AND. ios==0) THEN
    iun = k
    EXIT
  ENDIF
ENDDO
IF (iun == 0) GOTO 9999   ! non ho torvato nessuna unita' libera
!WRITE (*,*) "uso unita ",iun

! Cerco codice di errore per EOF
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
ENDFILE (k)
REWIND (k)
READ (k,*,IOSTAT=eof)
CLOSE(k)

! Cerco codice di errore per EOR
OPEN (unit=k, STATUS="SCRATCH", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
  PAD="NO", ERR=9999)
WRITE (k,'(a1)') "1" 
WRITE (k,'(a1)') "2"
REWIND (k)
READ (k,'(i1)',ADVANCE="NO",ERR=9999) idummy
READ (k,'(i1)',ADVANCE="NO",IOSTAT=eor) idummy
CLOSE(k)

!write (*,*) "eof,eor ",eof,eor
RETURN

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine get_eof_eor, usero' valori di default"
eof = -1
eor = -2
RETURN

END SUBROUTINE get_eof_eor

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION lowercase(chin) RESULT (chout)
!
! Replace uppercase letters with lowercase and takes off trailing blanks
! Non-literal characters are left unchanged.
!
IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN) :: chin
CHARACTER (LEN=LEN_TRIM(chin)) :: chout
!
INTEGER :: i,l
CHARACTER (LEN=26), PARAMETER :: &
upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
lower='abcdefghijklmnopqrstuvwxyz'

!--------------------------------------------------------------------------

chout=TRIM(chin)
DO i=1,LEN(chout)
  l=INDEX(upper,chin(i:i))
  IF (l == 0) THEN
    chout(i:i) = chin(i:i)
  ELSE
    chout(i:i) = lower(l:l)
  ENDIF
ENDDO

END FUNCTION lowercase

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION uppercase(chin) RESULT (chout)
!
! Replace lowercase letters with uppercase and takes off trailing blanks
! Non-literal characters are left unchanged.
!
IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN) :: chin
CHARACTER (LEN=LEN_TRIM(chin)) :: chout
!
INTEGER :: i,l
CHARACTER (LEN=26), PARAMETER :: &
upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
lower='abcdefghijklmnopqrstuvwxyz'

!--------------------------------------------------------------------------

chout=TRIM(chin)
DO i=1,LEN(chout)
  l=INDEX(lower,chin(i:i))
  IF (l == 0) THEN
    chout(i:i) = chin(i:i)
  ELSE
    chout(i:i) = upper(l:l)
  ENDIF
ENDDO

END FUNCTION uppercase

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

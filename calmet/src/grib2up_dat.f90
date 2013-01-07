PROGRAM grib2up_dat
!--------------------------------------------------------------------------
! Programma della catena Calmet
! Estrae da un pacco grib i files up*.dat richiesti per un run calmet di
! un giorno
! Uso: grib2up_dat.exe grib_3d file_pts file_srq [-10m grib_10m] [-test]
!
! Note:
! - Al momento, il programma gestisce solo le analisi LAMA
! - Per ottimizzare l'esecuzione, i grib devono essere ordinati:
!   1) per ora di validita' (25 scadenze)
!   2) per livello (dall'alto)
!   3) per parametro (P,T,U,V)
! - L'ordine dei punti deve essere lo stesso in file_pts e file_srq
! - Tracciato file di output (up.dat, esclusi headers): 
!   pp(mb), zz(m), tt(K), dd(grd), ff(m/s)
!
!                                         Versione 2.0.0, Enrico 02/08/2012
!--------------------------------------------------------------------------
USE file_utilities
USE seriet_utilities
USE date_handler
IMPLICIT NONE

! Parametri costanti
REAL, PARAMETER :: rdry = 287.
REAL, PARAMETER :: g = 9.8
REAL, PARAMETER :: rmis = -9999.     ! dati mancanti nei files seriet
INTEGER, PARAMETER :: iu0 = 30       ! 1a unita per apertura file di ouptut
INTEGER, PARAMETER :: idvar(4) = (/1,11,33,34/) ! codifica grib di P,T,U,V

! codifica grib livelli superficiali
INTEGER, PARAMETER :: idlevsup(4,3) = &
  RESHAPE((/1,105,105,105,0,2,10,10,0,0,0,0/),(/4,3/))

! Dichiarazioni per GRIBEX.
INTEGER, PARAMETER :: maxdim = 100000
INTEGER :: ksec2_first(1024)
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)
INTEGER :: iuz,iu3d,iusup,ksec2_sav(1024)

! Funzioni
REAL :: dir,mag

! Arrays dinamici
REAL, ALLOCATABLE :: orog(:),zz(:,:),var3d(:,:,:)
INTEGER, ALLOCATABLE :: idx(:)

! Altre variabili del programma
TYPE (csv_record) :: csvline
TYPE(date) :: data1,data2,datac
REAL :: dd,ff,rdum,lon,lat
INTEGER :: np,nl,nl2,nh,idlayb,idlayt,hh1,hh2,hhc
INTEGER :: cnt_par,idum,iz,hht,data(3),ora(2),scad(4),level(3),var(3)
INTEGER :: cnt_rew,cnt_rew2,cnt_mis(3)
INTEGER :: ios,ios4(4),eof,eor,ier,ier6(6),iret
INTEGER :: k,kl,kp,kh,kv,krec,kpar,k1,k2,idl,ii,jj,nf
CHARACTER(LEN=80) :: file_3d,file_z,file_sup,file_pts,file_srq,chpar
CHARACTER(LEN=80) :: fileout,chdum,lab,chrec
CHARACTER(LEN=3) :: next_arg
LOGICAL :: ltest,l10m

!==========================================================================
! 1) Preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comandi etc.

ltest = .FALSE.
l10m = .FALSE.

next_arg = ""
cnt_par = 0
ios4(:) = 0
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)
  IF (chpar == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP
  ELSE IF (TRIM(chpar) == "-test") THEN
    ltest = .TRUE.
  ELSE IF (TRIM(chpar) == "-10m") THEN
    next_arg = "10m"
    l10m = .TRUE.
  ELSE IF (next_arg == "10m") THEN
    file_sup = chpar
    next_arg = ""
  ELSE
    cnt_par = cnt_par + 1
    SELECT CASE (cnt_par)
    CASE (1)
      file_3d = chpar
    CASE (2)
      file_z = chpar
    CASE (3)
      file_pts = chpar
    CASE (4)
      file_srq = chpar
    CASE (5)
      READ(chpar,*,IOSTAT=ios4(1)) idlayb
    CASE (6)
      READ(chpar,*,IOSTAT=ios4(2)) idlayt
    CASE (7)
      READ(chpar,'(i4,3i2)',IOSTAT=ios4(3)) data1%yy,data1%mm,data1%dd,hh1
    CASE (8)
      READ(chpar,*,IOSTAT=ios4(4)) nh
    CASE DEFAULT
      CALL scrive_help
      STOP
    END SELECT
  ENDIF
  IF (ANY(ios4(1:4) /= 0)) THEN
    CALL scrive_help
    STOP
  ENDIF
ENDDO

IF (cnt_par /= 8 .OR. idlayb < 0 .OR. idlayt < 0 .OR. idlayb < idlayt) THEN
  CALL scrive_help
  STOP
ENDIF  

! Calcoli dipendenti dai parametri richiesti
nl2 = idlayb - idlayt + 1
IF (l10m) THEN
  nl = nl2 + 1
ELSE
  nl = nl2
ENDIF

hht = hh1 + nh - 1
data2 = data1 + hht/24
hh2 = MOD(hht,24)

! Codice EOF
CALL get_eof_eor(eof,eor)

! Disabilito controlli GRIBEX
CALL grsvck(0)

!--------------------------------------------------------------------------
! 1.2 Leggo da file_pts il numero di punti richiesto e alloco gli array

! Numero di punti richiesti
OPEN (UNIT=20, FILE=file_pts, STATUS="OLD", ACTION="READ", ERR=9999)
READ (20,'(a)',IOSTAT=ios) chrec
CALL test_header("lspts",chrec,iret)
IF (iret > 0) GOTO 9998

np = 0
DO k = 1,HUGE(0)
  READ (20,*,IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  np = np + 1
ENDDO
CLOSE(20)
WRITE (*,'(a,i3,a,i2,2(a,i4.4,3i2.2))') "Dati richiesti: ",np," punti ", &
  nl," livelli, da ", data1%yy,data1%mm,data1%dd,hh1," a ", &
  data2%yy,data2%mm,data2%dd,hh2
IF (np > 100) WRITE (*,*) &
  "Warning, con piu' di 100 punti il programma potrebbe funzionare male"

! Alloco gli array
ALLOCATE (idx(np))
ALLOCATE (zz(nl,np),orog(np),var3d(4,nl,np))

!--------------------------------------------------------------------------
! 1.3 Leggo da file_pts gli indici dei punti richiesti

OPEN (UNIT=20, FILE=file_pts, STATUS="OLD", ACTION="READ", ERR=9999)
READ (20,*)
DO k = 1,np
  READ (20,'(a)',IOSTAT=ios) chrec
  CALL init(csvline,RECORD=chrec,NFIELD=nf)
  IF (nf < 6) GOTO 9998
  CALL csv_record_getfield(csvline,FIELD=lon,IER=ier6(1))
  CALL csv_record_getfield(csvline,FIELD=lat,IER=ier6(2))
  CALL csv_record_getfield(csvline,FIELD=lab,IER=ier6(3))
  CALL csv_record_getfield(csvline,FIELD=ii,IER=ier6(4))
  CALL csv_record_getfield(csvline,FIELD=jj,IER=ier6(5))
  CALL csv_record_getfield(csvline,FIELD=idx(k),IER=ier6(6))
  CALL delete(csvline)
  IF (ANY(ier6(1:6) /= 0)) GOTO 9998
ENDDO
CLOSE(20)

!--------------------------------------------------------------------------
! 1.4 Leggo da file_srq label e quota dei punti richiesti

OPEN (UNIT=21, FILE=file_srq, STATUS="OLD", ACTION="READ", ERR=9997)
READ (21,*)
DO k = 1,np
  READ (21,'(2x,i5,40x,i4)') idum,iz
  IF (ios /= 0) GOTO 9996
  IF (idum /= idx(k)) GOTO 9995
  orog(k) = REAL(iz)
ENDDO
CLOSE(21)

!--------------------------------------------------------------------------
! 1.5 Leggo da file_z la quota dei layers, per ciascun punto. L'array zz e' 
! dimensionato (nl,np) e contiene le quote dei livelli, ordinati dall'alto.

zz(:,:) = rmis
IF (l10m) zz(nl,1:np) = orog(1:np) + 10.

CALL PBOPEN (iuz,file_z,'R',kret)
IF (kret.ne.0) GOTO 9994

DO 
  CALL PBGRIB(iuz,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    GOTO 9993
  ENDIF

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
  IF (ksec1(6) /= 8 .OR. ksec1(7) /= 110 .OR. ksec1(9) /= ksec1(8) + 1 .OR. &
      ksec1(8) > idlayb .OR. ksec1(8) < idlayt) CYCLE

  kl = ksec1(8) - idlayt + 1
  zz(kl,1:np) = field(idx(1:np))
ENDDO

IF (ANY(zz(:,:) == rmis)) GOTO 9992
ksec2_sav(:) = ksec2(:)

!--------------------------------------------------------------------------
! 1.6 Apro i grib di input 

CALL PBOPEN (iu3d,file_3d,'R',kret)
IF (kret.ne.0) GOTO 9991

IF (l10m) THEN
  CALL PBOPEN (iusup,file_sup,'R',kret)
  IF (kret.ne.0) GOTO 9990
ENDIF

!--------------------------------------------------------------------------
! 1.7 Apro i files di output e scrivo gli header

DO kp = 1,np
  IF (kp < 10) THEN
    WRITE (fileout,'(a2,i1,a4)') "up",kp,".dat"
  ELSE IF (kp < 100) THEN
    WRITE (fileout,'(a2,i2,a4)') "up",kp,".dat"
  ELSE
    WRITE (fileout,'(a2,i3,a4)') "up",kp,".dat"
  ENDIF

  OPEN (UNIT=iu0+kp, FILE=fileout, STATUS="REPLACE", FORM="FORMATTED")
  WRITE (iu0+kp,'(1x,6i5,f5.0)') MOD(data1%yy,100),jul(data1),hh1, &
    MOD(data2%yy,100),jul(data2),hh2,500.
  WRITE (iu0+kp,'(a)') "     F    F    F    F"
ENDDO

!==========================================================================
! 2) Lettura - scrittura (ciclo sugli istanti)
! L'array var3d e' dimensionato (4,nl,np), e contiene P,T,U,V per ogni 
!   punto e livello dell'istante corrente (prima della scrittura diventano 
!   P,T,DD,FF). I livelli sono ordinati dall'alto in basso; il livello a 
!   10 m (se c'e') e' l'ultimo.

cnt_rew = 0
cnt_rew2 = 0
cnt_mis(:) = 0

istanti: DO kh = 1,nh

  hht = hh1 + kh - 1
  datac = data1 + hht/24
  hhc = MOD(hht,24)
  data = (/datac%dd,datac%mm,datac%yy/)
  ora = (/hhc,0/)
  scad = (/1,0,0,13/)
  WRITE (*,'(a,i4.4,3(1x,i2.2))') "Elaboro istante ", &
    datac%yy,datac%mm,datac%dd,hhc

!--------------------------------------------------------------------------
! 2.1 Leggo i dati da file_3d

  DO kl = 1,nl2
    idl = kl+idlayt-1
    level(1:3) = (/110,idl,idl+1/)
    DO kv = 1,4
      var(1:3) = (/200,2,idvar(kv)/)
      CALL findgrib90(iu3d,kbuffer,maxdim,data,ora,scad,level,var,ier)
      IF (ier > 0) THEN
        GOTO 9989
      ELSE IF (ier == 0 .OR. ier == -2) THEN
        CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                    field,maxdim,kbuffer,maxdim,klen,'D',kret)
        IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
        IF (ANY(ksec2(1:11)/=ksec2_sav(1:11)) .OR. &
            ANY(ksec2(13:14)/=ksec2_sav(13:14))) GOTO 9987
        var3d(kv,kl,1:np) = field(idx(1:np))
        IF (ier == -2) cnt_rew = cnt_rew + 1
      ELSE
        var3d(kv,kl,1:np) = rmis
        cnt_mis(1) = cnt_mis(1) + 1
      ENDIF      
    ENDDO
  ENDDO

!--------------------------------------------------------------------------
! 2.2 Se e' richesto il livello aggiuntivo superificiale, leggo i dati da 
!     file_sup e ricalcolo la pressione

! Leggo i dati
  IF (l10m) THEN
    DO kv = 1,4
      level(1:3) = idlevsup(kv,1:3)
      var(1:3) = (/200,2,idvar(kv)/)

!print *,data,ora,scad,level,var
!read *
      CALL findgrib90(iusup,kbuffer,maxdim,data,ora,scad,level,var,ier)
      IF (ier > 0) THEN
        GOTO 9988
      ELSE IF (ier == 0 .OR. ier == -2) THEN
        CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                    field,maxdim,kbuffer,maxdim,klen,'D',kret)
        IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
        IF (ANY(ksec2(1:11)/=ksec2_sav(1:11)) .OR. &
            ANY(ksec2(13:14)/=ksec2_sav(13:14))) GOTO 9986
        var3d(kv,nl,1:np) = field(idx(1:np))
        IF (ier == -2) cnt_rew2 = cnt_rew2 + 1
      ELSE
        var3d(kv,nl,1:np) = rmis
        cnt_mis(2) = cnt_mis(2) + 1
      ENDIF      
    ENDDO

! Riporto P del livello superficiale a 10 m
    var3d(1,nl,1:np) = var3d(1,nl,1:np) - &
      var3d(1,nl,1:np)*10.*g/(rdry*var3d(2,nl,1:np))

  ENDIF

!--------------------------------------------------------------------------
! 2.3 Elaborazioni

! Se tutti i dati sono mancanti, passo all'istante successivo
  IF (ALL(var3d(:,:,:) == rmis)) THEN
    cnt_mis(3) = cnt_mis(3) + 1
    CYCLE istanti
  ENDIF

! Passo da U e V a DD e FF
  DO kl = 1,nl
  DO kp = 1,np
    IF (var3d(3,kl,kp) /= rmis .AND. var3d(4,kl,kp) /= rmis) THEN
      dd = dir(var3d(3,kl,kp),var3d(4,kl,kp))
      ff = mag(var3d(3,kl,kp),var3d(4,kl,kp))
      var3d(3,kl,kp) = dd
      var3d(4,kl,kp) = ff
    ENDIF
  ENDDO
  ENDDO

! Converto unita' di misura e dati mancanti
  WHERE (var3d(1,:,:) == rmis)
    var3d(1,:,:) = -99.9
  ELSEWHERE
    var3d(1,:,:) = var3d(1,:,:) / 100.
  ENDWHERE
  WHERE (var3d(2,:,:) == rmis)
    var3d(2,:,:) = 999.9
  ENDWHERE
  WHERE (var3d(3:4,:,:) == rmis)
    var3d(3:4,:,:) = 999
  ENDWHERE
  WHERE (zz(:,:) == rmis)
    zz(:,:) = 9999.
  ENDWHERE

! Se richiesta l'opzione -test, sovrascrivo alcuni dati (USER MODIFICATION)
  IF (ltest) THEN
    var3d(3,:,:) = 270.
    var3d(4,:,:) = 3.
  ENDIF

!--------------------------------------------------------------------------
! 2.4 Scrivo sui file up*.dat (il primo livello da scrivere e' quello piu'
!     in basso, mentre l'array var3d e' ordinato dall'alto)

  DO kp = 1, np
    WRITE (iu0+kp,'(3x,i4,5x,i5,5x,4i2,5x,i2,t69,i2)') & 
      9999,idx(kp),MOD(datac%yy,100),datac%mm,datac%dd,hhc, &
      nl,nl

    DO krec = 1, nl/4
      k2 = nl - (krec-1)*4
      k1 = nl - krec*4 + 1
      WRITE (iu0+kp,'(4(3x,f6.1,a1,f5.0,a1,f5.1,a1,i3,a1,i3))') &
        (var3d(1,kl,kp),"/",zz(kl,kp),"/",var3d(2,kl,kp),"/", &
         NINT(var3d(3,kl,kp)),"/",NINT(var3d(4,kl,kp)), & 
         kl = k2,k1,-1)
    ENDDO
  
    IF (MOD(nl,4) /= 0) THEN
      k1 = 1
      k2 = nl - (nl/4)*4
      WRITE (iu0+kp,'(4(3x,f6.1,a1,f5.0,a1,f5.1,a1,i3,a1,i3))') &
        (var3d(1,kl,kp),"/",zz(kl,kp),"/",var3d(2,kl,kp),"/", &
         NINT(var3d(3,kl,kp)),"/",NINT(var3d(4,kl,kp)), & 
         kl = k2,k1,-1)
    ENDIF
  ENDDO

!--------------------------------------------------------------------------
! Chiudo il ciclo sugli istanti

ENDDO istanti

WRITE (*,*) "grib2up_dat: Elaborazioni terminate"
WRITE (*,'(2(a,i5))') " Campi mancanti 3d ",cnt_mis(1), &
  " rewind su grib 3d ",cnt_rew
IF (l10m) WRITE (*,'(2(a,i5))') " Campi mancanti sup ",cnt_mis(2), &
  " rewind su grib sup ",cnt_rew2
WRITE (*,'(a,i3)') "Istanti interamente mancanti ",cnt_mis(3)

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_pts)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_pts)
STOP

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_srq)
STOP

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_srq)
STOP

9995 CONTINUE
WRITE (*,*) "Label inattesa in ",TRIM(file_srq)
WRITE (*,*) "Punto ",k," label attesa ",idx(k)," trovata ",idum
STOP

9994 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_z)
STOP

9993 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_z)
STOP

9992 CONTINUE
WRITE (*,*) "Quote di ",COUNT(zz(1:nl,1)==rmis), &
  " livelli non trovate in ",TRIM(file_z)
STOP

9991 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_3d)
STOP

9990 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_sup)
STOP

9989 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_3d)
STOP

9988 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_sup)
STOP

9987 CONTINUE
WRITE (*,*) "Errore, area diversa in ",TRIM(file_3d)
WRITE (*,'(a,3i4,a,3i4)') "lev ",level(1:3)," var ",var(1:3)
WRITE (*,*)
WRITE (*,'(2a)') TRIM(file_z),", ksec2(1:11),ksec2(13:14)"
WRITE (*,'(13i7)') ksec2_sav(1:11),ksec2_sav(13:14)
WRITE (*,'(2a)') TRIM(file_3d),", ksec2(1:11),ksec2(13:14)"
WRITE (*,'(13i7)') ksec2(1:11),ksec2(13:14)
STOP

9986 CONTINUE
WRITE (*,*) "Errore, area diversa in ",TRIM(file_sup)
WRITE (*,'(a,3i4,a,3i4)') "lev ",level(1:3)," var ",var(1:3)
WRITE (*,*)
WRITE (*,'(2a)') TRIM(file_z),", ksec2(1:11),ksec2(13:14)"
WRITE (*,'(13i7)') ksec2_sav(1:11),ksec2_sav(13:14)
WRITE (*,'(2a)') TRIM(file_sup),", ksec2(1:11),ksec2(13:14)"
WRITE (*,'(13i7)') ksec2(1:11),ksec2(13:14)
STOP

END PROGRAM grib2up_dat

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE findgrib90(iuin,kbuffer,maxdim,data,ora,scad,level,var,ier)
!--------------------------------------------------------------------------
! Traduzione f90 della patrunesca findgrib.f (f77)
!
! Modifiche:
! - maxdim (dimensionamento del buffer kbuffer) diventa un argomento intero
!   e solo di input
! - non c'e' piu' la chiamata a setpar per avere il n.ro di bytes di un
!   intero (la subr. potrebbe essere meno portabile...)
! - in caso di rewind, invece di scrivere su file ritorna ier = -2
!
!                                                 V 2.0,  Enrico 13/04/2005
!--------------------------------------------------------------------------
!
!	Ricerca all'interno di un file aperto con PBOPEN i grib
!	specificati dalla chiave data,ora,scad,level,var.
!       Tratta i  valori negativi nelle chiavi come wildcards
!
!	input:
!
!	iuin		I	unita` restituita da PBOPEN
!       maxdim          I       dimensionamento del buffer kbuffer
!
!	data(1)		I	giorno		)	
!	data(2)		I	mese		)
!	data(3)		I	anno			)  emissione
!	ora(1)		I	ora		)
!	ora(2)		I	minuti		)
!	scad(1)		I	indicator of unit of time range	(table 4)
!	scad(2)		I	periodo di tempo 1
!	scad(3)		I	periodo di tempo 2
!				(nel caso sia definita solo p1 o p2
!				viene testato solo il max tra p1 e p2)
!	scad(4)		I	time range indicator 		(table 5)
!	level(1)	I	indicator of type of level	(table 3)
!	level(2)	I	height, pressure etc. of levels
!	level(3)	I	height, pressure etc. of levels
!	var(1)		I	identification of originating/generating
!				 centre  (table 0)
!	var(2)		I	table 2 version number
!	var(3)		I	parameter			(table 2)
!
!	output:
!
!	kbuffer(maxdim)	I	buffer di debosito del grib estratto	
!	ier		I	codice errore
!				=0 tutto o.k.
!				=-1 grib not found
!                               =-2 grib trovato dopo rewind
!				altri > vedi errori pbgrib
!--------------------------------------------------------------------------

IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: iuin,maxdim
INTEGER, INTENT(IN) :: data(3),ora(2),scad(4),level(3),var(3)
INTEGER, INTENT(OUT):: kbuffer(maxdim),ier

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: klen,kret
REAL :: psec2(512),psec3(2)
REAL :: field(maxdim)

! altre variabili della subroutine

INTEGER :: datag(3),orag(2),scadg(4),levelg(3),varg(3)
INTEGER :: igiro

!--------------------------------------------------------------------------

igiro = 0

DO 
! Leggo il prossimo grib
  CALL PBGRIB(iuin,kbuffer,maxdim*4,klen,kret)

! Gestisco errore ed EOF
  IF (kret == -1 .AND. igiro == 0) THEN       ! EOF 1a volta, rewind
    CALL pbseek(iuin,0,0,kret)
    igiro = 1
    CYCLE

  ELSE IF (kret == -1 .AND. igiro == 1) THEN  ! EOF 2a volta, mi arrendo
    ier = -1
    RETURN

  ELSE IF (kret /= 0) THEN                    ! erore di lettura, termino
    ier = kret
    RETURN

  ENDIF

! Decodifico l'header del Grib
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'I',kret)

! Se ho trovato il Grib giusto, termino
  datag  = (/ksec1(12),ksec1(11),ksec1(10)+(ksec1(21)-1)*100/)
  orag   = (/ksec1(13),ksec1(14)/)
  scadg  = (/ksec1(15),ksec1(16),ksec1(17),ksec1(18)/)
  levelg = (/ksec1(7), ksec1(8), ksec1(9)/)
  varg   = (/ksec1(2), ksec1(1), ksec1(6)/)

  IF (ALL( datag == data   .OR. data < 0  ) .AND. &
      ALL( orag == ora     .OR. ora < 0   ) .AND. &
      ALL( scadg == scad   .OR. scad < 0  ) .AND. &
      ALL( levelg == level .OR. level < 0 ) .AND. &
      ALL( varg == var     .OR. var < 0   )) THEN

    IF (igiro == 0) THEN
      ier = 0
    ELSE IF (igiro == 1) THEN
      ier = -2
    ENDIF

    RETURN
  ENDIF

ENDDO

END SUBROUTINE findgrib90

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

function mag(u,v)
  IMPLICIT NONE
  REAL, INTENT(IN) :: u,v
  REAL mag

  mag=sqrt(u*u+v*v)

  RETURN
end function mag

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

function dir(u,v)
  IMPLICIT NONE
  REAL, INTENT(IN) :: u,v
  REAL dir
  REAL, PARAMETER :: dtr = 180./3.141592654

  IF      (u <= 0. .AND. v < 0.) THEN    ! 0 - 90
     dir = dtr*atan( u/v )
  ELSE IF (u < 0. .AND. v >= 0.) THEN    ! 90 - 180
     dir = 90. + dtr*atan( -v/u )
  ELSE IF (u >= 0. .AND. v > 0.) THEN    ! 180 - 270
     dir = 180. + dtr*atan( u/v )
  ELSE IF (u > 0. .AND. v <= 0.) THEN    ! 270 - 360
     dir = 270. + dtr*atan( -v/u )
  ELSE IF (u == 0. .AND. v == 0.) THEN
     dir = 0.
  ENDIF

  RETURN
end function dir

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Scrive a schermo un breve help
!
IMPLICIT NONE

!            12345678901234567890123456789012345678901234567890123456789012345678901234567890
WRITE (*,*) "Uso: grib2up_dat.exe grib_3d grib_z file_pts file_srq nlayb nlayt datah1 nh"
WRITE (*,*) "                     [-10m grib_10m] [-test] [-h]"
WRITE (*,*) ""
WRITE (*,*) "  grib_3d:  campi P,T,U,V model levels per il giorno richiesto"
WRITE (*,*) "  grib_z:   quote (sul livello del mare) dei model layers"
WRITE (*,*) "  file_pts: indici dei punti richiesti (formato .pts.csv)"
WRITE (*,*) "  file_srq: nome e orografia delle stazioni virtuali (formato srq_temp)"
WRITE (*,*) "  nlayb:    indice del model layer LM piu' in basso da elaborare"
WRITE (*,*) "  nlayt:    indice del model layer LM piu' in alto da elaborare"
WRITE (*,*) "  datah1:   primo istante da elaborare (YYYYMMDDHH)"
WRITE (*,*) "  nh        numero di istanti (ore) da elaborare (di norma 25)"
WRITE (*,*) "  -test:    scrive nei files up.dat valori di vento fittizi (per debug)"
WRITE (*,*) "  grib_10m: campi P,T,U,V superficiali (vengono aggiunti agli up.dat "
WRITE (*,*) "            come primo livello)"
WRITE (*,*) ""

RETURN
END SUBROUTINE scrive_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_eof_eor(eof, eor)
!-------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!-------------------------------------------------------------------------

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

RETURN

! Gestione errori
9999 CONTINUE
WRITE (*,*) "Errore in subroutine get_eof_eor, usero' valori di default"
eof = -1
eor = -2
RETURN

END SUBROUTINE get_eof_eor

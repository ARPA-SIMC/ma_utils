PROGRAM grib2chimere
!--------------------------------------------------------------------------
! Ricostruisce l'output di un run Chimere (unformatted) a partire dai GRIB
!   corrispondenti, scritti in un unico file.
!
! Uso: grib2chimere.exe filein fileout filespc NZ [-roz vcoord orog]
!
! NOTE:
! Il programma cerca i codici GRIB corrispondenti alle specie richieste in
!   una lista di files statici (tabella_200.txt ...)
! L'ordine delle specie nel .expa dovrebbe essere lo stesso di filespc; se 
!   questo non avviene, cosi' come se ci sono GRIB mancanti, il programma
!   rallenta molto.
!
! Al momento il programma non e' molto generale:
! - Gestione dei livelli : suppone che esistano NZ livelli, condificati 
!   come 109,K,0. La lista dei livelli potrebbe essere desunta direttamente
!   dai GRIB...
! - Mette a 0 le specie richieste in output e non presenti in input
! - Gestisce solo lo scanning flag 64 (010)
!
!                                         Versione 3.0.2, Enrico 02/12/2013
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

! 0.1 Interface per funzioni uppercase e lowercase
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

! 0.2 Parametri costanti
INTEGER, PARAMETER :: maxdim = 100000  ! dimensione massima dei GRIB
INTEGER, PARAMETER :: maxspc = 200     ! n.ro max specie
REAL, PARAMETER :: fc = 287*7.2868e16 / 9.81

! 0.3 Tabelle con la codifica GRIB delle specie

! Path di default delle tabelle seriet
! PKGDATAROOTDIR viene sostituito in fase di compilazione con il path delle
! tabelle seriet (di solito /usr/share/ma_utils). La sostituzione sfrutta 
! il comando gfortran -D; vedi Makefile.am nelle singole dir.
CHARACTER (LEN=40), PARAMETER :: tab_path_def = PKGDATAROOTDIR
CHARACTER (LEN=40), PARAMETER :: tab_env = "MA_UTILS_DAT"

! Tabelle in cui cercare i parametri Chimere
INTEGER, PARAMETER :: ntab=3              
INTEGER :: id_tab(ntab) = (/200,199,196/) 

! 0.4 Profilo standard dell'atmosfera (da Holton); valori ogni km
REAL, PARAMETER :: pstd(0:10) = (/101325.,89874.,79495.,70108.,61640., &
  54019.,47181.,41060.,35599.,30742.,26436./)

! 0.5 Dichiarazioni per GRIBEX.
INTEGER :: ksec2_first(1024)
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),klen,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! 0.6 Altre variabili del programma
TYPE(date) :: data1,data2,datac,datar,datar_sav
REAL, ALLOCATABLE :: conc(:,:,:),zz(:,:),ro(:,:),ac(:),bc(:),pp(:)
REAL :: orog(maxdim),lnpstd(0:10),w1,w2,lnps,ps,lnpp
INTEGER :: req_tab(maxspc),req_var(maxspc)
INTEGER :: idp,iu1,iu2,iu3,hh1,hh2,hh,dum_var,fok,nfs(maxspc)
INTEGER :: hhr,hhr_sav,sca,sca1,sca2
INTEGER :: idata,hhc,hh_tot,cem,data(3),ora(2),scad(4),level(3),var(3)
INTEGER :: np,nz,nt,nspc
INTEGER :: k,kz,kt,ks,kp,ios,eof,eor,ier,cnt_rew,p1,p2,p3,l1,l2,t1,t2
CHARACTER(LEN=120) :: file_tab(ntab)
CHARACTER (LEN=120) :: filein,fileout,filespc,fileor,filevc,chdum,arg(4)
CHARACTER (LEN=80) :: tab_path
CHARACTER (LEN=8) :: req_spc(maxspc),dum_spc
CHARACTER (LEN=2) :: next_arg,idscad
LOGICAL :: spc_pres(maxspc),first,lroz,req_ro,req_zz

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

!--------------------------------------------------------------------------
! 1.0 Variabili d'ambiente
tab_path = ""
CALL GETENV(tab_env,tab_path)
IF (TRIM(tab_path) == "") tab_path = tab_path_def

DO kt = 1,ntab
  WRITE (file_tab(kt),'(2a,i3.3,a4)') TRIM(tab_path), &
    "/tabella_",id_tab(kt),".txt"
ENDDO

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando
idp = 0
idscad="an"
lroz = .FALSE.
next_arg = ""
arg (:) = ""
fileor = ""
filevc = ""

DO kp = 1,HUGE(kp)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "") THEN  
    EXIT
  ELSE IF (TRIM(chdum) == "-forc") THEN  
    idscad = "fc"
  ELSE IF (TRIM(chdum) == "-roz") THEN  
    lroz = .TRUE.
    next_arg = "vc"
  ELSE IF (next_arg == "vc") THEN
    filevc = chdum
    next_arg = "or"
  ELSE IF (next_arg == "or") THEN
    fileor = chdum
    next_arg = ""
  ELSE
    idp = idp + 1
    IF (idp > 4) THEN
      CALL write_help
      STOP
    ENDIF 
    arg(idp) = chdum
  ENDIF
ENDDO

filein = arg(1)
fileout = arg(2)
filespc = arg(3)
READ (arg(4),*,IOSTAT=ios) nz

IF (filein == "" .OR. fileout == "" .OR. filespc == "" .OR. &
    ios /= 0 .OR. lroz .AND. (filevc == "" .OR. fileor == "")) THEN
  CALL write_help
  STOP
ENDIF

!--------------------------------------------------------------------------
! 1.2) Leggo l'elenco delle specie da filespc

! Trovo codice per EOF
CALL get_eof_eor(eof,eor)

! Leggo filespc
OPEN (UNIT=20, FILE=filespc, STATUS="OLD", ACTION="READ", ERR=9999)
  DO k = 1,maxspc
    READ (20,'(a)',IOSTAT=ios) chdum
    IF (ios == eof) EXIT
    IF (ios /= 0) GOTO 9998

    chdum = ADJUSTL(chdum)
    p1 = INDEX(chdum,' ')
    p2 = INDEX(chdum,CHAR(9))
    p3 = LEN(TRIM(chdum)) + 1
    IF (p1 > 0) p3 = MIN(p3,p1)
    IF (p2 > 0) p3 = MIN(p3,p2)
    req_spc(k) = chdum(1:p3-1)
  ENDDO

  IF (k > maxspc) THEN
    READ (20,'(a)',IOSTAT=ios) 
    IF (ios /= eof) GOTO 9997
  ENDIF

  nspc = k-1

CLOSE (20)

!--------------------------------------------------------------------------
! 1.3) Trovo i codici GRIB corrispondenti: scorro tutte le tabelle GRIB,
!      e memorizzo i codici GRIB delle variabili che mi servono.

req_tab(1:nspc) = 9999
req_var(1:nspc) = 9999

DO kt = 1, ntab

  OPEN (UNIT=22, FILE=file_tab(kt), STATUS="OLD", ACTION="READ", ERR=9996)

  DO
    READ (22,'(a)',IOSTAT=ios) chdum
    IF (ios /= 0) EXIT
    IF (TRIM(chdum) == "" .OR. chdum(1:1) == "!") CYCLE
    READ (chdum,'(1x,i3,2x,a8)',IOSTAT=ios) dum_var,dum_spc
    IF (ios /= 0) GOTO 9995

!   Verifico se la variabile letta e' compresa tra quelle richieste
    DO ks = 1, nspc
      IF (TRIM(uppercase(req_spc(ks))) == TRIM(uppercase(dum_spc))) THEN
        req_tab(ks) = id_tab(kt)
        req_var(ks) = dum_var
        EXIT
      ENDIF
    ENDDO

  ENDDO
  CLOSE (22)

ENDDO

IF (ANY(req_var(1:nspc) == 9999)) THEN
  WRITE (*,*)
  WRITE (*,'(a,i3,a)') "Warning: ",COUNT(req_var(1:nspc) == 9999), &
    "parametri non trovati nelle tabelle:" 
  DO ks = 1,nspc
    IF (req_var(ks) == 9999) WRITE (*,'(a)') req_spc(ks)
  ENDDO
  WRITE (*,*)
ENDIF

req_zz = .FALSE.
req_ro = .FALSE.
DO ks = 1,nspc
  IF (req_tab(ks) == 200 .AND. req_var(ks) == 8) req_zz = .TRUE.
  IF (req_tab(ks) == 200 .AND. req_var(ks) == 89) req_ro = .TRUE.
ENDDO

!--------------------------------------------------------------------------
! 1.4) Scorro una prima volta i GRIB per trovare: date estreme, dimensioni 
!      griglia, centro d'emissione, specie per cui non c'e' nessun grib. 
!      Controllo anche che i GRIB abbiano la stessa area.

data1 = date(31,12,9999)
hh1 = 24
data2 = date(1,1,0)
hh2 = 0
first = .TRUE.
spc_pres(1:nspc) = .FALSE.
CALL grsvck(0)
CALL PBOPEN (iu1,filein,'R',kret)
IF (kret.ne.0) GOTO 9991

DO
  
! Leggo il prossimo GRIB
  CALL PBGRIB(iu1,kbuffer,maxdim*4,klen,kret)
  IF (kret == -1) THEN
    EXIT
  ELSE IF (kret < -1) THEN
    WRITE(*,*) "Error pbgrib: kret ",kret
    STOP
  ENDIF

! Lo decodifico
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               field,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret

! Controllo che area e cem siano le stesse
  IF (first) THEN
    ksec2_first(:) = ksec2(:)
    cem = ksec1(2)
    np = ksec2(2) * ksec2(3)
    IF (ksec2(11) /= 64) GOTO 9994
  ELSE
    IF (ANY(ksec2(1:11) /= ksec2_first(1:11)) .OR. ksec1(2) /= cem) GOTO 9993
  ENDIF

! Verifico se ho trovato un nuovo parametro 
  DO ks = 1, nspc
  IF (ksec1(1) == req_tab(ks) .AND. ksec1(6) == req_var(ks)) THEN
    spc_pres(ks) = .TRUE.
    EXIT
  ENDIF
  ENDDO  

! Date estreme
  IF (idscad == "an") THEN          ! Cerco le date di validita' estreme
    CALL ksec1_valid(ksec1,datac,hhc,ier)
    IF (datac < data1 .OR. (datac == data1 .AND. hhc < hh1) ) THEN
      data1 = datac
      hh1 = hhc
    ELSE IF (datac > data2 .OR. (datac == data2 .AND. hhc > hh2) ) THEN
      data2 = datac
      hh2 = hhc
    ENDIF

  ELSE IF (idscad == "fc") THEN     ! cerco reftime e scadenze estreme
    IF (ksec1(18) /= 0) GOTO 9987
    datar%dd = ksec1(12)
    datar%mm = ksec1(11)
    datar%yy = ksec1(10) + 100 * (ksec1(21) - 1)
    hhr = ksec1(13)
    sca = ksec1(16)
    IF (first) THEN
      datar_sav = datar
      hhr_sav = hhr
      sca1 = sca
      sca2 = sca
    ELSE IF (datar /= datar_sav .OR. hhr /= hhr_sav) THEN
      GOTO 9986
    ELSE
      IF (sca < sca1) sca1 = sca
      IF (sca > sca2) sca2 = sca
    ENDIF

  ENDIF

  first = .FALSE.
ENDDO
!CALL PBCLOSE(iu1)

IF (idscad == "an") THEN
  t1 = 1
  nt = (data2 - data1) * 24 + (hh2 - hh1 + 1)
  t2 = nt
ELSE IF (idscad == "fc") THEN
  t1 = sca1
  nt = sca2 - sca1 + 1
  t2 = sca2
ENDIF

!--------------------------------------------------------------------------
! 1.5) Se devo inventare Ro e Z, leggo orografia e coefficenti dei livelli

IF (lroz) THEN
  ALLOCATE (zz(np,nz),ro(np,nz))

! Leggo orografia
  CALL PBOPEN (iu2,fileor,'R',kret)
  IF (kret.ne.0) GOTO 9990
  CALL PBGRIB(iu2,kbuffer,maxdim*4,klen,kret)
  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
               orog,maxdim,kbuffer,maxdim,klen,'D',kret)
  IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
!  CALL PBCLOSE(iu2)

  IF (ANY(ksec2(1:11) /= ksec2_first(1:11))) GOTO 9992
  IF (ANY(orog(1:np) < 0.) .OR. (ANY(orog(1:np) >= 10000.))) &
    orog(1:np) = MAX(MIN(orog(1:np),10000.),0.)
  IF (ksec1(6) /= 8) WRITE (*,*) &
    "Warning, parametro inatteso in grib orografia: ",ksec1(6)

! Leggo coefficienti livelli
  ALLOCATE (ac(nz),bc(nz),pp(nz))
  OPEN (UNIT=30, FILE=filevc, STATUS="OLD", ACTION="READ", ERR=9989)
  DO kz = 1,nz 
    READ (30,*,IOSTAT=ios) ac(kz),bc(kz)
    IF (ios /= 0) GOTO 9988
  ENDDO
  CLOSE (30)

! Calcolo Ps e Ros (interpolo il profilo standard sulla quota orografia)
  lnpstd(:) = LOG(pstd(:))

  DO kp = 1,np
!   Calcolo Ps
    l1 = INT(orog(kp)/1000.)
    l2 = l1 + 1
    w2 = orog(kp)/1000. - INT(orog(kp)/1000.)
    w1 = 1. - w2
    lnps = w1*lnpstd(l1) + w2*lnpstd(l2)
    ps = EXP(lnps)

!   Calcolo profilo di P
    DO kz = 1,nz
      pp(kz) = 1e5*ac(kz) + ps*bc(kz)
    ENDDO

!   Calcolo Z al top dei layers
    DO kz = 1,nz
      lnpp = LOG(pp(kz))
      DO l1 = 0,nz-1
        IF (lnpp <= lnpstd(l1)) EXIT
      ENDDO
      IF (lnpp > lnpstd(0)) l1 = 0
      l2 = l1 + 1
      w1 = (lnpstd(l2)-lnpp) / (lnpstd(l2)-lnpstd(l1))
      w2 = (lnpp-lnpstd(l1)) / (lnpstd(l2)-lnpstd(l1))
      zz(kp,kz) = 1000. * (w1*REAL(l1) + w2*REAL(l2))
    ENDDO

!   Calcolo Ro media dei layers
    ro(kp,1) = -fc * (pp(1)-ps) / (zz(kp,1)-orog(kp))
    DO kz = 2,nz
      ro(kp,kz) = -fc * (pp(kz)-pp(kz-1)) / (zz(kp,kz)-zz(kp,kz-1))
    ENDDO

  ENDDO

ENDIF

!--------------------------------------------------------------------------

WRITE (*,'(a,i6)') "Specie richieste :  ",nspc
WRITE (*,'(a,i6)') "Presenti nel grib:  ",COUNT(spc_pres(1:nspc))
WRITE (*,'(a,i6)') "P.ti griglia oriz.: ",np
WRITE (*,'(a,i6)') "Livelli verticali : ",nz
WRITE (*,'(a,i6)') "Scadenze orarie :   ",nt
IF (idscad == "an") THEN
  WRITE (*,'(a,i5,3i3)') "Data iniziale: ",data1%yy,data1%mm,data1%dd,hh1
  WRITE (*,'(a,i5,3i3)') "Data finale  : ",data2%yy,data2%mm,data2%dd,hh2
ELSE IF (idscad == "fc") THEN
  WRITE (*,'(a,i5,3i3)') "Refer. time:   ",datar%yy,datar%mm,datar%dd,hhr
  WRITE (*,'(a,2(1x,i3.3))') "Scad. estreme: ",sca1,sca2
ENDIF

IF (lroz) WRITE (*,'(a)') "Verranno aggiunti i campi (inventati) Z e Ro"
IF (lroz .AND. req_zz) &
  WRITE (*,'(a)') "Warning: Z e' anche tra i parametri richiesti"
IF (lroz .AND. req_ro) &
  WRITE (*,'(a)') "Warning: Ro e' anche tra i parametri richiesti"

IF (COUNT(spc_pres(1:nspc)) /= nspc) THEN
  WRITE (*,'(a)') "Specie non trovate:"
  DO ks = 1, nspc
    IF (.NOT. spc_pres(ks)) WRITE (*,'(a8,2x,2i4)') &
      req_spc(ks),req_tab(ks),req_var(ks)
  ENDDO  
ENDIF

WRITE (*,*)

!--------------------------------------------------------------------------
! 2) Leggo i GRIB e scrivo il file Chimere (ciclo sugli istanti richiesti)

ALLOCATE (conc(np,nz,nspc))

OPEN (UNIT=40, FILE=fileout, STATUS="REPLACE", FORM="UNFORMATTED")
CALL PBOPEN (iu3,filein,'R',kret)
nfs(:) = 0
cnt_rew = 0

DO kt = t1,t2

! Data corrente e parametri derivati
  IF (idscad == "an") THEN   
    hh_tot = hh1 + kt - 1
    datac = data1 + hh_tot/24
    hhc = MOD(hh_tot,24)
    idata = hhc + datac%dd*100 + datac%mm*10000 + datac%yy*1000000
    data = (/datac%dd,datac%mm,datac%yy/)
    ora = (/hhc,0/)
    scad = (/1,0,0,0/)

  ELSE IF (idscad == "fc") THEN
    hh_tot = hhr + kt
    datac = datar + hh_tot/24
    hhc = MOD(hh_tot,24)
    idata = hhc + datac%dd*100 + datac%mm*10000 + datac%yy*1000000
    data = (/datar%dd,datar%mm,datar%yy/)
    ora = (/hhr,0/)
    scad = (/1,kt,0,0/)
  ENDIF

! Cerco i GRIB che mi servono (ciclo su livelli e specie)
  fok = 0
  DO kz = 1,nz
  DO ks = 1,nspc
    IF (.NOT. spc_pres(ks)) THEN
      conc(1:np,kz,ks) = 0.
      CYCLE
    ENDIF

    level = (/109,kz,0/)
    var = (/cem,req_tab(ks),req_var(ks)/)

    CALL findgrib90(iu3,kbuffer,maxdim,data,ora,scad,level,var,ier)

    IF (ier == 0 .OR. ier == -2) THEN
      CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
                  field,maxdim,kbuffer,maxdim,klen,'D',kret)
      IF (kret.gt.0) WRITE(*,*) "Warning gribex: kret ",kret
      conc(1:np,kz,ks) = field(1:np)
      fok = fok + 1
      nfs(ks) = nfs(ks) + 1
      IF (ier == -2) cnt_rew = cnt_rew + 1
    ELSE
      conc(1:np,kz,ks) = 0.
    ENDIF      

  ENDDO
  ENDDO

! Scrivo sul file Chimere
  IF (.NOT. lroz) THEN
    WRITE (40) idata,(((conc(k,kz,ks),k=1,np),kz=1,nz),ks=1,nspc)
  ELSE
    WRITE (40) idata,(((conc(k,kz,ks),k=1,np),kz=1,nz),ks=1,nspc), &
      ((zz(k,kz),k=1,np),kz=1,nz),((ro(k,kz),k=1,np),kz=1,nz)
  ENDIF

  IF (fok /= nz*nspc .OR. cnt_rew /= 0) THEN
    WRITE (*,'(a,i5,3i3,2(a,i4),a,i5)') "Warning, istante: ", &
      datac%yy,datac%mm,datac%dd,hhc," campi validi: ",fok, &
      " su ",nz*nspc," rewind ",cnt_rew
  ENDIF

ENDDO

IF (ALL(nfs(1:nspc) == nt*nz) ) THEN
  WRITE (*,*) "Trovati tutti i campi richiesti"
ELSE 
  DO ks = 1,nspc
  IF (nfs(ks) < nt*nz) THEN
    WRITE (*,'(a,a8,a,2i4,2(a,i6))') "Specie ",TRIM(req_spc(ks))," (", &
      req_tab(ks),req_var(ks),"), presenti ",nfs(ks)," campi su ",nt*nz
  ENDIF
  ENDDO
ENDIF

STOP

!--------------------------------------------------------------------------
! 3) Gestione erori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filespc)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filespc)," record ",k
STOP

9997 CONTINUE
WRITE (*,*) "Richieste troppe specie, modificare il paramtro maxspc"
STOP

9996 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(file_tab(kt))
STOP

9995 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(file_tab(kt))
STOP

9994 CONTINUE
WRITE (*,*) "Scanning mode non gestito: ",ksec2(11)
STOP

9993 CONTINUE
WRITE (*,*) "I GRIB hanno area o cem diversi"
STOP

9992 CONTINUE
WRITE (*,*) "GRIB orografia definito su una'area diversa da concentrazioni"
STOP

9991 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)," kret = ",kret
STOP

9990 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fileor)," kret = ",kret
STOP

9989 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filevc)
STOP

9988 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filevc)
STOP

9987 CONTINUE
WRITE (*,*) "Timerange inatteso in ",TRIM(filein),ksec1(18)
STOP

9986 CONTINUE
WRITE (*,*) "Reftime diversi in ",TRIM(filein)
STOP

END PROGRAM grib2chimere

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

!print *,data
!print *,ora
!print *,scad
!print *,level
!print *,var
!read *

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

SUBROUTINE ksec1_valid(ksec1,data,hh,ier)
!--------------------------------------------------------------------------
! Data la sezione 1 di un GRIB, ritorna la data e l'ora di validita'
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: ksec1(*)
TYPE(date), INTENT(OUT) :: data
INTEGER, INTENT(OUT) :: hh,ier
!
TYPE(date) :: datar
INTEGER :: hhr,hh_tot,sca

!--------------------------------------------------------------------------

datar%dd = ksec1(12)
datar%mm = ksec1(11)
datar%yy = ksec1(10) + 100 * (ksec1(21) - 1)
hhr = ksec1(13)

! Gestione time range & time unit
SELECT CASE(ksec1(18))
CASE(0)
  IF (ksec1(16) == 0) THEN      ! unintialised analysis
    sca=0
  ELSE                          ! forecast
    IF (ksec1(15) == 1) THEN
      sca = ksec1(16)
    ELSE
      GOTO 9999
    ENDIF
  ENDIF

CASE(1)                         ! initialised analysis
  sca = 0

CASE(2:5)                       ! prodotto riferito a un intervallo
  IF (ksec1(15) == 1) THEN
    sca = ksec1(17)
  ELSE
    GOTO 9999
  ENDIF

CASE(13)                        ! analisi LAMA
  IF (ksec1(15) == 1 .AND. ksec1(16) == 0) THEN
    sca = 0
  ELSE
    GOTO 9999
  ENDIF

CASE DEFAULT                    ! time range non gestio
  GOTO 9998

END SELECT

! Calcolo data-ora di validita'
hh_tot = hhr + sca
data = datar + (hh_tot/24)
hh = MOD(hh_tot,24)
ier = 0

RETURN

! Messaggi d'errore
9999 CONTINUE
WRITE (*,*) "Errore ksec1_valid: time unit indicator non gestito ", &
  ksec1(15)
ier = 1
RETURN

9998 CONTINUE
WRITE (*,*) "Errore ksec1_valid: time range indicator non gestito ", &
  ksec1(18)
ier = 2
RETURN

END SUBROUTINE ksec1_valid

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION lowercase(chin) RESULT (chout)
!
! Replace uppercase letters with lowercase. Spaces and other non-literal 
! characters are left unchanged.
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
! Replace lowercase letters with uppercase. Spaces and other non-literal 
! characters are left unchanged.
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

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
WRITE (*,*) "Uso: grib2chimere.exe filein fileout filespc NZ [-roz vcoord orog] [-forc]"
WRITE (*,*) "  filespc: elenco specie richieste (LAT_SPEC)"
WRITE (*,*) "  NZ: numero di livelli verticali (codificati 109,k,0)"
WRITE (*,*) ""
WRITE (*,*) "  Con parametro -forc elabora previsioni con reftime fisso "
WRITe (*,*) "    (default: serie di analisi)"
WRITE (*,*) "  Con parametro -roz aggiunge altezza livelli e densita' aria "
WRITE (*,*) "    (inventati e costanti nel tempo):"
WRITE (*,*) "    vcoord: file con i coefficenti a e b dei livelli Chimere"
WRITE (*,*) "    orog: file grib con orografia relativa al grigliato di filein"
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

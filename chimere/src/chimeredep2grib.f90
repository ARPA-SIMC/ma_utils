PROGRAM chimeredep2grib
!--------------------------------------------------------------------------
! Legge un file di output di deposizioni di Chimere (una sola scadenza con
! deposizioni secche e umide realtiva a un run giornaliero) e scrive in 
! formato GRIB le deposioni secche ed umide di solfati, ammonio, nitrati.
!
! Uso: chimeredep2grib.exe filein fileout fileinfo filespcin igen 
!
!                                Versione 1.5, Enrico & Michele  02/12/2011
!--------------------------------------------------------------------------

USE date_handler
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
REAL, PARAMETER::fac=1.e4/6.022e23     ! conversione da molecole/cm2 a moli/m2

CHARACTER (Len=10),parameter::specie_dep(24)=(/ &
  "   H2SO4AQ","       SO2","   p1H2SO4","   p2H2SO4", &
  "   p3H2SO4","   p4H2SO4","   p5H2SO4","   p6H2SO4", &  ! H2SO4
  "     NH3AQ","       NH3","     p1NH3","     p2NH3", &
  "     p3NH3","     p4NH3","     p5NH3","     p6NH3", &  ! NH3
  "    HNO3AQ","      HNO3","    p1HNO3","    p2HNO3", &
  "    p3HNO3","    p4HNO3","    p5HNO3","    p6HNO3"/)   ! HNO3

! Dichiarazioni per GRIBEX.
INTEGER :: ksec0(2),ksec1(1024),ksec2(1024),ksec3(2),ksec4(512)
INTEGER :: kbuffer(maxdim),kword,kret
REAL    :: psec2(512),psec3(2)
REAL    :: field(maxdim)

! Altre variabili del programma
TYPE(date) :: datac,data_emi
DOUBLE PRECISION, ALLOCATABLE :: depdry(:,:),depwet(:,:)
REAL, ALLOCATABLE :: outdep(:,:)
REAL :: x1,y1,x2,y2,dx,dy,xrot,yrot,rdum
INTEGER :: version
INTEGER :: nvar,nvar_in,nlev,nx,ny,np,nvar_out,mm
INTEGER :: code_var(maxvar),tab_var(maxvar),lev_out(maxlev)
INTEGER :: cem,igen,idata,idata_ini,yy,scad_ini
INTEGER :: iu,k,kp,kvar,kscad,klev,ios,eof,eor,cnt_grb,idp
INTEGER :: ns_org,ns_mod,idx(maxvar)
INTEGER :: p1,ksm,kso,idum,hhc,hh_emi,bck_day
CHARACTER (LEN=80) :: chdum,filein,fileout,fileinfo,filespcin,arg(5)
CHARACTER (LEN=40) :: chrec,ids_org(maxvar)
CHARACTER (LEN=10) :: chdata
CHARACTER (LEN=3) :: proj

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

!--------------------------------------------------------------------------
! 1.1 Parametri da riga comando
idp = 0
version=2005

DO kp = 1,HUGE(kp)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP
  ELSE IF (TRIM(chdum) == "") THEN  
   EXIT
  ELSE
    idp = idp + 1
    arg(idp) = chdum
  ENDIF
ENDDO

filein = arg(1)
fileout = arg(2)
fileinfo = arg(3)
filespcin = arg(4)

READ (arg(5),*,IOSTAT=ios) igen
!write(6,*)filein,fileout,fileinfo
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
IF (cem <= 0 .OR. nlev < 0 .OR. x1 >= x2 .OR. y1 >= y2 .OR. nx <= 1 .OR. ny <= 1 ) GOTO 9997

nvar = k - nhead
nvar_out = COUNT(code_var(1:nvar) > 0)
nlev = 1

dx = (x2-x1)/REAL(nx-1)
dy = (y2-y1)/REAL(ny-1)
np = nx*ny

!--------------------------------------------------------------------------
! 1.3) Leggo l'elenco delle specie contenute nell'output deposizioni 
!      (coincide con l'elenco delle specie attive)

OPEN (UNIT=30, FILE=filespcin, STATUS="OLD", ACTION="READ", &
  FORM="FORMATTED")

ns_org = 0
DO kso = 1,HUGE(kso)
  READ (30,*,IOSTAT=ios) idum,chrec
  IF (ios /= 0) EXIT
  IF (ns_org >= maxvar) GOTO 9997

  chdum = ADJUSTL(chrec)
  ns_org = ns_org + 1
  p1 = INDEX(chdum," ")
  ids_org(ns_org) = chdum(1:p1-1)
ENDDO

! Trovo la posizione delle specie richieste nel file di input
idx(:) = 0
ns_mod=24
DO ksm = 1,ns_mod
  DO kso = 1,ns_org
  IF (TRIM(ADJUSTL(ids_org(kso))) == TRIM(ADJUSTL(specie_dep(ksm)))) THEN
    idx(ksm) = kso
    EXIT
  ENDIF
  ENDDO
  IF (idx(ksm) == 0) WRITE (*,*) "Specie ",TRIM(ADJUSTL(specie_dep(ksm))), &
    " non trovata, considero = 0."
ENDDO

WRITE (*,'(a,3i5,2f10.5)') "Parametri griglia (nx,ny,nz,dx,dy) ", &
  nx,ny,nlev,dx,dy
WRITE (*,'(2(a,i3))') "Specie da leggere: ",ns_org," da scrivere: ",nvar_out
WRITE (*,*) 

!--------------------------------------------------------------------------
! 1.4) Definisco la parte costante dell'header dei grib

! Sezione 1
ksec1(2) = cem
ksec1(3) = igen
ksec1(4) = 255
ksec1(5) = 128

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
! 1.5 Altre operazioni

! Alloco le varaibili
ALLOCATE (depdry(np,0:ns_org),depwet(np,0:ns_org),outdep(np,nvar_out))
depdry(:,:) = 0.
depwet(:,:) = 0.
outdep(:,:) = 0.

! Apro i files
OPEN (UNIT=31, FILE=filein, STATUS="OLD", FORM="UNFORMATTED", ERR=9996)
CALL PBOPEN (iu,fileout,'W',kret)

! Disabilito i controlli sui parametri GRIBEX
CALL grsvck(0)

! Trovo codice per EOF
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 2) Lettura, calcolo e scrittura: 

! Leggo deposizioni
READ (31,IOSTAT=ios) idata, &
 ((depdry(k,kvar),k=1,np),kvar=1,ns_org),((depwet(k,kvar),k=1,np),kvar=1,ns_org)
IF (ios /= 0) GOTO 9995

! Sommo le specie e le converto da molecole/(cm2*giorno) a moli/(m2*giorno)
outdep=0
DO ksm=1,8
  outdep(:,1) = outdep(:,1) + fac*REAL(depdry(:,idx(ksm)))
  outdep(:,2) = outdep(:,2) + fac*REAL(depwet(:,idx(ksm)))
ENDDO
DO ksm=9,16
  outdep(:,3) = outdep(:,3) + fac*REAL(depdry(:,idx(ksm)))
  outdep(:,4) = outdep(:,4) + fac*REAL(depwet(:,idx(ksm)))
ENDDO
DO ksm=17,24
  outdep(:,5) = outdep(:,5) + fac*REAL(depdry(:,idx(ksm)))
  outdep(:,6) = outdep(:,6) + fac*REAL(depwet(:,idx(ksm)))
ENDDO

DO kvar = 1,nvar_out
  WRITE (*,'(a,i4,3(2x,e14.7))') "OUTPUT  Campo,max,min,med: ",kvar, &
    MAXVAL(outdep(:,kvar)),MINVAL(outdep(:,kvar)),SUM(outdep(:,kvar))/REAL(np)
ENDDO

! Codifico Refernce time e timerange
! NB: nel file dep.sim e' scritto l'istante finale del periodo di media, 
! mentre il refernce time del grib e' l'istante iniziale.

WRITE (chdata,'(i10.10)') idata
READ (chdata,'(i4,3i2)',ERR=9999) datac%yy,datac%mm,datac%dd,hhc
IF (hhc > 23) GOTO 9994

IF (scad_ini == -1) THEN
  data_emi = datac - 1
  hh_emi = hhc
  ksec1(16) = 0
  ksec1(17) = 24
  ksec1(18) = 15

ELSE
  hh_emi = hhc - scad_ini - 24
  bck_day = (-1-hh_emi)/24 + 1

  data_emi = datac - bck_day
  hh_emi = hh_emi + bck_day * 24
  ksec1(16) = 24 * (datac-1 - data_emi) + (hhc - hh_emi)
  ksec1(17) = 24 * (datac - data_emi) + (hhc - hh_emi)
  IF (ksec1(16) < 0 .OR. ksec1(17) < 0) GOTO 9993
  ksec1(18) = 4

ENDIF

ksec1(15) = 1
ksec1(10) = 1 + MOD(data_emi%yy-1,100)
ksec1(21) = 1 + (data_emi%yy-1)/100
ksec1(11) = data_emi%mm
ksec1(12) = data_emi%dd
ksec1(13) = hh_emi

! Scrivo i grib
cnt_grb = 0
DO kvar = 1,nvar_out
  IF (code_var(kvar) <= 0) CYCLE

  ksec1(1) = tab_var(kvar)
  ksec1(6) = code_var(kvar)
  ksec1(7) = 109
  ksec1(8) = 1
  ksec1(9) = 0

  CALL GRIBEX (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4, &
    outdep(1:np,kvar),np,kbuffer,maxdim,kword,'C',kret)
  CALL PBWRITE(iu,kbuffer,ksec0(1),kret)
  IF (kret <= 0) WRITE(*,*) 'Errore pbwrite, kret ',kret

  cnt_grb = cnt_grb + 1
ENDDO

WRITE (*,'(a,i10,a,i6,a)') "Elaborata data  " ,idata, "  scritti ", &
  cnt_grb," grib"
CALL PBCLOSE (iu,kret)

!--------------------------------------------------------------------------
! 3) Rileggo il 1o record di filein per verificare se contiene altri dati
  
CLOSE (31)
OPEN (UNIT=31, FILE=filein, STATUS="OLD", FORM="UNFORMATTED")
READ (31,IOSTAT=ios) idata,depdry(:,:),depwet(:,:),rdum
IF (ios == 0) WRITE (*,*) "Warning: il file ",TRIM(filein), &
 " contiene altri dati!"
CLOSE(31)

STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

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

9994 CONTINUE
WRITE (*,*) "Ora illegale in input: ",TRIM(filein),hhc

9993 CONTINUE
WRITE (*,*) "Errore nel calcolo timerange!!!"

9992 CONTINUE
WRITE (*,*) "Date inconsistenti in ",TRIM(filein)
STOP

END PROGRAM chimeredep2grib

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
!
! Scrive a scehmo l'help del programma
!
!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: chimeredep2grib.exe filein fileout fileinfo filespcin igen [-h]"
WRITE (*,*) ""
WRITE (*,*) "filein:    dep.sim (output di un run giornaliero di Chimere)"
WRITE (*,*) "fileout:   deposizioni secche e umide di solfato, ammonio, nitrati"
WRITE (*,*) "fileinfo:  CHIMEREDEP_INFO.DAT"
WRITE (*,*) "filespcin: file ACTIVE_SPECIES relativo al run Chimere"
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

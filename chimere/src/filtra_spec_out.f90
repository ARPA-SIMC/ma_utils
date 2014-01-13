PROGRAM filtra_spec_out
!--------------------------------------------------------------------------
! Legge l'output unformatted di un run Chimere e scrive un file con le 
! specie specificate in LAT_SPEC, a cui aggiunge in ogni caso Z e Ro.
!
! Programma usato dalla catena Chimere (crea_input_bc.ksh)
!
!                               Versione 1.1.1, Enrico & Michele 13/01/2014
!--------------------------------------------------------------------------

IMPLICIT NONE

! Parametri costanti
INTEGER, PARAMETER :: maxspec = 200      ! max n.ro di specie nei files
INTEGER, PARAMETER :: maxdim =  10000000 ! max n.ro di dati in un record
REAL, PARAMETER :: rodef = 2.5E19        ! valore costante per dens. aria

REAL,ALLOCATABLE :: conc_in(:,:),conc_out(:,:)
REAL :: dum_arr(maxdim)
INTEGER :: np3d,ns_org,ns_mod,ns_file,ns_ok,idx(maxspec),ntry
INTEGER :: ios,eof,eor,p1,ksm,kso,kscad,idum,n1,n2,idata
CHARACTER (LEN=40) :: chrec,chdum,ids_org(maxspec),ids_mod(maxspec)
CHARACTER (LEN=200) :: fout_org,fout_mod,fout_spec,flat_spec,chpar

!--------------------------------------------------------------------------
! 1) Elaborazioni preliminari

! Parametri da riga comando
CALL getarg(1,fout_org)
CALL getarg(2,fout_spec)
CALL getarg(3,fout_mod)
CALL getarg(4,flat_spec)
CALL getarg(5,chpar)
READ (chpar,*,IOSTAT=ios) np3d

IF (fout_org == "" .OR. fout_spec == "" .OR. fout_mod == "" .OR. &
    flat_spec == "" .OR. ios /= 0) THEN
  CALL write_help
  STOP
ENDIF

! Codice per EOF
CALL get_eof_eor(eof,eor)
 write(6,*)'nver'
!--------------------------------------------------------------------------
! 2) Trovo il numero di specie effettivamente contenute in fout_org

! Ordine di grandezza
ntry = 1
DO 
  IF (ntry > maxdim/10) EXIT

  ntry = ntry * 10
  OPEN (UNIT=20, FILE=fout_org, STATUS="OLD",&
!, ACTION="READ", &
    FORM="UNFORMATTED", ERR=9993)
  READ (20,IOSTAT=ios) dum_arr(1:ntry)
   write(6,*)idata,ios,ntry 
  IF (ios /= 0) EXIT
  CLOSE(20)

ENDDO

! Ricerca binaria del numero esatto
n1 = ntry/10             ! n.ro piu' alto letto
n2 = ntry                ! n.ro piu' basso non letto

DO
  ntry = (n2+n1)/2
   write(6,*)'aaaa ntry ',ntry 

  OPEN (UNIT=20, FILE=fout_org, STATUS="OLD",  &
    FORM="UNFORMATTED")
  READ (20,IOSTAT=ios)idata, dum_arr(1:ntry)
  CLOSE(20)
  IF (ios == 0) THEN
    n1 = ntry
  ELSE
    n2 = ntry
  ENDIF

  IF (n2-n1 == 1) THEN
    EXIT
  ELSE IF (n2-n1 < 1) THEN
    WRITE (*,*) "Errore ricerca binaria"
    STOP
  ENDIF

ENDDO

IF (MOD(n1,np3d) /= 0) GOTO 9990
ns_file = n1/np3d

!--------------------------------------------------------------------------
! 3) Leggo le liste delle specie e trovo le corrispondenze
! Specie originali
OPEN (UNIT=30, FILE=fout_spec, STATUS="OLD", ACTION="READ", &
  FORM="FORMATTED", ERR=9999)

ns_org = 0
DO kso = 1,HUGE(kso)
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998
  IF (ns_org >= maxspec) GOTO 9997

  chdum = ADJUSTL(chrec)
  ns_org = ns_org + 1  
  p1 = INDEX(chdum," ")
  IF (p1 == 0) GOTO 9998
  ids_org(ns_org) = chdum(1:p1-1)
ENDDO

CLOSE (30)

! Specie richieste
OPEN (UNIT=31, FILE=flat_spec, STATUS="OLD", ACTION="READ", &
  FORM="FORMATTED", ERR=9996)

ns_mod = 0
DO ksm = 1,HUGE(ksm)
  READ (31,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9995
  IF (ns_mod >= maxspec-2) GOTO 9994

  chdum = ADJUSTL(chrec)
  ns_mod = ns_mod + 1  
  p1 = INDEX(chdum," ")
  IF (p1 == 0) GOTO 9995
  ids_mod(ns_mod) = chdum(1:p1-1)
ENDDO

CLOSE (31)

! Trovo la posizione nel file originario delle specie richieste
idx(:) = 0
DO ksm = 1,ns_mod
  DO kso = 1,ns_org
  IF (ids_org(kso) == ids_mod(ksm)) THEN
    idx(ksm) = kso
    EXIT
  ENDIF
  ENDDO
ENDDO
ns_ok = COUNT(idx(1:ns_mod) /= 0)

! Riepilogo a schermo
WRITE (*,'(a)') "filtra_spec_out.exe:"
WRITE (*,'(a,3i5)') "  Specie in file input, lista input, lista output: ", &
  ns_file, ns_org, ns_mod
WRITE (*,'(a,2i5)') "  Specie abbinate, messe a 0: ",ns_ok, ns_mod-ns_ok

! Gestione Z e Ro
IF (ns_file == ns_org + 2) THEN
  idx(ns_mod+1) = ns_org+1    ! Abbino Z
  idx(ns_mod+2) = ns_org+2    ! Abbino Ro
  ns_org = ns_org + 2
  ns_mod = ns_mod + 2
  WRITE (*,'(a)') "  Z e Ro abbinate"

ELSE IF (ns_file == ns_org + 1) THEN
  
  idx(ns_mod+1) = ns_org+1    ! Abbino Z
  idx(ns_mod+2) = -1          ! invento Ro
  ns_org = ns_org + 1
  ns_mod = ns_mod + 2
  WRITE (*,'(a)') "  Z abbinata, Ro inventata"

ELSE
  WRITE (*,'(a)') "Errore, file originario non include Z "
  STOP

ENDIF

!--------------------------------------------------------------------------
! 4) Leggo e riscrivo (ciclo sugli istanti)
ALLOCATE (conc_in(np3d,ns_org),conc_out(np3d,ns_mod))
OPEN (UNIT=40, FILE=fout_org, STATUS="OLD", ACTION="READ", &
  FORM="UNFORMATTED", ERR=9993)
OPEN (UNIT=41, FILE=fout_mod, STATUS="REPLACE", ACTION="WRITE", &
  FORM="UNFORMATTED")

DO kscad = 1, HUGE(kscad)

  READ (40,IOSTAT=ios) idata,conc_in(1:np3d,1:ns_org)
  write(6,*)idata,np3d,ns_org
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9992
  DO ksm = 1,ns_mod
    IF (idx(ksm) > 0) THEN
      conc_out(1:np3d,ksm) = conc_in(1:np3d,idx(ksm))
    ELSE IF (idx(ksm) == 0) THEN
      conc_out(1:np3d,ksm) = 0.
    ELSE IF (idx(ksm) == -1) THEN
      conc_out(1:np3d,ksm) = rodef
    ENDIF
  ENDDO
  WRITE (41) idata,conc_out(1:np3d,1:ns_mod)

ENDDO

CLOSE (40)
CLOSE (41)

!do ksm=1,ns_mod
!write (*,'(i3,a10,i3,1x,e12.5)') ksm,TRIM(ids_mod(ksm)),idx(ksm), &
!  SUM(conc_in(1:np3d,idx(ksm)))/np3d
!enddo

WRITE (*,'(a,i3,a)') "  Input file terminato, elaborate ",kscad-1," scadenze"
STOP

!--------------------------------------------------------------------------
! 5) Gestione erori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fout_spec)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(fout_spec)," record ",kso
STOP

9997 CONTINUE
WRITE (*,*) "N.ro max di specie superato in ",TRIM(fout_spec),maxspec
STOP

9996 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(flat_spec)
STOP

9995 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(flat_spec)," record ",ksm
STOP

9994 CONTINUE
WRITE (*,*) "N.ro max di specie superato in ",TRIM(flat_spec),maxspec
STOP

9993 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(fout_org)
STOP

9992 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(fout_org)," record ",kscad
STOP

9990 CONTINUE
WRITE (*,*) "Numero di dati incompatibile con numero punti: ",n1,np3d
STOP

END PROGRAM filtra_spec_out

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

SUBROUTINE write_help
!
! Scrive a schermo l'help del programma
!
!            123456789012345678901234567890123456789012345678901234567890123456789012345

WRITE (*,*) "Uso: filtra_spec_out.exe out_org out_spec out_mod lat_spec np3d"
WRITE (*,*) "Legge l'output Chimere e lo scrive lasciando solo le specie richieste"
WRITE (*,*) ""
WRITE (*,*) "out_org  (IN): output unformatted chimere originario"
WRITE (*,*) "out_spec (IN): lista specie in out_ogr "
WRITE (*,*) "out_mod (OUT): output unformatted chimere modificato"
WRITE (*,*) "lat_spec (IN): lista specie richieste in out_mod "
WRITE (*,*) "np3d     (IN): numero di punti 3d in out_org e out_mod"
WRITE (*,*)

END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

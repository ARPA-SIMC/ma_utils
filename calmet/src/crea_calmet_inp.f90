PROGRAM crea_calmet_inp
!-------------------------------------------------------------------------------
! Programma della catena Calmet.
! Modifica il calmet.inp base di un progetto, inserendo i parametri che 
! dipendono dalla data e dalle osservazioni disponibili.
! 
! Metodo:
! legge filemodello (calmet.inp base del progetto) e lo riscrive su calmet.inp;
! cerca l'inizio dei gruppi da modificare, e all'interno di cascuno ricostruisce
! i record che contengono i parametri da modificare.
!
!   param         gruppo  descrizione             origine dato modificato
!
! - NUSTA         0.a     n.ro di stazioni temp   srq_temp_PROJ.lst, up*.dat
! - UPDAT         0.b     nome file temp          srq_temp_PROJ.lst
! - IBYR,IBMO ... 1       data iniziale           date_calmet.inp
! - IRLG          1       lunghezza run           date_calmet.inp
! - NX, Ny ...    2       parametri griglia       aree_utm.dat (pre_calmet.inp)
! - NSSTA         4       n.ro stazioni superf.   srq_surf_PROJ.lst
! - SS**          7       anagrafica staz.sup.    srq_surf_PROJ.lst
! - US**          8       anagrafica staz.temp.   srq_temp_PROJ.lst
! 
! Compilazione:
! Usa il modulo per la gestione date date_hander.f90
!
! Sviluppi:
! abolire il controllo sui TEMP, che e' ridondante (viene ripetuto da 
! test_inp_calmet); vedi crea_calmet_inp.f90.v2dev
!
! Uso:
! crea_calmet_inp.exe filemodello filedate filesurf filetemp stept_req vers_calmet [-h]
! Con parametro -noobs: ignora filesurf, filetemp e stept_req, non scrive nessun
!   dato relativo alle osservazioni
!
!                                                      V4.3.2, Enrico 13/01/2014
!-------------------------------------------------------------------------------

USE date_handler
IMPLICIT NONE

! Parametri costanti. Le versioni Calmet fino a 5.6 usano IBTZ col segno cambiato
INTEGER, PARAMETER :: ibtz = +1, ibtz_old = -ibtz
CHARACTER (LEN=80), PARAMETER :: fileout = "calmet.inp"

! Path di default delle tabelle seriet
! PKGDATAROOTDIR viene sostituito in fase di compilazione con il path delle
! tabelle seriet (di solito /usr/share/ma_utils). La sostituzione sfrutta 
! il comando gfortran -D; vedi Makefile.am nelle singole dir.
CHARACTER (LEN=40) :: tab_path_def = PKGDATAROOTDIR
CHARACTER (LEN=40) :: tab_env = "MA_UTILS_DAT"

! Parametri da modificate
REAL, ALLOCATABLE :: ss_x(:),ss_y(:),ss_anh(:),ss_ibtz(:)
REAL, ALLOCATABLE :: us_x(:),us_y(:),us_ibtz(:)
INTEGER, ALLOCATABLE :: ss_id(:),us_id(:)
CHARACTER (LEN=9), ALLOCATABLE :: us_file(:)
CHARACTER (LEN=5), ALLOCATABLE :: ss_str(:),us_str(:)
CHARACTER (LEN=4), ALLOCATABLE :: ss_name(:),us_name(:)

REAL :: dgridkm,xorigkm,yorigkm,xlat0,xlon0
INTEGER :: ibyr,ibmo,ibdy,ibhr,irlg,ieyr,iemo,iedy,iehr
INTEGER :: nusta,nssta,nowsta,nm3d,nigf
INTEGER :: nx,ny,iutmzn

! Funzioni
CHARACTER (LEN=100) :: exinp,exinp_val

! Gestione parametri 
INTEGER :: kpar,cnt_par
CHARACTER (LEN=100) :: chpar

! Altre variabili del programma
LOGICAL, ALLOCATABLE :: temp_ok(:)
TYPE (date) :: data1,data2,data1_lst,data2_lst
REAL :: lon,lat,xutm,yutm,dum_x1,dum_x2,dum_y1,dum_y2,dx,dy
INTEGER :: eof,eor,ios,ios2,ier,cnt,cnt_ok,odg,idum,p,k
INTEGER :: vers_calmet
INTEGER :: hh1,hh2,hht,stept_req,id_net,id_usr
INTEGER :: dum_nx,dum_ny,dum_utmz,nusta_req
CHARACTER (LEN=200) :: chrec,chfmt,rec_out,chxyz
CHARACTER (LEN=200) :: filemodello,filedate,filesurf,filetemp,nfile,tab_path
CHARACTER (LEN=20) :: ch20
CHARACTER (LEN=10) :: dum_area,id_area
LOGICAL :: req_obs

!===============================================================================
! 1) Preliminari

! 1.0 IOSTAT per eof,eor
CALL get_eof_eor(eof,eor)

! 1.1 parametri da riga comandi
req_obs = .TRUE.
cnt_par = 0
DO kpar = 1,HUGE(0)
  CALL getarg(kpar,chpar)

  SELECT CASE (TRIM(chpar))
  CASE ("")
    EXIT
  CASE ("-h")
    WRITE (*,*) "Uso: crea_calmet_inp.exe filemodello filedate ", &
      "filesurf filetemp stept_req vers_calmet [-noobs]"
    STOP
 
  CASE ("-noobs")
    req_obs=.FALSE.

  CASE DEFAULT
    cnt_par = cnt_par + 1
    SELECT CASE(cnt_par)
    CASE(1)
      filemodello = TRIM(chpar)
    CASE(2)
      filedate = TRIM(chpar)
    CASE(3)
      filesurf = TRIM(chpar)
    CASE(4)
      filetemp = TRIM(chpar)
    CASE(5)
      READ (chpar,*,IOSTAT=ios) stept_req
    CASE(6)
      READ (chpar,*,IOSTAT=ios2) vers_calmet
    END SELECT

  END SELECT
ENDDO

IF (cnt_par /= 6 .OR. ios /= 0 .OR. ios2 /= 0) THEN
  WRITE (*,*) "Uso: crea_calmet_inp.exe filemodello filedate ", &
    "filesurf filetemp stept_req vers_calmet [-noobs]"
  STOP
ENDIF  

IF (vers_calmet > 52 .AND. vers_calmet < 57) THEN
  WRITE (*,*) "Gestione IBTZ non implemenatata per versioni Calmet 5.3 - 5.6"
  STOP
ENDIF

!===============================================================================
! 2) Leggo/calcolo il valore dei parametri che dovranno essere modificati

WRITE (*,'(a)') "crea_calmet_inp: inizio esecuzione"

!-------------------------------------------------------------------------------
! 2.1 Date: leggo la data iniziale, la converto da GMT a LST, calcolo la 
!     lughezza del run. L'anno ha 2 cifre per le versioni di calmet fino a 5.0, 
!     4 cifre per le versioni successive.

OPEN (UNIT=20, FILE=filedate, STATUS="OLD", ACTION="READ", ERR=9999)
READ (20,*, ERR=9999)
READ (20,*, ERR=9999)
READ (20,*, ERR=9999)
READ (20,'(i4,3i2)', ERR=9999) data1%yy,data1%mm,data1%dd,hh1
READ (20,'(i4,3i2)', ERR=9999) data2%yy,data2%mm,data2%dd,hh2
CLOSE (20)

irlg = (data2 - data1)*24 + (hh2 - hh1)

IF (hh1 + ibtz < 0) THEN        ! giorno LST < giorno GMT
  WRITE (*,*) "  Warning: la data iniziale LST e' diversa da quella GMT"
  ibhr = hh1 + ibtz + 24
  data1_lst = data1 - 1
ELSE IF (hh1 + ibtz > 24) THEN  ! giorno LST > giorno GMT
  WRITE (*,*) "  Warning: la data iniziale LST e' diversa da quella GMT"
  ibhr = hh1 + ibtz - 24
  data1_lst = data1 + 1
ELSE                            ! giorno LST = giorno GMT
  ibhr = hh1 + ibtz
  data1_lst = data1
ENDIF

IF (vers_calmet <= 50) THEN
  ibyr = MOD(data1_lst%yy,100)
ELSE
  ibyr = data1_lst%yy
ENDIF
ibmo = data1_lst%mm
ibdy = data1_lst%dd

IF (vers_calmet > 52) THEN
  hht = ibhr + irlg
  data2_lst = data1_lst + hht/24

  iehr = MOD(hht,24)
  ieyr = data2_lst%yy
  iemo = data2_lst%mm
  iedy = data2_lst%dd
ENDIF

!-------------------------------------------------------------------------------
! 2.2 Parametri dell'area

! Leggo da pre_calmet.inp il nome dell'area richiesta
OPEN (UNIT=23, FILE="pre_calmet.inp", STATUS="OLD", ACTION="READ",ERR=9996)
DO k = 1,8
  READ (23,*,ERR=9996)
ENDDO
READ (23,'(a)',ERR=9996) chrec
p = INDEX(ADJUSTL(chrec)," ")
id_area = chrec(1:MIN(p-1,10))

! Leggo da aree_utm.dat i parametri dell'area richiesta
CALL GETENV(tab_env,tab_path)
IF (TRIM(tab_path) == "") tab_path = tab_path_def
WRITE (nfile,'(2a)') TRIM(tab_path),"/aree_utm.dat"
OPEN (UNIT=24, FILE=nfile, STATUS="OLD", ACTION="READ", ERR=9995)

DO
  READ (24,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) THEN
    WRITE (*,*) "Area ",TRIM(id_area)," non trovata in ",TRIM(nfile)
    STOP
  ELSE IF (ios /= 0) THEN
    GOTO 9995
  ENDIF
  IF (chrec == "" .OR. chrec(1:1) == "!") CYCLE

  READ (chrec,'(a10,2(1x,i4),4(1x,f8.3),1x,i4)',IOSTAT=ios) &
      dum_area,dum_nx,dum_ny,dum_x1,dum_y1,dum_x2,dum_y2,dum_utmz
  IF (ios /= 0) GOTO 9995

  IF (TRIM(dum_area) == TRIM(id_area)) EXIT
ENDDO

nx = dum_nx
ny = dum_ny
dx = (dum_x2 - dum_x1) / REAL(dum_nx - 1)
dy = (dum_y2 - dum_y1) / REAL(dum_ny - 1)

dgridkm = MIN(dx,dy)
xorigkm = dum_x1 - dx/2.
yorigkm = dum_y1 - dy/2.
iutmzn = REAL(dum_utmz)

CALL utm2ll(xorigkm,yorigkm,iutmzn,.FALSE.,lat,lon)
xlat0 = lat
xlon0 = -lon

CLOSE(24)


!-------------------------------------------------------------------------------
! 2.3 Stazioni superficiali: 

IF (req_obs) THEN

! Conto le stazioni
  OPEN (UNIT=21, FILE=filesurf, STATUS="OLD", ACTION="READ", ERR=9998)
  READ (21,*,ERR=9998)
  
  cnt = 0
  DO
    READ (21,'(a)',IOSTAT=ios) chrec
    IF (ios /= 0) EXIT
    IF (chrec == "") CYCLE
  
!   READ (chrec,'(i2,i5,1x,a20,2(1x,f8.3))',IOSTAT=ios) id_net,id_usr,ch20,lat,lon 
    READ (chrec,'(i2,i5,1x,a20,a)',IOSTAT=ios) id_net,id_usr,ch20,chxyz
    READ (chxyz,*,IOSTAT=ios2) lat,lon

    IF (ios /= 0 .OR. ios2 /= 0) THEN
      WRITE (*,*) "  record illegale in filesurf, skippo: ",TRIM(chrec)
      CYCLE
    ENDIF
  
    cnt = cnt + 1
  ENDDO
  CLOSE(21)
  
  nssta = cnt
  IF (nssta > 999) WRITE (*,*) "  Warning crea_calmet_inp, troppe stazioni: ", &
    nssta
  
! Alloco arrays
  ALLOCATE (ss_str(nssta),ss_name(nssta),ss_id(nssta),ss_x(nssta),ss_y(nssta))
  ALLOCATE (ss_ibtz(nssta),ss_anh(nssta))
  
! Rileggo il file, calcolo le coordinate, definisco gli altri parametri
  OPEN (UNIT=21, FILE=filesurf, STATUS="OLD", ACTION="READ")
  READ (21,*)
  
  cnt = 0
  DO
    READ (21,'(a)',IOSTAT=ios) chrec
    IF (ios /= 0) EXIT
    IF (chrec == "") CYCLE
!   READ (chrec,'(i2,i5,1x,a20,2(1x,f8.3))',IOSTAT=ios) id_net,id_usr,ch20,lat,lon 
    READ (chrec,'(i2,i5,1x,a20,a)',IOSTAT=ios) id_net,id_usr,ch20,chxyz
    READ (chxyz,*,IOSTAT=ios2) lat,lon
    IF (ios /= 0 .OR. ios2 /= 0) CYCLE
  
    cnt = cnt + 1
  
    IF (cnt < 10) THEN
      odg = 1
    ELSE IF (cnt < 100) THEN
      odg = 2
    ELSE 
      odg = 3
    ENDIF
    WRITE (chfmt,'(a,i1,a)') "(a2,i",odg,")"
    WRITE (ss_str(cnt),chfmt) "SS",cnt
  
    WRITE (ss_name(cnt),'(i4.4)') cnt
    ss_id(cnt) = id_usr
  
    CALL ll2utm(lat,lon,iutmzn,xutm,yutm,idum)
  
    ss_x(cnt) = xutm
    ss_y(cnt) = yutm
    IF (vers_calmet <= 52) THEN
      ss_ibtz(cnt) = ibtz_old
    ELSE
      ss_ibtz(cnt) = ibtz
    ENDIF
    ss_anh(cnt) = 10.
  
  ENDDO
  CLOSE(21)

ELSE

  nssta = 0

ENDIF

!-------------------------------------------------------------------------------
! 2.4 Stazioni in quota: le conto, calcolo le coord., costruisco nomi files up*,
!     verifico che contengano dati sufficienti per essere usate da Calmet

IF (req_obs) THEN

! Conto le stazioni
  OPEN (UNIT=22, FILE=filetemp, STATUS="OLD", ACTION="READ", ERR=9997)
  READ (22,*,ERR=9998)
  
  cnt = 0
  DO
    READ (22,'(a)',IOSTAT=ios) chrec
    IF (ios /= 0) EXIT
    IF (chrec == "") CYCLE
  
!   READ (chrec,'(i2,i5,1x,a20,2(1x,f8.3))',IOSTAT=ios) id_net,id_usr,ch20,lat,lon 
    READ (chrec,'(i2,i5,1x,a20,a)',IOSTAT=ios) id_net,id_usr,ch20,chxyz
    READ (chxyz,*,IOSTAT=ios2) lat,lon

    IF (ios /= 0 .OR. ios2 /= 0) THEN
      WRITE (*,*) "  record illegale in filetemp, skippo: ",TRIM(chrec)
      CYCLE
    ENDIF
  
    cnt = cnt + 1
  ENDDO
  CLOSE(22)
  
  nusta_req = cnt
  IF (nusta_req > 999) WRITE (*,*) &
    "  Warning crea_calmet_inp, troppe stazioni: ",nusta_req
  
! Alloco arrays
  ALLOCATE (us_str(nusta_req),us_name(nusta_req),us_id(nusta_req), &
            us_x(nusta_req),us_y(nusta_req))
  ALLOCATE (us_ibtz(nusta_req),us_file(nusta_req))
  ALLOCATE (temp_ok(nusta_req))
  
! Rileggo il file, calcolo le coordinate, definisco gli altri parametri
  OPEN (UNIT=22, FILE=filetemp, STATUS="OLD", ACTION="READ")
  READ (22,*)
  
  cnt = 0
  cnt_ok = 0
  temp_ok(:) = .FALSE.
  DO
    READ (22,'(a)',IOSTAT=ios) chrec
    IF (ios /= 0) EXIT
    IF (chrec == "") CYCLE
!   READ (chrec,'(i2,i5,1x,a20,2(1x,f8.3))',IOSTAT=ios) id_net,id_usr,ch20,lat,lon
    READ (chrec,'(i2,i5,1x,a20,a)',IOSTAT=ios) id_net,id_usr,ch20,chxyz
    READ (chxyz,*,IOSTAT=ios2) lat,lon

    IF (ios /= 0 .OR. ios2 /= 0) CYCLE
  
    cnt = cnt + 1
  
    IF (cnt < 10) THEN
      odg = 1
    ELSE IF (cnt < 100) THEN
      odg = 2
    ELSE 
      odg = 3
    ENDIF
  
    WRITE (chfmt,'(a,i1,a,i1,a)') "(a2,i",odg,".",odg,",a4)"
    WRITE (us_file(cnt),chfmt) "up",cnt,".dat"
  
    CALL test_temp(us_file(cnt),data1,hh1,data2,hh2,stept_req,temp_ok(cnt),ier)
  
    IF (.NOT. temp_ok(cnt)) THEN
      WRITE (*,'(2(a,i2))') "  scarto il Temp ",cnt," errore ",ier
      CYCLE
    ENDIF
    cnt_ok = cnt_ok + 1
  
    WRITE (chfmt,'(a,i1,a)') "(a2,i",odg,")"
    WRITE (us_str(cnt_ok),chfmt) "US",cnt_ok
    WRITE (us_name(cnt_ok),'(i4.4)') cnt
    us_id(cnt_ok) = id_usr
  
    CALL ll2utm(lat,lon,iutmzn,xutm,yutm,idum)
    us_x(cnt_ok) = xutm
    us_y(cnt_ok) = yutm
    IF (vers_calmet <= 52) THEN
      us_ibtz(cnt_ok) = ibtz_old
    ELSE  
      us_ibtz(cnt_ok) = ibtz
    ENDIF

  ENDDO
  CLOSE(22)
  
  nusta = COUNT(temp_ok(1:nusta_req))

ELSE

  nusta = 0
  nusta_req = 0

ENDIF

!===============================================================================
! 3) Leggo filemodello e lo riscrivo (modificato) su calmet.inp

WRITE (*,'(a)') "  calcolati i nuovi parametri"

OPEN (UNIT=30, FILE=filemodello, STATUS="OLD", ACTION="READ", ERR=9994)
OPEN (UNIT=40, FILE=fileout, STATUS="REPLACE", ACTION="WRITE")

!-------------------------------------------------------------------------------
! 3.0.1 run title e gruppo 0a (inserisco numero stazioni in quota)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"NUSTA") /= 0) THEN
    CONTINUE

  ELSE IF (INDEX(exinp(chrec),"END") /= 0) THEN
    WRITE (40,'(a,i5,a)') "! NUSTA = ",nusta," !"
    WRITE (40,*)
    WRITE (40,'(a)') TRIM(chrec)
    EXIT
  ELSE
    WRITE (40,'(a)') TRIM(chrec)
  ENDIF

ENDDO

!-------------------------------------------------------------------------------
! 3.0.2 gruppi 0b-0f (inserisco nomi files stazioni in quota)
! Procedo fino a trovare !END! alla fine del gruppo 0f; riscrivo senza 
! modifiche gli !END! dei gruppi 0c,0d,0e

WRITE (40,*)
WRITE (40,'(a)') "Upper air files (one per station):"
DO k = 1,nusta_req
  IF (temp_ok(k)) WRITE (40,'(3a)') "! UPDAT=./",us_file(k)," ! !END!"
ENDDO

DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"UPDAT") /= 0) THEN
    CONTINUE
  ELSE IF (INDEX(exinp(chrec),"SEADAT") /= 0) THEN
    WRITE (40,'(a)') TRIM(chrec)
  ELSE IF (INDEX(exinp(chrec),"MM3DAT") /= 0) THEN
    WRITE (40,'(a)') TRIM(chrec)
  ELSE IF (INDEX(exinp(chrec),"IGFDAT") /= 0) THEN
    WRITE (40,'(a)') TRIM(chrec)
  ELSE IF (INDEX(exinp(chrec),"END") /= 0) THEN
    WRITE (40,'(a)') TRIM(chrec)
    EXIT
  ELSE
    WRITE (40,'(a)') TRIM(chrec)
  ENDIF

ENDDO

!-------------------------------------------------------------------------------
! 3.1 gruppo 1 (inserisco data iniziale e lunghezza del run)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"IBYR") /= 0   .OR. &
      INDEX(exinp(chrec),"IBMO") /= 0   .OR. &
      INDEX(exinp(chrec),"IBDY") /= 0   .OR. &
      INDEX(exinp(chrec),"IBHR") /= 0   .OR. &
      INDEX(exinp(chrec),"IBTZ") /= 0   .OR. &
      INDEX(exinp(chrec),"IRLG") /= 0         ) THEN

    CONTINUE

  ELSE IF (INDEX(exinp(chrec),"END") /= 0) THEN

    IF (vers_calmet <= 52) THEN
      WRITE (40,'(6x,a,i5,a)') "! IBYR = ",ibyr," !"
      WRITE (40,'(6x,a,i3,a)') "! IBMO = ",ibmo," !"
      WRITE (40,'(6x,a,i3,a)') "! IBDY = ",ibdy," !"
      WRITE (40,'(6x,a,i3,a)') "! IBHR = ",ibhr," !"
      WRITE (40,'(6x,a,i3,a)') "! IBTZ = ",ibtz_old," !"
      WRITE (40,'(6x,a,i3,a)') "! IRLG = ",irlg," !"
      WRITE (40,*)
      WRITE (40,'(a)') TRIM(chrec)
      EXIT

    ELSE 
      WRITE (40,'(6x,a,i5,a)') "! IBYR = ",ibyr," !"
      WRITE (40,'(6x,a,i3,a)') "! IBMO = ",ibmo," !"
      WRITE (40,'(6x,a,i3,a)') "! IBDY = ",ibdy," !"
      WRITE (40,'(6x,a,i3,a)') "! IBHR = ",ibhr," !"
      WRITE (40,'(6x,a,i3,a)') "! IBSEC = ",0,"   !"

      WRITE (40,'(6x,a,i5,a)') "! IEYR = ",ieyr," !"
      WRITE (40,'(6x,a,i3,a)') "! IEMO = ",iemo," !"
      WRITE (40,'(6x,a,i3,a)') "! IEDY = ",iedy," !"
      WRITE (40,'(6x,a,i3,a)') "! IEHR = ",iehr," !"
      WRITE (40,'(6x,a,i3,a)') "! IESEC = ",0,"   !"

      IF (ibtz < 0) THEN
        WRITE (40,'(6x,a,i2.2,a)') "! ABTZ = UTC-",-ibtz,"00 !"
      ELSE
        WRITE (40,'(6x,a,i2.2,a)') "! ABTZ = UTC+",ibtz,"00 !"
      ENDIF

      WRITE (40,*)
      WRITE (40,'(a)') TRIM(chrec)
      EXIT

    ENDIF

  ELSE

    WRITE (40,'(a)') TRIM(chrec)

  ENDIF

ENDDO

!-------------------------------------------------------------------------------
! 3.2 gruppo 2 (inserisco parametri griglia)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"NX") /= 0      .OR. &
      INDEX(exinp(chrec),"NY") /= 0      .OR. &
      INDEX(exinp(chrec),"DGRIDKM") /= 0 .OR. &
      INDEX(exinp(chrec),"XORIGKM") /= 0 .OR. &
      INDEX(exinp(chrec),"YORIGKM") /= 0 .OR. &
      INDEX(exinp(chrec),"XLAT0") /= 0   .OR. &
      INDEX(exinp(chrec),"XLON0") /= 0   .OR. &
      INDEX(exinp(chrec),"IUTMZN") /= 0        ) THEN

    CONTINUE

  ELSE IF (INDEX(exinp(chrec),"END") /= 0) THEN

    WRITE (40,'(6x,a,i5,a)')   "! NX = ",nx," !"
    WRITE (40,'(6x,a,i5,a)')   "! NY = ",ny," !"
    WRITE (40,'(6x,a,f9.3,a)') "! DGRIDKM = ",dgridkm," !"
    WRITE (40,'(6x,a,f9.3,a)') "! XORIGKM = ",xorigkm," !"
    WRITE (40,'(6x,a,f9.3,a)') "! YORIGKM = ",yorigkm," !"
    WRITE (40,'(6x,a,i5,a)')   "! IUTMZN  = ",iutmzn, " !"
    IF (vers_calmet <= 52) THEN
      WRITE (40,'(6x,a,f9.3,a)') "! XLAT0   = ",xlat0,  " !"
      WRITE (40,'(6x,a,f9.3,a)') "! XLON0   = ",xlon0,  " !"
    ENDIF
    WRITE (40,*)
    WRITE (40,'(a)') TRIM(chrec)
    EXIT

  ELSE

    WRITE (40,'(a)') TRIM(chrec)

  ENDIF

ENDDO


!-------------------------------------------------------------------------------
! 3.3 gruppo 3 (immutato)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"END") /= 0) THEN
    WRITE (40,'(a)') TRIM(chrec)
    EXIT
  ELSE
    WRITE (40,'(a)') TRIM(chrec)
  ENDIF

ENDDO

!-------------------------------------------------------------------------------
! 3.4 gruppo 4 (inserisco numero stazioni superficiali)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"NSSTA") /= 0) THEN
    CONTINUE
  ELSE IF (INDEX(exinp(chrec),"END") /= 0) THEN
    WRITE (40,'(6x,a,i5,a)') "! NSSTA = ",nssta," !"
    WRITE (40,*)
    WRITE (40,'(a)') TRIM(chrec)
    EXIT
  ELSE
    WRITE (40,'(a)') TRIM(chrec)
  ENDIF

ENDDO

!-------------------------------------------------------------------------------
! 3.5 gruppo 5 (immutato)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"END") /= 0) THEN
    WRITE (40,'(a)') TRIM(chrec)
    EXIT
  ELSE
    WRITE (40,'(a)') TRIM(chrec)
  ENDIF

ENDDO

!-------------------------------------------------------------------------------
! 3.6 gruppo 6 (immutato)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"END") /= 0) THEN
    WRITE (40,'(a)') TRIM(chrec)
    EXIT
  ELSE
    WRITE (40,'(a)') TRIM(chrec)
  ENDIF

ENDDO

!-------------------------------------------------------------------------------
! 3.7 gruppo 7 (inserisco anagrafica stazioni superficiali)
!     NB: insrisco tutte  le stazioni, perche' in surf.dat ci sono sempre tutte,
!     ev. con tutti i dati mancanti
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"END") /= 0) THEN
    DO k = 1,nssta
      IF (vers_calmet <= 52) THEN
        WRITE (40,'(5a,3x,i5,2(2x,f9.3),2x,f4.0,2x,f6.1,a)') & 
          "! ",ss_str(k),"='",ss_name(k),"'",ss_id(k), &
          ss_x(k),ss_y(k),ss_ibtz(k),ss_anh(k)," !"
      ELSE
        WRITE (40,'(5a,3x,i6,2(2x,f9.3),2x,f4.0,2x,f6.1,a)') & 
          "! ",ss_str(k),"='",ss_name(k),"'",ss_id(k), &
          ss_x(k),ss_y(k),ss_ibtz(k),ss_anh(k)," !"
      ENDIF
    ENDDO

    WRITE (40,*)
    WRITE (40,'(a)') TRIM(chrec)
    EXIT
  
  ELSE IF (exinp(chrec) /= "") THEN
    CONTINUE
  ELSE
    WRITE (40,'(a)') TRIM(chrec)
  ENDIF

ENDDO

!-------------------------------------------------------------------------------
! 3.8 gruppo 8 (inserisco anagrafica stazioni in quota buone)
!     NB: se le inserissi tutte, avrei problemi quando ne mancano alcune che non
!     sono le ultime
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"END") /= 0) THEN

    DO k = 1,nusta
      WRITE (40,'(5a,3x,i5,2(2x,f9.3),2x,f4.0,a)') & 
        "! ",us_str(k),"='",us_name(k),"'",us_id(k), &
        us_x(k),us_y(k),us_ibtz(k)," !"
    ENDDO

    WRITE (40,*)
    WRITE (40,'(a)') TRIM(chrec)
    EXIT

  ELSE IF (exinp(chrec) /= "") THEN
    CONTINUE
  ELSE
    WRITE (40,'(a)') TRIM(chrec)
  ENDIF

ENDDO

!-------------------------------------------------------------------------------
! 3.9 gruppo 9 (immutato)
DO 
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) GOTO 9994

  IF (INDEX(exinp(chrec),"END") /= 0) THEN
    WRITE (40,'(a)') TRIM(chrec)
    EXIT
  ELSE
    WRITE (40,'(a)') TRIM(chrec)
  ENDIF

ENDDO

CLOSE (30)
CLOSE (40)

!-------------------------------------------------------------------------------
! 3.10 Scrivo su log i parametri modificati (debug)

OPEN (UNIT=70, FILE="crea_calmet_inp_modif.log",STATUS="REPLACE",ACTION="WRITE")

WRITE (70,*)
WRITE (70,*) "date"
WRITE (70,*) ibyr,ibmo,ibdy,ibhr,irlg

WRITE (70,*)
WRITE (70,*) "area"
WRITE (70,*) dgridkm,xorigkm,yorigkm,xlat0,xlon0,nx,ny,iutmzn

WRITE (70,*)
WRITE (70,*) "staz. quo ",nusta_req," buone ",nusta
DO k = 1,nusta
  WRITE (70,*),temp_ok(k),us_str(k),us_name(k),us_id(k),us_x(k),us_y(k), &
    us_ibtz(k),us_file(k)
ENDDO

WRITE (70,*)
WRITE (70,*) "staz. sup ",nssta
DO k = 1,nssta
  WRITE (70,*) ss_str(k),ss_name(k),ss_id(k),ss_x(k),ss_y(k),ss_ibtz(k), &
    ss_anh(k)
ENDDO

STOP

!===============================================================================
! 4) Gestione errori

9999 CONTINUE
WRITE (*,*) "crea_calmet_inp: errore leggendo ",TRIM(filedate)
STOP

9998 CONTINUE
WRITE (*,*) "crea_calmet_inp: errore leggendo ",TRIM(filesurf)
STOP

9997 CONTINUE
WRITE (*,*) "crea_calmet_inp: errore leggendo ",TRIM(filetemp)
STOP

9996 CONTINUE
WRITE (*,*) "crea_calmet_inp: errore leggendo pre_calmet.inp"
STOP

9995 CONTINUE
WRITE (*,*) "crea_calmet_inp: errore leggendo ",TRIM(nfile)
STOP

9994 CONTINUE
WRITE (*,*) "crea_calmet_inp: errore leggendo ",TRIM(filemodello)
STOP

END PROGRAM crea_calmet_inp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION exinp(rec) RESULT (str)
!
! Funzione che, dato un record letto da calmet.inp, ritrorna la prima 
! stringa compresa tra ! che questo contiene.
! Se il record non contine coppie di !! ritorna una stringa vuota
!
IMPLICIT NONE

CHARACTER (LEN=200) :: rec
CHARACTER (LEN=100) :: str

INTEGER :: p1,p2,pp
!
str = ""

p1 = INDEX(rec,"!")
IF (p1 == 0) RETURN

pp = INDEX(rec(p1+1:),"!")
IF (pp == 0) RETURN
p2 = pp + p1

str = rec(p1+1:p2-1)

RETURN

END FUNCTION exinp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION exinp_val(rec) RESULT (str_val)
!
! Funzione che, dato un record letto da calmet.inp, cerca la prima stringa 
! compresa tra ! che questo contiene, e ritrorna la sottostringa che segue il
! primo =
! Se il record non contiene coppie di !!, o se non c'e' l' =, ritorna una 
! stringa vuota
!
IMPLICIT NONE

CHARACTER (LEN=200) :: rec
CHARACTER (LEN=100) :: str,str_val

INTEGER :: p1,p2,pp,p3
!
str = ""
str_val = ""

p1 = INDEX(rec,"!")
IF (p1 == 0) RETURN

pp = INDEX(rec(p1+1:),"!")
IF (pp == 0) RETURN
p2 = pp + p1

str = rec(p1+1:p2-1)

p3 = INDEX(str,"=")
IF (p3 == 0) RETURN
str_val = str(p3+1:)

RETURN

END FUNCTION exinp_val

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE test_temp(file,data1,hh1,data2,hh2,stept_req,ok,ier)
!-------------------------------------------------------------------------------
! Ritorna TRUE se il file specificato (formato up*.dat) contiene dati
! sufficienti per essere utilizzato da calmet. Attualmente bisogna che:
! - siano presenti i TEMP agli istanti iniziale e finale
! - la distanza tra due TEMP buoni sia al max. di 12 ore.
!-------------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

! Paramteri della subroutine
CHARACTER (LEN=9),INTENT(IN) :: file
TYPE (date), INTENT(IN) :: data1,data2
INTEGER, INTENT(IN) :: hh1,hh2,stept_req

LOGICAL, INTENT(OUT) :: ok
INTEGER, INTENT(OUT) :: ier

! Parametri che definiscono i Temp accettabili (user modification)
INTEGER, PARAMETER :: stept_cal = 12  ! intervallo tra 2 temp ammesso da Calmet

! Variabili interne
TYPE (date), ALLOCATABLE :: data_temp(:)
INTEGER, ALLOCATABLE :: hh_temp(:)

INTEGER :: ios(3),iios,ik,icn,nlev,k,cnt
INTEGER :: idstaz,yy,yy4,mm,dd,hh,n_temp_req,delta_hh
!-------------------------------------------------------------------------------

ok = .FALSE.

!-------------------------------------------------------------------------------
! 1) n.ro di Temp richiesti
n_temp_req = (data2-data1) * 24/stept_req + (hh2-hh1)/stept_req + 1
ALLOCATE (data_temp(n_temp_req), hh_temp(n_temp_req))

!-------------------------------------------------------------------------------
! 2) Leggo dal file numero e data dei Temp disponibili

! Header del file
OPEN (UNIT=62, FILE=file, STATUS="OLD", ACTION="READ", IOSTAT=ios(1))
READ (62,*,IOSTAT=ios(2))
READ (62,*,IOSTAT=ios(3))
IF (ANY(ios(1:3) /= 0)) THEN
  ier = 1
  RETURN
ENDIF

cnt = 0
DO 
! Header del Temp
  READ (62,'(3x,i4,5x,i5,5x,4i2,5x,i2,t69,i2)',IOSTAT=iios) &
    ik,idstaz,yy,mm,dd,hh,icn,nlev
  IF (iios /= 0) EXIT
  IF (nlev <= 0) THEN
    CLOSE(62)
    ier = 2
    RETURN
  ENDIF

! Dati del Temp
  DO k = 1, (nlev-1)/4 + 1
    READ (62,*,IOSTAT=iios)
    IF (iios /= 0) THEN
      CLOSE(62)
      ier = 3
      RETURN
    ENDIF
  ENDDO

! Il Temp e' giusto
  cnt = cnt + 1
  IF (yy < 50) THEN
    yy4 = yy + 2000
  ELSE
    yy4 = yy + 1900
  ENDIF
  data_temp(cnt) = date(dd,mm,yy4)
  hh_temp(cnt) = hh

ENDDO
CLOSE (62)

!-------------------------------------------------------------------------------
! 3) Controllo se le date rispondono ai requisiti per Calmet

! Nessun sodaggio buono
IF (cnt == 0) THEN
  ier = 10
  RETURN
ENDIF

! Data iniziale del run non contenuta in dati Temp
IF (data1 < data_temp(1) .OR. & 
    (data1 == data_temp(1) .AND. hh1 < hh_temp(1))) THEN
  ier = 11
  RETURN
ENDIF

! Data finale del run non contenuta in dati Temp
IF (data2 > data_temp(cnt) .OR. & 
    (data2 == data_temp(cnt) .AND. hh2 < hh_temp(cnt))) THEN
  ier = 12
  RETURN
ENDIF

! Il passo tra due temp e' troppo lungo
DO k = 2,cnt
  delta_hh = (data_temp(k) - data_temp(k-1)) * 24 + &
    (hh_temp(k) - hh_temp(k-1))
  IF (delta_hh > stept_cal) THEN
    ier = 13
    RETURN
  ENDIF
ENDDO

!-------------------------------------------------------------------------------
! 4) Se ho passato tutti i controlli, il temp e' giusto

ok = .TRUE.
RETURN

END SUBROUTINE test_temp

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!----------------------------------------------------------------------
      subroutine ll2utm(rlat,rlon,iz0,x,y,iz)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET
!    
! --- CALMET   Version: 5.0       Level: 970825                  LL2UTM
!
! --- PURPOSE:  Converts latitude/longitude to UTM coordinates
!
!           *** Universal Transverse Mercator (UTM) grid system divides
!           *** the world into 60 north-south zones, covering a 6 deg.
!           *** strip of longitude. Zone 1 begins between 180 and 174
!           *** degrees West longitude and progresses eastward to
!           *** zone 60.
!           *** This routine works in both No. & So. Hemispheres
!               Reference --
!                 "Map Projections--A Working Manual", p61,
!                  U.S. Geological Survey Professional Paper 1395,
!                    Note: assumes the Clarke 1866 ellipsoid
!               Adapted from --
!                  EPS version 2.0; subr. MAPGTU
!
! --- INPUTS:
!               RLAT - Real        - N Latitude in decimal degrees
!                                    (use negative for southern hemisphere)
!               RLON - Real        - E Longitude in decimal degrees
!                                    (use negative for western hemisphere)
!                IZ0 - Integer     - UTM zone override (used only if
!                                    IZ0 .ne. zero).
!
! --- OUTPUT:
!                  X - Real        - UTM easting in km
!                  Y - Real        - UTM northing in km
!                 IZ - Integer     - UTM zone
!
! --- LL2UTM called by:  READCF
! --- LL2UTM calls:      none
!----------------------------------------------------------------------

      real k0
      real N,M

      parameter (k0=0.9996)
      parameter (a=6378206.4)
      parameter (e2=0.00676866)
      parameter (ep2=0.0068148)
      parameter (false_e=500000.0)
      parameter (dtr=3.141592654/180.0)

      if (iz0 .eq. 0) then
! ---   Locate natural zone
          iz = int((180.0+rlon)/6.0) + 1
      else
! ---   Zone override
          iz = iz0
      endif

! --- Compute delta longitude in radians
      dl = dtr*(rlon - (6.0*iz-183.0))

! --- Convert phi (latitude) to radians
      p = dtr*rlat

      sinp = sin(p)
      N = a/sqrt(1.0-e2*sinp*sinp)
      tanp = tan(p)
      T = tanp*tanp
      cosp = cos(p)
      C = ep2*cosp*cosp
      A1 = dl*cosp
      M = 111132.0894*rlat - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p) &
        - 0.02*sin(6.0*p)

      A2 = A1**2
      A3 = A2*A1
      A4 = A2**2
      A5 = A4*A1
      A6 = A4*A2
      T2 = T**2

! --- Compute UTM x and y (km)
      x = 0.001*(k0*N*(A1+(1.0-T+C)*A3/6.0          &     
        + (5.0-18.0*T+T2+72.0*C-58.0*ep2)*A5/120.0) &
        + false_e)
      y = (M+N*tanp * (A2/2.0 + (5.0-T+9.0*C+4.0*C*C)*A4/24.0 &
        + (61.0-58.0*T+T2+600.0*C-330.0*ep2)*A6/720.0))
      false_n = 0.
      if (rlat .lt. 0.) then
! --- in km, unlike false_e
        false_n = 10000.
      endif
      y = 0.001*k0*y + false_n

      return
      end

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!----------------------------------------------------------------------
      subroutine utm2ll(x,y,iz,lsohem,rlat,rlon)
!----------------------------------------------------------------------
! VERSIONE CON SINTASSI F90 DELLA ROUTINE DI CALMET
!
! --- CALMET   Version: 5.0       Level: 970825                  UTM2LL
!
! --- PURPOSE:  Converts UTM coordinates to latitude/longitude
!               Works in both Northern & Southern Hemispheres
!               Reference--
!                 "Map Projections--A Working Manual", p61,
!                  U.S. Geological Survey Professional Paper 1395,
!                    Note: assumes the Clarke 1866 ellipsoid
!               Adapted from --
!                  EPS version 2.0; subr. MAPUTG
!
! --- INPUTS:
!                  X - real    - UTM easting in km
!                  Y - real    - UTM northing in km
!                 IZ - integer - UTM zone (6 deg N-S strip, range=1,60)
!             LSOHEM - logical - TRUE = southern hemisphere
!                                FALSE = northern hemisphere
!
! --- OUTPUT:
!               RLAT - real    - N Latitude in decimal degrees
!               RLON - real    - E Longitude in decimal degrees
!
! --- UTM2LL called by:  READCF
! --- UTM2LL calls:      none
!----------------------------------------------------------------------

      real k0,M,N1,l
      logical lsohem

      parameter (k0=0.9996)
      parameter (a=6378206.4)
      parameter (e1=0.001697916)
      parameter (e11=3.0*e1/2.0 - 27.0*e1*e1*e1/32.0)
      parameter (e12=21.0*e1*e1/16.0 - 55.0*e1*e1*e1*e1/32.0)
      parameter (e13=151.0*e1*e1*e1/96.0)
      parameter (e14=1097.0*e1*e1*e1*e1/512.0)
      parameter (e2=0.00676866)
      parameter (e4=e2*e2)
      parameter (e6=e2*e4)
      parameter (ep2=0.0068148)
      parameter (false_e=500000.0)
      parameter (rtd=180.0/3.141592654)

! --- Parameter definitions
!      k0        -  scale on central meridian
!      a         -  Clarke 1866 equatorial radius
!      e2        -  squared Clarke 1866 eccentricity
!      ep2       -  (e2/(1.0-e2)
!      false_e   -  false easting
!      rtd       -  radians to degrees conversion

! --- Central meridian
      rlon0 = iz*6.0 - 183.0

! --- Correct for false easting, southern hemisphere and change to meters
      xm = 1000.0*x - false_e
      if(LSOHEM) then
        ym = 1000.0 * (y-10000.)
      else
        ym = 1000.0 * y
      endif

      M = ym/k0
      u = M/(a*(1.0-e2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0))
      p1 = u + e11*sin(2.0*u) + e12*sin(4.0*u) + e13*sin(6.0*u) + &
               e14*sin(8.0*u)
      cosp1 = cos(p1)
      C1 = ep2*cosp1**2
      C2 = C1**2
      tanp1 = tan(p1)
      T1 = tanp1**2
      T2 = T1**2
      sinp1 = sin(p1)
      sin2p1 = sinp1**2
      N1 = a/sqrt(1.0-e2*sin2p1)
      R0 = 1.0-e2*sin2p1
      R1 = a*(1.0-e2)/sqrt(R0**3)

      D = xm/(N1*k0)
      D2=D**2
      D3=D*D2
      D4=D*D3
      D5=D*D4
      D6=D*D5

      p = p1 - (N1*tanp1/R1) * (D2/2.0                                  &
             - (5.0+3.0*T1+10.0*C1-4.0*C2-9.0*ep2)*D4/24.0              &
             + (61.0+90.0*T1+298.0*C1+45.0*T2-252*ep2-3.0*C2)*D6/720.0)
      rlat = rtd*p
      l = (D - (1.0+2.0*T1+C1)*D3/6.0                                   &
             + (5.0-2.0*C1+28*T1-3.0*C2+8.0*ep2+24.0*T2)*D5/120.0)/cosp1
      rlon = rtd*l + rlon0

      return
      end subroutine utm2ll

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

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


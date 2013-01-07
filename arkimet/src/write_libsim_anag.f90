PROGRAM write_libsim_anag
!--------------------------------------------------------------------------
! Converte un elenco di punti in formato .pts.csv o .ptn in un'anagrafica 
! in formato BUFR generic SIMC. 
! Programma della catena ak_seriet. Versione 1 come ptn2bufr.f90
!
!                                          Versione 3.0, Enrico, 26/07/2011
!--------------------------------------------------------------------------

USE vol7d_class
USE vol7d_dballe_class
USE seriet_utilities
IMPLICIT NONE

TYPE(vol7d) :: v7d
TYPE(vol7d_dballe) :: v7db_exp

DOUBLE PRECISION :: lon(maxpt),lat(maxpt),new_lon,new_lat,plat,plon

INTEGER :: idx_keep(maxpt)
INTEGER :: cnt_par,k,k2,i,pi,pj,pk,kpar,eof,eor,ios,ier,npts,uniq,npts_keep
INTEGER :: cnt_move 
CHARACTER (LEN=500) :: filein,fileout,chpar,chrec
CHARACTER (LEN=80) :: plabel,lab(maxpt)
CHARACTER (LEN=3) :: input_fmt
CHARACTER (LEN=1) :: next_arg
LOGICAL :: lmove

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comandi

! Default
uniq = 3
filein = ""
fileout = ""
input_fmt = ""

cnt_par = 0
next_arg = ""
DO kpar = 1,HUGE(kpar)
  CALL getarg(kpar,chpar)

  IF (TRIM(chpar) == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP 1
  ELSE IF (TRIM(chpar) == "-csv") THEN
    input_fmt = "csv"
  ELSE IF (TRIM(chpar) == "-ptn") THEN
    input_fmt = "ptn"
  ELSE IF (TRIM(chpar) == "-uniq") THEN
    next_arg = "u"
  ELSE IF (next_arg == "u") THEN
    IF (TRIM(chpar) == "all") THEN
      uniq = 0
    ELSE IF (TRIM(chpar) == "stop") THEN
      uniq = 1
    ELSE IF (TRIM(chpar) == "skip") THEN
      uniq = 2
    ELSE IF (TRIM(chpar) == "move") THEN
      uniq = 3
    ELSE 
      CALL scrive_help
      STOP 1
    ENDIF
    next_arg = ""
  ELSE
    cnt_par = cnt_par + 1
    IF (cnt_par == 1) THEN
      filein = chpar
    ELSE IF (cnt_par == 2) THEN
      fileout = chpar
    ENDIF

  ENDIF
ENDDO

IF (filein == "" .OR. fileout == "" .OR. input_fmt == "") THEN
  CALL scrive_help
  STOP 1
ENDIF  

! 1.2 trovo codice EOF
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 2) Leggo da filein elenco e coordinate dei punti

OPEN (UNIT=30, FILE=filein, STATUS="OLD", ACTION="READ", ERR=9999)

npts = 0
DO k = 1,HUGE(0)
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998

  IF (k == 1 .AND. input_fmt == "csv") THEN
    CALL test_header("lspts",chrec,ier)
    IF (ier > 0) GOTO 9997
    CYCLE

  ELSE IF (input_fmt == "csv") THEN
    CALL pts_csv_parser(chrec,pi,pj,pk,plat,plon,plabel,ier)

  ELSE IF (input_fmt == "ptn") THEN
    CALL ptn_parser(chrec,pi,pj,pk,plat,plon,plabel,ier)

  ENDIF

  IF (ier /= 0) THEN
    WRITE (*,*) "Errore ",ier," parsing ",TRIM(filein)," skip punto ",k
    CYCLE
  ELSE
    npts = npts + 1
    IF (npts > maxpt) GOTO 9994
    lat(npts) = plat
    lon(npts) = plon
    lab(npts) = plabel
  ENDIF
ENDDO

CLOSE (30)

!--------------------------------------------------------------------------
! 3) Se richiesto, controllo che non ci siano due punti con le stesse
!    coordinate. Esamino successivmaente i vari punti (ciclo "main"),
!    confrontandoli con quelli gia' elaborati.

IF (uniq > 0) THEN
  IF (uniq == 2) THEN
    npts_keep = 0
    idx_keep(:) = 0
  ELSE IF (uniq == 3) THEN
    cnt_move = 0
  ENDIF

  main: DO k = 1,npts
    IF (uniq == 3) lmove = .FALSE.

!   Cerco se tra quelli gia' eleborati ci sono punti con le stesse coord.
    DO k2 = 1,k-1
      IF (ABS(lon(k) - lon(k2)) < coo_eps .AND. &
          ABS(lat(k) - lat(k2)) < coo_eps) THEN
        IF (uniq == 1) THEN
          GOTO 9996
        ELSE IF (uniq == 2) THEN
          IF (ANY(idx_keep(1:npts_keep) == k2)) CYCLE main
        ELSE IF (uniq == 3) THEN
          lmove = .TRUE.
          EXIT
        ENDIF
      ENDIF
    ENDDO

!   Se necessario, salto/sposto il punto che sto esaminando
    IF (uniq == 2) THEN
      npts_keep = npts_keep + 1
      idx_keep(k) = npts_keep

    ELSE IF (uniq == 3 .AND. lmove) THEN
      DO i = 1,npts
        new_lon = lon(k) + DBLE(i)*2*coo_eps_shift
        new_lat = lat(k) + DBLE(i)*2*coo_eps_shift
        IF (ALL(ABS(lon(1:k-1) - new_lon) > coo_eps_shift .AND. &
                ABS(lat(1:k-1) - new_lat) > coo_eps_shift)) EXIT
      ENDDO
      IF (i > npts) GOTO 9995
      WRITE (*,'(2(a,1x,i4,1x,a,1x),2x,2(a,f10.5),a)') &
        "Sposto punto",k,TRIM(lab(k)),"coincidente con",k2,TRIM(lab(k2)), &
        " (",lon(k)," ",lat(k),")"
!     WRITE (*,'(2(a,1x,i4,1x,a,1x),2x,4(a,f17.12),a)') &
!       "Sposto punto",k,TRIM(lab(k)),"coincidente con",k2,TRIM(lab(k2)), &
!       " (da ",lon(k)," ",lat(k)," a ",new_lon," ",new_lat,")"
      lon(k) = new_lon
      lat(k) = new_lat
      cnt_move = cnt_move + 1

    ENDIF
  ENDDO main

  IF (uniq == 2) THEN
    DO k = 1,npts_keep
      lat(k) = lat(idx_keep(k))
      lon(k) = lon(idx_keep(k))
    ENDDO
    IF (npts /= npts_keep) WRITE (*,*) &
      "Saltati ",npts - npts_keep," punti con coordinate coincidenti"
    npts = npts_keep

  ELSE IF (uniq == 3 .AND. cnt_move > 0) THEN
    WRITE (*,*) "Spostati ",cnt_move," punti con coordinate coincidenti"

  ENDIF
ENDIF

!--------------------------------------------------------------------------
! 4) Scrivo i punti trovati in formato BUFR-genericSim

! Alloco un oggetto vol7d con tutte le dimensioni a 0 tranne ana
CALL init (v7d)
CALL vol7d_alloc(v7d, nana=npts, nnetwork=1)
CALL vol7d_alloc_vol(v7d)

! Assegno le coordinate dei punti e il nome della rete
DO k = 1, npts
  CALL init(v7d%ana(k), lon=lon(k), lat=lat(k))
ENDDO
v7d%network(1) = vol7d_network("arkimet")
CALL export(v7d, filename=fileout)

! Costruisco un oggeto vol7d_dballe per esportare in foramto BUFR
!CALL init(v7db_exp, file=.TRUE., filename=fileout, &
!  format="BUFR", write=.TRUE., wipe=.TRUE.)
!v7db_exp%vol7d = v7d
!CALL export(v7db_exp)

CALL delete(v7d)

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore leggendo header ",TRIM(filein)
STOP 2

9996 CONTINUE
WRITE (*,*) "Trovati punti coincidienti, mi fermo"
STOP 3

9995 CONTINUE
WRITE (*,*) "Errore nello spostamento dei punti coincidenti"
STOP 3

9994 CONTINUE
WRITE (*,'(a)') "Richiesti troppi punti, aumentare maxpt in seriet_utilities.f90 (",maxpt,")"
STOP 3

END PROGRAM write_libsim_anag

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: write_bufr_anag.exe  -csv/-ptn filein fileout [-uniq OPT]"
WRITE (*,*) "Legge un elenco di punti in formato .ptn o .pts.csv e lo riscrive Vol7d-native"
WRITE (*,*) "filein: elenco dei punti richiesti"
WRITE (*,*) "fileout: output in formato Bufr"
WRITE (*,*) "-csv/-ptn: specifica il formato del file di input"
WRITE (*,*) "-uniq: determina la gestione dei punti con coordinate identiche:"
WRITE (*,*) "       OPT puo' valere: "
WRITE (*,*) "       - all: scrive tutti i punti richiesti"
WRITE (*,*) "       - stop: non scrive fileout"
WRITE (*,*) "       - skip: scrive i punti doppi una sola volta"
WRITE (*,*) "       - move (default): sposta di qualche metro i punti doppi"
WRITE (*,*) 

RETURN

END SUBROUTINE scrive_help

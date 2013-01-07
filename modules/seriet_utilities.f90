MODULE seriet_utilities
!--------------------------------------------------------------------------
! Modulo che contiene dati costanti e funzioni relative alla codifica di
! dati in formato seriet
!
! Tracciato del formato .pts.csv:
! - header fisso, poi una riga per ogni punto
! - ogni riga contiene 3 campi, in formato csv standard:
!   1: longitudine (5 decimali)
!   2: latitudine (5 decimali)
!   3: label (eventualmente vuota)
! - possono essere presenti altri 3 campi con indici del punto (I,J,K; 0 = 
!   non indicato, <0 = fuori area), che non sono usati dalla catena seriet.
!
! Tracciato del formato .ptn (obsoleto):
! - una riga per ogni punto, no header
! - ogni riga contiene 3 campi, con separatore "!"
!   1) indici del punto (I,J) oppure (K,0)
!   2) coordinate del punto griglia piu'vicino al punto richiesto (lat,lon)
!   3) label (character, senza spazi e senza "!")
!
! Gestione dell'estrazione di due punti coincidenti.
! Se vengono richiesti due punti esattamente coincidenti, vg6d_getpoint
! ritorna un solo valore. Per riuscire ugalmente ad avere un dato per ogni
! punto richiesto, passo a vg6d_getpoint delle coordinate spostate di una 
! quantita' coo_eps_shift, ma la differenza e' abbastanza piccola da essere
! ignorata da gacsv2seriet (l'anagrafica v7d contiene le coord. spostate,
! il file .pts.csv quelle originali). C'e' il rischio che se un punto e' a
! cavallo di due celle, vengano estratti valori diversi.
! Una soluzione migliore sarebbe modificare vg6d_getpoint in modo da 
! elaborare ogni punto separatamente, e poi appendere i risultati.
!
!                                         Versione 1.1.2, Enrico 23/01/2012
!--------------------------------------------------------------------------

USE datetime_class
USE file_utilities

IMPLICIT NONE

! Dato mancante in output
REAL, PARAMETER :: rmis_ser = -9999.
REAL, PARAMETER :: rmis_sex = -1.E30
INTEGER, PARAMETER :: imis_ser = -9999

! Tolleranza per l'uguaglianza di due coordiante geografiche: 
! - ndec e' il numero di decimali usato nei files pts.csv;
! - coo_eps la minima differenza di coordinate per considerare distinti
!   due punti
! - coo_eps_shift e' la 
INTEGER, PARAMETER :: coo_ndec = 5
DOUBLE PRECISION, PARAMETER :: coo_eps = 0.000015
DOUBLE PRECISION, PARAMETER :: coo_eps_shift = 1.e-10

! Dimensionamenti massimi relativi a un'estrazione Arkimet
INTEGER, PARAMETER :: maxvl = 1000    ! coppie var-liv
INTEGER, PARAMETER :: maxtr = 500     ! timerange
INTEGER, PARAMETER :: maxvltr = 50000 ! terne var-liv-trange
INTEGER, PARAMETER :: maxpt = 1000    ! punti richiesti
INTEGER, PARAMETER :: maxqry = 20     ! sotto-query rettangolari

! Dimensionamenti massimi relativi agli archivi COSMO
INTEGER, PARAMETER :: maxaklay = 40  ! model layers
INTEGER, PARAMETER :: maxaklev = 41  ! model levels

! Tipo derivato "data e scadenza"
TYPE datascad
  TYPE(datetime) :: reftime
  INTEGER :: p1
END TYPE datascad

! Tipo derivato "report gacsv per seriet"
! datascad si compone di reftime e p1
! varliv contiene: cem,tab,var,level1,l1,l2
TYPE gacsv_report
  TYPE(datascad) :: datascad
  INTEGER :: p2,timerange,varliv(6),np,edition
  DOUBLE PRECISION :: lon,lat
  REAL :: value
ENDTYPE gacsv_report

INTERFACE OPERATOR (==)
  MODULE PROCEDURE datascad_eq_datascad
END INTERFACE
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE datascad_ne_datascad
END INTERFACE
INTERFACE OPERATOR (>)
  MODULE PROCEDURE datascad_gt_datascad
END INTERFACE
INTERFACE OPERATOR (>=)
  MODULE PROCEDURE datascad_ge_datascad
END INTERFACE
INTERFACE OPERATOR (<)
  MODULE PROCEDURE datascad_lt_datascad
END INTERFACE
INTERFACE OPERATOR (<=)
  MODULE PROCEDURE datascad_le_datascad
END INTERFACE

! Lista dei varliv che si riferiscono ai parametri fisiografici
! NB: i valori di varliv_*(4:5,:) possono essere 0 o mancanti

! Albedo
INTEGER, PARAMETER :: nvl_alb = 2
INTEGER, PARAMETER :: varliv_alb(6,nvl_alb) = RESHAPE((/ &
 -999,  2, 84,  1,  0,  0, &
 -999,200,124,  1,  0,  0 /), &
  (/6,nvl_alb/) )

! Roughness
INTEGER, PARAMETER :: nvl_z0 = 2
INTEGER, PARAMETER :: varliv_z0(6,nvl_z0) = RESHAPE((/ &
 -999,  2, 83,  1,  0,  0, &
 -999,200,123,  1,  0,  0 /), &
  (/6,nvl_alb/) )

! Orografia
INTEGER, PARAMETER :: nvl_orog = 3
INTEGER, PARAMETER :: varliv_orog(6,nvl_orog) = RESHAPE((/ &
 -999,  2,  8,  1,  0,  0, &
 -999,200,122,  1,  0,  0, &
 -999,200,  8,  1,  0,  0 /), &
  (/6,nvl_orog/) )

! Tracciato dei files grib_api_csv. In caso di modifica, aggiornare:
! tipo gacsv_report, subroutines gacsv_rep_setmiss e gacsv_rep_read
INTEGER, PARAMETER ::  nf_gacsv = 15
!  1234567890123456789012345
CHARACTER (LEN=25), PARAMETER, PRIVATE :: chf_gacsv(nf_gacsv) = (/ &
  "simpledate               ", &
  "p1h                      ", &
  "p2h                      ", &
  "timerange                ", &
  "level1                   ", &
  "l1                       ", &
  "l2                       ", &
  "centre                   ", &
  "category                 ", &
  "number                   ", &
  "npoint                   ", &
  "lon                      ", &
  "lat                      ", &
  "value                    ", &
  "editionNumber            "/)

! Tracciato dei files *.pts.csv
INTEGER, PARAMETER ::  nf_lspts = 6
CHARACTER (LEN=25), PARAMETER, PRIVATE :: chf_lspts(nf_lspts) = (/ &
!  1234567890123456789012345
  "longitude                ", &
  "latitude                 ", &
  "label                    ", &
  "i_index                  ", &        
  "j_index                  ", &        
  "k_index                  "/)

! Tracciato dei files *.varliv.csv
INTEGER, PARAMETER ::  nf_lscol = 12
CHARACTER (LEN=25), PARAMETER, PRIVATE :: chf_lscol(nf_lscol) = (/ &
!  1234567890123456789012345
  "centre                   ", &
  "category                 ", &
  "number                   ", &
  "cv2                      ", &
  "level1                   ", &
  "l1                       ", &
  "l2                       ", &
  "ndec                     ", &
  "vmin                     ", &
  "vmax                     ", &
  "model                    ", &
  "varname                  " /)

! Tracciato dei files *.datasca.csv
INTEGER, PARAMETER ::  nf_lsrow = 6
CHARACTER (LEN=25), PARAMETER, PRIVATE :: chf_lsrow(nf_lsrow) = (/ &
!  1234567890123456789012345
  "reftime_min              ", &
  "reftime_max              ", &
  "reftime_step             ", &
  "P1_min                   ", &
  "P1_max                   ", &
  "P1_step                  " /)

CONTAINS

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE build_header(type,string,ier)
!--------------------------------------------------------------------------
! Costruisce una stringa con l'header di un file csv del tipo richiesto
! type puo' essere uno tra: "gacsv", "lsrow", "lscol", "lspts"
!--------------------------------------------------------------------------

USE file_utilities
IMPLICIT NONE

CHARACTER (LEN=5), INTENT(IN) :: type
CHARACTER (LEN=*), INTENT(OUT) :: string
INTEGER, INTENT(OUT),OPTIONAL :: ier
!
TYPE (csv_record) :: csvline
INTEGER :: kf

!--------------------------------------------------------------------------

CALL init(csvline)

SELECT CASE (TRIM(ADJUSTL(type)))
CASE("gacsv")
  DO kf = 1, nf_gacsv
    CALL csv_record_addfield(csvline,TRIM(chf_gacsv(kf)))
  ENDDO
CASE("lspts")
  DO kf = 1, nf_lspts
    CALL csv_record_addfield(csvline,TRIM(chf_lspts(kf)))
  ENDDO
CASE("lsrow")
  DO kf = 1, nf_lsrow
    CALL csv_record_addfield(csvline,TRIM(chf_lsrow(kf)))
  ENDDO
CASE("lscol")
  DO kf = 1, nf_lscol
    CALL csv_record_addfield(csvline,TRIM(chf_lscol(kf)))
  ENDDO
CASE DEFAULT
  IF (PRESENT(ier)) ier = 1
  WRITE (*,*) "Sub. build_header: tipo di record non gestito ",type
  RETURN
END SELECT

string = csv_record_getrecord(csvline)
IF (PRESENT(ier)) ier = 0

CALL delete(csvline)
RETURN

END SUBROUTINE build_header

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE test_header(type,string,ier)
!--------------------------------------------------------------------------
! Controlla se string e' l'header di un file csv del tipo richiesto
! type puo' essere uno tra: "gacsv", "lsrow", "lscol", "lspts"
! ier = 0:  string corrisponde all'header richiesto
! ier = -1: string corrisponde all'header richiesto ma contiene dei campi
!           aggiuntivi
! ier = -2: string corrisponde all'header di un file .pts.csv senza gli 
!           indici
! ier = 1:  tipo di record non gestito
! ier = 2:  string contiene meno campi di quanto richiesto, ma i primi sono
!           giusti 
! ier = 3:  alcuni campi non corrispondono
!--------------------------------------------------------------------------

USE file_utilities
IMPLICIT NONE

CHARACTER (LEN=5), INTENT(IN) :: type
CHARACTER (LEN=*), INTENT(IN) :: string
INTEGER, INTENT(OUT) :: ier
!
TYPE (csv_record) :: csvline
INTEGER :: nf_in,nf_req,kf
CHARACTER (LEN=25) :: ch25
LOGICAL :: lok

!--------------------------------------------------------------------------

CALL init(csvline,RECORD=string,NFIELD=nf_in)
lok = .TRUE.

SELECT CASE (TRIM(ADJUSTL(type)))

CASE("gacsv")
  nf_req = nf_gacsv
  DO kf = 1, MIN(nf_req,nf_in)
    CALL csv_record_getfield(csvline,FIELD=ch25)
    IF (TRIM(ADJUSTL(ch25)) /= TRIM(ADJUSTL(chf_gacsv(kf)))) THEN
      lok = .FALSE.
      EXIT
    ENDIF
  ENDDO

CASE("lspts")
  nf_req = nf_lspts
  DO kf = 1, MIN(nf_req,nf_in)
    CALL csv_record_getfield(csvline,FIELD=ch25)
    IF (TRIM(ADJUSTL(ch25)) /= TRIM(ADJUSTL(chf_lspts(kf)))) THEN
      lok = .FALSE.
      EXIT
    ENDIF
  ENDDO

CASE("lsrow")
  nf_req = nf_lsrow
  DO kf = 1, MIN(nf_req,nf_in)
    CALL csv_record_getfield(csvline,FIELD=ch25)
    IF (TRIM(ADJUSTL(ch25)) /= TRIM(ADJUSTL(chf_lsrow(kf)))) THEN
      lok = .FALSE.
      EXIT
    ENDIF
  ENDDO

CASE("lscol")
  nf_req = nf_lscol
  DO kf = 1, MIN(nf_req,nf_in)
    CALL csv_record_getfield(csvline,FIELD=ch25)
    IF (TRIM(ADJUSTL(ch25)) /= TRIM(ADJUSTL(chf_lscol(kf)))) THEN
      lok = .FALSE.
      EXIT
    ENDIF
  ENDDO

CASE DEFAULT
  ier = 1
  WRITE (*,*) "Sub. test_header: tipo di record non gestito ",type
  CALL delete(csvline)
  RETURN
END SELECT

IF (.NOT. lok) THEN
  ier = 3
ELSE IF (nf_in < nf_req) THEN
  IF (TRIM(ADJUSTL(type)) == "lspts" .AND. nf_in == 3) THEN
    ier = -2
  ELSE
    ier = 2
  ENDIF
ELSE IF (nf_in > nf_req) THEN
  ier = 1
ELSE
  ier = 0
ENDIF

CALL delete(csvline)
RETURN
END SUBROUTINE test_header

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE gacsv_rep_read(chrecord,report,iret)
!
! Legge un record gacsv e salva i dati in una varibile di tipo gacsv_report
!
USE file_utilities
USE datetime_class
IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN) :: chrecord
TYPE(gacsv_report), INTENT(OUT) :: report
INTEGER, INTENT(OUT) :: iret

TYPE(csv_record) :: csvline
TYPE(datetime) :: reftime
INTEGER :: nf,yy,mm,dd,hh,minute,p1,ier(nf_gacsv),ier2
CHARACTER(LEN=12) :: ch12
!
CALL init(csvline, RECORD=chrecord)

CALL csv_record_getfield(csvline,FIELD=ch12,             IER=ier(1))
CALL csv_record_getfield(csvline,FIELD=p1,               IER=ier(2))
CALL csv_record_getfield(csvline,FIELD=report%p2,        IER=ier(3))
CALL csv_record_getfield(csvline,FIELD=report%timerange, IER=ier(4))
CALL csv_record_getfield(csvline,FIELD=report%varliv(4), IER=ier(7))
CALL csv_record_getfield(csvline,FIELD=report%varliv(5), IER=ier(5))
CALL csv_record_getfield(csvline,FIELD=report%varliv(6), IER=ier(6))
CALL csv_record_getfield(csvline,FIELD=report%varliv(1), IER=ier(8))
CALL csv_record_getfield(csvline,FIELD=report%varliv(2), IER=ier(9))
CALL csv_record_getfield(csvline,FIELD=report%varliv(3), IER=ier(10))
CALL csv_record_getfield(csvline,FIELD=report%np,        IER=ier(11))
CALL csv_record_getfield(csvline,FIELD=report%lon,       IER=ier(12))
CALL csv_record_getfield(csvline,FIELD=report%lat,       IER=ier(13))
CALL csv_record_getfield(csvline,FIELD=report%value,     IER=ier(14))
CALL csv_record_getfield(csvline,FIELD=report%edition,   IER=ier(15))

CALL delete(csvline)

READ(ch12,'(i4,4i2)',IOSTAT=ier2) yy,mm,dd,hh,minute
reftime = datetime_new(YEAR=yy,MONTH=mm,DAY=dd,HOUR=hh,MINUTE=minute)
report%datascad = datascad_new(reftime,p1)

IF (ANY(ier(:) /= 0) .OR. ier2 /= 0) THEN
  CALL gacsv_rep_setmiss(report)
  iret = 1
  RETURN
ENDIF

iret = 0
RETURN

END SUBROUTINE gacsv_rep_read

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE gacsv_rep_setmiss(report,reft)
!
! Assegna valori di default a una variabile di tipo "gacsv_report".
! Se reft = "max", reftime viene messo a "data massima", altrimenti a 
! "data minima". Le altre componenti sono messe al "missing value" LibSim
!
USE datetime_class
USE missing_values

IMPLICIT NONE
TYPE(gacsv_report), INTENT(INOUT) :: report
CHARACTER(LEN=3), INTENT(IN), OPTIONAL :: reft
!
TYPE(datetime) :: dtime
!

IF (PRESENT(reft)) THEN
  IF (reft == "min") THEN
    report%datascad = datascad_new(datetime_min,0)
  ELSE IF (reft == "max") THEN
    report%datascad = datascad_new(datetime_max,0)
  ENDIF
ELSE 
  report%datascad = datascad_new(datetime_min,0)
ENDIF

report%p2 = imiss
report%timerange = imiss
report%varliv(1:3) = imiss
report%varliv(4:6) = imiss
report%np = imiss
report%lat = rmiss
report%lon = rmiss
report%value = rmiss
report%edition = imiss

RETURN
END SUBROUTINE gacsv_rep_setmiss

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE read_fisiog_gacsv(filefis,npt,req_lat,req_lon,libsim, &
  alb_fis,z0_fis,zlev,zlay,orog)
!--------------------------------------------------------------------------
! Legge da filefis i parametri non dipendenti dal tempo relativi ai punti 
! richiesti (alb, z0, zlay, zlevs, orog).
! Filefis puo' non essere presente, oppure contenere solo alcuni parametri.
! Il tracciato e' lo stesso dei dati (gacsv) + header; i campi reletivi a 
! reftime e timerange vengono ignorati.
! Viene construito passando vg6d_getopint sui grib statici relativi al 
! dataset da cui sto estraendo.
!
! Note:
! - il parametro "libsim" serve a riconoscere la codifica dei model layers
!
!--------------------------------------------------------------------------
USE datetime_class
USE missing_values
USE file_utilities
IMPLICIT NONE

! Argomenti della subroutine
CHARACTER(LEN=80), INTENT(IN) :: filefis
INTEGER, INTENT(IN) :: npt
DOUBLE PRECISION, INTENT(IN) :: req_lat(npt),req_lon(npt)
LOGICAL, INTENT(IN) :: libsim 
REAL, INTENT(OUT) :: alb_fis(npt),z0_fis(npt),orog(npt)
REAL, INTENT(OUT) :: zlev(npt,maxaklev),zlay(npt,maxaklay)

! Variabili locali
TYPE(gacsv_report) :: record
INTEGER :: ios,iret,pts_map(npt),kpt,k,cnt,iu
CHARACTER (LEN=250) :: chrec

!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
! 1) Inizializzazioni

alb_fis(:) = rmiss
z0_fis(:) = rmiss
orog(:) = rmiss
zlev(:,:) = rmiss
zlay(:,:) = rmiss

IF (TRIM(filefis) == "") RETURN

iu = getunit()
IF (iu == -1) GOTO 9995
OPEN (UNIT=iu, FILE=filefis, STATUS="OLD", ACTION="READ", IOSTAT=ios)
IF (ios /= 0) GOTO 9999
READ (iu,'(a)',IOSTAT=ios) chrec
IF (ios /= 0) GOTO 9998
CALL test_header("gacsv",chrec,iret)
IF (iret > 0) GOTO 9997

!--------------------------------------------------------------------------
! 2) Leggo i dati e li associo ai punti richiesti
! NB: pts_map contiene, per ciascuno di punti presenti in filefis, il
!     numero del corrispondente punto richiesto (ie. n.ro riga in filepts)
pts_map(:) = 0

input: DO cnt = 1,HUGE(0)
! 2.1 Leggo il prossimo record
  READ (iu,'(a)',IOSTAT=ios) chrec
  IF (ios /= 0) EXIT
  CALL gacsv_rep_read(chrec,record,iret)
  IF (iret /= 0) GOTO 9996

! 2.2 Se ho letto i dati di un punto che trovo per la prima volta, cerco se
!     e' tra quelli richiesti. 
  IF (pts_map(record%np) == 0) THEN
    DO kpt = 1,npt    
!     Non posso associare p.ti diversi di filefis allo stesso p.to richiesto
      IF (ANY(pts_map(:) == kpt)) CYCLE
      IF (ABS(record%lon - req_lon(kpt)) < coo_eps .AND. &
          ABS(record%lat - req_lat(kpt)) < coo_eps) THEN
        pts_map(record%np) = kpt
        EXIT
      ENDIF
    ENDDO
    IF (kpt > npt) pts_map(record%np) = -1
  ENDIF

! 2.3 Se ho letto i dati di un punto richiesto, cerco a quale parametro 
!     corrisponde e ne salvo il valore
  IF (pts_map(record%np) < 0) CYCLE

! Orografia
  DO k = 1, nvl_orog
    IF (ALL(record%varliv(2:4) == varliv_orog(2:4,k))) THEN
      orog(pts_map(record%np)) = record%value   
      CYCLE input
    ENDIF
  ENDDO

! Albedo
  DO k = 1, nvl_alb
    IF (ALL(record%varliv(2:4) == varliv_alb(2:4,k))) THEN
      alb_fis(pts_map(record%np)) = record%value   
      CYCLE input
    ENDIF
  ENDDO

! Roughness
  DO k = 1, nvl_z0
    IF (ALL(record%varliv(2:4) == varliv_z0(2:4,k))) THEN
      z0_fis(pts_map(record%np)) = record%value   
      CYCLE input
    ENDIF
  ENDDO

! Model layers e model levels COSMO (la quota dei livelli Chimere e' 
! variabile, e non puo' essere scritta nell'intestazione dei files seriet)
  IF (libsim .AND. ALL(record%varliv(2:4) == (/2,8,105/))) THEN
    IF (c_e(record%varliv(6))) THEN
      zlay(pts_map(record%np),record%varliv(5)) = record%value
    ELSE
      zlev(pts_map(record%np),record%varliv(5)) = record%value
    ENDIF
  ELSE IF (.NOT. libsim) THEN
    IF (ALL(record%varliv(2:4) == (/2,8,110/))) THEN
      zlay(pts_map(record%np),record%varliv(5)) = record%value
    ELSE IF (ALL(record%varliv(2:4) == (/2,8,109/))) THEN
      zlev(pts_map(record%np),record%varliv(5)) = record%value
    ENDIF
  ENDIF

ENDDO input
CLOSE(iu)

RETURN

9999 CONTINUE
WRITE (*,*) "File fisiografico non trovato ",TRIM(filefis)
RETURN

9998 CONTINUE
WRITE (*,*) "Errore leggendo header file fisiografico ",TRIM(filefis)
RETURN

9997 CONTINUE
WRITE (*,*) "Errore ",iret," parsing header file fisiografico ", &
  TRIM(filefis)
RETURN

9996 CONTINUE
WRITE (*,'(3a,i5)') "Errore leggendo file ",TRIM(filefis)," record ",cnt
WRITE (*,'(a)') TRIM(chrec)
RETURN

9995 CONTINUE
WRITE (*,*) "Errore getunit..."
RETURN

END SUBROUTINE read_fisiog_gacsv

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

FUNCTION datascad_new(reftime,p1) RESULT (this)
!
! Costruttore del tipo derivato datascad
!
IMPLICIT NONE

TYPE(datetime), INTENT(IN) :: reftime
INTEGER, INTENT(IN) :: p1
TYPE(datascad) :: this
!
this%reftime = reftime
this%p1 = p1

RETURN
END FUNCTION datascad_new

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!==========================================================================
FUNCTION datascad_eq_datascad (datascad1,datascad2)  RESULT (ll)
!
USE datetime_class
IMPLICIT NONE
!
TYPE (datascad), INTENT(IN) :: datascad1, datascad2
LOGICAL :: ll

ll = (datascad1%reftime == datascad2%reftime) .AND. &
     (datascad1%p1 == datascad2%p1)

RETURN
END FUNCTION datascad_eq_datascad

!==========================================================================
FUNCTION datascad_ne_datascad (datascad1,datascad2)  RESULT (ll)
!
USE datetime_class
IMPLICIT NONE
!
TYPE (datascad), INTENT(IN) :: datascad1, datascad2
LOGICAL :: ll

ll = (datascad1%reftime /= datascad2%reftime) .OR. &
     (datascad1%p1 /= datascad2%p1)

RETURN
END FUNCTION datascad_ne_datascad

!==========================================================================
FUNCTION datascad_gt_datascad (datascad1,datascad2)  RESULT (ll)
!
USE datetime_class
IMPLICIT NONE
!
TYPE (datascad), INTENT(IN) :: datascad1, datascad2
LOGICAL :: ll

ll = (datascad1%reftime > datascad2%reftime) .OR. &
     ((datascad1%reftime == datascad2%reftime) .AND. &
      (datascad1%p1 > datascad2%p1))
RETURN
END FUNCTION datascad_gt_datascad

!==========================================================================
FUNCTION datascad_ge_datascad (datascad1,datascad2)  RESULT (ll)
!
USE datetime_class
IMPLICIT NONE
!
TYPE (datascad), INTENT(IN) :: datascad1, datascad2
LOGICAL :: ll

ll = (datascad1%reftime > datascad2%reftime) .OR. &
     ((datascad1%reftime == datascad2%reftime) .AND. &
      (datascad1%p1 >= datascad2%p1))
RETURN
END FUNCTION datascad_ge_datascad

!==========================================================================
FUNCTION datascad_lt_datascad (datascad1,datascad2)  RESULT (ll)
!
USE datetime_class
IMPLICIT NONE
!
TYPE (datascad), INTENT(IN) :: datascad1, datascad2
LOGICAL :: ll

ll = (datascad1%reftime < datascad2%reftime) .OR. &
     ((datascad1%reftime == datascad2%reftime) .AND. &
      (datascad1%p1 < datascad2%p1))
RETURN
END FUNCTION datascad_lt_datascad

!==========================================================================
FUNCTION datascad_le_datascad (datascad1,datascad2)  RESULT (ll)
!
USE datetime_class
IMPLICIT NONE
!
TYPE (datascad), INTENT(IN) :: datascad1, datascad2
LOGICAL :: ll

ll = (datascad1%reftime < datascad2%reftime) .OR. &
     ((datascad1%reftime == datascad2%reftime) .AND. &
      (datascad1%p1 <= datascad2%p1))
RETURN
END FUNCTION datascad_le_datascad

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE var2spec(var,ndec,vmin,vmax,str,cp2,ier,tab_path)
!--------------------------------------------------------------------------
! Data la codifica GRIB1 di una variabile (cem,tab,var), cerca nel file
! tabella_xxx_ser.txt corrispondente una serie di informazioni:
! - n.ro di decimali piu' appropriato per rappresentarla (-1 = notazione
!   esponenziale)
! - valori minimo e massimo accettabili
! - stringa identificativa della variabile
! - codice del tipo di varibile (comp2), che vale:
!     se e' comp.U del vento = codice dellacomp.V
!     se e' comp.V del vento = 500 + codice della comp.U
!     se e' temperatura      = -1
!     se e' Monin-Obukov     = -2
!     se e' SW Budget        = -3
!     se e' cloud cover      = -4
!     se e' flusso di calore = -5
!     negli altri casi       =  0
!
! Note:
! - cerca tabella_xxx_ser.txt in tab_path (se presente), o in 
!   $HOME_MINGUZZI/util/grib/dat; se non la trova, usa valori di defualt.
! - Codici d'errore (se /= 0 la subroutine ritorna dei valori di dafault):
!   0 = tutto ok
!   -1 = parametro non presente in tabella
!   -2 = file tabella_xxx_ser.txt non trovato
!   -3 = errore lettura dal file tabella_xxx_ser.txt 
!
!                                           Versione 2.0, Enrico 19/04/2011
!--------------------------------------------------------------------------
USE file_utilities
IMPLICIT NONE

! Argomenti della subroutine
INTEGER, INTENT(IN) :: var(3)
REAL, INTENT(OUT) :: vmin,vmax
INTEGER, INTENT(OUT) :: ndec,cp2,ier
CHARACTER (*), INTENT(OUT) :: str
CHARACTER (LEN=*), INTENT(IN), OPTIONAL :: tab_path

! tab_path di default
CHARACTER (LEN=40), PARAMETER :: tab_env_def = "HOME_MINGUZZI"
CHARACTER (LEN=40) :: tab_path_def = "util/grib/dat"

! Variabili locali
REAL :: vmin_t,vmax_t
INTEGER :: ndec_t,cp2_t,var_t
INTEGER :: ios,ios2,iu
CHARACTER (LEN=256) :: tab_file
CHARACTER (LEN=80) :: ch80
CHARACTER (LEN=8) :: ch8
CHARACTER (LEN=3) :: ch3

!--------------------------------------------------------------------------

! Assegno valori di default
ndec = -1
vmin = -HUGE(0.)
vmax = HUGE(0.)
WRITE (str,'(i3.3,a1,i3.3)') var(2),"_",var(3)
cp2 = 0

! Apro la tabella richiesta
IF (PRESENT(tab_path)) THEN
  WRITE (tab_file,'(3a,i3.3,a)') &
    TRIM(tab_path),"/","tabella_",var(2),"_ser.txt"
ELSE  
  CALL GETENV(tab_env_def,ch80)
  WRITE (tab_file,'(5a,i3.3,a)') &
    TRIM(ch80),"/",TRIM(tab_path_def),"/","tabella_",var(2),"_ser.txt"
ENDIF

iu = getunit()
OPEN (UNIT=iu, FILE=tab_file, STATUS="OLD", ACTION="READ", IOSTAT=ios)
IF (ios /= 0) THEN
  ier = -2
  RETURN
ENDIF

! Cerco il parametro richiesto
DO
  READ (iu,'(a)',IOSTAT=ios) ch80  
  IF (ios /= 0) EXIT
  IF (TRIM(ch80) == "" .OR. ch80(1:1) == "!") CYCLE

  READ (ch80,'(1x,i3,2x,a8,2x,a3,2(2x,f10.2),2x,i3)',IOSTAT=ios) &
    var_t,ch8,ch3,vmin_t,vmax_t,cp2_t

  IF (ch3 == "exp") THEN
    ndec_t = -1
  ELSE
    READ (ch3,*,IOSTAT=ios2) ndec_t
  ENDIF

  IF (ios /= 0 .OR. ios2 /= 0) THEN
    ier = -3
    RETURN
  ENDIF
  
  IF (var_t /= var(3)) CYCLE

  str = ch8
  ndec = ndec_t
  vmin = vmin_t
  vmax = vmax_t
  cp2 = cp2_t
  ier = 0
  CLOSE (iu)
  RETURN

ENDDO

ier = -1
RETURN

END SUBROUTINE var2spec

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE ptn_parser(chrec,i,j,k,lat,lon,label,ier)
!
! Interpreta un record di un file .ptn
!
IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN) :: chrec
CHARACTER (LEN=*), INTENT (OUT) :: label
DOUBLE PRECISION, INTENT (OUT) :: lat,lon
INTEGER, INTENT(OUT) :: i,j,k,ier
INTEGER :: ios1,ios2,p1,p2

! Inizializzo output a dato mancante
i = 0
j = 0
k = 0
lat = -9999.
lon = -9999.
label = ""

! Parsing
p1 = INDEX(chrec,"!")
p2 = p1 + INDEX(chrec(p1+1:),"!")
IF (p1 == 0 .OR. p2 == 0) THEN
  ier = 1
  RETURN
ENDIF

READ (chrec(:p1-1),*,IOSTAT=ios1) i,j
IF (j == 0) THEN
  k = i
  i = 0
ENDIF
READ (chrec(p1+1:p2-1),*,IOSTAT=ios2) lat,lon
label = TRIM(ADJUSTL(chrec(p2+1:)))

IF (ios1 /= 0 .OR. ios2 /= 0) THEN
  ier = 2
ELSE
  ier = 0
ENDIF

RETURN
END SUBROUTINE ptn_parser

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE pts_csv_parser(chrec,i,j,k,lat,lon,label,iret)
!
! Interpreta un record di un file .pts.csv
!
USE file_utilities
IMPLICIT NONE

! Parametri della subroutine
CHARACTER (LEN=*), INTENT(IN) :: chrec
CHARACTER (LEN=*), INTENT (OUT) :: label
DOUBLE PRECISION, INTENT (OUT) :: lat,lon
INTEGER, INTENT(OUT) :: i,j,k,iret

! Variabili locali
TYPE (csv_record) :: csvline
INTEGER :: nf,ier(3)

!--------------------------------------------------------------------------

! Inizializzo output a dato mancante
i = 0
j = 0
k = 0
lat = -9999.
lon = -9999.
label = ""

! Parsing
CALL init(csvline,RECORD=chrec,NFIELD=nf)

! Campi obbligatori
CALL csv_record_getfield(csvline,FIELD=lon,IER=ier(1))
CALL csv_record_getfield(csvline,FIELD=lat,IER=ier(2))
CALL csv_record_getfield(csvline,FIELD=label,IER=ier(3))
IF (ANY(ier(1:3) /= 0)) THEN
  iret = 1
  CALL delete(csvline)
  RETURN
ENDIF

! Campi opzionali
IF (nf == 6) THEN
  CALL csv_record_getfield(csvline,FIELD=i,IER=ier(1))
  CALL csv_record_getfield(csvline,FIELD=j,IER=ier(2))
  CALL csv_record_getfield(csvline,FIELD=k,IER=ier(3))
  IF (ANY(ier(1:3) /= 0)) THEN
    i=0
    j=0
    k=0
    iret = 2
    CALL delete(csvline)
    RETURN
  ENDIF
ENDIF

iret = 0
CALL delete(csvline)

RETURN
END SUBROUTINE pts_csv_parser

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE get_eof_eor(eof, eor)
!--------------------------------------------------------------------------
! Ritorna i codici di errore macchina-dipendenti corrispondenti alle 
! condizioni di EOF e EOR nella lettura di un file sequenziale formattato
!
! Secondo manuale, questi sono gli unici due casi in cui IOSTAT ritorna
! con un valore negativo. 
! Si noti che EOR riguarda solo non-advancinag READ
!--------------------------------------------------------------------------
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

END MODULE seriet_utilities

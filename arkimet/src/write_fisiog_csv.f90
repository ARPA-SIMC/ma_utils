PROGRAM write_fisiog_csv
!--------------------------------------------------------------------------
! Legge un file .pts.csv e il relativo fisiog.gacsv; scrive coordinate e
! parametri fisiografici di ciascun punto in un .csv con una riga per punto
!
!                                        Versione 1.0.1, Enrico, 16/01/2012
!--------------------------------------------------------------------------

USE missing_values
USE file_utilities
USE seriet_utilities
IMPLICIT NONE

REAL, PARAMETER :: rmis_csv = -999.
TYPE (csv_record) :: csvline
DOUBLE PRECISION :: lon(maxpt),lat(maxpt),plon,plat
REAL, ALLOCATABLE :: alb(:),z0(:),zlev(:,:),zlay(:,:),orog(:)
INTEGER :: cnt_par,kpar,eof,eor,ios,ier,k,pi,pj,pk,npts

CHARACTER (LEN=80) :: plabel,lab(maxpt),chdum
CHARACTER (LEN=120) :: filepts,filefis,fileout,chpar,chrec
CHARACTER (LEN=1) :: next_arg

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comandi

! Default
filepts = ""
filefis = ""
fileout = ""

cnt_par = 0
next_arg = ""
DO kpar = 1,HUGE(kpar)
  CALL getarg(kpar,chpar)

  IF (TRIM(chpar) == "") THEN
    EXIT
  ELSE IF (TRIM(chpar) == "-h") THEN
    CALL scrive_help
    STOP 1
  ELSE
    cnt_par = cnt_par + 1
    IF (cnt_par == 1) THEN
      filepts = chpar
    ELSE IF (cnt_par == 2) THEN
      filefis = chpar
    ELSE IF (cnt_par == 3) THEN
      fileout = chpar
    ENDIF

  ENDIF
ENDDO

IF (filepts == "" .OR. filefis == "" .OR. fileout == "") THEN
  CALL scrive_help
  STOP 1
ENDIF  

! 1.2 trovo codice EOF
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 2) Leggo da filepts elenco e coordinate dei punti

OPEN (UNIT=30, FILE=filepts, STATUS="OLD", ACTION="READ", ERR=9999)

npts = 0
DO k = 1,HUGE(0)
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998

  IF (k == 1) THEN
    CALL test_header("lspts",chrec,ier)
    IF (ier > 0) GOTO 9997
    CYCLE
  ELSE
    CALL pts_csv_parser(chrec,pi,pj,pk,plat,plon,plabel,ier)
  ENDIF

  IF (ier /= 0) THEN
    WRITE (*,*) "Errore ",ier," parsing ",TRIM(filepts)," skip punto ",k
    CYCLE
  ELSE
    npts = npts + 1
    IF (npts > maxpt) GOTO 9996
    lat(npts) = plat
    lon(npts) = plon
    lab(npts) = plabel
  ENDIF
ENDDO

CLOSE (30)

!--------------------------------------------------------------------------
! 3) Leggo da filefis orografia, albedo, roughness

ALLOCATE (alb(npts),z0(npts),orog(npts))
ALLOCATE (zlev(npts,maxaklev),zlay(npts,maxaklay))

CALL read_fisiog_gacsv(filefis,npts,lat(1:npts),lon(1:npts),.TRUE., &
  alb,z0,zlev,zlay,orog)

WHERE (.NOT. c_e(alb(:)))
  alb(:) = rmis_csv
ENDWHERE
WHERE (.NOT. c_e(z0(:)))
  z0(:) = rmis_csv
ENDWHERE
WHERE (.NOT. c_e(orog(:)))
  orog(:) = rmis_csv
ENDWHERE

!--------------------------------------------------------------------------
! 4) Scrivo sul file di output

OPEN (UNIT=31, FILE=fileout, STATUS="REPLACE", ACTION="WRITE")
WRITE (31,'(a)') "longitude,latitude,label,orography,roughness,albedo"
DO k = 1,npts
  CALL init(csvline)
  WRITE (chdum,'(f9.4)') lon(k)
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(chdum)))
  WRITE (chdum,'(f8.4)') lat(k)
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(chdum)))
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(lab(k))))
  WRITE (chdum,'(f6.1)') orog(k)
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(chdum)))
  WRITE (chdum,'(f8.3)') z0(k)
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(chdum)))
  WRITE (chdum,'(f8.3)') alb(k)
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(chdum)))
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)  
ENDDO
CLOSE (31)

STOP

!--------------------------------------------------------------------------
! Gestione errori

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filepts)
STOP 2

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filepts)
STOP 2

9997 CONTINUE
WRITE (*,*) "Errore leggendo header ",TRIM(filepts)
STOP 2

9996 CONTINUE
WRITE (*,'(a)') "Richiesti troppi punti, aumentare maxpt in seriet_utilities.f90 (",maxpt,")"
STOP 3

END PROGRAM write_fisiog_csv

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE scrive_help
!
! Visualizza a schermo l'hlep del programma
!
IMPLICIT NONE

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: write_fisiog_csv.exe filepts filefis fileout"
WRITE (*,*) "Programma della catena seriet: legge un elenco di punti (formato .pts.csv)" 
WRITE (*,*) "  e i relativi parametri fisografici (formato .gacsv), scrive un file .csv"
WRITE (*,*) "  con coord. e parametri fisiografici di ciascun punto (una riga per punto)"

RETURN

END SUBROUTINE scrive_help

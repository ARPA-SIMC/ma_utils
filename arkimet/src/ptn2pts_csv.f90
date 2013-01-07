PROGRAM ptn2pts_csv
!--------------------------------------------------------------------------
! Legge un file in formato .ptn e lo riscrive in formato .pts.csv
! Programma scritto per gestire la transiszione da arkimet a gribarch
!
!                                           Versione 1.0, Enrico 15/04/2011
!--------------------------------------------------------------------------

USE file_utilities
USE seriet_utilities
IMPLICIT NONE

! Variabili locali
TYPE (csv_record) :: csvline
DOUBLE PRECISION :: plat,plon
INTEGER :: idp,kp,npts,k,ios,eof,eor,ier,pi,pj,pk
CHARACTER (LEN=500) :: filein,fileout,chdum,chrec,plabel
CHARACTER (LEN=10) :: str_lat,str_lon,chfmt

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
idp = 0
filein = ""
fileout = ""
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO
IF (filein == "" .OR. fileout == "") THEN
  CALL write_help
  STOP 1
ENDIF

! 1.2 trovo codice EOF
CALL get_eof_eor(eof,eor)

!--------------------------------------------------------------------------
! 2) Leggo e riscrivo

! Apro files e scrivo header
OPEN (UNIT=30, FILE=filein, STATUS="OLD", ACTION="READ", ERR=9999)

OPEN (UNIT=31, FILE=fileout, STATUS="REPLACE")
chrec = ""
CALL build_header("lspts",chrec)
WRITE (31,'(a)') TRIM(chrec)

! Formato per scrittura coordinate
WRITE (chfmt,'(a,i2,a,i2,a)') "(f",coo_ndec+5,".",coo_ndec,")"

! 2.1 Ciclo sui punti
npts = 0
DO k = 1,HUGE(0)

! Leggo dal file .ptn
  READ (30,'(a)',IOSTAT=ios) chrec
  IF (ios == eof) EXIT
  IF (ios /= 0) GOTO 9998

  CALL ptn_parser(chrec,pi,pj,pk,plat,plon,plabel,ier)
  IF (ier /= 0) GOTO 9997
  npts = npts + 1

! Scrivo sul file .pts.csv
  CALL init(csvline)

! Coordinate (con il numero di decimali richiesto in seriet_utilities)
  WRITE (str_lon,chfmt) plon
  WRITE (str_lat,chfmt) plat
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_lon)))
  CALL csv_record_addfield(csvline,TRIM(ADJUSTL(str_lat)))

! Label
  CALL csv_record_addfield(csvline,TRIM(plabel))

! Indici
  IF (pj == 0) THEN
    CALL csv_record_addfield(csvline,0)
    CALL csv_record_addfield(csvline,0)
    CALL csv_record_addfield(csvline,pk)
  ELSE
    CALL csv_record_addfield(csvline,pi)
    CALL csv_record_addfield(csvline,pj)
    CALL csv_record_addfield(csvline,0)
  ENDIF

! Scrivo su file
  WRITE (31,'(a)') csv_record_getrecord(csvline)
  CALL delete(csvline)

ENDDO

! 2.2 Chiudo files e conclusione
CLOSE (30)
CLOSE (31)

WRITE (*,*) "Elaborati ",npts," punti"
STOP

!--------------------------------------------------------------------------
! 3) Gestione errori

9999 CONTINUE
WRITE (*,*)  "Errore aprendo ",TRIM(filein)
STOP 2
 
9998 CONTINUE
WRITE (*,*)  "Errore leggendo ",TRIM(filein)
STOP 3

9997 CONTINUE
WRITE (*,*) "Errore ",ier," parsing ",TRIM(filein)," punto ",k
STOP 4

END PROGRAM ptn2pts_csv

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: ptn2pts_csv.exe filein fileout [-h]" 
WRITE (*,*) "Legge un file in formato .ptn e lo riscrive in formato .pts.csv"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

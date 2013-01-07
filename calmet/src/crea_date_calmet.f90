PROGRAM crea_date_calmet
!--------------------------------------------------------------------------
! Programma della catena calmet.
! Calcola le date estreme di uno specifico run.
!
! Uso: crea_date_calmet.exe [-h] data run_len hh_int fileout
!
!                                                    V2.0,Enrico 01/09/2007
!--------------------------------------------------------------------------
USE date_handler
IMPLICIT NONE

TYPE(date) :: data_r1,data_r2,data_d1,data_d2
INTEGER :: hh_r1,hh_r2,hh_d1,hh_d2
INTEGER :: k,run_len,hh_int,days,ios(3)
CHARACTER (LEN=80) :: chpar,fileout

!--------------------------------------------------------------------------
! 1) Preliminari e input

! 1.1 parametri da riga comandi
CALL getarg(1,chpar)
READ (chpar,'(i4.4,3i2.2)',IOSTAT=ios(1)) &
  data_r1%yy,data_r1%mm,data_r1%dd,hh_r1
CALL getarg(2,chpar)
READ (chpar,'(i4.4,3i2.2)',IOSTAT=ios(2)) run_len
CALL getarg(3,chpar)
READ (chpar,'(i4.4,3i2.2)',IOSTAT=ios(3)) hh_int
CALL getarg(4,fileout)

IF (TRIM(fileout) == "" .OR. run_len <= 0 .OR. ANY(ios(:)/=0)) THEN
  WRITE (*,*) "Uso: crea_date_calmet.exe [-h] data run_len hh_int fileout"
  STOP
ENDIF

!--------------------------------------------------------------------------
! 2) Elaborazioni

! data-ora di fine run
hh_r2 = MOD(hh_r1 + run_len, 24)
days = (hh_r1 + run_len)/24
data_r2 = data_r1 + days

! data-ora di inizio-fine estrazione dati
IF (hh_int < 0) THEN                        ! interp. disabilitata

  data_d1 = data_r1
  data_d2 = data_r2
  hh_d1 = hh_r1
  hh_d2 = hh_r2

ELSE                                        ! interp. abilitata

 IF (hh_int > hh_r1) THEN
    days = (hh_int - hh_r1 +23 )/24
    data_d1 = data_r1 - days
    hh_d1 =  MOD(hh_r1 - hh_int +24*days, 24)
  ELSE
    days = (hh_r1 - hh_int)/24
    data_d1 = data_r1
    hh_d1 = MOD(hh_r1 - hh_int, 24)
  ENDIF

  hh_d2 = MOD(hh_r2 + hh_int, 24)
  days = (hh_r2 + hh_int)/24
  data_d2 = data_r2 + days

ENDIF

!--------------------------------------------------------------------------
! 3) Output

OPEN (UNIT=41, FILE=fileout,STATUS="REPLACE", &
  ACTION="WRITE")

WRITE (41,'(a)') "! File creato automaticamente da crea_estra_inp.f90."
WRITE (41,'(a)') "! Derivato da pre_calmet.inp; ore GMT; versione 1, 01/10/2002"
WRITE (41,'(a)') "!-------------------------------------------------------"
WRITE (41,'(i4.4,3i2.2,3x,a)') data_r1%yy,data_r1%mm,data_r1%dd,hh_r1, &
  "! data-ora di inizio run (YYYYMMDDHHMM)"
WRITE (41,'(i4.4,3i2.2,3x,a)') data_r2%yy,data_r2%mm,data_r2%dd,hh_r2, &
  "! data-ora di fine run (YYYYMMDDHHMM)"
WRITE (41,'(i4.4,3i2.2,3x,a)') data_d1%yy,data_d1%mm,data_d1%dd,hh_d1, &
  "! data-ora primo dato oss. da estrarre"
WRITE (41,'(i4.4,3i2.2,3x,a)') data_d2%yy,data_d2%mm,data_d2%dd,hh_d2, &
  "! data-ora ultimo dato oss. da estrarre"

CLOSE(41)

WRITE (*,'(2a)') "crea_date_calmet: scritto ",TRIM(fileout)
STOP

END PROGRAM crea_date_calmet


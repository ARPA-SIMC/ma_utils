PROGRAM grib_ana2forc
!--------------------------------------------------------------------------
! Dato un file con molti grib e lo riscrive con tutti i campi come 
! previsioni istantanee con reference time assegnato
! Per ora gestisce solo GRIB1
!
!                                          Versione 1.0.4 Enrico 31/03/2014
!--------------------------------------------------------------------------

USE grib_api
USE datetime_class
USE grib2_utilities
IMPLICIT NONE

INTEGER :: ifin,ifout,igin,igout,iret,en,yoc,cortod,scad_out
INTEGER :: yy,mm,dd,hh,kg,scad_min,scad_max,cntg
CHARACTER(LEN=200) :: filein,fileout
CHARACTER(LEN=10) :: ch10
TYPE(datetime) :: rtime_out,rtime_in,vtime_in
TYPE(timedelta) :: dlt

!--------------------------------------------------------------------------
! Parametri da riga comando
CALL getarg(1,filein)
CALL getarg(2,fileout)
CALL getarg(3,ch10)

IF (TRIM(filein) == "" .OR. TRIM(fileout) == "" .OR. &
  TRIM(filein) == "-h" .OR. TRIM(filein) == "--help") THEN
  WRITE (*,*) "Uso: grib_ana2forc.exe filein fileout YYYYMMDDHH"
  STOP
ENDIF

READ (ch10,'(i4,3i2)') yy,mm,dd,hh
CALL init(rtime_out, YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
cortod = (yy-1)/100 + 1
yoc = MOD(yy-1,100) + 1

! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

! Ciclo sui grib

scad_min = 9999
scad_max = -9999
cntg = 0

DO kg = 1,HUGE(0)

! Leggo il prossimo campo e lo duplico
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998
  CALL grib_get(igin,"editionNumber",en)
  IF (en /= 1) GOTO 9997

  CALL grib_clone(igin,igout)

! Trovo verification time
  CALL get_grib_time(igin,rtime_in,vtime_in,iret)
  IF (iret /= 0) GOTO 9996
  dlt = vtime_in - rtime_out
  CALL getval(dlt, AHOUR=scad_out)

  IF (scad_out < scad_min) scad_min = scad_out
  IF (scad_out > scad_max) scad_max = scad_out
  CALL grib_set(igout,"yearOfCentury",yoc)
  CALL grib_set(igout,"centuryOfReferenceTimeOfData",cortod)
  CALL grib_set(igout,"month",mm)
  CALL grib_set(igout,"day",dd)
  CALL grib_set(igout,"hour",hh)

  IF (en == 1) THEN
    CALL grib_set(igout,"unitOfTimeRange",1)
    CALL grib_set(igout,"P1",scad_out)
    CALL grib_set(igout,"P2",0)
    CALL grib_set(igout,"timeRangeIndicator",0)
  ELSE
    WRITE (*,*) "Non implementato"
  ENDIF

! Lo riscrivo
  CALL grib_write (igout,ifout)
  cntg = cntg + 1

! Libero memoria
  CALL grib_release(igin)
  CALL grib_release(igout)

ENDDO

WRITE (*,*) "Elaborazioni completate, grib elaborati ",cntg
WRITE (*,'(a,i4.4,3i2.2,2(a,i3.3))') "Reftime, scad.min, scad.max ", &
  yy,mm,dd,hh,"  +",scad_min,"  +",scad_max

STOP

9999 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP

9998 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP

9997 CONTINUE
WRITE (*,*) "Grib2 non ancora supportati"
STOP

9996 CONTINUE
WRITE (*,*) "Errore subr. grib_get_time ",iret
STOP

END PROGRAM grib_ana2forc

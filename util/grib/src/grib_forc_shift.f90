PROGRAM grib_forc_shift
!--------------------------------------------------------------------------
! Legge un file con molti grib, lo riscrive spostando (avanti o indietro)
! il reference time.
!
! Use: grib2_forc_shift.exe [-h] filein fileout hh_shift [-f0 file0]" [-fill]
!
! Notes: 
! - file0 usually comes from the same run as the data to be processsed.
!   this progam presently only checks ref.time and n.er of grid points  
!
! Compile command (SIMC servers)
! gfortran -I/usr/lib64/gfortran/modules -leccodes_f90 \
!   kinds.F90 missing_values.f90 io_units.F90 optional_values.f90 \
!   char_utilities.F90 err_handling.f90 datetime_class.F90 grib_forc_shfit.f90 \
!   grib_forc_shift.f90  
!
! On Atos, replace "-I.. -l.." with $ECCODES_INCLUDE $ECCODES_LIB 
!
!                                         Versione 1.0.0, Enrico 30/09/2025
!--------------------------------------------------------------------------

USE datetime_class
USE grib_api
IMPLICIT NONE

REAL, PARAMETER :: rmiss = HUGE(1.0)

TYPE (datetime) :: rtime,rtime_in,rtime_out
INTEGER :: ifin=0,ifin0=0,ifout=0,igin=0,igin0,igout=0
INTEGER :: idp,hh_shift,iret,kg,kg0,ng0,kp
INTEGER :: yy,mon,dd,hh,min,en,pdtn,tosp,iouftr,ioufft
INTEGER :: ft_in,ft_out,fth_in,fth_out,lotr_in,lotr_out,lotrh_in,lotrh_out
INTEGER :: cnt_ist,cnt_proc,cnt_fill,nf0,mlc

INTEGER, PARAMETER :: maxg0 = 10
INTEGER :: dis0(maxg0),pc0(maxg0),pn0(maxg0)
INTEGER :: dis,pc,pn,np_sav,np

REAL, ALLOCATABLE :: field0(:,:),field(:),field_out(:),field_mask(:)
REAL :: fill_value
CHARACTER(LEN=200) :: filein,filein0,fileout,chdum
CHARACTER(LEN=2) :: next_arg
LOGICAL :: lf0,lfill

!--------------------------------------------------------------------------
! 1) Preliminary

! 1.1 Command line
lf0 = .FALSE.
lfill = .FALSE.
idp = 0
next_arg = ""
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE IF (TRIM(chdum) == "-fill") THEN
    lfill = .TRUE.
  ELSE IF (next_arg == "f0") THEN
    filein0 = chdum
    lf0 = .TRUE.
 ELSE IF (TRIM(chdum) == "-f0") THEN
    next_arg = "f0"
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE (3)
      READ(chdum,*) hh_shift
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO

IF (idp < 3) THEN
  CALL write_help
  STOP 1
ENDIF

! 1.2 Open files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9999
CALL grib_open_file(ifout,fileout,"w")

IF (lf0) THEN
  CALL grib_open_file(ifin0,filein0,"r",iret)
  IF (iret /= GRIB_SUCCESS) GOTO 9994
ENDIF
  
! 1.3 If needed, read the file with intial values for non-instantaneous fields

IF (lf0) THEN
  nf0 = 0
  DO kg = 1,maxg0
    igin0 = -1
    CALL grib_new_from_file(ifin0,igin0,iret)
    IF (iret == GRIB_END_OF_FILE) EXIT
    IF (iret /= GRIB_SUCCESS) GOTO 9993

    CALL grib_get(igin0,"year",yy)
    CALL grib_get(igin0,"month",mon)                             
    CALL grib_get(igin0,"day",dd)                               
    CALL grib_get(igin0,"hour",hh)                              
    CALL grib_get(igin0,"minute",min)                              
    CALL grib_get(igin0,"numberOfDataPoints",np)                              
    rtime = datetime_new(YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)

    IF (kg == 1) THEN
      np_sav = np
      rtime_in = rtime
      ALLOCATE(field0(np_sav,maxg0),field(np_sav),field_out(np_sav))
      IF (lfill) ALLOCATE (field_mask(np_sav))
    ELSE
      IF (np /= np_sav) GOTO 9987
      IF (rtime /= rtime_in) GOTO 9989
    ENDIF

    CALL grib_get(igin0,"discipline",dis0(kg))
    CALL grib_get(igin0,"parameterCategory",pc0(kg))
    CALL grib_get(igin0,"parameterNumber",pn0(kg))
    CALL grib_get(igin0,"values",field0(:,kg))

  ENDDO
  IF (kg > maxg0) THEN
    GOTO 9988
  ELSE
    ng0 = kg
  ENDIF   
ENDIF

!--------------------------------------------------------------------------
! 2) Run (loop on input grib)

cnt_ist = 0
cnt_proc = 0
cnt_fill = 0

grib: DO kg = 1,HUGE(0)
  WRITE (*,*) "Processing grib: ",kg
   
! 2.1 Read and clone next grib
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9998
  CALL grib_clone(igin,igout)

! 2.2 GRIB edition, number of points
  CALL grib_get(igin,"editionNumber",en)
  IF ( en /= 2 ) GOTO 9997

  CALL grib_get(igin,"numberOfDataPoints",np)                              
  IF (kg == 1 .AND. .NOT. lf0) THEN
    np_sav = np
  ELSE
    IF (np /= np_sav) GOTO 9985
  ENDIF
    
! 2.3 Reference time
  CALL grib_get(igin,"year",yy)
  CALL grib_get(igin,"month",mon)                             
  CALL grib_get(igin,"day",dd)                               
  CALL grib_get(igin,"hour",hh)                              
  CALL grib_get(igin,"minute",min)                              
  rtime = datetime_new(YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)
 
  IF (kg == 1 .AND. .NOT. lf0) THEN
    rtime_in = rtime
    ALLOCATE(field0(np_sav,maxg0),field(np_sav),field_out(np_sav))
  ELSE
    IF (rtime /= rtime_in) GOTO 9986
  ENDIF

  rtime_out = rtime + timedelta_new(hour=hh_shift)
  CALL getval(rtime_out, YEAR=yy, MONTH=mon, DAY=dd, HOUR=hh, MINUTE=min)
  CALL grib_set(igout,"year",yy)
  CALL grib_set(igout,"month",mon)
  CALL grib_set(igout,"day",dd)
  CALL grib_set(igout,"hour",hh)
  CALL grib_set(igout,"minute",min)

!--------------------------------------------------------------------------
! 2.4 Fields that are instantaneous, maximum or minimum: change forecastTime

  CALL grib_get(igin,"productDefinitionTemplateNumber",pdtn)
  IF (pdtn == 11) THEN
    CALL grib_get(igin,"typeOfStatisticalProcessing",tosp)
  ELSE
    tosp = -999
  ENDIF
  IF (pdtn == 1 .OR. (pdtn == 11 .AND. (tosp == 2 .OR. tosp == 3)) ) THEN
     
!   Input forecastTime in hours
    CALL grib_get(igin,"forecastTime",ft_in)
    CALL grib_get(igin,"indicatorOfUnitForForecastTime",ioufft)
    IF (ioufft == 0) THEN       ! minutes
      IF (MOD(ft_in,60) /= 0) GOTO 9995
      fth_in = ft_in / 60.
    ELSE IF (ioufft == 1) THEN  ! hours
      fth_in = ft_in
    ELSE
      GOTO 9997
    ENDIF
    
!   Output forecastTime in hours
    IF (fth_in < hh_shift) THEN
      GOTO 9992
    ELSE
      fth_out = fth_in - hh_shift
    ENDIF
    
!   Output forecastTime in the same unit as input
    IF (ioufft == 0) THEN       ! minutes
      ft_out = fth_out * 60
    ELSE IF (ioufft == 1) THEN  ! hours
      ft_out = fth_out
    ENDIF

    CALL grib_set(igout,"forecastTime",ft_out)

!  If requested, fill the missing values (if any!)
    IF (lfill) THEN
      CALL grib_get(igout,"values",field_out(:))
      IF (COUNT(field_out(:) /= rmiss) == 0) THEN
        GOTO 9982 

      ELSE IF (COUNT(field_out(:) == rmiss) == 0) THEN
        CONTINUE

      ELSE
        fill_value = SUM(field_out(:), MASK=field_out(:)/=rmiss) / &
          REAL(COUNT(field_out(:)/=rmiss))
        WHERE (field_out(:) == rmiss)
          field_mask(:) = fill_value
        ELSEWHERE
          field_mask(:) = field_out(:)
        ENDWHERE

        CALL grib_set(igout,"values",field_mask(:))
        cnt_fill = cnt_fill + 1
        WRITE (*,*) "Grib ",kg,": ",COUNT(field_out(:)/=rmiss)," missing values set to ",fill_value

      ENDIF
    ENDIF

    cnt_ist = cnt_ist + 1
    
!--------------------------------------------------------------------------
! 2.5 Fields that are average or accumulated

  ELSE IF (pdtn == 11 .AND. (tosp == 0 .OR. tosp == 1) ) THEN

! 2.5.1 Change lenghtOfTimeRange

!   Input lengthOfTimeRange in hours
    CALL grib_get(igin,"lengthOfTimeRange",lotr_in)
    CALL grib_get(igin,"indicatorOfUnitForTimeRange",iouftr)
    IF (iouftr == 0) THEN       ! minutes
      IF (MOD(lotr_in,60) /= 0) GOTO 9995
      lotrh_in = lotr_in / 60.
    ELSE IF (iouftr == 1) THEN  ! hours
      lotrh_in = lotr_in
    ELSE
      GOTO 9997
    ENDIF
    
!   Output lengthOfTimeRange in hours
    IF (lotrh_in < hh_shift) THEN
      GOTO 9992
    ELSE
      lotrh_out = lotrh_in - hh_shift
    ENDIF
    
!   Output lengthOfTimeRange in the same unit as input
    IF (iouftr == 0) THEN       ! minutes
      lotr_out = lotrh_out * 60
    ELSE IF (iouftr == 1) THEN  ! hours
      lotr_out = lotrh_out
    ENDIF

    CALL grib_set(igout,"lengthOfTimeRange",lotr_out)

! 2.5.2 Change values

!   Find the corresponding filed in file0
    DO kg0 = 1,ng0
      CALL grib_get(igin,"discipline",dis)
      CALL grib_get(igin,"parameterCategory",pc)
      CALL grib_get(igin,"parameterNumber",pn)
      IF (dis==dis0(kg0) .AND. pc==pc0(kg0) .AND. pn==pn0(kg0)) EXIT
    ENDDO
    IF (kg0 > ng0) GOTO 9984
    CALL grib_get(igin,"values",field(:))

    IF (tosp == 1) THEN            ! field is cumulated
      field_out(:) = field(:) - field0(:,kg0)
      WRITE (*,*) "Processing cumulate field: ",dis,pc,pn
      mlc = MAXLOC(field(:),DIM=1)
      WRITE (*,*) "Input:  maxval, maxloc ", MAXVAL(field(:)),MAXLOC(field(:))
      WRITE (*,*) "Input:  ave, test ", SUM(field(:))/REAL(np_sav),field(mlc)
      WRITE (*,*) "F0:     ave, test ", SUM(field0(:,kg0))/REAL(np_sav),field0(mlc,kg0)
      WRITE (*,*) "Output: ave, test ", SUM(field_out(:))/REAL(np_sav),field_out(mlc)

    ELSE IF (tosp == 0) THEN       ! field is average
      field_out(:) = (field(:)*REAL(lotrh_in) - field0(:,kg0)*REAL(hh_shift)) / REAL(lotrh_out)
      WRITE (*,*) "Processing average field: ",dis,pc,pn,lotrh_in,hh_shift,lotrh_out
      mlc = MAXLOC(field(:),DIM=1)
      WRITE (*,*) "Input:  maxval, maxloc ", MAXVAL(field(:)),MAXLOC(field(:))
      WRITE (*,*) "Input:  ave, test ", SUM(field(:))/REAL(np_sav),field(mlc)
      WRITE (*,*) "F0:     ave, test ", SUM(field0(:,kg0))/REAL(np_sav),field0(mlc,kg0)
      WRITE (*,*) "Output: ave, test ", SUM(field_out(:))/REAL(np_sav),field_out(mlc)

    ENDIF

! 2.5.3 If requested, fill the missing values (if any!)
    IF (lfill) THEN
      IF (COUNT(field_out(:) /= rmiss) == 0) THEN
        GOTO 9982 

      ELSE IF (COUNT(field_out(:) == rmiss) == 0) THEN
        CALL grib_set(igout,"values",field_out(:))      ! field was processed 

      ELSE
        fill_value = SUM(field_out(:), MASK=field_out(:)/=rmiss) / &
          REAL(COUNT(field_out(:)/=rmiss))
        WHERE (field_out(:) == rmiss)
          field_mask(:) = fill_value
        ELSEWHERE
          field_mask(:) = field_out(:)
        ENDWHERE

        CALL grib_set(igout,"values",field_mask(:))     ! field was processed and filled
        cnt_fill = cnt_fill + 1
        WRITE (*,*) "Grib ",kg,": ",COUNT(field_out(:)/=rmiss)," missing values set to ",fill_value
      ENDIF

    ELSE 
      CALL grib_set(igout,"values",field_out(:))

    ENDIF

    cnt_proc = cnt_proc + 1

!--------------------------------------------------------------------------
! 2.6 Other fields

  ELSE
    GOTO 9983
  ENDIF
  
! 2.7 Write output
  CALL grib_write (igout,ifout)
  CALL grib_release (igin)
  CALL grib_release (igout)

ENDDO grib
  
!--------------------------------------------------------------------------
! 3) Conclusion

WRITE (*,*) "Elaborazioni completate, letti e riscritti ",kg-1," campi"
WRITE (*,*) "GRIB2 istantaneous: ",cnt_ist
WRITE (*,*) "GRIB2 processsed:   ",cnt_proc
IF (lfill) WRITE (*,*) "GRIB2 with filled missing values: ",cnt_fill

CALL grib_close_file(ifin)
CALL grib_close_file(ifout)
STOP

!--------------------------------------------------------------------------
! 4) Error management

9999 CONTINUE
WRITE (*,*) "Error opening ",TRIM(filein)
STOP 2

9998 CONTINUE
WRITE (*,*) "Error reading ",TRIM(filein)," grib n.er " ,kg
STOP 2

9997 CONTINUE
WRITE (*,*) "grib1 field, not allowed ",kg
STOP 3

9995 CONTINUE
WRITE (*,*) "ft (in minutes) is not an integer number of hours ",ft_in
STOP 3

9994 CONTINUE
WRITE (*,*) "Error opening ",TRIM(filein0)
STOP 2

9993 CONTINUE
WRITE (*,*) "Error reading ",TRIM(filein0)," grib n.er " ,kg
STOP 2

9992 CONTINUE
WRITE (*,*) "Error: forecast time < shift ",TRIM(filein)," grib n.er " ,kg
STOP 3

9989 CONTINUE
WRITE (*,*) "Error: different reftime in ",TRIM(filein0)," grib n.er " ,kg
STOP 4

9988 CONTINUE
WRITE (*,*) "Found more than ",maxg0," (maxg0) grib in ",TRIM(filein0)
STOP 4

9987 CONTINUE
WRITE (*,*) "Found ",np," points, expected ",np_sav," file ",TRIM(filein0)," grib ",kg
STOP 4

9986 CONTINUE
WRITE (*,*) "Error: different reftime in ",TRIM(filein)," grib n.er " ,kg
STOP 4

9985 CONTINUE
WRITE (*,*) "Found ",np," points, expected ",np_sav," file ",TRIM(filein)," grib ",kg
STOP 4

9984 CONTINUE
WRITE (*,*) "Statsitically processed parameter not found in ",TRIM(filein0)
WRITE (*,*) "discipline, category, number ",dis,pc,pn," grib in input is n.er ",kg
STOP 5

9983 CONTINUE
WRITE (*,*) "pdtn/tosp not allowed ",pdtn,tosp
STOP 6

9982 CONTINUE
WRITE (*,*) "Field entirely missing, values can not be replaced: grib number ",kg
STOP 7

END PROGRAM grib_forc_shift

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib2_forc_shift.exe [-h] filein fileout hh_shift [-f0 file0] [-fill]"
WRITE (*,*) "Legge un file con molti grib (edizione 2) e lo riscrive spostando il ref. time"
WRITE (*,*) "  hh_shift: spostamento richiesto per il ref. time (>0)"
WRITE (*,*) "  file0: eventuale file con i campi non istantanei all'istante del nuovo ref. time"
WRITE (*,*) "  -fill: sostituisce i valori mancanti con la media del campo"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

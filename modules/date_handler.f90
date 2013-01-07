!   Module for handlying dates calculations
MODULE date_handler
 
!--------------------------------------------------------------------------
! This module contains:
!
! - Declaration of "date" type (gg,mm,yyyy)
!
! - Definition of operators with "date" type arguments
!   *) date + N                     : add N days to a date
!   *) date1 - date2                : n.er of days between two dates
!   *) date1 > < == /= >= <= date2  : relation operators
!
! - Basic (low-level) functions
!   *) date2day
!   *) day2date
!
! - Functions & subroutines for implementation of operators
!   *) date_incr, date_decr, date_dist
!   *) date1_gt_date2, date1_lt_date2, date1_ge_date2, date1_le_date2 
!      date1_eq_date2, date1_ne_date2
!
! - Other functions
!   *) jul(date)             : for a given date, returns julian day
!   *) greg(julian_day,year) : for a given jul.day and year returns date
!   *) week_day(date)        : for a given date, returns day of week 
!                              (sun = 1)
!   *) grads_date            : returns a date as a ch*9 string with the 
!                              format of a GRADS ctl file (DDmmmYYYY)
!   *) date2sqldate          : returns a date as a ch*9 string with the 
!                              format of a SQL*Plus query (DD-MMM-YY)
!   *) sqldate2date          : inverse of date2sqldate
!
! 
! Current Code Owner: Enrico, (Johnny)
! 
! History: 
!  
! Version   Date         Comment 
! -------   ----         ------- 
! v1.2      30/05/2001   initial version (update 2)
! v1.3      05/11/2001   added "julian day" functions
! v1.4      10/06/2002   added "week_day" function
! v1.5      08/11/2002   added "grads_date" function
! v1.6      09/02/2004   added "date2sqldate" and "sqldate2date" functions
! v1.7      04/08/2004   modified to be compatible with Annapurna
!--------------------------------------------------------------------------
 
IMPLICIT NONE 
! Global (i.e. public) Declarations: 

INTEGER, PARAMETER :: gg_mese(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)

TYPE :: date
! Contains a date in the form day, month, year
INTEGER :: dd
INTEGER :: mm
INTEGER :: yy

END TYPE date

! Local (i.e. private) Declarations: 
 
! Operator definitions: 

INTERFACE OPERATOR (+)
  MODULE PROCEDURE date_incr
END INTERFACE

INTERFACE OPERATOR (-)
  MODULE PROCEDURE date_dist
  MODULE PROCEDURE date_decr
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE date1_eq_date2
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE date1_ne_date2
END INTERFACE

INTERFACE OPERATOR (>)
  MODULE PROCEDURE date1_gt_date2
END INTERFACE

INTERFACE OPERATOR (>=)
  MODULE PROCEDURE date1_ge_date2
END INTERFACE

INTERFACE OPERATOR (<)
  MODULE PROCEDURE date1_lt_date2
END INTERFACE

INTERFACE OPERATOR (<=)
  MODULE PROCEDURE date1_le_date2
END INTERFACE

CONTAINS 
! Define procedures contained in this module. 
!==========================================================================
FUNCTION date2day (cdate) RESULT (day)
!
! function to return the number of days between tha input date and 
! 01-jan-0000 (which correspond to day =1).
! 22/06/2001: bug fixed in bisestiles handling
!
IMPLICIT NONE

TYPE (date), INTENT(IN) :: cdate
INTEGER :: day

INTEGER :: im,m1,a1
!--------------------------------------------------------------------------

m1 = 0
DO im = 1, cdate%mm - 1
  IF (im == 2 .AND. MOD(cdate%yy,4) == 0) THEN
    m1 = m1 + 29
  ELSE
    m1 = m1 + gg_mese(im)
  ENDIF
ENDDO

a1 = cdate%yy*365 + (cdate%yy+3)/4
day = a1 + m1 + cdate%dd

RETURN
END FUNCTION date2day

!==========================================================================
FUNCTION day2date (day) RESULT (cdate)
!
! function to return the date of 01-jan-0000 plus "day" days.
! 22/06/2001: bug fixed in bisestiles handling
!
IMPLICIT NONE

INTEGER, INTENT(IN) :: day
TYPE (date) :: cdate

INTEGER :: im,a2,gresto,gg_curr_mese
!--------------------------------------------------------------------------

a2 = ((day-1) * 4) / (365*4 + 1)
gresto = day - (a2*365 + (a2+3) / 4)
cdate%yy = a2

DO im = 1,12
  IF (im == 2 .AND. MOD(a2,4) == 0) THEN
    gg_curr_mese = 29
  ELSE
    gg_curr_mese = gg_mese(im)
  ENDIF

  IF (gresto .le. gg_curr_mese ) EXIT
  gresto = gresto - gg_curr_mese
ENDDO

cdate%mm = im
cdate%dd = gresto

RETURN
END FUNCTION day2date
!==========================================================================


!==========================================================================
FUNCTION date_incr (cdate,incr) RESULT (new_date)
!
! function to return a date incremented of incr days. Incr can be positive
! or negative (a negative value resulting in new_date PRIOR then cdate).
!
IMPLICIT NONE

TYPE (date), INTENT(IN) :: cdate
INTEGER, INTENT(IN) :: incr
TYPE (date) :: new_date

INTEGER :: gi, gf
!--------------------------------------------------------------------------

gi = date2day(cdate)
gf = gi + incr
new_date = day2date(gf)

RETURN
END FUNCTION date_incr

!==========================================================================
FUNCTION date_decr (cdate,decr) RESULT (new_date)
!
! function to return a date decremented of decr days. Decr can be positive
! or negative (a negative value resulting in new_date LATER than cdate)
!
IMPLICIT NONE

TYPE (date), INTENT(IN) :: cdate
INTEGER, INTENT(IN) :: decr
TYPE (date) :: new_date

INTEGER :: gi, gf
!--------------------------------------------------------------------------

gi = date2day(cdate)
gf = gi - decr
new_date = day2date(gf)

RETURN
END FUNCTION date_decr

!==========================================================================
FUNCTION date_dist (date1,date2) RESULT (dist)
!
! function to evaluate the n.er of days between date1 and date2. The 
! difference is positive if date1 is bigger (later) than date2
!
IMPLICIT NONE

TYPE (date), INTENT(IN) :: date1, date2
INTEGER :: dist

INTEGER :: g1, g2
!--------------------------------------------------------------------------

g1 = date2day(date1)
g2 = date2day(date2)
dist = g1 - g2

RETURN
END FUNCTION date_dist
!==========================================================================


!==========================================================================
FUNCTION date1_gt_date2 (date1,date2)  RESULT (ll)
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: date1, date2
LOGICAL :: ll
INTEGER :: g1, g2

g1 = date2day(date1)
g2 = date2day(date2)
ll = (g1 > g2)

RETURN
END FUNCTION date1_gt_date2

!==========================================================================
FUNCTION date1_lt_date2 (date1,date2)  RESULT (ll)
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: date1, date2
LOGICAL :: ll
INTEGER :: g1, g2

g1 = date2day(date1)
g2 = date2day(date2)
ll = (g1 < g2)

RETURN
END FUNCTION date1_lt_date2

!==========================================================================
FUNCTION date1_ge_date2 (date1,date2)  RESULT (ll)
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: date1, date2
LOGICAL :: ll
INTEGER :: g1, g2

g1 = date2day(date1)
g2 = date2day(date2)
ll = (g1 >= g2)

RETURN
END FUNCTION date1_ge_date2

!==========================================================================
FUNCTION date1_le_date2 (date1,date2) RESULT (ll)
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: date1, date2
LOGICAL :: ll
INTEGER :: g1, g2

g1 = date2day(date1)
g2 = date2day(date2)
ll = (g1 <= g2)

RETURN
END FUNCTION date1_le_date2

!==========================================================================
FUNCTION date1_eq_date2 (date1,date2)  RESULT (ll)
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: date1, date2
LOGICAL :: ll
INTEGER :: g1, g2

g1 = date2day(date1)
g2 = date2day(date2)
ll = (g1 == g2)

RETURN
END FUNCTION date1_eq_date2

!==========================================================================
FUNCTION date1_ne_date2 (date1,date2)  RESULT (ll)
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: date1, date2
LOGICAL :: ll
INTEGER :: g1, g2

g1 = date2day(date1)
g2 = date2day(date2)
ll = (g1 /= g2)

RETURN
END FUNCTION date1_ne_date2
!==========================================================================


!==========================================================================
FUNCTION jul (cdate) RESULT (julday)
!
! function to return the julian day of a given date
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: cdate
INTEGER :: julday

julday = (cdate - date(1,1,cdate%yy)) + 1

RETURN
END FUNCTION jul

!==========================================================================
FUNCTION greg (julday, year) RESULT (cdate)
!
! function to return the date (day, month, year) corresponding to a given 
! year and julian day
!
! NB: the year must be specified to distinguish between bisestiles and
!     non-bisestiles
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: julday,year
TYPE (date) :: cdate

cdate = date(1,1,year) + (julday-1)

RETURN
END FUNCTION greg

!==========================================================================
FUNCTION week_day (cdate) RESULT (wday)
!
! function to return the day of the week (sunday = 1) for a given date
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: cdate
INTEGER :: wday

! reference date (Saturday 1/1/2000)
TYPE (date), PARAMETER :: ref_date = date(1,1,2000)
INTEGER, PARAMETER :: ref_wday = 7

wday = MOD((cdate - ref_date + ref_wday - 1), 7) + 1
IF (wday < 1) wday = wday + 7

RETURN
END FUNCTION week_day

!==========================================================================
FUNCTION grads_date (cdate) RESULT (ch9)
!
! function to return a date as a ch*9 string with the format required in a 
! GRADS ctl file (DDmmmYYYY)
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: cdate
CHARACTER(LEN=9) :: ch9

CHARACTER(LEN=3), PARAMETER :: ch3_month(12) = &
  (/'jan','feb','mar','apr','may','jun', &
    'jul','aug','sep','oct','nov','dec'/)
CHARACTER(LEN=1), PARAMETER :: dg(0:9) = &
  (/'0','1','2','3','4','5','6','7','8','9'/)

INTEGER :: dgd(2),dgy(4)

dgd(1) = cdate%dd/10
dgd(2) = MOD(cdate%dd,10)

dgy(1) = cdate%yy/1000
dgy(2) = cdate%yy/100 - 10*dgy(1)
dgy(3) = cdate%yy/10 - 100*dgy(1) - 10*dgy(2)
dgy(4) = MOD(cdate%yy,10)

ch9=dg(dgd(1))//dg(dgd(2))//ch3_month(cdate%mm)// &
  dg(dgy(1))//dg(dgy(2))//dg(dgy(3))//dg(dgy(4))

RETURN
END FUNCTION grads_date

!==========================================================================
FUNCTION date2sqldate (cdate) RESULT (ch9)
!
! function to return a date as a ch*9 string with the format required in a 
! SQL*Plus query (DD-MMM-YY)
!
IMPLICIT NONE
!
TYPE (date), INTENT(IN) :: cdate
CHARACTER(LEN=9) :: ch9

CHARACTER(LEN=5), PARAMETER :: ch5_month(12) = &
  (/'-JAN-','-FEB-','-MAR-','-APR-','-MAY-','-JUN-', &
    '-JUL-','-AUG-','-SEP-','-OCT-','-NOV-','-DEC-'/)
CHARACTER(LEN=1), PARAMETER :: dg(0:9) = &
  (/'0','1','2','3','4','5','6','7','8','9'/)

INTEGER :: dgd(2),dgy(4)

dgd(1) = cdate%dd/10
dgd(2) = MOD(cdate%dd,10)

dgy(1) = cdate%yy/1000
dgy(2) = cdate%yy/100 - 10*dgy(1)
dgy(3) = cdate%yy/10 - 100*dgy(1) - 10*dgy(2)
dgy(4) = MOD(cdate%yy,10)

ch9=dg(dgd(1))//dg(dgd(2))//ch5_month(cdate%mm)// &
  dg(dgy(3))//dg(dgy(4))

RETURN
END FUNCTION date2sqldate


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


!==========================================================================
FUNCTION sqldate2date (ch9) RESULT (cdate)
!
! function to convert a date as a ch*9 string with the format required in a 
! SQL*Plus query (DD-MMM-YY) to a cdate 
!
IMPLICIT NONE
!
TYPE (date) :: cdate
CHARACTER(LEN=9), INTENT(IN) :: ch9
CHARACTER(LEN=5) :: ch5
INTEGER :: i,year_tmp
LOGICAL :: valid_month

CHARACTER(LEN=5), PARAMETER :: ch5_month(12) = &
  (/'-JAN-','-FEB-','-MAR-','-APR-','-MAY-','-JUN-', &
    '-JUL-','-AUG-','-SEP-','-OCT-','-NOV-','-DEC-'/)
!
READ (ch9,'(i2.2,a5,i2.2)',ERR=901,END=901) cdate%dd,ch5,year_tmp
valid_month = .false.
DO i=1,12
   IF (ch5 == ch5_month(i)) THEN
      cdate%mm = i
      valid_month = .true.
      EXIT
   END IF
END DO
IF (.NOT.valid_month) THEN
   WRITE(*,*)"I'm sorry, I cannot interpret date ",ch9
   RETURN
END IF
IF (year_tmp > 80) THEN
   cdate%yy = year_tmp + 1900
ELSE
   cdate%yy = year_tmp + 2000
END IF

RETURN
901 WRITE(*,*)"I'm sorry, I cannot interpret date ",ch9
RETURN
END FUNCTION sqldate2date


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

END MODULE date_handler
 

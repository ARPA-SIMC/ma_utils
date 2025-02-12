!-----------------------------------------------------------------------------------------------------
!  This is a CHIMERE package for dates / calendar management
!  All dates must be in 10-digit YYYYMMDDHH format
!-----------------------------------------------------------------------------------------------------
! checkdate(idate):         Checks the format of a date given in 10-digit YYYYMMDDHH format
! dayofweek(idate,iday):    Returns the day (1:7) of week given a date in YYYYMMDDHH format
! ddate(idate,iy,im,id,ih): Returns Year, Month, Day and Hour of a YYYYMMDDHH date
! rdate(iy,im,id,ih,idate): Returns a YYYYMMDDHH date from Year, Month, Day, Hour
! idaytype(id):             Returns the type of week day (1=Week; 2=Saturday; 3=Sunday)
! interdat(idate,jdate):    Returns the number of hours between two dates idate and jdate
! reldat(idate,n,jdate):    Returns the date (jdate) n hours after (or before) idate
! ieom(im,iy):              Returns the end of the month given month MM and year YYYY
! wdate(iy,im,id,ih,cdate): Returns a WRF-like YYYY-MM-DD_HH:00:00 date from Year, Month, Day, Hour
! date_mozart(sday):        first date in 0-01-01 with 365 days a year
! date_cdo(sday):           sday format: YYYYMMDD.HHH (HHH being hours/24) outputs YYYYMMDDHH
!-----------------------------------------------------------------------------------------------------

subroutine checkdate(idate)

!  Checks the format of a date given in 10-digit YYYYMMDDHH format
!  INPUT:  IDATE   : The date to check
!  OUTPUT: ---
!  Date must be positive

implicit none
integer,intent(in) :: idate
integer :: iy,im,id,ih

  call ddate(idate,iy,im,id,ih)

  if(id.lt.0.or.idate.lt.0) then
      print *,'*** ERROR in CHECKDATE: DATE NEGATIVE:', id, idate
      call exit1('Bye')
  endif

  if(id.lt.1.or.id.gt.31) then
      print *,'*** ERROR in CHECKDATE on DAY:' , id, idate
      call exit1('Bye')
  endif

  if(im.lt.1.or.im.gt.12) then
      print *,'*** ERROR in CHECKDATE on MONTH:', im, idate
      call exit1('Bye')
  endif

  if(ih.lt.0.or.ih.gt.23) then
    print *,'*** ERROR in CHECKDATE on HOUR:' , ih, idate
    call exit1('Bye')
  endif

end subroutine checkdate

!-----------------------------------------------------------------------------

subroutine dayofweek(idate,iday)

!  Returns the day of week given a date in YYYYMMDDHH format
!  INPUT : IDATE    : The input date
!  OUTPUT: IDAY     : The day of week (1 [Monday] to 7 [Sunday])
!  26/12/1999 was a Sunday

implicit none
integer,intent(in)  :: idate
integer,intent(out) :: iday
integer :: idref,jtmp,jdate,inter
integer :: interdat

  call checkdate(idate)
  idref = 1999122600

!  Sets hour to 00
  jtmp = idate/100
  jdate = jtmp*100

!  Day of week
  inter = interdat(idref,jdate)/24
  iday = mod(inter,7)
  if(iday.le.0) iday = iday + 7

end subroutine dayofweek

!-----------------------------------------------------------------------------

subroutine ddate(idate,iy,im,id,ih)

!  Returns Year, Month, Day and Hour of a 10-digit date written in format YYYYMMDDHH
!  INPUT : IDATE   : YYYYMMDDHH date integer
!  OUTPUT: IY	   : Year
!	   IM	   : Month
!	   ID	   : Day
!	   IH	   : Hour

implicit none
integer,intent(in)  :: idate
integer,intent(out) :: iy,im,id,ih
integer :: iq

  iy = idate/1000000
  iq = idate - iy*1000000
  im = iq/10000
  iq = iq - im*10000
  id = iq/100
  ih = iq - id*100
  
end subroutine ddate

!-----------------------------------------------------------------------------

subroutine rdate(iy,im,id,ih,idate)

!  Returns a YYYYMMDDHH date from Year, Month, Day, Hour information
!  INPUT : IY	   : Year
!	   IM	   : Month
!	   ID	   : Day
!	   IH	   : Hour
!  OUTPUT: IDATE   : YYYYMMDDHH date integer

implicit none
integer,intent(in)  :: iy,im,id,ih
integer,intent(out) :: idate

  idate = ih + id*100 + im*10000 + iy*1000000

end subroutine rdate

!-----------------------------------------------------------------------------
function idaytype(id)

!  Returns the type of week day (1=Week; 2=Saturday; 3=Sunday)
!  *** WARNING: This is for France ! There are a couple of days-off
!  to correct for othe countries (ex: 14 Jul is considered as a Sunday)
!  INPUT : ID	     : The date in YYYYMMDDHH format
!  OUTPUT: IDAYTYPE  : Integer 1..5 [Week day], 6 [Saturday], or 7 [Sunday]

implicit none
integer,intent(in)  :: id
integer :: idaytype
integer,dimension(7),parameter :: itype=(/1,2,3,4,5,6,7/)
integer :: myear,mmonth,mday,mhour,itday

  call ddate(id,myear,mmonth,mday,mhour)
  call dayofweek(id,itday)
  idaytype = itype(itday)

END function idaytype
!-----------------------------------------------------------------------------

function interdat(idate,jdate)

!  Returns the number of hours between two dates idate and jdate
!  given in 10-digit format YYYYMMDDHH.
!  IDATE can be posterior OR anterior to JDATE. In the former case,
!  the result will be negative
!  INPUT :  IDATE    First date
!	    JDATE    Second date
!  OUTPUT:  INTERDAT The time interval between the two, in hours

implicit none
integer,intent(in) :: idate
integer,intent(in) :: jdate
integer :: interdat
integer :: ida0,ida1,isign,iy0,im0,id0,ih0,iy1,im1,id1,ih1,ndays,i,imo,iye,ieo
integer :: ieom

  interdat = -9999999

!  Dates check

  call checkdate(idate)
  call checkdate(jdate)

  if(jdate.gt.idate) then
     ida0 = idate
     ida1 = jdate
     isign = 1
  else
     ida0 = jdate
     ida1 = idate
     isign = -1
  endif

  call ddate(ida0,iy0,im0,id0,ih0)
  call ddate(ida1,iy1,im1,id1,ih1)

  imo = im0
  iye = iy0
  ndays = -id0
  do i=1,10000000
     if((imo.eq.im1).and.(iye.eq.iy1)) then
        ndays = ndays + id1
        interdat = isign*(ndays*24 - ih0 + ih1)
        goto 1001
     endif
     ieo = ieom(imo,iye)
     ndays = ndays + ieo
     imo = imo + 1
     if(imo.gt.12) then
        imo = 1
        iye = iye + 1
     endif
  enddo
  print *,'*** ERROR in INTERDAT: ',idate,jdate
1001 continue

END function interdat

!-----------------------------------------------------------------------------

subroutine reldat(idate,n,jdate)

!  Returns the date (jdate) n hours after (or before) idate
!  (according to the sign of n)
!  INPUT : IDATE     : 10-digit YYYYMMDDHH date
!	   N	     : Number of hours
!  OUTPUT: JDATE     : 10-digit YYYYMMDDHH date

implicit none
integer,intent(in)  :: idate
integer,intent(in)  :: n
integer,intent(out) :: jdate
integer :: iy,im,id,ih,ida,imo,iye,iho,nh
integer :: ieom

!  Date check
  call checkdate(idate)

!  Gives full information

  call ddate(idate,iy,im,id,ih)

  ida = id
  imo = im
  iye = iy
  iho = ih

  if(n.ge.0) then
     do nh=1,n
        iho = iho + 1
        if(iho.gt.23) then
           iho = 0
           ida = ida + 1
           if(ida.gt.ieom(imo,iye)) then
              ida = 1
              imo = imo + 1
              if(imo.gt.12) then
                 imo = 1
                 iye = iye + 1
              endif
           endif
        endif
     enddo
  else
     do nh=1,-n
        iho = iho - 1
        if(iho.lt.0) then
           iho = 23
           ida = ida - 1
           if(ida.lt.1) then
              imo = imo - 1
              if(imo.lt.1) then
                 imo = 12
                 iye = iye - 1
              endif
              ida = ieom(imo,iye)
           endif
        endif
     enddo
  endif

  call rdate(iye,imo,ida,iho,jdate)

  return
  
end subroutine reldat
!-----------------------------------------------------------------------------

function ieom(im,iy)

!  Returns the end of the month given month number (2 digits) and year (4 digits)

implicit none
integer,intent(in)  :: im
integer,intent(in)  :: iy
integer :: ieom

  ieom = 31
  if(im.eq.4.or.im.eq.6.or.im.eq.9.or.im.eq.11) ieom = 30
  if(im.eq.2) then
     if(mod(iy,4).ne.0) then
        ieom = 28
        return
     else
        if(mod(iy,100).ne.0) then
           ieom = 29
           return
        else
           if(mod(iy,400).eq.0) then
              ieom = 29
              return
           else
              ieom = 28
              return
           endif
        endif
     endif
  endif

end function ieom

!-----------------------------------------------------------------------------

subroutine wdate(iy,im,id,ih,cdate)

!  Returns a WRF-like YYYY-MM-DD_HH:00:00 date from Year, Month, Day, Hour information
!  INPUT : IY	   : Year
!	   IM	   : Month
!	   ID	   : Day
!	   IH	   : Hour
!  OUTPUT: CDATE   : YYYY-MM-DD_HH:00:00 date char

implicit none
integer,intent(in)  :: iy,im,id,ih
character(2)  :: cm,cd,ch
character(19) :: cdate

if (im .le.9 ) then
   write(cm,'(1a,I1)')'0',im
else
   write(cm,'(I2)') im
endif
if (id .le.9 ) then
   write(cd,'(1a,I1)')'0',id
else
   write(cd,'(I2)') id
endif
if (ih .le.9 ) then
   write(ch,'(1a,I1)')'0',ih
else
   write(ch,'(I2)') ih
endif

write(cdate,'(I4,1a,2a,1a,2a,1a,2a,6a)')iy,'-',cm,'-',cd,'_',ch,':00:00'

end subroutine wdate

!-----------------------------------------------------------------------------

function date_mozart(sday)

! first date in 0-01-01 with 365 days a year

double precision,intent(in) :: sday
double precision	 :: xidtmp
integer 		 :: date_mozart
integer 		 :: iy,im,id,ih
integer,dimension(0:12)  :: dcumul
integer 		 :: idtmp

iy=int(sday/365.)
xidtmp=(sday/365.-iy)*365
idtmp=int(xidtmp)+1
ih=nint((xidtmp-int(xidtmp))*24)

if (ih.eq.24) then
   ih=0
   idtmp=idtmp+1
endif

dcumul(:)=0

do i=1,12
   dcumul(i)=dcumul(i-1)+ieom(i,iy)
   if ((idtmp > dcumul(i-1)).and.(idtmp <= dcumul(i))) then
      im=i
      id=idtmp-dcumul(i-1)
   endif
enddo

date_mozart=iy*1000000+im*10000+id*100+ih

return

end function date_mozart
 
!-----------------------------------------------------------------------------

function date_cdo(sday)

! Input: sday format: YYYYMMDD.HHH (HHH being hours/24)
! Output: date in format YYYYMMDDHH

double precision, intent(in) :: sday
double precision, parameter :: d6=1.d6
double precision, parameter :: d4=1.d4
double precision, parameter :: d2=1.d2
integer :: date_cdo,iy,im,id,ih

iy=int( sday/d4)
im=int((sday - float(iy)*d4)/d2)
id=int( sday - float(iy)*d4 - float(im)*d2)
ih=int((sday - float(iy)*d4 - float(im)*d2 - float(id)) * 24.d0 + 0.5d0)
date_cdo=int(iy*d6+im*d4+id*d2+ih)

return

end function date_cdo
 
!-----------------------------------------------------------------------------
!  Returns the minutes and seconds nbsec seconds after (or before) imin, isec

subroutine relminsec(imin,isec,nbsec)
  implicit none
  integer,intent(inout) :: imin, isec
  integer               :: nbsec

  isec = mod(isec + nbsec,60)
  imin = mod(imin + nbsec / 60,60)
end subroutine relminsec

!-----------------------------------------------------------------------------
subroutine hourinmonth(icurdate,w1,w2,im1,im2)

! Returns the w1 and w2 weights to apply to two monthly fields for a specific time
! itimeslot: the time increment during the model time integration
! icurdate: the current run date in hours
! if j<15, we interpolate monthly data between (m-1) and (m)
! if j>=15, we interpolate monthly data between (m) and (m+1)

use chimere_params, only : iprec
implicit none

integer :: interdat
integer :: icurdate, iy,im,id,ih
integer :: iy1, im1, id1, ih1, idate1, nh1
integer :: iy2, im2, id2, ih2, idate2, nh2
real(kind=iprec) :: w1, w2

call ddate(icurdate,iy,im,id,ih)

! By defaut we consider that the monthly databases have values representative 
! of the 15th day of each month at 00:00 UTC.
ih1=0
ih2=0
id1=15
id2=15

if(id.lt.15)then
   if(im.gt.1)then
      iy1=iy
      im1=im-1
   else
      iy1=iy-1
      im1=12
   endif  
   iy2=iy
   im2=im
else
   if(im.lt.12)then
      iy2=iy
      im2=im+1
   else
      iy2=iy+1
      im2=01
   endif  
   iy1=iy
   im1=im
endif

call rdate(iy1,im1,id1,ih1,idate1)
call rdate(iy2,im2,id2,ih2,idate2)
nh1=interdat(idate1,icurdate)
nh2=interdat(icurdate,idate2)
w1=real(nh2)/real(nh1+nh2)
w2=real(nh1)/real(nh1+nh2)

end subroutine hourinmonth
!-----------------------------------------------------------------------------







!-*-f90-*-
module calendar
  !  This is a package for dates / calendar management                    
  !  All utility routines are included                                    

  implicit none

contains
  !*******************************************************************************************
  subroutine checkdate(idate)
    !  ### Utility routine                                                  
    !  Checks the format of a date given in 10-digit YYYYMMDDHH format      
    !  INPUT:  IDATE   : The date to check                                  
    !  OUTPUT: ---                                                          

    implicit none
    !***************************
    ! subroutine arguments
    integer,intent(in) :: idate

    ! local variables
    integer :: iy,im,id,ih
    !***************************

    call ddate(idate,iy,im,id,ih) 

    !  Date must be positive (no pollution before!)                         

    if(id.lt.0) print *,'*** ERROR in CHECKDATE: DATE NEGATIVE' 

    !  Other Checks                                                         

    if(im.lt.1.or.im.gt.12) print *,'*** ERROR in CHECKDATE on MONTH' 
    if(id.lt.1.or.id.gt.31) print *,'*** ERROR in CHECKDATE on DAY' 
    if(ih.lt.0.or.ih.gt.23) print *,'*** ERROR in CHECKDATE on HOUR' 

  END subroutine checkdate
  !*******************************************************************************************



  !*******************************************************************************************
  subroutine dayofweek(idate,iday) 
    !  ### Utility routine                                                  
    !  Returns the day of week given a date in YYYYMMDDHH format            
    !  INPUT : IDATE    : The input date                                    
    !  OUTPUT: IDAY     : The day of week (1 [Monday] to 7 [Sunday])        

    implicit none
    !***************************
    ! subroutine arguments
    integer,intent(in)  :: idate
    integer,intent(out) :: iday

    ! local variables
    integer :: idref,jtmp,jdate,inter
    !***************************

    !  Date check                                                           

    call checkdate(idate) 

    !  26/12/1999 was a Sunday, remember the Paris storm !!!                

    idref = 1999122600 

    !  Sets hour to 00                                                      

    jtmp = idate/100 
    jdate = jtmp*100 

    !  Day of week                                                          

    inter = interdat(idref,jdate)/24 
    iday = mod(inter,7) 
    if(iday.le.0) iday = iday + 7 

  END subroutine dayofweek
  !*******************************************************************************************



  !*******************************************************************************************
  subroutine ddate(idate,iy,im,id,ih) 
    !  ### Utility routine                                                  
    !  Returns Year, Month, Day and Hour of a 10-digit date written in forma
    !  YYYYMMDDHH                                                           
    !  INPUT : IDATE   : YYYYMMDDHH date integer                            
    !  OUTPUT: IY      : Year                                               
    !          IM      : Month                                              
    !          ID      : Day                                                
    !          IH      : Hour                                               

    implicit none
    !***************************
    ! subroutine arguments
    integer,intent(in)  :: idate
    integer,intent(out) :: iy,im,id,ih

    ! local variables
    integer :: iq
    !***************************

    iy = idate/1000000 
    iq = idate - iy*1000000 
    im = iq/10000 
    iq = iq - im*10000 
    id = iq/100 
    ih = iq - id*100 

  END subroutine ddate
  !*******************************************************************************************



  !*******************************************************************************************
  subroutine rdate(iy,im,id,ih,idate) 
    !  ### Utility routine                                                  
    !  Returns a YYYYMMDDHH date from Year, Month, Day, Hour information    
    !  INPUT : IY      : Year                                               
    !          IM      : Month                                              
    !          ID      : Day                                                
    !          IH      : Hour                                               
    !  OUTPUT: IDATE   : YYYYMMDDHH date integer                            

    implicit none
    !***************************
    ! subroutine arguments
    integer,intent(in)  :: iy,im,id,ih
    integer,intent(out) :: idate
    !***************************


    idate = ih + id*100 + im*10000 + iy*1000000 

  END subroutine rdate
  !*******************************************************************************************



  !*******************************************************************************************
  function idaytype(id) 
    !  ### Utility routine                                                  
    !  Returns the type of week day (1=Week; 2=Saturday; 3=Sunday)          
    !  *** WARNING: This is for France ! There are a couple of days-off     
    !  to correct for othe countries (ex: 14 Jul is considered as a Sunday) 
    !  INPUT : ID        : The date in YYYYMMDDHH format                    
    !  OUTPUT: IDAYTYPE  : Integer 1 [Week day], 2 [Saturday], or 3 [Sunday]

    implicit none
    !***************************
    ! function arguments
    integer,intent(in)  :: id
    integer :: idaytype

    ! parameters
    integer,dimension(7),parameter :: itype=(/1,1,1,1,1,2,3/)
    ! local variables
    integer :: myear,mmonth,mday,mhour,itday
    !***************************

    call ddate(id,myear,mmonth,mday,mhour) 
    call dayofweek(id,itday) 

    !  Special cases                                                        

    if(mday.eq.01.and.mmonth.eq.01) itday = 7 
    if(mday.eq.14.and.mmonth.eq.07) itday = 7 
    if(mday.eq.25.and.mmonth.eq.12) itday = 7 
    if(mday.eq.01.and.mmonth.eq.05) itday = 7 
    if(mday.eq.08.and.mmonth.eq.05) itday = 7 
    if(mday.eq.15.and.mmonth.eq.08) itday = 7 
    if(mday.eq.01.and.mmonth.eq.11) itday = 7 
    if(mday.eq.11.and.mmonth.eq.11) itday = 7 

    !  Ascension                                                            

    if(myear.eq.1998.and.mday.eq.21.and.mmonth.eq.05) itday = 7 
    if(myear.eq.1998.and.mday.eq.22.and.mmonth.eq.05) itday = 6 
    if(myear.eq.1998.and.mday.eq.23.and.mmonth.eq.05) itday = 7 

    if(myear.eq.1999.and.mday.eq.13.and.mmonth.eq.05) itday = 7 
    if(myear.eq.1999.and.mday.eq.14.and.mmonth.eq.05) itday = 6 
    if(myear.eq.1999.and.mday.eq.15.and.mmonth.eq.05) itday = 7 

    if(myear.eq.2000.and.mday.eq.01.and.mmonth.eq.06) itday = 7 
    if(myear.eq.2000.and.mday.eq.02.and.mmonth.eq.06) itday = 6 
    if(myear.eq.2000.and.mday.eq.03.and.mmonth.eq.06) itday = 7 

    if(myear.eq.2001.and.mday.eq.24.and.mmonth.eq.05) itday = 7 
    if(myear.eq.2001.and.mday.eq.25.and.mmonth.eq.05) itday = 6 
    if(myear.eq.2001.and.mday.eq.26.and.mmonth.eq.05) itday = 7 

    if(myear.eq.2002.and.mday.eq.09.and.mmonth.eq.05) itday = 7 
    if(myear.eq.2002.and.mday.eq.10.and.mmonth.eq.05) itday = 6 
    if(myear.eq.2002.and.mday.eq.11.and.mmonth.eq.05) itday = 7 

    !  Pentecote                                                            

    if(myear.eq.1998.and.mday.eq.01.and.mmonth.eq.06) itday = 7 
    if(myear.eq.1999.and.mday.eq.24.and.mmonth.eq.05) itday = 7 
    if(myear.eq.2000.and.mday.eq.12.and.mmonth.eq.06) itday = 7 
    if(myear.eq.2001.and.mday.eq.04.and.mmonth.eq.06) itday = 7 
    if(myear.eq.2002.and.mday.eq.20.and.mmonth.eq.05) itday = 7 

    !  Easter                                                               

    if(myear.eq.1998.and.mday.eq.13.and.mmonth.eq.04) itday = 7 
    if(myear.eq.1999.and.mday.eq.05.and.mmonth.eq.04) itday = 7 
    if(myear.eq.2000.and.mday.eq.24.and.mmonth.eq.04) itday = 7 
    if(myear.eq.2001.and.mday.eq.16.and.mmonth.eq.04) itday = 7 
    if(myear.eq.2002.and.mday.eq.01.and.mmonth.eq.04) itday = 7 

    idaytype = itype(itday) 


  END function idaytype
  !*******************************************************************************************



  !*******************************************************************************************
  function interdat(idate,jdate) 
    !  Returns the number of hours between two dates idate and jdate        
    !  given in 10-digit format YYYYMMDDHH.                                 
    !  IDATE can be posterior OR anterior to JDATE. In the former case,     
    !  the result will be negative                                          
    !  INPUT :  IDATE    First date                                         
    !           JDATE    Second date                                        
    !  OUTPUT:  INTERDAT The time interval between the two, in hours        

    implicit none
    !***************************
    ! function arguments
    integer,intent(in) :: idate
    integer,intent(in) :: jdate
    integer :: interdat

    ! local variables
    integer :: ida0,ida1,isign,iy0,im0,id0,ih0,iy1,im1,id1,ih1,ndays,i,imo,iye,ieo
    !***************************

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
          go to 1001 
       endif
       ieo = ieom(imo,iye) 
       ndays = ndays + ieo 
       imo = imo + 1 
       if(imo.gt.12) then 
          imo = 1 
          iye = iye + 1 
       endif
    enddo
    print *,'*** ERROR in INTERDAT: Please verify dates given' 
1001 continue 

  END function interdat
  !*******************************************************************************************




  !*******************************************************************************************
  subroutine reldat(idate,n,jdate) 
    !  ### Utility routine                                                  
    !  Returns the date (jdate) n hours after (or before) idate             
    !  (according to the sign of n)                                         
    !  INPUT : IDATE     : 10-digit YYYYMMDDHH date                         
    !          N         : Number of hours                                  
    !  OUTPUT: JDATE     : 10-digit YYYYMMDDHH date                         


    implicit none
    !***************************
    ! subroutine arguments
    integer,intent(in)  :: idate
    integer,intent(in)  :: n
    integer,intent(out) :: jdate

    ! local variables
    integer :: iy,im,id,ih,ida,imo,iye,iho,nh
    !***************************

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
  END subroutine reldat
  !*******************************************************************************************



  !*******************************************************************************************
  function ieom(im,iy) 

    !  Returns the end of the month given month number and year (in 4 digits

    implicit none
    !***************************
    ! function arguments
    integer,intent(in)  :: im
    integer,intent(in)  :: iy
    integer :: ieom

    !***************************

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

  END function ieom

end module calendar

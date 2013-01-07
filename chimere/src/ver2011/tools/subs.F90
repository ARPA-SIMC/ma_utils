!*******************************************************************************************
subroutine nc_err(status,line_nb,string2)
  use netcdf
  implicit none
  !***************************************
  ! subroutine arguments
  integer, intent(in) :: status
  integer :: line_nb
  character*(*) :: string2
  !***************************************

  if (status /= nf90_noerr) then
     line_nb=line_nb-1 ! Error occured at the preceding line
!#ifdef G95
!     line_nb=line_nb-1 ! bug in the __LINE__ macro under g95 ???
!#endif     
     print *, trim(string2),': netCDF error on source line',line_nb,trim(nf90_strerror(status))
     stop 'Stopped'
  end if
end subroutine nc_err


!*******************************************************************************************
function mm5date2numeric(date)
  !***************************************
  ! subroutine arguments
  character(len=*) :: date
  integer :: mm5date2numeric
  ! local variables
  integer :: iyear,imonth,iday,ihour
  !***************************************

  read(date(1:4),'(i4)')  iyear
  read(date(6:7),'(i2)')  imonth
  read(date(9:10),'(i2)') iday
  read(date(12:13),'(i2)')ihour
  mm5date2numeric = ihour + 100*iday + 10000*imonth + 1000000*iyear
end function mm5date2numeric


!*******************************************************************************************
function numeric2mm5date(idate)
  implicit none
  !***************************************
  ! subroutine arguments
  integer :: idate
  character(len=*) :: numeric2mm5date
  ! local variables
  integer :: iyear,imonth,iday,ihour,id
  !***************************************

  id=idate
  iyear=id/1000000
  id=id-iyear*1000000
  imonth=id/10000
  id=id-imonth*10000
  iday=id/100
  id=id-iday*100
  ihour=id

  numeric2mm5date='0000-00-00_00:00:00'
  write(unit= numeric2mm5date(1:4),  fmt='(i4.4)') iyear
  write(unit= numeric2mm5date(6:7),  fmt='(i2.2)') imonth
  write(unit= numeric2mm5date(9:10), fmt='(i2.2)') iday
  write(unit= numeric2mm5date(12:13),fmt='(i2.2)') ihour

end function numeric2mm5date


!*******************************************************************************************
subroutine get_system(usrname,hname,systime,cwd)


!#ifdef IFORT
!  use ifport
!  use ifposix
!#endif
  implicit none
  !***************************************
  ! subroutine arguments
  character(len=*) :: usrname
  character(len=*) :: hname
  character(len=*) :: systime
  character(len=*) :: cwd
  ! local variables
  integer :: i,j,nlen,status
  integer :: iutsname,ipw,myuid,pxerr,pxlen
  logical :: ziend
  ! external functions
#if defined(PGI)
  integer,external :: getcwd
  integer,external :: hostnm
#endif

  !***************************************

#if defined(IFORT)

  ! Get user name
  call pxfgetuid(myuid,pxerr)
  call pxfstructcreate('passwd',ipw,pxerr)
  call pxfgetpwuid(myuid,ipw,pxerr)
  call pxfstrget(ipw,'pw_name',usrname,pxlen,pxerr)
  nlen=len(usrname)
  do i=1,nlen
     ziend=((ichar(usrname(i:i))<ichar('A')).or.(ichar(usrname(i:i))>ichar('z')))
     if (ziend) then
        do j=i,nlen
           usrname(j:j)=' '
        end do
        exit
     end if
  end do
  ! Get current directory
  call pxfgetcwd(cwd,pxlen,pxerr)
  ! Get host name
  call pxfstructcreate('utsname',iutsname,pxerr)
  call pxfuname(iutsname,pxerr)
  call pxfstrget(iutsname,'nodename',hname,pxlen,pxerr)
  ! Get time
  systime=ctime(time())

#elif defined(G95)

  ! Get user name
  call getlog(usrname)
  nlen=len(usrname)
  do i=1,nlen
     ziend=((ichar(usrname(i:i))<ichar('A')).or.(ichar(usrname(i:i))>ichar('z')))
     if (ziend) then
        do j=i,nlen
           usrname(j:j)=' '
        end do
        exit
     end if
  end do
  ! Get current directory
  status=getcwd(cwd)
  ! Get host name
  status=hostnm(hname)
  ! Get time
  !systime=ctime(time())
  call fdate(systime)

#elif defined(PGI)

  ! Get user name
  call getenv('USER',usrname)
  nlen=len(usrname)
  do i=1,nlen
     ziend=((ichar(usrname(i:i))<ichar('A')).or.(ichar(usrname(i:i))>ichar('z')))
     if (ziend) then
        do j=i,nlen
           usrname(j:j)=' '
        end do
        exit
     end if
  end do
  ! Get current directory
  status=getcwd(cwd)
  ! Get host name
  status=hostnm(hname)
  ! Get time
  !systime=ctime(time())
  call fdate(systime)

#else

  usrname='unknown'
  hname='unknown'
  systime='unknown'
  cwd='unknown'

#endif

end subroutine get_system




!*******************************************************************************************
subroutine update_history(history,cwd,outfile,systime,usrname,hname,infile)
  implicit none
  !***************************************
  ! subroutine arguments
  character(len=*) :: history
  character(len=*) :: cwd
  character(len=*) :: outfile
  character(len=*) :: systime
  character(len=*) :: usrname
  character(len=*) :: hname
  character(len=*) :: infile
  !***************************************
  

#if (defined(G95) + defined(IFORT) + defined(PGI))

  history='File '//cwd(1:len_trim(cwd)) &
       //'/'//outfile(1:len_trim(outfile))// &
       ' was generated on '//systime(1:len_trim(systime))// &
       ' by '//usrname(1:len_trim(usrname))//' on '//hname(1:len_trim(hname))// &
       ' from input file '//infile(1:len_trim(infile)) //'  '// char(10) // &
       history(1:len_trim(history)-1) //'  '
#else

  history='unknown'

#endif

end subroutine update_history

  !****************************************************
  function notzero(x,limit)
    real x,limit
    real notzero
    
    notzero=x
    if((x.ge.0.).and.(x<limit)) then
       notzero=limit
    else if ((x.lt.0.).and.(x>-limit)) then
       notzero=-limit
    end if
  end function notzero

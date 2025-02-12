!--------------------------------------------------------------------------------------

! prints a message and exits with error code 1

subroutine exit1(string)
  character*(*) :: string
  print *, string
  stop 1
end subroutine exit1

!--------------------------------------------------------------------------------------

subroutine nc_err(status,line_nb,string2)

  use netcdf
  implicit none
  integer, intent(in) :: status
  integer, intent(in) :: line_nb
  character*(*) :: string2
  integer :: lnb

  if (status /= nf90_noerr) then
     lnb = line_nb
     lnb=lnb-1 ! Error occured at the preceding line
#ifdef G95
     lnb=lnb-1 ! bug in the __LINE__ macro under g95 ???
#endif
     print *, trim(string2),': netCDF error on source line',lnb,trim(nf90_strerror(status))
     print*, 'Error code is ', status
     stop 1
  endif
end subroutine nc_err


!--------------------------------------------------------------------------------------

function mm5date2numeric(date)

  implicit none
  character(len=*) :: date
  integer :: mm5date2numeric
  integer :: iyear,imonth,iday,ihour

  read(date(1:4),'(i4)')  iyear
  read(date(6:7),'(i2)')  imonth
  read(date(9:10),'(i2)') iday
  read(date(12:13),'(i2)')ihour
  mm5date2numeric = ihour + 100*iday + 10000*imonth + 1000000*iyear
end function mm5date2numeric


!--------------------------------------------------------------------------------------

function numeric2mm5date(idate)

  implicit none
  integer :: idate
  character(len=*) :: numeric2mm5date
  integer :: iyear,imonth,iday,ihour,id

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

!--------------------------------------------------------------------------------------

subroutine get_system(usrname,hname,systime,cwd)

#ifdef IFORT
  use ifport
  use ifposix
#endif

  implicit none
  character(len=*) :: usrname
  character(len=*) :: hname
  character(len=*) :: systime
  character(len=*) :: cwd
  integer :: i,j,nlen
  logical :: ziend

#ifdef IFORT
  integer :: iutsname,ipw,myuid,pxerr,pxlen
#endif
  ! external functions
#if defined(PGI)
  integer :: status
  integer,external :: getcwd
  integer,external :: hostnm
#endif
#if (defined(G95)+defined(GFORTRAN))
  integer :: status
#endif

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

#elif (defined(G95)+defined(GFORTRAN))

  ! Get user name
  call getlog(usrname)
  nlen=len(usrname)
  do i=1,nlen
     ziend=((ichar(usrname(i:i))<ichar('A')).or.(ichar(usrname(i:i))>ichar('z')))
     if (ziend) then
        do j=i,nlen
           usrname(j:j)=' '
        enddo
        exit
     endif
  enddo
  ! Get current directory
  status=getcwd(cwd)
  call nc_err(status,__LINE__,'__FILE__')
  ! Get host name
  status=hostnm(hname)
  call nc_err(status,__LINE__,'__FILE__')
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
        enddo
        exit
     endif
  enddo
  ! Get current directory
  status=getcwd(cwd)
  call nc_err(status,__LINE__,'__FILE__')
  ! Get host name
  status=hostnm(hname)
  call nc_err(status,__LINE__,'__FILE__')
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

!--------------------------------------------------------------------------------------
subroutine update_history(history,cwd,outfile,systime,usrname,hname,infile)
implicit none

  character(len=*) :: history
  character(len=*) :: cwd
  character(len=*) :: outfile
  character(len=*) :: systime
  character(len=*) :: usrname
  character(len=*) :: hname
  character(len=*) :: infile

#if (defined(G95) + defined(IFORT) + defined(PGI) + defined(GFORTRAN))

  history='File '//cwd(1:len_trim(cwd)) &
       //'/'//outfile(1:len_trim(outfile))// &
       ' was generated on '//systime(1:len_trim(systime))// &
       ' by '//usrname(1:len_trim(usrname))//' on '//hname(1:len_trim(hname))// &
       ' from input file '//infile(1:len_trim(infile)) //'  '// char(10) // &
       history(1:max(len_trim(history)-1,1)) //'  '
#else

  history='unknown'

#endif

end subroutine update_history

!-------------------------------------------------------------
function get_out_file(fn, di, nframes, idateend, simlab)
  ! Returns output file name when splitting with nframes
  ! Supposes standard name, e.g.: out.di_de_lab.nc
  ! File end date/time is actual last date/time in the file
  ! If simlab is not '' (empty) => it is used as a new label
  !   otherwise the label found in the filename is used

  implicit none

  interface
    subroutine strsplit(str, c, N, left, right)
      character(len=1), intent(in)       :: c
      character(len=*), intent(in)       :: str
      integer, intent(in)                :: N
      character(len=*),intent(out),optional :: left
      character(len=*),intent(out),optional :: right
    end subroutine strsplit
  end interface

  character(len=*) :: get_out_file
  character(len=*), intent(in) :: fn
  integer, intent(in) :: di, nframes, idateend
  character(len=*), intent(in) :: simlab
  integer :: de
  character(len=16) :: buf1, buf2
  character(len=32) :: prefix, lab
  character(len=512) :: path, left, right, basename

  ! Get file path, prefix and simulation label
  call strsplit(trim(fn), '/', -1, left=path, right=basename)
  call strsplit(trim(basename), '.', 1, left=prefix)

  call strsplit(trim(basename), '_', -1, left, right)
  call strsplit(trim(right), '.', -1, left=lab)

  call reldat(di, nframes-1, de)
  ! Cannot go beyond end date

  if (de > idateend) de = idateend

  write(buf1,*) di
  write(buf2,*) de
  write(buf2,*) idateend

  if (len_trim(simlab) > 0) lab = simlab

  get_out_file = trim(path) // '/' // trim(prefix) // '.' // trim(adjustl(buf1))//'_'//trim(adjustl(buf2))// '_' //trim(lab)// '.nc'

end function get_out_file

!-------------------------------------------------------------
subroutine strsplit(str, c, N, left, right)
  ! Splits the string str on Nth occurence of the character c
  ! N counts from start if N>0 and from end if N<0
  ! left and right are optional

  ! character to split the string
  character(len=1), intent(in)       :: c
  ! input string
  character(len=*), intent(in)       :: str
  integer, intent(in)                :: N
  character(len=*),intent(out),optional :: left
  character(len=*),intent(out),optional :: right
  integer :: i,cnt
  character(len=4096) :: buf
  logical :: back

  if (N < 0) then
    back = .true.
  else if (N > 0) then
    back = .false.
  else
    print *, 'strsplit(): "N" must not equal 0!'
    stop 1
  end if

  cnt = 0
  buf = str
  do while ( len_trim(buf) > 0 )
     cnt = cnt + 1
     i = index(trim(buf), c, back=back)
     if (i == 0 .or. abs(N) == cnt) exit
     if (N < 0) then
        buf = buf(:i-1)
     else
        buf = buf(i+1:len_trim(buf))
     end if
  end do

  if (present(left)) left = str(:i-1)
  if (present(right)) right = str(i+1:len_trim(str))

end subroutine strsplit






!--------------------------------------------------------------------------------------
  function notzero(x,limit)
    use chimere_params, only: iprec
    real(kind=iprec) x,limit
    real(kind=iprec) notzero

    notzero=x
    if((x.ge.0.).and.(x<limit)) then
       notzero=limit
    else if ((x.lt.0.).and.(x>-limit)) then
       notzero=-limit
    end if
  end function notzero
!--------------------------------------------------------------------------------------

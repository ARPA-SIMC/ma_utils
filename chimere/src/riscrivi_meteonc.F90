program riscrivi_meteonc

  ! reads  CHIMERE netCDF  METEO files and generates  new  file with different time
  ! for V2007C, chimere2008 
  ! version 1.3 16/01/2009
  use netcdf
  use date_handler

  implicit none

#define NCERR(lnum) if(ncstat/=NF90_NOERR) call nc_err(ncstat,lnum,'mkdiff.f90')

  type :: varmeta
     real    :: min
     real    :: max
     integer :: id3
     integer :: ndims
     logical :: is3d
     logical :: isreal
     character(len=16) :: varname
  end type varmeta
  type(varmeta),allocatable,dimension(:) :: varids1

  ! variables et parametres locaux
  character(*),parameter :: prg='mkdiff'
  integer                :: numarg             ! nb of arguments in command line
  character*132     :: fil1                    ! input file 1 name
  character*132     :: fil3                    ! output file name
  real,allocatable  :: buf3d1(:,:,:)
  real,allocatable  :: buf2d(:,:)
  real,allocatable  :: buf1d(:)

  integer :: ncstat
  integer :: ncid1,ncid3              ! ncid of input/output files
  integer :: times1_varid
  integer :: we1_dimid,  sn1_dimid,  bt1_dimid,    time1_dimid
  integer :: we3_dimid,  sn3_dimid,  bt3_dimid,    time3_dimid

  integer :: date3_dimid

  integer :: lon1_varid,lat1_varid
  integer :: lon3_varid,lat3_varid,time3_varid
  integer :: d_avcoord1_varid,d_avcoord3_varid
  integer :: d_bvcoord1_varid,d_bvcoord3_varid
  integer :: d_nphourm1_varid,d_nphourm3_varid
  integer :: vartype1
  character(len=40) :: buffatt1, buffatt2    ! string to hold attributes

  integer :: nzonal1,nmerid1,nlevels1,times1

  integer :: nvars1      ! number of variables in input file
  integer :: ivar,islot,idata

  logical :: is3d1

  character(len=19) :: datebuf1,datebuf3    ! to hold date in MM5 format

  character(len=3) charg
    integer::incr
    TYPE(date) :: data_in, data_out
   character(len=8)::p1,p2
 ! functions
  Integer :: mm5date2numeric ! to transform MM5 time into CHIMERE time
  character(len=19) :: numeric2mm5date ! the reciprocal

!!!

  numarg = iargc()
  if (numarg /= 3) call help
  call getarg(1, fil1)
  call getarg(2, fil3)
  call getarg(3, charg)
   read(charg,*)incr

   write(6,*)'inccc' ,incr
  ! open input files
  ncstat=nf90_open(fil1,NF90_NOWRITE,ncid1)
  NCERR(__LINE__)

  ! create output file
  ncstat=nf90_create(fil3,NF90_CLOBBER,ncid3)
  NCERR(__LINE__)

  ! check dimensions
  ! Time
  ncstat=nf90_inq_dimid(ncid1,'Time',time1_dimid)
  NCERR(__LINE__)
  ncstat=nf90_inquire_dimension(ncid1,time1_dimid,len=times1)
  NCERR(__LINE__)
  ncstat=nf90_def_dim(ncid3,'Time',times1,time3_dimid)
  NCERR(__LINE__)

  ! DateStrLen
  ncstat=nf90_def_dim(ncid3,'DateStrLen',19,date3_dimid)
  NCERR(__LINE__)

  ! west_east
  ncstat=nf90_inq_dimid(ncid1,'west_east'  ,we1_dimid)
  NCERR(__LINE__)
  ncstat=nf90_inquire_dimension(ncid1,we1_dimid,len=nzonal1)
  NCERR(__LINE__)
  ncstat=nf90_def_dim(ncid3,'west_east',nzonal1,we3_dimid)
  NCERR(__LINE__)

  ! south_north
  ncstat=nf90_inq_dimid(ncid1,'south_north',sn1_dimid)
  NCERR(__LINE__)
  ncstat=nf90_inquire_dimension(ncid1,sn1_dimid,len=nmerid1)
  NCERR(__LINE__)
  ncstat=nf90_def_dim(ncid3,'south_north',nmerid1,sn3_dimid)
  NCERR(__LINE__)

  ! bottom_top
  ncstat=nf90_inq_dimid(ncid1,'bottom_top' ,bt1_dimid)
  is3d1=(ncstat==NF90_NOERR)
  if (is3d1) then
     ncstat=nf90_inquire_dimension(ncid1,bt1_dimid,len=nlevels1)
     NCERR(__LINE__)
  else
     nlevels1=1
  end if
  ncstat=nf90_def_dim(ncid3,'bottom_top',nlevels1,bt3_dimid)
  NCERR(__LINE__)

  ! create coordinate variables
  ncstat=nf90_def_var(ncid3,'lon',  NF90_FLOAT,(/we3_dimid,sn3_dimid/),   lon3_varid)
  NCERR(__LINE__)
  ncstat=nf90_def_var(ncid3,'lat',  NF90_FLOAT,(/we3_dimid,sn3_dimid/),   lat3_varid)
  NCERR(__LINE__)
  ncstat=nf90_def_var(ncid3,'Times',NF90_CHAR,(/date3_dimid,time3_dimid/),time3_varid)
  NCERR(__LINE__)

  ncstat=nf90_def_var(ncid3,'a_vcoord',  NF90_FLOAT,(/bt1_dimid/),   d_avcoord3_varid)
  NCERR(__LINE__)
  ncstat=nf90_def_var(ncid3,'b_vcoord',  NF90_FLOAT,(/bt1_dimid/),   d_bvcoord3_varid)
  NCERR(__LINE__)
  ncstat=nf90_def_var(ncid3,'nphourm',  NF90_FLOAT,(/time1_dimid/),   d_nphourm3_varid)
  NCERR(__LINE__)




  ! Copy Title atrribute
  buffatt1=' '
  ncstat=nf90_get_att(ncid1,NF90_GLOBAL,'Title',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3,NF90_GLOBAL,'Title',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)

  ! Copy domain atrribute
  buffatt1=' '
  ncstat=nf90_get_att(ncid1,NF90_GLOBAL,'Domain',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3,NF90_GLOBAL,'Domain',buffatt1(1:len_trim(buffatt1)))

  ! write generatuing process
  buffatt1=' '
  ncstat=nf90_put_att(ncid3,NF90_GLOBAL,'Generating_process',"riscrivi_meteo")
  NCERR(__LINE__)
  ! write history trribute
  buffatt1=' '
  ncstat=nf90_put_att(ncid3,NF90_GLOBAL,'history', fil1 )
  NCERR(__LINE__)
 




 ! Copy lon, lat, atributes
  ncstat=nf90_inq_varid(ncid1,'lon',lon1_varid)
  NCERR(__LINE__)
  buffatt1=' '
  ncstat=nf90_get_att(ncid1,lon1_varid,'units',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3, lon3_varid,'units',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)

  buffatt1=' '
  ncstat=nf90_get_att(ncid1,lon1_varid,'long_name',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3, lon3_varid,'long_name',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)

  ncstat=nf90_inq_varid(ncid1,'lat',lat1_varid)
  NCERR(__LINE__)
  buffatt1=' '
  ncstat=nf90_get_att(ncid1,lat1_varid,'units',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3,lat3_varid,'units',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)

  buffatt1=' '
  ncstat=nf90_get_att(ncid1,lat1_varid,'long_name',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3,lat3_varid,'long_name',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)
!
!  legge TImes attribute
  ncstat=nf90_inq_varid(ncid1,'Times',times1_varid)
  NCERR(__LINE__)

! copy coord attribute
  ncstat=nf90_inq_varid(ncid1,'a_vcoord',d_avcoord1_varid)
  NCERR(__LINE__)
  buffatt1=' '
  ncstat=nf90_get_att(ncid1,d_avcoord1_varid,'units',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3, d_avcoord3_varid,'units',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)

  buffatt1=' '
  ncstat=nf90_get_att(ncid1,d_avcoord1_varid,'long_name',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3, d_avcoord3_varid,'long_name',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)

  ncstat=nf90_inq_varid(ncid1,'b_vcoord',d_bvcoord1_varid)
  NCERR(__LINE__)
  buffatt1=' '
  ncstat=nf90_get_att(ncid1,d_bvcoord1_varid,'units',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3,d_bvcoord3_varid,'units',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)

  buffatt1=' '
  ncstat=nf90_get_att(ncid1,d_bvcoord1_varid,'long_name',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3,d_bvcoord3_varid,'long_name',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)


  ncstat=nf90_inq_varid(ncid1,'nphourm',d_nphourm1_varid)
  NCERR(__LINE__)
  buffatt1=' '
  ncstat=nf90_get_att(ncid1,d_nphourm1_varid,'units',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3,d_nphourm3_varid,'units',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)

  buffatt1=' '
  ncstat=nf90_get_att(ncid1,d_nphourm1_varid,'long_name',buffatt1)
  NCERR(__LINE__)
  ncstat=nf90_put_att(ncid3,d_nphourm3_varid,'long_name',buffatt1(1:len_trim(buffatt1)))
  NCERR(__LINE__)




!
  ! create ordinary variables and clone their attributes
  ncstat=nf90_inquire(ncid1,nVariables=nvars1) ; NCERR(__LINE__)
  allocate(varids1(nvars1))
  do ivar=1,nvars1
     ncstat=nf90_inquire_variable( &
          ncid1,                   &
          ivar,                    &
          varids1(ivar)%varname,   &
          vartype1,                &
          varids1(ivar)%ndims)
     NCERR(__LINE__)

     varids1(ivar)%is3d=(varids1(ivar)%ndims==4)
     varids1(ivar)%isreal=((vartype1==NF90_FLOAT).or.(vartype1==NF90_DOUBLE))
     if (                                                                   &
          (varids1(ivar)%varname/='lon')   .and.                            &
          (varids1(ivar)%varname/='lat')   .and.                            &
          (varids1(ivar)%varname/='Times') .and.                            &
          (varids1(ivar)%varname/='a_vcoord') .and.                            &
          (varids1(ivar)%varname/='b_vcoord') .and.                            &
          (varids1(ivar)%varname/='nphourm') .and.                            &
          (varids1(ivar)%isreal)                                            &
          ) then
        if (varids1(ivar)%is3d) then
!        write(6,*)' v4 ',varids1(ivar)%ndims,varids1(ivar)%varname
           ncstat=nf90_def_var(                                             &
                ncid3,                                                      &
                varids1(ivar)%varname(1:len_trim(varids1(ivar)%varname)),   &
                vartype1,                                                   &
                (/we3_dimid,sn3_dimid,bt3_dimid,time3_dimid/),              &
                varids1(ivar)%id3                                           &
                )
           NCERR(__LINE__)
        else
!        write(6,*)' v3 ',varids1(ivar)%ndims,varids1(ivar)%varname
           ncstat=nf90_def_var(                                             &
                ncid3,                                                      &
                varids1(ivar)%varname(1:len_trim(varids1(ivar)%varname)),   &
                vartype1,                                                   &
                (/we3_dimid,sn3_dimid,time3_dimid/),                        &
                varids1(ivar)%id3                                           &
                )
           NCERR(__LINE__)
        end if
        ncstat=nf90_copy_att(ncid1,ivar,    'units',ncid3,varids1(ivar)%id3)
        ncstat=nf90_copy_att(ncid1,ivar,'long_name',ncid3,varids1(ivar)%id3)
     end if
  end do

  ncstat=nf90_enddef(ncid3)
  NCERR(__LINE__)
    write(6,*)'xlon '
  ! Fill xlong,xlat  coord nphourm variables
  allocate(buf2d(nzonal1,nmerid1))
  ncstat=nf90_get_var(ncid1,lon1_varid,buf2d)
  NCERR(__LINE__)
  ncstat=nf90_put_var(ncid3,lon3_varid,buf2d)
  NCERR(__LINE__)
  ncstat=nf90_get_var(ncid1,lat1_varid,buf2d)
  NCERR(__LINE__)
  ncstat=nf90_put_var(ncid3,lat3_varid,buf2d)
  NCERR(__LINE__)
  deallocate(buf2d)
  allocate(buf1d(nlevels1))
  ncstat=nf90_get_var(ncid1,d_avcoord1_varid,buf1d)
  NCERR(__LINE__)
  ncstat=nf90_put_var(ncid3,d_avcoord3_varid,buf1d)
  NCERR(__LINE__)
  ncstat=nf90_get_var(ncid1,d_bvcoord1_varid,buf1d)
  NCERR(__LINE__)
  ncstat=nf90_put_var(ncid3,d_bvcoord3_varid,buf1d)
  NCERR(__LINE__)
  deallocate(buf1d)
  allocate(buf1d(times1))
  ncstat=nf90_get_var(ncid1,d_nphourm1_varid,buf1d)
  NCERR(__LINE__)
  buf1d=buf1d*2  
 ncstat=nf90_put_var(ncid3,d_nphourm3_varid,buf1d)
  NCERR(__LINE__)
  deallocate(buf1d)

!

  ! fill time variable

  do islot=1,times1
     ncstat=nf90_get_var(ncid1,times1_varid,datebuf1,(/1,islot/),(/19,1/))
     NCERR(__LINE__)
     datebuf3=datebuf1
     
   p1(1:4)= datebuf1(1:4)
   p1(5:6)= datebuf1(6:7)
   p1(7:8)= datebuf1(9:10)
   write(6,*) 'incr ',incr
    READ (p1,'(i4,2i2.2)') data_in%yy,data_in%mm,data_in%dd
    write(6,*)data_in%yy
 
    data_out = data_in + incr
    WRITE (p2,'(i4.4,2i2.2)') data_out%yy,data_out%mm,data_out%dd
    datebuf3(1:4)=p2(1:4)
    datebuf3(6:7)=p2(5:6)
    datebuf3(9:10)=p2(7:8)
    write(6,*)'date in  ' ,datebuf1, ' date fi  ' ,datebuf3
     ncstat=nf90_put_var(ncid3,time3_varid,datebuf3,(/1,islot/),(/19,1/))
     NCERR(__LINE__)
  end do






  ! Copy  science variables

  allocate(buf3d1(nzonal1,nmerid1,nlevels1))
  do islot=1,times1
     do ivar=1,nvars1
        if (                                              &
             (varids1(ivar)%varname/='lon')   .and.       &
             (varids1(ivar)%varname/='lat')   .and.       &
            (varids1(ivar)%varname/='Times') .and.       &
          (varids1(ivar)%varname/='a_vcoord') .and.                            &
          (varids1(ivar)%varname/='b_vcoord') .and.                            &
          (varids1(ivar)%varname/='nphourm') .and.                            &
             (varids1(ivar)%isreal)                       &
             ) then
           if (varids1(ivar)%is3d) then
              ncstat=nf90_get_var(                        &
                   ncid1,                                 &
                   ivar,                                  &
                   buf3d1,                                &
                   (/1,1,1,islot/),                       &
                   (/nzonal1,nmerid1,nlevels1,1/)         &
                   )
              NCERR(__LINE__)

              ncstat=nf90_put_var(                        &
                   ncid3,                                 &
                   varids1(ivar)%id3,                     &
                   buf3d1,                         &
                   (/1,1,1,islot/),                       &
                   (/nzonal1,nmerid1,nlevels1,1/)         &
                   )
              NCERR(__LINE__)
           else
              ncstat=nf90_get_var(                        &
                   ncid1,                                 &
                   ivar,                                  &
                   buf3d1(:,:,1),                         &
                   (/1,1,islot/),                         &
                   (/nzonal1,nmerid1,1/)                  &
                   )
              NCERR(__LINE__)

              ncstat=nf90_put_var(                        &
                   ncid3,                                 &
                   varids1(ivar)%id3,                     &
                   buf3d1(:,:,1),                         &
                   (/1,1,islot/),                         &
                   (/nzonal1,nmerid1,1/)                  &
                   )
              NCERR(__LINE__)
           end if
        end if
     end do
  end do
  deallocate(buf3d1)


  ncstat=nf90_close(ncid1)
  NCERR(__LINE__)
  ncstat=nf90_close(ncid3)
  NCERR(__LINE__)


end program riscrivi_meteonc

!********************************************************************************************

subroutine help
  implicit none
  character(len=120) :: cmd
  call getarg(0, cmd)
  write(*,'(/,"Usage: ", A, "  <input file > <output file > <d+/-1 > ")') trim(cmd)
  stop
end subroutine help




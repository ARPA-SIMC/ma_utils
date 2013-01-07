! Collection of routines used by the CHIMERE model
!*******************************************************************************************
! 'readint' : read an integer defined by 'flag' in the parameter 'ival'
! 'readchar': read a character defined by 'flag'
! 'readreal': read a real defined by 'flag' in the parameter 'dval'
! 'readdble': read a dble precision defined by 'flag'
!             in the parameter 'dval'
!  If an error occurs, 'code' is non-zero.
!*******************************************************************************************


!*******************************************************************************************
!  subroutine for automatic check of free logical unit
!  and file opening. This subroutines uses 'iop' and 'iostop'.  
subroutine opfi(ioval,fich,form,status)
  !
  implicit none
  !*****************************************************************************************
  ! subroutine arguments
  integer :: ioval
  character(len=*)   :: fich
  character(len=*)   :: form
  character(len=*)   :: status
  !
  ! local variables
  character(len=2)   :: iocar
  ! functions
  integer,external   :: cl
  !*****************************************************************************************
  call iop(ioval)
  write(iocar,100)ioval

  if(form.eq.'f'.and.status.eq.'n')then
     open(unit=ioval,file=fich,err=800)
  endif
  if(form.eq.'f'.and.status.eq.'o')then
     open(unit=ioval,file=fich,err=801,status='OLD')
  endif
  if(form.eq.'f'.and.status.eq.'s')then
     open(unit=ioval,file=fich,err=800,status='UNKNOWN')
  endif
  if(form.eq.'u'.and.status.eq.'n')then
     open(unit=ioval,file=fich,err=802,form='unformatted')
  endif
  if(form.eq.'u'.and.status.eq.'o')then
     open(unit=ioval,file=fich,err=803, status='OLD',form='unformatted')
  endif
  if(form.eq.'u'.and.status.eq.'s')then
     open(unit=ioval,file=fich,err=802, status='UNKNOWN',form='unformatted')
  endif
  rewind(ioval)
  return
800 call iostop('Error while opening ascii file : '                 //fich)
801 call iostop('Error while opening ascii read-only file : '       //fich)
802 call iostop('Error while opening unformatted file : '           //fich)
803 call iostop('Error while opening unformatted read-only file : ' //fich)

100 format(i2.2)     
end subroutine opfi
!*******************************************************************************************


!*******************************************************************************************
subroutine readint(fichier,flag,ival,code)
  !
  implicit none
  !*****************************************************************************************
  ! subroutine arguments
  character(len=*) :: fichier
  character(len=*) :: flag
  integer :: ival 
  integer :: code
  !
  ! local variables
  character(len=132) :: ligne
  !*****************************************************************************************
  !
  call readchar(fichier,flag,ligne,code)
  if (code.ne.0) goto 999
  read(ligne,*,err=999) ival
  code = 0
  return
999 ival = -1
  code = 1
end subroutine readint
!*******************************************************************************************



!*******************************************************************************************
subroutine readreal(fichier,flag,dval,code)
  !
  implicit none
  !*****************************************************************************************
  ! subroutine arguments
  character(len=*) :: fichier
  character(len=*) :: flag
  real ::    dval 
  integer :: code
  !
  ! local variables
  character(len=132) :: ligne
  !*****************************************************************************************
  !      
  call readchar(fichier,flag,ligne,code)
  if(code.ne.0)goto 999
  read(ligne,*,ERR=999)dval
  code=0
  return
999 dval=-1.
  code=1
  return
end subroutine readreal
!*******************************************************************************************



!*******************************************************************************************
subroutine readdble(fichier,flag,dval,code)
  !
  implicit none
  !*****************************************************************************************
  ! subroutine arguments
  character(len=*) :: fichier
  character(len=*) :: flag
  real(kind=8)     ::    dval 
  integer          :: code
  !
  ! local variables
  character(len=132) :: ligne
  !*****************************************************************************************
  !      
  call readchar(fichier,flag,ligne,code)
  if(code.ne.0)goto 999
  read(ligne,*,ERR=999)dval
  code=0
  return
999 dval=-1.0D0
  code=1
  return
end subroutine readdble
!*******************************************************************************************



!*******************************************************************************************
subroutine readchar(fichier,flag,ligne,code)
  !
  implicit none
  !*****************************************************************************************
  ! subroutine arguments
  character(len=*) :: fichier
  character(len=*) :: flag
  character(len=*) :: ligne 
  integer          :: code
  !
  ! local variables
  integer,external :: cl
  integer :: iopref,coldc,coldf,colff,long
  !*****************************************************************************************
  !
  !     opening file named 'fichier'
  call opfi(iopref,fichier(1:cl(fichier)),'f','o')
  !
  !     Loop over file
993 continue
  read(iopref,999,err=998,end=998)ligne
  long=cl(ligne)
  !
  if ((ligne(1:1).eq.'#').or.(ligne(1:1).eq.'%').or. &
       (ligne(1:1).eq.'@').or.(ligne(1:1).eq.'*').or. &
       (long.eq.0))then
     goto 993
  else
     coldf=1
     do while((ligne(coldf:coldf).eq.' ').and.(coldf.lt.long))
        coldf=coldf+1
     enddo
     if(coldf.eq.long) goto 993
     colff=coldf
     do while((ligne(colff:colff).ne.'=').and. &
          (ligne(colff:colff).ne.'>').and. &
          (ligne(colff:colff).ne.' ').and.(colff.lt.long))
        colff=colff+1
     enddo
     if(colff.ne.long)colff=colff-1
     if((colff.ge.long-1).or.(ligne(coldf:colff).ne.flag))goto 993
  endif
  coldc=colff+1
  do while((coldc.lt.long).and. &
       &    ((ligne(coldc:coldc).eq.' ') &
       & .or.(ligne(coldc:coldc).eq.'=') &
       & .or.(ligne(coldc:coldc).eq.'>')))
     coldc=coldc+1
  enddo
  if((coldc.eq.long).and. &
       & ((ligne(coldc:coldc).eq.' ').or. &
       &  (ligne(coldc:coldc).eq.'=').or. &
       &  (ligne(coldc:coldc).eq.'>'))) goto 998
  close(iopref)
  ligne=ligne(coldc:long)
  code=0
  return
998 close(iopref)
  ligne='xxx'
  code=1
  return
999 format(100a)
end subroutine readchar
!*******************************************************************************************


!*******************************************************************************************
! 'iop' affects automatically a free logical unit number to open a file
! (from 10 to 99)
!      
! Function INQUIRE: see web documentation of g77:
! http://www.fortran.com/fortran/F77_std/rjcnf0001-sh-12.html#sh-12.7
subroutine iop(iounit)
  !
  implicit none
  !*****************************************************************************************
  ! subroutine arguments
  integer :: iounit
  !
  ! local variables
  integer :: theunit
  logical :: od
  !*****************************************************************************************
  !
  od = .true.
  do theunit = 10 , 99
     if ( od ) then
        iounit = theunit
        inquire (iounit, opened=od)
     endif
  enddo
  if ( od ) then
     call iostop ('No more file unit available in IOP')
  endif
end subroutine iop
!*******************************************************************************************


!*******************************************************************************************
! 'iostop' stops the program after an error with a comment line
subroutine iostop(comment)
  !
  implicit none
  !*****************************************************************************************
  ! subroutine arguments
  character(len=*) :: comment
  !*****************************************************************************************
  !
  write(*,'(A)') comment
  write(*,'(A)') 'Stopping the program by a call to IOSTOP'
  stop
end subroutine iostop
!*******************************************************************************************


!*******************************************************************************************
! 'cl' calculation of a character length strength
function cl(c)
  !
  implicit none
  !*****************************************************************************************
  ! function arguments
  character(len=*) :: c
  integer :: cl
  !
  ! local variables
  integer :: j
  !*****************************************************************************************
  !
  j = len(c) + 1
10 j = j - 1
  if ((c(j:j).eq.' ').and.(j.gt.1)) goto 10
  if ((j.eq.1).and.(c(j:j).eq.' ')) j = 0
  cl = j
end function cl
!*******************************************************************************************

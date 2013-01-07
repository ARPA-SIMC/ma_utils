subroutine layave(nlevels,nlayers,alti,x,xlay,hlay) 
  !  Gives CHIMERE layer averages of any variable                         

  implicit none

  !*****************************************************************************************
  ! subroutine arguments
  integer,intent(in) :: nlevels
  integer,intent(in) :: nlayers
  real,intent(in),dimension(0:nlevels) :: alti
  real,intent(in),dimension(0:nlevels) :: x
  real,intent(out),dimension(nlayers) :: xlay
  real,intent(in),dimension(nlayers) :: hlay

  ! local variables
  real,dimension(0:nlevels) :: xtmp
  integer :: nl,nla
  real :: xtot,xint,xtop

  !*****************************************************************************************

  xtmp(0) = 0.0 
  do nl=1,nlevels 
     xtot = 0.5*(alti(nl)-alti(nl-1))*(x(nl)+x(nl-1)) 
     xtmp(nl) = xtmp(nl-1) + xtot 
     do nla=1,nlayers 
        if(hlay(nla).gt.alti(nl-1) .and. hlay(nla).le.alti(nl)) then
           xtop = x(nl-1) &
                + (x(nl) - x(nl-1)) * (hlay(nla)- alti(nl-1)) / (alti(nl) - alti(nl-1))
           xint = 0.5*(hlay(nla)-alti(nl-1))*(xtop+x(nl-1))
           xlay(nla) = xtmp(nl-1) + xint
        endif
     enddo
  enddo


  do nla=nlayers,2,-1 
     xlay(nla) = (xlay(nla) - xlay(nla-1)) / (hlay(nla) - hlay(nla-1))
  enddo
  xlay(1) = xlay(1)/hlay(1)

END subroutine layave

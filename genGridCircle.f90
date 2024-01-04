program genGrid
  implicit none
  integer :: sidelength = 10, numNodes
  real*8, allocatable :: node(:,:)
  integer :: element(3,200)
  real :: dx, dy, r, th, dr, dth, rmax
  integer :: i, j, io,elemio, elemid

  rmax = 1.
  r = 0.1
  dr = 0.05
  dth = 10
  i = 1
  numNodes = ((rmax - r)/dr) * (360./dth)
  print *, numNodes

  allocate(node(2,numNodes))

  ! Placing gridpoints
  do while(r < rmax)
     th = 0.
     do while(th < 360.)
        node(1,i) = r*cosd(th)
        node(2,i) = r*sind(th)
        i = i + 1
        th = th + dth
     end do
     r = r + dr
  end do

!!$  i = 0
!!$  elemid = 0
!!$  do while(i .lt. 89)
!!$     i = i + 1    
!!$     if (mod(i,10) .eq. 0) cycle
!!$     elemid = elemid + 1
!!$     element(1,elemid) = i
!!$     element(2,elemid) = i+1
!!$     element(3,elemid) = i+10
!!$     elemid = elemid + 1
!!$     element(1,elemid) = i+1
!!$     element(2,elemid) = i+10+1
!!$     element(3,elemid) = i+1+9
!!$  end do
  
  

  open(newunit=io, file="node.dat")
  !open(newunit=elemio, file="element.dat")
  write(io, *) numNodes, -1, -1, 1, 1
  !write(elemio,*) elemid
  
  do i=1,numNodes
     write(io,*) node(:,i)
  end do
!!$  do i=1,elemid     
!!$     write(elemio,*) element(:,i)
!!$  end do
  
  close(io)
  !close(elemio)
  
end program genGrid

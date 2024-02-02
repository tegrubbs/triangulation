program genGrid
  implicit none
  integer :: sidelength = 10, numNodes
  real*8, allocatable :: node(:,:)
  integer :: element(3,200)
  real :: dx, dy, r
  integer :: i, j, io,elemio, elemid

  dx = 1./sidelength
  dy = dx
  
  numNodes = sidelength*sidelength

  allocate(node(2,numNodes))

  ! Placing gridpoints
  do i=1,sidelength
     !if (mod(i,2)==0) dx = dx*.9
     do j=1,sidelength
        call RANDOM_NUMBER(r)
        node(1,(i-1)*sidelength+j) = dx*i + r*.025
        call RANDOM_NUMBER(r)
        node(2,(i-1)*sidelength+j) = dy*j + r*.025
     end do
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
  write(io, *) numNodes, 0, 0, 1, 1
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

program genGrid
  implicit none
  integer :: sidelength = 50, numNodes
  real*8, allocatable :: node(:,:)
  integer :: element(3,200)
  real :: dx, dy, r, th, dr, dth, rmax, rlist(13)
  integer :: i, j, io,elemio, elemid, ri,x,y

  rmax = 0.8
  r = 0.7
  rlist(1) = 0.1
  do i=2,size(rlist)
     rlist(i) = rlist(i-1)*1.2
  end do

  dx = 10./sidelength
  dy = 10./sidelength
  
  
  dr = 0.05
  dth = 10
  i = 1
  numNodes = size(rlist) * (360./dth)
  print *, numNodes

  allocate(node(2,numNodes*10))

  ! Placing gridpoints
  do ri=1,size(rlist)

     th = 0.
     if (mod(ri,2) .eq. 0) then
        th = th + dth/2.
     end if
     r = rlist(ri)
     do while(th < 360.)
        node(1,i) = r*cosd(th)
        node(2,i) = r*sind(th)
        i = i + 1
        th = th + dth
     end do    
  end do

!!$  do x=1,sidelength
!!$     !if (mod(i,2)==0) dx = dx*.9
!!$    do y=1,sidelength        
!!$        node(1,i) = -1 + dx*x       
!!$        node(2,i) = -1 + dy*y
!!$        i = i+1
!!$    end do
!!$  end do

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
  write(io, *) i-1, -1, -1, 1, 1
  !write(elemio,*) elemid
  
  do j=1,i-1
     write(io,*) node(:,j)
  end do
!!$  do i=1,elemid     
!!$     write(elemio,*) element(:,i)
!!$  end do
  
  close(io)
  !close(elemio)
  
end program genGrid

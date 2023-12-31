program genGrid
  implicit none
  real*8 :: node(2,100)
  integer :: element(3,200)
  real :: dx = 0.1
  integer :: i, j, io,elemio, elemid

  do i=1,10
     do j=1,10
        node(1,(i-1)*10+j) = dx*i
        node(2,(i-1)*10+j) = dx*j
     end do
  end do

  i = 0
  elemid = 0
  do while(i .lt. 89)
     i = i + 1    
     if (mod(i,10) .eq. 0) cycle
     elemid = elemid + 1
     element(1,elemid) = i
     element(2,elemid) = i+1
     element(3,elemid) = i+10
     elemid = elemid + 1
     element(1,elemid) = i+1
     element(2,elemid) = i+10+1
     element(3,elemid) = i+1+9
  end do
  
  

  open(newunit=io, file="node.dat")
  open(newunit=elemio, file="element.dat")
  write(io, *) 100, 0, 0, 1, 1
  write(elemio,*) elemid
  
  do i=1,100
     write(io,*) node(:,i)
  end do
  do i=1,elemid     
     write(elemio,*) element(:,i)
  end do
  
  close(io)
  close(elemio)
  
end program genGrid

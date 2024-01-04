! check MAX_EDGE_LENGTH if things look strange.

module triangulate
  use stdlib_sorting, only: sort_index
  implicit none
  public :: read_nodes, read_elements, basic_triangulate, orient, edgeCheck, edge_length,element_exists,small_number_check,low_angle
  real, allocatable :: node(:,:)
  real :: bounds(4)
  integer, allocatable :: element(:,:), edge(:,:)
  integer :: num_elements, num_nodes, num_edges, edgeid, elemid ! -1 to account for header line.
  real, parameter :: MAX_EDGE_LENGTH = 0.2, MIN_VALUE = 1E-8, MIN_ANGLE=15.

contains

  subroutine read_nodes()
    character(8) :: nodeFile = "node.dat"
    integer :: nodeio, i

    open(newunit=nodeio, file=nodeFile)
    read(nodeio, *) num_nodes, bounds ! get the number of nodes and coordinate system from the first line.
    allocate(node(2,num_nodes))

    do i=1,num_nodes
       read(nodeio, *) node(:,i)
    end do           
    close(nodeio)

  end subroutine read_nodes
  
  subroutine read_elements()
    character(12) :: elemFile = "element.dat"
    integer :: elemio, i

    open(newunit=elemio, file=elemFile)
    read(elemio, *) num_elements ! get the number of elements.
    allocate(element(3,num_elements))

    do i=1,num_elements
       read(elemio, *) element(:,i)
    end do           
    close(elemio)

  end subroutine read_elements

  function orient(a,b,c) result(orientation)
    real :: a(2),b(2),c(2), orientation
    orientation = ((a(1) - c(1)) * (b(2) - c(2))) - ((a(2) - c(2)) * (b(1) - c(1)))
    if (orientation .eq. -0.) orientation = 0.    
    
  end function orient

  function edge_length(ed) result(length)
    real :: length
    integer :: ed(2)
    real :: a(2), b(2)

    a = node(:,ed(1))
    b = node(:,ed(2))

    length = norm2(a-b)
  end function edge_length
  

  ! checks if value is smaller than some threshold and sets it to zero if true.
  ! this is needed for the orientation check.
  subroutine small_number_check(value)
    real :: value
    if (value < MIN_VALUE) value = 0.
  end subroutine small_number_check
  

  function edgeCheck(ed) result(intersects)
    integer :: ed(2),i
    logical :: intersects,node1same,node2same
    real :: a(2), b(2), c(2), d(2), oa, ob, oc, od
    
    a = node(:,ed(1))
    b = node(:,ed(2))

    ! edgeid should be the index of the last edge.
    
    do i=1,edgeid
      c = node(:, edge(1,i))
      d = node(:, edge(2,i))

      ! checking if edge is the same.
      node1same = (edge(1,i) .eq. ed(1)) .or. (edge(1,i) .eq. ed(2))
      node2same = (edge(2,i) .eq. ed(1)) .or. (edge(2,i) .eq. ed(2))
      
      
      if (node1same .and. node2same) then         
         intersects = .false.
         exit
      end if

      oa = orient(c,d,a)
      ob = orient(c,d,b)
      oc = orient(a,b,c)
      od = orient(a,b,d)

      ! need to remove small values from orientation calculation.
      ! edges that are totally distance can be calculated as an intersection if they are collinear.
      ! These is becauase the orientation calculation returns a very small number on the order of 10^-9.
!!$      call small_number_check(oa)
!!$      call small_number_check(ob)
!!$      call small_number_check(oc)
!!$      call small_number_check(od)
      
      
      intersects = ((oa > 0.) .neqv. (ob > 0.)) .and. ((oc > 0.) .neqv. (od > 0.))
      

      ! if we have an intersection, it could be 2 edges that share 1 similar node. That can still be classified as an intersection
      ! according to the above math.
      if (intersects .and. .not. node1same .and. .not. node2same) then
!!$         print *, edge(:,i)
!!$         print *, orient(c,d,a), orient(c,d,b), orient(a,b,c), orient(a,b,d)
!!$         print *, "Fails intersection"
         exit
      else 
         intersects = .false.
      end if
      
    end do
    
  end function edgeCheck

  ! checks if element already exists.
  function element_exists(temp) result(exists)
    integer :: temp(3)
    integer :: i
    integer :: a(3),b(3),c(3),d(3)
    logical :: exists
    integer :: roll(3), roll2(3)
    exists = .false.
    
    do i=1, elemid-1

       ! first 3 checks if this element already exists.
       a = (element(:,i) .eq. temp(1))
       b = (element(:,i) .eq. temp(2))
       c = (element(:,i) .eq. temp(3))

       ! this helps prevent one larger element from encompassing smaller ones.
       d = (element(:,i) .eq. temp)
       roll = (cshift(temp, 1) .eq. element(:,i))
       roll2 = (cshift(temp, 2) .eq. element(:,i))
       
      
       
       ! if all nodes are the same, this element already exists.
       if (sum(a)>0 .and. sum(b)>0 .and. sum(c)>0) then
          exists = .true.
          !print *, "Fails existence check"
          exit
       ! if 2 nodes are the same and in the same order then they will either cause an overlap or enclose another element.
       else if (sum(d) > 1 .or. sum(roll) > 1 .or. sum(roll2) > 1) then
          exists = .true.
!!$          print *, temp
!!$          print *, element(:,i)
!!$          print *, "Fails enclosure check"
          exit          
       end if
    end do
    

  end function element_exists

  function low_angle(temp) result(small_angle)
    integer*8 :: temp(3)
    real :: pa(2),pb(2),pc(2), a,b,c, AA,BB,CC
    logical :: small_angle
    
    small_angle = .false.
    pa = node(:,temp(1))
    pb = node(:,temp(2))
    pc = node(:,temp(3))

    a = norm2(pb-pa)
    b = norm2(pc-pb)
    c = norm2(pa-pc)

    CC = acosd((a**2 + b**2 - c**2) / (2.*a*b))
    BB = acosd((a**2 + c**2 - b**2) / (2.*a*c))
    AA = 180. - BB - CC

   

    if (AA < MIN_ANGLE) then
       small_angle = .true.
       return
    end if

    if (BB < MIN_ANGLE) then
       small_angle = .true.
       return
    end if

    if (CC < MIN_ANGLE) then
       small_angle = .true.
       return
    end if

    
    
  end function low_angle
  
  
  

  subroutine basic_triangulate()
    integer :: nodeid, tempedge(2,3), i, j

    ! array that stores sorted indices must be 64bit integers.
    integer*8 :: sorted_nodes(num_nodes), tempElem(3)
    real :: displacement(2,num_nodes), distance(num_nodes), orientation
    logical :: intersection
    
    num_elements = 1000000
    num_edges = 1000000
    
    allocate(element(3,num_elements))
    allocate(edge(2, num_edges))



    elemid = 1
    edgeid = 1


    do nodeid=1,num_nodes



      !print *, "NODE: ", nodeid
       
      ! calculates the distance between this node and all the others, and sorts it to find the nearest nodes.
      displacement(1,:) = node(1,nodeid)
      displacement(2,:) = node(2,nodeid)
      displacement = displacement - node
      distance = norm2(displacement, 1)
      call sort_index(distance, sorted_nodes)

      
      tempElem(1) = sorted_nodes(1)

      ! explore all combinations of neighbors to try different elements.
      do i=2,10
         tempElem(2) = sorted_nodes(i)
         do j=2,10
          if (j .eq. i) cycle
          tempElem(3) = sorted_nodes(j)
          !print * ,tempElem

          if (low_angle(tempElem)) cycle
                
          orientation = orient( node(:,tempElem(1)), node(:,tempElem(2)), node(:,tempElem(3)))

          if (orientation .eq. 0.) cycle ! points are collinear and should not be made into an element.

         

          ! if orientation is negative, points are in wrong order, flipping one pair corrects the order.
          if (orientation .lt. 0.) then
             element(1, elemid) = tempElem(2)
             element(2, elemid) = tempElem(1)
             element(3, elemid) = tempElem(3)
             
          else
             element(:, elemid) = tempElem
          end if

           ! checking if element already exists.
          if(element_exists(element(:,elemid))) cycle


          ! now doing edge checks.

          tempedge(:,1) = element(1:2, elemid)
          tempedge(:,2) = element(2:3, elemid)
          tempedge(1,3) = element(3, elemid)
          tempedge(2,3) = element(1, elemid)          

          ! hardcoding maximum value for edge length here to prevent long edges.
          if (edge_length(tempedge(:,1)) > MAX_EDGE_LENGTH) cycle
          if (edge_length(tempedge(:,2)) > MAX_EDGE_LENGTH) cycle
          if (edge_length(tempedge(:,3)) > MAX_EDGE_LENGTH) cycle


          ! check if edge intersect with previous edges.
          ! if an edge intersects, this element needs to be canceled.

          if (edgeCheck(tempedge(:,1))) then
             !print *, "FAIL", tempedge(:,1)
             cycle
          end if

          if (edgeCheck(tempedge(:,2))) then
             !print *, "FAIL", tempedge(:,2)
             cycle
          end if

          if (edgeCheck(tempedge(:,3))) then
             !print *, "FAIL", tempedge(:,3)
             cycle
          end if


          edge(:,edgeid) = tempedge(:,1)
          edge(:,edgeid+1) = tempedge(:,2)
          edge(:,edgeid+2) = tempedge(:,3)

          !print *, elemid, element(:,elemid)

          edgeid = edgeid + 3
          elemid = elemid + 1
          
          
       end do
       
     end do
     
      
    end do
   
    num_elements = elemid - 1
    print *, num_elements, " elements generated."
    
 
  end subroutine basic_triangulate
  
  

end module triangulate


  

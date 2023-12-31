module triangulate
  use stdlib_sorting, only: sort_index
  implicit none
  public :: read_nodes, read_elements, basic_triangulate, orient, edgeCheck
  real, allocatable :: node(:,:)
  real :: bounds(4)
  integer, allocatable :: element(:,:), edge(:,:)
  integer :: num_elements, num_nodes, num_edges, edgeid ! -1 to account for header line.

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

!!$  function ccw(a,b,c) result(test)
!!$    real :: a(2), b(2), c(2)
!!$    logical :: test
!!$
!!$    test = (c(2)-a(2) * (b(1)-a(1))) > ((b(2) - a(2)) * (c(1)-(a(1)))
!!$    
!!$  end function ccw
  

  function edgeCheck(ed) result(intersects)
    integer :: ed(2),i
    logical :: intersects,node1same,node2same
    real :: a(2), b(2), c(2), d(2)
    
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
      
      
      intersects = ((orient(c,d,a) > 0.) .neqv. (orient(c,d,b) > 0.)) .and. ((orient(a,b,c) > 0.) .neqv. (orient(a,b,d) > 0.))

      ! if we have an intersection, it could be 2 edges that share 1 similar node. That can still be classified as an intersection
      ! according to the above math.
      if (intersects .and. .not. node1same .and. .not. node2same) then        
         exit
      end if
      
    end do
    
  end function edgeCheck
  

  subroutine basic_triangulate()
    integer :: nodeid,elemid, tempedge(2,3)

    ! array that stores sorted indices must be 64bit integers.
    integer*8 :: sorted_nodes(num_nodes), tempElem(3)
    real :: displacement(2,num_nodes), distance(num_nodes), orientation
    logical :: intersection
    
    num_elements = 1000
    num_edges = 1000
    
    allocate(element(3,num_elements))
    allocate(edge(2, num_edges))


    elemid = 1
    edgeid = 1


    do nodeid=1,num_nodes
       
      ! calculates the distance between this node and all the others, and sorts it to find the nearest nodes.
      displacement(1,:) = node(1,nodeid)
      displacement(2,:) = node(2,nodeid)
      displacement = displacement - node
      distance = norm2(displacement, 1)
      call sort_index(distance, sorted_nodes)

      tempElem = sorted_nodes(1:3)
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

      tempedge(1, 1) = element(1, elemid)
      tempedge(2, 1) = element(2, elemid)
      tempedge(1, 2) = element(2, elemid)
      tempedge(2, 2) = element(3, elemid)
      tempedge(1, 3) = element(3, elemid)
      tempedge(2, 3) = element(1, elemid)

      ! check if edge intersect with previous edges.
      ! if an edge intersects, this element needs to be canceled.
      print *, nodeid, element(:, elemid)
      
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
      
      edgeid = edgeid + 3
      elemid = elemid + 1
      
    end do
   
    num_elements = elemid

  end subroutine basic_triangulate
  
  

end module triangulate


  

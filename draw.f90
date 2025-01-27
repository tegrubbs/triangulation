module draw
  use :: procall
  implicit none
    
  public :: init, draw_nodes, draw_elements, draw_elem

    
  integer :: key
  
!!$  private :: draw_tri
  
  real, parameter, private :: RADIUS = .005 ! radius of nodes.
  integer, parameter, private :: WIN_WIDTH  = 900
  integer, parameter, private :: WIN_HEIGHT = 900
  integer, private :: win = 0
  real, private :: xb, xt, yb, yt

contains

  subroutine init(bounds)
    real :: bounds(4)
    call gopen(WIN_WIDTH, WIN_HEIGHT, win)
    call winname(win, 'Simple' // char(0))
    call newwindow(win, bounds(1), bounds(2), bounds(3) + 0.1*bounds(3), bounds(4) + 0.1*bounds(4)) ! sets coordinate system to [0,1] x [0,1].
    !call newwindow(win, -0.5, -0.5, 0.5, 0.5) ! sets coordinate system to [0,1] x [0,1].
    call gsetbgcolor(win, 'black' // char(0)) ! Set background colour.    
  end subroutine init



  subroutine draw_nodes(node)
    character(8) :: nodeString    
    integer :: i, num_nodes   
    real :: node(:,:)
    
    num_nodes = size(node, 2)
    
    do i=1,num_nodes
       
       ! Writes node id.       
       write(nodeString, '(i3)') i
       call newrgbcolor(win, 255, 255, 255)
       call drawstr(win, node(1,i), node(2,i), 14., nodeString, 0.0, 4)
       
       ! Draw node as small circle.
       call fillcirc(win, node(1,i), node(2,i), RADIUS, RADIUS)
       !call msleep(25)
    end do      

  end subroutine draw_nodes
  
      
    
  
  subroutine draw_elements(node, element, num_elements)    
    integer :: i
    real :: node(:,:)
    integer :: element(:,:), num_elements    
       
    do i=1, num_elements
       call draw_elem(node(:, element(:,i)), i)       
    end do

  end subroutine draw_elements
  
  
  subroutine draw_elem(elem, elemID)
    real :: elem(2,3)    
    integer :: i, elemID
    character(8) :: idstring
    call newrgbcolor(win, 255, 0, 0)
    !call fillpoly(win, elem(1,:), elem(2,:), 3, 0)
    
    ! loop over vertices to create triangle.
    do i=1,3       
       call newrgbcolor(win, 255, 0, 0)
       call drawline(win, elem(1,i), elem(2,i), elem(1, MOD(i,3)+1), elem(2, MOD(i,3)+1))        
       !call msleep(20) ! fun to see it drawn.                    
    end do
    

    ! Writes element id in approximately the center of the element.
    write(idstring, '(i3)') elemID
    call newrgbcolor(win, 0, 255, 0)
    call drawstr(win, sum(elem(1,:))/3. -.01 , sum(elem(2,:))/3., 14., idstring, 0.0, 8)
    
  end subroutine draw_elem

end module draw

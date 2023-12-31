program main
  use :: draw
  use :: triangulate

  call read_nodes()
  
  call init(bounds)

  call draw_nodes(node)

  call basic_triangulate()

  call draw_elements(node, element, num_elements)

   ! wait for key press then close.
  call ggetch(key)

  call gcloseall()
  

end program main

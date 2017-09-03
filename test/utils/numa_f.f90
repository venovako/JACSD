module numa_f

  use, intrinsic :: iso_c_binding
  implicit none

  interface
     subroutine move_data_pages(addr, sz, to_node, info) bind(c,name='move_data_pages_')
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr), value :: addr
       integer(c_size_t), intent(in) :: sz
       integer(c_int), intent(in) :: to_node
       integer(c_long), intent(out) :: info
     end subroutine move_data_pages
  end interface

contains

  subroutine move_data_pages_i(addr, sz, to_node, info)
    use, intrinsic :: iso_c_binding
    implicit none
    integer, intent(in), target :: addr
    integer, intent(in) :: sz
    integer, intent(in) :: to_node
    integer, intent(out) :: info

    type(c_ptr) :: addr_
    integer(c_size_t) :: sz_
    integer(c_int) :: to_node_
    integer(c_long) :: info_

    addr_ = c_loc(addr)
    sz_ = int(sz, c_size_t) * c_sizeof(addr)
    to_node_ = int(to_node, c_int)
    call move_data_pages(addr_, sz_, to_node_, info_)
    info = int(info_)
  end subroutine move_data_pages_i

  subroutine move_data_pages_l(addr, sz, to_node, info)
    use, intrinsic :: iso_c_binding
    implicit none
    logical, intent(in), target :: addr
    integer, intent(in) :: sz
    integer, intent(in) :: to_node
    integer, intent(out) :: info

    type(c_ptr) :: addr_
    integer(c_size_t) :: sz_
    integer(c_int) :: to_node_
    integer(c_long) :: info_

    addr_ = c_loc(addr)
    sz_ = int(sz, c_size_t) * c_sizeof(addr)
    to_node_ = int(to_node, c_int)
    call move_data_pages(addr_, sz_, to_node_, info_)
    info = int(info_)
  end subroutine move_data_pages_l

  subroutine move_data_pages_s(addr, sz, to_node, info)
    use, intrinsic :: iso_c_binding
    implicit none
    real, intent(in), target :: addr
    integer, intent(in) :: sz
    integer, intent(in) :: to_node
    integer, intent(out) :: info

    type(c_ptr) :: addr_
    integer(c_size_t) :: sz_
    integer(c_int) :: to_node_
    integer(c_long) :: info_

    addr_ = c_loc(addr)
    sz_ = int(sz, c_size_t) * c_sizeof(addr)
    to_node_ = int(to_node, c_int)
    call move_data_pages(addr_, sz_, to_node_, info_)
    info = int(info_)
  end subroutine move_data_pages_s

  subroutine move_data_pages_d(addr, sz, to_node, info)
    use, intrinsic :: iso_c_binding
    implicit none
    double precision, intent(in), target :: addr
    integer, intent(in) :: sz
    integer, intent(in) :: to_node
    integer, intent(out) :: info

    type(c_ptr) :: addr_
    integer(c_size_t) :: sz_
    integer(c_int) :: to_node_
    integer(c_long) :: info_

    addr_ = c_loc(addr)
    sz_ = int(sz, c_size_t) * c_sizeof(addr)
    to_node_ = int(to_node, c_int)
    call move_data_pages(addr_, sz_, to_node_, info_)
    info = int(info_)
  end subroutine move_data_pages_d

  subroutine move_data_pages_c(addr, sz, to_node, info)
    use, intrinsic :: iso_c_binding
    implicit none
    complex, intent(in), target :: addr
    integer, intent(in) :: sz
    integer, intent(in) :: to_node
    integer, intent(out) :: info

    type(c_ptr) :: addr_
    integer(c_size_t) :: sz_
    integer(c_int) :: to_node_
    integer(c_long) :: info_

    addr_ = c_loc(addr)
    sz_ = int(sz, c_size_t) * c_sizeof(addr)
    to_node_ = int(to_node, c_int)
    call move_data_pages(addr_, sz_, to_node_, info_)
    info = int(info_)
  end subroutine move_data_pages_c

  subroutine move_data_pages_z(addr, sz, to_node, info)
    use, intrinsic :: iso_c_binding
    implicit none
    double complex, intent(in), target :: addr
    integer, intent(in) :: sz
    integer, intent(in) :: to_node
    integer, intent(out) :: info

    type(c_ptr) :: addr_
    integer(c_size_t) :: sz_
    integer(c_int) :: to_node_
    integer(c_long) :: info_

    addr_ = c_loc(addr)
    sz_ = int(sz, c_size_t) * c_sizeof(addr)
    to_node_ = int(to_node, c_int)
    call move_data_pages(addr_, sz_, to_node_, info_)
    info = int(info_)
  end subroutine move_data_pages_z

end module numa_f

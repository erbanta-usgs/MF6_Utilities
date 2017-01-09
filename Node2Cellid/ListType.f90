module ListModule
  ! -- ListType implements a generic list.
  private
  public :: ListType, ListNodeType

  type :: ListType
    ! -- Public members
    character(len=20) :: name = ''
    type(ListNodeType), pointer, public :: firstNode => null()
    type(ListNodeType), pointer, private :: lastNode => null()
    type(ListNodeType), pointer, private :: currentNode=> null()
    integer, private :: currentNodeIndex = 0
    integer, private :: nodeCount = 0 ! Ned Todo: this is only for debugging
  contains
    ! -- Public procedures
    procedure, public :: Add => add_obj
    procedure, public :: AssignName
    procedure, public :: Clear => clear_list
    procedure, public :: Count => count_nodes
    procedure, public :: DeallocateBackward => deallocate_backward
    procedure, public :: GetNextValue => get_next_value
    procedure, public :: GetPreviousValue => get_previous_value
    generic,   public :: GetValue => get_value_by_index, get_current_value
    procedure, public :: Next => next_node
    procedure, public :: Previous => previous_node
    procedure, public :: Reset => reset_current_node
    ! -- Private procedures
    procedure, private :: get_current_value
    procedure, private :: get_value_by_index
    procedure, private :: next_node
    procedure, private :: previous_node
    procedure, private :: reset_current_node
    ! Finalization is not supported in gfortran (as of 4.10.0)
    !final :: clear_list
  end type ListType

  type :: ListNodeType
    class(*), pointer :: Value => null()
    type(ListNodeType), pointer :: nextNode => null()
    type(ListNodeType), pointer :: prevNode => null()
  contains
    ! -- Public procedure
    procedure, public :: GetValue => list_node_get_value
    ! -- Private procedures
    procedure, private :: DeallocValue => list_node_dealloc_value
    procedure, private :: SetValue => list_node_set_value
  end type ListNodeType

contains

  ! -- Type-bound procedures for ListType

  subroutine add_obj(this, objptr)
    implicit none
    ! -- dummy variables
    class(ListType), intent(inout) :: this
    class(*), pointer, intent(inout) :: objptr
    !
    if (.not. associated(this%firstNode)) then
      allocate(this%firstNode)
      this%firstNode%Value => objptr
      this%firstNode%prevNode => null()
      this%lastNode => this%firstNode
    else
      allocate(this%lastNode%nextNode)
      this%lastNode%nextNode%prevNode => this%lastNode
      this%lastNode%nextNode%value => objptr
      this%lastNode => this%lastNode%nextNode
    endif
    this%nodeCount = this%nodeCount + 1
    return
  end subroutine add_obj

  subroutine AssignName(this, name)
    implicit none
    ! dummy arguments
    class(ListType) :: this
    character(len=*), intent(in) :: name
    !
    this%name = name
    return
  end subroutine AssignName

  subroutine clear_list(this)
    ! **************************************************************************
    ! clear_list (finalizer)
    ! Deallocate all items in linked list
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
    ! -- dummy variables
    class(ListType) :: this
    ! -- Local variables
    type(ListNodeType), pointer :: current => null()
    type(ListNodeType), pointer :: next => null()
    !
    if (.not. associated(this%firstNode)) return
    ! -- The last node will be deallocated in the loop below.
    !    Just nullify the pointer to the last node to avoid
    !    having a dangling pointer.
    if (associated(this%lastNode)) then
      nullify(this%lastNode)
    endif
    !
    current => this%firstNode
    do while (associated(current))
      ! -- Assign a pointer to the next node in the list
      next => current%nextNode
      ! -- Deallocate the object stored in the current node
      call current%DeallocValue()
      ! -- Deallocate the current node
      deallocate(current)
      ! -- Advance to the next node
      current => next
    enddo
    ! -- All nodes have been deallocated. Now just nullify
    !    the pointer to the first node to avoid having a
    !    dangling pointer.
    this%firstNode => null()
    call this%reset_current_node()
    this%nodeCount = 0
    !
    return
  end subroutine clear_list

  integer function count_nodes(this)
    ! **************************************************************************
    ! count_nodes (implements Count)
    ! Return number of nodes in linked list
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
    ! -- dummy variables
    class(ListType) :: this
    ! -- Local variables
    type(ListNodeType), pointer :: current => null()
    integer :: k
    !
    k = 0
    if (associated(this%firstNode)) then
        current => this%firstNode
        k = 1
        do while (associated(current%nextNode))
        current => current%nextNode
        k = k + 1
        enddo
    endif
    !
    count_nodes = k
    return
  end function count_nodes

  subroutine deallocate_backward(this, fromNode)
    ! **************************************************************************
    ! deallocate_backward (implements DeallocateBackward)
    ! Deallocate fromNode and all previous nodes in list; reassign firstNode.
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
    ! dummy arguments
    class(ListType), target, intent(inout) :: this
    type(ListNodeType), pointer, intent(inout) :: fromNode
    ! local variables
    type(ListNodeType), pointer :: current => null()
    type(ListNodeType), pointer :: prev => null()
    !
    if (associated(fromNode)) then
      ! -- reassign firstNode
      if (associated(fromNode%nextNode)) then
        this%firstNode => fromNode%nextNode
      else
        this%firstNode => null()
      endif
      ! -- deallocate fromNode and all previous nodes
      current => fromNode
      do while (associated(current))
        prev => current%prevNode
        call current%DeallocValue()
        deallocate(current)
        this%nodeCount = this%nodeCount - 1
        current => prev
      enddo
      fromNode => null()
    endif
    !
    return
  end subroutine deallocate_backward

  function get_current_value(this) result(resultobj)
    implicit none
    class(ListType), target, intent(inout) :: this
    ! result
    class(*), pointer :: resultobj
    !
    resultobj => null()
    if (associated(this%currentNode)) then
      resultobj => this%currentNode%Value
    endif
    return
  end function get_current_value

  function get_next_value(this) result(resultobj)
    implicit none
    class(ListType), target, intent(inout) :: this
    ! result
    class(*), pointer :: resultobj
    !
    call this%next_node()
    resultobj => this%get_current_value()
    return
  end function get_next_value

  function get_previous_value(this) result(resultobj)
    implicit none
    class(ListType), target, intent(inout) :: this
    ! result
    class(*), pointer :: resultobj
    !
    call this%previous_node()
    resultobj => this%get_current_value()
    return
  end function get_previous_value

  function get_value_by_index(this, indx) result(resultobj)
    ! **************************************************************************
    ! get_value_by_index (implements GetValue)
    ! Return object stored in ListNodeType%Value by index in list
    ! **************************************************************************
    !
    !    SPECIFICATIONS:
    ! --------------------------------------------------------------------------
    implicit none
    ! dummy arguments
    class(ListType), intent(inout) :: this
    integer, intent(in) :: indx
    ! result
    class(*), pointer :: resultobj
    ! local variables
    integer :: i
    !
    ! -- Initialize
    resultobj => null()
    !
    ! -- Ensure that this%currentNode is associated
    if (this%currentNodeIndex == 0) then
      if (associated(this%firstNode)) then
        this%currentNode => this%firstNode
        this%currentNodeIndex = 1
      endif
    endif
    !
    ! -- Check indx position relative to current node index
    i = 0
    if (indx < this%currentNodeIndex) then
      ! Start at beginning of list
      call this%reset_current_node()
      if (associated(this%firstNode)) then
        this%currentNode => this%firstNode
        this%currentNodeIndex = 1
        i = 1
      endif
    else
      i = this%currentNodeIndex
    endif
    if (i == 0) return
    !
    ! -- If current node is requested node,
    !    assign pointer and return
    if (i==indx) then
      resultobj => this%currentNode%Value
      return
    endif
    !
    ! -- Iterate from current node to requested node
    do while (associated(this%currentNode%nextNode))
      this%currentNode => this%currentNode%nextNode
      this%currentNodeIndex = this%currentNodeIndex + 1
      if (this%currentNodeIndex==indx) then
        resultobj => this%currentNode%Value
        return
      endif
    enddo
    return
  end function get_value_by_index

  subroutine next_node(this)
    implicit none
    class(ListType), target, intent(inout) :: this
    !
    if (this%currentNodeIndex == 0) then
      this%currentNode => this%firstNode
      this%currentNodeIndex = 1
    else
      this%currentNode => this%currentNode%nextNode
      this%currentNodeIndex = this%currentNodeIndex + 1
    endif
    return
  end subroutine next_node

  subroutine previous_node(this)
    implicit none
    class(ListType), target, intent(inout) :: this
    !
    if (this%currentNodeIndex <= 1) then
      call this%reset_current_node()
    else
      this%currentNode => this%currentNode%prevNode
      this%currentNodeIndex = this%currentNodeIndex - 1
    endif
    return
  end subroutine previous_node

  subroutine reset_current_node(this)
    implicit none
    class(ListType), target, intent(inout) :: this
    !
    this%currentNode => null()
    this%currentNodeIndex = 0
    return
  end subroutine reset_current_node

  ! -- Type-bound procedures for ListNodeType

  subroutine list_node_dealloc_value(this)
    ! ************************************************************************
    ! Deallocate whatever is stored in the Value component of this node.
    ! ************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------
    implicit none
    class(ListNodeType), intent(inout) :: this
    !
    if (associated(this%Value)) then
      deallocate(this%Value)
    endif
    return
  end subroutine list_node_dealloc_value

  function list_node_get_value(this) result(valueObject)
    ! ************************************************************************
    ! Perform a pointer assignment of valueObject to the contents of
    ! this%Value
    ! ************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------
    implicit none
    class(ListNodeType), intent(inout) :: this
    class(*), pointer :: valueObject
    !
    valueObject => this%Value
    return
  end function list_node_get_value

  subroutine list_node_set_value(this, value)
    ! ************************************************************************
    ! Assign the contents of value to the object contained in any type derived
    ! from LinkableNodeType. Perform a pointer assignment of this%Value to
    ! the data contained in the object.
    ! -- Needs to be called instead of just using a pointer to assign
    !    the value.
    ! -- Needs to make this%Value point to data to be contained
    ! ************************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------------
    class(ListNodeType), intent(inout) :: this
    class(*), pointer,   intent(in)    :: value
    this%Value => value
    return
  end subroutine list_node_set_value

end module ListModule

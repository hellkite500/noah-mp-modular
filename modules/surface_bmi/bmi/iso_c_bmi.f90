! The Basic Model Interface (BMI) Fortran specification.
!
! This language specification is derived from the Scientific
! Interface Definition Language (SIDL) file bmi.sidl located at
! https://github.com/csdms/bmi.

module iso_c_bmif_2_0
  use bmif_2_0
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_char, c_null_char, c_int, c_double, c_float
  implicit none

  type box
    integer :: foobar
    class(bmi), pointer :: ptr => null()
  end type

  type string
    character(kind=c_char), allocatable :: item
  end type

  contains
    pure function c_to_f_string(c_string) result(f_string)
      implicit none
      character(kind=c_char, len=1), intent(in) :: c_string(:)
      character(len=:), allocatable :: f_string
      integer i,n

      !loop  through the c_string till terminator is found
      i = 1
      do
        if (c_string(i) == c_null_char) then 
            exit
        else
           i = i+1
        end if
      end do
      n = i - 1 ! trim terminator
      allocate(character(len=n) :: f_string)
      f_string = transfer( c_string(1:n), f_string )
    end function c_to_f_string

    pure function f_to_c_string(f_string) result(c_string)
      implicit none
      character(len=*), intent(in) :: f_string
      !Create a C compatable character array with room for a null terminator
      character(kind=c_char, len=1), dimension( len_trim(f_string) + 1 ) :: c_string

      !loop through the string, copy each char
      integer i,n
      n = len_trim(f_string)
      do i = 1, n
        c_string(i) = f_string(i:i)
      end do
      c_string(n+1) = c_null_char !make sure to add null terminator
    end function f_to_c_string

    ! Perform startup tasks for the model.
    function initialize(this, config_file) result(bmi_status) bind(C, name="initialize")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_FILE_NAME), intent(in) :: config_file
      integer(kind=c_int) :: bmi_status
      character(kind=c_char, len=:), allocatable :: f_file 
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      !convert c style string to fortran character array
      f_file = c_to_f_string(config_file)
      bmi_status = bmi_box%ptr%initialize(f_file)
    end function initialize

    ! Advance the model one time step.
    function update(this) result(bmi_status) bind(C, name="update")
      type(c_ptr) :: this
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%update()
    end function update

    ! Advance the model until the given time.
    function update_until(this, time) result(bmi_status) bind(C, name="update_until")
      type(c_ptr) :: this
      real(kind=c_double), intent(in) :: time
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%update_until(time)
    end function update_until

    ! Perform teardown tasks for the model.
    function finalize(this) result(bmi_status) bind(C, name="finalize")
      type(c_ptr) :: this
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%finalize()
      !clean up the wrapper
      !deallocate(bmi_box%ptr)
      deallocate(bmi_box)
    end function finalize

    ! Get the name of the model.
    function get_component_name(handle, name) result(bmi_status) bind(C, name="get_component_name")
      type(c_ptr) :: handle
      character(kind=c_char, len=1), dimension(*), intent(out) :: name
      character(kind=c_char, len=BMI_MAX_COMPONENT_NAME), pointer :: f_name
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(handle, bmi_box)
      bmi_status = bmi_box%ptr%get_component_name(f_name)
      !Set the c_string input (name), make sure to inlcude the null_terminator
      name(:len_trim(f_name)+1) = f_to_c_string(f_name)
    end function get_component_name

    ! Count the input variables.
    function get_input_item_count(this, count) result (bmi_status) bind(C, name="get_input_item_count")
      type(c_ptr) :: this
      integer(kind=c_int), intent(out) :: count
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_input_item_count(count)
    end function get_input_item_count

    ! Count the output variables.
    function get_output_item_count(this, count) result (bmi_status) bind(C, name="get_output_item_count")
      type(c_ptr) :: this
      integer(kind=c_int), intent(out) :: count
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_output_item_count(count)
    end function get_output_item_count

    ! List a model's input variables.
    function get_input_var_names(this, names) result(bmi_status) bind(C, name="get_input_var_names")
      type(c_ptr) :: this
      type(c_ptr), intent(inout)  :: names (*)
      character(kind=c_char, len=BMI_MAX_FILE_NAME), pointer :: f_names(:)
      character(kind=c_char, len=1), pointer :: c_buff_ptr(:)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      integer :: i

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      bmi_status = bmi_box%ptr%get_input_var_names(f_names)
      !print *, size(f_names)
      do i = 1, size(f_names)
        !For each pointer (one for each name), associate c_buff_ptr with the string names points to
        call c_f_pointer(names(i), c_buff_ptr, [ BMI_MAX_COMPONENT_NAME ] )
        !print *, c_to_f_string(c_buff_ptr)
        !assign the c_string to buffer
        c_buff_ptr = f_to_c_string(f_names(i))
      end do

    end function get_input_var_names

    ! List a model's output variables.
    function get_output_var_names(this, names) result(bmi_status) bind(C, name="get_output_var_names")
      type(c_ptr) :: this
      type(c_ptr), intent(inout)  :: names (*)
      character(kind=c_char, len=BMI_MAX_FILE_NAME), pointer :: f_names(:)
      character(kind=c_char, len=1), pointer :: c_buff_ptr(:)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box
      integer :: i

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)

      bmi_status = bmi_box%ptr%get_output_var_names(f_names)
      !print *, size(f_names)
      do i = 1, size(f_names)
        !For each pointer (one for each name), associate c_buff_ptr with the string names points to
        call c_f_pointer(names(i), c_buff_ptr, [ BMI_MAX_COMPONENT_NAME ] )
        !print *, c_to_f_string(c_buff_ptr)
        !assign the c_string to buffer
        c_buff_ptr = f_to_c_string(f_names(i))
      end do
    end function get_output_var_names

    ! Get the grid identifier for the given variable.
    function get_var_grid(this, name, grid) result(bmi_status) bind(C, name="get_var_grid")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      integer(kind=c_int), intent(out) :: grid
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_var_grid(c_to_f_string(name), grid)

    end function get_var_grid

    ! Get the data type of the given variable as a string.
    function get_var_type(this, name, type) result(bmi_status) bind(C, name="get_var_type")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      character(kind=c_char, len=1), intent(out) :: type (*)
      character(kind=c_char, len=BMI_MAX_COMPONENT_NAME) :: f_type
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_var_type(c_to_f_string(name), f_type)
      type(1:len_trim(f_type)+1) = f_to_c_string(f_type)

    end function get_var_type

    ! Get the units of the given variable.
    function get_var_units(this, name, units) result(bmi_status) bind(C, name="get_var_units")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      character(kind=c_char, len=1), intent(out) :: units (*)
      character(kind=c_char, len=BMI_MAX_COMPONENT_NAME) :: f_units
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_var_units(c_to_f_string(name), f_units)
      units(1:len_trim(f_units)+1) = f_to_c_string(f_units)
    end function get_var_units

    ! Get memory use per array element, in bytes.
    function get_var_itemsize(this, name, size) result(bmi_status) bind(C, name="get_var_itemsize")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      integer(kind=c_int), intent(out) :: size
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_var_itemsize(c_to_f_string(name), size)
    end function get_var_itemsize

    ! Get size of the given variable, in bytes.
    function get_var_nbytes(this, name, nbytes) result(bmi_status) bind(C, name="get_var_nbytes")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      integer(kind=c_int), intent(out) :: nbytes
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_var_nbytes(c_to_f_string(name), nbytes)
    end function get_var_nbytes

    ! Describe where a variable is located: node, edge, or face.
    function get_var_location(this, name, location) result(bmi_status) bind(C, name="get_var_location")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      character(kind=c_char, len=1), intent(out) :: location (*)
      character(kind=c_char, len=BMI_MAX_COMPONENT_NAME) :: f_location
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_var_location(c_to_f_string(name), f_location)
      location(1:len_trim(f_location)+1) = f_to_c_string(f_location)
    end function get_var_location

    ! Current time of the model.
    function get_current_time(this, time) result(bmi_status) bind(C, name="get_current_time")
      type(c_ptr) :: this
      real(kind=c_double), intent(out) :: time
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_current_time(time)
    end function get_current_time

    ! Start time of the model.
    function get_start_time(this, time) result(bmi_status) bind(C, name="get_start_time")
      type(c_ptr) :: this
      real(kind=c_double), intent(out) :: time
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_start_time(time)
    end function get_start_time

    ! End time of the model.
    function get_end_time(this, time) result(bmi_status) bind(C, name="get_end_time")
      type(c_ptr) :: this
      real(kind=c_double), intent(out) :: time
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_end_time(time)
    end function get_end_time

    ! Time units of the model.
    function get_time_units(this, units) result(bmi_status) bind(C, name="get_time_units")
      type(c_ptr) :: this
      character(kind=c_char, len=1), intent(out) :: units (*)
      character(kind=c_char, len=BMI_MAX_COMPONENT_NAME) :: f_units
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_time_units(f_units)
      units(1:len_trim(f_units)+1) = f_to_c_string(f_units)
    end function get_time_units

    ! Time step of the model.
    function get_time_step(this, time_step) result(bmi_status) bind(C, name="get_time_step")
      type(c_ptr) :: this
      real(kind=c_double), intent(out) :: time_step
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_time_step(time_step)
    end function get_time_step

    ! Get a copy of values (flattened!) of the given integer variable.
    function get_value_int(this, name, dest) result(bmi_status) bind(C, name="get_value_int")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      integer(kind=c_int) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      !FIXME this assumes dest has 1 element...what happends when it doesnt?
      !How to determine how big this flattened array should be apriori?
      bmi_status = bmi_box%ptr%get_value_int(c_to_f_string(name), dest(:1))
      
    end function get_value_int

    ! Get a copy of values (flattened!) of the given real variable.
    function get_value_float(this, name, dest) result(bmi_status) bind(C, name="get_value_float")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      real(kind=c_float) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      !FIXME this assumes dest has 1 element...what happends when it doesnt?
      !How to determine how big this flattened array should be apriori?
      bmi_status = bmi_box%ptr%get_value_float(c_to_f_string(name), dest(:1))
    end function get_value_float

    ! Get a copy of values (flattened!) of the given double variable.
    function get_value_double(this, name, dest) result(bmi_status) bind(C, name="get_value_double")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      real(kind=c_double) :: dest(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      !FIXME this assumes dest has 1 element...what happends when it doesnt?
      !How to determine how big this flattened array should be apriori?
      bmi_status = bmi_box%ptr%get_value_double(c_to_f_string(name), dest(:1))
    end function get_value_double

    ! Set new values for an integer model variable.
    function set_value_int(this, name, src) result(bmi_status) bind(C, name="set_value_int")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      integer(kind=c_int) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      !FIXME this assumes src has 1 element...what happends when it doesnt?
      !How to determine how big this flattened array should be apriori?
      bmi_status = bmi_box%ptr%set_value_int(c_to_f_string(name), src(:1))
    end function set_value_int

    ! Set new values for a real model variable.
    function set_value_float(this, name, src) result(bmi_status) bind(C, name="set_value_float")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      real(kind=c_float) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      !FIXME this assumes dest has 1 element...what happends when it doesnt?
      !How to determine how big this flattened array should be apriori?
      bmi_status = bmi_box%ptr%set_value_float(c_to_f_string(name), src(:1))
    end function set_value_float

    ! Set new values for a double model variable.
    function set_value_double(this, name, src) result(bmi_status) bind(C, name="set_value_double")
      type(c_ptr) :: this
      character(kind=c_char, len=1), dimension(BMI_MAX_COMPONENT_NAME), intent(in) :: name
      real(kind=c_double) :: src(*)
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      !FIXME this assumes dest has 1 element...what happends when it doesnt?
      !How to determine how big this flattened array should be apriori?
      bmi_status = bmi_box%ptr%set_value_double(c_to_f_string(name), src(:1))
    end function set_value_double

    ! Get number of dimensions of the computational grid.
    function get_grid_rank(this, grid, rank) result(bmi_status) bind(C, name="get_grid_rank")
      type(c_ptr) :: this
      integer(kind=c_int), intent(in) :: grid
      integer(kind=c_int), intent(out) :: rank
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_grid_rank(grid, rank)
    end function get_grid_rank

    ! Get the total number of elements in the computational grid.
    function get_grid_size(this, grid, size) result(bmi_status) bind(C, name="get_grid_size")
      type(c_ptr) :: this
      integer(kind=c_int), intent(in) :: grid
      integer(kind=c_int), intent(out) :: size
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_grid_size(grid, size)
    end function get_grid_size

    ! Get the grid type as a string.
    function get_grid_type(this, grid, type) result(bmi_status) bind(C, name="get_grid_type")
      type(c_ptr) :: this
      integer(kind=c_int), intent(in) :: grid
      character(kind=c_char, len=1), intent(out) :: type (*)
      character(kind=c_char, len=BMI_MAX_COMPONENT_NAME) :: f_type
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(box), pointer :: bmi_box

      !extract the fortran type from handle
      call c_f_pointer(this, bmi_box)
      bmi_status = bmi_box%ptr%get_grid_type(grid, f_type)
      type(1:len_trim(f_type)+1) = f_to_c_string(f_type)
    end function get_grid_type

    function register_bmi(this) result(bmi_status) bind(C, name="register_bmi")
      use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
      use bminoahmp
      implicit none
      type(c_ptr) :: this ! If not value, then from the C perspective `this` is a void**
      integer(kind=c_int) :: bmi_status
      !Create the momdel instance to use
      type(bmi_noahmp), target, save :: bmi_model !need to ensure scope/lifetime, use save attribute
      !Create a simple pointer wrapper
      type(box), pointer :: bmi_box

      !allocate the pointer box
      allocate(bmi_box)
      !allocate(bmi_box%ptr, source=bmi_model)
      bmi_box%foobar = 42 !test var FIXME remove
      !associate the wrapper pointer the created model instance
      bmi_box%ptr => bmi_model
      !Return the pointer to box
      this = c_loc(bmi_box)
      bmi_status = BMI_SUCCESS
    end function register_bmi

end module iso_c_bmif_2_0

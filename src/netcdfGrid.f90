module netCDF_utilities
  
  use netcdf
  use NamelistRead, only: namelist_type
  use bmi_grid

  implicit none
  save
  integer, parameter :: max_file_name_length = 512
  integer, parameter :: max_var_name_length = 256

  type, public :: netcdf2DMeta
    character*256                      :: name_att_dx         ! name of dx attribute, which must be associated with x dimension variable 
    character*256                      :: name_att_dy         ! name of dy attribute, which must be associated with y dimension variable 
    character*256                      :: name_dim_x          ! name of x dimension in netcdf file (e.g., 'Longitude'). Assume netcdf variable holding x dim coordinates (i.e., variable id = varid_x) has same name.
    character*256                      :: name_dim_y          ! name of y dimension in netcdf file (e.g., 'Latitude'). Assume netcdf variable holding y dim coordinates (i.e., variable id = varid_y) has same name.
    character*256                      :: name_var            ! name of variable in netcdf file
  end type

  type, public ::netcdf2DVar
    type(GridType) :: grid
    type(netcdf2DMeta) :: netcdf_attributes
    !FIXME is holding the data ref a good idea?
    integer,allocatable,dimension(:,:) :: data             ! vegetation type
    integer                            :: integerMissing 
    real                               :: realMissing   
    character(len=12)                  :: stringMissing
    character(len=max_var_name_length)                 :: name
    character(len=max_file_name_length) :: filename ! filename variables is read from
    ! An undocumented, but reasonable assumption...https://www.unidata.ucar.edu/support/help/MailArchives/netcdf/msg13254.html
    ! so an ncid < 0 will be assumed to be not open/used/associated ect...
    integer :: ncid = -1 ! netcdf id to pass to netcdf fucntions once `nf90_open` has been called

    contains
    procedure, public :: Init
    procedure, public :: Finalize ! Clean up netcdf resources
    procedure, public  :: ReadGridData
    procedure, private :: ReadSpatial
    procedure, private :: ReadVegtyp

  end type

contains

  subroutine Init(this, namelist, filename)
    class(netcdfGridVar)               :: this
    type(namelist_type),intent(in)     :: namelist
    character(len=max_file_name_length), intent(in) :: filename
    integer                            :: status ! netcdf status flag
    logical                            :: lexist ! flag for file existance

    this%filename = filename
    !----------------------------------------------------------------------------
    ! Set expected names for all NetCDF-defined variables, dimensions and attributes
    !----------------------------------------------------------------------------
    this%netcdf_attributes%name_att_dx     = 'dx'    
    this%netcdf_attributes%name_att_dy     = 'dy'       
    this%netcdf_attributes%name_dim_x      = 'Longitude'
    this%netcdf_attributes%name_dim_y      = 'Latitude'  
    !FIXME take vegtyp as 
    this%netcdf_attributes%name_var = 'vegtyp' 

    !----------------------------------------------------------------------------
    ! Use missing values from argument namelist_type
    !----------------------------------------------------------------------------
    this%integerMissing  = namelist%integerMissing
    this%realMissing     = namelist%realMissing
    this%stringMissing   = namelist%stringMissing

    !----------------------------------------------------------------------------
    ! Check that file exists and open it
    !----------------------------------------------------------------------------
    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
      write(*,*) 'ERROR Could not find ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif
    ! Store the ncid file handle for future calls to `nf90_` calls
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = this%ncid)
    if (status /= nf90_noerr) then
      write(*,*) 'ERROR Could not open ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif

    !----------------------------------------------------------------------------
    ! Read spatial information for file
    !----------------------------------------------------------------------------
    call this%ReadSpatial(this%ncid)
    end associate

  end subroutine

  subroutine Finlize(this)
    class(netcdfGridVar)               :: this
    integer status
    !----------------------------------------------------------------------------
    ! Close file
    !----------------------------------------------------------------------------
    !An undocumented, but reasonable assumption...https://www.unidata.ucar.edu/support/help/MailArchives/netcdf/msg13254.html
    if (this%ncid > 0) then
      status = nf90_close(this%ncid)
      if (status /= nf90_noerr) then
        write(*,*) 'Unable to close ''',trim(this%filename),''''; stop ":  ERROR EXIT" 
      end if
      this%ncid = -1
    end if
  end subroutine

  subroutine ReadVegtyp(this,filename)

    class(gridinfo_type)               :: this
    character*256,intent(in)           :: filename 
    logical                            :: lexist
    integer                            :: status       
    integer                            :: ix, iy
    integer                            :: ncid_vegtyp  ! NetCDF file ID number
    integer                            :: varid_vegtyp ! NetCDF variable ID number for vegtyp variable
    integer,allocatable,dimension(:,:) :: read_vegtyp  ! Local array to hold read-in vegtyp values before transferring to this%vegtyp

    associate(integerMissing  => this%integerMissing, &
              realMissing     => this%realMissing,    &
              stringMissing   => this%stringMissing,  &
              name_var_vegtyp => this%name_var_vegtyp)

    !----------------------------------------------------------------------------
    ! Check that file exists and open it
    !----------------------------------------------------------------------------
    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
      write(*,*) 'ERROR Could not find ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif
    status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid_vegtyp)
    if (status /= nf90_noerr) then
      write(*,*) 'ERROR Could not open ''',trim(filename),''''; stop ":  ERROR EXIT"
    endif

    !----------------------------------------------------------------------------
    ! Read spatial information for file
    !----------------------------------------------------------------------------
    call this%ReadSpatial(filename,ncid_vegtyp)

    !----------------------------------------------------------------------------
    ! Get vegtyp variable id
    !----------------------------------------------------------------------------
    status = nf90_inq_varid(ncid_vegtyp, name_var_vegtyp, varid_vegtyp)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to find the variable ''',trim(name_var_vegtyp),''' in ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Allocate and set local vegtyp array to missing values
    !----------------------------------------------------------------------------
    allocate(read_vegtyp(this%n_x,this%n_y))
    read_vegtyp(:,:) = this%integerMissing

    !----------------------------------------------------------------------------
    ! Read vegtyp from file
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid_vegtyp,varid_vegtyp,read_vegtyp)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to read variable ''',trim(name_var_vegtyp),''' from ''',trim(filename),''''
      stop ":  ERROR EXIT"
    end if

    !----------------------------------------------------------------------------
    ! Allocate vegtyp array for 'this' instance of gridinfo_type
    !----------------------------------------------------------------------------
    allocate(this%vegtyp(this%n_x,this%n_y))

    !----------------------------------------------------------------------------
    ! Check and then transfer read-in values to gridinfo_type 
    !----------------------------------------------------------------------------
    if(read_vegtyp(1,1) /= integerMissing) then
      this%vegtyp(:,:) = read_vegtyp(:,:)
    else 
      write(*,*) 'ERROR : problem reading vegtyp from ''',trim(filename),''''; stop
    end if

    !----------------------------------------------------------------------------
    ! Close file
    !----------------------------------------------------------------------------
    status = nf90_close(ncid_vegtyp)
    if (status /= nf90_noerr) then
      write(*,*) 'Unable to close ''',trim(filename),''''; stop ":  ERROR EXIT" 
    end if

    end associate

  end subroutine ReadVegtyp 

  subroutine ReadSpatial(this,ncid)

    class(gridinfo_type)           :: this
    integer,intent(in)             :: ncid
    integer                        :: status
    integer                        :: ix, iy
    integer                        :: read_nx     ! read-in n_x value
    integer                        :: read_ny     ! read-in n_y value
    real                           :: read_dx     ! read-in dx value
    real                           :: read_dy     ! read-in dy value
    real,allocatable,dimension(:)  :: read_lon    ! read-in longitude values along x dimensional grid edge
    real,allocatable,dimension(:)  :: read_lat    ! read-in latitude values along y dimensional grid edge
    integer                        :: varid_x     ! netcdf variable id for netcdf variable holding x dim coordinates (longitudes) for uniform_rectilinear grid
    integer                        :: varid_y     ! netcdf variable id for netcdf variable holding y dim coordinates (latitudes) for uniform_rectilinear grid
    integer                        :: dimid_x     ! netcdf dimension id for netcdf dimension for x dimension (longitude dimension)
    integer                        :: dimid_y     ! netcdf dimension id for netcdf dimension for y dimension (latitude dimension)
    
    associate(integerMissing => this%integerMissing, &
              realMissing    => this%realMissing,    &
              stringMissing  => this%stringMissing,  &
              name_att_dx    => this%netcdf2DMeta%name_att_dx,    &
              name_att_dy    => this%netcdf2DMeta%name_att_dy,    &
              name_dim_x     => this%netcdf2DMeta%name_dim_x,     &
              name_dim_y     => this%netcdf2DMeta%name_dim_y,     &
              filename       => this%filename)

    !----------------------------------------------------------------------------
    ! Initialize to missing values
    !----------------------------------------------------------------------------
    read_nx = integerMissing
    read_ny = integerMissing
    read_dx = realMissing
    read_dy = realMissing
    varid_x = integerMissing
    varid_y = integerMissing
    dimid_x = integerMissing
    dimid_y = integerMissing

    !----------------------------------------------------------------------------
    ! Read n_x and n_y
    !----------------------------------------------------------------------------
    !!!!!!
    ! NJF FIXME relax nx/ny requirements
    ! so we can search a netcdf file for a "subgrid" and read appropriately
    ! Would only need to validate that the grid requested is a contained within the grid in the netcdf description
    !!!!!!
    status = nf90_inq_dimid(ncid, trim(name_dim_x), dimid_x)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find dimension ''',trim(name_dim_x),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_inq_dimid(ncid, trim(name_dim_y), dimid_y)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find dimension ''',trim(name_dim_y),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid, dimid_x, len=read_nx)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read length of dimension ''',trim(name_dim_x),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    status = nf90_inquire_dimension(ncid, dimid_y, len=read_ny)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read length of dimension ''',trim(name_dim_y),''' in ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Read dx and dy
    !----------------------------------------------------------------------------
    status = nf90_inq_varid(ncid, trim(name_dim_x), varid_x)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find variable ''',trim(name_dim_x),''' in ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inq_varid(ncid, trim(name_dim_y), varid_y)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find variable ''',trim(name_dim_y),''' in ''',trim(filename),'''';  stop ":  ERROR EXIT"; end if
    status = nf90_inquire_attribute(ncid=ncid,varid=varid_x,name=name_att_dx)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find attribute ''',trim(name_att_dx),''' with variable ''',trim(name_dim_x),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_inquire_attribute(ncid=ncid,varid=varid_y,name=name_att_dy)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to find attribute ''',trim(name_att_dy),''' with variable ''',trim(name_dim_y),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_get_att(ncid=ncid, varid=varid_x, name=name_att_dx, values=read_dx)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read attribute ''',trim(name_att_dx),''' with variable ''',trim(name_dim_x),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if
    status = nf90_get_att(ncid=ncid, varid=varid_y, name=name_att_dy, values=read_dy)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read attribute ''',trim(name_att_dy),''' with variable ''',trim(name_dim_y),''' in ''',trim(filename),''''; stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Allocate local arrays
    !----------------------------------------------------------------------------
    allocate(read_lon(read_nx))
    allocate(read_lat(read_ny))

    !----------------------------------------------------------------------------
    ! Read latitude and longitude values
    !----------------------------------------------------------------------------
    status = nf90_get_var(ncid,varid_x,read_lon)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(name_dim_x),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if
    status = nf90_get_var(ncid,varid_y,read_lat)
    if (status /= nf90_noerr) then; write(*,*) 'Unable to read variable ''',trim(name_dim_y),''' from ''',trim(filename),'''';stop ":  ERROR EXIT"; end if

    !----------------------------------------------------------------------------
    ! Using allocated(this%lat) and allocated(this%lon) as indicators of a
    ! previous call to ReadSpatial. If no previous call to ReadSpatial, then 
    ! allocate this%lat and this%lon and then transfer values for all read-in 
    ! variables to 'this' instance of gridinfo_type. If this call is not the 
    ! first call to ReadSpatial, then check that read-in values match what was 
    ! previously transfered to 'this' instance of gridinfo_type.
    !----------------------------------------------------------------------------
    if((.NOT.(allocated(this%lat))).AND.(.NOT.(allocated(this%lon)))) then

      !----------------------------------------------------------------------------
      ! Allocate
      !----------------------------------------------------------------------------
      allocate(this%lon(read_nx,read_ny))
      allocate(this%lat(read_nx,read_ny))

      !----------------------------------------------------------------------------
      ! Transfer values
      !----------------------------------------------------------------------------
      if(read_nx /= integerMissing) then; this%n_x = read_nx; else; write(*,*) 'ERROR : problem reading n_x from ''',trim(filename),''''; stop; end if
      if(read_ny /= integerMissing) then; this%n_y = read_ny; else; write(*,*) 'ERROR : problem reading n_y from ''',trim(filename),''''; stop; end if
      if(read_dx /= realMissing)    then; this%dx  = read_dx; else; write(*,*) 'ERROR : problem reading dx from ''', trim(filename),''''; stop; end if
      if(read_dy /= realMissing)    then; this%dy  = read_dy; else; write(*,*) 'ERROR : problem reading dy from ''', trim(filename),''''; stop; end if
      if(read_lon(1) /= realMissing) then
        do ix = 1, read_nx
          this%lon(ix,:) = read_lon(ix)
        end do
      else; write(*,*) 'ERROR : problem reading lon from ''',trim(filename),''''; stop; end if
      if(read_lat(1) /= realMissing) then
        do iy = 1, read_ny
          this%lat(:,iy) = read_lat(iy)
        end do
      else; write(*,*) 'ERROR : problem reading lon from ''',trim(filename),''''; stop; end if
    
    else 

      !----------------------------------------------------------------------------
      ! Check that values match
      !----------------------------------------------------------------------------
      if(read_nx /= this%n_x) then; write(*,*) 'ERROR : n_x from ''',trim(filename),''' does not match n_x from other gridded inputs'; stop; end if
      if(read_ny /= this%n_y) then; write(*,*) 'ERROR : n_y from ''',trim(filename),''' does not match n_y from other gridded inputs'; stop; end if
      if(read_dx /= this%dx)  then; write(*,*) 'ERROR : dx from ''', trim(filename),''' does not match dx from other gridded inputs';  stop; end if
      if(read_dy /= this%dy)  then; write(*,*) 'ERROR : dy from ''', trim(filename),''' does not match dy from other gridded inputs';  stop; end if
      do ix = 1, read_nx
        if (all(read_lon(ix) /= this%lon(ix,:))) then; write(*,*) 'ERROR : lat values from ''', trim(filename),''' do not match lat values from other gridded inputs';  stop; end if
      end do
      do iy = 1, read_ny
        if (all(read_lat(iy) /= this%lon(:,iy))) then; write(*,*) 'ERROR : lat values from ''', trim(filename),''' do not match lat values from other gridded inputs';  stop; end if
      end do

    end if

    end associate

  end subroutine ReadSpatial


end module netCDF_utilities

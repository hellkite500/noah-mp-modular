module DomainGridType
  
  use NamelistRead, only: namelist_type
  use GridInfoType, only: gridinfo_type
  use DateTimeUtilsModule
  
  implicit none
  save
  private
  
  type, public :: domaingrid_type
  
    real                                  :: DT                ! run timestep (s)
    integer                               :: n_x               !
    integer                               :: n_y               !
    real                                  :: dx
    real                                  :: dy
    character(len=12)                     :: startdate         ! Start date of the model run ( YYYYMMDDHHmm ) 
    character(len=12)                     :: enddate           ! End date of the model run ( YYYYMMDDHHmm ) 
    character(len=12)                     :: nowdate           ! Current date of the model run ( YYYYMMDDHHmm ) 
    real*8                                :: start_datetime    ! unix start datetime (s since 1970-01-01 00:00:00) ?UTC? 
    real*8                                :: end_datetime      ! unix end datetime (s since 1970-01-01 00:00:00) ?UTC? 
    real*8                                :: curr_datetime     ! unix current datetime (s since 1970-01-01 00:00:00) ?UTC? 
    real*8, allocatable, dimension(:)     :: sim_datetimes     ! vector of unix sim times given start/end dates and dt (try 'ki8' type)
    integer                               :: itime             ! current integer time step of model run
    integer                               :: ntime             ! total number of integer time steps in model run
    double precision                      :: time_dbl          ! current time of model run in seconds from beginning
    real, allocatable, dimension(:,:)     :: lat               ! latitude (°)
    real, allocatable, dimension(:,:)     :: lon               ! longitude (°)
    real                                  :: ZREF              ! measurement height of wind speed (m)
    real, allocatable, dimension(:,:)     :: terrain_slope     ! terrain slope (°)
    real, allocatable, dimension(:,:)     :: azimuth           ! terrain azimuth or aspect (° clockwise from north)
    integer, allocatable, dimension(:,:)  :: vegtyp            ! land cover type
    integer, allocatable, dimension(:,:)  :: croptype          ! crop type
    integer, allocatable, dimension(:,:)  :: isltyp            ! soil type
    integer, allocatable, dimension(:,:)  :: IST               ! surface type 1-soil; 2-lake
    real, allocatable, dimension(:,:,:)   :: zsoil             ! depth of layer-bottom from soil surface
    real, allocatable, dimension(:,:,:)   :: dzsnso            ! snow/soil layer thickness [m]
    real, allocatable, dimension(:,:,:)   :: zsnso             ! depth of snow/soil layer-bottom
    integer, allocatable, dimension(:,:)  :: soilcolor         !
  
    contains
  
    procedure, public  :: Init         
    procedure, private :: InitAllocate 
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer
    procedure, public :: Reallocate
  
  end type 
  
  contains   
  
    subroutine Init(this, namelist, gridinfo)
  
      class(domaingrid_type) :: this
      type(namelist_type)    :: namelist
      type(gridinfo_type)    :: gridinfo
  
      call this%InitAllocate(namelist,gridinfo)
      call this%InitDefault()
  
    end subroutine Init
  
    subroutine Reallocate(this, grid)

    end subroutine
  
    subroutine InitAllocate(this, namelist, gridinfo)
  
      class(domaingrid_type)          :: this
      type(namelist_type), intent(in) :: namelist
      type(gridinfo_type), intent(in) :: gridinfo
  
      associate(n_x   => gridinfo%n_x,   &
                n_y   => gridinfo%n_y,   &
                nsoil => namelist%nsoil, &
                nsnow => namelist%nsnow)

      allocate(this%lat             (n_x,n_y))
      allocate(this%lon             (n_x,n_y))
      allocate(this%terrain_slope   (n_x,n_y))
      allocate(this%azimuth         (n_x,n_y))
      allocate(this%vegtyp          (n_x,n_y))
      allocate(this%croptype        (n_x,n_y))
      allocate(this%isltyp          (n_x,n_y))
      allocate(this%IST             (n_x,n_y))
      allocate(this%soilcolor       (n_x,n_y))
      allocate(this%zsoil           (n_x,n_y,nsoil))                   
      allocate(this%dzsnso          (n_x,n_y,-nsnow+1:nsoil)) 
      allocate(this%zsnso           (n_x,n_y,-nsnow+1:nsoil)) 

      end associate
  
    end subroutine InitAllocate
  
    subroutine InitDefault(this)
  
      class(domaingrid_type) :: this
  
      this%dt                  = huge(1.0)
      this%n_x                 = huge(1)
      this%n_y                 = huge(1)
      this%startdate           = 'EMPTYDATE999'
      this%enddate             = 'EMPTYDATE999'
      this%nowdate             = 'EMPTYDATE999'
      this%start_datetime      = huge(1)
      this%end_datetime        = huge(1)
      this%curr_datetime       = huge(1)
      this%itime               = huge(1) 
      this%ntime               = huge(1) 
      this%time_dbl            = huge(1.d0)
      this%lat(:,:)            = huge(1.0)
      this%lon(:,:)            = huge(1.0)
      this%terrain_slope(:,:)  = huge(1.0)
      this%azimuth(:,:)        = huge(1.0)
      this%ZREF                = huge(1.0)
      this%vegtyp(:,:)         = huge(1)
      this%croptype(:,:)       = huge(1)
      this%isltyp(:,:)         = huge(1)
      this%IST(:,:)            = huge(1)
      this%zsoil(:,:,:)        = huge(1)
      this%dzsnso(:,:,:)       = huge(1)
      this%zsnso(:,:,:)        = huge(1)

    end subroutine InitDefault
  
    subroutine InitTransfer(this,namelist,gridinfo)
  
      class(domaingrid_type)         :: this
      type(namelist_type),intent(in) :: namelist
      type(gridinfo_type),intent(in) :: gridinfo
      integer                        :: ii

      this%dt                   = namelist%dt
      this%dx                   = gridinfo%dx
      this%dy                   = gridinfo%dy
      this%n_x                  = gridinfo%n_x
      this%n_y                  = gridinfo%n_y
      this%startdate            = namelist%startdate
      this%enddate              = namelist%enddate
      this%lat(:,:)             = gridinfo%lat(:,:)
      this%lon(:,:)             = gridinfo%lon(:,:)
      this%terrain_slope(:,:)   = namelist%terrain_slope
      this%azimuth(:,:)         = namelist%azimuth
      this%ZREF                 = namelist%ZREF
      this%vegtyp(:,:)          = gridinfo%vegtyp(:,:)
      this%croptype(:,:)        = namelist%croptype
      this%isltyp(:,:)          = namelist%isltyp
      this%IST(:,:)             = namelist%sfctyp
      this%soilcolor(:,:)       = namelist%soilcolor
      this%start_datetime       = date_to_unix(namelist%startdate)  ! returns seconds-since-1970-01-01
      this%end_datetime         = date_to_unix(namelist%enddate)
      do ii = 1, namelist%nsoil
        this%zsoil(:,:,ii)      = namelist%zsoil(ii)
      end do
      do ii = -namelist%nsnow+1, namelist%nsoil
        this%dzsnso(:,:,ii)     = namelist%dzsnso(ii) 
      end do

    end subroutine InitTransfer
  
  end module DomainGridType
  
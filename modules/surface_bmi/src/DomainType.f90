module DomainType

use NamelistRead

implicit none
save
private

type, public :: domain_type

  integer           :: iloc
  integer           :: jloc
  real              :: DT    ! ? run timestep (s)
  character(len=12) :: startdate ! Start date of the model run ( YYYYMMDDHHmm ) 
  character(len=12) :: enddate   ! End date of the model run ( YYYYMMDDHHmm ) 
  character(len=12) :: nowdate   ! Current date of the model run ( YYYYMMDDHHmm ) 
  integer           :: itime     ! current integer time step of model run
  integer           :: ntime     ! total number of integer time steps in model run
  integer           :: nhours    ! total number of hours in run as specified in namelist
  double precision  :: time_dbl  ! current time of model run in seconds from beginning
  real    :: lat
  real    :: lon
  real    :: ZREF
  integer :: vegtyp
  integer :: croptype
  integer :: isltyp
  integer :: sfctyp
  integer :: IST    !surface type 1-soil; 2-lake
  real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
  real, allocatable, dimension(:) :: dzsnso  ! snow/soil layer thickness [m]
  real, allocatable, dimension(:) :: zsnso   ! depth of snow/soil layer-bottom

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate 
    procedure, private :: InitDefault     
    procedure, public  :: InitTransfer

end type domain_type

contains   

  subroutine Init(this, namelist)

    class(domain_type) :: this
    type(namelist_type) :: namelist

    call this%InitAllocate(namelist)
    call this%InitDefault()

  end subroutine Init


  subroutine InitAllocate(this, namelist)

    class(domain_type) :: this
    type(namelist_type) :: namelist

    allocate(this%zsoil (namelist%nsoil))  ; this%zsoil  (:)   = huge(1.0)
    allocate(this%dzsnso(-namelist%nsnow+1:namelist%nsoil))  ; this%dzsnso (:)   = huge(1.0)
    allocate(this%zsnso(-namelist%nsnow+1:namelist%nsoil))  ; this%zsnso (:)   = huge(1.0)

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(domain_type) :: this

    this%iloc      = huge(1)
    this%jloc      = huge(1)
    this%dt        = huge(1.0)
    this%startdate = 'EMPTYDATE999'
    this%enddate   = 'EMPTYDATE999'
    this%nowdate   = 'EMPTYDATE999'
    this%itime     = huge(1) 
    this%ntime     = huge(1)
    this%nhours    = huge(1)
    this%time_dbl  = huge(1.d0)
    this%lat       = huge(1.0)
    this%lon       = huge(1.0)
    this%ZREF      = huge(1.0)
    this%vegtyp    = huge(1)
    this%croptype  = huge(1)
    this%isltyp    = huge(1)
    this%sfctyp    = huge(1)
    this%IST       = huge(1)

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(domain_type) :: this
    type(namelist_type) :: namelist

    this%dt        = namelist%dt
    this%startdate = namelist%startdate
    this%enddate   = namelist%enddate
    this%nhours    = namelist%maxtime
    this%lat       = namelist%lat
    this%lon       = namelist%lon
    this%ZREF      = namelist%ZREF
    this%zsoil     = namelist%zsoil
    this%dzsnso    = namelist%dzsnso
    this%vegtyp    = namelist%vegtyp
    this%croptype  = namelist%croptype
    this%isltyp    = namelist%isltyp
    this%sfctyp    = namelist%sfctyp

  end subroutine InitTransfer

end module DomainType

module LevelsGridType

  !!!!
  ! NJF This is a misleading type...is there a reason it is implied that a level is more than one dimensional?
  ! I don't really see a difference between this and the regular LevelsType...
  !!!!
use NamelistRead, only: namelist_type
implicit none
save
private

type, public :: levelsgrid_type

  integer :: nsoil  ! number of soil layers
  integer :: nsnow  ! number of snow layers
  integer :: nveg   ! number of vegetation types in chosen table

  contains

    procedure, public  :: Init         
    procedure, private :: InitDefault  
    procedure, private :: InitAllocate
    procedure, public  :: InitTransfer       

end type

contains   

  subroutine Init(this, namelist)

    class(levelsgrid_type)          :: this
    type(namelist_type),intent(in)  :: namelist

    call this%InitAllocate(namelist)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist)

    class(levelsgrid_type)          :: this
    type(namelist_type),intent(in)  :: namelist

    ! Nothing to do

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(levelsgrid_type) :: this

    this%nsoil = huge(1)
    this%nsnow = huge(1)
    this%nveg  = huge(1)    

  end subroutine InitDefault

  subroutine InitTransfer(this, namelist)

    class(levelsgrid_type)          :: this
    type(namelist_type),intent(in)  :: namelist

    this%nsoil = namelist%nsoil
    this%nsnow = namelist%nsnow
    this%nveg  = namelist%nveg    

  end subroutine InitTransfer

end module LevelsGridType

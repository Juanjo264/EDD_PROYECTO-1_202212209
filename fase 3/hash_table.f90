module hash_table_m
        ! program main use iso_fortran_env, only: i64 => INT64 
    use tecnicos
    implicit none
    private
    integer :: table_size = 7
    integer, parameter :: MAX_USED_PERCENTAGE = 70

    type, public :: HashTable
        integer :: elements = 0
        type(tecnicoo), allocatable :: array(:)
        integer :: colisiones =0
        contains
        procedure :: insert
        procedure :: print
        procedure :: search
        procedure, private :: solve_collision
    end type HashTable
contains
subroutine insert(self, Key, tecnico)
    class(HashTable), intent(inout) :: self
    type(HashTable) :: newTable
    integer(kind=8), intent(in) :: key
    type(tecnicoo), intent(in) :: tecnico 
    type(tecnicoo), allocatable :: oldArray(:)
    real(kind=8) :: used_percentage
    integer(kind=8) :: pos

    ! If the table is empty, allocate it
    if (.not. allocated(self%array)) then
        allocate(self%array(0:table_size-1))
        self%array(:)%dpi =-1
    end if

    pos = get_position(key)

    ! If the position is already occupied, solve the collision
    if (self%array(pos)%dpi /= -1 .and. self%array(pos)%dpi /= key) then
        call self%solve_collision(pos)
    end if

    ! Store the key in the table
    self%array(pos)%dpi = key
    self%array(pos) = tecnico
    self%elements = self%elements + 1

    ! Check if the table is more than 70% full
    used_percentage = (self%elements * 1.0/table_size) * 100
    if (used_percentage > MAX_USED_PERCENTAGE) then
        ! Deallocate the table
        !call self%print()
        allocate(oldArray(0:table_size-1))
        oldArray = self%array(:)
        deallocate(self%array)
        ! do i=0, table_size-1
        !     print'(A, I0)', 'POSICION: ', i
        !     print '(A, I13)', 'DPI: ', oldArray(i)%dpi
        !     print '(A, A)', 'NOMBRE: ', oldArray(i)%nombre
        !     print '(A, A)', 'APELLIDO: ', oldArray(i)%apellido
        !     print '(A, A)', 'GENERO: ', oldArray(i)%genero
        !     print '(A, A)', 'DIRECCION: ', oldArray(i)%dir
        !     print '(A, I8)', 'TEL: ', oldArray(i)%tel
        ! end do
        ! Rehash the table
        newTable = rehashing(oldArray)
        deallocate(oldArray)

        self%array = newTable%array
        self%elements = newTable%elements
        self%colisiones=0
    end if
end subroutine insert

function rehashing(oldArray) result(newTable)
    type(tecnicoo), intent(in) :: oldArray(:)
    integer(kind=8) :: i
    type(HashTable) :: newTable
    ! Initialize the new table
    table_size = table_size*2
    allocate(newTable%array(0:table_size-1))
    newTable%array(:)%dpi = -1  ! Inicializar el arreglo con un valor invalido
    ! Insert the elements in the new table

    do i = 1, size(oldArray)
        if (oldArray(i)%dpi /= -1) then
            call newTable%insert(oldArray(i)%dpi, oldArray(i))
        end if
    end do
    
end function rehashing

    subroutine solve_collision(self, pos) 
        class(HashTable), intent(inout) :: self
        integer(kind=8), intent(inout) :: pos
        ! Hash function h'(k)
        do while(self%array(pos)%dpi/=-1)
            self%colisiones = self%colisiones +1
            pos = mod(pos,(7+1))*self%colisiones
        end do
    end subroutine solve_collision
    
    function get_position(key) result(t)
        integer(kind=8), intent(in) :: key
        integer(kind=8) :: t
        ! Hash function h(k)
        ! Multiplicative hashing
        t = mod(key,table_size)
    end function get_position

    subroutine search(self, key)
        class(HashTable), intent(inout) :: self
        integer(kind=8), intent(in) :: key
        integer(kind=8) :: pos

        pos = get_position(key)
        ! If the key is not in the table
        !
        !
        print '(a i0 a i0)' , "Position: ", pos, " Key: ", self%array(pos)%dpi
        print'(A A)', 'Value: '
        call self%array(pos)%print_t()
    end subroutine search

    subroutine print(self)
        class(HashTable), intent(inout) :: self
        integer :: i
        print '(a, i0)', "Size on table: ", table_size
        print '(a, i0)', "Elements on table: ", self%elements
        do i=0, table_size-1
            print'(A, I0)', 'POSICION: ', i
            call self%array(i)%print_t()
            ! print '(A, I13)', 'DPI: ', self%array(i)%dpi
            ! print '(A, A)', 'NOMBRE: ', self%array(i)%nombre
            ! print '(A, A)', 'APELLIDO: ', self%array(i)%apellido
            ! print '(A, A)', 'GENERO: ', self%array(i)%genero
            ! print '(A, A)', 'DIRECCION: ', self%array(i)%dir
            ! print '(A, I8)', 'TEL: ', self%array(i)%tel
        end do
    end subroutine print
end module hash_table_m

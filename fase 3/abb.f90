module abb_m
    implicit none
    private

    type :: Node_t
        integer :: value
        character(len=:), allocatable :: departamento
        character(len=:), allocatable :: direccion
        character(len=:), allocatable :: password
        type(Node_t), pointer :: right => null()
        type(Node_t), pointer :: left => null()
    end type Node_t

    type, public :: abb
        type(Node_t), pointer :: root => null()

    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: inorder
        procedure :: posorder
        procedure :: graph
        procedure :: print_all
        procedure :: login
    end type abb

contains    
    !Subrutinas del tipo abb
    subroutine insert(self, val, Dep, Dir, Pas)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val
        character(len=:), allocatable, intent(in) :: Dep 
        character(len=:), allocatable, intent(in) :: Dir
        character(len=:), allocatable, intent(in) :: Pas

        if (.not. associated(self%root)) then
            allocate(self%root)
            self%root%value = val
            self%root%departamento = Dep
            self%root%direccion = Dir
            self%root%password = Pas

        else
            call insertRec(self%root, val, Dep, Dir, Pas)
        end if
    end subroutine insert
    recursive subroutine insertRec(root, val, Dep, Dir, Pas)
        type(Node_t), pointer, intent(inout) :: root
        integer, intent(in) :: val
        character(len=:), allocatable, intent(in) :: Dep 
        character(len=:), allocatable, intent(in) :: Dir
        character(len=:), allocatable, intent(in) :: Pas

        if (val < root%value) then
            if (.not. associated(root%left)) then
                allocate(root%left)
                root%left%value = val
                root%left%departamento = Dep
                root%left%direccion = Dir
                root%left%password = Pas
            else
                call insertRec(root%left, val, Dep, Dir, Pas)
            end if
        else if (val > root%value) then
            if (.not. associated(root%right)) then
                allocate(root%right)
                root%right%value = val
                root%right%departamento = Dep
                root%right%direccion = Dir
                root%right%password = Pas
            else
                call insertRec(root%right, val, Dep, Dir, Pas)
            end if
        end if
    end subroutine insertRec

    subroutine delete(self, val, Dep, Dir, Pas)
        class(abb), intent(inout) :: self
        integer, intent(inout) :: val
        character(len=:), allocatable, intent(inout) :: Dep
        character(len=:), allocatable, intent(inout) :: Dir
        character(len=:), allocatable, intent(inout) :: Pas

        self%root => deleteRec(self%root, val,Dep, Dir, Pas)
    end subroutine delete
    recursive function deleteRec(root, value, departamento, direccion, password) result(res)
        type(Node_t), pointer :: root
        integer, intent(in) :: value
        character(len=:), allocatable, intent(in) :: departamento
        character(len=:), allocatable, intent(in) :: direccion
        character(len=:), allocatable, intent(in) :: password
        type(Node_t), pointer :: res
        type(Node_t), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (value < root%value) then
            root%left => deleteRec(root%left, value, departamento, direccion, password)
        else if (value > root%value) then
            root%right => deleteRec(root%right, value, departamento, direccion, password)
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
                return
            else
                call getMajorOfMinors(root%left, temp)
                root%value = temp%value
                root%departamento = temp%departamento
                root%direccion = temp%direccion
                root%password = temp%password
                root%left => deleteRec(root%left, temp%value, temp%departamento, temp%direccion, temp%password)
            end if
        end if

        res => root
    end function deleteRec
    recursive subroutine getMajorOfMinors(root, major)
        type(Node_t), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors

    subroutine preorder(self)
        class(abb), intent(in) :: self

        call preorderRec(self%root)
        write(*, '()')
    end subroutine preorder
    recursive subroutine preorderRec(root)
        type(Node_t), pointer, intent(in) :: root

        if(associated(root)) then
            ! RAIZ - IZQ - DER
            write(*, '(I0 A)', advance='no') root%value, " - "
            call preorderRec(root%left)
            call preorderRec(root%right)
        end if
    end subroutine preorderRec

    subroutine inorder(self)
        class(abb), intent(in) :: self

        call inordenRec(self%root)
        print *, ""
    end subroutine inorder
    recursive subroutine inordenRec(root)
        type(Node_t), pointer, intent(in) :: root

        if(associated(root)) then
            ! IZQ - RAIZ - DER
            call inordenRec(root%left)
            write(*, '(I0 A)', advance='no') root%value, " - "
            call inordenRec(root%right)
        end if
    end subroutine inordenRec

    subroutine posorder(self)
        class(abb), intent(in) :: self

        call posordenRec(self%root)
        print *, ""
    end subroutine posorder
    recursive subroutine posordenRec(root)
        type(Node_t), pointer, intent(in) :: root

        if(associated(root)) then
            ! IZQ - DER - RAIZ
            call posordenRec(root%left)
            call posordenRec(root%right)
            write(*, '(I0 A)', advance='no') root%value, " - "
        end if
    end subroutine posordenRec

    subroutine graph(self, filename)
        class(abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes

        createNodes = ''
        linkNodes = ''

        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

        if (associated(self%root)) then
            call RoamTree(self%root, createNodes, linkNodes)
        end if

        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot(filename, dotStructure)
        print *, "Archivo actualizado existosamente."
    end subroutine graph
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(Node_t), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_value
        character(len=:), allocatable :: str_dir, str_dep, str_pas

        if (associated(current)) then
            ! SE OBTIENE INFORMACION DEL NODO ACTUAL
          address = get_address_memory(current)
          write(str_value, '(I0)') current%Value
          createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
          ! VIAJAMOS A LA SUBRAMA IZQ
          if (associated(current%Left)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Left)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "L"];' // new_line('a')

          end if
          ! VIAJAMOS A LA SUBRAMA DER
          if (associated(current%Right)) then
            address = get_address_memory(current)
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Right)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "R"];' // new_line('a')
          end if

          call RoamTree(current%Left, createNodes, linkNodes)
          call RoamTree(current%Right, createNodes, linkNodes)
        end if
    end subroutine RoamTree
    subroutine write_dot(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename

        ! Agregar extensiones
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"

        open(10, file="graph/"//dot_filename, status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)

        ! Genera la imagen PNG
        call system("dot -Tpng graph/"// dot_filename //" -o graph/" // png_filename)
    end subroutine write_dot

    function get_address_memory(node) result(address)
        !class(matrix_t), intent(in) :: self
        type(Node_t), pointer :: node
        character(len=20) :: address
        ! integer 8
        integer*8 :: i

        i = loc(node) ! get the address of x
        ! convert the address to string
        write(address, 10) i 
        10 format(I0)

    end function get_address_memory


    subroutine print_all(self)
        class(abb), intent(in) :: self

        call print_all_rec(self%root)
    end subroutine print_all

    recursive subroutine print_all_rec(root)
        type(Node_t), pointer, intent(in) :: root

        if (associated(root)) then
            write(*, '(A,I0,A,A,A,A)') 'ID: ', root%value, ' Departamento: ', trim(root%departamento), &
                                       ' Dirección: ', trim(root%direccion)
            call print_all_rec(root%left)
            call print_all_rec(root%right)
        end if
    end subroutine print_all_rec

    function login(self, id, password) result(success)
        class(abb), intent(in) :: self
        integer, intent(in) :: id
        character(*), intent(in) :: password
        logical :: success

        success = .false.
        call login_rec(self%root, id, password, success)
    end function login

    recursive subroutine login_rec(root, id, password, success)
        type(Node_t), pointer, intent(in) :: root
        integer, intent(in) :: id
        character(*), intent(in) :: password
        logical, intent(inout) :: success

        if (associated(root)) then
            if (root%value == id .and. root%password == password) then
                success = .true.
            else
                call login_rec(root%left, id, password, success)
                call login_rec(root%right, id, password, success)
            end if
        end if
    end subroutine login_rec

end module abb_m


module tecnicos
    implicit none
    type tecnicoo
        integer(kind=8) :: dpi =0
        character(:), allocatable :: nombre, apellido, genero, direccion
        integer (kind=8):: telefono=0
        contains
        procedure :: print_t
    end type tecnicoo
contains

    subroutine print_t(self)
        class(tecnicoo), intent(inout) :: self
        print '(A, I13)', 'DPI: ', self%dpi
        print '(A, A)', 'NOMBRE: ', self%nombre
        print '(A, A)', 'APELLIDO: ', self%apellido
        print '(A, A)', 'GENERO: ', self%genero
        print '(A, A)', 'DIRECCION: ', self%direccion
        print '(A, I8)', 'TEL: ', self%telefono
        print *, "------------------------"
    end subroutine print_t
end module tecnicos
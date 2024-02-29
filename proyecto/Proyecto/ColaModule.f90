module ColaModule
    implicit none

    type, public :: nodo
        integer :: id
        character(:), allocatable :: nombre
        integer :: imgG
        integer :: imgP
        type(nodo), pointer :: siguiente
    end type nodo

    type, public :: colaR
        type(nodo), pointer :: inicio => null(), final => null()
        contains 
            procedure :: insertar
            procedure :: imprimir
    end type colaR

    contains

        subroutine insertar(self, id, nombre, imgG, imgP)
            class(colaR), intent(inout) :: self
            integer, intent(in) :: id
            character(len=*), intent(in) :: nombre
            integer, intent(in) :: imgG, imgP

            type(nodo), pointer :: actual
            allocate(actual)
            actual%id = id
            actual%nombre = nombre
            actual%imgG = imgG
            actual%imgP = imgP
            actual%siguiente => null()

            if (.not. associated(self%inicio)) then
                self%inicio => actual
                self%final => actual
            else
                self%final%siguiente => actual
                self%final => actual
            end if

            print *, 'Ingresa: ', nombre
        end subroutine insertar

        subroutine imprimir(cola)
            implicit none
            class(colaR), intent(in) :: cola
        
            type(nodo), pointer :: actual


            print *, "---------------------"
            print *, 'Cola de ingreso:'
        
            actual => cola%inicio
    
            do while (associated(actual))
                print *, 'ID:', actual%id, ' Nombre:', trim(actual%nombre), ' imgG:', actual%imgG, ' imgP:', actual%imgP
                actual => actual%siguiente
            end do
            print *, "---------------------"
        end subroutine imprimir

        subroutine graficar(this, filename)
            class(colaR), intent(in) :: this
            character(len=*), intent(in) :: filename

            integer :: unit
            type(nodo), pointer :: current
            integer :: count

            ! Abrir el archivo DOT
            open(unit, file=filename, status='replace')
            write(unit, *) 'digraph Cola {'
            write(unit, *) '    rankdir=LR;' ! Ordenar los nodos horizontalmente
            write(unit, *) '    label="Cola de Recepcion";' ! Agregar un título a la cola
            write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=pink];' ! Aplicar atributos a todos los nodos
            ! Escribir nodos y conexiones
            current => this%inicio
            count = 0
            do while (associated(current))
                count = count + 1
                write(unit, *) '    "Cliente', count, '" [label="ID: ', current%id, ', Nombre: ', trim(current%nombre), '"];'
                if (associated(current%siguiente)) then
                    write(unit, *) '    "Cliente', count, '" -> "Cliente', count+1, '";'
                end if
                current => current%siguiente
            end do 

            ! Cerrar el archivo DOT
            write(unit, *) '}'
            close(unit)

            ! Generar el archivo PNG utilizando Graphviz
            call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

            print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
        end subroutine graficar

end module ColaModule

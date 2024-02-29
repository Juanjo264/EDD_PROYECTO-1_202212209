program main
    use circleList
    use json_module
    use linkedList
    use pila_module
    use ColaModule


    implicit none
    integer :: option
    character(len=100) :: ruta
    type(colaR) :: colaPrueba

    ! Main menu loop
    do
        ! Display menu options
        print *, "MENU"
        print *, "1. Parámetros iniciales"
        print *, "   a. Carga masiva de clientes"
        print *, "   b. Cantidad de ventanillas"
        print *, "2. Ejecutar paso"
        print *, "3. Estado en memoria de las estructuras"
        print *, "4. Reportes"
        print *, "5. acerca de ←datos del estudiante"
        print *, "6. Salir"
        print *, "Ingrese su opción:"
        
        ! Read user option
        read(*, *) option
        
        ! Execute corresponding action based on user input
        select case(option)
            case(1)
                call parametros_iniciales()
            case(2)
                call imprimir_cola(colaPrueba)
            case(3)
                call estado_memoria()
            case(4)
                call reportes()
            case(5)
                call datos_estudiante()
            case(6)
                exit
            case default
                print *, "Opción inválida. Por favor, ingrese una opción válida."
        end select
    end do

contains

    subroutine parametros_iniciales()
        ! Display sub-menu options for Parámetros iniciales
        print *, "Parámetros iniciales:"
        print *, "a. Carga masiva de clientes"
        print *, ' Ingrese la ruta del archivo para cargar clientes'
        read(*,*) ruta
        call carga_masiva_clientes(ruta, colaPrueba)
        print *, " "
        print *, "b. Cantidad de ventanillas"
        call cantidad_ventanillas()
    end subroutine parametros_iniciales

    subroutine carga_masiva_clientes(ruta, colaPrueba)
        implicit none
        character(len=100), intent(in) :: ruta
        type(colaR) :: colaPrueba

        type(json_file) :: json   
        type(json_value), pointer :: listPointer, personPointer, attributePointer 
        type(json_core) :: jsonc  
        integer :: id
        character(:), allocatable :: nombre
        integer :: imgG
        integer :: imgP

        integer :: i, size    
        logical :: found

        call json%initialize()    
        call json%load(filename=ruta)  
        
        call json%info('',n_children=size)

        call json%get_core(jsonc)               
        call json%get('', listPointer, found)

        do i = 1, size                          
            call jsonc%get_child(listPointer, i, personPointer, found = found)  
            
            if (found) then                      
                call jsonc%get_child(personPointer, 'id', attributePointer, found = found) 
                if (found) then
                    call jsonc%get(attributePointer, id)  
                endif
                
                call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)
                if (found) then
                    call jsonc%get(attributePointer, nombre)  
                endif
                
                call jsonc%get_child(personPointer, 'img_g', attributePointer, found = found)
                if (found) then
                    call jsonc%get(attributePointer, imgG)  
                endif
                
                call jsonc%get_child(personPointer, 'img_p', attributePointer, found = found)
                if (found) then
                    call jsonc%get(attributePointer, imgP)  
                endif
        
                call colaPrueba%insertar(id, nombre, imgG, imgP)
            endif
        end do
        call graficar(colaPrueba, 'clientes')

        call json%destroy()                    
    end subroutine carga_masiva_clientes


    subroutine cantidad_ventanillas()
        implicit none
        integer :: num_ventanillas, i
        character(len=20), dimension(:), allocatable :: nombres_ventanillas
        type(linked_list), dimension(:), allocatable :: ventanillas
        type(pila), dimension(:), allocatable :: pilas_ventanillas
        character(len=100) :: filename
        
        ! Solicitar al usuario el número de ventanillas
        print *, "Ingrese el número de ventanillas:"
        read(*, *) num_ventanillas
        
        ! Asignar la longitud de las matrices
        allocate(nombres_ventanillas(num_ventanillas))
        allocate(ventanillas(num_ventanillas))
        allocate(pilas_ventanillas(num_ventanillas))
        
        ! Generar los nombres de las ventanillas y crear las pilas asociadas
        do i = 1, num_ventanillas
            write(nombres_ventanillas(i), '(a, i0)') 'Ventanilla ', i
            call ventanillas(i)%push(i) ! Esto es solo un ejemplo de operación en la lista enlazada
            call pilas_ventanillas(i)%push(i) ! Crear pila para esta ventanilla
        end do
        
        ! Abrir el archivo DOT para escribir todas las ventanillas
        filename = 'ventanillas.dot'
        open(unit=10, file=trim(filename), status='unknown')
        write(10, *) 'digraph Ventanillas {'
        write(10, *) '    rankdir=TB;' ! Ordenar los nodos horizontalmente
        write(10, *) '    label="Ventanillas";' ! Agregar un título a las ventanillas
        write(10, *) '    node [shape=box, style=filled, color=blue, fillcolor=pink];' ! Aplicar atributos a todos los nodos
        
        ! Escribir nodos y conexiones para cada ventanilla
        do i = 1, num_ventanillas
            write(10, *) '    "Ventanilla', i, '" [label="Numero: ', i, '"];'
            if (i /= num_ventanillas) then
                write(10, *) '    "Ventanilla', i, '" -> "Ventanilla', i+1, '";'
            endif
        end do
        
        write(10, *) '}'
        close(10)
        
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
        
        ! Liberar la memoria asignada
        deallocate(nombres_ventanillas)
        deallocate(ventanillas)
        deallocate(pilas_ventanillas)
        
        print *, "Configurando cantidad de ventanillas..."
    end subroutine cantidad_ventanillas



    subroutine imprimir_cola(cola)
        implicit none
        class(colaR), intent(in) :: cola
        
        ! Declaración de variables locales
        type(nodo), pointer :: actual

        ! Imprimir encabezado
        print *, "---------------------"
        print *, 'Cola de ingreso:'

        ! Inicializar el puntero actual al primer nodo de la cola
        actual => cola%inicio

        ! Iterar sobre la cola e imprimir cada elemento
        do while (associated(actual))
            print *, 'ID:', actual%id, ' Nombre:', trim(actual%nombre), ' imgG:', actual%imgG, ' imgP:', actual%imgP
            actual => actual%siguiente
        end do
        
        ! Imprimir línea divisoria
        print *, "---------------------"
    end subroutine imprimir_cola


    subroutine ejecutar_paso()
        print *, "Ejecutando paso..."
        ! Aquí va el código para ejecutar el paso
    end subroutine ejecutar_paso

    subroutine estado_memoria()
        ! Implementación del estado en memoria de las estructuras
        print *, "Estado en memoria de las estructuras:"
        ! Aquí va el código para mostrar el estado en memoria de las estructuras
    end subroutine estado_memoria

    subroutine reportes()
              print *, "Generando reportes..."
              ! Aquí va el código para generar reportes
    end subroutine reportes

    subroutine datos_estudiante()
        ! Información del estudiante

        print *, "Datos :"
        print *, "Juan Jose Almengor Tizol :"
        print *, "202212209 :"
        print *, "Ingenieria en sistemas :"
        print *, "Estructura de datos :"

    end subroutine datos_estudiante

end program main


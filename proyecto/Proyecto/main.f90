program main
    use circleList
    use json_module
    implicit none
    
    integer :: option
    
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
                call ejecutar_paso()
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
        character(len=1) :: sub_option
        
        ! Display sub-menu options for Parámetros iniciales
        print *, "Parámetros iniciales:"
        print *, "a. Carga masiva de clientes"
        print *, "b. Cantidad de ventanillas"
        print *, "Ingrese su opción:"
        read(*, '(A)') sub_option
        
        ! Execute corresponding action based on user input for Parámetros iniciales
        if (sub_option == 'a') then
            call carga_masiva_clientes()
        elseif (sub_option == 'b') then
            call cantidad_ventanillas()
        else
            print *, "Opción inválida. Por favor, ingrese una opción válida."
        end if
    end subroutine parametros_iniciales

    subroutine carga_masiva_clientes()
        implicit none
        ! Variables
        type(json_file) :: json ! JSON file object
        type(json_value), pointer :: list_p, client_p, attribute_p ! Pointers to JSON objects
        type(json_core) :: jsonc ! JSON core object
        character(:), allocatable :: nombre, id, img_g, img_p
        integer :: i, size ! Size of the list
        logical :: found ! Flag to check if the attribute was found

        call json%initialize()
        call json%load(filename='clients.json') ! Load the JSON file
        call json%info('', n_children=size) ! Get the number of children in the root object
        call json%get_core(jsonc) ! Get the core object, that will be used to get the children
        call json%get('', list_p, found) ! Get the list of children
        
        do i = 1, size
          ! Get the i-th child
          call jsonc%get_child(list_p, i, client_p, found=found)
          ! Get the attributes of the client
          call jsonc%get_child(client_p, 'id', attribute_p, found=found)
          if(found) call jsonc%get(attribute_p, id)

          call jsonc%get_child(client_p, 'nombre', attribute_p, found=found)
          if(found) call jsonc%get(attribute_p, nombre)

          call jsonc%get_child(client_p, 'img_g', attribute_p, found=found)
          if(found) call jsonc%get(attribute_p, img_g)

          call jsonc%get_child(client_p, 'img_p', attribute_p, found=found)
          if(found) call jsonc%get(attribute_p, img_p)

          ! Print the values
          print '(10a)', 'ID: ', trim(id), ' Nombre: ', trim(nombre), ' img_g: ', trim(img_g), ' img_p: ', trim(img_p)
        end do  
        ! Clean up
        call json%destroy()

        print *, "Cargando masivamente clientes desde JSON..."
        ! Aquí va el código para la carga masiva de clientes desde el JSON
    end subroutine carga_masiva_clientes
    
    subroutine cantidad_ventanillas()
        ! Implementación de la opción para configurar la cantidad de ventanillas
        print *, "Configurando cantidad de ventanillas..."
        ! Aquí va el código para configurar la cantidad de ventanillas
    end subroutine cantidad_ventanillas

    subroutine ejecutar_paso()
        ! Implementación de la ejecución de paso
        print *, "Ejecutando paso..."
        ! Aquí va el código para ejecutar el paso
    end subroutine ejecutar_paso

    subroutine estado_memoria()
        ! Implementación del estado en memoria de las estructuras
        print *, "Estado en memoria de las estructuras:"
        ! Aquí va el código para mostrar el estado en memoria de las estructuras
    end subroutine estado_memoria

    subroutine reportes()
        implicit none

        type(circle_list) :: cl

        call cl%add(1)
        call cl%add(2)
        call cl%add(3)
        call cl%add(4)
        call cl%add(5)

        call cl%print_dot("img1.dot")

        call cl%print()

        call cl%remove(3)
        print *, "After removing 3"
        call cl%print()

        call cl%remove(5)
        print *, "After removing 5"
        call cl%print()

        call cl%remove(1)
        call cl%remove(2)
        print *, "After removing 1, 2"
        call cl%print()

        call cl%remove(4)
        print *, "After removing 1, 2, 4"
        call cl%print()
        call cl%print_dot("img2.dot")

        call cl%add(6)
        call cl%add(7)
        print *, "After adding 6, 7"
        call cl%print()
        call cl%print_dot("img3.dot")
              print *, "Generando reportes..."
              ! Aquí va el código para generar reportes
    end subroutine reportes

    subroutine datos_estudiante()
        ! Información del estudiante
        print *, "Datos del estudiante:"
        print *, "Nombre: [Tu nombre]"
        print *, "Número de estudiante: [Tu número de estudiante]"
        print *, "Año de ingreso: [Tu año de ingreso]"
    end subroutine datos_estudiante

end program main


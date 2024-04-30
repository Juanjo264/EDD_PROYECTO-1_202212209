program main
    use abb_m
    implicit none
    integer :: opcion
    character(len=20) :: usuario, contrasena
    character(len=100) :: filenamec
    character(len=100) :: filenames
    character(len=100) :: filenamer
    character(len=100) :: filenametecnicos

    type(abb) :: tree


    print *, 'Ingrese el nombre de usuario:'
    read *, usuario
    print *, 'Ingrese la contraseña:'
    read *, contrasena

    !EDD1S2024
    !ProyectoFase3
    if (usuario == 'a' .and. contrasena == 'a') then
        do
            print *, 'Seleccione una opción:'
            print *, '1) Carga de archivos'
            print *, '   1.1) Sucursales'
            print *, '   1.2) Rutas'
            print *, '2) Sucursales'
            print *, '3) Reportes'
            print *, '4) Salir'
            read *, opcion

            select case (opcion)
                case (1)
                    call carga_archivos(tree)
                case (2)
                    call sucursales(tree, filenametecnicos)
                case (3)
                    call reportes()
                case (4)
                    print *, 'Saliendo...'
                    exit
                case default
                    print *, 'Opción no válida.'
            end select
        end do
    else
        print *, 'Usuario o contraseña incorrectos.'
    end if
end program main

subroutine carga_archivos(tree)
    use abb_m
    implicit none
    type(abb) :: tree
    integer :: opcion

    do
        print *, 'Seleccione una opción:'
        print *, '1) Cargar Sucursales'
        print *, '2) Cargar Rutas'
        print *, '3) Regresar al menú principal'
        read *, opcion

        select case (opcion)
            case (1)
                call cargar_sucursales(tree)
            case (2)
                call cargar_rutas()
            case (3)
                print *, 'Regresando al menú principal...'
                exit
            case default
                print *, 'Opción no válida.'
        end select
    end do
end subroutine carga_archivos

subroutine cargar_sucursales(tree)
    use abb_m
    use json_module
    implicit none
    type(abb) :: tree
    type(json_file) :: json
    type(json_value), pointer :: list_p, client_p, attribute_p ! Pointers to JSON objects
    type(json_core) :: jsonc ! JSON core object
    character(:), allocatable :: idstr, departamento, direccion, password ! Attributes of the client
    character(len=100) :: filenamesucursales, filename  ! File path
    integer :: id
    integer :: i, size 
    logical :: found

    ! Ask the user for the file path
    print *, "Ingresa el archivo de las sucursales:"
    read(*, '(A)') filenamesucursales

    ! Variables
    call json%initialize()
    call json%load(filenamesucursales) ! Load the JSON file
    call json%info('', n_children=size) ! Get the number of children in the root object
    call json%get_core(jsonc) ! Get the core object, that will be used to get the children
    call json%get('', list_p, found) ! Get the list of children
    
    do i = 1, size
        ! Get the i-th child
        call jsonc%get_child(list_p, i, client_p, found=found)
        ! Get the attributes of the client
        call jsonc%get_child(client_p, 'id', attribute_p, found=found)
        if(found) then
            call jsonc%get(attribute_p, idstr)
            read(idstr, *) id
        end if
        call jsonc%get_child(client_p, 'departamento', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, departamento)

        call jsonc%get_child(client_p, 'direccion', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, direccion)

        call jsonc%get_child(client_p, 'password', attribute_p, found=found)
        if(found) call jsonc%get(attribute_p, password)

        ! Print the values
        print *, 'ID: ', id, ' departamento: ', trim(departamento), ' direccion: ',&
        trim(direccion), ' password: ', trim(password)

        call tree%insert(id, departamento, direccion, password)

    end do  
    ! Clean up
    call json%destroy()
    
    print *, "Creating the graph of the tree"
    call tree%graph("inserted")
      print *, "Usuarios en el árbol:"
    call tree%print_all()

end subroutine cargar_sucursales

subroutine cargar_rutas()
    use iso_fortran_env, wp => real64, sp => real32
    use lista_adyacencia_m
    use json_module
    implicit none
    type(ListaAdyacencia) :: grafo
    integer :: start,end
    type(json_file) :: json ! JSON file object
    type(json_value), pointer :: list_p, client_p, attribute_p, grafo_p ! Pointers to JSON objects
    type(json_core) :: jsonc ! JSON core object
    integer :: s1, s2, imp_mantenimiento, i, size
    integer :: distancia
    logical :: found
    character(len=100) :: filenamerutas

    print *, "Ingresa el archivo de las Rutas:"
    read(*, '(A)') filenamerutas


    call json%initialize()
    call json%load(filename=filenamerutas)
    call json%get_core(jsonc)
    call json%get('grafo', grafo_p, found)
    call jsonc%info(grafo_p, n_children=size)

    do i = 1, size
        call jsonc%get_child(grafo_p, i, client_p, found)
        call jsonc%get_child(client_p, 's1', attribute_p, found)
        if(found) call jsonc%get(attribute_p, s1)
        call jsonc%get_child(client_p, 's2', attribute_p, found)
        if(found) call jsonc%get(attribute_p, s2)
        call jsonc%get_child(client_p, 'distancia', attribute_p, found)
        if(found) call jsonc%get(attribute_p, distancia)

        call jsonc%get_child(client_p, 'imp_mantenimiento', attribute_p, found)
        if(found) call jsonc%get(attribute_p, imp_mantenimiento)

        call grafo%insert(s1)
        call grafo%insert(s2)
        call grafo%crearConexion(s1, s2, distancia, imp_mantenimiento)
        call grafo%crearConexion(s2, s1, distancia, imp_mantenimiento)
    end do

    ! call grafo%crearGrafo()
    ! call grafo%dijkstra(21, 1)
    ! call grafo%dijkstra_max(21, 1)
    call json%destroy()

end subroutine cargar_rutas

subroutine sucursales(tree, filenametecnicos)
    use abb_m
    implicit none
    type(abb) :: tree
    integer :: id, option
    character(len=100) :: password
    character(len=100) :: filenametecnicos
    print *, "Ingrese su ID:"
    read *, id

    print *, "Ingrese su contraseña:"
    read *, password
    call tree%print_all()

    if (tree%login(id, password)) then
        print *, 'Inicio de sesión exitoso'
        do
            print *, 'Seleccione una opción:'
            print *, '1. Carga de tecnicos'
            print *, '2. Recorrido mas optimo'
            print *, '3. tecnico en especifico'
            print *, '4. listar tecnicos'
            print *, '5. generar reporte'
            print *, '6. Regresar'
            read *, option

            select case (option)
                case (1)
                    print *, 'Seleccionaste la opción 1'
                    call cargar_tecnicos(filenametecnicos)
                case (2)
                    print *, 'Seleccionaste la opción 2'
                    ! Aquí puedes llamar a la subrutina correspondiente a la opción 2
                case (3)
                    print *, 'Seleccionaste la opción 3'
                    ! Aquí puedes llamar a la subrutina correspondiente a la opción 3
                case (4)
                    print *, 'Seleccionaste la opción 4'
                    ! Aquí puedes llamar a la subrutina correspondiente a la opción 4
                case (5)
                    print *, 'Regresando al menú principal...'
                    return
                case default
                    print *, 'Opción no válida. Por favor, intenta de nuevo.'
            end select
        end do
    else
        print *, 'ID o contraseña incorrectos'
    end if

end subroutine sucursales

subroutine cargar_tecnicos(filenametecnicos)
    implicit none
    character(len=100) :: filenametecnicos
    
    print *, "Ingresa el archivo de los tecnicos:"
    read(*, '(A)') filenametecnicos
    print *, filenametecnicos ,"Cargando tecnicos..."
    print *, "Tecnicos cargados exitosamente"

end subroutine cargar_tecnicos

subroutine reportes()
implicit none

end subroutine reportes
subroutine reporte1()
    use iso_fortran_env, only: i64 => INT64
    use tecnicos
    use hash_table_m
    use json_module
    implicit none
    type(tecnicoo) :: tecnico1
    type(HashTable) :: myHashTable
    type(json_file) :: json
    type(json_value), pointer :: listPointer, personPointer, attributePointer, namePointer
    type(json_core) :: jsonc
    character(:),allocatable :: dpi_string, name_string, genero_string, direccion_string, apellido_string
    integer :: telefono
    integer :: i, size 
    integer(kind=16) :: dpi
    logical :: found

    call json%initialize()
    call json%load(filename='tecnicos.json')

    call json%info('',n_children=size)

    call json%get_core(jsonc)
    call json%get('', listPointer, found)

    do i = 1, size
        call jsonc%get_child(listPointer, i, personPointer, found = found)
        if (found) then
            call jsonc%get_child(personPointer, 'dpi', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, dpi_string)
                read(dpi_string, *) dpi
                tecnico1%dpi = dpi
            end if
            call jsonc%get_child(personPointer, 'nombre', namePointer, found = found)
            if (found) then
                call jsonc%get(namePointer, name_string)
                tecnico1%nombre = name_string
            end if
            call jsonc%get_child(personPointer, 'apellido', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, apellido_string)
                tecnico1%apellido = apellido_string
            end if
            call jsonc%get_child(personPointer, 'genero', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, genero_string)
                tecnico1%genero = genero_string
            end if
            call jsonc%get_child(personPointer, 'direccion', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, direccion_string)
                tecnico1%direccion = direccion_string
            end if
            call jsonc%get_child(personPointer, 'telefono', attributePointer, found = found)
            if (found) then
                call jsonc%get(attributePointer, telefono)
                tecnico1%telefono = telefono
            end if
            ! Add other attributes here following the same pattern
            ! Insertar tecnico1 en la tabla hash
            call myHashTable%insert(tecnico1%dpi, tecnico1)
        end if
    end do

    ! Imprimir la tabla hash
    call myHashTable%print()

    call json%destroy()

end subroutine reporte1
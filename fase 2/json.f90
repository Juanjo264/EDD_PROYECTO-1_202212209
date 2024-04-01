program menu

    use json_module
    use Colamodule
    use abb_m
    use matrix_m
    use AVL_Tree_M
    implicit none
    type(Tree_t), pointer :: avl_tree  ! Árbol AVL

    character(20) :: username, password
    integer :: choice


    ! Inicio de Sesión

    do 
        print *, "Bienvenido al sistema."
        print *, "Por favor ingrese su nombre de usuario y contraseña."
        print *, "Usuario:"
        read(*,*) username
        print *, "Contraseña:"
        read(*,*) password

        if (username == "admin" .and. password == "EDD2024") then
            ! Usuario es un administrador
            print *, "Inicio de sesión exitoso como administrador."
            do 
                print *, "Por favor seleccione una opción:"
                print *, "1. Módulo administrador"
                print *, "2. Árbol B de usuarios (Gráfico)"
                print *, "3. Operaciones sobre los usuarios (insertar, modificar y eliminar)"
                print *, "4. Operaciones de carga masiva de usuarios"
                print *, "5. Salir"
                read(*,*) choice
                select case (choice)
                    case (1)
                        ! Implementar módulo de administrador
                        call a()
                    case (2)
                        ! Implementar Árbol B de usuarios
                    case (3)
                        ! Implementar operaciones sobre los usuarios
                    case (4)
                        ! Operaciones de carga masiva de usuarios
                        call cargarMasivaImagen()
                    case (5)
                        exit
                    case default
                        print *, "Opción no válida."
                end select
            end do
        else
            ! Usuario es un usuario normal
            print *, "Inicio de sesión exitoso como usuario."
            do
                print *, "Por favor seleccione una opción:"
                print *, "1. Módulo de usuario"
                print *, "2. Visualizar reportes de las estructuras"
                print *, "3. Navegación y gestión de imágenes"
                print *, "4. Opciones de carga masiva"
                print *, "5. Salir"
                read(*,*) choice
                select case (choice)
                    case (1)
                        ! Implementar módulo de usuario
                        call b()
                    case (2)
                        ! Implementar visualización de reportes
                    case (3)
                        ! Implementar navegación y gestión de imágenes
                    case (4)
                        ! Implementar opciones de carga masiva
                    case (5)
                        exit
                    case default
                        print *, "Opción no válida."
                end select
            end do
        end if
    end do

contains

    subroutine a()
    print *, "admin"
    end subroutine a

    subroutine b()
    print *, "clientes"
    end subroutine b

    subroutine cargarMasivaImagen()
    implicit none
    type(json_file) :: json_imagenes, json_capas
    type(json_value), pointer :: root_imagenes_p, root_capas_p, capa_p, pixeles_p, pixel_p
    type(json_value), pointer :: fila_p, columna_p, color_p, id_capa_p, id_p, capas_p
    type(json_core) :: jsonc_imagenes, jsonc_capas
    type(matrix_t) :: my_matrix
    type(Tree_t), pointer :: avl_tree  ! Árbol AVL
    type(abb) :: abb_tree   ! Árbol ABB

    character(len=100) :: filename_imagenes, filename_capas

    integer :: i, j, k, l, imagen_size, capas_size, row, col, value, id_capa, id, pixeles_size
    character(len=:), allocatable :: color
    logical :: found
    allocate(avl_tree)  ! Allocate memory for avl_tree

    call my_matrix%init()

    call json_imagenes%initialize()
    call json_capas%initialize()
    print *, 'Ingrese la ruta del archivo para cargar imagenes'
    read(*,*) filename_imagenes
    print *, 'Ingrese la ruta del archivo para cargar capas'
    read(*,*) filename_capas
    call json_imagenes%load(filename_imagenes)
    call json_imagenes%get_core(jsonc_imagenes)
    call json_imagenes%get('', root_imagenes_p, found)
    if (.not. found) stop 'Root object not found'

    call jsonc_imagenes%info(root_imagenes_p, n_children=imagen_size)

    call avl_tree%newTree()  ! Inicializa el árbol AVL
    call json_capas%load(filename_capas)

    do i = 1, imagen_size
        call jsonc_imagenes%get_child(root_imagenes_p, i, id_p, found=found)
        if (.not. found) cycle

        call jsonc_imagenes%get_child(id_p, 'id', id_capa_p, found=found)
        if (.not. found) cycle
        call jsonc_imagenes%get(id_capa_p, id)

        call avl_tree%insert(id)  ! Inserta en el árbol AVL
        call avl_tree%GenerateGraph()

        call jsonc_imagenes%get_child(id_p, 'capas', capas_p, found=found)
        if (.not. found) cycle

        call jsonc_imagenes%info(capas_p, n_children=capas_size)

        do j = 1, capas_size
            call jsonc_imagenes%get_child(capas_p, j, id_capa_p, found=found)
            if (.not. found) cycle

            call jsonc_imagenes%get(id_capa_p, id_capa)

            call json_capas%get_core(jsonc_capas)

            call json_capas%get('', root_capas_p, found)
            if (.not. found) stop 'Root object not found'

            call jsonc_capas%info(root_capas_p, n_children=capas_size)

            do k = 1, capas_size
                call jsonc_capas%get_child(root_capas_p, k, capa_p, found=found)
                if (.not. found) cycle

                call jsonc_capas%get_child(capa_p, 'id_capa', id_capa_p, found=found)
                if (.not. found) cycle
                call jsonc_capas%get(id_capa_p, value)

                if (value == id_capa) then
                    call abb_tree%insert(id_capa)  ! Inserta en el árbol ABB

                    call jsonc_capas%get_child(capa_p, 'pixeles', pixeles_p, found=found)
                    if (.not. found) cycle

                    call jsonc_capas%info(pixeles_p, n_children=pixeles_size)

                    do l = 1, pixeles_size
                        call jsonc_capas%get_child(pixeles_p, l, pixel_p, found=found)
                        if (.not. found) cycle

                        call jsonc_capas%get_child(pixel_p, 'fila', fila_p, found=found)
                        call jsonc_capas%get(fila_p, row)

                        call jsonc_capas%get_child(pixel_p, 'columna', columna_p, found=found)
                        call jsonc_capas%get(columna_p, col)

                        call jsonc_capas%get_child(pixel_p, 'color', color_p, found=found)
                        call jsonc_capas%get(color_p, color)

                        call my_matrix%add(row, col, color)
                    end do
                end if
            end do
        end do
        call my_matrix%create_dot()
    end do
    call json_capas%destroy()

    call json_imagenes%destroy()
    call abb_tree%graph("ABB")
    write(*, '(A)') "Escribiendo en preorden: "
    call abb_tree%preorder()

    write(*, '(A)') "Escribiendo en inorder: "
    call abb_tree%inorder()

    print *, "Escribiendo en posorden: "
    call abb_tree%posorder()

    end subroutine cargarMasivaImagen

end program menu

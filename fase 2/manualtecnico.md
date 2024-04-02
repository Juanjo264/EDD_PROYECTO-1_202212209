# MANUAL TECNICO 

## Requisitos del sistema

Para que nuestro programa funcione correctamente tenemos que tener instalados algunos programas:

1. Instalar Visual Studio Code en nuestra computadora.
2. Instalar Fortran, preferiblemente la version mas reciente o que sea estable.
3. Tener instalado Graphviz, para las graficas.
4. Utilizar extenciones para previsualizar grafos y archivos.
   
# Pixel Print Studio
## Archivo json.f90
Este archivo contiene todo nuestro codigo inicial para Pixel Print Studio. 

```fortran
        print *, "-----------Pixel Print Studio.------------"
        print *, "Por favor ingrese su nombre de usuario y contraseña."
        print *, "Usuario:"
        read(*,*) username
        print *, "Contraseña:"
        read(*,*) password
```

En primer lugar, se muestra un mensaje de bienvenida "Pixel Print Studio". Luego, se solicita al usuario que ingrese su nombre de usuario y su contraseña, mostrando mensajes respectivos para cada campo de entrada. Utilizando la función read, el programa captura las entradas del usuario para el nombre de usuario y la contraseña, almacenándolos en las variables username y password respectivamente.

```fortran
        if (username == "admin" .and. password == "EDD2024") then
            ! Usuario es un administrador
            print *, "Inicio de sesión exitoso como administrador."
            do 
                print *, "Por favor seleccione una opción:"
                print *, "1. ver arbol B"
                print *, "2. Operaciones sobre los usuarios (insertar, modificar y eliminar)"
                print *, "3. Operaciones de carga masiva de usuarios"
                print *, "4. Salir"
                read(*,*) choice
                select case (choice)
                    case (1)
                        call verarbolB()
                    case (2)
                        call operacionusuarios()
                    case (3)
                        call cargamasivausu()
                    case (4)
                        exit
                    case default
                        print *, "Opción no válida."
                end select
            end do

```

verifica las credenciales del usuario para determinar si es un administrador. Si las credenciales son correctas, se muestra un mensaje de inicio de sesión exitoso y se presenta un menú con varias opciones. Estas opciones incluyen ver un árbol B, realizar operaciones sobre usuarios (como insertar, modificar y eliminar), realizar operaciones de carga masiva de usuarios o salir del programa. El usuario selecciona una opción ingresando un número correspondiente a la acción deseada. Dependiendo de la opción seleccionada, se llaman a diferentes funciones para llevar a cabo las acciones correspondientes

```fortran    
            print *, "Inicio de sesión exitoso como usuario."
            do
                print *, "Por favor seleccione una opción:"
                print *, "1. Visualizar reportes de las estructuras"
                print *, "2. Navegación y gestion de imágenes"
                print *, "3. Opciones de carga masiva"
                print *, "4. Salir "
                read(*,*) choice
                select case (choice)
                    case (1)
                        call reportes()
                    case (2)
                        call gestion()
                    case (3)
                        call cargarMasivaImagen()
                    case (4)
                        exit
                    case default
                        print *, "Opción no válida."
                end select
            end do
        end if
    end do

```
maneja el inicio de sesión exitoso como usuario estándar y lo buscamos el archivo usuarios.json para ingresar y proporciona un menú de opciones para realizar diferentes acciones dentro del programa, como visualizar reportes de estructuras, navegar y gestionar imágenes, realizar opciones de carga masiva o salir del programa. El usuario selecciona una opción ingresando un número correspondiente a la acción deseada, y el programa ejecuta la acción asociada a esa opción. Si se ingresa una opción no válida, se muestra un mensaje indicando que la opción seleccionada no es válida.

```fortran  

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
        character(len=10) :: order

        integer :: i, j, k, l, imagen_size, capas_size, row, col, value, id_capa, id, pixeles_size, num_capas, user_id, num_ids
        character(len=:), allocatable :: color
        logical :: found
        integer, allocatable :: user_ids(:)
        allocate(avl_tree)  ! Allocate memory for avl_tree

        call my_matrix%init()

        call json_imagenes%initialize()
        call json_capas%initialize()
        print *, 'Ingrese la ruta del archivo para cargar imagenes'
        read(*,*) filename_imagenes
        print *, 'Ingrese la ruta del archivo para cargar capas'
        read(*,*) filename_capas
        print *, 'Ingrese cuantas capas quiere'
        read(*,*) num_capas
        print *, 'Ingrese el orden (preorder, inorder, postorder)'
        read(*,*) order
        print *, 'Ingrese cuantos IDs de imagen quiere'
        read(*,*) num_ids
        allocate(user_ids(num_ids))
        print *, 'Ingrese los IDs de las imagenes'
        do i = 1, num_ids
            read(*,*) user_ids(i)
        end do

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

            if (any(user_ids == id)) then
                call avl_tree%insert(id)  ! Inserta en el árbol AVL
                call avl_tree%GenerateGraph()

                call jsonc_imagenes%get_child(id_p, 'capas', capas_p, found=found)
                if (.not. found) cycle

                call jsonc_imagenes%info(capas_p, n_children=capas_size)

                do j = 1, min(capas_size, num_capas)
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
            end if
        end do
        call json_capas%destroy()

        call json_imagenes%destroy()
        call abb_tree%graph("ABB")
        if (order == 'preorder') then
            write(*, '(A)') "Escribiendo en preorden: "
            call abb_tree%preorder()
        else if (order == 'inorder') then
            write(*, '(A)') "Escribiendo en inorder: "
            call abb_tree%inorder()
        else if (order == 'postorder') then
            print *, "Escribiendo en posorden: "
            call abb_tree%posorder()
        end if

        deallocate(user_ids)

    end subroutine cargarMasivaImagen

```
 • Se definen variables y tipos de datos necesarios, como estructuras para manejar archivos JSON, matrices, y árboles AVL y ABB.
 • Se solicita al usuario ingresar las rutas de los archivos JSON que contienen la información de las imágenes y las capas, así como otros parámetros como el número de capas a cargar y el orden de escritura.
 •Se leen los datos de las imágenes y se insertan en un árbol AVL si el ID de la imagen coincide con uno proporcionado por el usuario.
 •Para cada imagen seleccionada, se procesan sus capas asociadas. Se leen los datos de las capas, se insertan en un árbol ABB si el ID de la capa coincide con uno asociado a la imagen, y se extraen los píxeles de cada capa para ser almacenados en una matriz.
 •Se genera un archivo DOT que representa la matriz de píxeles como un grafo.
 •Se destruyen las estructuras de datos temporales.
 •Se genera un gráfico del árbol ABB.
 •Dependiendo del orden especificado por el usuario, se escribe en la salida estándar los elementos del árbol ABB en preorden, inorden o posorden.
 •Se libera la memoria asignada para el arreglo de IDs de imágenes.


## Archivo matrix.f90

```fortran
module matrix_m
  use header_m
  implicit none

  type matrix_t
    type(header_t), pointer :: col => null()
    type(header_t), pointer :: row => null()
    !type(graph_t), pointer :: graph => null()
    contains
    procedure :: init
    procedure :: add
    procedure :: print
    procedure, private :: verify_column
    procedure, private :: move_first_lr_pointers
    procedure, private :: move_middle_lr_pointers
    procedure, private :: move_last_lr_pointers

    procedure, private :: verify_row
    procedure, private :: move_first_ud_pointers
    procedure, private :: move_middle_ud_pointers
    procedure, private :: move_last_ud_pointers

    procedure :: create_dot
    procedure, private :: get_content
    procedure, private :: align_col_nodes
    procedure :: get_address_memory
    procedure :: write_dot

  end type matrix_t

  contains
  subroutine init(self)
    class(matrix_t), intent(inout) :: self
    type(header_t), pointer :: row_hdr
    type(header_t), pointer :: col_hdr

    allocate(row_hdr)
    allocate(col_hdr)
    call row_hdr%init_h_t()
    call col_hdr%init_h_t()
    self%row => row_hdr
    self%col => col_hdr
  end subroutine init

  subroutine add(self, row, col, value)
    class(matrix_t), intent(inout) :: self
    integer, intent(in) :: row, col
    character(len=7) :: value


    type(header_node), pointer :: row_hdr_n
    type(header_node), pointer :: col_hdr_n
    type(matrix_node), pointer :: new_mtx_n

    ! Add headers
    allocate(col_hdr_n)
    allocate(row_hdr_n)
    col_hdr_n => self%col%add(col)
    row_hdr_n => self%row%add(row)
    ! New matrix node
    allocate(new_mtx_n)
    new_mtx_n%value = value
    new_mtx_n%row = row_hdr_n%position
    new_mtx_n%col = col_hdr_n%position

    ! Move pointers
    call self%verify_column(row_hdr_n, new_mtx_n)
    call self%verify_row(col_hdr_n, new_mtx_n)

  end subroutine add

```
Aqui podemos representar matrices en nuestro codigo, con el metodo add es como agregamos a la matriz un valor, en este caso agregamos los valores de los pixeles para poder graficar despues, Se crean nodos de encabezado para la fila y la columna correspondientes utilizando el método add del tipo de dato header_t, que está asociado a las filas y columnas de la matriz.

Se crea un nuevo nodo de matriz (new_mtx_n) que contiene el valor del elemento a agregar y las posiciones de fila y columna.

Luego, se verifica y ajusta los punteros de encabezado tanto para la fila como para la columna utilizando los métodos  verify_column y verify_row.

```fortran

  subroutine create_dot(self)
    class(matrix_t), intent(inout) :: self
    character(:), allocatable :: code

    code = "digraph G{" // new_line('a')
    code = code // '  node[shape=box];' // new_line('a')
    code = code // '  MTX[ label = "Matrix", style = filled, fillcolor = firebrick1, group = 0 ];' // new_line('a')

    code = code // self%get_content()
    code = code // '}' // new_line('a')

    call self%write_dot(code)
        
  end subroutine create_dot


```
• code = "digraph G{" // new_line('a'): Inicializa la cadena de caracteres code con la declaración de un gráfico dirigido en formato DOT y una nueva línea.

• code = code // ' node[shape=box];' // new_line('a'): Agrega al código DOT la declaración de estilo para los nodos, que en este caso se establece en forma de caja, y una nueva línea.

• code = code // ' MTX[ label = "Matrix", style = filled, fillcolor = firebrick1, group = 0 ];' // new_line('a'): Agrega al código DOT la declaración del nodo principal de la matriz, con etiqueta "Matrix" y estilo de relleno, y una nueva línea.

• code = code // self%get_content(): Agrega al código DOT el contenido de la matriz utilizando el método get_content, que se encarga de generar el contenido de la matriz en formato DOT.

• code = code // '}' // new_line('a'): Agrega al código DOT el cierre del gráfico y una nueva línea.

• call self%write_dot(code): Llama al método write_dot para escribir el código DOT en un archivo.


## archivo abb.f90

Este módulo en Fortran, abb_m, define un tipo de dato abstracto (TDA) abb que representa un árbol binario de búsqueda (ABB). Aquí está el análisis detallado:

```fortran
module abb_m
    implicit none
    private

    type :: Node_t
        integer :: value
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
    end type abb

contains    
    !Subrutinas del tipo abb
    subroutine insert(self, val)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val

        if (.not. associated(self%root)) then
            allocate(self%root)
            self%root%value = val
        else
            call insertRec(self%root, val)
        end if
    end subroutine insert
    recursive subroutine insertRec(root, val)
        type(Node_t), pointer, intent(inout) :: root
        integer, intent(in) :: val
        
        if (val < root%value) then
            if (.not. associated(root%left)) then
                allocate(root%left)
                root%left%value = val
            else
                call insertRec(root%left, val)
            end if
        else if (val > root%value) then
            if (.not. associated(root%right)) then
                allocate(root%right)
                root%right%value = val
            else
                call insertRec(root%right, val)
            end if
        end if
    end subroutine insertRec
```
Define un tipo de dato Node_t que representa un nodo en el árbol. Cada nodo contiene un valor entero (value) y punteros a los nodos hijos izquierdo y derecho.

Define el TDA abb, que contiene un puntero al nodo raíz del árbol. Los procedimientos asociados con este TDA incluyen operaciones como inserción, eliminación y recorridos (preorden, inorden, posorden) en el árbol, así como la generación de un gráfico del árbol.

Tenemos un procedimiento para insertar un nuevo valor en el árbol. Este procedimiento llama a insertRec para realizar la inserción recursiva.

recursive subroutine insertRec: Subrutina recursiva para insertar un nuevo valor en el árbol. Compara el valor a insertar con el valor del nodo actual y decide si insertarlo en el subárbol izquierdo o derecho.

## archivo avl.f90
```fortran
module AVL_Tree_M
  implicit none

  ! Cons
  integer, parameter :: LEFT_HEAVY = -1
  integer, parameter :: BALANCED = 0
  integer, parameter :: RIGHT_HEAVY = +1

  type Node_t
      integer :: Value
      integer :: Factor
      type(Node_t), pointer :: Left => null()
      type(Node_t), pointer :: Right => null()
  end type Node_t

  type Tree_t
      type(Node_t), pointer :: root => null()
      contains
      procedure :: newTree
      procedure :: insert
      procedure :: generateGraph
  end type Tree_t

  contains

  function NewNode(value) result(nodePtr)
    type(Node_t), pointer :: nodePtr
    integer, intent(in) :: value
    allocate(nodePtr)
    nodePtr%Value = value
    nodePtr%Factor = 0
    nodePtr%Left => null()
    nodePtr%Right => null()
  end function NewNode

  subroutine newTree(self)
    class(Tree_t), intent(inout) :: self
    self%root => null()
  end subroutine newTree
  subroutine insert(tree, value)
    class(Tree_t), intent(inout) :: tree
    integer, intent(in) :: value
    logical :: increase

    increase = .false.
    tree%root => insert2(tree%root, value, increase)
  end subroutine insert
```

Cuando se inserta un nuevo valor en el árbol AVL, se realiza una inserción normal como en un árbol binario de búsqueda estándar. Sin embargo, después de la inserción, se comprueba el equilibrio del árbol recorriendo hacia arriba desde el nodo insertado hasta la raíz. Si se encuentra que un nodo viola la propiedad de equilibrio (la diferencia de alturas de sus subárboles es mayor que 1), se realizan rotaciones para restaurar el equilibrio. Estas rotaciones pueden ser de tipo simple (izquierda o derecha) o doble (izquierda-derecha o derecha-izquierda), dependiendo de la situación.

El proceso de inserción y equilibrado garantiza que la altura del árbol AVL permanezca logarítmica en función del número de nodos, lo que permite operaciones de búsqueda, inserción y eliminación en tiempo logarítmico en el peor de los casos.

## archivo header.f90
```fortran
module header_m
  
  private

  type, public :: matrix_node
    integer :: row, col
    character(len=7) :: value
    type(matrix_node), pointer :: left => null()
    type(matrix_node), pointer :: right => null()
    type(matrix_node), pointer :: up => null()
    type(matrix_node), pointer :: down => null()
  end type matrix_node

  !Definición del tipo nodo
  type, public:: header_node
    integer :: position = 0
    type(header_node), pointer :: next => null()
    type(header_node), pointer :: prev => null()
    type(matrix_node), pointer :: access => null()
    contains
        procedure :: init_h_n
  end type header_node

  !Definición de la cabecera
  type, public :: header_t
    type(header_node), pointer :: first => null()
    type(header_node), pointer :: last  => null()
    integer :: size = 0
    contains
      procedure, public :: init_h_t
      procedure :: add
      procedure :: print 
  end type header_t

  contains

  !Funciones
  subroutine init_h_t(self)
    class(header_t), intent(inout) :: self
    self%size = 0
    self%first => null()
    self%last => null()
  end subroutine init_h_t

  subroutine init_h_n(self)
    class(header_node), intent(inout) :: self
    self%position = 0
  end subroutine init_h_n

  function add(self, pos)
    class(header_t), intent(inout) :: self
    integer, intent(in) :: pos
    type(header_node), pointer :: new_node
    type(header_node), pointer :: current
    type(header_node), pointer :: add ! Value to return

    allocate(new_node)
    new_node%position = pos

    ! If the header is empty
    if(self%size == 0) then
      self%first => new_node
      self%last => new_node
      self%size = self%size + 1
      ! Return the new node
      add => new_node 
    else
      ! If the position is already in the first or last node
      if(pos == self%first%position) then
        add => self%first  ! Return the first node
        return
      else if(pos == self%last%position) then
        add => self%last ! Return the last node
        return
      end if
      ! Insert at the beginning
      if(pos < self%first%position) then
        new_node%next => self%first
        self%first%prev => new_node
        self%first => new_node
        self%size = self%size + 1
        ! Return the new node
        add => new_node
      ! Insert at the end
      else if(pos > self%last%position) then
        new_node%prev => self%last
        self%last%next => new_node
        self%last => new_node
        self%size = self%size + 1
        ! Return the new node
        add => new_node
      else
        ! Insert in the middle
        current => self%first%next
        do while(associated(current))
          ! If the position is already in the list
          if(current%position == pos) then
            deallocate(new_node)
            ! Return the current node
            add => current
            exit
          ! If the position is between two nodes
          else if(current%position > pos) then
            new_node%next => current
            new_node%prev => current%prev
            current%prev%next => new_node
            current%prev => new_node
            self%size = self%size + 1
            ! Return the new node
            add => new_node
            exit
          end if
          current => current%next
        end do
      end if
    end if
  end function add
```
Dentro de este módulo, se definen tres tipos de datos principales: matrix_node, header_node, y header_t. El tipo matrix_node representa un nodo en la matriz dispersa, conteniendo información sobre la posición, valor y punteros a nodos vecinos. Por otro lado, header_node representa un nodo en la estructura de encabezado, almacenando información sobre la posición del encabezado, así como punteros al siguiente y anterior en la lista de encabezados, y un enlace al nodo correspondiente en la matriz. Finalmente, header_t es el tipo que representa la cabecera de la estructura de encabezado, incluyendo punteros al primer y último nodo de encabezado en la lista, así como el tamaño de la lista. Además de la definición de tipos de datos, el módulo header_m contiene procedimientos para inicializar la cabecera y los nodos de encabezado.
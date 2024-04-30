module lista_adyacencia_m

    implicit none
    
    type, private :: a_nodo
        integer :: destino
        integer :: peso
        integer :: imp
        type(a_nodo), pointer :: sig => null()
    end type a_nodo

    type, private :: v_nodo
        integer :: val
        type(v_nodo), pointer :: sig => null()
        type(a_nodo), pointer :: raiz => null()

    contains
        procedure :: insertarArista
        procedure :: graficarArista
    end type v_nodo

    type :: ListaAdyacencia
        private
        type(v_nodo), pointer :: raiz => null()

    contains
        procedure :: insert
        procedure :: crearConexion
        procedure :: crearGrafo
        procedure :: dijkstra
        procedure :: dijkstra_max
    end type ListaAdyacencia

contains


subroutine dijkstra_max(self, origen, destino)
    class(ListaAdyacencia), intent(inout) :: self
    integer, intent(in) :: origen
    integer, intent(in) :: destino

    integer, allocatable :: dist(:), index_map(:), peso_sum(:)
    logical, allocatable :: visitado(:)
    integer :: n, max_index, max_dist, i, j
    type(v_nodo), pointer :: aux
    type(a_nodo), pointer :: edge

    print *, "Inicia Dijkstra Max"

    ! Contar el número de nodos y crear el mapeo de índices
    n = 0
    aux => self%raiz
    allocate(index_map(n), source=0)
    do while(associated(aux))
        n = n + 1
        index_map(aux%val) = n
        aux => aux%sig
    end do

    print *, "Número de nodos:", n

    ! Inicializar distancias y visitados
    allocate(dist(n), source=-huge(0))
    allocate(visitado(n), source=.false.)
    allocate(peso_sum(n), source=0)
    dist(index_map(origen)) = 0

    do while (count(.not. visitado) > 0)  ! Continuar hasta que todos los nodos sean visitados
        ! Encontrar el nodo con la distancia máxima, del conjunto de nodos no visitados
        max_dist = -huge(max_dist)
        max_index = -1
        do j = 1, n
            if (.not. visitado(j) .and. dist(j) > max_dist) then
                max_dist = dist(j)
                max_index = j
            end if
        end do


        ! Si no hay camino, terminar
        if (max_index == -1) exit

        ! Marcar el nodo seleccionado como visitado
        visitado(max_index) = .true.

        ! Actualizar los valores de dist de los nodos adyacentes al nodo seleccionado
        aux => self%raiz
        do while(associated(aux))
            if (index_map(aux%val) == max_index) then
                edge => aux%raiz
                do while(associated(edge))
                    i = index_map(edge%destino)
                    if (.not. visitado(i)) then
                        if (dist(max_index) + edge%imp > dist(i)) then
                            dist(i) = dist(max_index) + edge%imp
                            peso_sum(i) = peso_sum(max_index) + edge%peso
                        elseif (dist(i) == -huge(0)) then
                            dist(i) = dist(max_index) + edge%imp
                            peso_sum(i) = peso_sum(max_index) + edge%peso
                        end if
                    end if
                    edge => edge%sig
                end do
            end if
            aux => aux%sig
        end do

    end do

    ! Imprimir la distancia más larga al destino
    if (dist(index_map(destino)) /= -huge(0)) then
        print *, "La distancia más larga al destino es ", dist(index_map(destino))
        print*, "El peso total es: ", peso_sum(index_map(destino))
    else
        print *, "No hay camino al destino"
    end if

    deallocate(dist, visitado, index_map)
end subroutine dijkstra_max

subroutine dijkstra(self, origen, destino)
    class(ListaAdyacencia), intent(inout) :: self
    integer, intent(in) :: origen
    integer, intent(in) :: destino

    integer, allocatable :: dist(:), index_map(:), imp_sum(:)
    logical, allocatable :: visitado(:)
    integer :: n, min_index, min_dist, i, j
    type(v_nodo), pointer :: aux
    type(a_nodo), pointer :: edge

    ! Contar el número de nodos y crear el mapeo de índices
    n = 0
    aux => self%raiz
    allocate(index_map(n), source=0)
    do while(associated(aux))
        n = n + 1
        index_map(aux%val) = n
        aux => aux%sig
    end do

    ! Inicializar distancias, visitados e imp_sum
    allocate(dist(n), source=huge(0))
    allocate(visitado(n), source=.false.)
    allocate(imp_sum(n), source=0)
    dist(index_map(origen)) = 0

    do
        ! Encontrar el nodo con la distancia mínima, del conjunto de nodos no visitados
        min_dist = huge(min_dist)
        min_index = -1
        do j = 1, n
            if (.not. visitado(j) .and. dist(j) < min_dist) then
                min_dist = dist(j)
                min_index = j
            end if
        end do

        ! Si no hay camino, terminar
        if (min_index == -1) exit

        ! Marcar el nodo seleccionado como visitado
        visitado(min_index) = .true.

        ! Actualizar los valores de dist e imp_sum de los nodos adyacentes al nodo seleccionado
        aux => self%raiz
        do while(associated(aux))
            if (index_map(aux%val) == min_index) then
                edge => aux%raiz
                do while(associated(edge))
                    i = index_map(edge%destino)
                    if (.not. visitado(i) .and. dist(min_index) + edge%peso < dist(i)) then
                        dist(i) = dist(min_index) + edge%peso
                        imp_sum(i) = imp_sum(min_index) + edge%imp
                    end if
                    edge => edge%sig
                end do
            end if
            aux => aux%sig
        end do
    end do

    ! Imprimir la distancia más corta al destino y la suma de las "imp"
    if (dist(index_map(destino)) /= huge(0)) then
        print *, "La distancia más corta al destino es ", dist(index_map(destino))
        print *, "Las impresoras son: ", imp_sum(index_map(destino))
    else
        print *, "No hay camino al destino"
    end if

    deallocate(dist, visitado, index_map, imp_sum)

end subroutine dijkstra
    !Funcionalidades de la lista de adyacencia
    subroutine insert(self, valor)
        class(ListaAdyacencia), intent(inout) :: self
        integer, intent(in) :: valor

        type(v_nodo), pointer :: aux
        type(v_nodo), pointer :: nuevo
        allocate(nuevo)
        nuevo%val = valor

        if(.not. associated(self%raiz)) then
            self%raiz => nuevo
        else
            aux => self%raiz
            if(valor < self%raiz%val) then
                nuevo%sig => self%raiz
                self%raiz => nuevo

            else
                do while(associated(aux%sig)) 
                    if(valor < aux%sig%val) then
                        nuevo%sig => aux%sig
                        aux%sig => nuevo
                        exit
                    end if
                    aux => aux%sig
                end do

                if(.not. associated(aux%sig)) then
                    aux%sig => nuevo
                end if
            end if
        end if
    end subroutine insert

    subroutine crearConexion(self, origen, destino, peso, imp)
        class(ListaAdyacencia), intent(inout) :: self
        integer, intent(in) :: origen
        integer, intent(in) :: destino
        integer, intent(in) :: peso
        integer, intent(in) :: imp
        type(v_nodo), pointer :: aux
        aux => self%raiz

        do while(associated(aux))
            if(aux%val == origen) then
                call aux%insertarArista(destino, peso, imp)

                exit
            end if
            aux => aux%sig
        end do
    end subroutine crearConexion

    subroutine crearGrafo(self)
        class(ListaAdyacencia), intent(inout) :: self

        type(v_nodo), pointer :: aux
        character(len=150) :: nodo_dec
        character(len=100) :: comando
        character(len=20) :: nombre
        character(len=10) :: str_aux
        integer :: io
        integer :: i

        aux => self%raiz
        io = 1
        comando = "dot -Tpng ./grafo.dot -o ./grafo.png"
        open(newunit=io, file='./grafo.dot')
        write(io, *) 'digraph g {'
        do while(associated(aux))
            write(str_aux, '(I10)') aux%val
            nombre = '"Nodo'//trim(adjustl(str_aux))//'"'
            nodo_dec = trim(adjustl(nombre))//'[label="'//trim(adjustl(str_aux))//'"]'
            write(io, *) nodo_dec
            call aux%graficarArista(io)
            aux => aux%sig
        end do
        write(io, *) '}'
        close(io)

        call execute_command_line(comando, exitstat=i)

        if (i == 1) then
            print *, "Ocurrió un error al generar la imagen"

        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine crearGrafo



    !Funcionalidades del nodo que contiene los vértices (v_node)
    subroutine insertarArista(self, destino, peso, imp)
        class(v_nodo), intent(inout) :: self
        integer, intent(in) :: destino
        integer, intent(in) :: peso
        integer, intent(in) :: imp
        type(a_nodo), pointer :: nuevo
        type(a_nodo), pointer :: aux

        allocate(nuevo)
        nuevo%destino = destino
        nuevo%peso = peso
        nuevo%imp = imp
        if(.not. associated(self%raiz)) then
            self%raiz => nuevo

        else
            aux => self%raiz
            do while(associated(aux%sig))
                if(aux%destino == destino) then
                    return
                end if
                aux => aux%sig
            end do
            aux%sig => nuevo
        end if
    end subroutine insertarArista

subroutine graficarArista(self, io)
    class(v_nodo), intent(inout) :: self
    integer, intent(in) :: io

    type(a_nodo), pointer :: aux
    character(len=20) :: nombre_origen
    character(len=20) :: nombre_destino
    character(len=20) :: peso
    character(len=20) :: imp
    character(len=10) :: str_aux
    aux => self%raiz

    write(str_aux, '(I10)') self%val
    nombre_origen = '"Nodo'//trim(adjustl(str_aux))//'"'

    do while(associated(aux))
        if(aux%destino < self%val) then
            write(str_aux, '(I10)') aux%destino
            nombre_destino = '"Nodo'//trim(adjustl(str_aux))//'"'
            write(str_aux, '(I10)') aux%peso  ! Use the actual weight of the edge
            peso = 'Peso: '//trim(adjustl(str_aux))
            write(str_aux, '(I10)') aux%imp  ! Use the actual imp of the edge
            imp = 'Impresoras: '//trim(adjustl(str_aux))
            write(io, *) nombre_origen//'->'//nombre_destino//' [label="'//trim(peso)//', ' // trim(imp)//'", dir = both]'
        end if
        aux => aux%sig
    end do
end subroutine graficarArista

end module lista_adyacencia_m
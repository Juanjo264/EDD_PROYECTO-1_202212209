module linkedList
  implicit none

  type :: linked_list
    type(node), pointer :: head => null() ! head of the list

    contains
      procedure :: push
      procedure :: print
      procedure :: delete_by_position
      procedure :: graficarVentanillas

  end type linked_list

  type :: node
    integer :: value
    type(node), pointer :: next
  end type node

  contains

  subroutine push(self, value)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: value

    type(node), pointer :: newNode
    allocate(newNode)

    newNode%value = value
    newNode%next => null()

    if (.not. associated(self%head)) then
      self%head => newNode
    else
      newNode%next => self%head
      self%head => newNode
    end if

    print *, 'pushed:: ', value
  end subroutine push

  subroutine delete_by_position(self, position)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: position
    type(node), pointer :: current, previous
    integer :: counter

    current => self%head
    previous => null()

    if(position == 1) then
      self%head => current%next
      deallocate(current)
      return
    end if

    counter = 1
    do while (associated(current) .and. counter < position)
      previous => current
      current => current%next
      counter = counter + 1
    end do

    if (.not. associated(current)) then
      print *, 'Position not found'
      return
    end if

    previous%next => current%next
    deallocate(current)
  end subroutine delete_by_position

  subroutine print(self)
    class(linked_list), intent(in) :: self

    type(node), pointer :: current

    current => self%head

    do while (associated(current))
      print *, current%value
      current => current%next
    end do
  end subroutine print

  subroutine graficarVentanillas(this, filename)
      class(linked_list), intent(in) :: this
      character(len=*), intent(in) :: filename

      integer :: unit
      type(node), pointer :: current
      integer :: count

      ! Abrir el archivo DOT
      open(unit, file=filename, status='replace')
      write(unit, *) 'digraph Ventanillas {'
      write(unit, *) '    rankdir=TB;' ! Ordenar los nodos verticalmente
      write(unit, *) '    label="Ventanillas";' ! Agregar un título a las ventanillas
      write(unit, *) '    node [shape=box, style=filled, color=blue, fillcolor=pink];' ! Aplicar atributos a todos los nodos
      ! Escribir nodos y conexiones
      current => this%head
      count = 0
      do while (associated(current))
          count = count + 1
          write(unit, *) '    "Ventanilla', count, '" [label="Numero: ', current%value, '"];'
          if (associated(current%next)) then
              write(unit, *) '    "Ventanilla', count, '" -> "Ventanilla', count+1, '";'
          end if
          current => current%next
      end do 

      ! Cerrar el archivo DOT
      write(unit, *) '}'
      close(unit)

      ! Generar el archivo PNG utilizando Graphviz
      call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')

      print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
  end subroutine graficarVentanillas

end module linkedList
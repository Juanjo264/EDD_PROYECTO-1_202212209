# MANUAL TECNICO 

## Requisitos del sistema

Para que nuestro programa funcione correctamente tenemos que tener instalados algunos programas:

1. Instalar Visual Studio Code en nuestra computadora.
2. Instalar Fortran, preferiblemente la version mas reciente o que sea estable.
3. Tener instalado Graphviz, para las graficas.
4. Utilizar extenciones para previsualizar grafos y archivos.

## Archivo json.f90
Este archivo contiene todo nuestro codigo. 

```python
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
```
Contiene nuestro menu inicial con todas las especificaciones del menu inicial. 

```python
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

```
Esta subrutina carga datos de clientes desde un archivo JSON y los inserta en una cola de clientes (colaPrueba).
Utiliza un módulo llamado json_module para trabajar con archivos JSON.
Abre el archivo JSON especificado por la ruta y lee los datos de los clientes, como el ID, nombre, imágenes, etc.
Inserta cada cliente en la cola colaPrueba.
Finalmente, llama a una función graficar() para generar una representación visual de la cola.
``` python
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

```
Esta subrutina permite al usuario establecer el número de ventanillas.
Crea un número especificado de ventanillas y pilas asociadas.
Genera un archivo DOT para representar visualmente las ventanillas y utiliza Graphviz para convertirlo en una imagen PNG.


``` python
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



```
Esta subrutina imprime el contenido de la cola de clientes.
Recorre la cola y muestra los detalles de cada cliente, como el ID, nombre y imágenes


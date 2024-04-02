# MANUAL DE USUARIO 

## Requisitos del sistema

Para que nuestro programa funcione correctamente tenemos que tener instalados algunos programas:

1. Instalar Visual Studio Code en nuestra computadora.
2. Instalar Fortran, preferiblemente la version mas reciente o que sea estable.
3. Tener instalado Graphviz.
4. Instalar un lector de pdf.
  

## Menu Inicial
Iniciamos nuestro programa y nos dira esto, es un menu de inicio. 

```fortran
-----------Pixel Print Studio.------------
 Por favor ingrese su nombre de usuario y contrase├▒a.
 Usuario:
admin
 Contrase├▒a:
EDD2024
```
Entonces aqui colocamos nuestro usuario y contrasena, dependiendo que tipo de persona seamos, si tenemos un usuario o si queremos ingresar como administrador para acceder a las opciones. 
### ADMIN
Si ingresamos como administrador, tendremos las siguientes opciones:

```fortran
 1. ver arbol B
 2. Operaciones sobre los usuarios (insertar, modificar y eliminar)
 3. Operaciones de carga masiva de usuarios
 4. Salir
```
Aqui podemos eccoger entre ver el arbol B o hacer las diferentes operaciones con los usuarios, o cargar a los usuarios. 

### USUARIO
Como usuario tendremos las siguietes opciones:

```fortran
Inicio de sesi├│n exitoso como usuario.
 Por favor seleccione una opci├│n:
 1. Visualizar reportes de las estructuras
 2. Navegaci├│n y gestion de im├ígenes
 3. Opciones de carga masiva
 4. Salir
```
Y ya podemos escoger entre las diferentes opciones que tenemos, tenemos que ingresar con nuestro "dpi".

aqui hay un ejemplo de las diferentes iamgenes que podemos crear: 

![Alt text](matrioo.PNG)

```fortran
Aqui hay un ejemplo del funcionamiento:  Inicio de sesi├│n exitoso como usuario.
 Por favor seleccione una opci├│n:
 1. Visualizar reportes de las estructuras
 2. Navegaci├│n y gestion de im├ígenes
 3. Opciones de carga masiva
 4. Salir
3
 Ingrese la ruta del archivo para cargar imagenes
imagenes.json
 Ingrese la ruta del archivo para cargar capas
a.json
 Ingrese cuantas capas quiere
1
 Ingrese el orden (preorder, inorder, postorder)
inorder
 Ingrese cuantos IDs de imagen quiere
2
 Ingrese los IDs de las imagenes
2
7
 Archivo actualizado existosamente.
 Archivo actualizado existosamente.
 Archivo actualizado existosamente.
Escribiendo en inorder:
7 - 20 - 

```

Entonces primero cargamos las imagenes, despues las capas, luego seleccionamos cuentas capas vamos a querer para la grafica y seleccionamos el orden, despues ingresamos cuentos ID queremos, y escribimos los ID que queramos graficar, y por ultimo se graficara nuestra imagen, y nos mostrara el orden que decidimos.





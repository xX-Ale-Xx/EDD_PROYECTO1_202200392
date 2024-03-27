program main
  use json_module
  use matrix_m
  use Arbolbb
  use avl_tree
  implicit none

  type(json_file) :: json   ! Se declara una variable del tipo json_file
  type(json_value), pointer :: listPointer, personPointer, attributePointer , pixelPointer, listPointer2 &
  , pixelesPointer, ArrayPointer, ValuePointer, idCapaPointer, capaValue, imgListPointer
  type(json_core) :: jsonc 
  type(matrix_t) :: mtx
  type(abb) :: tree
  type(Tree_t), pointer :: treeAvl
  logical :: found
  integer :: choice
  integer :: i, size, size2, j
  do
    call printMenu()
    read(*,*) choice

    select case (choice)
        case (1)
            call option1()
        case (2)
            exit
        case default
            print *, "Opción no válida. Introduce un número del 1 al 3."
    end select
end do

contains


subroutine printMenu()
  print *, "Menú:"
  print *, "1. Iniciar sesion"
  print *, "2. Salir"
  print *, "Elige una opción:"
end subroutine

subroutine printMenuAdmin()
  print *, "Menú:"
  print *, "1. Cargar clientes"
  print *, "2. Registrar cliente"
  print *, "3. Modificar cliente"
  print *, "4. Eliminar cliente"
  print *, "5. ver arbol B"
  print *, "6. Salir"
  print *, "Elige una opción:"
end subroutine printMenuAdmin

subroutine printMenuCliente()
  print *, "Menú:"
  print *, "1. Cargar capas"
  print *, "2. Cargar imagenes"
  print *, "3. Cargar Albumes"
  print *, "4. Generar imagen recorrido limitado"
  print *, "5. Generar imagen por amplitud"
  print *, "6. Generar imagen por id de capas"
  print *, "7. Generar reportes"
  print *, "8. Cerrar sesion"
  print *, "Elige una opción:"
end subroutine printMenuCliente

subroutine accionesCliente()
  do
    call printMenuCliente()
    read(*,*) choice

    select case (choice)
        case (1)
            call cargarCapas()
        case (2)
            call cargarImgs()
        case (4)
            call gImgsRLimit()
        case (5)
            call gImgsRAmplitud()
        case (8)
            exit
        case default
            print *, "Opción no válida"
    end select
end do
end subroutine accionesCliente

subroutine gImgsRLimit()
  integer :: op
  integer :: pasos

  do
  print *, "Menú:"
  print *, "1. pre-order"
  print *, "2. in-order"
  print *, "3. post-order"
  print *, "4. Salir"
  print *, "Elige una opción:"
    read(*,*) choice

    select case (choice)
        case (1)
          print *, "Ingrese que imagen desea generar"
          read(*,*) op
          print *, "Ingrese cuantos pasos dese realizar"
          read(*,*) pasos
            call treeAvl%searchNode(op, pasos, 1)
        case (2)
          print *, "Ingrese que imagen desea generar"
          read(*,*) op
          print *, "Ingrese cuantos pasos dese realizar"
          read(*,*) pasos
            call treeAvl%searchNode(op, pasos, 2)
        case (3)
          print *, "Ingrese que imagen desea generar"
          read(*,*) op
          print *, "Ingrese cuantos pasos dese realizar"
          read(*,*) pasos
            call treeAvl%searchNode(op, pasos, 3)
        case (4)
            exit
        case default
            print *, "Opción no válida"
    end select
end do
end subroutine

subroutine gImgsRAmplitud()
  integer :: op
  print *, "Ingrese que imagen desea generar"
          read(*,*) op
  call treeAvl%searchNode(op, 1, 4)
end subroutine

subroutine cargarCapas()
  
  integer :: id
  integer :: fila
  integer :: columna
  character(len=3)::k
  character(:), allocatable :: color 
  character(len = 225)::nombreArchivoJs
  print *, "Escribe la ruta del archivo .js"
  read(*, '(A)') nombreArchivoJs
  call json%initialize()    ! Se inicializa el módulo JSON
  call json%load(filename=nombreArchivoJs)  ! Se carga el archivo JSON llamado 'data.json'
        ! Se imprime el contenido del archivo JSON (opcional
  
  call json%info('',n_children=size)

  call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
  call json%get('', listPointer, found)

  do i = 1, size  
    call mtx%init()  ! Se inicia un bucle sobre el número de elementos en el JSON
      call jsonc%get_child(listPointer, i, personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
      call jsonc%get_child(personPointer, 'id_capa', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
      if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
          call jsonc%get(attributePointer, id)
          print *, "este es el id"  ! Se obtiene el valor y se asigna a la variable 'nombre'
          print *, id
                     ! Se imprime el nombre sin espacios en blanco adicionales
      end if

      call jsonc%get_child(personPointer, 'pixeles', pixelesPointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
      if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
          call jsonc%info(pixelesPointer, n_children=size2)! Se obtiene el valor y se asigna a la variable 'nombre'
          do j=1, size2

            call jsonc%get_child(pixelesPointer, j, pixelPointer, found=found)
            if(found) then
              call jsonc%get_child(pixelPointer, 'fila', attributePointer, found=found)
              if(found) then 
                call jsonc%get(attributePointer, fila)
                print *, fila
              end if

              call jsonc%get_child(pixelPointer, 'columna', attributePointer, found=found)
              if(found) then 
                call jsonc%get(attributePointer, columna)
                print *, columna
              end if

              call jsonc%get_child(pixelPointer, 'color', attributePointer, found=found)
              if(found) then 
                call jsonc%get(attributePointer, color)
                print *, color
              end if

              call mtx%add(fila, columna, color)


            end if
            
          end do
          
        end if
        call tree%insert(id, mtx)   
  end do


call json%destroy()
  
end subroutine cargarCapas


subroutine cargarImgs()
  type(abb) :: treeForAvl
  
  integer :: id, valor
  
  character(len = 225)::nombreArchivoJs
  print *, "Escribe la ruta del archivo .js"
  read(*, '(A)') nombreArchivoJs
  call treeAvl%newTree()
  call json%initialize()    ! Se inicializa el módulo JSON
  call json%load(filename=nombreArchivoJs)  ! Se carga el archivo JSON llamado 'data.json'
        ! Se imprime el contenido del archivo JSON (opcional
  
  call json%info('',n_children=size)

  call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
  call json%get('', imgListPointer, found)

  do i = 1, size  
      
      call jsonc%get_child(imgListPointer, i, ValuePointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
      call jsonc%get_child(ValuePointer, 'id', idCapaPointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
      if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
          call jsonc%get(idCapaPointer, id)
          print *, "este es el id"  ! Se obtiene el valor y se asigna a la variable 'nombre'
          print *, id
                     ! Se imprime el nombre sin espacios en blanco adicionales
      end if

      call jsonc%get_child(ValuePointer, 'capas', idCapaPointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
      if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
          call jsonc%info(idCapaPointer, n_children=size2)! Se obtiene el valor y se asigna a la variable 'nombre'
          do j=1, size2

            call jsonc%get_child(idCapaPointer, j, capaValue, found=found)
            if(found) then
              
              if(found) then 
                call jsonc%get(capaValue, valor)
                print *, valor
                call mtx%init() 
               
                call tree%buscar_valor_arbol(valor, mtx)
                call treeForAvl%insert(valor, mtx)
              end if

            end if
            
          end do
          
        end if
           call treeAvl%insert(id, treeForAvl)
           call treeForAvl%inicializar_arbol_vacio()
  end do
call json%destroy()
end subroutine cargarImgs

subroutine option1()
  character(len=20) :: nombre
  character(len=20) :: contrasenia

  print *, "Ingrese el nombre"
  read(*,*) nombre
  print *, "Ingrese la contrasenia"
  read(*,*) contrasenia

  if(trim(nombre)=="admin" .and. trim(contrasenia)== "123") then
    call printMenuAdmin()
  else if(trim(nombre)=="javier" .and. trim(contrasenia)=="123") then
  call accionesCliente()
  end if
end subroutine option1

subroutine registrar()
  character :: nombre
  character :: DPI
  character :: contrasenia
  print *, "Ingrese el nombre"
  read(*,*) nombre
  print *, "Ingrese el DPI"
  read(*,*) DPI
  print *, "Ingrese la contrasen"
  read(*,*) contrasenia
end subroutine registrar

end program main

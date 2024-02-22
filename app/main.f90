program main
  !use DynamicQueueModule
  use json_module
  use linked_list_uso
  use cola_clientes
  use lista_ventanillas
  implicit none
  integer :: choice
  type(json_file) :: json   ! Se declara una variable del tipo json_file
    type(json_value), pointer :: listPointer, personPointer, attributePointer  ! Se declaran punteros a variables del tipo json_value
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    character(:), allocatable :: nombreCliente  ! Se declara una cadena de caracteres que se asignará dinámicamente
    integer :: imgPCliente, imgGCliente
    integer :: i, size        ! Se declaran variables enteras
    logical :: found
  type(cola) :: clientesEnCola
  type(ListaVentanillas) :: lista_vent
  call inicializar_cola(clientesEnCola)
  do
      call printMenu()
      read(*,*) choice

      select case (choice)
          case (1)
              call option1()
          case (2)
              call option2()
          case (3)
              call option3()
          case (4)
              call option4()
          case (5)
              call option5()
          case (6)
              exit
          case default
              print *, "Opción no válida. Introduce un número del 1 al 4."
      end select
  end do

contains

  subroutine printMenu()
      print *, "Menú:"
      print *, "1. Parametros iniciales"
      print *, "2. Ejecutar paso"
      print *, "3. Estado en memoria de las estructuras"
      print *, "4. Reportes"
      print *, "5. Acerca de"
      print *, "6. Salir"
      print *, "Elige una opción:"
  end subroutine

  subroutine printParametrosIniciales()
      print *, "a. Carga masiva de clientes"
      print *, "b. Cantidad de ventanillas"
  end subroutine

  subroutine option1()
    integer :: contador
    integer :: cantidad 
    character(len=5) :: caracterId
      character(1) :: choice2
      print *, "Has seleccionado la Opción 1."
      call printParametrosIniciales()
      read(*, '(A)') choice2
      select case (choice2)
      case ("a")
        print *, "hola a"
       ! call json%initialize()
    !call json%load(filename="C:\Users\Javier Avila\Desktop\fortranjs\fortranjs\datos.json")
        call json%initialize()    ! Se inicializa el módulo JSON
        call json%load(filename='C:\Users\Javier Avila\Desktop\fortranjs\fortranjs\datos.json')  ! Se carga el archivo JSON llamado 'data.json'
              ! Se imprime el contenido del archivo JSON (opcional
        
        call json%info('',n_children=size)
    
        call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call json%get('', listPointer, found)
    
        do i = 1, size  
            print *, "id", i                        ! Se inicia un bucle sobre el número de elementos en el JSON
            call jsonc%get_child(listPointer, i, personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
            call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
            if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
                call jsonc%get(attributePointer, nombreCliente)  ! Se obtiene el valor y se asigna a la variable 'nombre'
                print *, trim(nombreCliente)           ! Se imprime el nombre sin espacios en blanco adicionales
            end if

            call jsonc%get_child(personPointer, 'img_g', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
            if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
                call jsonc%get(attributePointer, imgGCliente)  ! Se obtiene el valor y se asigna a la variable 'nombre'
                print *, imgGCliente           ! Se imprime el nombre sin espacios en blanco adicionales
            end if
            
            call jsonc%get_child(personPointer, 'img_p', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
            if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
                call jsonc%get(attributePointer, imgPCliente)  ! Se obtiene el valor y se asigna a la variable 'nombre'
                print *, imgPCliente           ! Se imprime el nombre sin espacios en blanco adicionales
            end if
            
            call agregar_cliente(clientesEnCola, i, nombreCliente, imgGCliente, imgPCliente)

        end do

        

    ! Liberar recursos
    call json%destroy()

      case ("b")
          
          print *, "Ingrese la cantidad de ventanillas que desea:" 
          read(*,*) cantidad
          call agregar_ventanilla(lista_vent, cantidad)
          call recorrer_lista_ventanillas(lista_vent)

      case default
          print *, "Opción no válida. Introduce 'a' o 'b'."
  end select
  end subroutine

  subroutine option2()
      print *, "Has seleccionado la Opción 2."
      ! Aquí puedes agregar el código correspondiente a la opción 2
  end subroutine

  subroutine option3()
      print *, "Has seleccionado la Opción 3."
      ! Aquí puedes agregar el código correspondiente a la opción 3
  end subroutine

  subroutine option4()
    print *, "Has seleccionado la Opción 3."
    ! Aquí puedes agregar el código correspondiente a la opción 3
end subroutine

subroutine option5()
  print *, "Has seleccionado la Opción 5."
  print *, "Nombre: Javier Alejandro Avila Flores"
  print *, "Curso: Laboratorio Estructuras de Datos B"
  print *, "Carnet: 202200392"
  
end subroutine


end program main



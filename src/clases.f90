module pilaImg
  implicit none

  ! Puedes ajustar el tamaño máximo de la palabra según tus necesidades
  integer, parameter :: max_longitud_palabra = 100

  ! Aquí puedes agregar todos los datos que quieras que lleve tu nodo, como un nombre u otra información
  type, public :: Nodo
    character(max_longitud_palabra) :: palabra
    character(max_longitud_palabra) :: nomClientePila
    type(Nodo), pointer :: siguiente
  end type Nodo

  type, public :: Pilas
    type(Nodo), pointer :: tope => null()
  end type Pilas

contains

  subroutine agregar(p, palabra, nomClientePila)
    type(Pilas), intent(inout) :: p
    character(len=*), intent(in) :: palabra
    character(len=*), intent(in) :: nomClientePila
    type(Nodo), pointer :: nuevo_nodo

    allocate(nuevo_nodo)
    nuevo_nodo%palabra = trim(palabra)
    nuevo_nodo%nomClientePila = trim(nomClientePila)
    nuevo_nodo%siguiente => p%tope
    p%tope => nuevo_nodo
  end subroutine agregar


  subroutine eliminar(p, palabra)
    type(Pilas), intent(inout) :: p
    character(len=*), intent(out) :: palabra
    type(Nodo), pointer :: nodo_aux
  
    if (associated(p%tope)) then
      nodo_aux => p%tope
      palabra = trim(nodo_aux%palabra)
      p%tope => nodo_aux%siguiente
      deallocate(nodo_aux)
    else
      ! Aquí podrías manejar el caso cuando la pila está vacía
      print *, 'La pila está vacía.'
    end if
  end subroutine eliminar

 
  function estaVacia(p) result(vacia)
    type(Pilas), intent(in) :: p
    logical :: vacia
    vacia = .not. associated(p%tope)
  end function estaVacia

end module pilaImg


module lista_ventanillas
  use pilaImg
  implicit none

  type, public :: NodoVentanilla
      integer :: id
      logical :: enUsoPorCliente
      type(Pilas) :: pilaImagenes
      type(NodoVentanilla), pointer :: siguiente => null()
  end type NodoVentanilla

  type, public :: ListaVentanillas
  type(NodoVentanilla), pointer :: inicio => null()
  end type ListaVentanillas

  contains

  subroutine pasarClienteVentanilla(lista, nombreClienteAtender, imgPAtender, imgGAtender, pasosActuales)
    class(ListaVentanillas), intent(inout) :: lista
    character(*), intent(in) :: nombreClienteAtender
    integer, intent(in) :: imgPAtender, imgGAtender, pasosActuales
    type(NodoVentanilla), pointer :: actual
    integer :: contP, contG

    if (associated(lista%inicio)) then
        actual => lista%inicio
        do while (associated(actual))
            if (.not. actual%enUsoPorCliente) then
                ! La ventanilla está disponible
                actual%enUsoPorCliente = .true.
                print *, "Cliente atendido en la ventanilla ", actual%id
                print *, "Nombre del cliente: ", nombreClienteAtender
                print *, "Imagen P a atender: ", imgPAtender
                print *, "Imagen G a atender: ", imgGAtender
                do contP = 1, imgPAtender
                  call agregar(actual%pilaImagenes, "imgP", nombreClienteAtender)
                end do
                do contG = 1, imgGAtender
                  call agregar(actual%pilaImagenes, "imgG", nombreClienteAtender)
                end do
                exit
            end if
            actual => actual%siguiente
        end do
        if (pasosActuales>1) then
          call atenderClientes(lista)
        end if

    else
        print *, "La lista está vacía."
    end if
end subroutine pasarClienteVentanilla

  subroutine agregar_ventanilla(lista, cantidad)
      class(ListaVentanillas), intent(inout) :: lista
      type(NodoVentanilla), pointer :: nuevoNodo, actual
      integer, intent(in) :: cantidad
      integer :: i

      do i = 1, cantidad
          allocate(nuevoNodo)
          nuevoNodo%id = i
          nuevoNodo%enUsoPorCliente = .false.
          nuevoNodo%siguiente => null() 

          if (associated(lista%inicio)) then
              actual => lista%inicio
              do while(associated(actual%siguiente))
                  actual => actual%siguiente
              end do
              actual%siguiente => nuevoNodo              
          else
              lista%inicio => nuevoNodo
          end if
      end do
  end subroutine agregar_ventanilla

  subroutine atenderClientes(lista)
    class(ListaVentanillas), intent(inout) :: lista
    type(NodoVentanilla), pointer :: actual
    character(max_longitud_palabra) :: palabraEliminada

    if (associated(lista%inicio)) then
        actual => lista%inicio
        do while (associated(actual))
            if (actual%enUsoPorCliente) then
                
                call eliminar(actual%pilaImagenes, palabraEliminada)
                print *, "Cliente atendido en la ventanilla ", actual%id
                print *, "Imagen eliminada de la ventanilla ", actual%id, ": ", palabraEliminada
                if (estaVacia(actual%pilaImagenes)) then
                  actual%enUsoPorCliente = .false.
                end if
            end if
            actual => actual%siguiente
        end do
    else
        print *, "La lista de ventanillas está vacía."
    end if
end subroutine atenderClientes


  subroutine recorrer_lista_ventanillas(lista)
    class(ListaVentanillas), intent(in) :: lista
    type(NodoVentanilla), pointer :: actual

    if (associated(lista%inicio)) then
        actual => lista%inicio
        do while (associated(actual))
            print *, "ID del nodo: ", actual%id
            actual => actual%siguiente
        end do
    else
        print *, "La lista está vacía."
    end if
end subroutine recorrer_lista_ventanillas

end module lista_ventanillas

module cola_clientes
  implicit none

  private
  type, public :: cliente
    integer :: id_cliente
    character(len=50) :: nombre_cliente
    integer :: imagen_g
    integer :: imagen_p
  end type cliente

  type, public :: nodo
    type(cliente) :: datos
    type(nodo), pointer :: siguiente
  end type nodo

  type, public :: cola
    type(nodo), pointer :: frente
    type(nodo), pointer :: final
  end type cola

  public :: inicializar_cola, agregar_cliente, pop_cliente

contains

  subroutine inicializar_cola(c)
    type(cola), intent(out) :: c
    c%frente => null()
    c%final => null()
  end subroutine inicializar_cola

  subroutine agregar_cliente(c, id_cliente, nombre_cliente, imagen_g, imagen_p)
    type(cola), intent(inout) :: c
    integer, intent(in) :: id_cliente, imagen_g, imagen_p
    character(len=*), intent(in) :: nombre_cliente
    type(nodo), pointer :: nuevo_nodo

    allocate(nuevo_nodo)
    nuevo_nodo%datos%id_cliente = id_cliente
    nuevo_nodo%datos%nombre_cliente = nombre_cliente
    nuevo_nodo%datos%imagen_g = imagen_g
    nuevo_nodo%datos%imagen_p = imagen_p
    nuevo_nodo%siguiente => null()

    if (associated(c%final)) then
      c%final%siguiente => nuevo_nodo
    else
      c%frente => nuevo_nodo
    end if

    c%final => nuevo_nodo
  end subroutine agregar_cliente


  subroutine pop_cliente(c, id, nombre, imagen_g, imagen_p)
    type(cola), intent(inout) :: c
    integer, intent(out) :: id, imagen_g, imagen_p
    character(len=*), intent(out) :: nombre
    type(nodo), pointer :: nodo_aux

    if (.not. associated(c%frente)) then
      print *, "La cola está vacía."
      return
    end if

    id = c%frente%datos%id_cliente
    nombre = c%frente%datos%nombre_cliente
    imagen_g = c%frente%datos%imagen_g
    imagen_p = c%frente%datos%imagen_p

    nodo_aux => c%frente
    c%frente => c%frente%siguiente
    deallocate(nodo_aux)
  end subroutine pop_cliente

end module cola_clientes

module ColaModuleImpresion
  implicit none

  type Nodo
    character(20) :: palabra
    type(Nodo), pointer :: siguiente => null()
  end type Nodo

  type :: ColaDeImpresion
    type(Nodo), pointer :: frente => null()
    type(Nodo), pointer :: final => null()
  end type ColaDeImpresion

  contains

  subroutine agregar_a_cola(cola, palabra)
    implicit none
    type(ColaDeImpresion), intent(inout) :: cola
    character(20), intent(in) :: palabra
    type(Nodo), pointer :: nuevoNodo

    allocate(nuevoNodo)
    nuevoNodo%palabra = palabra
    nuevoNodo%siguiente => null()

    if (associated(cola%frente)) then
      cola%final%siguiente => nuevoNodo
      cola%final => nuevoNodo
    else
      cola%frente => nuevoNodo
      cola%final => nuevoNodo
    end if
  end subroutine agregar_a_cola

  subroutine imprimir_cola(cola)
    implicit none
    type(ColaDeImpresion), intent(in) :: cola
    type(Nodo), pointer :: actual

    if (associated(cola%frente)) then
      actual => cola%frente
      do while (associated(actual))
        print *, actual%palabra
        actual => actual%siguiente
      end do
    else
      print *, "La cola está vacía."
    end if
  end subroutine imprimir_cola

  subroutine pop_cola(cola, palabra)
    implicit none
    type(ColaDeImpresion), intent(inout) :: cola
    character(20), intent(out) :: palabra
    type(Nodo), pointer :: temp

    if (associated(cola%frente)) then
      palabra = cola%frente%palabra
      temp => cola%frente
      cola%frente => cola%frente%siguiente
      deallocate(temp)
    else
      print *, "La cola está vacía."
      palabra = ""
    end if
  end subroutine pop_cola

end module ColaModuleImpresion
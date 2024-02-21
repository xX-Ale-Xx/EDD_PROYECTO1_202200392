module pilaImg
  implicit none
  private

  ! Aqui podes agregar todos los datos que queres que lleve tu nodo, algun nombre o lo que queras
  type, public :: Nodo
  integer :: dato
  type(Nodo), pointer :: siguiente
  end type Nodo

  type, public :: Pilas
  type(Nodo), pointer :: tope => null()
  end type Pilas

  contains

  subroutine agregar(p, valor)
      type(Pilas), intent(inout) :: p
      integer, intent(in) :: valor
      type(Nodo), pointer :: nuevo_nodo
  
      allocate(nuevo_nodo)
      nuevo_nodo%dato = valor
      nuevo_nodo%siguiente => p%tope
      p%tope => nuevo_nodo
  end subroutine agregar

  subroutine eliminar(p, valor, exito)
      type(Pilas), intent(inout) :: p
      integer, intent(out) :: valor
      logical, intent(out) :: exito
      type(Nodo), pointer :: nodo_aux
  
      if (associated(p%tope)) then
          nodo_aux => p%tope
          valor = nodo_aux%dato
          p%tope => nodo_aux%siguiente
          deallocate(nodo_aux)
          exito = .true.
      else
          exito = .false.
      end if
  end subroutine eliminar

end module pilaImg

module lista_ventanillas
  use pilaImg
  implicit none

  type, public :: NodoVentanilla
      integer :: id
      type(Pilas) :: pilaImagenes
      type(NodoVentanilla), pointer :: siguiente => null()
  end type NodoVentanilla

  type, public :: ListaVentanillas
  type(NodoVentanilla), pointer :: inicio => null()
  end type ListaVentanillas

  contains

  subroutine agregar_ventanilla(lista, cantidad)
      class(ListaVentanillas), intent(inout) :: lista
      type(NodoVentanilla), pointer :: nuevoNodo, actual
      integer, intent(in) :: cantidad
      integer :: i

      do i = 1, cantidad
          allocate(nuevoNodo)
          nuevoNodo%id = i
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

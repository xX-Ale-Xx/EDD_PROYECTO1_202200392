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


  subroutine eliminar(p, nomClientePila)
    type(Pilas), intent(inout) :: p
    character(len=*), intent(out) :: nomClientePila
    type(Nodo), pointer :: nodo_aux
  
    if (associated(p%tope)) then
      nodo_aux => p%tope
      nomClientePila = trim(nodo_aux%nomClientePila)
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

!----------------------------------------------------
!  VENTANILLAS
!----------------------------------------------------
module lista_ventanillas
  use ColaDeImpresion_
  use pilaImg
  
  use ListaCircular
  implicit none

  type, public :: NodoVentanilla
      integer :: id
      logical :: enUsoPorCliente
      type(Pilas) :: pilaImagenes
      character(len = 100) :: nomClienteVentanilla
      integer :: contG, contP, imgPPasarCola, imgGPasarCola

      type(NodoVentanilla), pointer :: siguiente => null()
  end type NodoVentanilla

  type, public :: ListaVentanillas
  type(NodoVentanilla), pointer :: inicio => null()
  
  end type ListaVentanillas
  type(ListaCircularType) :: ListaCircularDeEspera
  type(ColaImpresion) :: ColaDeImpresionImgGrandes
  type(ColaImpresion) :: ColaDeImpresionImgPequenas
  

  contains

  
  subroutine pasarClienteVentanilla(lista, nombreClienteAtender, imgPAtender, imgGAtender, pasosActuales)
    class(ListaVentanillas), intent(inout) :: lista
    character(*), intent(in) :: nombreClienteAtender
    integer, intent(in) :: imgPAtender, imgGAtender, pasosActuales
    type(NodoVentanilla), pointer :: actual
    integer :: sumarImgEnVent

    if (associated(lista%inicio)) then
        actual => lista%inicio
        do while (associated(actual))
            if (.not. actual%enUsoPorCliente) then
                ! La ventanilla está disponible
                actual%contP = imgPAtender
                actual%contG = imgGAtender
                actual%imgPPasarCola = imgPAtender
                actual%imgGPasarCola = imgGAtender
                 
                actual%nomClienteVentanilla = nombreClienteAtender
                actual%enUsoPorCliente = .true.
                print *, "Cliente atendido en la ventanilla ", actual%id
                print *, "Nombre del cliente: ", nombreClienteAtender
                print *, "Imagen P a atender: ", imgPAtender
                print *, "Imagen G a atender: ", imgGAtender
                
                exit
            else if (actual%enUsoPorCliente) then
              
              if (actual%contG >0) then
                print *,"agrego a la pila la imagen de: " // actual%nomClienteVentanilla
                call agregar(actual%pilaImagenes, "imgG", actual%nomClienteVentanilla)
                  actual%contG = actual%contG - 1
                 
              else if (actual%contP >0) then
                print *,"agrego a la pila la imagen de: " // actual%nomClienteVentanilla
                call agregar(actual%pilaImagenes, "imgP", actual%nomClienteVentanilla)
                actual%contP = actual%contP - 1
              else 
                sumarImgEnVent = actual%imgGPasarCola + actual%imgPPasarCola
                
                actual%enUsoPorCliente = .false.
                call atenderClientes(actual, sumarImgEnVent)

              end if
            end if
            actual => actual%siguiente
        end do


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

  subroutine atenderClientes(actual, imgSumar)
    integer, intent(in) :: imgSumar
    type(NodoVentanilla), pointer :: actual
    character(max_longitud_palabra) :: palabraEliminada
    integer :: con
    
    do con = 1, imgSumar 
      call eliminar(actual%pilaImagenes,palabraEliminada)
    end do
    write(*,*)"Se vacio la pila con las img de:  "// palabraEliminada
    
    call AgregarNodoCircular(ListaCircularDeEspera,palabraEliminada, actual%imgGPasarCola, actual%imgPPasarCola , actual%id)
    

    do con = 1, actual%imgGPasarCola
      call Encolar(ColaDeImpresionImgGrandes, actual%nomClienteVentanilla)
    end do

    do con = 1, actual%imgPPasarCola
      call Encolar(ColaDeImpresionImgPequenas, actual%nomClienteVentanilla)
    end do
    print*, "COLA IMPRESORA GRANDE______________________________________"
    call ImprimirCola(ColaDeImpresionImgGrandes)
    print*, "COLA IMPRESORA PEQUENA_____________________________________"
    call ImprimirCola(ColaDeImpresionImgPequenas)

end subroutine atenderClientes


  subroutine recorrer_lista_ventanillas(lista)
    class(ListaVentanillas), intent(in) :: lista
    type(NodoVentanilla), pointer :: actual

    if (associated(lista%inicio)) then
        actual => lista%inicio
        do while (associated(actual))
            print *, "Vantanilla ", actual%id
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
    character(len=100) :: nombre_cliente
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

!-------------------------------------
!COLASIMPRESIONES
!-------------------------------------

module ColaDeImpresion_
  implicit none

  type, public :: Nodo
      character(50) :: nombreClienteImprimiendo
      
      type(Nodo), pointer :: siguiente
  end type Nodo

  type, public :: ColaImpresion
      type(Nodo), pointer :: frente => null()
      type(Nodo), pointer :: final => null()
  end type ColaImpresion

  contains


  subroutine Encolar(c, nombreCliente)
      type(ColaImpresion), intent(inout) :: c
      character(50), intent(in) :: nombreCliente
      

      type(Nodo), pointer :: nuevoNodo
      allocate(nuevoNodo)
      nuevoNodo%nombreClienteImprimiendo = nombreCliente
      
      nuevoNodo%siguiente => null()

      if (.not. associated(c%frente)) then
          c%frente => nuevoNodo
          c%final => nuevoNodo
      else
          c%final%siguiente => nuevoNodo
          c%final => nuevoNodo
      end if
  end subroutine Encolar

  subroutine DesencolarImpresion(c, nombreCliente)
      type(ColaImpresion), intent(inout) :: c
      character(50), intent(out) :: nombreCliente
      

      type(Nodo), pointer :: nodoDesencolado

      if (.not. associated(c%frente)) then
          print *, 'La cola está vacía.'
          return
      end if

      nodoDesencolado => c%frente
      c%frente => nodoDesencolado%siguiente

      nombreCliente = nodoDesencolado%nombreClienteImprimiendo
      

      deallocate(nodoDesencolado)
  end subroutine DesencolarImpresion

  subroutine ImprimirCola(c)
    type(ColaImpresion), intent(in) :: c

    type(Nodo), pointer :: nodoActual

    if (.not. associated(c%frente)) then
        print *, 'La cola está vacía.'
        return
    end if

    print *, 'Elementos en la cola de impresión:'
    nodoActual => c%frente
    do while (associated(nodoActual))
        print *, 'Cliente: ', nodoActual%nombreClienteImprimiendo
        nodoActual => nodoActual%siguiente
    end do
end subroutine ImprimirCola

subroutine ColaVacia(c, vacia)
  type(ColaImpresion), intent(in) :: c
  logical, intent(inout) :: vacia

  vacia =  associated(c%final)
end subroutine ColaVacia

end module ColaDeImpresion_


!---------------------------------------------------------
!-
!Lista clientes atendidos
!-
!--------------------------------------------------------

module ListaClientesMod
implicit none
  type, public :: Nodo
      character(50) :: nombreClienteAtendido
      integer :: CantImgImpresas
      integer :: VentanillaAtendida
      integer :: CantidadTotalPasos
      type(Nodo), pointer :: siguiente => null()
  end type Nodo

  type, public :: ListaClientes
      type(Nodo), pointer :: cabeza => null()
  end type ListaClientes

contains

  subroutine agregarCliente(lista, nombre, cantImpresas, ventanilla, cantPasos)
      ! Agrega un nuevo cliente a la lista
      type(ListaClientes), intent(inout) :: lista
      character(50), intent(in) :: nombre
      integer, intent(in) :: cantImpresas, ventanilla, cantPasos

      type(Nodo), pointer :: nuevoNodo
      allocate(nuevoNodo)
      nuevoNodo%nombreClienteAtendido = nombre
      nuevoNodo%CantImgImpresas = cantImpresas
      nuevoNodo%VentanillaAtendida = ventanilla
      nuevoNodo%CantidadTotalPasos = cantPasos

      nuevoNodo%siguiente => lista%cabeza
      lista%cabeza => nuevoNodo
  end subroutine agregarCliente

  subroutine imprimirLista(lista)
      ! Imprime la lista de clientes
      type(ListaClientes), intent(in) :: lista
      type(Nodo), pointer :: actual

      actual => lista%cabeza
      do while (associated(actual))
          print *, "Cliente: ", actual%nombreClienteAtendido
          print *, "Cantidad de Imágenes Impresas: ", actual%CantImgImpresas
          print *, "Ventanilla Atendida: ", actual%VentanillaAtendida
          print *, "Cantidad Total de Pasos: ", actual%CantidadTotalPasos
          print *, "----------------------------------------"
          actual => actual%siguiente
      end do
  end subroutine imprimirLista

end module ListaClientesMod


!------------------------------------------------
! LISTA SIMPLE IMAGENES IMPRESAS
!________________________________________________


module listaImagenesImpresas
  implicit none

  ! Definición del tipo de nodo
  type, public :: nodoImp
    character(10) :: imgTipo
    integer :: imgId
    type(nodoImp), pointer :: siguiente
  end type nodoImp

  

  ! Definición del tipo de lista
  type, public :: listaImgImp
    type(nodoImp), pointer :: cabeza => null()
  end type listaImgImp

  

contains

  ! Subrutina para agregar un nodo a la lista
  subroutine agregarNodoImpresion(L, tipo, id)
    type(listaImgImp), intent(inout) :: L
    character(10), intent(in) :: tipo
    integer, intent(in) :: id

    type(nodoImp), pointer :: nuevoNodo

    ! Crear un nuevo nodo y asignar los valores
    allocate(nuevoNodo)
    nuevoNodo%imgTipo = tipo
    nuevoNodo%imgId = id

    ! Enlazar el nuevo nodo al inicio de la lista
    nuevoNodo%siguiente => L%cabeza
    L%cabeza => nuevoNodo
  end subroutine agregarNodoImpresion

  ! Subrutina para imprimir la lista
  subroutine imprimirLista(L)
    type(listaImgImp), intent(in) :: L
    type(nodoImp), pointer :: actual

    if (.not. associated(L%cabeza)) then
      write(*, '(A)') 'La lista de imágenes impresas está vacía.'
      return
    end if

    actual => L%cabeza
    do while (associated(actual))
      print *, 'Tipo:', actual%imgTipo, ', ID:', actual%imgId
      actual => actual%siguiente
    end do
end subroutine imprimirLista

end module listaImagenesImpresas

!-----------------------------------
!LISTA DE ESPERA
!-----------------------------------


module ListaCircular
  use listaImagenesImpresas
  implicit none
  
  type, public :: NodoCircular
    character(50) :: nombreClienteEspera
    integer :: cantImgGEspera
    integer :: cantImgPEspera, idVentanillaAtendio
    type(listaImgImp) :: lista_imagenes_impresas
    type(NodoCircular), pointer :: siguiente
    type(NodoCircular), pointer :: anterior
  end type NodoCircular

  type, public :: ListaCircularType
    type(NodoCircular), pointer :: cabeza => null()
    type(NodoCircular), pointer :: ultimo => null()
  end type ListaCircularType

  contains

  subroutine AgregarNodoCircular(lista, nombreCliente, cantImgG, cantImgP, idVentanillaAtendio)
    type(ListaCircularType), intent(inout) :: lista
    type(NodoCircular), pointer :: nuevoNodo
    character(50), intent(in) :: nombreCliente
    integer, intent(in) :: cantImgG, cantImgP, idVentanillaAtendio
    

    allocate(nuevoNodo)
    nuevoNodo%nombreClienteEspera = nombreCliente
    nuevoNodo%cantImgGEspera = cantImgG
    nuevoNodo%cantImgPEspera = cantImgP
    nuevoNodo%idVentanillaAtendio = idVentanillaAtendio

    if (associated(lista%cabeza)) then
      nuevoNodo%siguiente => lista%cabeza
      nuevoNodo%anterior => lista%ultimo
      lista%ultimo%siguiente => nuevoNodo
      lista%cabeza%anterior => nuevoNodo
    else
      nuevoNodo%siguiente => nuevoNodo
      nuevoNodo%anterior => nuevoNodo
    end if

    lista%cabeza => nuevoNodo
    lista%ultimo => nuevoNodo
  end subroutine AgregarNodoCircular

  subroutine ImprimirListaCircular(lista)
    type(ListaCircularType), intent(in) :: lista
    type(NodoCircular), pointer :: nodoActual

    if (associated(lista%cabeza)) then
      ! Forward traversal
      write(*, '(A)') 'Forward traversal:'
      nodoActual => lista%cabeza
      do
        write(*, '(A, I5, I5)') trim(nodoActual%nombreClienteEspera), &
                                 nodoActual%cantImgGEspera, nodoActual%cantImgPEspera
        write(*, '(A)') 'Imágenes Impresas:'
        call imprimirLista(nodoActual%lista_imagenes_impresas)
        if (nodoActual%nombreClienteEspera == lista%ultimo%nombreClienteEspera) exit
        nodoActual => nodoActual%siguiente
      end do

      ! Backward traversal
      write(*, '(A)') 'Backward traversal:'
      nodoActual => lista%ultimo
      do
        write(*, '(A, I5, I5)') trim(nodoActual%nombreClienteEspera), &
                                 nodoActual%cantImgGEspera, nodoActual%cantImgPEspera
        write(*, '(A)') 'Imágenes Impresas:'
        call imprimirLista(nodoActual%lista_imagenes_impresas)
        if (nodoActual%nombreClienteEspera == lista%cabeza%nombreClienteEspera) exit
        nodoActual => nodoActual%anterior
      end do
    else
      write(*, '(A)') 'La lista está vacía.'
    end if
  end subroutine ImprimirListaCircular

end module ListaCircular

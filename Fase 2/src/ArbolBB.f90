module Arbolbb
    use matrix_m
    implicit none
    private

    type :: Node_t_bb
        integer :: value
        type(matrix_t) :: matriz
        type(Node_t_bb), pointer :: right => null()
        type(Node_t_bb), pointer :: left => null()
    end type Node_t_bb

    type, public :: abb
        type(Node_t_bb), pointer :: root => null()

    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: inorder
        procedure :: posorder
        procedure :: graph
        procedure :: buscar_valor_arbol
        procedure :: inicializar_arbol_vacio
        procedure :: profundidad_arbol
        procedure :: recorridoAmplitud
    end type abb

contains    
    !Subrutinas del tipo abb
    subroutine insert(self, val, mtx)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val
        type(matrix_t), intent(in) :: mtx
  !type(matrix_t) :: matriz
        if (.not. associated(self%root)) then
            allocate(self%root)
            self%root%value = val
            self%root%matriz = mtx
        else
            call insertRec(self%root, val, mtx)
        end if
    end subroutine insert
    recursive subroutine insertRec(root, val, mtx)
        type(Node_t_bb), pointer, intent(inout) :: root
        integer, intent(in) :: val
        type(matrix_t), intent(in) :: mtx
        if (val < root%value) then
            if (.not. associated(root%left)) then
                allocate(root%left)
                root%left%value = val
                root%left%matriz = mtx
            else
                call insertRec(root%left, val, mtx)
            end if
        else if (val > root%value) then
            if (.not. associated(root%right)) then
                allocate(root%right)
                root%right%value = val
                root%right%matriz = mtx
            else
                call insertRec(root%right, val, mtx)
            end if
        end if
    end subroutine insertRec

    subroutine buscar_valor_arbol(self, valor, mtx)
        class(abb), intent(inout) :: self
        integer, intent(in) :: valor
        type(matrix_t), intent(out) :: mtx
        type(Node_t_bb), pointer :: nodo
        nodo => null()
        ! Inicializamos el puntero al nodo encontrado como nulo
        
        ! Llamamos a la subrutina interna que realiza la búsqueda
        call buscar_valor_rec(self%root, valor, nodo)
        
        ! Si se encontró el valor, lo imprimimos
        if (associated(nodo)) then
            write(*, '(A, I0, A)') "Se encontró el valor ", nodo%value, " en el árbol."
            mtx = nodo%matriz
        else
            write(*, '(A, I0, A)') "No se encontró el valor ", valor, " en el árbol."
        end if
    end subroutine buscar_valor_arbol
    
    recursive subroutine buscar_valor_rec(nodo, valor, nodo_encontrado)
        type(Node_t_bb), pointer :: nodo
        integer, intent(in) :: valor
        type(Node_t_bb), pointer :: nodo_encontrado
        
        if (associated(nodo)) then
            if (nodo%value == valor) then
                ! Si el valor del nodo actual coincide con el valor buscado, lo asignamos al nodo_encontrado
                nodo_encontrado => nodo
            else if (valor < nodo%value) then
                ! Si el valor buscado es menor, buscamos en el subárbol izquierdo
                call buscar_valor_rec(nodo%left, valor,  nodo_encontrado)
            else
                ! Si el valor buscado es mayor, buscamos en el subárbol derecho
                call buscar_valor_rec(nodo%right, valor,  nodo_encontrado)
            end if
        end if
    end subroutine buscar_valor_rec

    subroutine delete(self, val)
        class(abb), intent(inout) :: self
        integer, intent(inout) :: val
    
        self%root => deleteRec(self%root, val)
    end subroutine delete

    recursive function deleteRec(root, value) result(res)
        type(Node_t_bb), pointer :: root
        integer, intent(in) :: value
        type(Node_t_bb), pointer :: res
        type(Node_t_bb), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (value < root%value) then
            root%left => deleteRec(root%left, value)
        else if (value > root%value) then
            root%right => deleteRec(root%right, value)
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
                return
            else
                call getMajorOfMinors(root%left, temp)
                root%value = temp%value
                root%matriz = temp%matriz
                root%left => deleteRec(root%left, temp%value)
            end if
        end if

        res => root
    end function deleteRec
    recursive subroutine getMajorOfMinors(root, major)
        type(Node_t_bb), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors

    subroutine preorder(self, limit)
        class(abb), intent(in) :: self
        type(matrix_t) :: mtx
        integer, intent(in):: limit
        integer :: contador = 0
        call mtx%init()
        print *, "ola "
        call preorderRec(self%root, mtx, limit, contador)
        write(*, '()')
        call mtx%create_dot("resultado_preorder")
    end subroutine preorder
    recursive subroutine preorderRec(root, mtx, limit, contador)
        type(Node_t_bb), pointer, intent(in) :: root
        type(matrix_t), intent(inout) :: mtx
      
        integer, intent(in):: limit
        integer, intent(inout) :: contador
        if(associated(root)) then
            ! RAIZ - IZQ - DER
            if(contador <= limit-1) then
            contador = contador + 1
           !write(a, '(A, I0)') "ola_", contador

            call root%matriz%recorrerMatriz(mtx)
            
            write(*, '(I0 A)', advance='no') root%value, " - "
            call preorderRec(root%left, mtx, limit, contador)
            call preorderRec(root%right, mtx, limit, contador)
            
            else
                return
            end if
        end if
    end subroutine preorderRec

    subroutine inorder(self, limit)
        class(abb), intent(in) :: self
        type(matrix_t) :: mtx
        integer, intent(in):: limit
        integer :: contador = 0
        call mtx%init()
        call inordenRec(self%root, mtx, limit, contador)
        print *, ""
        call mtx%create_dot("resultado_inorder")
    end subroutine inorder
    recursive subroutine inordenRec(root , mtx, limit, contador)
        type(Node_t_bb), pointer, intent(in) :: root
        type(matrix_t), intent(inout) :: mtx
      
        integer, intent(in):: limit
        integer, intent(inout) :: contador

        if(associated(root)) then
            ! IZQ - RAIZ - DER
            if(contador <= limit-1) then
            
            call inordenRec(root%left,  mtx, limit, contador)
            contador = contador + 1
            call root%matriz%recorrerMatriz(mtx)
            write(*, '(I0 A)', advance='no') root%value, " - "
            call inordenRec(root%right, mtx, limit, contador)
            else
                return
            end if
        end if
    end subroutine inordenRec

    subroutine posorder(self, limit)
        class(abb), intent(in) :: self
        type(matrix_t) :: mtx
        integer, intent(in):: limit
        integer :: contador = 0
        call mtx%init()
        call posordenRec(self%root, mtx, limit, contador)
        print *, ""
        call mtx%create_dot("resultado_posorder")
    end subroutine posorder
    recursive subroutine posordenRec(root, mtx, limit, contador)
        type(Node_t_bb), pointer, intent(in) :: root
        type(matrix_t), intent(inout) :: mtx
      
        integer, intent(in):: limit
        integer, intent(inout) :: contador

        if(associated(root)) then
            ! IZQ - DER - RAIZ
            if(contador <= limit-1) then
            
            call posordenRec(root%left, mtx, limit, contador)
            call posordenRec(root%right, mtx, limit, contador)
            contador = contador + 1
            call root%matriz%recorrerMatriz(mtx)
            write(*, '(I0 A)', advance='no') root%value, " - "
            else
                return
            end if
        end if
    end subroutine posordenRec

    subroutine graph(self, filename)
        class(abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        
        createNodes = ''
        linkNodes = ''

        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

        if (associated(self%root)) then
            call RoamTree(self%root, createNodes, linkNodes)
        end if
        
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot_abb(filename, dotStructure)
        print *, "Archivo actualizado existosamente."
    end subroutine graph
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(Node_t_bb), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_value

        if (associated(current)) then
            ! SE OBTIENE INFORMACION DEL NODO ACTUAL
          address = get_address_memory_abb(current)
          write(str_value, '(I0)') current%Value
          createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
          ! VIAJAMOS A LA SUBRAMA IZQ
          if (associated(current%Left)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory_abb(current%Left)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = ""];' // new_line('a')
    
          end if
          ! VIAJAMOS A LA SUBRAMA DER
          if (associated(current%Right)) then
            address = get_address_memory_abb(current)
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory_abb(current%Right)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = ""];' // new_line('a')
          end if
    
          call RoamTree(current%Left, createNodes, linkNodes)
          call RoamTree(current%Right, createNodes, linkNodes)
        end if
    end subroutine RoamTree
    subroutine write_dot_abb(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename
        
        
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        
        open(10, file="graph/"//dot_filename, status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)

        ! Genera la imagen PNG
        call system("dot -Tpng graph/"// dot_filename //" -o graph/" // png_filename)
        call system("start graph/" // png_filename)
    end subroutine write_dot_abb

    function get_address_memory_abb(node) result(address)
        
        type(Node_t_bb), pointer :: node
        character(len=20) :: address
        
        integer*8 :: i
    
        i = loc(node) 
        
        write(address, 10) i 
        10 format(I0)
    
    end function get_address_memory_abb

    subroutine inicializar_arbol_vacio(self)
        class(abb), intent(inout) :: self
        
        allocate(self%root)
        self%root => null()
        
    end subroutine inicializar_arbol_vacio

    subroutine recorridoAmplitud (self)
        class(abb), intent(in) :: self 
        type(matrix_t) :: matriz 
        integer :: alturaArbol, nivel

        call matriz%init()

        alturaArbol = self%profundidad_arbol()
        
        do nivel = 0, alturaArbol
        call recorrerNivel(self%root, nivel, matriz)
        end do
        
        call matriz%create_dot("resultado_Amplitud")
        end subroutine recorridoAmplitud

        recursive subroutine recorrerNivel(root, nivel, matriz)
        type (Node_t_bb), pointer, intent(in) :: root
        integer, intent(in) :: nivel 
        type(matrix_t), intent(inout) :: matriz 
        
        if(associated(root)) then
            if(nivel == 0) then

            call root%matriz%recorrerMatriz(matriz)
            
            print *, "Id: ", root%value
            else
            call recorrerNivel(root%left, nivel-1, matriz)
            call recorrerNivel(root%right, nivel-1, matriz)
            end if
        else 
            return
        end if

       
        
    end subroutine


    function profundidad_arbol(self) result(profundidad)
        class(abb), intent(in) :: self
        integer :: profundidad
    
       
        profundidad = profundidad_arbol_rec(self%root)    
       

    end function profundidad_arbol

    recursive function profundidad_arbol_rec(nodo) result(profundidadNodo)
        type(Node_t_bb), pointer, intent(in) :: nodo
        integer :: profundidadIzquierda, profundidadDerecha, profundidadNodo
    
        if (.not. associated(nodo)) then
            profundidadNodo = 0
            return
        end if

        profundidadIzquierda = profundidad_arbol_rec(nodo%left)
        profundidadDerecha = profundidad_arbol_rec(nodo%right)
        profundidadNodo = 1 + max(profundidadIzquierda, profundidadDerecha)

    end function profundidad_arbol_rec

  

end module Arbolbb
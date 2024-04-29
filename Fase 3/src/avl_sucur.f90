module Avl_Tree
    !use abb_m
    use uuid_module
    use hash_table
    implicit none
  
    ! Cons
    integer, parameter :: LEFT_HEAVY = -1
    integer, parameter :: BALANCED = 0
    integer, parameter :: RIGHT_HEAVY = +1
  
    type Node_t
        integer :: id,Factor
        character(:), allocatable ::dept,direccion,password
       type(HashTable):: tablahash
        type(Node_t), pointer :: Left => null()
        type(Node_t), pointer :: Right => null()
    end type Node_t
  
    type Tree_t
        type(Node_t), pointer :: root => null()
        contains
        procedure :: new_avl
        procedure :: insert_node
        procedure :: graph_avl_tree
        procedure :: searchNode
        procedure :: searchNode_hash
        procedure :: searchNode_listar
    end type Tree_t

    contains

    function new_branch(id,dept,dir,contra) result(nodePtr)
        type(Node_t), pointer :: nodePtr
        integer, intent(in) :: id
        character(:), allocatable,intent(in) ::dept,dir,contra
        allocate(nodePtr)
        nodePtr%id = id
        nodePtr%dept = dept
        nodePtr%direccion = dir
        nodePtr%password = contra
        nodePtr%Factor = 0
        nodePtr%Left => null()
        nodePtr%Right => null()
    end function new_branch

    subroutine new_avl(self)
        class(Tree_t), intent(inout) :: self
        self%root => null()
    end subroutine new_avl

    function rotationII(n, n1) result(result_node)
        type(Node_t), pointer :: n, n1, result_node
        
        n%Left => n1%Right
        n1%Right => n
        if (n1%Factor == -1) then
            n%Factor = 0
            n1%Factor = 0
        else
            n%Factor = -1
            n1%Factor = 1
        end if
        result_node => n1
    end function rotationII

    function rotationDD(n, n1) result(result_node)
        type(Node_t), pointer :: n, n1, result_node

        n%Right => n1%Left
        n1%Left => n
        if (n1%Factor == 1) then
            n%Factor = 0
            n1%Factor = 0
        else
            n%Factor = 1
            n1%Factor = -1
        end if
        result_node => n1
    end function rotationDD

    function rotationDI(n, n1) result(result_node)
        type(Node_t), pointer :: n, n1, result_node, n2
    
        n2 => n1%Left
        n%Right => n2%Left
        n2%Left => n
        n1%Left => n2%Right
        n2%Right => n1
        if (n2%Factor == 1) then
            n%Factor = -1
        else
            n%Factor = 0
        end if
        if (n2%Factor == -1) then
            n1%Factor = 1
        else
            n1%Factor = 0
        end if
        n2%Factor = 0
        result_node => n2
    end function rotationDI

    function rotationID(n, n1) result(result_node)
        type(Node_t), pointer :: n, n1, result_node, n2
        n2 => n1%Right
        n%Left => n2%Right
        n2%Right => n
        n1%Right => n2%Left
        n2%Left => n1
        if (n2%Factor == 1) then
            n1%Factor = -1
        else
            n1%Factor = 0
        end if
        if (n2%Factor == -1) then
            n%Factor = 1
        else
            n%Factor = 0
        end if
        n2%Factor = 0
        result_node => n2
    end function rotationID

    subroutine insert_node(tree, id,dir,dept,contra)
        class(Tree_t), intent(inout) :: tree
        integer, intent(in) :: id
        logical :: increase
        character(:), allocatable,intent(in) ::dept,dir,contra
        increase = .false.
        tree%root => recursive_insert(tree%root, id,dir,dept,contra, increase)
    end subroutine insert_node

    recursive function recursive_insert(root, id,dir,dept,contra, increase) result(result_node)
        type(Node_t), pointer :: root, result_node, n1
        logical :: increase
        character(:), allocatable,intent(in) ::dept,dir,contra
        integer, intent(in) :: id

        if (.not. associated(root)) then
            allocate(result_node)
            root => new_branch(id,dir,dept,contra)
            increase = .true.
        else if (id < root%id) then
            root%Left => recursive_insert(root%Left, id,dir,dept,contra, increase)
            if (increase) then
            select case (root%Factor)
                case (RIGHT_HEAVY)
                    root%Factor = 0
                    increase = .false.
                case (BALANCED)
                    root%Factor = -1
                case (LEFT_HEAVY)
                    n1 => root%Left
                    if (n1%Factor == -1) then
                        root => rotationII(root, n1)
                    else
                        root => rotationID(root, n1)
                    end if
                    increase = .false.
            end select
            end if
        else if (id > root%id) then
            root%Right => recursive_insert(root%Right,id,dir,dept,contra, increase)
            if (increase) then
                select case (root%Factor)
                case (RIGHT_HEAVY)
                    n1 => root%Right
                    if (n1%Factor == 1) then
                        root => rotationDD(root, n1)
                    else
                        root => rotationDI(root, n1)
                    end if
                    increase = .false.
                case (BALANCED)
                    root%Factor = 1
                case (LEFT_HEAVY)
                    root%Factor = 0
                    increase = .false.
                end select
            end if
        end if

        result_node => root
    end function recursive_insert

    recursive subroutine recursive_print(root, nombre, io)
        type(Node_t), pointer, intent(in) :: root
        character(len=36), intent(in) :: nombre
        integer :: io

        character(len=36) :: Right
        character(len=36) :: Left

        Right = generate_uuid()
        Left = generate_uuid()

        if(associated(root)) then
            write(io, *) '"Node_t'//nombre//'"[label= "', root%id, '"]'

            if(associated(root%Left)) then
                write(io, *) '"Node_t'//nombre//'"->"Node_t'//Left//'"'
            end if

            if(associated(root%Right)) then
                write(io, *) '"Node_t'//nombre//'"->"Node_t'//Right//'"'
            end if
            call recursive_print(root%Left, Left, io)
            call recursive_print(root%Right, Right, io)
        end if
    end subroutine recursive_print

    subroutine graph_avl_tree(self)
        class(Tree_t), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./avl_tree.dot")
        comando = "dot -Tpng ./avl_tree.dot -o ./avl_tree.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%root)) then
            call recursive_print(self%root, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graph_avl_tree

    subroutine searchNode(tree, id, contra, found)
        class(Tree_t), intent(inout) :: tree
        integer, intent(in) :: id
        character(:), allocatable,intent(in) ::contra
        type(Node_t), pointer :: foundNode
        logical, intent(inout) :: found
    
        if (.not. associated(tree%root)) then
            print *, "El árbol está vacío."
            return
        end if
    
        foundNode => searchNodeRecursive(tree%root, id, contra)
    
        if (associated(foundNode)) then
            print *, "El id ", foundNode%id, " se encontró, con la contraseña: ", foundNode%password           
            found = .true.
        else
            print *, "El valor no se encontro", id
        end if
    end subroutine searchNode
    
    recursive function searchNodeRecursive(currentNode, id, contra) result(foundNode)
        type(Node_t), pointer :: currentNode, foundNode
        character(:), allocatable,intent(in) ::contra
        integer, intent(in) :: id
    
        if (.not. associated(currentNode)) then
            foundNode => null()
            return
        end if
    
        if (currentNode%id == id) then
            foundNode => currentNode
            return
        elseif (id < currentNode%id) then
            foundNode => searchNodeRecursive(currentNode%Left, id, contra)
        else
            foundNode => searchNodeRecursive(currentNode%Right, id, contra)
        end if
    end function searchNodeRecursive

    ! busqueda para agregar tecnico
    subroutine searchNode_hash(tree, id, tecnico)
        class(Tree_t), intent(inout) :: tree
        integer, intent(in) :: id
        type(HashTable), intent(in) :: tecnico
        type(Node_t), pointer :: foundNode
    
        if (.not. associated(tree%root)) then
            print *, "El árbol está vacío."
            return
        end if
    
        foundNode => searchNodeRecursive_hash(tree%root, id, tecnico)
    
        if (associated(foundNode)) then
            print *, "El id ", foundNode%id, " se encontró, con la contraseña: ", foundNode%password           
            
        else
            print *, "El valor no se encontro", id
        end if
    end subroutine searchNode_hash
    
    recursive function searchNodeRecursive_hash(currentNode, id, tecnico) result(foundNode)
        type(Node_t), pointer :: currentNode, foundNode
        type(HashTable), intent(in) :: tecnico
        integer, intent(in) :: id
    
        if (.not. associated(currentNode)) then
            foundNode => null()
            return
        end if
    
        if (currentNode%id == id) then
            foundNode => currentNode
            foundNode%tablahash = tecnico
            print *, "Se agrego el tecnico a la tabla hash"
            call foundNode%tablahash%print()
            return
        elseif (id < currentNode%id) then
            foundNode => searchNodeRecursive_hash(currentNode%Left, id, tecnico)
        else
            foundNode => searchNodeRecursive_hash(currentNode%Right, id, tecnico)
        end if
    end function searchNodeRecursive_hash


    subroutine searchNode_listar(tree, id)
        class(Tree_t), intent(inout) :: tree
        integer, intent(in) :: id
        
        type(Node_t), pointer :: foundNode
    
        if (.not. associated(tree%root)) then
            print *, "El árbol está vacío."
            return
        end if
    
        foundNode => searchNodeRecursive_listar(tree%root, id)
    
        if (associated(foundNode)) then
            
            call foundNode%tablahash%print()
            
        else
            print *, "El valor no se encontro", id
        end if
    end subroutine searchNode_listar
    
    recursive function searchNodeRecursive_listar(currentNode, id) result(foundNode)
        type(Node_t), pointer :: currentNode, foundNode
       
        integer, intent(in) :: id
    
        if (.not. associated(currentNode)) then
            foundNode => null()
            return
        end if
    
        if (currentNode%id == id) then
            foundNode => currentNode
            return
        elseif (id < currentNode%id) then
            foundNode => searchNodeRecursive_listar(currentNode%Left, id)
        else
            foundNode => searchNodeRecursive_listar(currentNode%Right, id)
        end if
    end function searchNodeRecursive_listar



end module Avl_Tree
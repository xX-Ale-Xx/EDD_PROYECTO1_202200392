module routes
    implicit none
    type arista
        integer :: id
        integer :: weight
        integer :: parent_id
        integer :: printers
        type(arista), pointer :: next => null()
        type(arista), pointer :: prev => null()
    end type arista
    type lista_aristas
        type(arista), pointer :: head => null()
        type(arista), pointer :: tail => null()
    contains
        procedure :: add_ordenado
        procedure :: add_ordenado_2 
        procedure :: pop
        procedure :: is_empty
        procedure :: merge
        procedure :: merge_2
        procedure :: add_weight
    end type lista_aristas
    type result
        integer :: id
        integer :: weight
        integer :: printers
        type(result), pointer :: next => null()
    end type result
    type result_list
        integer :: total_weight, total_printers
        type(result), pointer :: head => null()
        type(result), pointer :: tail => null()
    contains
        procedure :: add_result
        procedure :: print
    end type result_list
    type node
        integer :: id 
        type(lista_aristas) :: vecinos
        type(node), pointer :: next => null()
    end type node
    type grafo
        integer :: n_nodes
        type(node), pointer :: head => null()
    contains
        procedure :: insert_data
        procedure :: insert_data_2
        procedure :: add_node
        procedure :: add_arista
        procedure :: add_arista_2
        procedure :: get_nodo
        procedure :: show
        procedure :: graficar
        procedure :: reset_grafo
    end type grafo
    type analyzer
        type(grafo):: grafo_data 
    contains
        procedure :: set_grafo
        procedure :: get_shortest_path
        procedure :: get_longest_path
    end type analyzer
contains


    subroutine add_ordenado_2(this, id, weight, parent_id, sort_by_id, printers)
        class(lista_aristas), intent(inout) :: this
        integer, intent(in) :: id, weight, parent_id, printers
        logical, intent(in) :: sort_by_id
        type(arista), pointer :: new_arista
        type(arista), pointer :: current
        type(arista), pointer :: previous
        allocate(new_arista)
        new_arista%id = id
        new_arista%weight = weight
        new_arista%parent_id = parent_id
        new_arista%printers = printers

        if (.not. associated(this%head)) then
            this%head => new_arista
            this%tail => new_arista
            return
        end if

        current => this%head
        previous => null()

        if (sort_by_id) then
            do while (associated(current))
                if (current%id > id) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        else
            do while (associated(current))
                if (current%weight < weight) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        end if

        if (.not. associated(previous)) then
            new_arista%next => this%head
            this%head%prev => new_arista
            this%head => new_arista
        else if (.not. associated(current)) then
            this%tail%next => new_arista
            new_arista%prev => this%tail
            this%tail => new_arista
        else
            previous%next => new_arista
            new_arista%prev => previous
            new_arista%next => current
            current%prev => new_arista
        end if
    end subroutine add_ordenado_2

    function pop(this) result(arista_res)
        class(lista_aristas), intent(inout) :: this
        type(arista), pointer :: arista_res
        if (.not. associated(this%head)) then
            arista_res => null()
            return
        end if
        arista_res => this%head
        this%head => this%head%next
        if (associated(this%head)) then
            this%head%prev => null()
        else
            this%tail => null()
        end if
    end function pop
    function is_empty(this) result(res)
        class(lista_aristas), intent(in) :: this
        logical :: res
        res = .not. associated(this%head)
    end function is_empty
    

    
    subroutine add_ordenado(this, id, weight, parent_id, sort_by_id, impresoras)
        class(lista_aristas), intent(inout) :: this
        integer, intent(in) :: id, weight, parent_id, impresoras
        logical, intent(in) :: sort_by_id
        type(arista), pointer :: new_arista
        type(arista), pointer :: current
        type(arista), pointer :: previous
        allocate(new_arista)
        new_arista%id = id
        new_arista%weight = weight
        new_arista%parent_id = parent_id
        new_arista%printers = impresoras

        if (.not. associated(this%head)) then
            this%head => new_arista
            this%tail => new_arista
            return
        end if

        current => this%head
        previous => null()

        if (sort_by_id) then
            do while (associated(current))
                if (current%id > id) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        else
            do while (associated(current))
                if (current%weight > weight) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        end if

        if (.not. associated(previous)) then
            new_arista%next => this%head
            this%head%prev => new_arista
            this%head => new_arista
        else if (.not. associated(current)) then
            this%tail%next => new_arista
            new_arista%prev => this%tail
            this%tail => new_arista
        else
            previous%next => new_arista
            new_arista%prev => previous
            new_arista%next => current
            current%prev => new_arista
        end if
    end subroutine add_ordenado


    subroutine merge_2(this,  to_merge)
        class(lista_aristas), intent(inout) :: this
        class(lista_aristas), intent(in) :: to_merge
        type(arista), pointer :: current

        current => to_merge%head
        do while (associated(current))
            call this%add_ordenado_2(current%id, current%weight, current%parent_id, .FALSE., current%printers)
            current => current%next
        end do
        
    end subroutine merge_2


    subroutine merge(this,  to_merge)
        class(lista_aristas), intent(inout) :: this
        class(lista_aristas), intent(in) :: to_merge
        type(arista), pointer :: current

        current => to_merge%head
        do while (associated(current))
            call this%add_ordenado(current%id, current%weight, current%parent_id, .FALSE., current%printers)
            current => current%next
        end do
        
    end subroutine merge

    subroutine add_weight(this, weight, printers)
        class(lista_aristas), intent(inout) :: this
        integer, intent(in) :: weight, printers
        type(arista), pointer :: current
        current => this%head
        do while (associated(current))
            current%weight = current%weight + weight
            current%printers = current%printers + printers
            current => current%next
        end do        
    end subroutine add_weight
    ! Result list methods
    subroutine add_result(this,  id, weight, printers)
        class(result_list), intent(inout) :: this
        integer, intent(in) :: id, weight, printers
        type(result), pointer :: new_result
        allocate(new_result)
        new_result%id = id
        new_result%weight = weight
        new_result%printers = printers
        if (.not. associated(this%head)) then
            this%head => new_result
            this%tail => new_result
            return
        end if
        this%tail%next => new_result
        this%tail => new_result  
        this%total_weight = this%tail%weight  
        this%total_printers = this%tail%printers
    end subroutine add_result
    subroutine print(this)
        class(result_list), intent(in) :: this
        type(result), pointer :: current
        current => this%head
        do while (associated(current))
            write(*,'(A, I0, A, I0)') 'Node: ', current%id, ", Acumulated Weight: ", current%weight
            current => current%next
        end do
    end subroutine print
    ! grafo methods
    subroutine insert_data(this, id, neighbor_id, weight, impresoras)
        class(grafo), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, weight, impresoras   
        type(node), pointer :: current

        current => this%get_nodo(id)
        if ( .NOT. associated(current) ) then
            call this%add_node(id)
            call this%add_arista(neighbor_id, weight, this%head, impresoras)
        else
            call this%add_arista(neighbor_id, weight, current, impresoras)
        end if
    end subroutine insert_data

    subroutine insert_data_2(this, id, neighbor_id, weight, printers)
        class(grafo), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, weight, printers
        type(node), pointer :: current

        current => this%get_nodo(id)
        if ( .NOT. associated(current) ) then
            call this%add_node(id)
            call this%add_arista_2(neighbor_id, weight, this%head, printers)
        else
            call this%add_arista_2(neighbor_id, weight, current, printers)
        end if
    end subroutine insert_data_2

    subroutine add_node(this,  id)
        class(grafo), intent(inout) :: this
        integer, intent(in) :: id
        type(node), pointer :: new_node

        allocate(new_node)
        new_node%id = id
        
        if (.not. associated(this%head)) then
            this%head => new_node
            return
        end if

        new_node%next => this%head
        this%head => new_node
    end subroutine add_node
    subroutine add_arista(this, id, weight, parent, impresoras)
        class(grafo), intent(inout) :: this
        integer, intent(in) :: id, weight, impresoras
        type(node), pointer :: parent
        type(node), pointer :: arista_node 
        arista_node => this%get_nodo(id)
        if ( .NOT. associated(arista_node) ) then
            call this%add_node(id)
        end if
        call parent%vecinos%add_ordenado(id, weight, parent%id, .TRUE., impresoras)
        this%n_nodes = this%n_nodes + 1
    end subroutine add_arista

    subroutine add_arista_2(this, id, weight, parent, printers)
        class(grafo), intent(inout) :: this
        integer, intent(in) :: id, weight, printers
        type(node), pointer :: parent
        type(node), pointer :: arista_node 
        arista_node => this%get_nodo(id)
        if ( .NOT. associated(arista_node) ) then
            call this%add_node(id)
        end if
        call parent%vecinos%add_ordenado_2(id, weight, parent%id, .TRUE., printers)
        this%n_nodes = this%n_nodes + 1
    end subroutine add_arista_2

    function get_nodo(this, id) result(retval)
        class(grafo), intent(in) :: this
        integer, intent(in) :: id
        type(node), pointer :: retval
        type(node), pointer :: current
        current => this%head
        do while (associated(current))
            if (current%id == id) then
                retval => current
                return
            end if
            current => current%next
        end do
        retval => null()  
    end function get_nodo
    subroutine show(this)
        class(grafo), intent(in) :: this
        type(node), pointer :: current
        type(arista), pointer :: current_arista
        current => this%head
        do while (associated(current))
            write(*,'(A, I0)') 'Node: ', current%id
            current_arista => current%vecinos%head
            do while (associated(current_arista))
                write(*,'(A, I0, A, I0, A)', advance='no') 'arista: ', current_arista%id, ", " ,current_arista%weight, " "
                current_arista => current_arista%next
            end do
            write(*, *) ''
            current => current%next
        end do
    end subroutine show
    ! Analyzer methods  
    subroutine set_grafo(this,  grafo_p)
        class(analyzer), intent(inout) :: this
        type(grafo), intent(in) :: grafo_p
        this%grafo_data = grafo_p
    end subroutine set_grafo
    function get_shortest_path(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total, sub_total_printers
        type(result_list), pointer :: retval
        type(lista_aristas), pointer :: queue
        type(node), pointer :: current_node
        type(arista), pointer :: current_arista
        print *, 'Getting shortest path from ', id_origin, ' to ', id_destination
        sub_total = 0
        sub_total_printers = 0
        allocate(retval)
        retval%total_weight = 0
        allocate(queue)
        current_node => this%grafo_data%get_nodo(id_origin)
        if ( associated(current_node) ) then
            call queue%merge(current_node%vecinos)
            call retval%add_result(id_origin, 0, 0)
        end if
        do while ( .NOT. queue%is_empty() )
            current_arista => queue%pop()
            sub_total = current_arista%weight
            sub_total_printers = current_arista%printers
            current_node => this%grafo_data%get_nodo(current_arista%id)
            if ( .NOT. associated(current_node) ) then
                print *, 'Node not found'
                exit
            end if
            if (current_node%id == id_destination) then
                print *, 'Found destination'
                call retval%add_result(current_node%id, sub_total, sub_total_printers)
                exit
            end if
            call current_node%vecinos%add_weight(sub_total, sub_total_printers)
            call queue%merge(current_node%vecinos)
            call retval%add_result(current_node%id, sub_total, sub_total_printers)
            current_node => current_node%next
        end do
    end function get_shortest_path

    function get_longest_path(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total, sub_total_printers
        type(result_list), pointer :: retval
        type(lista_aristas), pointer :: queue
        type(node), pointer :: current_node
        type(arista), pointer :: current_arista
        integer, allocatable :: max_weights(:)
        logical, allocatable :: visited(:)
    
        allocate(retval)
        retval%total_weight = 0
        retval%total_printers = 0
        allocate(queue)
        allocate(max_weights(this%grafo_data%n_nodes))
        allocate(visited(this%grafo_data%n_nodes))
        max_weights = -HUGE(0)  ! Inicializar pesos máximos con el menor valor posible
        visited = .false.       ! Inicializar todos los nodos como no visitados
    
        current_node => this%grafo_data%get_nodo(id_origin)
        if ( associated(current_node) ) then
            call queue%merge_2(current_node%vecinos)
            call retval%add_result(id_origin, 0, 0)
            max_weights(current_node%id) = 0
        end if
    
        do while ( .NOT. queue%is_empty() )
            current_arista => queue%pop()
            sub_total = current_arista%weight + max_weights(current_arista%parent_id)
            sub_total_printers = current_arista%printers
            if (sub_total > max_weights(current_arista%id)) then
                max_weights(current_arista%id) = sub_total
                current_node => this%grafo_data%get_nodo(current_arista%id)
                if ( .NOT. associated(current_node) ) then
                    print *, 'Node not found'
                    exit
                end if
                if (current_node%id == id_destination) then
                    print *, 'Found destination'
                    call retval%add_result(current_node%id, sub_total, sub_total_printers)
                    exit
                end if
                if (.not. visited(current_node%id)) then
                    call current_node%vecinos%add_weight(sub_total - max_weights(current_node%id), sub_total_printers)
                    call queue%merge_2(current_node%vecinos)
                    call retval%add_result(current_node%id, sub_total, sub_total_printers)
                    visited(current_node%id) = .true.
                end if
            end if
        end do
    end function get_longest_path

    subroutine graficar(this, filename)
        class(grafo), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(node), pointer :: current_node
        type(arista), pointer :: current_arista
        integer :: io_unit, io_stat
        
        open(newunit=io_unit, file=filename, status='replace', action='write', iostat=io_stat)
        if (io_stat /= 0) then
            print *, 'Error al abrir el archivo'
            return
        end if
        
        write(io_unit, *) 'digrafo G {'
        write(io_unit, *) '    rankdir=LR;'  ! Esto coloca el gráfico de izquierda a derecha
        write(io_unit, *) '    node [shape=circle];'  ! Estilo de los nodos
        
        current_node => this%head
        do while (associated(current_node))
            current_arista => current_node%vecinos%head
            do while (associated(current_arista))
                if (current_node%id /= current_arista%id) then
                    write(io_unit, '(A, I0, A, I0, A, I0, A)') '    ', current_node%id, ' -> ', &
                    current_arista%id, ' [label="', current_arista%weight, '"];'
                end if
                current_arista => current_arista%next
            end do
            current_node => current_node%next
        end do
        
        write(io_unit, *) '}'
        close(io_unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')     
        print *,"Arbol de Imagenes con Capas graficado con exito"   
        call system('start ' // trim(adjustl(filename)) // '.png')
    end subroutine graficar

    subroutine reset_grafo(this)
        class(grafo), intent(inout) :: this
        type(node), pointer :: current_node
        type(node), pointer :: temp_node
        type(arista), pointer :: current_arista
        type(arista), pointer :: temp_arista
    
        ! Recorrer todos los nodos del grafo
        current_node => this%head
        do while (associated(current_node))
            ! Liberar la lista de aristas de cada nodo
            current_arista => current_node%vecinos%head
            do while (associated(current_arista))
                temp_arista => current_arista
                current_arista => current_arista%next
                nullify(temp_arista%prev)
                nullify(temp_arista%next)
                deallocate(temp_arista)
            end do
            nullify(current_node%vecinos%head)
            nullify(current_node%vecinos%tail)
    
            ! Avanzar al siguiente nodo y liberar el actual
            temp_node => current_node
            current_node => current_node%next
            deallocate(temp_node)
        end do
    
        ! Reiniciar la cabeza del grafo y el número de nodos
        nullify(this%head)
        this%n_nodes = 0
    
    end subroutine reset_grafo
    
end module routes
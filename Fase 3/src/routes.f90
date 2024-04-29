module routes
    implicit none
    type edge
        integer :: id
        integer :: weight
        integer :: parent_id
        integer :: printers
        type(edge), pointer :: next => null()
        type(edge), pointer :: prev => null()
    end type edge
    type edge_list
        type(edge), pointer :: head => null()
        type(edge), pointer :: tail => null()
    contains
        procedure :: add_sorted
        procedure :: add_sorted_2 
        procedure :: pop
        procedure :: is_empty
        procedure :: merge
        procedure :: merge_2
        procedure :: add_weight
    end type edge_list
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
        type(edge_list) :: neighbors
        type(node), pointer :: next => null()
    end type node
    type graph
        integer :: n_nodes
        type(node), pointer :: head => null()
    contains
        procedure :: insert_data
        procedure :: insert_data_2
        procedure :: add_node
        procedure :: add_edge
        procedure :: add_edge_2
        procedure :: get_node
        procedure :: show
        procedure :: graficar
        procedure :: reset_graph
    end type graph
    type analyzer
        type(graph):: graph_data 
    contains
        procedure :: set_graph
        procedure :: get_shortest_path
        procedure :: get_longest_path
    end type analyzer
contains
    ! Edge list methods
    subroutine add_sorted(this, id, weight, parent_id, sort_by_id, impresoras)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: id, weight, parent_id, impresoras
        logical, intent(in) :: sort_by_id
        type(edge), pointer :: new_edge
        type(edge), pointer :: current
        type(edge), pointer :: previous
        allocate(new_edge)
        new_edge%id = id
        new_edge%weight = weight
        new_edge%parent_id = parent_id
        new_edge%printers = impresoras

        if (.not. associated(this%head)) then
            this%head => new_edge
            this%tail => new_edge
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
            new_edge%next => this%head
            this%head%prev => new_edge
            this%head => new_edge
        else if (.not. associated(current)) then
            this%tail%next => new_edge
            new_edge%prev => this%tail
            this%tail => new_edge
        else
            previous%next => new_edge
            new_edge%prev => previous
            new_edge%next => current
            current%prev => new_edge
        end if
    end subroutine add_sorted

    subroutine add_sorted_2(this, id, weight, parent_id, sort_by_id, printers)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: id, weight, parent_id, printers
        logical, intent(in) :: sort_by_id
        type(edge), pointer :: new_edge
        type(edge), pointer :: current
        type(edge), pointer :: previous
        allocate(new_edge)
        new_edge%id = id
        new_edge%weight = weight
        new_edge%parent_id = parent_id
        new_edge%printers = printers

        if (.not. associated(this%head)) then
            this%head => new_edge
            this%tail => new_edge
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
            new_edge%next => this%head
            this%head%prev => new_edge
            this%head => new_edge
        else if (.not. associated(current)) then
            this%tail%next => new_edge
            new_edge%prev => this%tail
            this%tail => new_edge
        else
            previous%next => new_edge
            new_edge%prev => previous
            new_edge%next => current
            current%prev => new_edge
        end if
    end subroutine add_sorted_2

    function pop(this) result(edge_res)
        class(edge_list), intent(inout) :: this
        type(edge), pointer :: edge_res
        if (.not. associated(this%head)) then
            edge_res => null()
            return
        end if
        edge_res => this%head
        this%head => this%head%next
        if (associated(this%head)) then
            this%head%prev => null()
        else
            this%tail => null()
        end if
    end function pop
    function is_empty(this) result(res)
        class(edge_list), intent(in) :: this
        logical :: res
        res = .not. associated(this%head)
    end function is_empty
    subroutine merge(this,  to_merge)
        class(edge_list), intent(inout) :: this
        class(edge_list), intent(in) :: to_merge
        type(edge), pointer :: current

        current => to_merge%head
        do while (associated(current))
            call this%add_sorted(current%id, current%weight, current%parent_id, .FALSE., current%printers)
            current => current%next
        end do
        
    end subroutine merge


    subroutine merge_2(this,  to_merge)
        class(edge_list), intent(inout) :: this
        class(edge_list), intent(in) :: to_merge
        type(edge), pointer :: current

        current => to_merge%head
        do while (associated(current))
            call this%add_sorted_2(current%id, current%weight, current%parent_id, .FALSE., current%printers)
            current => current%next
        end do
        
    end subroutine merge_2

    subroutine add_weight(this, weight, printers)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: weight, printers
        type(edge), pointer :: current
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
    ! Graph methods
    subroutine insert_data(this, id, neighbor_id, weight, impresoras)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, weight, impresoras   
        type(node), pointer :: current

        current => this%get_node(id)
        if ( .NOT. associated(current) ) then
            call this%add_node(id)
            call this%add_edge(neighbor_id, weight, this%head, impresoras)
        else
            call this%add_edge(neighbor_id, weight, current, impresoras)
        end if
    end subroutine insert_data

    subroutine insert_data_2(this, id, neighbor_id, weight, printers)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, weight, printers
        type(node), pointer :: current

        current => this%get_node(id)
        if ( .NOT. associated(current) ) then
            call this%add_node(id)
            call this%add_edge_2(neighbor_id, weight, this%head, printers)
        else
            call this%add_edge_2(neighbor_id, weight, current, printers)
        end if
    end subroutine insert_data_2

    subroutine add_node(this,  id)
        class(graph), intent(inout) :: this
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
    subroutine add_edge(this, id, weight, parent, impresoras)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, weight, impresoras
        type(node), pointer :: parent
        type(node), pointer :: edge_node 
        edge_node => this%get_node(id)
        if ( .NOT. associated(edge_node) ) then
            call this%add_node(id)
        end if
        call parent%neighbors%add_sorted(id, weight, parent%id, .TRUE., impresoras)
        this%n_nodes = this%n_nodes + 1
    end subroutine add_edge

    subroutine add_edge_2(this, id, weight, parent, printers)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, weight, printers
        type(node), pointer :: parent
        type(node), pointer :: edge_node 
        edge_node => this%get_node(id)
        if ( .NOT. associated(edge_node) ) then
            call this%add_node(id)
        end if
        call parent%neighbors%add_sorted_2(id, weight, parent%id, .TRUE., printers)
        this%n_nodes = this%n_nodes + 1
    end subroutine add_edge_2

    function get_node(this, id) result(retval)
        class(graph), intent(in) :: this
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
    end function get_node
    subroutine show(this)
        class(graph), intent(in) :: this
        type(node), pointer :: current
        type(edge), pointer :: current_edge
        current => this%head
        do while (associated(current))
            write(*,'(A, I0)') 'Node: ', current%id
            current_edge => current%neighbors%head
            do while (associated(current_edge))
                write(*,'(A, I0, A, I0, A)', advance='no') 'Edge: ', current_edge%id, ", " ,current_edge%weight, " "
                current_edge => current_edge%next
            end do
            write(*, *) ''
            current => current%next
        end do
    end subroutine show
    ! Analyzer methods  
    subroutine set_graph(this,  graph_p)
        class(analyzer), intent(inout) :: this
        type(graph), intent(in) :: graph_p
        this%graph_data = graph_p
    end subroutine set_graph
    function get_shortest_path(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total, sub_total_printers
        type(result_list), pointer :: retval
        type(edge_list), pointer :: queue
        type(node), pointer :: current_node
        type(edge), pointer :: current_edge
        print *, 'Getting shortest path from ', id_origin, ' to ', id_destination
        sub_total = 0
        sub_total_printers = 0
        allocate(retval)
        retval%total_weight = 0
        allocate(queue)
        current_node => this%graph_data%get_node(id_origin)
        if ( associated(current_node) ) then
            call queue%merge(current_node%neighbors)
            call retval%add_result(id_origin, 0, 0)
        end if
        do while ( .NOT. queue%is_empty() )
            current_edge => queue%pop()
            sub_total = current_edge%weight
            sub_total_printers = current_edge%printers
            current_node => this%graph_data%get_node(current_edge%id)
            if ( .NOT. associated(current_node) ) then
                print *, 'Node not found'
                exit
            end if
            if (current_node%id == id_destination) then
                print *, 'Found destination'
                call retval%add_result(current_node%id, sub_total, sub_total_printers)
                exit
            end if
            call current_node%neighbors%add_weight(sub_total, sub_total_printers)
            call queue%merge(current_node%neighbors)
            call retval%add_result(current_node%id, sub_total, sub_total_printers)
            current_node => current_node%next
        end do
    end function get_shortest_path

    function get_longest_path(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total, sub_total_printers
        type(result_list), pointer :: retval
        type(edge_list), pointer :: queue
        type(node), pointer :: current_node
        type(edge), pointer :: current_edge
        integer, allocatable :: max_weights(:)
        logical, allocatable :: visited(:)
    
        allocate(retval)
        retval%total_weight = 0
        retval%total_printers = 0
        allocate(queue)
        allocate(max_weights(this%graph_data%n_nodes))
        allocate(visited(this%graph_data%n_nodes))
        max_weights = -HUGE(0)  ! Inicializar pesos máximos con el menor valor posible
        visited = .false.       ! Inicializar todos los nodos como no visitados
    
        current_node => this%graph_data%get_node(id_origin)
        if ( associated(current_node) ) then
            call queue%merge_2(current_node%neighbors)
            call retval%add_result(id_origin, 0, 0)
            max_weights(current_node%id) = 0
        end if
    
        do while ( .NOT. queue%is_empty() )
            current_edge => queue%pop()
            sub_total = current_edge%weight + max_weights(current_edge%parent_id)
            sub_total_printers = current_edge%printers
            if (sub_total > max_weights(current_edge%id)) then
                max_weights(current_edge%id) = sub_total
                current_node => this%graph_data%get_node(current_edge%id)
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
                    call current_node%neighbors%add_weight(sub_total - max_weights(current_node%id), sub_total_printers)
                    call queue%merge_2(current_node%neighbors)
                    call retval%add_result(current_node%id, sub_total, sub_total_printers)
                    visited(current_node%id) = .true.
                end if
            end if
        end do
    end function get_longest_path

    subroutine graficar(this, filename)
        class(graph), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(node), pointer :: current_node
        type(edge), pointer :: current_edge
        integer :: io_unit, io_stat
        
        open(newunit=io_unit, file=filename, status='replace', action='write', iostat=io_stat)
        if (io_stat /= 0) then
            print *, 'Error al abrir el archivo'
            return
        end if
        
        write(io_unit, *) 'digraph G {'
        write(io_unit, *) '    rankdir=LR;'  ! Esto coloca el gráfico de izquierda a derecha
        write(io_unit, *) '    node [shape=circle];'  ! Estilo de los nodos
        
        current_node => this%head
        do while (associated(current_node))
            current_edge => current_node%neighbors%head
            do while (associated(current_edge))
                if (current_node%id /= current_edge%id) then
                    write(io_unit, '(A, I0, A, I0, A, I0, A)') '    ', current_node%id, ' -> ', &
                    current_edge%id, ' [label="', current_edge%weight, '"];'
                end if
                current_edge => current_edge%next
            end do
            current_node => current_node%next
        end do
        
        write(io_unit, *) '}'
        close(io_unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')     
        print *,"Arbol de Imagenes con Capas graficado con exito"   
        call system('start ' // trim(adjustl(filename)) // '.png')
    end subroutine graficar

    subroutine reset_graph(this)
        class(graph), intent(inout) :: this
        type(node), pointer :: current_node
        type(node), pointer :: temp_node
        type(edge), pointer :: current_edge
        type(edge), pointer :: temp_edge
    
        ! Recorrer todos los nodos del grafo
        current_node => this%head
        do while (associated(current_node))
            ! Liberar la lista de aristas de cada nodo
            current_edge => current_node%neighbors%head
            do while (associated(current_edge))
                temp_edge => current_edge
                current_edge => current_edge%next
                nullify(temp_edge%prev)
                nullify(temp_edge%next)
                deallocate(temp_edge)
            end do
            nullify(current_node%neighbors%head)
            nullify(current_node%neighbors%tail)
    
            ! Avanzar al siguiente nodo y liberar el actual
            temp_node => current_node
            current_node => current_node%next
            deallocate(temp_node)
        end do
    
        ! Reiniciar la cabeza del grafo y el número de nodos
        nullify(this%head)
        this%n_nodes = 0
    
        print *, "El grafo ha sido reiniciado."
    end subroutine reset_graph
    
end module routes
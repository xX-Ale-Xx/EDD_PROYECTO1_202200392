module merkle_tree
    use sha256_module
    implicit none
    integer :: u_id = 1
    type data
        integer :: u_id
        character(:), allocatable :: id_origin, address_origin                
        character(:), allocatable :: id_destination, address_destination
        character(:), allocatable :: cost_between, hash_value
        type(data), pointer :: next => null()
    end type data

    type hash_node
        integer :: u_id
        character(:), allocatable :: hash        
        type(hash_node), pointer :: left => null()
        type(hash_node), pointer :: right => null()
        type(data), pointer :: dataref => null()
    end type hash_node
    
    type merkle 
        type(hash_node), pointer :: top_hash => null()
        type(data), pointer :: data_head => null()
        type(data), pointer :: data_tail => null()
        integer :: pos = 0
    contains 
        procedure :: aniadir_adata
        procedure :: get_data
        procedure :: merkle_dot 
        procedure :: merkle_dot_rec
        procedure :: get_head_hash
        procedure :: data_length
        procedure :: crearArbol
        procedure :: gen_hash
        procedure :: generate
        
    end type merkle

contains

    
    
    function get_data(this, pos) result(data_node)
        class(merkle), intent(inout) :: this
        integer, intent(inout) :: pos
        type(data), pointer :: data_node
        data_node => this%data_head       
        do while (associated(data_node)) 
            if ( pos == 0 ) then
                return
            end if
            pos = pos - 1
            data_node => data_node%next
        end do
    end function get_data
    subroutine generate(this)
        class(merkle), intent(inout) :: this
        integer :: expo, i, pow         
        expo = 1

        do while (2 ** expo < this%data_length() )
            expo = expo + 1
        end do

        pow = 2 ** expo
        this%pos = pow 
        i = this%data_length()

        do while (i < pow)
            call this%aniadir_adata(-1, "null", -1, "null", -1)
            i = i + 1
        end do
        allocate(this%top_hash)
        call this%crearArbol(this%top_hash, expo)
        call this%gen_hash(this%top_hash, pow)
    end subroutine generate

    subroutine aniadir_adata(this, id_origin, address_origin, id_destination, address_destination, cost_between)
        class(merkle), intent(inout) :: this
        integer, intent(in) :: id_origin
        character(*), intent(in) :: address_origin
        integer, intent(in) :: id_destination
        character(*), intent(in) :: address_destination
        integer, intent(in) :: cost_between
        character(len=256) :: id_o_str, id_d_str, cost_str
        character(:), allocatable :: hash_value
        type(data), pointer :: new_data

        write(id_o_str, '(I10)') id_origin
        write(id_d_str, '(I10)') id_destination
        write(cost_str, '(I10)') cost_between

        hash_value = sha256(trim(id_o_str) // address_origin // trim(id_d_str) // address_destination // trim(cost_str))

        allocate(new_data)
        allocate(new_data%id_origin, source=id_o_str)
        allocate(new_data%address_origin, source=address_origin)
        allocate(new_data%id_destination, source=id_d_str)
        allocate(new_data%address_destination, source=address_destination)
        allocate(new_data%cost_between, source=cost_str)
        allocate(new_data%hash_value, source=hash_value)
        new_data%u_id = u_id
        u_id = u_id + 1

        if ( associated(this%data_head) ) then
            this%data_tail%next => new_data
            this%data_tail => new_data
        else 
            this%data_head => new_data
            this%data_tail => new_data
        end if

    end subroutine aniadir_adata


     subroutine crearArbol(this, node, expo)
        class(merkle), intent(inout) :: this 
        type(hash_node), pointer, intent(inout) :: node
        integer, intent(in) :: expo
        node%u_id = u_id
        u_id = u_id + 1

        if ( expo > 0 ) then
            allocate(node%left)
            allocate(node%right)
            call this%crearArbol(node%left, expo - 1)
            call this%crearArbol(node%right, expo - 1)
        end if    
        
    end subroutine crearArbol

    function data_length(this) result(res)
        class(merkle), intent(inout) :: this
        type(data), pointer :: tmp 
        integer :: res 
        res = 0
        tmp => this%data_head
        do while (associated(tmp))
            res = res + 1
            tmp => tmp%next
        end do        
    end function data_length

   

    subroutine gen_hash(this,  node, pow)
        class(merkle), intent(inout) :: this
        type(hash_node), pointer, intent(inout) :: node        
        character(:), allocatable :: hash
        integer, intent(in) :: pow
        integer :: tmp 
        
        if ( associated(node) ) then
            call this%gen_hash(node%left, pow)
            call this%gen_hash(node%right, pow)
            if ( .NOT. associated(node%left) .AND. .NOT. associated(node%right) ) then
                tmp = pow - this%pos
                node%dataref => this%get_data(tmp)
                this%pos = this%pos - 1
                hash = node%dataref%hash_value
                node%hash = hash
            else 
                hash = sha256(node%left%hash // node%right%hash)
                node%hash = hash
            end if
        end if
        
    end subroutine gen_hash

    

    subroutine merkle_dot(this)
        class(merkle), intent(inout) :: this        
        open(69, file='merkle.dot', status='replace')
        write(69, '(A)') 'digraph Merkle_tree {'
        write(69, '(A)') 'node [shape=record, fontname=Arial, fontsize=12];'
        call this%merkle_dot_rec(this%top_hash, 69)
        write(69, '(A)') '}'
        close(69)
        call execute_command_line('dot -Tpng merkle.dot -o merkle.png')
    end subroutine merkle_dot

    subroutine merkle_dot_rec(this,  tmp, unit)
        class(merkle), intent(inout) :: this
        class(hash_node), pointer, intent(in) :: tmp
        integer, intent(in) :: unit        
        if ( .NOT. associated(tmp) ) then
            return
        end if
        write(unit, '(I0, A, A, A)') tmp%u_id, ' [label="', tmp%hash, '"];'
        if ( associated(tmp%left) ) then
            write(unit, '(I0, A, I0, A)') tmp%u_id, ' -> ', tmp%left%u_id, ';'            
        end if
        if ( associated(tmp%right) ) then
            write(unit, '(I0, A, I0, A)') tmp%u_id, ' -> ', tmp%right%u_id, ';'
        end if
        call this%merkle_dot_rec(tmp%left, unit)
        call this%merkle_dot_rec(tmp%right, unit)
        if ( associated(tmp%dataref) ) then            
            write(unit, '(I0, A)') tmp%dataref%u_id, ' [label=<<TABLE><TR>'
            write(unit, '(A, A, A)') '<TD>id_origen: ', trim(tmp%dataref%id_origin), '</TD>'
            write(unit, '(A, A, A)') '<TD>direccion_origen: ', trim(tmp%dataref%address_origin), '</TD></TR>'
            write(unit, '(A, A, A)') '<TR><TD>id_destino: ', trim(tmp%dataref%id_destination), '</TD>'
            write(unit, '(A, A, A)') '<TD>direccion_destino: ', trim(tmp%dataref%address_destination), '</TD></TR>'
            write(unit, '(A, A, A)') '<TR><TD>costo: ', trim(tmp%dataref%cost_between), '</TD></TR>'
            write(unit, '(A)') '</TABLE>>];'
            write(unit, '(I0, A, I0, A)') tmp%u_id, ' -> ', tmp%dataref%u_id, ';'
        end if
    end subroutine merkle_dot_rec

    function get_head_hash(this) result(hash_value)
        class(merkle), intent(in) :: this
        character(:), allocatable :: hash_value
        
        if (.not. associated(this%top_hash)) then
            write(*, '(A)') "Error"
            hash_value = "ERROR"
            return
        else
            hash_value = this%top_hash%hash
        endif
    end function get_head_hash
    
    

end module merkle_tree
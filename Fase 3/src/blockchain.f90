module block_chain
    use routes, only: result_list, result
    use merkle_tree
    use Avl_Tree
    use json_module
    implicit none
    integer :: block_id = 0
    type block 
        integer :: index
        integer :: nonce
        character(:), allocatable :: timestamp
        type(result_list) :: data
        character(len=256) :: previous_hash = '0000'
        character(len=256) :: raiz_merkle = '0000'
        character(len=256) :: hash = '0000'
        type(block), pointer :: next => null()
        type(Tree_t) :: branches 
    contains
        procedure :: generarBloque
        procedure :: print_data
    end type block
    type chainer
        type(block), pointer :: head => null()
        type(block), pointer :: tail => null()
    contains 
        procedure :: aniadir_bloque
        procedure :: print_chain
        procedure :: generarJson
    end type chainer
contains
    subroutine generarBloque(this, new_data, new_branches, isPrinters)
    class(block), intent(inout) :: this
    type(result_list) :: new_data
    type(Tree_t):: new_branches
    type(result), pointer :: current
    type(Node_t), pointer :: b_origin, b_destination
    type(merkle) :: new_merkle
    logical, intent(in) :: isPrinters
    integer, dimension(8) :: values 
    character(20) :: timestamp
    this%index = block_id
    block_id = block_id + 1
    call date_and_time(values=values)
    write(timestamp, '(I0, A, I0, A, I0, A, I0, A, I0, A, I0)') values(3), '-', values(2) &
        , '-', values(1), '::', values(5), ':', values(6), ':', values(7)
    this%nonce = 4560
    this%timestamp = trim(timestamp)
    this%data = new_data
    this%branches = new_branches
    current => new_data%head
    do while ( associated(current) )
        b_origin => new_branches%searchBranch(current%id)
        if ( associated(b_origin) ) then
            if ( associated(current%next) ) then
                b_destination => new_branches%searchBranch(current%next%id)
                if ( associated(b_destination) ) then
                    if(isPrinters) then
                    call new_merkle%aniadir_adata(b_origin%id, b_origin%dept, b_destination%id,&
                    b_destination%dept, current%next%weight*80)
                    else
                        call new_merkle%aniadir_adata(b_origin%id, b_origin%dept, b_destination%id,&
                    b_destination%dept, current%next%printers*80)
                        endif
                end if
            end if
        end if
        current => current%next
    end do
    call new_merkle%generate()
    call new_merkle%merkle_dot()
    this%raiz_merkle = new_merkle%get_head_hash()
    end subroutine generarBloque
   
    subroutine aniadir_bloque(this, new_block)
        class(chainer), intent(inout) :: this
        type(block), intent(in) :: new_block  ! Cambio aquí
        type(block), pointer :: block_ptr
        character(len=100) :: index, nonce
        allocate(block_ptr)
        block_ptr%index = new_block%index
        block_ptr%timestamp = new_block%timestamp
        block_ptr%nonce = new_block%nonce
        block_ptr%data = new_block%data
        block_ptr%previous_hash = new_block%previous_hash
        block_ptr%raiz_merkle = new_block%raiz_merkle
        block_ptr%hash = new_block%hash
        block_ptr%branches = new_block%branches
    
        write(index, '(I0)') block_ptr%index
        write(nonce, '(I0)') block_ptr%nonce
        if ( .not. associated(this%head) ) then
            this%head => block_ptr
            block_ptr%hash = sha256(trim(index)//block_ptr%timestamp//trim(nonce)&
            //block_ptr%previous_hash//block_ptr%raiz_merkle)
            this%tail => block_ptr
        else
            this%tail%next => block_ptr
            block_ptr%previous_hash = this%tail%hash
            block_ptr%hash = sha256(trim(index)//block_ptr%timestamp//trim(nonce)&
            //block_ptr%previous_hash//block_ptr%raiz_merkle)
            this%tail => block_ptr
        endif
    end subroutine aniadir_bloque
    

    subroutine print_data(this)
        class(block), intent(in) :: this
        
        print *, 'Index: ', this%index
        print *, 'Timestamp: ', this%timestamp
        print *, 'Nonce: ', this%nonce
        call this%data%print()
        print *, 'Previous Hash: ', this%previous_hash
        print *, 'Root Merkle: ', this%raiz_merkle
        print *, 'Hash: ', this%hash
    end subroutine print_data

    subroutine print_chain(this)
        class(chainer), intent(inout) :: this
        type(block), pointer :: current

        current => this%head
        do while ( associated(current) )
            call current%print_data()
            current => current%next
        end do
        
    end subroutine print_chain

    subroutine generarJson(this, isPrinters)
        class(chainer), intent(inout) :: this    
        type(json_core) :: json
        type(json_value), pointer :: p, data_p, path 
        type(block), pointer :: current
        type(result), pointer :: current_data
        type(Node_t), pointer :: b_actual
        type(Node_t), pointer :: b_next
        logical, intent(in) :: isPrinters
        character(len=5) :: file_index
        current => this%head
        do while ( associated(current) )
            call json%initialize()
            call json%create_object(p, '')
            call json%add(p, 'INDEX', current%index)
            call json%add(p, 'TIMESTAMP', current%timestamp)
            call json%add(p, 'NONCE', current%nonce)
            ! Data
            call json%create_array(data_p, 'DATA')
            current_data => current%data%head
            do while ( associated(current_data) )
                b_actual => current%branches%searchBranch(current_data%id)
                if ( associated(b_actual) ) then
                    if ( associated(current_data%next) ) then
                        b_next => current%branches%searchBranch(current_data%next%id)
                        if ( associated(b_next) ) then
                            call json%create_object(path, '')
                            call json%add(path, 'sucursal_o', current_data%id)
                            call json%add(path, 'direccion_o', b_actual%dept//', '//b_actual%direccion)
                            call json%add(path, 'sucursal_d', current_data%next%id)                            
                            call json%add(path, 'direccion_d', b_next%dept//', '//b_next%direccion)
                            if(isPrinters) then
                                call json%add(path, 'costo', current_data%next%weight*80)
                            else
                                call json%add(path, 'costo', current_data%next%printers*80)
                            end if
                            call json%add(data_p, path)
                        end if
                    end if
                end if
                current_data => current_data%next
            end do
            call json%add(p, data_p)
            call json%add(p, 'PREVIOUSHASH', current%previous_hash)
            call json%add(p, 'ROOTMERKLE', current%raiz_merkle)
            call json%add(p, 'HASH', current%hash)
            nullify(data_p)
            nullify(path)  
            write(file_index, '(I0)') current%index
            call json%print(p, 'bloque_'//trim(file_index)//'.json')  
            print *, 'Bloque_'//trim(file_index)//' generado'
            current => current%next
        end do    
        call json%destroy(p)
    end subroutine generarJson

end module block_chain
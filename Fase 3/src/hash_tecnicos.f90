module hash_table
    implicit none
    private
    integer :: table_size = 7
    real, parameter :: R = 0.618034
    integer, parameter :: MAX_USED_PERCENTAGE = 70
    type tecnico
    integer(8) ::key,telefono
    character(:), allocatable::nombre,apellido,direccion
    end type tecnico
    type, public :: HashTable
        integer :: elements = 0
        type(tecnico), allocatable :: array(:)

        contains
        procedure :: insert
        procedure :: print
        procedure :: search
        procedure, private :: solve_collision
        procedure :: grafico
    end type HashTable
contains
    subroutine insert(self, key,nombre,apellido,direccion,telefono)
        class(HashTable), intent(inout) :: self
        type(HashTable) :: newTable
        integer(8), intent(in) :: key,telefono
        character(:), allocatable::nombre,apellido,direccion
        type(tecnico), allocatable :: oldArray(:)
        real :: used_percentage
        integer(8) :: pos
        
        ! If the table is empty, allocate it
        if(.not. allocated(self%array)) then
            allocate(self%array(0:table_size-1))
            self%array(:)%key = -1  ! Initialize all the elements to -1
        end if

        pos = get_position(key)

        ! If the position is already occupied, solve the collision
        if(self%array(pos)%key /= -1 .and. self%array(pos)%key /= key) then
            call self%solve_collision(pos)
        end if

        ! Store the key in the table
        self%array(pos)%key=key
        self%array(pos)%nombre=nombre
        self%array(pos)%apellido= apellido
        self%array(pos)%direccion=direccion
        self%array(pos)%telefono=telefono
        self%elements = self%elements + 1

        ! Check if the table is more than 75% full
        used_percentage = (self%elements * 1.0/table_size) * 100
        if(used_percentage > MAX_USED_PERCENTAGE) then
            ! Deallocate the table
            oldArray = self%array
            deallocate(self%array)
            ! Rehash the table
            newTable = rehashing(oldArray)
            self%array = newTable%array
            self%elements = newTable%elements
        end if
    end subroutine insert

    function rehashing(oldArray) result(newTable)
        type(tecnico), intent(in) :: oldArray(:)
        integer :: i
        type(HashTable) :: newTable
        
        ! Initialize the new table
        table_size = table_size*2
        allocate(newTable%array(0:table_size-1))
        newTable%array(:)%key = -1
        ! Insert the elements in the new table
        do i = 1, size(oldArray)
            if(oldArray(i)%key /= -1) then
            call newTable%insert(oldArray(i)%key,oldArray(i)%nombre,oldArray(i)%apellido,oldArray(i)%direccion,&
            oldArray(i)%telefono)
            end if
        end do
    end function rehashing

    subroutine solve_collision(self, pos)
        class(HashTable), intent(inout) :: self
        integer(8), intent(inout) :: pos
        ! Hash function h'(k)
        do while(self%array(pos)%key /= -1)
            pos = pos + 1
            pos = mod(pos, table_size)
        end do
    end subroutine solve_collision

    function get_position(key) result(pos)
        integer(8), intent(in) :: key
        !real :: t
        integer(8) :: pos
        ! Hash function h(k)
        ! Multiplicative hashing
        pos = mod(key,table_size)
    end function get_position

    subroutine search(self, key)
        class(HashTable), intent(inout) :: self
        integer(8), intent(in) :: key
        integer :: pos

        pos = get_position(key)
        ! If the key is not in the table
        !
        !
        print '(a i0 a i0)' , "Position: ", pos, " Key: ", self%array(pos)%key
    end subroutine search
    

    subroutine print(self)
        integer :: i
        class(HashTable), intent(inout) :: self
        print '(a, i0)', "Size on table: ", table_size
        print '(a, i0)', "Elements on table: ", self%elements
        do i = 0, size(self%array) - 1
            print '(i0, a, i0, a, a)',  i, " dpi: ", self%array(i)%key, " Nombre: ", self%array(i)%nombre
        end do
    end subroutine print

    subroutine grafico(self, filename)
        class(HashTable), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer :: i
        integer :: unit
    
        unit = 10  
        open(unit, file=filename, status='replace')
        print *, "GRAFICANDO HASH"
        write(unit, '(A)') 'digraph HashTable {'
        write(unit, '(A)') '  node [shape=plaintext];'
        write(unit, '(A)') '  hash_table [label=<'
        write(unit, '(A)') '    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="8" &
        BGCOLOR="#ffffff" COLOR="#000000">'
    
        ! Encabezado de la tabla
        write(unit, '(A)') '      <TR>'
        write(unit, '(A)') '        <TD></TD>' 
        write(unit, '(A,I0,A)') '        <TD BGCOLOR="#cccccc" BORDER="1" CELLPADDING="4"><FONT FACE="Helvetica" &
        COLOR="#000000"><B>DPI-Técnicos</B></FONT></TD>' 
        write(unit, '(A)') '      </TR>'
    
        ! Contenido de la tabla
        do i = 0, size(self%array) - 1
            write(unit, '(A)') '      <TR>'
            write(unit, '(A,I0,A)') '        <TD BGCOLOR="#eeeeee" BORDER="1" CELLPADDING="4"><FONT &
            FACE="Helvetica" COLOR="#000000">', i, '</FONT></TD>'
            if (self%array(i)%key /= -1) then
                write(unit, '(A,I0,A)') '        <TD BGCOLOR="#eeeeee" BORDER="1" CELLPADDING="4"><FONT &
                FACE="Helvetica" COLOR="#000000">', self%array(i)%key, '</FONT></TD>'
            else
                write(unit, '(A)') '        <TD BGCOLOR="#eeeeee" BORDER="1" CELLPADDING="4"><FONT &
                FACE="Helvetica" COLOR="#000000">-1</FONT></TD>'
            end if
            write(unit, '(A)') '      </TR>'
        end do
    
        write(unit, '(A)') '    </TABLE>'
        write(unit, '(A)') '  >];'
        write(unit, '(A)') '}'
    
        close(unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        call system('start ' // trim(adjustl(filename)) // '.png')
    end subroutine grafico
    
    
end module hash_table

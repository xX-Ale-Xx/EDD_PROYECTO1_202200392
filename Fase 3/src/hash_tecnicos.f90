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
        procedure :: get_data
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
            call self%solve_collision(key, pos)
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

    subroutine solve_collision(self, key, pos)
        class(HashTable), intent(inout) :: self
        integer(8), intent(in) :: key
        integer(8), intent(inout) :: pos
        integer(8) :: i, step, new_pos
    
        i = 1  ! Contador de colisiones
        ! Continúa buscando una nueva posición mientras la posición actual esté ocupada y no sea el DPI correcto
        do while (self%array(pos)%key /= -1 .and. self%array(pos)%key /= key)
            ! Calcula el paso usando la fórmula de doble dispersión
            step = mod(key, 7) + 1
            new_pos = mod(pos + step * i, table_size)
            ! Si encuentra una posición libre, sale del bucle
            if (self%array(new_pos)%key == -1) then
                pos = new_pos
                exit
            endif
            i = i + 1  ! Incrementa el contador de colisiones
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

    subroutine get_data(self, key)
        class(HashTable), intent(in) :: self
        integer(8), intent(in) :: key
        integer :: pos, initial_pos, i, step
        logical :: found
    
        found = .false.
        i = 0  ! Contador para evitar un bucle infinito en caso de que algo salga mal
    
        pos = get_position(key)
        initial_pos = pos
    
        ! Intenta encontrar el DPI
        do while (.not. found .and. i < table_size)
            if (self%array(pos)%key == key) then
                ! El DPI fue encontrado, imprime la información
                print *, "DPI encontrado en la posicion: ", pos
                print *, "Nombre: ", trim(self%array(pos)%nombre)
                print *, "Apellido: ", trim(self%array(pos)%apellido)
                print *, "Direccion: ", trim(self%array(pos)%direccion)
                print *, "Telefono: ", self%array(pos)%telefono
                found = .true.
            elseif (self%array(pos)%key == -1) then
                ! Llegó a un elemento sin asignar, termina la búsqueda
                exit
            else
                ! DPI no encontrado en esta posición, prueba con la siguiente posición
                step = mod(key, 7) + 1
                pos = mod(initial_pos + step * i, table_size)
            endif
            i = i + 1
        end do
    
        if (.not. found) then
            print *, "DPI no encontrado en la tabla."
        end if
    end subroutine get_data
    

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
            if(self%array(i)%key /= -1) then
                print '(i0, a, i0, a, a, a, a, a, a, a, i0)',  i, " DPI: ", self%array(i)%key, &
                " Nombre: ", self%array(i)%nombre, &
                " Apellido: ", self%array(i)%apellido, " Direccion: ", self%array(i)%direccion, &
                " Telefono: ", self%array(i)%telefono
            end if
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

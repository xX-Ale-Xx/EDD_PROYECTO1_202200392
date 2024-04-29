PROGRAM menu_program
    use json_module
    use hash_table
    use Avl_Tree
    use routes
    IMPLICIT NONE
    INTEGER :: opcion
    type(HashTable) :: table
    integer, parameter :: long = selected_int_kind(4)
    character(len=200) :: filename_json, filename_json_rutes
    character(:), allocatable :: name, lastname, genre
    character(:), allocatable :: address, departamento, direccion, password
    integer ::  i, size, id_sucursal
    integer*8 ::  dpi, phone
    character(:), allocatable ::  dpi_value, phone_value
    type(json_value), pointer :: tech_pointer, personPointer, attributePointer, &
    sucur_pointer, deptPointer, partPointer, grafo_pointer, edgePointer
    type(json_file) :: json
    type(json_core) :: jsonc
    logical :: found
    logical :: encontrado
    type(Tree_t) :: avl_sucursales
    type(graph) :: graphRutas
    type(graph) :: graphImpresoras
    integer :: distancia, s1, s2, imp_mantenimiento
    type(analyzer) :: analizador, analizadorImpresoras


DO
   call menu_programa()
    READ(*,*) opcion

    SELECT case (opcion)
        case (1)
            WRITE(*,*) "Carga de archivos seleccionada."
            CALL Menu_cargaArchivos()
        case (2)
            WRITE(*,*) "Sucursales seleccionado."
            print *, "Ingrese el id de la sucursal: "
            read (*,*) id_sucursal
            print *, "Ingrese la contraseña de la sucursal: "
            read (*,*) password
            encontrado = .false.
            call avl_sucursales%searchNode(id_sucursal, password, encontrado)
              
            if (encontrado) then
                call menu_sucursal()
            else
                print *, "Contraseña incorrecta"
            end if

        case (3)
            WRITE(*,*) "Reportes seleccionado."
        case (0)
            WRITE(*,*) "Saliendo del programa..."
            EXIT
        case DEFAULT
            WRITE(*,*) "Opción inválida. Intente nuevamente."
    END SELECT
END DO

CONTAINS

subroutine menu_sucursal()
do
    print *, "Bienvenido a la sucursal ", id_sucursal
    print *, "Ingrese una opción: "
    print *, "1. Carga de tecnicos"
    print *, "2. Generar recorrido más óptimo"
    print *, "3. Información Técnico en Específico"
    print *, "4. Listar Tecnicos"
    print *, "5. Generar Reportes"
    print *, "6. Regresar"
    read (*,*) opcion
    SELECT case (opcion)
    case (1)
        call cargarTecnicos()
    case (2)
        call generarMasOptimo()
    case (4)
        call avl_sucursales%searchNode_listar(id_sucursal)
    case (6)
        exit
    end SELECT
end do
        
end subroutine menu_sucursal

subroutine generarMasOptimo()
    type(result_list) :: lista_resultado, lista_imp 
    integer :: total
    print *, "Ingrese la sucursal de destino: "
    read (*,*) s2
    lista_resultado = analizador%get_shortest_path(id_sucursal, s2)
    call lista_resultado%print()
    total = lista_resultado%total_weight * 80
    print *, "El costo total del recorrido es: Q", total
    print *, "Las impresoras atendidas fueron : ",lista_resultado%total_printers
    lista_imp = analizadorImpresoras%get_longest_path(id_sucursal, s2)
    call lista_imp%print()
    total = lista_imp%total_weight * 100
    print *, "El costo total del recorrido es: Q", total   
    print *, "Los km totales son: ", lista_imp%total_printers
    call graphRutas%reset_graph()
    call graphImpresoras%reset_graph()
    call recargarRutas()
end subroutine generarMasOptimo

subroutine cargarTecnicos()
    ! Código para la opción "Carga de tecnicos"
    print *, "Ingrese la ruta del archivo de tecnicos: "
    read (*,'(A)') filename_json
    call json%initialize()    ! Se inicializa el módulo JSON
    call json%load(filename=filename_json)  ! Se carga el archivo JSON llamado 'config.json'
    call json%print()         ! Se imprime el contenido del archivo JSON (opcional)
    call json%info('',n_children=size)
    call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
    call json%get('', tech_pointer, found)

    do i=1, size
        call jsonc%get_child(tech_pointer, i, personPointer, found=found)

        call jsonc%get_child(personPointer, 'dpi', attributePointer, found=found)
        if (found) then
            call jsonc%get(attributePointer, dpi_value)
            print *, 'DPI: ', dpi_value
            read(dpi_value,'(I13)') dpi
            
        end if

        call jsonc%get_child(personPointer, 'nombre', attributePointer, found=found)
        if (found) then
            call jsonc%get(attributePointer, name)
            print *, 'Nombre: ', name
        end if

        call jsonc%get_child(personPointer, 'apellido', attributePointer, found=found)
        if (found) then
            call jsonc%get(attributePointer, lastname)
            print *, 'Apellido: ', lastname
        end if

        call jsonc%get_child(personPointer, 'genero', attributePointer, found=found)
        if (found) then
            call jsonc%get(attributePointer, genre)
            print *, 'Genero: ', genre
        end if

        call jsonc%get_child(personPointer, 'direccion', attributePointer, found=found)
        if (found) then
            call jsonc%get(attributePointer, address)
            print *, 'Direccion: ', address
        end if

        call jsonc%get_child(personPointer, 'telefono', attributePointer, found=found)
        if (found) then
            call jsonc%get(attributePointer, phone_value)
            print *, 'Telefono: ', phone_value
            read(phone_value,'(I13)') phone
        end if
        call table%insert(dpi, name, lastname, address, phone)
    end do
      
        call avl_sucursales%searchNode_hash(id_sucursal, table)
       

        call json%destroy()
end subroutine cargarTecnicos

subroutine Menu_cargaArchivos()
    IMPLICIT NONE
    INTEGER :: opcion

    WRITE(*,*) "1.1 Sucursales"
    WRITE(*,*) "1.2 Rutas"
    WRITE(*,*) "Ingrese una opción: "
    READ(*,*) opcion

    SELECT case (opcion)
        case (1)
           call cargarSucursales()
        case (2)
            call cargarRutas()
        case DEFAULT
            WRITE(*,*) "Opción inválida."
    END SELECT
end subroutine Menu_cargaArchivos

subroutine cargarRutas()
    WRITE(*,*) "Carga de rutas seleccionada."
    print *, "Ingrese la ruta del archivo de rutas: "
    read (*,'(A)') filename_json_rutes

    ! Inicializar y cargar el JSON desde el archivo
    call json%initialize()
    call json%load(filename=filename_json_rutes)
    call json%print()
    call json%info('grafo', n_children=size) ! Aquí especificamos 'grafo' como la clave del arreglo
    call json%get('grafo', grafo_pointer, found) ! Obtenemos el puntero al arreglo 'grafo'

    do i = 1, size
        call jsonc%get_child(grafo_pointer, i, edgePointer, found=found) ! Obtenemos el i-ésimo elemento del arreglo 'grafo'

        call jsonc%get_child(edgePointer, 's1', partPointer, found=found)
        if (found) then
            call jsonc%get(partPointer, s1)
            print *, 'Sucursal 1: ', s1
        end if

        call jsonc%get_child(edgePointer, 's2', partPointer, found=found)
        if (found) then
            call jsonc%get(partPointer, s2)
            print *, 'Sucursal 2: ', s2
        end if

        call jsonc%get_child(edgePointer, 'distancia', partPointer, found=found)
        if (found) then
            call jsonc%get(partPointer, distancia)
            print *, 'Distancia: ', distancia
        end if

        call jsonc%get_child(edgePointer, 'imp_mantenimiento', partPointer, found=found)
        if (found) then
            call jsonc%get(partPointer, imp_mantenimiento)
            print *, 'Impresoras en Mantenimiento: ', imp_mantenimiento
            print *, "--------------------------------------------------"
        end if
        call graphRutas%insert_data(s1,s2,distancia, imp_mantenimiento)
        call graphImpresoras%insert_data_2(s1,s2,imp_mantenimiento, distancia)
    end do

    call analizador%set_graph(graphRutas)
    call analizadorImpresoras%set_graph(graphImpresoras)
    call json%destroy()
end subroutine cargarRutas

subroutine recargarRutas()
    
    ! Inicializar y cargar el JSON desde el archivo
    call json%initialize()
    call json%load(filename=filename_json_rutes)
    
    call json%info('grafo', n_children=size) ! Aquí especificamos 'grafo' como la clave del arreglo
    call json%get('grafo', grafo_pointer, found) ! Obtenemos el puntero al arreglo 'grafo'

    do i = 1, size
        call jsonc%get_child(grafo_pointer, i, edgePointer, found=found) ! Obtenemos el i-ésimo elemento del arreglo 'grafo'

        call jsonc%get_child(edgePointer, 's1', partPointer, found=found)
        if (found) then
            call jsonc%get(partPointer, s1)
            
        end if

        call jsonc%get_child(edgePointer, 's2', partPointer, found=found)
        if (found) then
            call jsonc%get(partPointer, s2)
            
        end if

        call jsonc%get_child(edgePointer, 'distancia', partPointer, found=found)
        if (found) then
            call jsonc%get(partPointer, distancia)
           
        end if

        call jsonc%get_child(edgePointer, 'imp_mantenimiento', partPointer, found=found)
        if (found) then
            call jsonc%get(partPointer, imp_mantenimiento)
          
        end if
        call graphRutas%insert_data(s1,s2,distancia, imp_mantenimiento)
        call graphImpresoras%insert_data_2(s1,s2,imp_mantenimiento, distancia)
    end do
    
    call analizador%set_graph(graphRutas)
    call analizadorImpresoras%set_graph(graphImpresoras)
    call json%destroy()
end subroutine recargarRutas

subroutine cargarSucursales()
    WRITE(*,*) "Carga de sucursales seleccionada."
    print *, "Ingrese la ruta del archivo de sucursales: "
        read (*,'(A)') filename_json
        
        call json%initialize()
        call json%load(filename=filename_json)
        call json%print()
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', sucur_pointer, found)

        do i = 1, size
            call jsonc%get_child(sucur_pointer, i, deptPointer, found=found)


            call jsonc%get_child(deptPointer, 'id', partPointer, found=found)
            if (found) then
                call jsonc%get(partPointer, id_sucursal)
                print *, 'ID: ', id_sucursal
            end if
            
            call jsonc%get_child(deptPointer, 'departamento', partPointer, found=found)
            if (found) then
                call jsonc%get(partPointer, departamento)
                print *, 'Departamento: ', departamento
            end if
            
            call jsonc%get_child(deptPointer, 'direccion', partPointer, found=found)
            if (found) then
                call jsonc%get(partPointer, direccion)
                print *, 'Dirección: ', direccion
            end if
            
            call jsonc%get_child(deptPointer, 'password', partPointer, found=found)
            if (found) then
                call jsonc%get(partPointer, password)
                print *, 'Contraseña: ', password
            end if
            call avl_sucursales%insert_node(id_sucursal, departamento, direccion, password)
        end do
        call avl_sucursales%graph_avl_tree()
        call json%destroy()
end subroutine cargarSucursales

subroutine menu_programa()
        print *, "------------Menú de opciones------------"
        print *, "1. Carga de archivos"
        print *, "2. Sucursales"
        print *, "3. Reportes"
        print *, "0. Salir"
        print *, "Ingrese una opción: "
end subroutine menu_programa

END PROGRAM menu_program
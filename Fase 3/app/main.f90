PROGRAM menu_program
    use json_module
    use hash_table
    use block_chain
    use Avl_Tree
    use PasswordEncryptor
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
    type(Tree_t) :: sucursales_tree
    type(grafo) :: graphRutas
    type(grafo) :: graphImpresoras
    integer :: distancia, s1, s2, imp_mantenimiento
    type(analyzer) :: analizador, analizadorImpresoras
    character(:), allocatable :: hashedPassword
    character(len = 32) :: password_sucursal
    logical :: session_active
    type(block) :: blockChainMerckle
    type(chainer) :: chain

    DO
        call login(session_active)
        if (.not. session_active) then
            print *, "Intento fallido"
            exit
        endif    
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
            read (*,'(A)') password_sucursal
            print *, password_sucursal
            encontrado = .false.
            hashedPassword = hashPassword(trim(password_sucursal))
            call sucursales_tree%searchNode(id_sucursal, hashedPassword, encontrado)
              
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
end do

CONTAINS

subroutine menu_sucursal()
    integer(8) :: dpi_buscar
do
    print *, "Bienvenido a la sucursal ", id_sucursal
    print *, "Ingrese una opcion: "
    print *, "1. Carga de tecnicos"
    print *, "2. Generar recorrido mas optimo"
    print *, "3. Información Tecnico en Especifico"
    print *, "4. Listar Tecnicos"
    print *, "5. Generar Reportes"
    print *, "6. Regresar"
    read (*,*) opcion
    SELECT case (opcion)
    case (1)
        call cargarTecnicos()
    case (2)
        call generarMasOptimo()
    case (3)
        print *, "Ingrese el DPI del tecnico: "
        read (*,*) dpi_buscar
        call sucursales_tree%searchNode_tecnico_especifico(id_sucursal, dpi_buscar)
    case (4)
        call sucursales_tree%searchNode_listar(id_sucursal)
    case (6)
        exit
    end SELECT
end do
        
end subroutine menu_sucursal


subroutine login(success)
    logical, intent(out) :: success
    character(len=20) :: input_user, input_pass
    character(len=20), parameter :: correct_user = 'EDD'
    character(len=20), parameter :: correct_pass = '3'

    success = .false.

    print *, "Por favor, ingrese su usuario:"
    read(*, '(A)') input_user
    print *, "Por favor, ingrese su contraseña:"
    read(*, '(A)') input_pass

    if (trim(input_user) == correct_user .and. trim(input_pass) == correct_pass) then
        print *, "Inicio de sesión correcto."
        success = .true.
    else
        print *, "Usuario o contraseña incorrectos."
    endif
end subroutine login



subroutine generarMasOptimo()
    type(result_list) :: lista_resultado, lista_imp 
    integer :: total, total_imp, total2, total_imp2, ganancia1, ganancia2
    integer(8) :: dpi_te
    print *, "Ingrese la sucursal de destino: "
    read (*,*) s2
    lista_resultado = analizador%get_shortest_path(id_sucursal, s2)
    print *,"----------------------------"
    print *, "La ruta mas corta es:"
    print *,""
    call lista_resultado%print()
    total = lista_resultado%total_weight * 80
    total_imp = lista_resultado%total_printers * 100
    ganancia1 = total_imp - total
    print *, "El total de km recorridos fue de: ", lista_resultado%total_weight
    print *, "Las impresoras atendidas fueron : ",lista_resultado%total_printers
    lista_imp = analizadorImpresoras%get_longest_path(id_sucursal, s2)
    print *,"----------------------------"
    print *, "La ruta mas larga es:"
    print *,""
    call lista_imp%print()
    total2 = lista_imp%total_weight * 100
    total_imp2 = lista_imp%total_printers * 80
    ganancia2 = total2 - total_imp2
    print *, "El total de impresoras es de: ", lista_imp%total_weight    
    print *, "Los km totales son: ", lista_imp%total_printers
    print *,"----------------------------"
    print *, "LA RUTA RECOMENDADA ES: "
    if(ganancia1 > ganancia2) then
        print *, "La ruta mas corta es la mas optima!"
        print *, ""
        print *, "Resultados de la ruta mas corta"
        print *, "El costo total de la distancia recorrida es : ", total
        print *, "La ganancia total de las impresoras atendidas es : ", total_imp
        print *,""
        print *, "Resultados de la ruta mas larga"
        print *, "El costo total de la distancia recorrida es : ", total_imp2
        print *, "La ganancia total de las impresoras atendidas es : ", total2
        call blockChainMerckle%generarBloque(lista_resultado, sucursales_tree, .true.)
        call chain%aniadir_bloque(blockChainMerckle)
        call chain%generarJson(.true.)
    elseif (ganancia1 < ganancia2) then
        print *, "La ruta mas larga es la mas optima!"
        print *, ""
        print *, "Resultados de la ruta mas larga"
        print *, "El costo total de la distancia recorrida es : ", total_imp2
        print *, "La ganancia total de las impresoras atendidas es : ", total2

        print *, ""
        print *, "Resultados de la ruta mas corta"
        print *, "El costo total de la distancia recorrida es : ", total
        print *, "La ganancia total de las impresoras atendidas es : ", total_imp
        call blockChainMerckle%generarBloque(lista_imp, sucursales_tree, .false.)
        call chain%aniadir_bloque(blockChainMerckle)
        call chain%generarJson(.false.)
    else if(ganancia1 == ganancia2) then
        print *, "Ambas rutas son igual de optimas!"
        print *, "El costo total de la distancia recorrida es : ", total
        print *, "la ganancia total de las impresoras atendidas es : ", total_imp
        call blockChainMerckle%generarBloque(lista_resultado, sucursales_tree, .true.)
        call chain%aniadir_bloque(blockChainMerckle)
        call chain%generarJson(.true.)
    end if
    print *,"----------------------------"
    
    print *, "Ingrese el dpi del tecnico"
    read (*,*) dpi_te
    print *, "Datos del tecnico: "
    call sucursales_tree%searchNode_tecnico_especifico(id_sucursal, dpi_te)
    print *, ""

    call graphRutas%reset_grafo()
    call graphImpresoras%reset_grafo()
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
        call sucursales_tree%searchNode_hash(id_sucursal, table)
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

    call analizador%set_grafo(graphRutas)
    call analizadorImpresoras%set_grafo(graphImpresoras)
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
    
    call analizador%set_grafo(graphRutas)
    call analizadorImpresoras%set_grafo(graphImpresoras)
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
                hashedPassword = hashPassword(password)
            end if
            call sucursales_tree%insert_node(id_sucursal, departamento, direccion, hashedPassword)
        end do
        call sucursales_tree%graph_avl_tree()
        call json%destroy()
end subroutine cargarSucursales

subroutine menu_programa()
        print *, "------------Menu de opciones------------"
        print *, "1. Carga de archivos"
        print *, "2. Sucursales"
        print *, "3. Reportes"
        print *, "0. Salir"
        print *, "Ingrese una opción: "
end subroutine menu_programa

END PROGRAM menu_program
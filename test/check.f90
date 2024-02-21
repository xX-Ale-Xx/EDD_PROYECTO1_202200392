program example
    use :: json_module, rk => json_rk
    implicit none

    real(kind=rk)              :: t0, dt, tf, mu
    real(kind=rk), allocatable :: x0(:)
    type(json_file)            :: json
    logical                    :: is_found

    ! Initialise the json_file object.
    call json%initialize()

    ! Load the file.
    call json%load_file('config.json'); if (json%failed()) stop

    ! Read in the data.
    json_block: block
        call json%get('t0', t0, is_found); if (.not. is_found) exit json_block
        call json%get('dt', dt, is_found); if (.not. is_found) exit json_block
        call json%get('tf', tf, is_found); if (.not. is_found) exit json_block
        call json%get('mu', mu, is_found); if (.not. is_found) exit json_block
        call json%get('x0', x0, is_found); if (.not. is_found) exit json_block
    end block json_block

    ! Output values.
    if (is_found) then
        print *, t0, dt, tf, mu
        print *, x0
    end if

    ! Clean up.
    call json%destroy()
end program example

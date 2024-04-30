module PasswordEncryptor
    implicit none

contains

    function hashPassword(password) result(hashedPassword)
        character(len=*), intent(in) :: password
        character(len=32) :: hashedPassword
        integer :: i, sum

        sum = 0
        do i = 1, len(password)
            sum = sum + ichar(password(i:i))
        end do

        write(hashedPassword, '(Z32)') sum  
    end function hashPassword

end module PasswordEncryptor

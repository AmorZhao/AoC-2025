module string_utils
    implicit none
contains

    integer function parse_int(string) result(value)
        character(*), intent(in) :: string

        read(string, *) value

    end function parse_int

end module string_utils

module io_utils
    implicit none
contains

    subroutine read_lines(filename, lines, n, verbose)
        character(len=*), intent(in) :: filename
        character(len=200), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: n
        logical, intent(in), optional :: verbose
        integer :: unit1, unit2, ios_open, ios_read, count, i
        character(len=200) :: line

        n = 0
        if (allocated(lines)) deallocate(lines)

        count = 0
        open(newunit=unit1, file=filename, status="old", action="read", iostat=ios_open)
        if (ios_open /= 0) then
            allocate(lines(0))
            n = 0
            return
        end if

        do
            read(unit1, '(A)', iostat=ios_read) line
            if (ios_read /= 0) exit
            count = count + 1
        end do
        close(unit1)

        if (count <= 0) then
            allocate(lines(0))
            n = 0
            return
        end if

        allocate(lines(count))

        open(newunit=unit2, file=filename, status="old", action="read", iostat=ios_open)
        if (ios_open /= 0) then
            deallocate(lines)
            allocate(lines(0))
            n = 0
            return
        end if

        do i = 1, count
            read(unit2, '(A)', iostat=ios_read) line
            if (ios_read /= 0) then
                lines(i) = ''
            else
                lines(i) = adjustl(trim(line))
            end if
        end do
        close(unit2)

        n = count

        if (present(verbose)) then
            print *, "Read", n, "lines from ", trim(filename)
            ! do i = 1, n
            !     print *, trim(lines(i))
            ! end do
        end if
    end subroutine read_lines

end module io_utils

program main
    use io_utils
    use string_utils
    implicit none

    character(len=200), allocatable :: lines(:)
    integer :: n, i
    character(len=200) :: line
    character(len=1) :: direction
    integer :: clicks
    integer :: currentPosition

    call read_lines("2025/input/day1.txt", lines, n, verbose=.true.)

    block ! Part 1
        integer :: count0

        currentPosition = 50
        count0 = 0

        do i = 1, n
            line = trim(lines(i))
            direction = line(1:1)
            clicks = parse_int(line(2:))

            if (direction == 'L') then
                currentPosition = modulo(currentPosition - clicks, 100)
            else    ! direction == 'R'
                currentPosition = modulo(currentPosition + clicks, 100)
            end if

            if (currentPosition == 0) count0 = count0 + 1
        end do

        print *, "Day 1 part 1 =", count0
    end block

    block ! Part 2
        integer :: clicksToZero
        integer :: count0, total0

        currentPosition = 50
        total0 = 0

        do i = 1, n
            line = trim(lines(i))
            direction = line(1:1)
            clicks = parse_int(line(2:))

            if (direction == 'R') then
                clicksToZero = 100 - currentPosition
                currentPosition = modulo(currentPosition + clicks, 100)
            else    ! direction == 'L'
                clicksToZero = currentPosition
                currentPosition = modulo(currentPosition - clicks, 100)
            end if

            if (clicksToZero == 0) clicksToZero = 100

            if (clicks < clicksToZero) then
                count0 = 0
            else
                count0 = 1 + (clicks - clicksToZero) / 100
            end if

            total0 = total0 + count0
        end do

        print *, "Day 1 part 2 =", total0
    end block

end program main

PROGRAM main
    USE io_utils
    USE string_utils
    IMPLICIT NONE

    CHARACTER(LEN=200), ALLOCATABLE :: lines(:)
    INTEGER :: n, i
    CHARACTER(LEN=200) :: line
    CHARACTER(LEN=1) :: direction
    INTEGER :: clicks
    INTEGER :: currentPosition

    CALL ReadLines("2025/input/day1.txt", lines, n, verbose=.TRUE.)

    BLOCK ! Part 1
        INTEGER :: count0

        currentPosition = 50
        count0 = 0

        DO i = 1, n
            line = TRIM(lines(i))
            direction = line(1:1)
            clicks = ParseInt(line(2:))

            IF (direction == 'L') THEN
                currentPosition = modulo(currentPosition - clicks, 100)
            ELSE    ! direction == 'R'
                currentPosition = modulo(currentPosition + clicks, 100)
            END IF

            IF (currentPosition == 0) count0 = count0 + 1
        END DO

        PRINT *, "Day 1 part 1 =", count0
    END BLOCK

    BLOCK ! Part 2
        INTEGER :: clicksToZero
        INTEGER :: count0, total0

        currentPosition = 50
        total0 = 0

        DO i = 1, n
            line = TRIM(lines(i))
            direction = line(1:1)
            clicks = ParseInt(line(2:))

            IF (direction == 'L') THEN
                clicksToZero = currentPosition
                currentPosition = modulo(currentPosition - clicks, 100)
            ELSE    ! direction == 'R'
                clicksToZero = 100 - currentPosition
                currentPosition = modulo(currentPosition + clicks, 100)
            END IF

            IF (clicksToZero == 0) clicksToZero = 100

            IF (clicks < clicksToZero) THEN
                count0 = 0
            ELSE
                count0 = 1 + (clicks - clicksToZero) / 100
            END IF

            total0 = total0 + count0
        END DO

        PRINT *, "Day 1 part 2 =", total0
    END BLOCK

END PROGRAM main

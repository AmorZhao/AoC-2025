MODULE io_utils
    IMPLICIT NONE
CONTAINS

    SUBROUTINE ReadLines(filename, lines, n, verbose)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        CHARACTER(LEN=200), ALLOCATABLE, INTENT(OUT) :: lines(:)
        INTEGER, INTENT(OUT) :: n
        LOGICAL, INTENT(IN), OPTIONAL :: verbose
        INTEGER :: unit1, unit2, ios_open, ios_read, count, i
        CHARACTER(LEN=200) :: line

        n = 0
        IF (ALLOCATED(lines)) DEALLOCATE(lines)

        count = 0
        OPEN(NEWUNIT=unit1, FILE=filename, STATUS="old", ACTION="read", IOSTAT=ios_open)
        IF (ios_open /= 0) THEN
            ALLOCATE(lines(0))
            n = 0
            RETURN
        END IF

        DO
            READ(unit1, '(A)', IOSTAT=ios_read) line
            IF (ios_read /= 0) EXIT
            count = count + 1
        END DO
        CLOSE(unit1)

        IF (count <= 0) THEN
            ALLOCATE(lines(0))
            n = 0
            RETURN
        END IF

        ALLOCATE(lines(count))

        OPEN(NEWUNIT=unit2, FILE=filename, STATUS="old", ACTION="read", IOSTAT=ios_open)
        IF (ios_open /= 0) THEN
            DEALLOCATE(lines)
            ALLOCATE(lines(0))
            n = 0
            RETURN
        END IF

        DO i = 1, count
            READ(unit2, '(A)', IOSTAT=ios_read) line
            IF (ios_read /= 0) THEN
                lines(i) = ''
            ELSE
                lines(i) = ADJUSTL(TRIM(line))
            END IF
        END DO
        CLOSE(unit2)

        n = count

        IF (PRESENT(verbose)) THEN
            PRINT *, "Read", n, "lines from ", TRIM(filename)
            ! DO i = 1, n
            !     PRINT *, TRIM(lines(i))
            ! END DO
        END IF
    END SUBROUTINE ReadLines

END MODULE io_utils

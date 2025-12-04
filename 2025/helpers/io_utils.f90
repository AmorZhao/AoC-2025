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

        IF (verbose) THEN
            PRINT *, "Read", n, "lines from ", TRIM(filename)
            ! DO i = 1, n
            !     PRINT *, TRIM(lines(i))
            ! END DO
        END IF
    END SUBROUTINE ReadLines

    SUBROUTINE ReadSingleLine(filename, line, verbose)
        CHARACTER(LEN=*), INTENT(IN)            :: filename
        CHARACTER(:), ALLOCATABLE, INTENT(OUT)  :: line
        LOGICAL, INTENT(IN), OPTIONAL           :: verbose
        INTEGER     :: ios, unit
        CHARACTER(LEN=5000) :: tmp

        IF (ALLOCATED(line)) DEALLOCATE(line)

        OPEN(NEWUNIT=unit, FILE=filename, STATUS="old", ACTION="read", IOSTAT=ios)
        IF (ios /= 0) THEN
            ALLOCATE(CHARACTER(LEN=0) :: line)
            RETURN
        END IF

        READ(unit, '(A)', IOSTAT=ios) tmp
        CLOSE(unit)

        IF (ios == 0) THEN
            ALLOCATE(CHARACTER(LEN=LEN_TRIM(tmp)) :: line)
            line = TRIM(tmp)
        ELSE
            ALLOCATE(CHARACTER(LEN=0) :: line)
        END IF

        IF (verbose) THEN
            PRINT *, "Read from ", TRIM(filename), ": ", TRIM(line)
        END IF
    END SUBROUTINE ReadSingleLine

    SUBROUTINE ReadMatrix(filename, matrix, numRows, numColumns, verbose)
        CHARACTER(LEN=*), INTENT(IN) :: filename
        CHARACTER, ALLOCATABLE, INTENT(OUT) :: matrix(:,:)
        INTEGER, INTENT(OUT) :: numRows, numColumns
        LOGICAL, INTENT(IN), OPTIONAL :: verbose
        CHARACTER(LEN=200), ALLOCATABLE :: lines(:)
        INTEGER :: n, i, j

        CALL ReadLines(filename, lines, n, .FALSE.)

        numRows = n
        numColumns = LEN_TRIM(lines(1))
        ALLOCATE(matrix(numRows, numColumns))

        DO i = 1, numRows
            DO j = 1, numColumns
                matrix(i,j) = lines(i)(j:j)
            END DO
        END DO

        IF (verbose) THEN
            PRINT *, "Read matrix of size ", numRows, " x ", numColumns, " from ", TRIM(filename)
        END IF

    END SUBROUTINE ReadMatrix

END MODULE io_utils

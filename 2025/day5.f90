PROGRAM main
    USE io_utils
    USE string_utils
    USE number_utils
    IMPLICIT NONE

    CHARACTER(LEN=200), ALLOCATABLE :: lines(:)
    INTEGER :: n, i, j
    CHARACTER(LEN=200) :: line

    CALL ReadLines("input/day5.txt", lines, n, verbose=.TRUE.)

    BLOCK ! Part 1
        INTEGER :: numRanges
        TYPE(Range), ALLOCATABLE :: ranges(:)
        INTEGER :: dashPosition
        INTEGER(8) :: availableId
        INTEGER :: countFreshAvailable

        numRanges = 0
        DO i = 1, n
            IF (TRIM(lines(i)) == "") EXIT
            numRanges = numRanges + 1
        END DO

        ALLOCATE(ranges(numRanges))

        DO i = 1, numRanges
            line = TRIM(lines(i))
            dashPosition = INDEX(line, '-')

            ranges(i)%leftBound  = ParseInt_I8(line(1:dashPosition-1))
            ranges(i)%rightBound = ParseInt_I8(line(dashPosition+1:))
        END DO

        CALL SortByRangeLeft(ranges)

        CALL MergeRanges(ranges)

        countFreshAvailable = 0
        DO j = i + 1, n
            availableId = ParseInt_I8(TRIM(lines(j)))

            IF (IsInRange(availableId, ranges)) THEN
                countFreshAvailable = countFreshAvailable + 1
            END IF
        END DO

        PRINT *, "Day 5 part 1 =", countFreshAvailable
    END BLOCK

    BLOCK ! Part 2
        INTEGER :: numRanges
        TYPE(Range), ALLOCATABLE :: ranges(:)
        INTEGER :: dashPosition
        INTEGER(8) :: countFresh

        numRanges = 0
        DO i = 1, n
            IF (TRIM(lines(i)) == "") EXIT
            numRanges = numRanges + 1
        END DO

        ALLOCATE(ranges(numRanges))

        DO i = 1, numRanges
            line = TRIM(lines(i))
            dashPosition = INDEX(line, '-')

            ranges(i)%leftBound  = ParseInt_I8(line(1:dashPosition-1))
            ranges(i)%rightBound = ParseInt_I8(line(dashPosition+1:))
        END DO

        CALL SortByRangeLeft(ranges)

        CALL MergeRanges(ranges)

        numRanges = SIZE(ranges)

        countFresh = 0
        DO i = 1, numRanges
            countFresh = countFresh + (ranges(i)%rightBound - ranges(i)%leftBound + 1)
        END DO

        PRINT *, "Day 5 part 2 =", countFresh
    END BLOCK


END PROGRAM main

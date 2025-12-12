MODULE number_utils
    IMPLICIT NONE

    TYPE :: Range
        INTEGER(8) :: leftBound
        INTEGER(8) :: rightBound
    END TYPE Range

    TYPE :: CONNECTION
        INTEGER :: indexA
        INTEGER :: indexB
        INTEGER(8) :: distance    ! squared
    END TYPE CONNECTION

CONTAINS

    RECURSIVE SUBROUTINE QuickSort(A, left, right)
        INTEGER, INTENT(INOUT) :: A(:)
        INTEGER, INTENT(IN) :: left, right
        INTEGER :: i, j
        INTEGER :: pivot, tmp

        i = left
        j = right
        pivot = A((left + right) / 2)

        DO
            DO WHILE (A(i) < pivot)
                i = i + 1
            END DO

            DO WHILE (A(j) > pivot)
                j = j - 1
            END DO

            IF (i <= j) THEN
                tmp = A(i)
                A(i) = A(j)
                A(j) = tmp
                i = i + 1
                j = j - 1
            END IF

            IF (i > j) EXIT
        END DO

        IF (left < j) THEN
            CALL QuickSort(A, left, j)
        END IF

        IF (i < right) THEN
            CALL QuickSort(A, i, right)
        END IF

    END SUBROUTINE QuickSort

    SUBROUTINE Sort(A)
        INTEGER, INTENT(INOUT) :: A(:)

        IF (SIZE(A) > 1) THEN
            CALL QuickSort(A, 1, SIZE(A))
        END IF
    END SUBROUTINE Sort

    RECURSIVE SUBROUTINE QuickSort_I8(A, left, right)
        INTEGER(8), INTENT(INOUT) :: A(:)
        INTEGER, INTENT(IN) :: left, right
        INTEGER :: i, j
        INTEGER(8) :: pivot, tmp

        i = left
        j = right
        pivot = A((left + right) / 2)

        DO
            DO WHILE (A(i) < pivot)
                i = i + 1
            END DO

            DO WHILE (A(j) > pivot)
                j = j - 1
            END DO

            IF (i <= j) THEN
                tmp = A(i)
                A(i) = A(j)
                A(j) = tmp
                i = i + 1
                j = j - 1
            END IF

            IF (i > j) EXIT
        END DO

        IF (left < j) THEN
            CALL QuickSort_I8(A, left, j)
        END IF

        IF (i < right) THEN
            CALL QuickSort_I8(A, i, right)
        END IF

    END SUBROUTINE QuickSort_I8

    SUBROUTINE Sort_I8(A)
        INTEGER(8), INTENT(INOUT) :: A(:)

        IF (SIZE(A) > 1) THEN
            CALL QuickSort_I8(A, 1, SIZE(A))
        END IF
    END SUBROUTINE Sort_I8

    SUBROUTINE UniqueSort(A)
        INTEGER, ALLOCATABLE, INTENT(INOUT) :: A(:)
        INTEGER, ALLOCATABLE :: uniqueSorted(:)
        INTEGER :: n, uniqueLength, i

        n = SIZE(A)
        CALL Sort(A)

        ALLOCATE(uniqueSorted(n))
        uniqueLength = 1
        uniqueSorted(1) = A(1)

        DO i = 2, n
            IF (A(i) /= uniqueSorted(uniqueLength)) THEN
                uniqueLength = uniqueLength + 1
                uniqueSorted(uniqueLength) = A(i)
            END IF
        END DO

        CALL move_alloc(uniqueSorted, A)
        IF (SIZE(A) /= uniqueLength) THEN
            A = A(:uniqueLength)
        END IF
    END SUBROUTINE UniqueSort

    INTEGER FUNCTION GetIndex(A, targetValue) RESULT(index)
        INTEGER, INTENT(IN) :: A(:), targetValue
        INTEGER :: lowerBound, upperBound, mid
        lowerBound = 1
        upperBound = SIZE(A)
        DO WHILE (lowerBound <= upperBound)
            mid = (lowerBound + upperBound)/2
            IF (A(mid) == targetValue) THEN
                index = mid
                RETURN
            ELSE IF (A(mid) < targetValue) THEN
                lowerBound = mid + 1
            ELSE  ! IF (A(mid) > targetValue)
                upperBound = mid - 1
            END IF
        END DO

        PRINT *, "Value not found"
        STOP
    END FUNCTION GetIndex

    RECURSIVE SUBROUTINE QuickSortRanges(A, left, right)
        TYPE(Range), INTENT(INOUT) :: A(:)
        INTEGER, INTENT(IN) :: left, right
        INTEGER :: i, j
        TYPE(Range) :: pivot, tmp

        i = left
        j = right
        pivot = A((left + right) / 2)

        DO
            DO WHILE (A(i)%leftBound < pivot%leftBound)
                i = i + 1
            END DO

            DO WHILE (A(j)%leftBound > pivot%leftBound)
                j = j - 1
            END DO

            IF (i <= j) THEN
                tmp = A(i)
                A(i) = A(j)
                A(j) = tmp
                i = i + 1
                j = j - 1
            END IF

            IF (i > j) EXIT
        END DO

        IF (left < j) THEN
            CALL QuickSortRanges(A, left, j)
        END IF

        IF (i < right) THEN
            CALL QuickSortRanges(A, i, right)
        END IF

    END SUBROUTINE QuickSortRanges

    SUBROUTINE SortByRangeLeft(A)
        TYPE(Range), INTENT(INOUT) :: A(:)

        IF (SIZE(A) > 1) THEN
            CALL QuickSortRanges(A, 1, SIZE(A))
        END IF
    END SUBROUTINE SortByRangeLeft

    SUBROUTINE MergeRanges(ranges)
        TYPE(Range), ALLOCATABLE, INTENT(INOUT) :: ranges(:)
        TYPE(Range), ALLOCATABLE :: mergedRanges(:)
        INTEGER :: n, i, mergedId

        n = SIZE(ranges)
        ALLOCATE(mergedRanges(n))

        mergedRanges(1) = ranges(1)
        mergedId = 1

        DO i = 2, n
            IF (ranges(i)%leftBound <= mergedRanges(mergedId)%rightBound + 1) THEN
                mergedRanges(mergedId)%rightBound = MAX(mergedRanges(mergedId)%rightBound, ranges(i)%rightBound)
            ELSE
                mergedId = mergedId + 1
                mergedRanges(mergedId) = ranges(i)
            END IF
        END DO

        DEALLOCATE(ranges)
        ALLOCATE(ranges(mergedId))

        ranges = mergedRanges(1:mergedId)

        DEALLOCATE(mergedRanges)
    END SUBROUTINE MergeRanges

    LOGICAL FUNCTION IsInRange(id, ranges)
        INTEGER(8), INTENT(IN) :: id
        TYPE(Range), INTENT(IN) :: ranges(:)
        INTEGER :: numRanges, i

        numRanges = SIZE(ranges)

        DO i = 1, numRanges
            IF (id < ranges(i)%leftBound) THEN
                IsInRange = .FALSE.
                RETURN
            END IF

            IF (id >= ranges(i)%leftBound .AND. id <= ranges(i)%rightBound) THEN
                IsInRange = .TRUE.
                RETURN
            END IF
        END DO
    END FUNCTION IsInRange

    RECURSIVE SUBROUTINE QuickSortConnections(A, left, right)
        TYPE(CONNECTION), INTENT(INOUT) :: A(:)
        INTEGER, INTENT(IN) :: left, right
        INTEGER :: i, j
        TYPE(CONNECTION) :: pivot, tmp

        IF (left < 1 .OR. right > SIZE(A)) RETURN
        IF (left >= right) RETURN

        i = left
        j = right
        pivot = A((left + right) / 2)

        DO
            DO WHILE (A(i)%distance < pivot%distance)
                i = i + 1
            END DO

            DO WHILE (A(j)%distance > pivot%distance)
                j = j - 1
            END DO

            IF (i <= j) THEN
                tmp = A(i)
                A(i) = A(j)
                A(j) = tmp
                i = i + 1
                j = j - 1
            END IF

            IF (i > j) EXIT
        END DO

        IF (left < j) THEN
            CALL QuickSortConnections(A, left, j)
        END IF

        IF (i < right) THEN
            CALL QuickSortConnections(A, i, right)
        END IF
    END SUBROUTINE QuickSortConnections

    SUBROUTINE SortByDistance(connections)
        TYPE(CONNECTION), INTENT(INOUT) :: connections(:)

        IF (SIZE(connections) > 1) THEN
            CALL QuickSortConnections(connections, 1, SIZE(connections))
        END IF
    END SUBROUTINE SortByDistance

END MODULE number_utils

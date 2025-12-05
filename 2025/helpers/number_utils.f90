MODULE number_utils
    IMPLICIT NONE

    TYPE :: Range
        INTEGER(8) :: leftBound
        INTEGER(8) :: rightBound
    END TYPE Range

CONTAINS

    RECURSIVE SUBROUTINE QuickSort(A, left, right)
        REAL, INTENT(INOUT) :: A(:)
        INTEGER, INTENT(IN) :: left, right
        INTEGER :: i, j
        REAL :: pivot, tmp

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
        REAL, INTENT(INOUT) :: A(:)

        IF (SIZE(A) > 1) THEN
            CALL QuickSort(A, 1, SIZE(A))
        END IF
    END SUBROUTINE Sort

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

END MODULE number_utils

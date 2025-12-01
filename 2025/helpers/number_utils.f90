MODULE number_utils
    IMPLICIT NONE
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

END MODULE number_utils

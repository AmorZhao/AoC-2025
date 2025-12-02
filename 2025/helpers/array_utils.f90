MODULE array_utils
    IMPLICIT NONE
CONTAINS

    SUBROUTINE Append(array, newValue)
        INTEGER(8), ALLOCATABLE, INTENT(INOUT) :: array(:)
        INTEGER(8), INTENT(IN) :: newValue
        INTEGER :: n
        INTEGER(8), ALLOCATABLE :: tmp(:)

        IF (.NOT. ALLOCATED(array)) THEN
            ALLOCATE(array(1))
            array(1) = newValue
            RETURN
        END IF

        n = SIZE(array)
        CALL move_alloc(array, tmp)
        ALLOCATE(array(n+1))

        IF (n > 0) array(1:n) = tmp

        array(n+1) = newValue

        IF (ALLOCATED(tmp)) DEALLOCATE(tmp)
    END SUBROUTINE Append

END MODULE array_utils

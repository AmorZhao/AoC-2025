MODULE string_utils
    IMPLICIT NONE
CONTAINS

    INTEGER(8) FUNCTION ParseInt_I8(string) RESULT(value)
        CHARACTER(*), INTENT(IN) :: string
        INTEGER(8) :: tmp

        READ(string, *) tmp
        value = tmp
        RETURN
    END FUNCTION ParseInt_I8

    INTEGER FUNCTION ParseInt(string) RESULT(value)
        CHARACTER(*), INTENT(IN) :: string

        READ(string, *) value
        RETURN
    END FUNCTION ParseInt

    SUBROUTINE SplitString(input, delimiter, parts)
        CHARACTER(*), INTENT(IN)  :: input
        CHARACTER(1), INTENT(IN)  :: delimiter
        CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: parts(:)
        INTEGER :: i, n, start, len_input
        CHARACTER(:), ALLOCATABLE :: token

        len_input = LEN_TRIM(input)

        n = 1
        DO i = 1, len_input
            IF (input(i:i) == delimiter) n = n + 1
        END DO

        ALLOCATE(parts(n), MOLD=input)

        start = 1
        n = 0

        DO i = 1, len_input
            IF (input(i:i) == delimiter) THEN
                n = n + 1

                ALLOCATE(token, MOLD=input(start:i-1))
                token = input(start:i-1)

                parts(n) = token
                DEALLOCATE(token)

                start = i + 1
            END IF
        END DO

        n = n + 1
        ALLOCATE(token, MOLD=input(start:len_input))
        token = input(start:len_input)
        parts(n) = token
        DEALLOCATE(token)

    END SUBROUTINE SplitString

END MODULE string_utils

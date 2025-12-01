MODULE string_utils
    IMPLICIT NONE
CONTAINS

    INTEGER FUNCTION ParseInt(string) RESULT(value)
        CHARACTER(*), INTENT(IN) :: string

        READ(string, *) value

    END FUNCTION ParseInt

END MODULE string_utils

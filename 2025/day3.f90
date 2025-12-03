PROGRAM main
    USE io_utils
    USE string_utils
    IMPLICIT NONE

    CHARACTER(LEN=200), ALLOCATABLE :: lines(:)
    INTEGER :: n, i

    CALL ReadLines("input/day3.txt", lines, n, verbose=.TRUE.)

    BLOCK ! part 1
        CHARACTER(LEN=200) :: line
        INTEGER :: lineLength
        INTEGER :: tens, ones
        INTEGER :: maxJoltage, tmpJoltage
        INTEGER :: sum

        sum = 0

        DO i = 1, n
            line = TRIM(lines(i))
            lineLength = LEN_TRIM(line)

            maxJoltage = -1

            DO tens = 1, lineLength-1

                DO ones = tens+1, lineLength

                    tmpJoltage = ParseInt(line(tens:tens) // line(ones:ones))

                    IF (tmpJoltage > maxJoltage) maxJoltage = tmpJoltage

                END DO

            END DO

            sum = sum + maxJoltage
        END DO

        PRINT *, "Day 3 part 1 =", sum
    END BLOCK

    BLOCK ! part 1 non brute force
        CHARACTER(LEN=200) :: line
        INTEGER :: sum

        sum = 0

        DO i = 1, n
            line = TRIM(lines(i))

            sum = sum + ParseInt(getLargestDigits(line, 2))
        END DO

        PRINT *, "Day 3 part 1 =", sum
    END BLOCK

    BLOCK ! part 2
        CHARACTER(LEN=200) :: line
        INTEGER(8) :: sum

        sum = 0

        DO i = 1, n
            line = TRIM(lines(i))
            sum = sum + ParseInt_I8(getLargestDigits(line, 12))
        END DO

        PRINT *, "Day 3 part 2 =", sum
    END BLOCK

    CONTAINS

    FUNCTION getLargestDigits(digits, numDigits) RESULT(result)
        CHARACTER(*), INTENT(IN) :: digits
        INTEGER, INTENT(IN) :: numDigits
        CHARACTER(LEN=:), ALLOCATABLE :: result

        INTEGER :: lineLength
        CHARACTER(LEN=1) :: maxDigits(numDigits)
        CHARACTER :: currentDigit
        INTEGER :: j
        INTEGER :: numSelected, numRemainingDigits

        lineLength = LEN_TRIM(digits)

        IF (numDigits == lineLength) THEN
            result = digits
            RETURN
        END IF

        numSelected = 0

        DO j = 1, lineLength
            currentDigit = digits(j:j)
            numRemainingDigits = lineLength - j + 1

            DO WHILE (numSelected > 0 .AND. maxDigits(numSelected) < currentDigit &
                .AND. (numSelected - 1 + numRemainingDigits) >= numDigits)
                numSelected = numSelected - 1
            END DO

            IF (numSelected < numDigits) THEN
                numSelected = numSelected + 1
                maxDigits(numSelected) = currentDigit
            END IF
        END DO

        result = ""

        DO j = 1, numDigits
            result = result // maxDigits(j)
        END DO

    END FUNCTION getLargestDigits

END PROGRAM main

PROGRAM main
    USE io_utils
    USE string_utils
    IMPLICIT NONE

    CHARACTER(LEN=4000), ALLOCATABLE :: lines(:)
    INTEGER :: n, i, j

    CALL ReadLines("input/day6.txt", lines, n, verbose=.TRUE.)

    BLOCK ! part 1
        INTEGER :: numberLineLength, symbolLineLength
        CHARACTER(LEN=:), ALLOCATABLE :: line
        CHARACTER(LEN=:), ALLOCATABLE :: symbols
        CHARACTER :: currentSymbol
        INTEGER :: nextSymbolIndex
        INTEGER :: resultIndex
        INTEGER(8), ALLOCATABLE :: resultArray(:)
        INTEGER(8) :: sumResults

        DO i = 1, n-1
            IF (LEN_TRIM(lines(i)) > numberLineLength) THEN
                numberLineLength = LEN_TRIM(lines(i))
            END IF
        END DO

        symbolLineLength = LEN_TRIM(lines(n))

        ALLOCATE(CHARACTER(LEN=numberLineLength) :: line)
        ALLOCATE(CHARACTER(LEN=symbolLineLength) :: symbols)
        symbols = TRIM(lines(n))

        ALLOCATE(INTEGER(8) :: resultArray(symbolLineLength))
        resultArray = 0
        resultIndex = 0

        DO i = 1, symbolLineLength-1
            IF (symbols(i:i) .EQ. ' ') THEN
                CYCLE
            END IF

            currentSymbol = symbols(i:i)
            resultIndex = resultIndex + 1

            DO nextSymbolIndex = i+1, symbolLineLength
                IF (symbols(nextSymbolIndex:nextSymbolIndex) .NE. ' ') THEN
                    EXIT
                END IF
            END DO

            IF (currentSymbol .EQ. '+') THEN
                DO j = 1, n-1
                    line = TRIM(lines(j))
                    resultArray(resultIndex) = resultArray(resultIndex) + ParseInt_I8(line(i:nextSymbolIndex-2))
                END DO

            ELSE IF (currentSymbol .EQ. '*') THEN
                resultArray(resultIndex) = 1
                DO j = 1, n-1
                    line = TRIM(lines(j))
                    resultArray(resultIndex) = resultArray(resultIndex) * ParseInt_I8(line(i:nextSymbolIndex-2))
                END DO
            END IF
        END DO

        currentSymbol = symbols(nextSymbolIndex:symbolLineLength)
        resultIndex = resultIndex + 1
        IF (currentSymbol .EQ. '+') THEN
            DO j = 1, n-1
                line = TRIM(lines(j))
                resultArray(resultIndex) = resultArray(resultIndex) + ParseInt_I8(line(nextSymbolIndex:))
            END DO

        ELSE IF (currentSymbol .EQ. '*') THEN
            resultArray(resultIndex) = 1
            DO j = 1, n-1
                line = TRIM(lines(j))
                resultArray(resultIndex) = resultArray(resultIndex) * ParseInt_I8(line(nextSymbolIndex:))
            END DO
        END IF

        sumResults = 0
        DO i = 1, resultIndex
            sumResults = sumResults + resultArray(i)
        END DO

        PRINT *, "Day 6 part 1 =", sumResults
    END BLOCK

    BLOCK ! part 2
        INTEGER :: numberLineLength, symbolLineLength
        CHARACTER(LEN=:), ALLOCATABLE :: line
        CHARACTER(LEN=:), ALLOCATABLE :: symbols
        CHARACTER :: currentSymbol
        INTEGER :: nextSymbolIndex
        INTEGER :: resultIndex
        INTEGER(8), ALLOCATABLE :: resultArray(:)
        INTEGER(8) :: sumResults
        INTEGER :: columnarIndex
        CHARACTER(LEN=(n-1)) :: tmpColumnarString
        INTEGER(8) :: tmpColumnarValue

        DO i = 1, n-1
            IF (LEN_TRIM(lines(i)) > numberLineLength) THEN
                numberLineLength = LEN_TRIM(lines(i))
            END IF
        END DO

        symbolLineLength = LEN_TRIM(lines(n))

        ALLOCATE(CHARACTER(LEN=numberLineLength) :: line)
        ALLOCATE(CHARACTER(LEN=symbolLineLength) :: symbols)
        symbols = TRIM(lines(n))

        ALLOCATE(INTEGER(8) :: resultArray(symbolLineLength))
        resultArray = 0
        resultIndex = 0

        DO i = 1, symbolLineLength-1
            IF (symbols(i:i) .EQ. ' ') THEN
                CYCLE
            END IF

            currentSymbol = symbols(i:i)
            resultIndex = resultIndex + 1

            DO nextSymbolIndex = i+1, symbolLineLength
                IF (symbols(nextSymbolIndex:nextSymbolIndex) .NE. ' ') THEN
                    EXIT
                END IF
            END DO

            IF (currentSymbol .EQ. '+') THEN
                tmpColumnarString = ''

                DO columnarIndex = i, nextSymbolIndex-2
                    DO j = 1, n-1
                        line = lines(j)
                        IF (line(columnarIndex:columnarIndex) .NE. ' ') THEN
                            tmpColumnarString(j:j) = line(columnarIndex:columnarIndex)
                        ELSE
                            tmpColumnarString(j:j) = ''
                        END IF
                    END DO

                    tmpColumnarValue = ParseInt_I8(TRIM(tmpColumnarString))
                    resultArray(resultIndex) = resultArray(resultIndex) + tmpColumnarValue

                END DO

            ELSE IF (currentSymbol .EQ. '*') THEN
                tmpColumnarString = ''
                resultArray(resultIndex) = 1

                DO columnarIndex = i, nextSymbolIndex-2
                    DO j = 1, n-1
                        line = lines(j)
                        IF (line(columnarIndex:columnarIndex) .NE. ' ') THEN
                            tmpColumnarString(j:j) = line(columnarIndex:columnarIndex)
                        ELSE
                            tmpColumnarString(j:j) = ''
                        END IF
                    END DO

                    tmpColumnarValue = ParseInt_I8(TRIM(tmpColumnarString))
                    resultArray(resultIndex) = resultArray(resultIndex) * tmpColumnarValue
                END DO
            END IF
        END DO

        currentSymbol = symbols(nextSymbolIndex:symbolLineLength)
        resultIndex = resultIndex + 1

        IF (currentSymbol .EQ. '+') THEN
            tmpColumnarString = ''

            DO columnarIndex = nextSymbolIndex, numberLineLength
                DO j = 1, n-1
                    line = lines(j)
                    IF (line(columnarIndex:columnarIndex) .NE. ' ') THEN
                        tmpColumnarString(j:j) = line(columnarIndex:columnarIndex)
                    ELSE
                        tmpColumnarString(j:j) = ''
                    END IF
                END DO

                tmpColumnarValue = ParseInt_I8(TRIM(tmpColumnarString))
                resultArray(resultIndex) = resultArray(resultIndex) + tmpColumnarValue
            END DO

        ELSE IF (currentSymbol .EQ. '*') THEN
            tmpColumnarString = ''
            resultArray(resultIndex) = 1

            DO columnarIndex = nextSymbolIndex, numberLineLength
                DO j = 1, n-1
                    line = lines(j)
                    IF (line(columnarIndex:columnarIndex) .NE. ' ') THEN
                        tmpColumnarString(j:j) = line(columnarIndex:columnarIndex)
                    ELSE
                        tmpColumnarString(j:j) = ''
                    END IF
                END DO

                tmpColumnarValue = ParseInt_I8(TRIM(tmpColumnarString))
                resultArray(resultIndex) = resultArray(resultIndex) * tmpColumnarValue
            END DO
        END IF

        sumResults = 0
        DO i = 1, resultIndex
            sumResults = sumResults + resultArray(i)
        END DO

        PRINT *, "Day 6 part 2 =", sumResults
    END BLOCK

END PROGRAM main

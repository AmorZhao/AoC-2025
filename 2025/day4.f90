PROGRAM main
    USE io_utils
    USE string_utils
    IMPLICIT NONE

    CHARACTER, ALLOCATABLE :: matrix(:,:)
    INTEGER :: numRows, numColumns

    CALL ReadMatrix("input/day4.txt", matrix, numRows, numColumns, verbose=.TRUE.)

    BLOCK ! part 1
        INTEGER :: i, j
        INTEGER :: leftBound, rightBound, upperBound, lowerBound
        INTEGER :: countAdjacent
        INTEGER :: countAccessible

        countAccessible = 0

        DO i = 1, numRows
            DO j = 1, numColumns
                IF (matrix(i,j) == '@') THEN
                    upperBound = MAX(1, i-1)
                    lowerBound = MIN(numRows, i+1)
                    leftBound  = MAX(1, j-1)
                    rightBound = MIN(numColumns, j+1)

                    countAdjacent = COUNT( matrix(upperBound:lowerBound, leftBound:rightBound) == '@' ) - 1

                    IF (countAdjacent < 4) THEN
                        countAccessible = countAccessible + 1
                    END IF
                END IF
            END DO
        END DO

        PRINT *, "Day 4 part 1 =", countAccessible
    END BLOCK

    BLOCK ! part 2
        INTEGER :: i, j
        INTEGER :: upperBound, lowerBound, leftBound, rightBound
        INTEGER :: countAdjacent
        INTEGER :: countNewlyAccessible, countAccessible

        countAccessible = 0

        DO
            countNewlyAccessible = 0

            DO i = 1, numRows
                DO j = 1, numColumns
                    IF (matrix(i,j) == '@') THEN
                        upperBound = MAX(1, i-1)
                        lowerBound = MIN(numRows, i+1)
                        leftBound  = MAX(1, j-1)
                        rightBound = MIN(numColumns, j+1)

                        countAdjacent = COUNT( matrix(upperBound:lowerBound, leftBound:rightBound) == '@' ) - 1

                        IF (countAdjacent < 4) THEN
                            matrix(i,j) = 'x'
                            countNewlyAccessible = countNewlyAccessible + 1
                        END IF
                    END IF
                END DO
            END DO

            IF (countNewlyAccessible == 0) EXIT

            countAccessible = countAccessible + countNewlyAccessible
        END DO

        PRINT *, "Day 4 part 2 =", countAccessible
    END BLOCK

END PROGRAM main

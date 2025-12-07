PROGRAM main
    USE io_utils
    USE string_utils
    IMPLICIT NONE

    CHARACTER, ALLOCATABLE :: matrix(:,:)
    INTEGER :: numRows, numColumns

    CALL ReadMatrix("input/day7.txt", matrix, numRows, numColumns, verbose=.TRUE.)

    BLOCK ! part 1
        INTEGER, ALLOCATABLE :: beamMatrix(:,:)
        CHARACTER(LEN=numColumns) :: rowString
        INTEGER :: beamStartingPosition
        INTEGER :: splitMinPosition, splitMaxPosition
        INTEGER :: i, j
        INTEGER :: countSplits

        ALLOCATE(beamMatrix(numRows, numColumns))

        rowString = ''
        DO j = 1, numColumns
            rowString(j:j) = matrix(1, j)
        END DO
        beamStartingPosition = INDEX(rowString, 'S')

        beamMatrix = 0
        beamMatrix(2, beamStartingPosition) = 1

        splitMinPosition = beamStartingPosition
        splitMaxPosition = beamStartingPosition

        countSplits = 0
        DO i = 2, numRows/2

            DO j = splitMinPosition, splitMaxPosition
                IF (matrix(i*2-1, j) .EQ. '^') THEN
                    IF (beamMatrix(i*2-2, j) .EQ. 1) THEN
                        beamMatrix(i*2, j-1) = 1
                        beamMatrix(i*2, j+1) = 1
                        countSplits = countSplits + 1
                    END IF
                ELSE IF (beamMatrix(i*2-2, j) .EQ. 1) THEN
                    beamMatrix(i*2, j) = 1
                END IF
            END DO

            splitMinPosition = splitMinPosition - 1
            splitMaxPosition = splitMaxPosition + 1

        END DO

        PRINT *, "Day 7 part 1 =", countSplits
    END BLOCK

    BLOCK ! part 2
        INTEGER(8), ALLOCATABLE :: beamMatrix(:,:)
        CHARACTER(LEN=numColumns) :: rowString
        INTEGER :: beamStartingPosition
        INTEGER :: splitMinPosition, splitMaxPosition
        INTEGER :: i, j
        INTEGER(8) :: countTimelines

        ALLOCATE(beamMatrix(numRows, numColumns))

        rowString = ''
        DO j = 1, numColumns
            rowString(j:j) = matrix(1, j)
        END DO
        beamStartingPosition = INDEX(rowString, 'S')

        beamMatrix = 0
        beamMatrix(2, beamStartingPosition) = 1

        splitMinPosition = beamStartingPosition
        splitMaxPosition = beamStartingPosition

        DO i = 2, numRows/2

            DO j = splitMinPosition, splitMaxPosition
                IF (matrix(i*2-1, j) .EQ. '^') THEN
                    IF (beamMatrix(i*2-2, j) .NE. 0) THEN
                        beamMatrix(i*2, j-1) = beamMatrix(i*2, j-1) + beamMatrix(i*2-2, j)
                        beamMatrix(i*2, j+1) = beamMatrix(i*2, j+1) + beamMatrix(i*2-2, j)
                    END IF
                ELSE IF (beamMatrix(i*2-2, j) .NE. 0) THEN
                    beamMatrix(i*2, j) = beamMatrix(i*2, j) + beamMatrix(i*2-2, j)
                END IF
            END DO

            splitMinPosition = splitMinPosition - 1
            splitMaxPosition = splitMaxPosition + 1

        END DO

        countTimelines = 0
        DO j = 1, numColumns
            countTimelines = countTimelines + beamMatrix(numRows, j)
        END DO

        PRINT *, "Day 7 part 2 =", countTimelines
    END BLOCK


END PROGRAM main

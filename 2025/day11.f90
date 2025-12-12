PROGRAM main
    USE io_utils
    USE string_utils
    USE number_utils
    IMPLICIT NONE

    CHARACTER(LEN=100), ALLOCATABLE :: lines(:)
    INTEGER :: n, i, j, k

    CALL ReadLines("input/day11.txt", lines, n, verbose=.TRUE.)

    BLOCK ! part 1
        CHARACTER(LEN=100) :: line
        CHARACTER(LEN=3) :: deviceNames(n+1)
        LOGICAL :: connections(n+1, n+1)
        INTEGER :: youIndex, outIndex
        CHARACTER(:), ALLOCATABLE :: connectedDevices(:)
        INTEGER :: numConnectedDevices
        INTEGER :: countRoutes

        DO i = 1, n
            line = lines(i)
            deviceNames(i) = line(1:3)
            connections(i, :) = .FALSE.

            IF (deviceNames(i) .EQ. "you") THEN
                youIndex = i
            END IF
        END DO

        deviceNames(n+1) = "out"
        connections(n+1, :) = .FALSE.
        outIndex = n + 1

        DO i = 1, n
            line = lines(i)
            CALL SplitString(line(6:), ' ', connectedDevices)
            numConnectedDevices = SIZE(connectedDevices)

            DO j = 1, numConnectedDevices
                k = getDeviceIndex(connectedDevices(j)(1:3), deviceNames)
                connections(i, k) = .TRUE.
            END DO

            DEALLOCATE(connectedDevices)
        END DO

        ! PRINT *, "Device Names:", deviceNames
        ! PRINT *, "Connections Matrix:"
        ! DO i = 1, n + 1
        !     PRINT *, connections(i, :)
        ! END DO

        countRoutes = countRoutesDFS(youIndex, outIndex, connections)

        PRINT *, "Day 11 part 1 =", countRoutes
    END BLOCK

    BLOCK ! part 2
        CHARACTER(LEN=100) :: line
        CHARACTER(LEN=3) :: deviceNames(n+1)
        LOGICAL :: connections(n+1, n+1)
        INTEGER :: svrIndex, outIndex, dacIndex, fftIndex
        CHARACTER(:), ALLOCATABLE :: connectedDevices(:)
        INTEGER :: numConnectedDevices
        INTEGER(8) :: countRoutes
        INTEGER(8) :: countMemo(n+1, 0:1, 0:1)
        LOGICAL :: countMemoReady(n+1, 0:1, 0:1)

        DO i = 1, n
            line = lines(i)
            deviceNames(i) = line(1:3)
            connections(i, :) = .FALSE.

            IF (deviceNames(i) .EQ. "svr") THEN
                svrIndex = i
            ELSE IF (deviceNames(i) .EQ. "dac") THEN
                dacIndex = i
            ELSE IF (deviceNames(i) .EQ. "fft") THEN
                fftIndex = i
            END IF
        END DO

        deviceNames(n+1) = "out"
        connections(n+1, :) = .FALSE.
        outIndex = n + 1

        DO i = 1, n
            line = lines(i)
            CALL SplitString(line(6:), ' ', connectedDevices)
            numConnectedDevices = SIZE(connectedDevices)

            DO j = 1, numConnectedDevices
                k = getDeviceIndex(connectedDevices(j)(1:3), deviceNames)
                connections(i, k) = .TRUE.
            END DO

            DEALLOCATE(connectedDevices)
        END DO

        countMemo = 0
        countMemoReady = .FALSE.

        countRoutes = countRoutesWithNodes(svrIndex, outIndex, connections, &
                0, 0, dacIndex, fftIndex, countMemo, countMemoReady)

        PRINT *, "Day 11 part 2 =", countRoutes
    END BLOCK

    CONTAINS

    INTEGER FUNCTION getDeviceIndex(deviceName, deviceNames) RESULT(index)
        CHARACTER(LEN=3), INTENT(IN) :: deviceName
        CHARACTER(LEN=3), INTENT(IN) :: deviceNames(n+1)

        IF (deviceName .EQ. "out") THEN
            index = n + 1
            RETURN
        END IF

        DO index = 1, n
            IF (deviceNames(index) .EQ. deviceName) THEN
                RETURN
            END IF
        END DO

        PRINT *, "Device name not found", deviceName
        STOP
    END FUNCTION getDeviceIndex

    RECURSIVE FUNCTION countRoutesDFS(current, target, connections) RESULT(count)
        INTEGER, INTENT(IN) :: current, target
        LOGICAL, INTENT(IN) :: connections(n+1, n+1)
        INTEGER :: nextNode
        INTEGER :: count

        IF (current == target) THEN
            count = 1
            RETURN
        END IF

        count = 0

        DO nextNode = 1, n + 1
            IF (connections(current, nextNode)) THEN
                count = count + countRoutesDFS(nextNode, target, connections)
            END IF
        END DO

    END FUNCTION countRoutesDFS

    RECURSIVE FUNCTION countRoutesWithNodes(current, target, connections, visitedDAC, visitedFFT, dacIndex, fftIndex, countMemo, countMemoReady) RESULT(count)
        INTEGER, INTENT(IN) :: current, target
        LOGICAL, INTENT(IN) :: connections(n+1, n+1)
        INTEGER, INTENT(IN) :: visitedDAC, visitedFFT
        INTEGER, INTENT(IN) :: dacIndex, fftIndex
        INTEGER(8), INTENT(INOUT) :: countMemo(n+1,0:1,0:1)
        LOGICAL, INTENT(INOUT) :: countMemoReady(n+1,0:1,0:1)
        INTEGER :: nextNode
        INTEGER(8) :: count
        INTEGER :: nextDAC, nextFFT

        IF (countMemoReady(current, visitedDAC, visitedFFT)) THEN
            count = countMemo(current, visitedDAC, visitedFFT)
            RETURN
        END IF

        IF (current == target) THEN
            IF (visitedDAC .EQ. 1 .AND. visitedFFT .EQ. 1) THEN
                count = 1
            ELSE
                count = 0
            END IF
            countMemoReady(current, visitedDAC, visitedFFT) = .TRUE.
            countMemo(current, visitedDAC, visitedFFT) = count
            RETURN
        END IF

        count = 0
        DO nextNode = 1, n + 1
            IF (connections(current, nextNode)) THEN
                nextDAC = visitedDAC
                nextFFT = visitedFFT

                IF (nextNode == dacIndex) THEN
                    nextDAC =  MERGE(1, 0, .TRUE.)
                ELSE IF (nextNode == fftIndex) THEN
                    nextFFT = MERGE(1, 0, .TRUE.)
                END IF

                count = count + countRoutesWithNodes(nextNode, target, connections, &
                            nextDAC, nextFFT, dacIndex, fftIndex, countMemo, countMemoReady)
            END IF
        END DO

        countMemoReady(current, visitedDAC, visitedFFT) = .TRUE.
        countMemo(current, visitedDAC, visitedFFT) = count
    END FUNCTION countRoutesWithNodes

END PROGRAM main

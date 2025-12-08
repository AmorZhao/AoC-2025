PROGRAM main
    USE io_utils
    USE string_utils
    USE number_utils
    IMPLICIT NONE

    TYPE :: SpaceLocation
        INTEGER :: X
        INTEGER :: Y
        INTEGER :: Z
    END TYPE SpaceLocation

    CHARACTER(LEN=200), ALLOCATABLE :: lines(:)
    INTEGER :: n, i, j, k

    CALL ReadLines("input/day8.txt", lines, n, verbose=.TRUE.)

    BLOCK ! part 1
        TYPE(SpaceLocation) :: junctions(n)
        CHARACTER(:), ALLOCATABLE :: parts(:)
        CHARACTER(LEN=200) :: line
        INTEGER :: numPossibleConnections
        INTEGER :: numToConnect
        INTEGER :: numConnectionsMade
        TYPE(CONNECTION), ALLOCATABLE :: connections(:)

        INTEGER :: parent(n), rank(n)
        INTEGER :: a, b

        INTEGER :: countCircuits(n)
        INTEGER :: roots(n)
        INTEGER(8) :: result

        numToConnect = 1000   ! change to 1000 for test input

        DO i = 1, n
            line = TRIM(lines(i))
            CALL SplitString(line, ",", parts)

            junctions(i)%X = ParseInt(TRIM(parts(1)))
            junctions(i)%Y = ParseInt(TRIM(parts(2)))
            junctions(i)%Z = ParseInt(TRIM(parts(3)))
        END DO

        numPossibleConnections = n * (n - 1) / 2
        ALLOCATE(connections(numPossibleConnections))

        k = 0
        DO i = 1, n
            DO j = i + 1, n
                k = k + 1
                connections(k)%indexA = i
                connections(k)%indexB = j
                connections(k)%distance = &
                    INT(junctions(i)%X - junctions(j)%X, 8)**2 + &
                    INT(junctions(i)%Y - junctions(j)%Y, 8)**2 + &
                    INT(junctions(i)%Z - junctions(j)%Z, 8)**2
            END DO
        END DO

        CALL SortByDistance(connections)

        DO i = 1, n
            parent(i) = i
            rank(i) = 0
        END DO

        numConnectionsMade = 0
        DO i = 1, numPossibleConnections
            a = connections(i)%indexA
            b = connections(i)%indexB

            CALL makeConnection(a, b, parent, rank)
            numConnectionsMade = numConnectionsMade + 1

            IF (numConnectionsMade >= numToConnect) EXIT
        END DO

        DO i = 1, n
            roots(i) = getRoot(i, parent)
        END DO

        countCircuits = 0
        DO i = 1, n
            countCircuits(roots(i)) = countCircuits(roots(i)) + 1
        END DO

        CALL Sort(countCircuits)

        result = 1
        DO i = SIZE(countCircuits)-2, SIZE(countCircuits)
            result = result * INT(countCircuits(i), 8)
        END DO

        PRINT *, "Day 8 part 1 =", result
    END BLOCK

    BLOCK ! part 2
        TYPE(SpaceLocation) :: junctions(n)
        CHARACTER(:), ALLOCATABLE :: parts(:)
        CHARACTER(LEN=200) :: line

        INTEGER :: numPossibleConnections
        TYPE(CONNECTION), ALLOCATABLE :: connections(:)

        INTEGER :: parent(n), rank(n)
        INTEGER :: a, b, rootA, rootB
        INTEGER :: numCircuitsToConnect
        INTEGER :: lastA, lastB

        INTEGER(8) :: result

        DO i = 1, n
            line = TRIM(lines(i))
            CALL SplitString(line, ",", parts)

            junctions(i)%X = ParseInt(parts(1))
            junctions(i)%Y = ParseInt(parts(2))
            junctions(i)%Z = ParseInt(parts(3))
        END DO

        numPossibleConnections = n*(n-1)/2
        ALLOCATE(connections(numPossibleConnections))

        j = 0
        DO i = 1, n
            DO b = i+1, n
                j = j + 1
                connections(j)%indexA = i
                connections(j)%indexB = b

                connections(j)%distance = &
                    INT(junctions(i)%X - junctions(b)%X,8)**2 + &
                    INT(junctions(i)%Y - junctions(b)%Y,8)**2 + &
                    INT(junctions(i)%Z - junctions(b)%Z,8)**2
            END DO
        END DO

        CALL SortByDistance(connections)

        DO i = 1, n
            parent(i) = i
            rank(i) = 0
        END DO

        numCircuitsToConnect = n
        DO i = 1, SIZE(connections)
            a = connections(i)%indexA
            b = connections(i)%indexB

            rootA = getRoot(a, parent)
            rootB = getRoot(b, parent)

            IF (rootA /= rootB) THEN
                CALL makeConnection(rootA, rootB, parent, rank)
                numCircuitsToConnect = numCircuitsToConnect - 1

                lastA = a
                lastB = b

                IF (numCircuitsToConnect == 1) EXIT
            END IF
        END DO

        result = INT(junctions(lastA)%X,8) * INT(junctions(lastB)%X,8)

        PRINT *, "Day 8 part 2 =", result
    END BLOCK

    CONTAINS

    RECURSIVE FUNCTION getRoot(x, parent) RESULT(root)
        INTEGER, INTENT(IN) :: x
        INTEGER, INTENT(INOUT) :: parent(:)
        INTEGER :: root

        IF (parent(x) /= x) THEN
            parent(x) = getRoot(parent(x), parent)
        END IF
        root = parent(x)
    END FUNCTION getRoot

    SUBROUTINE makeConnection(a, b, parent, rank)
        INTEGER, INTENT(INOUT) :: parent(:), rank(:)
        INTEGER, INTENT(IN) :: a, b
        INTEGER :: rootA, rootB

        rootA = getRoot(a, parent)
        rootB = getRoot(b, parent)

        IF (rootA == rootB) RETURN

        IF (rank(rootA) < rank(rootB)) THEN
            parent(rootA) = rootB
        ELSE IF (rank(rootA) > rank(rootB)) THEN
            parent(rootB) = rootA
        ELSE
            parent(rootB) = rootA
            rank(rootA) = rank(rootA) + 1
        END IF
    END SUBROUTINE makeConnection

END PROGRAM main

PROGRAM main
    USE io_utils
    USE string_utils
    USE number_utils
    IMPLICIT NONE

    TYPE :: Location
        INTEGER :: X
        INTEGER :: Y
    END TYPE Location

    CHARACTER(LEN=20), ALLOCATABLE :: lines(:)
    INTEGER :: n, i, j

    CALL ReadLines("input/day9.txt", lines, n, verbose=.TRUE.)

    BLOCK ! part 1
        CHARACTER(LEN=20) :: line
        INTEGER :: commaPosition
        TYPE(LOCATION) :: redLocations(n)
        INTEGER(8) :: tmpArea
        INTEGER(8) :: maxArea

        DO i = 1, n
            line = TRIM(lines(i))
            commaPosition = INDEX(line, ',')

            redLocations(i)%X = ParseInt(line(1:commaPosition-1))
            redLocations(i)%Y = ParseInt(line(commaPosition+1:))
        END DO

        maxArea = 0
        DO i = 1, n
            DO j = i+1, n
                tmpArea = INT(ABS(redLocations(i)%X - redLocations(j)%X)+1, 8) * &
                          INT(ABS(redLocations(i)%Y - redLocations(j)%Y)+1, 8)

                IF (tmpArea > maxArea) THEN
                    maxArea = tmpArea
                END IF
            END DO
        END DO

        PRINT *, "Day 9 part 1 =", maxArea
    END BLOCK

    BLOCK ! part 2
        CHARACTER(LEN=20) :: line
        INTEGER :: commaPosition
        TYPE(LOCATION) :: redLocations(n)
        INTEGER, ALLOCATABLE :: px(:), py(:)
        INTEGER, ALLOCATABLE :: xlist(:), ylist(:)

        INTEGER :: k, m
        INTEGER :: xn, yn

        INTEGER, ALLOCATABLE :: edgeGrid(:,:)
        INTEGER, ALLOCATABLE :: realGrid(:,:)
        INTEGER, ALLOCATABLE :: prefix(:,:)

        INTEGER :: x1, x2, y1, y2
        INTEGER :: xr1, xr2, yr1, yr2
        INTEGER(8) :: tmpArea, maxArea
        INTEGER(8) :: numGood

        ALLOCATE(px(n), py(n))

        ALLOCATE(xlist(2*n))
        ALLOCATE(ylist(2*n))

        DO i = 1, n
            line = TRIM(lines(i))
            commaPosition = INDEX(line, ',')
            redLocations(i)%X = ParseInt(line(1:commaPosition-1))
            redLocations(i)%Y = ParseInt(line(commaPosition+1:))

            px(i) = redLocations(i)%X
            py(i) = redLocations(i)%Y

            xlist(2*i-1) = px(i)
            xlist(2*i) = px(i) + 1

            ylist(2*i-1) = py(i)
            ylist(2*i) = py(i) + 1
        END DO

        CALL UniqueSort(xlist)
        CALL UniqueSort(ylist)

        xn = SIZE(xlist)
        yn = SIZE(ylist)

        ALLOCATE(edgeGrid(xn, yn))
        edgeGrid = 0

        DO i = 1, n
            x1 = GetIndex(xlist, redLocations(i)%X)
            y1 = GetIndex(ylist, redLocations(i)%Y)

            x2 = GetIndex(xlist, redLocations(MOD(i, n) + 1)%X)
            y2 = GetIndex(ylist, redLocations(MOD(i, n) + 1)%Y)

            IF (x1 /= x2) THEN
                IF (x1 > x2) THEN
                    k = x1; x1 = x2; x2 = k
                END IF

                edgeGrid(x1, y1) = edgeGrid(x1, y1) + 1
                edgeGrid(x2, y1) = edgeGrid(x2, y1) + 2

                DO k = INT(x1)+1, INT(x2)-1
                    edgeGrid(k, y1) = edgeGrid(k, y1) + 3
                END DO
            END IF
        END DO

        ALLOCATE(realGrid(xn, yn))
        realGrid = MERGE(1, 0, .FALSE.)

        DO i = 1, xn
            m = 0
            DO j = 1, yn
                IF (m > 0 .OR. edgeGrid(i,j) > 0) THEN
                    realGrid(i,j) = MERGE(1, 0, .TRUE.)
                END IF

                m = IEOR(m, edgeGrid(i,j))
            END DO
        END DO

        ALLOCATE(prefix(xn+1, yn+1))
        prefix = 0

        DO i = 1, xn
            DO j = 1, yn
                prefix(i+1,j+1) = realGrid(i,j) + prefix(i,j+1) + prefix(i+1,j) - prefix(i,j)
            END DO
        END DO

        maxArea = 0
        DO i = 1, n
            DO j = 1, i-1
                x1 = MIN(px(i), px(j))
                x2 = MAX(px(i), px(j))
                y1 = MIN(py(i), py(j))
                y2 = MAX(py(i), py(j))

                xr1 = GetIndex(xlist, x1)
                xr2 = GetIndex(xlist, x2)
                yr1 = GetIndex(ylist, y1)
                yr2 = GetIndex(ylist, y2)

                numGood = prefix(xr2+1, yr2+1) - prefix(xr2+1, yr1) - prefix(xr1, yr2+1) + prefix(xr1, yr1)

                IF (numGood == INT((xr2 - xr1 + 1) * (yr2 - yr1 + 1), 8)) THEN
                    tmpArea = (x2 - x1 + 1) * (y2 - y1 + 1)

                    IF (tmpArea > maxArea) THEN
                        maxArea = tmpArea
                    END IF
                END IF

            END DO
        END DO

        PRINT *, "Day 9 part 2 =", maxArea
    END BLOCK

END PROGRAM main

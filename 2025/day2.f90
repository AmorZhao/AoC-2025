PROGRAM main
    USE io_utils
    USE number_utils
    USE string_utils
    USE array_utils
    IMPLICIT NONE

    CHARACTER(:), ALLOCATABLE :: line
    CHARACTER(:), ALLOCATABLE :: ranges(:)
    INTEGER :: i

    CALL ReadSingleLine("input/day2.txt", line, verbose=.TRUE.)

    CALL SplitString(line, ',', ranges)

    PRINT *, "Number of ranges:", SIZE(ranges)
    ! DO i = 1, SIZE(ranges)
    !     PRINT *, TRIM(ranges(i))
    ! END DO

    BLOCK ! part 1
        INTEGER(8) :: leftBound, rightBound
        INTEGER :: dashPosition
        INTEGER(8) :: totalInvalid
        INTEGER :: leftLength, rightLength
        INTEGER(8) :: halfValue, repeatedValue
        INTEGER(8) :: powerOfTen
        CHARACTER(32) :: leftString, rightString, tmpString
        INTEGER(8) :: halfMin, halfMax

        totalInvalid = 0

        DO i = 1, SIZE(ranges)
            dashPosition  = INDEX(ranges(i), '-')
            leftBound     = ParseInt_I8(TRIM(ranges(i)(1:dashPosition-1)))
            rightBound    = ParseInt_I8(TRIM(ranges(i)(dashPosition+1:)))

            WRITE(leftString, '(I0)') leftBound
            WRITE(rightString, '(I0)') rightBound

            leftLength = LEN_TRIM(leftString)
            rightLength = LEN_TRIM(rightString)

            IF (MOD(leftLength, 2) == 0) THEN
                halfMin = ParseInt(leftString(1:leftLength / 2))
                IF (halfMin < ParseInt(leftString((leftLength + 1) / 2 + 1:leftLength))) THEN
                    halfMin = halfMin + 1
                END IF
            ELSE IF (MOD(rightLength, 2) == 0) THEN
                halfMin = 10**((leftLength + 1) / 2 - 1)
            ELSE
                CYCLE
            END IF

            IF (MOD(rightLength, 2) == 0) THEN
                halfMax = ParseInt(rightString(1:rightLength / 2))
            ELSE
                halfMax = 10**((rightLength - 1) / 2) - 1
            END IF

            DO halfValue = halfMin, halfMax
                WRITE(tmpString, '(I0)') halfValue
                powerOfTen = 10**(LEN_TRIM(tmpString))

                repeatedValue = halfValue * powerOfTen + halfValue

                IF (repeatedValue > rightBound) EXIT

                totalInvalid = totalInvalid + repeatedValue
            END DO

        END DO

        PRINT *, "Day 2 part 1 =", totalInvalid

    END BLOCK

    BLOCK ! part 2 non brute-force
        INTEGER(8) :: leftBound, rightBound
        INTEGER :: dashPosition
        INTEGER(8) :: totalInvalid
        INTEGER :: leftLength, rightLength, maxLength
        CHARACTER(32) :: leftString, rightString, tmpString
        INTEGER :: digitCount, patternLength, repeatCount, k
        INTEGER(8) :: patternValue, fullValue
        INTEGER(8) :: minPattern, maxPattern

        INTEGER(8), ALLOCATABLE :: found(:)
        INTEGER :: foundCount
        LOGICAL :: isDuplicate

        foundCount = 0
        ALLOCATE(found(0))

        totalInvalid = 0

        DO i = 1, SIZE(ranges)
            dashPosition = INDEX(ranges(i), '-')
            leftBound  = ParseInt_I8(TRIM(ranges(i)(1:dashPosition-1)))
            rightBound = ParseInt_I8(TRIM(ranges(i)(dashPosition+1:)))

            WRITE(leftString,  '(I0)') leftBound
            WRITE(rightString, '(I0)') rightBound
            leftString  = TRIM(leftString)
            rightString = TRIM(rightString)

            leftLength  = LEN_TRIM(leftString)
            rightLength = LEN_TRIM(rightString)

            IF (MOD(rightLength, 2) == 0) THEN
                maxLength = rightLength / 2
            ELSE
                maxLength = (rightLength - 1) / 2
            END IF

            DO digitCount = leftLength, rightLength

                DO patternLength = 1, maxLength

                    IF (MOD(digitCount, patternLength) /= 0) CYCLE

                    repeatCount = digitCount / patternLength
                    IF (repeatCount < 2) CYCLE

                    IF (digitCount == leftLength) THEN
                        minPattern = ParseInt(leftString(1:patternLength))
                    ELSE
                        minPattern = 10**(patternLength - 1)
                    END IF

                    IF (digitCount == rightLength) THEN
                        maxPattern = ParseInt(rightString(1:patternLength))
                    ELSE
                        maxPattern = 10**patternLength - 1
                    END IF

                    IF (minPattern > maxPattern) CYCLE

                    DO patternValue = minPattern, maxPattern

                        fullValue = 0

                        DO k = 1, repeatCount
                            WRITE(tmpString, '(I0)') patternValue
                            fullValue = fullValue * (10**patternLength) + patternValue
                        END DO

                        IF (fullValue < leftBound) CYCLE
                        IF (fullValue > rightBound) EXIT

                        isDuplicate = .FALSE.
                        DO k = 1, foundCount
                            IF (found(k) == fullValue) THEN
                                isDuplicate = .TRUE.
                                EXIT
                            END IF
                        END DO

                        IF (.NOT. isDuplicate) THEN
                            foundCount = foundCount + 1
                            CALL Append(found, fullValue)
                            totalInvalid = totalInvalid + fullValue
                        END IF

                    END DO

                END DO
            END DO

        END DO

        PRINT *, "Day 2 part 2 =", totalInvalid

    END BLOCK

    BLOCK ! part 2 brute-force
        INTEGER(8) :: leftBound, rightBound
        INTEGER :: dashPosition
        INTEGER(8) :: totalInvalid
        INTEGER(8) :: candidate

        totalInvalid = 0

        DO i = 1, SIZE(ranges)
            dashPosition  = INDEX(ranges(i), '-')
            leftBound     = ParseInt_I8(TRIM(ranges(i)(1:dashPosition-1)))
            rightBound    = ParseInt_I8(TRIM(ranges(i)(dashPosition+1:)))

            DO candidate = leftBound, rightBound
                IF (IsRepeatedPattern(candidate)) THEN
                    totalInvalid = totalInvalid + candidate
                END IF
            END DO

        END DO

        PRINT *, "Day 2 part 1 =", totalInvalid

    END BLOCK

    CONTAINS

    LOGICAL FUNCTION IsRepeatedPattern(number)
        INTEGER(8), INTENT(IN) :: number
        CHARACTER(:), ALLOCATABLE :: fullString
        CHARACTER(:), ALLOCATABLE :: basePattern
        CHARACTER(:), ALLOCATABLE :: repeatedString
        INTEGER :: totalLength, patternLength, repetitions, index
        CHARACTER(32) :: tmp

        WRITE(tmp, '(I0)') number
        fullString = TRIM(tmp)
        totalLength = LEN(fullString)

        DO patternLength = 1, totalLength / 2
            IF (MOD(totalLength, patternLength) /= 0) CYCLE

            basePattern = fullString(1:patternLength)
            repetitions = totalLength / patternLength

            repeatedString = ""
            DO index = 1, repetitions
                repeatedString = repeatedString // basePattern
            END DO

            IF (repeatedString == fullString) THEN
                IsRepeatedPattern = .TRUE.
                RETURN
            END IF
        END DO

        IsRepeatedPattern = .FALSE.
    END FUNCTION IsRepeatedPattern

END PROGRAM main

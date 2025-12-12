PROGRAM main
    USE io_utils
    USE string_utils
    USE number_utils
    IMPLICIT NONE

    CHARACTER(LEN=200), ALLOCATABLE :: lines(:)
    INTEGER :: n, i, j
    CHARACTER(LEN=200) :: line
    INTRINSIC :: IEOR

    CALL ReadLines("input/day10.txt", lines, n, verbose=.TRUE.)

    BLOCK ! Part 1
        INTEGER :: lineLength
        INTEGER :: numLights, numButtons
        CHARACTER, ALLOCATABLE :: lightPattern(:)
        CHARACTER(LEN=:), ALLOCATABLE :: buttonPatterns(:)
        CHARACTER(LEN=:), ALLOCATABLE :: tmpPattern
        INTEGER :: patternIndex
        INTEGER :: buttonIndex
        INTEGER :: countPress

        INTEGER :: subset, b
        INTEGER :: best
        INTEGER :: targetMask, buttonMask(100)
        INTEGER :: p
        INTEGER :: pressCount
        INTEGER :: accMask

        countPress = 0

        DO i = 1, n
            line = TRIM(lines(i))
            lineLength = LEN_TRIM(line)

            numLights = 0
            DO j = 2, 12
                IF (line(j:j) == ']') EXIT
                numLights = numLights + 1
            END DO

            IF (ALLOCATED(lightPattern)) DEALLOCATE(lightPattern)
            ALLOCATE(lightPattern(numLights))
            lightPattern = ' '

            DO j = 2, numLights + 1
                IF(line(j:j) == '.') THEN
                    lightPattern(j-1) = '0'
                ELSE    ! IF (line(j:j) == '#')
                    lightPattern(j-1) = '1'
                END IF
            END DO

            ! PRINT *, "Line ", i, ": Light pattern = ", lightPattern

            numButtons = 0
            DO j = numLights + 3, lineLength
                IF (line(j:j) == '{') EXIT
                IF (line(j:j) == '(') THEN
                    numButtons = numButtons + 1
                END IF
            END DO

            IF (ALLOCATED(buttonPatterns)) DEALLOCATE(buttonPatterns)
            ALLOCATE(CHARACTER(LEN=numLights) :: buttonPatterns(numButtons))
            buttonPatterns = ''

            buttonIndex = 0
            DO j = numLights + 3, lineLength
                IF (line(j:j) == '{') EXIT
                IF (line(j:j) == ' ') CYCLE

                IF (line(j:j) == '(') THEN
                    buttonIndex = buttonIndex + 1
                    tmpPattern = REPEAT('0', numLights)
                ELSE IF (line(j:j) == ')') THEN
                    buttonPatterns(buttonIndex) = tmpPattern
                ELSE
                    patternIndex = ParseInt(line(j:j))
                    tmpPattern(patternIndex+1:patternIndex+1) = '1'
                END IF
            END DO

            ! DO j = 1, numButtons
            !     PRINT *, "  Button ", j, " pattern = ", buttonPatterns(j)
            ! END DO

            targetMask = 0
            do p = 1, numLights
                if (lightPattern(p) == '1') targetMask = IEOR(targetMask, SHIFTL(1, p-1))
            end do

            do b = 1, numButtons
                buttonMask(b) = 0
                do p = 1, numLights
                    if (buttonPatterns(b)(p:p) == '1') then
                        buttonMask(b) = IEOR(buttonMask(b), SHIFTL(1, p-1))
                    end if
                end do
            end do

            best = HUGE(1)

            do subset = 0, 2**numButtons - 1
                accMask = 0

                do b = 1, numButtons
                    if (BTEST(subset, b-1)) then
                        accMask = IEOR(accMask, buttonMask(b))
                    end if
                end do

                if (accMask == targetMask) then
                    pressCount = POPCNT(subset)
                    if (pressCount < best) best = pressCount
                end if
            end do

            countPress = countPress + best
        END DO

        PRINT *, "Day 10 part 1 =", countPress
    END BLOCK

END PROGRAM main

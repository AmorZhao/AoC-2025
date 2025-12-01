      PROGRAM MAIN
      INTEGER MAXLINES
      PARAMETER (MAXLINES = 5000)
      CHARACTER*10 LINES(MAXLINES)
      CHARACTER*10 LINE
      CHARACTER*1 DIRECTION
      INTEGER N, I
      INTEGER CURRENTPOSITION, CLICKS
      INTEGER COUNT0, TOTAL0, CLICKSTOZERO

      N = 0
      OPEN(10, FILE='../2025/input/day1.txt', STATUS='OLD')
100   READ(10,'(A)', END=200) LINE
      N = N + 1
      LINES(N) = LINE
      GOTO 100
200   CLOSE(10)
      PRINT *, 'Total lines read =', N

      CURRENTPOSITION = 50
      COUNT0 = 0

      DO 300 I = 1, N
         LINE = LINES(I)
         DIRECTION = LINE(1:1)
         READ(LINE(2:10), *) CLICKS

         IF (DIRECTION .EQ. 'L') THEN
            CURRENTPOSITION = MOD(CURRENTPOSITION - CLICKS, 100)
         ELSE
            CURRENTPOSITION = MOD(CURRENTPOSITION + CLICKS, 100)
         ENDIF

         IF (CURRENTPOSITION .EQ. 0) COUNT0 = COUNT0 + 1
300   CONTINUE
      PRINT *, 'Day 1 part 1 =', COUNT0

      CURRENTPOSITION = 50
      TOTAL0 = 0

      DO 400 I = 1, N
         LINE = LINES(I)
         DIRECTION = LINE(1:1)
         READ(LINE(2:10), *) CLICKS

         IF (DIRECTION .EQ. 'L') THEN
            CLICKSTOZERO = CURRENTPOSITION
            CURRENTPOSITION = MOD(CURRENTPOSITION - CLICKS, 100)
         ELSE
            CLICKSTOZERO = 100 - CURRENTPOSITION
            CURRENTPOSITION = MOD(CURRENTPOSITION + CLICKS, 100)
         ENDIF

C        MOD can return negative values in F77
         IF (CURRENTPOSITION .LT. 0) THEN
            CURRENTPOSITION = CURRENTPOSITION + 100
         ENDIF

         IF (CLICKSTOZERO .EQ. 0) CLICKSTOZERO = 100

         IF (CLICKS .LT. CLICKSTOZERO) THEN
            COUNT0 = 0
         ELSE
            COUNT0 = 1 + (CLICKS - CLICKSTOZERO) / 100
         ENDIF

         TOTAL0 = TOTAL0 + COUNT0
400   CONTINUE
      PRINT *, 'Day 1 part 2 =', TOTAL0
      END

C     - F77
C        MOD(-7, 3) = -1
C        (sign and type dependent on the first argument)
C     - F95
C        MODULO(-7, 3) = 2
C        (sign and type dependent on the second argument)


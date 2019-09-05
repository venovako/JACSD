    INFO = 0
    IF (N .LT. 0) INFO = -2
    SELECT CASE (SIDE)
    CASE ('R','r')
       RIGHT = .TRUE.
    CASE ('L','l')
       RIGHT = .FALSE.
    CASE DEFAULT
       INFO = -1
    END SELECT
    IF ((INFO .NE. 0) .OR. (N .EQ. 0)) RETURN

    IF (C1 .EQ. Q_ONE) THEN
       IF (C2 .EQ. Q_ONE) THEN
          INFO = 1
       ELSE IF (C2 .EQ. Q_ZERO) THEN
          INFO = 2
       ELSE IF (C2 .EQ. Q_MONE) THEN
          INFO = 3
       END IF
    ELSE IF (C1 .EQ. Q_ZERO) THEN
       IF (C2 .EQ. Q_ONE) THEN
          INFO = 4
       ELSE IF (C2 .EQ. Q_ZERO) THEN
          INFO = 5
       ELSE IF (C2 .EQ. Q_MONE) THEN
          INFO = 6
       END IF
    ELSE IF (C1 .EQ. Q_MONE) THEN
       IF (C2 .EQ. Q_ONE) THEN
          INFO = 7
       ELSE IF (C2 .EQ. Q_ZERO) THEN
          INFO = 8
       ELSE IF (C2 .EQ. Q_MONE) THEN
          INFO = 9
       END IF
    END IF

    IF ((INCX .EQ. 1) .AND. (INCY .EQ. 1)) THEN
       IF (RIGHT) THEN
          SELECT CASE (INFO)
          CASE (1) ! C1 =  1, C2 =  1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = W + Z*S1
                ZY(I) = W*S2 + Z
             END DO
          CASE (2) ! C1 =  1, C2 =  0
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = W + Z*S1
                ZY(I) = W*S2
             END DO
          CASE (3) ! C1 =  1, C2 = -1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = W + Z*S1
                ZY(I) = W*S2 - Z
             END DO
          CASE (4) ! C1 =  0, C2 =  1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S1
                ZY(I) = W*S2 + Z
             END DO
          CASE (5) ! C1 =  0, C2 =  0
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S1
                ZY(I) = W*S2
             END DO
          CASE (6) ! C1 =  0, C2 = -1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S1
                ZY(I) = W*S2 - Z
             END DO
          CASE (7) ! C1 = -1, C2 =  1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S1 - W
                ZY(I) = W*S2 + Z
             END DO
          CASE (8) ! C1 = -1, C2 =  0
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S1 - W
                ZY(I) = W*S2
             END DO
          CASE (9) ! C1 = -1, C2 = -1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S1 - W
                ZY(I) = W*S2 - Z
             END DO
          CASE DEFAULT
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = W*C1 + Z*S1
                ZY(I) = W*S2 + Z*C2
             END DO
          END SELECT
       ELSE ! left
          SELECT CASE (INFO)
          CASE (1) ! C1 =  1, C2 =  1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = W + Z*S2
                ZY(I) = W*S1 + Z
             END DO
          CASE (2) ! C1 =  1, C2 =  0
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = W + Z*S2
                ZY(I) = W*S1
             END DO
          CASE (3) ! C1 =  1, C2 = -1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = W + Z*S2
                ZY(I) = W*S1 - Z
             END DO
          CASE (4) ! C1 =  0, C2 =  1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S2
                ZY(I) = W*S1 + Z
             END DO
          CASE (5) ! C1 =  0, C2 =  0
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S2
                ZY(I) = W*S1
             END DO
          CASE (6) ! C1 =  0, C2 = -1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S2
                ZY(I) = W*S1 - Z
             END DO
          CASE (7) ! C1 = -1, C2 =  1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S2 - W
                ZY(I) = W*S1 + Z
             END DO
          CASE (8) ! C1 = -1, C2 =  0
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S2 - W
                ZY(I) = W*S1
             END DO
          CASE (9) ! C1 = -1, C2 = -1
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = Z*S2 - W
                ZY(I) = W*S1 - Z
             END DO
          CASE DEFAULT
             DO I = 1, N
                W = ZX(I)
                Z = ZY(I)
                ZX(I) = W*C1 + Z*S2
                ZY(I) = W*S1 + Z*C2
             END DO
          END SELECT
       END IF
    ELSE IF (INCX .EQ. INCY) THEN
       I = 1 - N
       IF (INCX .LT. 0) THEN
          KX = 1 + I*INCX
       ELSE
          KX = 1
       END IF
       IF (RIGHT) THEN
          SELECT CASE (INFO)
          CASE (1) ! C1 =  1, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = W + Z*S1
                ZY(KX) = W*S2 + Z
                KX = KX + INCX
             END DO
          CASE (2) ! C1 =  1, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = W + Z*S1
                ZY(KX) = W*S2
                KX = KX + INCX
             END DO
          CASE (3) ! C1 =  1, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = W + Z*S1
                ZY(KX) = W*S2 - Z
                KX = KX + INCX
             END DO
          CASE (4) ! C1 =  0, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S1
                ZY(KX) = W*S2 + Z
                KX = KX + INCX
             END DO
          CASE (5) ! C1 =  0, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S1
                ZY(KX) = W*S2
                KX = KX + INCX
             END DO
          CASE (6) ! C1 =  0, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S1
                ZY(KX) = W*S2 - Z
                KX = KX + INCX
             END DO
          CASE (7) ! C1 = -1, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S1 - W
                ZY(KX) = W*S2 + Z
                KX = KX + INCX
             END DO
          CASE (8) ! C1 = -1, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S1 - W
                ZY(KX) = W*S2
                KX = KX + INCX
             END DO
          CASE (9) ! C1 = -1, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S1 - W
                ZY(KX) = W*S2 - Z
                KX = KX + INCX
             END DO
          CASE DEFAULT
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = W*C1 + Z*S1
                ZY(KX) = W*S2 + Z*C2
                KX = KX + INCX
             END DO
          END SELECT
       ELSE ! left
          SELECT CASE (INFO)
          CASE (1) ! C1 =  1, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = W + Z*S2
                ZY(KX) = W*S1 + Z
                KX = KX + INCX
             END DO
          CASE (2) ! C1 =  1, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = W + Z*S2
                ZY(KX) = W*S1
                KX = KX + INCX
             END DO
          CASE (3) ! C1 =  1, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = W + Z*S2
                ZY(KX) = W*S1 - Z
                KX = KX + INCX
             END DO
          CASE (4) ! C1 =  0, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S2
                ZY(KX) = W*S1 + Z
                KX = KX + INCX
             END DO
          CASE (5) ! C1 =  0, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S2
                ZY(KX) = W*S1
                KX = KX + INCX
             END DO
          CASE (6) ! C1 =  0, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S2
                ZY(KX) = W*S1 - Z
                KX = KX + INCX
             END DO
          CASE (7) ! C1 = -1, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S2 - W
                ZY(KX) = W*S1 + Z
                KX = KX + INCX
             END DO
          CASE (8) ! C1 = -1, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S2 - W
                ZY(KX) = W*S1
                KX = KX + INCX
             END DO
          CASE (9) ! C1 = -1, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = Z*S2 - W
                ZY(KX) = W*S1 - Z
                KX = KX + INCX
             END DO
          CASE DEFAULT
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KX)
                ZX(KX) = W*C1 + Z*S2
                ZY(KX) = W*S1 + Z*C2
                KX = KX + INCX
             END DO
          END SELECT
       END IF
    ELSE ! general case
       I = 1 - N
       IF (INCX .LT. 0) THEN
          KX = 1 + I*INCX
       ELSE
          KX = 1
       END IF
       IF (INCY .LT. 0) THEN
          KY = 1 + I*INCY
       ELSE
          KY = 1
       END IF
       IF (RIGHT) THEN
          SELECT CASE (INFO)
          CASE (1) ! C1 =  1, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = W + Z*S1
                ZY(KY) = W*S2 + Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (2) ! C1 =  1, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = W + Z*S1
                ZY(KY) = W*S2
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (3) ! C1 =  1, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = W + Z*S1
                ZY(KY) = W*S2 - Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (4) ! C1 =  0, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S1
                ZY(KY) = W*S2 + Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (5) ! C1 =  0, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S1
                ZY(KY) = W*S2
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (6) ! C1 =  0, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S1
                ZY(KY) = W*S2 - Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (7) ! C1 = -1, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S1 - W
                ZY(KY) = W*S2 + Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (8) ! C1 = -1, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S1 - W
                ZY(KY) = W*S2
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (9) ! C1 = -1, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S1 - W
                ZY(KY) = W*S2 - Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE DEFAULT
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = W*C1 + Z*S1
                ZY(KY) = W*S2 + Z*C2
                KX = KX + INCX
                KY = KY + INCY
             END DO
          END SELECT
       ELSE ! left
          SELECT CASE (INFO)
          CASE (1) ! C1 =  1, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = W + Z*S2
                ZY(KY) = W*S1 + Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (2) ! C1 =  1, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = W + Z*S2
                ZY(KY) = W*S1
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (3) ! C1 =  1, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = W + Z*S2
                ZY(KY) = W*S1 - Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (4) ! C1 =  0, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S2
                ZY(KY) = W*S1 + Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (5) ! C1 =  0, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S2
                ZY(KY) = W*S1
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (6) ! C1 =  0, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S2
                ZY(KY) = W*S1 - Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (7) ! C1 = -1, C2 =  1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S2 - W
                ZY(KY) = W*S1 + Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (8) ! C1 = -1, C2 =  0
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S2 - W
                ZY(KY) = W*S1
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE (9) ! C1 = -1, C2 = -1
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = Z*S2 - W
                ZY(KY) = W*S1 - Z
                KX = KX + INCX
                KY = KY + INCY
             END DO
          CASE DEFAULT
             DO I = 1, N
                W = ZX(KX)
                Z = ZY(KY)
                ZX(KX) = W*C1 + Z*S2
                ZY(KY) = W*S1 + Z*C2
                KX = KX + INCX
                KY = KY + INCY
             END DO
          END SELECT
       END IF
    END IF

SUBROUTINE MYVJAC0(NPAIRS, VPAIRS, VTORS, VTORSV, M, N, G, LDG, V, LDV, MAXCYC, JSTRAT, TOL,&
     APP, AQQ, APQ, C, T, S, JPAIRS, LROT, LROT0, NROT, INFO)
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: NPAIRS, VPAIRS, VTORS, VTORSV, M, N, LDG, LDV, MAXCYC, JSTRAT
  INTEGER, INTENT(OUT) :: NROT(2), INFO(2)
  DOUBLE PRECISION, INTENT(IN) :: TOL
  DOUBLE PRECISION, INTENT(INOUT) :: G(LDG,N), V(LDV,N)
  DOUBLE PRECISION, INTENT(OUT) :: APP(VPAIRS), AQQ(VPAIRS), APQ(VPAIRS), C(VPAIRS), T(VPAIRS), S(VPAIRS)
  INTEGER, INTENT(OUT) :: LROT(VPAIRS), JPAIRS(2,NPAIRS), LROT0(VTORSV)

  INTEGER :: JS(JSMLEN), STEPS, CYC, STP, PAIR, P, Q, I, J, LROT1, LROT2
  DOUBLE PRECISION :: FASTR(5)

  DOUBLE PRECISION, EXTERNAL :: DDOT
  EXTERNAL :: DROTM, DSCAL, DSWAP

  !DIR$ ASSUME_ALIGNED G:MALIGN_B, V:MALIGN_B
  !DIR$ ASSUME_ALIGNED APP:MALIGN_B, AQQ:MALIGN_B, APQ:MALIGN_B, C:MALIGN_B, T:MALIGN_B, S:MALIGN_B
  !DIR$ ASSUME_ALIGNED JPAIRS:MALIGN_B, LROT:MALIGN_B, LROT0:MALIGN_B
  !DIR$ ASSUME (MOD(LDG,D_CL1_LEN) .EQ. 0)
  !DIR$ ASSUME (MOD(LDV,D_CL1_LEN) .EQ. 0)
  !DIR$ ASSUME (MOD(VPAIRS,D_VEC_LEN) .EQ. 0)
  !DIR$ ASSUME (MOD(VTORSV,D_VEC_LEN) .EQ. 0)

  INFO = 0
  NROT = 0

  ! STEPS = ((N / NPAIRS) * (N - 1)) / 2
  CALL JSTRAT_INIT(JS, JSTRAT, N, STEPS)
  IF (STEPS .LE. 0) THEN
     INFO(1) = 2
     INFO(2) = STEPS
     RETURN
  END IF

  !DIR$ VECTOR ALWAYS,ALIGNED
  LROT = 0

  DO CYC = 1, MAXCYC
     LROT2 = 0

     DO STP = 1, STEPS
        CALL JSTRAT_NEXT(JS, JPAIRS, INFO(2))
        IF (INFO(2) .EQ. 0) THEN
           INFO(1) = 4
           RETURN
        END IF

        DO PAIR = 1, NPAIRS
           P = IAND(JPAIRS(1,PAIR), JSMASK) + 1
           Q = IAND(JPAIRS(2,PAIR), JSMASK) + 1

           APP(PAIR) = DDOT(M, G(1,P), 1, G(1,P), 1) ! DNRM2(M, G(1,P), 1)**2
           AQQ(PAIR) = DDOT(M, G(1,Q), 1, G(1,Q), 1) ! DNRM2(M, G(1,Q), 1)**2
           APQ(PAIR) = DDOT(M, G(1,P), 1, G(1,Q), 1)
        END DO

        DO I = 1, VTORS
           PAIR = (I-1) * D_VEC_LEN
           !DIR$ VECTOR ALWAYS,ALIGNED
           DO J = 1, D_VEC_LEN
              C(PAIR+J) = SQRT(APP(PAIR+J))
              T(PAIR+J) = SQRT(AQQ(PAIR+J))
              S(PAIR+J) = ABS(APQ(PAIR+J))
              IF (S(PAIR+J) .LT. ((C(PAIR+J) * T(PAIR+J)) * TOL)) THEN
                 LROT(PAIR+J) = 0
              ELSE
                 LROT(PAIR+J) = 1
                 S(PAIR+J) = APQ(PAIR+J) * D_TWO
              END IF
           END DO
        END DO

        DO PAIR = NPAIRS + 1, VPAIRS
           LROT(PAIR) = 0
        END DO

        !DIR$ VECTOR ALWAYS,ALIGNED
        LROT0 = 0

        DO I = 1, VTORS
           PAIR = (I-1) * D_VEC_LEN
           !DIR$ VECTOR ALWAYS,ALIGNED
           LROT0(I) = SUM(LROT(PAIR+1:PAIR+D_VEC_LEN))
           IF (LROT0(I) .GT. 0) THEN
              !DIR$ VECTOR ALWAYS,ALIGNED
              DO J = 1, D_VEC_LEN
                 ! (T(PAIR+J) - C(PAIR+J)) * (T(PAIR+J) + C(PAIR+J))
                 C(PAIR+J) = AQQ(PAIR+J) - APP(PAIR+J)
                 ! Cotangent(2\phi)
                 T(PAIR+J) = C(PAIR+J) / S(PAIR+J)
                 ! Tangent(\phi)
                 !DIR$ FMA
                 T(PAIR+J) = D_ONE / (T(PAIR+J) + SIGN(SQRT(T(PAIR+J) * T(PAIR+J) + D_ONE), T(PAIR+J))) ! 1/Cotangent(\phi)
                 ! Cosine(\phi)
                 !DIR$ FMA
                 C(PAIR+J) = D_ONE / SQRT(T(PAIR+J) * T(PAIR+J) + D_ONE)
                 ! compute A_pp', A_qq' for sorting
                 !DIR$ FMA
                 APP(PAIR+J) = APP(PAIR+J) + T(PAIR+J) * APQ(PAIR+J)
                 !DIR$ FMA
                 AQQ(PAIR+J) = AQQ(PAIR+J) - T(PAIR+J) * APQ(PAIR+J)
              END DO
           END IF
        END DO

        !DIR$ VECTOR ALWAYS,ALIGNED
        LROT1 = SUM(LROT0)
        IF (LROT1 .EQ. 0) CYCLE

        DO PAIR = 1, NPAIRS
           IF (LROT(PAIR) .GT. 0) THEN
              P = IAND(JPAIRS(1,PAIR), JSMASK) + 1
              Q = IAND(JPAIRS(2,PAIR), JSMASK) + 1
              IF (T(PAIR) .EQ. D_ZERO) THEN
                 LROT(PAIR) = 0
                 IF (APP(PAIR) .LT. AQQ(PAIR)) THEN
                    CALL DSWAP(M, G(1,P), 1, G(1,Q), 1)
                    CALL DSWAP(N, V(1,P), 1, V(1,Q), 1)
                 ELSE ! no-op, should almost never happen
                    LROT1 = LROT1 - 1
                 END IF
              ELSE
                 IF (APP(PAIR) .LT. AQQ(PAIR)) THEN ! swap
                    FASTR(1) =  D_ONE
                    FASTR(2) =  T(PAIR)
                    FASTR(5) =  T(PAIR)
                    CALL DROTM(M, G(1,P), 1, G(1,Q), 1, FASTR)
                    CALL DROTM(N, V(1,P), 1, V(1,Q), 1, FASTR)
                    IF (C(PAIR) .LT. D_ONE) THEN
                       CALL DSCAL(M,  C(PAIR), G(1,P), 1)
                       CALL DSCAL(M, -C(PAIR), G(1,Q), 1)
                       CALL DSCAL(N,  C(PAIR), V(1,P), 1)
                       CALL DSCAL(N, -C(PAIR), V(1,Q), 1)
                    ELSE
                       LROT(PAIR) = 0
                       CALL DSCAL(M, D_MONE, G(1,Q), 1)
                       CALL DSCAL(N, D_MONE, V(1,Q), 1)
                    END IF
                 ELSE ! no swap
                    FASTR(1) =  D_ZERO
                    FASTR(3) =  T(PAIR)
                    FASTR(4) = -T(PAIR)
                    CALL DROTM(M, G(1,P), 1, G(1,Q), 1, FASTR)
                    CALL DROTM(N, V(1,P), 1, V(1,Q), 1, FASTR)
                    IF (C(PAIR) .LT. D_ONE) THEN
                       CALL DSCAL(M, C(PAIR), G(1,P), 1)
                       CALL DSCAL(M, C(PAIR), G(1,Q), 1)
                       CALL DSCAL(N, C(PAIR), V(1,P), 1)
                       CALL DSCAL(N, C(PAIR), V(1,Q), 1)
                    ELSE
                       LROT(PAIR) = 0
                    END IF
                 END IF
              END IF
           END IF
        END DO

        !DIR$ VECTOR ALWAYS,ALIGNED
        NROT(2) = NROT(2) + SUM(LROT)
        LROT2 = LROT2 + LROT1

        ! P = GET_IOUNIT('O')
        ! Q = STP - 1
        ! WRITE (UNIT=P,FMT='(A,I3)') 'G', Q
        ! DO I = 1, M
        !    DO J = 1, N
        !       WRITE (UNIT=P,FMT='(ES26.17E2)',ADVANCE='NO') G(I,J)
        !    END DO
        !    WRITE (UNIT=P,FMT=*)
        ! END DO
        ! WRITE (UNIT=P,FMT=*)
        ! WRITE (UNIT=P,FMT='(A,I3)') 'V', Q
        ! DO I = 1, M
        !    DO J = 1, N
        !       WRITE (UNIT=P,FMT='(ES26.17E2)',ADVANCE='NO') V(I,J)
        !    END DO
        !    WRITE (UNIT=P,FMT=*)
        ! END DO
        ! WRITE (UNIT=P,FMT=*)

     END DO

     IF (LROT2 .EQ. 0) EXIT
     NROT(1) = NROT(1) + LROT2
  END DO

  INFO(1) = 0
  INFO(2) = CYC

END SUBROUTINE MYVJAC0

SUBROUTINE VJAC0(FAST, M, N, G, LDG, V, LDV, MAXCYC, MAXTHR, JSTRAT, TOL, SIGMA, WORK, LWORK, IWORK, LIWORK, NROT, INFO)

  IMPLICIT NONE

  LOGICAL, INTENT(IN) :: FAST
  INTEGER, INTENT(IN) :: M, N, LDG, LDV, MAXCYC, MAXTHR, JSTRAT, LWORK, LIWORK
  INTEGER, INTENT(OUT) :: IWORK(LIWORK), NROT(2), INFO(2)
  DOUBLE PRECISION, INTENT(IN) :: TOL
  DOUBLE PRECISION, INTENT(INOUT) :: G(LDG,N)
  DOUBLE PRECISION, INTENT(OUT) :: V(LDV,N), SIGMA(N), WORK(LWORK)

  INTEGER :: BLASNT, NPAIRS, VPAIRS, VTORS, VTORSV, LW1, LIW1, LIW2, I, J
  DOUBLE PRECISION :: MYTOL

  DOUBLE PRECISION, EXTERNAL :: DNRM2
  EXTERNAL :: DLASET, DSCAL

  !DIR$ ASSUME_ALIGNED G:MALIGN_B,V:MALIGN_B, WORK:MALIGN_B,IWORK:MALIGN_B
  !DIR$ ASSUME (MOD(LDG,D_CL1_LEN) .EQ. 0)
  !DIR$ ASSUME (MOD(LDV,D_CL1_LEN) .EQ. 0)
  !DIR$ ASSUME (MOD(LWORK,D_CL1_LEN) .EQ. 0)
  !DIR$ ASSUME (MOD(LIWORK,I_CL1_LEN) .EQ. 0)

  IF (M .LT. 0) THEN
     INFO(1) = -2
     INFO(2) = M
  ELSE IF (N .LT. 0) THEN
     INFO(1) = -3
     INFO(2) = N
  ELSE IF (N .GT. M) THEN
     INFO(1) = -3
     INFO(2) = N
  ELSE IF (LDG .LT. M) THEN
     INFO(1) = -5
     INFO(2) = LDG
  ELSE IF (LDV .LT. N) THEN
     INFO(1) = -7
     INFO(2) = LDV
  ELSE IF (MAXCYC .LT. 0) THEN
     INFO(1) = -8
     INFO(2) = MAXCYC
  ELSE IF (MAXTHR .LT. 0) THEN
     INFO(1) = -9
     INFO(2) = MAXTHR
  ELSE IF (JSTRAT .LT. 0) THEN
     INFO(1) = -10
     INFO(2) = JSTRAT
  ELSE IF (.NOT. (TOL .GE. D_ZERO)) THEN
     INFO(1) = -11
     IF (TOL .LT. D_ZERO) THEN
        INFO(2) = -1
     ELSE
        INFO(2) = 0
     END IF
  ELSE IF (LWORK .LT. 0) THEN
     INFO(1) = -14
     INFO(2) = LWORK
  ELSE IF (LIWORK .LT. 0) THEN
     INFO(1) = -16
     INFO(2) = LIWORK
  ELSE
     INFO = 0
  END IF

  NROT = 0
  IF (INFO(1) .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  IF (N .EQ. 1) THEN ! one column only
     IF (M .EQ. 1) THEN
        IF (.NOT. FAST) SIGMA(1) = G(1,1)
        G(1,1) = D_ONE
     ELSE
        MYTOL = DNRM2(M, G(1,1), 1)
        BLASNT = BLAS_SET_NUM_THREADS(MAXTHR)
        CALL DSCAL(M, D_ONE / MYTOL, G(1,1), 1)
        BLASNT = BLAS_SET_NUM_THREADS(BLASNT)
        IF (.NOT. FAST) SIGMA(1) = MYTOL
     END IF
     V(1,1) = D_ONE
     RETURN
  ELSE IF (JSTRAT .LT. JSMENC) THEN
     NPAIRS = 1
  ELSE IF (MOD(N,2) .EQ. 0) THEN
     NPAIRS = N / 2
  ELSE ! parallel strategies for odd N not implemented
     INFO(1) = -10
     INFO(2) = N
     RETURN
  END IF

  J = MOD(NPAIRS, D_VEC_LEN)
  IF (NPAIRS .LT. D_VEC_LEN) THEN
     VPAIRS = D_VEC_LEN
  ELSE IF (J .NE. 0) THEN
     VPAIRS = NPAIRS + (D_VEC_LEN - J)
  ELSE
     VPAIRS = NPAIRS
  END IF

  VTORS = VPAIRS / D_VEC_LEN
  J = MOD(VTORS, D_VEC_LEN)
  IF (VTORS .LT. D_VEC_LEN) THEN
     VTORSV = D_VEC_LEN
  ELSE IF (J .NE. 0) THEN
     VTORSV = VTORS + (D_VEC_LEN - J)
  ELSE
     VTORSV = VTORS
  END IF

  I = VPAIRS
  IF (I .LT. D_CL1_LEN) THEN
     I = D_CL1_LEN
  ELSE
     J = MOD(I, D_CL1_LEN)
     IF (J .NE. 0) I = I + (D_CL1_LEN - J)
  END IF
  LW1 = I
  INFO(1) = 6 * LW1

  I = 2 * NPAIRS
  IF (I .LT. I_CL1_LEN) THEN
     I = I_CL1_LEN
  ELSE
     J = MOD(I, I_CL1_LEN)
     IF (J .NE. 0) I = I + (I_CL1_LEN - J)
  END IF
  LIW1 = I

  I = VPAIRS
  IF (I .LT. I_CL1_LEN) THEN
     I = I_CL1_LEN
  ELSE
     J = MOD(I, I_CL1_LEN)
     IF (J .NE. 0) I = I + (I_CL1_LEN - J)
  END IF
  LIW2 = I

  I = VTORS
  IF (I .LT. I_CL1_LEN) THEN
     I = I_CL1_LEN
  ELSE
     J = MOD(I, I_CL1_LEN)
     IF (J .NE. 0) I = I + (I_CL1_LEN - J)
  END IF
  INFO(2) = I + LIW1 + LIW2

  IF (LWORK .LT. INFO(1)) RETURN
  IF (LIWORK .LT. INFO(2)) RETURN

  IF (TOL .EQ. D_ZERO) THEN
     MYTOL = SCALE(EPSILON(MYTOL),-1) ! TOL = -0.0
     IF (SIGN(D_ONE, TOL) .EQ. D_ONE) MYTOL = MYTOL * SQRT(DBLE(M))
  ELSE
     MYTOL = TOL
  END IF

  IF (.NOT. FAST) THEN
     WRITE (GET_IOUNIT('O'),'(A,I11,A,I11,A,ES25.17E3,A)') 'WORK(', INFO(1), '), IWORK(', INFO(2), '), TOL(', MYTOL, ')'
  END IF

  BLASNT = BLAS_SET_NUM_THREADS(MAXTHR)
  CALL DLASET('A', N, N, D_ZERO, D_ONE, V, LDV) ! V = I_N

  CALL MYVJAC0(NPAIRS, VPAIRS, VTORS, VTORSV, M, N, G, LDG, V, LDV, MAXCYC, JSTRAT, MYTOL,&
       WORK(1), WORK(1 + LW1), WORK(1 + 2 * LW1), WORK(1 + 3 * LW1), WORK(1 + 4 * LW1), WORK(1 + 5 * LW1),&
       IWORK(1), IWORK(1 + LIW1), IWORK(1 + LIW1 + LIW2), NROT, INFO)
  IF (INFO(1) .EQ. 0) THEN
     IF (.NOT. FAST) THEN
        DO J = 1, N
           SIGMA(J) = DNRM2(M, G(1,J), 1)
           CALL DSCAL(M, D_ONE / SIGMA(J), G(1,J), 1)
        END DO
     END IF
  END IF
  BLASNT = BLAS_SET_NUM_THREADS(BLASNT)

END SUBROUTINE VJAC0

! Assumes INTEGERs and LOGICALs of their respecitve default kinds.
! Also, in a context of a single subroutine/function call, all its
! real, complex, and/or imaginary arguments/values have the same kind.
MODULE VN_IMAGINARY_F
#ifndef KIND_QUAD
#ifndef QX_WP
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#endif
#endif
  IMPLICIT NONE

#ifdef KIND_SINGLE
  INTEGER, PARAMETER, PRIVATE :: SWP = KIND_SINGLE
#else
  INTEGER, PARAMETER, PRIVATE :: SWP = KIND(0E0)
#endif

#ifdef KIND_DOUBLE
  INTEGER, PARAMETER, PRIVATE :: DWP = KIND_DOUBLE
#else
  INTEGER, PARAMETER, PRIVATE :: DWP = KIND(0D0)
#endif

#ifdef KIND_QUAD
  INTEGER, PARAMETER, PRIVATE :: QWP = KIND_QUAD
#else
#ifdef QX_WP
  INTEGER, PARAMETER, PRIVATE :: QWP = QX_WP
#else
  INTEGER, PARAMETER, PRIVATE :: QWP = REAL128
#endif
#endif

  REAL(KIND=SWP), PARAMETER, PRIVATE :: S_ZERO = 0.0_SWP
  REAL(KIND=DWP), PARAMETER, PRIVATE :: D_ZERO = 0.0_DWP
  REAL(KIND=QWP), PARAMETER, PRIVATE :: Q_ZERO = 0.0_QWP

  TYPE, BIND(C) :: CIMAGINARY
     REAL(KIND=SWP) :: J
  END TYPE CIMAGINARY
  TYPE, BIND(C) :: ZIMAGINARY
     REAL(KIND=DWP) :: J
  END TYPE ZIMAGINARY
  TYPE, BIND(C) :: XIMAGINARY
     REAL(KIND=QWP) :: J
  END TYPE XIMAGINARY

  ! unary operators
  INTERFACE OPERATOR(+)
     MODULE PROCEDURE CPLSJ,ZPLSJ,XPLSJ
  END INTERFACE OPERATOR(+)
  INTERFACE OPERATOR(-)
     MODULE PROCEDURE CNEGJ,ZNEGJ,XNEGJ
  END INTERFACE OPERATOR(-)

  ! binary arithmetic operators
  INTERFACE OPERATOR(+)
     MODULE PROCEDURE CJPLUSJ,ZJPLUSJ,XJPLUSJ
     MODULE PROCEDURE CJPLUSI,ZJPLUSI,XJPLUSI
     MODULE PROCEDURE CIPLUSJ,ZIPLUSJ,XIPLUSJ
     MODULE PROCEDURE CJPLUSR,ZJPLUSR,XJPLUSR
     MODULE PROCEDURE CRPLUSJ,ZRPLUSJ,XRPLUSJ
     MODULE PROCEDURE CJPLUSC,ZJPLUSC,XJPLUSC
     MODULE PROCEDURE CCPLUSJ,ZCPLUSJ,XCPLUSJ
  END INTERFACE OPERATOR(+)
  INTERFACE OPERATOR(-)
     MODULE PROCEDURE CJMINUSJ,ZJMINUSJ,XJMINUSJ
     MODULE PROCEDURE CJMINUSI,ZJMINUSI,XJMINUSI
     MODULE PROCEDURE CIMINUSJ,ZIMINUSJ,XIMINUSJ
     MODULE PROCEDURE CJMINUSR,ZJMINUSR,XJMINUSR
     MODULE PROCEDURE CRMINUSJ,ZRMINUSJ,XRMINUSJ
     MODULE PROCEDURE CJMINUSC,ZJMINUSC,XJMINUSC
     MODULE PROCEDURE CCMINUSJ,ZCMINUSJ,XCMINUSJ
  END INTERFACE OPERATOR(-)
  INTERFACE OPERATOR(*)
     MODULE PROCEDURE CJTIMESJ,ZJTIMESJ,XJTIMESJ
     MODULE PROCEDURE CJTIMESI,ZJTIMESI,XJTIMESI
     MODULE PROCEDURE CITIMESJ,ZITIMESJ,XITIMESJ
     MODULE PROCEDURE CJTIMESR,ZJTIMESR,XJTIMESR
     MODULE PROCEDURE CRTIMESJ,ZRTIMESJ,XRTIMESJ
     MODULE PROCEDURE CJTIMESC,ZJTIMESC,XJTIMESC
     MODULE PROCEDURE CCTIMESJ,ZCTIMESJ,XCTIMESJ
  END INTERFACE OPERATOR(*)
  INTERFACE OPERATOR(/)
     MODULE PROCEDURE CJDIVJ,ZJDIVJ,XJDIVJ
     MODULE PROCEDURE CJDIVI,ZJDIVI,XJDIVI
     MODULE PROCEDURE CIDIVJ,ZIDIVJ,XIDIVJ
     MODULE PROCEDURE CJDIVR,ZJDIVR,XJDIVR
     MODULE PROCEDURE CRDIVJ,ZRDIVJ,XRDIVJ
     MODULE PROCEDURE CJDIVC,ZJDIVC,XJDIVC
     MODULE PROCEDURE CCDIVJ,ZCDIVJ,XCDIVJ
  END INTERFACE OPERATOR(/)
  ! TODO: **

  ! relational operators
  INTERFACE OPERATOR(.EQ.)
     MODULE PROCEDURE CJEQJ,ZJEQJ,XJEQJ
     MODULE PROCEDURE CJEQI,ZJEQI,XJEQI
     MODULE PROCEDURE CIEQJ,ZIEQJ,XIEQJ
     MODULE PROCEDURE CJEQR,ZJEQR,XJEQR
     MODULE PROCEDURE CREQJ,ZREQJ,XREQJ
     MODULE PROCEDURE CJEQC,ZJEQC,XJEQC
     MODULE PROCEDURE CCEQJ,ZCEQJ,XCEQJ
  END INTERFACE OPERATOR(.EQ.)
  INTERFACE OPERATOR(.NE.)
     MODULE PROCEDURE CJNEJ,ZJNEJ,XJNEJ
     MODULE PROCEDURE CJNEI,ZJNEI,XJNEI
     MODULE PROCEDURE CINEJ,ZINEJ,XINEJ
     MODULE PROCEDURE CJNER,ZJNER,XJNER
     MODULE PROCEDURE CRNEJ,ZRNEJ,XRNEJ
     MODULE PROCEDURE CJNEC,ZJNEC,XJNEC
     MODULE PROCEDURE CCNEJ,ZCNEJ,XCNEJ
  END INTERFACE OPERATOR(.NE.)
  INTERFACE OPERATOR(.LT.)
     MODULE PROCEDURE CJLTJ,ZJLTJ,XJLTJ
  END INTERFACE OPERATOR(.LT.)
  INTERFACE OPERATOR(.LE.)
     MODULE PROCEDURE CJLEJ,ZJLEJ,XJLEJ
  END INTERFACE OPERATOR(.LE.)
  INTERFACE OPERATOR(.GE.)
     MODULE PROCEDURE CJGEJ,ZJGEJ,XJGEJ
  END INTERFACE OPERATOR(.GE.)
  INTERFACE OPERATOR(.GT.)
     MODULE PROCEDURE CJGTJ,ZJGTJ,XJGTJ
  END INTERFACE OPERATOR(.GT.)

  ! elemental functions
  INTERFACE ABS
     MODULE PROCEDURE CJABS,ZJABS,XJABS
  END INTERFACE ABS
  INTERFACE AIMAG
     MODULE PROCEDURE CJIMAG,ZJIMAG,XJIMAG
  END INTERFACE AIMAG
  INTERFACE CMPLX
     MODULE PROCEDURE CJCMPLX,ZJCMPLX,XJCMPLX
  END INTERFACE CMPLX
  INTERFACE CONJG
     MODULE PROCEDURE CJCONJG,ZJCONJG,XJCONJG
  END INTERFACE CONJG
  INTERFACE INT
     MODULE PROCEDURE CJINT,ZJINT,XJINT
  END INTERFACE INT
  INTERFACE REAL
     MODULE PROCEDURE CJREAL,ZJREAL,XJREAL
  END INTERFACE REAL

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELEMENTAL FUNCTION CPLSJ(A)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    TYPE(CIMAGINARY) :: CPLSJ
    CPLSJ%J = A%J
  END FUNCTION CPLSJ
  ELEMENTAL FUNCTION ZPLSJ(A)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    TYPE(ZIMAGINARY) :: ZPLSJ
    ZPLSJ%J = A%J
  END FUNCTION ZPLSJ
  ELEMENTAL FUNCTION XPLSJ(A)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    TYPE(XIMAGINARY) :: XPLSJ
    XPLSJ%J = A%J
  END FUNCTION XPLSJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELEMENTAL FUNCTION CNEGJ(A)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    TYPE(CIMAGINARY) :: CNEGJ
    CNEGJ%J = -A%J
  END FUNCTION CNEGJ
  ELEMENTAL FUNCTION ZNEGJ(A)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    TYPE(ZIMAGINARY) :: ZNEGJ
    ZNEGJ%J = -A%J
  END FUNCTION ZNEGJ
  ELEMENTAL FUNCTION XNEGJ(A)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    TYPE(XIMAGINARY) :: XNEGJ
    XNEGJ%J = -A%J
  END FUNCTION XNEGJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJPLUSJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    TYPE(CIMAGINARY) :: CJPLUSJ
    CJPLUSJ%J = A%J + B%J
  END FUNCTION CJPLUSJ
  PURE FUNCTION ZJPLUSJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    TYPE(ZIMAGINARY) :: ZJPLUSJ
    ZJPLUSJ%J = A%J + B%J
  END FUNCTION ZJPLUSJ
  PURE FUNCTION XJPLUSJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    TYPE(XIMAGINARY) :: XJPLUSJ
    XJPLUSJ%J = A%J + B%J
  END FUNCTION XJPLUSJ

  PURE FUNCTION CJPLUSI(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CJPLUSI
    CJPLUSI = CMPLX(REAL(B,SWP), A%J, SWP)
  END FUNCTION CJPLUSI
  PURE FUNCTION ZJPLUSI(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZJPLUSI
    ZJPLUSI = CMPLX(REAL(B,DWP), A%J, DWP)
  END FUNCTION ZJPLUSI
  PURE FUNCTION XJPLUSI(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XJPLUSI
    XJPLUSI = CMPLX(REAL(B,QWP), A%J, QWP)
  END FUNCTION XJPLUSI

  PURE FUNCTION CIPLUSJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CIPLUSJ
    CIPLUSJ = CMPLX(REAL(A,SWP), B%J, SWP)
  END FUNCTION CIPLUSJ
  PURE FUNCTION ZIPLUSJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZIPLUSJ
    ZIPLUSJ = CMPLX(REAL(A,DWP), B%J, DWP)
  END FUNCTION ZIPLUSJ
  PURE FUNCTION XIPLUSJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XIPLUSJ
    XIPLUSJ = CMPLX(REAL(A,QWP), B%J, QWP)
  END FUNCTION XIPLUSJ

  PURE FUNCTION CJPLUSR(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    REAL(KIND=SWP), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CJPLUSR
    CJPLUSR = CMPLX(B, A%J, SWP)
  END FUNCTION CJPLUSR
  PURE FUNCTION ZJPLUSR(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=DWP), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZJPLUSR
    ZJPLUSR = CMPLX(B, A%J, DWP)
  END FUNCTION ZJPLUSR
  PURE FUNCTION XJPLUSR(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=QWP), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XJPLUSR
    XJPLUSR = CMPLX(B, A%J, QWP)
  END FUNCTION XJPLUSR

  PURE FUNCTION CRPLUSJ(A,B)
    IMPLICIT NONE
    REAL(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CRPLUSJ
    CRPLUSJ = CMPLX(A, B%J, SWP)
  END FUNCTION CRPLUSJ
  PURE FUNCTION ZRPLUSJ(A,B)
    IMPLICIT NONE
    REAL(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZRPLUSJ
    ZRPLUSJ = CMPLX(A, B%J, DWP)
  END FUNCTION ZRPLUSJ
  PURE FUNCTION XRPLUSJ(A,B)
    IMPLICIT NONE
    REAL(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XRPLUSJ
    XRPLUSJ = CMPLX(A, B%J, QWP)
  END FUNCTION XRPLUSJ

  PURE FUNCTION CJPLUSC(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=SWP), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CJPLUSC
    CJPLUSC = CMPLX(REAL(B), A%J + AIMAG(B), SWP)
  END FUNCTION CJPLUSC
  PURE FUNCTION ZJPLUSC(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=DWP), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZJPLUSC
    ZJPLUSC = CMPLX(REAL(B), A%J + AIMAG(B), DWP)
  END FUNCTION ZJPLUSC
  PURE FUNCTION XJPLUSC(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=QWP), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XJPLUSC
    XJPLUSC = CMPLX(REAL(B), A%J + AIMAG(B), QWP)
  END FUNCTION XJPLUSC

  PURE FUNCTION CCPLUSJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CCPLUSJ
    CCPLUSJ = CMPLX(REAL(A), AIMAG(A) + B%J, SWP)
  END FUNCTION CCPLUSJ
  PURE FUNCTION ZCPLUSJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZCPLUSJ
    ZCPLUSJ = CMPLX(REAL(A), AIMAG(A) + B%J, DWP)
  END FUNCTION ZCPLUSJ
  PURE FUNCTION XCPLUSJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XCPLUSJ
    XCPLUSJ = CMPLX(REAL(A), AIMAG(A) + B%J, QWP)
  END FUNCTION XCPLUSJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJMINUSJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    TYPE(CIMAGINARY) :: CJMINUSJ
    CJMINUSJ%J = A%J - B%J
  END FUNCTION CJMINUSJ
  PURE FUNCTION ZJMINUSJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    TYPE(ZIMAGINARY) :: ZJMINUSJ
    ZJMINUSJ%J = A%J - B%J
  END FUNCTION ZJMINUSJ
  PURE FUNCTION XJMINUSJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    TYPE(XIMAGINARY) :: XJMINUSJ
    XJMINUSJ%J = A%J - B%J
  END FUNCTION XJMINUSJ

  PURE FUNCTION CJMINUSI(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CJMINUSI
    CJMINUSI = CMPLX(-REAL(B,SWP), A%J, SWP)
  END FUNCTION CJMINUSI
  PURE FUNCTION ZJMINUSI(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZJMINUSI
    ZJMINUSI = CMPLX(-REAL(B,DWP), A%J, DWP)
  END FUNCTION ZJMINUSI
  PURE FUNCTION XJMINUSI(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XJMINUSI
    XJMINUSI = CMPLX(-REAL(B,QWP), A%J, QWP)
  END FUNCTION XJMINUSI

  PURE FUNCTION CIMINUSJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CIMINUSJ
    CIMINUSJ = CMPLX(REAL(A,SWP), -B%J, SWP)
  END FUNCTION CIMINUSJ
  PURE FUNCTION ZIMINUSJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZIMINUSJ
    ZIMINUSJ = CMPLX(REAL(A,DWP), -B%J, DWP)
  END FUNCTION ZIMINUSJ
  PURE FUNCTION XIMINUSJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XIMINUSJ
    XIMINUSJ = CMPLX(REAL(A,QWP), -B%J, QWP)
  END FUNCTION XIMINUSJ

  PURE FUNCTION CJMINUSR(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    REAL(KIND=SWP), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CJMINUSR
    CJMINUSR = CMPLX(-B, A%J, SWP)
  END FUNCTION CJMINUSR
  PURE FUNCTION ZJMINUSR(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=DWP), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZJMINUSR
    ZJMINUSR = CMPLX(-B, A%J, DWP)
  END FUNCTION ZJMINUSR
  PURE FUNCTION XJMINUSR(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    REAL(KIND=QWP), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XJMINUSR
    XJMINUSR = CMPLX(-B, A%J, QWP)
  END FUNCTION XJMINUSR

  PURE FUNCTION CRMINUSJ(A,B)
    IMPLICIT NONE
    REAL(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CRMINUSJ
    CRMINUSJ = CMPLX(A, -B%J, SWP)
  END FUNCTION CRMINUSJ
  PURE FUNCTION ZRMINUSJ(A,B)
    IMPLICIT NONE
    REAL(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZRMINUSJ
    ZRMINUSJ = CMPLX(A, -B%J, DWP)
  END FUNCTION ZRMINUSJ
  PURE FUNCTION XRMINUSJ(A,B)
    IMPLICIT NONE
    REAL(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XRMINUSJ
    XRMINUSJ = CMPLX(A, -B%J, QWP)
  END FUNCTION XRMINUSJ

  PURE FUNCTION CJMINUSC(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=SWP), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CJMINUSC
    CJMINUSC = CMPLX(-REAL(B), A%J - AIMAG(B), SWP)
  END FUNCTION CJMINUSC
  PURE FUNCTION ZJMINUSC(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=DWP), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZJMINUSC
    ZJMINUSC = CMPLX(-REAL(B), A%J - AIMAG(B), DWP)
  END FUNCTION ZJMINUSC
  PURE FUNCTION XJMINUSC(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=QWP), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XJMINUSC
    XJMINUSC = CMPLX(-REAL(B), A%J - AIMAG(B), QWP)
  END FUNCTION XJMINUSC

  PURE FUNCTION CCMINUSJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CCMINUSJ
    CCMINUSJ = CMPLX(REAL(A), AIMAG(A) - B%J, SWP)
  END FUNCTION CCMINUSJ
  PURE FUNCTION ZCMINUSJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZCMINUSJ
    ZCMINUSJ = CMPLX(REAL(A), AIMAG(A) - B%J, DWP)
  END FUNCTION ZCMINUSJ
  PURE FUNCTION XCMINUSJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XCMINUSJ
    XCMINUSJ = CMPLX(REAL(A), AIMAG(A) - B%J, QWP)
  END FUNCTION XCMINUSJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJTIMESJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    REAL(KIND=SWP) :: CJTIMESJ
    CJTIMESJ = -(A%J * B%J)
  END FUNCTION CJTIMESJ
  PURE FUNCTION ZJTIMESJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    REAL(KIND=DWP) :: ZJTIMESJ
    ZJTIMESJ = -(A%J * B%J)
  END FUNCTION ZJTIMESJ
  PURE FUNCTION XJTIMESJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    REAL(KIND=QWP) :: XJTIMESJ
    XJTIMESJ = -(A%J * B%J)
  END FUNCTION XJTIMESJ

  PURE FUNCTION CJTIMESI(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    TYPE(CIMAGINARY) :: CJTIMESI
    CJTIMESI%J = A%J * B
  END FUNCTION CJTIMESI
  PURE FUNCTION ZJTIMESI(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    TYPE(ZIMAGINARY) :: ZJTIMESI
    ZJTIMESI%J = A%J * B
  END FUNCTION ZJTIMESI
  PURE FUNCTION XJTIMESI(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    TYPE(XIMAGINARY) :: XJTIMESI
    XJTIMESI%J = A%J * B
  END FUNCTION XJTIMESI

  PURE FUNCTION CITIMESJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    TYPE(CIMAGINARY) :: CITIMESJ
    CITIMESJ%J = A * B%J
  END FUNCTION CITIMESJ
  PURE FUNCTION ZITIMESJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    TYPE(ZIMAGINARY) :: ZITIMESJ
    ZITIMESJ%J = A * B%J
  END FUNCTION ZITIMESJ
  PURE FUNCTION XITIMESJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    TYPE(XIMAGINARY) :: XITIMESJ
    XITIMESJ%J = A * B%J
  END FUNCTION XITIMESJ

  PURE FUNCTION CJTIMESR(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    REAL(KIND=SWP), INTENT(IN) :: B
    TYPE(CIMAGINARY) :: CJTIMESR
    CJTIMESR%J = A%J * B
  END FUNCTION CJTIMESR
  PURE FUNCTION ZJTIMESR(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=DWP), INTENT(IN) :: B
    TYPE(ZIMAGINARY) :: ZJTIMESR
    ZJTIMESR%J = A%J * B
  END FUNCTION ZJTIMESR
  PURE FUNCTION XJTIMESR(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    REAL(KIND=QWP), INTENT(IN) :: B
    TYPE(XIMAGINARY) :: XJTIMESR
    XJTIMESR%J = A%J * B
  END FUNCTION XJTIMESR

  PURE FUNCTION CRTIMESJ(A,B)
    IMPLICIT NONE
    REAL(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    TYPE(CIMAGINARY) :: CRTIMESJ
    CRTIMESJ%J = A * B%J
  END FUNCTION CRTIMESJ
  PURE FUNCTION ZRTIMESJ(A,B)
    IMPLICIT NONE
    REAL(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    TYPE(ZIMAGINARY) :: ZRTIMESJ
    ZRTIMESJ%J = A * B%J
  END FUNCTION ZRTIMESJ
  PURE FUNCTION XRTIMESJ(A,B)
    IMPLICIT NONE
    REAL(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    TYPE(XIMAGINARY) :: XRTIMESJ
    XRTIMESJ%J = A * B%J
  END FUNCTION XRTIMESJ

  PURE FUNCTION CJTIMESC(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=SWP), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CJTIMESC
    CJTIMESC = CMPLX(-(A%J * AIMAG(B)), A%J * REAL(B), SWP)
  END FUNCTION CJTIMESC
  PURE FUNCTION ZJTIMESC(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=DWP), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZJTIMESC
    ZJTIMESC = CMPLX(-(A%J * AIMAG(B)), A%J * REAL(B), DWP)
  END FUNCTION ZJTIMESC
  PURE FUNCTION XJTIMESC(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=QWP), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XJTIMESC
    XJTIMESC = CMPLX(-(A%J * AIMAG(B)), A%J * REAL(B), QWP)
  END FUNCTION XJTIMESC

  PURE FUNCTION CCTIMESJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CCTIMESJ
    CCTIMESJ = CMPLX(-(AIMAG(A) * B%J), REAL(A) * B%J, SWP)
  END FUNCTION CCTIMESJ
  PURE FUNCTION ZCTIMESJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZCTIMESJ
    ZCTIMESJ = CMPLX(-(AIMAG(A) * B%J), REAL(A) * B%J, DWP)
  END FUNCTION ZCTIMESJ
  PURE FUNCTION XCTIMESJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XCTIMESJ
    XCTIMESJ = CMPLX(-(AIMAG(A) * B%J), REAL(A) * B%J, QWP)
  END FUNCTION XCTIMESJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJDIVJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    REAL(KIND=SWP) :: CJDIVJ
    CJDIVJ = A%J / B%J
  END FUNCTION CJDIVJ
  PURE FUNCTION ZJDIVJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    REAL(KIND=DWP) :: ZJDIVJ
    ZJDIVJ = A%J / B%J
  END FUNCTION ZJDIVJ
  PURE FUNCTION XJDIVJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    REAL(KIND=QWP) :: XJDIVJ
    XJDIVJ = A%J / B%J
  END FUNCTION XJDIVJ

  PURE FUNCTION CJDIVI(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    TYPE(CIMAGINARY) :: CJDIVI
    CJDIVI%J = A%J / B
  END FUNCTION CJDIVI
  PURE FUNCTION ZJDIVI(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    TYPE(ZIMAGINARY) :: ZJDIVI
    ZJDIVI%J = A%J / B
  END FUNCTION ZJDIVI
  PURE FUNCTION XJDIVI(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    TYPE(XIMAGINARY) :: XJDIVI
    XJDIVI%J = A%J / B
  END FUNCTION XJDIVI

  PURE FUNCTION CIDIVJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    TYPE(CIMAGINARY) :: CIDIVJ
    CIDIVJ%J = -(A / B%J)
  END FUNCTION CIDIVJ
  PURE FUNCTION ZIDIVJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    TYPE(ZIMAGINARY) :: ZIDIVJ
    ZIDIVJ%J = -(A / B%J)
  END FUNCTION ZIDIVJ
  PURE FUNCTION XIDIVJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    TYPE(XIMAGINARY) :: XIDIVJ
    XIDIVJ%J = -(A / B%J)
  END FUNCTION XIDIVJ

  PURE FUNCTION CJDIVR(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    REAL(KIND=SWP), INTENT(IN) :: B
    TYPE(CIMAGINARY) :: CJDIVR
    CJDIVR%J = A%J / B
  END FUNCTION CJDIVR
  PURE FUNCTION ZJDIVR(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=DWP), INTENT(IN) :: B
    TYPE(ZIMAGINARY) :: ZJDIVR
    ZJDIVR%J = A%J / B
  END FUNCTION ZJDIVR
  PURE FUNCTION XJDIVR(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    REAL(KIND=QWP), INTENT(IN) :: B
    TYPE(XIMAGINARY) :: XJDIVR
    XJDIVR%J = A%J / B
  END FUNCTION XJDIVR

  PURE FUNCTION CRDIVJ(A,B)
    IMPLICIT NONE
    REAL(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    TYPE(CIMAGINARY) :: CRDIVJ
    CRDIVJ%J = -(A / B%J)
  END FUNCTION CRDIVJ
  PURE FUNCTION ZRDIVJ(A,B)
    IMPLICIT NONE
    REAL(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    TYPE(ZIMAGINARY) :: ZRDIVJ
    ZRDIVJ%J = -(A / B%J)
  END FUNCTION ZRDIVJ
  PURE FUNCTION XRDIVJ(A,B)
    IMPLICIT NONE
    REAL(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    TYPE(XIMAGINARY) :: XRDIVJ
    XRDIVJ%J = -(A / B%J)
  END FUNCTION XRDIVJ

  PURE FUNCTION CJDIVC(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=SWP), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CJDIVC
    REAL(KIND=SWP) :: AVB
    CJDIVC = CMPLX(AIMAG(B), REAL(B), SWP)
    AVB = ABS(CJDIVC)
#ifdef VN_FAST_JDIVC
    CJDIVC = (A%J / AVB) * CJDIVC
#else
    IF (.NOT. (AVB .LE. HUGE(AVB))) THEN
       CJDIVC = CMPLX(SCALE(AIMAG(B), -1), SCALE(REAL(B), -1), SWP)
       AVB = ABS(CJDIVC)
       CJDIVC = SCALE(A%J, 1) * (CJDIVC / AVB)
    ELSE IF (AVB .EQ. S_ZERO) THEN
       ! Return whatever the compiler would if A was COMPLEX.
       CJDIVC = CMPLX(S_ZERO, A%J, SWP) / CJDIVC
    ELSE ! general case
       CJDIVC = A%J * (CJDIVC / AVB)
    END IF
#endif
  END FUNCTION CJDIVC
  PURE FUNCTION ZJDIVC(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=DWP), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZJDIVC
    REAL(KIND=DWP) :: AVB
    ZJDIVC = CMPLX(AIMAG(B), REAL(B), DWP)
    AVB = ABS(ZJDIVC)
#ifdef VN_FAST_JDIVC
    ZJDIVC = (A%J / AVB) * ZJDIVC
#else
    IF (.NOT. (AVB .LE. HUGE(AVB))) THEN
       ZJDIVC = CMPLX(SCALE(AIMAG(B), -1), SCALE(REAL(B), -1), DWP)
       AVB = ABS(ZJDIVC)
       ZJDIVC = SCALE(A%J, 1) * (ZJDIVC / AVB)
    ELSE IF (AVB .EQ. D_ZERO) THEN
       ! Return whatever the compiler would if A was COMPLEX.
       ZJDIVC = CMPLX(D_ZERO, A%J, DWP) / ZJDIVC
    ELSE ! general case
       ZJDIVC = A%J * (ZJDIVC / AVB)
    END IF
#endif
  END FUNCTION ZJDIVC
  PURE FUNCTION XJDIVC(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=QWP), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XJDIVC
    REAL(KIND=QWP) :: AVB
    XJDIVC = CMPLX(AIMAG(B), REAL(B), QWP)
    AVB = ABS(XJDIVC)
#ifdef VN_FAST_JDIVC
    XJDIVC = (A%J / AVB) * XJDIVC
#else
    IF (.NOT. (AVB .LE. HUGE(AVB))) THEN
       XJDIVC = CMPLX(SCALE(AIMAG(B), -1), SCALE(REAL(B), -1), QWP)
       AVB = ABS(XJDIVC)
       XJDIVC = SCALE(A%J, 1) * (XJDIVC / AVB)
    ELSE IF (AVB .EQ. Q_ZERO) THEN
       ! Return whatever the compiler would if A was COMPLEX.
       XJDIVC = CMPLX(Q_ZERO, A%J, QWP) / XJDIVC
    ELSE ! general case
       XJDIVC = A%J * (XJDIVC / AVB)
    END IF
#endif
  END FUNCTION XJDIVC

  PURE FUNCTION CCDIVJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=SWP) :: CCDIVJ
    CCDIVJ = CMPLX(AIMAG(A), -REAL(A), SWP) / B%J
  END FUNCTION CCDIVJ
  PURE FUNCTION ZCDIVJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=DWP) :: ZCDIVJ
    ZCDIVJ = CMPLX(AIMAG(A), -REAL(A), DWP) / B%J
  END FUNCTION ZCDIVJ
  PURE FUNCTION XCDIVJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    COMPLEX(KIND=QWP) :: XCDIVJ
    XCDIVJ = CMPLX(AIMAG(A), -REAL(A), QWP) / B%J
  END FUNCTION XCDIVJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJEQJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: CJEQJ
    CJEQJ = (A%J .EQ. B%J)
  END FUNCTION CJEQJ
  PURE FUNCTION ZJEQJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: ZJEQJ
    ZJEQJ = (A%J .EQ. B%J)
  END FUNCTION ZJEQJ
  PURE FUNCTION XJEQJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: XJEQJ
    XJEQJ = (A%J .EQ. B%J)
  END FUNCTION XJEQJ

  PURE FUNCTION CJEQI(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    LOGICAL :: CJEQI
    CJEQI = ((B .EQ. 0) .AND. (A%J .EQ. S_ZERO))
  END FUNCTION CJEQI
  PURE FUNCTION ZJEQI(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    LOGICAL :: ZJEQI
    ZJEQI = ((B .EQ. 0) .AND. (A%J .EQ. D_ZERO))
  END FUNCTION ZJEQI
  PURE FUNCTION XJEQI(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    LOGICAL :: XJEQI
    XJEQI = ((B .EQ. 0) .AND. (A%J .EQ. Q_ZERO))
  END FUNCTION XJEQI

  PURE FUNCTION CIEQJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    LOGICAL :: CIEQJ
    CIEQJ = ((A .EQ. 0) .AND. (B%J .EQ. S_ZERO))
  END FUNCTION CIEQJ
  PURE FUNCTION ZIEQJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    LOGICAL :: ZIEQJ
    ZIEQJ = ((A .EQ. 0) .AND. (B%J .EQ. D_ZERO))
  END FUNCTION ZIEQJ
  PURE FUNCTION XIEQJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    LOGICAL :: XIEQJ
    XIEQJ = ((A .EQ. 0) .AND. (B%J .EQ. Q_ZERO))
  END FUNCTION XIEQJ

  PURE FUNCTION CJEQR(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    REAL(KIND=SWP), INTENT(IN) :: B
    LOGICAL :: CJEQR
    CJEQR = ((B .EQ. S_ZERO) .AND. (A%J .EQ. B))
  END FUNCTION CJEQR
  PURE FUNCTION ZJEQR(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=DWP), INTENT(IN) :: B
    LOGICAL :: ZJEQR
    ZJEQR = ((B .EQ. D_ZERO) .AND. (A%J .EQ. B))
  END FUNCTION ZJEQR
  PURE FUNCTION XJEQR(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    REAL(KIND=QWP), INTENT(IN) :: B
    LOGICAL :: XJEQR
    XJEQR = ((B .EQ. Q_ZERO) .AND. (A%J .EQ. B))
  END FUNCTION XJEQR

  PURE FUNCTION CREQJ(A,B)
    IMPLICIT NONE
    REAL(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    LOGICAL :: CREQJ
    CREQJ = ((A .EQ. S_ZERO) .AND. (B%J .EQ. A))
  END FUNCTION CREQJ
  PURE FUNCTION ZREQJ(A,B)
    IMPLICIT NONE
    REAL(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    LOGICAL :: ZREQJ
    ZREQJ = ((A .EQ. D_ZERO) .AND. (B%J .EQ. A))
  END FUNCTION ZREQJ
  PURE FUNCTION XREQJ(A,B)
    IMPLICIT NONE
    REAL(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    LOGICAL :: XREQJ
    XREQJ = ((A .EQ. Q_ZERO) .AND. (B%J .EQ. A))
  END FUNCTION XREQJ

  PURE FUNCTION CJEQC(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=SWP), INTENT(IN) :: B
    LOGICAL :: CJEQC
    CJEQC = ((REAL(B) .EQ. S_ZERO) .AND. (A%J .EQ. AIMAG(B)))
  END FUNCTION CJEQC
  PURE FUNCTION ZJEQC(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=DWP), INTENT(IN) :: B
    LOGICAL :: ZJEQC
    ZJEQC = ((REAL(B) .EQ. D_ZERO) .AND. (A%J .EQ. AIMAG(B)))
  END FUNCTION ZJEQC
  PURE FUNCTION XJEQC(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=QWP), INTENT(IN) :: B
    LOGICAL :: XJEQC
    XJEQC = ((REAL(B) .EQ. Q_ZERO) .AND. (A%J .EQ. AIMAG(B)))
  END FUNCTION XJEQC

  PURE FUNCTION CCEQJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    LOGICAL :: CCEQJ
    CCEQJ = ((REAL(A) .EQ. S_ZERO) .AND. (B%J .EQ. AIMAG(A)))
  END FUNCTION CCEQJ
  PURE FUNCTION ZCEQJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    LOGICAL :: ZCEQJ
    ZCEQJ = ((REAL(A) .EQ. D_ZERO) .AND. (B%J .EQ. AIMAG(A)))
  END FUNCTION ZCEQJ
  PURE FUNCTION XCEQJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    LOGICAL :: XCEQJ
    XCEQJ = ((REAL(A) .EQ. D_ZERO) .AND. (B%J .EQ. AIMAG(A)))
  END FUNCTION XCEQJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJNEJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: CJNEJ
    CJNEJ = (A%J .NE. B%J)
  END FUNCTION CJNEJ
  PURE FUNCTION ZJNEJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: ZJNEJ
    ZJNEJ = (A%J .NE. B%J)
  END FUNCTION ZJNEJ
  PURE FUNCTION XJNEJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: XJNEJ
    XJNEJ = (A%J .NE. B%J)
  END FUNCTION XJNEJ

  PURE FUNCTION CJNEI(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    LOGICAL :: CJNEI
    CJNEI = ((B .NE. 0) .OR. (A%J .NE. S_ZERO))
  END FUNCTION CJNEI
  PURE FUNCTION ZJNEI(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    LOGICAL :: ZJNEI
    ZJNEI = ((B .NE. 0) .OR. (A%J .NE. D_ZERO))
  END FUNCTION ZJNEI
  PURE FUNCTION XJNEI(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    INTEGER, INTENT(IN) :: B
    LOGICAL :: XJNEI
    XJNEI = ((B .NE. 0) .OR. (A%J .NE. Q_ZERO))
  END FUNCTION XJNEI

  PURE FUNCTION CINEJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    LOGICAL :: CINEJ
    CINEJ = ((A .NE. 0) .OR. (B%J .NE. S_ZERO))
  END FUNCTION CINEJ
  PURE FUNCTION ZINEJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    LOGICAL :: ZINEJ
    ZINEJ = ((A .NE. 0) .OR. (B%J .NE. D_ZERO))
  END FUNCTION ZINEJ
  PURE FUNCTION XINEJ(A,B)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    LOGICAL :: XINEJ
    XINEJ = ((A .NE. 0) .OR. (B%J .NE. Q_ZERO))
  END FUNCTION XINEJ

  PURE FUNCTION CJNER(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    REAL(KIND=SWP), INTENT(IN) :: B
    LOGICAL :: CJNER
    CJNER = ((B .NE. S_ZERO) .OR. (A%J .NE. B))
  END FUNCTION CJNER
  PURE FUNCTION ZJNER(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=DWP), INTENT(IN) :: B
    LOGICAL :: ZJNER
    ZJNER = ((B .NE. D_ZERO) .OR. (A%J .NE. B))
  END FUNCTION ZJNER
  PURE FUNCTION XJNER(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    REAL(KIND=QWP), INTENT(IN) :: B
    LOGICAL :: XJNER
    XJNER = ((B .NE. Q_ZERO) .OR. (A%J .NE. B))
  END FUNCTION XJNER

  PURE FUNCTION CRNEJ(A,B)
    IMPLICIT NONE
    REAL(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    LOGICAL :: CRNEJ
    CRNEJ = ((A .NE. S_ZERO) .OR. (B%J .NE. A))
  END FUNCTION CRNEJ
  PURE FUNCTION ZRNEJ(A,B)
    IMPLICIT NONE
    REAL(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    LOGICAL :: ZRNEJ
    ZRNEJ = ((A .NE. D_ZERO) .OR. (B%J .NE. A))
  END FUNCTION ZRNEJ
  PURE FUNCTION XRNEJ(A,B)
    IMPLICIT NONE
    REAL(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    LOGICAL :: XRNEJ
    XRNEJ = ((A .NE. Q_ZERO) .OR. (B%J .NE. A))
  END FUNCTION XRNEJ

  PURE FUNCTION CJNEC(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=SWP), INTENT(IN) :: B
    LOGICAL :: CJNEC
    CJNEC = ((REAL(B) .NE. S_ZERO) .OR. (A%J .NE. AIMAG(B)))
  END FUNCTION CJNEC
  PURE FUNCTION ZJNEC(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=DWP), INTENT(IN) :: B
    LOGICAL :: ZJNEC
    ZJNEC = ((REAL(B) .NE. D_ZERO) .OR. (A%J .NE. AIMAG(B)))
  END FUNCTION ZJNEC
  PURE FUNCTION XJNEC(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=QWP), INTENT(IN) :: B
    LOGICAL :: XJNEC
    XJNEC = ((REAL(B) .NE. Q_ZERO) .OR. (A%J .NE. AIMAG(B)))
  END FUNCTION XJNEC

  PURE FUNCTION CCNEJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=SWP), INTENT(IN) :: A
    TYPE(CIMAGINARY), INTENT(IN) :: B
    LOGICAL :: CCNEJ
    CCNEJ = ((REAL(A) .NE. S_ZERO) .OR. (B%J .NE. AIMAG(A)))
  END FUNCTION CCNEJ
  PURE FUNCTION ZCNEJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=DWP), INTENT(IN) :: A
    TYPE(ZIMAGINARY), INTENT(IN) :: B
    LOGICAL :: ZCNEJ
    ZCNEJ = ((REAL(A) .NE. D_ZERO) .OR. (B%J .NE. AIMAG(A)))
  END FUNCTION ZCNEJ
  PURE FUNCTION XCNEJ(A,B)
    IMPLICIT NONE
    COMPLEX(KIND=QWP), INTENT(IN) :: A
    TYPE(XIMAGINARY), INTENT(IN) :: B
    LOGICAL :: XCNEJ
    XCNEJ = ((REAL(A) .NE. Q_ZERO) .OR. (B%J .NE. AIMAG(A)))
  END FUNCTION XCNEJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJLTJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: CJLTJ
    CJLTJ = (A%J .LT. B%J)
  END FUNCTION CJLTJ
  PURE FUNCTION ZJLTJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: ZJLTJ
    ZJLTJ = (A%J .LT. B%J)
  END FUNCTION ZJLTJ
  PURE FUNCTION XJLTJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: XJLTJ
    XJLTJ = (A%J .LT. B%J)
  END FUNCTION XJLTJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJLEJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: CJLEJ
    CJLEJ = (A%J .LE. B%J)
  END FUNCTION CJLEJ
  PURE FUNCTION ZJLEJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: ZJLEJ
    ZJLEJ = (A%J .LE. B%J)
  END FUNCTION ZJLEJ
  PURE FUNCTION XJLEJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: XJLEJ
    XJLEJ = (A%J .LE. B%J)
  END FUNCTION XJLEJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJGEJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: CJGEJ
    CJGEJ = (A%J .GE. B%J)
  END FUNCTION CJGEJ
  PURE FUNCTION ZJGEJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: ZJGEJ
    ZJGEJ = (A%J .GE. B%J)
  END FUNCTION ZJGEJ
  PURE FUNCTION XJGEJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: XJGEJ
    XJGEJ = (A%J .GE. B%J)
  END FUNCTION XJGEJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  PURE FUNCTION CJGTJ(A,B)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: CJGTJ
    CJGTJ = (A%J .GT. B%J)
  END FUNCTION CJGTJ
  PURE FUNCTION ZJGTJ(A,B)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: ZJGTJ
    ZJGTJ = (A%J .GT. B%J)
  END FUNCTION ZJGTJ
  PURE FUNCTION XJGTJ(A,B)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A,B
    LOGICAL :: XJGTJ
    XJGTJ = (A%J .GT. B%J)
  END FUNCTION XJGTJ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELEMENTAL FUNCTION CJABS(A)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    REAL(KIND=SWP) :: CJABS
    CJABS = ABS(A%J)
  END FUNCTION CJABS
  ELEMENTAL FUNCTION ZJABS(A)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=DWP) :: ZJABS
    ZJABS = ABS(A%J)
  END FUNCTION ZJABS
  ELEMENTAL FUNCTION XJABS(A)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    REAL(KIND=QWP) :: XJABS
    XJABS = ABS(A%J)
  END FUNCTION XJABS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELEMENTAL FUNCTION CJIMAG(A)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    REAL(KIND=SWP) :: CJIMAG
    CJIMAG = A%J
  END FUNCTION CJIMAG
  ELEMENTAL FUNCTION ZJIMAG(A)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=DWP) :: ZJIMAG
    ZJIMAG = A%J
  END FUNCTION ZJIMAG
  ELEMENTAL FUNCTION XJIMAG(A)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    REAL(KIND=QWP) :: XJIMAG
    XJIMAG = A%J
  END FUNCTION XJIMAG

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELEMENTAL FUNCTION CJCMPLX(A)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=SWP) :: CJCMPLX
    CJCMPLX = CMPLX(S_ZERO, A%J, SWP)
  END FUNCTION CJCMPLX
  ELEMENTAL FUNCTION ZJCMPLX(A)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=DWP) :: ZJCMPLX
    ZJCMPLX = CMPLX(D_ZERO, A%J, DWP)
  END FUNCTION ZJCMPLX
  ELEMENTAL FUNCTION XJCMPLX(A)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    COMPLEX(KIND=QWP) :: XJCMPLX
    XJCMPLX = CMPLX(Q_ZERO, A%J, QWP)
  END FUNCTION XJCMPLX

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELEMENTAL FUNCTION CJCONJG(A)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    TYPE(CIMAGINARY) :: CJCONJG
    CJCONJG%J = -A%J
  END FUNCTION CJCONJG
  ELEMENTAL FUNCTION ZJCONJG(A)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    TYPE(ZIMAGINARY) :: ZJCONJG
    ZJCONJG%J = -A%J
  END FUNCTION ZJCONJG
  ELEMENTAL FUNCTION XJCONJG(A)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    TYPE(XIMAGINARY) :: XJCONJG
    XJCONJG%J = -A%J
  END FUNCTION XJCONJG

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELEMENTAL FUNCTION CJINT(A)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    INTEGER :: CJINT
    CJINT = 0
  END FUNCTION CJINT
  ELEMENTAL FUNCTION ZJINT(A)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    INTEGER :: ZJINT
    ZJINT = 0
  END FUNCTION ZJINT
  ELEMENTAL FUNCTION XJINT(A)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    INTEGER :: XJINT
    XJINT = 0
  END FUNCTION XJINT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELEMENTAL FUNCTION CJREAL(A)
    IMPLICIT NONE
    TYPE(CIMAGINARY), INTENT(IN) :: A
    REAL(KIND=SWP) :: CJREAL
    CJREAL = S_ZERO
  END FUNCTION CJREAL
  ELEMENTAL FUNCTION ZJREAL(A)
    IMPLICIT NONE
    TYPE(ZIMAGINARY), INTENT(IN) :: A
    REAL(KIND=DWP) :: ZJREAL
    ZJREAL = D_ZERO
  END FUNCTION ZJREAL
  ELEMENTAL FUNCTION XJREAL(A)
    IMPLICIT NONE
    TYPE(XIMAGINARY), INTENT(IN) :: A
    REAL(KIND=QWP) :: XJREAL
    XJREAL = Q_ZERO
  END FUNCTION XJREAL

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE VN_IMAGINARY_F

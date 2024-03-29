MODULE VN_BLAS_F
  IMPLICIT NONE

  INTERFACE
     FUNCTION VN_BLAS_PREPARE() BIND(C,NAME='vn_blas_prepare')
       IMPLICIT NONE
       INTEGER :: VN_BLAS_PREPARE
     END FUNCTION VN_BLAS_PREPARE
  END INTERFACE

  INTERFACE
     FUNCTION VN_BLAS_FINISH(CURR, NBUF) BIND(C,NAME='vn_blas_finish')
       IMPLICIT NONE
       INTEGER, INTENT(OUT), TARGET :: CURR, NBUF
       INTEGER :: VN_BLAS_FINISH
     END FUNCTION VN_BLAS_FINISH
  END INTERFACE

  INTERFACE
     FUNCTION VN_BLAS_SET_NUM_THREADS(NT) BIND(C,NAME='vn_blas_set_num_threads')
       IMPLICIT NONE
       INTEGER, INTENT(IN), VALUE :: NT
       INTEGER :: VN_BLAS_SET_NUM_THREADS
     END FUNCTION VN_BLAS_SET_NUM_THREADS
  END INTERFACE

CONTAINS
END MODULE VN_BLAS_F

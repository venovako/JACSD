MODULE VN_CMPLXVIS_F
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  ! Please read ALL the comments below for details about the routines.

  INTERFACE
     FUNCTION VN_CMPLXVIS_START(ctx, fname, act, mA, nA, sx, sy, fname_len) BIND(C,name='vn_cmplxvis_start')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(OUT) :: ctx
       INTEGER, INTENT(IN), VALUE :: act, mA, nA, sx, sy, fname_len
       CHARACTER(KIND=c_char), INTENT(IN) :: fname(*)
       INTEGER :: VN_CMPLXVIS_START
     END FUNCTION VN_CMPLXVIS_START
  END INTERFACE

  INTERFACE
     FUNCTION VN_CMPLXVIS_FRAME(ctx, A, ldA) BIND(C,name='vn_cmplxvis_frame')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: ctx
       INTEGER, INTENT(IN), VALUE :: ldA
       COMPLEX(KIND=c_double), INTENT(IN) :: A(ldA,*)
       INTEGER :: VN_CMPLXVIS_FRAME
     END FUNCTION VN_CMPLXVIS_FRAME
  END INTERFACE

  INTERFACE
     FUNCTION VN_CMPLXVIS_STOP(ctx) BIND(C,name='vn_cmplxvis_stop')
       USE, INTRINSIC :: ISO_C_BINDING
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: ctx
       INTEGER :: VN_CMPLXVIS_STOP
     END FUNCTION VN_CMPLXVIS_STOP
  END INTERFACE

  ! Let B = A (the input matrix).
  INTEGER, PARAMETER :: VN_CMPLXVIS_OP_A   =  0
  ! Let B = A^H.
  INTEGER, PARAMETER :: VN_CMPLXVIS_OP_At  =  1
#ifndef VN_NO_BLAS
  ! Let B = A A^H.
  INTEGER, PARAMETER :: VN_CMPLXVIS_OP_AAh =  2
  ! Let B = A^H A.
  INTEGER, PARAMETER :: VN_CMPLXVIS_OP_AhA =  3
#endif

  ! Visualise the absolute value of B.
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Id     =  0
  ! Visualise log_2 of the absolute value of B.
  ! If the input is zero or subnormal, take -\infty.
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Lg     =  4
  ! Visualise log_e of the absolute value of B.
  ! If the input is zero or subnormal, take -\infty.
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Ln     =  8
  ! Visualise log_10 of the absolute value of B.
  ! If the input is zero or subnormal take -\infty.
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Log    = 12
  ! Visualise the phase of B in [0, 2\pi] instead of [-\pi, \pi].
  INTEGER, PARAMETER :: VN_CMPLXVIS_FN_Arg2PI = 16

  ! A temporary file is created to hold the frame dumps.

  ! Visualisation cannot proceed until the range of data in ALL frames is known,
  ! and is mapped to the chosen 256-colour (8-bit) palette.  The range and other
  ! needed metadata is written in the first 184 bytes of the dump file in ASCII
  ! form, so it is easily readable even when the rest of data is in binary form.

  ! With a given file prefix F, there has to be a palette file present in the
  ! current working directory, named F.plt and of the same form as examples in
  ! cmap/plt8 subdirectory here.  Each row contains three space-separated double
  ! precision values between 0 and 1, which are the amounts of red, green, and
  ! blue colour for the particular palette entry.  The examples provided are in
  ! principle the palettes available for the Matlab figures (jet, hsv, etc.).

  ! Palette colour 0 is reserved for negative infinites, and colour 255 for
  ! positive infinities and NaNs.  Apart from these special floating-point values,
  ! the data range is assumed to be finite, and is mapped to the remaining
  ! 254 palette colours (see normalise() function definition in vn_cmplxvis.c).

  ! The dump file is created in the current working directory, and is named
  ! F.dat.  It may be deleted once the bitmaps are created.

  ! The bitmaps for each frame are named FA%010d.bmp (the phase) and FR%010d.bmp
  ! (the absolute value), i.e., with a 0-padded 10-digit frame number.

CONTAINS
END MODULE VN_CMPLXVIS_F

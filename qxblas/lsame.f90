!> \brief \b LSAME
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       LOGICAL FUNCTION LSAME(CA,CB)
! 
!       .. Scalar Arguments ..
!       CHARACTER CA,CB
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> LSAME returns .TRUE. if CA is the same letter as CB regardless of
!> case.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] CA
!> \verbatim
!>          CA is CHARACTER*1
!> \endverbatim
!>
!> \param[in] CB
!> \verbatim
!>          CB is CHARACTER*1
!>          CA and CB specify the single characters to be compared.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee 
!> \author Univ. of California Berkeley 
!> \author Univ. of Colorado Denver 
!> \author NAG Ltd. 
!
!> \date November 2011
!
!> \ingroup aux_blas
!
!  =====================================================================
LOGICAL FUNCTION LSAME(CA,CB)
!
!  -- Reference BLAS level1 routine (version 3.1) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
!     .. Scalar Arguments ..
  CHARACTER, INTENT(IN) :: CA,CB
!     ..
!
! =====================================================================
!
!     .. Intrinsic Functions ..
  INTEGER, INTRINSIC :: ICHAR
!     ..
!     .. Local Scalars ..
  INTEGER :: INTA,INTB
!     ..
!
!     Test if the characters are equal
!
  LSAME = CA .EQ. CB
  IF (LSAME) RETURN
!
!     Now test for equivalence if both characters are alphabetic.
!
  INTA = ICHAR(CA)
  INTB = ICHAR(CB)
!
!     ASCII is assumed.
!
  IF ((INTA .GE. 97) .AND. (INTA .LE. 122)) INTA = INTA - 32
  IF ((INTB .GE. 97) .AND. (INTB .LE. 122)) INTB = INTB - 32
!
  LSAME = INTA .EQ. INTB
!
!     End of LSAME
!
END FUNCTION LSAME

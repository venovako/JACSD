!> \brief \b QISNAN tests input for NaN.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!> \htmlonly
!> Download QISNAN + dependencies 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/qisnan.f"> 
!> [TGZ]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/qisnan.f"> 
!> [ZIP]</a> 
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/qisnan.f"> 
!> [TXT]</a>
!> \endhtmlonly 
!
!  Definition:
!  ===========
!
!       LOGICAL FUNCTION QISNAN( Q )
! 
!       .. Scalar Arguments ..
!       REAL(WP)           Q
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> QISNAN returns .TRUE. if its argument is NaN, and .FALSE.
!> otherwise.  To be replaced by the Fortran 2003 intrinsic in the
!> future.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] Q
!> \verbatim
!>          Q is REAL(WP)
!>          Input to test for NaN.
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
!> \date September 2012
!
!> \ingroup auxOTHERauxiliary
!
!  =====================================================================
LOGICAL FUNCTION QISNAN(Q)
!
!  -- LAPACK auxiliary routine (version 3.4.2) --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     September 2012
!
  IMPLICIT NONE
  INCLUDE 'qx_wp.fi'
!     .. Scalar Arguments ..
  REAL(WP), INTENT(IN) :: Q
!     ..
!
!  =====================================================================
!
!  ..
!  .. Executable Statements ..

  QISNAN = (Q .NE. Q)

END FUNCTION QISNAN

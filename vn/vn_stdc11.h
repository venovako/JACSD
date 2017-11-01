#ifndef VN_STDC11_H
#define VN_STDC11_H

#ifndef VN_LIB_H
#error vn_stdc11.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#if (!defined(__STDC__) || (__STDC__ != 1))
#error NON-CONFORMING C IMPLEMENTATION
#endif /* __STDC__ */

#if (!defined(__STDC_HOSTED__) || (__STDC_HOSTED__ != 1))
#error NOT A HOSTED IMPLEMENTATION
#endif /* __STDC_HOSTED__ */

#if (__STDC_NO_COMPLEX__ == 1)
#error COMPLEX TYPES NOT SUPPORTED
#endif /* __STDC_NO_COMPLEX__ */

#ifdef VN_CHECK_IEC_559
#if (!defined(__STDC_IEC_559__) || (__STDC_IEC_559__ != 1))
#error IEC 60559 FLOATING-POINT ARITHMETIC NOT GUARANTEED
#endif /* __STDC_IEC_559__ */

#if (!defined(__STDC_IEC_559_COMPLEX__) || (__STDC_IEC_559_COMPLEX__ != 1))
#error IEC 60559 COMPLEX ARITHMETIC NOT GUARANTEED
#endif /* __STDC_IEC_559_COMPLEX__ */
#endif /* VN_CHECK_IEC_559 */

#if (__STDC_NO_VLA__ == 1)
#error VLA NOT SUPPORTED
#endif /* __STDC_NO_VLA__ */

#endif /* VN_STDC11_H */

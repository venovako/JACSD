/*
  VN_REAL_KIND == 4

 EPS:  1.192092896E-07
 MIN:  1.175494351E-38
 MAX:  3.402823466E+38
 INF:  INF            
-INF: -INF            

qNaN: NAN             , BITS(7FC00001)
QNaN: NAN             , BITS(FFC00002)
sNaN: NAN             , BITS(7F800003)
SNaN: NAN             , BITS(FF800004)

qNaN x QNaN: NAN             , BITS(7FC00001)
sNaN x SNaN: NAN             , BITS(7FC00003)

  VN_REAL_KIND == 8

 EPS:  2.22044604925031308E-16 
 MIN:  2.22507385850720138E-308
 MAX:  1.79769313486231571E+308
 INF:  INF                     
-INF: -INF                     

qNaN: NAN                      , BITS(7FF8000000000001)
QNaN: NAN                      , BITS(FFF8000000000002)
sNaN: NAN                      , BITS(7FF0000000000003)
SNaN: NAN                      , BITS(FFF0000000000004)

qNaN x QNaN: NAN                      , BITS(7FF8000000000001)
sNaN x SNaN: NAN                      , BITS(7FF8000000000003)

  VN_REAL_KIND == 10

 EPS:  1.084202172485504434007E-19  
 MIN:  3.362103143112093506263E-4932
 MAX:  1.189731495357231765021E+4932
 INF:  INF                          
-INF: -INF                          

qNaN: NAN                           , HI(7FFF), LO(C000000000000001)
QNaN: NAN                           , HI(FFFF), LO(C000000000000002)
sNaN: NAN                           , HI(7FFF), LO(8000000000000003)
SNaN: NAN                           , HI(FFFF), LO(8000000000000004)

qNaN x QNaN: NAN                           , HI(FFFF), LO(C000000000000002)
sNaN x SNaN: NAN                           , HI(FFFF), LO(C000000000000004)
*/

#include "vn_lib.h"

#ifdef VN_TEST
/*
  On MacOS (Intel64) + Clang, signalling NaNs are quieted.
*/
int main(int argc VN_VAR_UNUSED, char *argv[] VN_VAR_UNUSED)
{
  const vn_real
    eps = VN_REAL_EPS,
    mi  = VN_REAL_MIN,
    mx  = VN_REAL_MAX,
    pi  = MkNaN(VN_FALSE, VN_FALSE, 0),
    ni  = MkNaN(VN_TRUE,  VN_FALSE, 0),
    pqn = MkNaN(VN_FALSE, VN_TRUE,  1),
    nqn = MkNaN(VN_TRUE,  VN_TRUE,  2),
    psn = MkNaN(VN_FALSE, VN_FALSE, 3),
    nsn = MkNaN(VN_TRUE,  VN_FALSE, 4);
  vn_real_view view;

#if (4 == VN_REAL_KIND)
  (void)printf(" EPS: " VN_REAL_FMT "\n", eps);
  (void)printf(" MIN: " VN_REAL_FMT "\n", mi);
  (void)printf(" MAX: " VN_REAL_FMT "\n", mx);
  (void)printf(" INF: " VN_REAL_FMT "\n", pi);
  (void)printf("-INF: " VN_REAL_FMT "\n", ni);
  (void)printf("\n");
  view.whole = pqn;
  (void)printf("qNaN: " VN_REAL_FMT ", BITS(%X)\n", pqn, view.bits);
  view.whole = nqn;
  (void)printf("QNaN: " VN_REAL_FMT ", BITS(%X)\n", nqn, view.bits);
  view.whole = psn;
  (void)printf("sNaN: " VN_REAL_FMT ", BITS(%X)\n", psn, view.bits);
  view.whole = nsn;
  (void)printf("SNaN: " VN_REAL_FMT ", BITS(%X)\n", nsn, view.bits);
  (void)printf("\n");
  const vn_real
    qnxqn = pqn * nqn;
  view.whole = qnxqn;
  (void)printf("qNaN x QNaN: " VN_REAL_FMT ", BITS(%X)\n", qnxqn, view.bits);
  const vn_real
    snxsn = psn * nsn;
  view.whole = snxsn;
  (void)printf("sNaN x SNaN: " VN_REAL_FMT ", BITS(%X)\n", snxsn, view.bits);
#elif (8 == VN_REAL_KIND)
  (void)printf(" EPS: " VN_REAL_FMT "\n", eps);
  (void)printf(" MIN: " VN_REAL_FMT "\n", mi);
  (void)printf(" MAX: " VN_REAL_FMT "\n", mx);
  (void)printf(" INF: " VN_REAL_FMT "\n", pi);
  (void)printf("-INF: " VN_REAL_FMT "\n", ni);
  (void)printf("\n");
  view.whole = pqn;
  (void)printf("qNaN: " VN_REAL_FMT ", BITS(%llX)\n", pqn, view.bits);
  view.whole = nqn;
  (void)printf("QNaN: " VN_REAL_FMT ", BITS(%llX)\n", nqn, view.bits);
  view.whole = psn;
  (void)printf("sNaN: " VN_REAL_FMT ", BITS(%llX)\n", psn, view.bits);
  view.whole = nsn;
  (void)printf("SNaN: " VN_REAL_FMT ", BITS(%llX)\n", nsn, view.bits);
  (void)printf("\n");
  const vn_real
    qnxqn = pqn * nqn;
  view.whole = qnxqn;
  (void)printf("qNaN x QNaN: " VN_REAL_FMT ", BITS(%llX)\n", qnxqn, view.bits);
  const vn_real
    snxsn = psn * nsn;
  view.whole = snxsn;
  (void)printf("sNaN x SNaN: " VN_REAL_FMT ", BITS(%llX)\n", snxsn, view.bits);
#elif (10 == VN_REAL_KIND)
  (void)printf(" EPS: " VN_REAL_FMT "\n", eps);
  (void)printf(" MIN: " VN_REAL_FMT "\n", mi);
  (void)printf(" MAX: " VN_REAL_FMT "\n", mx);
  (void)printf(" INF: " VN_REAL_FMT "\n", pi);
  (void)printf("-INF: " VN_REAL_FMT "\n", ni);
  (void)printf("\n");
  view.whole = pqn;
  (void)printf("qNaN: " VN_REAL_FMT ", HI(%hX), LO(%llX)\n", pqn, view.bits.hi, view.bits.lo);
  view.whole = nqn;
  (void)printf("QNaN: " VN_REAL_FMT ", HI(%hX), LO(%llX)\n", nqn, view.bits.hi, view.bits.lo);
  view.whole = psn;
  (void)printf("sNaN: " VN_REAL_FMT ", HI(%hX), LO(%llX)\n", psn, view.bits.hi, view.bits.lo);
  view.whole = nsn;
  (void)printf("SNaN: " VN_REAL_FMT ", HI(%hX), LO(%llX)\n", nsn, view.bits.hi, view.bits.lo);
  (void)printf("\n");
  const vn_real
    qnxqn = pqn * nqn;
  view.whole = qnxqn;
  (void)printf("qNaN x QNaN: " VN_REAL_FMT ", HI(%hX), LO(%llX)\n", qnxqn, view.bits.hi, view.bits.lo);
  const vn_real
    snxsn = psn * nsn;
  view.whole = snxsn;
  (void)printf("sNaN x SNaN: " VN_REAL_FMT ", HI(%hX), LO(%llX)\n", snxsn, view.bits.hi, view.bits.lo);
#else /* unsupported */
#error VN_REAL_KIND must be one of { 4, 8, 10 }
#endif /* ?VN_REAL_KIND */

  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
#if (4 == VN_REAL_KIND)
/* If payload == 0 and quiet == VN_FALSE, will generate +/-Infinity instead. */
vn_real MkNaN(const vn_logical sgn, const vn_logical quiet, const vn_integer_4 payload)
{
  vn_real_view ret;
  ret.whole = (sgn ? -VN_REAL_INF : VN_REAL_INF);
  if (quiet)
    ret.parts.mnt |= UINT32_C(0x00400000);
  ret.parts.mnt |= (uint32_t)(payload & INT32_C(0x003FFFFF));
  return ret.whole;
}
vn_real MqNaN(const vn_integer_4 payload)
{
  return MkNaN((payload < INT32_C(0)), VN_TRUE, payload);
}
vn_real MsNaN(const vn_integer_4 payload)
{
  return MkNaN((payload < INT32_C(0)), VN_FALSE, payload);
}
#elif (8 == VN_REAL_KIND)
/* If payload == 0 and quiet == VN_FALSE, will generate +/-Infinity instead. */
vn_real MkNaN(const vn_logical sgn, const vn_logical quiet, const vn_integer_8 payload)
{
  vn_real_view ret;
  ret.whole = (sgn ? -VN_REAL_INF : VN_REAL_INF);
  if (quiet)
    ret.parts.mnt |= UINT64_C(0x0008000000000000);
  ret.parts.mnt |= (uint64_t)(payload & INT64_C(0x0007FFFFFFFFFFFF));
  return ret.whole;
}
vn_real MqNaN(const vn_integer_8 payload)
{
  return MkNaN((payload < INT64_C(0)), VN_TRUE, payload);
}
vn_real MsNaN(const vn_integer_8 payload)
{
  return MkNaN((payload < INT64_C(0)), VN_FALSE, payload);
}
#elif (10 == VN_REAL_KIND)
/* For quiet == VN_TRUE, if payload >= 0, will generate +/-Indefinite. */
vn_real MkNaN(const vn_logical sgn, const vn_logical quiet, const vn_integer_8 payload)
{
  vn_real_view ret;
  ret.whole = (sgn ? -VN_REAL_INF : VN_REAL_INF);
  if (quiet)
    ret.parts.mnt |= UINT64_C(0x4000000000000000);
  ret.parts.mnt |= (uint64_t)(payload & INT64_C(0x3FFFFFFFFFFFFFFF));
  return ret.whole;
}
vn_real MqNaN(const vn_integer_8 payload)
{
  return MkNaN((payload < INT64_C(0)), VN_TRUE, payload);
}
vn_real MsNaN(const vn_integer_8 payload)
{
  return MkNaN((payload < INT64_C(0)), VN_FALSE, payload);
}
#else /* unsupported */
#error VN_REAL_KIND must be one of { 4, 8, 10 }
#endif /* ?VN_REAL_KIND */
#endif /* ?VN_TEST */

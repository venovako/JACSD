/*
  VN_REAL_KIND == 4

 EPS:  1.192092896E-07
 MIN:  1.175494351E-38
 MAX:  3.402823466E+38
 INF:  INF
-INF: -INF

qNaN: NAN, BITS(7FC00001)
QNaN: NAN, BITS(FFC00002)
sNaN: NAN, BITS(7F800003)
SNaN: NAN, BITS(FF800004)

qNaN x QNaN: NAN, BITS(FFC00002)
sNaN x SNaN: NAN, BITS(FFC00004)

  VN_REAL_KIND == 8

 EPS:  2.22044604925031308E-016
 MIN:  2.22507385850720138E-308
 MAX:  1.79769313486231571E+308
 INF:  INF
-INF: -INF

qNaN: NAN, BITS(7FF8000000000001)
QNaN: NAN, BITS(FFF8000000000002)
sNaN: NAN, BITS(7FF0000000000003)
SNaN: NAN, BITS(FFF0000000000004)

qNaN x QNaN: NAN, BITS(FFF8000000000002)
sNaN x SNaN: NAN, BITS(FFF8000000000004)

  VN_REAL_KIND == 10

 EPS:  1.084202172485504434007E-0019
 MIN:  3.362103143112093506263E-4932
 MAX:  1.189731495357231765021E+4932
 INF:  INF
-INF: -INF

qNaN: NAN, HI(7FFF), LO(C000000000000001)
QNaN: NAN, HI(FFFF), LO(C000000000000002)
sNaN: NAN, HI(7FFF), LO(8000000000000003)
SNaN: NAN, HI(FFFF), LO(8000000000000004)

qNaN x QNaN: NAN, HI(FFFF), LO(C000000000000002)
sNaN x SNaN: NAN, HI(FFFF), LO(C000000000000004)
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
  (void)printf(" EPS: %s\n", vn_realtostr(eps));
  (void)printf(" MIN: %s\n", vn_realtostr(mi));
  (void)printf(" MAX: %s\n", vn_realtostr(mx));
  (void)printf(" INF: %s\n", vn_realtostr(pi));
  (void)printf("-INF: %s\n", vn_realtostr(ni));
  (void)printf("\n");
  view.whole = pqn;
  (void)printf("qNaN: %s, BITS(%X)\n", vn_realtostr(pqn), view.bits);
  view.whole = nqn;
  (void)printf("QNaN: %s, BITS(%X)\n", vn_realtostr(nqn), view.bits);
  view.whole = psn;
  (void)printf("sNaN: %s, BITS(%X)\n", vn_realtostr(psn), view.bits);
  view.whole = nsn;
  (void)printf("SNaN: %s, BITS(%X)\n", vn_realtostr(nsn), view.bits);
  (void)printf("\n");
  const vn_real
    qnxqn = pqn * nqn;
  view.whole = qnxqn;
  (void)printf("qNaN x QNaN: %s, BITS(%X)\n", vn_realtostr(qnxqn), view.bits);
  const vn_real
    snxsn = psn * nsn;
  view.whole = snxsn;
  (void)printf("sNaN x SNaN: %s, BITS(%X)\n", vn_realtostr(snxsn), view.bits);
#elif (8 == VN_REAL_KIND)
  (void)printf(" EPS: %s\n", vn_realtostr(eps));
  (void)printf(" MIN: %s\n", vn_realtostr(mi));
  (void)printf(" MAX: %s\n", vn_realtostr(mx));
  (void)printf(" INF: %s\n", vn_realtostr(pi));
  (void)printf("-INF: %s\n", vn_realtostr(ni));
  (void)printf("\n");
  view.whole = pqn;
  (void)printf("qNaN: %s, BITS(%llX)\n", vn_realtostr(pqn), view.bits);
  view.whole = nqn;
  (void)printf("QNaN: %s, BITS(%llX)\n", vn_realtostr(nqn), view.bits);
  view.whole = psn;
  (void)printf("sNaN: %s, BITS(%llX)\n", vn_realtostr(psn), view.bits);
  view.whole = nsn;
  (void)printf("SNaN: %s, BITS(%llX)\n", vn_realtostr(nsn), view.bits);
  (void)printf("\n");
  const vn_real
    qnxqn = pqn * nqn;
  view.whole = qnxqn;
  (void)printf("qNaN x QNaN: %s, BITS(%llX)\n", vn_realtostr(qnxqn), view.bits);
  const vn_real
    snxsn = psn * nsn;
  view.whole = snxsn;
  (void)printf("sNaN x SNaN: %s, BITS(%llX)\n", vn_realtostr(snxsn), view.bits);
#elif (10 == VN_REAL_KIND)
  (void)printf(" EPS: %s\n", vn_realtostr(eps));
  (void)printf(" MIN: %s\n", vn_realtostr(mi));
  (void)printf(" MAX: %s\n", vn_realtostr(mx));
  (void)printf(" INF: %s\n", vn_realtostr(pi));
  (void)printf("-INF: %s\n", vn_realtostr(ni));
  (void)printf("\n");
  view.whole = pqn;
  (void)printf("qNaN: %s, HI(%hX), LO(%llX)\n", vn_realtostr(pqn), view.bits.hi, view.bits.lo);
  view.whole = nqn;
  (void)printf("QNaN: %s, HI(%hX), LO(%llX)\n", vn_realtostr(nqn), view.bits.hi, view.bits.lo);
  view.whole = psn;
  (void)printf("sNaN: %s, HI(%hX), LO(%llX)\n", vn_realtostr(psn), view.bits.hi, view.bits.lo);
  view.whole = nsn;
  (void)printf("SNaN: %s, HI(%hX), LO(%llX)\n", vn_realtostr(nsn), view.bits.hi, view.bits.lo);
  (void)printf("\n");
  const vn_real
    qnxqn = pqn * nqn;
  view.whole = qnxqn;
  (void)printf("qNaN x QNaN: %s, HI(%hX), LO(%llX)\n", vn_realtostr(qnxqn), view.bits.hi, view.bits.lo);
  const vn_real
    snxsn = psn * nsn;
  view.whole = snxsn;
  (void)printf("sNaN x SNaN: %s, HI(%hX), LO(%llX)\n", vn_realtostr(snxsn), view.bits.hi, view.bits.lo);
#else /* unsupported */
#error VN_REAL_KIND must be one of { 4, 8, 10 }
#endif /* ?VN_REAL_KIND */

  return EXIT_SUCCESS;
}
#else /* !VN_TEST */
char *vn_realtostr(const vn_real x)
{
  static __thread char s[VN_REAL_WID + 1];
  int l = sprintf(s, VN_REAL_FMT, x);
  if (l <= 0)
    return (char*)NULL;
  char *d = s + VN_REAL_WID;
  for (--d; isblank(*d); --d)
    *d = '\0';
  char *e = strrchr(s, 'E');
  if (!e)
    return s;
  e += 2;
  l = (int)(strchr(e, '\0') - e);
  if (l >= VN_REAL_EDG)
    return s;
  d = s + VN_REAL_WID;
  e += l;
  for (int i = 0; i < l; ++i)
    *--d = *--e;
  for (--d; isdigit(*d); --d)
    *d = '0';
  return s;
}
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

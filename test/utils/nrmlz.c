#include "nrmlz.h"

#include <assert.h>
#include <complex.h>
#include <math.h>

void cnrmlz_(float _Complex *const c_out, const float _Complex *const c_in)
{
  assert(c_out);
  assert(c_in);
  const long double c_in_re = (long double)crealf(*c_in);
  const long double c_in_im = (long double)cimagf(*c_in);
  const long double magnl = hypotl(c_in_re, c_in_im);
  float *const c_out_f = (float*)c_out;
  c_out_f[0] = (float)(c_in_re / magnl);
  c_out_f[1] = (float)(c_in_im / magnl);
}

void znrmlz_(double _Complex *const z_out, const double _Complex *const z_in)
{
  assert(z_out);
  assert(z_in);
  const long double z_in_re = (long double)creal(*z_in);
  const long double z_in_im = (long double)cimag(*z_in);
  const long double magnl = hypotl(z_in_re, z_in_im);
  double *const z_out_d = (double*)z_out;
  z_out_d[0] = (double)(z_in_re / magnl);
  z_out_d[1] = (double)(z_in_im / magnl);
}

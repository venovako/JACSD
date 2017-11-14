#include "nrmlz.h"

#include <assert.h>
#include <complex.h>
#include <math.h>

float _Complex cnrmlz_(const float _Complex *const z)
{
  assert(z);
  const long double z_re = (long double)crealf(*z);
  const long double z_im = (long double)cimagf(*z);
  const long double magn = hypotl(z_re, z_im);
  return ((float)(z_re / magn) + _Complex_I * (float)(z_im / magn));
}

double _Complex znrmlz_(const double _Complex *const z)
{
  assert(z);
  const long double z_re = (long double)creal(*z);
  const long double z_im = (long double)cimag(*z);
  const long double magn = hypotl(z_re, z_im);
  return ((double)(z_re / magn) + _Complex_I * (double)(z_im / magn));
}

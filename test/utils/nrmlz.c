#include "nrmlz.h"

#include <complex.h>
#include <math.h>

float _Complex cnrmlz_(const float _Complex z[static 1])
{
  const long double z_re = (long double)crealf(*z);
  const long double z_im = (long double)cimagf(*z);
  const long double magn = hypotl(z_re, z_im);
  return ((float)(z_re / magn) + (float)(z_im / magn) * _Complex_I);
}

double _Complex znrmlz_(const double _Complex z[static 1])
{
  const long double z_re = (long double)creal(*z);
  const long double z_im = (long double)cimag(*z);
  const long double magn = hypotl(z_re, z_im);
  return ((double)(z_re / magn) + (double)(z_im / magn) * _Complex_I);
}

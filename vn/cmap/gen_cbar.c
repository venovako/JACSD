/* Linux: clang -DNDEBUG -D_GNU_SOURCE -I.. gen_cbar.c -L../.. -lvn -lm */
/* macOS: clang -DNDEBUG               -I.. gen_cbar.c -L../.. -lvn -lm */

#ifndef _GNU_SOURCE
#define exp10 __exp10
#endif // !_GNU_SOURCE
#include "vn_lib.h"

typedef double (*pfn)(double);

static double id(double x)
{
  return x;
}

int main(int argc, char *argv[])
{
  if ((argc < 9) || (argc > 10)) {
    (void)fprintf(stderr, "%s min_val max_val n_bars width height plt bmp id|lg|ln|log [fmt]\n", argv[0]);
    return EXIT_FAILURE;
  }
  char *endptr = (char*)NULL;
  const double min_val = strtod(argv[1], &endptr);
#ifndef NDEBUG
  (void)fprintf(stdout, "min_val = %#+.17e\n", min_val);
#endif // !NDEBUG
  if (*endptr || !isfinite(min_val))
    return EXIT_FAILURE;
  const double max_val = strtod(argv[2], &endptr);
#ifndef NDEBUG
  (void)fprintf(stdout, "max_val = %#+.17e\n", max_val);
#endif // !NDEBUG
  if (*endptr || !isfinite(max_val))
    return EXIT_FAILURE;
  if (max_val < min_val) {
    (void)fprintf(stderr, "max_val < min_val\n");
    return EXIT_FAILURE;
  }
  const long n_barsl = strtol(argv[3], &endptr, 0);
#ifndef NDEBUG
  (void)fprintf(stdout, "n_bars = %ld\n", n_barsl);
#endif // !NDEBUG
  if (*endptr)
    return EXIT_FAILURE;
  // should be n_barsl <= 0L, but for now handle only the non-degenerate case
  if (n_barsl <= 1L) {
    (void)fprintf(stderr, "n_bars <= 1\n");
    return EXIT_FAILURE;
  }
  const unsigned n_bars = (unsigned)n_barsl;
  if (256u % n_bars) {
    (void)fprintf(stderr, "256 %% n_bars != 0\n");
    return EXIT_FAILURE;
  }
  const unsigned n_colors = ((max_val == min_val) ? 1u : 254u);
#ifndef NDEBUG
  (void)fprintf(stdout, "n_colors = %u\n", n_colors);
#endif // !NDEBUG
  if (n_colors < n_bars) {
    (void)fprintf(stderr, "n_colors < n_bars\n");
    return EXIT_FAILURE;
  }

  const long widthl = strtol(argv[4], &endptr, 0);
#ifndef NDEBUG
  (void)fprintf(stdout, "width = %ld\n", widthl);
#endif // !NDEBUG
  if (*endptr)
    return EXIT_FAILURE;
  if (widthl <= 0L) {
    (void)fprintf(stderr, "width <= 0\n");
    return EXIT_FAILURE;
  }
  const unsigned width = (unsigned)widthl;
  const long heightl = strtol(argv[5], &endptr, 0);
#ifndef NDEBUG
  (void)fprintf(stdout, "height = %ld\n", heightl);
#endif // !NDEBUG
  if (*endptr)
    return EXIT_FAILURE;
  if (heightl < n_barsl) {
    (void)fprintf(stderr, "height <= n_bars\n");
    return EXIT_FAILURE;
  }
  if (heightl < widthl) {
    (void)fprintf(stderr, "height < width\n");
    return EXIT_FAILURE;
  }
  if (heightl % n_barsl) {
    (void)fprintf(stderr, "height %% n_bars != 0\n");
    return EXIT_FAILURE;
  }
  const unsigned height = (unsigned)heightl;

  const char *const plt = argv[6];
#ifndef NDEBUG
  (void)fprintf(stdout, "plt = %s\n", plt);
#endif // !NDEBUG
  const char *const bmp = argv[7];
#ifndef NDEBUG
  (void)fprintf(stdout, "bmp = %s\n", bmp);
#endif // !NDEBUG

  pfn fn = id;
  if (!strcmp(argv[8], "lg"))
    fn = exp2;
  else if (!strcmp(argv[8], "ln"))
    fn = exp;
  else if (!strcmp(argv[8], "log"))
    fn = exp10;
  else if (strcmp(argv[8], "id")) {
    (void)fprintf(stderr, "Scale not one of id, lg (base 2), ln (base e), log (base 10)!\n");
    return EXIT_FAILURE;
  }

  const char *const fmt = ((argc == 10) ? argv[9] : "%#+.17e");

  vn_bmp_t cbar = (vn_bmp_t)NULL;
  if (vn_bmp_create(&cbar, height, height, 8u))
    return EXIT_FAILURE;
  if (vn_bmp_read_cmap(cbar, plt))
    return EXIT_FAILURE;
  const vn_bmp_pixel_setter_t ps = vn_bmp_get_pixel_setter(cbar);
  if (!ps)
    return EXIT_FAILURE;

  if (vn_bmp_set_palette_color(cbar, 0u, ~0u))
    return EXIT_FAILURE;
  if (vn_bmp_set_palette_color(cbar, 255u, 0u))
    return EXIT_FAILURE;

  if (n_colors == 1u) {
    // TODO: complete the degenerate case
    for (int y = 0; y < height; ++y)
      for (unsigned x = 0u; x < width; ++x)
        ps(cbar, x, y, 127u);
  }
  else {
    const unsigned color_h = height / 256u;
    unsigned y = 0u;
    for (int c = 255; c >= 0; --c)
      for (unsigned y_ = 0u; y_ < color_h; ++y, ++y_)
        for (unsigned x = 0u; x < width; ++x)
          ps(cbar, x, y, (unsigned)(c ? c : 255));
    for (y = 0u; y < height; ++y)
      for (unsigned x = width - color_h; x < width; ++x)
        ps(cbar, x, y, 255u);
    for (y = 0u; y < height; ++y)
      for (unsigned x = 0u; x < color_h; ++x)
        ps(cbar, x, y, 255u);
    const unsigned bar_h = height / n_bars;
    for (y = 0u; y < height; y += bar_h)
      for (unsigned y_ = 0u; y_ < color_h; ++y_)
        for (unsigned x = color_h; x < width; x += 2u * color_h)
          for (unsigned x_ = 0u; x_ < color_h; ++x_)
            if (((x + x_) < width) && ((y + y_) < height))
              ps(cbar, x + x_, y + y_, 255u);
    const unsigned spc_x = width + color_h;
    (void)fprintf(stdout, "convert %s -font Courier-Bold -pointsize %u ", bmp, bar_h);
    (void)fprintf(stdout, "-annotate +%u+%u \'<= ", spc_x, (bar_h - color_h));
    (void)fprintf(stdout, fmt, fn(max_val));
    (void)fprintf(stdout, "\' ");
    const unsigned n_bars_1 = n_bars - 1u;
    const unsigned cpb = 256 / n_bars;
    const double wid = max_val - min_val;
    for (unsigned b = 1u; b < n_bars_1; ++b) {
      const unsigned c = 255u - b * cpb;
      const double val = fn(fma((fma(0.5, nextafter(1.0, 0.5), (c - 1.0)) / 253.0), wid, min_val));
      (void)fprintf(stdout, "-annotate +%u+%u \'<= ", spc_x, ((b + 1u) * bar_h - color_h));
      (void)fprintf(stdout, fmt, val);
      (void)fprintf(stdout, "\' ");
    }
    (void)fprintf(stdout, "-annotate +%u+%u \'>= ", spc_x, (height - color_h));
    (void)fprintf(stdout, fmt, fn(min_val));
    (void)fprintf(stdout, "\' A%s\n", bmp);
  }

  if (vn_bmp_fwrite(cbar, bmp))
    return EXIT_FAILURE;
  if (vn_bmp_destroy(&cbar))
    return EXIT_FAILURE;

  return EXIT_SUCCESS;
}

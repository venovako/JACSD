#ifndef VN_BMP_H
#define VN_BMP_H

#ifndef VN_LIB_H
#error vn_bmp.h not intended for direct inclusion
#endif /* !VN_LIB_H */

#ifndef VN_RGB_24
#define VN_RGB_24(r, g, b) (((((r) << 8u) | (g)) << 8u) | (b))
#else /* VN_RGB_24 */
#error VN_RGB_24 already defined
#endif /* !VN_RGB_24 */

typedef struct
{
  uint32_t f_size;
  uint16_t reserved[2]; /* { 0u, 0u } */
  uint32_t i_offset;
  uint32_t d_size; /* 40u = BMPINFOHEADER */
  uint32_t width;
  int32_t height;
  uint16_t planes; /* 1u */
  uint16_t bpp;
  uint32_t compression; /* 0u */
  uint32_t i_size;
  int32_t h_ppm; /* 0 */
  int32_t v_ppm; /* 0 */
  uint32_t c_palette; /* may be 0u if implied */
  uint32_t i_colors; /* 0u */
} vn_bmp_header_t;

typedef union
{
  uint32_t c;
  struct {
    uint8_t b;
    uint8_t g;
    uint8_t r;
    uint8_t a;
  } chan;
} vn_bmp_palette_entry_t;

typedef struct
{
  vn_bmp_palette_entry_t *palette;
  uint8_t *image;
  uint32_t i_ldaB;
  vn_bmp_header_t header;
} *vn_bmp_t;

typedef void (*vn_bmp_pixel_setter_t)(const vn_bmp_t bmp, const uint32_t x, const uint32_t y, const uint32_t c);
typedef uint32_t (*vn_bmp_pixel_getter_t)(const vn_bmp_t bmp, const uint32_t x, const uint32_t y);

VN_EXTERN_C vn_integer vn_bmp_create(vn_bmp_t *const bmp, const uint32_t width, const int32_t height, const uint16_t bpp);
VN_EXTERN_C vn_integer vn_bmp_destroy(vn_bmp_t *const bmp);

VN_EXTERN_C vn_integer vn_bmp_set_palette_color(const vn_bmp_t bmp, const uint32_t ix, const uint32_t c);
VN_EXTERN_C vn_integer vn_bmp_get_palette_color(const vn_bmp_t bmp, const uint32_t ix, uint32_t *const c);

VN_EXTERN_C vn_bmp_pixel_setter_t vn_bmp_get_pixel_setter(const vn_bmp_t bmp);
VN_EXTERN_C vn_bmp_pixel_getter_t vn_bmp_get_pixel_getter(const vn_bmp_t bmp);

VN_EXTERN_C vn_integer vn_bmp_fwrite(const vn_bmp_t bmp, const char *const fn);
VN_EXTERN_C vn_integer vn_bmp_read_cmap(const vn_bmp_t bmp, const char *const fn);

#endif /* !VN_BMP_H */

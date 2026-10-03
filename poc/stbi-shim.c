/* stbi-shim.c — the whole C side of the image previews, around stb_image.h.
 *
 * poc/image.lisp decodes PNG, GIF, BMP, PNM, QOI, TGA, ICO and WebP-lossless in
 * Lisp. JPEG is the one format where a from-scratch Lisp decoder would be a large,
 * slow, hard-to-get-right project (progressive scans, chroma upsampling, a fast
 * IDCT), and stb_image already does it in one public-domain header that needs
 * nothing but libc and libm — both of which the image ships anyway. So this file
 * compiles stb_image with ONLY the formats we want from it, and exposes four plain
 * functions with C types Lisp's SB-ALIEN can call without any struct definitions.
 * Rationale and costs: doc/previews.org, "Why stb_image for JPEG".
 *
 * Memory contract: lol_stbi_load returns a malloc'd RGBA buffer (4 bytes/pixel,
 * row-major, top-down); the caller copies it into Lisp and hands it back to
 * lol_stbi_free. The input is a pinned Lisp octet vector, read only.
 *
 * Build: poc/build-stbi.sh OUTDIR  (gcc -O2 -shared -fPIC, links libc + libm only).
 */

#define STBI_ONLY_JPEG
#define STBI_ONLY_PSD
#define STBI_ONLY_HDR
#define STBI_ONLY_PIC
#define STBI_NO_STDIO            /* Lisp reads the file; we only see memory      */
#define STBI_NO_THREAD_LOCALS    /* one caller at a time (PID 1's event loop)    */
#define STBI_MAX_DIMENSIONS 16384
#define STB_IMAGE_STATIC         /* keep stb's own symbols out of the .so's API  */
#define STB_IMAGE_IMPLEMENTATION
#include "vendor/stb_image.h"

#define API __attribute__((visibility("default")))

/* Width, height and channel count without decoding — so Lisp can refuse a
   40-megapixel bomb before any memory is spent. Returns 1 on success. */
API int lol_stbi_info(const unsigned char *buf, int len, int *w, int *h, int *comp)
{
    return stbi_info_from_memory(buf, len, w, h, comp);
}

/* Decode to RGBA8, whatever the source. COMP receives the source's own channel
   count (2 or 4 = it had alpha). NULL on failure; see lol_stbi_reason. */
API unsigned char *lol_stbi_load(const unsigned char *buf, int len,
                                 int *w, int *h, int *comp)
{
    return stbi_load_from_memory(buf, len, w, h, comp, 4);
}

API void lol_stbi_free(void *p) { stbi_image_free(p); }

/* stb's terse reason for the last failure ("bad huffman code", ...). */
API const char *lol_stbi_reason(void) { return stbi_failure_reason(); }

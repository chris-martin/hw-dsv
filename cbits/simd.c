#include "simd.h"

#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

typedef uint8_t v32si __attribute__ ((vector_size (32)));

char hexdigit[] = "0123456789abcdef";

void print_bits(uint64_t word) {
  putc('|', stdout);
  for (size_t i = 0; i < 64; ++i) {
    if (i == 32) {
      putc(' ', stdout);
    }
    putc((word & (1L << i)) ? '1' : '0', stdout);
  }
  printf("|\n");
}

void print256(char *label, __m256i var) {
  uint8_t *v = (uint8_t*)&var;
  printf("%s: ", label);
  
  for (size_t i = 0; i < 32; ++i) {
    printf("%c", hexdigit[(int)(0xf & (v[i] >> 0))]);
    printf("%c", hexdigit[(int)(0xf & (v[i] >> 4))]);

    if (i % 2 == 1) {
      printf(" ");
    }
  }

  printf("\n");
}

void print32(char *label, uint32_t word) {
  printf("%s: ", label);

  for (size_t i = 0; i < 32; ++i) {
    printf((word & (1 << i)) ? "1" : "0");
    printf(" ");

    if (i % 2 == 1) {
      printf(" ");
    }
  }

  printf("\n");
}

uint64_t build_ibs(
    uint8_t delimiter,
    uint64_t quote_count,
    char *in,
    size_t len,
    uint64_t *out_markers,
    uint64_t *out_newlines) {
  // printf("--------------------------------------------------------------------------------\n");
  // printf("len: %zu, quote_count: %llu, delimiter: %c\n", len, quote_count, delimiter);

  if (len % 64 != 0) {
    printf("Invalid length: %zu\n", len);
    return 0xffffffff;
  }

  v32si ws_newlines   = _mm256_set1_epi8('\n');
  v32si ws_delimiters = _mm256_set1_epi8(delimiter);

  uint64_t *tmp_delimiters  = (uint64_t *)malloc(len / 8);
  uint64_t *tmp_quote_mask  = (uint64_t *)malloc(len / 8);
  uint64_t *tmp_quotes      = (uint64_t *)malloc(len / 8);

  for (size_t i = 0; i < len / 64; ++i) {
    uint64_t lo_quote_toggle = quote_count & 1;
    // printf("lo_quote_toggle: %llu\n", lo_quote_toggle);

    v32si ws_quotes     = _mm256_set1_epi8('"');

    v32si lo_s = *(v32si*)(in + (i * 64));
    v32si lo_rb_newlines    = _mm256_cmpeq_epi8(lo_s, ws_newlines);
    v32si lo_rb_delimiters  = _mm256_cmpeq_epi8(lo_s, ws_delimiters);
    v32si lo_rb_quotes      = _mm256_cmpeq_epi8(lo_s, ws_quotes);

    uint32_t lo_rib_newlines    = (uint32_t)_mm256_movemask_epi8(lo_rb_newlines);
    uint32_t lo_rib_delimiters  = (uint32_t)_mm256_movemask_epi8(lo_rb_delimiters);
    uint32_t lo_rib_quotes      = (uint32_t)_mm256_movemask_epi8(lo_rb_quotes);
    uint64_t lo_rib_open_quotes = _pdep_u64(0x5555555555555555L << lo_quote_toggle, lo_rib_quotes);
    uint64_t lo_quote_mask      = 0x00000000ffffffffL & (((lo_rib_open_quotes << 1) | lo_quote_toggle) + ~lo_rib_quotes);

    // printf("lo_rib_quotes:      "); print_bits(lo_rib_quotes      ); printf("\n");
    // printf("lo_rib_open_quotes: "); print_bits(lo_rib_open_quotes ); printf("\n");
    // printf("lo_quote_mask:      "); print_bits(lo_quote_mask      ); printf("\n");

    // toggle64 carry w =
    // let c = carry .&. 0x1
    // in  let addend  = pdep (0x5555555555555555 .<. c) w
    //     in  ((addend .<. 1) .|. c) + comp w

    quote_count += _popcnt64(lo_rib_quotes);

    uint64_t hi_quote_toggle = quote_count & 1;
    // printf("hi_quote_toggle: %llu\n", hi_quote_toggle);

    v32si hi_s = *(v32si*)(in + (i * 64) + 32);
    v32si hi_rb_newlines    = _mm256_cmpeq_epi8(hi_s, ws_newlines);
    v32si hi_rb_delimiters  = _mm256_cmpeq_epi8(hi_s, ws_delimiters);
    v32si hi_rb_quotes      = _mm256_cmpeq_epi8(hi_s, ws_quotes);

    uint32_t hi_rib_newlines    = (uint32_t)_mm256_movemask_epi8(hi_rb_newlines);
    uint32_t hi_rib_delimiters  = (uint32_t)_mm256_movemask_epi8(hi_rb_delimiters);
    uint32_t hi_rib_quotes      = (uint32_t)_mm256_movemask_epi8(hi_rb_quotes);
    uint64_t hi_rib_open_quotes = _pdep_u64(0x5555555555555555L << hi_quote_toggle, hi_rib_quotes);
    uint64_t hi_quote_mask      = 0x00000000ffffffffL & (((hi_rib_open_quotes << 1) | hi_quote_toggle) + ~hi_rib_quotes);
    // printf("hi_quote_mask:      "); print_bits(hi_quote_mask); printf("\n");

    quote_count += _popcnt64(hi_rib_quotes);

    uint64_t rib_newlines   = ((uint64_t)hi_rib_newlines    << 32) | (uint64_t)lo_rib_newlines;
    uint64_t rib_delimiters = ((uint64_t)hi_rib_delimiters  << 32) | (uint64_t)lo_rib_delimiters;
    uint64_t rib_quotes     = ((uint64_t)hi_rib_quotes      << 32) | (uint64_t)lo_rib_quotes;
    uint64_t quote_mask     = ((uint64_t)hi_quote_mask      << 32) | (uint64_t)lo_quote_mask;
    uint64_t rib_markers    = rib_newlines | rib_delimiters;

    tmp_delimiters[i] = rib_delimiters;
    tmp_quotes[i]     = rib_quotes;
    out_newlines[i]   = rib_newlines  & quote_mask;
    out_markers[i]    = rib_markers   & quote_mask;
    tmp_quote_mask[i] = quote_mask;
  }

  // for (size_t line = 0; line < len / 64; ++line) {
  //   printf("|");
  //   for (size_t byte = 0; byte < 64; ++byte) {
  //     size_t i = line * 64 + byte;

  //     if (byte == 32) {
  //       putc(' ', stdout);
  //     }

  //     if (!isprint(in[i])) {
  //       putc('.', stdout);
  //     } else {
  //       putc(in[i], stdout);
  //     }
  //   }
  //   printf("|\n");
  //   // print_bits(tmp_quotes[line]);
  //   // print_bits(tmp_quote_mask[line]);
  //   print_bits(out_newlines[line]);
  //   print_bits(out_markers[line]);
  //   printf("|                                                                 |\n");
  // }

  return quote_count;
}

int example_main()
{
  FILE *in = fopen("example.csv", "r");
  FILE *out_newlines = fopen("example.csv.newlines.idx", "w");
  FILE *out_markers = fopen("example.csv.markers.idx", "w");

  if (!in || !out_newlines || !out_markers) {
    return 1;
  }

  char buffer[4096];
  uint64_t buffer_markers[64];
  uint64_t buffer_newlines[64];
  size_t quote_count = 0;

  size_t bytes_read = fread(buffer, 1, 4096, in);

  while (bytes_read > 0) {
    if (bytes_read < 4096) {
      memset(buffer + bytes_read, 0, 4096 - bytes_read);
    }

    quote_count = build_ibs(
      ',',
      quote_count,
      buffer,
      4096,
      buffer_markers,
      buffer_newlines);

    fwrite(buffer_newlines, sizeof(uint64_t), 64, out_newlines);
    fwrite(buffer_markers,  sizeof(uint64_t), 64, out_markers);

    bytes_read = fread(buffer, 1, 4096, in);
  }

  return 0;
}



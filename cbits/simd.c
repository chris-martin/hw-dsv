#include "simd.h"

#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef uint8_t v32si __attribute__ ((vector_size (32)));

uint64_t build_ibs(
    char delimiter,
    uint64_t quote_count,
    char *in,
    size_t len,
    uint64_t *out_markers,
    uint64_t *out_newlines) {
  if (len % 64 != 0) {
    printf("Invalid length: %zu\n", len);
    return 0xffffffff;
  }

  v32si ws_newlines   = _mm256_set1_epi8('\n');
  v32si ws_delimiters = _mm256_set1_epi8(delimiter);
  v32si ws_quotes     = _mm256_set1_epi8('"');

  for (size_t i = 0; i < len / 64; ++i) {
    v32si lo_s = *(v32si*)(in + (i * 64));
    v32si lo_rb_newlines    = _mm256_cmpeq_epi8(lo_s, ws_newlines);
    v32si lo_rb_delimiters  = _mm256_cmpeq_epi8(lo_s, ws_delimiters);
    v32si lo_rb_quotes      = _mm256_cmpeq_epi8(lo_s, ws_quotes);

    uint32_t lo_rib_newlines      = (uint32_t)_mm256_movemask_epi8(lo_rb_newlines);
    uint32_t lo_rib_delimiters    = (uint32_t)_mm256_movemask_epi8(lo_rb_delimiters);
    uint32_t lo_rib_quotes        = (uint32_t)_mm256_movemask_epi8(lo_rb_quotes);
    uint64_t lo_rib_open_quotes   = _pdep_u64(0x5555555555555555L << (quote_count & 1), lo_rib_quotes);
    uint64_t lo_rib_close_quotes  = _pdep_u64(0xaaaaaaaaaaaaaaaaL >> (quote_count & 1), lo_rib_quotes);

    quote_count += _popcnt64(lo_rib_quotes);

    v32si hi_s = *(v32si*)(in + (i * 64) + 32);
    v32si hi_rb_newlines    = _mm256_cmpeq_epi8(hi_s, ws_newlines);
    v32si hi_rb_delimiters  = _mm256_cmpeq_epi8(hi_s, ws_delimiters);
    v32si hi_rb_quotes      = _mm256_cmpeq_epi8(hi_s, ws_quotes);

    uint32_t hi_rib_newlines      = (uint32_t)_mm256_movemask_epi8(hi_rb_newlines);
    uint32_t hi_rib_delimiters    = (uint32_t)_mm256_movemask_epi8(hi_rb_delimiters);
    uint32_t hi_rib_quotes        = (uint32_t)_mm256_movemask_epi8(hi_rb_quotes);
    uint64_t hi_rib_open_quotes   = _pdep_u64(0x5555555555555555L << (quote_count & 1), hi_rib_quotes);
    uint64_t hi_rib_close_quotes  = _pdep_u64(0xaaaaaaaaaaaaaaaaL >> (quote_count & 1), hi_rib_quotes);

    quote_count += _popcnt64(hi_rib_quotes);

    uint64_t rib_newlines      = ((uint64_t)hi_rib_newlines     << 32) | (uint64_t)lo_rib_newlines;
    uint64_t rib_delimiters    = ((uint64_t)hi_rib_delimiters   << 32) | (uint64_t)lo_rib_delimiters;
    uint64_t rib_open_quotes   = ((uint64_t)hi_rib_open_quotes  << 32) | (uint64_t)lo_rib_open_quotes;
    uint64_t rib_close_quotes  = ((uint64_t)hi_rib_close_quotes << 32) | (uint64_t)lo_rib_close_quotes;
    uint64_t rib_markers       = rib_newlines | rib_delimiters;

    uint64_t quote_mask = (~rib_close_quotes) + rib_open_quotes;

    out_newlines[i] = rib_newlines & quote_mask;
    out_markers[i]  = rib_markers  & quote_mask;
  }

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



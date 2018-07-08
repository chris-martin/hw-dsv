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

void system_memcpy(
    char *target,
    char *source,
    size_t len) {
  memcpy(target, source, len);
}

#if defined(AVX512_ENABLED)
void avx512_memcpy(
    uint8_t *target,
    uint8_t *source,
    size_t len) {
  size_t aligned_len    = (len / 32) * 32;
  size_t remaining_len  = len - aligned_len;

  for (size_t i = 0; i < aligned_len; i += 32) {
    __m256i v = _mm256_maskz_loadu_epi8(0xffffffff, source + i);

    _mm256_mask_storeu_epi8 (target + i, 0xffffffff, v);
  }

  memcpy(target + aligned_len, source + aligned_len, remaining_len);
}

size_t avx512_cmpeq8(
    uint8_t byte,
    uint64_t *target,
    size_t target_length,
    uint8_t *source,
    size_t source_length) {
  if (source_length % 64 != 0) {
    fprintf(stderr, "In avx512_cmpeq8: source_length not divisible by 64");
    exit(1);
  }

  // v32si v_byte_x_32 = _mm256_set1_epi8(byte);

  // uint32_t *out_mask = (uint32_t*)target;

  // for (size_t i = 0; i < source_a_length / 32; ++i) {
  //   v32si v_data = _mm256_maskz_loadu_epi8(0xffffffff, source_a + (i * 32));
  //   v32si v_results = _mm256_cmpeq_epi8(v_data, v_byte_x_32);
  //   uint32_t v_results_packed = (uint32_t)_mm256_movemask_epi8(v_results);
  //   out_mask = x
  // }

  return 0;
}

#endif

int example_main() {
  uint8_t source[32] = "01234567890123456789012345678901";
  uint8_t target[33];
  avx512_memcpy(target, source, 32);
  target[32] = 0;
  printf("%s\n", target);

  return 0;
}

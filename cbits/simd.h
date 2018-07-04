
#include <immintrin.h>
#include <mmintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

uint64_t build_ibs(
    uint8_t delimiter,
    uint64_t quote_count,
    char *in,
    size_t len,
    uint64_t *out_markers,
    uint64_t *out_newlines);

#include <unistd.h>
#include <stdint.h>

void avx512_memcpy(
    uint8_t *target,
    uint8_t *source,
    size_t len);

void avx512_cmpeq8(
    uint8_t *target,
    uint8_t b,
    uint8_t *source,
    size_t len);

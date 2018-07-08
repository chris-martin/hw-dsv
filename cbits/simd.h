#include <unistd.h>
#include <stdint.h>

void avx512_memcpy(
    uint8_t *target,
    uint8_t *source,
    size_t len);

size_t avx512_cmpeq8(
    uint8_t byte,
    uint64_t *target,
    size_t target_length,
    uint8_t *source,
    size_t source_length);

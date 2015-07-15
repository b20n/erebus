#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pfor.h"

static uint64_t zigzag(int64_t n)
{
    return (n << 1) ^ (n >> 63);
}

static int64_t unzigzag(uint64_t n)
{
    return (n >> 1) ^ (-(n & 1));
}

static int ceil_log2(uint64_t x)
{
    /* From http://stackoverflow.com/questions/3272424/compute-fast-log-base-2-ceiling */
    static const uint64_t t[6] = {
        0xFFFFFFFF00000000ull,
        0x00000000FFFF0000ull,
        0x000000000000FF00ull,
        0x00000000000000F0ull,
        0x000000000000000Cull,
        0x0000000000000002ull
    };

    int y = (((x & (x - 1)) == 0) ? 0 : 1);
    int j = 32;
    int i, k;

    for (i = 0; i < 6; i++) {
        k = (((x & t[i]) == 0) ? 0 : j);
        y += k;
        x >>= k;
        j >>= 1;
    }

    return y;
}

static PforBlock *pfor_block_new_int(int bits, uint16_t size)
{
    PforBlock *pfor = malloc(sizeof(PforBlock));
    if (pfor == NULL) {
        return NULL;
    }

    pfor->size = size;
    pfor->i = 0;
    pfor->bits = bits;
    pfor->excs = NULL;
    pfor->packed = malloc(size * bits);
    if (pfor->packed == NULL) {
        pfor_block_free(pfor);
        return NULL;
    }

    memset(pfor->packed, 0x00, size * bits / 8);
    return pfor;
}

static void pfor_block_pack(uint64_t *values, uint64_t v, int bits, int offset)
{
    int index = (offset * bits) / 64;
    int shift = (offset * bits) % 64;
    values[index] |= (v % (1LU << bits)) << shift;
    if (shift + bits > 64) {
        values[index+1] = v >> (bits - shift);
    }
}

static void pfor_block_unpack(uint64_t *in, uint64_t *out, int bits, int n)
{
    int inp = 0;
    int outp, shift;
    for (outp = 0; outp < n; outp++) {
        shift = (outp * bits) % 64;
        if (shift + bits > 64) {
            out[outp] = (in[inp] >> shift);
            inp++;
            shift = shift + bits - 64;
            out[outp] |= (in[inp] % (1LU << shift)) << (bits - shift);
        } else {
            out[outp] = (in[inp] >> shift) % (1LU << bits);
        }
    }
}

static void pfor_block_repack(PforBlock *pfor)
{
    int i, min;
    int max = 0;
    int exceptions = 0;
    int size = pfor->i;
    int bitbuckets[64] = {0};
    uint64_t uv;
    int64_t* values = malloc(size * sizeof(int64_t));
    pfor_block_read(pfor, values);
    /*
     * TODO: Make passes here to determine if ZigZag or Delta encoding is
     * appropriate.
     */
    for (i = 0; i < size; i++) {
        uv = zigzag(values[i]);
        bitbuckets[ceil_log2(uv)]++;
        max |= uv;
    }
    max = ceil_log2(max);
    for (i = max; i >= 0; i--) {
        exceptions += bitbuckets[i];
        /* Store the estimated cost of choosing size = i in bitbuckets[i] */
        bitbuckets[i] = (size * i) + (sizeof(PforBlockExc) * 8 * exceptions);
    }
    min = max;
    for (i = 0; i <= max; i++) {
        if (bitbuckets[i] < min) {
            min = i;
        }
    }
    pfor_block_free(pfor);
    pfor = pfor_block_new_int(min, size);
    for (i = 0; i < size; i++) {
        pfor_block_write(pfor, values[i]);
    }
    pfor->size = size;
}

PforBlock *pfor_block_new(uint16_t size)
{
    return pfor_block_new_int(8, size);
}

void pfor_block_free(PforBlock *pfor)
{
    PforBlockExc *tmp, *exc = pfor->excs;
    while (exc != NULL) {
        tmp = exc->next;
        free(exc);
        exc = tmp;
    }

    free(pfor->packed);
    free(pfor);
}

uint16_t pfor_block_size(PforBlock *pfor)
{
    return pfor->size;
}

uint16_t pfor_block_count(PforBlock *pfor)
{
    return pfor->i;
}

int pfor_block_read(PforBlock *pfor, int64_t *read)
{
    int i;
    PforBlockExc *exc;
    pfor_block_unpack(pfor->packed, (uint64_t*) read, pfor->bits, pfor->i);
    for (i = 0; i < pfor->i; i++) {
        read[i] = unzigzag((uint64_t)read[i]);
    }

    exc = pfor->excs;
    while (exc != NULL) {
        read[exc->idx] = exc->value;
        exc = exc->next;
    }

    return pfor->i;
}

int pfor_block_write(PforBlock *pfor, int64_t v)
{
    uint64_t uv;
    PforBlockExc *exc;
    if (pfor->i == pfor->size) {
        pfor_block_repack(pfor);
        return 0;
    }

    uv = zigzag(v);
    if (ceil_log2(uv) <= pfor->bits) {
        pfor_block_pack(pfor->packed, uv, pfor->bits, pfor->i);
    } else {
        exc = malloc(sizeof(PforBlockExc));
        if (exc == NULL) {
            return 0;
        }

        exc->value = v;
        exc->idx = pfor->i;
        exc->next = pfor->excs;
        pfor->excs = exc;
        pfor_block_pack(pfor->packed, 0, pfor->bits, pfor->i);
    }

    pfor->i++;
    return 1;
}

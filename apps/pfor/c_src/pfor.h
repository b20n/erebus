#ifndef PFOR_H
#define PFOR_H

#include <stdint.h>

enum {
    DELTA = 1 << 0,
    ZIGZAG = 1 << 1
};

typedef struct PforBlockExc {
    int64_t value;
    uint16_t idx;
    struct PforBlockExc *next;
} PforBlockExc;

typedef struct {
    uint64_t *packed;
    PforBlockExc *excs;
    uint16_t size;
    uint16_t i;
    uint8_t bits;
    uint8_t flags;
} PforBlock;

typedef struct {
    PforBlock **blocks;
    int64_t t0;
    int64_t t;
    uint16_t block_size;
    int dt;
    int size;
    int i;
} Pfor;

typedef struct {
    int64_t from;
    int64_t until;
    int64_t *read;
    int size;
} PforRead;

extern const int64_t PFOR_SENTINEL;

Pfor *pfor_new(int);
void pfor_free(Pfor*);
int pfor_update(Pfor*, int64_t, int64_t);
unsigned char *pfor_serialize(Pfor*);
PforRead *pfor_read(Pfor*, int64_t, int64_t);
void pfor_read_free(PforRead*);

extern const int PFOR_BLOCK_SIZE;

PforBlock *pfor_block_new(uint16_t);
void pfor_block_free(PforBlock*);
uint16_t pfor_block_size(PforBlock*);
uint16_t pfor_block_count(PforBlock*);
int pfor_block_read(PforBlock*, int64_t*);
int pfor_block_write(PforBlock*, int64_t);

#endif

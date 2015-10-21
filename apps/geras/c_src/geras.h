#ifndef GERAS_H
#define GERAS_H

#include <stdint.h>
#include "erl_nif.h"

typedef struct GerasBlockExc {
    int64_t value;
    uint16_t idx;
    struct GerasBlockExc *next;
} GerasBlockExc;

typedef struct {
    ErlNifRWLock *lock;
    GerasBlockExc *excs;
    uint64_t *data;
    uint64_t t;
    uint8_t bits;
    uint8_t dt;
} GerasBlock;

typedef struct {
    /* Inclusive begin */
    int64_t begin;
    /* Exclusive end */
    int64_t end;
    int wal;
    ErlNifRWLock *lock;
    hash_t *data;
} GerasMemPartition;

typedef struct {
    int64_t begin;
    int64_t end;
    void *f;
    hash_t *metadata;
} GerasDiskPartition;

typedef struct {
    int partition_size;
    int mem_count;
    int disk_count;
    ErlNifRWLock *lock;
    GerasMemPartition *mem;
    GerasDiskPartition *disk;
} Geras;

typedef struct {
    int64_t *read;
    int64_t from;
    int64_t until;
    int dt;
    int size;
} GerasPartitionRead;

typedef struct {
    GerasPartitionRead *reads;
    int size;
} GerasRead;

#endif

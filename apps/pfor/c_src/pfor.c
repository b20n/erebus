#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "pfor.h"

const int64_t PFOR_SENTINEL = INT64_MAX;
static const int INITIAL_SIZE = 4;
static const uint16_t BLOCK_SIZE = 128;

static void pfor_add_block(Pfor *pfor)
{
    int i, newsize;
    PforBlock *block = pfor_block_new(pfor->block_size);
    if (pfor->i++ >= pfor->size) {
        newsize = (int)(pfor->size * 1.5);
        PforBlock **blocks = malloc(newsize * sizeof(PforBlock*));
        for (i=0; i < pfor->size; i++) {
            blocks[i] = pfor->blocks[i];
        }

        free(pfor->blocks);
        pfor->blocks = blocks;
        pfor->size = newsize;
    }

    pfor->blocks[pfor->i] = block;
}

Pfor *pfor_new(int dt)
{
    Pfor *pfor = NULL;
    pfor = malloc(sizeof(Pfor));
    if (pfor == NULL) {
        return NULL;
    }

    pfor->i = 0;
    pfor->size = INITIAL_SIZE;
    pfor->block_size = BLOCK_SIZE;
    pfor->blocks = malloc(pfor->size * sizeof(PforBlock*));
    pfor->blocks[0] = pfor_block_new(pfor->block_size);
    pfor->t = -1;
    pfor->t0 = -1;
    pfor->dt = dt;
    return pfor;
}

void pfor_free(Pfor *pfor)
{
    int i;
    if (pfor->blocks != NULL) {
        for (i = 0; i < pfor->size; i++) {
            if (pfor->blocks[i] != NULL) {
                pfor_block_free(pfor->blocks[i]);
            }
        }

        free(pfor->blocks);
    }

    if (pfor != NULL) {
        free(pfor);
    }
}

int pfor_update(Pfor *pfor, int64_t t, int64_t v)
{
    int i;
    int64_t gap;

    t = t - (t % pfor->dt);
    if (pfor->t == -1) {
        pfor->t0 = t;
    } else {
        gap = (t - (pfor->t + pfor->dt)) / pfor->dt;
        if (gap < 0) {
            return 0;
        }

        for (i = 0; i < gap; i++) {
            if (!pfor_block_write(pfor->blocks[pfor->i], PFOR_SENTINEL)) {
                pfor_add_block(pfor);
                pfor_block_write(pfor->blocks[pfor->i], PFOR_SENTINEL);
            }
        }
    }

    if (!pfor_block_write(pfor->blocks[pfor->i], v)) {
        pfor_add_block(pfor);
        pfor_block_write(pfor->blocks[pfor->i], v);
    }

    pfor->t = t;
    return 1;
}

unsigned char *pfor_serialize(Pfor *pfor)
{
    /* TODO */
    return NULL;
}

void pfor_read_free(PforRead* read)
{
    if (read->read != NULL) {
        free(read->read);
    }

    if (read != NULL) {
        free(read);
    }
}

PforRead *pfor_read(Pfor *pfor, int64_t from, int64_t until)
{
    int i, offset, startblock, endblock, toread, nread;
    int64_t *ptr, *buf;
    PforRead *read = NULL;

    /* Ranges are inclusive */
    from = (from - from) % pfor->dt;
    if (until % pfor->dt != 0) {
        until = until + pfor->dt;
        until = until - (until % pfor->dt);
    }

    if (from < pfor->t0) {
        from = pfor->t0;
    }
    if (until > pfor->t) {
        until = pfor->t;
    }

    if (until == -1 || from == -1) {
        return NULL;
    }

    if (until < from) {
        return NULL;
    }

    read = malloc(sizeof(PforRead));
    if (read == NULL) {
        return NULL;
    }

    read->from = from;
    read->until = until;
    /* Increment by one - reads are inclusive */
    read->size = ((until - from) / pfor->dt) + 1;
    /* Find the appropriate blocks to read from. */
    startblock = 0;
    /* The "index in the array" of the first point to be read */
    offset = read->from / pfor->dt;
    toread = 0;
    /*
     * Iterate over each block and take account of its size. Once the block
     * containing the first index to be read is found, break - that's the first
     * block that needs to be read. Note that this serves dual purpose in
     * indicating how many items in the first read block must be discarded when
     * returning the read to the user.
     *
     * N.B.: This functionality would be much simpler if blocks were guaranteed
     * to be the same size, but that is a brittle assumption that I'm not
     * willing to make. We may want to merge blocks, and we may want to read
     * blocks from disk that were written by an older version of pfor, with a
     * different block size.
     */
    for (;;) {
        /*
         * Overwrite toread on each iteration since we won't read any points
         * until we break out of this loop.
         */
        toread = pfor_block_count(pfor->blocks[startblock]);
        if (toread > offset) {
            /*
             * We've found the block containing the beginning of the read -
             * break, preserving the current values of startblock, toread, and
             * offset - they're all meaningful later.
             */
            break;
        }

        offset -= toread;
        startblock++;
    }

    /* Now search for the last block that needs to be read. */
    endblock = startblock;
    for (;;) {
        if (toread - offset >= read->size) {
            break;
        }

        endblock++;
        toread += pfor_block_count(pfor->blocks[endblock]);
    }

    buf = malloc(toread * sizeof(int64_t));
    read->read = malloc(read->size * sizeof(int64_t));
    if (buf == NULL) {
        pfor_read_free(read);
        return NULL;
    }

    if (read->read == NULL) {
        free(buf);
        pfor_read_free(read);
        return NULL;
    }

    ptr = buf;
    for (i = startblock; i <= endblock; i++) {
        nread = pfor_block_read(pfor->blocks[i], ptr);
        if (nread != pfor_block_count(pfor->blocks[i])) {
            free(buf);
            pfor_read_free(read);
            return NULL;
        }
        ptr += nread;
    }

    memcpy(read->read, buf + offset, sizeof(int64_t) * read->size);
    free(buf);
    return read;
}

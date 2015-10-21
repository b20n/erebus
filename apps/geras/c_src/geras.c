#include <limits.h>

#include "geras.h"

const int64_t GERAS_SENTINEL = INT64_MAX;

int64_t min(int64_t a, int64_t b) {
    return (a < b) ? a : b;
}

Geras *geras_new(int partition_size, int mem_count, int disk_count)
{
    Geras *geras = NULL;
    geras = malloc(sizeof(geras));
    if (geras == NULL) {
        return NULL;
    }

    geras->partition_size = partition_size;
    geras->mem_count = mem_count;
    geras->disk_count = disk_count;
    geras->lock = enif_rwlock_create("\0");
    geras->mem = calloc(mem_count * sizeof(GerasMemPartition*));
    geras->disk = calloc(disk_count * sizeof(GerasDiskPartition*));
    if (geras->mem == NULL || geras->disk == NULL) {
        geras_free(geras);
        return NULL;
    }

    return geras;
}

void geras_free(Geras *geras)
{
    int i;
    if (geras->lock != NULL) {
        enif_rwlock_destroy(geras->lock);
    }
    if (geras->mem != NULL) {
        for (i = 0; i < geras->mem_size; i++) {
            if (geras->mem[i] != NULL) {
                geras_mem_free(geras->mem[i]);
            }
        }

    }
    if (geras->disk != NULL) {
        for (i = 0; i < geras->disk_size; i++) {
            if (geras->disk[i] != NULL) {
                geras_disk_free(geras->disk[i]);
            }
        }

    }

    free(geras);
}

int geras_update(Geras *geras, int k, int dt, int64_t t, int64_t v)
{
    int i;
    GerasMemPartition *mp_new, *mp_old, *mp;
    GerasDiskPartition *dp_new, *dp_old;

    if (geras->mem[0]->end <= t) {
        /*
         * Time to roll the partitions.
         *
         * 1. Acquire an R/W lock (because the underlying data will be moving)
         *
         * 2. Allocate a new memory partition, snag a reference to the oldest
         * one, and move all old elements up one spot. Insert the newest
         * partition at the head of the array.
         *
         * 3. Same pattern for disk partitions - allocate a new one, snag a
         * reference, move up one spot, and insert.
         *
         * 4. Unlock, free the expired partitions, and perform the write.
         */
        enif_rwlock_rwlock(geras->lock);
        mp_new = geras_mem_new();
        if (mp_new == NULL) {
            enif_rwlock_rwunlock(geras->lock);
            return 1;
        }
        mp_old = geras->mem[geras->mem_count];
        for (i = 1; i < geras->mem_count; i++) {
            geras->mem[i] = geras->mem[i-1];
        }
        geras->mem[0] = mp_new;

        dp_new = geras_disk_new(mp_old);
        if (dp_new == NULL) {
            enif_rwlock_rwunlock(geras->lock);
            return 1;
        }
        dp_old = geras->disk[geras->disk_count];
        for (i = 1; i < geras->disk_count; i++) {
            geras->disk[i] = geras->disk[i-1];
        }
        geras->disk[0] = dp_new;
        /* RW unlock here - free can be done after unlocking */
        enif_rwlock_rwunlock(geras->lock);
        geras_mem_free(mp_old);
        geras_disk_free(dp_old);
    }

    mp = NULL;
    enif_rwlock_rlock(geras->lock);
    for (i = 0; i < geras->mem_count; i++) {
        if (geras->mem[i] != NULL && geras->mem[i]->begin <= t) {
            mp = geras->mem[i];
            break;
        }
    }
    enif_rwlock_runlock(geras->lock);

    if (mp == NULL) {
        return 1;
    }
    return geras_mem_update(k, dt, t, v);
}

int geras_read_int(GerasRead *read,
                   GerasPartitionRead *src,
                   int *dt,
                   int64_t *from,
                   int64_t until)
{
    int i, remaining;
    GerasPartitionRead *tgt = NULL;

    if (!r) {
        return 0;
    }

    if (src->dt != *dt) {
        tgt = calloc(sizeof(tgt));
        if (tgt == NULL) {
            return 1;
        }

        read->reads[read->size] = tgt;

        /*
         * A conservative allocation to avoid clever tricks - if we end up
         * discovering a different `dt` later on some of this space will go
         * unused, but it's short-lived, so who cares?
         */
        remaining = ((until - *from) / src->dt) + 1;
        tgt->read = calloc(remaining, sizeof(int64_t));
        if (tgt->read == NULL) {
            // TODO: dealloc
            return 1;
        }

        tgt->from = *from;
        /* Subtract one here - reads are inclusive */
        tgt->until = *from - src->dt;
        tgt->size = 0;
        tgt->dt = src->dt;
        *dt = src->dt;
        read->size++;
    }

    /*
     * If the source read starts past the current `from` value, fill in the gap
     * with sentinel values before copying memory across.
     */
    while (src->from > *from) {
        tgt->read[tgt->size] = GERAS_SENTINEL;
        tgt->size++;
        tgt->until += tgt->dt;
        *from += tgt->dt;
    }

    // TODO: handle case where from > src->from by assiging to i
    i = ??;
    // TODO: this isn't right
    remaining = ((min(until, src->until) - *from) / src->dt) + 1;
    // TODO: this isn't right
    for (i = 0; i < remaining; i++) {
        // TODO: copy value
        tgt->size++;
        tgt->until += tgt->dt;
        *from += tgt->dt;
    }

    return 0;
}

GerasRead *geras_read(Geras *geras, int k, int64_t from, int64_t until)
{
    int i, count, max, dt, remaining;
    GerasDiskPartition *d;
    GerasMemPartition *m;
    GerasPartitionRead *pread;
    GerasRead *read = NULL;

    read = calloc(sizeof(read));
    if (read == NULL) {
        return NULL;
    }

    /*
     * This read will touch at most `max` partitions; to be conservative,
     * allocate that much space in the read array. We won't actually use it all
     * unless each partition has a different `dt` value, which is highly
     * unlikely.
     */
    max = ((until - from) / geras->partition_size) + 1;
    read->reads = calloc(max * sizeof(pread));
    if (read->reads == NULL) {
        return NULL;
    }
    /*
     * The number of chunks (i.e., reads with distinct `dt` values) seen thus
     * far.
     */
    read->size = 0;

    /*
     * The last-seen `dt` value. 0 is an invalid value, so the first read will
     * always trigger a new insertion without an exceptional clause.
     */
    dt = 0;

    // TODO: consider figuring out a way to combine these two loops
    // and thereby avoid the wacky calls to geras_read_int...
    for (i = geras->disk_count - 1; i >= 0; i--) {
        d = geras->disk[i];
        if (d != NULL && d->begin <= until && d->end >= from) {
            pread = geras_disk_read(d, k);
            geras_read_int(read, pread, &dt, &from, until);
        }
    }

    for (i = geras->mem_count - 1; i >= 0; i--) {
        m = geras->mem[i];
        if (m != NULL && m->begin <= until && m->end >= from ) {
            pread = geras_disk_read(m, k);
            geras_read_int(read, pread, &dt, &from, until);
        }
    }

    while (from <= until) {
        // TODO: fill in gaps with nulls, don't forget to inc from
    }
}

void merge_reads(GerasRead *src, GerasRead* dst)
{
    if (src->dt > dst->dt) {
        resize(dst, src->dt);
    }
}

void resize(GerasRead *read, int dt)
{
    int64_t *arr;

    /*
     * There is a problem/lack of elegance here in attempting to support varying
     * collection intervals (dts): If dt increases partway through the read
     * path, then we should technically re-floor the read->from, which may cause
     * it to read different data than it would have otherwise - because the
     * older, higher resolution data would have returned more points. I can
     * think of several ways to work around this - collect the metadata first
     * and determine an appropriate interval (requires a new API), or go back
     * and re-read the first and last windows at the end (horrible). Thinking
     * that the best approach might be to return an array of reads, one for each
     * dt found and with appropriate values therein, i.e., let the caller deal
     * with it. If I'm a good graphing library I probably want to be given _all_
     * the data, not just some data downsampled to the lowest common
     * denominator...
     */

    arr = malloc((read->until - read->from + 1) / dt)
    if (
}

#include "geras.h"

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

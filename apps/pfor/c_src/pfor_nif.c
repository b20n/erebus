#include <string.h>

#include "erl_nif.h"
#include "pfor.h"

typedef struct
{
    ErlNifPid p;
    Pfor *pfor;
} PforWrap;

typedef struct
{
    ERL_NIF_TERM atom_error;
    ERL_NIF_TERM atom_ok;
    ERL_NIF_TERM atom_undefined;
    ErlNifResourceType* res_pfor;
} PforPriv;

static ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

static int check_pid(ErlNifEnv* env, PforWrap* wrap)
{
    ErlNifPid pid;
    enif_self(env, &pid);
    if (enif_compare(pid.pid, wrap->p.pid) == 0) {
        return 1;
    }
    return 0;
}

static ERL_NIF_TERM
pfor_nif_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PforPriv* priv = enif_priv_data(env);
    PforWrap* wrap = NULL;
    ERL_NIF_TERM ret;
    int dt;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[0], &dt)) {
        return enif_make_badarg(env);
    }

    wrap = (PforWrap*)enif_alloc_resource(priv->res_pfor, sizeof(PforWrap));
    memset(wrap, '\0', sizeof(PforWrap));
    enif_self(env, &(wrap->p));
    wrap->pfor = pfor_new(dt);
    ret = enif_make_resource(env, wrap);
    enif_release_resource(wrap);
    return enif_make_tuple2(env, priv->atom_ok, ret);
}

static ERL_NIF_TERM
pfor_nif_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PforPriv* priv = enif_priv_data(env);
    PforWrap* wrap = NULL;
    ErlNifSInt64 t;
    ErlNifSInt64 v;

    if (argc != 3) {
        return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], priv->res_pfor, (void**) &wrap)) {
        return enif_make_badarg(env);
    }

    if (!check_pid(env, wrap)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int64(env, argv[1], &t)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int64(env, argv[2], &v)) {
        return enif_make_badarg(env);
    }

    if (!pfor_update(wrap->pfor, t, v)) {
        return priv->atom_error;
    }

    return priv->atom_ok;
}

static ERL_NIF_TERM
pfor_nif_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PforPriv* priv = enif_priv_data(env);
    PforWrap* wrap = NULL;
    PforRead* read = NULL;
    ERL_NIF_TERM* points = NULL;
    ERL_NIF_TERM ret;

    int i;
    ErlNifSInt64 from;
    ErlNifSInt64 until;

    if (argc != 3) {
        return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], priv->res_pfor, (void**) &wrap)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int64(env, argv[1], &from)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int64(env, argv[2], &until)) {
        return enif_make_badarg(env);
    }

    read = pfor_read(wrap->pfor, from, until);
    if (read == NULL) {
        return priv->atom_error;
    }

    points = malloc(read->size * sizeof(ERL_NIF_TERM));
    if (points == NULL) {
        pfor_read_free(read);
        return priv->atom_error;
    }

    for (i = 0; i < read->size; i++) {
        if (read->read[i] == PFOR_SENTINEL) {
            points[i] = priv->atom_undefined;
        } else {
            points[i] = enif_make_int64(env, read->read[i]);
        }
    }

    ret = enif_make_tuple3(
        env,
        enif_make_int64(env, read->from),
        enif_make_int64(env, read->until),
        enif_make_list_from_array(env, points, read->size)
    );
    free(points);
    pfor_read_free(read);
    return ret;
}

static ERL_NIF_TERM
pfor_nif_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_badarg(env);
}

static void pfor_nif_destroy(ErlNifEnv* env, void* obj)
{
    // TODO: dealloc!
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    ErlNifResourceType* res;

    PforPriv* new_priv = (PforPriv*)enif_alloc(sizeof(PforPriv));
    if (new_priv == NULL) {
        return 1;
    }

    res = enif_open_resource_type(
        env,
        NULL,
        "pfor",
        pfor_nif_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
        NULL
    );

    if (res == NULL) {
        return 1;
    }

    new_priv->atom_error = make_atom(env, "error");
    new_priv->atom_ok = make_atom(env, "ok");
    new_priv->atom_undefined = make_atom(env, "undefined");
    new_priv->res_pfor = res;
    *priv = (void*)new_priv;
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    *priv = *old_priv;
    return 0;
}

static ErlNifFunc funcs[] = {
    {"new", 1, pfor_nif_new},
    {"update", 3, pfor_nif_update},
    {"read", 3, pfor_nif_read},
    {"to_list", 1, pfor_nif_to_list},
};

ERL_NIF_INIT(pfor, funcs, &load, NULL, &upgrade, &unload);

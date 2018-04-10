
#include "erlimage.h"

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM 
mk_ok(ErlNifEnv* env, const char* mesg)
{
      ERL_NIF_TERM ok = mk_atom(env, "ok");
      ERL_NIF_TERM str = enif_make_string(env, mesg, ERL_NIF_LATIN1);
      return enif_make_tuple2(env, ok, str);
}

ERL_NIF_TERM
error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "err"), mk_atom(env, mesg));
}

ERL_NIF_TERM 
response(ErlNifEnv* env, const char* mesg, ERL_NIF_TERM data )
{
      ERL_NIF_TERM status = mk_atom(env, "ok");
      return enif_make_tuple2(env, status, data);
}

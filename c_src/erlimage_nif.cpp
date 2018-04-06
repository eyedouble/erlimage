#include <stdio.h>
#include <string.h>
#include <cstdlib>
#include <iostream>
#include "erl_nif.h"
#include "FreeImage.h"

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
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

ERL_NIF_TERM 
mk_ok(ErlNifEnv* env, const char* mesg)
{
      ERL_NIF_TERM ok = mk_atom(env, "ok");
      ERL_NIF_TERM str = enif_make_string(env, mesg, ERL_NIF_LATIN1);
      return enif_make_tuple2(env, ok, str);
}

static ERL_NIF_TERM
load(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifEnv* msg_env;
    ERL_NIF_TERM result;    
    if(argc != 1) 
        return enif_make_badarg(env);
    
    msg_env = enif_alloc_env();
    if(msg_env == NULL)
        return mk_error(env, "environ_alloc_error");
    

    /* INPUTS */
    /* GET AND CONVERT ARG0 */
    int len0;
    enif_get_list_length(env, argv[0], (unsigned int*)&len0);
    char compileStyle[len0];
    enif_get_string(env, argv[0], (char*)compileStyle, len0 + 1, ERL_NIF_LATIN1);

    // if (status != 0)
    // {
    //     result = mk_error(env, sass_context_get_error_message(ctx));
    // } else
    // {
    //     result = mk_ok( env, sass_context_get_output_string(ctx) );
    // }



    result = mk_ok( env, "success." );

    enif_free_env(msg_env);
    return result;
}

static ERL_NIF_TERM 
t(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifEnv* msg_env;
    ERL_NIF_TERM result;    
    if(argc != 0) 
        return enif_make_badarg(env);
    
    msg_env = enif_alloc_env();
    if(msg_env == NULL)
        return mk_error(env, "environ_alloc_error");

    
    FreeImage_Initialise ( );
    FIBITMAP* bitmap = FreeImage_Load ( FIF_PNG, "q.png", 0 );

    //  result = mk_ok( env, FreeImage_GetVersion ( ) );
     if ( FreeImage_Save ( FIF_PNG, bitmap, "flat.png", 0 ) )
        result = mk_ok( env, FreeImage_GetVersion ( ) );

    FreeImage_Unload ( bitmap );      
    FreeImage_DeInitialise ( );

//     FIBITMAP* bitmap = FreeImage_Load ( FIF_PNG, "q.png", 0 );

//     if (!bitmap)
//         exit(1);
    
//     FreeImage_ConvertTo24Bits(bitmap);
//     // FIBITMAP* dib8_b = FreeImage_ColorQuantize(bitmap, FIQ_WUQUANT);   
//     // FIBITMAP* dib8_b = FreeImage_ColorQuantizeEx(bitmap, FIQ_WUQUANT, 255, 0, NULL);
//     FreeImage_GetBits(bitmap);
    


    // if (status != 0)
    // {
    //     result = mk_error(env, sass_context_get_error_message(ctx));
    // } else
    // {
    //     result = mk_ok( env, sass_context_get_output_string(ctx) );
    // }

    

    enif_free_env(msg_env);
    return result;
}

static ErlNifFunc nif_funcs[] = {
    {"load", 1, load}
    ,{"t", 0, t}
};

ERL_NIF_INIT(erlimage, nif_funcs, NULL, NULL, NULL, NULL);
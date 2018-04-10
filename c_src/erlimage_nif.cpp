

#include "erlimage/erlimage.h"
// #include "erlimage/erlimage_base.cpp"
// #include "erlimage/erlimage_io.cpp"
// #include "erlimage/erlimage_pixels.cpp"

static ERL_NIF_TERM
load(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"load", 1, load}


};

// static ERL_NIF_TERM
// readFile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
// {

//     ERL_NIF_TERM result;  

//     if(argc != 1) 
//         return enif_make_badarg(env);
    
//     int len0;
//     enif_get_list_length(env, argv[0], (unsigned int*)&len0);
//     char filePath[len0];
//     enif_get_string(env, argv[0], (char*)filePath, len0 + 1, ERL_NIF_LATIN1);

//     if ( !filePath )
//         return enif_make_badarg ( env );

//     FreeImage_Initialise ( );      
//     ERLIMAGE_L_DO ReadResponse = Erlimage_Io::fileRead ( filePath );

//     if ( ReadResponse.Status != 0 )
//         result = error ( env, "Error reading file" );

//     ERLIMAGE_I_DO ExportResponse = Erlimage_Pixels::exportPixels ( env, Dib );

//     if ( ExportResponse.Status != 0 )
//         rersult = error ( env, "Error exporting pixel array to interop format" );

//     result = response ( env, Response.Data );

//     FreeImage_Unload ( Dib );  
//     FreeImage_DeInitialise ( );
//     return result;
// }

ERL_NIF_INIT(erlimage, nif_funcs, NULL, NULL, NULL, NULL);















/* ddd 
#include "erlimage/erlimage.h"
#include "erlimage/erlimage_base.cpp"
#include "erlimage/erlimage_io.cpp"
#include "erlimage/erlimage_pixels.cpp" */


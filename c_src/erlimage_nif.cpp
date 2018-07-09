

#include "erlimage/erlimage.h"
#include "erlimage/erlimage_base.cpp"
#include "erlimage/erlimage_io.cpp"
#include "erlimage/erlimage_pixels.cpp"
#include "erlimage/erlimage_transform.cpp"

/* ====================
        READFILE
   ==================== */
static ERL_NIF_TERM
version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM result;  
    result = mk_ok(env, "Erlimage:0.0.5-beta;Freeimage:3.17.0;Libimagequant:2.11.10;");
    return result;
}
/* ====================
        READFILE
   ==================== */
static ERL_NIF_TERM
readFile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    if(argc != 1) return enif_make_badarg(env);    
    int len0;
    enif_get_list_length(env, argv[0], (unsigned int*)&len0);
    char filePath[len0];
    enif_get_string(env, argv[0], (char*)filePath, len0 + 1, ERL_NIF_LATIN1);
    if ( filePath == 0 ) return enif_make_badarg ( env );

    ERL_NIF_TERM result;
    FreeImage_Initialise ( );      
    ERLIMAGE_L_DO ReadResponse = Erlimage_Io::fileRead ( filePath );

    if ( ReadResponse.Status != 0 ) {
        FreeImage_DeInitialise ( );
        return error ( env, "Error reading file" );
    };        

    ERLIMAGE_I_DO ExportResponse = Erlimage_Pixels::exportPixels ( env, ReadResponse.Dib );

    if ( ExportResponse.Status != 0 ) {
        FreeImage_DeInitialise ( );        
        return error ( env, "Error exporting pixel array to interop format" );
    } ;

    result = response ( env, ExportResponse.Data );
    FreeImage_Unload ( ReadResponse.Dib );  
    FreeImage_DeInitialise ( );
    return result;
}

/* ====================
        WRITE JPG
   ==================== */
static ERL_NIF_TERM
writeJpg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    if(argc != 4) return enif_make_badarg(env);
    int len0;
    enif_get_list_length(env, argv[0], (unsigned int*)&len0);
    char filePath[len0];
    enif_get_string(env, argv[0], (char*)filePath, len0 + 1, ERL_NIF_LATIN1);
    if ( filePath == 0 ) return enif_make_badarg ( env );
    if(!enif_is_number ( env, argv[2]) ) return enif_make_badarg(env);
    if(!enif_is_number ( env, argv[3]) ) return enif_make_badarg(env);

    int quality, progressive; 
    enif_get_int(env, argv[2], &quality );    
    enif_get_int(env, argv[3], &progressive );

    if ( quality < 0 || quality > 100 ) return enif_make_badarg(env);
    if ( progressive < 0 || progressive > 1 ) return enif_make_badarg(env);

    FreeImage_Initialise ( );     
    ERLIMAGE_L_DO importResponse = Erlimage_Pixels::importPixels ( env, argv[1] );
    if ( importResponse.Status != 0 ) {
        FreeImage_DeInitialise ( );
        return error ( env, "Error importing interop format" );
    };
        
    ERL_NIF_TERM result;

    if( Erlimage_Io::writeJpg ( filePath, importResponse.Dib, quality, progressive ) != 0){
        result = error ( env, "Error writing file" );
    }
    else {
        result = response ( env, mk_ok ( env, "written" ) );
    }; 

    FreeImage_Unload ( importResponse.Dib );  
    FreeImage_DeInitialise ( );
    return result;
}

/* ====================
        WRITE WEBP
   ==================== */
static ERL_NIF_TERM
writeWebp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    if(argc != 4) return enif_make_badarg(env);
    int len0;
    enif_get_list_length(env, argv[0], (unsigned int*)&len0);
    char filePath[len0];
    enif_get_string(env, argv[0], (char*)filePath, len0 + 1, ERL_NIF_LATIN1);
    if ( filePath == 0 ) return enif_make_badarg ( env );
    if(!enif_is_number ( env, argv[2]) ) return enif_make_badarg(env);
    if(!enif_is_number ( env, argv[3]) ) return enif_make_badarg(env);

    int quality, lossless; 
    enif_get_int(env, argv[2], &quality );    
    enif_get_int(env, argv[3], &lossless );

    if ( quality < 0 || quality > 100 ) return enif_make_badarg(env);
    if ( lossless < 0 || lossless > 1 ) return enif_make_badarg(env);

    FreeImage_Initialise ( );     
    ERLIMAGE_L_DO importResponse = Erlimage_Pixels::importPixels ( env, argv[1] );
    if ( importResponse.Status != 0 ) {
        FreeImage_DeInitialise ( );
        return error ( env, "Error importing interop format" );
    };
        
    ERL_NIF_TERM result;

    if( Erlimage_Io::writeWebp ( filePath, importResponse.Dib, quality, lossless ) != 0){
        result = error ( env, "Error writing file" );
    }
    else {
        result = response ( env, mk_ok ( env, "written" ) );
    }; 

    FreeImage_Unload ( importResponse.Dib );  
    FreeImage_DeInitialise ( );
    return result;
}

/* ====================
        WRITE WEBP
   ==================== */
static ERL_NIF_TERM
rescale(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    if(argc != 4) return enif_make_badarg(env);
    if(!enif_is_number ( env, argv[1]) ) return enif_make_badarg(env);
    if(!enif_is_number ( env, argv[2]) ) return enif_make_badarg(env);
    if(!enif_is_number ( env, argv[3]) ) return enif_make_badarg(env);

    int width, height, algorithm; 
    enif_get_int(env, argv[1], &width );    
    enif_get_int(env, argv[2], &height );
    enif_get_int(env, argv[3], &algorithm );
    

    if ( width < 0 || width > 99999 ) return enif_make_badarg(env);
    if ( height < 0 || height > 99999 ) return enif_make_badarg(env);
    if ( algorithm < 0 || algorithm > 6 ) return enif_make_badarg(env);

    FreeImage_Initialise ( );     
    ERLIMAGE_L_DO importResponse = Erlimage_Pixels::importPixels ( env, argv[0] );
    if ( importResponse.Status != 0 ) {  
        FreeImage_DeInitialise ( );
        return error ( env, "Error importing interop format" );
    };

    importResponse = Erlimage_Transform::rescale ( importResponse, width, height, algorithm );
    if ( importResponse.Status != 0 ) {
        FreeImage_Unload ( importResponse.Dib ); 
        FreeImage_DeInitialise ( );
        return error ( env, "Error could not rescale" );
    };

    ERL_NIF_TERM result;

    ERLIMAGE_I_DO ExportResponse = Erlimage_Pixels::exportPixels ( env, importResponse.Dib );
    if ( ExportResponse.Status != 0 ) {
        FreeImage_Unload ( importResponse.Dib ); 
        FreeImage_DeInitialise ( );        
        return error ( env, "Error exporting pixel array to interop format" );
    };

    result = response ( env, ExportResponse.Data );

    FreeImage_Unload ( importResponse.Dib );  
    FreeImage_DeInitialise ( );
    return result;
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}

static ErlNifFunc nif_funcs[] = {
    {"version", 0, version}
    ,{"readFile", 1, readFile}
    ,{"writeJpg", 4, writeJpg}
    ,{"writeWebp", 4, writeWebp}
    ,{"rescale", 4, rescale}    
};

ERL_NIF_INIT(erlimage, nif_funcs, &load, NULL, &upgrade, &unload);

/* ddd 
#include "erlimage/erlimage.h"
#include "erlimage/erlimage_base.cpp"
#include "erlimage/erlimage_io.cpp"
#include "erlimage/erlimage_pixels.cpp" */

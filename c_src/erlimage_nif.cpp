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

/* UTILS */
// INPLACESWAP adopted from codeguru.com 
template <class T> void ERLIMAGEINPLACESWAP(T& a, T& b) {
	a ^= b; b ^= a; a ^= b;
}
BOOL ErlimageSwap(FIBITMAP* dib, int from, int to) {
	if(FreeImage_GetImageType(dib) != FIT_BITMAP) {
		return FALSE;
	}
		
	const unsigned bytesperpixel = FreeImage_GetBPP(dib) / 8;
	if(bytesperpixel > 4 || bytesperpixel < 3) {
		return FALSE;
	}
		
	const unsigned height = FreeImage_GetHeight(dib);
	const unsigned pitch = FreeImage_GetPitch(dib);
	const unsigned lineSize = FreeImage_GetLine(dib);
	
	BYTE* line = FreeImage_GetBits(dib);
	for(unsigned y = 0; y < height; ++y, line += pitch) {
		for(BYTE* pixel = line; pixel < line + lineSize ; pixel += bytesperpixel) {
			ERLIMAGEINPLACESWAP(pixel[from], pixel[to]);

		}
	}
	
	return TRUE;
}
/* END UTILS */

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
    FIBITMAP* dib = FreeImage_Load ( FIF_PNG, "lena.png", 0 );
    dib = FreeImage_ConvertTo32Bits(dib);

    int width = FreeImage_GetWidth ( dib );
    int height = FreeImage_GetHeight ( dib );
    int pitch = FreeImage_GetPitch ( dib );
    int bpp = FreeImage_GetBPP ( dib );
    size_t size = (height * width * 4);

    ErlNifBinary out;
    if(!enif_alloc_binary(size, &out)) {
        return enif_make_badarg(env);
    }

    FILE* f = fopen("dumpoutput.txt", "w");
    fprintf(f, "%i,%i,%i,%i;\n", width,height,pitch,bpp);
    
    int bytespp = FreeImage_GetLine(dib) / FreeImage_GetWidth(dib);
    for(unsigned y = 0; y < FreeImage_GetHeight(dib); y++) {

        BYTE *bits = FreeImage_GetScanLine(dib, y);
        for(unsigned x = 0; x < FreeImage_GetWidth(dib); x++) {
            // Set pixel color to green with a transparency of 128
            int pos = (y*width*bytespp)+(x*bytespp);
            out.data[pos+3] = bits[FI_RGBA_RED];
            out.data[pos+2] = bits[FI_RGBA_GREEN];
            out.data[pos+1] = bits[FI_RGBA_BLUE];
            out.data[pos] = bits[FI_RGBA_ALPHA];           
            fprintf(f, "pos:%i,R:%i,G:%i,B:%i,A:%i;\n", pos, out.data[pos+3],out.data[pos+2],out.data[pos+1],out.data[pos]);
            // jump to next pixel
            bits += bytespp;
        }
    }
    fclose(f);

    // FreeImage_ConvertToRawBits (&out.data[1], bitmap, 
    //     pitch, 
    //     bpp, 
    //     FI_RGBA_RED_MASK,
    //     FI_RGBA_GREEN_MASK,
    //     FI_RGBA_BLUE_MASK,
    //     FALSE);

    result = enif_make_tuple(env, 10, 
        mk_atom(env, "width"),
        enif_make_int(env, width),
        mk_atom(env, "height"),
        enif_make_int(env, height),
        mk_atom(env, "pitch"),
        enif_make_int(env, pitch),
        mk_atom(env, "bpp"),
        enif_make_int(env, bpp),
        mk_atom(env, "data"),
        enif_make_binary ( env, &out )
    );

    FreeImage_Unload ( dib );       
    FreeImage_DeInitialise ( );
    enif_free_env(msg_env);
    return result;
}

static ERL_NIF_TERM 
z(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    const ERL_NIF_TERM *arr;
    int arity;
    enif_get_tuple(env, argv[0], &arity, &arr);

    int inwidth, inheight, inpitch, inbpp;
    enif_get_int( env, arr[1], &inwidth);
    enif_get_int( env, arr[3], &inheight);
    enif_get_int( env, arr[5], &inpitch);
    enif_get_int( env, arr[7], &inbpp);

    ErlNifBinary in;    
    if(!enif_inspect_binary(env, arr[9], &in)) {
        return enif_make_badarg(env);
    }

    FreeImage_Initialise ( );


    

   

    /* TEST */    
    // Allocate a 32-bit dib
    FILE* f = fopen("dump.txt", "w");
    fprintf(f, "%i,%i,%i,%i;\n", inwidth,inheight,inpitch,inbpp);
    FIBITMAP *dib = FreeImage_Allocate(inwidth, inheight, inbpp, FI_RGBA_RED_MASK,
    FI_RGBA_GREEN_MASK, FI_RGBA_BLUE_MASK);
    // Calculate the number of bytes per pixel (3 for 24-bit or 4 for 32-bit)
    int bytespp = FreeImage_GetLine(dib) / FreeImage_GetWidth(dib);
    for(unsigned y = 0; y < FreeImage_GetHeight(dib); y++) {

        BYTE *bits = FreeImage_GetScanLine(dib, y);
        for(unsigned x = 0; x < FreeImage_GetWidth(dib); x++) {
            // Set pixel color to green with a transparency of 128
            int pos = (y*inwidth*bytespp)+(x*bytespp);
            bits[FI_RGBA_RED] = in.data[pos+3];
            bits[FI_RGBA_GREEN] = in.data[pos+2];
            bits[FI_RGBA_BLUE] = in.data[pos+1];
            bits[FI_RGBA_ALPHA] = in.data[pos];
            fprintf(f, "pos:%i,R:%i,G:%i,B:%i,A:%i;\n", pos, in.data[pos+3],in.data[pos+2],in.data[pos+1],in.data[pos]);
            // jump to next pixel
            bits += bytespp;
        }
    }
    fclose(f);
    FreeImage_Save ( FIF_PNG, dib, "AaA.png", 0 );
    /* END TEST */
    
    result = mk_ok( env, FreeImage_GetVersion ( ) );
    
    // ErlimageSwap(bitmap, 2, 0);
    // ErlimageSwap(bitmap, 3, 2);
    
    // if ( FreeImage_Save ( FIF_PNG, bitmap, "XxXr.png", 0 ) )
    //     result = mk_ok( env, FreeImage_GetVersion ( ) );

    FreeImage_Unload ( dib );      
    FreeImage_DeInitialise ( );    

    enif_free_env(msg_env);
    return result;
}

static ErlNifFunc nif_funcs[] = {
    {"load", 1, load}
    ,{"t", 0, t}
    ,{"z", 1, z}
};

ERL_NIF_INIT(erlimage, nif_funcs, NULL, NULL, NULL, NULL);
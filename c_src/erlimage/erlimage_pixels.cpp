#include "erlimage.h"

class Erlimage_Pixels {
    public:

        /* Import Erlimage interop data object */
        static ERLIMAGE_L_DO
        importPixels ( ErlNifEnv* env, ERL_NIF_TERM Input ) {
            ERLIMAGE_L_DO Result = {1, 0};

            if ( !enif_is_tuple ( env, Input ) ) return Result;

            const ERL_NIF_TERM *arr;
            int arity;
            enif_get_tuple ( env, Input, &arity, &arr );

            if ( !enif_is_number ( env, arr[1] ) ) return Result;
            if ( !enif_is_number ( env, arr[3] ) ) return Result;
            if ( !enif_is_number ( env, arr[5] ) ) return Result;
            if ( !enif_is_number ( env, arr[7] ) ) return Result;

            int inwidth, inheight, inpitch, inbpp;
            enif_get_int( env, arr[1], &inwidth);
            enif_get_int( env, arr[3], &inheight);
            enif_get_int( env, arr[5], &inpitch);
            enif_get_int( env, arr[7], &inbpp);

            ErlNifBinary in;    
            if(!enif_inspect_binary( env, arr[9], &in)) {
                return Result;
            }

            FIBITMAP *dib = FreeImage_Allocate ( inwidth, inheight, inbpp, 
            FI_RGBA_RED_MASK, FI_RGBA_GREEN_MASK, FI_RGBA_BLUE_MASK);
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
                   
                    // jump to next pixel
                    bits += bytespp;
                }
            } 
            Result.Status = 0;
            Result.Dib = dib;
            return Result;   
        } 

        static ERLIMAGE_I_DO 
        exportPixels ( ErlNifEnv* env, FIBITMAP* Dib ) {
            ERLIMAGE_I_DO Result = {1, 0};

            Dib = FreeImage_ConvertTo32Bits ( Dib );

            int width = FreeImage_GetWidth ( Dib );
            int height = FreeImage_GetHeight ( Dib );
            int pitch = FreeImage_GetPitch ( Dib );
            int bpp = FreeImage_GetBPP ( Dib );
            size_t size = ( height * width * 4 );

            ErlNifBinary out;
            if( !enif_alloc_binary ( size, &out ) ) return Result;            

            int bytespp = FreeImage_GetLine ( Dib ) / FreeImage_GetWidth ( Dib );
            for(unsigned y = 0; y < FreeImage_GetHeight ( Dib ); y++) {

                BYTE *bits = FreeImage_GetScanLine ( Dib , y);
                for(unsigned x = 0; x < FreeImage_GetWidth ( Dib ); x++) {
                    // Set pixel color to green with a transparency of 128
                    int pos = (y*width*bytespp)+(x*bytespp);
                    out.data[pos+3] = bits[FI_RGBA_RED];
                    out.data[pos+2] = bits[FI_RGBA_GREEN];
                    out.data[pos+1] = bits[FI_RGBA_BLUE];
                    out.data[pos] = bits[FI_RGBA_ALPHA];           

                    // jump to next pixel
                    bits += bytespp;
                }
            }

            Result.Status = 0;            
            Result.Data = enif_make_tuple(env, 10, 
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
            return Result;
        }        

}
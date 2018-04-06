
#include <stdio.h>
#include <string.h>
#include <cstdlib>
#include <iostream>
#include <algorithm>
#include <iterator>
#include "FreeImage.h"
#include "libimagequant.h"
#include "dev/lodepng.h"
#include "dev/lodepng.cpp"

using namespace std;



void rgb_to_rgba_callback(liq_color row_out[], int row_index, int width, void *user_info) {

    unsigned char *rgb_row = ((unsigned char *)user_info) + 3*width*row_index;

    for(int i=0; i < width; i++) {
        row_out[i].r = rgb_row[i*3+2];
        row_out[i].g = rgb_row[i*3+1];
        row_out[i].b = rgb_row[i*3];
        row_out[i].a = rgb_row[i*3+4];
    }
}


int liqconv ( FIBITMAP* original, BYTE* raw_rgba_pixels, int width, int height, int pitch  ) {
    liq_attr *handle = liq_attr_create();

    liq_image *input_image = liq_image_create_rgba(handle, raw_rgba_pixels, width, height, 0);
    // You could set more options here, like liq_set_quality

    // liq_image *input_image = liq_image_create_custom(handle, rgb_to_rgba_callback, raw_rgba_pixels, width, height, 0);

    liq_result *quantization_result;
    if (liq_image_quantize(input_image, handle, &quantization_result) != LIQ_OK) {
        fprintf(stderr, "Quantization failed\n");
        return EXIT_FAILURE;
    }

    // Use libimagequant to make new image pixels from the palette

    size_t pixels_size = width * height;
    BYTE *raw_8bit_pixels = (BYTE*) malloc(pixels_size);
    liq_set_dithering_level(quantization_result, 1.0);

    liq_write_remapped_image(quantization_result, input_image, raw_8bit_pixels, pixels_size);
    const liq_palette *palette = liq_get_palette(quantization_result);


    

   
    // FILE* f = fopen("pixbufafter.txt", "w");
    // for ( unsigned int iPix = 0; iPix < (width * height * 4); ++iPix )
    // {       
    //    fprintf( f, "%i \n", raw_8bit_pixels[iPix] );
    // }
    // fclose(f);

    
    // SwapRedBlue32(hImage);
    
    // FreeImage_ConvertTo8Bits ( original );
    // original = FreeImage_ColorQuantize(original, FIQ_WUQUANT );
    
    // RGBQUAD *upal = FreeImage_GetPalette(original);

    // upal = NULL;
    
    // for(int i=0; i < palette->count; i++) {
    //     fprintf( f, "%i, %i, %i, %i \n", palette->entries[i].r, palette->entries[i].g, palette->entries[i].b, palette->entries[i].a );      
    // }    

    // for(int i=0; i < palette->count; i++) {      
    //     upal[i].rgbReserved = palette->entries[i].a;
    //     upal[i].rgbRed = palette->entries[i].r;
    //     upal[i].rgbGreen = palette->entries[i].g;
    //     upal[i].rgbBlue = palette->entries[i].b;      
    // }

    
    FIBITMAP *la = FreeImage_ConvertFromRawBits( raw_8bit_pixels, width, height, width, 0, 0, 0, false);
    RGBQUAD *upal = FreeImage_GetPalette(la);

    for(int i=0; i < palette->count; i++) {      
        upal[i].rgbReserved = palette->entries[i].a;
        upal[i].rgbRed = palette->entries[i].r;
        upal[i].rgbGreen = palette->entries[i].g;
        upal[i].rgbBlue = palette->entries[i].b;      
    }
     
    

  



   // PNG QUANT PALETTE
    // FILE* f = fopen("pixbufafter.txt", "w");
    // fprintf( f, "colors: %i \n", FreeImage_GetColorsUsed(original) );
    //  for(int i=0; i < FreeImage_GetColorsUsed(original); i++) {
    //      fprintf( f, "%i, %i, %i, %i \n", upal[i].rgbReserved, upal[i].rgbRed, upal[i].rgbGreen, upal[i].rgbBlue );      
    // }
    // fclose(f);

    // PNG QUANT PALETTE
    // FILE* f = fopen("pixbufafter.txt", "w");
    //  for(int i=0; i < palette->count; i++) {
    //      fprintf( f, "%i, %i, %i, %i \n", palette->entries[i].r, palette->entries[i].g, palette->entries[i].b, palette->entries[i].a );      
    // }
    // fclose(f);
    

    // FIBITMAP* ZZ = FreeImage_LoadFromMemory(FIF_PNG, Kak);

    if ( FreeImage_Save ( FIF_PNG, la, "zz.png", 0 ) )
        cout << "PNG saved!" << endl;

    

    // LodePNGState state;
    // lodepng_state_init(&state);
    // state.info_raw.colortype = LCT_PALETTE;
    // state.info_raw.bitdepth = 8;
    // state.info_png.color.colortype = LCT_PALETTE;
    // state.info_png.color.bitdepth = 8;

    // for(int i=0; i < palette->count; i++) {
    //    lodepng_palette_add(&state.info_png.color, palette->entries[i].r, palette->entries[i].g, palette->entries[i].b, palette->entries[i].a);
    //    lodepng_palette_add(&state.info_raw, palette->entries[i].r, palette->entries[i].g, palette->entries[i].b, palette->entries[i].a);
    // }

    // unsigned char *output_file_data;
    // size_t output_file_size;
    // unsigned int out_status = lodepng_encode(&output_file_data, &output_file_size, raw_8bit_pixels, width, height, &state);
    // if (out_status) {
    //     fprintf(stderr, "Can't encode image: %s\n", lodepng_error_text(out_status));
    //     return EXIT_FAILURE;
    // }

    // const char *output_png_file_path = "quantized_example.png";
    // FILE *fp = fopen(output_png_file_path, "wb");
    // if (!fp) {
    //     fprintf(stderr, "Unable to write to %s\n", output_png_file_path);
    //     return EXIT_FAILURE;
    // }
    // fwrite(output_file_data, 1, output_file_size, fp);
    // fclose(fp);

    // printf("Written %s\n", output_png_file_path);

    // Done. Free memory.

    liq_result_destroy(quantization_result); // Must be freed only after you're done using the palette
    liq_image_destroy(input_image);
    liq_attr_destroy(handle);

    free(raw_8bit_pixels);
    // lodepng_state_cleanup(&state);
    return 0;
}

/// INPLACESWAP adopted from codeguru.com 
template <class T> void INPLACESWAP(T& a, T& b) {
	a ^= b; b ^= a; a ^= b;
}

BOOL 
SwapRedBlue32(FIBITMAP* dib) {
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
			INPLACESWAP(pixel[0], pixel[2]);
		}
	}
	
	return TRUE;
}



int main ( void ) {
    const char* filename = "q.png";

    // Access bits via FreeImage
    FREE_IMAGE_FORMAT fif;
    FIBITMAP* hImage;
    fif = FreeImage_GetFileType(filename, 0);
    if( fif == FIF_UNKNOWN )
        return false;    
    
    hImage = FreeImage_Load( fif, filename );

    int width = FreeImage_GetWidth( hImage );
    int height = FreeImage_GetHeight( hImage );
    int pitch = FreeImage_GetPitch(hImage);

    // Read image data with FreeImage
    unsigned int imageSize = width * height;

    unsigned char* pData = new unsigned char[imageSize];

    

    /* CONV TO 32bit */
    FIBITMAP* hOldImage = hImage;
    hImage = FreeImage_ConvertTo32Bits( hOldImage );
    FreeImage_Unload( hOldImage );
    /* END TO 32bit */

    FreeImage_FlipVertical(hImage);
    SwapRedBlue32(hImage);
    
    
    BYTE* pPixelData = NULL;
    pPixelData = (BYTE*) FreeImage_GetBits( hImage );
    if ( pPixelData == NULL )
    {
        return false;
    } 

       
    // FILE* f = fopen("pixbuf.txt", "w");
    // for ( unsigned int iPix = 0; iPix < (imageSize * 4); ++iPix )
    // {       
    //    fprintf( f, "%i \n", pPixelData[iPix] );
    // }
    // fclose(f);

   
    liqconv ( hImage, pPixelData, width, height, pitch );

}




// int zz ( void ) {

//     FreeImage_Initialise ( );

//     FIBITMAP* bitmap = FreeImage_Load ( FIF_PNG, "q.png", 0 );

//     if (!bitmap)
//         exit(1);
    
//     FreeImage_ConvertTo24Bits(bitmap);
//     // FIBITMAP* dib8_b = FreeImage_ColorQuantize(bitmap, FIQ_WUQUANT);   
//     // FIBITMAP* dib8_b = FreeImage_ColorQuantizeEx(bitmap, FIQ_WUQUANT, 255, 0, NULL);
//     FreeImage_GetBits(bitmap);
    
//     // FreeImage_Invert ( bitmap );

//     // if ( FreeImage_Save ( FIF_JPEG, bitmap, "flat.jpg", (75 | JPEG_PROGRESSIVE) ) )
//     //     cout << "JPEG saved!" << endl;

//     if ( FreeImage_Save ( FIF_PNG, dib8_b, "flat.png", 0 ) )
//         cout << "PNG saved!" << endl;
    
//     FreeImage_Unload ( bitmap );
//     FreeImage_Unload ( dib8_b );
//     FreeImage_DeInitialise ( );
   
//     return 0;
// }
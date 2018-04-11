#include "erlimage.h"


class Erlimage_Io {
    public:

        static ERLIMAGE_L_DO 
        fileRead ( const char* FilePath ) { 
            ERLIMAGE_L_DO Result = {1, 0};

            FREE_IMAGE_FORMAT FileType = determineFileType ( FilePath );    
            if ( FileType == FIF_UNKNOWN )
                return Result;

            FIBITMAP* dib = FreeImage_Load ( FileType, FilePath, 0 );
            dib = FreeImage_ConvertTo32Bits(dib);

            Result.Status = 0;
            Result.Dib = dib;
            return Result;
        }

        static int 
        writeJpg ( const char* FilePath, FIBITMAP* Dib, int Quality, int Progressive ) {
            Dib = FreeImage_ConvertTo24Bits ( Dib );
            switch ( Progressive ) {
                case 0:
                    if ( FreeImage_Save ( FIF_JPEG, Dib, FilePath, (Quality) ) )
                        return 0;
                case 1:
                    if ( FreeImage_Save ( FIF_JPEG, Dib, FilePath, (Quality | JPEG_PROGRESSIVE) ) )
                        return 0;
            };            
            return 1;
        }

        static int 
        writeWebp ( const char* FilePath, FIBITMAP* Dib, int Quality, int Lossless ) {            
            switch ( Lossless ) {
                case 0:
                    if ( FreeImage_Save ( FIF_WEBP, Dib, FilePath, (Quality) ) )
                        return 0;
                case 1:
                    if ( FreeImage_Save ( FIF_WEBP, Dib, FilePath, (Quality | WEBP_LOSSLESS) ) )
                        return 0;
            };            
            return 1;
        }

    private:

        static FREE_IMAGE_FORMAT 
        determineFileType ( const char* FilePath ) {
            return FreeImage_GetFileType ( FilePath, 0);               
        }

};

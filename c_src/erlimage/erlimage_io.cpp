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
        writeJPG ( const char* FilePath, FIBITMAP* Dib, int Quality, bool Progressive ) {
            Dib = FreeImage_ConvertTo24Bits ( Dib );
            if ( FreeImage_Save ( FIF_JPEG, Dib, FilePath, (75 | JPEG_PROGRESSIVE) ) )
                return 0;
            return 1;
        }

    private:

        static FREE_IMAGE_FORMAT 
        determineFileType ( const char* FilePath ) {
            return FreeImage_GetFileType ( FilePath, 0);               
        }

};

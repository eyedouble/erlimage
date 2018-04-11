#include "erlimage.h"

#include "erlimage.h"

class Erlimage_Transform {
    public:

        /* Import Erlimage interop data object */
        static ERLIMAGE_L_DO
        rescale ( ERLIMAGE_L_DO Input, int Width, int Height, int Algorithm ) {           
            ERLIMAGE_L_DO Result = {1, 0};

            Input.Dib = FreeImage_Rescale ( Input.Dib, Width, Height, FILTER_BICUBIC );

            if ( !Input.Dib )  return Result;
           
            Result.Status = 0;
            Result.Dib = Input.Dib;
            return Result;   
        } 
};
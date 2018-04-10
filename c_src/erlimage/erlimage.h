
#ifndef ERLIMAGE_H
#define ERLIMAGE_H

#include <stdio.h>
#include <string.h>
#include <cstdlib>
#include <iostream>
#include "erl_nif.h"
#include "FreeImage.h"


/* Local Data object */
typedef struct ERLIMAGE_L_DO {
  int Status;
  FIBITMAP *Dib;
} ERLIMAGE_L_DO;

/* Data object used for interoparatablity with erlang */
typedef struct ERLIMAGE_I_DO {
  int Status;
  ERL_NIF_TERM Data;
} ERLIMAGE_I_DO;

#endif // ERLIMAGE_H
#ifndef __BASE64_H__
#define __BASE64_H__

#include <string.h>
#include <stdio.h>
#include <unistd.h>
//#include <unistd.h>

typedef struct {
  char *data;  /* NULL on error */
  int size;    /* contain error code */
} OZ_Datum;

#endif

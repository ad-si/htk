#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "bdbcommon.h"

void mess(const char *fmt,...) {
   va_list ap;
   va_start(ap,fmt);
   vfprintf(stderr,fmt,ap);
   va_end(ap);
   fprintf(stderr,"\n");
   }

void run_db(const char *name,int ret) {
  if(ret) {
    mess("bdbclient: BDB function %s had error %s",name,db_strerror(ret));
    exit(EXIT_FAILURE);
    }
  }


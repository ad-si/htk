/* copy_file is a routine for copying files as fast as
   possible.  We need to do this sometimes for moving stuff
   in and out of the CVS cache.
   */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "copy_file.h"

#define BUFFER 16384 /* used for contents of file. */
int copy_file(const char *from,const char *to) {
   FILE *in;
   int result = 0;
   char *error = ""; /* used for additional error messages */

   in = fopen(from,"rb");
   if(!in) {
      result = EFROMBAD;
      error = strerror(errno);
      }
   else {
      FILE *out = fopen(to,"wb");
      if(!out) {
         result = ETOBAD;
         error = strerror(errno);
         }
      else {
         char *buffer = (char *) malloc(BUFFER);
         if(!buffer) {
            result = ENOBUFFMEM;
            }
         else {
            size_t nread,nwritten;
            do {
               nread = fread(buffer,sizeof(char),(size_t)BUFFER,in);
               nwritten = fwrite(buffer,sizeof(char),nread,out);
               } while ((nread>0)&&(nwritten>0));
            if(ferror(in)) result=EFROMBAD;
            if(ferror(out)) result=ETOBAD;
            free(buffer);
            }
         fclose(out);
         }
      fclose(in);
      }
   if(result) {
     fprintf(stderr,"copy_file.c: result %d from %s to %s\n error %s",
        result,from,to,error);
     } 
   return result;
   }

   

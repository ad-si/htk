/* This C program writes out to EVERYTHING.hs a Haskell module 
   EVERYTHING which imports each of the modules given to it as an 
   argument.  The point is that gmake libfasthere can get ghc --make
   to do import-chasing for a whole directory starting from just this
   file. */
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

int main(int argc,char *argv[]) {
   FILE *every = fopen("EVERYTHING.hs","w");
   const char *last_slash;
   int i;
   if(every==NULL) {
      fprintf(stderr,"Couldn't open EVERYTHING.hs\n");
      exit(EXIT_FAILURE);
      }
   /* We check for errors at the very end, but don't do it otherwise */
   fputs("module EVERYTHING where\n",every);
   for(i=1;i<argc;i++) {
      const char *module_file = argv[i];
      int len = strlen(module_file);
      int len_to_print;

      /* Remove a .hs or .lhs suffix */
      if(len>=4) {
         if(strcmp(module_file+(len-3),".hs")==0) {
            len_to_print = len-3;
            }
         else if(strcmp(module_file+(len-4),".lhs")==0) {
            len_to_print = len-4;
            }
         else {
            fprintf(stderr,
               "Module file name %s doesn't end in .hs or .lhs\n",
               module_file);
            exit(EXIT_FAILURE);
            }
         }
      else {
         fprintf(stderr,
            "Module file name %s is too short\n",module_file);
         exit(EXIT_FAILURE);
         }

      /* Cut off up to rightmost "/" character, if any. */
      last_slash = strrchr(module_file,'/');
      if(last_slash!=NULL) {
         const char *real_start=last_slash+1;
         len_to_print -= (real_start-module_file);
         module_file = real_start;       
         }
      fprintf(every,"import %.*s\n",len_to_print,module_file);
      }
   if(ferror(every)) {
      fprintf(stderr,"Error writing to EVERYTHING.hs\n");
      exit(EXIT_FAILURE);
      }
   if(fclose(every)) {
      fprintf(stderr,"Error closing EVERYTHING.hs\n");
      exit(EXIT_FAILURE);
      }
   return EXIT_SUCCESS;
   }

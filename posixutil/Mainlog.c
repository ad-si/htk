/* This program is a filter which takes one argument, a filename, and
   writes everything which passes through it to that file.  This is
   used by daVinci.debug and wish.debug. */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>


void do_error(const char *mess) {
   fprintf(stderr,"Error %s errno = %d\n",mess,errno);
   exit(EXIT_FAILURE);
   }

int main(int argc,char *argv[]) {
   if(argc==2) {
      FILE *log = fopen(argv[1],"w");
      if(!log) do_error("Couldn't open log file.\n");
      for(;;) {
         int ch;
         ch = getchar();
         if(ch==EOF) break;
         putchar(ch);
         fflush(stdout);
         putc(ch,log);
         fflush(log);
         }
      }
   else do_error("Needs exactly one argument, the name of the log file");
   exit(EXIT_SUCCESS);
   }

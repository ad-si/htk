/* Program to create a new temporary directory, and output its name.
   This should have just one argument, the start of the directory name.
   The rest will be modified appropriately. */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>

#include <sys/stat.h>
#include <sys/types.h>

#include "config.h"


#define MODE 0700

/* Error message routine */
void error(const char *fmt,...) {
   va_list ap;
   va_start(ap,fmt);
   vfprintf(stderr,fmt,ap);
   va_end(ap);
   exit(EXIT_FAILURE);
   }

#if HAVE_MKDTEMP
#define MKDTEMP mkdtemp
#else

#define DIRSEP (WINDOWS ? '\\' : '/')

/* Need to write our own version of the Linux function mkdtemp.  Except
   that since we are its only user we don't check the argument more than
   the absolute minimum, and feel free to clobber it. */
char *MKDTEMP(char *template) {
  int len;
  char *basename1,*basename,*dirname,*result;

  /* Remove XXXXXX from end */
  len = strlen(template);

  if(len<=6) {
      error("Argument %s to MKDTEMP needs to end XXXXXX",template);
      } 

  template[len-6]='\0';
  basename1=strrchr(template,DIRSEP);

  if (basename1) {
     basename = basename1 + 1;
     *basename1 = '\0'; 
     dirname = template;
     }
   else {
      basename = template;
      dirname = NULL;
      }
     
   basename[5]='\0'; 
       /* Truncate basename to 5 characters if necessary.  (If not,
          this will clobber one of the X's, since 6>5 */

   for(;;) {
      int failure;

      result = tempnam(dirname,basename);
      if(!result) {
         error("tempname returned NULL; out of memory?");
         }
      failure = mkdir(result,MODE);
      if(failure) {
         if (errno == EEXIST) { /* race condition */
            free(result);
            continue;
            }
         else { 
            error("mkdir returned %s: ",strerror(errno));
            }
         }
      else
         break;
      }

   return result;
   }

#endif




int main (int argc,char *argv []) {
   char *template;
   size_t t_len;
   char *result;
   if (argc != 2) {
      error("%s: Must have one argument",argv[0]);
      }
   t_len = strlen (argv[1]);
   
   template = malloc(t_len + 7);
   if (!template) {
      error("%s: no memory",argv[0]);
      }

   strcpy (template,argv[1]);
   strcpy (&(template[t_len]),"XXXXXX");
   umask(0777 & ~MODE); /* This prevents anyone else reading the directory */
   result = MKDTEMP(template);
   if(result) {
      puts(result);
      exit(EXIT_SUCCESS);
      }
   else {
      error("mkdtemp returned %s",strerror(errno));
      }
   }



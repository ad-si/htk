/* Program to create a new temporary directory, and output its name.
   This should have just one argument, the start of the directory name.
   The rest will be modified appropriately. */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


int main (int argc,char *argv []) {
   char *template;
   size_t t_len;
   char *result;
   if (argc != 2) {
      fprintf(stderr,"%s: Must have one argument",argv[0]);
      exit(EXIT_FAILURE);
      }
   t_len = strlen (argv[1]);
   
   template = malloc(t_len + 7);
   if (!template) {
      fprintf(stderr,"%s: no memory",argv[0]);
      exit(EXIT_FAILURE);
      }

   strcpy (template,argv[1]);
   strcpy (&(template[t_len]),"XXXXXX");
   result = mkdtemp(template);
   if(result) {
      puts(result);
      exit(EXIT_SUCCESS);
      }
   else {
      perror(argv[0]);
      exit(EXIT_FAILURE);
      }
   }

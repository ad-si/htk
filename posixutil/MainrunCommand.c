/* C program which reads a line from stdin then executes it as a command
   via the system function.  The exit code is output to stderr, as a line
   "\nEXITCODE %d\n".  This peculiar arrangement is to provide a workaround
   for reactor/ExecCommand.hs which allows us to know when a process finished.

   */
/* Maximum command length */
#define COMMANDLEN 2000

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void exitcode (int code) {
   fprintf(stderr,"\nEXITCODE %d\n",code);
   exit(EXIT_SUCCESS);
   }

int main() {
   char command[COMMANDLEN+2];
   int length,code;

   fgets(command,COMMANDLEN+2,stdin);
   length = strlen(command);
   if(command[length-1] != '\n') {
      fprintf(stderr,"runCommand: command too long.");
      exitcode(-1);
      }
   code = system(command);
   exitcode(code);
   }

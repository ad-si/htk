/* Function needed by ChildProcess to fork a thread and execute a process
   in it.

   We don't this in Haskell because there is NO WAY of preventing inconvenient
   Haskell threads running between when the thread is forked and the process
   is executed. */

#include <unistd.h>

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "forkExec.h"


extern int execvpe(char *, char **, char **); 
   /* provided, somehow, by Haskell RTS.  Analog of execvp, taking an
      environment */

void do_error (const char *path,const char *mess);
   /* output an error in a standard format. */

void debug_args(char *args[]);
   /* dump the list of strings provided to standard input */
/* Returns 0 if the fork did not succeed and prints a message
   "Attempt to start program . . ."
   to standard output if the exec fails.

   path is where to find the program.  If it has no slashes we search 
      PATH for it.
   the second argument is an argv array to use.  Thus argv[0] corresponds
      to the name of the program (or is probably redundant, since we've
      given that in path anyway); argv[1..] are the arguments.  After the
      last argument the array should contain a null pointer.
   the third argument is the environment to use.  This is, like the
      argv array, terminated with a null pointer.  Each value in it should be
      a string of the form key=value.  If the third argument is NULL, the
      environment is inherited in the normal way.

   the fourth argument fds specifies the plumbing for the child process.
   fds[0],fds[1],fds[2] should be the stdin, stdout and stderr the
   child process is to use.  In early versions of this function these
   were passed as separate elements, but unfortunately this didn't work
   because of a bug in GHC5.04.
   */
pid_t fork_exec (char *path,char * argv[],char * envp[],int fds[]) {
   pid_t pid;

#if 0
   fprintf(stderr,"%s\n",path);
   debug_args(argv);
   debug_args(envp);
#endif

   pid = fork ();
   if (pid < 0) { /* fork failed */
      do_error(path,"Fork failed");
      return 0;
      }
   else if (pid > 0) { /* fork succeeded and this is parent */
      return pid;
      }
   else { /* fork succeeded and this is child */
      if ((dup2 (fds[0],STDIN_FILENO) == -1) ||
          (dup2 (fds[1],STDOUT_FILENO) == -1) ||
          (dup2 (fds[2],STDERR_FILENO) == -1 )
          ) do_error (path,"dup2 failed");
      if (envp) {
         execvpe (path,argv,envp);
         }
      else {
         execvp (path,argv);
         }
      do_error(path,"Exec failed");
      exit(EXIT_FAILURE);
      }
   }

void do_error (const char *path,const char *mess) {
   char space[500];
   snprintf(space,500,"Attempt to start program %s failed: %s errno=%d\n",path,
      mess,(int) errno);
   write(STDERR_FILENO,space,strlen(space));
   }

void debug_args(char *args[]) {
   if(args) {
      for(;*args;args++) fprintf(stderr,"%s\n",*args);
      fprintf(stderr,"END\n");
      }
   else fprintf(stderr,"NONE\n");
   }
/* Function needed by ChildProcess to fork a thread and execute a process
   in it.

   We don't this in Haskell because there is NO WAY of preventing inconvenient
   Haskell threads running between when the thread is forked and the process
   is executed. */

#include <sys/types.h>

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
pid_t fork_exec (char *path,char * argv[],char * envp[],int fds[]);


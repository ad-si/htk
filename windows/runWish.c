/* Adapted from 
      http://msdn.microsoft.com/library/en-us/dllproc/prothred_4uus.asp 
*/
 
#include <stdio.h> 
#include <windows.h> 

/* Now follows the mingw interface, which we will use for reading
  from wish */ 
#include <io.h>
#include <fcntl.h> 

#include "runWish.h"
 
static HANDLE hChildStdinRd, hChildStdinWr, hChildStdinWrDup, 
   hChildStdoutRd, hChildStdoutWr, hChildStdoutRdDup, 
   hSaveStdin, hSaveStdout; 

static int hChildStdoutRdDupFd;
 
static BOOL CreateChildProcess(char *wish_path); 
static VOID ErrorExit(LPTSTR); 

void initialise_wish(char *wish_path) { 

   SECURITY_ATTRIBUTES saAttr; 
   BOOL fSuccess; 
 
// Set the bInheritHandle flag so pipe handles are inherited. 
 
   saAttr.nLength = sizeof(SECURITY_ATTRIBUTES); 
   saAttr.bInheritHandle = TRUE; 
   saAttr.lpSecurityDescriptor = NULL; 
 
   // The steps for redirecting child process's STDOUT: 
   //     1. Save current STDOUT, to be restored later. 
   //     2. Create anonymous pipe to be STDOUT for child process. 
   //     3. Set STDOUT of the parent process to be write handle to 
   //        the pipe, so it is inherited by the child process. 
   //     4. Create a noninheritable duplicate of the read handle and
   //        close the inheritable read handle. 
 
// Save the handle to the current STDOUT. 
 
   hSaveStdout = GetStdHandle(STD_OUTPUT_HANDLE); 
 
// Create a pipe for the child process's STDOUT. 
 
   if (! CreatePipe(&hChildStdoutRd, &hChildStdoutWr, &saAttr, 0)) 
      ErrorExit("Stdout pipe creation failed\n"); 
 
// Set a write handle to the pipe to be STDOUT. 
 
   if (! SetStdHandle(STD_OUTPUT_HANDLE, hChildStdoutWr)) 
      ErrorExit("Redirecting STDOUT failed"); 
 
// Create noninheritable read handle and close the inheritable read 
// handle. 

    fSuccess = DuplicateHandle(GetCurrentProcess(), hChildStdoutRd,
        GetCurrentProcess(), &hChildStdoutRdDup , 0,
        FALSE,
        DUPLICATE_SAME_ACCESS);
    if( !fSuccess )
        ErrorExit("DuplicateHandle failed");
    CloseHandle(hChildStdoutRd);

   // The steps for redirecting child process's STDIN: 
   //     1.  Save current STDIN, to be restored later. 
   //     2.  Create anonymous pipe to be STDIN for child process. 
   //     3.  Set STDIN of the parent to be the read handle to the 
   //         pipe, so it is inherited by the child process. 
   //     4.  Create a noninheritable duplicate of the write handle, 
   //         and close the inheritable write handle. 
 
// Save the handle to the current STDIN. 
 
   hSaveStdin = GetStdHandle(STD_INPUT_HANDLE); 
 
// Create a pipe for the child process's STDIN. 
 
   if (! CreatePipe(&hChildStdinRd, &hChildStdinWr, &saAttr, 0)) 
      ErrorExit("Stdin pipe creation failed\n"); 
 
// Set a read handle to the pipe to be STDIN. 
 
   if (! SetStdHandle(STD_INPUT_HANDLE, hChildStdinRd)) 
      ErrorExit("Redirecting Stdin failed"); 
 
// Duplicate the write handle to the pipe so it is not inherited. 
 
   fSuccess = DuplicateHandle(GetCurrentProcess(), hChildStdinWr, 
      GetCurrentProcess(), &hChildStdinWrDup, 0, 
      FALSE,                  // not inherited 
      DUPLICATE_SAME_ACCESS); 
   if (! fSuccess) 
      ErrorExit("DuplicateHandle failed"); 
 
   CloseHandle(hChildStdinWr); 
 
// Now create the child process. 
 
   if (! CreateChildProcess(wish_path)) 
      ErrorExit("Create process failed"); 

// Make an fd value corresponding to the childs output
   hChildStdoutRdDupFd = _open_osfhandle ((long) hChildStdoutRdDup,_O_RDONLY);
 
// After process creation, restore the saved STDIN and STDOUT. 
 
   if (! SetStdHandle(STD_INPUT_HANDLE, hSaveStdin)) 
      ErrorExit("Re-redirecting Stdin failed\n"); 
 
   if (! SetStdHandle(STD_OUTPUT_HANDLE, hSaveStdout)) 
      ErrorExit("Re-redirecting Stdout failed\n"); 
} 
 
static BOOL CreateChildProcess(char *wish_path) 
{ 
   PROCESS_INFORMATION piProcInfo; 
   STARTUPINFO siStartInfo; 
 
// Set up members of the PROCESS_INFORMATION structure. 
 
   ZeroMemory( &piProcInfo, sizeof(PROCESS_INFORMATION) );
 
// Set up members of the STARTUPINFO structure. 
 
   ZeroMemory( &siStartInfo, sizeof(STARTUPINFO) );
   siStartInfo.cb = sizeof(STARTUPINFO); 
 
// Create the child process. 
 
   return CreateProcess(NULL, 
      wish_path,     // command line 
      NULL,          // process security attributes 
      NULL,          // primary thread security attributes 
      TRUE,          // handles are inherited 
      0,             // creation flags 
      NULL,          // use parent's environment 
      NULL,          // use parent's current directory 
      &siStartInfo,  // STARTUPINFO pointer 
      &piProcInfo);  // receives PROCESS_INFORMATION 
}

void write_to_wish(const char *toWrite,size_t nChars) {
   DWORD dwWritten;

   WriteFile(hChildStdinWrDup,toWrite,(DWORD)nChars,&dwWritten,NULL);
   }

int get_readwish_fd() {
   return hChildStdoutRdDupFd;
   }

size_t read_from_wish(char *buffer,size_t bufferSize) { 
   /* Use the mingw function. */
   return (size_t) _read(hChildStdoutRdDupFd,(void *)buffer,
      (unsigned int) bufferSize);
   }
 

size_t read_from_wish_avail() {
   DWORD lpTotalBytesAvail;
   BOOL success;

   success = PeekNamedPipe(hChildStdoutRdDup,NULL,0,NULL,&lpTotalBytesAvail,
      NULL);

   if(success) {
      return (size_t) lpTotalBytesAvail;
      }
   else {
      ErrorExit("PeekNamedPipe failed");
      }
   }    

static VOID ErrorExit (LPTSTR lpszMessage) 
{ 
   fprintf(stderr, "%s\n", lpszMessage); 
   ExitProcess(0); 
} 
 

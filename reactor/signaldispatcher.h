/* #########################################################################

MODULE        : signaldispatcher.h
AUTHOR        : Einar Karlsen,  
		University of Bremen
		email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION	      : alpha
DESCRIPTION   : Signature of signal dispatcher state 


   ######################################################################### */


#ifndef _SIGNALDISPATCHER_H
#define _SIGNALDISPATCHER_H

void SetSignalFD (int fd);

/* Signal handler passed by Signal.hs to the standard C "signal" function */ 
void SendSignalFD(int signo);

#endif

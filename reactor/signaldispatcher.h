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

#include "Stg.h"

void SetSignalFD (int fd);

void SendSignalFD(int signo);

#endif

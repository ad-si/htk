/* #########################################################################

MODULE        : signaldispatcher.h
AUTHOR        : Einar Karlsen, George  
		University of Bremen
		email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION	      : alpha
DESCRIPTION   : Signature of signal dispatcher state 


   ######################################################################### */



#ifndef _COLLECTOR_H
#define _COLLECTOR_H

#include "Stg.h"

typedef void *address;

extern void SetCollectorFD (int fd);

extern void SendCollectorFD(address addr);

extern address AddrSendCollectorFD(void);

extern address NewAddr(void);

extern void FreeAddr(address addr);

#endif

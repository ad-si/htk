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
/* Sets the Fd on which to send collection messages */

extern void SendCollectorFD(address addr);
/* Send the attached Addr on the collection messages Fd. */

extern address AddrSendCollectorFD(void);
/* equals address of SendCollectorFD.  Thus I think this is the
   address to be given to ForeignObj. */

/* returns a new unique address. (from malloc(1).) */
extern address NewAddr(void);

/* frees the address */
extern void FreeAddr(address addr);

#endif

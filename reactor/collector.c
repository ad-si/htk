/* #########################################################################

MODULE        : collector.c
AUTHOR        : Einar Karlsen, George 
		University of Bremen
		email:  ewk@informatik.uni-bremen.de
DATE          : 1999
VERSION	      : alpha
DESCRIPTION   : Implementation of object collector 


   ######################################################################### */


#include "collector.h"
#include "Stg.h"

int collectorfd;


void SetCollectorFD (int fd) {
   collectorfd = fd;
   };


void SendCollectorFD(address addr) {
   char buf[255];
   sprintf(buf,"%d\n",addr);
   printf("sending object id %d over pipe %d \n",addr,collectorfd);
   write(collectorfd, buf, strlen(buf));	
   };

address AddrSendCollectorFD(void) {
   return ((void *) (SendCollectorFD));
   };


address NewAddr(void) {
   return (malloc(1));
   };


void FreeAddr(address addr) {
   free(addr);
   };


/* #########################################################################

MODULE        : signaldispatcher.c
AUTHOR        : Einar Karlsen,  
		University of Bremen
		email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION	      : alpha
DESCRIPTION   : Implementation of signal dispatcher state 


   ######################################################################### */


#include "signaldispatcher.h"


int signalfd;


extern void SetSignalFD (int fd) {
	signalfd = fd;
};


/* Signal handler passed by Signal.hs to the standard C "signal" function */ 
extern void SendSignalFD(int signo) {
	char buf[255];
	sprintf(buf,"%d\n",signo);
	printf("sending signal %d over pipe %d \n",signo,signalfd);
	write(signalfd, buf, strlen(buf));
	signal(signo,SendSignalFD); 	
};



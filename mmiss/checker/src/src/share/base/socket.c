/* Kommunikation fuer LCL3.0 */

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

/*
 Compiling ...

 for sun
  cc -c conn.c


 for apollo:
  cc -W0,-pic -r -a -o conn conn.c
*/

/* die ueblichen includes fuer Kommunikation */
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>


/* Dummy-Routine, auf Apollos noetig ?? */
/* main() {} */

int usocket() {
  int s = socket (AF_INET, SOCK_STREAM, 0);
  if (s < 0) {
    perror ("socket");
  }
  return s;
}
		    
int ubind(int port) {
  struct sockaddr_in insock;
  int s = usocket();
  if (s < 0) {
    return -1;
  }

  memset((char *)&insock, 0, sizeof (insock));
  insock.sin_family = AF_INET;
  insock.sin_port = htons ((unsigned short) port);
  insock.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(s, (struct sockaddr *) &insock, sizeof (insock))) {
    perror ("bind");
    return -1;
  }

  if (listen (s, 5) < 0) {
    perror ("listen");
    return -1;
  }
  return s;
}


int uaccept(int s)
{

/* Warten */
  int s1;
  /*  while ((s1 = accept (s, (struct sockaddr *) NULL, (int *)NULL)) < 0) */
  while ((s1 = accept (s, NULL, NULL)) < 0)
      {
	  if (errno != EINTR) 
	      {
		  perror("accept");
		  break;
	      }
      }

  return s1;
}

int uclose(int s) {
  return close(s);
}


int uconnect(char *host,int port)
{
  int flags = O_NONBLOCK;
  struct sockaddr_in insock;
  struct hostent *hp = gethostbyname(host);
  int s = usocket();

  if (s < 0) {
    return -1;
  }

  if (hp == NULL) {
    perror("gethostbyname");
    return -1;
  }

  memset((char *)&insock, 0, sizeof (insock));
  insock.sin_family = AF_INET;
  insock.sin_port = htons ((unsigned short) port);
  memcpy((char *) &insock.sin_addr,hp->h_addr,hp->h_length);
		
  if (connect(s,(struct sockaddr *) &insock,sizeof(insock)) < 0) {
    perror("connect");
    return -1;
  }

  if (fcntl(s, F_SETFL, flags) < 0) {
      perror("fcntl");
      return -1;
  }

  return s;
}


/* String von Socket in Puffer lesen */
int uread(int socket, int n, char *buf)
{
  int flags = O_NONBLOCK;
  int ret;

  if (fcntl(socket, F_SETFL, flags) < 0) {
      perror("fcntl");
      return -1;
  }

  while ((ret = read(socket,buf,n)) < 0) 
      {
	  if (errno == EAGAIN) 
	      {
		  /* Nothing on the socket returning -2 */
		  return -2;
	      }
	  else 
	      {
	      perror("read");
	      return -1;
	      }
      }
  
  buf[ret] = 0;

  return ret;
}


/* String von Socket in Puffer lesen */
int ureadwait(int socket, int n, char *buf)
{
  int ret;
  int bflags = 0;
  int nflags = O_NONBLOCK;

  if (fcntl(socket, F_SETFL, bflags) < 0) {
      perror("fcntl");
      return -1;
  }

  ret = read(socket,buf,n);

  if (fcntl(socket, F_SETFL, nflags) < 0) {
      perror("fcntl");
      return -1;
  }

  buf[ret] = 0;

  return ret;
}


/* String ueber Socket wegschicken */
int uwrite(int socket,char *str)
{
  int ret;

  while ((ret = write(socket,str,strlen(str))) < 0) {
    if (errno != EINTR) {
      perror("write");
      return -1;
    }
    str += ret;
  }
  return 0;
}

/* Temporaeren Dateinamen erzeugen */
char *utmpnam() {
  return tmpnam(NULL);
}


/* get the IP-Address of the connected host */
int ugetpeername(int sd)
{

  struct sockaddr_in name;
  int namelen = sizeof(name);

  if (getpeername(sd, (struct sockaddr *)&name, &namelen) < 0) {
    perror("getpeername");
    return -1;
  }
  
  
 return inet_ntoa(name.sin_addr);
   
}


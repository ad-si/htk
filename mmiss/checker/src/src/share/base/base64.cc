// This is a port for Mozart, original file header:
/*
Dave Winer, dwiner@well.com, UserLand Software, 4/7/97
 
I built this project using Symantec C++ 7.0.4 on a Mac 9500.
 
We needed a handle-based Base 64 encoder/decoder. Looked around the
net, found a bunch of code that couldn't easily be adapted to 
in-memory stuff. Most of them work on files to conserve memory. This
is inelegant in scripting environments such as Frontier.
 
Anyway, so I wrote an encoder/decoder. Docs are being maintained 
on the web, and updates at:
 
http://www.scripting.com/midas/base64/
 
If you port this code to another platform please put the result up
on a website, and send me a pointer. Also send email if you think this
isn't a compatible implementation of Base 64 encoding.
 
BTW, I made it easy to port -- layering out the handle access routines.
Of course there's a small performance penalty for this, and if you don't
like it, change it. Thanks!
*/

//#include "mozart.h"
//#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include "base64.h"

// main () 
// {}

static char encodingTable [64] = {

    'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',    
    'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
    'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
    'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
}; 


static bool isdatachar (unsigned char alpha) {
    return ( (alpha >= 'A') && (alpha <= 'Z') ) ||
        ( (alpha >= 'a') && (alpha <= 'z') ) ||
        ( (alpha >= '0') && (alpha <= '9') ) ||
        ( alpha == '+' ) ||
        ( alpha == '/' );
    //  ( alpha == '=' ) is a fillchar, not a datachar
}

static unsigned char getdatachar (OZ_Datum *h, unsigned int ix) {
    
    return (((*h).data) [ix]);
}

static void putdatachar (OZ_Datum *h, unsigned int ix, unsigned char ch) {
    //printf("[%i] %c ", ix, ch);
    ((*h).data) [ix] = ch;
}


static bool encodedatum (OZ_Datum *htext, OZ_Datum *h64, short linelength) {
    /*
      encode the datum. some funny stuff about linelength -- it only makes
      sense to make it a multiple of 4. if it's not a multiple of 4, we make
      it so (by only checking it every 4 characters. 
      
      further, if it's 0, we don't add any line breaks at all.
    */
    
    unsigned int origsize = 0;
    unsigned int diffsize = 0;
    int ctremaining;
    unsigned char ch;
    unsigned char inbuf [3], outbuf [4];
    short i;
    short charsonline = 0, ctcopy;
    
    unsigned int ixtext = 0;
    unsigned int insize = (*htext).size;
    //char *indata         = (*htext).data;
    //char *outdata        = (*h64).data;
    
    while (true) {
        
        ctremaining = insize - ixtext;
        
        if (ctremaining <= 0)
            break;
        
        for (i = 0; i < 3; i++) { 
            
            unsigned int ix = ixtext + i;
            
            if (ix < insize)
                inbuf [i] = getdatachar (htext, ix);
            else
                inbuf [i] = 0;
        } /*for*/
        
        outbuf [0] = (inbuf [0] & 0xFC) >> 2;        
        outbuf [1] = ((inbuf [0] & 0x03) << 4) | ((inbuf [1] & 0xF0) >> 4);
        outbuf [2] = ((inbuf [1] & 0x0F) << 2) | ((inbuf [2] & 0xC0) >> 6);
        outbuf [3] = inbuf [2] & 0x3F;
        
        origsize += diffsize;
        
        diffsize = 4;
        
        ctcopy = 4;    

        switch (ctremaining) {
        case 1: 
            ctcopy = 2;
            break;
        case 2: 
            ctcopy = 3;
            break;
        } /*switch*/
        
        //printf("ctcopy is %i\n",ctcopy);
	
        for (i = 0; i < ctcopy; i++)
            putdatachar (h64, origsize + i, encodingTable [outbuf [i]]);
        
        for (i = ctcopy; i < 4; i++)
            putdatachar (h64, origsize + i, '=');
        
        ixtext += 3;
        
        charsonline += 4;
        
        if (linelength > 0) { /*DW 4/8/97 -- 0 means no line breaks*/
            
            if (charsonline >= linelength) {
                
                charsonline = 0;
                
                origsize += diffsize;
                
                diffsize = 1;
                
                putdatachar (h64, origsize, '\n');
            }
        }
    } /*while*/
    
    return(true);
         
} /*encodedatum*/

        
static bool decodedatum (OZ_Datum *h64, OZ_Datum *htext) {
    
    int  origsize = 0;
    int  diffsize = 0;
    unsigned int ctremaining;
    unsigned char ch;
    unsigned char inbuf [3], outbuf [4];
    short i, ixinbuf;
    bool flignore;
    bool flendtext = false;
    
    unsigned int ixtext = 0;
    unsigned int insize = (*h64).size;
    //char *indata      = (*h64).data;
    //char *outdata     = (*htext).data;
    
    ixinbuf = 0;
    
    while (true) {
	
        if (ixtext >= insize)
            break;
        
        ch = getdatachar (h64, ixtext++);
	
        flignore = false;
	
        if      ((ch >= 'A') && (ch <= 'Z'))  ch = ch - 'A';
        else if ((ch >= 'a') && (ch <= 'z'))  ch = ch - 'a' + 26;
        else if ((ch >= '0') && (ch <= '9'))  ch = ch - '0' + 52;	
        else if (ch == '+')                   ch = 62;
        else if (ch == '/')                   ch = 63;      
        else if (ch == '=') /*noop -- can't ignore this one*/ 
            flendtext = true;
        else
            flignore = true;
        
        if (!flignore) {
            
            int ctcharsinbuf = 3;
            bool flbreak = false;
            
            if (flendtext) {
                
                if (ixinbuf == 0)
                    break;
                
                if ((ixinbuf == 1) || (ixinbuf == 2))
                    ctcharsinbuf = 1;
                else
                    ctcharsinbuf = 2;
                
                ixinbuf = 3;
                
                flbreak = true;
            }
            
            inbuf [ixinbuf++] = ch;
            
            if (ixinbuf == 4) {
                
                ixinbuf = 0;
                
                outbuf[0]= ( inbuf[0]         << 2) | ((inbuf[1] & 0x30) >> 4);
                outbuf[1]= ((inbuf[1] & 0x0F) << 4) | ((inbuf[2] & 0x3C) >> 2);
                outbuf[2]= ((inbuf[2] & 0x03) << 6) | ( inbuf[3] & 0x3F);
                
                origsize += diffsize;
                
                diffsize = ctcharsinbuf;
                
                for (i = 0; i < ctcharsinbuf; i++)
                    putdatachar (htext, origsize + i, outbuf [i]);
            }
            
            if (flbreak)
                break;
        }
    } /*while*/
    
 exit:
    
    return (true);
} /*decodedatum*/

extern "C"
int b64encode (char *input, int inputsize, char *output, int outputsize, int linelength)
{
OZ_Datum hinput;
OZ_Datum houtput;

hinput.size = inputsize;
hinput.data = input;

houtput.size = outputsize;
houtput.data = output;

encodedatum (&hinput, &houtput, linelength);
printf("Encoding is %s\n", output);
return(0);
} 

extern "C"
int b64decode (char *input, int inputsize, char *output, int outputsize)
{
OZ_Datum hinput;
OZ_Datum houtput;

hinput.size = inputsize;
hinput.data = input;

houtput.size = outputsize;
houtput.data = output;

decodedatum (&hinput, &houtput);

return(0);
}

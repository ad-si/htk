#ifndef NEW_OBJECT_H
#define NEW_OBJECT_H

/* This C function returns a unique int each time it is called.
   (Up to a limit of 2^32 iterations).

   Limitations: 
      1) not thread-safe
      2) not unique between machines.
      3) could conceivably overflow.
   */

int next_object_id();

#endif

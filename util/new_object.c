/* This C function returns a unique int each time it is called.
   (Up to a limit of 2^32 iterations).

   Limitations: 
      1) not thread-safe
      2) not unique between machines.
      3) could conceivably overflow
   */
#include "new_object.h"

static int object_id_source=0;

int next_object_id() {
   return object_id_source++;
   }

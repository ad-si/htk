/* This C function returns a unique double each time it is called.
   (Up to a limit of 2^54+1 iterations).

   Limitations: 
      1) not thread-safe
      2) not unique between machines
      3) (for speed) doesn't check when 2^54 limit is reached.
   */


static double object_id_source=-9007199254740992.0;
/* the least double distinct from it+1. */

double next_object_id() {
   return object_id_source++;
   }

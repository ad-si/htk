/* This C function returns a unique int each time it is called.
   (Up to a limit of 2^32 iterations).

   Limitations: 
      1) not thread-safe
      2) not unique between machines.
   */

int next_object_id();

/* This C function returns a unique double each time it is called.
   (Up to a limit of 2^54+1 iterations).

   Limitations: 
      1) not thread-safe
      2) not unique between machines.
   */

double next_object_id();

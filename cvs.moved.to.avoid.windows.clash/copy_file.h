/* copy_file is a routine for copying files as fast as
   possible.  We need to do this sometimes for moving stuff
   in and out of the CVS cache.
   */
int copy_file(const char *from,const char *to);
/* A negative result indicates an error as follows */

/* error on from */
#define EFROMBAD (-1)
/* error on to */
#define ETOBAD (-2)
/* no memory */
#define ENOBUFFMEM (-3) 

/* copy_string_to_file is a routine for a string to a file.
   (This is used for attributes.)  We give the string as
   (length,location)
   */
int copy_string_to_file(size_t length,const char *source,const char *to);
/* A negative result indicates an error.  The only possible value
   is ETOBAD. */

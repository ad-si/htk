#include "db.h"

/* Print a message */
void mess(const char *fmt,...);

/* Check the return value ret from a BDB operation called "name", and if it is
   set display an error message */
void run_db(const char *name,int ret);

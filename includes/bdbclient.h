#include "db.h"
#include "limits.h"

#if UINT_MAX == 4294967295U
typedef unsigned int uint32;
#else
#error Need to find a 32-bit unsigned int type.
#endif

/* If any of these functions fail they print a message to stderr and the whole
   program exits. */
DB *db_connect(const char *server);

/* The recno of the allocated record is returned in the third argument. */
void db_store(DB *db,const char *data,uint32 length,uint32 *recno);

/* A pointer to the returned data is retured in datap (this is malloc'd
   and should be free'd when we are finished with it).  The length is
   returned in length. */
void db_retrieve(DB *db,uint32 recno,char **datap,uint32 *length);




#ifndef BDBCLIENT_H
#define BDBCLIENT_H

#include "db.h"
#include "limits.h"

#if UINT_MAX == 4294967295U
typedef unsigned int uint32;
#else
#error Need to find a 32-bit unsigned int type.
#endif

/* If any of these functions fail they print a message to stderr and the whole
   program exits. */

/* Create a new DB item. */
DB *db_connect(const char *database);

/* Store a new item, as part of an existing transaction. 

   The recno of the allocated record is returned in the third argument. 
   */
void db_store(DB *db,const char *data,uint32 length,uint32 *recno);

/* Flush cached information to disk */
void db_flush(DB *db);

/* A pointer to the returned data is retured in datap.  The length is
   returned in length. 

   If no corresponding item can be found, datap is set to NULL and
   *length to 0.

   WARNING.  The space containing the returned data is allocated by
   BDB, and BDB may reuse it next time we retrieve anything from the
   database.  Therefore we should copy the data somewhere else before
   using db_retrieve again. */
void db_retrieve(DB *db,uint32 recno,char **datap,uint32 *length);

#endif


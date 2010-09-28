#ifndef BDBCLIENT_H
#define BDBCLIENT_H

#include "db.h"

/* If any of these functions fail they print a message to stderr and the whole
   program exits. */

/* Create a new database.  "database" should be the directory to
   store it in.  "dbName" should be an identifying name for the
   database.  (You may have one or more database in the same directory.) */
DB * db_connect(const char *database,const char *dbName);

/* Store a new item, as part of an existing transaction. 
   The recno of the allocated record is returned in the third argument. 
   */
void db_store(DB *db,DB_TXN *txn,const char *data,size_t length,
   size_t *recno);

/* Store an item with the given recno, possibly overwriting what
   was there before.
   */ 
void db_store_here(DB *db,DB_TXN *txn,const char *data,size_t length,
      size_t recno);

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
void db_retrieve(DB *db,size_t recno,char **datap,size_t *length);

/* create a cursor, to be used for scanning through all the items
   in the database. */
void db_cursor(DB *db,DBC **cursorp);

/* read an item at the cursor, or sets datap to NULL. */
void db_read_cursor(DBC *cursor,size_t *recno,char **datap,size_t *length);

/* close the cursor. */
void db_close_cursor(DBC *cursor);


/* Begin a new transaction, returning a handle to it */
DB_TXN *db_begin_trans(); 

/* Commit a transaction */
void db_end_trans(DB_TXN *trans);

/* Abort a transaction */
void db_abort_trans(DB_TXN *trans);

#endif


#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "bdbclient.h"
#include "bdbcommon.h"

static void debug (size_t recno,const char *data,size_t length);

static void ensure_db_env ();


static DB_ENV *db_env = NULL;

DB * db_connect(const char *database,const char *dbName) {
   DB *db;
   DB *result;
   int error; 

   ensure_db_env(database);

   run_db("db_create",
      db_create(&db,db_env,0));

#if ( (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR >= 1))) 
   run_db("db_open",
      db->open(db,NULL,dbName,NULL,DB_RECNO,DB_CREATE | DB_AUTO_COMMIT,0664));

#else
   run_db("db_open",
      db->open(db,dbName,NULL,DB_RECNO,DB_CREATE,0664));
   /* From BDB documentation for version 4.0.14  
      "There is no reason to wrap database opens inside of transactions."
   */
#endif


   return db;
   }

void ensure_db_env (const char *database) {
   if (!db_env) {
      run_db("db_env_create",
         db_env_create(&db_env,0));

      db_env->set_errfile(db_env,stderr);

      run_db("db_env->open",
         db_env->open(db_env,database,
            DB_CREATE | DB_RECOVER | DB_INIT_LOCK | DB_INIT_TXN
               | DB_INIT_MPOOL | DB_INIT_LOG ,0)); 
      }
   }

void db_store(DB *db,DB_TXN *txn,const char *data,size_t length,
      size_t *recno) {
   DBT key_dbt,data_dbt;
   db_recno_t db_recno;

   memset(&key_dbt, 0, sizeof(key_dbt));
   memset(&data_dbt, 0, sizeof(data_dbt));

   key_dbt.data = &db_recno;
   key_dbt.size = sizeof(db_recno);
   data_dbt.data=data;
   data_dbt.size=length;
   run_db("DB->put",
      db->put(db,txn,&key_dbt,&data_dbt,DB_APPEND));

   *recno = * ((size_t *) key_dbt.data);
   }

void db_store_here(DB *db,DB_TXN *txn,const char *data,size_t length,
      size_t recno) {
   DBT key_dbt,data_dbt;
   db_recno_t db_recno = recno;

   memset(&key_dbt, 0, sizeof(key_dbt));
   memset(&data_dbt, 0, sizeof(data_dbt));

   key_dbt.data = &db_recno;
   key_dbt.size = sizeof(db_recno);
   data_dbt.data=data;
   data_dbt.size=length;
   run_db("DB->put",
      db->put(db,txn,&key_dbt,&data_dbt,0));
   }

void db_retrieve(DB *db,size_t recno,char **datap,size_t *length) {
   DBT key_dbt,data_dbt;
   db_recno_t db_recno;
   int db_get_out;

   memset(&key_dbt, 0, sizeof(key_dbt));
   memset(&data_dbt, 0, sizeof(data_dbt));

   db_recno = (db_recno_t) recno;
   key_dbt.data = &db_recno;
   key_dbt.size = sizeof(db_recno);
   
   db_get_out = db->get(db,NULL,&key_dbt,&data_dbt,0);
   if (db_get_out == DB_NOTFOUND || db_get_out == DB_KEYEMPTY) {
      *datap = NULL;
      *length = 0;
      }
   else {    
      run_db("DB->get",db_get_out);

      *length = (size_t) data_dbt.size;
      *datap = (char *)data_dbt.data;
      }
   }

void db_cursor(DB *db,DBC **cursorp) {
   run_db("DB->cursor",db->cursor(db,NULL,cursorp,0));
   }

void db_read_cursor(DBC *cursor,size_t *recno,char **datap,size_t *length) {
   DBT key_dbt,data_dbt;

   int c_get_out;

   memset(&key_dbt, 0, sizeof(key_dbt));
   memset(&data_dbt, 0, sizeof(data_dbt));

   c_get_out = cursor->c_get(cursor,&key_dbt,&data_dbt,DB_NEXT);
   if (c_get_out == DB_NOTFOUND) {
      *recno = 0;
      *datap = NULL;
      *length = 0;
      }
   else {    
      run_db("DBC->c_get",c_get_out);

      *recno = (size_t) (* ((db_recno_t *)key_dbt.data));
      *length = (size_t) data_dbt.size;
      *datap = (char *)data_dbt.data;
      }
   }

void db_close_cursor(DBC *cursor) {
   run_db("DBcursor->c_close",cursor->c_close(cursor));
   }

void db_flush(DB *db) {
   db -> sync(db,0);
   }


void debug (size_t recno,const char *data,size_t length) {
   size_t toPrint = (length > 20 ? 20 : length);
   int i;
   fprintf (stderr,"%d ",(int)recno);
   for(i=0;i<toPrint;i++) {
      unsigned char c = data[i];
      fprintf (stderr,"%02x",(int)c);
      }
   fprintf(stderr," ");
   for(i=0;i<toPrint;i++) {
      unsigned char c = data[i];
      fprintf (stderr,"%c",(isprint(c)?c:'.'));
      }
   fprintf (stderr,"\n");
   }

/* Transactions */

DB_TXN *db_begin_trans() {
   DB_TXN *result;
   run_db("DB_ENV->txn_begin",
      db_env->txn_begin(db_env,NULL,&result,0));
   return result;
   }

void db_end_trans(DB_TXN *trans) {
   run_db("DB_TXN->commit",
      trans->commit(trans,0));
   }

void db_abort_trans(DB_TXN *trans) {
   run_db("DB_TXN->abort",
      trans->abort(trans));
   }


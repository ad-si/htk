#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "bdbclient.h"
#include "bdbcommon.h"

void debug (uint32 recno,const char *data,uint32 length);

DB * db_connect(const char *server) {
   DB_ENV *db_env;
   DB *db;
   DB *result;
   int error;
   
   run_db("db_env_create",
      db_env_create(&db_env,DB_CLIENT));
   run_db("db_env->set_rpc_server",
      db_env->set_rpc_server(db_env,NULL,server,20,3600,0));
   run_db("db_env->open",
      db_env->open(db_env,DATABASE,DB_CREATE | DB_INIT_MPOOL,0));
   run_db("db_create",
      db_create(&db,db_env,0));

#if ( (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR >= 1))) 
   run_db("db_open",
      db->open(db,NULL,DATABASE,NULL,DB_RECNO,DB_CREATE,0664));
#else
   run_db("db_open",
      db->open(db,DATABASE,NULL,DB_RECNO,DB_CREATE,0664));
#endif

   return db;
   }


void db_store(DB *db,const char *data,uint32 length,uint32 *recno) {
   DBT key_dbt,data_dbt;
   db_recno_t db_recno;

   memset(&key_dbt, 0, sizeof(key_dbt));
   memset(&data_dbt, 0, sizeof(data_dbt));
   key_dbt.data = &db_recno;
   key_dbt.size = sizeof(db_recno);

   data_dbt.data=data;
   data_dbt.size=length;
   run_db("DB->put",
      db->put(db,NULL,&key_dbt,&data_dbt,DB_APPEND));
   *recno = (uint32) db_recno;
   }

void db_retrieve(DB *db,uint32 recno,char **datap,uint32 *length) {
   DBT key_dbt,data_dbt;
   db_recno_t db_recno;
   char *permanent_data;

   memset(&key_dbt, 0, sizeof(key_dbt));
   memset(&data_dbt, 0, sizeof(data_dbt));

   db_recno = (db_recno_t) recno;
   key_dbt.data = &db_recno;
   key_dbt.size = sizeof(db_recno);
   run_db("DB->get",db->get(db,NULL,&key_dbt,&data_dbt,0));

   *length = (uint32) data_dbt.size;

   permanent_data = malloc(data_dbt.size);
   if (!permanent_data) {
      fprintf(stderr,"OUT OF MEMORY - malloc failed.");
      exit(EXIT_FAILURE);
      }

   memcpy(permanent_data,(char *)data_dbt.data,(size_t) data_dbt.size);
   *datap = (char *) permanent_data;
   }

void debug (uint32 recno,const char *data,uint32 length) {
   uint32 toPrint = (length > 20 ? 20 : length);
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

     


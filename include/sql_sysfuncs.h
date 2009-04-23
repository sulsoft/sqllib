/*
 * SQLLIB RDD Project source code:
 * Header for SQL System Misc Functions
 * 
 * A Free & Open source RDD for Harbour & xHarbour Compilers
 * Copyright 2008 Vailton Renato <vailtom@gmail.com> and
 *                Rossine Maia   <qiinfo@ig.com.br>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */
 
#define ESCAPE_SINGLE_QUOTES_WITHOUT_BACKSLASH     4           // Convert ' as '' ... 
#define ESCAPE_SQUARE BRACKET                      8           // http://sqlserver2000.databases.aspfaq.com/how-do-i-search-for-special-characters-e-g-in-sql-server.html        http://techtamasha.com/escape-single-quotes-and-wild-cards-_-in-ms-sql/20

/*
 * ISO Date format, supported by:
 * 
 *    MySQL       -> http://dev.mysql.com/doc/refman/5.0/en/date-and-time-types.html
 *    PostgreSQL  -> http://www.postgresql.org/docs/7.4/interactive/datatype-datetime.html 
 *    FireBird    -> http://www.firebirdsql.org/doc/contrib/FirebirdDateLiterals.html
 */
#define ESCAPE_FORMAT_DATE_ISO                     0           
#define ESCAPE_FORMAT_DATE_ORACLE                  1           // Oracle Date format as "INSERT INTO YOURTABLE(ico_data) VALUES(to_date('20081214','YYYYMMDD'))";
#define ESCAPE_FORMAT_DATE_MSSQL                   2           // M$ SQL Date format as http://msdn.microsoft.com/en-us/library/aa226054(SQL.80).aspx

#define ESCAPE_ADD_QUOTES                          1           // Force add single quotes on converted value 

#define SQLSYS_INFO_MAX_DRIVER_NAME                9           // e.g. "MYSQL", "PGSQL", "FIREBIRD", etc...
#define SQLSYS_INFO_MAX_BOOL_VALUE                 6           // e.g. "FALSE"
#define SQLSYS_INFO_MAX_BOOL_FLDNAME               8           // e.g. "Boolean"

#define SQL_ESCAPESTR( funcname )   static char * funcname ( char *source, ULONG *size, int Flags)
#define SR_ISVALID_DRIVER( x )         ( x  && x ->ID )
                                          
typedef char *(*PFUNC_ESCAPE_STR)( char *source, ULONG *size, int Flags);

typedef struct _SQLSYS_INFO 
{
   unsigned char ID;                                           /* Unique System ID */
   char          DrvName[SQLSYS_INFO_MAX_DRIVER_NAME];         /* Driver name as string */
   char          FieldDelim;                                   /* Used as field delimiter */   
   char          BoolTrue[SQLSYS_INFO_MAX_BOOL_VALUE];         /* TRUE  or '1' */
   char          BoolFalse[SQLSYS_INFO_MAX_BOOL_VALUE];        /* FALSE or '0' */     
   unsigned char BoolMaxSize;                                  /* Max Boolean value size, ie: 5 (FALSE) / 3 ('0', '1' ) */
   unsigned char DateFormat;                                   /* Date format to export values */   
   char          BoolFieldType[SQLSYS_INFO_MAX_BOOL_FLDNAME];  /* String with correct FieldType */    
   int           EscapeFlags;                                  /* Flags do configure SQLRUN_ESCAPE */
   PFUNC_ESCAPE_STR EscapeString;                             
} SQLSYS_INFO;

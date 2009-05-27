/*
 * $Id: postgres.c,v 1.26 2008/03/26 23:33:20 modalsist Exp $
 *
 * xHarbour Project source code:
 * PostgreSQL RDBMS low level (client api) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
 * www - http://www.xharbour.org
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
 *
 * See doc/license.txt for licensing terms.
 *
 */
#if defined(_MSC_VER)   
   /*
    * Eliminating deprecation warnings. See more info at:
    * http://msdn.microsoft.com/en-us/library/8ef0s5kh(VS.80).aspx
    */
   #define _CRT_SECURE_NO_WARNINGS
   #include <windows.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <hbapi.h>
#include <hbapifs.h>
#include <hbapiitm.h>
#include <hbstack.h>
#include <libpq-fe.h>
#include "sqllibconsts.ch"

#include "sqldebug.h"

#define VARHDRSZ                   4
#define BOOLOID	                16
#define INT8OID			          20
#define INT2OID		             21
#define INT4OID		             23
#define TEXTOID		             25
#define OIDOID			             26
#define FLOAT4OID                700
#define FLOAT8OID                701
#define CASHOID                  790                                                                
#define BPCHAROID		           1042
#define VARCHAROID		        1043
#define DATEOID			        1082
#define TIMEOID		           1083
#define TIMESTAMPOID	           1114
#define TIMESTAMPTZOID	        1184
#define TIMETZOID		           1266
#define BITOID	                 1560
#define VARBITOID	              1562
#define NUMERICOID		        1700

#define INV_WRITE		     0x00020000
#define INV_READ		     0x00040000

#ifndef HB_PGVERSION
#  ifdef PG_DIAG_INTERNAL_POSITION
#     define HB_PGVERSION     0x0800
#  else
#     define HB_PGVERSION     0x0700
#  endif
#endif

#include "dbstruct.ch"
#include "hbapirdd.h"
#include "hbdbferr.h"
#include "hbapilng.h"
#include "hbapierr.h"

#include "sqllibconsts.ch"
#include "sql_sysfuncs.h"

extern SQLSYS_INFO *SQLSYS_GETINFO( int nDriver );                           

/* 
 * Connection handling functions 
 */

HB_FUNC( PQCONNECT )
{
   char     conninfo[128];
   PGconn   *conn;

   if( hb_pcount() == 5 )
      sprintf( conninfo, "dbname = %s host = %s user = %s password = %s port = %i",
               hb_parcx(1), hb_parcx(2), hb_parcx(3), hb_parcx(4), (int) hb_parni(5) );

   conn = PQconnectdb( conninfo );
   hb_retptr( conn );
}

HB_FUNC( PQSETDBLOGIN )
{
    const char *pghost;
    const char *pgport;
    const char *pgoptions;
    const char *pgtty;
    const char *dbName;
    const char *login;
    const char *pwd;
                                         
    pghost    = hb_parcx(1);   
    pgport    = hb_parcx(2);   
    pgoptions = hb_parcx(3);
    pgtty     = hb_parcx(4);    
    dbName    = hb_parcx(5);   
    login     = hb_parcx(6);    
    pwd       = hb_parcx(8);      
                                         
    if (hb_pcount() == 7)
        hb_retptr( ( PGconn * ) PQsetdbLogin( pghost, pgport, pgoptions, pgtty, dbName, login, pwd) );
}   
                    
HB_FUNC(PQCLOSE)
{
    if (hb_parinfo(1))
        PQfinish(( PGconn * ) hb_parptr(1));
}

HB_FUNC(PQRESET)
{
    if (hb_parinfo(1))
        PQreset(( PGconn * ) hb_parptr(1));
}

HB_FUNC(PQPROTOCOLVERSION)
{
    if (hb_parinfo(1))
        hb_retni(PQprotocolVersion(( PGconn * ) hb_parptr(1)));
}

HB_FUNC(PQCLIENTENCODING)
{
    if (hb_parinfo(1))
        hb_retni(PQclientEncoding(( PGconn * ) hb_parptr(1)));
}

HB_FUNC(PQSETCLIENTENCODING)
{
    if (hb_pcount() == 2)
        hb_retni(PQsetClientEncoding(( PGconn * ) hb_parptr(1), hb_parcx(2)));
}
    
HB_FUNC(PQDB)
{   
    if (hb_parinfo(1))
        hb_retc(PQdb( ( PGconn * ) hb_parptr(1) ));
}
        
HB_FUNC(PQUSER)
{
    if (hb_parinfo(1))
       hb_retc(PQuser( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQPASS)
{
    if (hb_parinfo(1))
        hb_retc(PQpass( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQHOST)
{
    if (hb_parinfo(1))
        hb_retc(PQhost( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQPORT)
{
    if (hb_parinfo(1))
        hb_retc(PQport( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQTTY)
{
    if (hb_parinfo(1))
        hb_retc(PQtty( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQOPTIONS)
{
    if (hb_parinfo(1))
        hb_retc(PQoptions( ( PGconn * ) hb_parptr(1) ));
}

/* 
 * Query handling functions 
 */

HB_FUNC(PQCLEAR)
{
    if (hb_parinfo(1))
        PQclear(( PGresult * ) hb_parptr(1));
}

HB_FUNC(PQEXEC)
{
    PGresult   *res = NULL;

    if (hb_pcount() == 2)
    {
//printf( "PQexec( %p, '%s' )\n", ( PGconn * ) hb_parptr(1), hb_parcx(2) );   
        res = PQexec(( PGconn * ) hb_parptr(1), hb_parcx(2));
    }
    hb_retptr( res );        
}

HB_FUNC(PQEXECPARAMS)
{
    PGresult   *res = NULL;
    const char **paramvalues;
    int        i;
    long       n;

    PHB_ITEM   aParam;

    if (hb_pcount() == 3)
    {
        aParam = hb_param(3,HB_IT_ARRAY);

        n = hb_arrayLen(aParam);

        paramvalues = (const char **) hb_xgrab( sizeof( char *) * n );

        for (i=0;i < n;i++)
            paramvalues[i] = hb_arrayGetCPtr( aParam, i + 1 );

        res = PQexecParams(( PGconn * ) hb_parptr(1), hb_parcx(2), n, NULL, paramvalues, NULL, NULL, 1);

        hb_xfree( (void*)paramvalues);
    }
    hb_retptr( res );        
}

HB_FUNC(PQFCOUNT)
{
    PGresult   *res;
    int nFields = 0;

    if (hb_parinfo(1))
    {
        res = ( PGresult * ) hb_parptr(1);

        if (PQresultStatus(res) == PGRES_TUPLES_OK)
                nFields = PQnfields(res);
    }

    hb_retni(nFields);
}

HB_FUNC(PQLASTREC)
{
    PGresult   *res;
    int nRows = 0;

    if (hb_parinfo(1))
    {
        res = ( PGresult * ) hb_parptr(1);

        if (PQresultStatus(res) == PGRES_TUPLES_OK)
            nRows = PQntuples(res);
    }
    hb_retni(nRows);
}

HB_FUNC(PQGETVALUE)
{
    PGresult   *res;
    int         nRow, nCol;

    if (hb_pcount() == 3)
    {
        res = ( PGresult * ) hb_parptr(1);

        if (PQresultStatus(res) == PGRES_TUPLES_OK)
        {
            nRow = hb_parni(2) - 1;
            nCol = hb_parni(3) - 1;

            if (!PQgetisnull(res, nRow, nCol))
                hb_retc( PQgetvalue(res, nRow, nCol) );
        }
    } 
}

HB_FUNC(PQGETLENGTH)
{
    PGresult   *res;
    int         nRow, nCol;
    int         result = 0;

    if (hb_pcount() == 3)
    {
        res = ( PGresult * ) hb_parptr(1);

        if (PQresultStatus(res) == PGRES_TUPLES_OK)
        {
            nRow = hb_parni(2) - 1;
            nCol = hb_parni(3) - 1;

            result = PQgetlength(res, nRow, nCol);
        }
    }
    hb_retni(result);
}

HB_FUNC( PQMETADATA )
{
   PGresult *res;
   if( hb_parinfo( 1 ) )
   {
      res = ( PGresult * ) hb_parptr( 1 );
      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nFields = PQnfields( res ), i;
         PHB_ITEM pResult = hb_itemArrayNew( nFields ), pField;

         for( i = 0; i < nFields; i++ )
         {
            char  buf[256];
            Oid   type_oid = PQftype( res, i );
            int   typemod = PQfmod( res, i );
            int   length = 0;
            int   decimal = 0;

            switch( type_oid )
            {
               case BITOID:
                  if( typemod >= 0 )
                     length = ( int ) typemod;
                  strcpy( buf, "bit" );
                  break;

               case BOOLOID:
                  length = 1;
                  strcpy( buf, "boolean" );
                  break;

               case BPCHAROID:
                  if( typemod >= 0 )
                     length = ( int ) ( typemod - VARHDRSZ );
                  strcpy( buf, "character" );
                  break;

               case FLOAT4OID:
                  strcpy( buf, "real" );
                  break;

               case FLOAT8OID:
                  strcpy( buf, "double precision" );
                  break;

               case INT2OID:
                  strcpy( buf, "smallint" );
                  break;

               case INT4OID:
                  strcpy( buf, "integer" );
                  break;

               case OIDOID:
                  strcpy( buf, "bigint" );
                  break;

               case INT8OID:
                  strcpy( buf, "bigint" );
                  break;

               case NUMERICOID:
                  length = ( ( typemod - VARHDRSZ ) >> 16 ) & 0xffff;
                  decimal = ( typemod - VARHDRSZ ) & 0xffff;
                  strcpy( buf, "numeric" );
                  break;

               case DATEOID:
                  strcpy( buf, "date" );
                  break;

               case TIMEOID:
               case TIMETZOID:
                  strcpy( buf, "timezone" );
                  break;

               case TIMESTAMPOID:
               case TIMESTAMPTZOID:
                  strcpy( buf, "timestamp" );
                  break;

               case VARBITOID:
                  if( typemod >= 0 )
                     length = (int) typemod;
                  strcpy( buf, "bit varying" );
                  break;

               case VARCHAROID:
                  if( typemod >= 0 )
                     length = ( int ) ( typemod - VARHDRSZ );
                  strcpy( buf, "character varying" );
                  break;

               case TEXTOID:
                  strcpy(buf, "text");
                  break;

               case CASHOID:
                  strcpy( buf, "money" );
                  break;

               default:
                 strcpy( buf, "not supported" );
                 break;
            }

            pField = hb_arrayGetItemPtr( pResult, i + 1 );
            hb_arrayNew ( pField, 6 );
            hb_itemPutC ( hb_arrayGetItemPtr( pField, 1 ), PQfname( res, i ) );
            hb_itemPutC ( hb_arrayGetItemPtr( pField, 2 ), buf );
            hb_itemPutNI( hb_arrayGetItemPtr( pField, 3 ), length );
            hb_itemPutNI( hb_arrayGetItemPtr( pField, 4 ), decimal );
            hb_itemPutNL( hb_arrayGetItemPtr( pField, 5 ), PQftable( res, i ) );
            hb_itemPutNI( hb_arrayGetItemPtr( pField, 6 ), PQftablecol( res, i ) );
         }
         hb_itemRelease( hb_itemReturnForward( pResult ) );
      }
   }
}

/*
 * PQGetValueEx() returns a single field (attribute) value of one tuple of a 
 * PGresult, into correct value type.
 * 23/12/2008 - 10:13:17
 */
HB_FUNC( PQGETVALUEEX )
{
   PHB_ITEM pItem;
   PHB_ITEM aFieldInfo;
   BOOL bEmptyRecord = FALSE;
   
   PGresult *res;
   Oid type_oid;
   int typemod;
   int uiLen;
   int uiDec;
   int nWA, nRow, nField;

   if (!hb_parinfo( 1 ))
      return;
      
   res = ( PGresult * ) hb_parptr( 2 );
   
   if ( PQresultStatus( res ) != PGRES_TUPLES_OK )
      return;
   
   nWA    = hb_parni(1);
   nRow   = hb_parni(3) -1;
   nField = hb_parni(4) -1;
   aFieldInfo = hb_param(5, HB_IT_ARRAY );
                                                 
   uiLen  = hb_arrayGetNI( aFieldInfo, DBS_LEN );
   uiDec  = hb_arrayGetNI( aFieldInfo, DBS_DEC );      
   pItem  = hb_itemNew( NULL );   
   type_oid = PQftype( res, nField );
   typemod  = PQfmod ( res, nField );

   /* todo: test correct values from aWAData[] ...
   if ( (pArea->fEof) || (!nRow) )
      bEmptyRecord = TRUE;
   /***/
   if ( PQgetisnull( res, nRow, nField ))
      bEmptyRecord = TRUE;

   /*
    * Read: http://pgdocptbr.sourceforge.net/pg80/datatype.html
    */
   switch( hb_arrayGetNI( aFieldInfo, DBS_FIELD_TYPE ) )       // See TPQserver:TableStruct() 
   {
      case HB_FT_STRING:
         {
            ULONG ulSize;
            ULONG lLen = (ULONG) uiLen;

            /* PAD the string buffer with " " to fill correct LEN */
            if ( bEmptyRecord )
            {
               char * szResult = ( char * ) hb_xgrab( lLen + 1 );
               szResult[lLen] = '\0';

               hb_xmemset( szResult, ' ', lLen );
               hb_itemPutCPtr( pItem, ( char * ) szResult, lLen );

            } else {
               char * szResult = ( char * ) hb_xgrab( lLen + 1 );
               szResult[lLen] = '\0';

               ulSize = (ULONG ) PQgetlength( res, nRow, nField );

               hb_xmemset( szResult, ' ', lLen );
               hb_xmemcpy( szResult, PQgetvalue( res, nRow, nField ), ( ulSize > lLen) ? lLen : ulSize );
               hb_itemPutCPtr( pItem, ( char * ) szResult, lLen );
            }
            HB_TRACE(HB_TR_DEBUG, ("PQGetValueEx (char *) => %s", hb_itemGetCPtr( pItem )));
            break;
         }

      case HB_FT_MEMO:

         if ( bEmptyRecord )
            hb_itemPutC( pItem, "" );
         else
            hb_itemPutCL( pItem, PQgetvalue( res, nRow, nField ),
                        (ULONG ) PQgetlength( res, nRow, nField ) );

         hb_itemSetCMemo( pItem );
         HB_TRACE(HB_TR_DEBUG, ("PQGetValueEx (MEMO*) => %s",hb_itemGetCPtr( pItem )));
         break;

      case HB_FT_LOGICAL:
         {
            BOOL bVal;

            /* 02-10-2004 = Correcao para campos NULL: setar valor padrao */
            if ( bEmptyRecord )
            {
               bVal = FALSE;
            } else {
               char *temp = PQgetvalue( res, nRow, nField );
               bVal = (( strcmp( temp, "t") == 00 ) || ( strcmp( temp, "T") == 00 ));
            }

            hb_itemPutL( pItem,  bVal );
            HB_TRACE(HB_TR_DEBUG, ("PQGetValueEx (BOOL*) => %s", (bVal) ? ".T." : ".F." ));
            break;
         }

      case HB_FT_DATE:
         {
            if ( bEmptyRecord )
               hb_itemPutDS( pItem, NULL );
            else
            {
               char * szTemp = PQgetvalue( res, nRow, nField );
               char szDate[9];
            
               memcpy( szDate  , szTemp  , 4 );
               memcpy( szDate+4, szTemp+5, 2 );
               memcpy( szDate+6, szTemp+8, 2 );
               szDate[8] = '\0';               
               HB_TRACE(HB_TR_DEBUG, ("PQGetValueEx (DATE*) => %s", szDate ));
               hb_itemPutDS( pItem, szDate );
            }
         }
         break;
      
      case HB_FT_INTEGER:
      case HB_FT_FLOAT:
      case HB_FT_DOUBLE:
      case HB_FT_LONG:
         {
            char szBuffer[ 256 ];
            
            if ( bEmptyRecord )
            {
               hb_itemPutNDLen( pItem, 00,
                                   uiLen - ( uiDec + 1 ),
                                   uiDec );
            } else {
               size_t ulSize = (size_t) PQgetlength( res, nRow, nField );

               memset( szBuffer, 0, 256 );
               memcpy( szBuffer, PQgetvalue( res, nRow, nField ), ulSize );

               HB_TRACE(HB_TR_DEBUG, ("PQGetValueEx (LONG*) from buffer=> %s", szBuffer ));

               if( uiDec )
                  hb_itemPutNDLen( pItem, atof( szBuffer ), uiLen - ( uiDec + 1 ), uiDec );
               else
                  if( uiLen > 9 )
                     hb_itemPutNDLen( pItem, atof( szBuffer ), uiLen, uiDec );
                  else
                     hb_itemPutNLLen( pItem, atol( szBuffer ), uiLen );
            }
         }
         ///HB_TRACE(HB_TR_DEBUG, ("PQGetValueEx (LONG*) => %f", hb_itemGetND(pItem) ));
         break;
      default:
         //HB_TRACE(HB_TR_DEBUG, ("PQGetValueEx (UNKNOW TYPE) => %d", pField->uiType ));
         break;
   }
   hb_itemReturnRelease( pItem );
}

HB_FUNC( PQRESULT2ARRAY )
{
   PGresult *res;
   if( hb_parinfo( 1 ) )
   {
      res = ( PGresult * ) hb_parptr( 1 );
      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nRows = PQntuples(res), nRow;
         int nCols = PQnfields( res ), nCol;
         PHB_ITEM pResult = hb_itemArrayNew( nRows ), pRow;

         for( nRow = 0; nRow < nRows ; nRow++ )
         {  
            pRow = hb_arrayGetItemPtr( pResult, nRow + 1 );
            hb_arrayNew ( pRow, nCols );
            for( nCol = 0; nCol < nCols ; nCol++ )
            {
               hb_arraySetC( pRow, nCol + 1, PQgetvalue(res, nRow, nCol) );
            }
         }
         hb_itemRelease( hb_itemReturnForward( pResult ) );
      }
   }
}

HB_FUNC(PQTRANSACTIONSTATUS)
{
    if (hb_parinfo(1))
        hb_retni(PQtransactionStatus(( PGconn * ) hb_parptr(1) ));
}

HB_FUNC(PQERRORMESSAGE)
{
    if (hb_parinfo(1))
        hb_retc(PQerrorMessage(( PGconn * ) hb_parptr(1) ));
}

HB_FUNC(PQSTATUS)
{
    if (hb_parinfo(1))
        hb_retni(PQstatus(( PGconn * ) hb_parptr(1) ));
}

HB_FUNC(PQRESULTERRORMESSAGE)
{
    if (hb_parinfo(1))
        hb_retc(PQresultErrorMessage(( PGresult * ) hb_parptr(1)));
}

HB_FUNC(PQRESULTSTATUS)
{
    if (hb_parinfo(1))
        hb_retni(PQresultStatus(( PGresult * ) hb_parptr(1) ));
}


HB_FUNC(PQCMDSTATUS)
{
    if (hb_parinfo(1))
        hb_retc(PQcmdStatus( (PGresult *) hb_parptr(1) ));
}


HB_FUNC(PQCMDTUPLES)
{
    if (hb_parinfo(1))
        hb_retc(PQcmdTuples( (PGresult *) hb_parptr(1) ));
}

HB_FUNC(PQESCAPESTRING)
{
    char *source, *dest;
    size_t size;
        
    /* 
     * strlen() optimize and argument type check. Note que PQescapeStringInternal() 
     * does not support CHR(0) inside original buffer. Vailton - 14/12/2008 - 12:11:40     
     */
    if (!ISCHAR(1))
    {
         hb_retc( "" );
         return;
    }
    source = hb_parc(1);
    size   = (size_t) hb_parclen(1);    
    
    dest = (char *) hb_xgrab( size * 2 + 1);
    size = PQescapeString(dest, source, size);

    hb_retclen_buffer( dest, size );   
}


HB_FUNC(PQESCAPEBYTEA) /* deprecated */
{
    unsigned const char *from;
    unsigned char *to;
    size_t from_length;
    size_t to_length;
        
    from = ( BYTE * ) hb_parc(1);
    from_length = hb_parclen(1);
    to_length = from_length * 5 + 1;
    
    to = PQescapeBytea(from, from_length, &to_length);
    hb_retc( ( char * ) to );
    PQfreemem(to);
}


HB_FUNC(PQUNESCAPEBYTEA)
{
    unsigned char *from;
    size_t to_length;        
    
    from = PQunescapeBytea(( BYTE * ) hb_parcx(1), &to_length);
    hb_retclen( ( char * ) from, to_length);
    PQfreemem(from);
}


HB_FUNC(PQOIDVALUE)
{
    if (hb_parinfo(1))
        hb_retnl( ( Oid ) PQoidValue(( PGresult * ) hb_parptr(1) ));
}

HB_FUNC(PQOIDSTATUS)
{
    if (hb_parinfo(1))
        hb_retc( PQoidStatus(( PGresult * ) hb_parptr(1) ));
}

HB_FUNC(PQBINARYTUPLES)
{
    if (hb_parinfo(1))
        hb_retl( PQbinaryTuples(( PGresult * ) hb_parptr(1) ));
}

HB_FUNC(PQFTABLE)
{
    if (hb_pcount() == 2)
        hb_retnl( ( Oid ) PQftable(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQFTYPE)
{
    if (hb_pcount() == 2)
        hb_retnl( ( Oid ) PQftype(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQFNAME)
{
    if (hb_pcount() == 2)
        hb_retc( PQfname(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQFMOD)
{
    if (hb_pcount() == 2)
        hb_retni( PQfmod(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQFSIZE)
{
    if (hb_pcount() == 2)
        hb_retni( PQfsize(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQGETISNULL)
{
    if (hb_pcount() == 3)
        hb_retl( PQgetisnull(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 , hb_parni(3) - 1));
}

HB_FUNC(PQFNUMBER)
{
    if (hb_pcount() == 2)
        hb_retni( PQfnumber(( PGresult * ) hb_parptr(1), hb_parcx(2) ) + 1);
}

HB_FUNC(PQNTUPLES)
{
    if (hb_parinfo(1))
        hb_retnl( PQntuples(( PGresult * ) hb_parptr(1) ));
}

HB_FUNC(PQNFIELDS)
{
    if (hb_parinfo(1))
        hb_retnl( PQnfields(( PGresult * ) hb_parptr(1) ));
}

/* 
 * Asynchronous functions 
 */

HB_FUNC(PQSENDQUERY)
{
    int res = 0;        

    if (hb_pcount() == 2)
        res = PQsendQuery(( PGconn * ) hb_parptr(1), hb_parcx(2));

    hb_retl( res );        
}

HB_FUNC(PQGETRESULT)
{
    PGresult   *res = NULL;

    if (hb_parinfo(1))
        res = PQgetResult(( PGconn * ) hb_parptr(1));

    /* when null, no more result to catch */
    if (res)
        hb_retptr( res );        
}

HB_FUNC(PQCONSUMEINPUT)
{
    int res = 0;        

    if (hb_parinfo(1))
        res = PQconsumeInput(( PGconn * ) hb_parptr(1));

    hb_retl( res );        
}

HB_FUNC(PQISBUSY)
{
    int res = 0;        

    if (hb_parinfo(1))
        res = PQisBusy(( PGconn * ) hb_parptr(1));

    hb_retl( res );        
}

HB_FUNC(PQREQUESTCANCEL) /* deprecated */
{
    int res = 0;        

    if (hb_parinfo(1))
        res = PQrequestCancel(( PGconn * ) hb_parptr(1));

    hb_retl( res );        
}


HB_FUNC(PQFLUSH)
{
    if (hb_parinfo(1))
        hb_retni( PQflush(( PGconn * ) hb_parptr(1)) );
}


HB_FUNC(PQSETNONBLOCKING)
{
    if (hb_pcount() == 2)
        hb_retl( PQsetnonblocking( ( PGconn * ) hb_parptr(1), hb_parl(2) ) );
}

HB_FUNC(PQISNONBLOCKING)
{
    if (hb_parinfo(1))
        hb_retl( PQisnonblocking( ( PGconn * ) hb_parptr(1) ) );
}

HB_FUNC(PQSETERRORVERBOSITY)
{
    /* PQERRORS_TERSE   0
       PQERRORS_DEFAULT 1
       PQERRORS_VERBOSE 2
    */
    
    if (hb_pcount() == 2)
        hb_retni( ( PGVerbosity ) PQsetErrorVerbosity( ( PGconn * ) hb_parptr(1), ( PGVerbosity ) hb_parni(2) ) );
}


/* 
 * Large Object functions 
 */


HB_FUNC(LO_IMPORT)
{
    int ret = 0; 
    
    if (hb_pcount() == 2)
         ret = lo_import( ( PGconn * ) hb_parptr(1), hb_parcx(2) );

    hb_retni(ret);         
}

HB_FUNC(LO_EXPORT)
{
    int ret = 0; 
    
    if (hb_pcount() == 3)
    {
        ret = lo_export( ( PGconn * ) hb_parptr(1), ( Oid ) hb_parnl(2), hb_parcx(3) );

        if (ret != 1)
            ret = 0;
    }            

    hb_retl(ret);        
}

HB_FUNC(LO_UNLINK)
{   
    int ret = 0; 
           
    if (hb_pcount() == 2)
    {
        ret = lo_unlink( ( PGconn * ) hb_parptr(1), ( Oid ) hb_parnl(2) );        
        
        if (ret != 1)
            ret = 0;
    }            

    hb_retl(ret);        
}

/*
 * Support for SR_EscapeString() function.
 * TODO: Acrescentar um parametro indicando se á é para retornar a string com
 * ***** delimitadores. 
 * 14/12/2008 - 19:20:21
 */
SQL_ESCAPESTR( PGSQL_ESCAPE_STRING )
{
   char *dest;
   HB_SYMBOL_UNUSED( Flags );
   HB_TRACE(HB_TR_DEBUG,(" PGSQL_ESCAPE_STRING( '%s', %lu )", source, *size ));

   dest = (char *) hb_xgrab( (*size * 2) + 1);
  *size = PQescapeString(dest, source, *size);   
   return dest;   
}
 
/*
 * Register a custom info for PostgreSQL
 * 14/12/2008 - 18:20:37
 */
HB_FUNC( PGSQL_REGISTER_INFO )
{
   SQLSYS_INFO *pPostgreSQL = SQLSYS_GETINFO( ID_POSTGRESQL );

   pPostgreSQL -> ID           = ID_POSTGRESQL;  
   pPostgreSQL -> FieldDelim   = '"';  
   pPostgreSQL -> BoolMaxSize  = 7;
   pPostgreSQL -> DateFormat   = ESCAPE_FORMAT_DATE_ISO;                        // http://www.postgresql.org/docs/current/interactive/datatype-datetime.html#DATATYPE-DATETIME-DATE-TABLE                                   
   pPostgreSQL -> EscapeFlags  = ESCAPE_SINGLE_QUOTES_WITHOUT_BACKSLASH;
   pPostgreSQL -> EscapeString = &PGSQL_ESCAPE_STRING;

   strcpy( pPostgreSQL -> DrvName       , "PGSQL" );
   strcpy( pPostgreSQL -> BoolTrue      , "TRUE" );  
   strcpy( pPostgreSQL -> BoolFalse     , "FALSE" );  
   strcpy( pPostgreSQL -> BoolFieldType , "boolean" );   
   return;
/*
   pStruct -> ID           = ID_MYSQL;  
   pStruct -> FieldDelim   = '`';  
   pStruct -> BoolMaxSize  = 3;                  
   pStruct -> EscapeFlags  = 0;

   strcpy( pStruct -> DrvName       , "MYSQL" );
   strcpy( pStruct -> BoolTrue      , "1" );    
   strcpy( pStruct -> BoolFalse     , "0" );  
   strcpy( pStruct -> BoolFieldType , "tinyint" );

/**/
}

#if HB_PGVERSION >= 0x0800

HB_FUNC(PQSERVERVERSION)
{
    if (hb_parinfo(1))
        hb_retni(PQserverVersion(( PGconn * ) hb_parptr(1)));
}

HB_FUNC(PQGETCANCEL)
{
    if (hb_parinfo(1))
        hb_retptr( ( PGcancel * ) PQgetCancel( ( PGconn * ) hb_parptr(1) ) );
}        

HB_FUNC(PQCANCEL)
{
    char errbuf[256];
    int ret = 0;

    if (hb_parinfo(1))
        if (PQcancel( ( PGcancel * ) hb_parptr(1), errbuf, 255) == 1)
        {
            ret = 1;                
            hb_storc( errbuf, 2 );
        }            
        
    hb_retl(ret);            
}        

HB_FUNC(PQFREECANCEL)
{
    if (hb_parinfo(1))
        PQfreeCancel( ( PGcancel * ) hb_parptr(1) ) ;
}        

/*
HB_FUNC(PQESCAPEBYTEACONN) 
{
    unsigned const char *from;
    unsigned char *to;
    size_t from_length;
    size_t to_length;
        
    from = ( BYTE * ) hb_parc(2);
    from_length = hb_parclen(2);
    to_length = from_length * 5 + 1;
    
    to = PQescapeByteaConn(( PGconn * ) hb_parptr(1), from, from_length, &to_length);
    hb_retc( ( char * ) to );
    PQfreemem(to);
}
*/
#endif

/*
 * Send a query to DB, get result-set and error flag
 */
PGresult *pgsql_query_log( PGconn *conn, char *sql, BOOL * err )
{
   PGresult *result;

//   HB_TRACE( DEBUG,( "pgsql_query_log( %p, '%s', %d )", conn, sql, * err ) );

   result = PQexec( conn, sql );

   switch( PQresultStatus(result) )
   {  
      case PGRES_COMMAND_OK:
      case PGRES_TUPLES_OK:
         * err = FALSE;
         break;
         
      default:
         * err = TRUE;
   }

#ifdef SQL_DEBUG
//   HB_TRACE( HB_TR_DEBUG,( "*****************************************************" ));
   
   if ( * err )
   {
      HB_TRACE( HB_TR_DEBUG,( "ERROR: %p => PQexec(%p,'%s')\n %s", result, conn, sql, PQerrorMessage(conn) ));
   } else {
      HB_TRACE( HB_TR_DEBUG,(    "OK: %p => PQexec(%p,'%s')", result, conn, sql ));
   }

   HB_TRACE( HB_TR_DEBUG,( "*****************************************************" ));
#endif
   return result;
}

/*
 * Executa um comando SQL no PostgreSQL e retorna o RESULT SET do comando passado.
 * Utiliza 5 parametros para isto:
 *
 * 1° - Handle da conexão do PostgreSQL
 * 2° - Comando SQL
 * 3° - Variavel passada por referencia (@) que conterá o valor de .T. em caso
 *      de ocorrer erro no comando
 * 4° - Valor .T./.F. indicando se deve apresentar ao USUARIO um erro em tempo
 *      de execução, sobre o comando executado.
 * 5º - Valor .T. indicando que o valor de retorno pode ser limpo com PQClear()
 *      e nesta caso esta funcao sempre retornará NIL. 
 */
HB_FUNC( PGSQL_QUERY_LOG )
{
   PGresult *Result;
   PGconn   *pConn;
   char     *cSQL;
   BOOL bError, bRaise, bClear;

   pConn  = (PGconn *)hb_parptr(1);
   cSQL   = (char *)  hb_parc(2);
   bRaise = hb_parl(4);
   bClear = hb_parl(5);

   HB_TRACE_ARGS( "PGSQL_QUERY_LOG" );
   Result = pgsql_query_log( pConn, cSQL, &bError );

   /* Se ele quiser o 3° parametro de erro */ 
   if ( ( hb_pcount() > 2 ) && ( ISBYREF( 3 ) ) )
      hb_storl( bError, 3 );

   if (bClear)
   {
      PQclear( Result );
      hb_ret();
   } else 
      hb_retptr( (void *) Result );
   
   if ((bError && bRaise))
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, PQerrorMessage( pConn ), "PGSQL_QUERY_LOG", 5, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ), hb_paramError( 5 ) );
   }
}

/*
 * Verifica se as tabelas de sistema existem
 * i.e. SL$INDEXES - 21/01/2009 - 22:48:06
 */
HB_FUNC( PGSQL_CHECKSYSTEMTABLES )
{
  static BOOL bFirstOpen = TRUE;
  
  PGconn   *postgres = (PGconn *) hb_parptr(1);
  PGresult *result;

  BOOL bError;
  char *SchemaName = hb_parc(2);
  char SQL[900];
  
  HB_TRACE(HB_TR_DEBUG, ("PGSQL_CheckSystemTables(%p)", postgres ));
  
  if (!bFirstOpen)
  {
   HB_TRACE(HB_TR_DEBUG, (" >> bFirstOpen == FALSE" ));
   return;// SUCCESS;
  }

  /*
   * 1° Teste -> A tabela existe?
   */
  SQL[0] = '\0';
  sprintf( SQL, "select tablename from pg_tables where schemaname = '%s' and tablename = '%s' order by tablename limit 1", SchemaName, SL_INDEX );

  result = pgsql_query_log( postgres,SQL,&bError);

  if (bError)
  {
     PQclear(result);
     return;//  FAILURE;
  }
  
  bError = PQntuples(result)<1;
  PQclear(result);

  /*
   * 2° Teste as colunas conferem
   */
  if (!bError)
  {
     SQL[0] = '\0';
     sprintf( SQL, "SELECT \
                      \"indexstamp\" ,\
                      \"indextable\" ,\
                      \"indexfile\" ,\
                      \"indextag\" ,\
                      \"indexunique\" ,\
                      \"indexdescending\" ,\
                      \"indexfields\" ,\
                      \"indexfor\" ,\
                      \"indexkey\" ,\
                      \"indexkeysizes\" ,\
                      \"indexkeytypes\" ,\
                      \"indexcustomcol\" \
                     FROM  \"%s\".\"%s\"  LIMIT 1"
             , SchemaName, SL_INDEX);

     result = pgsql_query_log( postgres,SQL,&bError);

     if (bError)
       HB_TRACE(HB_TR_DEBUG, (" *** ERROR: FAILED TO CHECK COLUMSN INTO SQL$INDEXES ***" ));
       
     PQclear( result );
  }

  if (bError)
  {
      char seq[100];
      HB_TRACE(HB_TR_DEBUG, (" >> CRIANDO %s!!",SL_INDEX ));

      SQL[0] = '\0';
      sprintf( SQL,  "DROP TABLE \"%s\".\"%s\"", SchemaName, SL_INDEX );

      PQclear( PQexec(postgres,SQL) );

      SQL[0] = '\0';
      seq[0]='\0';
      sprintf( seq, "sqlindexes_%s%s", SL_COL_RECNO, "_seq");
      sprintf( SQL,  "CREATE SEQUENCE \"%s\".\"%s\" INCREMENT 1  MINVALUE 1 START 1", SchemaName, seq );

      PQclear( PQexec(postgres,SQL) );

      SQL[0] = '\0';
      sprintf( SQL,  "ALTER SEQUENCE \"%s\".\"%s\" RESTART WITH 1", SchemaName, seq );
      PQclear( PQexec(postgres,SQL) );
      
      SQL[0] = '\0';
      sprintf( SQL,  "CREATE TABLE \"%s\".\"%s\" (\
                     \"indexstamp\" date default NULL,\
                     \"indextable\" varchar(128) default NULL,\
                     \"indexfile\" varchar(254) NOT NULL default '',\
                     \"indextag\" varchar(128) default NULL,\
                     \"indexunique\" char(1) NOT NULL default 'F',\
                     \"indexdescending\" char(1) NOT NULL default 'F', \
                     \"indexfields\" varchar(255) default NULL,\
                     \"indexfor\" varchar(255) default NULL,\
                     \"indexkey\" varchar(255) default NULL,\
                     \"indexkeysizes\" varchar(40) default NULL,\
                     \"indexkeytypes\" varchar(40) default NULL,\
                     \"indexcustomcol\" smallint NOT NULL default 0,\
                     %s numeric(15) UNIQUE default nextval('%s'::regclass) NOT NULL \
                     )", SchemaName, SL_INDEX, SL_COL_RECNO, seq );

      PQclear( pgsql_query_log(postgres,SQL,&bError) );

      if (bError)
      {
         HB_TRACE(HB_TR_DEBUG, (" !PAU! NO SQL: %s",SQL ));
         return;//  FAILURE;
      }

      SQL[0] = '\0';
      sprintf( SQL,  "CREATE INDEX \"IndexTag\" ON \"%s\".\"%s\" USING btree (\"indextag\", \"indextable\")"
                     , SchemaName, SL_INDEX );

      result = pgsql_query_log(postgres,SQL,&bError);
      PQclear( result );

      if (bError)
      {
         HB_TRACE(HB_TR_DEBUG, (" !PAU! NO SQL: %s",SQL ));
         return;//  FAILURE;
      }
      bFirstOpen = FALSE;

      PQclear( PQexec(postgres,"COMMIT") );
  }

  HB_TRACE(HB_TR_DEBUG, (" !OK! Retornando...." ));
  bFirstOpen = FALSE;
  return;//  SUCCESS;
}

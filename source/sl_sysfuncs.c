/*
 * SQLLIB RDD Project source code:
 * SQL System Misc Functions
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
#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#include "sqllibconsts.ch"
#include "sql_sysfuncs.h"
#include "sqldebug.h"

SQLSYS_INFO pMySQLInfo, pPostgreSQL;

/*
 * Returns the internal driver ID of argument passed as string 
 */
HB_FUNC( SQLSYS_ID )
{
   char *cRddName = hb_parc(1);
   
   if ( strcmp( cRddName, "MYSQL" ) == 00 )
      hb_retni( ID_MYSQL );

   else if ( strcmp( cRddName, "PGSQL" ) == 00 )
      hb_retni( ID_POSTGRESQL );

   else 
      hb_retni( ID_NONE );
}
   
/*
 * Returns a string that identifies the drive linked to the ID passed as argument
 */   
HB_FUNC( SQLSYS_IDSTR )
{
   switch( hb_parni(1) )
   {
      case ID_MYSQL:
         hb_retc( "MYSQL" );
         break;

      case ID_POSTGRESQL:
         hb_retc( "PGSQL" );
         break;

      default: 
         hb_retc( "" );
         break;
   }
}

/*
 * Return the correct DBMS separator
 * 15/09/2008 - 21:25:46
 */
HB_FUNC( SQLSYS_SEP )
{
   switch( hb_parni( 1 ) )
   {
      case ID_MYSQL:
         hb_retc( "`" );
         break;

      case ID_POSTGRESQL:
         hb_retc( "\"" );
         break;

      default: 
         hb_retc( "" );
         break;
   }
}

/*
 * Call specific driver function to load any configuration needed...
 * 14/12/2008 - 20:05:26
 */
static
void SQLSYS_LOADINFO( const char *DriverName, SQLSYS_INFO *pStruct )
{
   PHB_DYNS pDynSym = hb_dynsymFind( DriverName );

   if ( pDynSym && hb_dynsymIsFunction( pDynSym ) )
   {
      HB_TRACE(HB_TR_DEBUG,(" SQLSYS_LOADINFO( '%s', %p ) ... (LOADing)", DriverName, pStruct ));
      hb_vmPushDynSym( pDynSym );
      hb_vmPushNil();
      hb_vmPushPointer( ( void * ) pStruct );
      hb_vmDo( 1 );
   } else {
      HB_TRACE(HB_TR_DEBUG,(" SQLSYS_LOADINFO( '%s', %p ) ... ERROR: DRIVER NOT FOUND!!! ###", DriverName, pStruct ));      
      memset( pStruct, '\0', sizeof( SQLSYS_INFO ) );  
   }
}

/*
 * Return pointer with
 * 14/12/2008 - 17:27:51
 */
SQLSYS_INFO *SQLSYS_GETINFO( int nDriver )                           
{
  static BOOL bInitialized = FALSE;
  
  if (!bInitialized)
  {
      bInitialized = TRUE;
      SQLSYS_LOADINFO( "MYSQL", &pMySQLInfo );
      SQLSYS_LOADINFO( "PGSQL", &pPostgreSQL);
  }
   
  switch( nDriver )
   {
      case ID_NONE:                 /* Get from current connection */
         return &pPostgreSQL;
      
      case ID_MYSQL:
         return &pMySQLInfo;

      case ID_POSTGRESQL:
         return &pPostgreSQL;
         
   }  
   return (SQLSYS_INFO *) NULL;
}

/*
 * Force the loading of drivers available.
 * 15/12/2008 - 13:34:45
 */
HB_FUNC( SQLSYS_GETINFO )
{
   SQLSYS_GETINFO(ID_UNKNOW); 
}

/*
 * PGSQL_ESCAPE_STRING escapes a string for use within an SQL command. This is 
 * useful when inserting  data  values  as  literal constants  in SQL commands. 
 * Certain characters (such  as  quotes  and  backslashes) must  be escaped to  
 * prevent them from being interpreted specially by the SQL parser.
 * 
 * SR_ESCAPESTRING performs this operation.
 * 14/12/2008 - 19:45:39
 */
HB_FUNC( SR_ESCAPESTRING )
{
   SQLSYS_INFO *pSysInfo;
   char *szString;
   ULONG ulLength;
      
   // Check correct params
   if (!ISCHAR(1) || !ISNUM(2))
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "SR_ESCAPESTRING", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
      hb_retc( "" );
      return;
   }

   // Get correct struct for manage this request   
   pSysInfo = SQLSYS_GETINFO( hb_parni(2) );
   
   if (pSysInfo && pSysInfo->EscapeString)
   {
      ulLength = hb_parclen(1);
      szString = (*pSysInfo->EscapeString) ( hb_parc(1), &ulLength, 0 );
         
      hb_retclen_buffer( szString, ulLength );
   } else 
      hb_retc("");      
}

/*
 * Formata a data para o padrao ISO 8601 (j  inclui as aspas para envio ao banco
 * 15/12/2008 - 00:02:11
 */
char * SQLSYS_DATETOISO( char * szDate, int iYear, int iMonth, int iDay, BOOL bQuoted )
{
   int i = ( bQuoted ? 1 : 0 );
   
   HB_TRACE(HB_TR_DEBUG, ("SL_GETDATEISO(%p, %d, %d, %d)", szDate, iYear, iMonth, iDay));

   if( iYear >= 0 && iMonth > 0 && iDay > 0 )
   {
      szDate[ i++ ] = ( char ) ( ( ( iYear / 1000 ) % 10 ) + '0' );
      szDate[ i++ ] = ( char ) ( ( ( iYear / 100 ) % 10 ) + '0' );
      szDate[ i++ ] = ( char ) ( ( ( iYear / 10 ) % 10 ) + '0' );
      szDate[ i++ ] = ( char ) ( ( iYear % 10 ) + '0' );
      
      i++;
      szDate[ i++ ] = ( char ) ( ( iMonth / 10 ) + '0' );
      szDate[ i++ ] = ( char ) ( ( iMonth % 10 ) + '0' );

      i++;
      szDate[ i++ ] = ( char ) ( ( iDay / 10 ) + '0' );
      szDate[ i   ] = ( char ) ( ( iDay % 10 ) + '0' );
   } else {
      memset( szDate, '0', 11 );
   }
      
   if (bQuoted)
   {
      szDate[  0 ] = '\'';
      szDate[  5 ] = '-';
      szDate[  8 ] = '-';
      szDate[ 11 ] = '\'';
      szDate[ 12 ] = '\0';
   } else {
      szDate[  4 ] = '-';
      szDate[  7 ] = '-';
      szDate[ 11 ] = '\0';
   }
   
   return szDate;
}

/*
 * SQLQuotedString( PHB_ITEM pItem, int nSystemID, BOOL lNullIfEmpty, ULONG *Length ) --> char * 
 * EX:
 *       ? SQLPARAMS( "insert into clientes (codigo, nome, data) values ($1, $2, $3)", { 1230, 'Renato', date() })
 *        // insert into clientes (codigo, nome, data) values ( 1230, 'Renato', 22081214 )
 * 
 * 14/12/2008 - 10:00:07
 */
char *SR_SQLQuotedString( PHB_ITEM pItem, int nSystemID, BOOL lNullIfEmpty, ULONG *Length ) 
{
   HB_SYMBOL_UNUSED( pItem );
   HB_SYMBOL_UNUSED( nSystemID );
   HB_SYMBOL_UNUSED( lNullIfEmpty );
   HB_SYMBOL_UNUSED( Length );
   return NULL;
}

HB_FUNC( SR_SQLQUOTEDSTRING )
{  
   hb_retc( "" );
}

/*
 * parametro 1: texto
 * parametro 2: caption
 * parametro 3: buttons
 * To DEBUG only 
 * 17/12/2008 - 12:20:15
 */
#include <windows.h>
HB_FUNC( SQLDEBUG )
{
   if ( ISCHAR(2) )
      hb_retni( MessageBox( (HWND) 0, hb_parcx( 1 ), hb_parcx( 2 ), hb_parni( 3 ) ) );
   else
      hb_retni( MessageBox( (HWND) 0, hb_parcx( 1 ), "SQLLIB", hb_parni( 3 ) ) );
}

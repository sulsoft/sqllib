/*
 * Harbour Project source code:
 * Tracing functions.
 *
 * Copyright 1999 Gonzalo Diethelm <gonzalo.diethelm@iname.com>
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
 *
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hb_io.h"
#include "hbtrace.h"
#include "sqllibconsts.ch"
#include "hbapicls.h"
                                                            #include "windows.h"

const char *sql_tr_file_ = "";
int         sql_tr_line_ = 0;

static int s_first   = 1;
static int s_enabled = 0;
static int s_flush   = 0;

static FILE * s_fp = NULL;

void sql_debug_func( char * );

// 
void sql_debug_init( void )
{
#ifdef SQL_DEBUG
   if( !s_enabled )
   {        
      char *env;
      
      if (s_first)
         s_fp = hb_fopen( "sql_debug.log", "w" );
      else
         s_fp = hb_fopen( "sql_debug.log", "a" );

      if( s_fp == NULL )
          s_fp = stderr;
          
      s_first   = 0;
      s_enabled = 1;
      
      env = hb_getenv( "HB_TR_FLUSH" );
      if( env != NULL && env[ 0 ] != '\0' )
         s_flush = 1;
      else
         s_flush = 0;

      if( env )
         hb_xfree( ( void * ) env );
      
      sql_debug_func( "SQL_DEBUGINIT" );      
   }
#endif
   return; 
}

void sql_debug_exit( void )
{
#ifdef SQL_DEBUG
   if( s_enabled )
   {
      sql_debug_func( "SQL_DEBUGINIT" );      
      fflush( s_fp );
      s_enabled = 0;
   }
#endif
}

// 17/03/2009 - 07:23:21
HB_FUNC( SQL_DEBUGINIT )
{
   hb_retl( s_enabled );

   if (!ISLOG(1))
      return;

   if (hb_parl( 1 )) 
      sql_debug_init();
   else
      sql_debug_exit();
   return;
}

#ifdef SQL_DEBUG
void sql_debug_printf( const char * fmt, ... )
{
   int i;
   va_list ap;

   if( !s_enabled )
      return;
      
   /*
    * Clean up the file, so that instead of showing
    *
    *   ../../../foo/bar/baz.c
    *
    * we just show
    *
    *   foo/bar/baz.c
    */
   for( i = 0; sql_tr_file_[ i ] != '\0'; ++i )
   {
      if( sql_tr_file_[ i ] != '.' &&
          sql_tr_file_[ i ] != '/' &&
          sql_tr_file_[ i ] != '\\' )
         break;
   }

   /*
    * Print file and line.
    */
   fprintf( s_fp, "%s:%d: ",
            sql_tr_file_ + i, sql_tr_line_ );

   /*
    * Print the name and arguments for the function.
    */
   va_start( ap, fmt );
   vfprintf( s_fp, fmt, ap );
   va_end( ap );

   /*
    * Print a new-line.
    */
   fprintf( s_fp, "\n" );

   sql_tr_file_ = "";
   sql_tr_line_ = -1;

   if( s_flush )
   {
      fflush( s_fp );
   }
}

static
BOOL sql_debug_procname( int iLevel )
{
   char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 + 10 ]; /* additional 10 bytes for line info (%hu) overhead */
   char file[ _POSIX_PATH_MAX + 1 ];
   USHORT uiLine;
   
   if( !s_enabled )
      return FALSE;
      
   if (hb_procinfo( iLevel++, buffer, &uiLine, file ) )
   {
      int l = strlen( buffer );
      hb_snprintf( buffer + l, sizeof( buffer ) - l, "(%hu)%s%s", uiLine, *file ? HB_I_(" in ") : "", file );   
      fprintf( s_fp, " called from %s", buffer );                    
      return TRUE;
   }
   return FALSE;
}

void sql_debug_item( PHB_ITEM pItem )
{
   BOOL bFreeReq = FALSE;
   ULONG ulLen;
   char * buffer;
   
   if( !s_enabled )
      return;
   if (!pItem)
   {
      fprintf( s_fp, "((null))" );
      return;
   }
   
   if ( HB_IS_SYMBOL( pItem ) )
   {
      fprintf( s_fp, "%p", hb_itemGetSymbol( pItem ));
      return;
   }
      
   if ( HB_IS_OBJECT( pItem ) )
   {
      USHORT uiClass = hb_objGetClass( pItem );
      const  char * szClass = hb_clsName( uiClass ),
                  * szFunc  = hb_clsFuncName( uiClass );
      fprintf( s_fp, "object of %s / %s():New()", szClass, szFunc);      
      return;
   }
      
   if ( HB_IS_BLOCK( pItem ) )
   {
      fprintf( s_fp, "{|| ... }" );
      return;
   }
      
   if ( HB_IS_POINTER( pItem ) )
   {
      fprintf( s_fp, "%p", hb_itemGetPtr( pItem ));
      return;
   }
   
   buffer = hb_itemString( pItem, &ulLen, &bFreeReq );
   
   if (HB_IS_STRING( pItem ))
   {
   // Little hack to format funcname - 17/03/2009 - 10:36:30
      if ( (buffer[ulLen-1] == ':' ) &&
           (strrchr( buffer, '('))   &&
           (strrchr( buffer, ')')) )
         fprintf( s_fp, "%s", buffer );
      else
         fprintf( s_fp, "\"%s\"", buffer );
   } else
      fprintf( s_fp, "%s", buffer );
   
   if( bFreeReq )
      hb_xfree( buffer );
   
   return;
}

void sql_debug_args( PHB_ITEM pItem )
{
   if( !s_enabled )
      return;
      
   if( HB_IS_ARRAY( pItem ) )
   {
      PHB_ITEM pParam;
      ULONG ulIndex;
      ULONG ulElements = hb_arrayLen( pItem );

      fprintf( s_fp, "{ " );
      
      for( ulIndex = 1; ulIndex <= ulElements; ulIndex++ )
      {
         pParam = hb_arrayGetItemPtr( pItem, ulIndex );

         if( HB_IS_ARRAY( pParam ) )
            sql_debug_args( pParam );
         else         
            sql_debug_item( pParam );
            
         if (( ulIndex != ulElements) )
            fprintf( s_fp, ", " );
         else
            fprintf( s_fp, " " );
      }
      fprintf( s_fp, "}" );
      return;
   }
   
   sql_debug_item( pItem );   
   return;
}

// 17/03/2009 - 10:11:02
void sql_debug_func( char *cFuncName )
{
   USHORT uiPCount = hb_pcount();
   USHORT uiParam;    

   if( !s_enabled )
      return;
      
   fprintf( s_fp, "%s( ", cFuncName );
   
   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {              
      sql_debug_args( hb_param( uiParam, HB_IT_ANY ) );
      
      if (uiParam != uiPCount )
         fprintf( s_fp, ", " );
      else
         fprintf( s_fp, " " );
   }
   
   fprintf( s_fp, ")" );
   sql_debug_procname( 1 );      
   fprintf( s_fp, "\n" );
}

HB_FUNC( SQL_DEBUG )
{
   USHORT uiPCount = hb_pcount();
   USHORT uiParam;    

   if( !s_enabled )
      return;
      
   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {              
      sql_debug_args( hb_param( uiParam, HB_IT_ANY ) );
      
      if (uiParam != uiPCount )
         fprintf( s_fp, ", " );
      else
         fprintf( s_fp, " " );
   }
   
   fprintf( s_fp, "\n" );
}

HB_FUNC( SQL_DEBUGEX )
{
   USHORT uiPCount = hb_pcount();
   USHORT uiParam;    

   if( !s_enabled )
      return;
      
   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {              
      sql_debug_args( hb_param( uiParam, HB_IT_ANY ) );
      
      if (uiParam > 1 && uiParam != uiPCount )
         fprintf( s_fp, ", " );
      else
         fprintf( s_fp, " " );
   }
   
   fprintf( s_fp, "\n" );
}

/*
 * Dump WA passed as arg1 into debug log file
 * 18/03/2009 - 18:23:47
 */ 
HB_FUNC( SQL_DUMPWA )
{
   PHB_ITEM pWAData = hb_param( 1, HB_IT_ARRAY );
   ULONG ulIndex;   
   char *saNames[] = { "WA_SYSTEMID          ",           
                       "WA_ENGINE            ",             
                       "WA_CONNECTION        ", 
                       "WA_POINTER           ", 
                       "WA_RESULT            ",                     
                       "WA_TABLETYPE         ", 
                       "WA_PACKET_SIZE       ", 
                       "WA_TRANSCOUNTER      ", 
                       "WA_TEMP              ", 
                       "WA_FLD_RECNO         ", 
                       "WA_FLD_DELETED       ", 
                       "WA_SCHEMA            ", 
                       "WA_DATABASE          ", 
                       "WA_TABLENAME         ", 
                       "WA_BUFFER_INFO_BASE  ", 
                       "WA_BUFFER_ARR        ",         
                       "WA_BUFFER_POS        ",         
                       "WA_BUFFER_ROWCOUNT   ",    
                       "WA_INVALIDBUFFER     ",      
                       "WA_WORKAREA_INFO_BASE", 
                       "WA_RECORDSET         ", 
                       "WA_BOF               ", 
                       "WA_EOF               ", 
                       "WA_LOCATEFOR         ", 
                       "WA_SCOPEINFO         ", 
                       "WA_FCOUNT            ", 
                       "WA_RECNO             ", 
                       "WA_RECCOUNT          ", 
                       "WA_APPEND            ", 
                       "WA_RECORDCHANGED     ", 
                       "WA_FOUND             ", 
                       "WA_LOCK              ", 
                       "WA_RECSIZE           ", 
                       "WA_STRUCT            ", 
                       "WA_REAL_STRUCT       ", 
                       "WA_SL_GOTOID         ", 
                       "WA_SL_GOTOP          ",     
                       "WA_SL_GOBOTTOM       ",     
                       "WA_INDEX             ",     
                       "WA_INDEX_CURR        ",
                       "WA_LAST_PACKSIZE     ",          
                       "WA_ALIAS             " };
   
   if( !s_enabled )
      return;
   if (!pWAData)
      return;
      
   fprintf( s_fp, "Dump for aWAData" );
   sql_debug_procname( 1 );
   fprintf( s_fp, " ****************\n" );
         
   for (ulIndex=1; ulIndex<= WA_SIZE; ulIndex++ )
   {
       fprintf( s_fp, " #%02d %s = ", ulIndex, saNames[ulIndex] );
       sql_debug_item( hb_arrayGetItemPtr( pWAData, ulIndex ) );
       fprintf( s_fp, "\n" );
   }   
   return;
}
#endif

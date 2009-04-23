/*
 * SQLLIB RDD Project source code:
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
#if defined(_MSC_VER)   
   /*
    * Eliminating deprecation warnings. See more info at:
    * http://msdn.microsoft.com/en-us/library/8ef0s5kh(VS.80).aspx
    */
   #define _CRT_SECURE_NO_WARNINGS
   #include <windows.h>
#endif

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbdate.h"

#include "sqllibconsts.ch"
#include "sql_sysfuncs.h"

#include "sqldebug.h"
#include "sql_itemapi.h"

extern BOOL SQLSYS_GETINFO( int nDriver );
extern char *SQLSYS_DATETOISO( char * szDate, int iYear, int iMonth, int iDay, BOOL bQuoted );

void sql_item_clear( sql_item_arg *pItemArg )
{
   memset( pItemArg, '\0', sizeof ( sql_item_arg ));
}

BOOL sql_item2str( sql_item_arg *pItemArg, SQLSYS_INFO *pSysInfo )
{
   ULONG lSize  = 0L;         /* For Temporary values */
   char *szText = NULL;       /* Temporary buffer */
   BOOL bFree   = TRUE;
   
   if (!pItemArg)
   {
      HB_TRACE(HB_TR_DEBUG,("  Invalid Argument -> %p", pItemArg ));
      return FALSE;
   }
      
   if (!pSysInfo)
   {
      pSysInfo = (SQLSYS_INFO *) SQLSYS_GETINFO( 0 );
      
      // Valid driver? It has been correctly loaded into memory?
      if (!SR_ISVALID_DRIVER(pSysInfo))
      {
         HB_TRACE(HB_TR_DEBUG,("  Invalid driver !!" ));
         return FALSE;
      }
   }   
   
   HB_TRACE(HB_TR_DEBUG,("  ARGUMENT  => '%s'", hb_itemTypeStr( pItemArg->pItem ) ));
   
   switch( hb_itemType( pItemArg->pItem ) )
   {
      case HB_IT_MEMO:
      case HB_IT_STRING:
      {         
         HB_TRACE(HB_TR_DEBUG,("  As Text   => '%s'", hb_itemGetCPtr( pItemArg->pItem ) ));
         
         lSize  = hb_itemGetCLen( pItemArg->pItem );
         szText = (*pSysInfo->EscapeString) ( hb_itemGetCPtr( pItemArg->pItem ), &lSize, 0 );
         
         /*
         char *szTemp, *szBuff;
         szTemp = (*pSysInfo->EscapeString) ( hb_itemGetCPtr( pItemArg->pItem ), &lSize, 0 );
         
         if (szTemp)
         {
            szText = (char *) hb_xgrab( lSize + 3L );   // chr(0) + 2 separators
            szBuff = szText;
            
           *szText = pSysInfo->FieldDelim; szText ++;
            memcpy( szText, szTemp, lSize );
            szText += lSize;
           *szText = pSysInfo->FieldDelim; szText ++;
           *szText = '\0';
           
           hb_xfree( szTemp );
         }
         /***/
         break;
      }
      case HB_IT_LOGICAL:
         HB_TRACE(HB_TR_DEBUG,("  As BOOL   => %d", (int) hb_itemGetL( pItemArg->pItem ) ));
         szText = (hb_itemGetL( pItemArg->pItem ) ? pSysInfo->BoolTrue : pSysInfo->BoolFalse); 
         lSize  = strlen( szText );
         bFree  = FALSE;
         break;
         
      case HB_IT_DATE:
      {               
         int iYear, iMonth, iDay;
         hb_dateDecode( hb_itemGetDL( pItemArg->pItem ), &iYear, &iMonth, &iDay );
         HB_TRACE(HB_TR_DEBUG,("  As DATE   => %04d/%02d/%02d", iYear, iMonth, iDay ));
         
         if (pSysInfo->DateFormat == ESCAPE_FORMAT_DATE_ISO)
         {               
            szText = SQLSYS_DATETOISO( (char *) hb_xgrab(13), iYear, iMonth, iDay, TRUE );
            lSize  = 12L;
         }
         break;
      }   
      case HB_IT_INTEGER:
      case HB_IT_DOUBLE:
      case HB_IT_LONG:
      {
         szText = hb_itemStr( pItemArg->pItem, NULL, NULL );
         HB_TRACE(HB_TR_DEBUG,("  As Number => %s", szText ));
   
         if( szText )
         {
            ULONG nToSkip = 0L;
   
            while( szText[ nToSkip] == ' ' )
               ++nToSkip;
   
            /* Rever isto aqui de perto */
            if( nToSkip )
               memmove( szText, szText + nToSkip, strlen( szText + nToSkip ) + 1 );
            
            lSize = strlen( szText );
            HB_TRACE(HB_TR_DEBUG,("  As Text => '%s' -- %lu", szText, lSize ));
         }
         break;
      }
         
      default:                    
      {
         szText = (char *) hb_xgrab(5);
        *szText = '\0';
         strcat( szText, "NULL" );
         lSize  = strlen( szText );
         break;
      }
   }
      
   if (!szText)
   {
      HB_TRACE(HB_TR_DEBUG,("  Impossivel converter este valor:" ));
      HB_TRACE_ARGS( pItemArg->pItem ); 
      return FALSE;
   }
      
   pItemArg->szResult = szText;
   pItemArg->ulSize   = lSize; 
   pItemArg->bDestroy = bFree;      
   return TRUE;
}

BOOL sql_itemarr2str( sql_item_arg *pItems[], USHORT uiCount )
{
   USHORT i;

   for (i=0; i< uiCount; i++ )
   {                                
      if (!sql_item2str( pItems[i], (SQLSYS_INFO *) NULL ))
         return FALSE;
   }
   return TRUE;
}

/*
 *  
 * 19/03/2009 - 10:00:36
 */
HB_FUNC( SQLITEM2STR )
{
   sql_item_arg pArg;
   PHB_ITEM pItem   = hb_param( 1, HB_IT_ANY ); /* SQL command */
   SQLSYS_INFO *pSysInfo;
   
   HB_TRACE_ARGS( "SQLITEM2STR" )
   
   pSysInfo = (SQLSYS_INFO *)SQLSYS_GETINFO( hb_parni(2) );

   if (!SR_ISVALID_DRIVER(pSysInfo))
   {
      HB_TRACE(HB_TR_DEBUG,(" Invalid Driver ==> %d", hb_parni(2)));
      return;
   }

   sql_item_clear( &pArg );   
   pArg.pItem = pItem;

   if (!sql_item2str( &pArg, (SQLSYS_INFO *) NULL ))
   {
      HB_TRACE(HB_TR_DEBUG,(" Conversion failed !!!" ));
      return;
   }

   if (pArg.bDestroy)
   {   
      hb_retclen_buffer( pArg.szResult, pArg.ulSize );
   } else {
      hb_retclen( pArg.szResult, pArg.ulSize );
   }   
   return;
}

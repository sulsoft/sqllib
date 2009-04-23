/*
 * SQLLIB RDD Project source code:
 * SQLParams() API
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbdate.h"

#include "sqllibconsts.ch"
#include "sql_sysfuncs.h"

#include "sqldebug.h"

extern BOOL SQLSYS_GETINFO( int nDriver );
extern char *SQLSYS_DATETOISO( char * szDate, int iYear, int iMonth, int iDay, BOOL bQuoted );

/*
 * Preenche um SQL passado como 1§ arqumento com os valores constantes no array 
 * passado como o 2§ argumento, formatado segundo o ID do driver passado como 3§
 * argumento. TODOS OS ARGUMENTOS SAO OBRIGATORIOS, ESPECIALMENTE O TERCEIRO.
 * EX:
 *       ? SQLPARAMS( "insert into clientes (codigo, nome, data, time ) values (?, ?, ?, \?)", { 1230, 'Renato', date(), 'NOW()' }, ID_MYSQL )
 *        // insert into clientes (codigo, nome, data, time ) values ( 1230, 'Renato', 22081214, NOW() )
 * 
 * 14/12/2008 - 10:00:07
 */
HB_FUNC( SQLPARAMS )
{
   ULONG lLength = 0L;         /* Total Length of New Result */
   ULONG lSize;                /* For Temporary values */
   ULONG lCount;               /* Items in array */
   ULONG lPos;                 /* Current item position  in pArray */
   BOOL  bLiteral;
   HB_TYPE type;
      
   char *cSQL;                 /* Original SQL */
   char *cResult;              /* New SQL as result */
   char *szText;               /* Temporary item */
   char *szTemp;               /* Temporary item #2 */
   
   int iYear, iMonth, iDay;
   
   PHB_ITEM pSQL   = hb_param( 1, HB_IT_ANY ); /* SQL command */
   PHB_ITEM pArray = hb_param( 2, HB_IT_ANY ); /* Values to fill SQL */
   PHB_ITEM pArgs  = NULL;
   PHB_ITEM pItem, pItem2;      
   
   SQLSYS_INFO *pSysInfo;
   
   HB_TRACE(HB_TR_DEBUG,("SQLPARAMS(\"%s\",%p,%d)", hb_itemGetCPtr( pSQL ), pArgs, hb_parni(3) ));
   
   /* Check first argument */
   if( !pSQL || !HB_IS_STRING( pSQL ) )
      goto fim;
      
   pSysInfo = (SQLSYS_INFO *)SQLSYS_GETINFO( (int) hb_parni(3) );
   
   // Valid driver? It has been correctly loaded into memory?
   if (!SR_ISVALID_DRIVER(pSysInfo))
      goto fim;
   
   cSQL  = hb_itemGetCPtr( pSQL ); 
   lSize = hb_itemGetCLen( pSQL );
   lCount= ((pArray) ? hb_arrayLen( pArray ) : 0L );
      
   HB_TRACE(HB_TR_DEBUG,(" DRIVER => %s", pSysInfo->DrvName ));
   HB_TRACE(HB_TR_DEBUG,(" cSQL   => %s", cSQL ));
   HB_TRACE(HB_TR_DEBUG,(" lSize  => %lu", lSize ));
   HB_TRACE(HB_TR_DEBUG,(" lCount => %lu", lCount ));
      
   /* Check second argument:  */   
   if( lCount < 1L )
   {
      hb_retclen( cSQL, lSize ); 
      return;
   }     
      
   /* Compute & Convert all values */
   pArgs = hb_itemNew( NULL ); hb_arrayNew( pArgs, lCount );      /* Create new array with converted values */
   lLength = lSize; // 0L;                                               
   
   for ( lPos= 1L; lPos <= lCount; lPos++ )
   {
      pItem  = hb_arrayGetItemPtr( pArray, lPos );
      szText = NULL;
      lSize  = 0L;
      
      HB_TRACE(HB_TR_DEBUG,(" Arg @%lu => '%s'", lPos, hb_itemTypeStr( pItem ) ));
      
      switch( hb_itemType( pItem ) )
      {
         case HB_IT_MEMO:
         case HB_IT_STRING:
            HB_TRACE(HB_TR_DEBUG,("  As Text => '%s'", hb_itemGetCPtr( pItem ) ));
            
            lSize  = hb_itemGetCLen( pItem );
            szText =  (*pSysInfo->EscapeString) ( hb_itemGetCPtr( pItem ), &lSize, 0 );

            lLength+= 2L;  /* Because delimeters */
            break;

         case HB_IT_LOGICAL:
            lLength += pSysInfo->BoolMaxSize;  /* Because delimeters */
            break;
            
         case HB_IT_DATE:
                  
            /* TODO: Move this code to a separate func */
            hb_dateDecode( hb_itemGetDL( pItem ), &iYear, &iMonth, &iDay );
            
            switch( pSysInfo->DateFormat )
            {  
               case ESCAPE_FORMAT_DATE_ISO:               
                  szText = SQLSYS_DATETOISO( (char *) hb_xgrab(13), iYear, iMonth, iDay, TRUE );
                  lSize  = 12L;
                  break;
            }
            break;
            
         case HB_IT_INTEGER:
         case HB_IT_DOUBLE:
         case HB_IT_LONG:
         {
            szText = hb_itemStr( pItem, NULL, NULL );
            HB_TRACE(HB_TR_DEBUG,("  As Text => '%s'", szText ));
      
            if( szText )
            {
               ULONG nToSkip = 0;
      
               while( szText[ nToSkip] == ' ' )
                  ++nToSkip;
      
               if( nToSkip )
                  memmove( szText, szText + nToSkip, strlen( szText + nToSkip ) + 1 );
               
               lSize = strlen( szText );
               HB_TRACE(HB_TR_DEBUG,("  As Text => '%s' -- %lu", szText, lSize ));
            }
            break;
         }
            
         default:
            lLength += 4;  /* NULL */
            break;
      }
      
      HB_TRACE(HB_TR_DEBUG,("   FINAL => '%s' - %lu", szText, lSize ));
      
      if ( szText )
      {
         lLength += lSize;
         hb_arraySetCPtr( pArgs, lPos, szText, lSize );
      }
   }
   
   HB_TRACE(HB_TR_DEBUG,(" lLength ==> %lu", lLength ));

   /*
    * Ok agora que temos todos os campos calculados, vamos entao montar o comando 
    * SQL de destino com os resultados que temos aqui.
    * 15/12/2008 - 00:16:44
    */
   szText = (char *) hb_xgrab( lLength + 1 );
   cResult= szText;
   lPos   = 0L;
    
   while ( *cSQL != '\0' )
   {
      // 21/01/2009 - 22:12:39
      if ( *cSQL== '\\' && cSQL[1] == '?' )
      {
         bLiteral = TRUE;
         cSQL ++;
         continue;
      }

     *szText = *cSQL;
     
      /* Need param ? */
      if (*cSQL == '?')
      {
         lPos ++;
         
         if (lPos <= lCount )
         {
            pItem  = hb_arrayGetItemPtr( pArgs, lPos );
            pItem2 = hb_arrayGetItemPtr( pArray, lPos );
            type   = hb_itemType( pItem2 );
            
            if (HB_IS_STRING( pItem ) )
            {
               lSize = hb_itemGetCLen( pItem );

               if ((type == HB_IT_MEMO) || (type == HB_IT_STRING ))
               {
                  if (!bLiteral)
                  {
                    *szText = '\'';
                     szText++;
                  }
                  memmove( szText, hb_itemGetCPtr( pItem ), lSize );
                  szText  += lSize;
                  
                  if (!bLiteral)
                  { 
                    *szText = '\'';
                     szText ++;
                  }
               } else {
                  memmove( szText, hb_itemGetCPtr( pItem ), lSize );
                  szText  += lSize;
               } 
               
               szText --;
            } else {            
               
               /* If ORIGINAL value is a Boolean or NIL, we have to customize it */
               switch( type )
               {
                  case HB_IT_LOGICAL:
                     szTemp = (hb_itemGetL( pItem2 ) ? pSysInfo->BoolTrue : pSysInfo->BoolFalse); 
                     lSize  = strlen( szTemp );
                     
                     memmove( szText, szTemp, lSize );
                     szText += lSize-1;
                     break;
         
                  default:
                     memmove( szText, "NULL", 4 );
                     szText += 3; 
                     break;
               }
            }
                        
            hb_itemClear( pItem );
         } else {
            memmove( szText, "NULL", 4 );
            szText += 3; 
         }
      }    
      szText ++; cSQL ++;
      bLiteral = FALSE;      
   }
   
  *szText = '\0';
   lLength= szText - cResult;    /* Com isto, cortamos qualquer excedente na string */
   
   HB_TRACE(HB_TR_DEBUG,(" Comando final ==> '%s'", cResult ));
   
   fim:         
      if (pArgs)
         hb_itemRelease( pArgs );
         
      if (lLength > 0L)
         hb_retclen_buffer( cResult, lLength );
      else
         hb_retc( "" );

      return;
}

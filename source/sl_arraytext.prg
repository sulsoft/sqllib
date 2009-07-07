/*
 * SQLLIB RDD Project source code
 * A Free & Open source RDD for Harbour & xHarbour Compilers
 *
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
#include "sqllibrdd.ch"

/*
 * Converte um array para string separado por delimitadores
 * Adicionado o 4° parametro para colcoar um sufixo automaticamente.
 * 16/10/2006 09:38:26
 */
FUNCTION SQLArray2Text( aArray, cSeparator, cDelimiter, cSufixStr )
   LOCAL cList := ''       // Define an empty array
   LOCAL nPos              // Position of cDelimiter in cList
   LOCAL nTot  := LEN( aArray )
   LOCAL Buff
   
   IF ValType(aArray) != 'A' 
      RETURN ""
   End
   
   DEFAULT cSeparator TO ","
   DEFAULT cDelimiter TO ""
   DEFAULT cSufixStr  TO ''
   
   // Loop while there are more items to extract
   FOR nPos := 1 TO nTot
       Buff := aArray[nPos]
   
       DO CASE
       CASE VALTYPE( Buff ) == "N" ; Buff := SQLNTrim( Buff ) 
       CASE VALTYPE( Buff ) == "D" ; Buff := DTOS( Buff )
       CASE VALTYPE( Buff ) == "L" ; Buff := IIF( Buff, 'T', 'F' )
       End
   
      cList += cDelimiter + Buff + cDelimiter + cSufixStr + IF( nPos < nTot, cSeparator, '' )
   END
   RETURN ( cList )
         
         
FUNCTION SQLText2Array( cList, cDelimiter )
   DEFAULT cDelimiter TO ","
   
   IF VALTYPE(cList) == 'A' 
      RETURN cList
   End
   RETURN hb_ATokens( cList, cDelimiter )

//--EOF--//

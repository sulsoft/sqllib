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

********************
function SL_ToString( x, lLineFeed, lInherited, lType, cFile, lForceLineFeed )
********************

    local s := ''
    local t := valtype( x )
    local i, j

    DEFAULT lLineFeed      := TRUE
    DEFAULT lInherited     := FALSE
    DEFAULT lType          := FALSE
    DEFAULT cFile          := ""
    DEFAULT lForceLineFeed := .F.
    
    do case
       case ( t == "C" )
            s := iif( lType, "[C]=", "" ) + '"' + x + '"'
       case ( t == "N" )
            s := iif( lType, "[N]=", "" ) + alltrim(str( x ))
       case ( t == "D" )
            s := iif( lType, "[D]=", "" ) + "ctod('"+ dtoc(x) +"')"
       case ( t == "L" )
            s := iif( lType, "[L]=", "" ) + iif( x, '.T.', '.F.' )
       case ( t == "M" )
            s := iif( lType, "[M]=", "" ) + '"' + x + '"'
       case ( t == "B" )
            s := iif( lType, "[B]=", "" ) + '{|| ... }'
       case ( t == "U" )
            s := iif( lType, "[U]=", "" ) + 'NIL'
       case ( t == "A" )
            s := iif( lType, "[A]=", "" ) + "{" + iif( valtype( x[1] ) = "A" .or. lForceLineFeed, CRLF, "" )
            j := len(x)
            
            for i := 1 to j
                s += iif( valtype( x[i] ) == "A", "  ", " " ) + iif( lForceLineFeed, " ", "" ) + SL_ToString( x[i], FALSE )
                s += iif( i <> j, ",", "" )
                if lLineFeed
                   if !lInherited .and. ( valtype( x[i] ) == "A" .or. lForceLineFeed )
                      s += CRLF
                   endif
                endif
            next
            
            s += "}"

       case ( t == "O" )
            if lInherited
               && É necessário linkar \harbour\lib\xhb.lib
               s := iif( lType, "[O]=", "" ) + hb_dumpvar( x ) + iif( lLineFeed, CRLF, "" )
            else
               s := iif( lType, "[O]=", "" ) + x:ClassName()+'():New()' + iif( lLineFeed, CRLF, "" )
            endif
    endcase
    
    if !empty( cFile )
       memowrit( cFile, s )
    endif
    
return s

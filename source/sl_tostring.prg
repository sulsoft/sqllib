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

FUNCTION SL_ToString( x, lLineFeed, lInherited )
    LOCAL s := ''
    LOCAL t := VALTYPE( x )
    LOCAL i,j

    IF lInherited == NIL THEN lInherited := FALSE
    IF lLineFeed  == NIL THEN lLineFeed  := TRUE

    DO CASE
    CASE ( t == "C" )
      s := '"' + x + '"'
    CASE ( t == "N" )
      s := alltrim(str( x ))
    CASE ( t == "D" )
      s := "CTOD('"+ DTOC(x) +"')"
    CASE ( t == "L" )
      s := iif( x, '.T.', '.F.' )
    CASE ( t == "M" )
      s := '"' + x + '"'
    CASE ( t == "B" )
      s := '{|| NIL } '
    CASE ( t == "U" )
      s := 'NIL'
    CASE ( t == "A" )
      s := "{"
      j := LEN(x)

      FOR i := 1 TO j
          s += SL_ToString( x[i], TRUE )

          IF ( i <> j )
             s += ","
          End

          IF lLineFeed
             IF ( !lInherited ) .and. VALTYPE( x[i] ) == "A" THEN;
                s += CHR(13)+CHR(10)
          End
      End

      s += "}"

    CASE ( t == "O" )
      s := x:ClassName()+'():New()'
    End
    RETURN s
    
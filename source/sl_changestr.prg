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
 Função para mudar a estrutura do arquivo - Rossine 03/07/2009 as 01:19
*/

************************
function SL_CHANGESTRUCT( cTable, aNewStruct, pConn, cSchema, lOnly, cTableBkp ) && Rossine 30/06/09
************************

local aConn, Id, aOldStruct, lRet := .F., cCampo, nPrKey
local bOldStr, bNewStr, n, cField, nOrd, cSql, aSql, aPks := { }, cPks := ""
local cTipoN, nSizeN, nDecimN, lNNullN, nPrKeyN, xDefauN
local cFieldO, cTipoO, nSizeO, nDecimO, lNNullO, xDefauO, cFieldN
local lUniquN, lUniquO, lUniqu

   DEFAULT cSchema   := "public"
   DEFAULT cTableBkp := ""
   DEFAULT lOnly     := .F.

   DEBUG_ARGS

   IF cTable == NIL
      RETURN .F.
   ELSE
      cTable := StrTran( Alltrim(cTable), "*", "%" )
   Endif

   if valtype( aNewStruct ) != "A"
      RETURN .F.
   endif   

   cTable := SQLAdjustFn( cTable )

   if !SL_TABLE( cTable )
      return .F.
   endif
   
   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' 
      RETURN .F.
   Endif

   cSchema    := SQLAdjustFn( cSchema )
   Id         := aConn[SL_CONN_SYSTEMID]
   aOldStruct := SL_DBSTRUCT( cTable, , .T. )

   if !lOnly && Quando não se especifica "ONLYFIELDS", automaticamente o backup é acionado por questão de segurança.
      set century OFF
      cTableBkp := dtos(date()) + strtran(time(),":","")
      set century ON
   endif

**msgstop( SL_ToString( aOldStruct ), "Old" )
**msgstop( SL_ToString( aNewStruct ), "New" )

   /*
    * Ajustamos os blocos de codigo agora para testar a existencia dos campos
    */
   bOldStr := { |cField| cCampo := Upper( Alltrim( cField )), aScan( aOldStruct, { |x| Alltrim(Upper(x[1])) == cCampo } ) }
   bNewStr := { |cField| cCampo := Upper( Alltrim( cField )), aScan( aNewStruct, { |x| Alltrim(Upper(x[1])) == cCampo } ) }

   /*
    * Montamos o comando SQL com base no driver atual
    */

   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        aSql  := { } 

        if !empty( cTableBkp )
           DEBUG "Backup das colunas foi acionado...."
           FOR n := 1 TO Len( aOldStruct )
               cField := SQLAdjustFn( aOldStruct[n,1] ) && Nome do campo
               nOrd   := bNewStr:Eval( cField )
               if cField != SL_COL_RECNO .and. cField != SR_COL_RECNO .and. cField != SL_COL_DELETED
                  if nOrd > 0 .or. !lOnly
                     COL_BACKUP( iif( nOrd > 0, .F., .T. ), cSchema, cTable, cField, cTableBkp, @aSql, aOldStruct, n, bNewStr, SQLAdjustFn( iif( nOrd > 0 .and. valtype(aNewStruct[nOrd,10]) = "C", aNewStruct[nOrd,10], cField ) ) )
                  endif
               endif
           next
        endif

        if !lOnly
           DEBUG "1º passo, verificamos as colunas a serem excluidas...."
           
           FOR n = 1 TO Len( aOldStruct )
               cField := SQLAdjustFn( aOldStruct[n,1] ) && Nome do campo
               nOrd   := bNewStr:Eval( cField )
               if nOrd > 0 .or. cField == SL_COL_RECNO .or. cField == SR_COL_RECNO .or. cField == SL_COL_DELETED
                  loop
               endif
               lUniqu := aOldStruct[n,6] && .T. indica o flag UNIQUE      
               nPrKey := aOldStruct[n,7] && > 0 indica o flag PRIMARY KEY 
               cSql   := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" DROP COLUMN ' + cField + " CASCADE"
               aadd( aSql, cSql )
               if valtype( nPrKey ) = "N" .and. nPrKey > 0
                  cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" DROP CONSTRAINT ' + cTable + "_" + SL_CONSTRAINT_PK + " CASCADE"
                  aadd( aSql, cSql )
               endif
               if valtype( lUniqu ) = "L" .and. lUniqu
                  cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" DROP CONSTRAINT ' + cTable + "_" + SL_CONSTRAINT_UNIQUE + "_" + cField + " CASCADE"
                  aadd( aSql, cSql )
               endif
           next
        endif

        DEBUG "2º Passo, adicionamos os novos campos à estrutura atual...."

        FOR n = 1 TO Len( aNewStruct )
            cField := SQLAdjustFn( aNewStruct[n,1] ) && Nome do campo
            cFieldN:= SQLAdjustFn( iif( valtype( aNewStruct[n,10] ) = "C", aNewStruct[n,10], cField ) ) && Novo Nome do campo
            nOrd   := bOldStr:Eval( cField )
            if nOrd = 0
               ADD_COLUMN( cSchema, cTable, aNewStruct, @aSql, cFieldN, n )
            endif
        next

        DEBUG "3º Passo, alteramos os campos que mudaram de <NOME> ou <TIPO> ou <TAMANHO> ou <DECIMAL> ou <NOT NULL> ou <UNIQUE> ou <PRIMARY KEY> ou <DEFAULT>..."

        FOR n = 1 TO Len( aNewStruct )
            cField := SQLAdjustFn( aNewStruct[n,1] ) && Nome do campo
            cFieldN:= SQLAdjustFn( iif( valtype( aNewStruct[n,10] ) = "C", aNewStruct[n,10], cField ) ) && Novo Nome do campo
            nOrd   := bOldStr:Eval( cField )
            if nOrd = 0
               loop
            endif
            cTipoN  := aNewStruct[n,2] && Tipo do campo, minimo 1¦ letra
            nSizeN  := aNewStruct[n,3] && Tamanho do campo              
            nDecimN := aNewStruct[n,4] && Casas decimais                
            lNNullN := aNewStruct[n,5] && .T. indica o flag NOT NULL    
            lUniquN := aNewStruct[n,6] && .T. indica o flag UNIQUE      
            nPrKeyN := aNewStruct[n,7] && > 0. indica o flag PRIMARY KEY 
            xDefauN := aNewStruct[n,8] && Expressão DEFAULT para o campo

            cFieldO := aOldStruct[nOrd,1] && Nome do Campo
            cTipoO  := aOldStruct[nOrd,2] && Tipo do campo, minimo 1¦ letra
            nSizeO  := aOldStruct[nOrd,3] && Tamanho do campo              
            nDecimO := aOldStruct[nOrd,4] && Casas decimais                
            lNNullO := aOldStruct[nOrd,5] && .T. indica o flag NOT NULL    
            lUniquO := aOldStruct[nOrd,6] && .T. indica o flag UNIQUE      
*            nPrKeyO := aOldStruct[nOrd,7] && > 0. indica o flag PRIMARY KEY 
            xDefauO := aOldStruct[nOrd,8] && Expressão DEFAULT para o campo

            if cFieldN != cFieldO
               DEBUG "Mudança de <Nome de campo> de [" + cFieldO + "] para [" + cFieldN + "]"
               cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" RENAME COLUMN ' + cFieldO + " TO " + cFieldN
               aadd( aSql, cSql )
            endif

**---------------------------*
** Somente serão convertidos *
**---------------------------*
** Caracter para Data        *
** Caracter para Numérico    *
** Data para Caracter        *
** Númerico para Caracter    *
**---------------------------*

            cSql := ""

            if cTipoN != cTipoO
               DEBUG "Mudança de <Tipo> de [" + cTipoO + "] para [" + cTipoN + "] no campo [" + cFieldN + "]"
               cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ALTER COLUMN ' + cFieldN
               if     cTipoN = "C"
                      DEBUG "Mudança de Nome de campo de " + cFieldO + " para " + cFieldN
                      if     cTipoO = "N"
                             cSql += " TYPE numeric(" + alltrim(str(nSizeN)) + iif( nDecimN > 0, "," + alltrim(str(nDecimN)), "" ) + ;
                                     " ) USING CAST ( " + cFieldN + " AS numeric )"
                      elseif cTipoO = "D"
*                             cSql += " TYPE character(" + alltrim(str(nSizeN)) + ") USING to_char( " + cFieldN + ", 'YYYYMMDD' )"
                             cSql += " TYPE varchar(" + alltrim(str(nSizeN)) + ") USING to_char( " + cFieldN + ", 'YYYYMMDD' )"
                      endif
               elseif cTipoN = "N"
                      if cTipoO = "C"
*                         cSql += " TYPE character(" + alltrim(str(nSizeN)) + ;
                         cSql += " TYPE varchar(" + alltrim(str(nSizeN)) + ;
                                 " ) USING CAST ( " + cFieldN + " AS varchar )"
                      endif
               elseif cTipoN = "D"
                      if cTipoO = "C"
                         cSql += " TYPE date USING to_date( " + cFieldN + ", 'YYYYMMDD' )"
                      endif
               endif
               aadd( aSql, cSql )
            else
               if nSizeO != nSizeN .or. nDecimO != nDecimN
                  if nSizeO != nSizeN
                     DEBUG "Mudança de <Tamanho> de [" + alltrim(str(nSizeO)) + "] para [" + alltrim(str(nSizeN)) + "] no campo [" + cFieldN + "]"
                  endif
                  if nDecimO != nDecimN
                     DEBUG "Mudança de <Tamanho Decimal> de [" + alltrim(str(nDecimO)) + "] para [" + alltrim(str(nDecimN)) + "] no campo [" + cFieldN + "]"
                  endif
                  cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ALTER COLUMN ' + cFieldN
                  if     cTipoN = "C"
*                         cSql += " TYPE character(" + alltrim(str(nSizeN)) + ")"
                         cSql += " TYPE varchar(" + alltrim(str(nSizeN)) + ")"
                  elseif cTipoN = "N"
                         cSql += " TYPE numeric(" + alltrim(str(nSizeN)) + iif( nDecimN > 0, "," + alltrim(str(nDecimN)), "" ) + ")"
                  endif
                  aadd( aSql, cSql )
               endif
            endif

            if     lNNullO .and. !lNNullN
                   DEBUG " Excluido <NOT NULL> para o campo [" + cFieldN + "]"
                   aadd( aSql, 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ALTER COLUMN ' + cFieldN + " DROP NOT NULL" )
            elseif lNNullN
                   DEBUG " Setado <NOT NULL> para o campo [" + cFieldN + "]"
                   aadd( aSql, 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ALTER COLUMN ' + cFieldN + " SET NOT NULL" )
            endif

            if     lUniquO .and. !lUniquN
                   DEBUG " Excluido <UNIQUE> para o campo [" + cFieldN + "]"
                   aadd( aSql, 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" DROP CONSTRAINT ' + cTable + "_" + SL_CONSTRAINT_UNIQUE + "_" + cFieldN + " CASCADE" )
            elseif lUniquN
                   DEBUG " Setado <UNIQUE> para o campo [" + cFieldN + "]"
                   aadd( aSql, 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ADD CONSTRAINT ' + cTable + "_" + SL_CONSTRAINT_UNIQUE + "_" + cFieldN + " UNIQUE (" + cFieldN + ")" )
            endif

            if valtype(nPrKeyN) = "N" .and. nPrKeyN > 0
               DEBUG " Setado <PRIMARY KEY> para o campo [" + cFieldN + "]"
               aadd( aPks, { nPrKeyN, cFieldN } )
            endif
            
            if  empty( xDefauO ) .and. !empty( xDefauN )
                DEBUG " Excluido <DEFAULT> para o campo [" + cFieldN + "]"
                aadd( aSql, 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ALTER COLUMN ' + cFieldN + " DROP DEFAULT" )
            endif
            
            if !empty( xDefauN )
               DEBUG " Setado <DEFAULT=" + xDefauN + "> para o campo [" + cFieldN + "]"
               cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ALTER COLUMN ' + cFieldN + " SET DEFAULT "
               if cTipoN $ [CD]
                  cSql += "'" + xDefauN + "'" && "to_date( '" + xDefauN + "', 'YYYYMMDD' )"
               else
                  cSql += xDefauN
               endif
               aadd( aSql, cSql )
            endif
        next

        if len(aPks) > 0
           asort( aPks,,,{ |x,y| x[1] < y[1] } )
           cPks := ""
           for n = 1 to len(aPks)
               cPks += aPks[n,2] + iif( n < len(aPks), ",", "" )
           next
           aadd( aSql, 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ADD CONSTRAINT ' + cTable + "_" +  SL_CONSTRAINT_PK + " PRIMARY KEY (" + cPks + ")" )
        endif

**        xmsgstop( SL_ToString( aSql,.T.,,, "DUMP.TXT", .T. ) )

        lRet := .T.

        if len(aSql) > 0
           for n = 1 to len(aSql)
               DEBUG "Inicio execução as " + time() + " - Sentença: [" + aSql[n] + "]"
               lRet := SL_EXECQUERYEX( aSql[n], aConn[1] )
               DEBUG "Final execução as " + time() + iif( lRet, " - SENTENÇA EXECUTADA COM SUCESSO", " - ERRO NA EXECUÇÃO DESTA SENTENÇA" )
**               xmsgstop( hb_valtostr(lRet) + "-" + SL_ToString( aSql[n],.T.,,, "DUMP.TXT", .T. ) )
               if !lRet
**                  SL_Error( 1000, "Erro na sentença: " + aSql[n], "Problema na modificação da estrutura da tabela [" + cTable + "]..." )
                  xmsgstop( "Erro na sentença: " + aSql[n], "Problema na modificação da estrutura da tabela [" + cTable + "]..." )
                  exit
               endif
           next
        endif

        if lRet
           /* Rodar Reindex() da tabela */
        endif

   OTHERWISE

   Endcase

RETURN lRet

**************************
static function COL_BACKUP( lRename, cSchema, cTable, cField, cTableBkp, aSql, aStruct, n, bStr, cNewName )
**************************

local cSql, nOrd, cName := "sl$bkp_" + cNewName + "_" + cField + "_" + cTableBkp

if lRename
   cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" RENAME COLUMN ' + cField + " TO " + cName
else
   nOrd := bStr:Eval( cName )
   if nOrd > 0
      cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" DROP COLUMN ' + cName + " CASCADE"
      aadd( aSql, cSql )
   endif
   ADD_COLUMN( cSchema, cTable, aStruct, @aSql, cName, n )
   cSql := 'UPDATE "' + cSchema + '"."' + cTable + '" SET ' + cName + ' = ' + cField
endif

aadd( aSql, cSql )
 
return aSql

**************************
static function ADD_COLUMN( cSchema, cTable, aStruct, aSql, cFieldN, n )
**************************

local cTipo  := aStruct[n,2] && Tipo do campo, minimo 1¦ letra
local nSize  := aStruct[n,3] && Tamanho do campo              
local nDecim := aStruct[n,4] && Casas decimais                
local lNNull := aStruct[n,5] && .T. indica o flag NOT NULL    
local lUniqu := aStruct[n,6] && .T. indica o flag UNIQUE      
local nPrKey := aStruct[n,7] && > 0 indica o flag PRIMARY KEY 
local xDefau := aStruct[n,8] && Expressão DEFAULT para o campo
local cSql, t

if valtype( xDefau ) != "C"
   xDefau := ""
endif

if ( t := at( "::character", xDefau ) ) > 0
   xDefau := left( xDefau, t - 1 )
endif

if ( t := at( "nextval", xDefau ) ) > 0
   xDefau := left( xDefau, t - 1 )
endif

cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable + '" ADD COLUMN ' + cFieldN

if     cTipo $ [CM]
*       cSql += " character(" + alltrim(str(nSize)) + ")"
       cSql += " varchar(" + alltrim(str(nSize)) + ")"
elseif cTipo = "N"
       cSql += " numeric(" + alltrim(str(nSize)) + iif( nDecim > 0, "," + alltrim(str(nDecim)), "" ) + ")"
elseif cTipo = "D"
       cSql += " date"
elseif cTipo = "L"
       cSql += " boolean"
endif

if !empty( xDefau ) .and. xDefau != "0" .and. xDefau != "' '"
   cSql += " DEFAULT "
   if cTipo $ [CD]
      cSql  += "'" + alltrim(xDefau) + "'" && "to_date( '" + xDefauN + "', 'YYYYMMDD' )"
   else
      cSql  += alltrim(xDefau)
   endif
endif

if lUniqu
   cSql += " UNIQUE "
endif

if lNNull
   cSql += " NOT NULL "
endif

aadd( aSql, cSql )

if valtype( nPrKey ) = "N" .and. nPrKey > 0
   aadd( aSql, 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ADD CONSTRAINT ' + cTable + "_" + SL_CONSTRAINT_PK + " PRIMARY KEY (" + cFieldN + ")" )
endif

if valtype( lUniqu ) = "L" .and. lUniqu
   aadd( aSql, 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" ADD CONSTRAINT ' + cTable + "_" + SL_CONSTRAINT_UNIQUE + "_" + cFieldN + " UNIQUE (" + cFieldN + ")" )
endif

return NIL

//--EOF--//

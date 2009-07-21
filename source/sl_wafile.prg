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
 * Mesmo procedimento da rotina acima adaptada para Ìndices
 */
FUNCTION SL_INDEXE( Index, pConn, cTagName )
      LOCAL cSql, aConn, Schema, Id, Ind

      IF Index == NIL
         RETURN .F.
      ELSE
         Index := SQLAdjustFn( ID_PREFIX + StrTran( Alltrim(Index), "*", "%" ) ) && Rossine 29/06/09 - Incluido <ID_PREFIX + >
      End

      aConn := SL_GETCONNINFO( pConn )

      IF VALTYPE( aConn ) != 'A' 
         RETURN .F.
      End
      
      Schema := aConn[SL_CONN_SCHEMA]   
      Id     := aConn[SL_CONN_SYSTEMID]
      
      /* Convetermos o nome da tabela */
      Ind := SQLParse( Index )

      /*
       * Montamos o comando SQL com base no driver atual
       */
      DO CASE
      CASE ( ID == ID_MYSQL )
         cSql := SQLPARAMS( 'SELECT indexfile FROM `\?` WHERE indexfile = ?', { SL_INDEX, Ind},Id)

      CASE ( ID == ID_POSTGRESQL )
         cSql := SQLPARAMS( 'SELECT indexfile FROM "\?"."\?" WHERE indexfile = ?', { Schema, SL_INDEX, Ind },Id)

      OTHERWISE
         RETURN .F.
      End

      IF cTagName != NIL
         cSql += " AND indextag = " + SQLStr( ID_PREFIX + SQLParse( cTagName ) )
      End

      cSql += ' LIMIT 1'

      /*
       * Executamos o SQL com esta funÁ„o q retorna um ARRAY
       * com o result de nossa query ...
       */

   RETURN Len( SQLArray( cSql,, aConn,, ID ) ) <> 00

/*
 * FUNÄ«O: TABLE( cTable )
 *         Retorna .T. caso a tabela exista no banco de dados atual
 *         vocà deve se conectadar ao DB antes de executar esta funá∆o,
 *         obviamente...
 *
 *         Caso o RDD da SQL LIB § esteja ativo o segundo parÉmetro informa
 *         a funá∆o que ela deve procurar localmente pelo arquivo solicitado
 *
 * Params: cTable   - ê o nome da tabela que vc deseja testar a existencia
 *         ForceSQL - ê um valor que faz com q indica que o arquivo deve ser
 *                    pesquisado localmente e n∆o no servidor.
 *
 *                    Se vc omitir este parÉmetro, ele ser† considerado como
 *                    .F. por padr∆o, caso o arquivo aberto atualmente com o
 *                    COMANDO USE, esteja usando o RDD para a SQL Lib OU SE
 *                    nenhum conex∆o ao bco de dados estiver aberta atÇ o
 *                    momento.
 */
FUNCTION SL_TABLE( Table, pConn )

      LOCAL cSql, aConn, Schema, Id, Tab

      IF Table == NIL
         RETURN .F.
      ELSE
         Table := SQLAdjustFn( StrTran( Alltrim(Table), "*", "%" ) )
      End

      aConn := SL_GETCONNINFO( pConn )

      IF VALTYPE( aConn ) != 'A' 
         RETURN .F.
      End
      
      Schema := aConn[SL_CONN_SCHEMA]   
      Id     := aConn[SL_CONN_SYSTEMID]
      
      /* Convetermos o nome da tabela */
      Tab := SQLParse( Table )

      /*
       * Montamos o comando SQL com base no driver atual
       */
      DO CASE
      CASE ( ID == ID_MYSQL )
         cSql := "SHOW TABLES LIKE '"+Tab+"'"

      CASE ( ID == ID_POSTGRESQL )
         cSql := "SELECT tablename FROM pg_tables WHERE schemaname = '"+Schema+"' AND tablename = '"+Tab+"' LIMIT 1"

      OTHERWISE
         RETURN .F.
      End

      /*
       * Executamos o SQL com esta funÁ„o q retorna um ARRAY
       * com o result de nossa query ...
       */

   RETURN Len( SQLArray( cSql,, aConn,, ID ) ) <> 00

/*
 * Mesmo procedimento da rotina acima adaptada para °ndices com TAGs
 */
FUNCTION SL_TAG( cTagName, Index, pConn )
      IF cTagName == NIL .or. Index == NIL THEN;
         RETURN .F.
         
      RETURN SL_INDEXE( Index, pConn, cTagName )

*#if 0
/*
 * Mesmo procedimento da rotina acima adaptada para Bancos de Dados
 */
FUNCTION SL_DATABASE( cDB, pConn )

      local cSql, Id, Res, Ind
      local lNew  := .F.
      local aConA := SL_GETCONNINFO()      && Rossine 25/06/09
      local cHost := SL_GETCONNPARAMS()[1] && Rossine 25/06/09
      local cUser := SL_GETCONNPARAMS()[2] && Rossine 25/06/09
      local cPass := SL_GETCONNPARAMS()[3] && Rossine 25/06/09
      local cDriv := SL_GETCONNPARAMS()[4] && Rossine 25/06/09
      local aConn
      
      IF cDB == NIL
         RETURN .F.
      ELSE
         cDB := StrTran( lower(Alltrim(cDB)), "*", "%" )
      End

/*
msgstop( "cHost: " + cHost + CRLF + ;
         "cUser: " + cUser + CRLF + ;
         "cPass: " + cPass + CRLF + ;
         "cDriv: " + cDriv )
*/

      IF VALTYPE( aConA ) != 'A'
         SL_CONN( cHost, , "template1", cUser, cPass, , cDriv )
         lNew  := .T.
         aConn := SL_GETCONNINFO( pConn )
      else
         aConn := aConA
      endif

      IF VALTYPE( aConn ) != 'A' 
         RETURN .F.
      End
      
**      Schema := aConn[SL_CONN_SCHEMA]   
      Id := aConn[SL_CONN_SYSTEMID]
      
      /*
       * Montamos o comando SQL com base no driver atual
       */
      DO CASE
      CASE ( ID == ID_MYSQL )
         cSql := "SHOW DATABASES LIKE '" + cDB + "'"

      CASE ( ID == ID_POSTGRESQL )
         cSql := "SELECT datname FROM pg_database WHERE datname = '" + cDB + "' ORDER BY datname"

      OTHERWISE
         if valtype( aConA ) != "U"
            SL_DISCONN()
            SL_SetConnection( aConA[SL_CONN_HANDLE] )
         endif
         RETURN .F.
      End

      /*
       * Executamos o SQL com esta funÁ„o q retorna um ARRAY
       * com o result de nossa query ...
       */
      Ind := SQLArray( cSql,, aConn,, ID )
      Res := Len( Ind ) <> 00

      if lNew  && Rossine 25/06/09
         SL_DISCONN()
      endif
   
      if valtype( aConA ) != "U"  && Rossine 25/06/09
         SL_SetConnection( aConA[SL_CONN_HANDLE] )
      endif

    RETURN Res
*#endif

/*
 * Funá∆o para retornar se determinada tabela existe dentro do banco de dados
 * 23/01/2009 - 09:12 - Rossine
 */
*****************
FUNCTION SL_FILE( cFile )
*****************
   RETURN SL_TABLE( cFile ) .or. SL_INDEXE( cFile )

**************************
function SL_CREATEDATABASE( cHost, nPort, cDb, cUser, cPwd, cDriverName, cSchema, lCreate )
**************************

   local aConn, Id, lRet, aConA := SL_GETCONNINFO(), cSql
   local lNew := .F.  && Rossine 25/06/09

   HB_SYMBOL_UNUSED( cHost )
   HB_SYMBOL_UNUSED( nPort )
   HB_SYMBOL_UNUSED( cUser )
   HB_SYMBOL_UNUSED( cPwd )
   HB_SYMBOL_UNUSED( cDriverName )
   HB_SYMBOL_UNUSED( cSchema )

   IF cDB == NIL
      RETURN .F.
   ELSE
      cDB := StrTran( lower(Alltrim(cDB)), "*", "%" )
   End

   IF VALTYPE( aConA ) != 'A'
      SL_CONN( cHost, , "template1", cUser, cPwd, , cDriverName )
      aConn := SL_GETCONNINFO()
      lNew  := .T.
   else
      aConn := aConA
   endif

   IF VALTYPE( aConn ) != 'A' 
      RETURN .F.
   End
   
   if lCreate .and. SL_DATABASE( cDB )
      msgstop( "Banco de dados j· existe !!!" )
**      SL_DISCONN( SL_GETCONN() )  Rossine 25/06/09 ???
      if valtype( aConA ) != "U"
         SL_DISCONN()
         SL_SetConnection( aConA[SL_CONN_HANDLE] )
      endif
      return .F.
   endif

   Id := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
        cSql := iif( lCreate, "create", "drop" ) + " database " + cDB

   CASE ( ID == ID_POSTGRESQL )
        cSql := iif( lCreate, "create", "drop" ) + " database " + cDB    && + " WITH OWNER = " + cUser

   OTHERWISE
**      SL_DISCONN( SL_GETCONN() )  Rossine 25/06/09 ???
      if valtype( aConA ) != "U"
         SL_DISCONN()
         SL_SetConnection( aConA[SL_CONN_HANDLE] )
      endif
      RETURN .F.
   End

   /*
    * Executamos o SQL com esta funÁ„o q retorna um ARRAY
    * com o result de nossa query ...
    */

   lRet := SL_EXECQUERYEX( cSql, aConn[1] )

**   Res := SL_DATABASE( cDB )

   if lNew  && Rossine 25/06/09
      SL_DISCONN()
   endif

   if valtype( aConA ) != "U"
      SL_SetConnection( aConA[SL_CONN_HANDLE] )
   endif

return lRet

**************************
function SL_RENAMEDATABASE( cDatabase, cNewDataBase, pConn )
**************************

local aConn, Id, lRet := .F., cSql
   
   DEFAULT cDataBase    := ""
   DEFAULT cNewDataBase := ""
   
   if empty( cDataBase ) .or. empty( cNewDataBase )
      return .F.
   endif
   
   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' 
      RETURN { }
   Endif

   DEBUG_ARGS
   
   cDataBase    := SQLAdjustFn( cDataBase )
   cNewDataBase := SQLAdjustFn( cNewDataBase )
   Id           := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        cSql := "ALTER DATABASE " + cDataBase + " RENAME TO " + cNewDataBase
        lRet := SL_EXECQUERYEX( cSql, aConn[1] )

   OTHERWISE

   Endcase

return lRet

***********************
function SL_DELETETABLE( cTableName, cSchema, pConn )  && Rossine 29/06/09
***********************

local aConn, cSequenceField, lRet  := .T., Id, cSql

   DEFAULT cSchema := iif( empty( SL_GetSchema() ), "public", SL_GetSchema() )
 
   IF cTableName == NIL
      RETURN .F.
   ELSE
      cTableName := StrTran( Alltrim(cTableName), "*", "%" )
   End

   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' 
      RETURN .F.
   End

   DEBUG_ARGS

   cSchema    := SQLAdjustFn( cSchema )
   cTableName := SQLAdjustFn( cTableName )
   Id         := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )
        if SL_INDEXE( cTableName )
           lRet := SL_DELETEINDEX( cTableName, cSchema, pConn )
        endif
        if lRet
           cSequenceField := cTableName + "_" + SL_COL_RECNO
           cSql := 'DROP SEQUENCE "' + cSchema + '"."' + cSequenceField + '" CASCADE'
           lRet := SL_EXECQUERYEX( cSql, aConn[1] )
           if lRet
              cSQL := 'DROP TABLE "' + cSchema + '"."' + cTableName + '"'
              lRet := SL_EXECQUERYEX( cSql, aConn[1] )
           endif
        endif
   OTHERWISE
        lRet := .F.
   End
   
return lRet

***********************
function SL_DELETEINDEX( cIndexname, cSchema, pConn ) && Rossine 29/06/09
***********************

local aConn, lRet  := .F., Id, cSql

   DEFAULT cSchema := iif( empty( SL_GetSchema() ), "public", SL_GetSchema() )
 
   IF cIndexname == NIL
      RETURN .F.
   ELSE
      cIndexname := ID_PREFIX + StrTran( Alltrim(cIndexname), "*", "%" ) && Rossine 30/06/09 - Incluido <ID_PREFIX + >
   End

   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' 
      RETURN .F.
   End

   DEBUG_ARGS

   cSchema    := SQLAdjustFn( cSchema )
   cIndexName := SQLAdjustFn( cIndexName )
   Id         := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        cSql := "DELETE FROM " + cSchema + "." + SL_INDEX + " where " + ;
                                           '"indexfile" = ' + "'" + cIndexname + "'"
        lRet := SL_EXECQUERYEX( cSql, aConn[1] )

   OTHERWISE
        lRet := .F.
   End
   
return lRet

/*
      1             2       3     4     5       6      7         8                                   9
    cField,      cType, nSize, nDec, bNull, UNIQUE, PRIMARY_KEY, xDef,                             hbType
{ { "sequ"        ,"N",    15,    0,   .T.,    .F.,    1,       "0"                                 ,7},
  { "fld1"        ,"N",    15,    3,   .T.,    .T.,    0,       "0"                                 ,7},
  { "fld2"        ,"C",    30,    0,   .T.,    .T.,    0,       "' '::character varying"            ,1},
  { "fld3"        ,"D",     8,    0,   .T.,    .T.,    0,       NIL                                 ,3},
  { "fld4"        ,"L",     1,    0,   .T.,    .T.,    0,       "false"                             ,2},
  { "sl_deleted"  ,"C",     1,    0,   .F.,    .F.,    0,       "' '::character varying"            ,1},
  { "sl_rowid"    ,"N",    15,    0,   .T.,    .T.,    0,       "nextval('test_sl_rowid'::regclass)",7}
}
*/
********************
function SL_DBSTRUCT( cTable, pConn, lExtend ) && Rossine 30/06/09
********************

local aConn, Id, aStruct := { }, aStr := { }, n
 
   DEFAULT lExtend := .F.
   
   IF cTable == NIL
      RETURN aStruct
   ELSE
      cTable := StrTran( Alltrim(cTable), "*", "%" )
   Endif

   cTable := SQLAdjustFn( cTable )
   aConn  := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' 
      RETURN aStruct
   Endif

   DEBUG_ARGS

   Id := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        aStruct := aConn[10]:TableStruct( cTable )

        if !lExtend
           for n = 1 to len(aStruct)
               if aStruct[n,1] != SL_COL_RECNO .and. aStruct[n,1] != SR_COL_RECNO .and. aStruct[n,1] != SL_COL_DELETED
                  aadd( aStr, { aStruct[n,1], aStruct[n,2], aStruct[n,3], aStruct[n,4], ;
                                 aStruct[n,5], aStruct[n,6], aStruct[n,7] } ) 
               endif
           next
        else
           aStr := aStruct
        endif
        
**msgstop( SL_ToString( aStr ), "Estrutura" )

   OTHERWISE

   Endcase

return aStr

***********************
function SL_RENAMETABLE( cOld, cNew, cSchema, pConn )
***********************

local aConn, Id, lRet := .F., cSql
   
   DEFAULT cSchema := iif( empty( SL_GetSchema() ), "public", SL_GetSchema() )

   IF valtype( cOld ) != "C" .or. valtype( cNew ) != "C"
      RETURN .F.
   ELSE
      cOld := StrTran( Alltrim(cOld), "*", "%" )
      cNew := StrTran( Alltrim(cNew), "*", "%" )
   Endif

   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' 
      RETURN .F.
   Endif

   DEBUG_ARGS
   
   cSchema := SQLAdjustFn( cSchema )
   cNew    := SQLAdjustFn( cNew )
   cOld    := SQLAdjustFn( cOld )
   Id      := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        if SL_TABLE( cOld ) .and. !SL_TABLE( cNew )
           cSql := 'ALTER TABLE "' + cSchema + '"."' + cOld + '" RENAME TO "' + cNew + '"'
           lRet := SL_EXECQUERYEX( cSql, aConn[1] )
        endif
        
   OTHERWISE

   Endcase

return lRet

* se eu especificar somente o schema, ent„o ele varrer· todas as tabelas daquele schema, apagando todas as colunas de backup
* SQL DELETE BACKUP CONNECTION <pConn> SCHEMA "001" TABLE "customer"

************************
function SL_DELETEBACKUP( pConn, cSchema, cTable )
************************

   LOCAL cSql, aConn, Id, lRet := .T., aStruct, n, t, cField, aTables := { }, lUniqu, nPrKey
   
   DEFAULT cTable  := ""
   DEFAULT cSchema := iif( empty( SL_GetSchema() ), "public", SL_GetSchema() )

   IF cTable == NIL
      RETURN .F.
   ELSE
      cTable := StrTran( Alltrim(cTable), "*", "%" )
   End

   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' 
      RETURN .F.
   End

   DEBUG_ARGS

   Id      := aConn[SL_CONN_SYSTEMID]
   cSchema := SQLAdjustFn( cSchema )

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )

   CASE ( ID == ID_POSTGRESQL )

        if empty( cTable )
           aTables := aConn[SL_CONN_POINTER]:ListTables()
        else
           aadd( aTables, cTable )
        endif

        for t = 1 to len(aTables)
            cTable := aTables[t]
            if left( cTable, 3 ) != "sl$"
               aStruct := SL_DBSTRUCT( cTable, , .T. )
               for n := 1 TO len( aStruct )
                   cField := SQLAdjustFn( aStruct[n,1] ) && Nome do campo
                   lUniqu := aStruct[n,6]
                   nPrKey := aStruct[n,7]
                   if left( cField, 7 ) == "sl$bkp_" .and. cField != SL_COL_RECNO .and. cField != SR_COL_RECNO .and. cField != SL_COL_DELETED
                      if lRet .and. valtype( nPrKey ) = "N" .and. nPrKey > 0
                         cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" DROP CONSTRAINT ' + cTable + "_" + SL_CONSTRAINT_PK + " PRIMARY KEY (" + cField + ")"
                         lRet := SL_EXECQUERYEX( cSql, aConn[1] )
                      endif
                      if lRet .and. valtype( lUniqu ) = "L" .and. lUniqu
                         cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" DROP CONSTRAINT ' + cTable + "_" + SL_CONSTRAINT_UNIQUE + "_" + cField + " UNIQUE (" + cField + ")"
                         lRet := SL_EXECQUERYEX( cSql, aConn[1] )
                      endif
                      if lRet
                         cSql := 'ALTER TABLE "' + cSchema + '"."' + cTable  + '" DROP COLUMN ' + cField + " CASCADE"
                         lRet := SL_EXECQUERYEX( cSql, aConn[1] )
                         if !lRet
                            msgstop( "N„o foi possÌvel apagar a Tabela: [" + cTable + "] !!!", "Erro" )
                            return .F.
                         endif
                      else
                         msgstop( "N„o foi possÌvel apagar a <PRIMARY KEY> da Tabela : [" + cTable + "] - Campo: [" + cField + "] !!!", "Erro" )
                         return .F.
                      endif
                   endif
               next
            endif
        next

   OTHERWISE
      RETURN .F.
   End

return lRet

************************
function SL_CREATESCHEMA( cSchema, pConn )
************************

local aConn, Id, lRet := .F., cSql
   
   DEFAULT cSchema := ""

   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' .or. empty( cSchema )
      RETURN .F.
   Endif

   DEBUG_ARGS
   
   cSchema := SQLAdjustFn( cSchema )
   Id      := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        cSql := "CREATE SCHEMA " + cSchema
        lRet := SL_EXECQUERYEX( cSql, aConn[1] )

   OTHERWISE

   Endcase

return lRet

************************
function SL_RENAMESCHEMA( cSchema, cNewSchema, pConn )
************************

local aConn, Id, lRet := .F., cSql
   
   DEFAULT cSchema    := ""
   DEFAULT cNewSchema := ""
   
   if empty( cSchema ) .or. empty( cNewSchema )
      return .F.
   endif
   
   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' 
      RETURN { }
   Endif

   DEBUG_ARGS
   
   cSchema    := SQLAdjustFn( cSchema )
   cNewSchema := SQLAdjustFn( cNewSchema )
   Id         := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        cSql := "ALTER SCHEMA " + cSchema + " RENAME TO " + cNewSchema
        lRet := SL_EXECQUERYEX( cSql, aConn[1] )

   OTHERWISE

   Endcase

return lRet

************************
function SL_DELETESCHEMA( cSchema, pConn )
************************

local aConn, Id, lRet := .F., cSql
   
   DEFAULT cSchema := ""
   
   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' .or. empty( cSchema )
      RETURN { }
   Endif

   DEBUG_ARGS
   
   cSchema := SQLAdjustFn( cSchema )
   Id      := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        cSql := "DROP SCHEMA " + cSchema + " CASCADE"
        lRet := SL_EXECQUERYEX( cSql, aConn[1] )

   OTHERWISE

   Endcase

return lRet

******************
function SL_SCHEMA( cSchema, cDataName, pConn )
******************

DEFAULT cSchema   := ""
DEFAULT cDataName := ""

return iif( ascan( SL_LISTSCHEMA( cSchema, cDataName, pConn ), { |aSchema| aSchema[2] == cSchema } ) > 0, .T., .F. )

**********************
function SL_LISTSCHEMA( cSchema, cDataName, pConn )
**********************

local aConn, Id, aRet := { }, cSql, aTmp, n
   
   DEFAULT cSchema   := ""
   DEFAULT cDataName := ""
   
   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( aConn ) != 'A' 
      RETURN { }
   Endif

   DEBUG_ARGS
   
   cSchema   := SQLAdjustFn( cSchema )
   cDataName := SQLAdjustFn( cDataName )
   Id        := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        cSql := "select a.datname, b.nspname from pg_database a, pg_namespace b" + CRLF + ;
                " where nspname not like '%pg_%' and nspname not like '%information%'"
        aTmp := SQLArray( cSql,, aConn,, ID )

        for n = 1 to len(aTmp)
            if empty( cDataName ) .or. aTmp[n,1] == cDataName
               if empty( cSchema ) .or. aTmp[n,2] == cSchema
                  aadd( aRet, aTmp[n] )
               endif
            endif
        next
        
   OTHERWISE

   Endcase

return aRet

***********************
function SL_EXECQUERYEX( cQuery, pConn, cDbf, lRecno )
***********************

local aConn, Id, lRet := .F., xRes, aStruct, cTipo, nTipo, aNewStr, n, t, aFld
   
   DEFAULT cQuery := ""
   DEFAULT cDbf   := ""
   DEFAULT lRecno := .F.
   
   aConn := SL_GETCONNINFO( pConn )

   IF VALTYPE( cQuery ) != "C"
      RETURN .F.
   Endif

   DEBUG_ARGS
   
   Id := aConn[SL_CONN_SYSTEMID]

   if valtype( cDbf ) = "C"
      xRes := PQexec( aConn[10]:pDB, cQuery )
   endif

   DO CASE
   CASE ( ID == ID_MYSQL )
*        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        if valtype( cDbf ) = "C"
           if empty( cDbf )
              if PQresultstatus( xRes ) = PGRES_COMMAND_OK
                 lRet := .T.
              else
*                 msgstop( cQuery + CRLF + CRLF + PQresultErrormessage( xRes ), "Erro na sentenÁa" )
                 msgstop( SL_ToString( cQuery + CRLF + CRLF + PQresultErrormessage( xRes ),.T.,,, "DUMP.TXT", .T. ) )
              endif
              PQclear( xRes )
           else
**              xRes := PQexec( aConn[10]:pDB, 'copy ( ' + cQuery + ' ) TO E' + "'\\Temp\\tempor.txt' with delimiter '|'" )
**              if PQresultstatus( xRes ) = PGRES_COMMAND_OK
              if PQresultStatus( xRes) == PGRES_TUPLES_OK
                 aStruct := PQmetadata( xRes )

**msgstop( SL_ToString( aStruct,.T.,,, "DUMP.TXT", .T. ) )

                 aNewStr := { }
                 for n = 1 TO len(aStruct)
                     if lRecno .or. ( lower(aStruct[n,1]) != SL_COL_RECNO .and. ;
                                      lower(aStruct[n,1]) != SR_COL_RECNO .and. ;
                                      lower(aStruct[n,1]) != SL_COL_DELETED )
                        cTipo := "C"
                        nTipo := HB_FT_STRING
                        do case
                           case upper(aStruct[ n, DBS_TYPE ]) $ [CHAR,CHARACTER,VARCHAR,TEXT,TIMESTAMP,TIME]
                                cTipo := "C"
                                nTipo := HB_FT_STRING
                                
                           case upper(aStruct[ n, DBS_TYPE ]) $ [MONEY,SMALLINT,INTEGER,BIGINT,DECIMAL,NUMERIC,REAL,DOUBLE,SERIAL,BIGSERIAL]
                                cTipo := "N"
                                nTipo := HB_FT_DOUBLE
                        
                           case upper(aStruct[ n, DBS_TYPE ]) $ [LOGICAL,BOOLEAN]
                                cTipo := "L"
                                nTipo := HB_FT_LOGICAL
                        
                           case upper(aStruct[ n, DBS_TYPE ]) $ [DATE]
                                cTipo := "D"
                                nTipo := HB_FT_DATE
                        
                           case at( "BLOB", upper(aStruct[ n, DBS_TYPE ]) ) > 0
                                cTipo := "M"
                                nTipo := HB_FT_MEMO
                        endcase
                        
                        aadd( aNewStr, { aStruct[ n, DBS_NAME ], cTipo, SL_GETFIELDSIZE( nTipo, aStruct[ n, DBS_LEN ] ), aStruct[ n, DBS_DEC ] } )
                     endif
                 next   

**msgstop( SL_ToString( aNewStr,.T.,,, "DUMP.TXT", .T. ) )

/*
  Vailton - Verificar se È possÌvel fazer a rotina abaixo diretamente dentro do postgres
*/
                 dbcreate( cDbf, aNewStr, "DBFCDX" ) &&, .T., "ARQTMP" )
**                 append from "\temp\tempor.txt" DELIMITED with PIPE
**                 ferase( "\temp\tempor.txt" )

                 aFld := SQLArray( cQuery,, aConn,, ID )
**msgstop( SL_ToString( aFLD,.T.,,.t., "DUMP.TXT", .T. ) )
                 lRet := len( aFld ) <> 00
                 if lRet
                    USE (cDbf) ALIAS "ARQTMP" via "DBFCDX" exclusive new
                    for n = 1 to len(aFld)
                        ARQTMP->( dbappend() )
                        for t = 1 to len(aFld[n])
                            if     ARQTMP->( fieldtype( t ) ) = "N"
                                   ARQTMP->( fieldput( t, val(aFld[n,t] ) ) )
                            elseif ARQTMP->( fieldtype( t ) ) = "D"
                                   ARQTMP->( fieldput( t, stod(aFld[n,t]) ) )
                            elseif ARQTMP->( fieldtype( t ) ) = "L"
                                   ARQTMP->( fieldput( t, iif( upper(aFld[n,t]) $ [TRUE,.T.,T,Y,YES,1], .T., .F. ) ) )
                            else
                               ARQTMP->( fieldput( t, aFld[n,t] ) )
                            endif
                        next
                    next
                    ARQTMP->( dbclosearea() )
                 endif
              else
                 msgstop( cQuery + CRLF + CRLF + PQresultErrormessage( xRes ), "Erro na sentenÁa" )
              endif

              PQclear( xRes )
           endif
        else
           cDbf := SQLArray( cQuery,, aConn,, ID )
           lRet := len( cDbf ) <> 00
        endif
        
   OTHERWISE

   Endcase

RETURN lRet

//--EOF--//

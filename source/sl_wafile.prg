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
      LOCAL cSql, aConn, Schema, Ind, Id, Result

      IF Index == NIL
         RETURN .F.
      ELSE
         Index := ID_PREFIX + StrTran( Alltrim(Index), "*", "%" ) && Rossine 29/06/09 - Incluido <ID_PREFIX + >
      End

      IF pConn == NIL
         aConn := SL_GETCONNINFO()
      ELSE
         aConn := SL_GETCONNINFO( pConn )
      End

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
      Ind    := SQLArray( cSql,, aConn,, ID )
      Result := Len( Ind ) <> 00
   RETURN Result

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
      LOCAL cSql, aConn, Schema, Tab, Id, Result

      IF Table == NIL
         RETURN .F.
      ELSE
         Table := StrTran( Alltrim(Table), "*", "%" )
      End

      IF pConn == NIL
         aConn := SL_GETCONNINFO()
      ELSE
         aConn := SL_GETCONNINFO( pConn )
      End

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
      Tab    := SQLArray( cSql,, aConn,, ID )
      Result := Len( Tab ) <> 00
   RETURN Result

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

      IF pConn == NIL
         aConn := SL_GETCONNINFO()
      ELSE
         aConn := SL_GETCONNINFO( pConn )
      End

**msgstop( "cHost: " + cHost + CRLF + ;
**         "cUser: " + cUser + CRLF + ;
**         "cPass: " + cPass + CRLF + ;
**         "cDriv: " + cDriv )

      IF VALTYPE( aConA ) != 'A'
         SL_CONN( cHost, , "template1", cUser, cPass, , cDriv )
         aConn := SL_GETCONNINFO()
         lNew  := .T.
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
   RETURN SL_TABLE( cFile ) .OR. SL_INDEXE( cFile )

/************************
* TODO: REVISTAR, PQ NAO TESTEI ESTA SITUA«√O AINDA!!! PODE CONTER BUG .........
function SL_DELETETABLE( cTableName, cSchema )
************************
   LOCAL aInfo := SL_GETCONNINFO()
   RETURN HB_EXECFROMARRAY( { FSL_DELETETABLE( aInfo[ SL_CONN_SYSTEMID ] ), aInfo[ SL_CONN_HANDLE ], cTableName, cSchema } )
/**/
********************
function SL_CREATEDB( cHost, nPort, cDb, cUser, cPwd, cDriverName, cSchema, lCreate )
********************

   local aConn, Id, Res, aConA := SL_GETCONNINFO(), cSql
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
      IF VALTYPE( aConn ) != 'A' 
         RETURN .F.
      End
   else
      aConn := aConA
   endif
   
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

   SQLArray( cSql,, aConn,, ID )

   Res := SL_DATABASE( cDB )

   if lNew  && Rossine 25/06/09
      SL_DISCONN()
   endif

   if valtype( aConA ) != "U"
      SL_SetConnection( aConA[SL_CONN_HANDLE] )
   endif

return Res

***********************
function SL_DELETETABLE( cTableName, cSchema, pConn )  && Rossine 29/06/09
***********************

local aConn, cSequenceField, nConn, lError, lRet  := .F., Id

   DEFAULT cSchema := "public"
 
   DEBUG_ARGS

   IF cTableName == NIL
      RETURN .F.
   ELSE
      cTableName := StrTran( Alltrim(cTableName), "*", "%" )
   End

   cTableName := SQLAdjustFn( cTableName )

   IF pConn == NIL
      aConn := SL_GETCONNINFO()
   ELSE
      aConn := SL_GETCONNINFO( pConn )
   End

   IF VALTYPE( aConn ) != 'A' 
      RETURN .F.
   End

   nConn := aConn[ SL_CONN_POINTER ]
   Id    := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        if SL_DELETEINDEX( cTableName, cSchema, pConn )
           cSequenceField := cTableName + "_" + SL_COL_RECNO
           cSql   := 'DROP SEQUENCE "' + cSchema + '"."' + cSequenceField + '" CASCADE'
           Ind    := SQLArray( cSql,, aConn,, ID )
           lError := Len( Ind ) <> 00
           if !lError
              cSQL   := 'DROP TABLE "' + cSchema + '"."' + cTableName + '"'
              Ind    := SQLArray( cSql,, aConn,, ID )
              lError := Len( Ind ) <> 00
              if !lError
                 lRet := .T.
              endif
           endif
        endif
   OTHERWISE
      RETURN .F.
   End
   
return lRet

***********************
function SL_DELETEINDEX( cIndexname, cSchema, pConn ) && Rossine 29/06/09
***********************

local aConn, lError, nConn, lRet  := .F., Id, Ind

   DEFAULT cSchema := "public"
 
   DEBUG_ARGS

   IF cIndexname == NIL
      RETURN .F.
   ELSE
      cIndexname := StrTran( Alltrim(cIndexname), "*", "%" )
   End

   cIndexName := SQLAdjustFn( cIndexName )

   IF pConn == NIL
      aConn := SL_GETCONNINFO()
   ELSE
      aConn := SL_GETCONNINFO( pConn )
   End

   IF VALTYPE( aConn ) != 'A' 
      RETURN .F.
   End

   nConn := aConn[ SL_CONN_POINTER ]
   Id    := aConn[SL_CONN_SYSTEMID]

   /*
    * Montamos o comando SQL com base no driver atual
    */
   DO CASE
   CASE ( ID == ID_MYSQL )
        cSql := "??"

   CASE ( ID == ID_POSTGRESQL )

        cSql  := "DELETE FROM " + cSchema + "." + SL_INDEX + " where " + ;
                                            '"indexfile" = ' + "'" + ID_PREFIX + cIndexname + "'"
        Ind   := SQLArray( cSql,, aConn,, ID )
        lRet  := Len( Ind ) = 00

   OTHERWISE
      RETURN .F.
   End
   
return lRet

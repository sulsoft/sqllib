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
 * Retorna um ARRAY com as informações solicitadas do DB atual
 */
** montar um exemplo para esta função. Separar isto num PRG a parte.
FUNCTION SQLGetDBInfo( nDBInfo, pConn, cFiltro1, cFiltro2 )
      LOCAL Result, SQL, Conn, Schema, Tab, ID, Res
      LOCAL Block
      
      default nDBInfo to DBI_GETVERSION
      default SQL     to ""

      DEBUG "SQLGetDBInfo( ", nDBInfo,", ",pConn, ", ",cFiltro1, ", ",cFiltro2, " )"
      
      if cFiltro1 == NIL
         cFiltro1 := ""
      else
         cFiltro1 := StrTran( cFiltro1, '*', '%' )
      end

      if cFiltro2 == NIL
         cFiltro2 := ""
      else
         cFiltro2 := StrTran( cFiltro2, '*', '%' )
      end

   /* Não especificou o HANDLE da conexão */
      IF pConn == NIL
         Conn   := SL_GETCONNINFO()
         Schema := "public" // SQLSCHEMA()
      ELSE
         Conn   := SL_GETCONNINFO( pConn )
         Schema := "public" // SQLSCHEMA()   
      End
      
      IF Valtype( Conn ) == NIL 
         RETURN nil
      End

      Id := Conn[SL_CONN_SYSTEMID]

      /*
       * Montamos o comando SQL com base no driver atual
       */
      DO CASE

      /*
       * Neste caso, ele quer o nome e a versão do servidor atualmente em uso
       */
      CASE ( nDBInfo == DBI_GETVERSION )

            DO CASE
            CASE ( ID == ID_MYSQL )
               SQL := "select concat( 'MySQL ', version() )"

            CASE ( ID == ID_POSTGRESQL )
               SQL := "select version()"
               Block := {|aRecno,nPos| nPos := Rat( ' on ', aRecno[1] ),;
                                       aRecno[1] := iif( nPos == 00, aRecno[1], SUBSTR( aRecno[1], 01, nPos-1 )),;
                                       AADD( Result, Trim( aRecno[1]) )}

            End          

      /*
       * Neste caso, ele quer os nomes de todas as tabelas
       */
      CASE ( nDBInfo == DBI_GETALLTABLES )

            cFiltro1 := StrTran( cFiltro1, '*', '%' )

            DO CASE
            CASE ( ID == ID_MYSQL )
               SQL := "SHOW TABLES " + iif( Empty(cFiltro1), '', "LIKE '"+cFiltro1+"'" )

            CASE ( ID == ID_POSTGRESQL )
               if Empty(cFiltro1)
                  SQL := "SELECT tablename FROM pg_tables WHERE schemaname = '"+Schema+"'"
               else
                  SQL := "SELECT tablename FROM pg_tables WHERE schemaname = '"+Schema+"' AND tablename LIKE '"+cFiltro1+"'"// LIMIT 1"
               end
            End

      /*
       * Neste outro caso, ele quer os nomes dos DBs criados neste servidor
       */
      CASE ( nDBInfo == DBI_GETALLDBS )

            cFiltro1 := StrTran( cFiltro1, '*', '%' )

            DO CASE
            CASE ( ID == ID_MYSQL )
               SQL := "SHOW DATABASES " + iif( Empty(cFiltro1), '', "LIKE '"+cFiltro1+"'" )

            CASE ( ID == ID_POSTGRESQL )
               SQL := "SELECT datname FROM pg_database "+ iif( Empty(cFiltro1), '', "WHERE datname LIKE '"+cFiltro1+"'" )+" ORDER BY datname"
            End

      /*
       * Puxa todos os indices com nome igual ao passado
       */
      CASE ( nDBInfo == DBI_GETALLINDEXES )

            cFiltro1 := StrTran( cFiltro1, '*', '%' )

            DO CASE
            CASE ( ID == ID_MYSQL )
               SQL := 'SELECT indexfile FROM `' + SL_INDEX + '` '

            CASE ( ID == ID_POSTGRESQL )
               SQL := 'SELECT indexfile FROM "' + SL_INDEX + '" '
            End

            IF Empty( cFiltro1 ) THEN;
               SQL += "WHERE indexfile = '" + cFiltro1 + "'"// LIMIT 1"

      /*
       * Puxa uma listagem de todos os usuarios cadastrados no banco
       */
      CASE ( nDBInfo == DBI_GETALLCONNUSERS )

            DO CASE
            CASE ( ID == ID_MYSQL )
               SQL   := 'SHOW FULL PROCESSLIST'
               Block := {|aRecno,cItem, nPos, nPorta| ;
                                              cItem  := aRecno[2] + "@" + aRecno[3],;
                                              nPos   := Rat( ':', cItem ),;
                                              nPorta := iif( nPos == 00, '0', SUBSTR( cItem, nPos+1 )),;
                                              cItem  := iif( nPos == 00, cItem, SUBSTR( cItem, 01, nPos-1 )),;
                                              AADD( Result, { cItem, aRecno[4], nPorta, aRecno[8] });
                        }

            CASE ( ID == ID_POSTGRESQL )
             * SQL   := "SELECT DISTINCT usename || '@' || client_addr::text AS username FROM pg_stat_activity ORDER BY username"
             * SQL   := "SELECT usename || '@' || client_addr::text AS username FROM pg_stat_activity ORDER BY username"
               SQL   := "select usename || '@' || client_addr::text AS username, datname, client_port, current_query from pg_stat_activity ORDER BY username"
               Block := {|aRecno,nPos| nPos := Rat( '/', aRecno[1] ),;
                                       aRecno[1] := iif( nPos == 00, aRecno[1], SUBSTR( aRecno[1], 01, nPos-1 )),;
                                       AADD( Result, aRecno )}
            End

      /*
       * Puxa uma listagem de todos os usuarios cadastrados no banco
       */
      CASE ( nDBInfo == DBI_GETALLUSERS )
            DO CASE
            CASE ( ID == ID_MYSQL )
               SQL := 'SELECT DISTINCT user FROM mysql.user;'

            CASE ( ID == ID_POSTGRESQL )
               SQL := "SELECT DISTINCT usename FROM pg_user ORDER BY usename"
            End

      /*
       * Puxa o NÚMERO representativo deste driver
       */
      CASE ( nDBInfo == DBI_GETSYSTEMID )
            RETURN ID

      /*
       * Puxa o NOME representativo deste driver
       */
      CASE ( nDBInfo == DBI_GETSYSTEMIDSTR )

            DO CASE
            CASE ( ID == ID_MYSQL )
               SQL := 'MYSQL'

            CASE ( ID == ID_POSTGRESQL )
               SQL := "PGSQL"
            End


      End

      /*
       * Executamos o SQL com esta função q retorna um ARRAY
       * com o result de nossa query ...
       */
      Result := {}

      IF !Empty( SQL )
         Tab := SQLArray( SQL,, Conn )
      ELSE
         Tab := {}
      End
      
      IF Tab == NIL THEN;
         Tab := {}

      DO CASE
      CASE ( nDBInfo == DBI_GETVERSION )

            IF !Empty( Tab )
               RETURN Tab[1][1]
            ELSE
               RETURN ""
            End

      OTHERWISE
            FOR Res := 1 TO Len( Tab )
                IF ( Block == NIL )
                   AADD( Result, Tab[Res][1] )
                ELSE
                   Block:Eval( Tab[Res] )
                End
            End
      End      
   RETURN Result

/*
 * Retorna um ARRAY com os nomes das tabelas existentes no DB atual
 */
FUNCTION SQLGetTables( pConn, cFiltro )
   RETURN SQLGetDBInfo( DBI_GETALLTABLES, pConn, SQLParse(cFiltro) )

/*
 * Retorna um ARRAY com os nomes dos DBs existentes na conexão atual
 */
FUNCTION SQLGetDBs( pConn, cFiltro )
   RETURN SQLGetDBInfo( DBI_GETALLDBS, pConn, SQLParse(cFiltro) )

/*
 * Retorna um ARRAY com os nomes dos indices existentes na conexão atual
 */
FUNCTION SQLGetIndexes( pConn, cFiltro )
   RETURN SQLGetDBInfo( DBI_GETALLINDEXES, pConn, SQLParse(cFiltro) )

/*
 * Retorna um ARRAY com os nomes os usuarios atuais do servidor
 */
FUNCTION SQLGetUsers( pConn )
   RETURN SQLGetDBInfo( DBI_GETALLUSERS, pConn )

/*
 * Retorna um ARRAY com os nomes dos indices existentes na conexão atual
 */
FUNCTION SQLGetConnectedUsers( pConn )
   RETURN SQLGetDBInfo( DBI_GETALLCONNUSERS, pConn )

/*
 * SQLServerVersionNum( pConn ) --> Server Version as numeric value
 * 27/04/2009 - 15:26:53
 */
FUNCTION SQLServerVersionNum( pConn, nSysID )
	LOCAL Temp
	
   IF pConn == NIL
      pConn  := SL_GETCONNINFO()      
   ELSE
      pConn  := SL_GETCONNINFO( pConn )
   End
   IF pConn == NIL 
      RETURN nil
   End
   
   IF nSysID == NIL 
      nSysID := pConn[SL_CONN_SYSTEMID]
   End
   
	IF ( nSysID == ID_MYSQL ) .OR. ( nSysID == ID_POSTGRESQL )
		Temp := SQLArray("select version()",, pConn,, nSysID)
		IF ValType( Temp ) == "A" .AND. Len( Temp ) == 1
		  Temp := Temp[1,1]
		  Temp := Substr( Temp, At( " ", Temp ) + 1 )
		  
		  IF ( nSysID == ID_POSTGRESQL )
		  		// Sample: PostgreSQL 8.2.5
		  		Temp := Substr( Temp, 1, At( " ", Temp ) - 1 )
		  End
		  RETURN VAL( substr(Temp,1,1)+'0'+substr(Temp,3,1)+'0'+substr(Temp,5,1) )
		End
	End
	RETURN 0
	
/*
 * Retorna a versao do servidor SQL no formato string
 */
FUNCTION SQLServerVersion(pConn)
   RETURN SQLGetDBInfo( DBI_GETVERSION, pConn )

FUNCTION SQLParse( cFileName, nParStyle )
   HB_SYMBOL_UNUSED( nParStyle )
   RETURN cFileName

FUNCTION SQLNTrim( nNumber )
   RETURN Alltrim( Str( nNumber ))

FUNCTION SQLSTR(x)
   IF VALTYPE(x) == 'L' 
      x := iif( x, "T", "F" )
   End
   RETURN "'" + x + "'"

/*
 * Converte o valor passado como argumento para uma string
 */    
FUNCTION ANY2SQL( uField, nLen, nDec, lFormat, lMaskText, cType )
      LOCAL l,t := cType

      DEBUG uField, nLen, nDec, lFormat, lMaskText, cType
      DEBUG l,t, VALTYPE( uField )

      IF cType == NIL THEN;
         t := VALTYPE( uField )

      DEFAULT nLen      TO 00
      DEFAULT nDec      TO 00
      DEFAULT lFormat   TO TRUE
      DEFAULT lMaskText TO TRUE

      DO CASE
      CASE ( t == 'C' )

           /* Testa o tamanho da STRING */
           IF !( nLen == 00 )
              l := LEN( uField )

              /* Passou do tamanho? */
              IF ( l > nLen ) THEN;
                 uField := LEFT( uField, nLen )
           End

           ** ajustar isto
//           IF ( lMaskText ) THEN;
//              uField := MYSQL_ESCAPE_STRING(uField)

           IF (lFormat) THEN;
              uField := "'" + uField + "'"

           RETURN uField

      CASE ( t == 'M' )

//           IF ( lMaskText ) THEN;
//              uField := MYSQL_ESCAPE_STRING(uField)

           IF (lFormat) THEN;
              uField := "'" + uField + "'"
           RETURN uField

      CASE ( t == "N" )
           /* Testa o tamanho da STRING */
           IF ( nLen == 00 )
              uField := STR( uField )

           ELSEIF ( nDec == 00 )
              uField := STR( uField, nLen )

           ELSEIF ( True )
              uField := STR( uField, nLen, nDec )

           End

           RETURN RTrim(uField)

      CASE ( t == "D" )
           IF (lFormat)
              //uField := DTOS( uField ) - 18/11/2005 - 20:20hs
              uField := "'" +;
                        STRZERO( YEAR( uField ), 4 ) + '-' +;
                        STRZERO(MONTH( uField ), 2 ) + '-' +;
                        STRZERO(  DAY( uField ), 2 ) + "'"
           ELSE
              uField := STRZERO( YEAR( uField ), 4 ) + '-' +;
                        STRZERO(MONTH( uField ), 2 ) + '-' +;
                        STRZERO(  DAY( uField ), 2 )
           End
           RETURN uField

      CASE ( t == "L" )
           RETURN IIF( uField, "1", "0" )
      END
      RETURN nil

#ifndef FWVERSION
   
   function msgyesno( msg, tit )   
   if tit != nil
      tit := cvaltochar(tit)
   else
      tit := procname(1) + "(" + alltrim( str( procline(1) )) + ")"
   end            
   return SQLDEBUG( StrTRan( cvaltochar(msg), ';', chr(10)), tit, 0x00000004 ) == 6

   function msgstop(msg,tit)
   if tit != nil
   else
      tit := procname(1) + "(" + alltrim( str( procline(1) )) + ")"
   end         
   return SQLDEBUG( StrTRan( cvaltochar(msg), ';', chr(10)), cvaltochar(tit), 0x00000030 )

   function msgerror(msg,tit)
   if tit != nil
   else
      tit := procname(1) + "(" + alltrim( str( procline(1) )) + ")"
   end         
   return SQLDEBUG( StrTRan( cvaltochar(msg), ';', chr(10)), cvaltochar(tit), 0x00000010 )
   
   function msginfo(msg,tit)
   if tit != nil
      tit := cvaltochar( tit )
   else
      tit := procname(1) + "(" + alltrim( str( procline(1) )) + ")"
   end         
   return SQLDEBUG( StrTRan( cvaltochar(msg), ';', chr(10)), tit, 0x00000020 )
   /*
    * Para o caso dele nao usar FW e/ou estiver testando em modo console
    * Vailton @ 11/11/2008 - 21:58:53 
    */
   function VerArray( aArray )
   local x := HB_VALTOSTR( aArray )   
      memoedit(x)
   return NIL
   
   function cValToChar( uVal )
      local cType := ValType( uVal )
      do case
         case cType == "C" .or. cType == "M"
              return uVal
   
         case cType == "D"
              return DToC( uVal )
   
         case cType == "L"
              return If( uVal, ".T.", ".F." )
   
         case cType == "N"
              return AllTrim( Str( uVal ) )
   
         case cType == "B"
              return "{|| ... }"   

         case cType == "U"
              return "NIL"
   
         case cType == "A"
              uVal := HB_VALTOSTR( uVal )
              
              IF Left( uVal, 2 ) == 'E"' .and. At( "\r\n", uVal )<>0
                 uVal := StrTran( Substr( uVal, 3 ), '\r\n', Chr(10) )
              End
              return uVal
   
         case cType == "O"
              uVal := HB_VALTOSTR( uVal )
              
              IF Left( uVal, 2 ) == 'E"' .and. At( "\r\n", uVal )<>0
                 uVal := StrTran( Substr( uVal, 3 ), '\r\n', Chr(10) )
              End
              return uVal
   
         otherwise
              return ""
      endcase
      /**/
   
   return nil
#endif

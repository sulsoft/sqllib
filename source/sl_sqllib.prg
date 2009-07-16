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
#define MAX_CONN_COUNT           429496729

ANNOUNCE SQLLIB

* Handle atual para o SQLSetConn
static saConnections   := {}		/* All stablished connections */

static snConnHandle    := 00		/* Current connection ID */
static saConnInfo      := NIL		/* Current Connection */
static s_aMyLocks      := {}     && Rossine 30/10/08

/* Controle de Conexoes */
static nConnCount      := 0      /* between 1 and MAX_CONN_COUNT */  

#IfnDef __XHARBOUR__
 THREAD static s_nPacketSize := 50
 THREAD static s_cSysSchema  := ''
 THREAD static s_cSchema     := 'public'
 THREAD static s_cQuery      := ''
 THREAD static s_nConn
 THREAD static s_cHost       := ""
 THREAD static s_cUser       := ""
 THREAD static s_cPwd        := ""
 THREAD static s_cDriverName := ""
#else
 static s_nPacketSize := 50
 static s_cSysSchema  := ''
 static s_cSchema     := 'public'
 static s_cQuery      := ''
 static s_nConn
 static s_cHost       := ""
 static s_cUser       := ""
 static s_cPwd        := ""
 static s_cDriverName := ""
#endif
/*
 * Efetua uma conexao … um banco de dados, retorna zero em caso de erro ou o 
 * numero da conexao efetuada com o servidor SQL. Em caso afirmativo armazena-o  
 * no array de conexäes j  efetuadas.
 * 12/12/2008 - 19:09:35
 */
****************
FUNCTION SL_CONN( cHost, cPort, cDb, cUser, cPwd, nFlags, cRddName, cSchema, cCharSet )
****************

   LOCAL xPointer  
   LOCAL cRddFunc
   LOCAL nResult
   LOCAL nSysID
   
   DEFAULT cPort      TO "5432"
   DEFAULT cSchema    TO "" 
   DEFAULT cCharSet   TO ""
   DEFAULT cHost      TO s_cHost
   DEFAULT cUser      TO s_cUser
   DEFAULT cPwd       TO s_cPwd
   DEFAULT cRddName   TO s_cDriverName

   DEBUG_ARGS

   SQLERR_CLEAR()
/*
msgstop( cPort    + CRLF + ;
         cSchema  + CRLF + ;
         cCharSet + CRLF + ;
         cHost    + CRLF + ;
         cUser    + CRLF + ;
         cPwd     + CRLF + ;
         cRddName + CRLF )
*/
   IF Empty( cHost )
      SQLERR_SET( 1000, "Missing HOSTNAME" )
      RETURN 00
   End

   IF Empty( cUser )
      SQLERR_SET( 1000, "Missing USERNAME" )
      RETURN 00
   End

   IF Empty( cRddName )
      SQLERR_SET( 1000, "Missing DRIVER name" )
      RETURN 00
   End

 * Checa algum poss¡vel ALIAS para este RDD
   cRddName := SL_CheckAlias( Alltrim(Upper( cRddName )) )

 * Tenta conexao com o Driver
   cRddFunc := SL_RDD_NAME + '_' + cRddName

   /*
    * 07-03-2006 - Checamos se a devida função foi corretamente linkada - e
    * exibimos a msg de erro mais apropriada ;)
    */

   IF TYPE( cRddFunc + '()' ) == 'U'
      SQLERR_SET( 1000, "Invalid driver ID '"+cRddName + "'")
      RETURN 00
   End

   cPort    := IIF( valtype( cPort ) == 'C', VAL( cPort ), cPort )
   xPointer := HB_ExecFromArray( cRddFunc, { @cHost, @cPort, @cDb, @cUser, @cPwd, @nFlags, @cSchema, @cCharSet } )
   
   IF ( valtype( xPointer ) != 'P' .AND. ;
        valtype( xPointer ) != 'O' ) 
      
      DEBUG "O driver de conexão " + cRddFunc + "() falhou:", HB_VALTOSTR( xPointer )
      RETURN 00
   End

   /* TODO: Turn this BLOCK Thread Safe! */
   nConnCount += 1
**msgstop( "nConnCount: " + str(nConnCount) )
   IF nConnCount == MAX_CONN_COUNT
      nConnCount := 1         
   End
   
   nResult  := nConnCount
   nSysID   := SQLSYS_ID( cRddName )

   DEBUG 'AADD( saConnections, { ', nResult, cRddName, cHost, cPort, cDb, cUser, cPwd, nFlags, SQLSYS_IDSTR( cRddName ), xPointer, cSchema, cCharSet, '} )'

   AADD( saConnections, { nResult,;  // SL_CONN_HANDLE  
                         cRddName,;  // SL_CONN_RDD     
                            cHost,;  // SL_CONN_HOST    
                            cPort,;  // SL_CONN_PORT    
                              cDb,;  // SL_CONN_DB      
                            cUser,;  // SL_CONN_USER    
                             cPwd,;  // SL_CONN_PASSWORD
                           nFlags,;  // SL_CONN_FLAGS   
                           nSysID,;  // SL_CONN_SYSTEMID
                         xPointer,;  // SL_CONN_POINTER    -- Pode ser um OBJ ou um POINTER 
                          cSchema,;  // SL_CONN_SCHEMA
                         cCharSet,;  // SL_CONN_CHARSET
                         		  0 ;  // SL_CONN_VERSION
                      }) 

   Atail(saConnections)[ SL_CONN_VERSION] :=	SQLServerVersionNum( Atail(saConnections), nSysID )	// 27/04/2009 - 15:27:44 - Vailton

   snConnHandle := nResult
   saConnInfo   := aClone( Atail(saConnections) )

   SQLRegisterDrv( nSysID, cRddName )
   SL_SetSchema( cSchema )        && Rossine 12/01/09
   SL_SetQuery( "" )              && Rossine 12/01/09
   SL_SetConnection( xPointer )   && Rossine 12/01/09

RETURN nResult

/*
 * Valida se para o RDD solicitado é um ALIAS e  
 * existe um nome correto a ser enviado.
 */
*********************************
static function SL_CheckAlias( cRddName )
*********************************

   LOCAL a := ';' + cRddName + ';'
   
   IF ( a $ ';POSTGRESQL;POSTGRES;POSTGRE;POST;PSQL;POSTSQL;POSTGSQL;PGS;' )
      cRddName := 'PGSQL'
   End
   
   return cRddName   

/*
 * Efetua uma conexao com o  servidor SQL com base  numa string de conexão. Por 
 * padrão, esta função chama SL_CONN() com os dados dectetados em cConnStr;
 * caso você  deseje  apenas desmembrar  cConnStr em  partes, utilize o segundo
 * argumento que pode ser um array que conterá as informações capturas,evitando
 * assim de que a conexão tente ser estabelecida. 
 * 13/12/2008 - 00:35:15
 */
*************************
function SL_CONNPARSE( cConnStr, aResult )
*************************

   LOCAL cHost    := 'localhost'
   LOCAL cPort    := ''
   LOCAL cDb      := ''             // Obrigatorio ao contrario do MySQL
   LOCAL cUser    := ''
   LOCAL cPwd     := ''
   LOCAL nFlags   := '0'
   LOCAL cRddName := ''
   LOCAL cSchema  := ''
   LOCAL cCharSet := ''  
   LOCAL lError   := FALSE
   LOCAL aArray, Item, Value, Pos, At

   /* Validate params ... */
   IF valtype( cConnStr ) != 'C' .AND. ;
      valtype( cConnStr ) != 'M'
      *
      return 0
   End
   /*
    * Ajusta os parametros para pegar o ESCAPE das STRINGs
    */
   cConnStr := StrTran( cConnStr, "'", "\'" )
   cConnStr := StrTran( cConnStr, "\", "\\" )
   /*
    * Converte para ARRAY
    */
   aArray   := SQLText2Array( cConnStr, ';' )

   SQLERR_CLEAR()

   FOR Pos := 1 TO LEN( aArray )

       Item := aArray[Pos]
       At   := At( "=", Item )

       IF ( At == 00 )
          lError := TRUE
          SQLERR_SET( 1000, "Invalid connection string" )
          EXIT
       End

       Value := Substr( Item, At +1 )
       Item  := Substr( Item, 1, At-1 )
       Item  := Alltrim(Upper( Item )) + ";"
       Item  := StrTran(Item, "  ", " " )
       Item  := StrTran(Item, "  ", " " )

       DO CASE
       CASE ( Item  $ "UID;USER;ID;USER ID;USUARIO;NOME;USERNAME;USER NAME;NAME;LOGIN;" )
            cUser    := Value

       CASE ( Item  $ "PWD;PASS;PASSWD;PASSWRD;PASSWORD;SENHA;" )
            cPwd     := Value

       CASE ( Item  $ "DTB;DB;DBNAME;DATA;DATABASE;BASE;PATH;BASEDEDADOS;BASE DE DADOS;DADOS;BANCO DE DADOS;BANCO;DADOS;DBQ;TNS;" )
            cDb      := Value

       CASE ( Item  $ "DRIVER;RDD;DRV;SQL;LIB;" )
            cRddName := Value

       CASE ( Item  $ "HOST;SERVER;SERVIDOR;CONECTAR;IP;CONECTA;HST;" )
            cHost    := Value

       CASE ( Item  $ "PORT;PORTA;ESCUTA;EM;" )
            cPort    := Value

       CASE ( Item  $ "CHARSET;" )       
            cCharSet := Value
            
       CASE ( Item  $ "SCHEMA;" )       
            cSchema := Value
            
       CASE ( Pos == 01 )
            cRddName := SUBST( Item, 01, Len( Item ) -1 )
            /*
             * Ajustamos pq o FireBird pode ter uma string assim:
             * firebird=127.0.0.1:c:\path\file.gdb
             */
            IF ( (At := At( ":", Value )) == 00 )
               cHost := Value
            Else
               cHost := Substr( Value, 1, At -1 )
               cDb   := Substr( Value, At+1 )
            End

       OTHERWISE
            lError := TRUE
            SQLERR_SET( 1000, "Invalid connection string => PARSE ERROR AT '"+ Item + "'" )
          EXIT
       End
   End

   /* Any error? Abort! */
   IF ( lError ) THEN;
      return 0 
   
   /*
    * It is only to recover the information in this conection string?
    */
   IF valtype( aResult ) == 'A'   
      IF Len( aResult ) < SL_CONN_COUNT 
         aSize( aResult, SL_CONN_COUNT )
      End
      
      aResult[SL_CONN_HANDLE  ] := 0
      aResult[SL_CONN_RDD     ] := SL_CheckAlias( cRddName )
      aResult[SL_CONN_HOST    ] := cHost
      aResult[SL_CONN_PORT    ] := VAL(cPort)
      aResult[SL_CONN_DB      ] := cDb
      aResult[SL_CONN_USER    ] := cUser
      aResult[SL_CONN_PASSWORD] := cPwd
      aResult[SL_CONN_FLAGS   ] := VAL( nFlags )
      aResult[SL_CONN_SYSTEMID] := SQLSYS_ID( aResult[SL_CONN_RDD] )
      aResult[SL_CONN_POINTER ] := NIL
      aResult[SL_CONN_SCHEMA  ] := cSchema
      aResult[SL_CONN_CHARSET ] := cCharSet
      return aResult[SL_CONN_SYSTEMID]
   End
      
   return SL_CONN( cHost, VAL(cPort), cDb, cUser, cPwd, nFlags, ;
                        cRddName, cSchema, cCharSet )

/*
 * Detecta o driver ID necessário para a connectionstring fornecida.
 * 15/12/2008 - 13:33:12
 */
**********************
function SL_DSN2ID( cConnStr )
**********************

   LOCAL aInfo  := {}
   LOCAL nSysID := ID_UNKNOW
   
   IF SL_CONNPARSE( cConnStr, aInfo ) <> 00
      nSysID := aInfo[SL_CONN_SYSTEMID]
   End   
   return nSysID

/*
 * Retorna .T. se este driver estiver linkado ao aplicativo atual.
 * 15/12/2008 - 14:07:10
 */
***********************
function SL_ISVALID( nSysID )
***********************

   LOCAL cRdd := SQLSYS_IDSTR( nSysID )
   
   IF !Empty( cRdd )      
      cRdd := SL_RDD_NAME + '_' + cRdd  + '_SQLARRAY'
      
      IF !(TYPE( cRdd + '()' ) == 'U')
         return .T.
      End
   End
   return .F.
    
/*
 * Derruba uma conexão específica ou todas as conexões ou ainda a conexão atual,  
 * com o banco de dados que estiver conectado. Retorna .T. indicando sucesso.
 */
***********************
function SL_DISCONN( nHandle )
***********************

   LOCAL Type  := valtype( nHandle )
   LOCAL aConn := NIL
   LOCAL cFunc, lRet
   LOCAL b

   IF Type == 'L'
       DEBUG 'Disconnecting all from saConnections'
       FOR b := LEN( saConnections ) TO 1 STEP -1
           nHandle := saConnections[B][SL_CONN_HANDLE]
           SL_DISCONN( nHandle )   
       End
       RETURN .T.
       
   ELSEIF Type == 'U'
      DEBUG 'Getting Current Connect number'
      nHandle := SL_GETCONN()
      
   ELSEIF Type <> "N"
      DEBUG 'Wrong Handle type ....'
      RETURN .F.
      
   End
   
   IF nHandle == 00 THEN;
      return .F.
   
   FOR b := len( saConnections ) TO 1 STEP -1
       IF saConnections[b][SL_CONN_HANDLE] == nHandle
          aConn := saConnections[b]
          Exit
       End
   End
   
   IF aConn == NIL THEN;
      return .F.

   /*
    * Tenta desconectar o driver
    */
   DEBUG 'Tenta desconectar o driver'

**   cFunc := SL_RDD_NAME + '_DISCONN_' + aConn[SL_CONN_RDD]
   cFunc := 'SL_DISCONN_' + aConn[SL_CONN_RDD]
   lRet  := &cFunc( aConn[SL_CONN_POINTER] )
   
   /*
    * Aqui ele deve remover o Handle do ARRAY saConnections
    */
   DEBUG 'Aqui ele deve remover o Handle do ARRAY saConnections'
   
   IF valtype(lRet) == 'L' .and. lRet

      IF ( snConnHandle == nHandle ) 
         snConnHandle := 00
         saConnInfo   := NIL
      End

      aDel( saConnections, b )
      aSize( saConnections, len(saConnections)-1 )
   End
   return lRet
   
/*
 * Retorna o numero da conexao atual
 * 12/12/2008 - 23:21:16
 */   
*******************
function SL_GETCONN
*******************

return snConnHandle  
   
/*
 * Retorna todas as conexoes com todas suas propriedades.
 * 14/07/2009 - 16:46:00 - By Rossine
 */   
***********************
function SL_GETCONN_ALL
***********************

return iif( valtype( saConnections ) != "A", { }, saConnections )

/*
 * Retorna o numero da conexao ao qual o ALIAS() atual está vinculado
 * 12/12/2008 - 23:21:16
 */   

***************************
function SL_GETCONNBYALIAS( nAlias )
***************************
   LOCAL aWAData

   IF VALTYPE( nAlias ) == 'U'
   	nAlias := Select()
  	End
   IF VALTYPE( nAlias ) == 'C'
   	nAlias := Select( nAlias )
  	End
  	IF VALTYPE( nAlias ) <> "N" .OR. ( nAlias == 00 )
  	   RETURN 0
  	End
  	
   aWAData := USRRDD_AREADATA( nAlias )
   
   IF valtype( aWAData ) == 'A' 
      return aWAData[ WA_CONNECTION ]
   End   
   RETURN 0
   
/*
 * Puxa informações sobre uma determinada conexão procurando pelo Handle
 * 13/12/2008 - 00:22:26
 */
***********************
function SL_GETCONNINFO( nHandle )
***********************

   LOCAL c := VALTYPE( nHandle )
   
   IF (c == "U") .OR. ( c == "N" .and. nHandle == 00 )
      RETURN saConnInfo                            
      
   ELSEIF c == "A" .AND. Len( nHandle ) == SL_CONN_COUNT
      * Usamos isto caso ele já esteja passando por engano um array com os dados
      * de uma conexão aparentemente válida - 23/05/2009 - 23:11:01
      IF ValType( nHandle[SL_CONN_HANDLE] ) $ "PON"
         RETURN nHandle
      End
      
   ELSEIF c $ "PON"   
      * NOTE que ele pega as conexões mais recentes primeiro para o caso dele
      * estar procurando as ultimas ou mais recentes.
      FOR c := len( saConnections ) TO 1 STEP -1
          IF saConnections[c,SL_CONN_HANDLE] == nHandle
             RETURN aClone( saConnections[c] )
          End
      End
   ELSEIF c = "C"  && By Rossine 14/07/09
      FOR c := len( saConnections ) TO 1 STEP -1
          IF saConnections[c,SL_CONN_DB] == nHandle
             RETURN saConnections[c,SL_CONN_HANDLE]
          endif
      next
   End 
   
   RETURN nil         

/*
 * Puxa informações sobre uma determinada conexão procurando pelo ID do Driver.
 * NOTE que ele pega as conexões mais recentes primeiro.
 * 15/12/2008 - 10:07:51
 */   
*******************************
function SL_GETCONNINFOBYID( nDriverID )
*******************************
   LOCAL c := VALTYPE( nDriverID )
   
   IF (c == "U") .OR. ( c == "N" .and. nDriverID == 00 )
      RETURN saConnInfo           
                       
   ELSEIF c == 'N'
      FOR c := len( saConnections ) TO 1 STEP -1
          IF saConnections[c,SL_CONN_SYSTEMID] == nDriverID
             RETURN aClone( saConnections[c] )
          End
      End
   End 
   RETURN nil      

/*
 * Altera o SCHEMA atual onde as TABELAS DE SISTEMA se encontram.
 * 18/03/2009 - 17:51:46
 */
FUNCTION SL_SetSystemSchema( cSChema )  && Rossine 07/10/08
   LOCAL cOld := s_cSysSchema
    
   IF VALTYPE( cSChema ) == 'C'
      s_cSysSchema := cSChema
   End   
   RETURN cOld
   
*************************
* TODO: Isto tem que ser válido para cada CONEXÃO e sendo assim deve estar
*       guardado dentro de saConnections[] - 23/05/2009 - 23:17:44

FUNCTION SL_SetSchema( cSChema )  && Rossine 07/10/08
*************************
   LOCAL cOld := s_cSchema    
   IF VALTYPE( cSChema ) == 'C'
      s_cSchema := cSChema
   End   
   
**   msgstop( SL_ToString( saConnInfo,.T.,,, "DUMP.TXT", .T. ), cSchema )

   saConnInfo[SL_CONN_SCHEMA] := cSchema && Rossine 11/07/09

**   msgstop( SL_ToString( saConnInfo,.T.,,, "DUMP.TXT", .T. ), cSchema )

   RETURN cOld

************************
FUNCTION SL_SetQuery( cQuery )  && Rossine 07/10/08
************************
   LOCAL cOld := s_cQuery    
   IF VALTYPE( cQuery ) == 'C'
      s_cQuery := cQuery
   End  
   RETURN cOld

*****************************
FUNCTION SL_SetConnection( nConn )
*****************************
   LOCAL nOld := s_nConn
   
   IF VALTYPE( nConn ) == 'N'
      s_nConn := nConn
   End   
   RETURN nOld

FUNCTION SL_PacketSize( nSize )
   LOCAL nOld := s_nPacketSize
   
   IF VALTYPE( nSize ) == 'N'
      s_nPacketSize := nSize
   End
   RETURN nOld

**********************                              
function SL_CONNPARAMS( cHost, cUser, cPwd, cDriverName )  
**********************                              
                                                    
s_cHost       := cHost
s_cUser       := cUser                              
s_cPwd        := cPwd                               
s_cDriverName := cDriverName                        
                                                    
return NIL                                          

*************************
function SL_GETCONNPARAMS
*************************

return { s_cHost, s_cUser, s_cPwd, s_cDriverName }

/*
 * Retorna a versão da SQL LIB em uso
 * 23/05/2009 - 22:18:31
 */
FUNCTION SL_Version()

   RETURN SQL_VTEXT + SQL_VERSION

/*
 * Seta uma conexão pelo nome do Banco de dados - By Rossine
 * 11/07/09 - 10:50:00
 */
*********************
function SL_GETCONNBD( cDb )
*********************

   local n, xRet
   	
   if ( n := ascan( saConnections, { |aParams| aParams[SL_CONN_DB] == cDb } ) ) > 0
      xRet := saConnections[n,SL_CONN_POINTER]
   endif      

return xRet

/*
 * Pega uma conexão pelo nome do Banco de dados - By Rossine
 * 11/07/09 - 11:45:00
 */
*********************
function SL_SETCONNBD( cDb )
*********************

   local n, lRet := .F.
   	
   if ( n := ascan( saConnections, { |aParams| aParams[SL_CONN_DB] == cDb } ) ) > 0
      SL_SetConnection( saConnections[n,SL_CONN_POINTER] )
      lRet := .T.
   endif      

return lRet

/*
 * Pega o schema que está ativo
 * 11/07/09 - 16:45:00
 */
*********************
FUNCTION SL_GetSchema
*********************

return saConnInfo[SL_CONN_SCHEMA]

//--EOF--//

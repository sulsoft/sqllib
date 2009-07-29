#include "SQLLIB.ch"

REQUEST SQLLIB
REQUEST PGSQL
REQUEST DBFCDX
REQUEST HB_LANG_PT

static cArqLog

#define CRLF chr(13) + chr(10)

function Main()
   
   local lRet, nConn1, nConn2, aFiles

   cArqLog := ""
      
   HB_LANGSELECT( "PT" )

   set date     to BRITISH
   set deleted  ON
   set century  ON

   SET AUTOPEN  OFF
   SET AUTORDER TO 1

   SQL CONN PARAMS TO HOST "localhost" ;
                      USER "postgres"  ;
                  PASSWORD "postgres"  ;
                       VIA "PGSQL"

   if !SL_DATABASE( "demosqllib" )
      SQL CREATE DATABASE "demosqllib" INTO lRet
      IF !lRet
         xmsgstop( "Não foi possível criar a tabela <demosqllib>" )
         QUIT
      End
   endif

   SQL CONNECT DATABASE "demosqllib" ;
                 SCHEMA "public" ;
                   INTO nConn1  

   IF nConn1 = 0
      xmsgerror( "Falha na conexao:;;" + SQLERR_MSG() )
      QUIT
   End
   
   if !SL_FILE( "customer" )
      aFiles := { "cadas\Customer.dbf" }
      SQL IMPORT DBF aFiles VIA "DBFCDX" PACK INTO lRet
   else
      lRet := .T.
   endif

   if !SL_DATABASE( "demotst" )
      SQL CREATE DATABASE "demotst" INTO lRet
      if !lRet
         SQL DISCONNECT FROM nConn1
         xmsgstop( "Não foi possível criar a tabela <demotst>" )
         QUIT
      endif
   endif

   SQL CONNECT DATABASE "demotst" ;
                 SCHEMA "public" ;
                   INTO nConn2  

   IF nConn2 = 0
      SQL DISCONNECT FROM nConn1
      xmsgerror( "Falha na conexao:;;" + SQLERR_MSG() )
      QUIT
   End

   if !SL_FILE( "sales" )
      aFiles := { "cadas\sales.dbf" }
      SQL IMPORT DBF aFiles VIA "DBFCDX" PACK INTO lRet
   else
      lRet := .T.
   endif

   USE "Customer" ALIAS "SQLCust" via "SQLLIB" shared new CONNECTION nConn1

   USE "sales" ALIAS "SQLSales" via "SQLLIB" shared new CONNECTION nConn2

**xmsgstop( SL_ToString( SL_GETCONN(),.T.,,, "DUMP.TXT", .T. ) )
**xmsgstop( SL_SetConnection( nConn1 ) )
**xmsgstop( SL_ToString( SL_GETCONN(),.T.,,, "DUMP.TXT", .T. ) )
**xmsgstop( nConn1 )
   cls 

   LOG_MSG( "alias() :", alias() )
   LOG_MSG( "alias(1):", alias(1) )
   LOG_MSG( "alias(2):", alias(2) )
   LOG_MSG( "select():", select() )
   LOG_MSG( "dbselectarea(1):", dbselectarea(1) )
   LOG_MSG( "select():", select() )
   LOG_MSG( "alias() :", alias() )

   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )
   LOG_MSG( "Estou posicionado no registro", recno() )

   LOG_MSG( "DbRLock-Registro 2 travado ?", SQLCust->( DbRLock(2) ) )
   LOG_MSG( "Estou posicionado no registro", dbgoto(2) )
   LOG_MSG( "Estou posicionado no registro", recno() )
   LOG_MSG( "islocked-Registro 2 travado ?", SQLCust->( islocked(2) ) )

   LOG_MSG( "DbRLock-Registro 3 travado ?", SQLCust->( DbRLock(3) ) )
   LOG_MSG( "Estou posicionado no registro", dbgoto(3) )
   LOG_MSG( "Estou posicionado no registro", recno() )
   LOG_MSG( "islocked-Registro 3 travado ?", SQLCust->( islocked(3) ) )

*   LOG_MSG( "Travando  Registro 2: ", SQLCust->( dbrlock(2) ) )
*   LOG_MSG( "Registro 2 travado ?", SQLCust->( islocked() ) )
*   LOG_MSG( "Travando  Registro 3: ", SQLCust->( dbrlock(3) ) )
*   LOG_MSG( "Registro 3 travado ?", SQLCust->( islocked() ) )
   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )

PARAR()

   LOG_MSG( "Estou posicionado no registro", dbunlock(2) )
   LOG_MSG( "Estou posicionado no registro", dbunlock(3) )
   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )
   
PARAR()
cls

   LOG_MSG( "movendo para o registro 2", SQLSALES->( dbgoto( 2 ) ) )
   LOG_MSG( "Registro 2 travado ?", SQLSALES->( DbRLock() ) )

   SQLSALES->NOTES := "teste 21"
   SQLSALES->( dbcommit() )
   SQLSALES->( dbunlock() )

   LOG_MSG( "movendo para o registro 3", SQLSALES->( dbgoto( 3 ) ) )
   LOG_MSG( "Registro 3 travado ?", SQLSALES->( DbRLock(3) ) )

   replace SQLSALES->NOTES with "teste 22"
   SQLSALES->( dbcommit() )
   SQLSALES->( dbunlock() )

   LOG_MSG( "Estou posicionado no registro", recno() )
   LOG_MSG( "Estou posicionado no registro", recno() )
   LOG_MSG( "Vou incluir um registro: ", SQLCust->( dbappend() ) )
   LOG_MSG( "Estou posicionado no registro", recno() )

PARAR()
cls

   ? "Agora vamos iniciar uma transação/commit..."
   
   SQL STARTTRANS ALIAS "SQLSALES" INTO lRet

   if !lRet
      SQL DISCONNECT FROM nConn1
      SQL DISCONNECT FROM nConn2
      xmsgstop( "Não foi possível iniciar a transação !!!" )
      QUIT
   else
      LOG_MSG( "movendo para o registro 3", SQLSALES->( dbgoto( 3 ) ) )
      LOG_MSG( "Registro 3 travado ?", SQLSALES->( DbRLock() ) )
      SQLSALES->NOTES := "testando <Transação>"
      SQLSALES->( dbcommit() )
      SQLSALES->( dbunlock() )
      SQL COMMIT ALIAS "SQLSALES" INTO lRet
      SQL ENDTRANS ALIAS "SQLSALES" INTO lRet
PARAR( "-Veja no PGAdmin se isto aparece na tabela <SALES>-" )
   endif

   cls
   ? "Agora vamos iniciar uma transação/rollback..."
   
   LOG_MSG( "select():", select() )
   LOG_MSG( "alias() :", alias() )

   SQL STARTTRANS SELECT 2 INTO lRet

   if !lRet
      SQL DISCONNECT FROM nConn1
      SQL DISCONNECT FROM nConn2
      xmsgstop( "Não foi possível iniciar a transação !!!" )
      QUIT
   else
      LOG_MSG( "movendo para o registro 3", SQLSALES->( dbgoto( 3 ) ) )
      LOG_MSG( "Registro 3 travado ?", SQLSALES->( DbRLock() ) )
      LOG_MSG( "Registro 3 travado ?", SQLSALES->( islocked() ) )
      SQLSALES->NOTES := "Isto não pode aparecer"
PARAR( "-Veja no PGAdmin se isto aparece na tabela <SALES>-" )
**      SQLSALES->( dbcommit() )
**      SQLSALES->( dbunlock() )
      SQL ROLLBACK SELECT 2 INTO lRet
      LOG_MSG( "Estou posicionado no registro", SQLSALES->( recno() ) )
      LOG_MSG( "Registro 3 travado ?", SQLSALES->( islocked() ) )
      SQL ENDTRANS SELECT 2 INTO lRet
PARAR( "-Veja no PGAdmin se isto aparece na tabela <SALES>-" )
   endif

   LOG_MSG( "select():", select() )
   LOG_MSG( "alias() :", alias() )

   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )
   LOG_MSG( "Estou posicionado no registro", recno() )
   LOG_MSG( "Travando  Registro 2: ", SQLCust->( dbrlock(2) ) )
   LOG_MSG( "movendo para o registro 2", dbgoto( 2 ) )
   LOG_MSG( "Registro 2 travado ?", SQLCust->( islocked() ) )
   LOG_MSG( "Estou posicionado no registro", recno() )
   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )
   LOG_MSG( "Estou posicionado no registro", recno() )
PARAR()
cls
   LOG_MSG( "movendo para o registro 3", dbgoto( 3 ) )
   LOG_MSG( "Estou posicionado no registro", SQLCust->( recno() ) )
   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )
   LOG_MSG( "Registro 3 travado ?", SQLCust->( islocked() ) )
   LOG_MSG( "Travando  Registro 3: ", SQLCust->( dbrlock() ) )
   LOG_MSG( "movendo para o registro 3", dbgoto( 3 ) )
   LOG_MSG( "Estou posicionado no registro", SQLCust->( recno() ) )
   LOG_MSG( "Registro 3 travado ?", SQLCust->( islocked() ) )
   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )
   LOG_MSG( "Nome do campo numero 1: " + SQLCust->( fieldname( 1 ) ) )
   LOG_MSG( "Nome do campo numero 2: " + SQLCust->( fieldname( 2 ) ) )
   LOG_MSG( "Conteudo do campo 1: ", SQLCust->( fieldget( 1 ) ) )
   LOG_MSG( "Conteudo do campo 2: ", SQLCust->( fieldget( 2 ) ) )

PARAR()
cls
   LOG_MSG( "movendo para o registro 3", dbgoto( 3 ) )

   if SQLCust->( dbrlock(3) )
      LOG_MSG( "Registro 3 travado-Antes fieldput ?", SQLCust->( islocked() ) )
      LOG_MSG( "Gravando campos", SQLCust->( islocked() ) )
      SQLCust->( fieldput( 1, "teste 50" ) )
      SQLCust->( fieldput( 2, "teste 51" ) )
      LOG_MSG( "Registro 3 travado - Antes dbcommit() ?", SQLCust->( islocked() ) )
      SQLCust->( dbcommit() )
      LOG_MSG( "Registro 3 travado - Depois dbcommit() ?", SQLCust->( islocked() ) )
      SQLCust->( dbunlock() )
      LOG_MSG( "Registro 3 travado - Depois dbunlock() ?", SQLCust->( islocked() ) )
   endif

   LOG_MSG( "Conteudo do campo 1: ", SQLCust->( fieldget( 1 ) ) )
   LOG_MSG( "Conteudo do campo 2: ", SQLCust->( fieldget( 2 ) ) )
   LOG_MSG( "Destravando  Registro 3: ", SQLCust->( dbunlock() ) )
   LOG_MSG( "Registro 3 travado ?", SQLCust->( islocked() ) )
   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )
   LOG_MSG( "Destravando  Registro 2: ", SQLCust->( dbunlock(2) ) )
   LOG_MSG( "Registro 2 travado ?", SQLCust->( islocked() ) )

PARAR()
cls

   LOG_MSG( "Estou posicionado no registro", SQLCust->( recno() ) )
   LOG_MSG( "Vou incluir um registro: ", SQLCust->( dbappend() ) )
   LOG_MSG( "Estou posicionado no registro", SQLCust->( recno() ) )
   LOG_MSG( "Travando  Registro " + alltrim(str(SQLCust->( recno() )))+ ": ", SQLCust->( dbrlock(SQLCust->( recno() )) ) )

   if SQLCust->( islocked() )
      LOG_MSG( "gravando os campos", SQLCust->( islocked() ) )
      LOG_MSG( "Gravando campos", SQLCust->( islocked() ) )
      SQLCust->( fieldput( 1, "teste 31" ) )
      SQLCust->( fieldput( 2, "teste 32" ) )
      SQLCust->( dbcommit() )
      SQLCust->( dbunlock() )
   endif

PARAR()
cls

   ? "Vou incluir um registro: ", SQLCust->( dbappend() )
   ? "Estou posicionado no registro", SQLCust->( recno() )
   ? "Travando  Registro " + alltrim(str(SQLCust->( recno() )))+ ": ", SQLCust->( dbrlock(SQLCust->( recno() )) )

   if SQLCust->( islocked() )
      LOG_MSG( "Gravando campos", SQLCust->( islocked() ) )
      SQLCust->( fieldput( 1, "teste de exclusão" ) )
      SQLCust->( fieldput( 2, "teste 41" ) )
      SQLCust->( dbcommit() )
      SQLCust->( dbdelete() )
      SQLCust->( dbunlock() )
   endif

   LOG_MSG( "alias() :", alias() )

   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )

   for n = 100 to 150
       LOG_MSG( "Excluindo registro ", n )
       SQLCust->( dbrlock(n) )
       SQLCust->( dbgoto(n) )
       SQLCust->( dbdelete() )
       SQLCust->( dbunlock() )
       SQLCust->( dbcommit() )
   next

   LOG_MSG( "dbrlocklist(): ", dbrlocklist() )
PARAR()
cls

   LOG_MSG( "alias() :", alias() )
   
   SQLCust->( dbcommit() )

   SQLCust->( dbgotop() )

   LOG_MSG( "SL_DELETEINDEX: ", hb_valtostr( SL_DELETEINDEX( "_tmp", , , nConn1 ) ) )

   index on nome tag "001" to _tmp
   index on last tag "002" to _tmp
   
   set index to _tmp

   LOG_MSG( "Existe TAG 001 em <customer> ?", hb_valtostr( SL_TAG( "001", "_TMP", "CUSTOMER", nConn1 ) ) )
   LOG_MSG( "Existe TAG 002 em <customer> ?", hb_valtostr( SL_TAG( "002", "_TMP", "CUSTOMER", nConn1 ) ) )
   LOG_MSG( "Existe TAG 003 em <customer> ?", hb_valtostr( SL_TAG( "003", "_TMP", "CUSTOMER", nConn1 ) ) )

   SQLCust->( dbgotop() )
   
   cls 
   
   SQLCust->( browse() )

   LOG_MSG( "alias() :", alias() )

   LOG_MSG( "Deletou o indice <_tmp> ? ", hb_valtostr( SL_DELETEINDEX( "_tmp" ) ) ) && Aqui var dar erro pois não passei o numero da conexao que está "_tmp"
                                                                                     &&  e a conexao atual é nConn2
                            
   LOG_MSG( "Deletou o indice <_tmp> ? ", hb_valtostr( SL_DELETEINDEX( "_tmp", , , nConn1 ) ) ) && Aqui vai dar certo

   dbcloseall()

   SQL DISCONNECT FROM nConn1
   SQL DISCONNECT FROM nConn2
   
   memowrit( "logteste.txt", cArqLog )

   cls

   ? ""
   ? ""
   ? ""
   ? "Foi gerado o arquivo de log: [logteste.txt]"
   ? ""
   ? ""
   ? ""

return NIL

***********************
static function LOG_MSG( cMsg, xVal )
***********************

cArqLog += cMsg + " - [" + SL_ToString(xVal) + "]" + CRLF + CRLF

SetColor( "W/B" )
? pad( cMsg + " - [" + SL_ToString(xVal) + "]", 80 )
SetColor( "W/N" )

return NIL

**************
function PARAR( cMsg )
**************

if valtype( cMsg ) = "U"
   cMsg := "-Parada-"
endif

SetColor( "W+/R" )
? pad( cMsg, 79 )
inkey(0)
SetColor( "W/N" )

return NIL

//--EOF--//

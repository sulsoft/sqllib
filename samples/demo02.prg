#include "SQLLIB.ch"

REQUEST SQLLIB
REQUEST PGSQL
REQUEST DBFCDX
REQUEST HB_LANG_PT

function Main()
   
   local lRet, nConn, aFiles
   
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
         msgstop( "Não foi possível criar a tabela <demosqllib>" )
         QUIT
      End
   endif

   SQL CONNECT DATABASE "demosqllib" ;
                 SCHEMA "public" ;
                   INTO nConn  

   IF nConn = 0
      msgerror( "Falha na conexao:;;" + SQLERR_MSG() )
      QUIT
   End
   
   if !SL_FILE( "customer" )
      aFiles := { "cadas\Customer.dbf" }
      SQL IMPORT DBF aFiles VIA "DBFCDX" PACK INTO lRet
   else
      lRet := .T.
   endif

   USE "Customer" ALIAS "SQLCust" via "SQLLIB" shared new

   cls 

   ? alias()
   ? "dbrlocklist(): " + SL_ToString(dbrlocklist())
   ? "Travando  Registro 2: ", SQLCust->( dbrlock(2) )
   ? "dbrlocklist(): " + SL_ToString(dbrlocklist())
   ? "Estou posicionado no registro", recno()
   ? "movendo para o registro 3", dbgoto( 3 )
   ? "Estou posicionado no registro", SQLCust->( recno() )
   ? "dbrlocklist(): " + SL_ToString(dbrlocklist())
   ? "Registro 3 travado ?", SQLCust->( islocked() )
   ? "Travando  Registro 3: ", SQLCust->( dbrlock() )
   ? "Registro 3 travado ?", SQLCust->( islocked() )
   ? "dbrlocklist(): " + SL_ToString(dbrlocklist())
   ? "Nome do campo numero 1: " + SQLCust->( fieldname( 1 ) )
   ? "Nome do campo numero 2: " + SQLCust->( fieldname( 2 ) )
   ? "Conteudo do campo 1: ", SQLCust->( fieldget( 1 ) )
   ? "Conteudo do campo 2: ", SQLCust->( fieldget( 2 ) )

wait "-parando-"
   ? "movendo para o registro 3", dbgoto( 3 )

   if SQLCust->( dbrlock(3) )
      SQLCust->( fieldput( 1, "teste 10" ) )
      SQLCust->( fieldput( 2, "teste 11" ) )
      SQLCust->( dbcommit() )
   endif

   ? "Conteudo do campo 1: ", SQLCust->( fieldget( 1 ) )
   ? "Conteudo do campo 2: ", SQLCust->( fieldget( 2 ) )
   ? "Destravando  Registro 3: ", SQLCust->( dbunlock() )
   ? "Registro 3 travado ?", SQLCust->( islocked() )
   ? "dbrlocklist(): " + SL_ToString(dbrlocklist())
   ? "Destravando  Registro 2: ", SQLCust->( dbunlock(2) )

wait "-parando-"
cls

   ? "Estou posicionado no registro", SQLCust->( recno() )
   ? "Vou incluir um registro: ", SQLCust->( dbappend() )
   ? "Estou posicionado no registro", SQLCust->( recno() )
   ? "Travando  Registro "+alltrim(str(SQLCust->( recno() )))+ ": ", SQLCust->( dbrlock(SQLCust->( recno() )) )

   if SQLCust->( islocked() )
      ? "gravando os campos", SQLCust->( islocked() )
      SQLCust->( fieldput( 1, "teste 1" ) )
      SQLCust->( fieldput( 2, "teste 2" ) )
      SQLCust->( dbcommit() )
      SQLCust->( dbunlock() )
   endif

wait "-parando-"

   ? "Vou incluir um registro: ", SQLCust->( dbappend() )
   ? "Estou posicionado no registro", SQLCust->( recno() )
   ? "Travando  Registro "+alltrim(str(SQLCust->( recno() )))+ ": ", SQLCust->( dbrlock(SQLCust->( recno() )) )

   if SQLCust->( islocked() )
      ? "gravando os campos", SQLCust->( islocked() )
      SQLCust->( fieldput( 1, "teste de exclusão" ) )
      SQLCust->( fieldput( 2, "teste 2" ) )
      SQLCust->( dbdelete() )
      SQLCust->( dbcommit() )
      SQLCust->( dbunlock() )
   endif

   SQLCust->( dbgotop() )
   
   cls 
   
   SQLCust->( browse() )

   dbcloseall()

   SQL DISCONNECT FROM nConn

return NIL

//--EOF--//
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
   ? "Estou posicionado no registro", SQLCust->( recno() )
   ? "movendo para o registro 3", dbgoto( 3 )
   ? "Estou posicionado no registro", SQLCust->( recno() )
   ? ""
wait "-parando-"
   cls
? "Registro 3 travado ?", SQLCust->( islocked() )
   SQLCust->( dbrlock() )
   ? "Registro 3 travado ?", SQLCust->( islocked() )
wait "-parando-"
   cls
   SQLCust->( fieldput( 1, "teste 1" ) )
   SQLCust->( fieldput( 2, "teste 2" ) )

   SQLCust->( dbunlock() )

   ? "Registro 3 travado ?", SQLCust->( islocked() )
wait "-parando-"
   SQLCust->( dbgotop() )
   
   cls 
   
   SQLCust->( browse() )

   dbcloseall()

   SQL DISCONNECT FROM nConn

return NIL

//--EOF--//
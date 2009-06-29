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

   USE Customer ALIAS "DBFCust" via "DBFCDX" exclusive new

   cls
   ? "Destroy index <DBFCust> ?: " + hb_valtostr( filedelete( "_tmp.cdx" ) )
   ? "index <DBFCust> exist ?: " + hb_valtostr( file( "_tmp.cdx" ) )

   index on FIRST                        tag tag_first     to _TMP && Order 1
   index on dtos(hiredate)               tag tag_hiredate  to _TMP && Order 2
   index on hb_valtostr(married)         tag tag_married   to _TMP && Order 3
   index on str(salary, 9, 2 )           tag tag_salary    to _TMP && Order 4
   index on FIRST + dtos(hiredate)       tag tag_Fhiredate to _TMP && Order 5
   index on FIRST + hb_valtostr(married) tag tag_Fmarried  to _TMP && Order 6
   index on FIRST + str(salary, 9, 2 )   tag tag_FFsalary  to _TMP && Order 7

   ? "index <DBFCust> exist ?: " + hb_valtostr( file( "_tmp.cdx" ) )
   wait "Press ENTER..."

   cls 
   
   ? "Testing DBFCDX"
   ? "--------------"
   ? ""
   READ_REG( 1, "DBFCust", "Rick" )
   ? replicate( "-", 80 )
   READ_REG( 2, "DBFCust", dtos(ctod("17/01/1987")) )
   ? replicate( "-", 80 )
   READ_REG( 3, "DBFCust", ".T." )
   ? replicate( "-", 80 )
   READ_REG( 4, "DBFCust", str( 23700, 9, 2 ) )
   ? replicate( "-", 80 )
   READ_REG( 5, "DBFCust", pad( "Rick", 20 ) + dtos(ctod("17/01/1987")) )
   ? replicate( "-", 80 )
   READ_REG( 6, "DBFCust", pad( "Rick", 20 ) + ".T." )
   ? replicate( "-", 80 )
   READ_REG( 7, "DBFCust", pad( "Rick", 20 ) + str( 23700, 9, 2 ) )
   ? ""
   wait "Press ENTER..."

   dbcloseall()

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

*   if SL_FILE( "customer" )
*      ? "Destroy Table <customer> ?: " + hb_valtostr( SL_DELETETABLE( "CUSTOMER" ) ) && Repare que passo o nome em maiusculo de proposito.
*      wait "Press ENTER..."
*   endif
   
   if !SL_FILE( "customer" )
      aFiles := { "Customer.dbf" }
      SQL IMPORT DBF aFiles VIA "DBFCDX" PACK INTO lRet
   else
      lRet := .T.
   endif

   USE Customer ALIAS "SQLCust" via "SQLLIB" exclusive new

   cls 

   ? "Destroy index <SQLCust> ?: " + hb_valtostr( SL_DELETEINDEX( "_tmp" ) )
   ? "index <SQLCust> exist ?: " + hb_valtostr( SL_INDEXE( "_tmp" ) )
   wait "Press ENTER..."

   index on FIRST                        tag tag_first     to _TMP && Order 1
   index on dtos(hiredate)               tag tag_hiredate  to _TMP && Order 2
*   index on hb_valtostr(married)         tag tag_married   to _TMP && Order 3
   index on str(salary, 9, 2 )           tag tag_salary    to _TMP && Order 4
   index on FIRST + dtos(hiredate)       tag tag_Fhiredate to _TMP && Order 5
*   index on FIRST + hb_valtostr(married) tag tag_Fmarried  to _TMP && Order 6
   index on FIRST + str(salary, 9, 2 )   tag tag_FFsalary  to _TMP && Order 7

   cls
   ? "index <SQLCust> exist ?: " + hb_valtostr( SL_INDEXE( "_tmp" ) ) && Vailton, aqui deveria aceitar soemnte
   wait "Press ENTER..."

   cls 
   
   ? ""
   ? ""
   ? "Testing SQLLIB"
   ? "--------------"
   ? ""
   READ_REG( 1, "SQLCust", "Rick" )  && Vailton, assim não acha o registro.
   ? replicate( "-", 80 )
   READ_REG( 1, "SQLCust", pad( "Rick", 20 ) )  && Vailton, assim dá certo mas me SQLCust o recno = 501 sendo sl_rowId = 8
   ? replicate( "-", 80 )
   READ_REG( 2, "SQLCust", dtos(ctod("17/01/1987")) )
   ? replicate( "-", 80 )
*   READ_REG( 3, "SQLCust", ".T." )
*   ? replicate( "-", 80 )
   READ_REG( 3, "SQLCust", str( 23700, 9, 2 ) )
   ? replicate( "-", 80 )
   READ_REG( 4, "SQLCust", pad( "Rick", 20 ) + dtos(ctod("17/01/1987")) )
   ? replicate( "-", 80 )
*   READ_REG( 6, "SQLCust", pad( "Rick", 20 ) + ".T." )
*   ? replicate( "-", 80 )
   READ_REG( 5, "SQLCust", pad( "Rick", 20 ) + str( 23700, 9, 2 ) )
   wait "Press ENTER..."

   cls
   ? "Vou posicionar o ponteiro no registro 396"
   SQLCust->( dbgoto( 396 ) )
   ? "O registro 396 esta deletado ? " + iif( SQLCust->( deleted() ), "Sim", "Nao" )
   ? "Conteudo do campo [" + SQLCust->( fieldname(1) ) + "]=[" + alltrim(hb_valtostr(SQLCust->( fieldget(1) ))) + "]"
   ? "Estou no registro [" + alltrim(str(SQLCust->( recno() ))) + "]"
   ? "Vou deletar o registro 396"
   SQLCust->( dbdelete() )
   ? "Vou posicionar o ponteiro no registro 396"
   SQLCust->( dbgoto( 396 ) )
   ? "Conteudo do campo [" + SQLCust->( fieldname(1) ) + "]=[" + alltrim(hb_valtostr(SQLCust->( fieldget(1) ))) + "]"
   ? "Estou no registro [" + alltrim(str(SQLCust->( recno() ))) + "]"
   wait "Press ENTER..."

   BROWSE_ORD( 1 )
   BROWSE_ORD( 2 )
   BROWSE_ORD( 3 )
   BROWSE_ORD( 4 )
   BROWSE_ORD( 5 )

   dbcloseall()

   SQL DISCONNECT FROM nConn

return NIL

************************
static function READ_REG( nOrder, cAlias, cChv )
************************

(cAlias)->( dbSetorder( nOrder ) )

if (cAlias)->( dbseek( cChv, .F. ) )
   ? "Index: " + (cAlias)->( indexkey() ) + " Found register = <" + cChv + "> ? -> YES   Num recno: " + alltrim(str((cAlias)->( recno() ) ) )
else
   ? "Index: " + (cAlias)->( indexkey() ) + " Found register = <" + cChv + "> ? -> NO"
endif

return NIL

**************************
static function BROWSE_ORD( nOrder )
**************************

cls
SQLCust->( dbSetorder( nOrder ) )
SQLCust->( dbgotop() )
@0, 0 say replicate( "_", 80 )
@0, 0 say "Ordem numero: [" + alltrim(str(nOrder) ) + "]___Ordenado por: [" + alltrim(SQLCust->( indexkey() )) + "]"
SQLCust->( browse() )

return NIL

*#include "FiveWin.ch"

#include "SQLLIB.ch"

#include "postgres.ch"
#include "dbinfo.ch"

#ifdef __HARBOUR__
   #ifndef __XHARBOUR__

      #include "hbcompat.ch"  && tem que incluir esta linha para harbour pois o try/catch precisa dela.
   
   #endif /* __XHARBOUR__ */
#endif /* __HARBOUR__ */

REQUEST SQLLIB
REQUEST PGSQL
REQUEST DBFCDX
REQUEST HB_LANG_PT

#ifndef FWVERSION
   #xcommand DEFAULT <uVar1> := <uVal1> ;
                  [, <uVarN> := <uValN> ] => ;
                     If( <uVar1> == nil, <uVar1> := <uVal1>, ) ;;
                   [ If( <uVarN> == nil, <uVarN> := <uValN>, ); ]
   #define CRLF chr(13)+chr(10)
//   REQUEST HB_GT_WVT
#else
   #include "xbrowse.ch"
#endif

*************
function Main
************* 

local nConn, aFiles, lRet, aRet

cls

rddsetdefault( "DBFNTX" )

**SQL CREATEDB "teste" OWNER "postgres"

SQL CONNECT ON "localhost" ;
          USER "postgres" ;
      PASSWORD "postgres" ;
      DATABASE "rossine" ;
           LIB "PGSQL" ;
        SCHEMA "public" ;
          INTO nConn  

if !SQL_FILE( "arq001" )
   aFiles := { "arq001.dbf", "arq002.dbf", "arq003.dbf" }
   SQL IMPORT DBF aFiles VIA "DBFNTX" PACK DELETE EVAL { || TESTE() } EVERY 1 APPENDEVAL { || REGISTRO() } INTO lRet
else
   lRet := .T.
endif

if lRet
   USE arq001 ALIAS "arq001" VIA "SQLLIB" NEW 
   INDEX ON a01_chv TO arq001
   SET INDEX TO arq001
   SET ORDER TO 1
   
   browse()
   
   dbcloseall()

   cls

   wait "...agora vamos retornar os arquivos para DBF. Tecle ENTER..."
   
   SQL EXPORT DBF VIA "DBFNTX" PACK DELETE EVAL { || TESTE() } EVERY 1 COPYEVAL { || REGISTRO() } INTO aRet

   if len(aRet) > 0
      wait "Arquivos retornados para DBF" + CRLF &&  + valtoprg( aRet )
   else
      wait "Problemas no retorno para .DBF !!!"
   endif
else
   wait "Arquivos não importados para o banco de Dados !!!"
   quit
endif

dbcloseall()

SQL DISCONNECT FROM nConn

return NIL

**************
function TESTE
**************

alert( "eval file" )

return NIL

*****************
function REGISTRO
*****************

alert( "eval register" )

return NIL
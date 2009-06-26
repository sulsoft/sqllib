*#include "FiveWin.ch"

#include "SQLLIB.ch"
#include "postgres.ch"
#include "hbusrrdd.ch"
#include "dbinfo.ch"
#include "simpleio.ch"

*#define SQL_DEBUG
#include "..\include\SQLLIBrdd.ch"

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
#endif

#define CRIAR_DADOS

function Main()

   local nConn

   HB_LANGSELECT( "PT" )

   set date       to BRITISH
   set deleted    ON
   set century    ON

   SET AUTOPEN    OFF
   SET AUTORDER   TO 1

*  SQL_DEBUGINIT()
   
   cHost := "localhost"
   cPass := "postgres"
   cUser := "postgres"

   SQL CONN PARAMS TO HOST cHost ;
                      USER cPass ;
                  PASSWORD cUser ;
                       VIA "PGSQL"

wait SL_DATABASE( "Demopgsql" )  && Tinha que retornar .T.

   if !SL_DATABASE( "Demopgsql" )
      SQL CREATE DATABASE "Demopgsql" INTO lRet
   endif

* Vailton, verificar porque que aqui retorna .F. sendo que o BD foi criado acima.

wait SL_DATABASE( "Demopgsql" )  && Tinha que retornar .T.

   SQL CONNECT ON cHost ;
             USER cUser ;
         PASSWORD cPass ;
         DATABASE "demopgsql" ;
              LIB "PGSQL" ;
           SCHEMA "public" ;
             INTO nConn  

   if !SL_TABLE( "teste" )
      DBCreate( "teste", { { "cod" ,  "c", 4, 0 },;
                           { "desc",  "c", 3, 0 },;
                           { "date",  "d",10, 0 },;
                           { "valor", "n",13, 3 },;
                           { "status","c", 1, 0 } }, "SQLLIB" )
   endif
wait SL_TABLE( "teste" )
   USE "teste" ALIAS "teste" VIA "SQLLIB" NEW exclusive
wait alias()

   zap
wait "-3-"

*  SQL_DEBUGINIT( .T. )

   INDEX ON cod                              TAG ID1 TO "teste"
   INDEX ON desc                             TAG ID2 TO "teste"
   INDEX ON date                             TAG ID3 TO "teste"
   INDEX ON cod + desc                       TAG ID4 TO "teste"
   INDEX ON cod + dtos(date)                 TAG ID5 TO "teste"
   INDEX ON str(valor) + dtos(date)          TAG ID6 TO "teste"
   INDEX ON status + cod + desc              TAG ID7 TO "teste"
   INDEX ON cod + desc + status              TAG ID8 TO "teste"
   CLOSE INDEXES

*  SQL_DEBUGINIT( .F. )

   SET INDEX TO teste
   SET ORDER TO 1

#ifdef CRIAR_DADOS
      IF LastRec() < 100
         d := date() - 295
         
         FOR I := 1 TO 100
             DbAppend()
             FieldPut(1, STRZERO( i, 4 ) )
             FieldPut(2, 'CCC' )
             FieldPut(3, ++d )
             FieldPut(4, seconds() + .123 )
             FieldPut(5, 'A' )

             DbAppend()
             FieldPut(1, STRZERO( i, 4 ) )
             FieldPut(2, 'AAA' )
             FieldPut(3, ++d )
             FieldPut(4, seconds() + .849 )
             FieldPut(5, 'A' )

             DbAppend()
             FieldPut(1, STRZERO( i, 4 ) )
             FieldPut(2, 'BBB' )
             FieldPut(3, ++d )
             FieldPut(4, seconds() + .315 )
             FieldPut(5, 'I' )
         End
      End
      DbCommit()      
#endif
   CLOSE

   SET PACKETSIZE TO 12
   USE "teste" ALIAS teste VIA "SQLLIB" NEW exclusive

   SET INDEX TO "teste"
   SET ORDER TO 7

   browse()

   dbcloseall()

   SQL DISCONNECT FROM nConn

return NIL
 
*******************************
function GetErrorInfo( oError )
*******************************
   local cInfo := "", n

   cInfo += "Description: " + oError:Description + Chr( 13 ) + Chr( 10 )
   cInfo += "GenCode: " + AllTrim( Str( oError:genCode ) ) + Chr( 13 ) + Chr( 10 )
   cInfo += "Operation: " + oError:Operation + Chr( 13 ) + Chr( 10 )
   
   if valtype( oError:Filename ) == 'C'
      cInfo += "Filename: " + oError:Filename + Chr( 13 ) + Chr( 10 )
   end
 
   cInfo += Chr( 13 ) + Chr( 10 ) +;
            'Arguments'  + Chr( 13 ) + Chr( 10 ) +;
            '===========================' + Chr( 13 ) + Chr( 10 )
            
   if ValType( oError:Args ) == "A"
      for n = 1 to Len( oError:Args )
          cInfo += "Args[" + AllTrim( Str( n ) ) + "] => " + ;
                   HB_VALTOSTR( oError:Args[ n ] )  + Chr( 13 ) + Chr( 10 )
      next
   endif
 
   cInfo += Chr( 13 ) + Chr( 10 ) + ;
            'Call Stack '  + Chr( 13 ) + Chr( 10 ) +;
            '===========================' + Chr( 13 ) + Chr( 10 )
   n = 2   
   while ! Empty( ProcName( n ) )
      cInfo += ProcName( n ) + "(" + AllTrim( Str( ProcLine( n++ ) ) ) + ")" + Chr( 13 ) + Chr( 10 )
   end

return hb_OemToAnsi(cInfo)

init procedure EuPrimeiro
#ifndef FWVERSION
   ErrorBlock( { | oError | MsgError( GetErrorInfo( oError ), "Error" ), __Quit() } )
   //SetUnhandledExceptionFilter( @GpfHandler() )
#endif
   return nil

#include "hbgtinfo.ch"
exit procedure EuPorUltimo
   IF hb_gtInfo( HB_GTI_ISGRAPHIC )
      wait
   End
   return      

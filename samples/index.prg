#include "./../include/sqllib.ch"
#include "./../postgres.ch"
#include "dbinfo.ch"
#IfnDef __XHARBOUR__
   #include "hbusrrdd.ch"
#else
   #include "usrrdd.ch"
#endif

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

function Main()

   HB_LANGSELECT( "PT" )

   set date       to BRITISH
   set deleted    ON
   set century    ON

   SET AUTOPEN    OFF
   SET AUTORDER   TO 1

   SQL_DEBUGINIT()
   
   SQL CONNECT ON "localhost" ;
             USER "postgres" ;
         PASSWORD "postgres" ;
         DATABASE "demo" ;
              LIB "PGSQL" ;
           SCHEMA "public" ;
             INTO nConn  

   USE netcli ALIAS cli VIA "SQLLIB" NEW
   
   IF !Index( "cli" )
      CLEAR INDEXES 
      
      /* CDX Style */
      INDEX ON CLI_ABREV                TAG ID1 TO CLI 
      INDEX ON CLI_NOME                 TAG ID2 TO CLI 
      INDEX ON CLI_CGC                  TAG ID3 TO CLI 
      INDEX ON dtos(CLI_ANIV)           TAG ID4 TO CLI 
      INDEX ON CLI_KEY                  TAG ID5 TO CLI 
      INDEX ON CLI_END                  TAG ID6 TO CLI
      INDEX ON CLI_ANIV + CLI_NOME      TAG ID6 TO CLI
      INDEX ON cli_abrev+dtos(cli_aniv) TAG ID7 TO X
      CLOSE INDEXES
   End
   
   SET INDEX TO cli
   SET ORDER TO 5
   
   msginfo( indexord() )
   msginfo( indexkey() )
   quit
   
   
   go bottom
   set order to TAG 'id6'
   GO TOP
   GO BOTTOM
   
   set order to TAG 'id4'
   GO TOP
   GO BOTTOM
   QUIT
 
CLEAR INDEXES 

/* CDX Style */
INDEX ON CLI_ABREV                TAG ID1 TO CLI 
INDEX ON CLI_NOME                 TAG ID2 TO CLI 
INDEX ON CLI_CGC                  TAG ID3 TO CLI 
INDEX ON dtos(CLI_ANIV)           TAG ID4 TO CLI 
INDEX ON CLI_KEY                  TAG ID5 TO CLI 
INDEX ON CLI_END                  TAG ID6 TO CLI
INDEX ON CLI_ANIV + CLI_NOME      TAG ID6 TO CLI
INDEX ON cli_abrev+dtos(cli_aniv) TAG ID7 TO X
quit

SET INDEX TO cli

CLS
? 'order info'
? '-----'
SET ORDER TO 1
? IndexKey(), indexord()
SET ORDER TO TAG id1
? IndexKey(), indexord()
SET ORDER TO TAG id3
? IndexKey(), indexord()
wait

CLS
? 'indexkey:', IndexKey()
? '-----'
FOR i := 1 TO 10
   ? '#' + StrZero( i, 2 ), IndexKey(i), indexord(i), indexkey()
End
wait


cls
? 'indexkey:', IndexKey()
? 'go top'
GO TOP
? '-----'
FOR i := 1 TO 5                       
   ? cli_abrev, cli_nome, cli_aniv
   skip 
End

wait
quit
 
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

//--EOF--//

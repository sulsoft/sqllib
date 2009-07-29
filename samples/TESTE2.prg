*#include "FiveWin.ch"

#include "SQLLIB.ch"
#include "postgres.ch"
#include "hbusrrdd.ch"
#include "dbinfo.ch"
#include "simpleio.ch"

#define SQL_DEBUG
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

REQUEST DBFCDX

#define RDD    "DBFCDX"
#define RDD    "SQLLIB"

*#define CRIAR_DADOS

function Main()

   HB_LANGSELECT( "PT" )

   set date       to BRITISH
   set deleted    ON
   set century    ON

   SET AUTOPEN    OFF
   SET AUTORDER   TO 1

/*   
   a := { "Dom P'ablo", 1.23, DATE(), 1==1, nil, {|| 1 } }
   
   for i := 1 to len(a)
       ? str(i,1) +': ' + SQLITEM2STR(a[i],2)
   end
quit   
/**/   

xmsgstop( SL_GETCONNPARAMS()[1] + CRLF + ;
         SL_GETCONNPARAMS()[2] + CRLF + ;
         SL_GETCONNPARAMS()[3] + CRLF + ;
         SL_GETCONNPARAMS()[4] )
         
  SQL CONN PARAMS TO HOST "localhost" ;
                     USER "postgres" ;
                 PASSWORD "postgres" ;
                      VIA "PGSQL"

xmsgstop( SL_GETCONNPARAMS()[1] + CRLF + ;
         SL_GETCONNPARAMS()[2] + CRLF + ;
         SL_GETCONNPARAMS()[3] + CRLF + ;
         SL_GETCONNPARAMS()[4] )

xmsgstop( "vou criar o database 'demo1'" + CRLF + ;
         "Ele existe ?: " + SL_ToString( SL_DATABASE( "demo1" ) ) )

   SQL CREATE DATABASE "demo1" ;
           USER "postgres" ;
       PASSWORD "postgres" ;
            LIB "PGSQL" INTO lRet

if !lRet
   xmsgstop( "O Banco de dados <demo1> não foi criado" )
endif

xmsgstop( "O database 'demo1' foi criado ?: " + SL_ToString( SL_DATABASE( "demo1" ) ) + " <- Aqui tinha que retornar .T. :(" )

xmsgstop( "vou criar o database 'demo2'" + CRLF + ;
         "Ele existe ?: " + SL_ToString( SL_DATABASE( "demo2" ) ) )

   SQL CREATE DATABASE "demo2" INTO lRet

if !lRet
   xmsgstop( "O Banco de dados <demo2> não foi criado" )
endif

xmsgstop( "O database 'demo2' foi criado ?: " + SL_ToString( SL_DATABASE( "demo2" ) ) + " <- Aqui tinha que retornar .T. :(" )

xmsgstop( "agora vou apagar o database 'demo1'" )

   SQL DELETE DATABASE "demo1" ;
           USER "postgres" ;
       PASSWORD "postgres" ;
            LIB "PGSQL"

xmsgstop( "O database 'demo1' existe ?: " + SL_ToString( SL_DATABASE( "demo1" ) ) + " <- Aqui tinha que retornar .F. :)" )

xmsgstop( "agora vou apagar o database 'demo2'" )

   SQL DELETE DATABASE "demo2"

xmsgstop( "O database 'demo2' existe ?: " + SL_ToString( SL_DATABASE( "demo2" ) ) + " <- Aqui tinha que retornar .F. :)" )

if !SL_DATABASE( "demo" ) && Nao poderia ter entrado aqui pois o banco de dados existe.

   xmsgstop( "vou criar o banco de dados 'demo'" + CRLF + ;
            "Nao poderia ter entrado aqui pois o banco de dados existe." )
         
   SQL CREATE DATABASE "demo" ;
           USER "postgres" ;
       PASSWORD "postgres" ;
            LIB "PGSQL"
endif

   SQL CONNECT ON "localhost" ;
             USER "postgres" ;
         PASSWORD "postgres" ;
         DATABASE "demo" ;
              LIB "PGSQL" ;
           SCHEMA "public" ;
             INTO nConn  
xmsgstop( SQLLIB_GETCONN() )

   IF (nConn == 00)
      xmsgerror( "Falha na conexao:;;" + SQLERR_MSG() )
      QUIT
   End

xmsgstop( "vou criar teste1" )

   SQL CREATE DATABASE "teste1"    ;
                  HOST "localhost" ;
                  USER "postgres" ;
              PASSWORD "postgres" ;
                SCHEMA "public" ;
                   LIB "PGSQL" ;
                  INTO lCreate

xmsgstop( "O banco de dados 'teste1' foi criado ? " + SL_ToString( lCreate ) )

xmsgstop( "porque que aqui está chegando 0 (zero) ?: " + SL_ToString( SQLLIB_GETCONN() ) )

   cTB  := 'm4'
   
   IF .t.
#ifdef CRIAR_DADOS
      DBCreate( cTB, {;
                      { "cod" ,  "c", 4, 0 },;
                      { "desc",  "c", 3, 0 },;
                      { "date",  "d",10, 0 },;
                      { "valor", "n",13, 3 },;
                      { "status","c", 1, 0 } ;
                      }, RDD )
#endif
      USE (cTB) ALIAS cli VIA RDD NEW
      
   SQL_DEBUGINIT( .T. )
      INDEX ON cod                              TAG ID1 TO (cTB)
      INDEX ON desc                             TAG ID2 TO (cTB)
      INDEX ON date                             TAG ID3 TO (cTB)
      INDEX ON cod + desc                       TAG ID4 TO (cTB)
      INDEX ON cod + dtos(date)                 TAG ID5 TO (cTB)
      INDEX ON str(valor) + dtos(date)          TAG ID6 TO (cTB)
      INDEX ON status + cod + desc              TAG ID7 TO (cTB)
      INDEX ON cod + desc + status              TAG ID8 TO (cTB)
      CLOSE INDEXES
   SQL_DEBUGINIT( .F. )

      SET INDEX TO (cTB)
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
   End

   USE (cTB) ALIAS cli VIA RDD NEW

   SET INDEX TO (cTB)
   SET ORDER TO 7

   SQL_DEBUGINIT( .T. )
   *bGoTop()
   DbGoto( 2 )
   CLS
   
   ? 'indexkey: ', IndexKey()
   ? '*********************'

   WHILE !Eof()         
         DEBUG recno(), status, cod, desc,eof(), bof(), found()
         ?     recno(), status, cod, desc,eof(), bof(), found()
         skip
   End
   SQL_DEBUGINIT( .F. )
   ? '*********************'


QUIT

















   IF !SL_Table( "m3" )
      xmsginfo( "Vamos criar a tabela M2" )
      DBCreate( "m3", {;
                      { "cod" , "c", 4, 0 },;
                      { "desc", "c", 3, 0 } ;
                      }, "SQLLIB" )
   End
   
   SET PACKETSIZE TO 5
   USE m3 ALIAS cli VIA "SQLLIB" NEW   
   *CLEAR INDEXES 
   
   IF !SL_Index( "m3" )
      INDEX ON cod                      TAG ID1 TO m3
      INDEX ON desc                     TAG ID2 TO m3
      CLOSE INDEXES
   End
   
   SET INDEX TO m3
   SET ORDER TO 1
   
   IF LastRec() < 100
      FOR I := 1 TO 100
          DbAppend()
          FieldPut(1, STRZERO( i, 4 ) )
          FieldPut(2, 'CCC' )  

          DbAppend()
          FieldPut(1, STRZERO( i, 4 ) )
          FieldPut(2, 'AAA' )  

          DbAppend()
          FieldPut(1, STRZERO( i, 4 ) )
          FieldPut(2, 'BBB' )  
      End
      DbCommit()
   End
   
   DbGoTop()
   ? indexord(), indexkey()
   CLS
   
   WHILE !Eof()
         ? recno(), cod, desc,eof(), bof()//   , found()  
         skip
   End

//   DbGoBottom()   
//   WHILE !Bof()
//         ? recno(), cod
//         skip -1
//   End
   
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
 
*********************
function GetErrorInfo( oError )
********************* 

   local cInfo := "", n

   cInfo += "Description: " + oError:Description + Chr( 13 ) + Chr( 10 )
   cInfo += "GenCode: " + AllTrim( Str( oError:genCode ) ) + Chr( 13 ) + Chr( 10 )
   cInfo += "Operation: " + oError:Operation + Chr( 13 ) + Chr( 10 )
   
   if valtype( oError:Filename ) == 'C' .AND. !EMPTY( oError:Filename )
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
   ErrorBlock( { | oError | xmsgError( GetErrorInfo( oError ), "Error" ), __Quit() } )
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


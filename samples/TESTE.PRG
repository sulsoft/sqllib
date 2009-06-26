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

REQUEST DBFCDX

#define  RDD    "DBFCDX"
#define RDD    "SQLLIB"

#define CRIAR_DADOS

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

msgstop( SL_GETCONNPARAMS()[1] + CRLF + ;
         SL_GETCONNPARAMS()[2] + CRLF + ;
         SL_GETCONNPARAMS()[3] + CRLF + ;
         SL_GETCONNPARAMS()[4] )
         
  SQL CONN PARAMS TO HOST "localhost" ;
                     USER "postgres" ;
                 PASSWORD "postgres" ;
                      VIA "PGSQL"

msgstop( SL_GETCONNPARAMS()[1] + CRLF + ;
         SL_GETCONNPARAMS()[2] + CRLF + ;
         SL_GETCONNPARAMS()[3] + CRLF + ;
         SL_GETCONNPARAMS()[4] )

msgstop( "vou criar o database 'demo'" + CRLF + ;
         "Ele existe ?: " + SL_ToString( SL_DATABASE( "demo" ) ) )

   SQL CREATE DATABASE "demo" INTO lRet

msgstop( "Conseguiu  criar 'demo' ? " + SL_ToString( lRet ) )

   SQL CONNECT DATABASE "demo" ;
                 SCHEMA "public" ;
                   INTO nConn  

   IF (nConn == 00)
      msgerror( "Falha na conexao:;;" + SQLERR_MSG() )
      QUIT
   End

   cTB  := 'm4'
   
*  IF .f.
#ifdef CRIAR_DADOS
      DBCreate( cTB, {;
                      { "cod" ,  "c", 4, 0 },;
                      { "desc",  "c", 3, 0 },;
                      { "date",  "d",10, 0 },;
                      { "valor", "n",13, 3 },;
                      { "status","c", 1, 0 } ;
                      }, RDD )
#endif
      USE (cTB) ALIAS cli VIA RDD NEW exclusive
zap

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
*  End

   SET PACKETSIZE TO 12
   USE (cTB) ALIAS cli VIA RDD NEW

   SET INDEX TO (cTB)
   SET ORDER TO 7

   SQL_DEBUGINIT( .T. )
   *DbGoTop()
   DbGoto( 7 )
   CLS

   ? 'Testando movimenta‡Æo simples dos registros...'
   SHOWRECNO()
   SKIP +1
   SHOWRECNO()
   SKIP +2
   SHOWRECNO()
   SKIP +3
   SHOWRECNO()
   SKIP +4
   SHOWRECNO()
   SKIP +5
   
   SHOWRECNO()
   SKIP -5
   SHOWRECNO()
   SKIP -4
   SHOWRECNO()
   SKIP -3
   SHOWRECNO()
   SKIP -2
   SHOWRECNO()
   SKIP -1
   SHOWRECNO()
   
   wait                   
   cls
   
   ? 'Testando movimenta‡Æo aleatoria'
   ? 'indexkey: ', IndexKey()
   ? '*********************'
   
   _t := 15
   
   for i := 1 to _t         
         SHOWRECNO()
         skip
   End

   ? '***'
   SHOWRECNO()
   ? '***'
   
   for i := 1 to _t         
         SHOWRECNO()
         skip-1
   End
   SQL_DEBUGINIT( .F. )

   ? '*********************'
   SHOWRECNO()
   ? '***'
   
   wait
   cls
   ? 'Testando o do while'
   ? 'indexkey: ', IndexKey()
   ? '*********************'
   
   go top
   while !eof()
         SHOWRECNO()
         skip
   End

   ? '*********************'

   wait
   cls
   go top
   browse()
   quit
   

   IF !SL_Table( "m3" )
      msginfo( "Vamos criar a tabela M2" )
      DBCreate( "m3", {;
                      { "cod" , "c", 4, 0 },;
                      { "desc", "c", 3, 0 } ;
                      }, "SQLLIB" )
   End
   
   SET PACKETSIZE TO 5
   USE m3 ALIAS cli VIA "SQLLIB" NEW   
   *CLEAR INDEXES 
   
   IF !SL_Indexe( "m3" )
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

FUNCTION SHOWRECNO()
   DEBUG recno(), status, cod, desc,eof(), bof(), found()
   ?     recno(), status, cod, desc,eof(), bof(), found()
   RETURN


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
   
   return nil

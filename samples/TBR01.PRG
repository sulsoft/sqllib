// Include Header Files
#include "inkey.ch"
#include "setcurs.ch"

#include "SQLLIB.ch"

REQUEST SQLLIB
REQUEST PGSQL

REQUEST DBFCDX
REQUEST HB_LANG_PT

#define     COLSEP     CHR(32)  + CHR(179) + CHR(32)
#define     HEADSEP    CHR(205) + CHR(209) + CHR(205)
#define     FOOTSEP    CHR(205) + CHR(207) + CHR(205)

FUNCTION Main( cRDD )

   LOCAL b, column, nKey, cScreen, nConn, lRet

   cScreen := savescreen()
   
   if valtype( cRdd ) != "C" .or. .not. upper(cRdd) $ [CDX,SQL]
      clear screen
      @10, 01 say "Para executar este exemplo, voce precisa escolher uma das duas opcoes:"
      @12, 01 say "Ex: TBR01 CDX  -> executa este exemplo usando o driver RDDCDX"
      @13, 01 say "Ex: TBR01 SQL  -> executa este exemplo usando o driver SQLLIB"
      quit
   endif

   cRdd := upper(cRdd)
   
   HB_LANGSELECT( "PT" )

   SET SCOREBOARD OFF
   SET CONFIRM    ON
   set date       to BRITISH
   set deleted    ON
   set century    ON
   SET AUTOPEN    OFF
   SET AUTORDER   TO 1

   CRIA_BD( "CDX" )
   
   if cRdd == "SQL"
      SQL CONN PARAMS TO HOST "localhost" ;
                         USER "postgres"  ;
                     PASSWORD "postgres"  ;
                          VIA "PGSQL"
      if !SL_DATABASE( "demosqllib" )
         SQL CREATE DATABASE "demosqllib" INTO lRet
         IF !lRet
            msgstop( "Não foi possível criar a tabela <demosqllib>" )
            QUIT
         EndIF
      endif
      SQL CONNECT DATABASE "demosqllib" ;
                    SCHEMA "public" ;
                      INTO nConn  
      IF nConn = 0
         msgerror( "Falha na conexao:;;" + SQLERR_MSG() )
         QUIT
      EndIF

      CRIA_BD( "SQL" )

**      if !SL_FILE( "test" )
         SQL IMPORT DBF { "test.dbf" } VIA "DBFCDX" PACK
**      endif
      SL_DELETEINDEX( "_tmp" )
      USE test ALIAS "test" via "SQLLIB" exclusive new
   else
      USE test ALIAS "test" via "DBFCDX" exclusive new
   endif

   index on str(fld1,15,3) to _tmp

*  SETCURSOR(SC_NONE)

   SETCOLOR("N/W")

   @ 2, 6 TO 21,72 DOUBLE COLOR "W+/BG"

   b:= TBrowseDB( 3, 7, 20, 71 )

   b:colorSpec := "W+/BG,N/W,W/N,N,GR+/W,N/BG,B+/BG,GR+/BG"

   b:colSep  := COLSEP
   b:headSep := HEADSEP
   b:footSep := FOOTSEP

   // TBColumn objects
   column := TBColumnNew( "Sequence", {|| test->SEQU} )
   column:footing := "SEQU"
   b:addColumn( column )

   column := TBColumnNew( "Field 1", {|| test->fld1} )
   column:footing := "First"
   b:addColumn( column )

   column := TBColumnNew( "Field 2", {|| test->fld2} )
   b:addColumn( column )

   column := TBColumnNew( "Field 3", {|| test->fld3} )
   b:addColumn( column )

   column := TBColumnNew( "Field 4", {|| test->fld4} )
   column:footing := "Last;Column"
   b:addColumn( column )

   b:freeze := 1

   DO WHILE .T.

      @22, 00 say pad( "[ESC] Encerrar       [DEL] Deletar Registro         [INS] Inserir Registro", 80 )

      IF b:colPos <= b:freeze
         b:colPos := b:freeze + 1
      ENDIF

      ForceStable( b )

      IF b:stable
         IF b:hitTop .OR. b:hitBottom
            TONE(87.3,1)
         ENDIF
         nKey := INKEY(0)
         // Process key
         IF !TBMoveCursor( b, nKey )
            IF     nKey == K_ESC
                   EXIT
            elseif nKey == K_INS
                   TEST->( dbappend() )
            ELSEIF nKey == K_DEL
                   TEST->( dbdelete() )
            ENDIF
         ENDIF
      ENDIF

   ENDDO

   restscreen(,,,,cScreen)

   dbcloseall()

   if cRdd == "SQL"
      SQL DISCONNECT FROM nConn
   endif

   SETCURSOR(SC_NORMAL)
   ? " "
   
RETURN NIL

/*****
 *
 * Cursor Movement Methods
 *
 */

STATIC FUNCTION TBMoveCursor( o, nKey )

   LOCAL nFound
   STATIC aKeys := { K_DOWN      , {|obj| obj:down()},;
                     K_UP        , {|obj| obj:up()},;
                     K_PGDN      , {|obj| obj:pageDown()},;
                     K_PGUP      , {|obj| obj:pageUp()},;
                     K_CTRL_PGUP , {|obj| obj:goTop()},;
                     K_CTRL_PGDN , {|obj| obj:goBottom()},;
                     K_RIGHT     , {|obj| obj:right()},;
                     K_LEFT      , {|obj| obj:left()},;
                     K_HOME      , {|obj| obj:home()},;
                     K_END       , {|obj| obj:end()},;
                     K_CTRL_LEFT , {|obj| obj:panLeft()},;
                     K_CTRL_RIGHT, {|obj| obj:panRight()},;
                     K_CTRL_HOME , {|obj| obj:panHome()},;
                     K_CTRL_END  , {|obj| obj:panEnd()} }

   nFound := ASCAN( aKeys, nKey )

   IF nFound != 0
      EVAL( aKeys[++nFound], o )
   ENDIF

RETURN nFound != 0

STATIC FUNCTION ForceStable( obj )

   obj:refreshall()

   DISPBEGIN()
      DO WHILE !obj:stabilize()
      ENDDO
   DISPEND()

RETURN (NIL)

***********************
static function CRIA_BD( cRdd )
***********************

local aStruct := { }, n

* Aqui criamos a Tabela dentro do banco de dados, já com as caracteristicas "NOT NULL", "UNIQUE", "PRIMARY_KEY"

SQL ADD FIELD "SEQU" "N" (15, 0) NOT NULL UNIQUE PRIMARY_KEY INTO aStruct
SQL ADD FIELD "FLD1" "N" (15, 3) NOT NULL UNIQUE             INTO aStruct
SQL ADD FIELD "FLD2" "C" (30, 0) NOT NULL UNIQUE             INTO aStruct
SQL ADD FIELD "FLD3" "D" (08, 0) NOT NULL UNIQUE             INTO aStruct
SQL ADD FIELD "FLD4" "L" (01, 0) NOT NULL UNIQUE             INTO aStruct

**msgstop( SL_ToString( astruct,,,, "DUMP.TXT" ) )

if cRdd == "SQL"
   if SL_TABLE( "test" )
      SL_DELETETABLE( "test" )
   endif
   dbcreate( "test", aStruct, "SQLLIB" )

   USE test ALIAS "TEST" via "SQLLIB" exclusive new

msgstop( SL_ToString( TEST->( dbStruct()        ), , , , "dump.txt" ), "Usando DbStruct()" )
msgstop( SL_ToString( SL_DBSTRUCT( "test"       ), , , , "dump.txt" ), "Usando SL_DbStruct()" )
msgstop( SL_ToString( SL_DBSTRUCT( "test",, .T. ), , , , "dump.txt" ), "Usando SL_DbStruct() com retorno <EXTEND>" )
   
   TEST->( dbclosearea() )
else
   dbcreate( "test", aStruct, "DBFCDX" )
   USE test ALIAS "TEST" via "DBFCDX" exclusive new
   for n = 1 to 100
       TEST->( dbappend() )
       TEST->SEQU := n
       TEST->FLD1 := hb_random( 1, 1000 ) * .03
       TEST->FLD2 := "Registro numero " + alltrim(str(n,3))
       TEST->FLD3 := date() + n
       TEST->FLD4 := iif( n < 50, .T., .F. )
   next
   TEST->( dbclosearea() )
endif

return NIL

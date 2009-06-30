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
   LOCAL b, column, nKey, aColors, cScreen

   cScreen := savescreen()
   
   if valtype( cRdd ) != "C" .or. .not. upper(cRdd) $ [CDX,SQL]
      clear screen
      @10, 01 say "Para executar este exemplo, voce precisa escolher uma das duas opcoes <CDX> ou <SQL>:"
      @12, 01 say "Ex: TBR01 CDX  -> executa este exemplo usando o driver RDDCDX"
      @13, 01 say "Ex: TBR01 SQL  -> executa este exemplo usando o driver SQLLIB"
      quit
   endif

   cRdd := upper(cRdd)
   
   SET SCOREBOARD OFF
   SET CONFIRM    ON
   HB_LANGSELECT( "PT" )

   set date     to BRITISH
   set deleted  ON
   set century  ON

   SET AUTOPEN  OFF
   SET AUTORDER TO 1

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
      if !SL_FILE( "test" )
         aFiles := { "test.dbf" }
         SQL IMPORT DBF aFiles VIA "DBFCDX" PACK INTO lRet
      else
         lRet := .T.
      endif
      USE test ALIAS "test" via "SQLLIB" exclusive
      SL_DELETEINDEX( "_tmp" )
   else
      USE test ALIAS "test" via "DBFCDX" exclusive
   endif

   index on fld2 to _tmp

*  SETCURSOR(SC_NONE)

   SETCOLOR("N/W")
   SCROLL()
   @ 2, 6 TO 22,72 DOUBLE COLOR "W+/BG"

   b:= TBrowseDB( 3, 7, 21, 71 )

   b:colorSpec := "W+/BG,N/W,W/N,N,GR+/W,N/BG,B+/BG,GR+/BG"

   b:colSep  := COLSEP
   b:headSep := HEADSEP
   b:footSep := FOOTSEP

   // TBColumn objects
   column := TBColumnNew( "Field 1", {|| test->fld1} )
   column:footing := "First"
   b:addColumn( column )
   column := TBColumnNew( "Field 2", {|| test->fld2} )
   b:addColumn( column )
   column := TBColumnNew( "Field 3", {|| test->fld3} )
   b:addColumn( column )
   column := TBColumnNew( "Field 4", {|| test->fld4} )
   b:addColumn( column )
   column := TBColumnNew( "Field 5", {|| test->fld5} )
   column:colorBlock := { |y| iif(y <= 3, { 6, 2 }, IF(y >= 5, { 7, 2 }, { 8, 2 } )) }
   column:footing := "Last;Column"
   b:addColumn( column )

   b:freeze := 1

   DO WHILE .T.

      IF ( b:colPos <= b:freeze )
         b:colPos := b:freeze + 1
      ENDIF

      DO WHILE ( !b:stabilize() )
      ENDDO

      IF ( b:stable )
         IF ( b:hitTop .OR. b:hitBottom )
            TONE(87.3,1)
            TONE(40,3.5)

         ENDIF
         DO WHILE( (nKey := INKEY(.5) ) == 0 )
            @  0, 70 SAY " " + TIME() + " "
         ENDDO

      ENDIF

      // Process key
      IF !TBMoveCursor( b, nKey )
         IF     ( nKey == K_ESC )
                SCROLL()
                EXIT
         elseif nKey = K_INS
                TEST->( dbappend() )
                ForceStable( b )
                TEST->( dbgoto( TEST->( recno() ) ) )
         ELSEIF nKey = K_DEL
                TEST->( dbdelete() )
                ForceStable( b )
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
   STATIC aKeys := ;
       { K_DOWN      , {|obj| obj:down()},;
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
   IF (nFound != 0)
      EVAL( aKeys[++nFound], o )

   ENDIF

RETURN (nFound != 0)

STATIC FUNCTION ForceStable( obj )

   obj:refreshall()
   DISPBEGIN()
   DO WHILE !obj:stabilize()
   ENDDO
   DISPEND()

RETURN (NIL)

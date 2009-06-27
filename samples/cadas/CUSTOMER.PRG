#include "FiveWin.ch"
#include "xbrowse.ch"
#include "Customer.ch"

#include "SQLLIB.ch" 

REQUEST SQLLIB
REQUEST PGSQL
REQUEST DBFCDX
REQUEST HB_LANG_PT

static oWnd, oClients, oClient, oName, oAddress, oState, oSalary
static cName, oBrw, cAddress, cState, nSalary
static oBtnUp, oBtnDn, oBtnEd, oBtnCa, oBtnRe, oBtnNe, oBtnDe

//----------------------------------------------------------------------------//

function Main()

   local oBar, aFiles, lRet, nServer, nDb

   HB_LANGSELECT( "PT" )

   set date     to BRITISH
   set deleted  ON
   set century  ON

   SET AUTOPEN  OFF
   SET AUTORDER TO 1

   SET _3DLOOK  ON

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
      aFiles := { "Customer.dbf", "sales.dbf" }
      SQL IMPORT DBF aFiles VIA "DBFCDX" PACK INTO lRet
   else
      lRet := .T.
   endif

   USE Customer ALIAS "customer" via "SQLLIB" exclusive

*   SQL_DEBUGINIT( .T. )
   index on FIRST to _TMP
*   SQL_DEBUGINIT( .F. )

   USE sales alias "sales" via "SQLLIB" NEW exclusive

   SELECT customer

   SQL_DEBUGINIT( .T. )
   customer->( browse() )
   SQL_DEBUGINIT( .F. )


   DEFINE WINDOW oWnd TITLE "Reporting tools" MDI ;
     MENU BuildMenu() COLOR "N/W"

   DEFINE BUTTONBAR oBar OF oWnd SIZE 60, 60 2007

   DEFINE BUTTON OF oBar ACTION MsgInfo( "Click" ) ;
      FILENAME "\fwh\bitmaps\attach.bmp" PROMPT "Attach"

   DEFINE BUTTON OF oBar ACTION MsgInfo( "Click" ) ;
      FILENAME "\fwh\bitmaps\calendar.bmp" PROMPT "Calendar"

   DEFINE BUTTON OF oBar ACTION MsgInfo( "Click" ) ;
      FILENAME "\fwh\bitmaps\people2.bmp" PROMPT "Clients"

   DEFINE BUTTON OF oBar ACTION MsgInfo( "Click" )

   SET MESSAGE OF oWnd TO "Testing the FiveWin Report Class" CENTERED

   ACTIVATE WINDOW oWnd ;
      VALID MsgYesNo( "Do you want to end?" )

   dbcloseall()

   SQL DISCONNECT FROM nServer

return nil

//----------------------------------------------------------------------------//

function BuildMenu()

   local oMenu

   MENU oMenu
      MENUITEM "&DataBases"
      MENU
         MENUITEM "&Clients..." ACTION  BrwClients() ;
            MESSAGE "Clients management"

         MENUITEM "&End" ACTION oWnd:End() ;
            MESSAGE "End this test"

      ENDMENU

      oMenu:AddMdi()              // Add standard MDI menu options

   ENDMENU

return oMenu

//----------------------------------------------------------------------------//

function BrwClients()

   local oIco, oBar

   if oClients != nil
      return nil
   endif

   DEFINE ICON oIco FILENAME "\fwh\icons\customer.ico"

   DEFINE WINDOW oClients TITLE "Clients management" MDICHILD ICON oIco

   DEFINE BUTTONBAR oBar OF oClients

   DEFINE BUTTON OF oBar ACTION ShowClient()

*   @ 3, 0 XBROWSE oBrw OF oClients ALIAS "customer" AUTOCOLS LINES CELL && FASTEDIT

*    @ 3, 0 XBROWSE oBrw of oClients alias alias() AUTOCOLS
/*
   oBrw := TXBrowse():New( oClients )
   oBrw:SetRDD()
   oBrw:nTop                     := 05
   oBrw:nLeft                    := 05
   oBrw:nBottom                  := 120
   oBrw:nRight                   := 100
   oBrw:nColDividerStyle         := LINESTYLE_BLACK
   oBrw:nRowDividerStyle         := LINESTYLE_BLACK
   oBrw:lColDividerComplete      := .T.
   oBrw:nMarqueeStyle            := MARQSTYLE_HIGHLCELL
   oBrw:lHScroll                 := .F.
   oBrw:lVScroll                 := .T.
   oBrw:lAllowRowSizing          := .F.
   oBrw:lAllowColSwapping        := .F.
   oBrw:lAllowColHiding          := .F.
   oBrw:nHeaderLines             := 2
   oBrw:nRowHeight               := 22
   oBrw:lRecordSelector          := .F.
   oBrw:bKeyChar                 := { | nKey | If( nKey == VK_ESCAPE, oClients:End(), NIL ) }   
   oBrw:CreateFromCode()
   oClients:oClient := oBrw
*/

   @ 2, 0 LISTBOX oBrw FIELDS OF oClients ;
      SIZE 500, 500 ;
      ON CHANGE ChangeClient()

   oClients:SetControl( oBrw )
   
   dbgotop()
   
   ACTIVATE WINDOW oClients VALID( oClients := nil, .t. )        // We destroy the object

return nil

//----------------------------------------------------------------------------//

function ShowClient()

   local oIco

   nSalary := 0

   if oClient != nil
      return nil
   endif

   DEFINE ICON oIco FILENAME "\fwh\icons\Person.ico"

   DEFINE DIALOG oClient RESOURCE "CLIENT" ;
      ICON oIco

   REDEFINE SAY ID 3 OF oClient
   REDEFINE SAY ID 4 OF oClient
   REDEFINE SAY ID 5 OF oClient
   REDEFINE SAY ID 6 OF oClient

   REDEFINE GET oName    VAR cName    ID ID_NAME    OF oClient
   REDEFINE GET oAddress VAR cAddress ID ID_ADDRESS OF oClient
   REDEFINE GET oState   VAR cState   ID ID_STATE   OF oClient
   REDEFINE GET oSalary  VAR nSalary  ID ID_SALARY  PICT "@E 999999.99" OF oClient
   
   REDEFINE BUTTON oBtnUp ID ID_PREV OF oClient ACTION GoReg( "UP" )

   REDEFINE BUTTON oBtnDn ID ID_NEXT OF oClient ACTION GoReg( "DOWN" )

   REDEFINE BUTTON oBtnEd ID ID_EDIT OF oClient ACTION EDIT()

   REDEFINE BUTTON oBtnCa ID ID_CANC OF oClient ACTION CANCEL()

   REDEFINE BUTTON oBtnRe ID ID_RECO OF oClient ACTION RECORD()

   REDEFINE BUTTON oBtnNe ID ID_NEW  OF oClient ACTION NEWREG()

   REDEFINE BUTTON oBtnDe ID ID_DEL  OF oClient ACTION DELREG()

   SELECT Sales     // We select Sales to properly initialize the Browse

   REDEFINE LISTBOX FIELDS ID ID_SALES OF oClient

   CANCEL()
   
   ACTIVATE DIALOG oClient CENTERED NOWAIT ;
      VALID ( oClient := nil, .t. )           // Destroy the object

   SELECT customer
   
   ChangeClient()
   
return nil

//----------------------------------------------------------------------------//

function NEWREG

   customer->( dbappend() )
   customer->( dbcommit() )
   
   oBrw:GoBottom()
   
   oBrw:refresh()

return nil

//----------------------------------------------------------------------------//

function DELREG

   msgrun( "", , { || .T. } )
   
   SQL_DEBUGINIT( .T. )
   customer->( dbdelete() )
   SQL_DEBUGINIT( .F. )
   
   customer->( dbcommit() )
   
   oBrw:refresh()
   
return nil

//----------------------------------------------------------------------------//

function EDIT

   oName:enable()
   oAddress:enable()
   oState:enable()
   oSalary:enable()

   oBtnUp:disable()
   oBtnDn:disable()
   oBtnEd:disable()
   oBtnNe:disable()
   oBtnDe:disable()
   oBtnRe:enable()
   oBtnCa:enable()

   oName:setfocus()

return nil

//----------------------------------------------------------------------------//

function CANCEL

   oName:disable()
   oAddress:disable()
   oState:disable()
   oSalary:disable()
   oBtnUp:enable()
   oBtnDn:enable()
   oBtnEd:enable()
   oBtnCa:disable()
   oBtnRe:disable()
   oBtnNe:enable()
   oBtnDe:enable()
   
return nil

//----------------------------------------------------------------------------//

function RECORD

   customer->First  := cName
   customer->Street := cAddress
   customer->State  := cState
   customer->salary := nSalary
       
   oBrw:DrawSelect()
   
   CANCEL()
   
return nil

//----------------------------------------------------------------------------//

function ChangeClient()

   if oClient != nil
      cName := customer->First
      oName:Refresh()
      cAddress := customer->Street
      oAddress:Refresh()
      cState := customer->State
      oState:Refresh()
      oSalary:varput( customer->salary )
      oSalary:Refresh()
   endif

return nil

//----------------------------------------------------------------------------//

function GoReg( cTipo )

   msgrun( "", , { || .T. } )

   if oClients != nil
      if cTipo == "UP"
         oClients:oControl:GoUp()
      else
         oClients:oControl:GoDown()
      endif
   else
      if cTipo == "UP"
         SKIP -1
         if boF()
            GO TOP
         endif
      else
         SKIP 1
         if EoF()
            GO BOTTOM
         endif
      endif
   endif

   ChangeClient()

return nil

//----------------------------------------------------------------------------//
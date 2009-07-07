#include "sqllibrdd.ch"

/*
 * Rotina para importar DBF para o Banco de dados SQL
 * 22/01/2009 - 21:12 - Rossine
 */

************************
function SL_IMPORT_FILES( aFiles, cVia, lPack, lDelete, bBlock, nEvery, bBlockApp )
************************

local lRet := .T.
local cFile
local aStruct
local cFileSQL
local cFileDBF
local cExteDBF
local nCtd := 0

DEFAULT cVia    := rddsetdefault()
DEFAULT lPack   := .F.
DEFAULT lDelete := .F.
DEFAULT nEvery  := 1

if valtype( aFiles ) != "A"
   msgstop( "Arquivo(s) a ser(em) importado(s) não definido(s) !!!" + CRLF + CRLF + ;
            "É necessário que se passe uma <ARRAY> definida com o(s) nome(s) do(s) arquivo(s)" + CRLF + ;
            "a ser(em) importado(s) !!!", "Atenção" )
   return .F.
endif

if len( aFiles ) = 0
   msgstop( "Não há arquivos a serem importados !!!", "Atenção" )
   return .F.
endif

asort( aFiles,,,{ |x,y| upper(x) < upper(y) } )

/*
 Aqui verificamos se os arquivos DBF a serem importados existem.
 30/03/2009 - 11:20 - Rossine
*/

for each cFile in aFiles
    cFileDBF := lower(alltrim( cFile ))
    if !file( cFileDBF )
       msgstop( "Arquivo não existe [" + cFile + "]. Tecle ENTER...", "Atenção" )
       lRet := .F.
       exit
    endif
next

if !lRet
   return .F.
endif

rddsetdefault( cVia )

for each cFile in aFiles

    if valtype( bBlock ) = "B"
       nCtd++

       if nCtd >= nEvery
          #IfnDef __XHARBOUR__
          BEGIN SEQUENCE WITH {|oErr| Break( oErr )}
             Eval( bBlock )
          RECOVER

          End
          #else
          TRY
             Eval( bBlock )
          catch e
             ? e:description
          End
          #endif
          nCtd := 0
       endif
    endif
    
    cFileDBF := lower(alltrim( cFile ))
    cExteDBF := substr( cFileDBF, rat( ".", cFileDBF ) )
/*
    if !file( cFileDBF )
       msgstop( "Arquivo não existe [" + cFile + "]. Tecle ENTER...", "Atenção" )
       lRet := .F.
       exit
    endif
*/
    cFileSQL := left( cFileDBF, at( ".", cFileDBF ) - 1 )

    if rat( "\", cFileSQL ) > 0
       cFileSQL := substr( cFileSQL, rat( "\", cFileSQL ) + 1 )
    endif

    if SL_TABLE( cFileSQL )
       SL_DELETETABLE( cFileSQL )
    endif

    use ( cFileDBF ) alias "EXPORT_DBF" via (cVia) NEW exclusive

    if lPack
       EXPORT_DBF->( __dbpack() )
    endif
    EXPORT_DBF->( dbgotop() )
    
    aStruct := EXPORT_DBF->( dbStruct() )
    
    EXPORT_DBF->( dbCloseArea() )

    dbCreate( cFileSQL, aStruct, "SQLLIB" )

    use ( cFileSQL ) alias "EXPORT_SQL" via "SQLLIB" NEW exclusive
**    use ( cFileDBF ) alias "EXPORT_DBF" via (cVia) NEW exclusive

   EXPORT_SQL->( __dbzap() )

    if valtype( bBlockApp ) = "B"
       append from (cFileDBF) via (cVia) for eval( bBlockApp )
    else
       append from (cFileDBF) via (cVia)
**       copy to (cFileSQL) via "SQLLIB"
    endif

    EXPORT_SQL->( dbCloseArea() )
**    EXPORT_DBF->( dbCloseArea() )

    if lDelete
       aEval( Directory( strtran( cFileDBF, cExteDBF, "" ) + "*.*" ), {|a| Ferase( a[1] ) })
*       filedelete( strtran( cFileDBF, cExteDBF, "" ) + "*.*" )
    endif
next

dbcloseall()

rddsetdefault( "SQLLIB" )

return lRet

/*
 * Rotina para exportar SQL para DBF
 * 23/01/2009 - 09:12 - Rossine
 */

************************
function SL_EXPORT_FILES( aFiles, cVia, lPack, lDelete, bBlock, nEvery, bBlockCopy )
************************

local cFile, aExports := { }, nCtd := 0

DEFAULT aFiles  := { }
DEFAULT cVia    := rddsetdefault()
DEFAULT lPack   := .F.
DEFAULT lDelete := .F.
DEFAULT nEvery  := 1

rddsetdefault( "SQLLIB" )

if len( aFiles ) = 0
   aFiles := SQLGetTables()
endif

asort( aFiles,,,{ |x,y| upper(x) < upper(y) } )

for each cFile in aFiles

    if valtype( bBlock ) = "B"
       nCtd++
       if nCtd >= nEvery
          #IfnDef __XHARBOUR__
          BEGIN SEQUENCE WITH {|oErr| Break( oErr )}
             Eval( bBlock )
          RECOVER

          End
          #else
          TRY
             Eval( bBlock )
          catch e
             ? e:description
          End
          #endif

          nCtd := 0
       endif
    endif

    if SL_file( cFile ) .and. cFile != "sl$indexes"
       use ( cFile ) alias "EXPORT_SQL" via "SQLLIB" NEW exclusive
       if lPack
          EXPORT_SQL->( __dbpack() )
       endif

       if valtype( bBlockCopy ) = "B"
          copy to ( cFile ) via (cVia) for eval( bBlockCopy )
       else
          copy to ( cFile ) via (cVia)
       endif
       
       EXPORT_SQL->( dbCloseArea() )
       
       if lDelete
          @22, 01 say pad( "Excluindo a Tabela [" + cFile + "]. Aguarde...", 80 )
          SL_DELETETABLE( cFile )
       endif

       aadd( aExports, cFile )
    endif
next

dbcloseall()

rddsetdefault( cVia )

return aExports

//--EOF--//

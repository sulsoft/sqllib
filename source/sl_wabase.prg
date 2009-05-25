/*
 * SQLLIB RDD Project source code
 * A Free & Open source RDD for Harbour & xHarbour Compilers
 *
 * Copyright 2008 Vailton Renato <vailtom@gmail.com> and
 *                Rossine Maia   <qiinfo@ig.com.br>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */
#include "sqllibrdd.ch"

static aSystemDrivers  := NIL

FUNCTION SQLRegisterDrv( nSysID, cRddName )
   LOCAL n, aFuncs
   
   DEBUG_ARGS
   
   IF aSystemDrivers == NIL
      aSystemDrivers := array( ID_MAX_DRIVERS )
   End

   IF aSystemDrivers[ nSysID ] == NIL
      aSystemDrivers[ nSysID ] := array( ID_MAX_FUNCTIONS )
      
      aFuncs := { "SL_CREATE", ;
                  "SL_CREATEFLDS",;
                  "SL_OPEN", ;
                  "SL_GOTOID", ;
                  "SL_GOTOP", ;
                  "SL_GOBOTTOM", ;
                  "SL_DELETED", ;
                  "SL_DELETE", ;
                  "SL_RECCOUNT", ;
                  "SL_GETVALUE", ;
                  "SL_PUTVALUE", ;
                  "SL_INFO", ;
                  "SL_LOCK", ;
                  "SL_PACK", ;
                  "SL_ZAP", ;
                  "SL_ORDCREATE", ;
                  "SL_ORDDESTROY", ;
                  "SL_ORDLSTADD", ;
                  "SL_SEEK", ;
                  "SL_WRITERECORD", ;
                  "SL_ORDINFO", ;
                  "SL_EXECQUERY", ;
                  "SL_EXECQUERY_MSG", ;
                  "SL_EXECQUERY_DES", ;
                  "SL_QUICKQUERY", ;
                  "SL_COMMIT", ;
                  "SL_ROLLBACK", ;
                  "SL_CLEARINDEX", ;
                  "SL_STARTTRANS", ;
                  "SL_ENDTRANS", ;
                  "SL_GETFIELDTYPE", ;
                  "SL_GETFULLTABLE", ;
                  "SL_DELETETABLE" }

      FOR n = 1 to len(aFuncs)
          aSystemDrivers[ nSysID, n ] := __DYNSN2SYM( aFuncs[n] + "_" + cRddName )
      End
   End
   RETURN nil
   
************************
static function SL_INIT( nRDD )
************************
 
   USRRDD_RDDDATA( nRDD, nil )
 
return SUCCESS

************************
static function SL_EXIT( nRDD )  && Rossine 05/11/08
************************

   HB_SYMBOL_UNUSED( nRDD )

return SUCCESS

/*
 * Auxiliar function to RECCOUNT()
 * 24/05/2009 - 17:49:11
 */
STATIC;
FUNCTION SL_GetCurrentTime()
   RETURN VAL(  STRZERO(  Year( Date() ),4 ) + ;
                STRZERO( Month( Date() ),2 ) + ;
                STRZERO(   Day( Date() ),2 ) + ;
                STRTRAN( TIME(), ':', '' ) )
 
***********************
static function SL_NEW( nWA )
***********************

   local aWAData := Array( WA_SIZE )
   local aConn   := SL_GETCONNINFO()
   local oConn   := TSL_CONNECTION():New( aConn )

   IF valtype( aConn ) == 'U' 
      SL_ERROR( 2000, "Can't locate a client connection to any server. Use SQL CONNECTION command before to continue." )
      return FAILURE
   End

   aWAData[ WA_BOF ]             := .F.
   aWAData[ WA_EOF ]             := .F.
   aWAData[ WA_FOUND ]           := .F.
   aWAData[ WA_RECCOUNT ]        := 0
   SL_CLEAR_RECCACHE( aWAData )
 
   aWAData[ WA_BUFFER_ARR ]      := NIL
   aWAData[ WA_BUFFER_POS ]      := 0
   aWAData[ WA_BUFFER_ROWCOUNT ] := 0
   aWAData[ WA_INVALIDBUFFER ]   := .F.
   aWAData[ WA_APPEND ]          := .F.
   aWAData[ WA_RECORDCHANGED ]   := .F.

   aWAData[ WA_SCHEMA ]          := ""
   aWAData[ WA_TABLENAME ]       := ""

   aWAData[ WA_CONNECTION ]      := oConn
   aWAData[ WA_DATABASE ]        := oConn:DataBase
   aWAData[ WA_POINTER ]         := oConn:Pointer
   aWAData[ WA_SYSTEMID ]        := oConn:SystemID
   aWAData[ WA_ENGINE ]          := oConn:RddName
   aWAData[ WA_SCHEMA ]          := oConn:Schema   && Rossine 03/01/09
   aWAData[ WA_VERSION ]         := aConn[ SL_CONN_VERSION ] // Vailton - 27/04/2009 - 15:53:21

   SL_SetSchema( aWAData[ WA_SCHEMA ] )

   aWAData[ WA_PACKET_SIZE ]     := SL_PacketSize()
   
   // puxar isto de outro local, para poder alternar conforme a necessidade
   aWAData[ WA_FLD_RECNO ]       := 0
   aWAData[ WA_FLD_DELETED ]     := 0

   aWAData[ WA_TRANSCOUNTER ]    := 0
   aWAData[ WA_TABLETYPE    ]    := TS_SINGLE_SQL
   
   // Current indexes implementation 
   aWAData[ WA_INDEX ]           := {}
   aWAData[ WA_INDEX_CURR ]      := 00 // default set order to 0  

   USRRDD_AREADATA( nWA, aWAData )
   return SUCCESS

********************************************************************************
* Lembre-se que este arquivo .PRG deve se manter independente do banco de dados
* em uso. Ou seja, o módulo que  escrevermos deve tratar todas as chamadas e só
* me retornar os resultado como .T. / .F.
* 18/12/2008 - 13:08:55
********************************************************************************
**************************
static function SL_CREATE( nWA, aOpenInfo )  && XBASE - DBCREATE()
**************************

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nResult

   aWAData[ WA_TABLENAME ] := aOpenInfo[ UR_OI_NAME ]
   
   nResult := HB_ExecFromArray( { FSL_CREATE( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, aOpenInfo } )

   return nResult
 
********************************
static function SL_CREATEFIELDS( nWA, aStruct )
********************************

   LOCAL aWAData := USRRDD_AREADATA( nWA )
                 
   HB_ExecFromArray( { FSL_CREATEFLDS( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, aStruct } )

   return SUCCESS
 
************************
static function SL_OPEN( nWA, aOpenInfo )  && XBASE - DBUSEAREA()
************************

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL aField, nResult, nTotalFields, n
   local s_aStruct

   nResult := HB_ExecFromArray( { FSL_OPEN( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, aOpenInfo  } )

   IF nResult != SUCCESS
      RETURN nResult
   End

   /* Check system fields */
   IF aWAData[ WA_TABLETYPE ] == TS_SINGLE_SQL .and. aWAData[ WA_FLD_RECNO ] == 0
      ERROR_NO_PK(aWAData)
      RETURN FAILURE
   End

   s_aStruct              := aWAData[ WA_STRUCT ]
   aWAData[ WA_BOF ]      := aWAData[ WA_EOF ] := .F.
   aWAData[ WA_RECSIZE ]  := 0  && Rossine  22/10/08
   aWAData[ WA_FCOUNT ]   := Len( s_aStruct ) // Hasta que hagamos una conexion real
   aWAData[ WA_ALIAS ]    := aOpenInfo[ UR_OI_ALIAS ]  && Rossine - 
   nTotalFields           := aWAData[ WA_FCOUNT ]

   UR_SUPER_SETFIELDEXTENT( nWA, nTotalFields )
 
   FOR n := 1 TO nTotalFields
       aField := ARRAY( UR_FI_SIZE )
       aField[ UR_FI_NAME ]    := s_aStruct[ n, DBS_NAME ]
**     aField[ UR_FI_TYPE ]    := s_aStruct[ n, DBS_FIELD_TYPE ]
**     aField[ UR_FI_TYPE ]    := SL_GETFIELDTYPE( s_aStruct[ n, 2 ] )
       aField[ UR_FI_TYPE ]    := HB_ExecFromArray( { FSL_GETFIELDTYPE( aWAData[ WA_SYSTEMID ] ), s_aStruct[ n, 2 ] } )
       aField[ UR_FI_TYPEEXT ] := s_aStruct[ n, DBS_TYPE ] &&  0   Rossine 22/10/08
       aField[ UR_FI_LEN ]     := SL_GETFIELDSIZE( aField[ UR_FI_TYPE ], s_aStruct[ n, DBS_LEN ] )  && Rossine 22/10/08
       aField[ UR_FI_DEC ]     := s_aStruct[ n, DBS_DEC ]
       aWAData[ WA_RECSIZE ]   += aField[ UR_FI_LEN ]
       UR_SUPER_ADDFIELD( nWA, aField )
   End

   IF ( nResult := UR_SUPER_OPEN( nWA, aOpenInfo ) ) == SUCCESS
      nResult := SL_GOTOP( nWA )
   End   
   RETURN nResult
 
*************************
static function SL_CLOSE( nWA )  && XBASE - DBCLOSE()
*************************
 
   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif
   
//   cTable := SQLGetFullTableName( aWAData )
//   do while ( n := ascan( s_aMyLocks, { |aLock| aLock[2] = cTable } ) ) > 0
//      adel( s_aMyLocks, n , .T. )
//   enddo

return UR_SUPER_CLOSE( nWA )
 
/*
 * return correct field for paging without separators
 * 22/12/2008 - 18:35:43
 */
***********************
function SL_PKFIELD( nWA )
***********************

   LOCAL aWAData 
   
   IF     valtype( nWA ) == "U"
          aWAData := USRRDD_AREADATA( Select() )

   ELSEIF valtype( nWA ) == "N"
          aWAData := USRRDD_AREADATA( nWA )

   ELSEIF valtype( nWA ) == "A"
          aWAData := ( nWA )
   End
   
   IF valtype( AWAData ) != "A" .or. AWAData[ WA_FLD_RECNO ] == 0
      return ""
   End   

   return AWAData[ WA_REAL_STRUCT, AWAData[ WA_FLD_RECNO ], DBS_NAME ]

/*
 * Emulate DbGoto()
 */
**************************
static function SL_GOTOID( nWA, nRecord )  && XBASE - DBGOTO()
**************************

   LOCAL aWAData := USRRDD_AREADATA( nWA )
 
   IF SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   End

   if aWAData[ WA_SL_GOTOID ] = NIL
      aWAData[ WA_SL_GOTOID ]   := "SELECT " + SL_GetFieldNames( aWAData ) + ;
                                     " FROM " + SQLGetFullTableName( aWAData ) + ;
                                   ' WHERE "' + SL_PKFIELD( nWA ) + '" = ? LIMIT 1'
   endif

   SQLBUFFER_DELETE( aWAData )   

   aWAData[ WA_BUFFER_POS ] :=  1 // my row inside aWAData
 
   IF HB_ExecFromArray( { FSL_GOTOID( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, nRecord } ) != SUCCESS
      return FAILURE
   End

   aWAData[ WA_BOF ] := aWAData[ WA_BUFFER_ROWCOUNT ] < 1
   aWAData[ WA_EOF ] := aWAData[ WA_BUFFER_ROWCOUNT ] < 1
   
   return SUCCESS
 
/*
 * Emulate DbGoTop()
 */
*************************
static function SL_GOTOP( nWA )  && XBASE - DBGOTOP() 
*************************

   LOCAL aWAData := USRRDD_AREADATA( nWA )
  
   IF SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   End

   IF aWAData[ WA_SL_GOTOP ] == NIL
      aWAData[ WA_SL_GOTOP ] := "SELECT " + SL_GetFieldNames( aWAData ) + ;
                                  " FROM " + SQLGetFullTableName( aWAData ) 
   End

   SQLBUFFER_DELETE( aWAData )
   
// aWAData[ WA_BUFFER_ROWCOUNT ] := 10 // number of lines retrieved
// aWAData[ WA_RECNO ]           :=  1 && Rossine 07/10/08
   aWAData[ WA_BUFFER_POS ]      :=  1 // my row inside aWAData
 
   IF HB_ExecFromArray( { FSL_GOTOP( aWAData[ WA_SYSTEMID ] ), nWa, aWAData } ) != SUCCESS
      return FAILURE
   End

   aWAData[ WA_BOF ] := .T.                                && Rossine 28/12/08
   aWAData[ WA_EOF ] := aWAData[ WA_BUFFER_ROWCOUNT ] < 1  && Rossine 28/12/08

   USRRDD_SETTOP( nWA, .T. )
   USRRDD_SETBOTTOM( nWA, .F. )
   RETURN SL_UpdateFlags( nWA, aWAData )

****************************
static function SL_GOBOTTOM( nWA )  && XBASE - DBGOBOTTOM()
****************************

   local aWAData := USRRDD_AREADATA( nWA )
 
   IF SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   End

   IF aWAData[ WA_SL_GOBOTTOM ] = NIL
      aWAData[ WA_SL_GOBOTTOM ] := "SELECT " + SL_GetFieldNames( aWAData ) + ;
                                     " FROM " + SQLGetFullTableName( aWAData )
   End

   SQLBUFFER_DELETE( aWAData )

// aWAData[ WA_BUFFER_ROWCOUNT ] := 10       // number of lines retrieved
// aWAData[ WA_RECNO ]           := aWAData[ WA_RECCOUNT ]             && Rossine 07/10/08
   aWAData[ WA_BUFFER_POS ]      := 1        // my row inside aWAData

   if HB_ExecFromArray( { FSL_GOBOTTOM( aWAData[ WA_SYSTEMID ] ), nWa, aWAData } ) != SUCCESS
      return FAILURE
   endif

   aWAData[ WA_BOF ] := aWAData[ WA_BUFFER_ROWCOUNT ] < 1  && Rossine 28/12/08
   // TODO: tenho duvidas se em DBF o comportamento é o mesmo que o abaixo... verificar!
   //aWAData[ WA_EOF ] := .T.  && Rossine 28/12/08
   aWAData[ WA_EOF ] := aWAData[ WA_BUFFER_ROWCOUNT ] < 1  && Rossine 28/12/08

   USRRDD_SETTOP( nWA, .F. )
   USRRDD_SETBOTTOM( nWA, .T. )

   RETURN SL_UpdateFlags( nWA, aWAData )

FUNCTION SL_UpdateFlags( nWA, aWAData )

   USRRDD_SETBOF( nWA, aWAData[ WA_BOF ] )
   USRRDD_SETEOF( nWA, aWAData[ WA_EOF ] )
   RETURN SUCCESS
   
/*
 * Lê mais um pacote de dados direto do banco e atualiza os flags internos.
 * retorna SUCCESS indicando êxito e em nCount parametro passado por referência a 
 * quantidade de registros obtidos.
 * 21/03/2009 - 13:25:15
 */
STATIC;
FUNCTION SL_FETCH( nWA, aWAData, nDirection )

   LOCAL nLimit
   LOCAL cLimit
   LOCAL aRules 
   LOCAL cOrderBy
   LOCAL cWhere 
   LOCAL cSQL
   LOCAL nOptions
   LOCAL i

//   DEBUG_ARGS
   aRules  := SL_BuildWhere( aWAData, nDirection )
   cOrderBy:= SL_BuildOrderBy( aWAData, nDirection )
   nLimit  := aWAData[ WA_PACKET_SIZE ]
   cLimit  := " LIMIT " + STR( nLimit )

   /* SQL RDD bug! *
   DEBUG aWAData[WA_LAST_PACKSIZE] 
   IF aWAData[WA_LAST_PACKSIZE] == NIL; nLimit :=  32; End
   IF aWAData[WA_LAST_PACKSIZE] ==  32; nLimit := 125; End
   IF aWAData[WA_LAST_PACKSIZE] == 125; nLimit := 502; End
   IF aWAData[WA_LAST_PACKSIZE] == 502; nLimit := 502; End        
   
   aWAData[WA_LAST_PACKSIZE] := nLimit      
   DEBUG aWAData[WA_LAST_PACKSIZE] 
   /* **** */

//      Com UNIONs isto não vai funcionar...
//      IF nOffSet != 00
//         cLimit += " OFFSET " + STR( nOffSet ) 
//      End
   IF Empty( aRules )
      cWhere := SL_BuildWhereStr( nWA, aWAData, .T., aRules, nDirection )
      cSQL   := aWAData[ WA_SL_GOTOP ] + " WHERE " + cWhere + " ORDER BY " + cOrderBy + cLimit 
   ELSE
      cWhere := SL_BuildWhereStr( nWA, aWAData, .T., aRules, nDirection )
      cSQL   := "(SELECT * FROM (" + aWAData[ WA_SL_GOTOP ] + " WHERE " + cWhere + " ORDER BY " + cOrderBy + cLimit + ") TMP1"
      
      i := 1
      
      WHILE !Empty( aRules[2] )
          i++
          cWhere := SL_BuildWhereStr( nWA, aWAData, .F., aRules, nDirection )
          cSQL += ")" + CRLF + ' UNION ' + CRLF 
          cSQL += "(SELECT * FROM (" + aWAData[ WA_SL_GOTOP ] + " WHERE " + cWhere + " ORDER BY " + cOrderBy + cLimit + ") TMP" + alltrim(str(i))             
      End 
      
      cSQL += ") ORDER BY " + cOrderBy + cLimit
   End

   IF nDirection == MS_DOWN
      nOptions := EU_IGNORE_FIRST + EU_EOF_ON_EMPTY
   ELSE
      nOptions := EU_IGNORE_FIRST + EU_BOF_ON_EMPTY
   End
   
   // TODO: Evitar chamar esta função diretamente!!
   RETURN PGSQL_ExecAndUpdate( nWa, aWAData, cSQL, nDirection, nOptions )
   
/*
 * Avança ou retrocede a quantidade de registros desejadas dentro do buffer
 * 21/03/2009 - 13:22:08
 */
static;
function SL_SKIPRAW( nWA, nRecords )  && XBASE - DBSKIP()

   LOCAL aWAData    := USRRDD_AREADATA( nWA )
   LOCAL nOffSet    
   LOCAL lFechtData 
   
   LOCAL nDirection
   LOCAL lInverse

 * DEBUG_ARGS
   DEBUG iif( nRecords<0, "retrocedendo ", "avancando " ) + alltrim( str( nRecords )) + ' regs     ----  Posição atual: '+  alltrim(str(aWAData[ WA_BUFFER_POS ])) + '/' + alltrim(str(aWAData[ WA_BUFFER_ROWCOUNT ]))
      
   DEFAULT nRecords := 1  && Rossine 07/10/08

   IF SL_GOCOLD( nWA ) != SUCCESS
      RETURN FAILURE
   End

   IF nRecords == 0
      RETURN SUCCESS 
   End

   lInverse := ( aWAData[ WA_RESULT_DIRECTION ] == MS_UP )
   
   IF lInverse
      nRecords *= -1
   End

   IF nRecords > 0
DEBUG "MS_DOWN: Ele está AVANÇANDO no dados!", nRecords
      nDirection := MS_DOWN

      IF ( aWAData[ WA_BUFFER_POS ] + nRecords) <= aWAData[ WA_BUFFER_ROWCOUNT ]
         aWAData[ WA_BUFFER_POS ] += nRecords
         aWAData[ WA_RECNO ] := SL_GETVALUE_PGSQL( nWa, aWAData, aWAData[ WA_FLD_RECNO ], .T. )
DEBUG "MS_DOWN: Nao precisou ler mais dados! Posição atual:", alltrim(str(aWAData[ WA_BUFFER_POS ])) + '/' + alltrim(str(aWAData[ WA_BUFFER_ROWCOUNT ])), '  *** Recno -> ', aWAData[ WA_RECNO ]
         //// substituir acima por isto:
         //// SL_GETVALUE_WA( nWA, AWAData[ WA_FLD_RECNO ], @aWAData[ WA_RECNO ], .T. )

         aWAData[ WA_BOF ] := aWAData[ WA_EOF ] := False
         RETURN SL_UpdateFlags( nWA, aWAData )
      End

      /*
       * Forçamos ele a puxar só a quantidade de registros que faltam considerando
       * a quantidade de itens que já possuimos em nosso buffer atual.
       */
      lFechtData := .T.
      nOffSet    := nRecords - ( aWAData[ WA_BUFFER_ROWCOUNT ] - aWAData[ WA_BUFFER_POS ] )

      /* Facilitamos o trabalho para SL_GETVALUE_WA() usada em SL_BUILDWHERE() */
      aWAData[ WA_BUFFER_POS ] := aWAData[ WA_BUFFER_ROWCOUNT ]
   ELSE
DEBUG "MS_UP: Ele está VOLTANDO no buffer!", nRecords
      nDirection := MS_UP

      IF ( aWAData[ WA_BUFFER_POS ] + nRecords) >= 1
         aWAData[ WA_BUFFER_POS ] += nRecords
         aWAData[ WA_RECNO ] := SL_GETVALUE_PGSQL( nWa, aWAData, aWAData[ WA_FLD_RECNO ], .T. )
DEBUG "MS_UP: Nao precisou ler mais dados! Posição atual:", alltrim(str(aWAData[ WA_BUFFER_POS ])) + '/' + alltrim(str(aWAData[ WA_BUFFER_ROWCOUNT ])), '  *** Recno -> ', aWAData[ WA_RECNO ]
         aWAData[ WA_BOF ] := aWAData[ WA_EOF ] := False
         RETURN SL_UpdateFlags( nWA, aWAData )
      End

      lFechtData := .T.
      nOffSet    := ( ABS( nRecords ) - aWAData[ WA_BUFFER_POS ] ) + 1

      aWAData[ WA_BUFFER_POS ] := 1
   End

   IF lFechtData
      aWAData[ WA_RECNO ] := SL_GETVALUE_PGSQL( nWa, aWAData, aWAData[ WA_FLD_RECNO ], .T. )
      DEBUG "Vamos ler mais dados:", nOffSet, ' regs à partir do registro',aWAData[ WA_RECNO ]

      IF lInverse
         nDirection := iif( nDirection == MS_DOWN, MS_UP, MS_DOWN )
      End

      IF nOffSet < 0
         nOffSet := ABS( nOffSet )
      End

      WHILE SL_FETCH( nWA, aWAData, nDirection ) == SUCCESS
      
            /* Eof?! */  
            IF (nDirection == MS_UP    .AND. aWAData[ WA_BOF ]) .OR. ;
               (nDirection == MS_DOWN  .AND. aWAData[ WA_EOF ])
               Exit
            End
            
DEBUG "Tinha que ler  mais dados: " + alltrim(str(nOffSet)) + ' regs '//à partir do registro ' + alltrim(str( aWAData[ WA_RECNO ] ))
            /* ex: need 3 records / 5 loaded? */         
            IF ( aWAData[ WA_BUFFER_POS ] + nOffSet ) <= aWAData[ WA_BUFFER_ROWCOUNT ] 
DEBUG "FETCH: Li os dados e parei na posição atual:", alltrim(str(aWAData[ WA_BUFFER_POS ])) + '/' + alltrim(str(aWAData[ WA_BUFFER_ROWCOUNT ])), '  *** Recno -> ', aWAData[ WA_RECNO ]         
               aWAData[ WA_BUFFER_POS ] += nOffSet -1
               aWAData[ WA_RECNO ] := SL_GETVALUE_PGSQL( nWa, aWAData, aWAData[ WA_FLD_RECNO ], .T. )
DEBUG "FETCH: Li os dados e parei na posição atual:", alltrim(str(aWAData[ WA_BUFFER_POS ])) + '/' + alltrim(str(aWAData[ WA_BUFFER_ROWCOUNT ])), '  *** Recno -> ', aWAData[ WA_RECNO ]         
               Exit
            End 
            
            /* Read more records! */  
//               nOffSet += ( aWAData[ WA_BUFFER_POS ] -1 ) 
            *nOffSet -= ( aWAData[ WA_BUFFER_ROWCOUNT ] )
            aWAData[ WA_BUFFER_POS ] := aWAData[ WA_BUFFER_ROWCOUNT ]
            aWAData[ WA_RECNO ]      := SL_GETVALUE_PGSQL( nWa, aWAData, aWAData[ WA_FLD_RECNO ], .T. )
            nOffSet ++
            DEBUG "VAMOS LER MAIS DADOS"
      End
      DEBUG "Posição atual:", alltrim(str(aWAData[ WA_BUFFER_POS ])) + '/' + alltrim(str(aWAData[ WA_BUFFER_ROWCOUNT ])), '  *** Recno -> ', aWAData[ WA_RECNO ]
   End   

   // Same as dbf1.c... - 25/05/2009 - 10:15:45
   USRRDD_SETTOP( nWA, .F. )
   USRRDD_SETBOTTOM( nWA, .F. )

RETURN SL_UpdateFlags( nWA, aWAData )
 
***********************
static function SL_BOF( nWA, lBof )  && XBASE - BOF()
***********************

   local aWAData := USRRDD_AREADATA( nWA )
 
   lBof := aWAData[ WA_BOF ]
   
   USRRDD_SETBOF( nWA, lBof )  && Rossine 28/12/08
   
return SUCCESS

***********************
static function SL_EOF( nWA, lEof )  && XBASE - EOF()
***********************

   local aWAData := USRRDD_AREADATA( nWA )
 
   lEof := aWAData[ WA_EOF ]

   USRRDD_SETEOF( nWA, lEof )  && Rossine 28/12/08

return SUCCESS
 
***************************
static function SL_DELETED( nWA, lDeleted )  && XBASE - DBDELETED()
***************************

   local aWAData := USRRDD_AREADATA( nWA )
   local nRecNo  := 0

   SL_RECNO( nWA, @nRecNo )

   lDeleted := HB_ExecFromArray( { FSL_DELETED( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, nRecno } )

return SUCCESS
 
**************************
static function SL_DELETE( nWA )    && XBASE - DBDELETE()
**************************

   local aWAData := USRRDD_AREADATA( nWA )
   local nRecNo := 0

   SL_RECNO( nWA, @nRecNo )

   HB_ExecFromArray( { FSL_DELETE( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, nRecno } )

   SL_SKIPRAW( nWA, 1 )
 
return SUCCESS
 
*************************
STATIC;
FUNCTION SL_RECNO( nWA, nRecNo )   && XBASE - RECNO()
    ** Evitamos ter 2 funções com o mesmo código!
   RETURN SL_RECID( nWA, @nRecNo )

******************
STATIC;
FUNCTION SL_RECID( nWA, nRecNo )   && XBASE - RECNO()
   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF aWAData[ WA_EOF ]
      IF SL_RECCOUNT( nWA, @nRecNo ) == SUCCESS   // Simulate xBase EOF() - 25/05/2009 - 15:06:38
         * TODO: BUG: Esta linha abaixo nao seria necessario se a linha acima
         * funcionasse corretamente, o que nao tem ocorrido aqui nos meus testes
         nRecNo := aWAData[ WA_RECCOUNT ] +1
      End
   ELSE
      IF nRecNo == 0
         nRecNo := aWAData[ WA_RECNO ]
      ELSE
         aWAData[ WA_RECNO ] := nRecNo
      End
   End
   RETURN SUCCESS

****************************
STATIC ;
FUNCTION SL_RECCOUNT( nWA, nRecords )  && XBASE - LASTREC() / RECCOUNT()
   LOCAL aWAData  := USRRDD_AREADATA( nWA )
   LOCAL nExpires := aWAData[ WA_RECCOUNT_EXPIRES ]
   LOCAL nNow     := SL_GetCurrentTime()

   IF nNow <= ( nExpires + SL_TIMER_RECCOUNT )
      * Use cache here!
      DEBUG "Using cache here -->",aWAData[ WA_RECCOUNT ]
   ELSE
      HB_ExecFromArray( { FSL_RECCOUNT( aWAData[ WA_SYSTEMID ] ), nWa, aWAData } )
      
      aWAData[ WA_RECCOUNT_EXPIRES ] := SL_GetCurrentTime()
      DEBUG "Updated cache value -->",aWAData[ WA_RECCOUNT ]
   End

   nRecords := aWAData[ WA_RECCOUNT ]
   RETURN SUCCESS

****************************
//static;
function SL_GETVALUE_WA( nWA, nField, xValue, lHidden )  && XBASE - FIELDGET()
****************************

   local aBuffer, nRow
   local aWAData, bEmpty
   local s_aStruct
   
   IF ValType( nWA ) == 'A'
      aWAData := nWA
      nWA     := Select()
   ELSE 
      aWAData := USRRDD_AREADATA( nWA )
   End
   
   bEmpty  := aWAData[ WA_EOF ]   
   aBuffer := aWAData[ WA_BUFFER_ARR ]
   nRow    := aWAData[ WA_BUFFER_POS ]

   IF !bEmpty .and. Valtype( aBuffer ) == "A"
      /* We are positioned properly within the buffer? */
      if nRow < 1 .OR. nRow > aWAData[ WA_BUFFER_ROWCOUNT ]
         msgstop( "SL_GETVALUE_WA() -> We are positioned properly within the buffer?" )
         return SUCCESS
      endif
      /* This line had been changed before? If not it have NIL value */
      if aBuffer[ nRow ] != NIL
         xValue := aBuffer[ nRow, nField ]
 
         // This is a empty or original value?
         IF (xValue != NIL)
            return SUCCESS
         endif
      endif
   endif

   IF (bEmpty)
      s_aStruct := aWAData[ WA_REAL_STRUCT ]

      //DEBUG nField, s_aStruct
   
       /* Get formated value */
       do case
       case s_aStruct[ nField ][ DBS_TYPE ] == "CHAR"
            xValue := Space( s_aStruct[ nField ][ DBS_LEN ] )
 
       case s_aStruct[ nField ][ DBS_TYPE ] == "MEMO"
            xValue := ''
 
       case s_aStruct[ nField ][ DBS_TYPE ] == "LOGICAL"
            xValue := .F.
 
       case s_aStruct[ nField ][ DBS_TYPE ] == "DATE"
            xValue := CTOD('')
 
       otherwise  // Numeric field..
            xValue := 0
       endcase
   ELSE
      /* Extract correct value from original buffer - 22/12/2008 - 16:54:47 - Vailton */
      xValue := HB_ExecFromArray( { FSL_GETVALUE( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, nField, lHidden } )
   End
 
return SUCCESS

****************************
static function SL_PUTVALUE( nWA, nField, xValue )   && XBASE - FIELDPUT()
****************************

   local aWAData := USRRDD_AREADATA( nWA )
 * local lApp    := aWAData[ WA_APPEND ]
   local aBuffer, nRow
   /*
    * Check if a temporary buffer already exists. (Vailton)
    * 16/09/2008 - 07:49:55
    */
   IF aWAData[ WA_BUFFER_ARR ] == NIL
      IF SQLBUFFER_CREATE( aWAData ) != SUCCESS
         ** Check any error here! (Especially if this function is written in C)
         return FAILURE
      endif
   endif
 
   aBuffer := aWAData[ WA_BUFFER_ARR ]
   nRow    := aWAData[ WA_BUFFER_POS ]
 
   /* We are positioned properly within the buffer? */
   if nRow < 1 .OR. nRow > aWAData[ WA_BUFFER_ROWCOUNT ]
      *msgstop( "SL_PUTVALUE() -> We are positioned properly within the buffer?", "SQLLIB Line " + LTrim( Str( ProcLine( 0 ) ) ) )
      return FAILURE
   endif
 
   /* This line had been changed before? If not it have NIL value */
   if aBuffer[ nRow ] == NIL
      aBuffer[ nRow ] := ARRAY( aWAData[ WA_FCOUNT ] )
   endif
 
** && Rossine 07/10/08

   HB_ExecFromArray( { FSL_PUTVALUE( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, xValue, nField } )

   aBuffer[ nRow, nField ]     := xValue
   aWAData[ WA_RECORDCHANGED ] := .T.

return SUCCESS
 
**************************
static function SL_APPEND( nWA, lUnLockAll )  && XBASE - DBAPPEND()
**************************
 
   local aWAData := USRRDD_AREADATA( nWA )

   HB_SYMBOL_UNUSED( lUnLockAll )

   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif

   /* Destroy any buffer data if exists */
   SQLBUFFER_DELETE( aWAData )
 
   aWAData[ WA_RECCOUNT ]++          // meanwhile we don´t do a real connection

   aWAData[ WA_APPEND        ] := .T.
   aWAData[ WA_RECORDCHANGED ] := .T.
   aWAData[ WA_RECNO ]         := aWAData[ WA_RECCOUNT ]             && Rossine 07/10/08

   /* Create a new buffer for current area*/
   aWAData[ WA_BUFFER_ROWCOUNT ]:= 1 // One line... for Append values only
   SQLBUFFER_CREATE( aWAData )
 
   aWAData[ WA_BUFFER_POS ]     := 1
*   msgstop( "dbAppend()", "SQLLIB Line " + LTrim( Str( ProcLine( 0 ) ) ) )
 
return SUCCESS
 
*************************
static function SL_FLUSH( nWA )  && XBASE - DBCOMMIT()
*************************

   local aWAData := USRRDD_AREADATA( nWA )
   
   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif
 
   if aWAData[ WA_TRANSCOUNTER ] == 0
//      if len(s_aMyLocks) > 0  && Rossine 01/11/08
//         dbrlock( s_aMyLocks[1,1] )  && Travo novamente todos os outros registros
//      else
         SL_EndTrans( aWAData ) && Rossine 07/10/08
//      endif
   endif

return SUCCESS
 
**************************
static function SL_GOCOLD( nWA )
**************************

   local aWAData := USRRDD_AREADATA( nWA )
 
   if !aWAData[ WA_RECORDCHANGED ]
      return SUCCESS
   endif
 
return SL_WRITERECORD( nWA )

*************************** 
static function SL_RAWLOCK( nWA, nAction, nRecNo )
***************************

   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( nAction )
   HB_SYMBOL_UNUSED( nRecNo )
 
return SUCCESS
 
************************
static function SL_INFO( nWA, nTypo, aList  )  && XBASE - dbrlocklist()
************************
 
   local aWAData := USRRDD_AREADATA( nWA )

   HB_SYMBOL_UNUSED( nTypo )
   HB_SYMBOL_UNUSED( aList )

   HB_ExecFromArray( { FSL_INFO( aWAData[ WA_SYSTEMID ] ), nWa, aWAData } )

return SUCCESS

************************
static function SL_LOCK( nWA, aLockInfo )  && XBASE - DBRLOCK()
************************

#ifdef _OFF_
   local aWAData := USRRDD_AREADATA( nWA )
   local nResult, xRecId, i
   local lRet, nRecno := iif( aLockInfo[ UR_LI_RECORD ] = NIL, aWAData[ WA_RECNO ], aLockInfo[ UR_LI_RECORD ] )

   lRet := HB_ExecFromArray( { FSL_LOCK( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, nRecno, s_aMyLocks } )

**lRet := SL_QuickQuery( aWAData, cSql )

**msgstop( valtoprg(lRet) + "-" + cSql, "Proc: " + PROCESSO() )

**msgstop( valtoprg( oSql:TransactionStatus() ) + "-" + valtoprg( lret ), "Proc: " + PROCESSO() )

**   if valtype( lRet ) = "A"
   if lRet  && Rossine 01/11/08
      aLockInfo[ UR_LI_METHOD ] := DBLM_MULTIPLE
      aLockInfo[ UR_LI_RECORD ] := iif( aLockInfo[ UR_LI_RECORD ] = NIL, aWAData[ WA_RECNO ], aLockInfo[ UR_LI_RECORD ] )
      aLockInfo[ UR_LI_RESULT ] := .T.
      aWAData [ WA_LOCK ]       := .T.   && Rossine 22/10/08

      if ascan( s_aMyLocks, { |aLock| aLock[1] = aLockInfo[ UR_LI_RECORD ] .and. aLock[2] = SQLGetFullTableName( aWAData ) } ) = 0
         aadd( s_aMyLocks, { aLockInfo[ UR_LI_RECORD ], SQLGetFullTableName( aWAData ) } )
      endif

**      nResult := UR_SUPER_LOCK( nWA, aLockInfo )
**msgstop( "nResult: " + valtoprg( nResult ) )
   else
      if len(s_aMyLocks) > 0         && Rossine 01/11/08
         dbrlock( s_aMyLocks[1,1] )  && Travo novamente todos os outros registros
      else 
         SL_ExecQuery( aWAData, "commit", , .F. )
      endif
   endif
   
   return lRet
#endif
   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( aLockInfo )

   return .f.
/*     
**   return iif( valtype( lRet ) = "A", .T., .F. )

**   return SUCCESS

  &&  Convert EXCLUSIVE locks to DBLM_MULTIPLE 
**   IF aLockInfo[ UR_LI_METHOD ] == DBLM_EXCLUSIVE
**      aLockInfo[ UR_LI_METHOD ] := DBLM_MULTIPLE
**      aLockInfo[ UR_LI_RECORD ] := aWAData[ WA_RECNO ] && RECNO()
**   ENDIF

   IF aLockInfo[ UR_LI_METHOD ] == DBLM_MULTIPLE      && RLOCK 

      xRecID := aLockInfo[ UR_LI_RECORD ]

      IF EMPTY( xRecID )
         xRecID := aWAData[ WA_RECNO ] && RECNO()
         aLockInfo[ UR_LI_RECORD ] := aWAData[ WA_RECNO ] && RECNO()
         aLockInfo[ UR_LI_RESULT ] := .T.
         aWAData [ WA_LOCK ]       := .T.
         return SUCCESS
      ENDIF

*      nResult := UR_SUPER_LOCK( nWA, aLockInfo )

*      IF nResult == SUCCESS 
*         IF aLockInfo[ UR_LI_RESULT ]
*            aWAData [ WA_LOCK ] := .T.
*         ENDIF
*      ENDIF

*msgstop( "1 = " + VALTOPRG( aLockInfo ) )

*      return nResult
         return SUCCESS

   ELSEIF aLockInfo[ UR_LI_METHOD ] == DBLM_FILE      && FLOCK
      
      IF aWData[ 1 ] > 0
         ++aWData[ 1 ]
         return SUCCESS
      ENDIF

      nResult := UR_SUPER_LOCK( nWA, aLockInfo )
      IF nResult == SUCCESS 

         && FLOCK always first remove all RLOCKs, even if it fails
         ASIZE( aWData[ 2 ], 0 )

         IF aLockInfo[ UR_LI_RESULT ]
            aWData[ 1 ] := 1
         ENDIF
      ENDIF

      return nResult

   ENDIF

   aLockInfo[ UR_LI_RESULT ] := .F.

return FAILURE
*/ 
**************************
static function SL_UNLOCK( nWA, xRecID )   && XBASE - DBUNLOCK()
**************************
 
   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( xRecID )

return SUCCESS
   
#ifdef _OFF_   
   local aWAData := USRRDD_AREADATA( nWA ), n, nReg := recno()

**   HB_SYMBOL_UNUSED( xRecID )

   if xRecID != NIL
      nReg := xRecID
   endif

   aWAData [ WA_LOCK ] := .F.
   
   if ( n := ascan( s_aMyLocks, { |aLock| aLock[1] = nReg .and. aLock[2] = SQLGetFullTableName( aWAData ) } ) ) > 0  && Rossine 01/11/08
      adel( s_aMyLocks, n , .T. )
   endif

   SL_FLUSH( nWA )

*   nResult := UR_SUPER_UNLOCK( nWA, xRecID )

*   IF nResult == SUCCESS 
*      aWAData [ WA_LOCK ] := .F.
*      SL_FLUSH( nWA )
*   ENDIF

**return nResult
#endif
 
*****************************
static function SL_SETFILTER( nWA, aFilterInfo )
*****************************
 
   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( aFilterInfo )
  
return SUCCESS

******************************* 
static function SL_CLEARFILTER( nWA )
*******************************

   HB_SYMBOL_UNUSED( nWA )
 
return SUCCESS
 
************************
static function SL_PACK( nWA )  && XBASE - __DBPACK()
************************

   local aWAData := USRRDD_AREADATA( nWA )

   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif

   HB_ExecFromArray( { FSL_PACK( aWAData[ WA_SYSTEMID ] ), nWa, aWAData } )
 
return SUCCESS

***********************
static function SL_ZAP( nWA )  && XBASE - __DBZAP()
***********************

   local aWAData := USRRDD_AREADATA( nWA )
 
   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif
 
   HB_ExecFromArray( { FSL_ZAP( aWAData[ WA_SYSTEMID ] ), nWa, aWAData } )

   aWAData[ WA_RECCOUNT ] := 0 // temporary value meanwhile we don´t do a real connection
   aWAData[ WA_RECNO ]    := 0   && Rossine 07/10/08

return SUCCESS
 
*****************************
static function SL_SETLOCATE( nWA, aScopeInfo )
*****************************

   local aWAData := USRRDD_AREADATA( nWA )
 
   aScopeInfo[ UR_SI_CFOR ] := SQLTranslate( aWAData[ WA_LOCATEFOR ] )
 
   aWAData[ WA_SCOPEINFO ] := aScopeInfo
 
return SUCCESS

************************** 
static function SL_LOCATE( nWA, lContinue )
**************************

   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( lContinue )

   //local aWAData    := USRRDD_AREADATA( nWA )
 
   // USRRDD_SETFOUND( nWA, ! oRecordSet:EOF )
   // aWAData[ WA_EOF ] := oRecordSet:EOF
 
return SUCCESS

****************************
static function SL_CLEARREL( nWA )
****************************

   HB_SYMBOL_UNUSED( nWA )
//   local aWAData := USRRDD_AREADATA( nWA )
//   local nKeys := 0, cKeyName

return SUCCESS
 
***************************
static function SL_RELAREA( nWA, nRelNo, nRelArea )
***************************

   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( nRelNo )
   HB_SYMBOL_UNUSED( nRelArea )
//   local aWAData := USRRDD_AREADATA( nWA )

return SUCCESS
 
***************************
static function SL_RELTEXT( nWA, nRelNo, cExpr )
***************************

   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( nRelNo )
   HB_SYMBOL_UNUSED( cExpr )
//   local aWAData := USRRDD_AREADATA( nWA )

return SUCCESS
 
**************************
static function SL_SETREL( nWA, aRelInfo )
**************************

   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( aRelInfo  )
//   local aWAData := USRRDD_AREADATA( nWA )
//   local cParent := Alias( aRelInfo[ UR_RI_PARENT ] )
//   local cChild  := Alias( aRelInfo[ UR_RI_CHILD ] )
//   local cKeyName := cParent + "_" + cChild
 
return SUCCESS
 
/*
 * Open a existing index into current WA
 * 15/01/2009 - 20:09:32
 */
STATIC ;
FUNCTION SL_ORDLSTADD( nWA, aOrderInfo )
   LOCAL aWAData := USRRDD_AREADATA( nWA )
 
/* DBORDERINFO
#define UR_ORI_BAG            1
#define UR_ORI_TAG            2
#define UR_ORI_BLOCK          3
#define UR_ORI_RESULT         4
#define UR_ORI_NEWVAL         5
#define UR_ORI_ALLTAGS        6
#define UR_ORI_SIZE           6
*/

   ** TS_COMPLEX_SQL ** nao suporta INDEX **
   if aWAData[ WA_TABLETYPE ] == TS_COMPLEX_SQL
      return SUCCESS
   end
   
   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif

   HB_ExecFromArray( { FSL_ORDLSTADD( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, aOrderInfo } )
   RETURN SUCCESS
 
*******************************
static function SL_ORDLSTCLEAR( nWA )
*******************************

   local aWAData := USRRDD_AREADATA( nWA )
 
   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif
 
   aWAData[ WA_INDEX ] := {}  && Vailton - 15/01/2009 - 21:18:10
 
return SUCCESS

********************************
static function SL_ORDLSTDELETE( nWA, aOrderInfo )
********************************

   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif

   HB_SYMBOL_UNUSED( aOrderInfo )
return SUCCESS

/*
 * Create a custom index for current WA
 */
STATIC;
FUNCTION SL_ORDCREATE( nWA, aOrderCreateInfo )
*****************************

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL aStruct := aWAData[WA_STRUCT]
   LOCAL Keys    := {}
   LOCAL Sizes   := {}
   LOCAL Fields  := {}                                   
   LOCAL aKeys
   LOCAL lComplex
   LOCAL oError
   LOCAL aOrderInfo
   LOCAL i,j

   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif

   /* Parse indexkey */
   aKeys := SL_PARSEKEY( aOrderCreateInfo[UR_ORCR_CKEY], @lComplex, aStruct )

   IF ( Len( aKeys ) < 1 )
      SL_ERROR( 1002, "SQLLIB - Invalid or corrupt key for index",,, aOrderCreateInfo [UR_ORCR_BAGNAME] )
      RETURN FAILURE
   End

   /*
    * 21-02-2006 => Ele pode criar indices customizados?
    */
   IF ( lComplex /* .and. !SQLUseCustomIndexes() */  )
      SL_ERROR( 1002, "SQLLIB - Complex index key not supported",,, aOrderCreateInfo [UR_ORCR_BAGNAME] )
      RETURN FAILURE
   End
   
   /*
    * 16-01-2006 - CDX define o valor padrao para o campo UR_ORCR_BAGNAME, ajustamos isto.
    */
   IF Empty(aOrderCreateInfo[UR_ORCR_BAGNAME]) 
      aOrderCreateInfo[UR_ORCR_BAGNAME] := SQLParse( aWAData[ WA_TABLENAME ] )
   ELSE
      aOrderCreateInfo[UR_ORCR_BAGNAME] := SQLParse( aOrderCreateInfo[UR_ORCR_BAGNAME] )
   End

   /* Validate fieldnames */
   FOR i := 1 TO LEN(aKeys)
       j := aScan( aStruct, {|_1| Upper(_1[1]) == Upper(aKeys[i]) })

       IF ( j == 00 )
         SL_ERROR( 1002, "SQLLIB - Field column not found:"+aKeys[i] ) 
         RETURN FAILURE
      End

    // Nome dos Campos
       AADD(Fields, aKeys[i] )
    // Sintaxe em Clipper para este campo ser convertido em CARACTER
       AADD(Keys  , SL_FLD2CHAR( aStruct[j] ))
       AADD(Sizes , aStruct[j,DBS_LEN] )
   END

   IF aOrderCreateInfo[UR_ORCR_UNIQUE]
      oError := ErrorNew()
      oError:GenCode     := EG_CREATE
      oError:SubCode     := 1004
      oError:Description := HB_LANGERRMSG( EG_CREATE ) + " (" + ;
                            HB_LANGERRMSG( EG_UNSUPPORTED ) + ")"
      oError:FileName    := aOrderCreateInfo [UR_ORCR_BAGNAME]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
   End
      
   IF HB_ExecFromArray( { FSL_ORDCREATE( aWAData[ WA_SYSTEMID ] ), nWa, aWAData,;
                            aOrderCreateInfo, Fields, Keys, Sizes } ) != SUCCESS
      RETURN FAILURE
   End
   
   /*
    * SET ORDER TO automaticamente apos a criacao do novo INDEX em conformidade 
    * com a doc. do Clipper disponivel em: http://www.ousob.com/ng/53guide/ngcc94d.php
    * 24/05/2009 - 00:31:31
    */
   aOrderInfo := Array( UR_ORI_SIZE )
   aOrderInfo[ UR_ORI_TAG ] := Len( aWAData[ WA_INDEX ] )
   
   RETURN SL_ORDLSTFOCUS( nWA, aOrderInfo )
 
******************************
static function SL_ORDDESTROY( nWA, aOrderInfo )
******************************

   local aWAData := USRRDD_AREADATA( nWA ), lResult
 
   if SL_GOCOLD( nWA ) != SUCCESS
      return FAILURE
   endif

   lResult := HB_ExecFromArray( { FSL_ORDDESTROY( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, aOrderInfo } )

   lResult := iif( valtype( lResult ) = "L" .and. lResult, SUCCESS, FAILURE )

return lResult


/*
 *
 * 15/01/2009 - 23:41:04
 */
STATIC;
FUNCTION SL_ORDINFO( nWA, nIndex, aOrderInfo )  && Rossine 07/10/08
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nResult := SUCCESS
   LOCAL aIndex, nPos, nCount

   IF SL_GOCOLD( nWA ) != SUCCESS  && Rossine 08/01/09
      RETURN FAILURE
   End

   nCount := LEN( aWAData[ WA_INDEX ] )
  
   IF Empty( aOrderInfo[ UR_ORI_TAG ] )
      nPos := aWAData[ WA_INDEX_CURR ]
   ELSE
      nPos := aOrderInfo[ UR_ORI_TAG ]      
   End
  
   IF ( nPos == 00 ) .OR. (!( nPos >= 1 .AND. nPos <= nCount ))

      * Empty values       
      SWITCH (nIndex)         
      /* (C) The name of the file containing this order     */
      CASE DBOI_INDEXNAME            
      /* (C) The order's conditional expression     */
      CASE DBOI_CONDITION            
      /* (C) The order's key expression             */
      CASE DBOI_EXPRESSION
      /* (C) The name of the order                      */
      CASE DBOI_NAME
      /* (C) The name of the file containing this order     */
      CASE DBOI_BAGNAME
      /* (C) The extension of the file containing this order */
      CASE DBOI_BAGEXT
           aOrderInfo[ UR_ORI_RESULT ] := ""
           Exit
      
      /* (N) The count of ORDERS contained in an index file or in total */
      CASE DBOI_ORDERCOUNT
      /* (N) The OS file handle of the index     */
      CASE DBOI_FILEHANDLE
      /* (N) The current key position in scope and filter  */
      CASE DBOI_POSITION
      /* (N) The current key position disregarding filters */
      CASE DBOI_RECNO      
      /* (N) The count of keys in scope and filter */
      CASE DBOI_KEYCOUNT      
      /* (N) The numeric position in the list of orders */     
      CASE DBOI_NUMBER
           aOrderInfo[ UR_ORI_RESULT ] := 0
           Exit      

      /* (L) Does the order have a FOR condition? */
      CASE DBOI_ISCOND
      /* (L) Is the order DESCENDing? */
      CASE DBOI_ISDESC
      /* (L) Does the order have the UNIQUE attribute? */
      CASE DBOI_UNIQUE
           aOrderInfo[ UR_ORI_RESULT ] := .F.
           Exit      
      End
   ELSE
      aIndex := aWAData[ WA_INDEX, nPos ]
   
      SWITCH (nIndex)   
      
      /* (C) The name of the file containing this order     */
      CASE DBOI_INDEXNAME            
           aOrderInfo[ UR_ORI_RESULT ] := aIndex[ IDX_BAG ]
           Exit
      
      /* (C) The order's conditional expression     */
      CASE DBOI_CONDITION            
           aOrderInfo[ UR_ORI_RESULT ] := aIndex[ IDX_FOR ]
           Exit
           
      /* (C) The order's key expression             */
      CASE DBOI_EXPRESSION
           aOrderInfo[ UR_ORI_RESULT ] := SQLArray2Text( aIndex[ IDX_KEYS ], '+' )
           Exit
           
      /* (C) The name of the order                      */
      CASE DBOI_NAME
           aOrderInfo[ UR_ORI_RESULT ] := aIndex[ IDX_TAG ]
           Exit
           
      /* (N) The numeric position in the list of orders */     
      CASE DBOI_NUMBER
           aOrderInfo[ UR_ORI_RESULT ] := nPos 
           Exit
      
      /* (C) The name of the file containing this order     */
      CASE DBOI_BAGNAME
           aOrderInfo[ UR_ORI_RESULT ] := aIndex[ IDX_BAG ]
           Exit
           
      /* (C) The extension of the file containing this order */
      CASE DBOI_BAGEXT
           aOrderInfo[ UR_ORI_RESULT ] := ""
           Exit           
           
      /* (N) The count of ORDERS contained in an index file or in total */
      CASE DBOI_ORDERCOUNT
           nCount := 0
           
           FOR nPos := 1 TO Len( aWAData[ WA_INDEX ] )
               IF aWAData[ WA_INDEX, nPos, IDX_BAG ] == aIndex[ IDX_BAG ]
                  nCount ++
               End
           End
           
           aOrderInfo[ UR_ORI_RESULT ] := nCount
           Exit
           
      /* (N) The OS file handle of the index     */
      CASE DBOI_FILEHANDLE
           aOrderInfo[ UR_ORI_RESULT ] := 0
           Exit
           
      /* (L) Does the order have a FOR condition? */
      CASE DBOI_ISCOND
           aOrderInfo[ UR_ORI_RESULT ] := !Empty( aIndex[ IDX_FOR ] )
           Exit
           
      /* (L) Is the order DESCENDing? */
      CASE DBOI_ISDESC
           aOrderInfo[ UR_ORI_RESULT ] := aIndex[ IDX_DESCEND ] 
           Exit
      
      /* (L) Does the order have the UNIQUE attribute? */
      CASE DBOI_UNIQUE
           aOrderInfo[ UR_ORI_RESULT ] := aIndex[ IDX_UNIQUE ] 
           Exit
      
      /* (N) The current key position in scope and filter  */
      CASE DBOI_POSITION
           aOrderInfo[ UR_ORI_RESULT ] := 0
           Exit
           
      /* (N) The current key position disregarding filters */
      CASE DBOI_RECNO      
           SL_RECID( nWA, @aOrderInfo[ UR_ORI_RESULT ] )
           Exit
           
      /* (N) The count of keys in scope and filter */
      CASE DBOI_KEYCOUNT      
           aOrderInfo[ UR_ORI_RESULT ] := 0
           Exit
      
      End
   End   
   RETURN nResult

*******************************
STATIC;
FUNCTION SL_ORDLSTFOCUS( nWA, aOrderInfo )  && XBASE - DBSETORDER() / OrdSetFocus()
   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nOrder, cTag

   cTag := aOrderInfo[ UR_ORI_TAG ]
   
   IF ValType( cTag ) == "N"
      nOrder := cTag
   ELSE
      cTag   := Lower( cTag )
      nOrder := aScan( aWAData[ WA_INDEX ], {| idx | idx[ IDX_TAG ] == cTag })
   End
   
   aWAData[ WA_INDEX_CURR ] := nOrder
   return SUCCESS

************************
static function SL_SEEK( nWA, lSoftSeek, cKey, lFindLast )  && XBASE - DBSEEK()
************************

   local aWAData := USRRDD_AREADATA( nWA )
   local aRet, nRecno

   if SL_GOCOLD( nWA ) != SUCCESS  && Rossine 08/01/09
      return FAILURE
   endif

**   HB_SYMBOL_UNUSED( nWA )
**   HB_SYMBOL_UNUSED( lSoftSeek )
**   HB_SYMBOL_UNUSED( cKey )
**   HB_SYMBOL_UNUSED( lFindLast )

   aRet                := HB_ExecFromArray( { FSL_SEEK( aWAData[ WA_SYSTEMID ] ), nWa, aWAData, lSoftSeek, cKey, lFindLast } )
   aWAData[ WA_FOUND ] := aRet [1]
   nRecno              := aRet [2]
   
   if valtype( aWAData[ WA_FOUND ] ) = "L"
      SL_FOUND( nWA, aWAData[ WA_FOUND ] )
      if aWAData[ WA_FOUND ]
         SL_GOTOID( nWA, nRecno )
      else
         if !lSoftSeek
            SL_GOBOTTOM( nWA )
            SL_SKIPRAW( nWA, 1 )
         endif
      endif
   else
      aWAData[ WA_FOUND ] := .F.
      SL_GOBOTTOM( nWA )
      SL_SKIPRAW( nWA, 1 )
   endif

return SUCCESS

*************************
static function SL_FOUND( nWA, lFound )  && XBASE - FOUND()
*************************

   local aWAData := USRRDD_AREADATA( nWA )

**   HB_SYMBOL_UNUSED( nWA )
**   HB_SYMBOL_UNUSED( lFound )

   lFound := aWAData[ WA_FOUND ]

   USRRDD_SETFOUND( nWA, lFound )  && Rossine 28/12/08

return SUCCESS

****************************
function SQLLIB_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID )
****************************

   local cSuperRDD := nil  /* NO SUPER RDD */
   local aSQLFunc[ UR_METHODCOUNT ]

/*
 Movement and positioning methods
*/
   aSQLFunc[ UR_BOF           ] := ( @SL_BOF() )            &&  1
   aSQLFunc[ UR_EOF           ] := ( @SL_EOF() )            &&  2
   aSQLFunc[ UR_FOUND         ] := ( @SL_FOUND() )          &&  3
   aSQLFunc[ UR_GOBOTTOM      ] := ( @SL_GOBOTTOM() )       &&  4
   aSQLFunc[ UR_GOTO          ] := ( @SL_GOTOID() )         &&  5
   aSQLFunc[ UR_GOTOID        ] := ( @SL_GOTOID() )         &&  6
   aSQLFunc[ UR_GOTOP         ] := ( @SL_GOTOP() )          &&  7
   aSQLFunc[ UR_SEEK          ] := ( @SL_SEEK() )           &&  8
   aSQLFunc[ UR_SKIP          ] := ( @SL_SKIPRAW() )        &&  9
** aSQLFunc[ UR_SKIPFILTER    ] := ( @SL_SKIPFILTER() )     && 10
   aSQLFunc[ UR_SKIPRAW       ] := ( @SL_SKIPRAW() )        && 11
                                                                  
/*
 Data management 
*/                                             
** aSQLFunc[ UR_ADDFIELD      ] := ( @SL_ADDFIELD() )       && 12
   aSQLFunc[ UR_APPEND        ] := ( @SL_APPEND() )         && 13
   aSQLFunc[ UR_CREATEFIELDS  ] := ( @SL_CREATEFIELDS() )   && 14
   aSQLFunc[ UR_DELETE        ] := ( @SL_DELETE() )         && 15
   aSQLFunc[ UR_DELETED       ] := ( @SL_DELETED() )        && 16
** aSQLFunc[ UR_FIELDCOUNT    ] := ( @SL_FIELDCOUNT() )     && 17
** aSQLFunc[ UR_FIELDDISPLAY  ] := ( @SL_FIELDDISPLAY() )   && 18
   aSQLFunc[ UR_FIELDINFO     ] := ( @SL_FIELDINFO() )      && 19
   aSQLFunc[ UR_FIELDNAME     ] := ( @SL_FIELDNAME() )      && 20
   aSQLFunc[ UR_FLUSH         ] := ( @SL_FLUSH() )          && 21
** aSQLFunc[ UR_GETREC        ] := ( @SL_GETREC() )         && 22
   aSQLFunc[ UR_GETVALUE      ] := ( @SL_GETVALUE_WA() )    && 23
** aSQLFunc[ UR_GETVARLEN     ] := ( @SL_GETVARLEN() )      && 24
   aSQLFunc[ UR_GOCOLD        ] := ( @SL_GOCOLD() )         && 25
** aSQLFunc[ UR_GOHOT         ] := ( @SL_GOHOT() )          && 26
** aSQLFunc[ UR_PUTREC        ] := ( @SL_PUTREC() )         && 27
   aSQLFunc[ UR_PUTVALUE      ] := ( @SL_PUTVALUE() )       && 28
** aSQLFunc[ UR_RECALL        ] := ( @SL_RECALL() )         && 29
   aSQLFunc[ UR_RECCOUNT      ] := ( @SL_RECCOUNT() )       && 30
   aSQLFunc[ UR_RECINFO       ] := ( @SL_RECINFO() )        && 31
   aSQLFunc[ UR_RECNO         ] := ( @SL_RECNO() )          && 32
   aSQLFunc[ UR_RECID         ] := ( @SL_RECID() )          && 33
** aSQLFunc[ UR_SETFIELDEXTENT] := ( @SL_SETFIELDEXTENT() ) && 34
                                                                  
/*
 WorkArea/Database management 
*/                                
   aSQLFunc[ UR_ALIAS         ] := ( @SL_ALIAS() )          && 35
   aSQLFunc[ UR_CLOSE         ] := ( @SL_CLOSE() )          && 36
   aSQLFunc[ UR_CREATE        ] := ( @SL_CREATE() )         && 37
   aSQLFunc[ UR_INFO          ] := ( @SL_INFO() )           && 38
   aSQLFunc[ UR_NEW           ] := ( @SL_NEW() )            && 39
   aSQLFunc[ UR_OPEN          ] := ( @SL_OPEN() )           && 40
** aSQLFunc[ UR_RELEASE       ] := ( @SL_RELEASE() )        && 41
** aSQLFunc[ UR_STRUCTSIZE    ] := ( @SL_STRUCTSIZE() )     && 42
** aSQLFunc[ UR_SYSNAME       ] := ( @SL_SYSNAME() )        && 43
** aSQLFunc[ UR_DBEVAL        ] := ( @SL_DBEVAL() )         && 44
   aSQLFunc[ UR_PACK          ] := ( @SL_PACK() )           && 45
** aSQLFunc[ UR_PACKREC       ] := ( @SL_PACKREC() )        && 46
** aSQLFunc[ UR_SORT          ] := ( @SL_SORT() )           && 47
** aSQLFunc[ UR_TRANS         ] := ( @SL_TRANS() )          && 48
** aSQLFunc[ UR_TRANSREC      ] := ( @SL_TRANSREC() )       && 49
   aSQLFunc[ UR_ZAP           ] := ( @SL_ZAP() )            && 50

/*                                                                  
   Relational Methods
*/
**   aSQLFunc[ UR_CHILDEND      ] := ( @SL_CHILDEND() )       && 51
**   aSQLFunc[ UR_CHILDSTART    ] := ( @SL_CHILDSTART() )     && 52
**   aSQLFunc[ UR_CHILDSYNC     ] := ( @SL_CHILDSYNC() )      && 53
**   aSQLFunc[ UR_SYNCCHILDREN  ] := ( @SL_SYNCCHILDREN() )   && 54
   aSQLFunc[ UR_CLEARREL      ] := ( @SL_CLEARREL() )       && 55
**   aSQLFunc[ UR_FORCEREL      ] := ( @SL_FORCEREL() )       && 56
   aSQLFunc[ UR_RELAREA       ] := ( @SL_RELAREA() )        && 57
**   aSQLFunc[ UR_RELEVAL       ] := ( @SL_RELEVAL() )        && 58
   aSQLFunc[ UR_RELTEXT       ] := ( @SL_RELTEXT() )        && 59
   aSQLFunc[ UR_SETREL        ] := ( @SL_SETREL() )         && 60
                                                                  
/*
 Order Management 
*/                                            
   aSQLFunc[ UR_ORDLSTADD     ] := ( @SL_ORDLSTADD() )      && 61
   aSQLFunc[ UR_ORDLSTCLEAR   ] := ( @SL_ORDLSTCLEAR() )    && 62
   aSQLFunc[ UR_ORDLSTDELETE  ] := ( @SL_ORDLSTDELETE() )   && 63
   aSQLFunc[ UR_ORDLSTFOCUS   ] := ( @SL_ORDLSTFOCUS() )    && 64
**   aSQLFunc[ UR_ORDLSTREBUILD ] := ( @SL_ORDLSTREBUILD() )  && 65
**   aSQLFunc[ UR_ORDSETCOND    ] := ( @SL_ORDSETCOND() )     && 66
   aSQLFunc[ UR_ORDCREATE     ] := ( @SL_ORDCREATE() )      && 67
   aSQLFunc[ UR_ORDDESTROY    ] := ( @SL_ORDDESTROY() )     && 68
   aSQLFunc[ UR_ORDINFO       ] := ( @SL_ORDINFO() )        && 69
                                                                  
/* 
Filters and Scope Settings 
*/                                  
   aSQLFunc[ UR_CLEARFILTER   ] := ( @SL_CLEARFILTER() )    && 70
**   aSQLFunc[ UR_CLEARLOCATE   ] := ( @SL_CLEARLOCATE() )    && 71
**   aSQLFunc[ UR_CLEARSCOPE    ] := ( @SL_CLEARSCOPE() )     && 72
**   aSQLFunc[ UR_COUNTSCOPE    ] := ( @SL_COUNTSCOPE() )     && 73
**   aSQLFunc[ UR_FILTERTEXT    ] := ( @SL_FILTERTEXT() )     && 74
**   aSQLFunc[ UR_SCOPEINFO     ] := ( @SL_SCOPEINFO() )      && 75
   aSQLFunc[ UR_SETFILTER     ] := ( @SL_SETFILTER() )      && 76
   aSQLFunc[ UR_SETLOCATE     ] := ( @SL_SETLOCATE() )      && 77
**   aSQLFunc[ UR_SETSCOPE      ] := ( @SL_SETSCOPE() )       && 78
**   aSQLFunc[ UR_SKIPSCOPE     ] := ( @SL_SKIPSCOPE() )      && 79
   aSQLFunc[ UR_LOCATE        ] := ( @SL_LOCATE() )         && 80
                                                                  
/*
 Miscellaneous 
*/                                               
**   aSQLFunc[ UR_COMPILE       ] := ( @SL_COMPILE() )        && 81
**   aSQLFunc[ UR_ERROR         ] := ( @SL_ERROR() )          && 82
**   aSQLFunc[ UR_EVALBLOCK     ] := ( @SL_EVALBLOCK() )      && 83
                                                                  
/*
 Network operations 
*/                                          
   aSQLFunc[ UR_RAWLOCK       ] := ( @SL_RAWLOCK() )        && 84
   aSQLFunc[ UR_LOCK          ] := ( @SL_LOCK() )           && 85
   aSQLFunc[ UR_UNLOCK        ] := ( @SL_UNLOCK() )         && 86
                                                                  
/*
 Memofile functions 
*/                                          
**   aSQLFunc[ UR_CLOSEMEMFILE  ] := ( @SL_CLOSEMEMFILE() )   && 87
**   aSQLFunc[ UR_CREATEMEMFILE ] := ( @SL_CREATEMEMFILE() )  && 88
**   aSQLFunc[ UR_GETVALUEFILE  ] := ( @SL_GETVALUEFILE() )   && 89
**   aSQLFunc[ UR_OPENMEMFILE   ] := ( @SL_OPENMEMFILE() )    && 90
**   aSQLFunc[ UR_PUTVALUEFILE  ] := ( @SL_PUTVALUEFILE() )   && 91
                                                                  
/*
 Database file header handling 
*/                               
**   aSQLFunc[ UR_READDBHEADER  ] := ( @SL_READDBHEADER() )   && 92
**   aSQLFunc[ UR_WRITEDBHEADER ] := ( @SL_WRITEDBHEADER() )  && 93
                                                                  
/*
 non WorkArea functions       
*/                                
   aSQLFunc[ UR_INIT          ] := ( @SL_INIT() )           && 94
   aSQLFunc[ UR_EXIT          ] := ( @SL_EXIT() )           && 95
**   aSQLFunc[ UR_DROP          ] := ( @SL_DROP() )           && 96
**   aSQLFunc[ UR_EXISTS        ] := ( @SL_EXISTS() )         && 97
**   aSQLFunc[ UR_RDDINFO       ] := ( @SL_RDDINFO() )        && 98
                                                                  
/*
 Special and reserved methods 
*/                                
**   aSQLFunc[ UR_WHOCARES      ] := ( @SWHOCARES() )          && 99

return USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, cSuperRDD,;
                            aSQLFunc )
 
**************************
init procedure SQLLIB_INIT
**************************

 * For‡a o carregamento de todos os drivers disponiveis para a memoria
   SQLSYS_GETINFO()
 * Registra o nosso RDD   
   rddRegister( "SQLLIB", RDT_FULL )
   
return 

*************************
static function SL_ALIAS( nWA, cAlias )  && Rossine 29/12/08
*************************
   
   local aWAData := USRRDD_AREADATA( nWA )
   
   cAlias := aWAData[ WA_ALIAS ]   && Tive que fazer assim porque a variavel "cAlias" chega aqui vazia.

return SUCCESS

********************************
static function SL_GETFIELDSIZE( nDBFFieldType, nSQLFieldSize )  && Rossine 07/10/08
********************************

   local nDBFFieldSize := 0

   do case
 
      case nDBFFieldType == HB_FT_STRING
           nDBFFieldSize := nSQLFieldSize
 
      case nDBFFieldType == HB_FT_INTEGER
           nDBFFieldSize := nSQLFieldSize
 
      case nDBFFieldType == HB_FT_DOUBLE
           nDBFFieldSize := nSQLFieldSize
 
      case nDBFFieldType == HB_FT_DATE
           nDBFFieldSize := 8
 
      case nDBFFieldType == HB_FT_LOGICAL
           nDBFFieldSize := 1
 
      case nDBFFieldType == HB_FT_MEMO
           nDBFFieldSize := 10
 
   endcase
 
return nDBFFieldSize
 
*********************
function SQLTranslate( cExpr )
*********************

   if Left( cExpr, 1 ) == '"' .and. Right( cExpr, 1 ) == '"'
      cExpr := SubStr( cExpr, 2, Len( cExpr ) - 2 )
   endif
 
   cExpr := StrTran( cExpr, '""', "" )
   cExpr := StrTran( cExpr, '"', "'" )
   cExpr := StrTran( cExpr, "''", "'" )
   cExpr := StrTran( cExpr, "==", "=" )
   cExpr := StrTran( upper(cExpr), ".AND.", "and" )
   cExpr := StrTran( upper(cExpr), ".OR.", "or" )
 
return cExpr
 
/*
 *
 * 22/12/2008 - 14:21:31
 */
***************************** 
function SL_GETFIELDNAMES( aWAData )
*****************************

   LOCAL cRddSep   := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )
   LOCAL s_aStruct := aWAData[ WA_REAL_STRUCT ]
   LOCAL cFields   := ""
   LOCAL n
   
   for n = 1 TO Len( s_aStruct )
      cFields += cRddSep + s_aStruct[ n, DBS_NAME ] + cRddSep + ", "
   next

   return SubStr( cFields, 1, Len( cFields ) - 2 )
 
***********************************
static function GetFieldEmptyValues( aWAData, lRecNo )
***********************************
 
   local cValues   := "", n
   local cRddSep   := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )
   local s_aStruct := aWAData[ WA_STRUCT ]
 
   for n = 1 to Len( s_aStruct )
      if lRecNo
         cValues += cRddSep + Space( s_aStruct[ n ][ DBS_LEN ] ) + cRddSep + ", "
      else
         if s_aStruct[ n ][ DBS_NAME ] != aWAData[ WA_FLD_RECNO ]
            cValues += cRddSep + Space( s_aStruct[ n ][ DBS_LEN ] ) + cRddSep + ", "    // That field is this? (Vailton)
         endif
      endif
   next
 
   cValues = SubStr( cValues, 1, Len( cValues ) - 2 )
 
return cValues
 
/*
 * return a full-qualified tablename based on the SYSID in use
 * 15/09/2008 - 21:49:27
 */
****************************
function SQLGetFullTableName( aWAData )
****************************

return HB_ExecFromArray( { FSL_GETFULLTABLE( aWAData[ WA_SYSTEMID ] ), aWAData } )
 
/*
 * return the correct name for a table
 * 15/09/2008 - 21:25:46
 */
********************
function SQLAdjustFn( cDataBase )
********************

   if "." $ cDataBase
      cDataBase = StrTran( cDataBase, ".", "_" )
*   else
*      cDataBase += "_dbf"
   endif

   return lower(cDataBase)  && Rossine 03/01/09
 
/*
 * Write current record into server with correct SQL command. Based on lAppend
 * to build INSERT / UPDATE statements.
 * 16/09/2008 - 08:13:32
 */
*******************************
static function SL_WRITERECORD( nWA )
*******************************

   local aWAData := USRRDD_AREADATA( nWA )
 * local lApp    := aWAData[ WA_APPEND ]
   local aBuffer, nRow, nRecNo := 0
   
   IF aWAData[ WA_BUFFER_ARR ] == NIL
      return SUCCESS
   endif

   aBuffer := aWAData[ WA_BUFFER_ARR ]
   nRow    := aWAData[ WA_BUFFER_POS ]
 
   /* We are positioned properly within the buffer? */
   if nRow < 1 .OR. nRow > aWAData[ WA_BUFFER_ROWCOUNT ]
      return FAILURE
   endif
 
   /* This line had been changed before? If not it have NIL value */
   if aBuffer[ nRow ] == NIL
      return SUCCESS
   endif
 
   SL_RECID( nWA, @nRecNo )

   HB_ExecFromArray( { FSL_WRITERECORD( aWAData[ WA_SYSTEMID ] ), nWa, aWAData } )

   aWAData[ WA_APPEND        ] := .F.
   aWAData[ WA_RECORDCHANGED ] := .F.
 
return SUCCESS

***************************
static function SL_RECINFO( nWA, nRecord, nInfoType, uInfo )  && Rossine 07/10/08
***************************

   local nResult := SUCCESS
#ifdef UR_DBRI_DELETED   
   local aWAData := USRRDD_AREADATA( nWA )
   local lDeleted
   
   do case
   case nInfoType == UR_DBRI_DELETED  && OK

        nResult   := SL_DELETED( nWA, &lDeleted )
        uInfo     := lDeleted
        
   case nInfoType == UR_DBRI_LOCKED  && OK
        uInfo     := aWAData [ WA_LOCK ]
        
   case nInfoType == UR_DBRI_RECSIZE  && OK
        uInfo     := aWAData[ WA_RECSIZE ]
   
   case nInfoType == UR_DBRI_RECNO  && OK
        nResult   := SL_RECID( nWA, @nRecord )
        
   case nInfoType == UR_DBRI_UPDATED  && OK
        uInfo     := aWAData[ WA_RECORDCHANGED ]
        
   case nInfoType == UR_DBRI_ENCRYPTED
        uInfo     := .F.
   case nInfoType == UR_DBRI_RAWRECORD
        uInfo     := ""
   case nInfoType == UR_DBRI_RAWMEMOS
        uInfo     := ""
   case nInfoType == UR_DBRI_RAWDATA
        nResult   := SL_GOTOID( nWA, nRecord )
        uInfo     := ""
   endcase
#else
   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( nRecord )
   HB_SYMBOL_UNUSED( nInfoType )
   HB_SYMBOL_UNUSED( uInfo )
#endif

return nResult

*****************************
static function SL_FIELDNAME( nWA, nField, cFieldName ) && XBASE - FIELDNAME()
*****************************

   local aWAData   := USRRDD_AREADATA( nWA )
   local s_aStruct := aWAData[ WA_STRUCT ]
   local nResult   := SUCCESS

   #IfnDef __XHARBOUR__
   BEGIN SEQUENCE WITH {|oErr| Break( oErr )}
      cFieldName := s_aStruct[ nField ][ DBS_NAME ]

   RECOVER
      cFieldName := ""
      nResult    := FAILURE
   End
   #else
   TRY
      cFieldName := s_aStruct[ nField ][ DBS_NAME ]
   catch e
      ? e:description
      cFieldName := ""
      nResult    := FAILURE
   End
   #endif

return nResult

*****************************
static function SL_FIELDINFO( nWA, nField, nInfoType, uInfo )  && Rossine 07/10/08
*****************************

   local aWAData   := USRRDD_AREADATA( nWA )
   local s_aStruct := aWAData[ WA_STRUCT ]

   do case
   case nInfoType == DBS_NAME
        uInfo := s_aStruct[ nField ][ DBS_NAME ]

   case nInfoType == DBS_TYPE
        uInfo := s_aStruct[ nField ][ DBS_TYPE ]

/*
       nType := SL_GETFIELDTYPE( s_aStruct[ nField ][ DBS_TYPE ] )
       do case
       case nType == HB_FT_STRING
           uInfo := "C"
       case nType == HB_FT_LOGICAL
           uInfo := "L"
       case nType == HB_FT_MEMO
           uInfo := "M"
       case nType == HB_FT_OLE
           uInfo := "G"
       case nType == HB_FT_PICTURE
           uInfo := "P"
       case nType == HB_FT_ANY
           uInfo := "V"
       case nType == HB_FT_DATE
           uInfo := "D"
       case nType == HB_FT_DATETIME
           uInfo := "T"
       case nType == HB_FT_TIMESTAMP
           uInfo := "@"
       case nType == HB_FT_LONG
           uInfo := "N"
       case nType == HB_FT_INTEGER
           uInfo := "I"
       case nType == HB_FT_DOUBLE
           uInfo := "B"
       otherwise
           uInfo := "U"
       ENDcase
*/
   case nInfoType == DBS_LEN
        
        uInfo := s_aStruct[ nField ][ DBS_LEN ]
        // Un campo mayor de 1024 lo consideramos un campo memo
        uInfo := If( uInfo > 1024, 10, uInfo )

   case nInfoType == DBS_DEC

        uInfo := s_aStruct[ nField ][ DBS_DEC ]

#ifdef DBS_FLAG
   case nInfoType == DBS_FLAG
        uInfo := 0
#endif
#ifdef DBS_STEP
   case nInfoType == DBS_STEP
        uInfo := 0
#endif
   otherwise
       return FAILURE
   endcase

return SUCCESS

*****************************
function SL_CurrentServer  && Rossine 07/10/08
*****************************

local nWA := select(), aWAData := USRRDD_AREADATA( nWA )

return aWAData[ WA_POINTER ]

*************************
function SL_ExecQuery( aWAData, cQuery, cMsg, lMsg )  && Rossine 07/10/08
*************************

local lRet, oQuery

DEFAULT cMsg := "Não foi possível realizar a operação baixo: "
DEFAULT lMsg := .T.

HB_ExecFromArray( { FSL_EXECQUERY( aWAData[ WA_SYSTEMID ] ), @aWAData, cQuery, @oQuery, @lRet } )

if lRet .and. lMsg
   HB_ExecFromArray( { FSL_EXECQUERY_MSG( aWAData[ WA_SYSTEMID ] ), @oQuery, cMsg, cQuery } )
endif

HB_ExecFromArray( { FSL_EXECQUERY_DES( aWAData[ WA_SYSTEMID ] ), @oQuery } )

cQuery := ""

return !lRet

/* Faz querie que retorna apenas 1 valor de coluna */

**************************
function SL_QuickQuery( aWAData, cQuery )  && Rossine 07/10/08
**************************
local temp  // Funcionava sem isto aqui??? Como ???    o_O
   HB_ExecFromArray( { FSL_QUICKQUERY( aWAData[ WA_SYSTEMID ] ), @aWAData, cQuery, @temp } )
return temp

**********************
function SL_Commit  && Rossine 07/10/08
**********************

local nWA := select(), aWAData := USRRDD_AREADATA( nWA )

return HB_ExecFromArray( { FSL_COMMIT( aWAData[ WA_SYSTEMID ] ), aWAData } )

************************
function SL_Rollback  && Rossine 07/10/08
************************

local nWA := select(), aWAData := USRRDD_AREADATA( nWA )

return HB_ExecFromArray( { FSL_ROLLBACK( aWAData[ WA_SYSTEMID ] ), aWAData } )

*************************
function SL_DataToSql( cData )  && Rossine 07/10/08
*************************

return "'" + cData + "'"

**************************
function SL_ClearIndex( lAll )
**************************

local nWA := select(), aWAData := USRRDD_AREADATA( nWA )

DEFAULT lAll := .F.

return HB_ExecFromArray( { FSL_CLEARINDEX( aWAData[ WA_SYSTEMID ] ), aWAData, lAll } )

**************************
function SL_StartTrans
**************************

local nWA := select(), aWAData := USRRDD_AREADATA( nWA )

return HB_ExecFromArray( { FSL_STARTTRANS( aWAData[ WA_SYSTEMID ] ), aWAData } )

************************
function SL_EndTrans( aWAData )  && Rossine 07/10/08
************************

return HB_ExecFromArray( { FSL_ENDTRANS( aWAData[ WA_SYSTEMID ] ), aWAData } )


/*
*********************
function SL_IsLocked( nRecno, cSchema, cTabela ) && Não utilizado  Rossine
*********************

local cSql, lRet, cRecno

DEFAULT nRecno  := recno()
DEFAULT cSchema := SL_SetSchema()
DEFAULT cTabela := s_cTableName

cRecno := alltrim(str(nRecno))

cSql := "select * from public.isrowlocked('" + cSchema + "." + cTabela + "','" + SL_COL_RECNO + "','" + cRecno + "')"
lRet := SL_ExecQuery( cSql, , .F. )

if !lRet
   if ascan( s_aMyLocks, { |aLock| aLock[1] = nRecno .and. aLock[2] = cTabela } ) > 0
      lRet := .T.
   endif
endif

msgstop( valtoprg(lRet) + "-" + cSql, "Proc: " + PROCESSO() )

return lRet
*/

/*
CREATE OR REPLACE function a003.isrowlocked(text, text, text)
  RETURNS boolean AS
$BODY$
DECLARE
   myst TEXT;
BEGIN
   myst = 'SELECT 1 FROM '||$1||' WHERE '
   ||$2||'='||$3||' FOR UPDATE NOWAIT';
   EXECUTE myst;
   RETURN FALSE;
   EXCEPTION WHEN lock_not_available THEN
      RETURN TRUE;
END;
$BODY$
  LANGUAGE 'plpgsql' VOLATILE STRICT
  COST 100;

ALTER FUNCTION a003.isrowlocked(text, text, text) OWNER TO adilson;
*/

/*
CREATE OR REPLACE FUNCTION istablalock(text)
RETURNS BOOL
-- devuelve verdadero si la tabla esta bloqueada
-- el %1 es la tabla a bloquear
LANGUAGE plpgsql
VOLATILE
STRICT
AS
$gsm$
BEGIN
EXECUTE 'LOCK TABLE '||quote_ident($1) ||' IN SHARE ROW EXCLUSIVE MODE';
RETURN FALSE;
EXCEPTION WHEN lock_not_available THEN
    RETURN true;
END;
$gsm$;
*/

/*
** Implementação em TPOSTGRES.PRG para testar se SEQUENCIA existe: Rossine  06/10/08
...
    METHOD   SequenceExists( cSequence ) && Rossine
...
METHOD SequenceExists( cSequence ) CLASS TPQserver  && Rossine  06/10/08

    local result := .F.
    local cQuery
    local res
  
    cQuery := "select sequence_name "
    cQuery += "  from information_schema.Sequences "
    cQuery += " where sequence_name = " + SL_DataToSql(lower(cSequence)) + " and sequence_schema = " +  + SL_DataToSql( ::Schema )

    res := PQexec( ::pDB, cQuery )

    if PQresultstatus(res) == PGRES_TUPLES_OK
        result   := (PQlastrec(res) != 0)
        ::lError := .F.
        ::cError := ''    
    else
        ::lError := .T.
        ::cError := PQresultErrormessage(res)       
    endif
    
    PQclear(res)
    
return result    
*/

/*
 * Check if current indexkey is complex 
 * 20/01/2009 - 20:04:30 - Vailton Renato
 */
FUNCTION SL_ComplexCheck( cChave, aCampos )
   LOCAL a,b,c,d,i
   LOCAL aExp
   LOCAL lComplex:= FALSE
   LOCAL lFunc,lInFunc
   
   b := 0
   c := 1
   d := ''
   aExp := {}
   lInFunc := FALSE
   
   FOR i := 00 TO Len( cChave )
       a := SUBST(cChave,i,1)
   
       IF ( a $ '"{]'+chr(39) )
          b ++
          lComplex := TRUE
   
       ELSEIF ( a $ '"}]'+chr(39) )
          b --
          lComplex := TRUE
   
       ELSEIF ( a == '(' ) .OR. ; //.and. ( b == 00 )
              ( a == ',' ) .OR. ;
              ( a == ')' ) .OR. ;
              FALSE
   
          lFunc := ( a == '(' )
   
          d := SUBST( cChave, c, (i-c) )
          d := Upper(Alltrim( d ))
          c := i+1
   
          AADD( aExp, d )
   
          IF (d == 'STR') .OR. ( d == 'DTOS' )
            *Validacao por enquanto normal...
            DEBUG " * Validacao por enquanto normal...",d
            lInFunc := TRUE
          ELSEIF lInFunc .and. IsDigit(d)
            * É um numero, deixa passar
            DEBUG " * É um numero, deixa passar",d
          ELSEIF !lFunc .and. aScan( aCampos, {|_1|  Upper( _1[1] ) == d }) != 00
            * É um campo desta tabela... deixa passar
            DEBUG " * É um campo desta tabela... deixa passar",d
          ELSE
            DEBUG " * É um campo COMPLEXO!!",d
             lComplex := TRUE
          End
       End
   End
   RETURN lComplex

/*
 * Função para retornar se determinada tabela existe dentro do banco de dados
 * 23/01/2009 - 09:12 - Rossine
 */

*****************
**function SL_FILE( cFile )
*****************

**return iif( ascan( SQLGetTables(), lower(cFile) ) > 0, .T., .F. )

************************
function SL_DELETETABLE( cTableName, cSchema )
************************

   HB_SYMBOL_UNUSED( cTableName )
   HB_SYMBOL_UNUSED( cSchema )

   msginfo( 'ajustar isto!!!' )
return .f.
//   local aInfo := SL_GETCONNINFO( snConnHandle )

//return HB_ExecFromArray( { FSL_DELETETABLE( aInfo[9] ), snConnHandle, cTableName, cSchema } )

/*
 * Parse current indexkey to detect fieldnames & validate complex keys
 * 20/01/2009 - 20:06:50 - Vailton Renato
 */
FUNCTION SL_PARSEKEY( cKey, bIsComplex, aFields )
      LOCAL i,l,k,p
      LOCAL a,b,c,d,e,f,x
      LOCAL aExp
      LOCAL aResult := {}
      LOCAL lComplex:= FALSE

      b := 0
      c := 1
      f := ""
      p := Len( cKey )+1
      aExp := {}
      cKey += '+'
      
      FOR i := 00 TO p
          a := SUBST(cKey,i,1)

          IF LEN( f ) <> 0

            * Testa se ele está fechando a STRING corretamente
             IF ( f == a ) THEN;
                f := ""

        * Se ele abriu uma STRING
          ELSEIF ( a $ ["'] )
             f := a;
             
        * Abriu parenteses ou algo assim
          ELSEIF ( a $ '({]' )
             b ++

        * Fechou parenteses ou similar
          ELSEIF ( a $ ')}]' )
             b --
             
        * Concatenando STRINGs
          ELSEIF ( a == '+' ) .and. ( b == 00 )
             d := SUBST( cKey, c, (i-c) )
             d := Trim( d )
             c := i+1

             /* 
              * Corrigimos o problema do FIELD->CAMPO para o momento da indexação/
              * 13/08/2008 - 12:34:55
              */
             x := Lower(d)             
             if Left( x, 7 ) == 'field->' 
                d := Trim( Substr( d, 8 ) )
             elseif Left( x, 8 ) == 'field ->' 
                d := Trim( Substr( d, 9 ) )
             end              

             DEBUG d, aFields
             IF lComplex
                SL_ComplexCheck( @d, aFields )
             ELSE
                lComplex := SL_ComplexCheck( @d, aFields )
             End

             AADD( aExp, d )
          End
      End

      IF ( lComplex )
         bIsComplex := TRUE
         RETURN aExp
      End

      FOR a := 1 TO LEN( aExp )
          c := alltrim(aExp[a])
          d := ''
          k := c
          l := False

          FOR i := 1 TO LEN(c)
              e := SUBST( c, i, 1 )
              IF ( e == "(" )
                 l := True

              ELSEIF ( l ) .AND.(( e == ")" ) .OR. ;
                                 ( e == "/" ) .OR. ;
                                 ( e == "," ) .OR. ;
                                 ( e == "\" ) .OR. ;
                                 ( e == "+" ) .OR. ;
                                 ( e == "*" ))
                    k := alltrim(d)
                    EXIT
              ELSEIF ( l )
                 d += e
              END
          END
          AADD( aResult, k )
      END

      bIsComplex := FALSE
   RETURN aResult

/*
 * Convert a fieldname to a valid Clipper sintax
 * 20/01/2009 - 21:24:50
 */
FUNCTION SL_FLD2CHAR( aFieldInfo )
      LOCAL d,l,n,t,r

      d := aFieldInfo[ DBS_DEC  ]
      l := aFieldInfo[ DBS_LEN  ]
      n := aFieldInfo[ DBS_NAME ]
      t := aFieldInfo[ DBS_TYPE ]
      r := ''

      DO CASE
      CASE ( t == 'C' ) ; r := n
      CASE ( t == 'M' ) ; r := n
      CASE ( t == "N" ) ; r := "STR(" + n + "," + alltrim( str(l) ) + "," + alltrim( str(d) ) + ")"
      CASE ( t == "D" ) ; r := "DTOS(" + n + ")"
      CASE ( t == "L" ) ; r := n
      OTHERWISE
         SL_ERROR( 1002, "SQLLIB - Create Index on Column '"+n+"' not supported" )
      END
      RETURN r

/*
 * Monta o comando SQL da clausula ORDER BY conforme o indice ativo.
 * 18/03/2009 - 21:57:24
 */
FUNCTION SL_BuildOrderBy( aWAData, nDirection )
   LOCAL cOrderBy
   LOCAL aCurrIdx
   LOCAL aFields
   LOCAL cSep
   LOCAL lDown
   LOCAL c,i
   
   DEFAULT nDirection   TO MS_DOWN
   
   cOrderBy := ""
   cSep     := SQLSYS_SEP( aWAData[WA_SYSTEMID] )
   
   IF aWAData[ WA_INDEX_CURR ] == 0
      lDown    := (nDirection == MS_UP)
      aFields  := {}
   ELSE
      aCurrIdx := aWAData[ WA_INDEX, aWAData[ WA_INDEX_CURR ] ]
      aFields  := aCurrIdx[ IDX_FIELDS ]             

      IF aCurrIdx[ IDX_DESCEND ]
         lDown := (nDirection == MS_DOWN)
      ELSE
         lDown := (nDirection == MS_UP)
      End 
   End
   
   c := Len( aFields )

   FOR i := 1 TO c
       cOrderBy += cSep + aFields[i] + cSep + ;
                  iif( lDown, ' DESC, ', ' ASC, ' )
   End
   cOrderBy += cSep + SL_PKFIELD( aWAData ) + cSep + iif( lDown, ' DESC', ' ASC' )
   RETURN cOrderBy

/*
 * Monta as informações básicas para a montagem da clausula WHERE no comando SQL 
 * 19/03/2009 - 18:31:04
 */   
FUNCTION SL_BuildWhere( aWAData, nDirection )
   LOCAL aCurrIdx, aRules, aValues
   LOCAL nFCount, ID, i, p

   DEBUG nDirection
   
   IF aWAData[ WA_INDEX_CURR ] == 00
      DEBUG "Nenhum indice ativo, retornando..."
      RETURN {}
   End
   
   aCurrIdx := aWAData[ WA_INDEX, aWAData[ WA_INDEX_CURR ] ]
   nFCount  := Len( aCurrIdx[ IDX_FIELDS ] )       
   aValues  := Array( nFCount )
   ID       := aWAData[ WA_SYSTEMID ]

#ifdef SL_DEBUG
   IF ( nDirection == MS_DOWN )
      DEBUG "Estamos descendo no recorset..."
   ELSE
      DEBUG "Estamos subindo no recorset..."
   End
#endif   
   aRules := {}
   
   FOR i := 1 TO nFCount - 2
       AADD( aRules, IIF( nDirection == MS_DOWN, '>=', '<=' ) )
   End

   IF nFCount >= 2 
      AADD( aRules, ' =' )
   End
   
   IF nFCount >= 1 
      AADD( aRules, IIF( nDirection == MS_DOWN, ' >', ' <' ) )
   End

   DEBUG "Regras concluídas ..:", aRules      
   
   FOR i := 1 TO nFCount
     * Get index fieldname position
       p := aCurrIdx[ IDX_FIELDPOS, i ]  
       
       SL_GETVALUE_WA( aWAData, p, @aValues[i] )
       DEBUG "Valor original ..:", aValues[i]

       aValues[i] := SQLITEM2STR( aValues[i], ID )
       DEBUG "Valor convertido :", aValues[i]  
   End

   DEBUG "Valores convertidos :", aValues
   RETURN { aValues, aRules } 

/*
 * Monta as informações básicas para a montagem da clausula WHERE no comando SQL 
 * 19/03/2009 - 18:31:04
 */   
FUNCTION SL_BuildWhereStr( nWA, aWAData, lFirst, aDefaultRules, nDirection )
   LOCAL ID, sep, fld, pos, typ, i
   LOCAL aCurrIdx
   LOCAL aFields
   LOCAL szRecno
   LOCAL CurrRowId
   LOCAL aRules
   LOCAL aValues
   LOCAL nFCount
   LOCAL cWhere  := '('
   
   DEBUG_ARGS

   ID  := aWAData[ WA_SYSTEMID ]
   sep := SQLSYS_SEP( ID )

   IF aWAData[ WA_INDEX_CURR ] == 0 .OR. Empty( aDefaultRules )
      *
   ELSE  
      aCurrIdx := aWAData[ WA_INDEX, aWAData[ WA_INDEX_CURR ] ]
      aFields  := aCurrIdx[ IDX_FIELDS ]
            
      aValues  := aDefaultRules[1]
      nFCount  := Len( aValues )
      
      IF lFirst
         aRules := Array( nFCount )
         
         IF nDirection == MS_DOWN
            aFill( aRules, '>=' )
         ELSE
            aFill( aRules, '<=' )
         End
         
      ELSE
         aRules := aDefaultRules[2]
      End
      
      FOR i := 1 TO nFCount         
         fld := sep + aFields[i] + sep
         pos := aCurrIdx[ IDX_FIELDPOS, i ]  
         typ := AWAData[ WA_REAL_STRUCT, pos, DBS_TYPE ]
         
         IF ( typ == 'D' ) .and. Empty( aValues[i] )
            cWhere += '1 = 1 '
         ELSEIF ( typ == 'C' ) .and. Len( aValues[i] ) == 2    // empty string
            cWhere += '1 = 1 '
         ELSEIF ( Empty( aValues[i] ) )
            cWhere += '1 = 1 '
         ELSE
            IF typ == 'C'
               cWhere += fld + aRules[i] + "'" + aValues[i] + "' "
            ELSE
               cWhere += fld + aRules[i] +aValues[i] + ' '
            End
         End
         
         IF i != nFCount
            cWhere += "and "
         End
      End
   End
      
   IF lFirst .OR. aWAData[ WA_INDEX_CURR ] == 0
      szRecno := AWAData[ WA_REAL_STRUCT, AWAData[ WA_FLD_RECNO ], DBS_NAME ]
//DEBUG AWAData[ WA_REAL_STRUCT ]
//DEBUG AWAData[ WA_FLD_RECNO ]
      SL_GETVALUE_WA( nWA, AWAData[ WA_FLD_RECNO ], @CurrRowId, .T. )
      
      IF aWAData[ WA_INDEX_CURR ] == 0
         // Havia um BUG aqui antes que não testava o conteudo desta variavel,
         // foi ajustado em 25/05/2009 - 10:49:06 - vailton
         IF nDirection == MS_DOWN
            cWhere += sep + szRecno + sep + ' > ' + SQLITEM2STR( CurrRowId ) +' )'
         ELSE
            cWhere += sep + szRecno + sep + ' < ' + SQLITEM2STR( CurrRowId ) +' )'
         End
      ELSE
         IF nDirection == MS_DOWN
            cWhere += ' AND ' + sep + szRecno + sep + ' >= ' + SQLITEM2STR( CurrRowId ) +' )'
         ELSE
            cWhere += ' AND ' + sep + szRecno + sep + ' <= ' + SQLITEM2STR( CurrRowId ) +' )'
         End
      End                              
   ELSE
      cWhere += ")"
      
      /* Adjust next rules */
      aSize( aDefaultRules[1], Len(aDefaultRules[1]) -1 )                          
       aDel( aDefaultRules[2], 1 )
      aSize( aDefaultRules[2], Len(aDefaultRules[2]) -1 )
      
      i := Len(aDefaultRules[2])
                 
      IF i > 2
         aDefaultRules[2,i]   := IIF( nDirection == MS_DOWN, '>', '<' )
         aDefaultRules[2,i-1] := '='
      ELSEIF i = 1
         aDefaultRules[2,i]   := IIF( nDirection == MS_DOWN, '>', '<' ) // '='
      End                      
   End               
   
   DEBUG "WHERE COMPLETO: " + cWhere
   RETURN cWhere
   
#ifdef _n_ 
*UNCTION _MYSL_WHERE( pArea, nDirection, CurrRowId, lGoTopBott, szRecno )
      LOCAL Order, Index, Where, Fields, Fields2
      LOCAL i,j,f,v,f1,f2,t1,t2,sc1,sc2,r1,r2
      LOCAL SoftChar

      LOCAL rule, pos, tc
      LOCAL c

      DEBUG_ARGS

      DEFAULT nDirection   TO MS_DOWN
      DEFAULT lGoTopBott   TO False
      DEFAULT szRecno      TO AWAData[ WA_REAL_STRUCT, AWAData[ WA_FLD_RECNO ], DBS_NAME ]

      DEBUG_ARGS
      
      szRecno := RDD_SEP + szRecno + RDD_SEP

      CurrRowId := 
      IF ( _SL_SETAPPEND( pArea ) )
         CurrRowId := __GetColBuffer( __SETSQLAPPBUFFER( pArea ), 1, _SL_ROWID( pArea ) )
         TRACE ' ON APPENDIG: ROW ID CHANGED TO --> ' + CurrRowId

      ELSEIF ( VALTYPE( CurrRowId ) == 'N' )
         CurrRowId := NTrim(CurrRowId)
      End

      Order    := ListAllIndex( pArea )

      IF (Order == NIL) .or. Empty( Order ) THEN;
         RETURN ''

      Where    := "("
      Fields   := ""
      Fields2  := ""

      f1 := f2 := 00

      IF ( lGoTopBott )
         // NÆo faz nada...
      ELSE

         IF Empty( Order )
            j  := 00
         ELSE
            j  := LEN( Order[KEY_FNAMES] ) // KEY_LASTVALUES] )
         End

      // Total de Campos
         tc   := j
         pos  := 00

         IF ( nDirection == MS_DOWN )
         // As regras
            rule := {}

            for i := 1 to tc-2
                aadd( rule, '>=' )
            end

            if tc >= 2 then;
               aadd( rule, ' =' )
            if tc >= 1 then;
               aadd( rule, ' >' )
         ELSE
         // As regras
            rule := {}

            for i := 1 to tc-2
                aadd( rule, '<=' )
            end

            if tc >= 2 then;
               aadd( rule, ' =' )
            if tc >= 1 then;
               aadd( rule, ' <' )
         End

      // Regra inicial
         Where += '( '

         for c := 1 to tc
            // Pega o nome do campo
               f := '`' + Order[KEY_FNAMES][c] + '`'

            // Pega o valor do campo
               v := _MYSL_FETCH_COL( pArea,  Order[KEY_FPOS][c] )
            // v := SL_ANY2SEEK( v, TRUE ,,, Order[KEY_FTYPES][c] ) - 01-09-2005 : 06:15 hs remove o TRUE
               v := SL_ANY2SEEK( v, FALSE,,, Order[KEY_FTYPES][c] )

               IF ( nDirection == MS_DOWN )
                  IF ( Order[KEY_FTYPES][c] == 'D' ) .and. Empty( v )
                     Where += '1 = 1 and '
                  ELSEIF /*( Order[KEY_FTYPES][c] == 'C' ) .and.*/ ( v == "" )
                     Where += '1 = 1 and '
                  ELSE
                     Where += f + ' >= ' + v + ' and '
                  End
               ELSE
                  IF ( Order[KEY_FTYPES][c] == 'D' ) .and. Empty( v )
                     Where += f + 'IS NULL and '
                  ELSEIF /*( Order[KEY_FTYPES][c] == 'C' ) .and.*/ ( v == "" )
                     Where += '1 = 1 and '
                  ELSE
                     Where += f + ' <= ' + v + ' and '
                  End
               End
         end

         IF ( nDirection == MS_DOWN )
            Where += szRecno +' > ' + CurrRowId +' )'
         ELSE
            Where += '(' + szRecno +' < ' + CurrRowId +' OR ' + szRecno + ' IS NULL ))'
         End

      // Monta as outras regras
         FOR i := tc TO 1 STEP -1
             Where += ' OR ( '

             FOR c := 1 TO i
               // Pega o nome do campo
                  f := '`' + Order[KEY_FNAMES][c] + '`'

               // Pega o valor do campo
                  v := _MYSL_FETCH_COL( pArea,  Order[KEY_FPOS][c] )
               // v := SL_ANY2SEEK( v, TRUE ,,, Order[KEY_FTYPES][c] ) - 01/09/2005 : 06:15
                  v := SL_ANY2SEEK( v, FALSE,,, Order[KEY_FTYPES][c] )

                  IF ( Order[KEY_FTYPES][c] == 'D' )
                     IF Empty( v )
                        if ( c == 1 ) .and. ( i == 1 )
                           Where += '1=0'

                        elseif rule[pos+c] == '>='
                           Where += '1 = 1'
                        elseif rule[pos+c] == ' ='
                           Where += f + ' IS NULL'
                        elseif rule[pos+c] == ' >'
                           Where += f + ' IS NOT NULL'
                        elseif rule[pos+c] == ' <'
                           Where += f + ' IS NULL'
                        elseif rule[pos+c] == '<='
                           Where += f + ' IS NULL'
                        end

                     ELSE
                        if rule[pos+c] == ' <'
                           Where += '(' + f + ' < ' + v + ' OR ' + f + ' IS NULL )'
                        else
                           Where += f + rule[pos+c] + v
                        end
                     End

                  ELSEIF /*( Order[KEY_FTYPES][c] == 'C' ) .and.*/ ( v == "" )
                     Where += '1 = 1'

                  ELSE
                     Where += f + rule[pos+c] + v
                  End

                  Where += if(  c == i, '', ' and ' )
             End

             Where += ' )'
             pos ++
         End

         IF Empty( Order )
            *
         ELSEIF ( f2<>00 )
            IF nDirection == MS_DOWN
               Where += " OR (" + Fields2 + " AND "
               Where += szRecno + ' < ' + CurrRowId + " OR "+szRecno+" IS NULL"
               Where += ")"
            ELSE
               Where += " OR (" + Fields2 + " AND "
               Where += szRecno + ' > '+ CurrRowId
               Where += ")"
            End
         End

         Where += ") "
      End

      /*
       * SetScope v2.5
       */
      i := _SL_SCOPE(pArea,SC_SCOPETOP)
      j := _SL_SCOPE(pArea,SC_SCOPEBOTTOM)

      IF Empty(i) THEN;
         i := NIL

      IF Empty(j) THEN;
         j := NIL

      TRACE ' ###  --> ', i, j

      IF ( i == NIL ) .and. ( j == NIL )
         IF ( lGoTopBott ) THEN;
            Where := ""
      ELSE
         IF ( lGoTopBott )
            Where := ' ( '
         ELSE
            Where += " AND ( "
         End

         IF Empty( Order )
            Where += ' 1=1 '
         ELSE
         // Total de Campos
            tc := LEN( Order[KEY_FNAMES] ) // KEY_LASTVALUES] )
            t1 := 1
            sc1:= sc2 := ''
            
            /* bug fix - 13/08/2008 - 14:17:15 */
            if i = NIL then i := ''
            if j = NIL then j := ''

            FOR c := 1 TO tc
               // Pega o nome do campo
                  f := '`' + Order[KEY_FNAMES][c] + '`'
                  v := ''

               // Pega o TAMANHO do campo
                  t2:= Order[KEY_FSIZES][c]

               // Pega o valor do campo
                  f1:= Substr( i, t1, t2 )
                 *f2:= Substr( i, t1, t2 ) // 14-12-2005 - para pegar em parte
                  f2:= Substr( j, t1, t2 )

               // Ajusta a posição para o novo campo
                  t1+= t2

                  IF (f1=='') THEN r1 := '*'
                  IF (f2=='') THEN r2 := '*'

                  IF r1 <> '*' .and. r2 == '*'
                     r1 := '>='
                     
                  ELSEIF r1 = '*' .and. r2 <> '*'
                     r2 := '<='
                     
                  ELSEIF r1 <> '*' .and. r2 <> '*'
                     r1 := '>='
                     r2 := '<='
                     
                     if ( f1==f2 ) then r1 := r2 := ' ='
                     
                     if (r1 == ' =') .and. Len(f1)<t2 then r1 := ' LIKE '
                     if (r2 == ' =') .and. Len(f2)<t2 then r2 := ' LIKE '
                  End

                  IF (r1=='*') THEN sc1 += '1=1' //+ iif( c == tc, '', ' and ' )
                  IF (r2=='*') THEN sc2 += '1=1' // + iif( c == tc, '', ' and ' )
                  
                  /*
                   * 31/08/2006 @ 08:22hs
                   * Qdo dá um ordScope() numa tabela com campos caracter, ex:
                   *
                   * Campo: {{ 'NOME', 'C', 50, 0 }}
                   *
                   * Se filtrar por "J", "S"... ele não filtra certo, pq o filtro
                   * é montado assim: >= "J%" and <= "S%". Por isto houve esta
                   * revisão neste trecho de código. Detectado pela SysFarm.
                   *
                   *
                   * Ajeitar isto no PostgreSQL
                   */
                  SoftChar := nil
                  
                  IF !(f1 == '')
                     if (Len(f1)<t2) .and. (Order[KEY_FTYPES][c] == 'C') .and. (r1 <> ' LIKE ') then;
                        SoftChar := ''
                        
                     f1 := SL_ANY2SEEK( f1, FALSE,,Len(f1)<t2, Order[KEY_FTYPES][c], SoftChar )
                  End
                  
                  IF !(f2 == '')
                     if (Len(f2)<t2) .and. (Order[KEY_FTYPES][c] == 'C') .and. (r2 <> ' LIKE ')  then;
                        SoftChar := Chr(255)
                     f2 := SL_ANY2SEEK( f2, FALSE,,Len(f2)<t2, Order[KEY_FTYPES][c], SoftChar )
                  End

                  IF ( r1 <> '*' )
                     rule := r1
                     IF ( Order[KEY_FTYPES][c] == 'D' )
                        IF Empty( f1 )
                           if false // ( c == 1 ) .and. ( i == 1 )
                              sc1 += '1=0'
                           elseif rule == '>='
                              sc1 += '1 = 1'
                           elseif rule == ' ='
                              sc1 += f + ' IS NULL'
                           elseif rule == ' >'
                              sc1 += f + ' IS NOT NULL'
                           elseif rule == ' <'
                              sc1 += f + ' IS NULL'
                           elseif rule == '<='
                              sc1 += f + ' IS NULL'
                           end
                        ELSE
                           if rule == ' <'
                              sc1 += '(' + f + ' < ' + f1 + ' OR ' + f + ' IS NULL )'
                           else
                              sc1 += f + rule + f1
                           end
                        End
                     ELSE
                        sc1 += f + rule + f1
                     End
                  End
                  IF ( r2 <> '*' )
                     rule := r2
                     IF ( Order[KEY_FTYPES][c] == 'D' )
                        IF Empty( f2 )
                           if false // ( c == 1 ) .and. ( i == 1 )
                              sc2 += '1=0'
                           elseif rule == '>='
                              sc2 += '1 = 1'
                           elseif rule == ' ='
                              sc2 += f + ' IS NULL'
                           elseif rule == ' >'
                              sc2 += f + ' IS NOT NULL'
                           elseif rule == ' <'
                              sc2 += f + ' IS NULL'
                           elseif rule == '<='
                              sc2 += f + ' IS NULL'
                           end
                        ELSE
                           if rule == ' <'
                              sc2 += '(' + f + ' < ' + f2 + ' OR ' + f + ' IS NULL )'
                           else
                              sc2 += f + rule + f2
                           end
                        End
                     ELSE
                        sc2 += f + rule + f2
                     End
                  End

                  IF c <> tc
                     sc1 += ' and '
                     sc2 += ' and '
                  End
            End
         End
         
         IF Empty(sc2)
            Where += sc1 + ")"
                                                   
         /*
          * 31-08-2006 @ 08:34 coloquei para ajudar no filtro
          */
         ELSEIF sc1 == sc2
            Where += sc1 + ")"

         ELSE
            Where += '(' + sc1 + ') AND (' + sc2 + "))"
         End
      End
      ************************************************************

      IF SET(_SET_DELETED)
         IF !Empty( Where ) THEN;
            Where += " AND "

         Where += "( " + SL_COL_DELETED + " = 'F' ) "
      End

   // 14/03/2005
      IF ( __SQLFILTERLEN() <> 00 )
         IF Empty(Where)
            Where += '(' + SQLFilter( NIL, pArea ) + ')' // 25/10/2007 - 11:36:11 Colocamos os parenteses para evitar BUGs
         ELSE
            Where += ' and (' + SQLFilter( NIL, pArea ) + ')'
         End
      End

     IF !( lGoTopBott ) .and. !( nDirection == MS_DOWN ) THEN;
        Where += ' AND (' + szRecno +' <> ' + CurrRowId +') '

      RETURN Where
#endif 
 
**-------------------**
** Final de Programa **
**-------------------**

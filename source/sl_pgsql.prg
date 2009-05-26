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

REQUEST  SL_PGSQL_SQLARRAY

/*
 * Retorna o ID do driver para uso do PostgreSQL com SQLLIB
 */
**************
function PGSQL
**************

   /*
    * Registra os dados da estrutura STRUCT_INFO - implementado em postgres_api.c
    * de modo que o SQLLIB saber† que h† mais um DRIVER disponivel para ele. Este
    * tipo de aá∆o  facilitar† ao programador s¢ linkar os drivers que ele deseja
    * utilizar tais como MYSQL, POSTGRESQL, ORACLE, FIREBIRD, etc. 
    *
    * Fiz isto por achar a melhor opá∆o... ficou complexo, mas creio ser o modo
    * mais profissional de fragmentar o c¢digo e poder dividir o RDD em modulos.
    * Vailton - 14/12/2008 - 18:27:14
    */
   PGSQL_REGISTER_INFO()

return ID_POSTGRESQL

/*
 * A single alias for function above...
 * 15/12/2008 - 13:23:47 
 */
*******************
function POSTGRESQL
*******************

RETURN PGSQL()   
/*
 * Efetua uma conex∆o com o banco PostgreSQL e retorna o ponteiro da conexao
 * 12/12/2008 - 23:27:30
 */
*********************
function SQLLIB_PGSQL( cHost, nPort, cDb, cUser, cPwd, nFlags, cRddName, cSchema, cCharSet )
*********************
   LOCAL oSql
   
*  TRACE "SQLLIB_PGSQL( ", cHost, nPort, cDb, cUser, cPwd, nFlags, cRddName, " )"

   HB_SYMBOL_UNUSED( NFLAGS )
   HB_SYMBOL_UNUSED( CRDDNAME )
   HB_SYMBOL_UNUSED( CCHARSET )

   DEFAULT cHost   to "localhost"
   DEFAULT nPort   to 0
   DEFAULT cDb     to "template1"
   DEFAULT cSchema to "public"
   
   nPort  := IIF( nPort == 0, 5432, nPort )

/* Isto n„o È preciso mais, pois agora passamos a variavel cSchema como parametro. 03/01/09
   // Extract Schema from current cDB string (like "public.demos" 
   IF (( p := At( ".", cDb ) ) != 00 )
      cSchema := Substr( cDb, 1, p-1 )
      cDb     := Substr( cDb, p+1 ) 
   End
*/

   oSql := TPQServer():New( cHost, cDb, cUser, cPwd, nPort, cSchema ) 

   /* Any error? */
   IF oSql:NetErr() 
      SQLERR_SET( 8000, oSql:cError )
      
      DEBUG "A conexao falhou: " + oSql:cError
      
      oSQL : Destroy()
      oSQL = NIL
   End
   
   RETURN oSql

/*
 * Fecha a conex∆o com o banco de dados PostgreSQL representada pelo objeto oSQL
 * 13/12/2008 - 00:28:01
 */
*****************************
function SL_DISCONN_PGSQL( oSQL )
*****************************

      oSQL : QueryEx( "COMMIT" )
      oSQL : QueryEx( "COMMIT" )
      oSQL : Close()
               
   RETURN .T.
   
/*
 * Abre uma tabela j† existente
 * 18/12/2008 - 18:56:16
 */
***********************
function SL_OPEN_PGSQL( nWa, aWAData, aOpenInfo )
***********************

   LOCAL oSql        := aWAData[ WA_POINTER ]
   LOCAL nConn       := oSQL:pDB
   LOCAL cSchema     := aWAData[ WA_SCHEMA ]
   LOCAL aStruct     
   LOCAL cTableName
   LOCAL nFldCount
   LOCAL aField
   LOCAL lField
   LOCAL aTemp
   LOCAL i 

   HB_SYMBOL_UNUSED( nWA )

   IF Empty( cSchema )
      cSchema := "public"
   End

   /* Aqui o RDD deve converter o nome do arquivo para um formato correto dentro do BD */
   cTableName := SQLAdjustFn( aOpenInfo[ UR_OI_NAME ] )

   /* Update with (possible) correct values */
   aWAData[ WA_SCHEMA ]    := cSchema
   aWAData[ WA_TABLENAME ] := cTableName

   PGSQL_CHECKSYSTEMTABLES( nConn, cSchema )

   /* Check if requested table exists - 20/12/2008 - 22:12:34 */
   aStruct   := oSql:TableStruct( aWAData[ WA_TABLENAME ] )
   nFldCount := Len( aStruct )

   IF nFldCount < 1
      SL_ERROR( 1001, "PGSQL - Table or View does not exist",,, aWAData[ WA_TABLENAME ], 2 )
      RETURN FAILURE
   End
   
   aWAData[ WA_STRUCT ]      := {}
   aWAData[ WA_REAL_STRUCT ] := {}
   
   FOR i := 1 TO nFldCount    
       aField := aStruct[ i ]
       lField := Lower( aField[ DBS_NAME ] )
       
     * PK column?  
       IF     lField == SL_COL_RECNO .OR. ;
              lField == SR_COL_RECNO    
              aWAData[ WA_FLD_RECNO ] := i
         
     * Deleted flag?    
       ELSEIF lField == SL_COL_DELETED 
              aWAData[ WA_FLD_DELETED ] := i
           
     * Another field?      
       ELSE         
          aTemp := aClone( aField )          
          AADD( aTemp, i )    /* <------<<<             Indicates current pos */
          AADD( aWAData[ WA_STRUCT ], aTemp )
                                   
       End       
       AADD( aWAData[ WA_REAL_STRUCT ], aField )                    
   End

   **   SL_ExecQuery( aWAData, 'SET TRANSACTION READ WRITE ISOLATION LEVEL SERIALIZABLE' )
   **   SL_ExecQuery( aWAData, 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE')   
   RETURN SUCCESS

********************************************************************************
* Aqui tratamos todas as chamadas aos banco que forem necess·rias e que devemos 
* apenas retornar os  resultado ao SQLLIB indicando sucesso ou n„o da operaÁ„o.
* 18/12/2008 - 13:08:55
********************************************************************************
*************************
function SL_CREATE_PGSQL( nWa, aWaData, aOpenInfo )
*************************

   LOCAL cSql, cSequenceField 

   local oSql        := aWAData[ WA_POINTER ]
   LOCAL nConn       := oSQL:pDB
   LOCAL cSchema     := aWAData[ WA_SCHEMA ]
   LOCAL cTableName  := aWAData[ WA_TABLENAME ]
   LOCAL cFieldRecno := SL_COL_RECNO
   LOCAL lTemp       := .F.
   LOCAL lError      := .F.

   HB_SYMBOL_UNUSED( nWA )
   HB_SYMBOL_UNUSED( aOpenInfo )

   IF Empty( cSchema )
      cSchema := "public"
   End

   /* Aqui o RDD deve converter o nome do arquivo para um formato correto dentro do BD */
   cTableName := SQLAdjustFn( cTableName )

   /* Update with (possible) correct values */
   aWAData[ WA_SCHEMA ]    := cSchema
   aWAData[ WA_TABLENAME ] := cTableName
                                      
   /*
    * In some drivers - such as PostgreSQL - are needed more than one command
    * to create the table. This should be seen  within the routine that build
    * the SQL strings. (Vailton)
    * 15/09/2008 - 20:08:47
    */

   cSql := "DELETE FROM " + aWAData[ WA_SCHEMA ] + "." + SL_INDEX + " where " + ;
                                       '"indextable" = ' + "'" + aWAData[ WA_TABLENAME ] + "'"
**msgstop( csql )
   PGSQL_QUERY_LOG( nConn, cSQL, nil, .F., .T. )
                                       
   cSequenceField := cTableName + "_" + cFieldRecno

   cSql := 'DROP SEQUENCE "' + cSchema + '"."' + cSequenceField + '" CASCADE'
   PGSQL_QUERY_LOG( nConn, cSql, NIL, .F., .T. )

   /* We created the sequence to create the table */
   cSql := 'CREATE SEQUENCE "' + cSchema + '"."' + cSequenceField + '" INCREMENT 1'
   cSql += ' MINVALUE 1 CACHE 1'
   
   PGSQL_QUERY_LOG( nConn, cSql, NIL, .T., .T. )
   
   /*
    * … mais rapido mandarmos excluir a tabela, do que testar se ela existe e 
    * depois mandarmos excluir ela de l·. Ent„o para economizar banda, mandamos 
    * a tabela pro lixo e n„o exibimos nenhum msg de erro caso venha a ocorrer.
    *
    * AtÈ porque comandos de criaÁ„o de tabelas (DDL) tais como estes n„o podem
    * ser enviados dentro de um bloco de transaÁ„o e portanto qualquer msg de erro
    * nao ir„o comprometer dados importantes ainda pendentes ou v„o? (o_O)
    */   
   cSQL := 'DROP TABLE "' + cSchema + '"."' + cTableName + '"'
   PGSQL_QUERY_LOG( nConn, cSql, NIL, .F., .T. )

   /* We created the table now */
   cSql := 'CREATE TABLE "' + cSchema + '"."' + cTableName + '" (' +;
                               aWAData[ WA_TEMP, 1 ]
   
   cSql += ', "' + cFieldRecno + '"' + " NUMERIC(15,0) DEFAULT nextval('" +;
                    cSequenceField + "'::regclass) UNIQUE NOT NULL"

   IF !Empty( aWAData[ WA_TEMP,2 ] )
      cSql += ', CONSTRAINT ' + cTableName + "_" +  SL_CONSTRAINT_PK  + ' PRIMARY KEY (' +  aWAData[ WA_TEMP,2 ] + ')'
   End

   cSql += ')'
   aWAData[ WA_TEMP ] := nil

   /* Aqui executamos o comando e deixamos a msg de erro aparecer ao usuario */
   PGSQL_QUERY_LOG( nConn, cSql, @lError, .T., .T. )

   IF !lError .AND. !lTemp 
      PQclear( PQexec( nConn, "COMMIT" ))
   End
   RETURN SUCCESS

*****************************
function SL_CREATEFLDS_PGSQL( nWa, aWAData, aStruct )
*****************************

   LOCAL aField
   LOCAL cType
   LOCAL xDefault
   LOCAL nLen, nDec
   LOCAL cSQL
   LOCAL cConstraintPK := ''
   LOCAL n

   HB_SYMBOL_UNUSED( nWA )

   AAdd( aStruct, { SL_COL_DELETED, "C",  1, 0 } ) // to emulate DBF deleted value
 
   cSQL := ""
   aWAData[ WA_TEMP ]      := {'',''}
   aWAData[ WA_FCOUNT ]    := Len( aStruct ) + 1
   aWAData[ WA_REAL_STRUCT]:= {}

   FOR n := 1 TO Len( aStruct ) 
       aField := aStruct[ n ]                                  // field info
       cType  := Upper( Left( alltrim( aField[ DBS_TYPE ] ), 1))  // fieldtype
       nLen   := aField[ DBS_LEN ]
       nDec   := aField[ DBS_DEC ]          
       xDefault := ''
       
       IF n > 1
          cSQL += ", "          
       End
 
       IF ( Empty( aField[ DBS_NAME ] ) )
          SL_ERROR( 1028, "PGSQL - Invalid field Name: '" +  aField[ DBS_NAME ] + "' at pos " + alltrim(str(n)) )
          RETURN FAILURE
       End

       DO CASE
       /*
        * Aqui aceitamos campos com valores n„o-padr„o, desde que estejam
        * devidamente indicados - precedidos por '@' ex: bytearray, etc..
        */
       CASE ( Left( cType, 1 ) == '@' )
           cType := Substr( aField[ DBS_TYPE ], 2 )
           
       CASE Len( aField ) >= DBS_FIELD_TYPE .AND. ;
            VALTYPE( aField[ DBS_FIELD_TYPE ] ) == 'L' .AND. aField[DBS_FIELD_TYPE]

           cType := aField[ DBS_TYPE ]

       CASE ( cType == "C" )
           cType := "VARCHAR"
           xDefault := "' '"

       CASE ( cType == "N" )
           cType := "NUMERIC"
           xDefault := "0"

       CASE ( cType == "M" )
         // Changed: Vailton REnato at 22/08/2005 : 17:58, new column type for memo fields
           cType := "TEXT"

       CASE ( cType == "D" )
           cType := "DATE"
*           xDefault := "'" + SL_NULLDATE + "'"

       CASE ( cType == "L" )
           cType := "BOOLEAN" 
           xDefault := 'FALSE'
         
       OTHERWISE
           SL_ERROR( 1028, "PGSQL - Invalid field type: '" + cType + "' for field " + aField[ DBS_NAME ] )
           RETURN FAILURE
       End
       
       /* Name of the fields in lower case */
       cSQL += '"' + lower( aField[ DBS_NAME ] ) + '" ' + cType
       
       IF ( ';' + UPPER(cType) + ';' $ ';DATE;TEXT;BOOLEAN;SERIAL;' )
          * This field type does not support length modifier
       ELSEIF nLen == -1
          * -1 disables size modifier inside sql string
       ELSE
          cSQL += " (" + Alltrim(Str( nLen ))
   
          IF ( cType == "NUMERIC" .and. nDec <> 00 )
             IF nLen >= nDec+2
                cSQL +=  "," + Alltrim(Str( nDec ))
             ELSE
                SL_ERROR( 1028, "PGSQL - Invalid field length for field " +;
                                          aField[ DBS_NAME ] + " '" + cType +"'," + ;
                                          alltrim( str( nLen ))+','+ alltrim( str( nDec )) )
                RETURN FAILURE
             End
          End
   
          cSQL +=  ")"
       END
   
       IF ( Len( aField ) >= DBS_DEFAULT )
          IF VALTYPE( aField[ DBS_DEFAULT ]) == 'C'
             IF LEFT( aField[ DBS_DEFAULT ],1) == '@'
                xDefault := SubStr( aField[ DBS_DEFAULT ], 2 )
             ELSE
                xDefault := "'" + PQESCAPESTRING( aField[ DBS_DEFAULT ] ) + "'"
             End
          End
          IF VALTYPE( aField[ DBS_DEFAULT ]) == 'N'
             xDefault := alltrim( str( aField[ DBS_DEFAULT ] ))
          End
          IF VALTYPE( aField[ DBS_DEFAULT ]) == 'D'
             xDefault := "'" +;
                  STRZERO( YEAR( aField[ DBS_DEFAULT ] ), 4 ) + '-' +;
                  STRZERO(MONTH( aField[ DBS_DEFAULT ] ), 2 ) + '-' +;
                  STRZERO(  DAY( aField[ DBS_DEFAULT ] ), 2 ) + "'"
          End
          IF VALTYPE( aField[ DBS_DEFAULT ]) == 'L'
             xDefault := iif( aField[ DBS_DEFAULT ], 'TRUE', 'FALSE' )
          End
       End
       
       IF ( Len( aField ) >= DBS_REQUIRED )
          IF (VALTYPE( aField[ DBS_REQUIRED ]) == 'L' .AND. ;
                       aField[ DBS_REQUIRED ] ) 
             cSQL += ' NOT NULL'
          End
       End

       IF ( Len( aField ) >= DBS_UNIQUE )
          IF (VALTYPE( aField[ DBS_UNIQUE ]) == 'L' .AND. ;
                       aField[ DBS_UNIQUE ] ) 
             cSQL += ' UNIQUE'
          End
       End
       
       IF !Empty( xDefault ) 
          cSQL +=  " DEFAULT " + xDefault
       End              

       /*
        * Testamos se este campo È uma chave prim·ria.
        * 24/05/2009 - 01:49:31
        */
       IF ( Len( aField ) >= DBS_PRIMARY_KEY )
          IF (VALTYPE( aField[ DBS_PRIMARY_KEY ]) == 'L' .AND. ;
                       aField[ DBS_PRIMARY_KEY ] ) 
                                 
             IF !Empty( cConstraintPK )
                cConstraintPK += ','
             End
               
             cConstraintPK += '"' + lower( aField[ DBS_NAME ] ) + '" '
          End
       End
   End

   aWAData[ WA_TEMP,1 ] := cSQL
   aWAData[ WA_TEMP,2 ] := cConstraintPK
   RETURN SUCCESS
   
/*
 * Run a specific query and updates the internal flags.
 * 22/12/2008 - 15:48:28
 */
***********************************
function PGSQL_EXECANDUPDATE( nWa, aWAData, cQuery, nDirection, nOptions )
***********************************

   LOCAL oConn   := aWAData[ WA_CONNECTION ]
   LOCAL lAdjust := .F.
   LOCAL pResult
   LOCAL nRows
   LOCAL xOldKey, xNewKey

   DEBUG nWa, cQuery, nDirection, nOptions

 * IF aWAData[ WA_RESULT_DIRECTION ] == MS_NONE
   IF (nDirection == MS_DOWN) .OR. (nDirection == MS_UP)
      * Ok
   ELSE
      SL_ERROR( 1000, "Invalid buffer direction ==> " + alltrim( str( aWAData[ WA_RESULT_DIRECTION ] )), .F., .F., aWAData[ WA_TABLENAME ] )
      RETURN FAILURE
   End

   DEFAULT nDirection   TO MS_NONE
   DEFAULT nOptions     TO EU_IGNORE_FIRST
   
   pResult := PQExec( oConn:pDB, cQuery )
   
   IF PQresultstatus( pResult ) != PGRES_TUPLES_OK
      PQClear( pResult )
      RETURN FAILURE
   End

   nRows := PQNTUPLES( pResult )
   
   IF SL_HAS( nOptions, EU_IGNORE_FIRST )
      xOldKey := aWAData[ WA_RECNO ]
      xNewKey := PQGETVALUE( pResult, 1, aWAData[ WA_FLD_RECNO ] )

      DEBUG xOldKey, xNewKey

      IF ValType( xOldKey ) != 'C'
         xOldKey := SQLITEM2STR( xOldKey )
      End

    * BUG: Erroneous first record is current record?
    *      20/03/2009 - 16:55:58
      IF xOldKey != xNewKey
         * Ok, no error
         DEBUG 'Nenhum ajuste no KEY necess·rio:', xOldKey, xNewKey
      ELSEIF nRows > 1
         lAdjust := .T.
         DEBUG 'Ser· necess·rio ignorar o primeiro registro!', xOldKey, xNewKey
      ELSE
         * Force EOF!
         DEBUG 'EOF detectado!'

         nRows := 0     // forÁa EOF() / BOF() - 25/05/2009 - 21:23:49

//         pResult := PQExec( oConn:pDB, aWAData[ WA_SL_GOTOP ] + " WHERE 1=0" )

//         IF PQresultstatus( pResult ) != PGRES_TUPLES_OK
//            PQClear( pResult )
//            RETURN FAILURE
//         End
      End
   End
   
   * We want to ignore if an empty result-set has found?   25/05/2009 - 11:36:34
   IF ( nRows < 1 ) .AND. ( SL_HAS( nOptions, EU_BOF_ON_EMPTY ) .OR. ;
                            SL_HAS( nOptions, EU_EOF_ON_EMPTY ) )

      PQClear( pResult )
      
      IF SL_HAS( nOptions, EU_BOF_ON_EMPTY )
         aWAData[ WA_BOF ] := True
      End
      IF SL_HAS( nOptions, EU_EOF_ON_EMPTY )
         aWAData[ WA_EOF ] := True
      End

      DEBUG "Ignorando Result-set vazio!", aWAData[ WA_BOF ], aWAData[ WA_EOF ]
      RETURN SL_UpdateFlags( nWA, aWAData )
   End
   
   IF ValType( aWAData[ WA_RESULT ] ) == "P"
      PQClear( aWAData[ WA_RESULT ] )
      aWAData[ WA_RESULT ] := nil
   End

   aWAData[ WA_RESULT ]          := pResult
   aWAData[ WA_RESULT_DIRECTION ]:= nDirection

   aWAData[ WA_BUFFER_POS ]      := iif( lAdjust, 2, 1 )
   aWAData[ WA_BUFFER_ROWCOUNT ] := nRows

   /* Update recno position */
   IF aWAData[ WA_TABLETYPE ] == TS_COMPLEX_SQL
      aWAData[ WA_RECNO ] := 1
   ELSE
      aWAData[ WA_RECNO ] := SL_GETVALUE_PGSQL( nWa, aWAData, aWAData[ WA_FLD_RECNO ], .T. )
   End

   aWAData[ WA_BOF ] := aWAData[ WA_BUFFER_ROWCOUNT ] < 1
   aWAData[ WA_EOF ] := aWAData[ WA_BUFFER_ROWCOUNT ] < 1
   RETURN SL_UpdateFlags( nWA, aWAData )

*************************
function SL_GOTOID_PGSQL( nWa, aWAData, nRecno )
*************************

   LOCAL cSQL

  cSQL := SQLPARAMS( aWAData[ WA_SL_GOTOID ], { AllTrim( Str( nRecNo ) ) }, ID_POSTGRESQL )
  
  RETURN PGSQL_ExecAndUpdate( nWa, aWAData, cSQL, MS_DOWN, EU_IGNORE_FIRST )


/*
 * Perform DbGoTop() on current WA. 
 * TODO: Convert this routine into a C function to turn it faster ************** 
 * 22/12/2008 - 14:13:07
 */       
FUNCTION SL_GOTOP_PGSQL( nWa, aWAData )
   LOCAL cSQL 
    
   cSQL := SQLParams( "\? ORDER BY \? LIMIT \? ",; 
                      { aWAData[ WA_SL_GOTOP ], ;
                        SL_BUILDORDERBY( aWAData, MS_DOWN ), ;
                        STR( aWAData[ WA_PACKET_SIZE ] ) ;
                      }  ,;
            ID_POSTGRESQL )
            
  RETURN PGSQL_ExecAndUpdate( nWa, aWAData, cSQL, MS_DOWN, EU_IGNORE_NONE )

/*
 * Perform DbGoTop() on current WA
 * 22/01/2009 - 12:07:04
 */       
FUNCTION SL_GOBOTTOM_PGSQL( nWa, aWAData ) && DbGoBottom()
   LOCAL cSQL
   
   cSQL := SQLParams( "\? ORDER BY \? LIMIT \? ",; 
                      { aWAData[ WA_SL_GOBOTTOM ], ;
                        SL_BUILDORDERBY( aWAData, MS_UP ), ;
                        STR( aWAData[ WA_PACKET_SIZE ] ) ;
                      }  ,;
            ID_POSTGRESQL )
            
  RETURN PGSQL_ExecAndUpdate( nWa, aWAData, cSQL, MS_UP, EU_IGNORE_NONE )

**************************
function SL_DELETED_PGSQL( nWa, aWAData, nRecno )
**************************

local cRddSep := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )
local cSql

cSql := "SELECT " + cRddSep + SL_COL_DELETED + cRddSep + " FROM " + SQLGetFullTableName( aWAData ) + ;
        ' where "' + SL_PKFIELD( nWA ) + '" = ' + AllTrim( Str( nRecNo ) )

**   msgstop( cSql, "cSql DELETED SQLLIB Line " + LTrim( Str( ProcLine( 0 ) ) ) )

**   SL_ExecQuery( @cSql, , @oQuery )  && Rossine 07/10/08
**   lDeleted := oQuery:getrow( oQuery:nRecno ):aRow[1]
**   oQuery:destroy()

return iif( !set( _SET_DELETED ) .and. SL_QuickQuery( aWAData, cSql ) = "T", .T., .F. )  && Rossine 07/10/08

*************************
function SL_DELETE_PGSQL( nWa, aWAData, nRecno )
*************************

local cRddSep := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )
local cSql

cSql :=iif( set( _SET_DELETED ), "UPDATE " + SQLGetFullTableName( aWAData ) + " SET " + cRddSep + SL_COL_DELETED + cRddSep + " = 'T'" + ' where "' + SL_PKFIELD( nWA ) + '" = ' + AllTrim( Str( nRecNo ) ), ;
                                 "delete from " + SQLGetFullTableName( aWAData ) + ' where "' + SL_PKFIELD( nWA ) + '" = ' + AllTrim( Str( nRecNo ) ) )

return SL_ExecQuery( aWAData, cSql )

***************************
function SL_RECCOUNT_PGSQL( nWa, aWAData )
***************************   
   LOCAL cRddSep := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )
   LOCAL cField  := cRddSep + SL_PKFIELD( nWA ) + cRddSep
   LOCAL cTemp   := '0'
   LOCAL cSql

   cSql := 'SELECT ' + cField +;
           ' FROM '  + SQLGetFullTableName( aWAData ) + ;
           ' ORDER BY ' + cField + ' DESC'+;
           ' LIMIT 1'
    
   SL_QuickQuery_PGSQL( aWAData, cSql, @cTemp )
   aWAData[ WA_RECCOUNT ] := VAL( cTemp )
   RETURN SUCCESS

/*
 * Retrieve a value from original buffer
 * 22/12/2008 - 16:56:02
 */
***************************
function SL_GETVALUE_PGSQL( nWa, aWAData, nField, lHidden )
***************************

   LOCAL aField, nRow
   
   DEFAULT lHidden TO .F.
   
   /* Include hidden fields? */
   IF !lHidden
      nField := Atail( aWAData[ WA_STRUCT, nField ] )
   End
   
   aField := aWAData[ WA_REAL_STRUCT, nField ]
   nRow   := aWAData[ WA_BUFFER_POS ]
   
   IF Valtype( aWAData[ WA_RESULT ] ) == 'P'
      RETURN PQGetValueEx( nWA, aWAData[ WA_RESULT ], nRow, nField, aField )
   End

   RETURN nil

***************************
function SL_PUTVALUE_PGSQL( nWa, aWAData, xValue, nField )
***************************

local cSql
local s_aStruct := aWAData[ WA_STRUCT ]
local cRddSep   := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )

cSql := "UPDATE " + SQLGetFullTableName( aWAData ) + " SET "
cSql += cRddSep + s_aStruct[ nField ][ DBS_NAME ] + cRddSep + " = "

/* Get formated value */
do case
case s_aStruct[ nField ][ DBS_TYPE ] == "C" && "CHAR"

     cSql += "'" + StrTran( xValue, "'", "\'" ) + "'"    // StrTran() to emulate escape

case s_aStruct[ nField ][ DBS_TYPE ] == "M" && "MEMO"

     cSql += "'" + StrTran( xValue, "'", "\'" ) + "'"    // StrTran() to emulate escape
     
case s_aStruct[ nField ][ DBS_TYPE ] == "L" && "LOGICAL"

     cSql += IIF( xValue, "TRUE", "FALSE" )         // http://www.postgresql.org/docs/8.1/interactive/datatype-boolean.html

case s_aStruct[ nField ][ DBS_TYPE ] == "D" && "DATE"

     IF Empty( xValue )
        cSql += "NULL"
     ELSE
        cSql += "'" + DTOS( xValue ) + "'"
     End

otherwise  // Numeric field..
     cSql += AllTrim( Str( xValue ) )

End

cSql += " where " + cRddSep + SL_PKFIELD( nWA ) + cRddSep + " = " 
cSql += AllTrim( Str( aWAData[WA_RECNO] ) ) 

return SL_ExecQuery( aWAData, cSql )

***********************
function SL_INFO_PGSQL( nWa, aWAData )
***********************

local cSql

   HB_SYMBOL_UNUSED( nWA )
**   cSql := "select * from pg_locks"

**   lRet := SL_QuickQuery( aWAData, cSql )

/*
   cSql := 'SELECT'                                                                     + CRLF + ;                      
           '  pid                AS "Pid",'                                             + CRLF + ;                      
           '  virtualtransaction AS "VT",'                                              + CRLF + ;                      
           '  client_addr        AS "estaÁ„o",'                                         + CRLF + ;                      
           '  D.datname          AS "base de dados",'                                   + CRLF + ;                      
           '  relname            AS "tabela",'                                          + CRLF + ;                      
           '  C.reltuples        AS "Rows",'                                            + CRLF + ;                      
           '  CASE'                                                                     + CRLF + ;                      
           "    WHEN mode = 'AccessShareLock'  THEN 'Acesso a tabela'"                  + CRLF + ;                      
           "    WHEN mode = 'ExclusiveLock'    THEN 'Travamento da tabela'"             + CRLF + ;                      
           "    WHEN mode = 'RowShareLock'     THEN 'Acesso ao registro da tabela'"     + CRLF + ;                      
           "    WHEN mode = 'RowExclusiveLock' THEN 'Travamento do registro da tabela'" + CRLF + ;                      
           '  END AS "modo"'                                                            + CRLF + ;                      
           'FROM pg_locks L'                                                            + CRLF + ;                      
           "  INNER JOIN pg_class C         ON C.oid = L.relation AND C.relkind = 'r'"  + CRLF + ;                      
           '  INNER JOIN pg_database D      ON D.oid = L.database'                      + CRLF + ;                      
           '  INNER JOIN pg_stat_activity S ON S.procpid = L.pid'                       + CRLF + ;                      
           'where'                                                                      + CRLF + ;                      
           "  relname NOT LIKE 'pg_%'"                                                  + CRLF + ;                      
           'ORDER BY Pid, modo, client_addr, relname;'                                  + CRLF                          
*/
**   cSql := "SELECT " + cRddSep + SL_PKFIELD( nWA ) + cRddSep + " FROM " + SQLGetFullTableName( aWAData ) + " AS a, " + aWAData[ WA_SCHEMA ] + ".pgrowlocks('" + SQLGetFullTableName( aWAData ) + "') AS p where p.locked_row = a.ctid"
**   cSql := "SELECT " + SL_PKFIELD( nWA ) + " FROM " + SQLGetFullTableName( aWAData ) + " AS a, " + aWAData[ WA_SCHEMA ] + ".pgrowlocks('" + SQLGetFullTableName( aWAData ) + "') AS p where p.locked_row = a.ctid"
**   cSql := "SELECT * FROM " + SQLGetFullTableName( aWAData ) + " AS a, " + aWAData[ WA_SCHEMA ] + ".pgrowlocks('" + SQLGetFullTableName( aWAData ) + "') AS p where p.locked_row = a.ctid"

cSql := "SELECT * FROM " + SQLGetFullTableName( aWAData ) + " AS a, public.pgrowlocks('" + SQLGetFullTableName( aWAData ) + "') AS p where p.locked_row = a.ctid"

return SL_QuickQuery( aWAData, cSql )

***********************
function SL_LOCK_PGSQL( nWa, aWAData, nRecno, s_aMyLocks )
***********************

local cSql,n
local cRddSep := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )

**   cSql := "select waiting from pg_stat_activity where waiting = 't' and current_query = 'SELECT * FROM " + SQLGetFullTableName( aWAData ) + ' where ' + cRddSep + SL_PKFIELD( nWA ) + cRddSep + ' = ' + alltrim(str(aWAData[ WA_RECNO ])) + " FOR UPDATE'"
**   cSql := "select * from pg_stat_activity" && where current_query = 'SELECT * FROM " + SQLGetFullTableName( aWAData ) + ' where ' + cRddSep + SL_PKFIELD( nWA ) + cRddSep + ' = ' + alltrim(str(aWAData[ WA_RECNO ])) + " FOR UPDATE'"
**   lRet := SL_QuickQuery( aWAData, cSql )

**memowrit( "sql.txt", valtoprg(lRet) )
**msgstop( valtoprg(lret) )

**   SL_ExecQuery( aWAData, 'copy (' + cSql + ' ) TO E' + "'\\Temp\\tempor.txt' with delimiter '|'" )

**cSql := 'BEGIN WORK;' + CRLF + ;
**        'SAVEPOINT MEU_PONTO;' + CRLF + ;
**        'SELECT * FROM ' + SQLGetFullTableName( aWAData ) + ' where ' + cRddSep + SL_PKFIELD( nWA ) + cRddSep + ' = ' + alltrim(str(aWAData[ WA_RECNO ])) + ' FOR UPDATE;' + CRLF + ;
**        'ROLLBACK TO MEU_PONTO;' + CRLF + ;
**        'SELECT * FROM ' + SQLGetFullTableName( aWAData ) + ' limit 1;' + CRLF + ;
**        'COMMIT WORK;'

**lRet := SL_QuickQuery( aWAData, cSql )

**SQLSTARTTRANS()
**try
**cSql := 'BEGIN WORK;' + CRLF + ;
**        'SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED ;' + CRLF + ;
**        'SAVEPOINT MEU_PONTO;' + CRLF + ;

**SQLSTARTTRANS()

**        'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;' + CRLF + ;
**cSql := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;' + CRLF + ;

cSql := 'commit;'        + CRLF + ;  && Rossine 01/11/08
        'begin work;'    + CRLF + ;
        'select * from ' + SQLGetFullTableName( aWAData ) + ' where ' + cRddSep + SL_PKFIELD( nWA ) + cRddSep + ' = ' + ;
                           alltrim(str( nRecno )) + ' for update nowait;' + CRLF

if len(dbrlocklist()) > 0  && Rossine 01/11/08
   for n = 1 to len(s_aMyLocks)
       if s_aMyLocks[n,2] = SQLGetFullTableName( aWAData ) .and. s_aMyLocks[n,1] != nRecno
          cSql += 'select * from ' + SQLGetFullTableName( aWAData ) + ' where ' + cRddSep + SL_PKFIELD( nWA ) + cRddSep + ' = ' + ;
                  alltrim(str(s_aMyLocks[n,1])) + ' for update nowait;' + CRLF
       endif
   next
endif

**lRet := SL_QuickQuery( aWAData, cSql )
**msgstop( valtoprg(lRet) + "-" + cSql, "Proc: " + PROCESSO() )

**msgstop( cSql, "Proc: " + PROCESSO() )
**catch
**msgstop( "-2-" )
**end

return SL_ExecQuery( aWAData, cSql, , .F. )

***********************
function SL_PACK_PGSQL( nWa, aWAData )
***********************

   LOCAL cRddSep := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )
   LOCAL cSql

   HB_SYMBOL_UNUSED( nWA )
   cSql := "DELETE FROM " + SQLGetFullTableName( aWAData ) + " where " + cRddSep + SL_COL_DELETED + cRddSep + " = 'T'" &&  // __dbPack()"

return SL_ExecQuery( aWAData, cSql )

**********************
function SL_ZAP_PGSQL( nWa, aWAData )
**********************

local cSql, cSequenceField := "sequencia_" + SL_PKFIELD( nWa )
 
**   msgstop( cSql, "cSql ZAP SQLLIB Line " + LTrim( Str( ProcLine( 0 ) ) ) )

cSql := "DELETE FROM " + SQLGetFullTableName( aWAData ) && + " // __dbZap()"

SL_ExecQuery( aWAData, cSql )

cSql := "ALTER SEQUENCE " + aWAData[ WA_SCHEMA ] + '.' + cSequenceField + ' START 1'

SL_ExecQuery( aWAData, cSql )

cSequenceField := aWAData[ WA_TABLENAME ] + "_" + SL_PKFIELD( nWa )

cSql := "ALTER SEQUENCE " + aWAData[ WA_SCHEMA ] + '.' + cSequenceField + ' START 1'

SL_ExecQuery( aWAData, cSql )

cSql := 'VACUUM FULL ANALYZE ' + SQLGetFullTableName( aWAData )

return SL_ExecQuery( aWAData, cSql )

****************************
function SL_ORDCREATE_PGSQL( nWa, aWAData, aOrderCreateInfo, aFields, aKeys, aSizes )
****************************
   LOCAL aValues
	LOCAL cSql
	LOCAL cIdx
	LOCAL cBag
	LOCAL cTag
	LOCAL lDesc
	LOCAL n
   local pSQL 		:= aWAData[ WA_POINTER ]:pDB
   local nVersion := aWAData[ WA_VERSION  ]
   local oError   
   local aOrderInfo
   
   // Add ID_PREFIX to void conflit with reserved words (vailton - 15/01/2009 - 22:08:54)
   cIdx := ID_PREFIX + lower(aOrderCreateInfo [UR_ORCR_BAGNAME]) // + iif( empty( aOrderCreateInfo [UR_ORCR_TAGNAME] ), "", "_" ) + lower(aOrderCreateInfo [UR_ORCR_TAGNAME])
   cBag := cIdx + iif( empty( aOrderCreateInfo [UR_ORCR_TAGNAME] ), "", "_" ) + lower(aOrderCreateInfo [UR_ORCR_TAGNAME])
   cTag := strtran( lower(aOrderCreateInfo [UR_ORCR_TAGNAME]), "'", "" )      
   cSql := 'DROP INDEX "' + aWAData[ WA_SCHEMA ] + '"."' + cBag + '"'   

   if aOrderCreateInfo [UR_ORCR_UNIQUE]
      oError := ErrorNew()
      oError:GenCode     := EG_CREATE
      oError:subCode     := 1004
      oError:description := HB_LANGERRMSG( EG_CREATE ) + " (" + ;
                            HB_LANGERRMSG( EG_UNSUPPORTED ) + ")"
      oError:FileName    := cIdx
      oError:CanDefault  := .F.
      UR_SUPER_ERROR( nWA, oError )
      return FAILURE
   endif

   PGSQL_QUERY_LOG( pSQL, cSql,,, True )
   
   cSql := 'DELETE FROM "' + aWAData[ WA_SCHEMA ] + '"."' + SL_INDEX + '" where ' + ;
                                      '"indextable" = ' + "'" + aWAData[ WA_TABLENAME ] + "' and  " + ;
                                      '"indexfile" = '  + "'" + cIdx + "'"
                                      
   IF !Empty( cTag )
      cSQL += " and indextag = '" + cTag + "'"
   End
   
   PGSQL_QUERY_LOG( pSQL, cSql,,, True )
      
   DEBUG aOrderCreateInfo
         
   cSql  := 'CREATE INDEX "' + cBag + '" ON "' + aWAData[ WA_TABLENAME ] + '" ( '   
   lDesc := iif( valtype( aOrderCreateInfo [UR_ORCR_CONDINFO] ) == "A" .and. aOrderCreateInfo [UR_ORCR_CONDINFO,DBOI_ISDESC], .T., .F. )
   
   /* Montamos o comando SQL */      
   for n = 1 to len(aFields)
       cSql += '"' + lower( aFields[n] ) + '"' 
		 
		 IF lDesc
		 	 cSql += " DESC "
  		 ELSEIF nVersion > 080300
			 cSql += " ASC "
		 End

  		 IF nVersion > 080300
		 	cSql += " NULLS FIRST" + iif( n < len(aFields), ",", "" )
		 ELSE
		 	cSql += iif( n < len(aFields), ",", "" )
		 End
   next                                                                                  
   
   cSql += " )"
   
   if valtype( aOrderCreateInfo [UR_ORCR_CONDINFO] ) == "A" .and. !empty( aOrderCreateInfo [UR_ORCR_CONDINFO,DBOI_CONDITION] )
      cSql += " WHERE "
      cSql += lower( SQLTranslate( aOrderCreateInfo [UR_ORCR_CONDINFO,DBOI_EXPRESSION] ) )
   endif   
   
   PGSQL_QUERY_LOG( pSQL, cSql,, True, True )
   
   cSql := 'INSERT INTO "\?"."\?" '+;
            '( IndexStamp,IndexTable,IndexFile,IndexTag,IndexFields,IndexKey,IndexKeySizes,IndexFor, IndexUnique,IndexDescending ) ' +;
            ' VALUES ( \?, ?, ?, ?, ?, ?, ?, ?, ?, ? )'  

   aValues := {}
   AADD( aValues, aWAData[ WA_SCHEMA ])                                         // Schema
   AADD( aValues, SL_INDEX)                                                     // Table for insert
   AADD( aValues, 'NOW()' )                                                     // IndexStamp: Current date & time 
   AADD( aValues, aWAData[ WA_TABLENAME ])                                      // IndexTable: Table with current table
   AADD( aValues, cIdx)                                                         // IndexFile:  Current new index name
   AADD( aValues, cTag)                                                         // IndexTag:   Tag to emulate CDX support
   AADD( aValues, SQLArray2Text( aFields, ";") )                                // IndexFields
   AADD( aValues, SQLArray2Text( aKeys, ";") )                                  // IndexKey
   AADD( aValues, SQLArray2Text( aSizes, ";") )                                 // IndexKeySizes
   AADD( aValues, ' ' )                                                         // IndexFor
   AADD( aValues, iif( aOrderCreateInfo[UR_ORCR_UNIQUE], 'T', 'F' ) )           // IndexUnique
   AADD( aValues, iif( lDesc,'T','F'))                                          // IndexDescending
    
   cSql := SQLParams( cSQL, aValues, ID_POSTGRESQL )

   PGSQL_QUERY_LOG( pSQL, cSql,, True, True )
   PGSQL_QUERY_LOG( pSQL, "COMMIT",,, True )
   
   /*
    * Order ADD apos a criacao do novo INDEX em conformidade com a doc. do Clipper
    * disponivel em: http://www.ousob.com/ng/53guide/ng8059c.php
    * 24/05/2009 - 00:01:46
    */
   aOrderInfo := Array(UR_ORI_SIZE)
   aOrderInfo[UR_ORI_BAG] := Substr( cIdx, Len( ID_PREFIX ) +1 )
   aOrderInfo[UR_ORI_TAG] := cTag
   
   RETURN SL_ORDLSTADD_PGSQL( nWa, aWAData, aOrderInfo ) 

*****************************
function SL_ORDDESTROY_PGSQL( nWa, aWAData, aOrderInfo )
*****************************

   LOCAL cSql
   LOCAL aIndex
   LOCAL cTag
   LOCAL nOrder
   LOCAL lRet := .F.
   
   HB_SYMBOL_UNUSED( nWA )
   
   cTag := aOrderInfo[ UR_ORI_TAG ]
   
   if valType( cTag ) == "N"
      nOrder := cTag
   else
      cTag   := Lower( cTag )
      nOrder := aScan( aWAData[ WA_INDEX ], {| idx | idx[ IDX_TAG ] == cTag })
   endif
   if nOrder <1 .OR. nOrder > Len( aWAData[ WA_INDEX ] )
      return lRet
   end
   
   if nOrder = 0 .or. empty( aWAData[ WA_INDEX ] )
      cSql := "select indexfile, indextag from " + aWAData[ WA_SCHEMA ] + "." + SL_INDEX
      cSql += " where indextable = '" + aWAData[ WA_TABLENAME ] + "'"
      aIndex := SL_QuickQuery( aWAData, cSql )
      if !empty( aInDex )
         cSql := 'drop index "' + aIndex[1,1] + "_" + aIndex[1,2] + '"'
         cTag := aIndex[1,2]
      endif
   else
      cSql := 'drop index "' + aWAData[ WA_INDEX ][nOrder,IDX_BAG] + "_" + aWAData[ WA_INDEX ][nOrder,IDX_TAG] + '"'
      cTag := aWAData[ WA_INDEX ][nOrder,IDX_TAG]
   endif
   
   if !empty( cSql ) .and. SL_ExecQuery( aWAData, cSql )
      cSql := "delete from " + aWAData[ WA_SCHEMA ] + "." + SL_INDEX + " where " + ;
              '"indextable" = ' + "'" + aWAData[ WA_TABLENAME ] + "'" + ;
              ' and "indextag" = ' + "'" + lower(alltrim(cTag)) + "'"
      if SL_ExecQuery( aWAData, cSql )
         lRet := .T.
      endif
   endif
return lRet

/*
 * Open a existing index into current WA
 * 15/01/2009 - 19:55:13
 */
FUNCTION SL_ORDLSTADD_PGSQL( nWa, aWAData, aOrderInfo )
   LOCAL cFile := aOrderInfo[ UR_ORI_BAG ] 
   LOCAL cTag  := aOrderInfo[ UR_ORI_TAG ]
   LOCAL aIndexes, aInfo, cSQL, cError, i, p, f 
   
   HB_SYMBOL_UNUSED( nWA )
   
   cSQL := 'SELECT * FROM "' + aWAData[ WA_SCHEMA ] + '"."' + SL_INDEX
   cSQL += '" WHERE IndexFile = '
   cSQL += "'" + ID_PREFIX + cFile + "' "
   
   IF cTag != NIL
      cSQL += " AND IndexTag = '" + cTag + "' "
   End
   
   cSQL += ' ORDER BY "' + SL_COL_RECNO + '"' 

   SL_QuickQuery_PGSQL( aWAData, cSQL, @aIndexes )
   
   // Not found? Raise an exception?
   IF Empty( aIndexes )         
      IF cTag != NIL
         cError := "Error: index tag '"+cTag+"' in "+cFile+" does not exist!"
      ELSE      
         cError := "Error: index "+cFile+" does not exist!"
      End
      SL_Error( 1003, cError, .T., .F., cFile, 2 )    
      RETURN FAILURE
   End   
   
   /*
    * This is not the best way to save the information about the current index. But 
    * we do so and then rewrite this bit of code.
    */
   FOR i := 1 TO Len( aIndexes )
   
     * Build index info
       aInfo := Array( IDX_MAX_CONSTS )
              
       aInfo[ IDX_TABLE ]     := aIndexes[ i, 02 ]
       aInfo[ IDX_BAG ]       := aIndexes[ i, 03 ]
       aInfo[ IDX_TAG ]       := aIndexes[ i, 04 ]
       aInfo[ IDX_FIELDS ]    := hb_ATokens( aIndexes[ i, 07 ], ';' )
       aInfo[ IDX_KEYS ]      := hb_ATokens( aIndexes[ i, 09 ], ';' )
       aInfo[ IDX_KEYSIZES ]  := hb_ATokens( aIndexes[ i, 10 ], ';' )
       aInfo[ IDX_DESCEND ]   := aIndexes[ i, 06 ] == 'T'
       aInfo[ IDX_FOR ]       := aIndexes[ i, 08 ]
       aInfo[ IDX_UNIQUE ]    := aIndexes[ i, 05 ] == 'T'
       aInfo[ IDX_ROWID ]     := aIndexes[ i, 12 ]
       
     * Adjust index field pos
       aInfo[ IDX_FIELDPOS ]  := Array( Len( aInfo[ IDX_FIELDS ] ) )
       
       FOR p := 1 TO Len( aInfo[ IDX_FIELDS ] )
           f := Upper( aInfo[ IDX_FIELDS, p ] )
           
           aInfo[ IDX_FIELDPOS, p ] := aScan( aWAData[WA_REAL_STRUCT], {|_1| Upper(_1[1]) == f } )           
       End

     * Convert KEY_SIZES into numeric values
       AEval( aInfo[ IDX_KEYSIZES], {|_1,_2| aInfo[ IDX_KEYSIZES, _2 ] := Val( _1 ) })  
     * Add to current WA struct
       AADD( aWAData[ WA_INDEX ], aInfo )
   End   
   RETURN SUCCESS

******************************
function SL_WRITERECORD_PGSQL( nWa, aWAData )
******************************
   
   local cRddSep   := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )
   local lApp      := aWAData[ WA_APPEND ]
   local cVal      := "" && Rossine 07/10/08
   local s_aStruct := aWAData[ WA_STRUCT ]
   local cSql
   local nCount
   local nField
   local aBuffer
   local nRow
   
   HB_SYMBOL_UNUSED( nWA )
   if lApp
      cSql := "INSERT INTO " + SQLGetFullTableName( aWAData ) + " ( "
   else
      cSql := "UPDATE " + SQLGetFullTableName( aWAData ) + " SET "
   end
   
   nCount  := 0
   aBuffer := aWAData[ WA_BUFFER_ARR ]
   nRow    := aWAData[ WA_BUFFER_POS ]
   
   for nField = 1 to Len( s_aStruct )
   
       if aBuffer[ nRow, nField ] == NIL
          loop
       end
       nCount ++
   
       /* Get the current fieldname */
       cSql += iif( nCount > 1, ", ", "" ) + cRddSep + s_aStruct[ nField ][ DBS_NAME ] + cRddSep
   
       /* Get formated value */
       do case
       case s_aStruct[ nField ][ DBS_TYPE ] == "C" && "CHAR"
   
            /* Test correct field length to avoid some errors */
            if Len(aBuffer[ nRow, nField ]) <= s_aStruct[ nField ][ DBS_LEN ]
               if lApp  && Rossine 07/10/08
                  cVal += iif( nCount > 1, ", ", "" ) + "'" + StrTran( aBuffer[ nRow, nField ], "'", "\'" ) + "'"    // StrTran() to emulate escape  && Rossine 07/10/08
               else
                  cSql += "'" + StrTran( aBuffer[ nRow, nField ], "'", "\'" ) + "'"    // StrTran() to emulate escape
               endif
            else
               if lApp  && Rossine 07/10/08
                  cVal += iif( nCount > 1, ", ", "" ) + "'" + StrTran( LEFT( aBuffer[ nRow, nField ], s_aStruct[ nField ][ DBS_LEN ]), "'", "\'" ) + "'"  && Rossine 07/10/08
               else
                  cSql += "'" + StrTran( LEFT( aBuffer[ nRow, nField ], s_aStruct[ nField ][ DBS_LEN ]), "'", "\'" ) + "'"
               endif
            endif
   
       case s_aStruct[ nField ][ DBS_TYPE ] == "M" && "MEMO"
   
            if lApp  && Rossine 07/10/08
               cVal += iif( nCount > 1, ", ", "" ) + "'" + StrTran( aBuffer[ nRow, nField ], "'", "\'" ) + "'"    // StrTran() to emulate escape  && Rossine 07/10/08
            else
               cSql += "'" + StrTran( aBuffer[ nRow, nField ], "'", "\'" ) + "'"    // StrTran() to emulate escape
            endif
            
       case s_aStruct[ nField ][ DBS_TYPE ] == "L" && "LOGICAL"
   
            if lApp  && Rossine 07/10/08
               cVal += iif( nCount > 1, ", ", "" ) + IIF( aBuffer[ nRow, nField ], "TRUE", "FALSE" )         // http://www.postgresql.org/docs/8.1/interactive/datatype-boolean.html  && Rossine 07/10/08
            else
               cSql += iif( nCount > 1, ", ", "" ) + IIF( aBuffer[ nRow, nField ], "TRUE", "FALSE" )         // http://www.postgresql.org/docs/8.1/interactive/datatype-boolean.html
            endif
   
       case s_aStruct[ nField ][ DBS_TYPE ] == "D" && "DATE"
   
            IF Empty( aBuffer[ nRow, nField ] )
               if lApp
                  cVal += iif( nCount > 1, ", ", "" ) + "NULL"
               else
                  cSql += "NULL"
               endif
            ELSE
               if lApp  && Rossine 07/10/08
                  cVal += iif( nCount > 1, ", ", "" ) + "'" + DTOS( aBuffer[ nRow, nField ] ) + "'"  && Rossine 07/10/08
               else
                  cSql += "'" + DTOS( aBuffer[ nRow, nField ] ) + "'"
               endif
            End
   
       otherwise  // Numeric field..
            if lApp  && Rossine 07/10/08
               cVal += iif( nCount > 1, ", ", "" ) + AllTrim( Str( aBuffer[ nRow, nField ] ) )  && Rossine 07/10/08
            else
               cSql += AllTrim( Str( aBuffer[ nRow, nField ] ) )
            endif
   
       End
   next
   
   if lApp
      cSql += " ) values ( " + cVal + " )"
   else
      cSql += ' where "' + SL_PKFIELD( aWAData ) + '" = ' + AllTrim( Str( aWAData[ WA_RECNO ] ) )
   endif
   
return SL_ExecQuery( aWAData, cSql )

********************************
FUNCTION SL_QuickQuery_PGSQL( aWAData, cQuery, xResult )
********************************

local pQuery, temp, aTemp, x, y   
local oSql := aWAData[ WA_POINTER ]

**memowrit( "sql.txt", cQuery )

pQuery := PQexec( oSql:pDB, cQuery )
if PQresultstatus( pQuery ) == PGRES_TUPLES_OK
   if PQLastrec( pQuery ) != 0
       if PQFcount( pQuery ) == 1 .and. PQLastrec( pQuery ) == 1
           temp    := PQGetValue( pQuery, 1, 1 )
           xResult := iif( temp == NIL, "", temp )
       else                    
           xResult := {}
           for x := 1 to PQLastrec( pQuery )
               aTemp := {}
               for y := 1 to PQfcount( pQuery )
                   temp := PQGetValue( pQuery, x, y )
                   aadd( aTemp, iif( temp == NIL, "", temp ) ) 
               next
               aadd( xResult, aTemp)
           next                
       endif                
   endif
endif    

PQclear( pQuery ) 
           
return NIL       

/* Pega resultado de uma sequence */   
*********************
function SL_Sequence( Sequence_name )  && Rossine 07/10/08
*********************

local nWA := select(), aWAData := USRRDD_AREADATA( nWA )

return Val( SL_QuickQuery( aWAData, "SELECT nextval(" + SL_DataToSql( sequence_name ) + ")" ) )

#ifndef PQTRANS_INTRANS
   #define PQTRANS_INTRANS  2
#endif

********************************
function SL_STARTTRANS_PGSQL( aWAData )  && Rossine 07/10/08
********************************

local oSql := aWAData[ WA_POINTER ]

if PQtransactionstatus( oSql:pDB ) != PQTRANS_INTRANS
   oSql:StartTransaction()
endif        

return NIL

******************************
function SL_ENDTRANS_PGSQL( aWAData )  && Rossine 07/10/08
******************************

local oSql := aWAData[ WA_POINTER ]

SL_COMMIT()

return ( PQtransactionstatus( oSql:pDB ) == PQTRANS_INTRANS )          

********************************
function SL_CLEARINDEX_PGSQL( aWAData, lAll )
   LOCAL aIndex, cSql, n
   
   cSql := "select indextag from " + aWAData[ WA_SCHEMA ] + "." + SL_INDEX
   
   if !lAll
      cSql += " where indextable = '" + aWAData[ WA_TABLENAME ] + "'"
   endif
   
   //SL_QuickQuery_PGSQL( aWAData, cSql, @aIndex ) - BUG QDO S‡ TEM 1 INDICE
   aIndex := SL_PGSQL_SQLARRAY( aWAData[ WA_CONNECTION ], cSQL )
   
   if len(aIndex) > 0
      for n = 1 to len(aIndex)
          ordDestroy( aIndex[n,1] )
      next
   endif
   
return NIL

******************************
function SL_ROLLBACK_PGSQL( aWAData )
******************************

local oSql := aWAData[ WA_POINTER ]

return oSql:rollback()
   
*******************************
function SL_EXECQUERY_PGSQL( aWAData, cQuery, oQuery, lRet )
*******************************

local oSql := aWAData[ WA_POINTER ]

oQuery                   := oSql:Execute( cQuery )
lRet                     := oQuery:neterr()
aWAData[ WA_POINTER ]    := oSql       // TODO: Faz setindo isto aqui???  (( o_O ))

return NIL

***********************************
function SL_ExecQuery_MSG_PGSQL( oQuery, cMsg, cQuery )
***********************************

   HB_SYMBOL_UNUSED( oQuery )
   HB_SYMBOL_UNUSED( cMsg )
   HB_SYMBOL_UNUSED( cQuery )
   
*msgstop( cMsg + CRLF + cQuery + CRLF + CRLF + ;
*         "        <<< MENSAGEM DE ERRO >>>" + CRLF + oQuery:ErrorMsg(), "AtenÁ„o - Proc: " /* + PROCESSO() */ )

return NIL

***********************************
function SL_ExecQuery_DES_PGSQL( oQuery )
***********************************

oQuery:destroy()

return NIL

****************************
function SL_COMMIT_PGSQL( aWAData )
****************************

local oSql := aWAData[ WA_POINTER ]

return oSql:Commit()

***********************
function SL_SEEK_PGSQL( nWa, aWAData, lSoftSeek, cKey, lFindLast )
***********************

   local s_aStruct := aWAData[ WA_STRUCT ]
   local cRddSep   := SQLSYS_SEP( aWAData[ WA_SYSTEMID ] )
   local aIndexKey := HB_ATokens( (aWAData[ WA_ALIAS ])->(indexKey()), "+" )
   local cSql, n, t, cIdx, cKeyTmp, cVal, aRet
   local cFields := ""
   
   HB_SYMBOL_UNUSED( nWa )
   HB_SYMBOL_UNUSED( lSoftSeek )
   HB_SYMBOL_UNUSED( lFindLast )
   
   cKeyTmp := cKey
   
   if len( aIndexKey ) > 0
      for n = 1 to len( aIndexKey )
          cIdx := strtran(alltrim(aIndexKey[n]),'"',"")
          t    := ascan( s_aStruct, { |aIdx| alltrim(aIdx[DBS_NAME]) = cIdx } )
          if t > 0
             cFields += cRddSep + s_aStruct[ t ][ DBS_NAME ] + cRddSep + " = "
             cVal    := left( cKeyTmp, s_aStruct[ t ][ DBS_LEN ] )
             cKeyTmp := substr( cKeyTmp, s_aStruct[ t ][ DBS_LEN ] + 1 )
             if s_aStruct[ t ][ DBS_TYPE ] = "C"
                cFields += "'" + cVal + "'"
             else
                cFields += cVal
             endif
             if n < len( aIndexKey )
                cFields += " and "
             endif
          endif
      next
   else
      cFields += "'" + (aWAData[ WA_ALIAS ])->(indexKey()) + "' = " + cKeyTmp
   endif
   
   cSql := "select * from " + SQLGetFullTableName( aWAData ) + " where " + cFields + " and " + ;
                              cRddSep + "sl_deleted" + cRddSep + " = ' '" && order by " + iif( lFindLast, "DESC", "ASC" ) + " NULLS FIRST"
   
   aRet := SL_QuickQuery( aWAData, cSql )

return iif( len(aRet) > 0, { .T., val(aRet[1,aWAData[ WA_FLD_RECNO ]]) } , { .F., 0 } )

/*
 * Executa o SQL e retorna um ARRAY
 */

******************************
function SL_PGSQL_SQLARRAY( pConn, cSQL, aFieldNames, lAssoc )
******************************

   LOCAL aResult := {}
   LOCAL aInfo
   LOCAL Rows, Res, Flds,i,r,c,err

   DEBUG_ARGS
   
   HB_SYMBOL_UNUSED( lAssoc )
   
      IF pConn == NIL 
         aInfo := SL_GETCONNINFOBYID( ID_POSTGRESQL )
         
      ELSEIF VALTYPE( pConn ) == 'A'
         aInfo := pConn
         
      ELSEIF VALTYPE( pConn ) == 'N'
         aInfo := SL_GETCONNINFO( pConn )  
      End

*      TRACE "SL_PGSQL_SQLARRAY( ", pConn, ", ", cSQL, ", ", aFieldNames, ", ", lAssoc, " )"
      IF ( aInfo == NIL ) THEN;
         RETURN aResult

      res := pgsql_query_log( aInfo[ SL_CONN_POINTER ]:pDB, cSQL, @err, False )

      IF err 
         PQclear( Res )
         RETURN aResult
      End

      Rows := PQntuples(Res)
      Flds := PQnFields(res)

      /*
       * Terceiro campo agora d· opÁ„o para pegar os nomes
       * dos campos dum RESULTSET
       * 17-12-2005 : 09:49hs
       */
      IF (( VALTYPE(aFieldNames) == 'A' ))
         FOR i := 1 TO Flds
             AADD(aFieldNames,PQfname(res,i))
         End
      End

      /*
       * Agora temos opÁ„o de obter o resultado como um array associativo. Esta
       * contribuiÁ„o foi enviada por AntÛnio Campos, de Portugual em 28/04/2006.
       * 07-06-2006 : 17:01
       */
#ifdef __XHARBOUR__
      IF VALTYPE( lAssoc ) != 'B' THEN;
         lAssoc := False

      IF lAssoc
         FOR r := 1 TO Rows
             aRow := {=>}
             hSetCaseMatch( aRow, .T. )

             FOR c := 1 TO  Flds
                aRow[ lower(PQfname(res,i)) ] := PQGetValue( res, r,c )
             End
             AADD( aResult, aRow )
         END
      ELSE
#endif
         FOR r := 1 TO Rows
             AADD( aResult, {} )

             FOR c := 1 TO  Flds
                AADD( aResult[r], PQGetValue( res, r,c ))
             End
         END
#ifdef __XHARBOUR__
      End
#endif

      PQclear( Res )

   RETURN aResult

*******************************
function SL_GETFIELDTYPE_PGSQL( cSqlFieldType )   && Rossine 23/12/08
*******************************

   local nDBFFieldType := 0
 
   do case
      case cSqlFieldType $ [C,CHAR]  && Rossine 22/10/08
           nDBFFieldType := HB_FT_STRING
 
      case cSqlFieldType $ [L,LOGICAL]  && Rossine 22/10/08
           nDBFFieldType := HB_FT_LOGICAL
 
      case cSqlFieldType $ [N,NUMERIC]  && Rossine 22/10/08
           nDBFFieldType := HB_FT_INTEGER
 
      case cSqlFieldType == "BIGINT"
           nDBFFieldType := HB_FT_DOUBLE

      case cSqlFieldType $ [D,DATA]  && Rossine 22/10/08
           nDBFFieldType := HB_FT_INTEGER
 
   endcase
 
return nDBFFieldType

*******************************
function SL_GETFULLTABLE_PGSQL( aWAData )
*******************************

return '"' + aWAData[ WA_SCHEMA ] + '"."' + aWAData[ WA_TABLENAME ] + '"'

******************************
function SL_DELETETABLE_PGSQL( snConnHandle, cTableName, cSchema )
******************************

   local lRet  := .F.
   local oSql  := SL_GETCONNINFO( snConnHandle )[SL_CONN_POINTER]
   local nConn := oSQL:pDB
   local lError
   local cSql
   local cSequenceField 
   
   DEFAULT cSchema := "public"

   /* Aqui o RDD deve converter o nome do arquivo para um formato correto dentro do BD */
   cTableName := SQLAdjustFn( cTableName )

   cSql := "DELETE FROM " + cSchema + "." + SL_INDEX + " where " + ;
                                       '"indextable" = ' + "'" + cTableName + "'"

   PGSQL_QUERY_LOG( nConn, cSQL, @lError, .F., .T. )

   if !lError
      cSequenceField := cTableName + "_" + SL_COL_RECNO
      cSql := 'DROP SEQUENCE "' + cSchema + '"."' + cSequenceField + '" CASCADE'

      PGSQL_QUERY_LOG( nConn, cSql, @lError, .F., .T. )
      if !lError
         cSQL := 'DROP TABLE "' + cSchema + '"."' + cTableName + '"'
         PGSQL_QUERY_LOG( nConn, cSql, @lError, .F., .T. )
         if !lError
            lRet := .T.
         endif
      endif
   endif
   
return lRet

**-------------------**
** Final de Programa **
**-------------------**

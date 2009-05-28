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

STATIC snLastErrorMsg  := ""
STATIC snLastErrorCode := 00

/*
 * Clear previous error flags. This is not thread-safe
 * 12/12/2008 - 21:35:45
 */
FUNCTION SQLERR_CLEAR()
   
      snLastErrorMsg  := ''
      snLastErrorCode := 0
   
   RETURN nil 

/*
 * GET/SET Last SQLLIB RDD Error Message
 */
FUNCTION SQLERR_MSG( cNewMSG )
   LOCAL i := snLastErrorMsg

      IF  VALTYPE( cNewMSG ) == 'C' THEN;
         snLastErrorMsg := cNewMSG

   RETURN i
   
/*
 * GET/SET Last SQLLIB RDD Error Number
 */
FUNCTION SQLERR_NUM( nCode )
   LOCAL i := snLastErrorCode

      IF VALTYPE( nCode ) THEN;
         snLastErrorCode := nCode

   RETURN i

/*
 * Altera o ultimo numero de erro e msg ocorrido
 */
PROCEDURE SQLERR_SET( nErrorNum, cErrorMSG, cSQL, lWriteLog )
   LOCAL i := Len( cErrorMSG )
   
   DEFAULT lWriteLog TO .T. // .F.
   
   IF SubStr(cErrorMSG,i,1) == chr(10)
#ifdef __XHARBOUR__
      cErrorMSG[i] := " "
#else
      cErrorMSG := Stuff(cErrorMSG,i,1,"")
#endif
   End
      
   snLastErrorMsg  := cErrorMSG
   snLastErrorCode := nErrorNum

   IF lWriteLog   
      IF ValType( cSQL ) == "C"
         __WRITE_ERRLOG( ;
                        'SQLLIB/'+;
                        iif(snLastErrorCode==NIL, "", alltrim(TransForm(snLastErrorCode, "" ))) + ': '+;
                        cSQL;
                        )
      End
      __WRITE_ERRLOG( ;
                     'SQLLIB/'+;
                     iif(snLastErrorCode==NIL, "", alltrim(TransForm(snLastErrorCode, "" ))) + ': '+;
                     snLastErrorMsg;
                     )
   End
   RETURN
   
/*
 * Gera um erro em tempo de execucao
 */
FUNCTION SL_Error( ErrCode, Msg, lDefault, lRetry, cFileName, nFError, nTries )
   LOCAL oErr
   DEBUG ' >>', ErrCode, Msg, lDefault, lRetry, cFileName, nFError, nTries
   
   DEFAULT ErrCode   TO snLastErrorCode
   DEFAULT Msg       TO snLastErrorMsg
   DEFAULT nFError   TO 00
   DEFAULT lDefault  TO False
   DEFAULT lRetry    TO False
   DEFAULT cFileName TO ""
   DEFAULT nTries    TO 01

   oErr := ErrorNew()
   oErr:severity     := IF( ErrCode > 1000, ES_ERROR, ES_WARNING )
   oErr:genCode      := ErrCode
   oErr:subSystem    := "SQLLIB"
   oErr:subCode      := ErrCode

   IF VALTYPE( Msg ) == 'C'
      oErr:description  := Msg
   ELSE
      oErr:description  := snLastErrorMsg
   End

   oErr:canRetry     := lRetry
   oErr:canDefault   := lDefault
   oErr:fileName     := cFileName
   oErr:osCode       := nFERROR
   oErr:Tries        := nTries

   __WRITE_ERRLOG( ;
                  oErr:subSystem + '/'+;
                  iif(ErrCode==NIL, "", alltrim(TransForm(ErrCode, "" ))) + ': '+;
                  oErr:description;
                  )

   RETURN Eval(ErrorBlock(), oErr)

/*
 * Write the last SQL into
 * LOG.TXT when a Error Raise
 */
FUNCTION SL_LOG( cText )
   RETURN __WRITE_ERRLOG( cText )

FUNCTION __WRITE_ERRLOG( cSQL, cAbrev )
   LOCAL file

   IF VALTYPE( cSQL ) != 'C'
      RETURN .F.
   End
   
   IF file( SL_LOG_FILENAME )
      file = FOpen( SL_LOG_FILENAME, 18 )
   ELSE
      file = FCreate( SL_LOG_FILENAME )
   End

   fseek(file, 0, 2 )
   fwrite( file,  DTOC( DATE() ) + ' ' + TIME() + ' ' )

   IF cAbrev <> NIL 
      fwrite( file,  cAbrev )
   End

   fwrite( file,  cSQL )
   fwrite( file,  chr(13)+chr(10) )
   fclose( file )
   RETURN .T.

/* Generic default error msgs */
FUNCTION SQLERR_WITHOUT_PK( AWAData )
   RETURN SL_Error( SL_ERR_MISSING_PK, aWAData[ WA_ENGINE ] + ": No primary key detected for table "+SQLGetFullTableName( aWAData )+"!" )
   
/*
 * Generic error for SQL statements
 * 28/05/2009 - 10:16:43
 */
FUNCTION SQLERR_QUERY_ERROR( cError, cSQL )
   SL_LOG( cSQL )
   RETURN SL_ERROR( SL_ERR_QUERY_ERROR, cError )

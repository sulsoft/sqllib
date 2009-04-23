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

/*
 * Executa um SQL direto no banco e retorna um ARRAY()
 * TODO: Escrever estas rotinas direto em C para acelerar isto (se for possivel)
 */
FUNCTION SQLArrayAssoc( cSQL, aParms, nHandle, Array, nSystemID )
   RETURN SQLArray( cSQL, aParms, nHandle, Array, nSystemID, .T. )
   
/*
 * Executa um SQL direto no banco e retorna um ARRAY()
 * SQLArray( cSQL, aParms, nHandle, Array, nSystemID, lAssoc )
 */
FUNCTION SQLArray( cSQL, aParms, nHandle, Array, nSystemID, lAssoc )
   LOCAL Rdd, cFunc, aInfo

   DEBUG_ARGS
   
   IF VALTYPE( aParms ) == 'A' .AND. LEN( aParms ) >= 00 THEN;
      cSQL := SQLPARAMS( cSQL, aParms )

   /*
    * Se ele passou o HANDLE da conexão e passou tb o SYSID,
    * então converte o SYSID para o nome do RDD, sem percorrer o ARRAY
    */
   IF (nHandle != NIL) .AND. ( nSystemID != NIL )
      Rdd    := SQLSYS_IDSTR( nSystemID )
   ELSE
    * aInfo  := SQLGetConnectionInfo( nHandle )
      IF VALTYPE( nHandle ) == 'A'
         aInfo := nHandle
      ELSE
         aInfo := SL_GETCONNINFO( nHandle )
      End
      /*
       * TODO: Gerar erro aqui, avisar o SL_ERRORNO() disto!!
       */
      IF aInfo == NIL THEN; 
         RETURN {}

    * Puxa os dados da conexão ativa
      nHandle := aInfo[SL_CONN_HANDLE]
      Rdd     := aInfo[SL_CONN_RDD]
   End
   
   IF VALTYPE( lAssoc ) != 'L' THEN;
      lAssoc := FALSE

 * Tenta executar a SQL com o Driver
   cFunc := 'SL_' + Rdd + '_SQLARRAY'
   RETURN HB_ExecFromArray( cFunc, { nHandle, cSQL, Array, lAssoc } )

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
#ifndef _SQLLIBRDD_CH_
   #define _SQLLIBRDD_CH_

   #ifndef SQL_DEBUG
      #define SQL_VTEXT       "SQLLIB"
   #else
      #define SQL_VTEXT       "SQLLIB (DBG)"
   #endif
   
   #define SQL_VERSION_NUM    "0.2"
   #define SQL_VERSION_NUM1   0 /* Contantes usadas em DBINFO(DBI_RDD_VERSION) */ 
   #define SQL_VERSION_NUM2   2
   #define SQL_MINOR_VERSION  "a"
   
   #define SQL_BACKUP_VERSION "1.5"
   #define SQL_VERSION        "v" + SQL_VERSION_NUM + SQL_MINOR_VERSION
      
   #define _SQL_TIMER_RECCOUNT 15

   /* Moviment Scheme */
   #define MS_NONE            0
   #define MS_DOWN            1
   #define MS_UP              2
         
   /*   
    * SQL Struct & Constants for SQLLIB RDD...
    */
   #include "common.ch"
   #include "rddsys.ch"
   #include "fileio.ch"
   #include "error.ch"      
   #include "dbstruct.ch"
   #include "postgres.ch"    
   #include "dbinfo.ch"      
   #include "hbclass.ch"
   #include "SQLLIB.ch"
   
   #define  CRLF                          ( CHR(13)+CHR(10) )
   #define SL_CLEAR_RECCACHE(aWAData)     ( aWAData[ WA_RECCOUNT_EXPIRES ] := 0 )
   
   /* Comandos Personalizados */
   #Command If <Cond> then <*Cmd1*> ;
              => If <Cond> ; <Cmd1> ; End

   /* Another Contants */
   #xtranslate TRUE         => .T.
   #xtranslate FALSE        => .F.
   #xtranslate INHERITED    => Super:
   
   #xcommand DEFAULT <uVar1> := <uVal1> ;
                  [, <uVarN> := <uValN> ] => ;
                     iif( <uVar1> == nil, <uVar1> := <uVal1>, ) ;;
                   [ iif( <uVarN> == nil, <uVarN> := <uValN>, ); ]


//   #ifndef _HB_USR_RDD_CH
//      #define _HB_USR_RDD_CH
//      #include "hbusrrdd.ch"
//   #endif

   #IfnDef __XHARBOUR__
      #include "hbusrrdd.ch"
      #xcommand TRY  => BEGIN SEQUENCE WITH {|oErr| Break( oErr )}
      #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
      #xcommand FINALLY => ALWAYS
   #else
      #include "usrrdd.ch"
   #endif

   /* Comandos para DEBUG */
   #ifdef SQL_DEBUG
      #command DEBUG <*x*> => SQL_DEBUGEX( alltrim(ProcName(0))+ "("+alltrim(str(ProcLine(0)))+"):",<x> )
      #command DEBUG_ARGS  => SQL_DEBUGEX( alltrim(ProcName(0))+ "("+alltrim(str(ProcLine(0)))+") "+alltrim(str(len(hb_aparams()))) + ' arg(s)'+":", hb_aparams(), "called from " + alltrim(ProcName(1))+ "("+alltrim(str(ProcLine(1)))+") in " + ProcFile(1) + ' / ' + alltrim(ProcName(2))+ "("+alltrim(str(ProcLine(2)))+") in " + ProcFile(2) +':' )  
   #else
      #command DEBUG <*x*> =>
      #command DEBUG_ARGS  =>
   #endif      

   /* Harbour RDD's compatible DEFINE's */
   #ifndef SUCCESS
      #define SUCCESS         0
   #endif
   #ifndef FAILURE
      #define FAILURE         1
   #endif
   #define ERRDATATYPE        2
   #define ERRDATAWIDTH       3   
#endif

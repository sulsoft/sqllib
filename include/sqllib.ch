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

#IfnDef __XHARBOUR__
   #include "hbusrrdd.ch"
else
   #include "usrrdd.ch"
#endif

//#ifndef _HB_USR_RDD_CH
//   #define _HB_USR_RDD_CH
//#endif

#include "dbinfo.ch"

REQUEST SQLLIB

#ifndef _SQLLIB_CH
   #define _SQLLIB_CH
   
   #command SELECT SCHEMA <cSchemaName>         => SL_SetSchema( <(cSchemaName)> )
   #command SELECT SYSTEM SCHEMA <cSchemaName>  => SL_SetSystemSchema( <(cSchemaName)> )

   * Seta os parametros para o BD
  #command SQL CONN PARAMS TO HOST <cHost>                                ;
                              USER <cUser>                                ;
                          PASSWORD <cPwd>                                 ;
                               VIA <cDriverName>                          ;
                                                                       => ;
                   SL_CONNPARAMS( <cHost>, <cUser>, <cPwd>, <cDriverName> )

   * Cria o BD com parametros separados
   #command SQL CREATE DATABASE <cDb>                                ;
                [ HOST <cHost> ]                                     ;
                [ USER <cUser> ]                                     ;
                [ PASSWORD <cPwd> ]                                  ;
                [ <y:LIB,DRIVER,RDD,VIA> <cDriverName> ]             ;
                [ PORT <nPort> ]                                     ;
                [ SCHEMA <cSchema> ]                                 ;
                [ INTO <lRet> ]                                      ;
                                                                  => ;
       [ <lRet> := ] SL_CREATEDB( <cHost>, <nPort>, <cDb>, <cUser>,  ;
                     <cPwd>, <cDriverName>, <cSchema>, .T. )

   * Deleta um BD com parametros separados
   #command SQL DELETE DATABASE <cDb>                                ;
                [ HOST <cHost> ]                                     ;
                [ USER <cUser> ]                                     ;
                [ PASSWORD <cPwd> ]                                  ;
                [ <y:LIB,DRIVER,RDD,VIA> <cDriverName> ]             ;
                [ PORT <nPort> ]                                     ;
                [ SCHEMA <cSchema> ]                                 ;
                [ INTO <lRet> ]                                      ;
                                                                  => ;
       [ <lRet> := ] SL_CREATEDB( <cHost>, <nPort>, <cDb>, <cUser>,  ;
                     <cPwd>, <cDriverName>, <cSchema>, .F. )

   * Conecta no BD com parametros separados
   #command SQL CONNECT [ ON <cHost> ]                                      ;
                        [ PORT <nPort> ]                                    ;
                        [ DATABASE <cDb> ]                                  ;
                        [ USER <cUser> ]                                    ;
                        [ PASSWORD <cPwd> ]                                 ;
                        [ OPTIONS <nFlags> ]                                ;
                        [ SCHEMA <cSchema> ]                                ;
                        [ CHARSET <cCharSet> ]                              ;
                        [ <y:LIB,DRIVER,RDD,VIA> <cDriverName> ]            ;
                        [ INTO <nCnn> ]                                     ;
                                                                         => ;
       [ <nCnn> := ] SL_CONN( <cHost>, <nPort>, <cDb>, <cUser>,         ;
                     <cPwd>, <nFlags>, <cDriverName>, <cSchema>, <cCharSet> )

   * Importar DBF para SQL  && Rossine 23/01/09
   #command SQL IMPORT DBF <aFiles>                                         ;
                       [ VIA <cVia> ]                                       ;
                       [ <lPack:PACK> ]                                     ;
                       [ <lDelete:DELETE> ]                                 ;
                       [ INTO <lRet> ]                                      ;
                       [ EVAL <bBlock> ]                                    ;
                       [ EVERY <nEvery> ]                                   ;
                       [ APPENDEVAL <bBlockApp> ]                           ;
                                                                         => ;
         [ <lRet> := ] SL_IMPORT_FILES( <aFiles>, <cVia>, <.lPack.>, <.lDelete.>, <bBlock>, <nEvery>, <bBlockApp> )

   * Exportar SQL para DBF  && Rossine 23/01/09
   #command SQL EXPORT DBF [ <aFiles> ]                                     ;
                       [ VIA <cVia> ]                                       ;
                       [ <lPack:PACK> ]                                     ;
                       [ <lDelete:DELETE> ]                                 ;
                       [ INTO <aRet> ]                                      ;
                       [ EVAL <bBlock> ]                                    ;
                       [ EVERY <nEvery> ]                                   ;
                       [ COPYEVAL <bBlockCopy> ]                            ;
                                                                         => ;
         [ <aRet> := ] SL_EXPORT_FILES( <aFiles>, <cVia>, <.lPack.>, <.lDelete.>, <bBlock>, <nEvery>, <bBlockCopy> )

   #command SQL DELETE TABLE <cTable> => SL_DELETETABLE( cTable )  && Rossine 23/01/09

   * Conecta no DB apartir de uma string * * * S.R. LIKE STYLE * * *
   #command SQL CONNECT <cConn> [INTO <nCnn>] => [<nCnn> := ] SL_CONNPARSE( <cConn> )
   
   * Desconectar-se do DB
   #command SQL DISCONNECT [FROM] <nHandle> => SL_DISCONN( <nHandle> )
   #command SQL DISCONNECT [<lAll:ALL>]     => SL_DISCONN( <.lAll.> )
   
   #command USE <(cDb)> [VIA <cRddName>] [ALIAS <cAlias>] [<lNew: NEW>] ;
               [ <lExclusive: EXCLUSIVE>] [<lShared: SHARED>] [<lReadOnly: READONLY>] ;
               [ CODEPAGE <cCodePage>] [INDEX <(index1)> [, <(indexN)>]] ;
               [ SCHEMA <cSChema> ] ;
               [ QUERY <cQuery> ] ;
               [ INTO <nCnn> ] => ;
               [; SL_SetSChema( <cSChema> ) ] ;
               [; SL_SetQuery( <cQuery> ) ] ;
               [; SL_SetConnection( <nCnn> ) ; ] ;
               dbUseArea( <.lNew.>, <cRddName>, <(cDb)>, <(cAlias)>, ;
                          if( <.lShared.> .or. <.lExclusive.>, !<.lExclusive.>, NIL ), ;
                          <.lReadOnly.> [, <cCodePage>] ) ;
               [; dbSetIndex( <(index1)> ) ] ;
               [; dbSetIndex( <(indexN)> ) ]
   
   #xcommand CLEAR INDEXES     => SL_ClearIndex()
   #xcommand CLEAR INDEXES ALL => SL_ClearIndex( .T. )

   *
   * Especifica QTOS registros devem ser trazidos do servidor por vez
   * utilize a mesma fun‡Æo sem parametros para recuperar o valor atual... (default:50)
   *
   * Detalhe importante, use apenas ANTES de abrir o arquivo...
   *
   #command SET PACKETSIZE [TO] <x>               => SL_PacketSize( <x> )
   *
   #include "sqllibconsts.ch"   
#endif

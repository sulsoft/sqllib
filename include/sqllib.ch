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

//#IfnDef __XHARBOUR__
//   #include "hbusrrdd.ch"
//#else
//   #include "usrrdd.ch"
//#endif

//#ifndef _HB_USR_RDD_CH
//   #define _HB_USR_RDD_CH
//#endif

#include "dbinfo.ch"

REQUEST SQLLIB

#ifndef _SQLLIB_CH
   #define _SQLLIB_CH
   
   #command SELECT SCHEMA <cSchemaName>        => SL_SetSchema( <(cSchemaName)> )
   #command SELECT SYSTEM SCHEMA <cSchemaName> => SL_SetSystemSchema( <(cSchemaName)> )

  /*
    Seta os parametros para o Banco de dados
  */
  #command SQL CONN PARAMS TO HOST <cHost>                                ;
                              USER <cUser>                                ;
                          PASSWORD <cPwd>                                 ;
                               VIA <cDriverName>                          ;
                                                                       => ;
                   SL_CONNPARAMS( <cHost>, <cUser>, <cPwd>, <cDriverName> )

   /*
     Conecta a um Banco de Dados
   */
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
       [ <nCnn> := ] SL_CONN( <cHost>, <nPort>, <cDb>, <cUser>,             ;
                     <cPwd>, <nFlags>, <cDriverName>, <cSchema>, <cCharSet> )

   /*
     Cria um Banco de Dados
   */
   #command SQL CREATE DATABASE <cDb>                                      ;
                [ HOST <cHost> ]                                           ;
                [ USER <cUser> ]                                           ;
                [ PASSWORD <cPwd> ]                                        ;
                [ <y:LIB,DRIVER,RDD,VIA> <cDriverName> ]                   ;
                [ PORT <nPort> ]                                           ;
                [ SCHEMA <cSchema> ]                                       ;
                [ INTO <lRet> ]                                            ;
                                                                        => ;
       [ <lRet> := ] SL_CREATEDATABASE( <cHost>, <nPort>, <cDb>, <cUser>,  ;
                     <cPwd>, <cDriverName>, <cSchema>, .T. )

   /*
     Deleta um Banco de Dados
   */
   #command SQL DELETE DATABASE <cDb>                                      ;
                [ HOST <cHost> ]                                           ;
                [ USER <cUser> ]                                           ;
                [ PASSWORD <cPwd> ]                                        ;
                [ <y:LIB,DRIVER,RDD,VIA> <cDriverName> ]                   ;
                [ PORT <nPort> ]                                           ;
                [ SCHEMA <cSchema> ]                                       ;
                [ INTO <lRet> ]                                            ;
                                                                        => ;
       [ <lRet> := ] SL_CREATEDATABASE( <cHost>, <nPort>, <cDb>, <cUser>,  ;
                     <cPwd>, <cDriverName>, <cSchema>, .F. )

   /*
     Renomeia um Banco de Dados
   */
   #command SQL RENAME DATABASE <cDb> TO <cNewDB>                          ;
                [ CONNECTION <pConn> ]                                     ;
                [ INTO <lRet> ]                                            ;
                                                                        => ;
       [ <lRet> := ] SL_RENAMEDATABASE( <cDb>, <cNewDB>, <pConn> )

   /*
     Importar DBF para SQL  && Rossine 23/01/09
   */
   #command SQL IMPORT DBF <aFiles>                                            ;
                       [ VIA <cVia> ]                                          ;
                       [ CONNECTION <pConn> ]                                  ;
                       [ SCHEMA <cSChema> ]                                    ;
                       [ <lPack:PACK> ]                                        ;
                       [ <lDelete:DELETE> ]                                    ;
                       [ EVAL <bBlock> ]                                       ;
                       [ EVERY <nEvery> ]                                      ;
                       [ APPENDEVAL <bBlockApp> ]                              ;
                       [ INTO <lRet> ]                                         ;
                                                                            => ;
         [ <lRet> := ] SL_IMPORT_FILES( <aFiles>, <cVia>, <pConn>, <cSChema>,  ;
                       <.lPack.>, <.lDelete.>, <bBlock>, <nEvery>, <bBlockApp> )

   /*
     Exportar SQL para DBF  && Rossine 23/01/09
   */
   #command SQL EXPORT DBF [ <aFiles> ]                                         ;
                       [ VIA <cVia> ]                                           ;
                       [ CONNECTION <pConn> ]                                   ;
                       [ SCHEMA <cSChema> ]                                     ;
                       [ <lPack:PACK> ]                                         ;
                       [ <lDelete:DELETE> ]                                     ;
                       [ EVAL <bBlock> ]                                        ;
                       [ EVERY <nEvery> ]                                       ;
                       [ COPYEVAL <bBlockCopy> ]                                ;
                       [ INTO <aRet> ]                                          ;
                                                                             => ;
         [ <aRet> := ] SL_EXPORT_FILES( <aFiles>, <cVia>, <pConn>, <cSChema>,   ;
                       <.lPack.>, <.lDelete.>, <bBlock>, <nEvery>, <bBlockCopy> )

   /*
     Deleta uma Tabela
   */
   #command SQL DELETE TABLE <cTable> => SL_DELETETABLE( cTable )  && Rossine 23/01/09

   /*
     Conecta no Banco de Dados apartir de uma string * * * S.R. LIKE STYLE * * *
   */
   #command SQL CONNECT <cConn> [INTO <nCnn>] => [<nCnn> := ] SL_CONNPARSE( <cConn> )
   
   /*
     Desconecta do Banco de Dados
   */
   #command SQL DISCONNECT [FROM <nCnn>] => SL_DISCONN( <nCnn> )
   #command SQL DISCONNECT [<lAll:ALL>]  => SL_DISCONN( <.lAll.> )

   /*
     Comando para abertura de tabelas
   */
   
   #command USE <(cDb)>                                                                ;
               [ VIA <cRddName> ]                                                      ;
               [ ALIAS <cAlias> ]                                                      ;
               [ <lNew: NEW> ]                                                         ;
               [ <lExclusive: EXCLUSIVE> ]                                             ;
               [ <lShared: SHARED> ]                                                   ;
               [ <lReadOnly: READONLY> ]                                               ;
               [ CODEPAGE <cCodePage>]                                                 ; 
               [ INDEX <(index1)> [, <(indexN)> ] ]                                    ;
               [ CONNECTION <nCnn> ]                                                   ;
               [ SCHEMA <cSChema> ]                                                    ;
               [ QUERY <cQuery> ]                                                      ;
                                                                                    => ;
               [; SL_SetConnection( <nCnn> ) ; ]                                       ;
               [; SL_SetSChema( <cSChema> ) ]                                          ;
               [; SL_SetQuery( <cQuery> ) ]                                            ;
               dbUseArea( <.lNew.>, <cRddName>, <(cDb)>, <(cAlias)>,                   ;
                          if( <.lShared.> .or. <.lExclusive.>, !<.lExclusive.>, NIL ), ;
                          <.lReadOnly.> [, <cCodePage>] )                              ;
               [; dbSetIndex( <(index1)> ) ]                                           ;
               [; dbSetIndex( <(indexN)> ) ]
   
   #xcommand CLEAR INDEXES     => SL_ClearIndex()
   #xcommand CLEAR INDEXES ALL => SL_ClearIndex( .T. )

   /*
     Comandos para Transações no Banco de Dados. - By Rossine 23/07/09
   */

   #command SQL STARTTRANS                          ;
                  [ <y:SELECT,ALIAS> <xSelect> ]    ;
                  [ INTO <lRet> ]                   ;
                                                 => ;
                  [ <lRet> ] := SL_StartTrans( <xSelect> )

   #command SQL ENDTRANS                            ;
                  [ <y:SELECT,ALIAS> <xSelect> ]    ;
                  [ INTO <lRet> ]                   ;
                                                 => ;
                  [ <lRet> ] := SL_EndTrans( <xSelect> )

   #command SQL COMMIT                              ;
                  [ <y:SELECT,ALIAS> <xSelect> ]    ;
                  [ INTO <lRet> ]                   ;
                                                 => ;
                  [ <lRet> ] := SL_Commit( <xSelect> )

   #command SQL ROLLBACK                            ;
                  [ <y:SELECT,ALIAS> <xSelect> ]    ;
                  [ INTO <lRet> ]                   ;
                                                 => ;
                  [ <lRet> ] := SL_Rollback( <xSelect> )

   /*
     Especifica QTOS registros devem ser trazidos do servidor por vez
     utilize a mesma fun‡Æo sem parametros para recuperar o valor atual... (default:50)
   
     Detalhe importante, use apenas ANTES de abrir o arquivo...
   */
   
   #command SET PACKETSIZE [TO] <x> => SL_PacketSize( <x> )

   /*
    * Novos comandos que lhe auxiliarÆo no uso da fun‡Æo DBcreate()
    * Para maiores informa‡äes, consulte estes links em portugues:
    *
    *  MySQL link: http://dev.mysql.com/doc/refman/4.1/pt/create-table.html
    *  Post  link: http://pgdocptbr.sourceforge.net/pg80/sql-createtable.html
    */
   #command SQL ADD FIELD <cFieldName> <cType> ( <nSize>[, <nDec>] ) ;
                [ NEWNAME <cNewName> ]           ;
                [ DEFAULT <cDf> ]                ;
                [ <nn: NOT NULL> ]               ;
                [ <u: UNIQUE> ]                  ;
                [ PRIMARY KEY <nPk> ]            ;
                [ INTO <aStruct> ]               ;
                                              => ;
                AADD( <aStruct>, { <cFieldName>, ;    //  1º Nome do campo
                                        <cType>, ;    //  2º Tipo do campo, minimo 1¦ letra
                                        <nSize>, ;    //  3º Tamanho do campo
                       iif(<.nDec.>, <nDec>, 0), ;    //  4º Casas decimais
                                         <.nn.>, ;    //  5º .T. indica o flag NOT NULL
                                          <.u.>, ;    //  6º .T. indica o flag UNIQUE
                                          <nPk>, ;    //  7º > 0 indica o flag PRIMARY KEY
                                          <cDf>, ;    //  8º Expressão DEFAULT para o campo
                                            .F., ;    //  9º .T. indica campo no formato SQL
                                    <cNewName> } )    // 10º Novo Nome do Campo
                
   #command SQL ADD FIELD <cFieldName> TYPE <cType> ;
                [ NEWNAME <cNewName> ]           ;
                [DEFAULT <cDf> ]                 ;
                [<nn:NOT NULL> ]                 ;
                [<u:UNIQUE> ]                    ;
                [ PRIMARY KEY <nPk> ]            ;
                INTO <aStruct>                => ;
                                                 ;
                AADD( <aStruct>, { <cFieldName>, ;     //  1º Nome do campo
                                        <cType>, ;     //  2º Tipo do campo, minimo 1¦ letra
                                              0, ;     //  3º Tamanho do campo
                                              0, ;     //  4º Casas decimais
                                         <.nn.>, ;     //  5º .T. indica o flag NOT NULL
                                          <.u.>, ;     //  6º .T. indica o flag UNIQUE
                                          <nPk>, ;     //  7º > 0 indica o flag PRIMARY KEY
                                          <cDf>, ;     //  8º ExpressÆo DEFAULT para o campo
                                            .T., ;     //  9º .T. indica campo no formato SQL
                                    <cNewName> } )     // 10º Novo Nome do Campo
   
   /*
    Comando para mudar o nome de uma tabela. -  By Rossine 01/07/09
   */

   #command SQL RENAMETABLE <cOld> TO <cNew>                        ;
                [ CONNECTION <pConn> ]                              ;
                [ SCHEMA <cSchema> ]                                ;
                [ INTO <lRet> ]                                     ;
                                                                 => ;
   [ <lRet> := ] SL_RENAMETABLE( <cOld>, <cNew>, <cSchema>, <pConn> )
   
   /*
    Comando para modificação da estrutura dentro do banco de dados. - By Rossine 01/07/09
   */

   #command SQL CHANGESTRUCT <cTable> WITH <aNewStruct>                                               ;
                    [ CONNECTION <pConn> ]                                                            ;
                    [ SCHEMA <cSchema> ]                                                              ;
                    [ <u: ONLYFIELD, ONLYFIELDS> ]                                                    ;
                    [ BACKUP [<cTableBkp>] ]                                                          ;
                    [ INTO <lRet> ]                                                                   ;
                                                                                                   => ;
        [ <lRet> := ] SL_CHANGESTRUCT( <cTable>, <aNewStruct>, <pConn>, <cSchema>, <.u.>, <cTableBkp> )
   
   // CUIDADO: Se o parâmetro ONLYFIELD não for especificado, todas as outras colunas serão renomeadas para
   //          colunas de backup (excluidas)

   /*
    Comando para deletar do banco de dados as colunas de backup de um determinado Schema ou tabela
   */

   #command SQL DELETE BACKUP                                       ;
                    [ CONNECTION <pConn> ]                          ;
                    [ SCHEMA <cSchema> ]                            ;
                    [ TABLE <cTable> ]                              ;
                    [ INTO <lRet> ]                                 ;
                                                                 => ;
        [ <lRet> := ] SL_DELETEBACKUP( <pConn>, <cSchema>, <cTable> )

   /*
    Comando para criar um novo sChema
   */

   #command SQL CREATE SCHEMA <cSchema>                   ;
                [ CONNECTION <pConn> ]                    ;
                [ INTO <lRet> ]                           ;
                                                       => ;
        [ <lRet> := ] SL_CREATESCHEMA( <cSchema>, <pConn> )

   /*
    Comando para renomear um novo sChema
   */

   #command SQL RENAME SCHEMA <cSchema> TO <cNewSchema>                 ;
                              [ CONNECTION <pConn> ]                    ;
                              [ INTO <lRet> ]                           ;
                                                                     => ;
        [ <lRet> := ] SL_RENAMESCHEMA( <cSchema>, <cNewSchema>, <pConn> )

   /*
    Comando para deletar um novo sChema
   */

   #command SQL DELETE SCHEMA <cSchema>                   ;
                    [ CONNECTION <pConn> ]                ;
                    [ INTO <lRet> ]                       ;
                                                       => ;
        [ <lRet> := ] SL_DELETESCHEMA( <cSchema>, <pConn> )

   /*
    Comando para verificar se existe um schema
   */

   #command SQL EXIST SCHEMA <cSchema>                           ;
                    [ DATABASE <cDataName> ]                     ;
                    [ CONNECTION <pConn> ]                       ;
                    [ INTO <lRet> ]                              ;
                                                              => ;
        [ <lRet> := ] SL_SCHEMA( <cSchema>, <cDataName>, <pConn> )

   /*
    Comando para listar os schemas de um determinado banco de dados
   */

   #command SQL LIST SCHEMA [ <cSchema> ]                            ;
                    [ DATABASE <cDataName> ]                         ;
                    [ CONNECTION <pConn> ]                           ;
                    [ INTO <aRet> ]                                  ;
                                                                  => ;
        [ <aRet> := ] SL_LISTSCHEMA( <cSchema>, <cDataName>, <pConn> )

   #command SQL LIST DATABASE                        ;
                    [ CONNECTION <pConn> ]           ;
                    [ INTO <aRet> ]                  ;
                                                  => ;
        [ <aRet> := ] SL_LISTSCHEMA( "", "", <pConn> )

   #command SQL FULLDELETE <x:ON,OFF> => SL_FullDelete( <"x"> )

   #command SQL CHARSET TO <cCharSet>                ;
                    [ CONNECTION <pConn> ]           ;
                    [ INTO <lRet> ]                  ;
                                                  => ;
         [ <lRet> := ] SL_CharSet( <cCharSet>, <pConn> )

   #include "sqllibconsts.ch"   
#endif

//--EOF--//

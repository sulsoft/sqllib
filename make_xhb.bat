@echo off

set XHB_PATH=c:\xhb\bin

if "%XHB_PATH%" == "" goto ERRO_2

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist libpq.dll goto ERRO_1

:BUILD_OK
   %XHB_PATH%\xbuild sqllib.lib.xbp
   %XHB_PATH%\xlib libpq.dll /out:lib\libpq.lib
   goto EXIT

:ERRO_1
   @echo.
   @echo.
   @echo Atencao, copie a seguinte DLL para dentro dessa pasta: LIBPQ.DLL
   @echo Essa DLL pode ser encontrada dentro da pasta de instalacao do POSTGRESQL
   @echo EX: C:\Arquivos de Programas\PostgreSql\LIB
   @echo.
   Pause
   goto EXIT

:ERRO_2
   @echo.
   @echo.
   @echo Atencao, voce deve definir o caminho correto da variavel de ambiente:
   @echo SET XHB_PATH , abaixo um exemplo:
   @echo SET XHB_PATH = C:\xhb\bin
   @echo.
   Pause
   goto EXIT

:CLEAN
   del /s *.bak
   del /s *.tds
   del /s *.map
   del /s *.log

:EXIT

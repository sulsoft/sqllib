@echo off

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

if not exist libpq.dll goto ERRO_1

:BUILD_OK
   set XHB_PATH=c:\xhb\bin
   xbuild sqllib.lib.xbp
   xlib libpq.dll /out:lib\libpq.lib
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

:CLEAN
   del /s *.bak
   del /s *.tds
   del /s *.map
   del /s *.log

:EXIT

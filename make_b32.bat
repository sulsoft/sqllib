@echo off

set HRB_DIR=%HB_PATH%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if not exist lib md lib

if not exist libpq.dll goto ERRO_1

if exist %HRB_PATH%\lib\hbvm.lib goto BUILD_HB

:BUILD_HB
   cd source
   hbmk2 -trace -inc -info -n -hblib -osqllib *.prg *.c > make_b32.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK
   
:BUILD_XHB
   cd source
   hbmake sqllib.bc
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK
   
:BUILD_OK
   copy sqllib.lib .\..\lib\sqllib.lib
   cd..
   implib -a lib\libpq.lib libpq.dll
   goto EXIT

:BUILD_ERR
   notepad source\make_b32.log
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
   del /s source\*.ppo
   del /s *.bak
   del /s *.tds
   del /s *.map
   del /s *.log

:EXIT

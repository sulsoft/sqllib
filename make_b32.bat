@echo off

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if not exist lib md lib

if not exist libpq.dll goto ERRO_1

if exist %HRB_PATH%\lib\vm.lib goto BUILD_XHB

:BUILD_HB
   cd source
   hbmk2 -trace -inc -info -n -hblib -osqllib *.prg *.c > make.log
   cd..
   if errorlevel 1 goto BUILD_ERR

:BUILD_XHB
   cd source
   hbmake sqllib.bc
   cd..
   if errorlevel 1 goto BUILD_ERR

:BUILD_OK
   implib -a lib\libpq.lib libpq.dll
   rem copy lib\sqllib.lib %HB_PATH%\lib\sqllib.lib
   rem copy lib\libpq.lib %HB_PATH%\lib\libpq.lib
   goto EXIT

:BUILD_ERR
   notepad source\make_b32.log
   goto EXIT

:ERRO_1
   @echo.
   @echo.
   @echo Atencao, copie a seguinte DLL para dentro dessa pasta: LIBPQ.DLL
   @echo Essa DLL pode ser encontrada dentro da pasta de instalação do POSTGRESQL
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

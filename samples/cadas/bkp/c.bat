@echo off

call envhrba.bat BCC

if "%1" == "" goto ERRO_1
if exist %1.EXE del %1.EXE


hbmk2 -trace -info -n -gtgui %1.prg %1 customer.hbm customer.hbc > bld.log

if errorlevel 1 goto BUILD_ERR
goto EXIT

:BUILD_ERR
   notepad bld.log
   goto EXIT

:ERRO_1
   CLS
   @echo.
   @echo Favor Informar o nome do arquivo EX:
   @echo BLD TESTE1
   @echo.
   PAUSE
   goto EXIT

:EXIT

call envhrbd.bat

@echo off

if not exist libpq.dll goto ERRO_1
if "%HB_INC_PGSQL%" == "" goto ERRO_2

if "%HB_DIR%" == "" SET HRB_DIR=%HB_PATH%

if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN
if not exist lib md lib

if not exist %HRB_DIR%\lib\hbvm.lib goto BUILD_XHB

:BUILD_HB
   cd source
   hbmk2 -trace -inc -info -i%HB_INC_PGSQL% -n -hblib -osqllib *.prg *.c > make_b32.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK
   
:BUILD_XHB
   cd source
   hbmake sqllib.bc
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK
   
:BUILD_OK
   copy sqllib.lib .\..\lib\sqllib.lib
   del sqllib.lib
   cd..
   implib -a lib\libpq.lib libpq.dll
   goto CLEAN

:BUILD_ERR
   notepad source\make_b32.log
   cd..
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
   @echo SET HB_INC_PGSQL , abaixo um exemplo:
   @echo SET HB_INC_PGSQL = C:\Arquivos de Programas\PostgreSql\include
   @echo.
   Pause
   goto EXIT

:CLEAN
   del /s source\*.ppo
   del /s source\*.log
   del /s *.map
   del /s *.log
   goto EXIT
   
:EXIT

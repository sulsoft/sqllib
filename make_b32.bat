@echo off
if "%1" == "clean" goto CLEAN
if "%1" == "CLEAN" goto CLEAN

:INICIO
if "%HB_INC_PGSQL%" == ""  goto ERRO_2
if not exist libpq.dll 		goto ERRO_1
                         
if "%HRB_DIR%" == "" if not "%HB_INSTALL_PREFIX%" == "" 	SET HRB_DIR=%HB_INSTALL_PREFIX%
if "%HRB_DIR%" == "" if not "%HB_PATH%"  == "" 				SET HRB_DIR=%HB_PATH%

if not exist lib md lib
if not exist %HRB_DIR%\lib\hbvm.lib goto BUILD_XHB

:BUILD_HB
   cd source
   rem hbmk2    -trace -inc -info -i%HB_INC_PGSQL% -n -w -es2 -hblib -osqllib *.prg *.c > make_b32.log
       hbmk2 -b -trace -tshead=sql_tshead.ch -info -i%HB_INC_PGSQL% -n -w -hblib -osqllib *.prg *.c > make_b32.log
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK
   
:BUILD_XHB
   cd source
   hbmake sqllib.bc
   if errorlevel 1 goto BUILD_ERR
   goto BUILD_OK
   
:BUILD_OK
   copy /y sqllib.lib .\..\lib\sqllib.lib
   del sqllib.lib
   cd..
   implib -a lib\libpq.lib libpq.dll
   goto CLEAN

:BUILD_ERR

   if     "%ERRCMD%" == "" if exist make_b32.log notepad make_b32.log
   if not "%ERRCMD%" == "" if exist make_b32.log %ERRCMD% make_b32.log
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

	if exist %HB_INC_PGSQL%\..\LIB\LIBPQ.DLL goto COPIA_DLL
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
   
:COPIA_DLL

	@echo.
	@echo Encontramos a DLL necessaria localizada na seguinte pasta:
	@echo %HB_INC_PGSQL%\..\LIB\LIBPQ.DLL
	@echo deseja copia-la para esta pasta?
	choice
	if errorlevel 2 goto EXIT	
	copy %HB_INC_PGSQL%\..\LIB\LIBPQ.DLL 
   goto INICIO

:CLEAN
   del /s .\source\win\*.c
   del /s .\source\win\*.obj
   del /s source\*.ppo
   del /s source\*.log
   del /s *.map
   del /s *.tds
   del /s *.log
   del /s *.exe
   del /s *.ppo
   goto EXIT
   
:EXIT

@echo off
cls
cd ..
if exist sqlrun.lib del sqlrun.lib 
call make.bat
cd tests
if exist ..\sqlrun.lib call make.bat

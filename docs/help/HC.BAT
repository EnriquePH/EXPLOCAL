@echo off
if "%2" == "-n" goto run31
hc30.exe %1 > helpedit.err
goto end
:run31
hc31.exe %1
:end
@echo on

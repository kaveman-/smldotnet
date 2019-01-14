@echo off
if "%CMDEXTVERSION%"=="" goto :downlevel

if "%SMLNETPATH%"=="" (
  setlocal
  if exist %0 (
    call :procabs %~dp0 "%*"
  ) else if exist %0.bat (
    call :procabs %~dp0 "%*"
  ) else (
    if "%~dp$PATH:0%"=="" (
      call :procrel %0.bat "%*"
    ) else (
      call :procabs %~dp$PATH:0
    )
    goto :eof
  )
) else (
  %SMLNETPATH%\bin\run.x86-win32.exe @SMLload=%SMLNETPATH%\bin\smlnet.x86-win32 %*
)
goto :eof

:procrel
setlocal 
set D=%~dp$PATH:1
set SMLNETPATH=%D:\bin\=%
%SMLNETPATH%\bin\run.x86-win32 @SMLload=%SMLNETPATH%\bin\smlnet.x86-win32 %~2
GOTO :eof

:procabs
set D=%1
set SMLNETPATH=%D:\bin\=%
%SMLNETPATH%\bin\run.x86-win32 @SMLload=%SMLNETPATH%\bin\smlnet.x86-win32 %~2
goto :eof

:downlevel
if not "%SMLNETPATH%"=="" goto :alreadyset
set SMLNETPATH=C:\smlnet
:alreadyset
if not exist %SMLNETPATH%\bin goto :error
%SMLNETPATH%\bin\run.x86-win32.exe @SMLload=%SMLNETPATH%\bin\smlnet.x86-win32 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto :end
:error
echo Please set SMLNETPATH to the directory containing SML.NET.
:end

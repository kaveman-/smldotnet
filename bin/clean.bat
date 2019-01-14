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
  call :clean %*
)
goto :eof

:procrel
setlocal 
set D=%~dp$PATH:1
set SMLNETPATH=%D:\bin\=%
call :clean %~2
GOTO :eof

:procabs
set D=%1
set SMLNETPATH=%D:\bin\=%
call :clean %~2

goto :eof

:downlevel
echo Sorry, clean is not available on Windows 98/ME.
goto :end

:clean
if "%FrameworkVersion%"=="" (
	pushd %SMLNETPATH%\bin\%FrameworkVersion%
) else (
	pushd %SMLNETPATH%\bin
)
for /F %%i in ('dir /s/b *.enc') do call :delfile %%i
for /F %%i in ('dir /s/b *.lst') do call :delfile %%i
popd

call :deldir %SMLNETPATH%\src\basis 
for /F %%i in ('dir /ad/s/b %SMLNETPATH%\src\basis') do call :deldir %%i

call :deldir .
for /F %%i IN ('dir /ad/s/b') DO call :deldir %%i
goto :eof

:deldir
if exist %1\.smlnetobj (
  echo Deleting %1\.smlnetobj
  rmdir /Q /S %1\.smlnetobj
)
if exist %1\.smlnetdep (
  echo Deleting %1\.smlnetdep
  rmdir /Q /S %1\.smlnetdep
)
goto :eof

:delfile
if exist %1 (
  echo Deleting %1
  del /Q %1
)
goto :eof
:end




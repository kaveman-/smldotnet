@echo off

echo Build SML.NET command-line compiler
echo -----------------------------------
call bld\smlnetenv
if /I %ERRORLEVEL% NEQ 0 goto :enverror

echo Building SML.NET binary; logs in %SMLNETPATH%\bld\build.smlnet.log and %SMLNETPATH%\bld\build.smlnet.err
pushd %SMLNETPATH%
if exist bin\smlnet.x86-win32 (
  del bin\smlnet.x86-win32
)

rem !!!BEWARE!!! this doesn't work with backslashes in arguments
call %SMLNJ_HOME%\bin\ml-build src/sources.cm TopLevel.entry bin/smlnet >bld\build.smlnet.log 2>bld\build.smlnet.err
if errorlevel 1 goto :smlerror
if not exist bin\smlnet.x86-win32 goto :smlerror
popd

echo Copying SML/NJ runtime system into %SMLNETPATH%\bin
copy /Y %SMLNJ_HOME%\bin\.run\run.x86-win32.exe %SMLNETPATH%\bin

if "%ERRORLEVEL%"=="0" (
  echo Build successful
  goto :eof
) else (
  echo Build failed. See %CD%\build.log for details.
  exit /B 1
)
goto :eof

:enverror
echo Failed to set up SML.NET build environment
exit /B 1

:smlerror
echo Failed to build SML.NET compiler: %SMLNETPATH%\bld\build.smlnet.log follows.
type %SMLNETPATH%\bld\build.smlnet.log
popd
exit /B 1

@echo off
rem Set up environment for building SML.NET
rem Run this from the directory in which you installed SML.NET

if "%SMLNETPATH%"=="" (
  echo SMLNETPATH is %CD%
  set SMLNETPATH=%CD%
) else (
  echo SMLNETPATH is %CD%; was %SMLNETPATH%
  set SMLNETPATH=%CD%
)

rem Set up SML/NJ compiler path, if not already set
if "%SMLNJ_HOME%"=="" (
  set SMLNJ_HOME=%SMLNETPATH%\tools\smlnj
)
echo SMLNJ_HOME is %SMLNJ_HOME%

if exist %SMLNETPATH%\bld\clrvars.bat (
	echo Using  %SMLNETPATH%\bld\clrvars.bat to determine location of clrs
	call %SMLNETPATH%\bld\clrvars.bat
	exit /B 0
)

rem If no clrvars.bat, try to determine FrameworkDir and FrameworkVersion using getsysdir
call %SMLNETPATH%\bin\getsysdir.exe %SMLNETPATH%\bld\getsysdir.out
if /I %ERRORLEVEL% NEQ 0 (
  echo Failed to determine FrameworkDir and FrameworkVersion using %SMLNETPATH%\bin\getsysdir.exe
  exit /B 1
)


set FrameworkDir=
set FrameworkVersion=
for /F %%X in (%SMLNETPATH%\bld\getsysdir.out) do (
  if not defined FrameworkDir (
    set FrameworkDir=%%X
  ) else (
    if not defined FrameworkVersion (
     set FrameworkVersion=%%X
    )	
  )
)

exit /B 0

:error
exit /B 1



@echo off
set arg1=%1

:: setup compiler environment
if x%arg1% == xbot goto skip1
call ..\Scripts\setup_intel_compilers.bat
:skip1

Title Building FDS (Intel MPI) for 64 bit Windows

make SHELL="%ComSpec%" VPATH="../../Source" -f ..\makefile_scarc impi_intel_win_scarc_db
if x%arg1% == xbot goto skip2
pause
:skip2

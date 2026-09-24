@echo off
setlocal

rem Uso: makefile build | run | clean | solve | test
rem   build  monta a ROM (wandquest.nes) e o arquivo de labels (wandquest.lbl)
rem   run    abre a ROM no FCEUX
rem   clean  apaga os arquivos gerados pelo build
rem   solve  valida as fases e gera docs\SOLUCOES.md e tests\solutions.lua
rem   test   solve + build + joga as 20 fases no FCEUX (resultado em tests\result.txt)

if "%1" == "build" (
    call :build
    exit /b %errorlevel%
) else if "%1" == "clean" (
    echo Cleaning files...
    del /Q *.o *.nes *.lbl
    if errorlevel 1 (
        echo Error on cleaning the files.
        exit /b 1
    ) else (
        echo Files clean.
    )
) else if "%1" == "run" (
    echo Execution emulate...
    qfceux wandquest.nes
    if errorlevel 1 (
        echo Error on execute the emulator.
        exit /b 1
    )
) else if "%1" == "solve" (
    call :solve
    exit /b %errorlevel%
) else if "%1" == "test" (
    call :solve || exit /b 1
    call :build || exit /b 1
    echo Running the level test in FCEUX...
    del /Q tests\result.txt 2>nul
    qfceux --loadlua "%~dp0tests\levels_test.lua" "%~dp0wandquest.nes"
    type tests\result.txt
    findstr /C:"ALL PASSED" tests\result.txt >nul || exit /b 1
) else (
    echo Unknown command. Try to use "build", "clean", "run", "solve" or "test".
)
exit /b 0

:build
echo Building the project...
ca65 -g wandquest.asm -o wandquest.o
if errorlevel 1 (
    echo Error on assembling the project.
    exit /b 1
)
ld65 -C nes.cfg wandquest.o -o wandquest.nes -Ln wandquest.lbl
if errorlevel 1 (
    echo Error on linking the project.
    exit /b 1
)
echo Build done.
exit /b 0

:solve
echo Solving the levels...
set PYTHONIOENCODING=utf-8
python tools\solver.py
if errorlevel 1 (
    echo Some level is invalid or has no solution.
    exit /b 1
)
exit /b 0

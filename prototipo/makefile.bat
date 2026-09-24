@echo off
setlocal
cd /d "%~dp0"

rem Uso: makefile build | run | clean | art
rem   build  monta a ROM (agente.nes) e o arquivo de labels (agente.lbl)
rem   run    abre a ROM no FCEUX
rem   clean  apaga os arquivos gerados pelo build
rem   art    regenera o cenário, a fase e os sprites (tools\*.py) e monta a ROM

if "%1" == "build" (
    call :build
    exit /b %errorlevel%
) else if "%1" == "run" (
    qfceux agente.nes
) else if "%1" == "clean" (
    del /Q *.o *.nes *.lbl 2>nul
    echo Files clean.
) else if "%1" == "art" (
    set PYTHONIOENCODING=utf-8
    python tools\draw_tiles.py || exit /b 1
    python tools\make_level.py || exit /b 1
    python tools\draw_sprites.py || exit /b 1
    call :build
    exit /b %errorlevel%
) else (
    echo Unknown command. Try "build", "run", "clean" or "art".
)
exit /b 0

:build
echo Building the prototype...
ca65 -g main.asm -o agente.o
if errorlevel 1 (
    echo Error on assembling.
    exit /b 1
)
ld65 -C nes.cfg agente.o -o agente.nes -Ln agente.lbl
if errorlevel 1 (
    echo Error on linking.
    exit /b 1
)
echo Build done.
exit /b 0

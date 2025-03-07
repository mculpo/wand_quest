@echo off
setlocal

if "%1" == "build" (
    echo Building the project...
    ca65 wandquest.asm -o wandquest.o
    ld65 -C nes.cfg wandquest.o -o wandquest.nes
    if errorlevel 1 (
        echo Error on building or linking the project.
        exit /b 1
    ) else (
        echo Build done.
    )
) else if "%1" == "clean" (
    echo Cleaning files...
    del /Q *.o *.nes
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
) else (
    echo Unknown command. Try to use "build", "clean" or "run".
)

endlocal
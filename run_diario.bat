@echo off
setlocal enabledelayedexpansion
REM ============================================================
REM Wrapper para el Programador de tareas de Windows / doble clic manual.
REM Llama a run_diario.R, que corre clima.R y luego
REM pipeline_ndvi_powerbi.R (ver ese archivo para el detalle).
REM
REM Si conoces la ruta exacta de tu Rscript.exe, es mas confiable que la
REM auto-deteccion de abajo: quita el REM de la siguiente linea y pon tu
REM ruta completa (para saberla: abre R y corre R.home("bin")):
REM set "RSCRIPT_CMD=C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
REM
REM Este .bat SIEMPRE deja un log en logs\bat_ultima_corrida.log, incluso
REM si Rscript no se encuentra o run_diario.R no esta donde se espera
REM (casos que run_diario.R por si solo no puede registrar, porque nunca
REM llega a arrancar).
REM ============================================================

set "REPO_DIR=%~dp0"

if not exist "%REPO_DIR%logs" mkdir "%REPO_DIR%logs"
set "BAT_LOG=%REPO_DIR%logs\bat_ultima_corrida.log"

echo ============================================== > "%BAT_LOG%"
echo Corrida iniciada: %DATE% %TIME% >> "%BAT_LOG%"
echo Carpeta detectada (REPO_DIR): %REPO_DIR% >> "%BAT_LOG%"

REM --- Localizar Rscript: 1) el que hayas fijado arriba a mano,
REM     2) el PATH del sistema, 3) la instalacion tipica en Program Files ---
if not defined RSCRIPT_CMD (
  where Rscript >nul 2>&1
  if !ERRORLEVEL! EQU 0 (
    set "RSCRIPT_CMD=Rscript"
    echo Rscript encontrado en el PATH del sistema >> "%BAT_LOG%"
  ) else (
    echo Rscript NO esta en el PATH. Buscando instalacion en "C:\Program Files\R\"... >> "%BAT_LOG%"
    for /f "delims=" %%D in ('dir /b /ad /o-n "C:\Program Files\R\R-*" 2^>nul') do (
      if not defined RSCRIPT_CMD if exist "C:\Program Files\R\%%D\bin\Rscript.exe" (
        set "RSCRIPT_CMD=C:\Program Files\R\%%D\bin\Rscript.exe"
      )
    )
  )
)

if not defined RSCRIPT_CMD (
  echo ERROR: no se encontro Rscript.exe ni en el PATH ni en C:\Program Files\R\ >> "%BAT_LOG%"
  echo ERROR: no se encontro Rscript.exe.
  echo Abre run_diario.bat en un editor de texto y fija la ruta a mano
  echo en la linea "set RSCRIPT_CMD=..." cerca del inicio del archivo.
  echo ^(para saber tu ruta: abre R y corre R.home^("bin"^)^)
  timeout /t 30 >nul 2>&1
  exit /b 9009
)

echo Usando Rscript: !RSCRIPT_CMD! >> "%BAT_LOG%"

if not exist "%REPO_DIR%run_diario.R" (
  echo ERROR: no se encontro "%REPO_DIR%run_diario.R" >> "%BAT_LOG%"
  echo ERROR: no se encontro "%REPO_DIR%run_diario.R"
  echo Revisa que run_diario.bat este en la MISMA carpeta que run_diario.R,
  echo la carpeta R\ y la carpeta data\ ^(la raiz del repo^).
  timeout /t 30 >nul 2>&1
  exit /b 1
)

"!RSCRIPT_CMD!" "%REPO_DIR%run_diario.R" >> "%BAT_LOG%" 2>&1
set "RC=%ERRORLEVEL%"

echo Codigo de salida: %RC% >> "%BAT_LOG%"
echo Corrida terminada: %DATE% %TIME% >> "%BAT_LOG%"

if not "%RC%"=="0" (
  echo.
  echo Algo fallo (codigo %RC%^). Revisa, en la carpeta del repo:
  echo   - logs\bat_ultima_corrida.log   ^(errores antes de arrancar R^)
  echo   - logs\ultimo_estado.txt        ^(resultado de clima.R / pipeline^)
  echo   - logs\run_^<timestamp^>.log      ^(log completo de la corrida^)
  echo Esta ventana se cierra sola en 30 segundos...
  timeout /t 30 >nul 2>&1
)

exit /b %RC%

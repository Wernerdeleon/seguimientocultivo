@echo off
setlocal
REM ============================================================
REM Wrapper para el Programador de tareas de Windows / doble clic manual.
REM Llama a run_diario.R, que corre clima.R y luego
REM pipeline_ndvi_powerbi.R (ver ese archivo para el detalle).
REM
REM Si "Rscript" no esta en el PATH del sistema, cambia la linea
REM "set RSCRIPT_CMD=Rscript" de abajo por la ruta completa a tu
REM instalacion de R, por ejemplo:
REM   set RSCRIPT_CMD="C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
REM (para saber cual es la tuya: abre R y corre R.home("bin"))
REM
REM Este .bat SIEMPRE deja un log en logs\bat_ultima_corrida.log, incluso
REM si Rscript no se encuentra o run_diario.R no esta donde se espera
REM (casos que run_diario.R por si solo no puede registrar, porque nunca
REM llega a arrancar).
REM ============================================================

set "REPO_DIR=%~dp0"
set "RSCRIPT_CMD=Rscript"

if not exist "%REPO_DIR%logs" mkdir "%REPO_DIR%logs"
set "BAT_LOG=%REPO_DIR%logs\bat_ultima_corrida.log"

echo ============================================== > "%BAT_LOG%"
echo Corrida iniciada: %DATE% %TIME% >> "%BAT_LOG%"
echo Carpeta detectada (REPO_DIR): %REPO_DIR% >> "%BAT_LOG%"
echo Buscando Rscript en el PATH... >> "%BAT_LOG%"
where %RSCRIPT_CMD% >> "%BAT_LOG%" 2>&1

if not exist "%REPO_DIR%run_diario.R" (
  echo ERROR: no se encontro "%REPO_DIR%run_diario.R" >> "%BAT_LOG%"
  echo ERROR: no se encontro "%REPO_DIR%run_diario.R"
  echo Revisa que run_diario.bat este en la MISMA carpeta que run_diario.R,
  echo la carpeta R\ y la carpeta data\ ^(la raiz del repo^).
  timeout /t 30 >nul 2>&1
  exit /b 1
)

%RSCRIPT_CMD% "%REPO_DIR%run_diario.R" >> "%BAT_LOG%" 2>&1
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

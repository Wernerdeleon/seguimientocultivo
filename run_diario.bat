@echo off
REM ============================================================
REM Wrapper para el Programador de tareas de Windows.
REM Llama a run_diario.R, que corre clima.R y luego
REM pipeline_ndvi_powerbi.R (ver ese archivo para el detalle).
REM
REM Si "Rscript" no esta en el PATH del sistema, reemplaza la linea
REM de abajo por la ruta completa a tu instalacion de R, por ejemplo:
REM   "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
REM (para saber cual es la tuya: abre R y corre R.home("bin"))
REM ============================================================

set "REPO_DIR=%~dp0"

Rscript "%REPO_DIR%run_diario.R"

exit /b %ERRORLEVEL%

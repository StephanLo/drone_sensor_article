ECHO off
call conda activate mapir
IF NOT EXIST "C:\Users\Ram\tools\MAPIR_imageprocessing\camera-scripts-main\full_process\calibration\calib.tif" (
    ECHO First generate a calibration image calibration\calib.tif The program will now exit
    PAUSE
    EXIT
    ) ELSE (

REM Starting raw to tif conversion.
python "Convert_Survey3_RAW_to_Tiff.py" "C:\Users\Ram\tools\MAPIR_imageprocessing\camera-scripts-main\full_process\inFolder" "C:\Users\Ram\tools\MAPIR_imageprocessing\camera-scripts-main\full_process\temp\convert_out"

REM Start Vignette Correction
python "Vignette_Correction.py" "C:\Users\Ram\tools\MAPIR_imageprocessing\camera-scripts-main\full_process\temp\convert_out\Processed_1" "C:\Users\Ram\tools\MAPIR_imageprocessing\camera-scripts-main\full_process\temp\vignette_out" "C:\Users\Ram\tools\MAPIR_imageprocessing\camera-scripts-main\full_process\correctionFolder"

REM Start Calibration to reflectance
python "calibration.py" "C:\Users\Ram\tools\MAPIR_imageprocessing\camera-scripts-main\full_process\calibration\calib.tif" "C:\Users\Ram\tools\MAPIR_imageprocessing\camera-scripts-main\full_process\temp\vignette_out\Processed_1" "C:\Users\Ram\tools\MAPIR_imageprocessing\camera-scripts-main\full_process\outFolder"
)

pause >nul
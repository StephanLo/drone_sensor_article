@echo on
call conda activate mapir
python "Convert_Survey3_RAW_to_Tiff.py" "C:\Users\Ram\Documents\lab-projects\mapir_raw" "C:\Users\Ram\Documents\lab-projects\mapir_processed"
pause >nul
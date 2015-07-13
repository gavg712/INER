@echo off
C:
rem Path to executables of R
PATH c:\Program Files\R\R-3.2.0\bin\;%path%
rem Path to tmy generator script
cd C:\Users\Analista INER\Desktop\tmy_gen
rem 'code.r' name of script
Rscript code.r 1

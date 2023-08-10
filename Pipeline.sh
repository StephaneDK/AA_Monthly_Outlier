#!/bin/sh

#Outliers Analysis path
path_local_outlier='C:\Users\steph\Documents\DK\Work\Forecasting book sales and inventory\Auxilary tasks\Amazon Advertisment\Code\'
path_DK_outlier='C:\Users\snichanian\Documents\DK\Work\Forecasting book sales and inventory\Auxilary tasks\Amazon Advertisment\Code\'

#ROAS Analysis path
path_local_ROAS='C:\Users\steph\Documents\DK\Work\Forecasting book sales and inventory\Auxilary tasks\Amazon Advertisment\ROAS Analysis\Code\'
path_DK_ROAS='C:\Users\snichanian\Documents\DK\Work\Forecasting book sales and inventory\Auxilary tasks\Amazon Advertisment\ROAS Analysis\Code\'

#Change this line for directory
path_code_outlier=$path_local_outlier
path_code_ROAS=$path_local_ROAS

cd "$path_code_outlier"

export path_code_outlier
export path_code_ROAS

python "fetch_data_snfk.py"

Rscript "Outliers Analysis US.R" 
Rscript "Outliers Analysis UK.R" 





cd "$path_code_ROAS"

Rscript "Roas Analysis - UK.R" 
Rscript "Roas Analysis - US.R" 





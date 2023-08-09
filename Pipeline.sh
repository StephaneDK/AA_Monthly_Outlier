#!/bin/sh
python "C:\Users\steph\Documents\DK\Work\Forecasting book sales and inventory\Auxilary tasks\Amazon Advertisment\Code\fetch_data_snfk.py"

Rscript "C:\Users\steph\Documents\DK\Work\Forecasting book sales and inventory\Auxilary tasks\Amazon Advertisment\Code\Outliers Analysis UK.R" 
Rscript "C:\Users\steph\Documents\DK\Work\Forecasting book sales and inventory\Auxilary tasks\Amazon Advertisment\Code\Outliers Analysis US.R" 

Rscript "C:\Users\steph\Documents\DK\Work\Forecasting book sales and inventory\Auxilary tasks\Amazon Advertisment\ROAS Analysis\Code\Roas Analysis - UK.R" 
Rscript "C:\Users\steph\Documents\DK\Work\Forecasting book sales and inventory\Auxilary tasks\Amazon Advertisment\ROAS Analysis\Code\Roas Analysis - US.R" 





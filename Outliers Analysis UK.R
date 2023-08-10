cat("UK Outlier Analysis Start\n")
renv::activate()

#renv::restore(packages = "renv")

#.libPaths()

#Declaring variables for directories change
path_var <- Sys.getenv("path_code_outlier")
path_var_csv <- gsub("Code","csv",path_var)
path_var_output <- gsub("Code","Output",path_var)

#Calling outside scripts
source(paste0(path_var,"OutsideBorders.R"))
source(paste0(path_var,"ThresholdingAlgo.R"))

#source("C:\\Users\\snichanian\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Pipeline\\Code\\OutsideBorders.R")
#source("C:\\Users\\snichanian\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\code\\ThresholdingAlgo.R")

oldw <- getOption("warn")
options(warn = -1)

suppressMessages({
  
  library(tidyverse)
  library(stringr)
  library(reshape2)
  library(openxlsx)
  library(magrittr)
  library(rstatix)
  
  
})


options(scipen=999, digits = 3 )

`%notin%` <- Negate(`%in%`)

all_days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")


#Setting the directory where all files will be used from for this project
setwd(path_var_csv)
#setwd("C:\\Users\\snichanian\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\csv")

#Importing and clean Sales Data
Sales_UK <- read.csv("UK_AA_Data.csv", header = T, stringsAsFactors = FALSE)
Sales_UK$AA.Reason.Code <- gsub("Core title", "Core Title", Sales_UK$AA.Reason.Code)

setwd(path_var_output)
#setwd("C:\\Users\\snichanian\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\Output")


#-----------------------------------------------------------------------------------------------------------------------
#                                 Outliers Detection
#-----------------------------------------------------------------------------------------------------------------------

#Outlier Boxplots
Sales_UKA_outliers <- 
            Sales_UK  %>%
            subset(AA.Status %in% c("A") )  %>%
            group_by(AA.Reason.Code ) %>%
            rstatix::identify_outliers(AA_SPEND) %>%
            select(ISBN,is.outlier)

Sales_UKA <- Sales_UK  %>%
             subset(AA.Status %in% c("A") ) %>%
             merge(Sales_UKA_outliers, by = "ISBN", all.x=T)  

Sales_UKA <- Sales_UKA %>% 
  mutate(outlier = ifelse(is.outlier=="TRUE" , Title,NA))

ggplot(Sales_UKA, aes(x=AA.Reason.Code, y=AA_SPEND)) + 
  geom_boxplot()  +
  ggrepel::geom_text_repel(aes(label=outlier)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),  
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("UK AA Spend Boxplots Status A") +
  ylab("AA Spend (£)") 

ggsave("UK AA Spend Boxplots Status A.png",width = 11.4, height = 8.44, dpi = 300)








#Outlier Boxplots
Sales_UKB_outliers <- 
  Sales_UK  %>%
  subset(AA.Status %in% c("B") )  %>%
  group_by(AA.Reason.Code ) %>%
  rstatix::identify_outliers(AA_SPEND) %>%
  select(ISBN,is.outlier)

Sales_UKB <- Sales_UK  %>%
  subset(AA.Status %in% c("B") ) %>%
  merge(Sales_UKB_outliers, by = "ISBN", all.x=T)  

Sales_UKB <- Sales_UKB %>% 
  mutate(outlier = ifelse(is.outlier=="TRUE" , Title,NA))

ggplot(Sales_UKB, aes(x=AA.Reason.Code, y=AA_SPEND)) + 
  geom_boxplot()  +
  ggrepel::geom_text_repel(aes(label=outlier)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),  
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("UK AA Spend Boxplots Status B") +
  ylab("AA Spend (£)") 

ggsave("UK AA Spend Boxplots Status B.png",width = 11.4, height = 8.44, dpi = 300)








#Outlier Boxplots
Sales_UKC_outliers <- 
  Sales_UK  %>%
  subset(AA.Status %in% c("C") )  %>%
  group_by(AA.Reason.Code ) %>%
  rstatix::identify_outliers(AA_SPEND) %>%
  select(ISBN,is.outlier)

Sales_UKC <- Sales_UK  %>%
  subset(AA.Status %in% c("C") ) %>%
  merge(Sales_UKC_outliers, by = "ISBN", all.x=T)  

Sales_UKC <- Sales_UKC %>% 
  mutate(outlier = ifelse(is.outlier=="TRUE" , Title,NA))

ggplot(Sales_UKC, aes(x=AA.Reason.Code, y=AA_SPEND)) + 
  geom_boxplot()  +
  ggrepel::geom_text_repel(aes(label=outlier)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),  
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("UK AA Spend Boxplots Status C") +
  ylab("AA Spend (£)") 

ggsave("UK AA Spend Boxplots Status C.png",width = 11.4, height = 8.44, dpi = 300)




# define a function to find lower level outliers
FindOutliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  # we identify extreme outliers
  mid.threshold.upper = (iqr * 1) + upperq
  mid.threshold.lower = lowerq - (iqr * 1)
  result <- which(data > mid.threshold.upper | data < mid.threshold.lower)
}


#Identifying lower level outlier
Sales_UKA_outliers_low <- 
  Sales_UK  %>%
  subset(AA.Status %in% c("A") ) %>%
  mutate(is.outlier = "FALSE") %>%
  slice(FindOutliers(AA_SPEND)) %>%
  mutate(is.outlier = "LOW")

Sales_UKB_outliers_low <- 
  Sales_UK  %>%
  subset(AA.Status %in% c("B") ) %>%
  mutate(is.outlier = "FALSE") %>%
  slice(FindOutliers(AA_SPEND)) %>%
  mutate(is.outlier = "LOW")

Sales_UKC_outliers_low <- 
  Sales_UK  %>%
  subset(AA.Status %in% c("C") ) %>%
  mutate(is.outlier = "FALSE") %>%
  slice(FindOutliers(AA_SPEND)) %>%
  mutate(is.outlier = "LOW")


#Outlier csv
Sales_UKA_outliers <- 
  Sales_UK  %>%
  subset(AA.Status %in% c("A") ) %>%
  group_by(AA.Reason.Code) %>%
  rstatix::identify_outliers(AA_SPEND) %>%
  mutate(is.outlier = "HIGH") %>%
  relocate(AA.Reason.Code, .after = AA.Status) %>%
  select(1:15) %>%
  rbind(Sales_UKA_outliers_low) %>%  
  group_by(ISBN) %>% 
  filter(!(is.outlier=="LOW" & n() > 1)) %>%
  as.data.frame() %>% 
  mutate( CPC = round(CPC,3),
          CVR = round(CVR,3) ) %>%   #mutate(across(13:14, round, 3)) %>%
  arrange(AA.Reason.Code, is.outlier)

Sales_UKB_outliers <- 
  Sales_UK  %>%
  subset(AA.Status %in% c("B") )  %>%
  group_by(AA.Reason.Code ) %>%
  rstatix::identify_outliers(AA_SPEND) %>%
  mutate(is.outlier = "HIGH") %>%
  relocate(AA.Reason.Code, .after = AA.Status) %>%
  select(1:15) %>%
  rbind(Sales_UKB_outliers_low) %>%  
  group_by(ISBN) %>% 
  filter(!(is.outlier=="LOW" & n() > 1)) %>% 
  as.data.frame() %>% 
  mutate( CPC = round(CPC,3),
          CVR = round(CVR,3) ) %>%
  arrange(AA.Reason.Code, is.outlier)

Sales_UKC_outliers <- 
  Sales_UK  %>%
  subset(AA.Status %in% c("C") )  %>%
  group_by(AA.Reason.Code ) %>%
  rstatix::identify_outliers(AA_SPEND) %>%
  mutate(is.outlier = "HIGH") %>%
  relocate(AA.Reason.Code, .after = AA.Status) %>%
  select(1:15) %>%
  rbind(Sales_UKC_outliers_low) %>%  
  group_by(ISBN) %>% 
  filter(!(is.outlier=="LOW" & n() > 1)) %>% 
  as.data.frame() %>% 
  mutate( CPC = round(CPC,3),
          CVR = round(CVR,3) ) %>% 
  arrange(AA.Reason.Code, is.outlier)

Sales_Outliers <- rbind(Sales_UKA_outliers, Sales_UKB_outliers )
Sales_Outliers <- rbind(Sales_Outliers, Sales_UKC_outliers )
Sales_Outliers <- arrange(Sales_Outliers, AA.Status, AA.Reason.Code,is.outlier, desc(AA_SPEND))
Sales_Outliers$ISBN <- as.character(Sales_Outliers$ISBN )

#Highlighting columns with inventory issues
wb <- createWorkbook()
addWorksheet(wb, sheetName="UK")
writeData(wb, sheet="UK", x=Sales_Outliers)


#auto width for columns
width_vec <- suppressWarnings(apply(Sales_Outliers, 2, function(x) max(nchar(as.character(x)) + 1, na.rm = TRUE)))
width_vec_header <- nchar(colnames(Sales_Outliers))  + 3
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, "UK", cols = 1:ncol(Sales_Outliers), widths = max_vec_header+2 )


#adding filters
addFilter(wb, "UK", rows = 1, cols = 1:ncol(Sales_Outliers))

setColWidths(wb, "UK",  cols = 2, widths = 40)

#Centering cells
centerStyle <- createStyle(halign = "center")
addStyle(wb, "UK", style=centerStyle, rows = 1:nrow(Sales_Outliers)+1, cols = 5:ncol(Sales_Outliers), 
         gridExpand = T, stack = TRUE)

# Adding borders
invisible(OutsideBorders(
  wb,
  sheet_ = "UK",
  rows_ = 1:nrow(Sales_Outliers)+1,
  cols_ = 1:6
))

invisible(OutsideBorders(
  wb,
  sheet_ = "UK",
  rows_ = 1:nrow(Sales_Outliers)+1,
  cols_ = 7:14
))

invisible(OutsideBorders(
  wb,
  sheet_ = "UK",
  rows_ = 1:nrow(Sales_Outliers)+1,
  cols_ = 15
))

pred_date <- Sys.Date() + 6 - match(weekdays(Sys.Date()), all_days)

saveWorkbook(wb, paste0("UK Outliers - ",pred_date,".xlsx"), overwrite = T) 


#-----------------------------------------------------------------------------------------------------------------------
#                                 Spend Change Over Time
#-----------------------------------------------------------------------------------------------------------------------
# 
# 
# setwd("C:\\Users\\steph\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\csv")
# 
# 
# Sales_UK <- read.csv("UK_AA_Data_COT.csv", header = T, stringsAsFactors = FALSE)
# Sales_UK <- 
#   Sales_UK %>%
#   plyr::rename(c("ONSALE_DATE" = "Pub_Date")) %>%
#   mutate(AA.Reason.Code = gsub("Core title", "Core Title",AA.Reason.Code)) %>%
#   select(1:9) %>%
#   dcast(Country + Title + Asin + ISBN + Pub_Date + AA.Status + AA.Reason.Code ~ Month.Date, 
#         value.var="AA_SPEND", fun.aggregate = NULL)
# 
# setwd("C:\\Users\\steph\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\Output")
# 
# #Function to change AA Spend NA to 0 after book is published
# for (i in 1:nrow(Sales_UK)){
#   
#   for (j in 8:ncol(Sales_UK)){
#       
#     if ( as.Date(Sales_UK$Pub_Date[i]) <= as.Date(paste0(colnames(Sales_UK)[j],"-01")) ) {
#       
#       if (is.na(Sales_UK[i,j])){
#         
#         Sales_UK[i,j] <- 0
#         
#       }
#       
#     }    
#     
#   }
#   
# }  
# 
# 
# 
# 
# #Slope Calculation (trend)
# Sales_UK$Slope <- NA
# Sales_UK$SD <- NA
# for (i in 1:nrow(Sales_UK)){
#   
#   if (!all(is.na(as.numeric(Sales_UK[i,c(8:28)])))){
#     Sales_UK$Slope[i] <- as.numeric(lm(as.numeric(Sales_UK[i,c(8:28)]) ~ c(1:21), na.action=na.exclude)$coefficients[2])
#     Sales_UK$SD[i] <- sd(Sales_UK[i,c(8:28)], na.rm=T)
#     
#   }
# }
# 
# Sales_UK$Trend <- NA
# Sales_UK <- arrange(Sales_UK, Sales_UK$Slope)
# 
# 
# #Highlighting columns with inventory issues
# wb <- createWorkbook()
# addWorksheet(wb, sheetName="UK")
# writeData(wb, sheet="UK", x=Sales_Outliers)
# 
# 
# #auto width for columns
# width_vec <- suppressWarnings(apply(Sales_Outliers, 2, function(x) max(nchar(as.character(x)) + 1, na.rm = TRUE)))
# width_vec_header <- nchar(colnames(Sales_Outliers))  + 3
# max_vec_header <- pmax(width_vec, width_vec_header)
# setColWidths(wb, "UK", cols = 1:ncol(Sales_Outliers), widths = max_vec_header+2 )
# 
# 
# #adding filters
# addFilter(wb, "UK", rows = 1, cols = 1:ncol(Sales_Outliers))
# 
# setColWidths(wb, "UK",  cols = 2, widths = 40)
# 
# #Centering cells
# centerStyle <- createStyle(halign = "center")
# addStyle(wb, "UK", style=centerStyle, rows = 1:nrow(Sales_Outliers)+1, cols = 8:ncol(Sales_Outliers), 
#          gridExpand = T, stack = TRUE)
# 
# # Adding borders
# invisible(OutsideBorders(
#   wb,
#   sheet_ = "UK",
#   rows_ = 1:nrow(Sales_Outliers)+1,
#   cols_ = 1:7
# ))
# 
# invisible(OutsideBorders(
#   wb,
#   sheet_ = "UK",
#   rows_ = 1:nrow(Sales_Outliers)+1,
#   cols_ = 8:29
# ))
# 
# invisible(OutsideBorders(
#   wb,
#   sheet_ = "UK",
#   rows_ = 1:nrow(Sales_Outliers)+1,
#   cols_ = 30:31
# ))
# 
# invisible(OutsideBorders(
#   wb,
#   sheet_ = "UK",
#   rows_ = 1:nrow(Sales_Outliers)+1,
#   cols_ = 32
# ))
# 
# freezePane(
#   wb,
#   sheet = "UK",
#   firstActiveRow = 3,
#   firstActiveCol = 3
# )
# 
# 
# 
# pred_date <- Sys.Date() + 6 - match(weekdays(Sys.Date()), all_days)
# 
# #This needs to be produced once a month
# #saveWorkbook(wb, paste0("AA Spend Analysis COT - ",pred_date,".xlsx"), overwrite = T) 
# 
# 

renv::deactivate()
cat("UK Outlier Analysis End\n\n")


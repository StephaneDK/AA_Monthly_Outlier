library.path <- .libPaths("C:/Users/steph/Documents/R/win-library/4.0")
source("C:\\Users\\steph\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Pipeline\\Code\\OutsideBorders.R")
source("C:\\Users\\steph\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\code\\ThresholdingAlgo.R")

oldw <- getOption("warn")
options(warn = -1)

suppressMessages({
  
  library(tidyverse, lib.loc = library.path)
  library(stringr, lib.loc = library.path)
  library(reshape2, lib.loc = library.path)
  library(ggthemes, lib.loc = library.path)
  library(gridExtra, lib.loc = library.path)
  library(forecast, lib.loc = library.path)
  library(aTSA, lib.loc = library.path)
  library(DescTools, lib.loc = library.path)
  library(plyr, lib.loc = library.path)
  library(EnvStats, lib.loc = library.path)
  library(qcc, lib.loc = library.path)
  library(openxlsx, lib.loc = library.path)
  library(magrittr, lib.loc = library.path)
  library(rstatix, lib.loc = library.path)
  library(mcp, lib.loc = library.path)
  library(changepoint, lib.loc = library.path)
  
})


options(warn = oldw)

options(scipen=999, digits = 3 )

`%notin%` <- Negate(`%in%`)

all_days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")


#Setting the directory where all files will be used from for this project
setwd("C:\\Users\\steph\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\csv")


#setwd("C:\\Users\\steph\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\Output")


#-----------------------------------------------------------------------------------------------------------------------
#                                 Outliers Detection
#-----------------------------------------------------------------------------------------------------------------------


View(Sales_UK)

#Importing and clean Sales Data
Sales_UK <- read.csv("CampaignData.csv",  skip = 4, header = T, stringsAsFactors = FALSE)

#Outlier Boxplots
Sales_UK <- 
            Sales_UK  %>%
  
            subset(state == "enabled" &
                   Serving.Status == "Enabled") %>%
  
            mutate(country = case_when( grepl("\\$", ROAS )    ~ "US",
                                        grepl("\\£", ROAS )    ~ "UK",
                                        grepl("\\€", ROAS )    ~ "DE"),
                   ROAS = str_replace_all(ROAS, c("\\$" = "", "£" = "", "€" = "", "," = "")),
                   Spend = str_replace_all(Spend, c("\\$" = "", "£" = "", "€" = "", "," = "")),
                   Sales = str_replace_all(Sales, c("\\$" = "", "£" = "", "€" = "", "," = "")) ) %>%
  
            mutate(ROAS = as.numeric(ROAS),
                   Spend = as.numeric(Spend),
                   Sales = as.numeric(Sales),
                   lbl = paste0(Name," - (", Sales, ")"),
                   rnk = rownames(.)) %>%
  
            arrange(ROAS, desc(Spend)) #%>%
            View(.)
              

#Spend vs Sales
ggplot(Sales_UK, aes(x=Spend, y=Sales)) + geom_point() +  
  geom_smooth(method='lm', formula= y~x) +
  ggrepel::geom_text_repel(aes(label = if_else(ROAS < 2 & Spend > 300, rnk, NULL)))
#  geom_text(aes(label = if_else(ROAS > 50, Name, NULL))) +

  #+ stat_summary(fun.data=mean_se, geom="pointrange", alpha=0.25)


Sales_UK_ok <- Sales_UK[Sales_UK$ROAS < 2 & Sales_UK$Spend > 300,]
View(Sales_UK_ok)

write.csv( Sales_UK_ok, "Spend vs Sales.csv", row.names = F)

#ggsave("UK AA Spend Boxplots Status B.png",width = 11.4, height = 8.44, dpi = 300)








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
  mutate(across(13:14, round, 3)) %>% 
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
  mutate(across(13:14, round, 3)) %>% 
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
  mutate(across(13:14, round, 3)) %>% 
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


setwd("C:\\Users\\steph\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\csv")


Sales_UK <- read.csv("UK_AA_Data_COT.csv", header = T, stringsAsFactors = FALSE)
Sales_UK <- 
  Sales_UK %>%
  plyr::rename(c("ONSALE_DATE" = "Pub_Date")) %>%
  mutate(AA.Reason.Code = gsub("Core title", "Core Title",AA.Reason.Code)) %>%
  select(1:9) %>%
  dcast(Country + Title + Asin + ISBN + Pub_Date + AA.Status + AA.Reason.Code ~ Month.Date, 
        value.var="AA_SPEND", fun.aggregate = NULL)

setwd("C:\\Users\\steph\\Documents\\DK\\Work\\Forecasting book sales and inventory\\Auxilary tasks\\Amazon Advertisment\\Output")

#Function to change AA Spend NA to 0 after book is published
for (i in 1:nrow(Sales_UK)){
  
  for (j in 8:ncol(Sales_UK)){
      
    if ( as.Date(Sales_UK$Pub_Date[i]) <= as.Date(paste0(colnames(Sales_UK)[j],"-01")) ) {
      
      if (is.na(Sales_UK[i,j])){
        
        Sales_UK[i,j] <- 0
        
      }
      
    }    
    
  }
  
}  


# value.ts = ts(as.numeric(Sales_UK[4,8:28]), frequency=12, start=c(2021,4), end=c(2022,12))
# plot(value.ts)
# 
# #Mean changepoint
# mvalue = cpt.mean(value.ts, method="BinSeg") #mean changepoints using PELT
# cpts(mvalue)
# plot(mvalue)
# 
# #Variance changepoint
# vnvalue = cpt.var(diff(value.ts), method="BinSeg", Q=6) 
# cpts(vnvalue)
# plot(vnvalue)



#Slope Calculation (trend)
Sales_UK$Slope <- NA
Sales_UK$SD <- NA
for (i in 1:nrow(Sales_UK)){
  
  if (!all(is.na(as.numeric(Sales_UK[i,c(8:28)])))){
    Sales_UK$Slope[i] <- as.numeric(lm(as.numeric(Sales_UK[i,c(8:28)]) ~ c(1:21), na.action=na.exclude)$coefficients[2])
    Sales_UK$SD[i] <- sd(Sales_UK[i,c(8:28)], na.rm=T)
    
  }
}

Sales_UK$Trend <- NA
Sales_UK <- arrange(Sales_UK, Sales_UK$Slope)


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
addStyle(wb, "UK", style=centerStyle, rows = 1:nrow(Sales_Outliers)+1, cols = 8:ncol(Sales_Outliers), 
         gridExpand = T, stack = TRUE)

# Adding borders
invisible(OutsideBorders(
  wb,
  sheet_ = "UK",
  rows_ = 1:nrow(Sales_Outliers)+1,
  cols_ = 1:7
))

invisible(OutsideBorders(
  wb,
  sheet_ = "UK",
  rows_ = 1:nrow(Sales_Outliers)+1,
  cols_ = 8:29
))

invisible(OutsideBorders(
  wb,
  sheet_ = "UK",
  rows_ = 1:nrow(Sales_Outliers)+1,
  cols_ = 30:31
))

invisible(OutsideBorders(
  wb,
  sheet_ = "UK",
  rows_ = 1:nrow(Sales_Outliers)+1,
  cols_ = 32
))

freezePane(
  wb,
  sheet = "UK",
  firstActiveRow = 3,
  firstActiveCol = 3
)



pred_date <- Sys.Date() + 6 - match(weekdays(Sys.Date()), all_days)

#This needs to be produced once a month
#saveWorkbook(wb, paste0("AA Spend Analysis COT - ",pred_date,".xlsx"), overwrite = T) 




# 
# View(Sales_UK)
# Sales_UKA <- Sales_UK  %>%
#   subset(AA.Status %in% c("A") ) 
# 
# Sales_UKB <- Sales_UK  %>%
#   subset(AA.Status %in% c("B") ) 
# 
# Sales_UKC <- Sales_UK  %>%
#   subset(AA.Status %in% c("C") ) 
# 
# #Creating regression model of spend vs sales
# model_A <- lm(AA_SALES ~ AA_SPEND, data = Sales_UKA)
# 
# #Creating regression model of spend vs sales
# model_B <- lm(AA_SALES ~ AA_SPEND, data = Sales_UKB)
# 
# #Creating regression model of spend vs sales
# model_C <- lm(AA_SALES ~ AA_SPEND, data = Sales_UKC)
# 
# summary(model_A)
# summary(model_B)
# summary(model_C)
# 
# #Plotting residuals
# par(mfrow = c(2, 2))
# plot(model_SS)
# 
# View(Sales_UK[c(1008,1072,1077),])
# 
# #Calculating outliers with Cook's distance
# cooks_outliers <- cooks.distance(model_A)
# OutliersA <- cooks_outliers[(cooks_outliers > 1)]
# OutliersA
# 
# #Calculating outliers with Cook's distance
# cooks_outliers <- cooks.distance(model_B)
# OutliersB <- cooks_outliers[(cooks_outliers > 1)]
# OutliersB
# 
# View(Sales_UK[c(1077),])
# 
# 
# #Calculating outliers with Cook's distance
# cooks_outliers <- cooks.distance(model_C)
# OutliersC <- cooks_outliers[(cooks_outliers > 1)]
# OutliersC
# 
# View(Sales_UK[c(1064,1067),])
# 
# 
# 
# 
# 
# 
# #Boxplot
# is_outlier <- function(x) {
#   return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 4 * IQR(x))
# }
# 
# Sales_UK2 <- Sales_UK  %>%
#   subset(AA.Status %in% c("A", "B", "C") )  %>%
#   group_by(AA.Reason.Code) %>%
#   mutate(outlier = ifelse(is_outlier(AA_SPEND), Title, as.numeric(NA)))
# 
# View(Sales_UK2)
# 
# ggplot(Sales_UK2, aes(x=AA.Reason.Code, y=AA_SPEND)) + 
#   geom_boxplot()  +
#   geom_text(aes(label=outlier), na.rm=TRUE, vjust=-.9,
#             position=position_jitter(width=0,height=-1)) +
#   facet_wrap(~AA.Status) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),  
#         axis.title.x = element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   ggtitle("UK AA Spend Boxplots") +
#   ylab("AA Spend (£)") 
# 
# 
# #Standard deviations
# aggregate(AA_SPEND ~ AA.Status, Sales_UK2, function(x) c(count = length(x) ,sum = sum(x) ,mean = mean(x), median = median(x) , sd = sd(x)))
# 
# 


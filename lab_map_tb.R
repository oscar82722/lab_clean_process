rm(list=ls())
library(rjson)
library(readxl)
library(writexl)
library(data.table)


data_folder='C:/Users/wang/Desktop/lab_clean_process/'


name_f <- list.files(paste0(data_folder, "clean/name/"))
unit_f <- list.files(paste0(data_folder, "clean/unit/clean_unit/"))
u <- intersect(name_f, unit_f)
u <- gsub('.xlsx', '', u)

for(order_code in u){
  raw_df <- data.table(read_xlsx(paste0(data_folder,'lab_name_count.xlsx'),
                                 sheet = order_code))
  n <- raw_df[ASSAY_ITEM_NAME== 'overall',N]
  name_file <- data.table(read_xlsx(paste0(data_folder,'/clean/name/', order_code, '.xlsx')))
  unit_file <- data.table(read_xlsx(paste0(data_folder,'/clean/unit/clean_unit/', order_code, '.xlsx')))
  
  df <- merge(raw_df, name_file[,.(ORDER_CODE,
                                   ASSAY_ITEM_NAME,
                                   ASSAY_NAME)], by=c('ORDER_CODE', 'ASSAY_ITEM_NAME'))
  
  df <- merge(df, unit_file[,.( ORDER_CODE, UNIT_DATA, Unit_Clean)],
              by=c('ORDER_CODE', 'UNIT_DATA'))
  
  df <- df[!is.na(ASSAY_NAME) & !is.na(Unit_Clean)]
  df <- df[,.(N=sum(N)),by=.(ORDER_CODE, ASSAY_NAME, Unit_Clean)]
  df <- df[order(ASSAY_NAME,-N)]
  df[,percent:=paste0(round(N/n,4)*100,'%')]
  df <- df[,.(ORDER_CODE, ASSAY_NAME, Unit_Clean, Assay_Unit=NA, trans=NA, N, percent)]
  colnames(df)[7] <- paste0('percent(n=', n,')')
  p <- paste0(data_folder,'/clean/unit/map_unit/', order_code, '.xlsx')
  write_xlsx(df, p)
  cat(order_code, '\n')
}



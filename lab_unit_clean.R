rm(list=ls())
library(rjson)
library(readxl)
library(writexl)
library(data.table)

judge_func <- function(df, rule_v, col){
  if(length(rule_v)!=0){
    df <- df[,colnames(df),with=F]
    col_v <- NULL
    for(i in 1:length(rule_v)){
      df[,r:=ifelse(grepl(rule_v[i], get(col)),1,0)]
      colnames(df)[dim(df)[2]] <- paste0('r', i)
      col_v <- c(col_v, paste0('r', i))
    }
    r <- apply(df[,col_v, with=F], 1, sum)
    r <- ifelse(r > 0, 1, 0)
  }else{
    r <- 0
  }
  return(r)
}


lab_unit_classifier <- function(order_code,
                                data_folder='C:/Users/wang/Desktop/lab_clean/'){
  
  cat(order_code, '\n')
  # read rule data
  rule_data <- fromJSON(file=paste0(data_folder, "lab_unit.json"))
  raw_df <- data.table(read_xlsx('C:/Users/wang/Desktop/lab_clean/lab_name_count.xlsx',
                                 sheet = order_code))
  
  clean_name <- data.table(read_xlsx(paste0('C:/Users/wang/Desktop/lab_clean/clean/name/', order_code, '.xlsx')))
  raw_df <- raw_df[ASSAY_ITEM_NAME %in% clean_name[!is.na(ASSAY_NAME), ASSAY_ITEM_NAME]]
  
  res_v <- NULL
  for(unit_name in names(rule_data)){
    cat('    ', unit_name, '\n')
    
    # Include
    raw_df[,include := judge_func(df=raw_df,
                                  rule_v = rule_data[[unit_name]][['Include']],
                                  col='UNIT_DATA')]
    cat('        Include done \n')
    
    # exclude
    raw_df[,exclude := judge_func(df=raw_df,
                                  rule_v = rule_data[[unit_name]][['Exclude']],
                                  col='UNIT_DATA')]
    cat('        Exclude done \n')
    
    raw_df[,res:=ifelse((include == 1) & (exclude == 0), unit_name, '')]
    raw_df <- raw_df[,-c('include', 'exclude'),with=F]
    colnames(raw_df)[dim(raw_df)[2]] <- paste0('res_', unit_name)
    res_v <- c(res_v, paste0('res_', unit_name))
  }
  
  # clean ASSAY_NAME
  raw_df[,Assay_Unit := do.call(paste, c(.SD, sep = ";")),
         by=.(ORDER_CODE, ASSAY_ITEM_NAME, UNIT_DATA,N), .SDcols=res_v]
  raw_df <- raw_df[,.(ORDER_CODE,
                      ASSAY_ITEM_NAME,
                      UNIT_DATA,
                      N,
                      Assay_Unit=ifelse(Assay_Unit==';', '', Assay_Unit))]
  
  raw_df[,Assay_Unit:=gsub('\\;+$', '', Assay_Unit)]
  raw_df[,Assay_Unit:=gsub('^\\;+', '', Assay_Unit)]
  
  # unique
  raw_df <- raw_df[,.(N = sum(N)),by=.(ORDER_CODE, UNIT_DATA, Assay_Unit)]
  
  # write result
  write_xlsx(raw_df, paste0(data_folder, 'clean/unit/clean_unit/', order_code, '.xlsx'))
}

lab_unit_classifier(order_code='08002C')
lab_unit_classifier(order_code='08003C')
lab_unit_classifier(order_code='08004C')
lab_unit_classifier(order_code='08006C')

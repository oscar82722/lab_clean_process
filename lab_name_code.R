rm(list=ls())
library(rjson)
library(readxl)
library(writexl)
library(data.table)

judge_func <- function(df, rule_v, col){
  if(length(rule_v)!=0){
    df <- df[,colnames(df),with=F]
    # col to lower
    df[, col_judge:=tolower(get(col))]
    col_v <- NULL
    for(i in 1:length(rule_v)){
      df[,r:=ifelse(grepl(tolower(rule_v[i]), col_judge),1,0)]
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

lab_name_classifier <- function(order_code,
                                data_folder= 'C:/Users/wang/Desktop/lab_clean_process/'){

  cat(order_code, '\n')
  # read rule data
  rule_data <- fromJSON(file=paste0(data_folder, "lab_name.json"))
  raw_df <- data.table(read_xlsx(paste0(data_folder,'lab_name_count.xlsx'),
                                 sheet = order_code))
  
  res_v <- NULL
  for(lab_name in names(rule_data[[order_code]])){
    cat('    ', lab_name, '\n')
    
    # Include
    raw_df[,include := judge_func(df=raw_df,
                                  rule_v = rule_data[[order_code]][[lab_name]][['Include']],
                                  col='ASSAY_ITEM_NAME')]
    cat('        Include done \n')
    
    # exclude
    raw_df[,exclude := judge_func(df=raw_df,
                                  rule_v = rule_data[[order_code]][[lab_name]][['Exclude']],
                                  col='ASSAY_ITEM_NAME')]
    cat('        Exclude done \n')
    
    raw_df[,res:=ifelse((include == 1) & (exclude == 0), lab_name, '')]
    raw_df <- raw_df[,-c('include', 'exclude'),with=F]
    colnames(raw_df)[dim(raw_df)[2]] <- paste0('res_', lab_name)
    res_v <- c(res_v, paste0('res_', lab_name))
  }
  
  # clean ASSAY_NAME
  raw_df[,ASSAY_NAME := do.call(paste, c(.SD, sep = ";")),
         by=.(ORDER_CODE,ASSAY_ITEM_NAME,UNIT_DATA,N), .SDcols=res_v]
  raw_df <- raw_df[,.(ORDER_CODE,
                      ASSAY_ITEM_NAME,
                      UNIT_DATA,
                      N,
                      ASSAY_NAME=ifelse(ASSAY_NAME==';', '', ASSAY_NAME))]
  
  raw_df[,ASSAY_NAME:=gsub('\\;+$', '', ASSAY_NAME)]
  raw_df[,ASSAY_NAME:=gsub('^\\;+', '', ASSAY_NAME)]
  
  # unique
  raw_df <- raw_df[,.(N = sum(N)),by=.(ORDER_CODE, ASSAY_ITEM_NAME, ASSAY_NAME)]
  
  # write result
  write_xlsx(raw_df, paste0(data_folder, 'clean/name/', order_code, '.xlsx'))
}



lab_name_classifier(order_code='08002C')
lab_name_classifier(order_code='08003C')
lab_name_classifier(order_code='08004C')
lab_name_classifier(order_code='08006C')
lab_name_classifier(order_code='08022B')
lab_name_classifier(order_code='08024B')
lab_name_classifier(order_code='08025B')
lab_name_classifier(order_code='08026C')
lab_name_classifier(order_code='08036B')
lab_name_classifier(order_code='08036C')
lab_name_classifier(order_code='08037B')
lab_name_classifier(order_code='08038B')
lab_name_classifier(order_code='08079B')
lab_name_classifier(order_code='10511C')
lab_name_classifier(order_code='12193B')
lab_name_classifier(order_code='12193C')
lab_name_classifier(order_code='12013C')
lab_name_classifier(order_code='12014C')
lab_name_classifier(order_code='12015C')
lab_name_classifier(order_code='08011C')











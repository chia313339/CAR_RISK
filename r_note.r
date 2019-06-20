
# 資料探索
DM_PROCESS = function(dataset){
  dataset = as.data.frame(dataset)
  col_list = names(dataset)
  print(paste(Sys.time(),'開始資料探索'))
  
  TYPE_LIST = unlist(lapply(dataset,function(x) class(x)))
  print(paste(Sys.time(),'資料型態統計'))
  print(table(TYPE_LIST))
  
  UNI_CNT = unlist(lapply(dataset,function(x) length(unique(x))))
  print(paste(Sys.time(),'有',sum(UNI_CNT==1),'個欄位都是唯一值'))
  
  NA_CNT = unlist(lapply(dataset,function(x) sum(is.na(x))))
  NA_PORB = NA_CNT/nrow(dataset)
  print(paste(Sys.time(),'有',sum(NA_CNT>0),'個欄位有缺失值'))
  
  DF = data.frame('欄位名稱' = col_list,
                  '欄位型態' = TYPE_LIST,
                  '元素個數' = UNI_CNT,
                  '缺失個數' = NA_CNT,
                  '缺失占比' = NA_PORB,row.names = NULL)
  head(DF)
  return(DF)
}

#
report_result(df,'PRED_Y')
report_result(df,'POLICY_RESULT_CODE')

report_result = function(df,result_col){
  RESULT_COL = result_col
  
  PRED_RESULT = c('人工審查','人工審查','自動核保','自動核保')
  REAL_RESULT = c('出險','未出險','出險','未出險')
  tmp_tb=table(df[[RESULT_COL]])
  tmp_1=tmp_tb[1]
  tmp_2=tmp_tb[2]
  POLICY_NUM = c(tmp_1,tmp_1,tmp_2,tmp_2)
  POLICY_NUM_PROB = c(tmp_1/sum(tmp_tb),tmp_1/sum(tmp_tb),tmp_2/sum(tmp_tb),tmp_2/sum(tmp_tb))
  POLICY_RESULT = c(sum(df$INSURE>0 & df[[RESULT_COL]]=='1'),sum(df$INSURE==0 & df[[RESULT_COL]]=='1'),sum(df$INSURE>0 & df[[RESULT_COL]]=='0'),sum(df$INSURE==0 & df[[RESULT_COL]]=='0'))
  POLICY_RESULT_PROB =c(sum(df$INSURE>0 & df[[RESULT_COL]]=='1')/tmp_1,sum(df$INSURE==0 & df[[RESULT_COL]]=='1')/tmp_1,sum(df$INSURE>0 & df$PRED_Y=='0')/tmp_2,sum(df$INSURE==0 & df$PRED_Y=='0')/tmp_2)
  PREMIUM = c(sum(df$PREMIUM[df[[RESULT_COL]]=='1']),sum(df$PREMIUM[df[[RESULT_COL]]=='1']),sum(df$PREMIUM[df[[RESULT_COL]]=='0']),sum(df$PREMIUM[df[[RESULT_COL]]=='0']))
  INSURE = c(sum(df$INSURE[df[[RESULT_COL]]=='1']),sum(df$INSURE[df[[RESULT_COL]]=='1']),sum(df$INSURE[df[[RESULT_COL]]=='0']),sum(df$INSURE[df[[RESULT_COL]]=='0']))
  LOSS_RATE = INSURE/PREMIUM
  result_df = data.frame('預測結果'=PRED_RESULT,
                         '實際結果'=REAL_RESULT,
                         '承保件數'=POLICY_NUM,
                         '承保比例'=POLICY_NUM_PROB,
                         '各承保件數'=POLICY_RESULT,
                         '各承保件比例'=POLICY_RESULT_PROB,
                         '總簽單保費'=PREMIUM,
                         '總理賠金額'=INSURE,
                         '損率'=LOSS_RATE
  )
  return(result_df)
}




















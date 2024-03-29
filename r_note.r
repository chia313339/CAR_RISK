
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

 ###################################
drop_list = c('APPLLTE_NO', 'PRODUCT_CODE', 'PRODUCT_POLICY_CODE', 'PRODUCT_SUB_CODE', 'POLICY_NO', 'POLICY_STATUS', 'APPLLTE_DATE', 'ISSUE_DATE', 'END_DATETIME', 'INSRNCE_TARGET', 'CONTINUE_POLICY_NO', 'AGENT_ID', 'COUNSEL_ID', 'CUSTOMER_CLASSFY', 'CUSTOMER_ID', 'IS_MAIN_INSURED', 'BIRTHDAY', 'OCCU_CATEGORY_CODE', 'OCCU_CATEGORY_CODE_IND', 'POLICY_TYPE', 'CAR_NO', 'ENGINE_NO', 'ORIGIN_ISSUE_DATE', 'MADE_DATE', 'LEGAL_PREMIUM', 'TRADE_FORCE_NO', 'TRADE_START_DATE', 'ACCEPT_OFFICER', 'VEHICLE_KIND_NAME', 'MODEL_NAME', 'IS_WHITE_LIST')
drop_ind =  names(dataset)  %in% drop_list
dataset = dataset[!drop_ind]


numeric_col = c('DISCOUNT_PREMIUM', 'INSRNCE_AMOUNT_COMPLEX', 'INSRNCE_AMOUNT_BURGLAR', 'INSRNCE_AMOUNT_LIAB', 'INSRNCE_AMOUNT_PASSENGER', 'INSRNCE_AMOUNT_OTHER', 'INSRNCE_AMOUNT_SUM', 'PREMIUM_COMPLEX', 'PREMIUM_BURGLAR', 'PREMIUM_LIAB', 'PREMIUM_PASSENGER', 'PREMIUM_OTHER', 'ENGINE_EXHAUST', 'MAXIMUM_LOAD', 'REPLACE_PRICE', 'BODY_PREMIUM_RATE', 'LCNR_PREMIUM_RATE', 'AGE_RATE', 'BODY_CLAIM_TIMES1', 'BODY_CLAIM_TIMES2', 'BODY_CLAIM_TIMES3', 'BODY_CLAIM_RATE', 'LIAB_CLAIM_TIMES1', 'LIAB_CLAIM_TIMES2', 'LIAB_CLAIM_TIMES3', 'LIAB_CLAIM_RATE', 'SHORT_PREMIUM_RATE', 'DEPRECIA_RATE', 'TAXI_PRE_FACTOR', 'RISK_SCORE', 'INSURE_COMPLEX', 'INSURE_BURGLAR', 'INSURE_LIAB', 'INSURE_PASSENGER', 'INSURE_OTHERS', 'INSURE_FORCE', 'INSURE_FORCE_INJURE', 'INSURE', 'INSURE_COMPLEX_L1Y', 'INSURE_BURGLAR_L1Y', 'INSURE_LIAB_L1Y', 'INSURE_PASSENGER_L1Y', 'INSURE_OTHERS_L1Y', 'INSURE_FORCE_L1Y', 'INSURE_FORCE_INJURE_L1Y', 'INSURE_L1Y', 'INSURE_COMPLEX_L3Y', 'INSURE_BURGLAR_L3Y', 'INSURE_LIAB_L3Y', 'INSURE_PASSENGER_L3Y', 'INSURE_OTHERS_L3Y', 'INSURE_FORCE_L3Y', 'INSURE_FORCE_INJURE_L3Y', 'INSURE_L3Y', 'INSURE_COMPLEX_L5Y', 'INSURE_BURGLAR_L5Y', 'INSURE_LIAB_L5Y', 'INSURE_PASSENGER_L5Y', 'INSURE_OTHERS_L5Y', 'INSURE_FORCE_L5Y', 'INSURE_FORCE_INJURE_L5Y', 'INSURE_L5Y', 'INSURE_COMPLEX_CNT_L1Y', 'INSURE_BURGLAR_CNT_L1Y', 'INSURE_LIAB_CNT_L1Y', 'CAR_INSURE_COMPLEX_L1Y', 'CAR_INSURE_BURGLAR_L1Y', 'CAR_INSURE_LIAB_L1Y', 'CAR_INSURE_PASSENGER_L1Y', 'CAR_INSURE_OTHERS_L1Y', 'CAR_INSURE_FORCE_L1Y', 'CAR_INSURE_FORCE_INJURE_L1Y', 'CAR_INSURE_L1Y', 'CAR_INSURE_COMPLEX_L3Y', 'CAR_INSURE_BURGLAR_L3Y', 'CAR_INSURE_LIAB_L3Y', 'CAR_INSURE_PASSENGER_L3Y', 'CAR_INSURE_OTHERS_L3Y', 'CAR_INSURE_FORCE_L3Y', 'CAR_INSURE_FORCE_INJURE_L3Y', 'CAR_INSURE_L3Y', 'CAR_INSURE_COMPLEX_L5Y', 'CAR_INSURE_BURGLAR_L5Y', 'CAR_INSURE_LIAB_L5Y', 'CAR_INSURE_PASSENGER_L5Y', 'CAR_INSURE_OTHERS_L5Y', 'CAR_INSURE_FORCE_L5Y', 'CAR_INSURE_FORCE_INJURE_L5Y', 'CAR_INSURE_L5Y', 'LOSS_RATE_COMPLEX', 'LOSS_RATE_BURGLAR', 'LOSS_RATE_LIAB', 'LOSS_RATE_PASSENGER', 'LOSS_RATE_OTHERS', 'LOSS_RATE', 'DIV_INSURE', 'DIV_PREMIUM', 'DIV_INSURED_CNT', 'DIV_POLICY_CNT', 'DIV_LOSS_RATE') 
integer_col = c('IS_CONTINUE_POLICY_NO', 'IS_COMPLEX', 'IS_BURGLAR', 'IS_LIAB', 'IS_PASSENGER', 'IS_OTHER', 'CATEGORY_KIND_CNT', 'COMPLEX_MAIN', 'COMPLEX_RECOVERY', 'COMPLEX_UNCLEAR', 'COMPLEX_WEATHER', 'COMPLEX_HUMAN', 'COMPLEX_DEPRECIATION', 'COMPLEX_MORTO', 'COMPLEX_MORTOSTEAL', 'BURGLAR_CAR', 'BURGLAR_DEPRECIATION', 'BURGLAR_CARFEE', 'BURGLAR_MORTO', 'BURGLAR_HIGHPRICE', 'LIAB_BODY', 'LIAB_FINANCE', 'LIAB_MULTPEOPLE', 'LIAB_CONSOLATION', 'LIAB_PASSENGER', 'LIAB_ALCOHOL', 'PASSENGER_MORTO', 'PASSENGER_DRIVE', 'PASSENGER_BODY', 'PASSENGER_CAR', 'OTHER_CARFEE', 'OTHER_ROADSERVICE', 'CUSTOMER_TYPE', 'IDNTFCTN_TYPE', 'SEX', 'AGE', 'MARRIAGE', 'ORIGIN_ISSUE_DUR', 'CAR_AGE', 'ENGINE_EXHAUST_UNIT', 'AUTO_HAND', 'IS_ADD_OUTFIT', 'BODY_NO', 'LCNR_NO', 'BODY_CLAIM_CODE', 'LIAB_CLAIM_CODE', 'SPECIAL_INSRNCE_TYPE', 'INSURE_BODY', 'TAXI_PRENOCLAIM_YEAR', 'TAXI_PRENOTOTAL_CLAIM', 'USE_TYPE', 'VEHICLE_SIZE', 'SPECIAL_ENACTMENT', 'ORIGIN_CODE', 'COMPLEX_CNT_L3Y', 'BURGLAR_CNT_L3Y', 'LIAB_CNT_L3Y', 'PASSENGER_CNT_L3Y', 'OTHERS_CNT_L3Y', 'FORCE_CNT_L3Y', 'FORCE_INJURE_CNT_L3Y', 'COMPLEX_IND_L3Y', 'BURGLAR_IND_L3Y', 'LIAB_IND_L3Y', 'PASSENGER_IND_L3Y', 'OTHERS_IND_L3Y', 'FORCE_IND_L3Y', 'FORCE_INJURE_IND_L3Y', 'INSURE_CAUSE_L1Y', 'INSURE_CAUSE_L3Y', 'INSURE_CAUSE_L5Y', 'INSURE_PASSENGER_CNT_L1Y', 'INSURE_OTHERS_CNT_L1Y', 'INSURE_FORCE_CNT_L1Y', 'INSURE_FORCE_INJURE_CNT_L1Y', 'INSURE_CNT_L1Y', 'INSURE_COMPLEX_CNT_L3Y', 'INSURE_BURGLAR_CNT_L3Y', 'INSURE_LIAB_CNT_L3Y', 'INSURE_PASSENGER_CNT_L3Y', 'INSURE_OTHERS_CNT_L3Y', 'INSURE_FORCE_CNT_L3Y', 'INSURE_FORCE_INJURE_CNT_L3Y', 'INSURE_CNT_L3Y', 'INSURE_COMPLEX_CNT_L5Y', 'INSURE_BURGLAR_CNT_L5Y', 'INSURE_LIAB_CNT_L5Y', 'INSURE_PASSENGER_CNT_L5Y', 'INSURE_OTHERS_CNT_L5Y', 'INSURE_FORCE_CNT_L5Y', 'INSURE_FORCE_INJURE_CNT_L5Y', 'INSURE_CNT_L5Y', 'CAR_INSURE_COMPLEX_CNT_L1Y', 'CAR_INSURE_BURGLAR_CNT_L1Y', 'CAR_INSURE_LIAB_CNT_L1Y', 'CAR_INSURE_PASSENGER_CNT_L1Y', 'CAR_INSURE_OTHERS_CNT_L1Y', 'CAR_INSURE_FORCE_CNT_L1Y', 'CAR_INSURE_FORCE_INJURE_CNT_L1Y', 'CAR_INSURE_CNT_L1Y', 'CAR_INSURE_COMPLEX_CNT_L3Y', 'CAR_INSURE_BURGLAR_CNT_L3Y', 'CAR_INSURE_LIAB_CNT_L3Y', 'CAR_INSURE_PASSENGER_CNT_L3Y', 'CAR_INSURE_OTHERS_CNT_L3Y', 'CAR_INSURE_FORCE_CNT_L3Y', 'CAR_INSURE_FORCE_INJURE_CNT_L3Y', 'CAR_INSURE_CNT_L3Y', 'CAR_INSURE_COMPLEX_CNT_L5Y', 'CAR_INSURE_BURGLAR_CNT_L5Y', 'CAR_INSURE_LIAB_CNT_L5Y', 'CAR_INSURE_PASSENGER_CNT_L5Y', 'CAR_INSURE_OTHERS_CNT_L5Y', 'CAR_INSURE_FORCE_CNT_L5Y', 'CAR_INSURE_FORCE_INJURE_CNT_L5Y', 'CAR_INSURE_CNT_L5Y', 'IS_TRADE_FORCE', 'IS_LIMIT_DRIVER', 'IS_CHANGE_CARNO')
factor_col = c('UNIT_NO', 'POLICY_RESULT_CODE', 'BUSINESS_ORIGIN', 'COPERATE_COMPANY_NO', 'CHANNEL_TYPE', 'BROKER_NO', 'AGENT_KIND', 'BUSINESS_TYPE', 'OCCU_TYPE1_CODE', 'MODEL_FULL_NO', 'VEHICLE_KIND_NO', 'KIND_NO', 'MAXIMUM_LOAD_UNIT', 'COMPUTER_CALCULAT_CODE', 'IS_NEW_CAR', 'QUESTION_CAR_NO_IND', 'QUESTION_ENGIN_IND', 'IS_NOT_FORCE', 'MODEL_SUB_NAME', 'MODEL_NO', 'MOTOR_FLG', 'RISK_LEVEL', 'SUSPECT_LEVEL', 'INTRDUCE_KIND', 'RELATION_TYPE')
character_col = c('CONTRACT_NO', 'AGENT_DIV_NO', 'COUNSEL_DIV_NO') 

typechange = function(dataset,numeric_col,integer_col,factor_col,character_col){
  dataset = as.data.frame(dataset)
  if(sum(numeric_col!="")>0) dataset[numeric_col] = lapply(dataset[numeric_col], as.numeric) 
  if(sum(integer_col!="")>0) dataset[integer_col] = lapply(dataset[integer_col], as.integer) 
  if(sum(factor_col!="")>0) dataset[factor_col] = lapply(dataset[factor_col], as.factor) 
  if(sum(character_col!="")>0) dataset[character_col] = lapply(dataset[character_col], as.character) 
  return(dataset)
}

dataset = typechange(dataset,numeric_col,integer_col,factor_col,character_col)


##############
label_encoder = function(vec){
  levels = names(sort(table(vec)))
  function(x){
    match(x, levels)
  }
}














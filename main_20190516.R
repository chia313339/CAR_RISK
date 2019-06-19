rm(list = ls())
gc()

setwd("~/R/ML/CAR/policy_rick")
# library('doParallel')
# cl<-makeCluster(50) 
# registerDoParallel(cl)


dataset = as.data.frame(dataset)

################################################################################

# dataset$START_DATETIME = as.Date(dataset$START_DATETIME)
# dataset$END_DATETIME = as.Date(dataset$END_DATETIME)

drop_list =c('APPLLTE_NO',  'PRODUCT_CODE', 'PRODUCT_POLICY_CODE',	'PRODUCT_SUB_CODE',	'POLICY_NO', 'POLICY_STATUS',	'POLICY_RESULT_CODE',	'APPLLTE_DATE', 'ISSUE_DATE',	'END_DATETIME',	'INSRNCE_TARGET', 'CONTINUE_POLICY_NO',	'AGENT_ID',	'COUNSEL_ID', 'CUSTOMER_CLASSFY',	'CUSTOMER_ID',	'IS_MAIN_INSURED', 'BIRTHDAY',	'OCCU_CATEGORY_CODE', 'OCCU_CATEGORY_CODE_IND',	'POLICY_TYPE',	'CAR_NO', 'ENGINE_NO',	'ORIGIN_ISSUE_DATE',	'MADE_DATE', 'LEGAL_PREMIUM',	'TRADE_FORCE_NO',	'TRADE_START_DATE', 'ACCEPT_OFFICER',	'VEHICLE_KIND_NAME',	'MODEL_NAME', 'IS_WHITE_LIST')
drop_ind =  names(dataset)  %in% drop_list
dataset = dataset[!drop_ind]


rm(dataset2017,dataset2016,dataset2015,dataset2014,dataset2013,dataset2012)

################################################################################


# 轉換函數
label_encoder = function(vec){
  levels = sort(unique(vec))
  function(x){
    match(x, levels)
  }
}

# feature engineer
LE_UNIT_NO = label_encoder(dataset$UNIT_NO)
tmp_col = as.integer(LE_UNIT_NO(dataset$UNIT_NO))
dataset$UNIT_NO = tmp_col

#tmp_col = ifelse(dataset$BUSINESS_ORIGIN %in% c('XXX','XXZ','XXY','XXJ'),dataset$BUSINESS_ORIGIN,'OTHS')
LE_BUSINESS_ORIGIN = label_encoder(dataset$BUSINESS_ORIGIN)
tmp_col = as.integer(LE_BUSINESS_ORIGIN(dataset$BUSINESS_ORIGIN))
dataset$BUSINESS_ORIGIN = tmp_col

LE_COPERATE_COMPANY_NO = label_encoder(dataset$COPERATE_COMPANY_NO)
tmp_col = as.integer(LE_COPERATE_COMPANY_NO(dataset$COPERATE_COMPANY_NO))
dataset$COPERATE_COMPANY_NO = tmp_col

LE_AGENT_DIV_NO = label_encoder(dataset$AGENT_DIV_NO)
tmp_col = as.integer(LE_AGENT_DIV_NO(dataset$AGENT_DIV_NO))
dataset$AGENT_DIV_NO = tmp_col

LE_COUNSEL_DIV_NO = label_encoder(dataset$COUNSEL_DIV_NO)
tmp_col = as.integer(LE_COUNSEL_DIV_NO(dataset$COUNSEL_DIV_NO))
dataset$COUNSEL_DIV_NO = tmp_col

LE_BROKER_NO = label_encoder(dataset$BROKER_NO)
tmp_col = as.integer(LE_BROKER_NO(dataset$BROKER_NO))
dataset$BROKER_NO = tmp_col

LE_AGENT_KIND = label_encoder(dataset$AGENT_KIND)
tmp_col = as.integer(LE_AGENT_KIND(dataset$AGENT_KIND))
dataset$AGENT_KIND = tmp_col

tmp_col = as.integer(ifelse(dataset$SEX=='M',1,ifelse(dataset$SEX=='F',0,NA)))
dataset$SEX = tmp_col

LE_BUSINESS_TYPE = label_encoder(dataset$BUSINESS_TYPE)
tmp_col = as.integer(LE_BUSINESS_TYPE(dataset$BUSINESS_TYPE))
dataset$BUSINESS_TYPE = tmp_col

LE_OCCU_TYPE1_CODE = label_encoder(dataset$OCCU_TYPE1_CODE)
tmp_col = as.integer(LE_OCCU_TYPE1_CODE(dataset$OCCU_TYPE1_CODE))
dataset$OCCU_TYPE1_CODE = tmp_col

LE_MODEL_FULL_NO = label_encoder(dataset$MODEL_FULL_NO)
tmp_col = as.integer(LE_MODEL_FULL_NO(dataset$MODEL_FULL_NO))
dataset$MODEL_FULL_NO = tmp_col

LE_VEHICLE_KIND_NO = label_encoder(dataset$VEHICLE_KIND_NO)
tmp_col = as.integer(LE_VEHICLE_KIND_NO(dataset$VEHICLE_KIND_NO))
dataset$VEHICLE_KIND_NO = tmp_col

LE_KIND_NO = label_encoder(dataset$KIND_NO)
tmp_col = as.integer(LE_KIND_NO(dataset$KIND_NO))
dataset$KIND_NO = tmp_col

LE_BODY_NO = label_encoder(dataset$BODY_NO)
tmp_col = as.integer(LE_BODY_NO(dataset$BODY_NO))
dataset$BODY_NO = tmp_col

LE_MODEL_SUB_NAME = label_encoder(dataset$MODEL_SUB_NAME)
tmp_col = as.integer(LE_MODEL_SUB_NAME(dataset$MODEL_SUB_NAME))
dataset$MODEL_SUB_NAME = tmp_col

LE_MODEL_NO = label_encoder(dataset$MODEL_NO)
tmp_col = as.integer(LE_MODEL_NO(dataset$MODEL_NO))
dataset$MODEL_NO = tmp_col

LE_RISK_LEVEL = label_encoder(dataset$RISK_LEVEL)
tmp_col = as.integer(LE_RISK_LEVEL(dataset$RISK_LEVEL))
dataset$RISK_LEVEL = tmp_col

LE_SUSPECT_LEVEL = label_encoder(dataset$SUSPECT_LEVEL)
tmp_col = as.integer(LE_SUSPECT_LEVEL(dataset$SUSPECT_LEVEL))
dataset$SUSPECT_LEVEL = tmp_col

tmp_col = as.integer(ifelse(is.na(dataset$COMPUTER_CALCULAT_CODE),0,1))
dataset$COMPUTER_CALCULAT_CODE = tmp_col

tmp_col = as.integer(ifelse(dataset$IS_CONTINUE_POLICY_NO=='N',0,1))
dataset$IS_CONTINUE_POLICY_NO = tmp_col

LE_MAXIMUM_LOAD_UNIT = label_encoder(dataset$MAXIMUM_LOAD_UNIT)
tmp_col = as.integer(LE_MAXIMUM_LOAD_UNIT(dataset$MAXIMUM_LOAD_UNIT))
dataset$MAXIMUM_LOAD_UNIT = tmp_col

dataset$AUTO_HAND = as.integer(dataset$AUTO_HAND)

tmp_col = as.integer(ifelse(dataset$IS_NEW_CAR=='N',0,1))
dataset$IS_NEW_CAR = tmp_col

tmp_col = as.integer(ifelse(dataset$QUESTION_CAR_NO_IND=='N',0,1))
dataset$QUESTION_CAR_NO_IND = tmp_col

tmp_col = as.integer(ifelse(dataset$QUESTION_ENGIN_IND=='N',0,1))
dataset$QUESTION_ENGIN_IND = tmp_col

tmp_col = as.integer(ifelse(dataset$IS_NOT_FORCE=='N',0,1))
dataset$IS_NOT_FORCE = tmp_col

tmp_col = as.integer(ifelse(dataset$MOTOR_FLG=='N',0,1))
dataset$MOTOR_FLG = tmp_col

dataset$FORCE_INJURE_IND_L3Y = as.integer(dataset$FORCE_INJURE_IND_L3Y)

# remove strange data
dataset = dataset[dataset$ORIGIN_ISSUE_DUR>=0,]
dataset = dataset[dataset$AGE>=0,]

dataset$START_DATETIME = as.Date(dataset$START_DATETIME)

traindata = dataset[dataset$START_DATETIME<'2017-01-01',]
testdata = dataset[dataset$START_DATETIME>='2017-01-01',]

saveRDS(traindata,'traindata.rds')
saveRDS(testdata,'testdata.rds')



###############################################################



#########################################
setwd("~/R/ML/CAR/policy_risk")
library(caret)
library(xgboost)
library(pROC)

testdata = readRDS('testdata.rds')

Test_X$START_DATETIME = NULL
Test_X$INSURE_COMPLEX=NULL
Test_X$INSURE_BURGLAR=NULL
Test_X$INSURE_LIAB=NULL
Test_X$INSURE_PASSENGER=NULL
Test_X$INSURE_OTHERS=NULL
Test_X$INSURE_FORCE=NULL
Test_X$INSURE_FORCE_INJURE=NULL
Test_X$INSURE=NULL

tmp =  ifelse(Test_X$LOSS_RATE_COMPLEX>0,1,0)
tmp[is.na(tmp)]=0
Test_X$LOSS_RATE_COMPLEX=tmp

tmp =  ifelse(Test_X$LOSS_RATE_BURGLAR>0,1,0)
tmp[is.na(tmp)]=0
Test_X$LOSS_RATE_BURGLAR=tmp

tmp =  ifelse(Test_X$LOSS_RATE_LIAB>0,1,0)
tmp[is.na(tmp)]=0
Test_X$LOSS_RATE_LIAB=tmp

tmp =  ifelse(Test_X$LOSS_RATE_PASSENGER>0,1,0)
tmp[is.na(tmp)]=0
Test_X$LOSS_RATE_PASSENGER=tmp

tmp =  ifelse(Test_X$LOSS_RATE_OTHERS>0,1,0)
tmp[is.na(tmp)]=0
Test_X$LOSS_RATE_OTHERS=tmp

tmp =  ifelse(Test_X$LOSS_RATE>0,1,0)
tmp[is.na(tmp)]=0
Test_X$LOSS_RATE=tmp

gc()

Test_X$LOSS_RATE_COMPLEX=NULL
Test_X$LOSS_RATE_BURGLAR=NULL
Test_X$LOSS_RATE_LIAB=NULL
Test_X$LOSS_RATE_PASSENGER=NULL
Test_X$LOSS_RATE_OTHERS=NULL

TargetColumn = 'LOSS_RATE'
ID_column = 'CONTRACT_NO'

Test_Y = Test_X[[TargetColumn]]
Test_X = Test_X[,-which(colnames(Test_X) %in% c(TargetColumn,ID_column))]

pred_te <- rep(0, nrow(Test_X))

model_1 = readRDS('model_1.rds')
model_2 = readRDS('model_2.rds')
model_3 = readRDS('model_3.rds')
model_4 = readRDS('model_4.rds')

pred_test  = predict(model_1, as.matrix(Test_X) )
pred_te = pred_te +pred_test
pred_test  = predict(model_2, as.matrix(Test_X) )
pred_te = pred_te +pred_test
pred_test  = predict(model_3, as.matrix(Test_X) )
pred_te = pred_te +pred_test
pred_test  = predict(model_4, as.matrix(Test_X) )
pred_te = pred_te +pred_test

pred_test = pred_te/4
auc(Test_Y,pred_test)

Pred_Y = ifelse(pred_test > 0.475,'1','0')
# confusionMatrix(Pred_Y,Test_Y)


# 建立混淆矩陣並顯示結果
confusion_matrix = table(Pred_Y,Test_Y,dnn = c("預測", "實際"))
confusion_matrix
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
LH=diag(confusion_matrix)/colSums(confusion_matrix)
accuracy
LH




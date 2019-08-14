#-----------------------------------------         Prepare Codebase   --------------------------------------###
#Set working directory
setwd("C:/Users/rishabh.samdarshi/Desktop/Vision Forecasting/For model/Model Iterations")

library(dplyr)
library(broom)
library(reshape)
library(fastDummies)
library(writexl)
require(caTools)
library(car)
library(lubridate)
library(caret)
library(glmnet)

#-----------------------------------------         Read ADS   --------------------------------------###
master_ads<-read.csv("C:\\Users\\rishabh.samdarshi\\Desktop\\My Codes\\MARS Regression Model\\Treated_ADS_Cat_v2.csv", stringsAsFactors = F)

str(master_ads)


#End Potential
EndPotential_dummy<-data.frame()
EndPotential_dummy<-dummy_cols(master_ads$End_Potential, remove_most_frequent_dummy = TRUE)
colnames(EndPotential_dummy)=c("End_Potential","End_Potential_High","End_Potential_Low","End_Potential_Medium")
EndPotential_dummy$End_Potential<-NULL



# #Holiday Flag
# Holiday_dummy<-data.frame()
# Holiday_dummy <-dummy_cols(master_ads$HolidayCategory, remove_most_frequent_dummy = T)
# colnames(Holiday_dummy)<-c("Holiday.Flag","Flag_Pre_Holiday","Flag_Holiday","Flag_PostHoliday")
# Holiday_dummy$Holiday.Flag<-NULL

# #EndType Flag
# EndType_dummy<-data.frame()
# EndType_dummy<-dummy_cols(master_ads$End_Type, remove_most_frequent_dummy = T)
# colnames(EndType_dummy)<-c("EndType","Flag_SemiShelved","Flag_SemiBulk","Flag_Shelved")
# EndType_dummy$EndType<-NULL


detach("package:fastDummies", unload=TRUE)

#Merge with master ADS
master_ads <- cbind(master_ads,EndPotential_dummy)
# master_ads <- cbind(master_ads,Holiday_dummy)
# master_ads<- cbind(master_ads,EndType_dummy)


#Add temperature
temperature_df<-read.csv("C:/Users/palojtal/Desktop/Backup from TP/Syed/Vision Forecasting/For model/For model/Temp.Dummy.File.csv", stringsAsFactors = F)

#Merge with master ads 
master_ads <- left_join(master_ads,temperature_df)


#Read brand mapping file
brand<-read.csv("C:/Users/palojtal/Desktop/Backup from TP/Syed/Vision Forecasting/For model/For model/Brand Mapping.csv", stringsAsFactors = F)
brand <- brand %>% select(Product,Pack.Type)

#Merge with masterads
master_ads <- left_join(master_ads,brand)

#Create a SP_Category dataframe to merge back with model results later
sp_cat<-unique(master_ads %>% select(SP_Key,Sub.Category))
sp_packtype<-unique(master_ads %>% select(SP_Key,Pack.Type))
sp_weight<-unique(master_ads %>% select(SP_Key,size.g.))

# Create the season flags for the ads 

master_ads$WeekEndNumber<- as.Date.character(master_ads$WeekEndNumber,"%Y/%m/%d")

master_ads$Season_Winter<- unlist(Map(function(x) ifelse(x %in% c(6,7,8),1,0), month(master_ads$WeekEndNumber)))
master_ads$Season_Summer<- unlist(Map(function(x) ifelse(x %in% c(12,1,2),1,0), month(master_ads$WeekEndNumber)))
master_ads$Season_Autumn<- unlist(Map(function(x) ifelse(x %in% c(3,4,5),1,0), month(master_ads$WeekEndNumber)))
master_ads$Season_Spring<- unlist(Map(function(x) ifelse(x %in% c(9,10,11),1,0), month(master_ads$WeekEndNumber)))



#-----------------------------------------        Start Models   --------------------------------------###

library(lme4)



#Set columns required for model
working_ADS<-master_ads %>% select(SP_Key,
                                   log_volume,
                                   log_price,
                                   DisplayPercentage,
                                   End_Potential_Medium,End_Potential_High,End_Potential_Low,
                                   Trend_Value,
                                   Season_Winter,Season_Spring,Season_Summer,Season_Autumn,
                                   STOCKUP_FLAG,W1,W2,W3,W4,
                                   Inter_C1_Price,
                                   Inter_C2_Price,
                                   Inter_C3_Price,
                                   Inter_C4_Price,
                                   Intra_C1_Price,
                                   Intra_C2_Price,
                                   Intra_C3_Price,
                                   Intra_C4_Price)


#Create trains and test splits
train_working_df<-data.frame()
test_working_df<-data.frame()

list_of_sp_keys<-unique(working_ADS$SP_Key)

set.seed(123)   #  set seed to ensure you always have same random numbers generated


for (key in list_of_sp_keys){
  print(paste("Current key is ", key))
  curr_df<- working_ADS %>% filter(SP_Key==key) %>% arrange(Trend_Value)
  sample = sample.split(curr_df,SplitRatio = 0.75) 
  train_curr_df<-subset(curr_df,sample ==TRUE)
  test_curr_df<-subset(curr_df, sample==FALSE)
  if(nrow(train_curr_df)>40){
    train_working_df<-rbind(train_working_df,train_curr_df)
    test_working_df<-rbind(test_working_df,test_curr_df)}
}

#Remove NA
train_working_df <- train_working_df[complete.cases(train_working_df),]
test_working_df <- test_working_df[complete.cases(test_working_df),]


#Remove duplicate rows
train_working_df <- unique(train_working_df)
test_working_df<-unique(test_working_df)

# 
# write.csv(train_working_df,"MAPECHECKADS_Train.csv",row.names = F)
# write.csv(test_working_df,"MAPECHECKADS_Test.csv",row.names = F)



#Train model on training data

by_store_product <- train_working_df %>%group_by(SP_Key)



coefficient <-do(by_store_product,tidy(step(lm(log_volume ~ log_price + DisplayPercentage +
                                                 Trend_Value + Season_Spring + Season_Summer+ Season_Autumn + 
                                                 End_Potential_Medium+End_Potential_High+End_Potential_Low+
                                                 W1+W2+W3+W4+
                                                 log(Inter_C1_Price)+
                                                 log(Inter_C2_Price)+
                                                 log(Inter_C3_Price)+
                                                 log(Inter_C4_Price)+
                                                 log(Intra_C1_Price)+
                                                 log(Intra_C2_Price)+
                                                 log(Intra_C3_Price)+
                                                 log(Intra_C4_Price)
                                               , data=.))))
                                              # , method = "glmnet"
                                              # , tuneGrid = grid
                                              # , trControl = control 
                                              # , standardize = TRUE
                                              # , maxit= 10000000))



rsquared <-do(by_store_product,glance(step(lm(log_volume ~ log_price + DisplayPercentage +
                                                Trend_Value + Season_Spring + Season_Summer+ Season_Autumn +
                                                End_Potential_Medium+End_Potential_High+End_Potential_Low+
                                                W1+W2+W3+W4+
                                                log(Inter_C1_Price)+
                                                log(Inter_C2_Price)+
                                                log(Inter_C3_Price)+
                                                log(Inter_C4_Price)+
                                                log(Intra_C1_Price)+
                                                log(Intra_C2_Price)+
                                                log(Intra_C3_Price)+
                                                log(Intra_C4_Price)
                                              , data=.))))


#Prepare coeffecients and R squared and write results to CSV file 
coefficient$std.error<-NULL
coefficient$statistic<-NULL
coefficient$p.value<-NULL
rsquared<-rsquared[,1:2]
ModeL_Results<-cast(coefficient, SP_Key ~ term)
Final_results<-left_join(ModeL_Results,rsquared)
Final_results<-left_join(Final_results,sp_cat)
Final_results<-left_join(Final_results,sp_packtype)
Final_results<-left_join(Final_results,sp_weight)


#write_xlsx(Final_results, "Model1_Season_endpotential.xlsx")

results_modded<-Final_results 


###############------------------------------------Model Results Change ------------------------------------##############################3



library(splitstackshape)


#Add SP keys for which model dropped values
results_full<-results_modded
results_split<-cSplit(results_full, "SP_Key", "*")

names(results_split)[names(results_split) == 'SP_Key_1'] <- 'State'
names(results_split)[names(results_split) == 'SP_Key_2'] <- 'Product'


results_split$SP_Key<-paste(results_split$State,results_split$Product,sep="*")
list_of_SP_Keys_limited<-unique(results_split$SP_Key)

results_split$State<-as.character(results_split$State)
results_split$Product<-as.character(results_split$Product)


#Beta treatment by columns
results_split$End_Potential_High[results_split$End_Potential_High<0]<-NA
results_split$End_Potential_Low[results_split$End_Potential_Low<0]<-NA
results_split$End_Potential_Medium[results_split$End_Potential_Medium<0]<-NA

results_split$`log(Inter_C1_Price)`[results_split$`log(Inter_C1_Price)`<0]<-NA
results_split$`log(Inter_C2_Price)`[results_split$`log(Inter_C2_Price)`<0]<-NA
results_split$`log(Inter_C3_Price)`[results_split$`log(Inter_C3_Price)`<0]<-NA
results_split$`log(Inter_C4_Price)`[results_split$`log(Inter_C4_Price)`<0]<-NA

results_split$`log(Intra_C1_Price)`[results_split$`log(Intra_C1_Price)`<0]<-NA
results_split$`log(Intra_C2_Price)`[results_split$`log(Intra_C2_Price)`<0]<-NA
results_split$`log(Intra_C3_Price)`[results_split$`log(Intra_C3_Price)`<0]<-NA
results_split$`log(Intra_C4_Price)`[results_split$`log(Intra_C4_Price)`<0]<-NA


results_split$`log(Inter_C1_Price)`[results_split$`log(Inter_C1_Price)`>(-0.1*results_split$log_price)]<-NA
results_split$`log(Inter_C2_Price)`[results_split$`log(Inter_C2_Price)`>(-0.1*results_split$log_price)]<-NA
results_split$`log(Inter_C3_Price)`[results_split$`log(Inter_C3_Price)`>(-0.1*results_split$log_price)]<-NA
results_split$`log(Inter_C4_Price)`[results_split$`log(Inter_C4_Price)`>(-0.1*results_split$log_price)]<-NA

results_split$`log(Intra_C1_Price)`[results_split$`log(Intra_C1_Price)`>(-0.1*results_split$log_price)]<-NA
results_split$`log(Intra_C2_Price)`[results_split$`log(Intra_C2_Price)`>(-0.1*results_split$log_price)]<-NA
results_split$`log(Intra_C3_Price)`[results_split$`log(Intra_C3_Price)`>(-0.1*results_split$log_price)]<-NA
results_split$`log(Intra_C4_Price)`[results_split$`log(Intra_C4_Price)`>(-0.1*results_split$log_price)]<-NA







results_split$W1[results_split$W1>0]<-NA
results_split$W2[results_split$W2>0]<-NA
# results_split$W3[results_split$W3>0]<-NA
# results_split$W4[results_split$W4>0]<-NA

#Price, trend and season flags are intuitive







#Create DFs for coefficients at grouped levels
grp_prod<-results_split %>% select(-State,-Pack.Type,-SP_Key)%>% group_by(Product) %>% summarise_all(funs(mean), na.rm = TRUE)
grp_prod[is.na(grp_prod)]<-NA


grp_packtype<-results_split %>% select(-State,-Product,-SP_Key)%>% group_by(Pack.Type) %>% summarise_all(funs(mean), na.rm = TRUE)
grp_packtype[is.na(grp_packtype)]<-NA


grp_subcat<-results_split %>% select(-State,-Product,-SP_Key)%>% group_by(Sub.Category) %>% summarise_all(funs(mean), na.rm = TRUE)
grp_subcat[is.na(grp_subcat)]<-NA



#Create list of keys 
list_of_products<-unique(grp_prod$Product)
list_of_packs<-unique(grp_packtype$Pack.Type)

#Create empty DF to store model coefficeints
full_model_mod <- results_split[0,]


library(lazyeval)

for (key in list_of_SP_Keys_limited){
  print(key)
  
  
  curr_coef_line<-results_split %>% filter(SP_Key==key)
  
  product_name<-curr_coef_line$Product
  pack_name<-curr_coef_line$Pack.Type
  subcategory_name<-curr_coef_line$Sub.Category
  
  
  
  for(col_no in 2:22){
    print(col_no)
    column_name<-colnames(curr_coef_line)[col_no]
    mean_state<-grp_prod[[column_name]][grp_prod$Product==product_name]
    mean_pack<-grp_packtype[[column_name]][grp_packtype$Pack.Type==pack_name]
    mean_category<-grp_subcat[[column_name]][grp_subcat$Sub.Category==subcategory_name]
    x<-c(as.numeric(mean_state),as.numeric(mean_pack),as.numeric(mean_category))
    overall_mean<-mean(x,na.rm=TRUE)
    if(is.na(curr_coef_line[,col_no])){
      if (is.na(mean_state)==FALSE) {curr_coef_line[,col_no]<-mean_state 
      } else if (is.na(mean_pack)==FALSE) {curr_coef_line[,col_no]<-mean_pack 
      } else if (is.na(mean_category)==FALSE) {curr_coef_line[,col_no]<-mean_category 
      } else curr_coef_line[,col_no]<-overall_mean 
      
    }
    
  }
  
  full_model_mod<-rbind(full_model_mod,curr_coef_line)
  
} 
  




is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

full_model_mod[is.nan(full_model_mod)] <- NA



#-----------------------------------------         IQR and capping ---------------------------------------------------``#####################
model_iqr<-as.data.frame(full_model_mod)

for(column_number in 6:9){
  print(column_number)
  max=quantile(model_iqr[,column_number], na.rm = TRUE)[[4]]
  min=quantile(model_iqr[,column_number], na.rm = TRUE)[[2]]
  print(min)
  print(max)
  
  model_iqr[,column_number][model_iqr[,column_number]>max]<-max
  model_iqr[,column_number][model_iqr[,column_number]<min]<-min
  
}


# for(i in 19:22){
#   model_iqr[is.na(model_iqr[,i]), i] <- mean(model_iqr[,i], na.rm = TRUE)
# }
# 



nacount<-data.frame(sapply(model_iqr, function(x) sum(is.na(x))))

model_iqr[is.na(model_iqr)]<-0








#------------------------------------------------      MApe for each SP_key  --------------------------------------------
MAPE_df<-data.frame(matrix(nrow=length(list_of_sp_keys),ncol = 2,data = 0))
MAPE_df$SP_key<-list_of_sp_keys
MAPE_df <- MAPE_df[,3:1]
colnames(MAPE_df)<-c("SP_Key","MAPE_Value_Log","MAPE_Value_Actuals")


#Create test df with predictions
Test_predictions<-data.frame()


#Predict on test data and calculate MAPE
for(key in list_of_sp_keys){
  print(paste("Current key is ", key))
  # model_curr_res<-ModeL_Results %>% filter(SP_Key==key)
  model_curr_res<-model_iqr %>% filter(SP_Key==key)
  model_curr_res[is.na(model_curr_res)]<-0
  curr_df<- test_working_df %>% filter(SP_Key==key) %>% arrange(Trend_Value)
  curr_df<-unique(curr_df)

  #Calculations on log volume
  curr_df$Pred_log_vol<-model_curr_res$`(Intercept)` +
    model_curr_res$DisplayPercentage*curr_df$DisplayPercentage +
    model_curr_res$End_Potential_High*curr_df$End_Potential_High +
    model_curr_res$End_Potential_Low*curr_df$End_Potential_Low +
    model_curr_res$End_Potential_Medium*curr_df$End_Potential_Medium +
    model_curr_res$Season_Autumn*curr_df$Season_Autumn +
    model_curr_res$Season_Spring*curr_df$Season_Spring +
    model_curr_res$Season_Summer*curr_df$Season_Summer +
    model_curr_res$`log(Inter_C1_Price)`*log(curr_df$Inter_C1_Price) +
    model_curr_res$`log(Inter_C2_Price)`*log(curr_df$Inter_C2_Price) +
    model_curr_res$`log(Inter_C3_Price)`*log(curr_df$Inter_C3_Price) +
    model_curr_res$`log(Inter_C4_Price)`*log(curr_df$Inter_C4_Price) +
    model_curr_res$`log(Intra_C1_Price)`*log(curr_df$Intra_C1_Price) +
    model_curr_res$`log(Intra_C2_Price)`*log(curr_df$Intra_C2_Price) +
    model_curr_res$`log(Intra_C4_Price)`*log(curr_df$Intra_C4_Price) +
    model_curr_res$log_price*curr_df$log_price +
    model_curr_res$Trend_Value*curr_df$Trend_Value +
    model_curr_res$W1*curr_df$W1 +
    model_curr_res$W2*curr_df$W2 +
    model_curr_res$W3*curr_df$W3 +
    model_curr_res$W4*curr_df$W4

  curr_df$APE_Log_Vol<-abs((((curr_df$Pred_log_vol)-(curr_df$log_volume))/curr_df$log_volume)*100)
  curr_MAPE_Log_Vol<-mean(curr_df$APE_Log_Vol)


  #Calculations on Actual volume
  curr_df$Sales_Volume_Act<-exp(curr_df$log_volume)
  curr_df$Sales_Volume_Pred<-exp(curr_df$Pred_log_vol)

  curr_df$APE_Vol<-abs((((curr_df$Sales_Volume_Pred)-(curr_df$Sales_Volume_Act))/curr_df$Sales_Volume_Act)*100)
  curr_MAPE_Vol<-mean(curr_df$APE_Vol)
    
  
  

  MAPE_df$MAPE_Value_Log[MAPE_df$SP_Key == key] <- round(curr_MAPE_Log_Vol,digits = 2)
  MAPE_df$MAPE_Value_Actuals[MAPE_df$SP_Key == key] <-round(curr_MAPE_Vol,digits = 2)
  Test_predictions<-rbind(Test_predictions,curr_df)
}

Output_Model<-left_join(model_iqr,MAPE_df)

# #Merge with mode coefficients and R squared df
# Output_Model<-left_join(Final_results,MAPE_df)


write_xlsx(Output_Model, "Model1_Season_endpotential.xlsx")


write.csv(Test_predictions, "Model1_predictions.csv", row.names = F)


########## Code for GLMNET ##################


grid<-expand.grid(.alpha=seq(0,1,by=.5),.lambda=seq(0,0.2,by=.1))
control<-trainControl(method = "LOOCV")
by_store_product <- train_working_df %>%group_by(SP_Key)

for (keys in SP_key) {
    
    currentTrainData <- train_working_df %<% filter(SP_Key == key)
    trainModel <-train(log_volume ~ log_price + DisplayPercentage +
                                                 Trend_Value + Season_Spring + Season_Summer+ Season_Autumn + 
                                                 End_Potential_Medium+End_Potential_High+End_Potential_Low+
                                                 W1+W2+W3+W4+
                                                 log(Inter_C1_Price)+
                                                 log(Inter_C2_Price)+
                                                 log(Inter_C3_Price)+
                                                 log(Inter_C4_Price)+
                                                 log(Intra_C1_Price)+
                                                 log(Intra_C2_Price)+
                                                 log(Intra_C3_Price)+
                                                 log(Intra_C4_Price)
                                               , data= currentTrainData
                                               , method = "glmnet"
                                               , tuneGrid = grid
                                               , trControl = control 
                                               , standardize = TRUE
                                               , maxit= 10000000)}
  
  coef


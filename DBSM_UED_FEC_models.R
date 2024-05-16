library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(randomForest)
library(ggplot2)

## set directory
setwd("C:/Users/--/")

data1 = read.table("--/hotmaps_all.txt", header=TRUE)
data1 = subset(data1,subsector!="total")


## Run iteratively to fit reduced models
## control variables 
datax = data1[,-c(16)] #fec or ued,
#datax = datax[,-c(3)] #out subsector
#datax = datax[,c("fec","country","sector","age","U_wall","U_win","U_roof","U_floor")] # only country, sector, age and physical variables
#datax = datax[,c("fec","country","sector","age")] # only sector and age
#datax = datax[,c("fec","country","sector")] # only sector 


error_t = 1e9

set.seed(123)
smp_size <- floor(0.75 * nrow(datax))
train_ind <- sample(seq_len(nrow(datax)), size = smp_size)

train <- datax[train_ind, ]
test <- datax[-train_ind, ]


for (ss in seq(0,3000,by=500)){
  set.seed(ss)
  for (i in 2:15){
    for(j in seq(5,100,by=5)){
      ## replace ued or fec
      rf_model = randomForest(fec ~ ., data=train, ntree=j, mtry=i, importance=TRUE)
      err = mean(rf_model$mse)
      pred_train = predict(rf_model,train)
      pred_test = predict(rf_model,test)
      mape_train = mean(abs(train$fec-pred_train)/train$fec)
      mse_test = sum((test$fec-pred_test)^2)/dim(test)[1]
      mape_test = mean(abs(test$fec-pred_test)/test$fec)
      if(mape_test < error_t){
        res_rf = matrix(c(paste("rf-leaf-",i,"-trees-",j,"-seed-",ss),mape_test,mse_test,mape_train,err),1,5)
        best = rf_model
        error_t = mape_test
        if(ss==0 & i==2){res_all=res_rf}else{res_all = rbind(res_all,res_rf)}
      }
      
    }
  }
}

(bb = apply(res_all[,-1], 2, which.min)[1])
(rr = res_all[bb,])

## replace according to the best model
best_mtry = 7
best_ntree = 45
best_seed = 0

## confirm
for (ss in seq(0,3000,by=500)){
  set.seed(ss)
  smp_size <- floor(0.75 * nrow(datax))
  train_ind <- sample(seq_len(nrow(datax)), size = smp_size)
  train <- datax[train_ind, ]
  test <- datax[-train_ind, ]
  ss2=best_seed
  set.seed(ss2)
  
  ## replace ued or fec
  rf_model = randomForest(fec ~ ., data=train, ntree=best_ntree, mtry=best_mtry, importance=TRUE)
  err = mean(rf_model$mse)
  pred_train = predict(rf_model,train)
  pred_test = predict(rf_model,test)
  mape_train = mean(abs(train$fec-pred_train)/train$fec)
  mse_test = sum((test$fec-pred_test)^2)/dim(test)[1]
  mape_test = mean(abs(test$fec-pred_test)/test$fec)
  res_rf_v = matrix(c(paste("rf-leaf-",i,"-trees-",j,"-seed-",ss2,"-train-test_seed-",ss),mape_test,mse_test,mape_train,err),1,5)
  if(ss==0){res_all_v=res_rf_v}else{res_all_v = rbind(res_all_v,res_rf_v)}
  
}

## save the model
save(best,file = "--/fec_hotmaps_all.RData") 
## save the error/model history
write.table(res_rf, file = "--/best_model_fec_err_all.txt", sep= "\t",row.names = FALSE, col.names = TRUE)
write.table(res_all, file = "--/best_models_fec_err_all.txt", sep = "\t",row.names = FALSE, col.names = TRUE)
write.table(res_all_v, file = "--/confirmation_best_model_fec_all.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## use the model
modelx = get(load("--/fec_hotmaps_all.RData"))

## confirm (2)
## replace ued or fec
set.seed(123)
train_ind <- sample(seq_len(nrow(datax)), size = smp_size)
train <- datax[train_ind, ]
test <- datax[-train_ind, ]
pred_train = predict(modelx,train)
pred = predict(modelx,test)
mean(abs(train$fec-pred_train)/train$fec)
mean(abs(test$fec-pred)/test$fec)

## Figures
vi <- as.data.frame(cbind(rownames(modelx$importance),round(modelx$importance[,"%IncMSE"],6)*0.01))
colnames(vi) <- c("Features","%MSE Increase")
vi$`%MSE Increase` <- as.numeric(as.character(vi$`%MSE Increase`))
vi = vi[with(vi, order(vi$`%MSE Increase`)), ]
vi = subset(vi,`%MSE Increase`>0)
vi$Features = factor(vi$Features, levels = vi$Features)
tiff("--/fec_all.jpeg", units="cm", width=20, height=12, res=2000)
ggplot(vi) + geom_bar(aes(`%MSE Increase`,Features), stat="identity",fill = "steelblue")+
  theme_classic() 
dev.off()


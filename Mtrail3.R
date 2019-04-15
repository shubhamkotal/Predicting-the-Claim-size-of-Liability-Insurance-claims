library(DMwR)
library(dplyr)
library(glmnet)
library(caret)
#clear environment
rm(list = ls(all=TRUE))

#set working directory
setwd("D:\\mithexam49")

#read csv files
train_data1=read.csv("Train_ClaimDetails-1542969243754.csv",header = TRUE,na.strings = c("?",","," ","","0","_","-0", "-"))
test_data1=read.csv("Test_ClaimDetails-1542969243754.csv",header = TRUE,na.strings = c("?",","," ","","0","_","-0", "-"))
train_label=read.csv("Train-1542969243754.csv",header = TRUE,na.strings = c("?",","," ","","0","_","-0", "-"))
test_label=read.csv("predictions-1542969243754.csv",header = TRUE,na.strings = c("?",","," ","","0","_","-0", "-"))
train_policy=read.csv("Train_Policy_Demographics-1542969243754.csv",header = TRUE,na.strings = c("?",","," ","","0","_","-0", "-"))
test_policy=read.csv("Test_Policy_Demographics-1542969243754.csv",header = TRUE,na.strings = c("?",","," ","","0","_","-0", "-"))

sample_data=read.csv("predictions-1542969243754.csv")

#check structure
str(train_data1)
str(train_label)
str(test_policy)
#merge trian and label
train_data2 = merge(train_data1,train_label,by.x = 'ClaimID',by.y = 'ClaimID',all = TRUE)
dim(train_data2)

#merge test and label
#remove size
str(test_data1)
test_label$ClaimSize=NULL
test_data2 = merge(test_data1,test_label,by.x = 'ClaimID',by.y = 'ClaimID',all = TRUE)

#merge data2 and train policy
train_data3 = merge(train_data2,train_policy,by.x = 'ClaimID',by.y = 'ClaimID',all = TRUE)
test_data3 = merge(test_data2,test_policy,by.x = 'ClaimID',by.y = 'ClaimID',all = TRUE)

## train data3 and test data3 are final data sets after doing merge of datasets

#check structure and dimension
str(train_data3)
str(test_data3)
dim(train_data3)
dim(test_data3)

# checking for missing values
sum(is.na(train_data3))
sum(is.na(test_data3))

#removing cols which has more than 50% na
library(DMwR)
length(manyNAs(train_data3, 0.5))
names(train_data3[colSums(is.na(train_data3))>(0.50*(dim(train_data3)[1]))])

#drop the columns with more than 50% of NAs for "train data"
drop.cols<-names(train_data3[colSums(is.na(train_data3))>(0.50*(dim(train_data3)[1]))])
train_data4<-train_data3[!colnames(train_data3)%in%drop.cols]

# check na
sum(is.na(train_data4))
#check col wise
sapply(train_data4,function(x)sum(is.na(x)))
# imputation for numeric
train_data5=centralImputation(train_data4)
sum(is.na(train_data5))

#drop the columns with more than 50% of NAs for "test data"
drop.cols<-names(test_data3[colSums(is.na(test_data3))>(0.50*(dim(test_data3)[1]))])
test_data4<-test_data3[!colnames(test_data3)%in%drop.cols]

# check na
sum(is.na(test_data4))
#check col wise
sapply(test_data4,function(x)sum(is.na(x)))
#imputation for numeric
test_data5=centralImputation(test_data4)
sum(is.na(test_data5))

## train_data5 & test_data5 are final data set after removings NA values


##check str 
str(train_data5)
str(test_data5)

# change  class of variable to appropiate type

factor_cols=c("Anyothercontributors","PolicyType","PolicyForm")

train_data5[,factor_cols]=data.frame(sapply(train_data5[,factor_cols],as.character))
train_data5[,factor_cols]=data.frame(sapply(train_data5[,factor_cols],as.factor))

test_data5[,factor_cols]=data.frame(sapply(test_data5[,factor_cols],as.character))
test_data5[,factor_cols]=data.frame(sapply(test_data5[,factor_cols],as.factor))

str(train_data5)
str(test_data5)

#remove un necessary variables
train_data5$ClaimID=NULL
train_data5$OtherMotorVehicle=NULL
train_data5$PolicyID=NULL

test_data5$ClaimID=NULL
test_data5$OtherMotorVehicle=NULL
test_data5$PolicyID=NULL

### check frequency for Date_reported and issued
train_data5 %>% select(Date_reported) %>% group_by(Date_reported) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% filter(count>140) %>% 
  select(Date_reported) %>% na.omit() -> orglist
str(train_data5$Date_reported)

orglist <- as.character(orglist$Date_reported)
str(train_data5$Date_reported)

temp1 <- as.character(train_data5$Date_reported)
temp1 <- ifelse(temp1 %in% orglist,temp1,ifelse(is.na(temp1),NA,'Other'))
train_data5$Funder <- factor(temp1)
str(train_data5$Date_reported)

temp2 <- as.character(test_data5$Date_reported)
temp2 <- ifelse(temp2 %in% orglist,temp2,ifelse(is.na(temp2),NA,'Other'))
test_data5$Funder <- factor(temp2, levels = levels(train_data5$Funder))

##hence have 1level most time so will remove
train_data5$Date_reported=NULL
train_data5$Injury_Date=NULL
test_data5$Date_reported=NULL
test_data5$Injury_Date=NULL

#remove new created variable has only 1level
train_data5$Funder=NULL
test_data5$Funder=NULL

#split data into two sets of train_data5

split=createDataPartition(train_data5$ClaimSize,p=0.7,list = FALSE)
train1=train_data5[split,]
val1=train_data5[-split,]

str(train1)



#standarization
std_method=preProcess(train1[,setdiff(names(train1),c("Work_related_injury_status","SystemicPoisoning_other","Falls",
                                                      "Non_economicloss","Exemplarydamages","WhetherPrimaFacie_JointandSeveralLiability",
                                                      "WorkersCompAvailability","CollateralSourcesAvailability",
                                                      "Anyothercontributors","Match_Multiclaimant_multiInterestedparties_claim",
                                                      "ClaimSize","PolicyType","PolicyForm",
                                                      "Employment_status"))],method=c("center","scale"))

#predict on train and val
train2=predict(std_method,train1)
val2=predict(std_method,val1)
test2=predict(std_method,test_data5)
#remove unnecessary variable
train2$SystemicPoisoning_other=NULL
train2$Falls=NULL
val2$SystemicPoisoning_other=NULL
val2$Falls=NULL
test2$SystemicPoisoning_other=NULL
test2$Falls=NULL
train2$Match_Multiclaimant_multiInterestedparties_claim=NULL
val2$Match_Multiclaimant_multiInterestedparties_claim=NULL
test2$Match_Multiclaimant_multiInterestedparties_claim=NULL

#check correlation
str(train2)
library(corrplot)
corrdata <- train2[,c('BusinessClass','Age_Injured','PrimaFacie_percentagefault_insured','CombinedSingleLimit',
                      'PolicyLimitPerInjury')]
corrdata <- corrdata
str(corrdata)
corr_mat <- cor(corrdata)
corrplot(corr_mat, method = "circle")

library(randomForest)
#model1
model1 = randomForest(ClaimSize ~ . , train2,ntree =500,mtry = 16,na.action = na.omit)
plot(model1)
importance(model1)
varImpPlot(model1)

preds_train1 = predict(model1,train2,type = "class") 
preds_val1 = predict(model1, val2,type = "class")

#build confusion matrix
confusionMatrix(preds_train1, train2$ClaimSize)
confusionMatrix(preds_val1, val2$ClaimSize)

#model2 (after smotting)
set.seed(232)
trainsmote<-SMOTE(ClaimSize~.,data=train2,perc.over = 400,perc.under = 200 )
table(trainsmote$ClaimSize)
prop.table(table(trainsmote$ClaimSize))

set.seed(232)
testsmote<-SMOTE(ClaimSize~.,data=val2,perc.over = 400,perc.under = 200 )
table(testsmote$ClaimSize)
prop.table(table(testsmote$ClaimSize))

#model
model2 = randomForest(ClaimSize ~ . , trainsmote,ntree =500,mtry = 16,na.action = na.omit)
plot(model2)
importance(model2)
varImpPlot(model2)

preds_train2 = predict(model2,trainsmote,type = "class") 
preds_val2 = predict(model2, testsmote,type = "class")
# Build Confusion matrix
confusionMatrix(preds_train2, trainsmote$ClaimSize)
confusionMatrix(preds_val2, testsmote$ClaimSize)
###save
preds_test_DT2<-predict(model2,test2,type="class")
sample_data$ClaimSize=preds_test_DT2
write.csv(sample_data,"finaltrail1.csv",row.names = FALSE)

#model3
#model3 (decision tree)
library(rpart)
model_DT1 <- rpart(ClaimSize ~ . , trainsmote,na.action=na.omit)

preds_train_dt <- predict(model_DT1,trainsmote,type = "class")
preds_val_DT <- predict(model_DT1,testsmote,type="class")

#Error Metrics on train and val
confusionMatrix(preds_train_dt, trainsmote$ClaimSize)

confusionMatrix(preds_val_DT, testsmote$ClaimSize)

#dummification
library(dummies)
dummies = dummyVars(ClaimSize~.,data = train2)
train_dum = data.frame(predict(dummies, train2))
val_dum = data.frame(predict(dummies, val2))

train_dum$ClaimSize = train2$ClaimSize
names(train_dum)

val_dum$ClaimSize = val2$ClaimSize
names(val_dum)

#model4 after dummification
model_dum <- rpart(ClaimSize ~ . , train_dum)

preds_train_dum <- predict(model_dum,train_dum,type = "class")
preds_val_dum <- predict(model_dum,val_dum,type="class")

#Error Metrics on train and val
confusionMatrix(preds_train_dum, train_dum$ClaimSize)
confusionMatrix(preds_val_dum, val_dum$ClaimSize)

#model5
##---------------------------------------------------------Knn model-----------------------------------------------

#using knn3
library(caret)
model_knn <- knn3(ClaimSize ~ . , train2, k = 5)


preds_knn_train <- predict(model_knn, train2,type="class")
preds_knn_val <- predict(model_knn, val2,type="class")
preds_knn_test<-predict(model_knn,test2,type = "class")
# Build Confusion matrix
confusionMatrix(preds_knn_train, train2$ClaimSize)
confusionMatrix(preds_knn_val, val2$ClaimSize)

###save
preds_test_DT2<-predict(model_knn,test2,type="class")
sample_data$ClaimSize=preds_test_DT2
write.csv(sample_data,"finalknn.csv",row.names = FALSE)

###model6
# Building the model on train data
library(e1071)

dumvar=dummyVars(ClaimSize~.,data=train2)
dummies=dummyVars(~.,data=test2)

train_target=train2$ClaimSize
val_target=val2$ClaimSize
train=predict(dumvar,train2)
val=predict(dumvar,val2)

x.train=predict(dumvar, newdata = train2)
y.train=train2$ClaimSize
x.val = predict(dumvar, newdata = val2)
y.val = val2$ClaimSize
x.test=predict(dummies,newdata=test2)


model_svm  =  svm(x = x.train, y =y.train, type = "C-classification", kernel = "linear", cost = 10)
summary(model)



# Predict on train and test using the model
pred_train_svm  =  predict(model_svm, x.train) # x is all the input variables
pred_val_svm=predict(model_svm,x.val)

pred_test_svm=predict(model_svm,x.test)


# Build Confusion matrix

confusionMatrix(pred_train_svm,y.train)
confusionMatrix(pred_val_svm,y.val)


############# impute by mode
#for train
val <- unique(train2[!is.na(train2)])             
mode <- val[which.max(tabulate(match(train2, val)))] 

vec_imp <- train2                                   
vec_imp[is.na(vec_imp)] <- mode  

dim(train2)
View(vec_imp)

#for val
val1 <- unique(val2[!is.na(val2)])             
mode <- val1[which.max(tabulate(match(val2, val1)))] 

vec_imp1 <- val2                                   
vec_imp1[is.na(vec_imp1)] <- mode  

dim(train2)
View(vec_imp1)

#for test
val2 <- unique(test2[!is.na(test2)])             
mode <- val2[which.max(tabulate(match(test2, val1)))] 

vec_imp2 <- test2                                   
vec_imp2[is.na(vec_imp2)] <- mode  

dim(train2)
View(vec_imp2)

#model after imputation by mode
#model1
library(randomForest)
modelL = randomForest(ClaimSize ~ . , vec_imp,ntree =500,mtry = 16,na.action = na.omit)
plot(modelL)
importance(modelL)
varImpPlot(modelL)

preds_trainl = predict(modelL,vec_imp,type = "class") 
preds_vall = predict(modelL, vec_imp1,type = "class")

# Build Confusion matrix
confusionMatrix(preds_trainl, vec_imp$ClaimSize)
confusionMatrix(preds_vall, vec_imp1$ClaimSize)

###save
preds_test_DT2<-predict(modelL,vec_imp2,type="class")
sample_data$ClaimSize=preds_test_DT2
write.csv(sample_data,"finalbymode.csv",row.names = FALSE)


#modellast (after smotting)
set.seed(232)
trainsmote1<-SMOTE(ClaimSize~.,data=vec_imp,perc.over = 400,perc.under = 200 )
table(trainsmote1$ClaimSize)
prop.table(table(trainsmote1$ClaimSize))

set.seed(232)
testsmote1<-SMOTE(ClaimSize~.,data=vec_imp1,perc.over = 400,perc.under = 200 )
table(testsmote1$ClaimSize)
prop.table(table(testsmote1$ClaimSize))


modell2 = randomForest(ClaimSize ~ . , trainsmote1,ntree =500,mtry = 16,na.action = na.omit)
plot(modell2)
importance(modell2)
varImpPlot(modell2)

preds_trainl2 = predict(modell2,trainsmote1,type = "class") 
preds_vall2 = predict(modell2, testsmote1,type = "class")

# Build Confusion matrix
confusionMatrix(preds_trainl2, trainsmote1$ClaimSize)
confusionMatrix(preds_vall2, testsmote1$ClaimSize)

###save
preds_test_DT2<-predict(modell2,vec_imp2,type="class")
sample_data$ClaimSize=preds_test_DT2
write.csv(sample_data,"finalbymodesmot.csv",row.names = FALSE)

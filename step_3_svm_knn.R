getwd()
setwd("C:/Users/Admin/Desktop/data_prep/project")

library(reshape)
library(ggplot2)
install.packages("ROSE")
library(ROSE)
library(reshape2)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("corrplot")
library(corrplot)
library(MASS)
library(dplyr)
library(caret)
library(fastDummies)
install.packages("randomForest")
library(randomForest)

credit<-read.csv("UCI_Credit_Card.csv")
str(credit)

credit$SEX<-as.factor(credit$SEX)
credit$EDUCATION<-as.factor(credit$EDUCATION)
credit$MARRIAGE<-as.factor(credit$MARRIAGE)
credit$PAY_0<-as.factor(credit$PAY_0)
credit$PAY_2<-as.factor(credit$PAY_2)
credit$PAY_3<-as.factor(credit$PAY_3)
credit$PAY_4<-as.factor(credit$PAY_4)
credit$PAY_5<-as.factor(credit$PAY_5)
credit$PAY_6<-as.factor(credit$PAY_6)
credit$default.payment.next.month<-as.factor(credit$default.payment.next.month)

str(credit)
summary(credit)
dim(credit)

sapply(credit,function(count) sum(is.na(count)))

credit_newfact<-credit[c(2,6,13:24)]

credit_new<-credit[c(2,3,4,5,6,7:12,13,15,17,19:25)]
dim(credit_new)
str(credit_new)

table(credit_new$default.payment.next.month)

set.seed(300)
indxTrain <- createDataPartition(y = credit_new$default.payment.next.month,p = 0.60,list = FALSE)
training <- credit_new[indxTrain,]
testing <- credit_new[-indxTrain,]

prop.table(table(training$default.payment.next.month)) * 100
prop.table(table(testing$default.payment.next.month)) * 100
prop.table(table(credit_new$default.payment.next.month)) * 100


trainX <- training[,names(training) != "Direction"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues


set.seed(400)
#ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)




#data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data

train.rose<-ROSE(default.payment.next.month~.,data=training,seed=1)$data
table(train.rose$default.payment.next.month)

test.rose<-ROSE(default.payment.next.month~.,data=testing,seed=1)$data
table(test.rose$default.payment.next.month)

#linear graph
lm(credit$BILL_AMT3~credit$BILL_AMT4)
ggplot(credit)+geom_point(aes(x=credit$BILL_AMT3, y=credit$BILL_AMT4),color="red")+
  geom_abline(intercept = 3922.457, slope = 0.996, size=1, color="blue", linetype="dashed")+
  labs(x = 'Amount of bill statement in April, 2005 (NT dollar)
       ') +
  labs(y = 'Amount of bill statement in May, 2005 (NT dollar)
       ')


knnFit <- train(default.payment.next.month ~ ., data = train.rose, method = "knn", preProcess = c("center","scale"))
plot(knnFit)
knnFit

knnPredict <- predict(knnFit,newdata = test.rose)
confusionMatrix(knnPredict, test.rose$default.payment.next.month )


#randomforest

rf_mod=randomForest(default.payment.next.month ~ . , data = train.rose)
rf_mod
plot(rf_mod)
rf_pred <- predict(rf_mod,newdata = test.rose)
head(rf_pred)

confusionMatrix(rf_pred, test.rose$default.payment.next.month )

#ROC

library(ROCR)

#p_test_naive<-prediction(rf_pred, test.rose$default.payment.next.month)
pred <- prediction(as.numeric(rf_pred), as.numeric(test.rose$default.payment.next.month))
perf_naive<-performance(pred, "tpr", "fpr")
perf_naive
plot(perf_naive, colorize=T)
performance((perf_naive))

#outlier
boxplot(credit_new$BILL_AMT1)
boxplot(credit_new$BILL_AMT1)$out

outliers_1 <- boxplot(credit_new$BILL_AMT1)$out

credit_new[which(credit_new$BILL_AMT1 %in% outliers_1),]

crc <- credit_new[-which(credit_new$BILL_AMT1 %in% outliers_1),]

boxplot(crc$BILL_AMT1)$out

dim(crc)

outliers_2 <- boxplot(crc$BILL_AMT3)$out
crc_2 <- crc[-which(crc$BILL_AMT3 %in% outliers_2),]
boxplot(crc_2$BILL_AMT3)$out
dim(crc_2)

outliers_3 <- boxplot(crc_2$BILL_AMT5)$out
crc_3 <- crc_2[-which(crc_2$BILL_AMT5 %in% outliers_3),]
boxplot(crc_3$BILL_AMT5)$out
dim(crc_3)

outliers_4 <- boxplot(crc_3$PAY_AMT1)$out
crc_4 <- crc_3[-which(crc_3$PAY_AMT1 %in% outliers_4),]
boxplot(crc_4$PAY_AMT1)$out
dim(crc_4)

outliers_5 <- boxplot(crc_4$PAY_AMT2)$out
crc_5 <- crc_4[-which(crc_4$PAY_AMT2 %in% outliers_5),]
boxplot(crc_5$PAY_AMT2)$out
dim(crc_5)

#PCA
library(factoextra)
data_pca <- transform(credit_new[c(1,5,12:20)])
str(data_pca)
data_pca[,-1]

all_pca <- prcomp(data_pca[,], cor=TRUE, scale = TRUE)

summary(all_pca)
options(scipen=999)
pca_new<-all_pca$x[,c(1:8)]
dim(pca_new)

comb_pca<-cbind(credit_new[,c(2:4,6:11,21)],pca_new)
dim(comb_pca)

#rf using pca

set.seed(300)
indxTrain <- createDataPartition(y = comb_pca$default.payment.next.month,p = 0.70,list = FALSE)
training <- comb_pca[indxTrain,]
testing <- comb_pca[-indxTrain,]


train.rose<-ROSE(default.payment.next.month~.,data=training,seed=1)$data
table(train.rose$default.payment.next.month)
test.rose<-ROSE(default.payment.next.month~.,data=testing,seed=1)$data
table(test.rose$default.payment.next.month)


rf_pca=randomForest(default.payment.next.month ~ . , data = train.rose)
rf_pca
plot(rf_pca)
rf_pred_pca <- predict(rf_pca,newdata = test.rose)
head(rf_pred_pca)

confusionMatrix(rf_pred_pca, test.rose$default.payment.next.month )

importance(rf_pca)

#knn with pca

knnFit_pca <- train(default.payment.next.month ~ ., data = train.rose, method = "knn", preProcess = c("center","scale"))
plot(knnFit_pca)
knnFit_pca

knnPredict_pca <- predict(knnFit_pca,newdata = test.rose)
confusionMatrix(knnPredict_pca, test.rose$default.payment.next.month )

#logistic with pca
#Both forward and backward model

model_log.fit3<-glm(default.payment.next.month~.,data=train.rose,family="binomial") %>%
  stepAIC(trace=FALSE,direction = "both")

summary(model_log.fit3)
plot(model_log.fit3)




model_log.fit3$xlevels[["PAY_4"]] <- union(model_log.fit3$xlevels[["PAY_4"]], levels(test.rose$PAY_4))
model_log.fit3$xlevels[["PAY_5"]] <- union(model_log.fit3$xlevels[["PAY_5"]], levels(test.rose$PAY_5))
model_log.fit3$xlevels[["PAY_6"]] <- union(model_log.fit3$xlevels[["PAY_6"]], levels(test.rose$PAY_6))
p_3<-predict(model_log.fit3,test.rose,type='response')

pred3<-ifelse(p_3>0.5,1,0)
tab3<-table(prediction=pred3, Actual=test.rose$default.payment.next.month)

accuracy_logit_test<-sum(diag(tab3))/sum(tab3)
accuracy_logit_test

error_logit_test<-1-sum(diag(tab3))/sum(tab3)
error_logit_test

confusionMatrix(ifelse(p_3>0.5,1,0),test.rose$default.payment.next.month)

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





#newdata <- mydata[c(1,5:10)]

credit_newfact<-credit[c(2,6,13:24)]

cor_res<-cor(credit_newfact,use="pairwise.complete.obs",method="pearson")
round(cor_res,3)

corrplot(cor_res)

ggcorrplot(cor_res)

melted_cor_res <- melt(cor_res)
head(melted_cor_res)

ggplot(data = melted_cor_res, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

credit_new<-credit[c(2,3,4,5,6,7:12,13,15,17,19:25)]
dim(credit_new)
str(credit_new)

table(credit_new$default.payment.next.month)



set.seed(165)
train.id<-sample(c(1:dim(credit_new)[1]),dim(credit_new)[1]*0.6)
train<-credit_new[train.id,]
test<-credit_new[-train.id,]



table(train$default.payment.next.month)
table(test$default.payment.next.month)


#data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data

train.rose<-ROSE(default.payment.next.month~.,data=train,seed=1)$data
table(train.rose$default.payment.next.month)

test.rose<-ROSE(default.payment.next.month~.,data=test,seed=1)$data
table(test.rose$default.payment.next.month)




dim(train.rose)
dim(test.rose)

str(train.rose)

#data_drop <- droplevels(data)

#train.rose<-droplevels(train.rose)
#test.rose<-droplevels(test.rose)
#str(train.rose)
#str(test.rose)

library(MASS)
library(dplyr)
library(caret)
#FORWARD
model_log.fit<-glm(default.payment.next.month~.,data=train.rose,family="binomial")%>%
  stepAIC(trace=FALSE,direction = "forward")

summary(model_log.fit)
model_log.fit


model_log.fit$xlevels[["PAY_4"]] <- union(model_log.fit$xlevels[["PAY_4"]], levels(test.rose$PAY_4))

p_1<-predict(model_log.fit,test.rose,type='response')

pred1<-ifelse(p_1>0.5,1,0)
tab1<-table(prediction=pred1, Actual=test.rose$default.payment.next.month)

accuracy_logit_test<-sum(diag(tab1))/sum(tab1)
accuracy_logit_test

error_logit_test<-1-sum(diag(tab1))/sum(tab1)
error_logit_test

confusionMatrix(ifelse(p_1>0.5,1,0),test.rose$default.payment.next.month)


#BACKWARD
model_log.fit2<-glm(default.payment.next.month~.,data=train.rose,family="binomial") %>%
  stepAIC(trace=FALSE,direction = "backward")

summary(model_log.fit2)
model_log.fit2

model_log.fit2$xlevels[["PAY_4"]] <- union(model_log.fit2$xlevels[["PAY_4"]], levels(test.rose$PAY_4))

p_2<-predict(model_log.fit2,test.rose,type='response')

pred2<-ifelse(p_2>0.5,1,0)
tab2<-table(prediction=pred2, Actual=test.rose$default.payment.next.month)

accuracy_logit_test<-sum(diag(tab2))/sum(tab2)
accuracy_logit_test

error_logit_test<-1-sum(diag(tab2))/sum(tab2)
error_logit_test

confusionMatrix(ifelse(p_2>0.5,1,0),test.rose$default.payment.next.month)


#Both forward and backward model

model_log.fit3<-glm(default.payment.next.month~.,data=train.rose,family="binomial") %>%
  stepAIC(trace=FALSE,direction = "both")

summary(model_log.fit3)
model_log.fit3

model_log.fit3$xlevels[["PAY_4"]] <- union(model_log.fit3$xlevels[["PAY_4"]], levels(test.rose$PAY_4))

p_3<-predict(model_log.fit3,test.rose,type='response')

pred3<-ifelse(p_3>0.5,1,0)
tab3<-table(prediction=pred3, Actual=test.rose$default.payment.next.month)

accuracy_logit_test<-sum(diag(tab3))/sum(tab3)
accuracy_logit_test

error_logit_test<-1-sum(diag(tab3))/sum(tab3)
error_logit_test

confusionMatrix(ifelse(p_3>0.5,1,0),test.rose$default.payment.next.month)

str(credit_new)

install.packages("factoextra")
library(factoextra)
data_pca <- transform(credit_new[c(1,5,12:20)])
str(data_pca)

all_pca <- prcomp(data_pca[,-1], cor=TRUE, scale = TRUE)
summary(all_pca)

chisq.test(credit_new$default.payment.next.month,credit_new$LIMIT_BAL,correct = FALSE)
chisq.test(credit_new$default.payment.next.month,credit_new$AGE,correct = FALSE)


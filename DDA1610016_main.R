# Dijender Saini DDA1610016

library(car)
library("Hmisc")
library("ROCR")
library("caret")
library("ggplot2")
library(caTools)
library("MASS")

# Load data into german_credit dataframe
german_credit<-read.csv("E:/IIITB/Course3/SubmittedAssignmentLogisticRegression/german.csv",strip.white=TRUE)

#########Checkpoint 1#########
# Checking for NA values
length(grep("TRUE",is.na(german_credit))) # None found

str(german_credit)

unique(german_credit$Present.residence.since)

# Categorical variables
# 1.	Status.of.existing.checking.account
# 2.	Credit.history
# 3.	Purpose 
# 4.	Savings.account.bonds
# 5.	Present.employment.since.
# 6.	Personal.status.and.sex
# 7.	Other.debtors...guarantors
# 8.	Property
# 9.	Other.installment.plans
# 10.	Housing.
# 11.	Job_status
# 12.	Telephone.
# 13.	foreign.worker

# Age.in.Years
plot1<-ggplot(german_credit,aes(x=german_credit$Age.in.Years,fill=as.factor(german_credit$Default_status)))
plot1<-plot1+geom_histogram(color="black",binwidth=5)
plot1<-plot1+ggtitle("Age Vs. Default Status")+xlab("Age")+guides(fill=guide_legend(title="Default Status"))
plot1

# Credit history

plot2<-ggplot(german_credit,aes(x=as.factor(german_credit$Credit.history),fill=as.factor(german_credit$Default_status)))
plot2<-plot2+geom_bar(color="black",position=position_dodge())
plot2<-plot2+ggtitle("Credit History Vs. Default Status")+xlab("Credit History")+guides(fill=guide_legend(title="Default Status"))
plot2

#Personal.status.and.sex
plot3<-ggplot(german_credit,aes(x=as.factor(german_credit$Personal.status.and.sex),fill=as.factor(german_credit$Default_status)))
plot3<-plot3+geom_bar(color="black",position=position_dodge())
plot3<-plot3+ggtitle("Personal Status & Sex Vs. Default Status")+xlab("Personal Status & Sex")+guides(fill=guide_legend(title="Default Status"))
plot3
 

# Job_status
plot4<-ggplot(german_credit,aes(x=as.factor(german_credit$Job_status),fill=as.factor(german_credit$Default_status)))
plot4<-plot4+geom_bar(color="black",position=position_dodge())
plot4<-plot4+ggtitle("Job Status Vs. Default Status")+xlab("Job Status")+guides(fill=guide_legend(title="Default Status"))
plot4


# Purpose 
plot5<-ggplot(german_credit,aes(x=as.factor(german_credit$Default_status),fill=as.factor(german_credit$Purpose)))
plot5<-plot5+geom_bar(color="black",position=position_dodge())
plot5<-plot5+ggtitle("Purpose Vs. Default Status")+xlab("Default Status")+guides(fill=guide_legend(title="Purpose"))
plot5

#########Checkpoint 2#########

# Checking for NA values
length(grep("TRUE",is.na(german_credit))) # None found

unique(german_credit$Present.residence.since) # 4 Unique values
unique(german_credit$Number.of.existing.credits.at.this.bank.) #4 Unique values
unique(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.) #2 Unique values
unique(german_credit$Installment.rate.in.percentage.of.disposable.income) #4 Unique values

# Change to factor
german_credit$Present.residence.since<-as.factor(german_credit$Present.residence.since)
german_credit$Number.of.existing.credits.at.this.bank.<-as.factor(german_credit$Number.of.existing.credits.at.this.bank.)
german_credit$Number.of.people.being.liable.to.provide.maintenance.for.<-as.factor(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)
german_credit$Installment.rate.in.percentage.of.disposable.income<-as.factor(german_credit$Installment.rate.in.percentage.of.disposable.income)

# Changing Default_status to factor
german_credit$Default_status<-as.factor(german_credit$Default_status)

str(german_credit)
# Checking for outliers in continous variables
# 1. Duration.in.month
# 2. Credit.amount
# 3. Age.in.Years

length(boxplot.stats(german_credit$Duration.in.month)$out) #70
length(boxplot.stats(german_credit$Credit.amount)$out) #72
length(boxplot.stats(german_credit$Age.in.Years)$out) #23

# Outlier treatment
# Higher values to be capped at Upper hinge + 1.5X IQR
# Lower values to be capped at Lower hinge - 1.5X IQR

# Duration.in.month
LHinge<-quantile(german_credit$Duration.in.month,prob=0.25)
UHinge<-quantile(german_credit$Duration.in.month,prob=0.75)

IQR<-UHinge-LHinge
german_credit$Duration.in.month<-ifelse(german_credit$Duration.in.month<LHinge-1.5*IQR,LHinge-1.5*IQR,
                                ifelse(german_credit$Duration.in.month>UHinge+1.5*IQR,UHinge+1.5*IQR,german_credit$Duration.in.month))

# Credit.amount
LHinge<-quantile(german_credit$Credit.amount,prob=0.25)
UHinge<-quantile(german_credit$Credit.amount,prob=0.75)

IQR<-UHinge-LHinge
german_credit$Credit.amount<-ifelse(german_credit$Credit.amount<LHinge-1.5*IQR,LHinge-1.5*IQR,
                                ifelse(german_credit$Credit.amount>UHinge+1.5*IQR,UHinge+1.5*IQR,german_credit$Credit.amount))

# Age.in.Years
LHinge<-quantile( german_credit$Age.in.Years,prob=0.25)
UHinge<-quantile( german_credit$Age.in.Years,prob=0.75)

IQR<-UHinge-LHinge
german_credit$Age.in.Years<-ifelse( german_credit$Age.in.Years<LHinge-1.5*IQR,LHinge-1.5*IQR,
                                ifelse( german_credit$Age.in.Years>UHinge+1.5*IQR,UHinge+1.5*IQR, german_credit$Age.in.Years))
 
# Checking for outliers
length(boxplot.stats(german_credit$Duration.in.month)$out) #0
length(boxplot.stats(german_credit$Credit.amount)$out) #0
length(boxplot.stats(german_credit$Age.in.Years)$out) #0

# Creating dummy variables for non-binary factor variables 
# 1.	Status.of.existing.checking.account
# 2.	Credit.history
# 3.	Purpose 
# 4.	Savings.account.bonds
# 5.	Present.employment.since.
# 6.  Installment.rate.in.percentage.of.disposable.income 
# 7.	Personal.status.and.sex
# 8.	Other.debtors...guarantors
# 9.	Property
# 10.	Other.installment.plans
# 11. Present.residence.since
# 12.	Housing.
# 13. Number.of.existing.credits.at.this.bank.
# 14.	Job_status


str(german_credit)

# Creating dummy variables for Status.of.existing.checking.account
dummy_1 <- data.frame(model.matrix( ~Status.of.existing.checking.account, data = german_credit))
dummy_1<-dummy_1[,-1]

# Creating dummy variables for Credit.history
dummy_2 <- data.frame(model.matrix( ~Credit.history, data = german_credit))
dummy_2<-dummy_2[,-1]

# Creating dummy variables for Purpose 
dummy_3 <- data.frame(model.matrix( ~Purpose, data = german_credit))
dummy_3<-dummy_3[,-1]

# Creating dummy variables for Savings.account.bonds
dummy_4 <- data.frame(model.matrix( ~Savings.account.bonds, data = german_credit))
dummy_4<-dummy_4[,-1]

# Creating dummy variables for Present.employment.since.
dummy_5 <- data.frame(model.matrix( ~Present.employment.since., data = german_credit))
dummy_5<-dummy_5[,-1]

# Creating dummy variables for Installment.rate.in.percentage.of.disposable.income
dummy_6 <- data.frame(model.matrix( ~Installment.rate.in.percentage.of.disposable.income, data = german_credit))
dummy_6<-dummy_6[,-1]

# Creating dummy variables for Personal.status.and.sex
dummy_7 <- data.frame(model.matrix( ~Personal.status.and.sex, data = german_credit))
dummy_7<-dummy_7[,-1]

# Creating dummy variables for Other.debtors...guarantors
dummy_8 <- data.frame(model.matrix( ~Other.debtors...guarantors, data = german_credit))
dummy_8<-dummy_8[,-1]

# Creating dummy variables for Property
dummy_9 <- data.frame(model.matrix( ~Property, data = german_credit))
dummy_9<-dummy_9[,-1]

# Creating dummy variables for Other.installment.plans
dummy_10 <- data.frame(model.matrix( ~Other.installment.plans, data = german_credit))
dummy_10<-dummy_10[,-1]

# Creating dummy variables for Present.residence.since
dummy_11 <- data.frame(model.matrix( ~Present.residence.since, data = german_credit))
dummy_11<-dummy_11[,-1]

# Creating dummy variables for Housing.
dummy_12 <- data.frame(model.matrix( ~Housing., data = german_credit))
dummy_12<-dummy_12[,-1]

# Creating dummy variables for Number.of.existing.credits.at.this.bank.
dummy_13 <- data.frame(model.matrix( ~Number.of.existing.credits.at.this.bank., data = german_credit))
dummy_13<-dummy_13[,-1]

# Creating dummy variables for Job_status
dummy_14 <- data.frame(model.matrix( ~Job_status, data = german_credit))
dummy_14<-dummy_14[,-1]

# Dummy variables not created for binary variables but updated to 1 and 0
# 15. Number.of.people.being.liable.to.provide.maintenance.for
# 16.	Telephone.
# 17.	foreign.worker

levels(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)
# Change to string
german_credit$Number.of.people.being.liable.to.provide.maintenance.for.<-as.character(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)
# Translate to binary 0 and 1
german_credit$Number.of.people.being.liable.to.provide.maintenance.for.<-ifelse(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.=="1","0","1")
# Change numeric for modelling
german_credit$Number.of.people.being.liable.to.provide.maintenance.for.<-as.numeric(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)


levels(german_credit$Telephone.)
# Change to string
german_credit$Telephone.<-as.character(german_credit$Telephone.)
# Translate to binary 0 and 1
german_credit$Telephone.<-ifelse(german_credit$Telephone.=="A191","0","1")
# Change numeric for modelling
german_credit$Telephone.<-as.numeric(german_credit$Telephone.)

levels(german_credit$foreign.worker)
# Change to string
german_credit$foreign.worker<-as.character(german_credit$foreign.worker)
# Translate to binary 0 and 1
german_credit$foreign.worker<-ifelse(german_credit$foreign.worker=="A201","0","1")
# Change numeric for modelling
german_credit$foreign.worker<-as.numeric(german_credit$foreign.worker)

# Putting all variables together
german_credit_final<-cbind(german_credit[,c(21,2,5,13,18,19,20)],
                           dummy_1,
                           dummy_2,
                           dummy_3,
                           dummy_4,
                           dummy_5,
                           dummy_6,
                           dummy_7,
                           dummy_8,
                           dummy_9,
                           dummy_10,
                           dummy_11,
                           dummy_12,
                           dummy_13,
                           dummy_14)


#########Checkpoint 3#########
# Creating Train and Test data in the ratio 7:3 using sample split on Default_status variable
set.seed(100)
split_german_credit = sample.split(german_credit_final$Default_status, SplitRatio = 0.7)
table(split_german_credit)
german_credit_final.train = german_credit_final[split_german_credit,]
german_credit_final.test = german_credit_final[!(split_german_credit),]

str(german_credit_final.train)

#########Checkpoint 4#########
# Modelling using all variables
first_model = glm(Default_status ~ ., data = german_credit_final.train, family = "binomial")
summary(first_model)

# Stepwise selection of variables
best_model = stepAIC(first_model,direction = "both")
summary(best_model)  # AIC : 679.45
vif(best_model)


# Resulting model :
glm(formula = Default_status ~ Duration.in.month + Age.in.Years + 
      foreign.worker + Status.of.existing.checking.accountA13 + 
      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
      Credit.historyA33 + Credit.historyA34 + PurposeA41 + PurposeA42 + 
      PurposeA43 + PurposeA45 + PurposeA49 + Savings.account.bondsA64 + 
      Savings.account.bondsA65 + Present.employment.since.A74 + 
      Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
      Other.installment.plansA143 + Present.residence.since2 + 
      Housing.A152, family = "binomial", data = german_credit_final.train)

# Removing  foreign.worker p-value: 0.149211
default.model1<-glm(formula = Default_status ~ Duration.in.month + Age.in.Years + 
                      Status.of.existing.checking.accountA13 + 
                      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                      Credit.historyA33 + Credit.historyA34 + PurposeA41 + PurposeA42 + 
                      PurposeA43 + PurposeA45 + PurposeA49 + Savings.account.bondsA64 + 
                      Savings.account.bondsA65 + Present.employment.since.A74 + 
                      Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                      Other.installment.plansA143 + Present.residence.since2 + 
                      Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model1) #AIC: 679.95
vif(default.model1)  

# Removing Age.in.Years p-value: 0.163234
default.model2<-glm(formula = Default_status ~ Duration.in.month +
                      Status.of.existing.checking.accountA13 + 
                      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                      Credit.historyA33 + Credit.historyA34 + PurposeA41 + PurposeA42 + 
                      PurposeA43 + PurposeA45 + PurposeA49 + Savings.account.bondsA64 + 
                      Savings.account.bondsA65 + Present.employment.since.A74 + 
                      Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                      Other.installment.plansA143 + Present.residence.since2 + 
                      Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model2) #AIC: 679.93
vif(default.model2) 

# Removing PurposeA45 p-value: 0.157843
default.model3<-glm(formula = Default_status ~ Duration.in.month +
                      Status.of.existing.checking.accountA13 + 
                      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                      Credit.historyA33 + Credit.historyA34 + PurposeA41 + PurposeA42 + 
                      PurposeA43 + PurposeA49 + Savings.account.bondsA64 + 
                      Savings.account.bondsA65 + Present.employment.since.A74 + 
                      Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                      Other.installment.plansA143 + Present.residence.since2 + 
                      Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model3) #AIC: 680.26
vif(default.model3) 

# Removing Installment.rate.in.percentage.of.disposable.income3 p-value:0.172956
default.model4<-glm(formula = Default_status ~ Duration.in.month +
                      Status.of.existing.checking.accountA13 + 
                      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                      Credit.historyA33 + Credit.historyA34 + PurposeA41 + PurposeA42 + 
                      PurposeA43 + PurposeA49 + Savings.account.bondsA64 + 
                      Savings.account.bondsA65 + Present.employment.since.A74 + 
                      Installment.rate.in.percentage.of.disposable.income4 + 
                      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                      Other.installment.plansA143 + Present.residence.since2 + 
                      Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model4) #AIC: 680.1
vif(default.model4)

# Removing PurposeA49 p-value: 0.141697 
default.model5<-glm(formula = Default_status ~ Duration.in.month +
                      Status.of.existing.checking.accountA13 + 
                      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                      Credit.historyA33 + Credit.historyA34 + PurposeA41 + PurposeA42 + 
                      PurposeA43 + Savings.account.bondsA64 + 
                      Savings.account.bondsA65 + Present.employment.since.A74 + 
                      Installment.rate.in.percentage.of.disposable.income4 + 
                      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                      Other.installment.plansA143 + Present.residence.since2 + 
                      Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model5) #AIC: 680.3
vif(default.model5)

# Removing PurposeA42 p-value: 0.10869
default.model7<-glm(formula = Default_status ~ Duration.in.month +
                      Status.of.existing.checking.accountA13 + 
                      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                      Credit.historyA33 + Credit.historyA34 + PurposeA41 + 
                      PurposeA43 + Savings.account.bondsA64 + 
                      Savings.account.bondsA65 + Present.employment.since.A74 + 
                      Installment.rate.in.percentage.of.disposable.income4 + 
                      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                      Other.installment.plansA143 + Present.residence.since2 + 
                      Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model7) #AIC: 680.91
vif(default.model7)

# Removing PurposeA43 p-value:0.21393
default.model8<-glm(formula = Default_status ~ Duration.in.month +
                      Status.of.existing.checking.accountA13 + 
                      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                      Credit.historyA33 + Credit.historyA34 + PurposeA41 + 
                      Savings.account.bondsA64 + 
                      Savings.account.bondsA65 + Present.employment.since.A74 + 
                      Installment.rate.in.percentage.of.disposable.income4 + 
                      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                      Other.installment.plansA143 + Present.residence.since2 + 
                      Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model8) #AIC: 680.47
vif(default.model8)

# Removing Present.employment.since.A74 p-value: 0.066623   
default.model9<-glm(formula = Default_status ~ Duration.in.month +
                      Status.of.existing.checking.accountA13 + 
                      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                      Credit.historyA33 +
                      Credit.historyA34 + PurposeA41 + 
                      Savings.account.bondsA64 + 
                      Savings.account.bondsA65 +
                      Installment.rate.in.percentage.of.disposable.income4 + 
                      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                      Other.installment.plansA143 + Present.residence.since2 + 
                      Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model9) #AIC: 681.97
vif(default.model9)

# Removing PurposeA41 p-value: 0.044782
default.model10<-glm(formula = Default_status ~ Duration.in.month +
                      Status.of.existing.checking.accountA13 + 
                      Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                      Credit.historyA33 + 
                       Credit.historyA34 +
                      Savings.account.bondsA64 + 
                      Savings.account.bondsA65 +
                      Installment.rate.in.percentage.of.disposable.income4 + 
                      Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                      Other.installment.plansA143 + Present.residence.since2 + 
                      Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model10) #AIC: 684.28
vif(default.model10)



# Removing Other.debtors...guarantorsA103 p-value:0.04508
default.model11<-glm(formula = Default_status ~ Duration.in.month +
                       Status.of.existing.checking.accountA13 + 
                       Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                       Credit.historyA33 + Credit.historyA34 +
                       Savings.account.bondsA64 + 
                       Savings.account.bondsA65 +
                       Installment.rate.in.percentage.of.disposable.income4 + 
                       Personal.status.and.sexA93 + 
                       Other.installment.plansA143 + Present.residence.since2 + 
                       Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model11) #AIC: 686.71
vif(default.model11)

# Removing Credit.historyA33 p-value: 0.003901 
default.model12<-glm(formula = Default_status ~ Duration.in.month +
                         Status.of.existing.checking.accountA13 + 
                         Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                         Credit.historyA34 +
                         Savings.account.bondsA64 + 
                         Savings.account.bondsA65 +
                         Installment.rate.in.percentage.of.disposable.income4 + 
                         Personal.status.and.sexA93 + 
                         Other.installment.plansA143 + Present.residence.since2 + 
                         Housing.A152, family = "binomial", data = german_credit_final.train)
summary(default.model12) #AIC: 693.42
vif(default.model12)


default.model12.test<-glm(formula = Default_status ~ Duration.in.month +
                       Status.of.existing.checking.accountA13 + 
                       Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                       Credit.historyA34 +
                       Savings.account.bondsA64 + 
                       Savings.account.bondsA65 +
                       Installment.rate.in.percentage.of.disposable.income4 + 
                       Personal.status.and.sexA93 + 
                       Other.installment.plansA143 + Present.residence.since2 + 
                       Housing.A152, family = "binomial", data = german_credit_final.test)
summary(default.model12.test) #AIC: 693.42
vif(default.model12.test)

#########Checkpoint 5#########
# Model evaluation

# C - Statistic

# Training data set
german_credit_final.train$predicted_def_stat = predict(default.model12,  type = "response")
rcorr.cens(german_credit_final.train$predicted_def_stat,german_credit_final.train$Default_status)  #c: 0.8094

# Test data set
german_credit_final.test$predicted_def_stat = predict(default.model12.test,  type = "response")
rcorr.cens(german_credit_final.test$predicted_def_stat,german_credit_final.test$Default_status)  #C: 0.7659

# KS - Statistic

# Training data set
model_score_train <- prediction(german_credit_final.train$predicted_def_stat,german_credit_final.train$Default_status)
model_perf_train <- performance(model_score_train, "tpr", "fpr")
ks_table_train <- attr(model_perf_train, "y.values")[[1]] - (attr(model_perf_train, "x.values")[[1]])
ks_train = max(ks_table_train)
which(ks_table_train == ks_train) #218
ks_train #KS stats=0.529932
# To find decile: 218/700
218/700 # 4th Decile

# Test data set
model_score_test <- prediction(german_credit_final.test$predicted_def_stat,german_credit_final.test$Default_status)
model_perf_test <- performance(model_score_test, "tpr", "fpr")
ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
ks_test = max(ks_table_test)
which(ks_table_test == ks_test) #128
ks_test #0.4063492
# To find decile: 128/300
128/300 # 5th Decile

# ROC curve and Confusion matrix
# Training data set
plot(model_perf_train,col = "red", lab = c(10,10,10))
confusionMatrix(as.numeric(german_credit_final.train$predicted_def_stat>0.3),german_credit_final.train$Default_status, positive = "1")

# Test data set
plot(model_perf_test,col = "red", lab = c(10,10,10))
confusionMatrix(as.numeric(german_credit_final.test$predicted_def_stat>0.3),german_credit_final.test$Default_status, positive = "1")


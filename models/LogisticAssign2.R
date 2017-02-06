library(pastecs)
library(Hmisc)
library(psych)
library(car)
library(gmodels)
library(scales)
library(lmtest)
library(pscl)

library(Deducer) 
library(fmsb)
library(caret)
library(randomForest)
library(ROSE)
library(ineq)
library(PresenceAbsence)


hosmerlem <-  function (y, yhat=n*prob, g = 10) 
{
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 
                                                           1, 1/g)), include.lowest = T)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  if (chisq<1*(10^(-20))) P="." else P=ifelse((g==2),1,pchisq(chisq, g - 2,lower.tail=FALSE))
  yhat
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}


mydata <- read.csv("C:/AMBER/Great Lakes/Predictive Modelling/Logistic Assignment2/Cellphone.csv", header = TRUE)
numcol <- ncol(mydata)
numrow <- nrow(mydata)

#Summary does not show any missing value
summary(mydata)

#we want to infer /predict about churn therefore churn is dependent variable
#ContractRenewal and DataPlan are two categorical variables. Lets convert them into factors
mydata$Churn <- as.factor(mydata$Churn)
mydata$ContractRenewal <- as.factor(mydata$ContractRenewal)
mydata$DataPlan <- as.factor(mydata$DataPlan)

#Exploratory data analysis

#base churn rate : 14.5% churn rate in sample dataset 
as.matrix(prop.table(table(mydata$Churn))*100)

#Since Data is a bit Skewed , Oversampling would be considered while model building

#Univariate Analysis
#-----------------Proportions for Categorical Variables---------------------------
data.frame(Proportion =sort(round(prop.table(table(mydata$ContractRenewal))*100,2),decreasing = TRUE))
data.frame(Proportion =sort(round(prop.table(table(mydata$DataPlan))*100,2),decreasing = TRUE))

#Univariate Analysis of Contineous Variables .Checking distribution of all
#contineous variables and recording observations
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(mydata$AccountWeeks, main = "Account Weeks")
hist(mydata$DataUsage, main = "Data Usage")
hist(mydata$CustServCalls, main = "Service Calls")
hist(mydata$DayMins, main = "Montly Day Minutes")
hist(mydata$DayCalls, main = "Avg Daily Calls")
hist(mydata$MonthlyCharge, main = "Avg Monthly Bill")
hist(mydata$OverageFee, main = "Largest Overage Fee")
hist(mydata$RoamMins, main = "Avg Roaming Min")


attach(mydata)
ggplot(mydata, aes(x= ContractRenewal,  group=Churn)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="ContractRenewal") +
  facet_grid(~Churn) +
  scale_y_continuous(labels=percent)



ggplot(mydata, aes(x= DataPlan,  group=Churn)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="DataPlan") +
  facet_grid(~Churn) +
  scale_y_continuous(labels=percent)



ggplot(mydata, aes(as.factor(Churn),AccountWeeks )) +
  geom_boxplot()+
  labs(title = "AccountWeeks")

ggplot(mydata, aes(as.factor(Churn),DataUsage )) +
  geom_boxplot()+
  labs(title = "DataUsage")

ggplot(mydata, aes(as.factor(Churn),CustServCalls )) +
  geom_boxplot()+
  labs(title = "CustServCalls")

ggplot(mydata, aes(as.factor(Churn),DayMins )) +
  geom_boxplot()+
  labs(title = "DayMins")

ggplot(mydata, aes(as.factor(Churn),DayCalls )) +
  geom_boxplot()+
  labs(title = "DayCalls")

ggplot(mydata, aes(as.factor(Churn),MonthlyCharge )) +
  geom_boxplot()+
  labs(title = "MonthlyCharge")


ggplot(mydata, aes(as.factor(Churn),OverageFee )) +
  geom_boxplot()+
  labs(title = "OverageFee")

ggplot(mydata, aes(as.factor(Churn),RoamMins )) +
  geom_boxplot()+
  labs(title = "RoamMins")
#----------------Data Partitioning------------------
library(caTools)
library(ROCR)

set.seed(200)
spl = sample.split(mydata$Churn, SplitRatio = 0.70)

data.dev <- subset(mydata, spl == TRUE)
data.valid <- subset(mydata, spl == FALSE)

#------------ OVERSAMPLING--------------------------------
data.oversample <- ovun.sample(Churn ~ ., data = data.dev,
                               method = "over",
                               p=0.5, seed=1)$data

table(data.oversample$Churn)
table(data.dev$Churn)

#----------------Model Building-----------------------
fit1 <- glm(Churn~.,family = "binomial",data=data.oversample)

summary(fit1)

#using step wise regression to select variables
fit2 <- step(fit1)
summary(fit2)
vif(fit2)

#Removing daycalls from predictor variable as it is not significant
fit2  <- glm(formula = Churn ~ ContractRenewal +
                               DataPlan +
                               CustServCalls +
                               DayMins +
                               OverageFee +
                               RoamMins, 
                               family = "binomial",
                               data = data.oversample)

#--------------Performance Parameters----------------------------
predict.log <- predict(fit2, newdata = data.valid, type = "response")
out <- ifelse(predict.log >= .5, 1,0)
confusionMatrix(data = out, reference = data.valid$Churn, positive = "1")
pred <- prediction(predict.log,data.valid$Churn)
au <-as.numeric(performance(pred, "auc")@y.values)

perf <- performance(pred,"tpr","fpr")
plot(perf, main = "ROC : Logistic Regression", colorize=T)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]]) 

# And then a lift chart
perf <- performance(pred,"lift","rpp")
plot(perf, main="Lift : Logistic Regression", colorize=T)

au
KS


#--------------Interpretation---------------------

#  (Intercept)      -3.1640282  0.2889324 -10.951  < 2e-16 ***
#  ContractRenewal1 -2.2815707  0.1218245 -18.728  < 2e-16 ***
#  DataPlan1        -0.8792524  0.0950352  -9.252  < 2e-16 ***
#  CustServCalls     0.5758723  0.0272169  21.159  < 2e-16 ***
#  DayMins           0.0140170  0.0007028  19.944  < 2e-16 ***
#  OverageFee        0.0965862  0.0152006   6.354  2.1e-10 ***
#  RoamMins          0.0496151  0.0138353   3.586 0.000336 ***
  ---
  
summary(fit2)
fit2.coef <- coef(fit2)
fit2.exp.coef <- exp(fit2.coef)

fit2.exp.coef

#Intercept) ContractRenewal1        DataPlan1    CustServCalls          DayMins       OverageFee 
#0.04225518       0.10212367       0.41509311       1.77868134       1.01411573       1.10140447 
#RoamMins 
#1.05086655 

#Effect of individual predictors

#Contract Renewal :
# 1/0.10212367 = 9.792049
#Customer renewal is significant predictor of customer churn behaviour. All other factors remaining same,
#Cusomer who do not renew contract have almost 9.8 times odds of churning compared to those who renew contract.

#Data Plan :
# 1/0.41509311 = 2.409098
#If customer is having dataplan or not is significant predictor of customer churn behaviour. All other factors 
#remaining same Cusomer who do not use company data plans 2.4 times odds of churning compared to 
#those who opt for data plans



hist(mydata$CustServCalls)
#Customer Service Calls :  Logically if customers are making large number complaints, he is more likely 
#to churn. As suggested by model also that every increases in complaints increases odds of churn by 1.77 
#times or every increase in comlaint increases odds of churn by 77% higher

#Average Day Time Min Per Month : It does not make sense to find out if every 1 min increase in montly usage 
#will affect odds of churn . Therefore it would be useful matrix to find out the increase of monthly usage
#min which would lead to 50% increases in odds of churn. It can be calucated as 50/1.01411573 = approx 49.
#Therefore it can be stated that if monthly usage increases by 49 min then odds of churn increases by 50%
#while all other factors remain same

#Overage FEE : Compared to previous year customer odds of churn increased by 10% if this year maximum
#overage fee for customer goes up by 1 unit while all the factors remain same. While model state this parameter
#to be significant but in practice it might not be very significant as duration of is once in year


#Roaming Min : Range of roaming min in dataset is from 0 to 20. Therefore odds of a person who in on roaming 
#for average 20 min per month have approximate 20% more odds of churning compared to a person who do not use
#roaming at all. Contineous high usage of romaing can be an indication that customer will churn provided
#all other factors remain same.


#Lets see how probablity vary with Customer Service Call when other values are fixed. Since Contract Renewal and Dataplan
#are two most significant categorical variables. We would plot probablites with varying Customer serivce call for various combination
#of these two categorical variables

CustSevcCall = seq(from=0, to=9, by=1)

#NC = No contract renewal 
#NP = No Data Plan 
logOdds.NC.NP   = -3.003 + .575*CustSevcCall
logOdds.NC.P    = -3.883 + .575*CustSevcCall
logOdds.C.NP    = -5.285 + .575*CustSevcCall
logOdds.C.P     = -6.164 + .575*CustSevcCall

Pchurn.NC.NP   = exp(logOdds.NC.NP) /(1 + exp(logOdds.NC.NP))
Pchurn.NC.P    = exp(logOdds.NC.P) /(1 + exp(logOdds.NC.P))
Pchurn.C.NP    = exp(logOdds.C.NP) /(1 + exp(logOdds.C.NP))
Pchurn.C.P     = exp(logOdds.C.P) /(1 + exp(logOdds.C.P))

#windows()
#par(mfrow=c(2,2))
#plot(x=CustSevcCall, y=Pchurn.NC.NP, type="l", col="blue", lwd=2, 
#     ylab="Pr(Churn)", main="Churn prob When No Reneweal and No DataPlan")
#plot(x=CustSevcCall, y=Pchurn.NC.P, type="l", col="blue", lwd=2, 
#     ylab="Pr(Churn)", main="Churn prob When No Reneweal and No DataPlan")
#plot(x=CustSevcCall, y=Pchurn.C.NP, type="l", col="blue", lwd=2, 
#     ylab="Pr(Churn)", main="Churn prob When No Reneweal and No DataPlan")
#plot(x=CustSevcCall, y=Pchurn.C.P, type="l", col="blue", lwd=2, 
#     ylab="Pr(Churn)", main="Churn prob When No Reneweal and No DataPlan")


windows()
plot(x=CustSevcCall, y=Pchurn.NC.NP, type="l", col="red", lwd=1, 
     ylab="Pr(Y=1)", main="CHURN Probablities")
lines(x=CustSevcCall, y=Pchurn.NC.P, col="blue", lwd=1)
lines(x=CustSevcCall, y=Pchurn.C.NP,   col="red",  lwd=2, lty="dotted")
lines(x=CustSevcCall, y=Pchurn.C.P,   col="blue", lwd=2, lty="dotted")
legend("topleft", legend=c("No Reneweal/No DataPlan", "No Reneweal/DataPlan", 
                               "Reneweal/No DataPlan", "Reneweal/DataPlan"), 
                                lty=c("solid", "solid", "dotted","dotted"),
                                lwd=c(1,1,2,2), col=c("red", "blue", "red", "blue"))


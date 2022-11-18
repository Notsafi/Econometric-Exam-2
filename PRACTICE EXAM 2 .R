#2021 Q4
load("~/Desktop/Statistic & Econmetric/Data/acs2017_ny/acs2017_ny_data.RData")
#subset = data. dependnet variable is restrict
summary(acs2017_ny)
hours <- subset(acs2017_ny, (acs2017_ny$UHRSWORK < 35) | (acs2017_ny$UHRSWORK >= 35))
summary(acs2017_ny)
# i created a subset because i wanted to see the difference between people that work less then 35 hours because that in NYC makes you part time
#while over 35 hours makes you full time to get a split difference of how many people are full time vs part time

#2021 Q5 
restrict1 <-(acs2017_ny$UHRSWORK < 35) & (acs2017_ny$SEX == "Male") 
data_new1 <- subset(acs2017_ny,restrict1) 
t.test(restrict1)
sd(restrict1)
summary(data_new1$SEX) #SUMMARY:27%
prop.table(summary(data_new1$UHRSWORK), margin = NULL)

restrict2 <-(acs2017_ny$UHRSWORK >=35) & (acs2017_ny$SEX == "Male")
data_new2 <- subset(acs2017_ny,restrict2) 
t.test(restrict2)
sd(restrict2)
summary(data_new2$UHRSWORK) #SUMMARY:22%
prop.table(summary(data_new2$UHRSWORK), margin = NULL)

restrict3 <-(acs2017_ny$UHRSWORK < 35) & (acs2017_ny$SEX == "Female") 
data_new3 <- subset(acs2017_ny,restrict3) 
t.test(restrict3)
sd(restrict3)
summary(data_new3$SEX) #SUMMARY:34%
prop.table(summary(data_new3$UHRSWORK), margin = NULL)

restrict4 <-(acs2017_ny$UHRSWORK >=35) & (acs2017_ny$SEX == "Female")
data_new4 <- subset(acs2017_ny,restrict4) 
t.test(restrict4)
sd(restrict4)
summary(data_new4$UHRSWORK) #SUMMARY:18%
prop.table(summary(data_new4$UHRSWORK), margin = NULL)

#Based on the subgroups of genders based on hours worked i learned that there are more females that worked full time then men.
#However there is less women that work part then both men hours. 


#2021 Q6 MODEL OLS
model1 <- lm(UHRSWORK~ AGE + female +educ_hs + educ_college + educ_advdeg, data = hours)
summary(model1)

#6a education causes you to work full time so the positive number causes the dependent variable. age means that the older u get the less likely to work full time
#higher education makes it likedly to work more hours so interaction with dummy variables is importat 

#6b 

#6c #c so we reject the null hypothesis because there is a relationship between the independnet variable and dependent variable because the f statistic pvalue is lower than all the significance level so the join test is not all zero 

#6d
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1,  educ_hs = 1, educ_college = 1, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(model1, newdata = to_be_predicted2, type= "response")
summary(to_be_predicted2$yhat)
#This person that is female between that age and has that education status has a AVERGAE of working 57 hours a week 

#6e 
set.seed(1)
index<-sample(x=2,size=nrow(hours),replace=TRUE,prob=c(0.7,0.3))
train<-hours[index==1,]
test<-hours[index==2,]
trainmodel<-lm(UHRSWORK~ AGE + female +educ_hs + educ_college + educ_advdeg, data = hours)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$UHRSWORK,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum


#7 LOGIT MODEL
#example for creating binary factors acs2017_ny$school1<-(acs2017_ny$SCHOOL == "Yes, in school") or acs2017_ny$race<- (acs2017_ny$RACE == 8) summary(acs2017_ny$race)
#summary(acs2017_ny$school1)
summary(acs2017_ny)
acs2017_ny$fulltime <-(acs2017_ny$UHRSWORK >=35) 
summary(acs2017_ny)

model_logit1 <- glm(fulltime  ~ AGE + female +educ_hs + educ_college + educ_advdeg, data = hours, family= binomial)
summary(model_logit1)
#a demographic factors are exogenous factors they dont have  a change on other varibale. you cant change it. #so interaction with dummy variables is importatant because it changes the level of estimate which can show us the causation they have on dependent variable 
#b polynomial is age^2 or age ^3 it shows a jump in the plot diagraph like a pattern, its more beneficials for variables taht have huge afctors like income, age. independent must cause the dependent variable
# estimates do make it plasuiable because as education level increase causes you to work full time so the positive number are the plausible estimate since they play a factor
#age means that 1 incrimate causes the estimate to increase that level to be fulltime. so the older u get the less likely to work full time and higher education makes it likedly to work more hours 
# We know they are statistically significant based on the stars because of the confidence interval. so the pvalue must be lower then significance level to say its significant. 
#null hypothesis is there is no relationship between the independent and dependent varibale and alternative hypothesis is there is a relationship  

#7c equation for f statistic if you dont get it in the regression
# null deviance - residual deviance, null deviance DF - Residual deviance DF 
pchisq(263579-237720, 196584-196539)
#Reason; The p-value turns out to be 1. Since this p-value is not less than 0.05, we fail to reject the null hypothesis. This means we do not have sufficient evidence to say that the true distribution of customers is different from the distribution that the shop owner claimed. There is a joint significance

#7D 
to_be_predicted3 <- data.frame(AGE = 25:55, female = 1,  educ_hs = 0, educ_college = 0, educ_advdeg = 1)
to_be_predicted3$yhat <- predict(model_logit1, newdata = to_be_predicted3, type= "response")
summary(to_be_predicted3$yhat)

#7e
set.seed(1)
index<-sample(x=2,size=nrow(hours),replace=TRUE,prob=c(0.7,0.3))
train<-hours[index==1,]
test<-hours[index==2,]
trainmodel<-glm(fulltime  ~ AGE + female +educ_hs + educ_college + educ_advdeg, data = hours)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$fulltime,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum
#Type 1 error is the 5782 and type II errors is 12801. It can predict the train data accurately.
#It correctly predicts 68% of the train data.


#QUESTION 8
require(pROC)
roc_curve<-roc(test$fulltime,prob)
x<-1-roc_curve$specificities
y<-roc_curve$sensitivities
plot(x=x,y=y,xlim=c(0,1),ylim=c(0,1),xlab="1-specificity",
     ylab="sensitivity",main="ROC Curve",type="l",lwd=2)
abline(a=0,b=1,col="grey")
auc<-roc_curve$auc
text(0.5,0.4,paste("AUC",round(auc,digits=2)),col="blue")
#The y axis is the rate of true positive(accurate) while the x axis is the rate of false postives(inaccuarate). 
#Therefore, the black line is greater then the grey line this represents true positives.

#question 8
model_logit1 <- glm(fulltime  ~ AGE + female +educ_hs + educ_college + educ_advdeg, data = hours)
summary(model_logit1)
summary(model_logit1$fitted)
summary(hours$fulltime)
pred_model_logit1 <- (model_logit1$fitted > 0.5)
table(pred_model_logit1, hours$fulltime)
frac_correct_l1a <- mean(as.numeric(as.numeric(pred_model_logit1) == hours$fulltime))
pred_model_logit1b <- (model_logit1$fitted > mean(hours$fulltime))
table(pred_model_logit1b, hours$fulltime)
frac_correct_l1b <- mean(as.numeric(as.numeric(pred_model_logit1b) == hours$fulltime))
frac_correct_try <- rep(0,140)
for (indx in 1:140) {
  pred_model_indx <- (model_logit1$fitted > (indx/200) )
  frac_correct_try[indx] <- mean(as.numeric(as.numeric(pred_model_indx) == hours$fulltime))
}
plot((seq(140)/200),frac_correct_try)
#confidence level of 0.5 has a 68% accuracy of predicting the model















#HOUSEHOLD DATA 


#4a shop=data
shop <- subset(Household_Pulse_data, (Household_Pulse_data$Shop_in_store == "shopped in store"))
summary(Household_Pulse_data)

#restrict
load("~/Desktop/Statistic & Econmetric/Data/Household_Pulse_data.RData")
Household_Pulse_data$eat <-(Household_Pulse_data$eat_in_restaurant == "eat at restaurant indoors")
Household_Pulse_data$neat <-(Household_Pulse_data$eat_in_restaurant == "no")
summary(Household_Pulse_data)

#6
#ols 
model1 <- lm(eat~ EEDUC + ANYWORK + works_remote, data = shop)
summary(model1)

to_be_predicted2 <- data.frame(ANYWORK = "yes employment in last 7 days", EEDUC = "bach deg", works_remote = "worked remotely")
to_be_predicted2$yhat <- predict(model1, newdata = to_be_predicted2, type= "response")
summary(to_be_predicted2$yhat)

#6E
set.seed(1)
index<-sample(x=2,size=nrow(shop),replace=TRUE,prob=c(0.7,0.3))
train<-shop[index==1,]
test<-shop[index==2,]
trainmodel<-lm(eat~ EEDUC + ANYWORK + works_remote, data = shop)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$eat,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum


#logit 
model_logit1 <- glm(eat~ EEDUC + ANYWORK + works_remote, data = shop, family= binomial)
summary(model_logit1)

to_be_predicted3 <- data.frame(ANYWORK = "yes employment in last 7 days", EEDUC = "bach deg", works_remote = "worked remotely")
to_be_predicted3$yhat <- predict(model_logit1, newdata = to_be_predicted3, type= "response")
summary(to_be_predicted3$yhat)

set.seed(1)
index<-sample(x=2,size=nrow(shop),replace=TRUE,prob=c(0.7,0.3))
train<-shop[index==1,]
test<-shop[index==2,]
trainmodel<-glm(eat~ EEDUC + ANYWORK + works_remote, data = shop)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$eat,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum

#q8
require(pROC)
roc_curve<-roc(test$eat,prob)
x<-1-roc_curve$specificities
y<-roc_curve$sensitivities
plot(x=x,y=y,xlim=c(0,1),ylim=c(0,1),xlab="1-specificity",
     ylab="sensitivity",main="ROC Curve",type="l",lwd=2)
abline(a=0,b=1,col="grey")
auc<-roc_curve$auc
text(0.5,0.4,paste("AUC",round(auc,digits=2)),col="blue")

#q8

model_logit1 <- glm(eat~ EEDUC + ANYWORK + works_remote, data = shop, family= binomial)
summary(model_logit1)
summary(model_logit1$fitted)
summary(shop$eat)
pred_model_logit1 <- (model_logit1$fitted > 0.5)
table(pred_model_logit1, shop$eat)
frac_correct_l1a <- mean(as.numeric(as.numeric(pred_model_logit1) == shop$eat))
pred_model_logit1b <- (model_logit1$fitted > mean(shop$eat))
table(pred_model_logit1b, shop$eat)
frac_correct_l1b <- mean(as.numeric(as.numeric(pred_model_logit1b) == shop$eat))
frac_correct_try <- rep(0,140)
for (indx in 1:140) {
  pred_model_indx <- (model_logit1$fitted > (indx/200) )
  frac_correct_try[indx] <- mean(as.numeric(as.numeric(pred_model_indx) == shop$eat))
}
plot((seq(140)/200),frac_correct_try)
# around 60% accuracy at the confidence level of 0.7 
















#BRFFS DATAAAA

load("~/Desktop/Statistic & Econmetric/Data/brfss_data/BRFSS2013_a.RData")
summary(dat3)
#variable he gives you is always a restrcit 
dat3$flushot <-(dat3$FLUSHOT6 =="Yes") 
summary(dat3)

genhealth<- subset(dat3,(dat3$GENHLTH =="Excellent")) 
summary(genhealth)

pt(7.34,19998,lower.tail = TRUE)

model1 <- lm(flushot ~ MENTHLTH + MEDCOST + PERSDOC2, data = genhealth)
summary(model1)

to_be_predicted2 <- data.frame(MENTHLTH = 11, MEDCOST = "Yes", PERSDOC2 = "More than one")
to_be_predicted2$yhat <- predict(model1, newdata = to_be_predicted2, type= "response")
summary(to_be_predicted2$yhat)

set.seed(1)
index<-sample(x=2,size=nrow(genhealth),replace=TRUE,prob=c(0.7,0.3))
train<-genhealth[index==1,]
test<-genhealth[index==2,]
trainmodel<-lm(flushot ~ MENTHLTH + MEDCOST + PERSDOC2, data = genhealth)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$flushot,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum


model_logit1 <- glm(flushot ~ MENTHLTH + MEDCOST + PERSDOC2, data = genhealth, family= binomial)
summary(model_logit1)


pchisq(180742-178886, 206287-206283)

to_be_predicted3 <- data.frame(MENTHLTH = 11, MEDCOST = "Yes", PERSDOC2 = "More than one")
to_be_predicted3$yhat <- predict(model_logit1, newdata = to_be_predicted3, type= "response")
summary(to_be_predicted3$yhat)

set.seed(1)
index<-sample(x=2,size=nrow(genhealth),replace=TRUE,prob=c(0.7,0.3))
train<-genhealth[index==1,]
test<-genhealth[index==2,]
trainmodel<-glm(flushot ~ MENTHLTH + MEDCOST + PERSDOC2, data = genhealth)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$flushot,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum

require(pROC)
roc_curve<-roc(test$flushot,prob)
x<-1-roc_curve$specificities
y<-roc_curve$sensitivities
plot(x=x,y=y,xlim=c(0,1),ylim=c(0,1),xlab="1-specificity",
     ylab="sensitivity",main="ROC Curve",type="l",lwd=2)
abline(a=0,b=1,col="grey")
auc<-roc_curve$auc
text(0.5,0.4,paste("AUC",round(auc,digits=2)),col="blue")

model_logit1 <-  glm(flushot ~ X_SMOKER3 + X_HISPANC + MARITAL, data = genhealth)
summary(model_logit1)
summary(model_logit1$fitted)
summary(genhealth$flushot)
pred_model_logit1 <- (model_logit1$fitted > 0.5)
table(pred_model_logit1, genhealth$flushot)
frac_correct_l1a <- mean(as.numeric(as.numeric(pred_model_logit1) == genhealth$flushot))
pred_model_logit1b <- (model_logit1$fitted > mean(genhealth$flushot))
table(pred_model_logit1b, genhealth$flushot)
frac_correct_l1b <- mean(as.numeric(as.numeric(pred_model_logit1b) == genhealth$flushot))
frac_correct_try <- rep(0,140)
for (indx in 1:140) {
  pred_model_indx <- (model_logit1$fitted > (indx/200) )
  frac_correct_try[indx] <- mean(as.numeric(as.numeric(pred_model_indx) == genhealth$flushot))
}
plot((seq(140)/200),frac_correct_try)













#NHIS DATA

load("~/Desktop/Statistic & Econmetric/Data/NHIS_2017.RData")
summary(data_use1)
data_use1$health <-(data_use1$person_healthstatus =="Excellent") 
summary(data_use1)
vet<- subset(data_use1,(data_use1$veteran_stat == 1)) 
summary(vet)

model1 <- lm(health ~ MEDICARE + MEDICAID + REGION, data = vet)
summary(model1)

to_be_predicted2 <- data.frame(MEDICARE = 1, MEDICAID = 1, REGION = "Northeast")
to_be_predicted2$yhat <- predict(model1, newdata = to_be_predicted2, type= "response")
summary(to_be_predicted2$yhat)

set.seed(1)
index<-sample(x=2,size=nrow(vet),replace=TRUE,prob=c(0.7,0.3))
train<-vet[index==1,]
test<-vet[index==2,]
trainmodel<-lm(health ~ MEDICARE + MEDICAID + REGION, data = vet)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$health,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum

model_logit1 <- glm(health ~ MEDICARE + MEDICAID + REGION, data = vet, family= binomial)
summary(model_logit1)

to_be_predicted2 <- data.frame(MEDICARE = 1, MEDICAID = 1, REGION = "Northeast")
to_be_predicted2$yhat <- predict(model_logit1, newdata = to_be_predicted2, type= "response")
summary(to_be_predicted2$yhat)

set.seed(1)
index<-sample(x=2,size=nrow(vet),replace=TRUE,prob=c(0.7,0.3))
train<-vet[index==1,]
test<-vet[index==2,]
trainmodel<-glm(health ~ MEDICARE + MEDICAID + REGION, data = vet)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$health,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum


require(pROC)
roc_curve<-roc(test$health,prob)
x<-1-roc_curve$specificities
y<-roc_curve$sensitivities
plot(x=x,y=y,xlim=c(0,1),ylim=c(0,1),xlab="1-specificity",
     ylab="sensitivity",main="ROC Curve",type="l",lwd=2)
abline(a=0,b=1,col="grey")
auc<-roc_curve$auc
text(0.5,0.4,paste("AUC",round(auc,digits=2)),col="blue")
#subset then restrict)

model_logit1 <-  glm(health ~ MEDICARE + MEDICAID + REGION, family= binomial,data = vet,)
summary(model_logit1)
summary(model_logit1$fitted)
summary(vet$health)
pred_model_logit1 <- (model_logit1$fitted > 0.5)
table(pred_model_logit1, vet$health)
frac_correct_l1a <- mean(as.numeric(as.numeric(pred_model_logit1) == vet$health))
pred_model_logit1b <- (model_logit1$fitted > mean(vet$health))
table(pred_model_logit1b, vet$health)
frac_correct_l1b <- mean(as.numeric(as.numeric(pred_model_logit1b) == vet$health))
frac_correct_try <- rep(0,140)
for (indx in 1:140) {
  pred_model_indx <- (model_logit1$fitted > (indx/200) )
  frac_correct_try[indx] <- mean(as.numeric(as.numeric(pred_model_indx) == vet$health))
}
plot((seq(140)/200),frac_correct_try)






















#STATSSS
#2019

#1.  For a Normal Distribution that has mean -6 and standard deviation 9.8 , what is the area to the left of -19.72 ?
pnorm(-19.72, mean=-6, sd=9.8, lower.tail = TRUE)
#1B For a Normal Distribution that has mean 9 and standard deviation 4.6 , what is the area in both tails farther from the mean than -0.66
pnorm(-0.66, mean=9, sd=4.6)*2 
#1C For a Normal Distribution that has mean -8 and standard deviation 7.3 , what is the area to the right of -19.68
pnorm(-19.68, mean=-8, sd=7.3, lower.tail = FALSE) 
#1D For a Normal Distribution that has mean -12 and standard deviation 9.4 , what is the area to the right of 10.56
pnorm(10.56, mean=-12, sd=9.4, lower.tail = FALSE) 
#1E For a Normal Distribution that has mean 2 and standard deviation 0.4 , what is the area to the left of 2.36 ?
pnorm(2.36, mean=2, sd=.4, lower.tail = TRUE)
#1F For a Normal Distribution that has mean 4 and standard deviation 2.2 , what is the area to the right of 2.9 ?
pnorm(2.9, mean=4, sd=2.2, lower.tail = FALSE) 
#1G  For a Normal Distribution that has mean -4 and standard deviation 1.4 , what is the area in both tails farther
#from the mean than -5.68
pnorm(-5.68, mean=-4, sd=1.4)*2
#1H For a Normal Distribution that has mean 1 and standard deviation 9.8 , what is the area in both tails farther
#from the mean than -22.52 ?
pnorm(-22.52, mean=1, sd=9.8)*2
#1I For a Normal Distribution that has mean -12 and standard deviation 4.6 what two values leave probability
#0.441 in both tails?
qnorm(.441/2,-12,4.6, lower.tail = TRUE) , qnorm(.441/2,-12,4.6 ,lower.tail = FALSE)
#1J For a Normal Distribution that has mean -11 and standard deviation 8.4 what two values leave probability
#0.248 in both tails?
qnorm(.248/2,-11,8.4, lower.tail = TRUE) , qnorm(.248/2,-11,8.4, lower.tail = FALSE)
#1K  A regression coefficient is estimated to be equal to 7.02 with standard error 5.4 ; there are 17 degrees of freedom. What is the p-value (from the t-statistic) against the null hypothesis of zero?
2*pt(q= 1.3, df=17, lower.tail=FALSE) 
#1L A regression coefficient is estimated to be equal to 4.96 with standard error 6.2 ; there are 6 degrees of freedom. What is the p-value (from the t-statistic) against the null hypothesis of zero?
2*pt(q= .8, df=6, lower.tail=FALSE) 
#1M  A regression coefficient is estimated to be equal to -9.2 with standard error 4.6 ; there are 18 degrees of freedom. What is the p-value (from the t-statistic) against the null hypothesis of zero?
pt(q= -2, df=18, lower.tail=TRUE)*2 or, 1-.03041073 , .03041073*2


#2 pt(q= 3.80520765, df=3, lower.tail=TRUE), 1-.9840542 , .0159458*2
#OR 2*pt(q= 3.80520765, df=3, lower.tail=FALSE)
#pt(q= 2.98658856, df=3, lower.tail=TRUE), 1-.9708554 , .0291446*2
#pt(q= 6.34847298, df=3, lower.tail=TRUE), 1-.9960469 , .0039531*2
#pt(q= -.24115696, df=3, lower.tail=TRUE), 1-.412488 , .587512*2
#2B anything under 95% confidence interval which is under .05







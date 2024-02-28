# Step 1: Call mandatory libraries 
library(pacman)
p_load(ggplot2,dplyr,reader,caret,psych,MASS,descr,boot,corrplot,tidyverse,rms,lubridate)

#Step 2: Load the training and test data set into R
setwd("C:\\Users\\Pranav Belmannu\\Documents\\MKTG")
elec_loan_train_data<-read.csv(file.choose())
elec_loan_test_data<-read.csv(file.choose())

# Step 3.1: View the data
head(elec_loan_train_data,10)

# Step 3.2: Plot histogram of reposesessed column
ggplot(elec_loan_train_data,aes(x=repossessed))+geom_bar()

#Step 3.3: 
# Extract years with pattern matching for accountage
accountageyrs<-str_extract(elec_loan_train_data$accountage,".")
accountageyrs<-as.numeric(accountageyrs)
# Extract years with pattern matching for lengthofcredithistory
lengthofcredithistoryyrs<-str_extract(elec_loan_train_data
                                      $lengthofcredithistory,".")
lengthofcredithistoryyrs<-as.numeric(lengthofcredithistoryyrs)
## Extract months with pattern matching for accountage
accountagemon<-str_extract(elec_loan_train_data$accountage," ..")
accountagemonfinal<-rep(0,times=length(accountagemon))
for(i in 1:length(accountagemon))
{if((endsWith(accountagemon[i],"m")==TRUE))
{accountagemonfinal[i]<-str_extract(accountagemon[i]," .")}
  else accountagemonfinal[i]<-accountagemon[i]
}
accountagemonfinal<-as.numeric(accountagemonfinal)
## Extract months with pattern matching for lengthofcredithistory
lengthofcredithistorymon<-str_extract(elec_loan_train_data
                                      $lengthofcredithistory," ..")
lengthofcredithistorymonfinal<-rep(0,times=length(lengthofcredithistorymon))
for(i in 1:length(lengthofcredithistorymon))
{if((endsWith(lengthofcredithistorymon[i],"m")==TRUE))
{lengthofcredithistorymonfinal[i]<-str_extract(lengthofcredithistorymon[i]," .")
}
  else lengthofcredithistorymonfinal[i]<-lengthofcredithistorymon[i] 
}
lengthofcredithistorymonfinal<-as.numeric(lengthofcredithistorymonfinal)

#Calculate years for accountage and lengthofcredithistory

accountage<-rep(0,times=length(accountageyrs))
lengthofcredithistory<-rep(0,times=length(lengthofcredithistoryyrs))
for(i in 1:length(accountagemon))
{
  accountage[i]<-(accountageyrs[i]*12+(accountagemonfinal[i]))/12
  lengthofcredithistory[i]<-(lengthofcredithistoryyrs[i]*12
                             +(lengthofcredithistorymonfinal[i]))/12
}
elec_loan_train_data$accountage<-accountage
elec_loan_train_data$lengthofcredithistory<-lengthofcredithistory

date1<-NULL
date2<-NULL

date1<-as.Date(elec_loan_train_data$dob,"%d-%m-%Y")
date2<-as.Date(elec_loan_train_data$dob,"%m/%d/%Y")
date1[is.na(date1)]<-date2[!is.na(date2)]
elec_loan_train_data$dob<-date1
date3<-as.Date(elec_loan_train_data$financedon,"%d-%m-%Y")
date4<-as.Date(elec_loan_train_data$financedon,"%m/%d/%Y")
date3[is.na(date3)]<-date4[!is.na(date4)]
elec_loan_train_data$financedon<-date3

elec_loan_train_data$age<-difftime(Sys.Date(),elec_loan_train_data$dob)

####DO the same for test data
# Extract years with pattern matching for accountage
accountageyrs<-str_extract(elec_loan_test_data$accountage,".")
accountageyrs<-as.numeric(accountageyrs)
# Extract years with pattern matching for lengthofcredithistory
lengthofcredithistoryyrs<-str_extract(elec_loan_test_data
                                      $lengthofcredithistory,".")
lengthofcredithistoryyrs<-as.numeric(lengthofcredithistoryyrs)
## Extract months with pattern matching for accountage
accountagemon<-str_extract(elec_loan_test_data$accountage," ..")
accountagemonfinal<-rep(0,times=length(accountagemon))
for(i in 1:length(accountagemon))
{if((endsWith(accountagemon[i],"m")==TRUE))
{accountagemonfinal[i]<-str_extract(accountagemon[i]," .")}
  else accountagemonfinal[i]<-accountagemon[i]
}
accountagemonfinal<-as.numeric(accountagemonfinal)
## Extract months with pattern matching for lengthofcredithistory
lengthofcredithistorymon<-str_extract(elec_loan_test_data
                                      $lengthofcredithistory," ..")
lengthofcredithistorymonfinal<-rep(0,times=length(lengthofcredithistorymon))
for(i in 1:length(lengthofcredithistorymon))
{if((endsWith(lengthofcredithistorymon[i],"m")==TRUE))
{lengthofcredithistorymonfinal[i]<-str_extract(lengthofcredithistorymon[i]," .")
}
  else lengthofcredithistorymonfinal[i]<-lengthofcredithistorymon[i] 
}
lengthofcredithistorymonfinal<-as.numeric(lengthofcredithistorymonfinal)

#Calculate years for accountage and lengthofcredithistory

accountage<-rep(0,times=length(accountageyrs))
lengthofcredithistory<-rep(0,times=length(lengthofcredithistoryyrs))
for(i in 1:length(accountagemon))
{
  accountage[i]<-(accountageyrs[i]*12+(accountagemonfinal[i]))/12
  lengthofcredithistory[i]<-(lengthofcredithistoryyrs[i]*12
                             +(lengthofcredithistorymonfinal[i]))/12
}
elec_loan_test_data$accountage<-accountage
elec_loan_test_data$lengthofcredithistory<-lengthofcredithistory

date1<-NULL
date2<-NULL

date1<-as.Date(elec_loan_test_data$dob,"%d-%m-%Y")
date2<-as.Date(elec_loan_test_data$dob,"%m/%d/%Y")
date1[is.na(date1)]<-date2[!is.na(date2)]
elec_loan_test_data$dob<-date1
date3<-as.Date(elec_loan_test_data$financedon,"%d-%m-%Y")
date4<-as.Date(elec_loan_test_data$financedon,"%m/%d/%Y")
date3[is.na(date3)]<-date4[!is.na(date4)]
elec_loan_test_data$financedon<-date3

elec_loan_test_data$age<-difftime(Sys.Date(),elec_loan_test_data$dob)


rm(accountage,accountagemon,accountagemonfinal,accountageyrs,i
   ,lengthofcredithistory,lengthofcredithistorymon
   ,lengthofcredithistorymonfinal,lengthofcredithistoryyrs,date1,date2,date3,date4)
# Step 3.3: verify how the data is distributed
summary(elec_loan_train_data)

# Step 3.4: To check which variables can converted into factors
unique(elec_loan_train_data$modelversioncode)
unique(elec_loan_train_data$showroomid)
unique(elec_loan_train_data$typeofemployement)
unique(elec_loan_train_data$residencestate)
unique(elec_loan_train_data$waspriorcustomer)
unique(elec_loan_train_data$creditbureauriskassessment)
unique(elec_loan_train_data$delinquentaccountsinlastyear)
unique(elec_loan_train_data$numberofccsopenedlastyear)

# Step 3.5: Convert columns into factors 
elec_loan_train_data$stimulusrecipient<-as.factor(elec_loan_train_data
                                                  $stimulusrecipient)
elec_loan_train_data$typeofemployement<-as.factor(elec_loan_train_data
                                                  $typeofemployement)
elec_loan_train_data$repossessed<-as.factor(elec_loan_train_data$repossessed)
elec_loan_train_data$creditbureauriskassessment<-as.factor(elec_loan_train_data
                                                    $creditbureauriskassessment)
elec_loan_train_data$modelversioncode<-as.factor(elec_loan_train_data$modelversioncode)

# Step 4: Plot a correlation matrix plot
elec_loan_train_data[,-c(1,3,4,8)]%>%select_if(is.numeric)%>%cor()%>%corrplot()
# NOTE: %>%select(-c(1,3,8)) or %>%select(-c(tid,residencestate,showroomid)) 
# is not working in my machine because MASS library was loaded first so a 
# different kind of select function pops up.

# Step 5: Build a Logistic Regression model on train data set.
basicLRmodel<-glm(repossessed~.-tid
,family=binomial,data=elec_loan_train_data)
# Step 6: review the result of summary 
summary(basicLRmodel)
# Step 7: Verify the pseudo R2
LogRegR2(basicLRmodel)

# Step 8:examine the coefficients correctly using the odds ratio
coefsexp<-coef(basicLRmodel)%>%exp()%>%round(2)
coefsexp

# Step 9: based on the odds ratio we figure out some insignificant columns and 
#remove them and build model
basicLRmodel2<-glm(repossessed~.-tid-dob-financedon-showroomid--costofvehicle-dob-currentbalance
                   -approvedamount-age
                  ,family=binomial,data=elec_loan_train_data)

summary(basicLRmodel2)
LogRegR2(basicLRmodel2)
coefsexp2<-coef(basicLRmodel2)%>%exp()%>%round(2)
coefsexp2

#Step 10- build a new model
basicLRmodel3<-stepAIC(basicLRmodel2,trace=0)
#Step2 Review the new model-output sample is attached below for your own reference

summary(basicLRmodel3)

#Step 11 Save the new formula for the new model ,this will be needed if you do validations.
formulaLogit<-as.formula(summary(basicLRmodel3)$call)
formulaLogit
#Step 12 Examine the odds ratios to check for changes
coefexp2<-coef(basicLRmodel3)%>%exp()%>%round(2)
coefexp2
#Step 13 Examine the new model's pseudo Rsquare.The results show that model tuning
#did not negatively affect the explanatory power of the model.
LogRegR2(basicLRmodel3)

#Step 14 creation of prediction columns
elec_loan_train_data$predFullProb<-predict(basicLRmodel2,type="response",
                                  na.action=na.exclude)
elec_loan_train_data$predFull<-predict(basicLRmodel2,type="response",na.action=na.exclude)
elec_loan_train_data$predFull<-ifelse(elec_loan_train_data$predFull>0.5,1,0)

#Step 15 Build the confusion matrix 
elec_loan_train_data$predFull<-as.factor(elec_loan_train_data$predFull)
elec_loan_train_data$repossessed<-as.factor(elec_loan_train_data$repossessed)

confusionMatrixfull<-confusionMatrix(elec_loan_train_data$repossessed,elec_loan_train_data$predFull)

#Step 16 Visual representation of output 

fourfoldplot(confusionMatrixfull$table,color=c('red','green'),conf.level = 0,
             margin=1,main="Confusion Matrix of the Full Model")

payoffMatrix <- data.frame(threshold = seq(from = 0.1, to = 0.5, by = 0.1),
                           payoff = NA, accuracy=NA)
payoffMatrix #check the matrix to make sure it is set up correctly
# Step 17 - build confusion matrices in an iterative manner with varying thresholds and
# Step 18 Calculate the payoff and save it to the corresponding row created
for(i in 1:length(payoffMatrix$threshold)) {
  elec_loan_train_data$pred <- predict(basicLRmodel3, type = "response", na.action = na.exclude)
  elec_loan_train_data$pred <- ifelse(elec_loan_train_data$pred > payoffMatrix$threshold[i] , 1,
                             0)
  elec_loan_train_data$pred <- as.factor(elec_loan_train_data$pred)
  confMatrix <- confusionMatrix(elec_loan_train_data$repossessed, elec_loan_train_data$pred)
  payoffMatrix$payoff[i] <- confMatrix$table[1,1]*1000 + confMatrix$table[2,1]*(-250)
  payoffMatrix$accuracy[i] <- as.numeric(confMatrix$overall[1])
}

# Step 19 - Review the results – output sample is attached below for your own reference.
# Typically, we try to choose the highest payoff from the output to maximize profits
# and/or minimize losses as well as have good accuracy – i.e., a model that is both useful
# and good.
payoffMatrix

library(boot)

# Step 20 Load the boot library and setup a run for a cross validation with the threshold that we
#chose earlier (0.4). The idea here is to verify if the model has stability when the data is split.
# Let us write a quick function to make easy work of this in case we need to use it several times.
costAcc <- function(r, pi = 0) {
  pi <- ifelse(pi > 0.5, 1, 0)
  cm <- confusionMatrix(as.factor(r), as.factor(pi))
  acc <- as.numeric(cm$overall[1])
  return
  return(acc)
}
# Step 21 - Recalculate the cross validated accuracy with the cv.glm function and review. We can
#see that our results for the training set are very comparable verifying the stability of our chosen
#model. K=6 refers to 6 sub samples being used here. You can choose to increase it for more rigor.
# the more you increase K, the more sub-samples are taken from the data.
set.seed(534381)
cv.glm(elec_loan_train_data, basicLRmodel3, cost = costAcc, K = 3)$delta[1]




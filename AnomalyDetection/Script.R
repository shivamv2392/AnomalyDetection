####################################################
# set woking directory
####################################################
setwd("E:/AnomalyDetection/KaggleAnomalyDetection")

####################################################
# read training data and label
####################################################

data_train <- read.csv("E:/AnomalyDetection/KaggleAnomalyDetection/german.adcg.tr", header = FALSE, sep = " ", stringsAsFactors = FALSE)
str(data)

data_label <- read.csv("E:/AnomalyDetection/KaggleAnomalyDetection/german.adcg.tr.label", header = TRUE, sep = ",")
str(data_label)

data_test <- read.csv("E:/AnomalyDetection/KaggleAnomalyDetection/german.adcg.tt", header = FALSE, sep = " ")
str(data_test)

####################################################
# assign column names
####################################################

names(data_train) <- c("AccountStatus"
   , "DurationMonths"
   , "CreditHistory"
   , "Purpose"
   , "CreditAmount"
   , "SavingAccount"
   , "EmploymentSince"
   , "InstallmentRate"
   , "StatusSex"
   , "OtherDebtors"
   , "ResidentSince"
   , "Property"
   , "Age"
   , "InstallmentPlans"
   , "Housing"
   , "NumofExistingCreditCard"
   , "Job"
   , "PeopleLiableMaintenace"
   , "Telephone"
   , "ForeignWorker")

str(data_train)

names(data_test) <- c("AccountStatus"
   , "DurationMonths"
   , "CreditHistory"
   , "Purpose"
   , "CreditAmount"
   , "SavingAccount"
   , "EmploymentSince"
   , "InstallmentRate"
   , "StatusSex"
   , "OtherDebtors"
   , "ResidentSince"
   , "Property"
   , "Age"
   , "InstallmentPlans"
   , "Housing"
   , "NumofExistingCreditCard"
   , "Job"
   , "PeopleLiableMaintenace"
   , "Telephone"
   , "ForeignWorker")

str(data_train)

str(data_test)

data_test$Prediction <- NA #adding label in test dataset

data_train <- cbind(data_train, data_label) #combine both training data and label

data_train$Prediction <- as.factor(data_train$Prediction)

data_train_x <- data_train

str(data_train_x)

######################################################
# BROUTA Package
######################################################
#install.packages("Boruta")
str(data_train)
library(Boruta)
convert <- c(1, 3, 4, 6, 7, 9, 10, 12, 14, 15, 17, 19, 20, 22)
data_train[, convert] <- data.frame(apply(data_train[, convert], 2, as.factor)) #2 - processing along columns
set.seed(123)
xyz <- Boruta(Prediction ~ . - Id, data = data_train, doTrace = 2)
print(xyz)
final.boruta <- TentativeRoughFix(xyz)
print(final.boruta)
boruta.df <- attStats(final.boruta)
print(boruta.df)

######################################
#AccountStatus				Confirmed
#DurationMonths				Confirmed
#CreditHistory				Confirmed
#Purpose					 Rejected
#CreditAmount				Confirmed
#SavingAccount				Confirmed
#EmploymentSince			Confirmed
#InstallmentRate			 Rejected
#StatusSex					Confirmed
#OtherDebtors				Confirmed
#ResidentSince				 Rejected
#Property					Confirmed
#Age						Confirmed
#InstallmentPlans			Confirmed
#Housing					Confirmed
#NumofExistingCreditCard	 Rejected
#Job						 Rejected
#PeopleLiableMaintenace		 Rejected
#Telephone					 Rejected
#ForeignWorker				Confirmed
#####################################

######################################################
# plotting graph
######################################################
library(ggplot2)
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$AccountStatus), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$CreditHistory), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$Purpose), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$SavingAccount), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$EmploymentSince), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$StatusSex), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$OtherDebtors), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$Property), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$InstallmentPlans), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$Housing), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$Job), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$Telephone), position = "dodge")
ggplot(data_train, aes(data_train$Prediction)) + geom_bar(aes(fill = data_train$ForeignWorker), position = "dodge")
############################################################
# Finding correlation between categorical features and label
############################################################
# p-value
# null hypothesis, the variable are independent
# p-value = 0, means the categorical variable are dependent 
# p-value = 1, means the categorical variable are independent
# p-value < 0.05, reject the null hypothesis
# p-value > 0.05, vice-versa
corr <- table(data_train$AccountStatus, data_train$Prediction) #keep
chisq.test(corr)
# since p-value is less than 0.05, the label and feature are dependent or correlated. We need to keep those values for analysis.
corr <- table(data_train$CreditHistory, data_train$Prediction) #keep
chisq.test(corr)
corr <- table(data_train$Purpose, data_train$Prediction) #keep
chisq.test(corr)
corr <- table(data_train$SavingAccount, data_train$Prediction) #keep
chisq.test(corr)
corr <- table(data_train$EmploymentSince, data_train$Prediction) #keep
chisq.test(corr)
corr <- table(data_train$StatusSex, data_train$Prediction) #keep
chisq.test(corr)
corr <- table(data_train$OtherDebtors, data_train$Prediction) #keep, boderline
chisq.test(corr)
corr <- table(data_train$Property, data_train$Prediction) #keep
chisq.test(corr)
corr <- table(data_train$InstallmentPlans, data_train$Prediction) #keep
chisq.test(corr)
corr <- table(data_train$Housing, data_train$Prediction) #keep
chisq.test(corr)
corr <- table(data_train$Job, data_train$Prediction) #ignore
chisq.test(corr)
corr <- table(data_train$Telephone, data_train$Prediction) #ignore
chisq.test(corr)
corr <- table(data_train$ForeignWorker, data_train$Prediction) #keep
chisq.test(corr)
#############################################################
# Finding correlation between continous features and label
#############################################################
results = aov(Age ~ Prediction, data = data_train) #keep
summary(results)
results = aov(CreditAmount ~ Prediction, data = data_train) #keep
summary(results)
results = aov(InstallmentRate ~ Prediction, data = data_train) #ignore
summary(results)
results = aov(ResidentSince ~ Prediction, data = data_train) #ignore
summary(results)
result = aov(NumofExistingCreditCard ~ Prediction, data = data_train) #ignore
summary(results)
result = aov(PeopleLiableMaintenace ~ Prediction, data = data_train) #ignore
summary(results)
result = aov(DurationMonths ~ Prediction, data = data_train) #ignore
summary(results)
###############################################################
# feature engineering to create some new variables
###############################################################
data_train$StatusSex[which(data_train$StatusSex == "A91")] = "male,divorced/seperated"
data_train$StatusSex[which(data_train$StatusSex == "A92")] = "female,divorced/separated/married"
data_train$StatusSex[which(data_train$StatusSex == "A93")] = "male,single"
data_train$StatusSex[which(data_train$StatusSex == "A94")] = "male,married/widowed"
data_train$StatusSex[which(data_train$StatusSex == "A95")] = "female,single"
table(data_train$StatusSex)
data_train$Status <- gsub(".*,", "", data_train$StatusSex)
data_train$Sex <- gsub(",.*", "", data_train$StatusSex)
corr <- table(data_train$Sex, data_train$Prediction) # can keep
chisq.test(corr)
corr <- table(data_train$Status, data_train$Prediction) #keep
chisq.test(corr)
hist(data_train$Age)
data_train$Age_grouping <- cut(data_train$Age, breaks = c(0, 20, 40, 60, 80, 100), right = FALSE)
hist(table(data_train$Age_grouping))
table(data_train$Age_grouping)
str(data_train)
data_train$Age_grouping <- as.character(data_train$Age_grouping)
corr <- table(data_train$Age_grouping, data_train$Prediction) #ignore
chisq.test(corr)
######################################################
# Variables to keep and remove
######################################################
######### CONTINUOUS #########
#Age# keep
#CreditAmount# keep
######### CONTINUOUS #########
AccountStatus,
	Age,
	CreditAmount,
	CreditHistory,
	Housing,
	EmploymentSince,
	InstallmentPlans,
	StatusSex
######## CATEGORICAL #########
#AccountStatus# keep
#CreditHistory# keep 
#Purpose# keep
#SavingAccount# keep
#EmploymentSince# keep
#StatusSex# keep
#Property# keep
#InstallmentPlans# keep
#Housing# keep
#ForeignWorker# keep
######## CATEGORICAL #########
########################################################################
# Find correlation between categorical and categorical feature variables
########################################################################
# p-value
# null hypothesis, the variable are independent
# p-value = 0, means the categorical variable are dependent 
# p-value = 1, means the categorical variable are independent
# p-value < 0.05, reject the null hypothesis
# p-value > 0.05, vice-versa
# since p-value is less than 0.05, the label and feature are dependent or correlated. We need to keep those values for analysis.
str(data_train)
data_train_cat <- data_train[, c("AccountStatus"
, "CreditHistory"
, "Purpose"
, "SavingAccount"
, "EmploymentSince"
, "StatusSex"
, "Property"
, "InstallmentPlans"
, "Housing"
, "ForeignWorker")]
str(data_train_cat)
corr <- table(data_train$CreditHistory, data_train$AccountStatus) #ignore
chisq.test(corr)
corr <- table(data_train$Purpose, data_train$AccountStatus) #ignore
chisq.test(corr)
corr <- table(data_train$SavingAccount, data_train$AccountStatus) #ignore
chisq.test(corr)
corr <- table(data_train$EmploymentSince, data_train$AccountStatus) #ignore
chisq.test(corr)
corr <- table(data_train$StatusSex, data_train$AccountStatus) #keep
chisq.test(corr)
corr <- table(data_train$Property, data_train$AccountStatus) #keep
chisq.test(corr)
corr <- table(data_train$InstallmentPlans, data_train$AccountStatus) #keep
chisq.test(corr)
corr <- table(data_train$Housing, data_train$AccountStatus) #keep
chisq.test(corr)
corr <- table(data_train$ForeignWorker, data_train$AccountStatus) #keep
chisq.test(corr)
##The null hypothesis of the Chi-Square test is that no relationship exists on the categorical variables in the population
#####################################################################
# Find correlation between continous and continous feature variables
#####################################################################
data_train_conti <- data_train[, c("Age", "CreditAmount")]
str(data_train_conti)
cor(data_train_conti) # non of the continous variables are highly correlated
#####################################################################
# Find correlation between continous and categorical feature variables
#####################################################################

######################################
#AccountStatus				Confirmed
#DurationMonths				Confirmed
#CreditHistory				Confirmed
#CreditAmount				Confirmed
#SavingAccount				Confirmed
#EmploymentSince			Confirmed
#StatusSex					Confirmed
#OtherDebtors				Confirmed
#Property					Confirmed
#Age						Confirmed
#InstallmentPlans			Confirmed
#Housing					Confirmed
#ForeignWorker				Confirmed
#####################################


########################################################################
# Applying model NaiveBayes for classification
########################################################################

#install.packages("e1071")
library(naivebayes)


str(data_train)

confirmed_var <- c( "AccountStatus"
, "DurationMonths"
, "CreditHistory"
, "CreditAmount"
, "SavingAccount"
, "EmploymentSince"
, "StatusSex"
, "OtherDebtors"
, "Property"
, "Age"
, "InstallmentPlans"
, "Housing"
, "ForeignWorker"
, "Prediction")

data_train <- data_train[, confirmed_var]

str(data_train)
str(data_test)

data_test$Id <- data_train_x$Id

nrow(data_test)
nrow(data_train)
library(e1071)
model <- naiveBayes(Prediction ~ ., data = data_train)

data_test$Prediction <- predict(model, data_test)

answer <- data.frame(data_test$Prediction)

write.csv(answer, file = "answer.csv")

names(answer) <- c("Prediction")

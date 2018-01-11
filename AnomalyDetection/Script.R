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

data_train$Prediction <- as.factor(data_train$Prediction)

str(data_train)

str(data_test)

data_test$Prediction <- NA #adding label in test dataset

data_train <- cbind(data_train, data_label) #combine both training data and label

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
# p-value = 0, means the categorical variable are independent
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

#AccountStatus#
#DurationMonths#
#CreditHistory#
#Purpose#
#CreditAmount#
#SavingAccount#
#EmploymentSince#
#InstallmentRate#
#StatusSex#
#OtherDebtors#
#ResidentSince#
#Property#
#Age#
#InstallmentPlans#
#Housing#
#NumofExistingCreditCard#
#Job#
#PeopleLiableMaintenace#
#Telephone#
#ForeignWorker#

########################################################################
# Find correlation between categorical and categorical feature variables
########################################################################

str(data_train)

data_train_cat <- data_train[, c("Job"
, "Telephone"
, "ForeignWorker"
, "Age_grouping"
, "OtherDebtors"
,"InstallmentPlans")]

str(data_train_cat)

corr <- table(data_train$Age_grouping, data_train$ForeignWorker) #keep

chisq.test(corr)

corr <- table(data_train$Telephone, data_train$ForeignWorker) #keep

chisq.test(corr)

corr <- table(data_train$Telephone, data_train$Age_grouping) #keep

chisq.test(corr)

corr <- table(data_train$Telephone, data_train$Age_grouping) #keep

chisq.test(corr)

corr <- table(data_train$Telephone, data_train$Age_grouping) #keep

chisq.test(corr)

##The null hypothesis of the Chi-Square test is that no relationship exists on the categorical variables in the population

#####################################################################
# Find correlation between continous and continous feature variables
#####################################################################

data_train_conti <- data_train[, c("ResidentSince"
, "NumofExistingCreditCard"
, "PeopleLiableMaintenace")]

str(data_train_conti)

cor(data_train_conti) # non of the continous variables are highly correlated

#####################################################################
# Find correlation between continous and continous feature variables
#####################################################################



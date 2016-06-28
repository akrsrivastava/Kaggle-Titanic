#k-NN prediction of Kaggle Titanic Dataset
library(stringr)
library(dplyr)
library(class)
library(gmodels)
train <- read.csv("D:/amit/R/Kaggle/Titanic/train.csv", stringsAsFactors=FALSE)
test <- read.csv("D:/amit/R/Kaggle/Titanic/test.csv", stringsAsFactors=FALSE)


#Section 1  -------
#In the first Section we will use Train data to build and test kNN model



train_org <- train
test_org <- test

str(train)

#Convert Survived to factor
train$Survived <- factor(train$Survived,levels=c("0","1"),labels = c(0,1))
str(train$Survived)

prop.table(table(train$Survived))

summary(train)




#There are 177 NAs in Age. Lets impute
#FOr NAs of men we will use the mean of men's Age to impute

#Lets first pick up all mens with Mr. in there title
mr <- train[which(str_detect(train$Name,"Mr\\.")),]
mr
summary(mr$Age)
str(mr$Age)
hist(mr$Age)
#This is right skewed so we will take median as the age to be imputed
mr_median <- median(mr$Age,na.rm=TRUE) #29.50
mr[is.na(mr$Age),]$Age <- mr_median
summary(mr$Age)


#Now Mrs.
mrs <- train[which(str_detect(train$Name,"Mrs\\.")),]
mrs
summary(mrs$Age) #17 NA's
str(mrs$Age)
hist(mrs$Age)
#This is normal so we will take mean as the age to be imputed
mrs_mean <- mean(mrs$Age,na.rm=TRUE) #35.9
mrs[is.na(mrs$Age),]$Age <- mrs_mean

summary(mrs$Age) #Now 0 NAs


#Now Miss.
miss <- train[which(str_detect(train$Name,"Miss\\.")),]
miss
summary(miss$Age) #36 NA's
str(miss$Age)
hist(miss$Age)
#This is ~normal so we will take mean as the age to be imputed
miss_mean <- mean(miss$Age,na.rm=TRUE) #21.77
miss[is.na(miss$Age),]$Age <- miss_mean

summary(miss$Age) #Now 0 NAs

#Now Master.
master <- train[which(str_detect(train$Name,"Master\\.")),]
master
summary(master$Age) #4 NA's
str(master$Age)
hist(master$Age)
#This is skewed so we will take median as the age to be imputed
master_median <- median(master$Age,na.rm=TRUE) #3.5
master[is.na(master$Age),]$Age <- master_median
summary(master$Age) #Now 0 NAs

nrow(mr)+nrow(master)+nrow(miss)+nrow(mrs)
#This is 868 while row count in train is 891
#So 23 names do not have a title
#We will impute mean of entire train dataset
combined=rbind(mr,mrs,miss,master)
nrow(combined)

others <- train %>%
            filter(!(PassengerId %in% combined$PassengerId))

nrow(others)

summary(others) #1 NA
head(others$Name)
#There's one NA still left in others. We will impute that with the mean of train
others[which(is.na(others$Age)),]$Age=mean(train$Age,na.rm=TRUE)
summary(others) #Now no NA

# Time to join back all dataset
nrow(mr)+nrow(mrs)+nrow(master)+nrow(miss)+nrow(others)==nrow(train) # True
train <- rbind(mr,mrs,master,miss,others)
#Integrity checks
nrow(train[which(str_detect(train$Name,"Mrs\\.")),])
nrow(train_org[which(str_detect(train$Name,"Mrs\\.")),])
nrow(train[which(str_detect(train$Name,"Mr\\.")),])
nrow(train_org[which(str_detect(train$Name,"Mr\\.")),])
nrow(train[which(str_detect(train$Name,"Miss\\.")),])
nrow(train_org[which(str_detect(train$Name,"Miss\\.")),])
nrow(train[which(str_detect(train$Name,"Master\\.")),])
nrow(train_org[which(str_detect(train$Name,"Master\\.")),])


#The train dataset has become sorted with mr rows then mrs rows etc
#Need to randomize

g <- runif(891,0,1)
train <- train[order(g),]
head(train)


summary(train)
#kNN works on numeric features
#Convert Sex into numeric
train$male=ifelse(train$Sex=="male",0,1)

#Convert Embarked into numeric
train$EmbarkCity <- ifelse(train$Embarked=="S",0,(ifelse(train$Embarked=="Q",1,2)))
#train$EmbarkCity =0,1,2 as per train$Embarked S,Q or C 

#Drop the ID,sex,ticket,cabin,name,embarked feature. Not sure right now if SibSp, Parch would
#be relevant features so keeping them for time being
train$PassengerId <- NULL
train$Sex <- NULL #We have the male numeric feature instead of this
train$Ticket <- NULL
train$Cabin <- NULL
train$Name <- NULL
train$Embarked <- NULL ##We have the EmbarkCity numeric feature instead of this
summary(train)


#Since the test data set does not have the Survived feature
#we will have to break the current train set into train and test set
#Find out the best model and then later on apply to the given test data
#Create the train and test labels
train_labels <- train[1:700,]$Survived
test_labels <- train[701:891,]$Survived
train_complete_lables <- train$Survived #Will use this when the entire train 
#will be used for training 

#Now drop Survived from the train set as this is the target feature
train$Survived <- NULL

#Now normalize all numeric features
minmax_normalize <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
train_n <- as.data.frame(lapply(train,minmax_normalize))

summary(train_n) 
#Prepare the knn model??
#Prepare the train and test data set
train_n_data <- train_n[1:700,]
test_n_data <- train_n[701:891,]

knn_model_pred <- knn(train=train_n_data, test=test_n_data, cl=train_labels, k=26)
CrossTable(knn_model_pred,test_labels) #36/191 errors

######Now we need to analyze and prepare the test data set
#library(stringr)
#library(dplyr)
#library(class)
#library(gmodels)


#Section 2 ---------------
#In this section we will use Test data to predict and will use entire train data to train

#train <- read.csv("D:/amit/R/Kaggle/Titanic/train.csv", stringsAsFactors=FALSE)
#test <- read.csv("D:/amit/R/Kaggle/Titanic/test.csv", stringsAsFactors=FALSE)

#str(test)

#Convert Survived to factor
#test$Survived <- factor(test$Survived,levels=c("0","1"),labels = c("Yes","No"))
#str(test$Survived)

#prop.table(table(test$Survived))

summary(test)



#There are 86 NAs in Age. Lets impute
#FOr NAs of men we will use the mean of men's Age to impute

#Lets first pick up all mens with Mr. in there title
mr <- test[which(str_detect(test$Name,"Mr\\.")),]
mr
summary(mr$Age)
str(mr$Age)
hist(mr$Age)
#This is right skewed so we will take median as the age to be imputed
mr_median <- median(mr$Age,na.rm=TRUE) #28.50
mr[is.na(mr$Age),]$Age <- mr_median
summary(mr$Age) #Now zero NAs


#Now Mrs.
mrs <- test[which(str_detect(test$Name,"Mrs\\.")),]
mrs
summary(mrs$Age) #10 NA's
str(mrs$Age)
hist(mrs$Age)
#This is normal so we will take mean as the age to be imputed
mrs_mean <- mean(mrs$Age,na.rm=TRUE) #35.9
mrs[is.na(mrs$Age),]$Age <- mrs_mean

summary(mrs$Age) #Now 0 NAs


#Now Miss.
miss <- test[which(str_detect(test$Name,"Miss\\.")),]
miss
summary(miss$Age) #14 NA's
str(miss$Age)
hist(miss$Age)
#This is ~normal so we will take mean as the age to be imputed
miss_mean <- mean(miss$Age,na.rm=TRUE) #21.77
miss[is.na(miss$Age),]$Age <- miss_mean

summary(miss$Age) #Now 0 NAs

#Now Master.
master <- test[which(str_detect(test$Name,"Master\\.")),]
master
summary(master$Age) #4 NA's
str(master$Age)
hist(master$Age)
#This is skewed so we will take median as the age to be imputed
master_median <- median(master$Age,na.rm=TRUE) #3.5
master[is.na(master$Age),]$Age <- master_median
summary(master$Age) #Now 0 NAs

nrow(mr)+nrow(master)+nrow(miss)+nrow(mrs)
#This is 411 while row count in test is 418
#So 7 names do not have a title
#We will impute mean of entire test dataset
combined=rbind(mr,mrs,miss,master)
nrow(combined)

others <- test %>%
  filter(!(PassengerId %in% combined$PassengerId))

nrow(others)

summary(others) #1 NA
head(others$Name)
#There's one NA still left in others. We will impute that with the mean of test
others[which(is.na(others$Age)),]$Age=mean(test$Age,na.rm=TRUE)
summary(others) #Now no NA

# Time to join back all dataset
nrow(mr)+nrow(mrs)+nrow(master)+nrow(miss)+nrow(others)==nrow(test) # True
test <- rbind(mr,mrs,master,miss,others)
#Integrity checks
nrow(test[which(str_detect(test$Name,"Mrs\\.")),])
nrow(test_org[which(str_detect(test$Name,"Mrs\\.")),])
nrow(test[which(str_detect(test$Name,"Mr\\.")),])
nrow(test_org[which(str_detect(test$Name,"Mr\\.")),])
nrow(test[which(str_detect(test$Name,"Miss\\.")),])
nrow(test_org[which(str_detect(test$Name,"Miss\\.")),])
nrow(test[which(str_detect(test$Name,"Master\\.")),])
nrow(test_org[which(str_detect(test$Name,"Master\\.")),])


#The test dataset has become sorted with mr rows then mrs rows etc
#Need to randomize

g <- runif(418,0,1)
test <- test[order(g),]
head(test)


summary(test)
#There is one NA in Fare
#Impute with the mean Fare
test[which(is.na(test$Fare)),]$Fare = mean(test$Fare,na.rm=TRUE)
summary(test)



#kNN works on numeric features
#Convert Sex into numeric
test$male=ifelse(test$Sex=="male",0,1)

#Convert Embarked into numeric
test$EmbarkCity <- ifelse(test$Embarked=="S",0,(ifelse(test$Embarked=="Q",1,2)))
#test$EmbarkCity =0,1,2 as per test$Embarked S,Q or C 

#Drop the ID,sex,ticket,cabin,name,embarked feature. Not sure right now if SibSp, Parch would
#be relevant features so keeping them for time being
test_PassengerId <- test$PassengerId # We need this in the reponse csv.
test$PassengerId <- NULL
test$Sex <- NULL #We have the male numeric feature instead of this
test$Ticket <- NULL
test$Cabin <- NULL
test$Name <- NULL
test$Embarked <- NULL ##We have the EmbarkCity numeric feature instead of this
summary(test)
#Create the test labels
#test_labels <- test$Survived
#Now drop Survived from the test set as this is the target feature
#test$Survived <- NULL

#Now normalize all numeric features
minmax_normalize <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
test_n <- as.data.frame(lapply(test,minmax_normalize))

summary(test_n) 

#Now prepare data for knn
#We will use entire 891 rows of train data for training 

train_final <- train_n
test_final <- test_n

knn_model_test_pred <- knn(train=train_final, test=test_final, cl=train_complete_lables, k=29)
#CrossTable(knn_model_pred,test_labels) #36/191 errors

response_df <- data.frame(PassengerID=test_PassengerId,Survived=knn_model_test_pred)
summary(response_df)
str(response_df)
write.csv(response_df,file="D:/amit/R/Kaggle/Titanic/my_solution.csv",row.names=FALSE)
#response_df_o <- response_df[order(response_df$PassengerID),]

#Titanic- Naive Bayes prediction
library(stringr)
library(dplyr)
library(class)
library(gmodels)
train <- read.csv("D:/amit/R/Kaggle/Titanic/train.csv", stringsAsFactors=FALSE)
test <- read.csv("D:/amit/R/Kaggle/Titanic/test.csv", stringsAsFactors=FALSE)

train_org <- train
test_org <- test

summary(train)


# Section 1. Remove the 177 NAs in train ---------------------------------------------
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
#-------------------

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

#Section 1 ends


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


#Naive Bayes works on categorical features

#COnvert Survived into factor
train$Survived <- factor(train$Survived,levels=c(0,1))

#Convert Sex into factor
train$male=as.factor(train$Sex)

#Convert Embarked into numeric
train$EmbarkCity <- ifelse(train$Embarked=="S",0,(ifelse(train$Embarked=="Q",1,2)))
#train$EmbarkCity =0,1,2 as per train$Embarked S,Q or C 



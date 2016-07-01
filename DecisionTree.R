library(C50)
library(stringr)
library(dplyr)
library(class)
library(gmodels)
library(ggplot2)
train <- read.csv("D:/amit/R/Kaggle/Titanic/train.csv", stringsAsFactors=FALSE)
test <- read.csv("D:/amit/R/Kaggle/Titanic/test.csv", stringsAsFactors=FALSE)
train_org <- train
test_org <- test
#Combine the two data frames. But first use an indicator to check which row comes from which dataset

#test$IsTest <- TRUE
#train$IsTest <- FALSE

#Add Survived Col to test
#test$Survived <- NA
#combi <- bind_rows(train,test)

#summary(combi)

#To Analyse Training data we will use just the train set
#combi_train <- train_org

#summary(combi_train)

summary(train)
#Convert Survived to Factor
train$Survived=as.factor(train$Survived)

head(train$Name,20)
prop.table(table(train$Survived))*100
#61.61 did not survive 38.38% survived

#Lets check for NAs and blanks
summary(train) 
#177 NAs in Age

#Lets check for blanks in the character variables
sum(ifelse(str_trim(train$Name)=="",1,0)) #0 blanks
sum(ifelse(str_trim(train$Sex)=="",1,0)) #0 blanks
sum(ifelse(str_trim(train$Ticket)=="",1,0)) #0 blanks
sum(ifelse(str_trim(train$Cabin)=="",1,0)) #687 blanks...Too many blanks. Can not be used for model
sum(ifelse(str_trim(train$Embarked)=="",1,0)) #2 blanks...Check for imputation


#Missing value treatment for Embarked
train[which(str_trim(train$Embarked)==""),]
#These two travelled on the same ticket number and paid 80$
table(train$Embarked)
train[train$Ticket=="113572",] #Searching if anyone else travelled on the same ticket , but no
train[train$Fare==80,] #Searching if anyone else paid 80$, but no
ggplot(train) +geom_boxplot(aes(x=factor(Embarked),y=Fare)) + facet_wrap("Pclass") #See Analysis Doc
train[which(str_trim(train$Embarked)==""),]$Embarked <- "C"


#Extract Titles
NameSplit <- strsplit(train[,"Name"],("[.,]"))
Title <-sapply(NameSplit, "[[", 2) 
Title <- as.data.frame(Title)
train$Title <- (str_trim(Title$Title))

table(train$Title)

#Check if there is any special feature relevant to the royalities
royalty <- train %>%
  filter (!Title %in% c("Mr","Mrs","Miss","Master"))

prop.table(table(royalty$Survived))
prop.table(table(train$Survived))

#44% of royalties survived as compared to 39% overall survival rate
#There is a very slight advantage of being royalty

prop.table(table(train$Title,train$Survived))

#TOo many Title. Lets combine the royalties to one group
#levels(train$Title) <- c(levels(train$Title),"Royalty")
train[!train$Title %in% c("Mr","Mrs","Miss","Master"),]$Title <- "Royalty"
table(train$Title)
prop.table(table(train$Title,train$Survived))*100
ggplot(train) + geom_bar(aes(x=Title,stat="identity", fill=Survived))
#CLearly Mr has significant disadvantage

#177 rows in Age is NA
#We will impute the baisis of Title in the name
table(train$Title, is.na(train$Age))
# NAs for Mr.
train[train$Title=="Mr" & is.na(train$Age),"Age"] <- mean(train[train$Title=="Mr",]$Age,na.rm=TRUE)      

# NAs for Miss.
train[train$Title=="Miss" & is.na(train$Age),"Age"] <- mean(train[train$Title=="Miss",]$Age,na.rm=TRUE)      

# NAs for Mrs.
train[train$Title=="Mrs" & is.na(train$Age),"Age"] <- mean(train[train$Title=="Mrs",]$Age,na.rm=TRUE)      

# NAs for Royalty.
train[train$Title=="Royalty" & is.na(train$Age),"Age"] <- mean(train[train$Title=="Royalty",]$Age,na.rm=TRUE)   

# NAs for Master.
train[train$Title=="Master" & is.na(train$Age),"Age"] <- mean(train[train$Title=="Master",]$Age,na.rm=TRUE)      

## Imputation Done. No more NAs in train
summary(train)




#Title wise survival rate
ggplot(train) + geom_bar(aes(x=Title,fill=Survived),position="fill") 
ggplot(train) + geom_bar(aes(x=Title,fill=Survived),position="fill") + facet_wrap("Sex")
#Age wise survival rate#
prop.table(table(train$Age,train$Survived))
hist(train$Age)



#CAtegorize Age into different bins 0-20 20-40 40-60 60-80
fn_agecat <- function (x){
  ifelse((x>=0 & x<=20) ,"A",
    (ifelse((x>40 & x<=60), "B",
     (ifelse((x>60 & x<=80),"C","D")
    )
    )
  )
  )
}

train$AgeCategory <- fn_agecat(train$Age)
train$AgeCategory <- as.factor((train$AgeCategory))

prop.table(table(train$AgeCategory,train$Survived),1)*100

ggplot(train) + geom_bar(aes(x=AgeCategory,fill=Survived),position="fill")

ggplot(train) + geom_bar(aes(x=AgeCategory,fill=Survived),position="fill") + facet_wrap("Sex")


#Now analyze PClass
prop.table(table(train$Pclass,train$Survived),1)*100
#63 % of the first class Passenger survived
ggplot(train) + geom_bar(aes(x=Pclass,fill=Survived),position="fill")
ggplot(train) + geom_bar(aes(x=Pclass,fill=Survived),position="fill") + facet_wrap("Sex")


#Lets prepare a decision tree first before analyzing more
summary(train)
#Prepare train for modelling
train_model <- train
train_model$PassengerId <- NULL
#For the moment, I will use Age instead of AgeCategory to minimize information loss
#So will drop AgeCategory
train_model$AgeCategory <- NULL
#Drop NAme
train_model$Name <- NULL
#Drop Ticket
train_model$Ticket <- NULL
#Drop Cabin
train_model$Cabin <- NULL

#Convert Sex to factor
train_model$Sex <- factor(train_model$Sex)
#Convert Pclass to factor
train_model$Pclass <- factor(train_model$Pclass)

#Convert Embarked to factor
train_model$Embarked <- factor(train_model$Embarked)

#Convert SibSp to factor
train_model$SibSp <- factor(train_model$SibSp)

#Convert Parch to factor
train_model$Parch <- factor(train_model$Parch)

#Convert Title to factor
train_model$Title <- factor(train_model$Title)

#Divide train into train and test
trainingdata <- train_model[1:750,]
testdata <- train_model[751:891,]
#Drop Survived from trainingdata
trainingdata_survived <- trainingdata$Survived
trainingdata$Survived <- NULL


#Model 1 : Train data divided into train and test. C5.0 Model with Zero Boost
predict_model <- C5.0(trainingdata,trainingdata_survived,trials=1) #Not using boosting
predict_model
summary(predict_model)
plot(predict_model)

#Try on the test data
test_predicted_survived <- predict(predict_model,testdata, type="class")
CrossTable(testdata$Survived,test_predicted_survived) #14% Error rate

#Model 2 : Train data divided into train and test. C5.0 Model with 10 Boost
predict_model <- C5.0(trainingdata,trainingdata_survived,trials=10) #Not using boosting
predict_model
summary(predict_model)
plot(predict_model)

#Try on the test data
test_predicted_survived <- predict(predict_model,testdata, type="class")
CrossTable(testdata$Survived,test_predicted_survived) #14% Error rate

#Check if AgeCategory will improve the model
#Prepare train for modelling
train_model <- train
train_model$PassengerId <- NULL
#For the moment, I will use AgeCategory instead of Age to check impact on model accuracy
#So will drop AgeCategory
train_model$Age <- NULL
#Drop NAme
train_model$Name <- NULL
#Drop Ticket
train_model$Ticket <- NULL
#Drop Cabin
train_model$Cabin <- NULL

#Convert Sex to factor
train_model$Sex <- factor(train_model$Sex)
#Convert Pclass to factor
train_model$Pclass <- factor(train_model$Pclass)

#Convert Embarked to factor
train_model$Embarked <- factor(train_model$Embarked)

#Convert SibSp to factor
train_model$SibSp <- factor(train_model$SibSp)

#Convert Parch to factor
train_model$Parch <- factor(train_model$Parch)

#Convert Title to factor
train_model$Title <- factor(train_model$Title)

#Divide train into train and test
trainingdata <- train_model[1:750,]
testdata <- train_model[751:891,]
#Drop Survived from trainingdata
trainingdata_survived <- trainingdata$Survived
trainingdata$Survived <- NULL

#Model 1 : Train data divided into train and test. C5.0 Model with Zero Boost
predict_model <- C5.0(trainingdata,trainingdata_survived,trials=1) #Not using boosting
predict_model
summary(predict_model)
plot(predict_model)

#Try on the test data
test_predicted_survived <- predict(predict_model,testdata, type="class")
CrossTable(testdata$Survived,test_predicted_survived) #14% Error rate

#Model 2 : Train data divided into train and test. C5.0 Model with 10 Boost
predict_model <- C5.0(trainingdata,trainingdata_survived,trials=10) #Not using boosting
predict_model
summary(predict_model)
plot(predict_model)

#Try on the test data
test_predicted_survived <- predict(predict_model,testdata, type="class")
CrossTable(testdata$Survived,test_predicted_survived) #14% Error rate


#Now preparing the test data from the test.csv file
#We will match it with train as much as possible
str(test)
summary(test) #Age has 86 NAs, Fare has 1 NA, 

#Check for blanks in character variables
sum(ifelse(str_trim(test$Name)=="",1,0)) #0 blanks
sum(ifelse(str_trim(test$Sex)=="",1,0)) #0 blanks
sum(ifelse(str_trim(test$Ticket)=="",1,0)) #0 blanks
sum(ifelse(str_trim(test$Cabin)=="",1,0)) #327 blanks. Too many. Needs to be dropped from model
sum(ifelse(str_trim(test$Embarked)=="",1,0)) #0 blanks

#For Age we will impute similar to train
#Extract Titles
NameSplit <- strsplit(test[,"Name"],("[.,]"))
Title <-sapply(NameSplit, "[[", 2) 
Title <- as.data.frame(Title)
test$Title <- (str_trim(Title$Title))

#TOo many Title. Lets combine the royalties to one group
#levels(train$Title) <- c(levels(train$Title),"Royalty")
test[!test$Title %in% c("Mr","Mrs","Miss","Master"),]$Title <- "Royalty"
table(test$Title)

#86 rows in Age is NA
#We will impute the baisis of Title in the name. We will impute with mean of the train
table(test$Title, is.na(test$Age))
# NAs for Mr.
test[test$Title=="Mr" & is.na(test$Age),"Age"] <- mean(train[train$Title=="Mr",]$Age,na.rm=TRUE)      

# NAs for Miss.
test[test$Title=="Miss" & is.na(test$Age),"Age"] <- mean(train[train$Title=="Miss",]$Age,na.rm=TRUE)      

# NAs for Mrs.
test[test$Title=="Mrs" & is.na(test$Age),"Age"] <- mean(train[train$Title=="Mrs",]$Age,na.rm=TRUE)      

# NAs for Royalty.
test[test$Title=="Royalty" & is.na(test$Age),"Age"] <- mean(train[train$Title=="Royalty",]$Age,na.rm=TRUE)   

# NAs for Master.
test[test$Title=="Master" & is.na(test$Age),"Age"] <- mean(train[train$Title=="Master",]$Age,na.rm=TRUE)      

table(test$Title, is.na(test$Age))
#No NAs remaining

test$AgeCategory <- fn_agecat(test$Age)
test$AgeCategory <- as.factor((test$AgeCategory))


#Prepare test for model testing
test_model <- test

#For the moment, I will use Age instead of Age Category to check impact on model accuracy

#So will drop Age
test_model$Age <- NULL

#Drop NAme
test_model$Name <- NULL

#Drop Ticket
test_model$Ticket <- NULL

#Drop CabinL
test_model$Cabin <- NULL

#Convert Sex to factor
test_model$Sex <- factor(test_model$Sex)

#Convert Pclass to factor
test_model$Pclass <- factor(test_model$Pclass)

#Convert Embarked to factor
test_model$Embarked <- factor(test_model$Embarked)

#Convert SibSp to factor
test_model$SibSp <- factor(test_model$SibSp)

#Convert Parch to factor
test_model$Parch <- factor(test_model$Parch)

#Convert Title to factor
test_model$Title <- factor(test_model$Title)

#Divide train into train and test
trainingdata <- train_model
testdata <- test_model

#Drop Survived from trainingdata
trainingdata_survived <- trainingdata$Survived
trainingdata$Survived <- NULL

test_PassengerId <- test$PassengerId # We need this in the reponse csv.
test$PassengerId <- NULL

#Model 1 : Train data divided into train and test. C5.0 Model with Zero Boost
predict_model_1 <- C5.0(trainingdata,trainingdata_survived,trials=1) #Not using boosting


#Try on the test data
test_predicted_survived <- predict(predict_model_1,testdata, type="class")


#Model 2 : Train data divided into train and test. C5.0 Model with 10 Boost
predict_model_2 <- C5.0(trainingdata,trainingdata_survived,trials=10) #Not using boosting


#Try on the test data
test_predicted_survived <- predict(predict_model_2,testdata, type="class")


#Submit to Kaggle Code
response_df <- data.frame(PassengerID=test_PassengerId,Survived=test_predicted_survived)
summary(response_df)
str(response_df)
write.csv(response_df,file="D:/amit/R/Kaggle/Titanic/my_solution_c5_0_boost.csv",row.names=FALSE)

library(stringr)
library(ggplot2)
library(dplyr)
library(dummies)
library(caret)
library(Boruta)
library(xgboost)
train <- read.csv("D:/amit/Data Science/Kaggle/Titanic/train.csv", stringsAsFactors=FALSE)
test <- read.csv("D:/amit/Data Science/Kaggle/Titanic/test.csv", stringsAsFactors=FALSE)

train$IsTrain <- TRUE
test$IsTrain <- FALSE
test$Survived <- 0
combi <- rbind(train,test)
str(combi)

combi$Survived <- as.factor(combi$Survived)
combi$Sex <- factor(combi$Sex)
#Check blanks
sapply(combi,function(x){sum(ifelse(str_trim(x)=="",1,0))}) #Check no. of blanks in the features
#1014 Cabin blank, 2 Embarked blank

#Check NAs
sapply(combi,function(x){sum(ifelse(is.na(x),1,0))}) #Check no. of NAs in the features
#263 age is NA

#Lets first check Embarked
table(combi$Embarked)
combi[combi$Embarked=="",]
#Both passenger have ticket nbr 113572. Ideally they should be family member. But from Sibsp or Parch, it does not seem so
combi[combi$Ticket=="113572",] #No other passenger on the same ticket
ggplot(combi) +geom_boxplot(aes(x=factor(Embarked),y=Fare)) + facet_wrap("Pclass")
#As per boxplot it is more likely that Embarked is C
#Lets impute Embarked with the most common Embarked value i.e C
combi[combi$Embarked=="","Embarked"] ="C"

#Lets check Age NAs now

#Extract Titles
NameSplit <- strsplit(combi[,"Name"],("[.,]"))
Title <-sapply(NameSplit, "[[", 2) 
Title <- as.data.frame(Title)
combi$Title <- (str_trim(Title$Title))

table(combi$Title)

#TOo many Title. Lets combine the royalties to one group
combi[!combi$Title %in% c("Mr","Mrs","Miss","Master"),"Title"] <- "Royalty"
table(combi$Title)


ggplot(combi[combi$Title=="Mrs",]) + geom_density(aes(x=Age))

#We will impute the baisis of Title in the name
table(combi$Title, is.na(combi$Age))
# NAs for Mr.
combi[combi$Title=="Mr" & is.na(combi$Age),"Age"] <- median(combi[combi$Title=="Mr",]$Age,na.rm=TRUE)      

# NAs for Miss.
combi[combi$Title=="Miss" & is.na(combi$Age),"Age"] <- median(combi[combi$Title=="Miss",]$Age,na.rm=TRUE)      

# NAs for Mrs.
combi[combi$Title=="Mrs" & is.na(combi$Age),"Age"] <- median(combi[combi$Title=="Mrs",]$Age,na.rm=TRUE)      

# NAs for Royalty.
combi[combi$Title=="Royalty" & is.na(combi$Age),"Age"] <- median(combi[combi$Title=="Royalty",]$Age,na.rm=TRUE)   

# NAs for Master.
combi[combi$Title=="Master" & is.na(combi$Age),"Age"] <- median(combi[combi$Title=="Master",]$Age,na.rm=TRUE)      

# 1 Fare is NA
combi[is.na(combi$Fare),] 
combi[is.na(combi$Fare),"Fare"] <- median (combi[combi$Embarked=="S" & combi$Pclass==3,"Fare"],na.rm=TRUE)


#We can now drop Name and Passenger Id
combi$Name <- NULL

combi$PassengerId <- NULL

#
g1 <- ggplot(subset(combi,combi$IsTrain==TRUE)) 

#Proportion of each Sex Surving
sex_survived_df <- with(subset(combi,combi$IsTrain==TRUE), prop.table(table(Sex,Survived),1))
sex_survived_df <- as.data.frame(sex_survived_df)
ggplot(sex_survived_df) + geom_bar(aes(x=Sex,y=Freq, fill=Survived),stat="identity" )


#Proportion of each Title Surving
Title_survived_df <- with(subset(combi,combi$IsTrain==TRUE), prop.table(table(Title,Survived),1))
Title_survived_df <- as.data.frame(Title_survived_df)
ggplot(Title_survived_df) + geom_bar(aes(x=Title,y=Freq, fill=Survived),stat="identity" )


ggplot(combi) + geom_histogram(aes(x=Age, fill=Survived),binwidth=10,fill="orange",color="black",breaks=seq(0,90,5))

#CAtegorize Age into different bins 0-20 20-40 40-60 60-80
fn_agecat <- function (x){
  ifelse((x>=0 & x<=15) ,"A",
         (ifelse((x>15 & x<=30), "B",
                 (ifelse((x>30 & x<=50),"C","D")
                 )
         )
         )
  )
}

combi$AgeCategory <- fn_agecat(combi$Age)
combi$AgeCategory <- as.factor((combi$AgeCategory))

#Proportion of each Age Category Surving
Age_survived_df <- with(subset(combi,combi$IsTrain==TRUE), prop.table(table(AgeCategory,Survived),1))
Age_survived_df <- as.data.frame(Age_survived_df)
ggplot(Age_survived_df) + geom_bar(aes(x=AgeCategory,y=Freq, fill=Survived),stat="identity",position="dodge" )




#Group people with the same ticket number
ticket_passenger_df <- subset(combi,combi$IsTrain==TRUE) %>%
                        group_by(Ticket) %>% 
                        arrange(Ticket) %>%
                       # select(Survived,Ticket) %>%
                       # mutate(SurvRatio=sum(as.integer(Survived))/count(Ticket)) %>%
                        group_size()

#Feature Importance using Boruta
df_for_boruta <- subset(combi,combi$IsTrain==TRUE)
df_for_boruta$Age <- NULL
df_for_boruta$Ticket <- NULL
df_for_boruta$Cabin <- NULL
df_for_boruta$Embarked <- factor(df_for_boruta$Embarked)
df_for_boruta$IsTrain <- NULL
df_for_boruta$Title <- factor(df_for_boruta$Title)

df_factors <- subset(df_for_boruta,select=c("Sex","Embarked","Title","AgeCategory"))
dmy <- dummyVars("~." ,data=df_factors,fullRank = TRUE)
df_factors <- data.frame(predict(dmy,df_factors))

df_for_boruta <- df_for_boruta[,!names(df_for_boruta) %in% c("Sex","Embarked","Title","AgeCategory")]
df_for_boruta <- bind_cols(df_for_boruta,df_factors)

#boruta_features <- Boruta(data=df_for_boruta,Survived~.,doTrace=2)
boruta_features$finalDecision

#All features are deemed important
# Pclass         SibSp         Parch          Fare      Sex.male    Embarked.Q    Embarked.S 
# Confirmed     Confirmed     Confirmed     Confirmed     Confirmed     Confirmed     Confirmed 
# Title.Miss      Title.Mr     Title.Mrs Title.Royalty AgeCategory.B AgeCategory.C AgeCategory.D 
# Confirmed     Confirmed     Confirmed     Confirmed     Confirmed     Confirmed     Confirmed 

#Set up XGBoost
#We will use the dataframe created for Boruta as it is already dummyfied and non essential features have 
#been removed
#train_xgBoost <- df_for_boruta
# train_xgBoost <- df_for_boruta[-c(1:99),]
# test_xgBoost<- df_for_boruta[1:99,] 
train_xgBoost_Survived_label <- train_xgBoost$Survived
#train_xgBoost_Survived_label <- as.numeric(levels(train_xgBoost_Survived_label))[train_xgBoost_Survived_label]
#http://stackoverflow.com/questions/37041943/troubleshooting-xgboost-in-r


train_xgBoost$Survived <- NULL

# train_xgBoost <- xgb.DMatrix(data.matrix (train_xgBoost),label =train_xgBoost_Survived_label)
# xgb_model <- xgboost(train_xgBoost, nrounds=330,objective="binary:logistic",eta=0.01 )



#Cross Validation
# xgb.crossValidation <- xgb.cv(data=data.matrix (train_xgBoost),label =train_xgBoost_Survived_label,nrounds=1000,objective="binary:logistic" ,
#                               nfold=10, eta= 0.3, early_stop_round = 4,maximize=FALSE )
# xgb_model <- xgboost(data=data.matrix (train_xgBoost),label =train_xgBoost_Survived_label,nrounds=330,objective="reg:linear",eta=0.01 )

#Tuning XGBoost using Caret


# pack the training control parameters
xgb.cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 5,
                            #summaryFunction = twoClassSummary,
                            #classProbs = TRUE,
                            allowParallel=TRUE
                            )

xgb.grid <- expand.grid(nrounds = c(300,500,1000),
                        eta = c(0.05,0.1,0.2) ,
                        max_depth = c(6,8,10),
                        gamma=1,
                        colsample_bytree=1,
                        min_child_weight=1
)
Sys.time()
xgb_train_1 = train(
  x=as.matrix(train_xgBoost),
  y=train_xgBoost_Survived_label,
  trControl = xgb.cv.ctrl,
  tuneGrid = xgb.grid,
  method = "xgbTree",
  objective="reg:logistic"
  # nthread=4,
  #eval_metric="auc"
  #early_stopping_rounds = 4,
  #maximize=FALSE
)
Sys.time()
#Prediction
#Predict on the train data set
# train_predict_survived <- predict(xgb_train_1$finalModel,as.matrix(train_xgBoost))
# actual_train_survived <- combi[combi$IsTrain==TRUE ,"Survived"] 
# actual_train_survived <- actual_train_survived[-c(1:99)]
# #actual_train_predict are the probabilities. Need to get a probability threshold where 
# #max no of predictions are same
# calculateThreshold <- function(actual,predicted){ 
#     Errors <- 0
#   for (i in seq(0.1,0.9,0.05))
#     {
#     #Errors <- 0  
#     ##print(i)
#     x <- ifelse(predicted < i,1,0)
#     print(x)
#     Errors <- c(Errors,sum(ifelse(actual!=x,1,0)))
#    
#     } 
#     return(Errors[-1])
# }
# #On the local test set
# calculateThreshold(actual_train_survived,train_predict_survived)
# 
# test_xgBoost$Survived <- NULL
# test_xgBoost <- xgb.DMatrix(data.matrix(test_xgBoost))
# Survived_Predict <- predict(xgb_train_1$finalModel,test_xgBoost)
# 
# Survived_Predict <- ifelse(Survived_Predict<0.5,1,0)
# 
#Prepare test data set, same as the train data set was prepared

xgb_test <- subset(combi,combi$IsTrain==FALSE)
xgb_test$Age <- NULL
xgb_test$Ticket <- NULL
xgb_test$Cabin <- NULL
xgb_test$Embarked <- factor(xgb_test$Embarked)
xgb_test$IsTrain <- NULL
xgb_test$Title <- factor(xgb_test$Title)

df_factors <- subset(xgb_test,select=c("Sex","Embarked","Title","AgeCategory"))
dmy <- dummyVars("~." ,data=df_factors,fullRank = TRUE)
df_factors <- data.frame(predict(dmy,df_factors))

xgb_test <- xgb_test[,!names(xgb_test) %in% c("Sex","Embarked","Title","AgeCategory")]
xgb_test <- bind_cols(xgb_test,df_factors)
xgb_test$Survived <- NULL
xgb_test <- xgb.DMatrix(data.matrix(xgb_test))
Survived_Predict <- predict(xgb_train_1$finalModel,xgb_test)
Survived_Predict <- ifelse(Survived_Predict<=0.5,1,0) #Survived_Predict is the probability of the first level i.e 0.

response_df <- data.frame(PassengerID=test$PassengerId,Survived=Survived_Predict)
summary(response_df)
str(response_df)
write.csv(response_df,file="D:/amit/Data Science/Kaggle/Titanic/XGboost.csv",row.names=FALSE)

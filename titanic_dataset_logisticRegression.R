library(tidyverse)
library(caret)
library(Information)
library(e1071)
library(ROCR)
library(MASS)
library(car)
library(class)
#install.packages("class")
#install.packages("VIM")
library(VIM)
remove(list = ls())

train_df <- read.csv(file = "train.csv",stringsAsFactors = F)
test_df <- read.csv(file = "test.csv",stringsAsFactors = F)

str(train_df)
str(test_df)
head(train_df)
head(test_df)

# Manipulating the Survivied Column in test and train datasets
train_df_new <- train_df[,-2]
train_df_new$Survived <- train_df$Survived

test_df_new <- test_df
test_df_new$Survived <- 0
head(train_df_new)
head(test_df_new)

# Survived Column is missing in the test dataset

# Data Cleaning
# Find NAs
sapply(train_df_new, function(x) sum(is.na(x)))
sapply(test_df_new, function(x) sum(is.na(x)))

# Comments ages of 177 passengers are unknown.
# Test data_frame has 86 NAs, Fare has one 1 NA

# Checking Embarked
table(train_df$Embarked)
table(test_df$Embarked)

# replacing the empty values in the embarked with NA
train_df_new[which(train_df_new$Embarked %in% ""),]$Embarked <- NA

# Comment: 2 Empty columns found

# Imputation of Missing values
#---- grouping using WOE Analysis
# # Using the WOE analysis and to get an approximation
# train_df[which(is.na(train_df$Age)),]$Age <- "missing"
# test_df[which(is.na(test_df$Age)),]$Age <- "missing"
# 
# create_infotables(data = train_df[c("Age","Survived")],y = "Survived",bins = 20,parallel = T)
# # There are no matching NAs available to assign the missing NA value
# Imputation of missing values using K-Nearest Neighbour Analysis

names(train_df_new)
str(train_df_new)

# imputing the NAs in the train dataframe
imputed_knnTrain_df <- kNN(data = train_df_new[,-1],#[,which(names(train_df) %in% c("Age","Embarked"))],
                           variable = c("Age","Embarked"),impNA = T)#,catFun = mode,numFun = median )
#warnings()
summary(imputed_knnTrain_df)

#------------------------------------------------
# Identifying the imputed values in train and test Data frames
imputed_knnTrain_df[which(imputed_knnTrain_df["Age_imp"] == TRUE),"Age"]
imputed_knnTrain_df[which(imputed_knnTrain_df["Age_imp"] == TRUE),]
# replacing the imputed values age with
train_df_new$Age <- imputed_knnTrain_df$Age

# imputed Embarked
imputed_knnTrain_df[which(imputed_knnTrain_df["Embarked_imp"] == TRUE),"Embarked"]
imputed_knnTrain_df[which(imputed_knnTrain_df["Embarked_imp"] == TRUE),]
train_df_new$Embarked <- imputed_knnTrain_df$Embarked

# imputing the NAs in the test dataframe.
imputed_knn_test_df <- kNN(data = test_df,variable = c("Age","Fare"))

# finding the "Age" imputed values in the test_df
imputed_knn_test_df[which(imputed_knn_test_df["Age_imp"] == TRUE),"Age"]
imputed_knn_test_df[which(imputed_knn_test_df["Age_imp"] == TRUE),]

# finding the "Fare" imputed values in the imputed test dataframe
imputed_knn_test_df[which(imputed_knn_test_df["Fare_imp"] == TRUE),"Fare"]
imputed_knn_test_df[which(imputed_knn_test_df["Fare_imp"] == TRUE),]

# Replacing the imputed values in the test dataframe
test_df_new$Age <- imputed_knn_test_df$Age
test_df_new$Fare <- imputed_knn_test_df$Fare
#-------------------------------------------------
# checking the NAs
sapply(train_df_new, function(x) sum(is.na(x)))
sapply(test_df_new, function(x) sum(is.na(x)))
#Comments: All NAs are imputed form the dataset.

# Separating the title, First Name and Last Name
# master_df_new <- master_df %>% separate(col = Name, into = c("LastName","NewName"),sep = ",",extra = "merge") %>%
#   separate(col = NewName,into = c("Title","FirstName"),sep = ". ",extra = "merge") 
# 
# master_df_new$Title <- str_trim(string = master_df_new$Title,side = "both")

train_df_cleaned <- train_df_new %>% separate(col = Name, into =  c("LastName","NewName"),sep = ",",extra = "merge") %>%
  separate(col = NewName, into =  c("Title","FirstName"),sep = ". ",extra = "merge")

test_df_cleaned <- test_df_new %>% separate(col = Name, into =  c("LastName","NewName"),sep = ",",extra = "merge") %>%
  separate(col = NewName, into =  c("Title","FirstName"),sep = ". ",extra = "merge")

head(train_df_cleaned)
head(test_df_cleaned)

# Trimming the Empty spaces
train_df_cleaned$Title <- str_trim(string = train_df_cleaned$Title,side = "both")
test_df_cleaned$Title <- str_trim(string = test_df_cleaned$Title,side = "both")

#
unique(train_df_cleaned$Title)
unique(test_df_cleaned$Title)
# 
# # There are duplicate tickets, indicating these people travlled with
sum(duplicated(train_df_new$Ticket))
train_df_new[which(duplicated(train_df_new$Ticket)),]

# # Roundup Age and Fare to Numeric with roundup zero
train_df_cleaned$Age <- round(train_df_cleaned$Age,digits = 0)
train_df_cleaned$Fare <- round(train_df_cleaned$Fare,digits = 0)

# test data frame
test_df_cleaned$Age <- round(test_df_cleaned$Age,digits = 0)
test_df_cleaned$Fare <- round(test_df_cleaned$Fare,digits = 0)

# Analyse the Cabin number
train_df_cleaned$CabinYesNo <- ifelse(train_df_cleaned$Cabin == "",0,1)
test_df_cleaned$CabinYesNo <- ifelse(test_df_cleaned$Cabin == "",yes = 0,no = 1)

# convert Passengerid, Passenger Class, Sex, Siblings/Spouses, Parent Children, Embarked to factor
names(train_df_cleaned)
names(test_df_cleaned)

# Total features:
# [1] "PassengerId" "Survived"    "Pclass"      "last Name"   "Title"       "FirstName"   "Sex"         "Age"         "SibSp"      
# [10] "Parch"       "Ticket"      "Fare"        "Cabin"       "Embarked"   "CabinYesNo"

# To be removed:
# "PassengerId","Cabin","last Name","FirstName","Ticket"

# Remaining features are:
# "Survived"    "Pclass"  "Title" "Sex" "Age" "SibSp" "Parch"  "Fare" "Embarked" "CabinYesNo" 


# Final data_frame for the analysis
# Removing the features not required in the analysis FirstName, LastName "Cabin","last Name","FirstName","Ticket"

features_to_be_removed <- c("PassengerId","Cabin","LastName","FirstName","Ticket")
train_df_new_cleaned <- train_df_cleaned[,-which(names(train_df_cleaned) %in% features_to_be_removed)]
test_df_new_cleaned <- test_df_cleaned[,-which(names(test_df_cleaned) %in% features_to_be_removed)]

head(train_df_new_cleaned)
head(test_df_new_cleaned)

# Converting the parameters to factors
z_to_factor <- which(names(train_df_new_cleaned) %in% 
                       c("Pclass", "Title", "Sex", "SibSp","Parch","Embarked","Survived","CabinYesNo"))
train_df_factored <- as.data.frame(lapply(train_df_new_cleaned[,z_to_factor], as.factor))
str(train_df_factored)
train_df_cleaned <- data.frame(train_df_factored,
                               Age = train_df_new_cleaned$Age,
                               Fare = train_df_new_cleaned$Fare)
head(train_df_cleaned)
# Converting the features in the test dataframe to factors
z_to_factor <- which(names(test_df_cleaned) %in% 
                        c("Pclass", "Title","Sex", "SibSp","Parch","Embarked","Survived","CabinYesNo"))
test_df_factored <- as.data.frame(lapply(test_df_cleaned[,z_to_factor], as.factor))
str(test_df_factored)
test_df_cleaned <- data.frame(test_df_factored,
                              Age = test_df_cleaned$Age,
                              Fare = test_df_cleaned$Fare)

head(test_df_cleaned)
str(test_df_cleaned)
str(train_df_cleaned)


# EDA ---------

# UNIVARIATE ANALYSIS 

# Surival by Sex
ggplot(train_df_cleaned,aes(Sex,fill = Survived)) + geom_bar() +
  ggtitle(label = "Survival by Sex")

# box plot of fares
ggplot(train_df_cleaned,aes(x = Pclass,y = Fare)) + geom_boxplot() + ggtitle(label = "Boxplot of PassengerClass and their fare")
# Comments: There are several observatiosn that exists as outliers


# Survival by Title
# Several women were survived while men sacrified for women
ggplot(train_df_cleaned,aes(Title,fill = Survived)) + geom_bar() + 
  ggtitle(label = "Surival by Title") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(0.2,0.8),
        legend.background = element_blank())
# Sevel Men died, Miss, Mrs, Children, Ms, Sir, Countess, Mme, Mlle have survived

# Survival by Age groups
ggplot(train_df_cleaned,aes(Age,Fare,col = Survived,label = Embarked)) + geom_label(na.rm = T) +
  geom_point(na.rm = TRUE) +
  ggtitle(label = "Survival by Age Groups") +
  theme(legend.position = c(0.2,0.8),
        legend.background = element_blank())

# Comments:
# Several Children Survived, people whose ticket fare was higher too had Survived.
# Death Rate is highest where the fares were higher.

#
ggplot(data = train_df_cleaned,aes(Pclass,fill = Survived)) + 
  geom_bar() +
  theme(legend.position = c(0.2,0.8),
        legend.background = element_blank())
# Comments:
# There are more deaths from third class Passengers. Percentage of deaths in the First clss passengers is lesser.

ggplot(data = train_df_cleaned,aes(Embarked, fill = Survived)) + 
  geom_bar() + ggtitle("Surviors Place of Embarkment")
# Several people who had embared in Southhampton were killed. While the percentage of deaths from Chebourg and Queenstown is lesser.

ggplot(data = train_df_cleaned,aes(CabinYesNo,fill = Survived)) + geom_bar() 
# People who were assigned cabins survived, than to the people who did have cabins

# bivariate analysis
# CabinYesNo, PClass, Survived, Fare, Age

# Chances of Survival of SibSp and Parch
ggplot(data = train_df_cleaned,aes(x = SibSp,y = Parch,col = Survived)) + geom_point()


# Scaling of the Fare
boxplot(x = train_df_cleaned$Fare)

zz <- boxplot.stats(x = train_df_cleaned$Fare,coef = 1.58)
zz$out
#
#boxplot.stats
#apply(X = train_df_cleaned[,"Fare"], FUN = scale)
head(train_df_cleaned)

boxplot(scale(x = train_df_cleaned$Fare,scale = T))

plot(y = train_df$Fare,x = train_df$PassengerId)


#----------------------------------
# Dummy variables
#----------------------------------
unique(train_df_cleaned$Sex)
# Assign Male -1 and Female - 0
levels(train_df_cleaned$Sex) <- c(0,1)
levels(test_df_cleaned$Sex) <- c(0,1)

str(test_df_cleaned)
str(train_df_cleaned)

z_headers <- which(names(train_df_cleaned) %in% c("Title","Pclass","Embarked","SibSp","Parch"))

# creating dummy data frame
train_df_dummy <- cbind(train_df_cleaned[,-z_headers],
                        as.data.frame(model.matrix(object = ~Title, data = train_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~Pclass, data = train_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~Embarked, data = train_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~SibSp, data = train_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~Parch, data = train_df_cleaned))[,-1])


# creating the test dummy data_frame
z_headers <- which(names(test_df_cleaned) %in% c("Title","Pclass","Embarked","SibSp","Parch"))

test_df_dummy <- cbind(test_df_cleaned[,-z_headers],
                       as.data.frame(model.matrix(object = ~Title,data = test_df_cleaned))[,-1],
                       as.data.frame(model.matrix(object = ~Pclass, data = test_df_cleaned))[,-1],
                       as.data.frame(model.matrix(object = ~Embarked, data = test_df_cleaned))[,-1],
                       as.data.frame(model.matrix(object = ~SibSp, data = test_df_cleaned))[,-1],
                       as.data.frame(model.matrix(object = ~Parch, data = test_df_cleaned))[,-1])

head(train_df_dummy)
head(test_df_dummy)

#----------------------------------------------------------------------
# Sampling of training dataset and creating a validation dataset (70-30 split)
#----------------------------------------------------------------------

index <- sample(x = 1:nrow(train_df_dummy),size = nrow(train_df_dummy) * 0.7)

# Validation dataset and training datasets
validation_df <- train_df_dummy[-index,]
train_df_dummy <- train_df_dummy[index,]


#---------------------------------
# Model Building
#---------------------------------

options(scipen = 999)
install.packages("arm")
library(arm)

initial_model <- glm(formula = Survived ~.,family = "binomial",data = train_df_dummy)#,control=glm.control(maxit=1))
summary(initial_model)

# stepAIC of the initial model
model_step <- stepAIC(object = initial_model,direction = "both")
summary(model_step)
vif(model_step)
warnings()

# TitleSir      33.77115 4133.12755   0.008     0.993481
model_1 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
    TitleDr + TitleMajor + TitleMaster + TitleMr + 
    Pclass2 + Pclass3 + SibSp3 + SibSp4 + SibSp5 + SibSp8, family = "binomial", 
    data = train_df_dummy)

summary(model_1)
vif(model_1)

# SibSp5      -17.49763  947.93330  -0.018     0.985273   

model_2 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
    TitleDr + TitleMajor + TitleMaster + TitleMr + Pclass2 + 
    Pclass3 + SibSp3 + SibSp4 + SibSp8, family = "binomial", 
    data = train_df_dummy)

summary(model_2)
vif(model_2)

# SibSp8      -16.53690  809.59082  -0.020     0.983703 

model_3 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
    TitleDr + TitleMajor + TitleMaster + TitleMr + Pclass2 + 
    Pclass3 + SibSp3 + SibSp4, family = "binomial", 
    data = train_df_dummy)
  

summary(model_3)
vif(model_3)

# TitleMajor   1.14028    1.79119   0.637    0.524382    
model_4 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
    TitleDr + TitleMaster + TitleMr + Pclass2 + 
    Pclass3 + SibSp3 + SibSp4, family = "binomial", data = train_df_dummy)

summary(model_4)
vif(model_4)

# TitleMr      0.42714    0.82922   0.515    0.606475   

model_5 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
    TitleDr + TitleMaster + Pclass2 + Pclass3 + SibSp3 + 
    SibSp4, family = "binomial", data = train_df_dummy)
summary(model_5)
vif(model_5)

# TitleDr      0.46029    0.97113   0.474             0.635522
model_6 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
    TitleMaster + Pclass2 + Pclass3 + SibSp3 + SibSp4, 
    family = "binomial", data = train_df_dummy)
summary(model_6)
vif(model_6)

# TitleCol     1.19488    1.50749   0.793             0.427995   
model_7 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + 
    TitleMaster + Pclass2 + Pclass3 + SibSp3 + SibSp4, family = "binomial", 
    data = train_df_dummy)
  
summary(model_7)
vif(model_7)

# Pclass2     -0.99556    0.46233  -2.153             0.031291 * 
model_8 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleMaster + 
    Pclass3 + SibSp3 + SibSp4, family = "binomial", 
    data = train_df_dummy)
summary(model_8)
vif(model_8)


# CabinYesNo  1.01855    0.43016   2.368             0.017892 *  
model_9 <- glm(formula = Survived ~ Sex + Age + 
    TitleMaster + Pclass2 + Pclass3 + SibSp3 + SibSp4, family = "binomial", 
    data = train_df_dummy)

summary(model_9)

# AIC increases after the MODEL_7, and model_7 has the lowest AIC. The null deviance and residual deviance of Model_7 represents the least value.

final_model <- model_7
# Thus features that significantly the impacted the Survival of passenger are 1. Age, 2. Sex, 3.PassengerClass they travlled 4. Relationship between the Survivors 3. Finally Kids were most protective and were highest percentage among the survivors

#----------------------------------------
# Model Evaluation on Validation dataset
#----------------------------------------
prob_Surv_val <- predict(object = final_model,newdata = validation_df,type = "response")
prob_Surv_val

# determining the probability of prediction
predi_surv_validation_data_set <- as.factor(ifelse(prob_Surv_val > 0.5, "yes","no"))
actual_surv_validataion_data_set <- as.factor(ifelse(validation_df$Survived == 1, "yes","no"))
str(actual_surv_validataion_data_set)
str(predi_surv_validation_data_set)

zz_conf <- confusionMatrix(predi_surv_validation_data_set,actual_surv_validataion_data_set)
zz_conf$byClass[1]
zz_conf$byClass[2]

# Finding the optimum cut-off Probability

cutoff_per <- function(cutoff) {
  predict_surv <- factor(ifelse(prob_Surv_val >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predict_surv,actual_surv_validataion_data_set,positive = "yes")
  z_accuracy <- conf$overall[1]
  z_sensitivity <- conf$byClass[1]
  z_specificity <- conf$byClass[2]
  z_out <- t(as.matrix(c(z_accuracy,z_sensitivity,z_specificity)))
  return(z_out)
}


s <- seq(0.016,0.97,length.out = 100)
output_matrix <- matrix(nrow = 100,ncol = 3,data = 0)

# preparing the confusion matrix for different values of s.

for(i in 1:100) {
  output_matrix[i,] <- cutoff_per(s[i])
}

colnames(output_matrix) <- c("Accuracy","Sensitivity","Specificity")

surv_confMat_df <- as.data.frame(output_matrix)
View(surv_confMat_df)

# The cutoff probability
cutoff_probability <- s[which.min(abs(surv_confMat_df$Sensitivity - surv_confMat_df$Specificity))]

pred_surv_cutoff <- factor(ifelse(prob_Surv_val >= cutoff_probability, "yes","no"))

conf_mat <- confusionMatrix(pred_surv_cutoff,actual_surv_validataion_data_set)
conf_mat

 # Accuracy : 0.847              
 #                 95% CI : (0.7982, 0.8879)   
 #    No Information Rate : 0.5858             
 #    P-Value [Acc > NIR] : <0.0000000000000002
 #                                             
 #                  Kappa : 0.6876             
 # Mcnemar's Test P-Value : 0.3487             
 #                                             
 #            Sensitivity : 0.8471             
 #            Specificity : 0.8468   

library(ggplot2)
ggplot(as.data.frame(output_matrix)) +
  geom_line(aes(x = s,y = Sensitivity,col = "black")) + 
  geom_line(aes(x = s,y = Specificity,col = "blue")) +
  geom_line(aes(x = s,y = Accuracy,col = "green")) +
  geom_line(aes(x = s,y = abs(Sensitivity + Specificity + Accuracy))) +
  scale_color_discrete(labels = c("Sensitivity","Specificity","Accuracy")) +
  geom_vline(xintercept = c(cutoff_probability),linetype = "dotted") + 
  xlab(label = "Predicted Probability") + ylab(label = "Sensitiviy, Specificity, and Accuracy")

#----------------------------
# Model Evaluation
#----------------------------
predict_survival <- prediction(predictions = prob_Surv_val,labels = validation_df$Survived)
View(predict_survival)

perf_titanic <- performance(predict_survival,"tpr", "fpr")
plot(perf_titanic)

lift_titanic <- performance(prediction.obj = predict_survival,"lift","rpp")
plot(lift_titanic)

auc_titanic <- performance(predict_survival,measure = "auc")
auc_titanic@y.values

# KS-Static
ks_statistic <- max(attr(perf_titanic,'y.values')[[1]] - attr(perf_titanic,'x.values')[[1]])
plot(perf_titanic, main = paste0( "KS Statistic = ",round(ks_statistic*100,2),"%"))

#-----------------------------
# Predicting the test dataset
#-----------------------------

pred_test_data <- predict(object = final_model,newdata = test_df_dummy,type = "response")


predicted_test <- data.frame(Passengerid = test_df$PassengerId, Survived_Probability = pred_test_data)

head(predicted_test)
View(pred_test)

row_number(test_df_cleaned)

























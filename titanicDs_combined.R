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

master_df <- rbind(train_df_new,test_df_new)

# Survived Column is missing in the test dataset

# Data Cleaning
# Find NAs
sapply(master_df, function(x) sum(is.na(x)))

# Comments ages of 263 passengers are unknown.
# Test data_frame has 86 NAs, Fare has one 1 NA

# Checking Embarked
table(master_df$Embarked)

master_df[which(master_df$Embarked %in% ""),]$Embarked <- NA

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

dim(master_df)

# imputing the NAs in the train dataframe
imputed_master_df <- kNN(data = master_df,variable = c("Age","Fare","Embarked"))

warnings()
summary(imputed_master_df)

# Imputed Values of Age
imputed_master_df[which(imputed_master_df["Age_imp"] == TRUE),"Age"]
# imputing the values in the master Dataframe
master_df$Age <- imputed_master_df$Age
master_df$Fare <- imputed_master_df$Fare
master_df$Embarked <- imputed_master_df$Embarked

# checking the NAs
sapply(master_df, function(x) sum(is.na(x)))
#Comments: All NAs are imputed form the dataset.

# Separating the title, First Name and Last Name
master_df_new <- master_df %>% separate(col = Name, into = c("LastName","NewName"),sep = ",",extra = "merge") %>%
  separate(col = NewName,into = c("Title","FirstName"),sep = ". ",extra = "merge") 
master_df_new$Title %>% head()

# removing the white spaces
master_df_new$Title <- str_trim(string = master_df_new$Title,side = "both")

# Roundup Age and Fare to Numeric with roundup zero
master_df_new$Age <- round(master_df_new$Age,digits = 0)
master_df_new$Fare <- round(master_df_new$Fare,digits = 0)

# Analyse the Cabin number
master_df_new$CabinYesNo <- ifelse(master_df_new$Cabin == "",0,1)

# convert Passengerid, Passenger Class, Sex, Siblings/Spouses, Parent Children, Embarked to factor
names(master_df_new)

# Total features:
# [1] "PassengerId" "Survived"    "Pclass"      "last Name"   "Title"       "FirstName"   "Sex"         "Age"         "SibSp"      
# [10] "Parch"       "Ticket"      "Fare"        "Cabin"       "Embarked"   "CabinYesNo"

# To be removed:
# "PassengerId","Cabin","LastName","FirstName","Ticket"

# Remaining features are:
# "Survived"    "Pclass"  "Title" "Sex" "Age" "SibSp" "Parch"  "Fare" "Embarked" "CabinYesNo" 


# Final data_frame for the analysis
# Removing the features not required in the analysis FirstName, LastName "Cabin","last Name","FirstName","Ticket"

features_to_be_removed <- c("PassengerId","Cabin","LastName","FirstName","Ticket")
master_df_cleaned <- master_df_new[,-which(names(master_df_new) %in% features_to_be_removed)]

head(master_df_cleaned)
head(master_df_new)

# Converting the parameters to factors
z_to_factor <- which(names(master_df_cleaned) %in% 
                       c("Pclass", "Title", "Sex", "SibSp","Parch","Embarked","Survived","CabinYesNo"))
master_df_factored <- as.data.frame(lapply(master_df_cleaned[,z_to_factor], as.factor))
str(master_df_factored)
master_df_cleaned <- data.frame(master_df_factored,
                               Age = master_df_cleaned$Age,
                               Fare = master_df_cleaned$Fare)
head(master_df_cleaned)


# EDA ---------

# UNIVARIATE ANALYSIS 

# Surival by Sex
ggplot(master_df_cleaned,aes(Sex,fill = Survived)) + geom_bar() +
  ggtitle(label = "Survival by Sex")

# box plot of fares
ggplot(master_df_cleaned,aes(x = Pclass,y = Fare)) + geom_boxplot() + ggtitle(label = "Boxplot of PassengerClass and their fare")
# Comments: There are several observatiosn that exists as outliers


# Survival by Title
# Several women were survived while men sacrified for women
ggplot(master_df_cleaned,aes(Title,fill = Survived)) + geom_bar() + 
  ggtitle(label = "Surival by Title") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(0.2,0.8),
        legend.background = element_blank())
# Sevel Men died, Miss, Mrs, Children, Ms, Sir, Countess, Mme, Mlle have survived

# Survival by Age groups
ggplot(master_df_cleaned,aes(Age,Fare,col = Survived,label = Embarked)) + geom_label(na.rm = T) +
  geom_point(na.rm = TRUE) +
  ggtitle(label = "Survival by Age Groups") +
  theme(legend.position = c(0.2,0.8),
        legend.background = element_blank())

# Comments:
# Several Children Survived, people whose ticket fare was higher too had Survived.
# Death Rate is highest where the fares were higher.

# Passenger class Survived
ggplot(data = master_df_cleaned,aes(Pclass,fill = Survived)) + 
  geom_bar() +
  theme(legend.position = c(0.2,0.8),
        legend.background = element_blank())

# Comments:
# There are more deaths from third class Passengers. Percentage of deaths in the First clss passengers is lesser.
ggplot(data = master_df_cleaned,aes(Embarked, fill = Survived)) + 
  geom_bar() + ggtitle("Surviors Place of Embarkment")
# Several people who had embared in Southhampton were killed. While the percentage of deaths from Chebourg and Queenstown is lesser.

# Cabin hasn't booked
ggplot(data = master_df_cleaned,aes(CabinYesNo,fill = Survived)) + geom_bar() + ggtitle("Survival by Cabin Bookings")
# People who were assigned cabins survived, than to the people who did have cabins

# bivariate analysis
# CabinYesNo, PClass, Survived, Fare, Age

# Chances of Survival of SibSp and Parch
ggplot(data = master_df_cleaned,aes(x = as.numeric(SibSp),y = as.numeric(Parch),col = Survived)) + 
  geom_point(position = "jitter") + 
  ggtitle("Survivals of Sibling Spouse and Par Child ")

# Scaling of the Fare
boxplot(x = master_df_cleaned$Fare)

zz <- boxplot.stats(x = master_df_cleaned$Fare,coef = 1.58)
zz$out

#
#boxplot.stats
#apply(X = master_df_cleaned[,"Fare"], FUN = scale)
head(master_df_cleaned)

plot(y = train_df$Fare,x = train_df$PassengerId)

#----------------------------------
# Dummy variables
#----------------------------------
unique(master_df_cleaned$Sex)
# Assign Male -1 and Female - 0
levels(master_df_cleaned$Sex) <- c(0,1)

z_headers <- which(names(master_df_cleaned) %in% c("Title","Pclass","Embarked","SibSp","Parch"))

# creating dummy data frame
master_df_dummy <- cbind(master_df_cleaned[,-z_headers],
                        as.data.frame(model.matrix(object = ~Title, data = master_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~Pclass, data = master_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~Embarked, data = master_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~SibSp, data = master_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~Parch, data = master_df_cleaned))[,-1])

head(master_df_dummy)

# Splitting into train and test dummy datasets
set.seed(100)
index <- sample(x = 1:nrow(master_df_dummy),size = nrow(master_df_dummy)* 0.70,replace = F)

train_df_dummy <- master_df_dummy[index,]
test_df_dummy <- master_df_dummy[-index,]
head(train_df_dummy)
head(test_df_dummy)
#---------------------------------
# Model Building
#---------------------------------

options(scipen = 999)

initial_model <- glm(formula = Survived ~.,family = "binomial",data = train_df_dummy)
summary(initial_model)

model_step <- stepAIC(object = initial_model,direction = "both")
summary(model_step)
vif(model_step)

# TitleSir      34.208094 4136.221557   0.008        0.993401   
model_1 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
                 TitleDr + TitleMajor + TitleMaster + TitleMr + TitleMrs + 
                 Pclass2 + Pclass3 + EmbarkedS + SibSp1 + SibSp3 + 
                 SibSp4 + SibSp5 + SibSp8 + Parch4 + Parch5, family = "binomial", 
               data = train_df_dummy)
  
  
  
summary(model_1)
vif(model_1)

# Parch4      -16.090752 979.439376  -0.016        0.986893   

model_2 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
                 TitleDr + TitleMajor + TitleMaster + TitleMr + TitleMrs + 
                 Pclass2 + Pclass3 + EmbarkedS + SibSp1 + SibSp3 + SibSp4 + 
                 SibSp5 + SibSp8 + Parch5, family = "binomial", data = train_df_dummy)

summary(model_2)
vif(model_2)

# SibSp8      -16.660719 750.189556  -0.022        0.982281 

model_3 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
                 TitleDr + TitleMajor + TitleMaster + TitleMr + TitleMrs + 
                 Pclass2 + Pclass3 + EmbarkedS + SibSp1 + SibSp3 + SibSp4 + 
                 SibSp5 + Parch5, family = "binomial", data = train_df_dummy)

summary(model_3)
vif(model_3)

# SibSp5      -16.157896 571.692897  -0.028        0.977452 
model_4 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
                 TitleDr + TitleMajor + TitleMaster + TitleMr + TitleMrs + 
                 Pclass2 + Pclass3 + EmbarkedS + SibSp1 + SibSp3 + SibSp4 + 
                 Parch5, family = "binomial", data = train_df_dummy)

summary(model_4)
vif(model_4)

# TitleDr      1.054095   1.381943   0.763       0.445605    

model_5 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
                 TitleMajor + TitleMaster + TitleMr + TitleMrs + 
                 Pclass2 + Pclass3 + EmbarkedS + SibSp1 + SibSp3 + SibSp4 + 
                 Parch5, family = "binomial", data = train_df_dummy)

  
summary(model_5)
vif(model_5)

# TitleMr      0.368288   0.699477   0.527       0.598527 

model_6 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
                 TitleMajor + TitleMaster + TitleMrs + Pclass2 + 
                 Pclass3 + EmbarkedS + SibSp1 + SibSp3 + SibSp4 + Parch5, 
               family = "binomial", data = train_df_dummy)
  
summary(model_6)
vif(model_6)

# TitleMajor   0.684039   1.435021   0.477             0.633593 

model_7 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleCol + 
                 TitleMaster + TitleMrs + Pclass2 + Pclass3 + 
                 EmbarkedS + SibSp1 + SibSp3 + SibSp4 + Parch5, family = "binomial", 
               data = train_df_dummy)
summary(model_7)
vif(model_7)

# TitleCol     1.108380   1.517994   0.730             0.465292   
model_8 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + 
                 TitleMaster + TitleMrs + Pclass2 + Pclass3 + EmbarkedS + 
                 SibSp1 + SibSp3 + SibSp4 + Parch5, family = "binomial", data = train_df_dummy)
summary(model_8)
vif(model_8)

# SibSp1      -0.212389   0.236059  -0.900             0.368266 

model_9 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleMaster + 
                 TitleMrs + Pclass2 + Pclass3 + EmbarkedS + SibSp3 + 
                 SibSp4 + Parch5, family = "binomial", data = train_df_dummy)
summary(model_9)
vif(model_9)

# Parch5      -1.458964   1.189202  -1.227             0.219882    
model_10 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleMaster + 
                  TitleMrs + Pclass2 + Pclass3 + EmbarkedS + SibSp3 + SibSp4,family = "binomial", data = train_df_dummy)

summary(model_10)
vif(model_10)

# EmbarkedS      -0.297908   0.252605  -1.179  0.23826
model_11 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleMaster + 
                  TitleMrs + Pclass2 + Pclass3 + SibSp3 + SibSp4, 
                family = "binomial", data = train_df_dummy)
summary(model_11)
vif(model_11)

# TitleMrs     0.440174   0.321217   1.370             0.170583 
model_12 <- glm(formula = Survived ~ Sex + CabinYesNo + Age + TitleMaster + 
                Pclass2 + Pclass3 + SibSp3 + SibSp4, family = "binomial", 
                data = train_df_dummy)
summary(model_12)
vif(model_12)

# CabinYesNo  0.685866   0.346487   1.979             0.047761 * 
model_13 <- glm(formula = Survived ~ Sex + Age + TitleMaster + 
                  Pclass2 + Pclass3 + SibSp3 + SibSp4, family = "binomial", 
                data = train_df_dummy)
summary(model_13)
vif(model_13)

# Thus features that significantly the impacted the Survival of passenger are 1. Age, 2. Sex, 3.PassengerClass they travlled 4. Relationship between the Survivors 3. Finally Kids were most protective and were highest percentage among the survivors

# Model prediction.
final_model <- model_13

#---------- Validation of pedictive model against the test data
probability_of_survived <- predict(object = final_model,newdata = test_df_dummy,type = "response")

test_df_new$Prob_Surv <- probability_of_survived
testt_surv_pred <- as.factor(ifelse(probability_of_survived > 0.5,1,0))
testt_surv_pred
test_df_new$Survived


confusionMatrix(data = testt_surv_pred,reference = factor(test_df_new$Survived))












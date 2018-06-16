library(tidyverse)
library(caret)
library(Information)
library(e1071)
library(ROCR)
library(MASS)
library(car)

train_df <- read.csv(file = "train.csv",stringsAsFactors = F)
str(train_df)
View(train_df)

# Data Cleaning
# Find NAs
sapply(train_df, function(x) sum(is.na(x)))
# Comments ages of 177 passengers are unknown.

# Checking Embarked
table(train_df$Embarked)
train_df[which((train_df$Embarked) %in% ""),]


#---- WOE Analysis
# Using the WOE analysis and to get an approximation
train_df[which(is.na(train_df$Age)),]$Age <- "missing"
create_infotables(data = train_df[c("Age","Survived")],y = "Survived",bins = 20,parallel = T)
# There are no matching NAs available to assign the missing NA value

# split name
head(train_df)

# str_split(string = train_df$Ticket,pattern = "[+[:digit:]]+") %>% head()
train_df_new <- train_df %>% separate(col = Name, into =  c("last Name","NewName"),sep = ",",extra = "merge") %>% 
  separate(col = NewName, into =  c("Title","FirstName"),sep = ". ",extra = "merge") 

head(train_df_new)
unique(train_df_new$Title)

# Split ticket
nrow(train_df_new)

# There are duplicat tickets, indicating these people travlled with
sum(duplicated(train_df_new$Ticket))
train_df_new[which(duplicated(train_df_new$Ticket)),]

# Roundup Age and Fare to Numeric with roundup zero
train_df_new$Age <- round(train_df_new$Age,digits = 0)
train_df_new$Fare <- round(train_df_new$Fare,digits = 0)

# # Analyse the Cabin number
train_df_new$CabinYesNo <- ifelse(train_df_new$Cabin == "",0,1)

# convert Passengerid, Passenger Class, Sex, Siblings/Spouses, Parent Children, Embarked to factor
names(train_df_new)

# Total features:
# [1] "PassengerId" "Survived"    "Pclass"      "last Name"   "Title"       "FirstName"   "Sex"         "Age"         "SibSp"      
# [10] "Parch"       "Ticket"      "Fare"        "Cabin"       "Embarked"   "CabinYesNo"
# To be removed:
# "PassengerId","Cabin","last Name","FirstName","Ticket"
# Remaining features are:
# "Survived"    "Pclass"  "Title" "Sex" "Age" "SibSp" "Parch"  "Fare" "Embarked" "CabinYesNo" 


# Final data_frame for the analysis
# Removing the features not required in the analysis FirstName, LastName "Cabin","last Name","FirstName","Ticket"
names(train_df_new)
features_to_be_removed <- c("PassengerId","Cabin","last Name","FirstName","Ticket")
train_df_new_cleaned <- train_df_new[,-which(names(train_df_new) %in% features_to_be_removed)]
head(train_df_new_cleaned)

# Converting the parameters to factors
z_to_factor <- which(names(train_df_new_cleaned) %in% 
                       c("Pclass", "Title", "Sex", "SibSp","Parch","Embarked","Survived","CabinYesNo"))
train_df_factored <- as.data.frame(lapply(train_df_new_cleaned[,z_to_factor], as.factor))
str(train_df_factored)
train_df_cleaned <- data.frame(train_df_factored,
                               Age = train_df_new_cleaned$Age,
                               Fare = train_df_new_cleaned$Fare)
head(train_df_cleaned)

# EDA ---------

# UNIVARIATE ANALYSIS 

# Surival by Sex
ggplot(train_df_cleaned,aes(Sex,fill = Survived)) + geom_bar() +
  ggtitle(label = "Survival by Sex")

# Survival by Title
# Several women were survived while men sacrified for women
ggplot(train_df_cleaned,aes(Title,fill = Survived)) + geom_bar() + 
  ggtitle(label = "Surival by Title") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(0.2,0.8),
        legend.background = element_blank())
# Sevel Men died, Miss, Mrs, Children, Ms, Sir, Countess, Mme, Mlle have survived

# Survival by Age groups
ggplot(train_df_cleaned,aes(Age,Fare,col = Survived)) + 
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

ggplot(data = train_df_cleaned,aes(Embarked, fill = Survived)) + geom_bar()
# Several people who had embared in Southhampton were killed. While the percentage of deaths from Chebourg and Queenstown is lesser.

ggplot(data = train_df_cleaned,aes(CabinYesNo,fill = Survived)) + geom_bar() 
# People who were assigned cabins survived, than to the people who did have cabins

# bivariate analysis
# CabinYesNo, PClass, Survived, Fare, Age

# Chances of Survival of SibSp and Parch
ggplot(data = train_df_cleaned,aes(x = SibSp,y = Parch,col = Survived)) + geom_point()


# Dummy variables
unique(train_df_cleaned$Sex)
# Assign Male -1 and Female - 0
levels(train_df_cleaned$Sex) <- c(0,1)

str(train_df_cleaned)
z_headers <- which(names(train_df_cleaned) %in% c("Title","Pclass","Embarked"))

# creating dummy data frame
train_df_dummy <- cbind(train_df_cleaned[,-z_headers],
                        as.data.frame(model.matrix(object = ~Title, data = train_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~Pclass, data = train_df_cleaned))[,-1],
                        as.data.frame(model.matrix(object = ~Embarked, data = train_df_cleaned))[,-1])


head(train_df_dummy)

# Model Building

initial_model <- glm(formula = Survived ~.,family = "binomial",data = train_df_dummy,na.action = na.omit)
summary(initial_model)

model_step <- stepAIC(object = initial_model,direction = "both")
summary(model_step)









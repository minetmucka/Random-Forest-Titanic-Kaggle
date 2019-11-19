
#############################################################################################################
## Minet Mucka Polsinelli                                                                                   #
## Objective: Titanic Data Set Survival Prediction using Random Forest for Kaggle Competition (9TH Place/43)#
## Data source: titanic data set split into train and test                                                  #
## Please install "rpart" package: install.packages("rpart") for Decision Tree                              #
## Please install "rpart" package: install.packages("rpart.plot") for Decision Tree                         #
## Please install "rpart" package: install.packages("randomForest") for Random Forest                       #
## Please install "Amelia" package: install.packages("Amelia") for data imputation                          #
## Please install "deplyr" package: install.packages("dplyr") for data exploration                          #
#############################################################################################################

##Clear Environment
#rm(list = ls())

# My working directry and my file names
## working.directory <- "C:/Users/muckam/Desktop/DataScienceBootcamp/Homework Answers"
setwd("C:/Users/muckam/Desktop/DataScienceBootcamp/Homework Answers")

#Reading in the train and test set, set the working directory for all future inputs and outputs

titanic.train <- read.csv("train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv("test.csv", stringsAsFactors = FALSE, header = TRUE)

###############################
# titanic.fullne two datasets #
###############################

#Mark rows as either train or test
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet  <- FALSE

#In order to titanic.fullne the two datasets together, both test and train sets need to have the same columns.
#Give the test set a survived column
titanic.test$Survived <- NA  

#titanic.fullne the two datasets into one
titanic.full <- rbind(titanic.train, titanic.test)

#How many missing values?
library(Amelia)
missmap(titanic.train)

########################
# Clean Missing Values #
########################

## Missing Values of  Embarked ##
table(titanic.full$Embarked)
#     C   Q   S 
# 2 270 123 914 #2 blank strings

table(titanic.full$Embarked=='')
# FALSE  TRUE 
# 1307     2

# Fill in the missing values of embarked with the mode
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

##Missing Values of Age ##

#Fills age NAs with the global median (28). na.rm=TRUE if its a missing value. Can enhance with regression model to estimate NA's
age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.median


#######################
# Categorical Casting #
#######################
titanic.full$Pclass <- as.ordered(titanic.full$Pclass)  
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Survived <- as.factor(titanic.full$Survived)


#dplyr explore the data
library(dplyr)

#Select first 5 rows using slice
slice5 <- slice(titanic.full, 1:5)
slice5

#Arrange passengers by fare price (acending order)
fare_arranged <- arrange(titanic.full, Fare)
View(fare_arranged)

#Arrange by descending order 
fare_desc <- arrange(titanic.full, desc(Fare))

#Get distinct Pclass types
distinct(titanic.full, Pclass)

#Avg Pclass of all the passengers
#Max price for a fare
summarise(titanic.full, max_fare = max(Fare, na.rm=TRUE))

#Max age
summarise(titanic.full, max_age = max(Age, na.rm=TRUE))

#Disaster was famous for saving "women and children first"
summary(titanic.full$Sex)

#Proportion of sex and survivors 1 is survived 
#We need to tell the command to give us proportions in the 1st dimension which stands for the rows 
prop.table(table(titanic.full$Sex, titanic.full$Survived),1)

#Now on to test to see if all the children survived as well 
summary(titanic.full$Age)

#Label the children with a binary of 0 not child and 1 as child 
titanic.full$Child <- 0
titanic.full$Child[titanic.full$Age < 18] <- 1

summary(titanic.full$Child)

#Find the number of survivors for the different subsets 
#The aggregate command takes a formula with the target variable on the left hand side of the tilde symbol and the variables to subset over on the right
aggregate(Survived ~ Child + Sex, data=titanic.full, FUN=sum)

#Total number of people in each subset
#Length of the Survived vector for each subset and output the result
aggregate(Survived ~ Child + Sex, data=titanic.full, FUN=length)

#Another feature variable is costs of ticket, we'll bin the fares 
summary(titanic.full$Fare)
titanic.full$Fare2 <- '30+'
titanic.full$Fare2[titanic.full$Fare < 30 & titanic.full$Fare >= 20] <- '20-30' 
titanic.full$Fare2[titanic.full$Fare < 20 & titanic.full$Fare >= 10] <- '10-20'
titanic.full$Fare2[titanic.full$Fare < 10] <- '<10'

#Change name field to a character 
titanic.full$Name <- as.character(titanic.full$Name)
titanic.full$Name[1]

#Break apart title from persons name
strsplit(titanic.full$Name[1], split='[,.]')

#^Those symbols in the square brackets are called regular expressions
#Isolate the title 
strsplit(titanic.full$Name[1], split='[,.]')[[1]][2]

#We feed sapply our vector of names and our function that we just came up with. 
#It runs through the rows of the vector of names, and sends each name to the function
titanic.full$Title <- sapply(titanic.full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
View(titanic.full)

#Strip off those spaces from the beginning of the titles
titanic.full$Title <- sub(' ', '', titanic.full$Title)

#View the titles of everyone 
table(titanic.full$Title)

#Mademoiselle and Madame are pretty similar (so long as you don't mind offending) so let's titanic.fullne them into a single category
titanic.full$Title[titanic.full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

#%in% operator checks to see if a value is part of the vector we're comparing it to. 
#So here we are titanic.fullning two titles, "Mme" and "Mlle", 
#into a new temporary vector using the c() operator and seeing if any of the existing titles 
#in the entire Title column match either of them. We then replace any match with "Mlle"

#Reducing the outliers to more simple titles
titanic.full$Title[titanic.full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
titanic.full$Title[titanic.full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#Change the variable back to a factor
titanic.full$Title <- factor(titanic.full$Title)

#Two variables SibSb and Parch that indicate the number of family members the passenger is travelling with
titanic.full$FamilySize <- titanic.full$SibSp + titanic.full$Parch + 1
titanic.full$Fare2 <- as.factor(titanic.full$Fare2)

##Remove PassengerID, Name, Ticket, and Cabin attributes. Removing columns 1,4,9 and 11
titanic.full <- titanic.full[, -c(1, 4, 9, 11)]

##############################
# Separate datasets back out #
##############################

titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.test
titanic.train

#Decsion Trees 

library(rpart)
#rpart command works similarly to the aggregate function 
#You feed it the equation, headed up by the variable of interest and followed by the variables used for prediction

titanic.tree <- rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + Title + Fare2 + Child + FamilySize,
             data=titanic.train,
             method="class")

summary(titanic.tree)

#Plot of the decision tree
plot(titanic.tree)
text(titanic.tree)

library(rpart.plot)
rpart.plot(titanic.tree)

#New prediction with decision tree 
titanic.prediction <- predict(titanic.tree, titanic.test, type = "class")
titanic.prediction
titanic.submit <- data.frame(PassengerId = titanic.test$PassengerId, Survived = titanic.prediction)
View(titanic.submit)
write.csv(titanic.submit, file = "titanictree.csv", row.names = FALSE) 

##Comparison table
titanic.comparison <- titanic.test
titanic.comparison$Predictions <- titanic.prediction
#titanic.comparison[ , c("Survived", "Predictions")]

##Extract the test data to build the confusion matrix
titanic.confusion <- table(titanic.prediction, titanic.submit$Survived)
titanic.submit$Survived
titanic.confusion  #I dont have the response variable for test!!!!????

####Parameter Tuning ####

##Setting control parameters for rpart
##Check ?rpart.control for what the parameters do
tree.params <- rpart.control(minsplit=20, minbucket=7, maxdepth=30, cp=0.01)

##Fit decision model to training set
##Use parameters from above and Gini index for splitting
titanic.tree <- rpart(Survived ~ ., data = titanic.train, 
                   control=tree.params, parms=list(split="gini"))

#-----------------------

##Load the library
library(randomForest)

#install.packages("randomForest")
head(titanic.train)

table(titanic.train$Survived)

##Also, removing the IsTrainSet Boolean as it is TRUE for all the rows, can't be used as node 
titanic.train <- titanic.train[, -9]
titanic.test <- titanic.test[, -9]

titanic.rf.models <- randomForest(Survived ~ ., data=titanic.train, importance=TRUE, ntree=500)
print(titanic.rf.models)

#Tells you what varibles are important, use when add Importance = TRUE.
varImpPlot(titanic.rf.models) 

##MODEL EVALUATION
##Predict test set outcomes, reporting class labels
titanic.rf.predictions <- predict(titanic.rf.models, titanic.test, type="response")
titanic.rf.predictions

##Let'd make a copy of the test data 
titanic.test.outputs <- titanic.test
head(titanic.test.outputs)

##Let's check the frequency of dead and survived
table(titanic.test.outputs$Survived)

##Now, let's replace the Survived values in the test data by the values predicted by the model
titanic.test.outputs$Survived <- titanic.rf.predictions

##Let's check the frequency of dead and survived based on predictions
table(titanic.test.outputs$Survived)

write.csv(titanic.test.outputs, "my_output_file.csv", row.names = FALSE)



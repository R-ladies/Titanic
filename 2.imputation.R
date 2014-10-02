setwd("/Users/gqueiroz/Dropbox/Rladies/Meetups/Kaggle.Competition/Titanic/")
train <- read.csv("./data/train.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = "")
str(train)

train$Survived <- as.factor(train$Survived)
train$Sex <- as.factor(train$Sex)
train$Pclass <- as.factor(train$Pclass)
train$Embarked <- as.factor(train$Embarked)

# Time to tackle those missing ages. 
# A common approach to this type of situation is to replacing the missings with the average of the available values. 
# In this case, that would mean replacing 177 missing Age values with 29.7.
summary(train$Age) # 177 NAs

# ===================================
#           AGES  
# ===================================
head(train$Name, n = 10L) # Notice the titles -- Mr., Mrs., Miss., Master. -- following each of the surnames. 


## function for extracting honorific (i.e. title) from the Name feature
getTitle <- function(data) {
    title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
    title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
    data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
    return (data$Title)
}   

# Let's fetch the titles, given them their own column in the df.train data frame, and look at the uniques.
train$Title <- getTitle(train)
head(train)

unique(train$Title)
length(unique(train$Title)) # 17 uniques

# To identify the titles which have at least one record with an age missing, 
# I'll use the bystats function from the Hmisc package.
options(digits=2)
# install.packages("Hmisc")
library(Hmisc)
bystats(train$Age, train$Title, fun = function(x) c(Mean=mean(x),Median=median(x)))

# -----------------------------------------------------------------------------
# Imputation
# -----------------------------------------------------------------------------
# Now I can assign the titles with at least one missing Age value to a list

## list of titles with missing Age value(s) requiring imputation
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

# then pass that list to the following custom function I created for imputing the missing ages:   
imputeMedian <- function(impute.var, filter.var, var.levels) {
    for (v in var.levels) {
        impute.var[ which( filter.var == v)] <- 
            impute(impute.var[ which( filter.var == v)])
        }    
    return (impute.var)
    }
# I apply the impute function from the Hmisc package on a per-title basis to assign the median of the available ages to the missing age(s). 
# For example, the single record with a missing Age value and Title="Dr" will be assigned the median of the ages 
# from the 6 records with Title="Dr" which do have age data.
train$Age[which(train$Title=="Dr")]

# After doing the age imputations, I check the Age data and find that the function seems to have done its job.
train$Age <- imputeMedian(train$Age, train$Title, titles.na.train)
train$Age[which(train$Title=="Dr")]
# Checking the median
median(train$Age[train$Title=="Dr"],na.rm = TRUE)

options(digits=5)
summary(train$Age) # no more NAs


# ===================================
#           EMBARKED  
# ===================================
# You may recall that the Embarked feature also had at least one missing value. A summary of that data...
describe(train$Embarked) # 2 NAs
summary(train$Embarked) # 2 NAs
# ...reveals just two missings. It should be fine to replace those missings with "S", the most common value.
train$Embarked[which(is.na(train$Embarked))] <- 'S'
describe(train$Embarked) # No more NAs

# ===================================
#           FARE
# ===================================

# While there are no missing Fare values, a summary does show at least one Fare=0
summary(train$Fare)
subset(train, Fare < 7, select= c(Age, Title, Pclass, Fare))

#The jump in fares from 0 to the 4-7 range suggests errors. 
# I replaced the zero Fare values with the median fare from the respective passenger class using the imputMedian function introduced earlier.

## impute missings on Fare feature with median fare by Pclass
train$Fare[ which( train$Fare == 0 )] <- NA
train$Fare <- imputeMedian(train$Fare, train$Pclass, 
                              as.numeric(levels(train$Pclass)))
# I see the titles as more than merely a guide for imputation of missing ages. A passenger's title can reflect gender, his/her position on the ship (officers & royalty), and access to a lifeboat (where "Master" superceded "Mr"). Making the effort to get the Title feature model-ready seems worthwhile.

#Recall from the bystats results above that the training data contains 17 different titles. We already know that "Master" and "Mr" should separate the males into roughly two groups by age. The following script...
train$Title <- factor(train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))

boxplot(train$Age ~ train$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")
train$Title


## Title consolidation
train$Title <- gsub("Capt|Col|Don|Dr|Jonkheer|Lady|Major|Rev|Sir", "Noble", train$Title)
train$Title <- gsub("the Countess|Ms", "Mrs", train$Title)
train$Title <- gsub("Mlle|Mme", "Miss", train$Title)
table(train$Title)



# I assigned the Countess of Rothes, a woman in first class and the sole passenger with a "Countess" title, to the "Mrs" group. In retrospect, I could have placed her under the "Noble" umbrella. Given that 91 of the 94 female first-class passengers in the training set survived, I was willing to live with that choice.

#All of the work done designing the new Title column can be considered a part of feature engineering. The other features I chose to add are generated using custom function featureEngrg, which can be applied to both the training data in df.train and the Kaggle-provided test data in df.infer.
require(plyr)     # for the revalue function 
require(stringr)  # for the str_sub function

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEngrg <- function(data) {
    ## Using Fate ILO Survived because term is shorter and just sounds good
    data$Fate <- data$Survived
    ## Revaluing Fate factor to ease assessment of confusion matrices later
    data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
    ## Boat.dibs attempts to capture the "women and children first"
    ## policy in one feature.  Assuming all females plus males under 15
    ## got "dibs' on access to a lifeboat
    data$Boat.dibs <- "No"
    data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
    data$Boat.dibs <- as.factor(data$Boat.dibs)
    ## Family consolidates siblings and spouses (SibSp) plus
    ## parents and children (Parch) into one feature
    data$Family <- data$SibSp + data$Parch
    ## Fare.pp attempts to adjust group purchases by size of family
    data$Fare.pp <- data$Fare/(data$Family + 1)
    ## Giving the traveling class feature a new look
    data$Class <- data$Pclass
    data$Class <- revalue(data$Class, 
                          c("1"="First", "2"="Second", "3"="Third"))
    ## First character in Cabin number represents the Deck 
    data$Deck <- substring(data$Cabin, 1, 1)
    data$Deck[ which( is.na(data$Deck ))] <- "UNK"
    data$Deck <- as.factor(data$Deck)
    ## Odd-numbered cabins were reportedly on the port side of the ship
    ## Even-numbered cabins assigned Side="starboard"
    data$cabin.last.digit <- str_sub(data$Cabin, -1)
    data$Side <- "UNK"
    data$Side[which(isEven(data$cabin.last.digit))] <- "port"
    data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
    data$Side <- as.factor(data$Side)
    data$cabin.last.digit <- NULL
    return (data)
}

## add remaining features to training data frame
train <- featureEngrg(train)

train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                 "Class", "Deck", "Side", "Fare", "Fare.pp", 
                 "Embarked", "Family", "Survived")
train.munged <- train[train.keeps]
head(train.munged)

write.csv(train.munged,"/Users/gqueiroz/Dropbox/Rladies/Meetups/Kaggle.Competition/Titanic/data/train.munged.csv", row.names = FALSE, na = "" )

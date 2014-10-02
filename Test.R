load.project()
summary(train$Age)
head(train$Name)

train$Survived <- as.factor(train$Survived)
train$Sex <- as.factor(train$Sex)
train$Pclass <- as.factor(train$Pclass)
train$Embarked <- as.factor(train$Embarked)

getTitle <- function(data) {
    title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
    title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
    data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
    return (data$Title)
}   

train$Title <- getTitle(train)

head(train)

unique(train$Title)


options(digits=2)
#install.packages("Hmisc")
library(Hmisc)
bystats(train$Age, train$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))

titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")
    
imputeMedian <- function(impute.var, filter.var, var.levels) {
    for (v in var.levels) {
        impute.var[ which( filter.var == v)] <- 
            impute(impute.var[ which( filter.var == v)])
        }    
    return (impute.var)
    }

train$Age[which(train$Title=="Dr")]
train$Age[train$Title=="Dr"]


train$Age <- imputeMedian(train$Age, train$Title, titles.na.train)
train$Age[which(train$Title=="Dr")]
median(train$Age[train$Title=="Dr"],na.rm = TRUE)

summary(train$Embarked)

train$Embarked[which(is.na(train$Embarked))] <- 'S'


subset(train, Fare < 7)[order(subset(train, Fare < 7)$Fare, 
                                 subset(train, Fare < 7)$Pclass), 
                           c("Age", "Title", "Pclass", "Fare")]

## impute missings on Fare feature with median fare by Pclass
train$Fare[ which( train$Fare == 0 )] <- NA
train$Fare <- imputeMedian(train$Fare, train$Pclass, 
                              as.numeric(levels(train$Pclass)))

train$Title <- factor(train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))
boxplot(train$Age ~ train$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")
## function for assigning a new title value to old title(s) 
changeTitles <- function(data, old.titles, new.title) {
    for (honorific in old.titles) {
        data$Title[ which( data$Title == honorific)] <- new.title
    }
    return (data$Title)
}
## Title consolidation
train$Title <- changeTitles(train, 
                            c("Capt", "Col", "Don", "Dr", 
                              "Jonkheer", "Lady", "Major", 
                              "Rev", "Sir"),"Noble")
train$Title <- changeTitles(train, c("the Countess", "Ms"), "Mrs")
train$Title <- changeTitles(train, c("Mlle", "Mme"), "Miss")
train$Title <- as.factor(train$Title)
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
#install.packages("caret")
library(caret)
set.seed(23)
training.rows <- createDataPartition(train.munged$Survived, 
                                     p = 0.8,list=FALSE)
head(training.rows)
dim(training.rows)
dim(train)
train.batch <- train.munged[training.rows, ]
test.batch <- train.munged[-training.rows, ]
head(train.batch)
head(test.batch)
names(train.batch)
Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, 
                       data = train.batch, family=binomial("logit"))
Titanic.logit.1

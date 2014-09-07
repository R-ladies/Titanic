setwd("/Users/gqueiroz/Dropbox/Rladies/Meetups/Kaggle.Competition/Titanic/")
library('ProjectTemplate')
load.project()

train2 <- read.csv("./data/train.csv", header = TRUE, stringsAsFactors = FALSE)
str(train2)

#' Look at the first 10 rows
head(train, 10)
#' Look at the last rows
tail(train)

#' See the structuture of the data
str(train)
#' Variables
names(train)
#'Summary Stats
summary(train)
#' How may people survived?
table(train$Survived) # 342 passengers survived, while 549 died
#' How about a proportion?
prop.table(table(train$Survived))

mean(train$Age, na.rm = TRUE)
dim(subset(x = train, subset = train$Age < 10))


#' How many men and women?
summary(train$Sex) # Majority of passengers were male. 

# Now let’s expand the proportion table command we used last time to do a two-way comparison 
# on the number of males and females that survived:
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived), 1)  # row-wise

# Ages?
summary(train$Age)
table(train$Survived)


# Making basic visualizations
barplot(table(train$Survived),
        names.arg = c("Died", "Survived"),
        main="Survived (passenger fate)", col="red")

barplot(table(train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")

hist(train$Fare, main="Fare (fee paid for ticket[s])",
     xlab = NULL, 
     col="darkgreen")


mosaicplot(train$Pclass ~ train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")


mosaicplot(train$Sex ~ train$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, 
           color=TRUE, 
           xlab="Sex", ylab="Survived")


###############################################################################

names(train)
head(train$Name, n=10L)


#'Cleaning the TRAIN Data
# http://statsguys.wordpress.com/2014/01/11/data-analytics-for-beginners-pt-2/

#'Removing Variables Not Used for the Model
#' Remove the variables: PassengerID, Ticket, Fare, Cabin, and Embarked
train <- train[-c(1,9:12)]
head(train)
str(train)
#' Replacing Gender variable (Male/Female) with a Dummy Variable (0/1)
table(train$Sex)
train$Sex = gsub("female", 1, train$Sex)
train$Sex = gsub("^male", 0, train$Sex)
table(train$Sex)
#0   1 
#577 314

#' Making Inferences on Missing Age Values

#' Cause age entries could be an important variable we try inferencing them based on a relationship between title
#' and age;
#'  we’re essentially assuming that Mrs.X will older than Ms.X. Moreover, we’re (naively) assuming that people
#'  with the same titles are closer together in age.
#'So first, we put the index of people with the specified surname into a list for further processing. In R we use #' the grep() function which will return a vector of row numbers which have a specified surname.

# First change Name to be character
train$Name <- as.character(train$Name)

master_vector = grep("Master.",train$Name, fixed=TRUE)
print(master_vector)
head(train, 20)

miss_vector = grep("Miss.", train$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", train$Name, fixed=TRUE)
mr_vector = grep("Mr.", train$Name, fixed=TRUE)
dr_vector = grep("Dr.", train$Name, fixed=TRUE)


#'You might have noticed that there are other less frequent titles such as Reverend or Colonel which we are
#'ignoring for now.

for(i in master_vector) {
    train$Name[i] = "Master"
}

for(i in miss_vector) {
    train$Name[i] = "Miss"
}

for(i in mrs_vector) {
    train$Name[i] = "Mrs"
}

for(i in mr_vector) {
    train$Name[i] = "Mr"
}

for(i in dr_vector) {
    train$Name[i] = "Dr"
}



#' Making Inference on Missing Age Values: Inputting Title-group averages

# We replace the missing ages with their respective title-group average. This means that if we have a missing age # entry for a man named Mr. Bond, # we substitute his age for the average age for all passenger with the title Mr. # Similarly for Master, Miss, Mrs, and Dr. 
# We then write a for loop that goes through the entire Train data set and # checks if the age value is missing. 
# If it is, we assign it according to the surname of the observation. This code snippet is a bit complicated; you can just copy and paste for now if # you’re not confident about understanding it!
    
    
master_age = round(mean(train$Age[train$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(train$Age[train$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age = round(mean(train$Age[train$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(train$Age[train$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(train$Age[train$Name == "Dr"], na.rm = TRUE), digits = 2)
for (i in 1:nrow(train)) {
    if (is.na(train[i,5])) {
        if (train$Name[i] == "Master") {
            train$Age[i] = master_age
        } else if (train$Name[i] == "Miss") {
            train$Age[i] = miss_age
        } else if (train$Name[i] == "Mrs") {
            train$Age[i] = mrs_age
        } else if (train$Name[i] == "Mr") {
            train$Age[i] = mr_age
        } else if (train$Name[i] == "Dr") {
            train$Age[i] = dr_age
        } else {
            print("Uncaught Title")
        }
    }
}

#' Creating New Variables to Strengthen Our Model
#' VARIABLE 1: CHILD.
# This additional variable choice stems from the fact that we suspect that being a child might affect the survival rate of a passenger.

#'We start by creating a child variable. This is done by appending an empty column to the dataset, titled “Child”.
#'We then populate the column with value “1”, if the passenger is under the age of 12, and “2” otherwie.
train$Child <- c()
head(train)
for (i in 1:nrow(train)) {
    if (train$Age[i] <= 12) {
        train$Child[i] = 1
    } else {
        train$Child[i] = 2
    }
}

#'VARIABLE 2: FAMILY
# This variable is meant to represent the family size of each passenger by adding the number of Siblings/Spouses and Parents/Children (we add 1 so # # minimum becomes 1). 
# We’re guessing that larger families are less likely to survive, or perhaps it is the other way around. The beautiful part is that it doesn’t matter! 
# The model we build will optimize for the problem. All we’re indicating is that there might be a relationship between family size and survival rate.

train["Family"] = NA
head(train)
for(i in 1:nrow(train)) {
    x = train$SibSp[i]
    y = train$Parch[i]
    train$Family[i] = x + y + 1
}

#'VARIABLE 3: MOTHER
#We add another variable indicating whether the passenger is a mother.
#This is done by going through the passengers and checking to see if the title is Mrs and if the number of kids is greater than 0. This also includes any titles with Mrs and if the number of parents is greater than 0
train["Mother"] <- NA
for(i in 1:nrow(train)) {
    if(train$Name[i] == "Mrs" & train$Parch[i] > 0) {
        train$Mother[i] = 1
    } else {
        train$Mother[i] = 2
    }
}
head(train)

### MOVE TO ANALYSIS 
# Skipped the step: cleaning the test data (I'll do it later)

#' Training a Model  
#' 
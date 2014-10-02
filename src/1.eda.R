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

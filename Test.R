#load.project()
summary(train$Age)
head(train$Name)

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



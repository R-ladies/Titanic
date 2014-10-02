#install.packages("caret")
library(caret)

set.seed(23)
## split training data into train batch and test batch
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
#The deviance was reduced by 332.2 points on 713-705=8 degrees of freedom (DF), a significant reduction...
1 - pchisq(332.2, df=8)
anova(Titanic.logit.1, test="Chisq")

Titanic.logit.2 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare.pp,                        
                       data = train.batch, family=binomial("logit"))
anova(Titanic.logit.2, test="Chisq")

glm(Fate ~ Sex + Class + Age + Family + Embarked, 
    data = train.batch, family=binomial("logit"))
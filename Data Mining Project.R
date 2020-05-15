rm(list=ls())
survey=read.csv(file.choose(),header=T)
names(survey)
attach(survey)
summary(survey)

library(randomForest)
library(tree)
library(ggplot2)

## linear regression
set.seed(1)
train = sample(nrow(survey), nrow(survey)*0.7) # 70% testing data and 30% training
survey.train = survey[train,]
survey.test = survey[-train, ]
overallscore.test = overallscore[-train]


dim(survey.train)
dim(survey.test)

M1 = lm(overallscore ~., data = survey.train)
formula(M1)
coef(M1)
summary(M1)

## this is to find the most important predictors for the model 
# we want to start of with nothing in the model and then gradually add
# so our final model is shown using this formula 
# the scope formula M1 is saying that we want to include all of the var
fitstart = lm(overallscore ~ 1, data = survey.train)
step(fitstart, direction = "forward", scope = formula(M1))


overallscore.pred1 = predict(M1, newdata = survey.test)
M1MSE = mean((overallscore.test - overallscore.pred1)^2)


plot1 = ggplot(aes(x = overallscore.test, y= overallscore.pred1),
               data = data.frame(overallscore.test, overallscore.pred1))
plot1 + geom_point() + 
  geom_abline(color = 'red') +
  ggtitle(paste("Linear Regression in r^2=", summary(M1)$r.squared))

############################################################################


set.seed(1)
#bagginig create several subsets of data from training sample and reduce varience
#bag.suvey = randomForest(overallscore~., data=survey, subset=train, mtry=8, importance=TRUE)

train = sample(nrow(survey),nrow(survey)*0.7 )
tree.model=tree(overallscore~ gender+ lunch + race.ethnicity+ parental.level.of.education+ test.preparation.course,survey, subset=train)
survey.test=survey[-train,]
overallscore.test=overallscore[-train]

#10-fold cross validation to find the best subtree 
cv.model = cv.tree(tree.model, K=10)
cv.model

prune.model=prune.tree(tree.model,best=7)# I was not sure if best=8 or 7 is better please check the code output to check again

plot(prune.model)
text(prune.model,pretty=0)

#make a prune tree to make predictions in the testing set 
model.test=survey[-train,"overallscore"]
tree.pred = predict(prune.model, newdata=survey[-train,])
mean((tree.pred-model.test)^2)

set.seed(1)

bag.model=randomForest(overallscore~ gender+ lunch + race.ethnicity+ parental.level.of.education+ test.preparation.course,data=survey, subset=train, mtry=5, importance=TRUE)
bag.model

#performance of the bagging
yhat.bag = predict(bag.model,newdata=survey[-train,])
mean((yhat.bag-model.test)^2)

#Random Forest 
set.seed(1)
rf.model=randomForest(overallscore~ gender+ lunch + race.ethnicity+ parental.level.of.education+ test.preparation.course,data=survey,subset=train,mtry=2,importance=TRUE) #PLEASE double check if mtry should be 3
yhat.rf = predict(rf.model,newdata=survey[-train,])
mean((yhat.rf-model.test)^2)

# the last 2 lines show importance of each variable 

#mean decrease of accuracy in predictions on the out of bag samples when a given variable is excluded from the model. 
importance(rf.model)

#total decrease in node impurity
varImpPlot(rf.model)


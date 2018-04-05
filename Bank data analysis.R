rm(list=ls())

bankData = read.csv('C:/Users/Deepam Jain/Documents/GitHub/Bank-data-analysis/Input Data/Bank Case.csv',stringsAsFactors=TRUE)

str(bankData)
head(bankData)
summary(bankData)

#making the dependent variable a boolean variable

bankData$y <- bankData$y == 'yes'

#duration is not required for the analysis as the bank doesn't know how long the duration of a call is going 
#to be before making it

bankData$duration <- NULL

#set.seed(1)
isTraining = runif(nrow(bankData))<.7
trainingData = subset(bankData,isTraining)
validationData = subset(bankData,!isTraining)

#checking the correlation between variables

library(corrplot)

corrplot(cor(model.matrix(~.,data=bankData)))

#descriptive analytics to see initial statistics and picking the most useful variables

summary(lm(y~.,data=bankData))
anova(lm(y~.,data=bankData))

library('leaps')
basicSubset = regsubsets(y~.,data=bankData)
basicSummary = summary(basicSubset)

bestAIC = which.min(basicSummary$cp)
bestBIC = which.min(basicSummary$bic)

coef(basicSubset,bestBIC)

library(earth)
earth1 = earth(y~.,data=trainingData)
plotmo(earth1)

library(glmnet)

#lasso has a penalty term which can reduce the magnitude of coefficients all the way to zero

lassoFit = glmnet(model.matrix(~.,data=bankData[,-11]),bankData$y,alpha=1)
plot(lassoFit,xvar='lambda',sub='The X-Axis is the penalty parameters (logged)')

#stick to a higher lambda for explanatory analysis to see most important variable 

predict(lassoFit,s = .01, type = 'coefficients')

predict(lassoFit,s = .04, type = 'coefficients')

#running crossvalidation for predictions on lasso as well as other models

cvLassoFit = cv.glmnet(model.matrix(~.,data=bankData[,-11]),bankData$y,alpha=1)
predict(cvLassoFit,s = cvLassoFit$lambda.min, type = 'coefficients')

validationMat = model.matrix(~.,data=validationData[,-11])

cvLassoFit = cv.glmnet(model.matrix(~.,data=trainingData[,-11]),trainingData$y,alpha=1)
mean((validationData$y - predict(cvLassoFit,newx= validationMat, type = 'response',s=cvLassoFit$lambda.min)^2))

cvRidgeFit = cv.glmnet(model.matrix(~.,data=trainingData[,-11]),trainingData$y,alpha=0)
mean((validationData$y - predict(cvRidgeFit,newx= validationMat, type = 'response',s=cvRidgeFit$lambda.min)^2))

cvElasticFit = cv.glmnet(model.matrix(~.,data=trainingData[,-11]),trainingData$y,alpha=.5)
mean((validationData$y - predict(cvElasticFit,newx= validationMat, type = 'response',s=cvElasticFit$lambda.min)^2))
 
#it can be observed that loan and housing are the least important variables so remove it and rerun models

trainingMat2 = model.matrix(~.^2,data=trainingData[,-c(6,7,11)])
validationMat2 = model.matrix(~.^2,data=validationData[,-c(6,7,11)])

cvLassoFit2 = cv.glmnet(trainingMat2,trainingData$y,alpha=1)
mean((validationData$y - predict(cvLassoFit2,newx= validationMat2, type = 'response',s=cvLassoFit2$lambda.min)^2))

cvRidgeFit2 = cv.glmnet(trainingMat2,trainingData$y,alpha=0)
mean((validationData$y - predict(cvRidgeFit2,newx= validationMat2, type = 'response',s=cvRidgeFit2$lambda.min)^2))

cvElasticFit2 = cv.glmnet(trainingMat2,trainingData$y,alpha=.5)
mean((validationData$y - predict(cvElasticFit2,newx= validationMat2, type = 'response',s=cvElasticFit2$lambda.min)^2))

#cvLassoFit2 performs the best. Shrinkage methods actually reduce the variance of predictions, improving
#predictive performance.

#Running other models

#Test and Evaluate linear models.
lm1 = lm(y~age+factor(month),data=trainingData)
lm2 = lm(y~poly(age,3)+factor(month),data=trainingData)
lm3 = lm(y~.,data=trainingData)
lm4 = lm(y~.^2,data=trainingData)

mean((predict(lm1,validationData) - validationData$y)^2)
mean((predict(lm2,validationData) - validationData$y)^2)
mean((predict(lm3,validationData) - validationData$y)^2)
mean((predict(lm4,validationData) - validationData$y)^2)


anova(lm3)

#Housing/Loan seem unimportant.  Lets delete those. 
lm5 = lm(y~.^2,data=trainingData[,-c(6,7)])
mean((predict(lm5,validationData) - validationData$y)^2)

lm6 = lm(y~age+job+marital+education+default+contact+factor(month)+factor(day_of_week)+age*job+
           age*marital+age*education+job*default+job*month+marital*month+education*month+default*
           contact+default*month*contact*month+month*day_of_week,data=trainingData)

mean((predict(lm6,validationData) - validationData$y)^2)

library('earth')

earth1 = earth(y~.,data=trainingData)
earth2 = earth(y~.,data=trainingData,degree=2)
earth3 = earth(y~age+job+marital+education+default+contact+factor(month)+factor(day_of_week)+age*job+
                 age*marital+age*education+job*default+job*month+marital*month+education*month+default*
                 contact+default*month*contact*month+month*day_of_week,data=trainingData)
#Earth 2 is the best of these

mean((predict(earth1,validationData) - validationData$y)^2)
mean((predict(earth2,validationData) - validationData$y)^2)
mean((predict(earth3,validationData) - validationData$y)^2)


earth4 = earth(y~.,data=trainingData,degree=2,thres=0)
earth5 = earth(y~.,data=trainingData,degree=2,thres=0.01)
earth6 = earth(y~.,data=trainingData,degree=2,thres=0.1)

mean((predict(earth4,validationData) - validationData$y)^2)
mean((predict(earth5,validationData) - validationData$y)^2)
mean((predict(earth6,validationData) - validationData$y)^2)

#model 4 is the best model


lm1CallRanking = order(predict(lm1,validationData),decreasing = TRUE)
earth1CallRanking = order(predict(earth1,validationData),decreasing = TRUE)
#Best, partially tuned model:
earth4CallRanking = order(predict(earth4,validationData),decreasing = TRUE)

nCalls = 2000
#Count successes in top 2000 calls
randomSuccess = round(mean(validationData$y)*nCalls)
lm1Success = sum(validationData$y[lm1CallRanking[1:nCalls]])
earth1Success = sum(validationData$y[earth1CallRanking[1:nCalls]])
earth4Success = sum(validationData$y[earth4CallRanking[1:nCalls]])
print(c(paste(randomSuccess),paste(lm1Success),paste(earth1Success),paste(earth4Success)))

#Using Neural Nets for next set of predictions

install.packages("neuralnet")
library('neuralnet')
bankDataMat = model.matrix(~.,data=bankData)

#Rename some of the columns so the method works correctly
colnames(bankDataMat)[3] <- "jobbluecollar"
colnames(bankDataMat)[8] <- "jobselfemployed"

#Split into training/validatoin data 
trainingMat = bankDataMat[isTraining,]
validationMat = bankDataMat[!isTraining,]


#Generate a correct formula
col_list <- paste(c(colnames(validationMat[,-c(1,44)])),collapse="+")
col_list <- paste(c("yTRUE~",col_list),collapse="")
f <- formula(col_list)


#This fit a simple neural network with 3 units in the hidden layer.  
basicSingleLayerNNet <- neuralnet(f, data=trainingMat,
                                  algorithm = "rprop+",
                                  hidden=c(3),
                                  threshold=0.1,
                                  stepmax = 1e+06)

#Get predictions for the validation data (this is super finicky)
output <- compute(basicSingleLayerNNet, validationMat[,-c(1,44)],rep=1)
#Calculate out of sample performance
mean((validationMat[,44] - output$net.result)^2)

#This will take a lot of time to compute.
deepLearningNnet1 <- neuralnet(f, data=trainingMat,
                               algorithm = "rprop+",
                               hidden=c(3,3),
                               threshold=0.1,
                               stepmax = 1e+06)
output <- compute(deepLearningNnet1, validationMat[,-c(1,44)],rep=1)
mean((validationMat[,44] - output$net.result)^2)

plot(deepLearningNnet1)


deepLearningNnet2 <- neuralnet(f, data=trainingMat,
                               algorithm = "rprop+",
                               hidden=c(10,3),
                               threshold=0.1,
                               stepmax = 1e+06)
output <- compute(deepLearningNnet2, validationMat[,-c(1,44)],rep=1)
mean((validationMat[,44] - output$net.result)^2)

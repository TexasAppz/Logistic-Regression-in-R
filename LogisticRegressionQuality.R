# Logistic Regression in R

Quality = read.csv("Quality.csv")
str(Quality)

# Creating Training and Testing Sets
#install.packages("caTools")
library(caTools)

set.seed(88)
spl = sample.split(Quality$PoorCare, SplitRatio = 0.75)
QualityTrain = subset(Quality, spl==TRUE)
QualityTest = subset(Quality, spl==FALSE)

## Creating (="Training") the Model
# Building a Logistic Regression Model: First Try
QualityModel = glm(PoorCare ~ ERVisits + OfficeVisits + Narcotics + ProviderCount + NumberClaims + StartedOnCombination, data = QualityTrain, family=binomial)
summary(QualityModel)
# we will see below (line 29 onwards) what metrics to use to evaluate our model. We don't have R^2 anymore.
# Note: AIC is Akaike Information Criterion, estimates the relative quality of statistical models for a given set of data.
# we use AIC to compare multiple models and attempt to minimize information loss (from trying to creating a model)
# we want low AIC

# Refining the Model
# using p-value here
QualityModel = glm(PoorCare ~ OfficeVisits + Narcotics, data = QualityTrain, family=binomial)
summary(QualityModel)

# Evaluating the Model on the Training Set
PredictTrain = predict(QualityModel, type="response") 
#type=response gives you probabilities as predictions
#otherwise you would be getting the log-odds as predictions
PredictTrain
table(QualityTrain$PoorCare, PredictTrain > 0.5)
table(QualityTrain$PoorCare, PredictTrain > 0.7)
table(QualityTrain$PoorCare, PredictTrain > 0.2)

#install.packages("ROCR")
library(ROCR)

ROCRpred = prediction(PredictTrain, QualityTrain$PoorCare) #this transforms input data into a standardized format
ROCCurve = performance(ROCRpred, "tpr", "fpr") #computes performance measures true positive rate (tpr) and false positive rate (fpr)
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

# Testing the model
PredictTest = predict(QualityModel, type="response", newdata=QualityTest)
PredictTest
table(QualityTest$PoorCare, PredictTest > 0.5)
# What is the accuracy of your model on the testing set?

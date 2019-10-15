# Probability of the dependent variable "PoorCare" by P(y=1)
# indepedent vartibles x1,x2,...xk
# produce numbers b/w o and 1
# this functions platos close to zero and to one
# positive values of term inside exponential are predictive of class 1
# negative vaues are predictive of class zero

# Odds = P(y=1) / P(y=0); where if Odds > 1 y = 1 is more likely
# Odds = e^(b0 + b1x1 + ... + bkxk)
# log(Odds) = Bo + b1x1 + ... + bkxk ==> this is called "Logit" and looks like linear regression
# the bigger the Logit, the bigger P(y =1), and probability of receiving poor care
# Logit = -2.65 + 0.082(OfficeVisits) + 0.0076(Narcotics)
# glm = generalized linear model in R ( because a transformation of the Odds using the logit give us a linear equation)
# QualityModel = glm(PoorCare ~ ERVisits + OfficeVisits + Narcotics _ providerCount + NumberClaims + StartedOnCombination, data=TrainQuality, family =binomial)
#summary(QualityModel)
# threshold: if P(PoorCar = 1) >= t; predict poor quality
# if P(PoorCare = 1) < t; predict good quality
# select t = 0.5
#Confusion Matric = diagonal predict correct, and the other diagonal preodict incorrect
################    Predict Good (0)     Predict Poor (1)
#Actual Good (0)        TN                   FP
#Actual Poor (1)        FN                   TP
# Sensitivity = TP / (TP + FN)
# Specificity = TN (TN + FP)

#PredictTrain = predict(QuaityModel, type="ressponse")
#table(QualityTrain$PoorCare, PredictTrain > 0.5)
#table(QualityTrain$PoorCare, PredictTrain > 0.7)
#table(QualityTrain$PoorCare, PredictTrain > 0.2)

# x vs. y axis chart: False Positive Rate vs True Positive Rate
# TP rate (Sensitivity) on y-axis propagation of poor care caught
# False positive rate (1 - Specificity) on x-axis Proportion of good care labeled as poor care

#Evaluate the model: AUC (Area Under the Curve)
# max = 1, min = 0.5; less than min is bad; some Resuls are about 0.77

# Accuracey = (TN + TP) / # of observations;
# Threshold = 0.5 => Accuracy = 80 /99 = 80.8% (This is for training set)
# Baseline










install.packages('ROCR') 
install.packages('gtools')
install.packages('gdata')
install.packages('gplots')
install.packages('caTools')
library(caTools)
library(ROCR)
library(gtools)
library(gdata)
library(gplots)

Fram = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/LogisticReg-Files/Framingham.csv")

summary(Fram)
str(Fram)
set.seed(88)

spl <-sample.split(Fram$TenYearCHD, SplitRatio = 0.75)
FramTrain <- subset(Fram, spl==TRUE)
FramTest <- subset(Fram, spl==FALSE)
#FramReg <-lm(TenYearCHD ~ )

###########
parole = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/LogisticReg-Files/Parole.csv")
str(parole)
nrow(parole)
table(parole$Violator)
set.seed(88)

spl <-sample.split(parole$Violator, SplitRatio = 0.75)
ParoleTrain <- subset(parole, spl==TRUE)
ParoleTest <- subset(parole, spl==FALSE)
paroleModel =glm(Violator ~ Male + Age + State + TimeServed + RaceWhite+ MaxSentence + MultipleOffenses + Crime, data = ParoleTrain, family = binomial)
summary(paroleModel)

# logit = -3.47139 ... = -1.28281; Prob = 1/1+ e^-(logit) = 0.2171, 22%; --> 8%

















# Logistic Regression in R

FirstFram = read.csv("Framingham.csv")
str(FirstFram)
summary(FirstFram)
#Fram = read.csv("Framingham.csv")

#LINE BELOW ABOUT NA.OMIT IS EXTREMELY IMPORTANT
#BECAUSE GLM KNOWS TO OMIT MISSING DATA POINTS BUT NOT TABLE 
#AND YOU RUN INTO DIMENSION ERRORS
## COMMENT IT OUT AND SEE WHAT HAPPENS

#Fram = FirstFram
Fram = na.omit(FirstFram)
summary(Fram)
str(Fram)
head(Fram)




# Creating Training and Testing Sets
#install.packages("caTools")
library(caTools)

set.seed(88)
spl = sample.split(Fram$TenYearCHD, SplitRatio = 0.7)
FramTrain = subset(Fram, spl==TRUE)
FramTest = subset(Fram, spl==FALSE)
write.csv(FramTrain,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/LogisticReg-Files/FramTrain.csv")
write.csv(FramTest,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/LogisticReg-Files/FramTest.csv")

# Building a Logistic Regression Model: First Try
FramModel = glm(TenYearCHD ~ male + age + education + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose, data = FramTrain, family=binomial)
summary(FramModel)

# After Refining
FramModel = glm(TenYearCHD ~ male + age + cigsPerDay + prevalentHyp + totChol + sysBP + glucose, data = FramTrain, family=binomial)
summary(FramModel)


# Evaluating the Model
FramPredictTrain = predict(FramModel, type="response")
table(FramTrain$TenYearCHD, FramPredictTrain > 0.5)
# Accuracy for cutoff =0.5: (2160+36)/(2160+36+11+354)=85.75%
table(FramTrain$TenYearCHD, FramPredictTrain > 0.7)
#Accuracy for cutoff =0.7:(2171+5) / (2171+5 + 0 + 385) = 84.96%
table(FramTrain$TenYearCHD, FramPredictTrain > 0.2)
# Accuracy for cutoff = 0.2: (1699+219)/(1699+472+171+219)=74.89%

# With cutoff = 0.2 here I guess right more often than I guess wrong when actual =0 (true for all cutoffs) and when actual =1 (only true for this cutoff) 
# Baseline predicts 0, accuracy = (2160+11)/(2160+11+354+36)=84.77%
# But for our purposes it is not acceptable to predict no one will get CHD!
# At least with our model we can rank residents in decreasing order of risk

#install.packages("ROCR")
#install.packages("gplots")
#install.packages("stats")
library(stats)
library(gplots)
library(ROCR)

#AUC for training set
ROCRpred = prediction(FramPredictTrain, FramTrain$TenYearCHD)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

#AUC for testing set
FramPredictTest = predict(FramModel, type="response", newdata=FramTest)
table(FramTest$TenYearCHD, FramPredictTest > 0.2)
ROCRpred2 = prediction(FramPredictTest, FramTest$TenYearCHD)
ROCCurve2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCCurve2)
plot(ROCCurve2, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred2, "auc")@y.values)

######## Parole Example
Parole <- read.csv("/Users/jaimegarcia/Documents/DESKTOP_101/code/R_code/SMU/LogisticReg-Files/Parole.csv")
str(Parole)
nrow(Parole)
ncol(Parole)
table(Parole$Violator)
# 78/(597 + 78) = 11.6%


library(caTools)
library(ROCR)
set.seed(88)
spl = sample.split(Parole$Violator, SplitRatio = 0.70)
ParoleTrain = subset(Parole, spl==TRUE)
ParoleTest = subset(Parole, spl==FALSE)

ParoleModel = glm(Violator ~ Male + RaceWhite + Age + State + TimeServed +  MaxSentence + MultipleOffenses 
                  + Crime, data = ParoleTrain, family = binomial)
summary(ParoleModel)


ParoleModel = glm(Violator ~ Male + RaceWhite + Age + State + TimeServed +  MaxSentence + MultipleOffenses 
                  + Crime, data = ParoleTrain, family = binomial)
summary(ParoleModel)
# logit = -3.293 + 0.65(1) - 0.67(1) + 0.017*50 - 0.17*1 -0.068*3 +0.045*12 + 0.007 *1  =  -1.28281
# Prob = 1 / (1 + e^-(-1.28281)) = 0.2171 = 22% (Model not refined)
#If model refined (removed Crime)  -> Final model w/ State and MultipleOffenses  ->Prob of Violation = 8%










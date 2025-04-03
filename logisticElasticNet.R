#Load packages 
packs = c('dplyr', 'caret','pROC','glmnet')
lapply(packs,require,character.only=TRUE)

#Data load
load("numeric_features_retained_no_YeoJ.RData")
load("factor_features_retained.RData")
totaldata = cbind(numeric_features_retained_no_yeoJ,
                  factor_features_retained)
#Remove those with a missing flag
totaldata = totaldata[totaldata$missingFlag != 1, ]
#Remove columns with zero variance and missing flag
totaldata = totaldata[,-c(22,29)]

#Preprocessing and cross validation
set.seed(1)
trainIndex = createDataPartition(totaldata$loan_status, p = 0.25, list = FALSE)
Y = select(totaldata, loan_status) %>% unlist(.)
Xdf = select(totaldata, -loan_status)

#Train/test splits
Ytrain = Y[trainIndex]
Ytest = Y[-trainIndex]

#Create dummies 
X = model.matrix(~., Xdf)
XnoInt = X[,-1] #Remove intercept
rm(X)

XtrainNumeric = XnoInt[,1:18]
XtrainDummy = XnoInt[19:ncol(XnoInt)]

#Train/test splits
XtrainNumeric = XnoInt[trainIndex,]
centerScaleTrain = preProcess(Xtrain, method = c('center','scale'))
Xtrain = predict(centerScaleTrain, Xtrain)
Xtest = predict(centerScaleTrain, XnoInt[-trainIndex,])

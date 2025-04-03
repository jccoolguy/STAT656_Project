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
trainIndex = createDataPartition(totaldata$loan_status, p = 0.50, list = FALSE)
Y = select(totaldata, loan_status) %>% unlist(.)
Xdf = select(totaldata, -loan_status)

#Train/test splits
Ytrain = Y[trainIndex]
Ytest = Y[-trainIndex]

levels(Ytrain)
Ytrain = relevel(Ytrain, ref = "Fully Paid")
Ytest = relevel(Ytest, ref = "Fully Paid")

#Create dummies 
X = model.matrix(~., Xdf)
XnoInt = X[,-1] #Remove intercept
rm(X)

XNumeric = XnoInt[,1:18]
XDummy = XnoInt[, 19:ncol(XnoInt)]
rm(XnoInt)

#Train/test splits
XtrainNumeric = XNumeric[trainIndex,]
centerScaleTrain = preProcess(XtrainNumeric, method = c('center','scale'))
XtrainNumeric = predict(centerScaleTrain, XtrainNumeric)
XtestNumeric = predict(centerScaleTrain, XNumeric[-trainIndex,])

XtrainDummy = XDummy[trainIndex, ]
XtestDummy = XDummy[-trainIndex, ]


#Combine data
traindata = cbind(XtrainNumeric,XtrainDummy)
testdata = cbind(XtestNumeric, XtestDummy)
rm(XtestNumeric, XtrainNumeric,XtestDummy,XtrainDummy,Xdf,XDummy,XNumeric,totaldata)

#Fit model
K = 10
trainControl = trainControl(method="cv",
                            number = K)
tuneGrid = expand.grid('alpha'=c(0,.25,.5,.75,1),
                       'lambda' = seq(1*10^(-6),
                                      .002,
                                      length.out = 30))
elasticMod = train(x = traindata,
                   y = Ytrain,
                   method = "glmnet",
                   trControl = trainControl,
                   tuneGrid = tuneGrid)
elasticMod$bestTune

#Dimension of best model
dim(elasticMod$finalModel$beta)
elasticMod$finalModel$beta[,1:6]


matplot(x = log(elasticMod$finalModel$lambda),
        t(elasticMod$finalModel$beta),
        type='l',
        ylab='Coefficient Path',
        xlab = 'log(lambda)')
abline(v = log(elasticMod$bestTune$lambda))




#Fit logistic regression
glmnetOut = glmnet(x = traindata,
                   y = Ytrain,
                   alpha = elasticMod$bestTune$alpha,
                   family = 'binomial',
                   standardize = FALSE)

betaHat = coef(glmnetOut, s=elasticMod$bestTune$lambda)
betaHat


probHatTest = predict(glmnetOut,
                      testdata,
                      s=elasticMod$bestTune$lambda,
                      type = 'response')
YhatTestGlmnet = ifelse(probHatTest > 0.12, 'Charged Off', 'Fully Paid')
YhatTestGlmnet = as.factor(YhatTestGlmnet)
YhatTestGlmnet = relevel(YhatTestGlmnet, ref= "Fully Paid")

table(Ytest, YhatTestGlmnet)


mean(YhatTestGlmnet == Ytest)

#ROC
rocOut = roc(response = Ytest,
             probHatTest)
plot(rocOut)

ind = which.min(rocOut$sensitivities >= 0.80)
rocOut$thresholds[ind]
rocOut$sensitivities[ind]
rocOut$specificities[ind]

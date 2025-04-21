# Ignore following code
# 
# #Data load
# load("numeric_features_retained_no_YeoJ.RData")
# load("factor_features_retained.RData")
# totaldata = cbind(numeric_features_retained_no_yeoJ,
#                   factor_features_retained)
# 
# #Remove those with a missing flag
# totaldata = totaldata[totaldata$missingFlag != 1, ]
# 
# #Remove columns with zero variance and missing flag
# totaldata = totaldata[,-c(22,29)]
# 
# #Preprocessing and cross validation
# set.seed(1)
# trainIndex = createDataPartition(totaldata$nocreditrequirement, p = 0.50, list = FALSE)
# Y = select(totaldata, loan_status) %>% unlist(.)
# Xdf = select(totaldata, -loan_status)
# 
# #Train/test splits
# Ytrain = Y[trainIndex]
# Ytest = Y[-trainIndex]
# 
# levels(Ytrain)
# Ytrain = relevel(Ytrain, ref = "Fully Paid")
# Ytest = relevel(Ytest, ref = "Fully Paid")
# 
# #Create dummies 
# X = model.matrix(~., Xdf)
# XnoInt = X[,-1] #Remove intercept
# rm(X)
# 
# XNumeric = XnoInt[,1:18]
# XDummy = XnoInt[, 19:ncol(XnoInt)]
# rm(XnoInt)
# 
# #Train/test splits
# XtrainNumeric = XNumeric[trainIndex,]
# centerScaleTrain = preProcess(XtrainNumeric, method = c('center','scale'))
# XtrainNumeric = predict(centerScaleTrain, XtrainNumeric)
# XtestNumeric = predict(centerScaleTrain, XNumeric[-trainIndex,])
# 
# XtrainDummy = XDummy[trainIndex, ]
# XtestDummy = XDummy[-trainIndex, ]
# 
# 
# #Combine data
# traindata = cbind(XtrainNumeric,XtrainDummy)
# testdata = cbind(XtestNumeric, XtestDummy)
# rm(XtestNumeric, XtrainNumeric,XtestDummy,XtrainDummy,Xdf,XDummy,XNumeric,totaldata)

# Load data
load("smote_train.Rda")
load("Ytrain_smote.Rda")
load("Ytest.Rda")
load("Xtest.Rda")

# Load packages 
packs = c('dplyr', 'caret','pROC','glmnet')
lapply(packs,require,character.only=TRUE)

#Fit model
K = 10
trainControl = trainControl(method="cv",
                            number = K)
tuneGrid = expand.grid('alpha'=c(0,.25,.5,.75,1),
                       'lambda' = seq(1*10^(-6),
                                      .002,
                                      length.out = 30))
elasticMod = train(x = smote_train,
                   y = Ytrain_smote,
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
        xlab = 'log(lambda)',
        xlim = c(log(1*10^(-6)), log(0.002)))
abline(v = log(elasticMod$bestTune$lambda))




#Fit logistic regression
glmnetOut = glmnet(x = no_smote_train,
                   y = Ytrain_nosmote,
                   alpha = elasticMod$bestTune$alpha,
                   family = 'binomial',
                   standardize = FALSE)

betaHat = coef(glmnetOut, s=elasticMod$bestTune$lambda)
betaHat


probHatTest = predict(glmnetOut,
                      testdata,
                      s=elasticMod$bestTune$lambda,
                      type = 'response')


#Calibration plot
ytest2test = relevel(Ytest,
                     "Charged Off")
calPlot = calibration(ytest2test ~ probHatTest,
                      cuts = 5)
xyplot(calPlot)

# #Preds for confusion matrix
# YhatTestGlmnet = ifelse(probHatTest > 0.5, 'Charged Off', 'Fully Paid')
# YhatTestGlmnet = as.factor(YhatTestGlmnet)
# YhatTestGlmnet = relevel(YhatTestGlmnet, ref= "Fully Paid")
# 
# #Confusion matrix
# table(Ytest, YhatTestGlmnet)
# 
# 
# mean(YhatTestGlmnet == Ytest)

#ROC
rocOut = roc(Ytest,
             as.vector(probHatTest))
plot(rocOut, legacy.axes=T)
rocOut$auc
roc_elastic_data_smote = cbind(rocOut$thresholds,rocOut$sensitivities,rocOut$specificities)
save(roc_elastic_data,
     file = "roc_elastic_data_smote.Rda")
# ind = which.min(rocOut$sensitivities >= 0.80)
# rocOut$thresholds[ind]
# rocOut$sensitivities[ind]
# rocOut$specificities[ind]





#Get optimized threshold
lossF = function(threshold, C = 0.8){
  YhatTestGlmnet = ifelse(probHatTest > threshold, 'Charged Off', 'Fully Paid')
  YhatTestGlmnet = as.factor(YhatTestGlmnet)
  YhatTestGlmnet = relevel(YhatTestGlmnet, ref= "Fully Paid")
  
  #Sensitivity calculation
  truepos = sum(YhatTestGlmnet == "Charged Off" & 
                  Ytest == "Charged Off")
  allpos = sum(Ytest == "Charged Off")
  sensitivityT = truepos / allpos
  
  #Specificity calculation
  trueneg = sum(YhatTestGlmnet == "Fully Paid" & 
                  Ytest == "Fully Paid")
  allneg = sum(Ytest == "Fully Paid")
  specificityT = trueneg / allneg
  
  return(C * sensitivityT + (1-C) * specificityT)
}



optim = optimize(f = lossF,
                 interval = c(0,1))
objectiveThreshold = optim$objective
objectiveThreshold

#Threshold = 0.2005444
#sensitivityT = 0.4590551
#specificityT = 0.8013406
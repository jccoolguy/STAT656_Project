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
        xlab = 'log(lambda)')
abline(v = log(elasticMod$bestTune$lambda))


#Fit logistic regression
glmnetOut = glmnet(x = smote_train,
                   y = Ytrain_smote,
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


#ROC
rocOut = roc(Ytest,
             as.vector(probHatTest))
plot(rocOut, legacy.axes=T)
rocOut$auc
roc_elastic_data_smote = cbind(rocOut$thresholds,rocOut$sensitivities,rocOut$specificities)
save(roc_elastic_data_smote,
     file = "roc_elastic_data_smote.Rda")



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
#sensitivityT = 0.9748031
#specificityT = 0.1518825
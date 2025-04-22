#Threshold calculation
#Get optimized threshold
lossF = function(threshold, C = 0.8){
  YhatTestGlmnet = ifelse(probHatTest$`Charged Off` > threshold, 'Charged Off', 'Fully Paid')
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

#
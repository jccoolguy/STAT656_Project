#Load packages 
packs = c('dplyr', 'caret','e1071','readr', 'stringr')
lapply(packs,require,character.only=TRUE)

load("jackPreprocessedData.RData")
load("treyPreprocessedData.RData")
load("credit_history_fixed.RData")
load("emp_length_factor.RData")

#Check that dimensions are the same 
dim(data_pre_processed)
dim(treyPreprocessedData)

#Merge data sets 
preprocessedTotal = cbind(data_pre_processed,
                          treyPreprocessedData)

#Drop 2 columns: issue_d & earliest_cr_y
preprocessedTotal = preprocessedTotal[,-c(34:35)]

#Add 2 columns: 
preprocessedTotal = cbind(preprocessedTotal,
                          credit_history)
preprocessedTotal = cbind(preprocessedTotal,
                          emp_length_factor)


#Remove currents 
preprocessedTotal = preprocessedTotal[preprocessedTotal$loan_status == "Fully Paid" | 
                                        preprocessedTotal$loan_status == "Charged Off" | 
                                        preprocessedTotal$loan_status == "Does not meet the credit policy. Status:Fully Paid" | 
                                        preprocessedTotal$loan_status == "Does not meet the credit policy. Status:Charged Off",]

unique(preprocessedTotal$loan_status)
dim(preprocessedTotal)

#Create dummy variable for "not meeting credit requirement
nocreditrequirement = str_detect(preprocessedTotal$loan_status,
                                                   "Does not meet the credit policy.")
preprocessedTotal$nocreditrequirement = as.factor(ifelse(nocreditrequirement == T, 
                                               1, 
                                               0))

#Reset loan status column 
preprocessedTotal$loan_status = as.factor(ifelse(preprocessedTotal$loan_status == "Does not meet the credit policy. Status:Fully Paid",
                                       "Fully Paid",
                                       ifelse(preprocessedTotal$loan_status == "Does not meet the credit policy. Status:Charged Off",
                                              "Charged Off",
                                              ifelse(preprocessedTotal$loan_status == "Charged Off",
                                                     "Charged Off",
                                                     "Fully Paid"))))

unique(preprocessedTotal$loan_status)

#See which observations have a lot of missing values 
#Check rows with a lot of NAs
i = 1:nrow(preprocessedTotal)
naCounts = rowSums(is.na(preprocessedTotal[i,]))
head(naCounts)
sum(naCounts > 10)

#Set up flag column 
missingFlag = ifelse(naCounts>10,
                     1,
                     0)

preprocessedTotal$missingFlag = as.factor(missingFlag)

#Numeric variables 
preprocessedTotalNum = select_if(preprocessedTotal,
                               is.numeric) 
str(preprocessedTotalNum)

#Factor variables 
preprocessedTotalFactor = select_if(preprocessedTotal,
                                 is.factor) 
str(preprocessedTotalFactor)

#Character variables 
preprocessedTotalChr = select_if(preprocessedTotal,
                                    is.character) 
str(preprocessedTotalChr)

#Take out columns with near zero variance 
preprocessedTotalNum2 = preprocessedTotalNum %>% 
  preProcess(method = c('nzv')) %>%
  predict(newdata = preprocessedTotalNum)

dim(preprocessedTotalNum2)

#NA median imputation
preprocessedTotalNum3 = preProcess(preprocessedTotalNum2,
                           method = 'medianImpute',
                           k = 5)
preprocessedTotalNumImpute = predict(preprocessedTotalNum3,preprocessedTotalNum2)
anyNA(preprocessedTotalNumImpute)


#Check skewness/extreme observations 
skewed = apply(preprocessedTotalNumImpute,2,skewness)
skewed

#Yeo J transformation 
preprocessedTotalNumYeoJ = preprocessedTotalNumImpute %>%
  select_if(abs(skewed) > 1.5) %>%
  preProcess(method = "YeoJohnson") %>% 
  predict(newdata = preprocessedTotalNumImpute)

plotData = data.frame('skewed' = preprocessedTotalNumImpute$revol_bal,
                      'unskewed' = preprocessedTotalNumYeoJ$revol_bal)
plotData %>% ggplot() +
  geom_histogram(aes(x=skewed, y = after_stat(density)), alpha = 0.5,  fill = 'blue') + 
  geom_histogram(aes(x = unskewed,  y = after_stat(density)), alpha = 0.5, color = 'red')

#Extreme values 
pcaOut = prcomp(preprocessedTotalNumYeoJ,
                scale=TRUE,
                center=TRUE)
preprocessedTotalNumYeoJscores = data.frame(pcaOut$x)
ggplot(data = preprocessedTotalNumYeoJscores) + 
  geom_point(aes(x = PC1, y = PC2))

#Nothing stands out 



#Save as RData
save(preprocessedTotalNumImpute,
     file = "NumericVariablesWithoutYeoJ.RData")
save(preprocessedTotalNumYeoJ,
     file = "NumericVariablesWithYeoJ.RData")
save(preprocessedTotalFactor,
     file = "FactorVariablesClean.RData")

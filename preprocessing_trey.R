#Load packages 
packs = c('dplyr', 'caret','e1071','readr')
lapply(packs,require,character.only=TRUE)

#Load data
LoanStats3a = read_csv("Database/LoanStats3a.csv")
problems(LoanStats3a) #Got a character when it expected a double
head(LoanStats3a)

#Get subset for preprocessing
treyPreprocessing = LoanStats3a %>%
  select(41:80)
head(treyPreprocessing)

#Check data structures 
str(treyPreprocessing)

#Number of unique values 
sapply(treyPreprocessing,
       function(x){ length(unique(x)) })


#Go one-by-one to double check structures

##last_pymnt_d does not have year so I left it as a character
##next_pyment_d does not have year so I left it as a character

##policy_code should be a factor 
##application_type should be a factor
treyPreprocessingFactors = treyPreprocessing %>%
  select(c("policy_code",
           "application_type")) %>%
  mutate_all(factor)


#Check for all missing values 
apply(treyPreprocessing,
      FUN = function(x){(sum(is.na(x))==length(x))},
      MARGIN = 2)

treyPreprocessingNumeric = treyPreprocessing %>%
  select(-c("bc_util",
            "bc_open_to_buy",
            "avg_cur_bal",
            "acc_open_past_24mths",
            "inq_last_12m",
            "total_cu_tl",
            "inq_fi",
            "total_rev_hi_lim",
            "all_util",
            "max_bal_bc",
            "open_rv_24m",
            "open_rv_12m",
            "il_util",
            "total_bal_il",
            "mths_since_rcnt_il",
            "open_il_24m",
            "open_il_12m",
            "open_il_6m",
            "open_acc_6m",
            "tot_cur_bal",
            "tot_coll_amt",
            "verification_status_joint",
            "dti_joint",
            "annual_inc_joint",
            "mths_since_last_major_derog",
            "policy_code",
            "application_type")) %>% 
  select(where(is.numeric)) 

#Check dimensions
dim(treyPreprocessingNumeric)
dim(treyPreprocessingFactors)



#Now that data types/NAs are checked, look at variance
table(treyPreprocessingFactors$policy_code)
table(treyPreprocessingFactors$application_type)

#Both factor variables have no variability...can remove

#Impute missing values in numeric variables 
#Impute missing values 
XimputeMedian = preProcess(treyPreprocessingNumeric,
                           method = 'medianImpute',
                           k = 5)
NumericNoMissing = predict(XimputeMedian,
                           treyPreprocessingNumeric)
anyNA(NumericNoMissing)

#Analyze variance...also center and scale
nearZeroVar(NumericNoMissing,
            saveMetrics = T)
treyPreprocessingNumericWVariance = NumericNoMissing %>% 
  preProcess(method = c('nzv')) %>%
  predict(newdata = NumericNoMissing)

dim(treyPreprocessingNumericWVariance)

#6 variables dropped due to near zero variance 


#Look at # of NAs in each of the numeric variables
sum(is.na(treyPreprocessingNumericWVariance$total_rec_prncp))
sum(is.na(treyPreprocessingNumericWVariance$total_rec_int))
sum(is.na(treyPreprocessingNumericWVariance$recoveries))
sum(is.na(treyPreprocessingNumericWVariance$last_pymnt_amnt))

#Now analyze skew 
skewnessVector = treyPreprocessingNumericWVariance %>%
  sapply(e1071::skewness,na.rm=T)

names(treyPreprocessingNumericWVariance)[abs(skewnessVector)> 1.5]

#Plot those with absolute skew > 1.5 
hist(treyPreprocessingNumericWVariance$total_rec_int,
     main = "Histogram",
     xlab = "Total Interest Received To Date")
hist(treyPreprocessingNumericWVariance$recoveries,
     main = "Histogram",
     xlab = "Post Charge Off Gross Recovery")
hist(treyPreprocessingNumericWVariance$last_pymnt_amnt,
     main = "Histogram",
     xlab = "Last Total Payment Amount Received")

#Perform Yeo-Johnson transformation 
YJTransformedVariables = treyPreprocessingNumericWVariance%>%
  select(c("total_rec_int",
           "recoveries",
           "last_pymnt_amnt"))%>%
  preProcess(method = 'YeoJohnson') %>%
  predict(newdata=treyPreprocessingNumericWVariance %>%
            select(c("total_rec_int",
                     "recoveries",
                     "last_pymnt_amnt")))

#Check histograms 
hist(YJTransformedVariables$total_rec_int,
     main = "Histogram",
     xlab = "YJ Total Interest Received To Date")
hist(YJTransformedVariables$recoveries,
     main = "Histogram",
     xlab = "YJ Post Charge Off Gross Recovery")
hist(YJTransformedVariables$last_pymnt_amnt,
     main = "Histogram",
     xlab = "YJ Last Total Payment Amount Received")


#Recoveries looks odd 
summary(treyPreprocessingNumericWVariance$recoveries)
sum(treyPreprocessingNumericWVariance$recoveries > 0,
    na.rm=T)


#Look for extreme values 
pcaOut = prcomp(YJTransformedVariables)

YeoJscores = data.frame(pcaOut$x)
ggplot(data = YeoJscores) + 
  geom_point(aes(x = PC1, y = PC2))


#2 extreme observations 
extremeObs1 = which.min(YeoJscores$PC1)
treyPreprocessingNumericWVariance[extremeObs1,]


extremeObs2 = which.min(YeoJscores$PC2)
treyPreprocessingNumericWVariance[extremeObs2,]


#Do correlation filtering once we get all numeric variables together? 

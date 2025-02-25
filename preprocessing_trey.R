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


#Check for missing values 
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

nearZeroVar(treyPreprocessingNumeric,
            saveMetrics = T)
treyPreprocessingNumericWVariance = treyPreprocessingNumeric %>% 
  preProcess(method = 'nzv') %>%
  predict(newdata = treyPreprocessingNumeric)

dim(treyPreprocessingNumericWVariance)

#6 variables dropped due to near zero variance 

#Now analyze skew 



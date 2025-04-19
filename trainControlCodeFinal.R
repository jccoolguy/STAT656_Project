# This file creates the training and test split
# to be used for model training and evaluation

# Load packages 
packs = c('dplyr', 'caret','pROC','glmnet')
lapply(packs,require,character.only=TRUE)

# Data load
load("numeric_features_retained_no_YeoJ.RData")
load("factor_features_retained.RData")
totaldata = cbind(numeric_features_retained_no_yeoJ,
                  factor_features_retained)

# Remove those with a missing flag
totaldata = totaldata[totaldata$missingFlag != 1, ]

# Remove columns with zero variance and missing flag
totaldata = totaldata[,-c(22,29)]

# Bin states into US Census regions 
west = c("WA", "OR", "CA", "NV", "ID", "MT", "UT", "AZ", "WY", "CO", "NM")
midwest = c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH")
south = c("TX", "OK", "AR", "LA", "MS", "AL", "TN", "KY", "GA", "FL", "SC", "NC", "VA", "WV", "DC", "MD", "DE")
northeast = c("PA", "NJ", "NY", "CT", "RI", "MA", "VT", "NH", "ME")
pacific = c("AK", "HI")

# Create region column
region = ifelse(totaldata$addr_state %in% west,
                "West",
                ifelse(totaldata$addr_state %in% midwest,
                       "Midwest",
                       ifelse(totaldata$addr_state %in% south,
                              "South",
                              ifelse(totaldata$addr_state %in% northeast,
                                     "Northeast",
                                     "Pacific"))))
head(totaldata$addr_state)
head(region)

totaldata$region = as.factor(region)
sum(is.na(totaldata$region))


#Drop state column
totaldata = totaldata %>% select(-addr_state)

#Drop pacific region
sum(totaldata$region=="Pacific")
totaldata = totaldata[totaldata$region!="Pacific",]
sum(totaldata$region=="Pacific")

#Stratify based on credit policy and outcome variable
totaldata$groups = as.factor(paste0(totaldata$loan_status,
                         totaldata$nocreditrequirement))
set.seed(123)
trainIndex = createDataPartition(totaldata$groups,
                                 p = 0.80,
                                 list = FALSE)


#Response and independent variables
totaldata = totaldata %>% select(-groups)
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

#Save
no_smote_train = traindata
save(no_smote_train,
     file="no_smote_train.Rda")
save(testdata,
     file = "Xtest.Rda")
Ytrain_nosmote = Ytrain
save(Ytrain_nosmote,
     file = "Ytrain_nosmote.Rda")
save(Ytest,
     file = "Ytest.Rda")

################################################################################

#Smote algorithm
library(smotefamily)
set.seed(123)
smote_train = SMOTE(X = as.data.frame(traindata),
                    target = Ytrain)

#Get corrected data
smote_train = smote_train$data
Ytrain_smote = as.factor(smote_train$class)
Ytrain_smote = relevel(Ytrain_smote, ref = "Fully Paid")
table(Ytrain_smote)
smote_train = smote_train %>% select(-class)

smote_train = save(smote_train,
                   file = "smote_train.Rda")
Ytrain_smote = save(Ytrain_smote,
                    file = "Ytrain_smote.Rda")

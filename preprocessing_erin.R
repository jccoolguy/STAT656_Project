library(readr)
require(dplyr)
require(caret)
LoanStats3a <- read_csv("Database/LoanStats3a.csv")

##I will be working with columns 81 through 111
erinsColumns = LoanStats3a[,81:111]

erinsDataQuant = erinsColumns %>%
  select(where(is.numeric))
erinsDataFactor = erinsColumns %>%
  select(where(is.factor))
erinsDataLogical = erinsColumns %>%
  select(where(is.logical))

##of my 31 columns, 2 are currently labeled as numeric and 29 are logical
##this may not be accurate



str(erinsColumns)
#2 showing as numeric are pub_rec_bankruptcies and tax_liens
#based on data dictionary both of these truly are numeric

variablesWithNum = grep('num',names(erinsColumns), value=TRUE)
variablesWithMonths = c(grep('mths',names(erinsColumns), 
                             value=TRUE),grep('mo_sin',names(erinsColumns), value=TRUE))
variablesWithTotal = grep('total',names(erinsColumns), value=TRUE)

erinsSegData = erinsColumns %>%
  mutate(across(all_of(variablesWithNum), as.numeric)) %>%
  mutate(across(all_of(variablesWithMonths), as.numeric))%>%
  mutate(across(all_of(variablesWithTotal), as.numeric))

str(erinsSegData)

#only 4 still showing as logical - checked with dictionary and they should be numeric too

erinsSegDataFinal = erinsSegData %>%
  mutate(across(names(erinsSegData), as.numeric))

str(erinsSegDataFinal)
#all 31 columns are numeric

i = seq(1,31,by=1)
naCounts = colSums(is.na(erinsSegDataFinal[,i]))
length(naCounts!=nrow(erinsColumns))
##for all but pub_rec_bankruptcies and tax_liens, every observation is missing

erinsNonMissingColumns = erinsSegDataFinal%>%select(c("pub_rec_bankruptcies","tax_liens"))

max(na.omit(erinsNonMissingColumns$tax_liens)) #1
mean(na.omit(erinsNonMissingColumns$pub_rec_bankruptcies)) #.045
mean(na.omit(erinsNonMissingColumns$tax_liens)) #.000024



erinsPreprocessedColumns = erinsSegDataFinal %>% preProcess(method = 'nzv') %>%
  predict(newdata = erinsColumns)

#both remaining columns are removed

##CONCLUSION: NONE OF THE 31 COLUMNS ARE HELPFUL FOR THE ANALYSIS





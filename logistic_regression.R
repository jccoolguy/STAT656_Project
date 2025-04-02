#Logistic Regression
trainingDataIndex = createDataPartition(flightsNotCancelled$OP_UNIQUE_CARRIER,
                                        p=.5,
                                        list = FALSE)
trainingData      = flightsNotCancelled[trainingDataIndex,]
testingData       = flightsNotCancelled[-trainingDataIndex,]

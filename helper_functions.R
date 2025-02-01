get_description = function(feature,dict = dictionary, column = "LoanStatNew"){
#'Performs a search on the data dictionary
  dict[str_detect(dict[,column], feature),]
}
load("emp_length.Rdata")

#Unique values and counts
table(emp_length)

#Removing year/years
emp_length_no_years = gsub(" year| years", "",as.vector(emp_length$emp_length))
table(emp_length_no_years)

#Creating factor levels
emp_length_factor = as.factor(ifelse(emp_length_no_years == "< 1",
                              "Minimal",
                              ifelse(emp_length_no_years == "n/a"|emp_length_no_years == "",
                                     "Unknown",
                                     ifelse(emp_length_no_years == "10+",
                                            "Very Long",
                                            ifelse(emp_length_no_years %in% c("1","2","3"),
                                                   "Short", ifelse(emp_length_no_years %in% c("4","5","6"),
                                                                   "Moderate",ifelse(emp_length_no_years %in% c("7","8","9"),
                                                                                     "Long","")))))))
#Verifying counts
table(emp_length_factor)

#Saving Down Object
save(emp_length_factor,
     file = "emp_length_factor")
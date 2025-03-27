load("emp_length.Rdata")

#Looking at current status of variable
emp_length |> 
  group_by(emp_length_no_year) |> 
  summarise(n = n())
emp_vector = as.vector(emp_length$emp_length_no_year)

#Dealing with strings
emp_vector_missings = ifelse(emp_vector == "n/a"|emp_vector == "","Unknown Job History",emp_vector)
emp_vector_less_than_one = ifelse(emp_vector_missings == "< 1 ","Short Job History",emp_vector_missings)
emp_vector_high_experience = ifelse(emp_vector_less_than_one == "10+ ", "Very Long Job History", emp_vector_less_than_one)

#Dealing with numbers

#Removing spaces
emp_vector_no_space = emp_vector_high_experience = sub(" ","",emp_vector_high_experience)

#Creating moderate and long history
strings = c("Unknown Job History","Short Job History", "Very Long Job History")
emp_vector_moderate = ifelse((!emp_vector_no_space %in% strings) & as.numeric(emp_vector_no_space) >= 1 & as.numeric(emp_vector_no_space) <= 5,"Moderate Job History",
                             emp_vector_no_space)




#emp_vector_low = ifelse(emp_vector_missings == "< 1 "|emp_vector <= 3,"Low Experience",emp_vector_missings)
#emp_vector_high = ifelse(emp_vector_moderate == "10+ ","High Experience", emp_vector_moderate)
#emp_vector_moderate = ifelse(emp_vector_low >= 3 & emp_vector <= 10, "Moderate Experience",emp_vector_low)
#emp_vector_high = ifelse(emp_vector_moderate == "10+ ","High Experience", emp_vector_moderate)
#emp_vector_high
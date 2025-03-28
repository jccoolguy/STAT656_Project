library(tidyverse)
load("credit_dates.RData")

#examining unique values for both variables
credit_dates |> 
  group_by(issue_d) |> 
  summarise(count = n()) |> 
  print(n = 60)
#Issue date field is consistently Y-MMM

credit_dates |> 
  group_by(earliest_cr_line) |> 
  summarise(count = n()) |> 
  view()
#Earliest credit line varies between Y-MMM and MMM-YY. Seems to change at year of 2001.

#Getting Month and Year for both variables

#For Issue Date we use %y - %b pattern as it is consistent throughout
issue_date <- credit_dates |> 
  mutate(issue_date = as.double(as.yearmon(issue_d,"%y-%b"))) |>
  select(issue_date)

#For Earliest Credit Date we need to use an if statement to check the placement of "-", the placement changes when going from 2000 onward, we alo note that those need to be 1900 years
# and not 2000 years, with the exception of 00
earliest_cr_date <- credit_dates |> 
  mutate(earliest_cr_date = 
           ifelse(str_sub(earliest_cr_line,4,4) == "-",
                  as.yearmon(paste(str_sub(earliest_cr_line,1,4),"19",str_sub(earliest_cr_line,-2,-1), sep = ""),"%b-%Y"),
                  as.yearmon(earliest_cr_line,"%y-%b"))) |> 
  mutate(earliest_cr_date =
           ifelse(str_sub(earliest_cr_line,-2,-1) == "00",
                  as.yearmon(earliest_cr_line, "%b-%y"), earliest_cr_date)) |> 
  select(earliest_cr_date)

#Creating a credit history variable           
credit_history <- cbind(issue_date,earliest_cr_date) |> 
  mutate(credit_history = issue_date - earliest_cr_date) |> 
  select(credit_history)

save(credit_history,
     file = "credit_history_fixed.RData")

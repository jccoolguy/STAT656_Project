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

credit_dates |> 
  mutate(issue_date = as.Date(as.yearmon(issue_d,"%y-%b")))
#library(zoo)
#credit_dates |>
#  mutate(index = as.integer(gregexpr("-",issue_d))) |>
#  mutate(issue_year = 2000 + as.integer(str_sub(issue_d,1,index - 1)), issue_month = str_sub(issue_d, -3, -1))
  
  #mutate(issue_year = issue_d[1:index])
 # mutate(issue_year = issue_d[1:(as.numeric(gregexpr("-",issue_d)[[1]][1])-1)])

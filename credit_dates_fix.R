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

#credit_dates |> 
#  mutate(issue_year = issue_d =  )

library(tidyverse)
library(readxl)

## Extract data from excel control sheet
grw_3u_excel <-
  read_excel("process_data/excel_control.xlsx",
             sheet = "grw_3u",
             range = "A1:B83") %>% 
  drop_na()


grw_3u <- 
  grw_3u_excel %>% 
  select(growth)


previous_year_grw <-
grw_3u_excel %>%
  mutate(years = growth<1 | growth > 1) %>% 
  filter(years == first(years)) %>% 
  last() %>% 
  select(year) %>% 
pull(year)

######################################################
library(tidyverse)

grw_3u <- 
  
  read_excel("process_data/excel_control.xlsx",
           sheet = "grw_3u",
           range = "A1:B83") %>% 
  drop_na() %>% 
  select(growth)

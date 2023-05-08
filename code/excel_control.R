library(tidyverse)
library(readxl)

grw_3u <- 
  
  read_excel("process_data/excel_control.xlsx",
           sheet = "grw_3u",
           range = "A1:B83") %>% 
  drop_na() %>% 
  select(growth)

read_excel("process_data/excel_control.xlsx",
           sheet = "grw_3u",
           range = "A1:B83") %>% 
  drop_na() %>% 
  mutate(year_slc = if_else(growth < 1, 
                            read_excel("process_data/excel_control.xlsx",
                                       sheet = "grw_3u",
                                       range = "A1:B83") %>% 
                              drop_na() %>% 
                              mutate(yesr = year - 1), 
                            read_excel("process_data/excel_control.xlsx",
                                       sheet = "grw_3u",
                                       range = "A1:B83") %>% 
                              drop_na() %>% 
                              mutate(yesr = year + 1)))

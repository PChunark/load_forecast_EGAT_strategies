library(tidyverse)  # Package for data manipulation
library(readxl) #Package for reading excel file. It is not a core package in tidyverse package

#Electricity use in the office and others
use_office <- 
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
             sheet = "C_LossUse",
             range = "C18:D45",
             col_names = c("year_th", "ely_gwh")) %>% 
  mutate(year = year_th - 543)

#Electricity use in egat substations and their office
use_egatsub <- 
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
             sheet = "C_LossUse",
             range = "C59:D86",
             col_names = c("year_th", "ely_gwh")) %>% 
  mutate(year = year_th - 543)

#Electricity use in station service (off-load)
use_statserv <- 
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
             sheet = "C_LossUse",
             range = "C100:D127",
             col_names = c("year_th", "ely_gwh")) %>% 
  mutate(year = year_th - 543)

#Electricity use in Mae Moh
use_maemoh <- 
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
             sheet = "C_LossUse",
             range = "C149:D176",
             col_names = c("year_th", "ely_gwh")) %>% 
  mutate(year = year_th - 543)

#Electricity use in pump and storage
use_pump <- 
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
             sheet = "C_LossUse",
             range = "C190:D217",
             col_names = c("year_th", "ely_gwh")) %>% 
  mutate(year = year_th - 543)

#Total use: office + egatsub + statserv + maemoh + pump
total_usepump <- 
  
  use_office %>% 
  mutate(use_egatsub  = use_egatsub$ely_gwh,
         use_statserv = use_statserv$ely_gwh,
         use_maemoh   = use_maemoh$ely_gwh,
         use_pump     = use_pump$ely_gwh,
         total        = ely_gwh + use_egatsub + use_statserv + use_maemoh + use_pump)



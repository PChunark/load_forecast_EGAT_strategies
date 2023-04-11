tran_loss_percent <- 
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
             sheet = "C_LossUse",
             range = "C321:D348",
             col_names = c("year_th", "percent_loss")) %>% 
  mutate(year = year_th - 543)

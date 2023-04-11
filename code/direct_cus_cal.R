read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final_Sent อศง.xlsx",
           sheet = "DCs",
           range = "A6:K44",
           col_names = c("direct_cus", 2019:2028)) %>% 
  drop_na() %>% 
  filter(str_detect(direct_cus, "Energy Demand ")) %>% 
  pivot_longer(-direct_cus, names_to = "year", values_to = "dc_gwh")
  
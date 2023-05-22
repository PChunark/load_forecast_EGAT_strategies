## Extract the PEA electricity demand (energy requirement) till 2022
pea_ene_ext <-
read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final_Sent อศง.xlsx",
           sheet = "PEA",
           range = "A41:N56",
           col_names = c("category","a", 2017:2028)) %>%
  select(!a) %>% 
  pivot_longer(-category, names_to = "year", values_to = "pea_gwh") %>%
  filter(category == "ความต้องการพลังงานไฟฟ้าในระบบไฟฟ้า กฟภ.") %>% 
  mutate(year = as.numeric(year),
         year_th = year + 543) %>% 
  select(year_th, year, pea_gwh, !category) %>% 
  filter(year %in% c(2017:2022))

 pea_ene_latest <-
  pea_ene_ext %>%
  last() %>% 
  select(pea_gwh) %>% 
  pull()
  
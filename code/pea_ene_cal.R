library(tidyverse)

#Extract budget load from PEA @budget load 66-67

b_pea_ene <-
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final_Sent อศง.xlsx",
             sheet = "PEA",
             range = "A41:N56",
             col_names = c("category","a", 2017:2028)) %>%
  select(!a) %>% 
  pivot_longer(-category, names_to = "year", values_to = "pea_gwh") %>%
  filter(category == "- ซื้อจาก กฟผ.") %>% 
  mutate(year_th = as.numeric(year) + 543)  

#Extract PDP2022 load from MEA in NIDA _BAU_ND_EE70
pdp2022_pea_ene <-
  
  read_excel("raw_data/93 EGAT_25Aug2022_13 Energy ภาพรวม NIDA_BAU_ND_EE70__Sent.xlsx",
             sheet = "PEA",
             range = "AA11:AW20",
             col_names = c("category","a", 2017:2037)) %>%
  select(!a) %>% 
  pivot_longer(-category, names_to = "year", values_to = "pea_gwh") %>%
  filter(category == "- ซื้อจาก กฟผ.") %>% 
  mutate(year_th = as.numeric(year) + 543)  

#Combine budget load and PDP2022 load

newpea_ene <-
  
rbind(b_pea_ene %>% filter(year >= 2019), 
      pdp2022_pea_ene %>% filter(year >=2029)) #%>% 
# ggplot(aes(x = year, y = pea_gwh, group = category))+
# geom_line()

#combine MEA and PEA
mea_pea_ene <- cbind(newmea_ene, newpea_ene %>% select(pea_gwh)) %>% 
  mutate(mea_pea_gwh = mea_gwh + pea_gwh,
         shr_mea = mea_gwh / mea_pea_gwh,
         shr_pea = pea_gwh / mea_pea_gwh)

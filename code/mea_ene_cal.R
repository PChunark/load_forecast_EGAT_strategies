library(tidyverse)

#Extract budget load from MEA @budget load 66-67
b_mea_ene <-
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final_Sent อศง.xlsx",
             sheet = "MEA",
             range = "A8:N27",
             col_names = c("sector","a", 2017:2028)) %>%
  select(!a) %>% 
  drop_na() %>% 
  pivot_longer(-sector, names_to = "year", values_to = "mea_gwh") %>%
  # ggplot(aes(x = year, y = mea_gwh, group = sector, color = sector))+
  # geom_line(show.legend = FALSE)+
  # facet_wrap(~sector, scales = "free_y")
  mutate(year_th = as.numeric(year) + 543) %>% 
  filter(sector == "From EGAT")
  
#Extract PDP2022 load from MEA in NIDA _BAU_ND_EE70
pdp2022_mea_ene <-
  
read_excel("raw_data/93 EGAT_25Aug2022_13 Energy ภาพรวม NIDA_BAU_ND_EE70__Sent.xlsx",
           sheet = "MEA",
           range = "A8:AB33",
           col_names = c("sector","a", 2012:2037)) %>%
  select(!a) %>% 
  drop_na() %>% 
  pivot_longer(-sector, names_to = "year", values_to = "mea_gwh") %>%
  # ggplot(aes(x = year, y = mea_gwh, group = sector, color = sector))+
  # geom_line(show.legend = FALSE)+
  # facet_wrap(~sector, scales = "free_y")
  mutate(year_th = as.numeric(year) + 543) %>% 
  filter(sector == "From EGAT")

#Combine budget load and PDP2022 load

newmea_ene <-
  
  rbind(b_mea_ene %>% filter(year >= 2019), 
        pdp2022_mea_ene %>% filter(year >=2029)) #%>% 
  # ggplot(aes(x = year, y = mea_gwh, group = sector))+
  # geom_line()
 
  
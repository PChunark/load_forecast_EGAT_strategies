library(tidyverse)
library(readxl)

mea_vspp_ene_ext <-

read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
           sheet = "I_Actual",
           range = "CE7:CM31",
           col_names = c("solar", "wind", "biomass", "biogas", "msw", "hydro", "enecrop", "cogen", "tot_vspp_gwh")) %>% 
  replace(is.na(.),0) %>% 
  mutate(year = 1997:2021,
         year_th = year + 543) %>% 
  select(year, year_th,everything()) %>%
  pivot_longer(-year&-year_th, names_to = "fuel", values_to = "mea_vspp_gwh") #%>% 
  # ggplot(aes(x = year, y = mea_vspp_gwh, group = fuel, color = fuel)) +
  # geom_line(show.legend = FALSE) +
  # facet_wrap(~fuel, scales = "free_y")

pea_vspp_ene_ext <-
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
           sheet = "I_Actual",
           range = "DK7:DT31",
           col_names = c("solar","pvrftop", "wind", "biomass", "biogas", "msw", "hydro", "enecrop", "cogen", "tot_vspp_gwh"))%>%
  replace(is.na(.),0) %>% 
  mutate(year = 1997:2021,
         year_th = year + 543,
         slr_and_rftop = solar + pvrftop) %>% 
  select(year, year_th,slr_and_rftop,everything(), -solar,-pvrftop) %>%
  pivot_longer(-year&-year_th, names_to = "fuel", values_to = "pea_vspp_gwh") #%>%
  # ggplot(aes(x = year, y = pea_vspp_gwh, group = fuel, color = fuel)) +
  # geom_line(show.legend = FALSE) +
  # facet_wrap(~fuel, scales = "free_y")


pea_dede_ene <-
  
read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
           sheet = "I_Actual",
           range = "ET7:ET31",
           col_names = "pea_dede_gwh") %>% 
  mutate(year = 1997:2021,
         year_th = year +543)

pea_sfgen_ene <-
  
read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
           sheet = "I_Actual",
           range = "EZ7:EZ31",
           col_names = "pea_sfgen_gwh") %>% 
  mutate(year = 1997:2021,
         year_th = year +543)


#

tot_vspp_ext <-
cbind(mea_vspp_ene_ext %>% 
        filter(year >= 2015,
               fuel == "tot_vspp_gwh") %>% 
        select(year, year_th, mea_vspp_gwh), 
      pea_vspp_ene_ext %>% 
        filter(year >= 2015,
               fuel == "tot_vspp_gwh") %>% 
        select(pea_vspp_gwh),
      pea_dede_ene %>% 
        filter(year >= 2015) %>% 
        select(pea_dede_gwh),
      pea_sfgen_ene %>% 
        filter(year >= 2015) %>% 
        select(pea_sfgen_gwh)
        ) %>% 
  mutate(tot_pea_vspp_gwh = pea_vspp_gwh + pea_dede_gwh + pea_sfgen_gwh,
         tot_vspp_ext_gwh = mea_vspp_gwh + pea_vspp_gwh + pea_dede_gwh + pea_sfgen_gwh) %>% 
  pivot_longer(-year&-year_th,names_to = "vspp",values_to = "ene_vspp_ext_gwh") #%>% 
  # ggplot(aes(x = year, y = ene_vspp_ext_gwh, group = vspp, color = vspp))+
  # geom_line(show.legend = FALSE)+
  # facet_wrap(~vspp, scales = "free_y")


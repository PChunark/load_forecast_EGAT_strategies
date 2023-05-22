source("code/vspp_ext_cal.R")

# Extract budget load from MEA @budget load 66-67 ####
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
  mutate(year = as.numeric(year),
         year_th = as.numeric(year) + 543) %>% 
  filter(sector == "MEA's System Requirement")
  
# Extract PDP2022 load from MEA in NIDA _BAU_ND_EE70 ####
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
  mutate(year = as.numeric(year),
         year_th = as.numeric(year) + 543) %>% 
  filter(sector == "MEA's System Requirement")

# Combine budget load and PDP2022 load ####

newmea_ene <-
  
  rbind(b_mea_ene %>% filter(year >= 2019), 
        pdp2022_mea_ene %>% filter(year >=2029)) #%>% 
  # ggplot(aes(x = year, y = mea_gwh, group = sector))+
  # geom_line()
 
# Extract VSPP from MEA in PDP2018REV1 ####

mea_vspp_pdp2018r1 <-
  
  read_excel("raw_data/01 Load_PDP2018 ปรับ VSPP_13Jan2020_Final (PDP2018R1).xlsx",
           sheet = "I_VSPP",
           range = "B29:L51",
           col_names = c("year","solar", "wind", "hydro", "biomass", "biogas", "waste", "crop", "geothermal", "re_eeothers", "cogen")) %>%
  replace(is.na(.), 0 ) %>%
  mutate(total = rowSums(across(c(solar:cogen)))) %>% 
  pivot_longer(-year, names_to = "fuel", values_to = "mea_gwh") %>% 
  mutate(year_th = as.numeric(year) + 543) #%>% 
# ggplot(aes(x = year, y = mea_gwh, group = fuel, color = fuel))+
# geom_line(show.legend = FALSE)+
# facet_wrap(~fuel, scales = "free_y")

mea_newvspp_pdp2018r1 <-
  
  read_excel("raw_data/01 Load_PDP2018 ปรับ VSPP_13Jan2020_Final (PDP2018R1).xlsx",
             sheet = "I_VSPP",
             range = "M29:T51",
             col_names = c("biomass", "biogas", "solar_community", "waste", "wind", "ind_waste", "biomass_community", "ee")) %>%
  replace(is.na(.), 0 ) %>%
  mutate(year = 2015:2037,
         total = rowSums(across(c(biomass:ee)))) %>% 
  pivot_longer(-year, names_to = "fuel", values_to = "mea_gwh") %>% 
  mutate(year = as.numeric(year),
         year_th = as.numeric(year + 543)) #%>% 
# ggplot(aes(x = year, y = mea_gwh, group = fuel, color = fuel))+
# geom_line(show.legend = FALSE)+
# facet_wrap(~fuel, scales = "free_y")

# Select MEA vspp and new vspp from PDP2018REV1 ####
tot_mea_vspp_pdp2018r1 <- 
  
mea_vspp_pdp2018r1 %>% 
  select(year,fuel, mea_gwh) %>% 
  filter(fuel == "total")

tot_mea_newvspp_pdp2018r1 <-
  
mea_newvspp_pdp2018r1 %>% 
  select(year,fuel, mea_gwh) %>% 
  filter(fuel == "total")

tot_mea_vspp_pdp2018rev1 <-

tot_mea_vspp_pdp2018r1 %>%
  mutate(tot_mea_gwh = mea_gwh + tot_mea_newvspp_pdp2018r1$mea_gwh) %>% 
  select(!mea_gwh)

# Calculate EGAT sale from PDP2018REV1 ####

egt_sle_mea_pdp2018r1 <-

left_join(newmea_ene,
          tot_mea_vspp_pdp2018rev1%>% 
            select(year,tot_mea_gwh) %>% 
            filter(year >= 2019)) %>% 
  mutate(egatsale = mea_gwh - tot_mea_gwh)

# Extract VSPP in MEA region from PDP2023 case7

tot_mea_vspp_pdp2022c7 <-
  
  
read_excel("raw_data/Case7_VSPP+DEDE+NVSPP_เพิ่มภาค.xlsx",
           sheet = "MAC",
           range = "A28:Q49",
           col_names = c("fuel", 2022:2037)) %>%
  replace(is.na(.), 0 ) %>% 
  pivot_longer(-fuel, names_to = "year", values_to = "mea_gwh") %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_wider(names_from = fuel, values_from = mea_gwh) %>% 
  rename_all(tolower) %>% 
  pivot_longer(-year, names_to = "fuel", values_to = "tot_mea_gwh") %>% 
  filter(fuel == "total")


tot_mea_vspp_ene_pdp2022c7 <-
  
full_join(mea_vspp_ene_ext %>% 
  filter(year >= 2015,
         fuel == "tot_vspp_gwh") %>% 
  select(year, year_th, mea_vspp_gwh),
tot_mea_vspp_pdp2022c7%>% 
  select(year,tot_mea_gwh) %>% 
  filter(year >= 2019) ,
by= c("mea_vspp_gwh" = "tot_mea_gwh",
      "year" = "year")) %>% 
  select(!year_th) 

#
egt_sle_mea_pdp2022c7 <-
  
  left_join(newmea_ene,
            tot_mea_vspp_ene_pdp2022c7 %>% 
              select(year,mea_vspp_gwh) %>% 
              filter(year >= 2019)) %>% 
  mutate(egatsale = mea_gwh - mea_vspp_gwh)


### Calculating for worst case  
### Calculation for MEA electricity demand

newmea_ene_req <-
newload3u %>% 
  mutate(cumulative = lag(cumprod(grw_mea_dmd$growth),n = 0,default = 1)) %>% 
  mutate(mea_ene_req_gwh = cumulative * mea_ene_latest) %>% 
  select(year_th, year, mea_ene_req_gwh) %>% 
  mutate(tot_mea_vspp_pdp2018rev1 %>% 
           filter(year >= 2019)%>% 
           select(-year, -fuel)) %>% 
  select(year, year_th, mea_ene_req_gwh, mea_vspp_pdp2018r1 = tot_mea_gwh) %>% 
  mutate(egt_sle_mea_worst_case = mea_ene_req_gwh - mea_vspp_pdp2018r1)

source("code/vspp_ext_cal.R")
source("code/mea_ene_cal.R")

#Extract budget load from PEA @budget load 66-67

b_pea_ene <-
  
  read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final_Sent อศง.xlsx",
             sheet = "PEA",
             range = "A41:N56",
             col_names = c("category","a", 2017:2028)) %>%
  select(!a) %>% 
  pivot_longer(-category, names_to = "year", values_to = "pea_gwh") %>%
  filter(category == "ความต้องการพลังงานไฟฟ้าในระบบไฟฟ้า กฟภ.") %>% 
  mutate(year = as.numeric(year),
         year_th = year + 543)  

#Extract PDP2022 load from MEA in NIDA _BAU_ND_EE70
pdp2022_pea_ene <-
  
  read_excel("raw_data/93 EGAT_25Aug2022_13 Energy ภาพรวม NIDA_BAU_ND_EE70__Sent.xlsx",
             sheet = "PEA",
             range = "AA11:AW20",
             col_names = c("category","a", 2017:2037)) %>%
  select(!a) %>% 
  pivot_longer(-category, names_to = "year", values_to = "pea_gwh") %>%
  filter(category == "ความต้องการพลังงานไฟฟ้า") %>% 
  mutate(year = as.numeric(year),
         year_th = year + 543)  

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


#Extract VSPP from PEA in PDP2018REV1

 pea_vspp_pdp2018r1 <-
  
  read_excel("raw_data/01 Load_PDP2018 ปรับ VSPP_13Jan2020_Final (PDP2018R1).xlsx",
             sheet = "I_VSPP",
             range = "B105:L127",
             col_names = c("year","solar", "wind", "hydro", "biomass", "biogas", "waste", "crop", "geothermal", "re_eeothers", "cogen")) %>%
  replace(is.na(.), 0 ) %>%
  mutate(total = rowSums(across(c(solar:cogen)))) %>% 
  pivot_longer(-year, names_to = "fuel", values_to = "pea_gwh") %>% 
  mutate(year_th = year + 543) #%>% 
# ggplot(aes(x = year, y = pea_gwh, group = fuel, color = fuel))+
# geom_line(show.legend = FALSE)+
# facet_wrap(~fuel, scales = "free_y")

 pea_newvspp_pdp2018r1 <-
    
    read_excel("raw_data/01 Load_PDP2018 ปรับ VSPP_13Jan2020_Final (PDP2018R1).xlsx",
               sheet = "I_VSPP",
               range = "M105:T127",
               col_names = c("biomass", "biogas", "solar_community", "waste", "wind", "ind_waste", "biomass_community", "ee")) %>%
    replace(is.na(.), 0 ) %>%
    mutate(year = 2015:2037,
           total = rowSums(across(c(biomass:ee)))) %>% 
    pivot_longer(-year, names_to = "fuel", values_to = "pea_gwh") %>% 
    mutate(year = as.numeric(year),
           year_th = as.numeric(year + 543)) #%>% 
  # ggplot(aes(x = year, y = pea_gwh, group = fuel, color = fuel))+
  # geom_line(show.legend = FALSE)+
  # facet_wrap(~fuel, scales = "free_y")
 
 pea_dede_pdp2018r1 <-
   
 read_excel("raw_data/01 Load_PDP2018 ปรับ VSPP_13Jan2020_Final (PDP2018R1).xlsx",
            sheet = "I_VSPP",
            range = "V105:W127",
            col_names = c("hydro", "solar")) %>%
   replace(is.na(.), 0 ) %>%
   mutate(year = 2015:2037,
          total = rowSums(across(c(hydro:solar)))) %>% 
   pivot_longer(-year, names_to = "fuel", values_to = "pea_dede_gwh") %>% 
   mutate(year_th = year + 543) #%>% 
 # ggplot(aes(x = year, y = pea_dede_gwh, group = fuel, color = fuel))+
 # geom_line(show.legend = FALSE)+
 # facet_wrap(~fuel, scales = "free_y")

 pea_slfgen_pdp2018r1 <-
   
   read_excel("raw_data/01 Load_PDP2018 ปรับ VSPP_13Jan2020_Final (PDP2018R1).xlsx",
              sheet = "I_VSPP",
              range = "Y105:AB127",
              col_names = c("diesel", "hydro", "solar", "wind")) %>%
   replace(is.na(.), 0 ) %>%
   mutate(year = 2015:2037,
          total = rowSums(across(c(diesel:wind)))) %>% 
   pivot_longer(-year, names_to = "fuel", values_to = "pea_slfgen_gwh") %>% 
   mutate(year_th = year + 543) #%>% 
 # ggplot(aes(x = year, y = pea_slfgen_gwh, group = fuel, color = fuel))+
 # geom_line(show.legend = FALSE)+
 # facet_wrap(~fuel, scales = "free_y")
 
# Combine DEDE & PEA SELFGEN
 
pea_dede_slfgen_pdp2018r1 <-
  
 left_join(pea_dede_pdp2018r1 %>% filter(fuel == "total"),
           pea_slfgen_pdp2018r1 %>% filter(fuel == "total")) %>% 
   mutate(total = rowSums(across(c(pea_dede_gwh, pea_slfgen_gwh)))) %>% 
   select(!fuel&!year_th) %>% 
   pivot_longer(-year, names_to = "variables", values_to = "pea_dede_slfgen_gwh") #%>% 
# ggplot(aes(x = year, y = pea_dede_slfgen_gwh, group = variables, color = variables))+
# geom_line(show.legend = FALSE)+
# facet_wrap(~variables, scales = "free_y")
    
# Select PEA vspp, new vspp, DEDE, PEA selfgen from PDP2018REV1
#Select total PEA vspp
tot_pea_vspp_pdp2018r1 <- 
  
  pea_vspp_pdp2018r1 %>% 
  select(year,fuel, pea_gwh) %>% 
  filter(fuel == "total")

# Select total new vspp
tot_pea_newvspp_pdp2018r1 <-
  
  pea_newvspp_pdp2018r1 %>% 
  select(year,fuel, pea_gwh) %>% 
  filter(fuel == "total")

# Select total DEDE and PEA selfgen
tot_pea_dedesefgen_pdp2018r1 <-
  
  pea_dede_slfgen_pdp2018r1 %>% 
  filter(variables == "total")

# Combine total vspp
tot_pea_vspp_pdp2018rev1 <-
  
  tot_pea_vspp_pdp2018r1 %>% 
  mutate(tot_pea_gwh = pea_gwh + 
                       tot_pea_newvspp_pdp2018r1$pea_gwh +
                       tot_pea_dedesefgen_pdp2018r1$pea_dede_slfgen_gwh) %>% 
  select(!pea_gwh)    

#Calculate EGAT sale from PDP2018REV1

egt_sle_pea_pdp2018r1 <-

  left_join(newpea_ene,
            tot_pea_vspp_pdp2018rev1%>% 
              select(year,tot_pea_gwh) %>% 
              filter(year >= 2019)) %>% 
  mutate(egatsale = pea_gwh - tot_pea_gwh)

# Calculate share of PEA and MEA using vspp from PDP2018 rev1

shr_mea_pea_pdp2018r1 <-
  
egt_sle_mea_pdp2018r1 %>% 
  select(year, egatsale) %>% 
mutate(egatsle_to_pea = egt_sle_pea_pdp2018r1$egatsale,
       shr_egatsle_mea = egatsale/rowSums(across(c(egatsale, egatsle_to_pea))),
       shr_egatsle_pea = egatsle_to_pea/rowSums(across(c(egatsale, egatsle_to_pea)))
       )


# Extract vspp energy from NAC region in PDP2023case7

nac_vspp_pdp2022c7 <-
  
read_excel("raw_data/Case7_VSPP+DEDE+NVSPP_เพิ่มภาค.xlsx",
           sheet = "NAC",
           range = "A28:Q49",
           col_names = c("fuel", 2022:2037)) %>%
  replace(is.na(.), 0 ) %>% 
  pivot_longer(-fuel, names_to = "year", values_to = "nac_pea_gwh") %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_wider(names_from = fuel, values_from = nac_pea_gwh) %>% 
  rename_all(tolower) %>% 
  pivot_longer(-year, names_to = "fuel", values_to = "tot_nac_gwh") %>% 
  filter(fuel == "total")

# Extract vspp energy from NEC region in PDP2023case7

nec_vspp_pdp2022c7 <-

read_excel("raw_data/Case7_VSPP+DEDE+NVSPP_เพิ่มภาค.xlsx",
           sheet = "NEC",
           range = "A28:Q49",
           col_names = c("fuel", 2022:2037)) %>%
  replace(is.na(.), 0 ) %>% 
  pivot_longer(-fuel, names_to = "year", values_to = "nec_pea_gwh") %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_wider(names_from = fuel, values_from = nec_pea_gwh) %>% 
  rename_all(tolower) %>% 
  pivot_longer(-year, names_to = "fuel", values_to = "tot_nec_gwh") %>% 
  filter(fuel == "total")


# Extract vspp energy from CAC region in PDP2023case7

cac_vspp_pdp2022c7 <-
  
read_excel("raw_data/Case7_VSPP+DEDE+NVSPP_เพิ่มภาค.xlsx",
           sheet = "CAC",
           range = "A28:Q49",
           col_names = c("fuel", 2022:2037)) %>%
  replace(is.na(.), 0 ) %>% 
  pivot_longer(-fuel, names_to = "year", values_to = "cac_pea_gwh") %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_wider(names_from = fuel, values_from = cac_pea_gwh) %>% 
  rename_all(tolower) %>% 
  pivot_longer(-year, names_to = "fuel", values_to = "tot_cac_gwh") %>% 
  filter(fuel == "total")


# Extract vspp energy from SAC region in PDP2023case7

sac_vspp_pdp2022c7<-
  
read_excel("raw_data/Case7_VSPP+DEDE+NVSPP_เพิ่มภาค.xlsx",
           sheet = "SAC",
           range = "A28:Q49",
           col_names = c("fuel", 2022:2037)) %>%
  replace(is.na(.), 0 ) %>% 
  pivot_longer(-fuel, names_to = "year", values_to = "sac_pea_gwh") %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_wider(names_from = fuel, values_from = sac_pea_gwh) %>% 
  rename_all(tolower) %>% 
  pivot_longer(-year, names_to = "fuel", values_to = "tot_sac_gwh") %>% 
  filter(fuel == "total")

# Combine VSPP from PEA 
tot_pea_vspp_ene_pdp2022c7 <-
  
nac_vspp_pdp2022c7 %>%
  select(!fuel) %>% 
  mutate(nec_vspp_pdp2022c7 %>% 
           select(tot_nec_gwh)) %>% 
  mutate(cac_vspp_pdp2022c7 %>% 
           select(tot_cac_gwh)) %>% 
  mutate(sac_vspp_pdp2022c7 %>% 
           select(tot_sac_gwh)) %>% 
  mutate(tot_gwh = rowSums(across(c(tot_nac_gwh:tot_sac_gwh)))) %>% 
  select(year, tot_gwh)

# Combine pea vspp existing and vspp from PDP2022 Case 7

tot_pea_vspp_pdp2022c7 <- 
  
full_join(tot_vspp_ext %>% 
         filter(year >= 2019,
                vspp == "tot_pea_vspp_gwh") %>% 
         select(year, ene_vspp_ext_gwh),
         tot_pea_vspp_ene_pdp2022c7,
         by = c("ene_vspp_ext_gwh" = "tot_gwh",
                "year" = "year")
         )


# Calculate EGAT sale to PEA in PDP2022c7

egt_sle_pea_pdp2022c7 <-
  
  left_join(newpea_ene,
            tot_pea_vspp_pdp2022c7) %>% 
  mutate(egatsale = pea_gwh - ene_vspp_ext_gwh) 


  # Calculate share of PEA and MEA using vspp from PDP2018 rev1
  
shr_mea_pea_pdp2022c7 <-
  
  egt_sle_mea_pdp2022c7 %>% 
  select(year, egatsale) %>% 
  mutate(egatsle_to_pea = egt_sle_pea_pdp2022c7$egatsale,
         shr_egatsle_mea = egatsale/rowSums(across(c(egatsale, egatsle_to_pea))),
         shr_egatsle_pea = egatsle_to_pea/rowSums(across(c(egatsale, egatsle_to_pea)))
  )
  
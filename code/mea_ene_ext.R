
mea_ene_ext <- 
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
  filter(sector == "MEA's System Requirement" &
         year %in% c(2017:2022))

f <- read_csv("./data/organized_data_to_append/JUST_JOHNSON.csv")



e_1_2_3 <- f %>% select(num_months_sampled, ch4_ebu) %>% 
  filter(num_months_sampled %in% c(1,2,3)) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_ebu),
         months_sampled = "A: 1-3 months") %>%
  select(-sd_ebu)


e_4_5_6 <- f %>% select(num_months_sampled, ch4_ebu) %>% 
  filter(num_months_sampled %in% c(4,5,6)) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_ebu),
         months_sampled = "B: 4-6 months")%>%
  select(-sd_ebu)

e_7_8_9 <- f %>% select(num_months_sampled, ch4_ebu) %>% 
  filter(num_months_sampled %in% c(7,8,9)) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_ebu),
         months_sampled = "C: 7-9 months")%>%
  select(-sd_ebu)

e_10_11_12 <- f %>% select(num_months_sampled, ch4_ebu) %>% 
  filter(num_months_sampled %in% c(10,11,12)) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_ebu),
         months_sampled = "D: 10-12 months")%>%
  select(-sd_ebu)

e_12_and_up <- f %>% select(num_months_sampled, ch4_ebu) %>% 
  filter(num_months_sampled > 12) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_ebu),
         months_sampled = "E: 12+ months")%>%
  select(-sd_ebu)

ebu_month_error <- rbind(e_1_2_3,e_4_5_6,e_7_8_9,e_10_11_12,e_12_and_up)%>%
  mutate(error = "Ebu_month_error")


#Diffusion


d_1_2_3 <- f %>% select(num_months_sampled, ch4_diff) %>% 
  filter(num_months_sampled %in% c(1,2,3)) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_diff),
         months_sampled = "A: 1-3 months")%>%
  select(-sd_diff)


d_4_5_6 <- f %>% select(num_months_sampled, ch4_diff) %>% 
  filter(num_months_sampled %in% c(4,5,6)) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_diff),
         months_sampled = "B: 4-6 months")%>%
  select(-sd_diff)

d_7_8_9 <- f %>% select(num_months_sampled, ch4_diff) %>% 
  filter(num_months_sampled %in% c(7,8,9)) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_diff),
         months_sampled = "C: 7-9 months")%>%
  select(-sd_diff)

d_10_11_12 <- f %>% select(num_months_sampled, ch4_diff) %>% 
  filter(num_months_sampled %in% c(10,11,12)) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_diff),
         months_sampled = "D: 10-12 months")%>%
  select(-sd_diff)

d_12_and_up <- f %>% select(num_months_sampled, ch4_diff) %>% 
  filter(num_months_sampled > 12) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd = exp(sd_diff),
         months_sampled = "E: 12+ months")%>%
  select(-sd_diff)

diff_month_error <- rbind(d_1_2_3,d_4_5_6,d_7_8_9,d_10_11_12,d_12_and_up) %>%
  mutate(error = "Diff_month_error")

monthly_error <- rbind(ebu_month_error,diff_month_error)



#### # of sites


e_sites_1_5 <- f %>% select(num_sites_sampled, ch4_ebu) %>% 
  filter(num_sites_sampled %in% c(1,2,3,4,5)) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd_ebu = exp(sd_ebu))


e_sites_6_10 <- f %>% select(num_sites_sampled, ch4_ebu) %>% 
  filter(num_sites_sampled %in% c(6,7,8,9,10)) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd_ebu = exp(sd_ebu))

e_sites_11_15 <- f %>% select(num_sites_sampled, ch4_ebu) %>% 
  filter(num_sites_sampled %in% c(11,12,13,14,15)) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd_ebu = exp(sd_ebu))

e_sites_16_20 <- f %>% select(num_sites_sampled, ch4_ebu) %>% 
  filter(num_sites_sampled %in% c(16,7,18,19,20)) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd_ebu = exp(sd_ebu))

e_sites_20 <- f %>% select(num_sites_sampled, ch4_ebu) %>% 
  filter(num_sites_sampled > 20) %>%
  na.omit(.) %>%
  mutate(l_ch4_ebu = log(ch4_ebu)) %>%
  filter(!is.infinite(l_ch4_ebu)) %>%
  summarise(sd_ebu = sd(l_ch4_ebu, na.rm = T)) %>%
  mutate(geo_sd_ebu = exp(sd_ebu))


#Diffusion


d_sites_1_5 <- f %>% select(num_sites_sampled, ch4_diff) %>% 
  filter(num_sites_sampled %in% c(1,2,3,4,5)) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd_diff = exp(sd_diff))


d_sites_6_10 <- f %>% select(num_sites_sampled, ch4_diff) %>% 
  filter(num_sites_sampled %in% c(6,7,8,9,10)) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd_diff = exp(sd_diff))

d_sites_11_15 <- f %>% select(num_sites_sampled, ch4_diff) %>% 
  filter(num_sites_sampled %in% c(11,12,13,14,15)) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd_diff = exp(sd_diff))

d_sites_16_20 <- f %>% select(num_sites_sampled, ch4_diff) %>% 
  filter(num_sites_sampled %in% c(16,17,18,19,20)) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd_diff = exp(sd_diff))

d_sites_20 <- f %>% select(num_sites_sampled, ch4_diff) %>% 
  filter(num_sites_sampled > 20) %>%
  na.omit(.) %>%
  mutate(l_ch4_diff = log(ch4_diff)) %>%
  filter(!is.infinite(l_ch4_diff)) %>%
  summarise(sd_diff = sd(l_ch4_diff, na.rm = T)) %>%
  mutate(geo_sd_diff = exp(sd_diff))



# read in data set that has been linked to GLCP Hydrobasin climate data

## --> quantify the number of total samples from each site by multiplying the 
## --> number of months sampled by the number of sites collecting CH4 emissions

## --> calculate a waterbody_id column for each indvidual lake
## --> this will convey observation uncertainty in a bayes framework

base <- read_csv("./data/organized_data_to_append/GLEE_data_with_GLCP_link.csv")%>%
  mutate(tot_sampling_events = num_months_sampled * num_sites_sampled) %>%
  group_by(lat, lon) %>%
  mutate(waterbody_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(temp_for_model_K = ifelse(is.na(mean_temp_k), effective_obs_wtemp_k, mean_temp_k))

# Calcualte the standard error based on the sample number for ebullition and diffusion seperately

f <- base %>%
  select(tot_sampling_events, ch4_ebu, ch4_diff)

ebu_error <- list()

sample_numbers_ebu <- f %>% select(tot_sampling_events, ch4_ebu) %>% na.omit(.)
sample_numbers_ebu <-c(unique(sample_numbers_ebu$tot_sampling_events))

  
for(s in 1:length(sample_numbers_ebu)){

  error <- f %>% select(tot_sampling_events, ch4_ebu) %>% 
    filter(tot_sampling_events == sample_numbers_ebu[s]) %>%
    na.omit(.) %>%
    mutate(logged = log(ch4_ebu)) %>%
    filter(!is.infinite(logged)) %>%
    summarise(sd = sqrt(sum((logged - mean(logged))^2)/sample_numbers_ebu[s])) %>%
    mutate(total_samples = sample_numbers_ebu[s],
           flux_type = "ebullition")
  
  ebu_error[[s]] <- error
  
}

ebu_error = do.call(rbind, ebu_error)

ebu_error2 <- ebu_error %>%
  na.omit(.) %>%
  mutate(total_samples = as.numeric(total_samples)) %>% 
  arrange(total_samples) %>%
  mutate(sample_size = ifelse(total_samples <= 10,"a: 1-10", NA),
         sample_size = ifelse(total_samples > 10,"b: 11-20", sample_size),
         sample_size = ifelse(total_samples > 20,"c: 21-30", sample_size),
         sample_size = ifelse(total_samples > 30,"d: 31-40", sample_size),
         sample_size = ifelse(total_samples > 40,"e: 41-50", sample_size),
         sample_size = ifelse(total_samples > 50,"f: 51-60", sample_size),
         sample_size = ifelse(total_samples > 60,"g: 61-100", sample_size),
         sample_size = ifelse(total_samples > 100,"h: 100+", sample_size)) %>%
  group_by(sample_size, flux_type) %>%
  summarize(sd = (mean(sd)))


diff_error <- list()

sample_numbers_diff <- f %>% select(tot_sampling_events, ch4_diff) %>% na.omit(.)
sample_numbers_diff <-c(unique(sample_numbers_diff$tot_sampling_events))

for(s in 1:length(sample_numbers_diff)){
  
  error <- f %>% select(tot_sampling_events, ch4_diff) %>% 
    filter(tot_sampling_events == sample_numbers_diff[s]) %>%
    na.omit(.) %>%
    mutate(logged = log(ch4_diff)) %>%
    filter(!is.infinite(logged)) %>%
    summarise(sd = sqrt(sum((logged - mean(logged))^2)/sample_numbers_diff[s])) %>%
    mutate(total_samples = sample_numbers_diff[s],
           flux_type = "diffusion")
  
  diff_error[[s]] <- error
  
}

diff_error = do.call(rbind, diff_error)

diff_error2 <- diff_error %>%
  na.omit(.) %>%
  mutate(total_samples = as.numeric(total_samples)) %>% 
  arrange(total_samples) %>%
  mutate(sample_size = ifelse(total_samples <= 10,"a: 1-10", NA),
         sample_size = ifelse(total_samples > 10,"b: 11-20", sample_size),
         sample_size = ifelse(total_samples > 20,"c: 21-30", sample_size),
         sample_size = ifelse(total_samples > 30,"d: 31-40", sample_size),
         sample_size = ifelse(total_samples > 40,"e: 41-50", sample_size),
         sample_size = ifelse(total_samples > 50,"f: 51-60", sample_size),
         sample_size = ifelse(total_samples > 60,"g: 61-100", sample_size),
         sample_size = ifelse(total_samples > 100,"h: 100+", sample_size)) %>%
  group_by(sample_size, flux_type) %>%
  summarize(sd = (mean(sd)))

error_all <- rbind(ebu_error2, diff_error2)

ggplot(error_all, aes(sample_size, exp(sd)))+
  geom_bar(stat = "identity")+
  facet_wrap(~flux_type, scales = "free")


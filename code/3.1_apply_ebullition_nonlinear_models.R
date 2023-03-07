library(minpack.lm)

### TEMPERATURE MODELS ###

### MONTHLY TIMESTEP ###
ebu_base_temp <- base %>% select(ch4_ebu, temp_for_model_K, waterbody_id, month) %>%
  na.omit(.)%>%
  mutate(temp_for_model_C = temp_for_model_K-273.15)

# First-order Arhennius
monthly_ebu_FOA = nlsLM(ch4_ebu ~ A * exp(a * temp_for_model_C),
                        start = list(A = 0.12, a = 0.035),
                        data = ebu_base_temp,
                        control = nls.lm.control(maxiter=1000))
summary(monthly_ebu_FOA) # get MSE value

# Modified First-order Arhennius (from Aben et al 2018 & Johnson et al 2022)
monthly_ebu_MFOA = nlsLM(ch4_ebu ~ A * a^(temp_for_model_C-20),
                         start = list(A = 100, a = 1.1),
                         data = ebu_base_temp,
                         control = nls.lm.control(maxiter=1000))
summary(monthly_ebu_MFOA) # get MSE value

# Modified First-order Arhennius (from Aben et al 2018 & Johnson et al 2022)
monthly_ebu_MFOA = nlsLM(ch4_ebu ~ A * a^(temp_for_model_C-20),
                         start = list(A = 100, a = 1.1),
                         data = ebu_base_temp,
                         control = nls.lm.control(maxiter=1000))
summary(monthly_ebu_MFOA) # get MSE value

monthly_FOA_function <- function(x) {4.14072 * exp(0.15768*x)}
monthly_MFOA_function <- function(x) {96.98125 * 1.17077 ^ (x-20)}
aben_MFOA_function <- function(x) {100 * 1.1 ^ (x-20)}

ebu_function_output <- data.frame(x = -15:35,            # Create data for ggplot2
                                  values = c(
                                    monthly_FOA_function(-15:35),
                                    monthly_MFOA_function(-15:35),
                                    johnson_MFOA_function(-15:35)),
                                  model = rep(c(
                                    "First-Order (F-O) Arhennius",
                                    "Modified F-O Arhennius",
                                    "Aben's Modified F-O Arhennius"), each = 51))

ggplot(ebu_base_temp, aes(x = temp_for_model_C, y = ch4_ebu))+
  geom_point()+
  geom_line(data = ebu_function_output, aes(x, values, group = model, color = model), lwd = 2)+
  scale_color_viridis(discrete = T, option = "C")+
  coord_cartesian(ylim=c(0, 3000))+
  theme_classic()



### YEARLY TIMESTEP ###

ebu_base_yearly <- base %>% select(year, ch4_ebu, waterbody_id, temp_for_model_K) %>%
  na.omit(.)%>%
  mutate(temp_for_model_C = temp_for_model_K-273.15) %>%
  group_by(waterbody_id, year)%>%
  summarize_all(funs(mean), na.rm = T)

# First-order Arhennius
yearly_ebu_FOA = nlsLM(ch4_ebu ~ A * exp(a * temp_for_model_C),
                        start = list(A = 0.12, a = 0.035),
                        data = ebu_base_yearly,
                        control = nls.lm.control(maxiter=1000))
summary(yearly_ebu_FOA) # get MSE value

# Modified First-order Arhennius (from Aben et al 2018 & Johnson et al 2022)
yearly_ebu_MFOA = nlsLM(ch4_ebu ~ A * a^(temp_for_model_C-20),
                         start = list(A = 100, a = 1.1),
                         data = ebu_base_yearly,
                         control = nls.lm.control(maxiter=1000))
summary(yearly_ebu_MFOA) # get MSE value

# Modified First-order Arhennius (from Aben et al 2018 & Johnson et al 2022)
yearly_ebu_MFOA = nlsLM(ch4_ebu ~ A * a^(temp_for_model_C-20),
                         start = list(A = 100, a = 1.1),
                         data = ebu_base_yearly,
                         control = nls.lm.control(maxiter=1000))
summary(yearly_ebu_MFOA) # get MSE value

yearly_FOA_function <- function(x) {13.71050 * exp(0.08813*x)}
yearly_MFOA_function <- function(x) {79.89646 * 1.09213 ^ (x-20)}
aben_MFOA_function <- function(x) {100 * 1.1 ^ (x-20)}

ebu_function_output <- data.frame(x = -15:35,            # Create data for ggplot2
                                  values = c(
                                    yearly_FOA_function(-15:35),
                                    yearly_MFOA_function(-15:35),
                                    aben_MFOA_function(-15:35)),
                                  model = rep(c(
                                    "First-Order (F-O) Arhennius",
                                    "Modified F-O Arhennius",
                                    "Aben's Modified F-O Arhennius"), each = 51))

ggplot(ebu_base_yearly, aes(x = temp_for_model_C, y = ch4_ebu))+
  geom_point()+
  geom_line(data = ebu_function_output, aes(x, values, group = model, color = model), lwd = 2)+
  scale_color_viridis(discrete = T, option = "C")+
  coord_cartesian(ylim=c(0, 3000))+
  theme_classic()

### SITE MEAN ###

ebu_base_site <- base %>% select(ch4_ebu, waterbody_id, temp_for_model_K) %>%
  na.omit(.)%>%
  mutate(temp_for_model_C = temp_for_model_K-273.15) %>%
  group_by(waterbody_id)%>%
  summarize_all(funs(mean), na.rm = T)

# First-order Arhennius
site_ebu_FOA = nlsLM(ch4_ebu ~ A * exp(a * temp_for_model_C),
                       start = list(A = 0.12, a = 0.035),
                       data = ebu_base_site,
                       control = nls.lm.control(maxiter=1000))
summary(site_ebu_FOA) # get MSE value

# Modified First-order Arhennius (from Aben et al 2018 & Johnson et al 2022)
site_ebu_MFOA = nlsLM(ch4_ebu ~ A * a^(temp_for_model_C-20),
                        start = list(A = 100, a = 1.1),
                        data = ebu_base_site,
                        control = nls.lm.control(maxiter=1000))
summary(site_ebu_MFOA) # get MSE value

site_FOA_function <- function(x) {19.18062 * exp(0.06981*x)}
site_MFOA_function <- function(x) {77.48887 * 1.07230 ^ (x-20)}
aben_MFOA_function <- function(x) {100 * 1.1 ^ (x-20)}

ebu_function_output <- data.frame(x = -15:35,            # Create data for ggplot2
                                  values = c(
                                    site_FOA_function(-15:35),
                                    site_MFOA_function(-15:35),
                                    aben_MFOA_function(-15:35)),
                                  model = rep(c(
                                    "First-Order (F-O) Arhennius",
                                    "Modified F-O Arhennius",
                                    "Aben's Modified F-O Arhennius"), each = 51))

ggplot(ebu_base_site, aes(x = temp_for_model_C, y = ch4_ebu))+
  geom_point()+
  geom_line(data = ebu_function_output, aes(x, values, group = model, color = model), lwd = 2)+
  scale_color_viridis(discrete = T, option = "C")+
  coord_cartesian(ylim=c(0, 3000))+
  theme_classic()


### LAKE AREA MODELS ###

### LAKE AREA ALL DATA ###

ebu_base_area <- base %>% select(ch4_ebu, waterbody_id, month, surf_area_k) %>%
  na.omit(.) %>%
  mutate(ch4_ebu_scaled = scale(ch4_ebu),
         surf_area_k_scaled = scale(surf_area_k))

area_monthly_model <- lm(ch4_ebu_scaled ~ surf_area_k_scaled, data = ebu_base_area)
summary(area_monthly_model)




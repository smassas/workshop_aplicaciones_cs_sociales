library(dplyr)
data <- datasets::airquality


# Select ------------------------------------------------------------------

data %>% select(c(3,4))

# Mutate ------------------------------------------------------------------

data <- data %>% 
  mutate(Month = ifelse(Month == "5", "Abril", 
                        ifelse(Month == "6", "Mayo",
                               ifelse(Month == "7", "Junio",
                                      ifelse(Month == "8", "Agosto",
                                             ifelse(Month == "9", "Septiembre", "Otro")))))) 

data %>% 
  mutate(Month = case_when(Month == 5 ~ "Abril",
                           TRUE ~ ""))

data %>% 
  mutate(Month = gsub(pattern = "5", replacement = "Abril", x = Month),
         Month = gsub(pattern = "6", replacement = "Mayo", x = Month))
          

# Filter ------------------------------------------------------------------

data %>% filter(Ozone >= 50 & Temp >= 80)
filter(data, Solar.R %in% c("175", "190", "194"))
filter(data, Solar.R == "175" | Solar.R == "190" | Solar.R == "194")

# Group_by ----------------------------------------------------------------

data %>%
  group_by(Month) %>% 
  mutate(rank = min_rank(x = Temp)) %>% 
  arrange(Temp) %>% 
  summarize(mean = mean(Temp),
            sd = sd(Temp)) %>% 
  mutate(Analisis = ifelse(mean > 80.0, "Temperatura alta", "Temperatura usual"))


# Slice -------------------------------------------------------------------

data %>% 
  slice(2:4)

# Count -------------------------------------------------------------------

data %>% 
  count(Temp)



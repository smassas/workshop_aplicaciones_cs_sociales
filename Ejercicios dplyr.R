library(dplyr)
library(datasets)

# Select ------------------------------------------------------------------

# 1. Almacenar la base `airquality` en un vector llamado `data`

data <- datasets::airquality

# 2. Seleccionar variable `Ozone`, `Solar.R`, `Wind` y `Temp` y almacenar en vector X.

data %>% select(Ozone, Solar.R, Wind, Temp)

# 3. Seleccionar la columna 4 y 5 del dataframe.

data %>% select(c(4,5))

# Filter ------------------------------------------------------------------

# 1. De la base `airquality` filtrar la variable `Temp`, tal que siempre sea >= a 68.

data %>% 
  select(Temp) %>% 
  filter(Temp >= 68)

# 2. Dos condiciones. Filtrar cuando `Ozone` >= 50 y `Temp` >= 88.

data %>% 
  filter(Ozone >= 50 & Temp >= 88)

# 3. Filtrar cuando `Month` sea 7 u 8, y `Day` 10 o 11.

data %>% 
  filter(Month %in% c("7", "8") & Day %in% c("10", "11"))

# 4. Filtrar cuando `Solar.R` sea 175, 190 o 194.

data %>% 
  filter(Solar.R %in% c("175", "190", "194"))

filter(data, Solar.R == "175" | Solar.R == "190" | Solar.R == "194")


# Mutate ------------------------------------------------------------------

# 1. Recodificar etiquetas de la variable `Month`, tal que 5 = mayo, 6 = junio, 7 = julio, 8 = agosto y 9 = septiembre.

data %>% 
  mutate(Month = ifelse(Month == "5", "Abril",
                        ifelse(Month == "6", "Mayo",
                               ifelse(Month == "7", "Junio",
                                      ifelse(Month == "8", "Agosto",
                                             ifelse(Month == "9", "Septiembre", "Ninguno"))))))


data %>% 
  mutate(Month = case_when(Month == 5 ~ "Abril",
                           TRUE ~ ""))


# 2. Crear nueva variable que evalúe el estado de la temperatura. Si es > 80.0, es temperatura atípica o si es < 80.0, temperatura usual.

data %>% 
  mutate(Analisis = ifelse(Temp > 80.0, "outlier", "normal"))

# Group_by ----------------------------------------------------------------

# 1. De la base `airquality` agrupar por `Month` y almacenar en un vector llamado `agrupamiento`.

agrupamiento <- data %>% 
                  group_by(Month)

# 2. Calcular el promedio, mínimo, máximo y desviación estándar por cada mes.

agrupamiento %>% 
  summarize(Promedio = mean(Temp),
            Mínimo = min(Temp),
            Máximo = max(Temp),
            SD = sd(Temp))

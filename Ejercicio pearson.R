Llamados_ventas <- c(96, 40, 104, 128, 164, 76, 72, 80, 36, 180, 44, 36)
planes_contratados <- c(41, 41, 51, 53, 60, 61, 50, 28, 48, 70, 33, 30)

data <- data.frame(Llamados_ventas, planes_contratados)

library(dplyr)

plot(data$Llamados_ventas ~ data$planes_contratados)

data <- data %>% 
  mutate(diff_xbarra = Llamados_ventas - mean(Llamados_ventas),
         diff_ybarra = planes_contratados - round(mean(planes_contratados), 0),
         prod_desv = diff_xbarra * diff_ybarra) %>% 
  summarise(sum(prod_desv)) 
  
cor1_manual <- data/((12-1)*sd(Llamados_ventas)*sd(planes_contratados))
         
cor_formula <- cor(planes_contratados, Llamados_ventas)

cbind(cor1_manual, cor_formula)


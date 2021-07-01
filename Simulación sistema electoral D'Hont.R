Simulacion_electoral <- function (votos, listas, n_escanos) {
  divisor.mat <- sum(votos)/sapply(votos, "/", seq(1, n_escanos, 
                                                   1))
  colnames(divisor.mat) <- listas
  m.mat <- tidyr::gather(as.data.frame(divisor.mat), key = "nombre", 
                         value = "valor", everything())
  m.mat <- m.mat[rank(m.mat$valor, ties.method = "random") <= 
                   n_escanos, ]
  rle.escanos <- rle(as.character(m.mat$nombre))
  if (sum(rle.escanos$length) != n_escanos) 
    stop(paste("Número de asientos distribuidos no es igual a", 
               n_escanos))
  rle.escanos$length
}

Simulacion_electoral(votos = c(340000, 280000,160000), 
                     listas = c("Lista A", "Lista B", "Lista C"),
                     n_escanos = 7)
            

library(htmlwidgets)
library(DT)
library(dplyr)

data <- readxl::read_xlsx(path = "dhont.xlsx")
data <- data.frame(data[-c(1)], row.names = data$...1)
data <- data %>% select(1:3) 

DT::datatable(data, caption = 'Tabla 1: Método de conversión voto/escaño', 
              options = list(searching = T, paging = F, info = F, initComplete = JS(
  "function(settings, json) {",
  "$('body').css({'font-family': 'Gill Sans'});",
  "}"))) %>% 
           DT::saveWidget(datable, 'datable.html')



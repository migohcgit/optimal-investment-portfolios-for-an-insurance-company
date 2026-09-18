library(dplyr)
library(readr)

#preapara los dataframes para trabajar
walmex <- read_csv("walmex.csv", col_select = c(Date, Price))
gfnorte <- read_csv("gfnorte.csv", col_select = "Price")

walmex <- walmex %>%
  rename(precio_walmex = Price)

gfnorte <- gfnorte %>%
  rename(precio_gfnorte = Price)

rendimientos <- bind_cols(walmex, gfnorte)

rendimientos <- rendimientos %>%
  mutate(
    rendimiento_walmex = (precio_walmex / lead(precio_walmex))-1,
    rendimiento_gfnorte = (precio_gfnorte / lead(precio_gfnorte))-1
    )

#prepara los valores de medidas de tendencia central y dispersión
esperado_walmex <- mean(rendimientos$rendimiento_walmex, na.rm = TRUE)
varianza_walmex <- var(rendimientos$rendimiento_walmex, na.rm = TRUE)
esperado_gfnorte <- mean(rendimientos$rendimiento_gfnorte, na.rm = TRUE)
varianza_gfnorte <- var(rendimientos$rendimiento_gfnorte, na.rm = TRUE)
desviacion_walmex <- sd(rendimientos$rendimiento_walmex, na.rm = TRUE)
desviacion_gfnorte <- sd(rendimientos$rendimiento_gfnorte, na.rm = TRUE)
correlacion <- cov(rendimientos$rendimiento_walmex, rendimientos$rendimiento_gfnorte, use = "complete.obs")
tasa_libre_de_riesgo = 0.0628 #anual, CETES a 28 días

#crea el dataframe de los portafolios
portafolios <- data.frame(
  "no. portafolio" = 1:21,
  "w1 walmex"      = paste0(seq(0, 100, by = 5), "%"),
  "w2 gfnorte"     = paste0(seq(100, 0, by = -5), "%"),
  check.names = FALSE
)

#crea valores númericos para trabajar con ellos apartir de los porcentajes
w1_num <- as.numeric(gsub("%", "", portafolios$'w1 walmex')) / 100
w2_num <- as.numeric(gsub("%", "", portafolios$'w2 gfnorte')) / 100

#opera las filas para conocer las medidas de cada portafolio
portafolios$rend_esperado <- (w1_num * esperado_walmex) + (w2_num * esperado_gfnorte)
portafolios$varianza <- (w1_num^2 * varianza_walmex) + (w2_num^2 * varianza_gfnorte) + (2 * w1_num * w2_num * correlacion)
portafolios$desv <- portafolios$varianza^(1/2)
portafolios$sharp_ratio <- (portafolios$rend_esperado - tasa_libre_de_riesgo/12) / portafolios$desv

#crea un archivo csv del dataframe portafolios
write.csv(portafolios, file = "portafolios.csv", row.names = FALSE)







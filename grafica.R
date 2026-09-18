library(dplyr)
library(readr)
library(ggplot2)

portafolios_grafica <- read_csv("portafolios.csv")

ggplot(portafolios, aes(x = desv, y = rend_esperado)) +
  geom_point(color = "#008080", size = 3) +
  labs(
    title = "Diagrama de Dispersión",
    x = "Riesgo del portafolio",
    y = "Rendimiento del portafolio"
  ) +
  theme_minimal()


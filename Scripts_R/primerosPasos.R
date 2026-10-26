# ==============================================================================
# PRIMERAS PASOS EN R: ANÁLISIS DE AIRBNB CON TIDYVERSE
# ==============================================================================

# 1. Instalar y cargar la librería estrella (solo se instala la primera vez)
# install.packages("tidyverse")
library(tidyverse)

# 2. Cargar el dataset (Equivalente a pd.read_csv)
# read_csv de tidyverse es más rápido y genera un 'tibble' de forma automática
df <- read_csv("listings_unificado.csv")

# Ver las primeras filas y estructura
head(df)
glimpse(df) # Es el equivalente al df.info() de Pandas

# 3. Filtrado y Limpieza Básica (Equivalente a máscaras de Pandas)
# Vamos a quedarnos con alojamientos con precios mayores a 0 y menores a 500
df_filtrado <- df %>% 
  filter(price > 0 & price < 500)

# 4. Agrupación y Métricas (Equivalente a .groupby().agg())
# Queremos saber el precio promedio y total de alojamientos por Ciudad
resumen_ciudad <- df_filtrado %>% 
  group_by(ciudad) %>% 
  summarise(
    total_alojamientos = n(),
    precio_medio = mean(price, na.rm = TRUE)
  )

print(resumen_ciudad)

# 5. El primer gráfico con ggplot2 
# Replicamos el gráfico de barras del precio medio por ciudad
ggplot(data = resumen_ciudad, aes(x = reorder(ciudad, -precio_medio), y = precio_medio, fill = ciudad)) +
  geom_col() +
  labs(
    title = "Precio Promedio por Ciudad en R",
    x = "Ciudad",
    y = "Precio Medio ($)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Quitar leyenda repetitiva

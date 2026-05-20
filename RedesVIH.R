
# Tablas de nuevos casos #

casos_2024 <- data.frame(
  Sexo = c("Hombres", "Mujeres"),
  Casos = c(2438, 386)
)

casos_2024$Total_de_casos <- sum(casos_2024$Casos)
casos_2024

casos_2025 <- data.frame(
  Sexo = c("Hombres", "Mujeres"),
  Casos = c(2632, 365)
)

casos_2025$Total_de_casos <- sum(casos_2025$Casos)
casos_2025

# Gráfica de barras de nuevos casos #
library(ggplot2)
nuevos_casos <- ggplot() +
  geom_col(data = casos_2024, aes(x=Sexo, y=Casos),
           fill = "lightblue", colour = "lightblue", 
           alpha = 0.5, width = 0.5) +
  geom_col(data = casos_2025, aes(x= Sexo, y=Casos),
           fill = "orange", colour = "orange", 
           alpha = 0.5, width = 0.4) +
  labs(
    title = "Comparativa de Casos por sexo 2024 vs. 2025", 
    x = "Sexo (Hombres / Mujeres)",
    y = "Número de casos total hasta el tercer trimestre") +
  theme_minimal()

plot(nuevos_casos)

###############################################################################

library(igraph)
library(dplyr)

vih_data<-read.csv("vih_red_completa.csv")

#modificar la base de datos para corregir un error#

library(tidyverse)
vih_data_r <- vih_data$resistencia=="Sin resistencia"
vih_data$resistencia[vih_data_r] <- "sin_resistencia"

conexiones <- vih_data %>%
  filter(cluster_id != 0 & cd4_a < 200) %>%    # Seleccionamos solo aquellos en clusters y con conteo de linfocitos CD4  mayor a 500
  group_by(cluster_id) %>%      # Agrupamos mediante este criterio
  filter(n() >= 4) %>%          # Excluye clusters con menos de 4 individuos
  summarise(pares = list(combn(id_muestra, 2, simplify = F))) %>% 
            # Nuevo df, "pares" que contiene las combinaciones del id según su cluster
  tidyr::unnest(pares) %>%  # Deshanida la lista anterior para que igraph pueda leerla
  mutate(from = sapply(pares, `[[`, 1),      # Genera dos nuevas columnas "from" y "to" 
         to   = sapply(pares, `[[`, 2)) %>%  # señalando de dónde sale y a dónde llega 
  select(from, to)            # Se descarta la columna "pares"

colores  <- c("NNRTI" = "lightblue", "NRTI"="orange", 
              "PI"="lightgreen", "sin_resistencia"="purple")

g <- graph_from_data_frame(conexiones, directed = F)

plot(g, vertex.label = NA,  vertex.size  = 6, 
     vertex.color = colores[vih_data$resistencia])

vcount(g) # Número de nodos
ecount(g) # Número de conexiones

# Tabla de datos limpia con los datos que vamos a utilizar #

columnas_non <- c("colonia", "municipio", "cp", "seguridad_social") #crear un nuevo 
#objeto en el que indiquemos las columnas que no necesitamos#

vih_data_limpia <-vih_data[, !names(vih_data) %in% columnas_non] #para no modificar 
#nuestra base de datos original, creamos una nueva tabla, ahí indicamos de donde va 
#a tomar los datos de las columnas a borrar#

#haremos la comparación de edad con varias categorías, así que vamos a eliminar 
#aquellos datos que no tengan edad para que quede mejor la tabla#

vih_data_limpia <- vih_data_limpia[vih_data_limpia$edad != "", ]
#en la tabla no hay "0" o "NA", son simplemente espacios vacíos, por lo que en la 
#base de datos limpia vamos a indicar de que columna queremos eliminar esos espacios
#vacíos#

# Comparación de los datos obtenidos de la base de datos #

################################################################################

write_graph(g, "red_VIHP1.graphml", format = "graphml") 
# Exportar para visualizar en Cytoscape pero no sirvió


###############################################################################

conexiones_buena <- vih_data_limpia %>%
  filter(cluster_id != 0 & cd4_a < 200) %>%    # Seleccionamos solo aquellos en clusters y con conteo de linfocitos CD4  mayor a 500
  group_by(cluster_id) %>%      # Agrupamos mediante este criterio
  filter(n() >= 4) %>%          # Excluye clusters con menos de 4 individuos
  summarise(pares = list(combn(id_muestra, 2, simplify = F))) %>% 
  # Nuevo df, "pares" que contiene las combinaciones del id según su cluster
  tidyr::unnest(pares) %>%  # Deshanida la lista anterior para que igraph pueda leerla
  mutate(from = sapply(pares, `[[`, 1),      # Genera dos nuevas columnas "from" y "to" 
         to   = sapply(pares, `[[`, 2)) %>%  # señalando de dónde sale y a dónde llega 
  select(from, to)            # Se descarta la columna "pares"


g2 <- graph_from_data_frame(conexiones_buena, directed = F)

vcount(g2) ## corroborar si se hizo bien la base 

############## graficar completo ###############3

layout <- layout_with_graphopt(g2, niter  = 15000, charge = 0.08)


plot(g2,
     vertex.label = NA,
     vertex.size = 3,
     vertex.color = colores[vih_data_limpia$resistencia],
     layout = layout)

legend("bottomright",
       legend = names(colores),
       fill   = colores,
       cex    = 0.5,
       bty    = "n")


########### graficar por clusters  ##############

par(mfrow = c(2, 4))

subg <- decompose(g2)

for (sg in subg) {
  plot(sg, vertex.label = NA,  vertex.size  = 8, 
       vertex.color = colores[vih_data_limpia$resistencia])
}

legend("bottomright",
       legend = names(colores),
       fill   = colores,
       cex    = 0.5,
       bty    = "n")

par(mfrow = c(1, 1))

###################################################################################

library(ggplot2)

# Gráfica para comparar los niveles de linfocitos CD4 por rango de edad #

ggplot(vih_data_limpia, aes(x = edad_rango, y = cd4_a)) +
  geom_boxplot(fill = "pink3", color = "pink2") +
  labs(
    title = "Conteo de linfocitos CD4 por rango de edad", 
    x = "Rango de edad", 
    y = "Conteo de células CD4"
  ) +
  theme_minimal()


# Gráfica para comparar los niveles de carga de virulencia por rango de edad #

ggplot(vih_data_limpia, aes(x = edad_rango, y = cv)) +
  geom_boxplot(fill = "red3", color = "red4") +
  labs(
    title = "Nivel de carga viral por rango de edad", 
    x = "Rango de edad", 
    y = "Carga viral"
  ) +
  theme_minimal()

# Gráfica de resistencia con la toma de las muestras #










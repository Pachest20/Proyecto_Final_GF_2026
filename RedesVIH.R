
library(igraph)
library(dplyr)

vih_data<-read.csv("vih_red_completa.csv")

conexiones <- vih_data %>%
  filter(cluster_id != 0 & cd4_a < 200) %>%    # Seleccionamos solo aquellos en clusters y con conteo de linfocitos CD4  mayor a 500
  group_by(cluster_id) %>%      # Agrupamos mediante este criterio
  filter(n() >= 4) %>%          # Excluye clusters con menos de 4 individuos
  summarise(pares = list(combn(id_muestra, 2, simplify = F))) %>% 
            # Nuevo df, "pares" que contiene las combinaciones del id según su cluster
  tidyr::unnest(pares) %>%  # Deshanida la lista anterior para que igraph pueda leerla
  mutate(from = sapply(pares, `[[`, 1),      # Genera dos nuevas columnas "from" y "to" 
         to   = sapply(pares, `[[`, 2)) %>%  # señalando de dónde sale y a dónde llega 
  select(from, to)            # Se descarta la columna pares

colores  <- c("NNRTI" = "lightblue", "NRTI"="orange", 
              "PI"="lightgreen", "Sin resistenica"="purple")

g <- graph_from_data_frame(conexiones, directed = F)

plot(g, vertex.label = NA,  vertex.size  = 6, 
     vertex.color = colores[vih_data$resistencia])

vcount(g) # Número de nodos
ecount(g) # Número de conexiones

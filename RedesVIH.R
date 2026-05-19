
library(igraph)
library(dplyr)

vih_data<-read.csv("vih_red_completa.csv")

conexiones <- vih_data %>%
  filter(cluster_id != 0) %>%      # Seleccionamos solo aquellos en clusters
  group_by(cluster_id) %>%         # Agrupamos mediante este criterio
  summarise(pares = list(combn(id_muestra, 2, simplify = FALSE))) %>% 
            # Nuevo df, "pares" que contiene las combinaciones del id según su cluster
  tidyr::unnest(pares) %>%  # Deshanida la lista anterior para que igraph pueda leerla
  mutate(from = sapply(pares, `[[`, 1),      # Genera dos nuevas columnas "from" y "to" 
         to   = sapply(pares, `[[`, 2)) %>%  # señalando de dónde sale y a dónde llega 
  select(from, to)            # Se descarta la columna pares

g <- graph_from_data_frame(conexiones, directed = F)

# plot(g, vertex.label = NA,  vertex.size  = 3, vertex.color = "purple")
# Este grafo no corre debido a que es muy extenso, se optó por filtrar más la base de datos

vcount(g) # Número de nodos
ecount(g) # Número de conexiones

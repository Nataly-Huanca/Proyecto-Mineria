#Proyecto final de MINERIA DE DATOS 2
#Nataly Huanca Choque stringr)
#-------FIDALGA---------
rm(list = ls())
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
#Se puede hacer el scraping manual, como tenemos varias categorias y productos
#seria moroso hacerlo, entonces usamos una funcion para que el scrpaing sea mas valioso y facil 
scrap_fidalga <- function(categoria_url, nombre_categoria) {
  cat("Scrapeando categoría:", nombre_categoria, "\n")
  pagina <- tryCatch(read_html(categoria_url), error = function(e) NULL)
  if (is.null(pagina)) return(NULL)
  
  productos <- html_nodes(pagina, ".grid-item-border")
  
  nombres <- html_text2(html_nodes(productos, ".product-title"))
  precios <- html_text2(html_nodes(productos, ".price-regular"))
  n <- min(length(nombres), length(precios))
  if (n == 0) return(NULL)
  
  df <- data.frame(
    tienda = "Fidalga",
    categoria = nombre_categoria,
    nombre = nombres[1:n],
    precio = precios[1:n],
    fecha = Sys.Date(),
    stringsAsFactors = FALSE
  )
  df <- df %>% distinct()  #Se elimina datos duplicados en la bd de Fidalga
  return(df)
}
#categorias de la tienda
categorias_fidalga <- tribble(
  ~categoria, ~url,
  "abarrotes", "https://www.fidalga.com/collections/despensa-y-abarrotes",
  "bebidas", "https://www.fidalga.com/collections/bebidas-y-licores",
  "fiambres", "https://www.fidalga.com/collections/carnes",
  "lacteos", "https://www.fidalga.com/collections/lacteos",
  "panaderia", "https://www.fidalga.com/collections/panaderia-y-reposteria",
  "cuidado_hogar", "https://www.fidalga.com/collections/limpieza-del-hogar",
  "cuidado_personal", "https://www.fidalga.com/collections/cuidado-personal"
)
#Ejecutamos la bd con la limpieza automatica
bd_fidalga <- map2_dfr(categorias_fidalga$url, categorias_fidalga$categoria, scrap_fidalga)
View(bd_fidalga)
print(nrow(bd_fidalga))
write.csv(bd_fidalga, "fidalga_canasta_digital.csv", row.names = FALSE)

#el precio se vuelve en numerico
#Limpieza de precio 
bd_fidalga <- bd_fidalga %>%
  mutate(precio_num = precio %>%
           str_replace_all(",", ".") %>%                #cambia coma por punto en la bd
           str_replace_all("[^0-9\\.]", "") %>%         # se elimina el punto o coma
           as.numeric())
summary(bd_fidalga$precio_num)
sum(is.na(bd_fidalga$precio_num))

###----Analisis predictivo por categoria 
ggplot(bd_fidalga, aes(x = categoria, y = precio_num, fill = categoria)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de precios por categoría - Fidalga", y = "Precio (Bs)", x = "Categoría") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##---Regresion lineal 
modelo1 <- lm(precio_num ~ categoria, data = bd_fidalga)
summary(modelo1)
##---Prediccion 
nuevos <- data.frame(categoria = c("lacteos", "panaderia", "bebidas"))
predict(modelo1, newdata = nuevos)
##---Clustering
set.seed(123)
datos_cluster <- bd_fidalga %>%
  select(precio_num) %>%
  scale()

clust <- kmeans(datos_cluster, centers = 3)
bd_fidalga$grupo <- as.factor(clust$cluster)
#Visualizacion
ggplot(bd_fidalga, aes(x = categoria, y = precio_num, color = grupo)) +
  geom_jitter(width = 0.3, size = 3, alpha = 0.8) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Clustering de productos Fidalga~precio",
    x = "Categoría",
    y = "Precio (Bs)"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  scale_color_manual(values = c("red", "green", "blue"))
write.csv(bd_fidalga, "fidalga_canasta_digital_ANALISIS.csv", row.names = FALSE)

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
####
##------FARMACORP-----
rm(list = ls())
library(jsonlite)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
#PARA CREAR LA BASE DE DATOS DE FARMACORP, EL SITIO WEB TIENE CODIGO JAVA LO QUE NO PERMITE
#EXTRAER TODOS LOS PRODUCTOS EN UNA SOLA FUNCION COMO FIDALGA, LA PSOIBLE SOLUCION
#DE CADA CATEGORIA VER POR SUBCATEGORIA, HACER UN MINI BD Y LUEGO JUNTAR TODO EN UNA BD TOTAL
#PARA LA TIENDA, ES LO QUE REALIZA A CONTINUACION: 
######
#Scraping para Infusiones, Te, Cafe (Abarrotes)
scrap_farmacorp <- function(categoria_url, nombre_categoria) {
  cat("Scrapeando Abarrotes:", nombre_categoria, "\n")
  pagina <- tryCatch(read_html(categoria_url), error = function(e) NULL)
  if (is.null(pagina)) return(NULL)
  
  productos <- html_nodes(pagina, ".productitem")
  nombres <- html_text2(html_nodes(productos, "h2.productitem--title"))
  precios <- html_text2(html_nodes(productos, "div.price--main span.money"))
  n <- min(length(nombres), length(precios))
  if (n == 0) return(NULL)
  
  df <- data.frame(
    tienda = "Farmacorp",
    sub_categoria = nombre_categoria,
    nombre = nombres[1:n],
    precio = precios[1:n],
    fecha = Sys.Date(),
    stringsAsFactors = FALSE
  )
  return(df)
}

# Tabla de subcategorías
subcategorias_farmacorp <- tribble(
  ~categoria, ~url,
  "cafe", "https://farmacorp.com/collections/infusiones-te-cafe-y-otros/cafe?grid_list=grid-view",
  "chocolates", "https://farmacorp.com/collections/infusiones-te-cafe-y-otros/chocolates?grid_list=grid-view",
  "hierbas y mates", "https://farmacorp.com/collections/infusiones-te-cafe-y-otros/hierbas-y-mates?grid_list=grid-view",
  "infusiones te cafe y otros", "https://farmacorp.com/collections/infusiones-te-cafe-y-otros/infusiones-te-cafe-y-otros?grid_list=grid-view",
  "snacks y confiteria", "https://farmacorp.com/collections/infusiones-te-cafe-y-otros/snack-y-confiteria?grid_list=grid-view"
)

# Scrapear todas las subcategorías y unir
bd_abarrotes <- map2_dfr(
  subcategorias_farmacorp$url,
  subcategorias_farmacorp$categoria,
  scrap_farmacorp
)
View(bd_abarrotes)
print(nrow(bd_abarrotes))

##------Scraping para Agua, Jugos y gaseosas (Bebidas)
scrap_farmacorp <- function(categoria_url, nombre_categoria) {
  cat("Scrapeando Bebidas:", nombre_categoria, "\n")
  pagina <- tryCatch(read_html(categoria_url), error = function(e) NULL)
  if (is.null(pagina)) return(NULL)
  
  productos <- html_nodes(pagina, ".productitem")
  nombres <- html_text2(html_nodes(productos, "h2.productitem--title"))
  precios <- html_text2(html_nodes(productos, "div.price--main span.money"))
  n <- min(length(nombres), length(precios))
  if (n == 0) return(NULL)
  
  df <- data.frame(
    tienda = "Farmacorp",
    sub_categoria = nombre_categoria,
    nombre = nombres[1:n],
    precio = precios[1:n],
    fecha = Sys.Date(),
    stringsAsFactors = FALSE
  )
  return(df)
}

# Tabla de subcategorías
subcategorias_farmacorp <- tribble(
  ~categoria, ~url,
  "agua jugos y gaseosas", "https://farmacorp.com/collections/agua-jugos-y-gaseosas/agua-jugos-y-gaseosas?grid_list=grid-view" ,
  "agua jugos y gaseosas", "https://farmacorp.com/collections/agua-jugos-y-gaseosas?constraint=agua-jugos-y-gaseosas&view=view-36&grid_list=grid-view",
  "agua jugos y gaseosas", "https://farmacorp.com/collections/agua-jugos-y-gaseosas?constraint=agua-jugos-y-gaseosas&view=view-48&grid_list=grid-view")
# Scrapear todas las subcategorías y unir
bd_bebidas <- map2_dfr(
  subcategorias_farmacorp$url,
  subcategorias_farmacorp$categoria,
  scrap_farmacorp
)
print(nrow(bd_bebidas))
View(bd_bebidas)

##Scraping para Lacteos, quesos y yogurt (Lacteos)
scrap_farmacorp <- function(categoria_url, nombre_categoria) {
  cat("Scrapeando Lacteos:", nombre_categoria, "\n")
  pagina <- tryCatch(read_html(categoria_url), error = function(e) NULL)
  if (is.null(pagina)) return(NULL)
  
  productos <- html_nodes(pagina, ".productitem")
  nombres <- html_text2(html_nodes(productos, "h2.productitem--title"))
  precios <- html_text2(html_nodes(productos, "div.price--main span.money"))
  n <- min(length(nombres), length(precios))
  if (n == 0) return(NULL)
  
  df <- data.frame(
    tienda = "Farmacorp",
    sub_categoria = nombre_categoria,
    nombre = nombres[1:n],
    precio = precios[1:n],
    fecha = Sys.Date(),
    stringsAsFactors = FALSE
  )
  return(df)
}

# Tabla de subcategorías
subcategorias_farmacorp <- tribble(
  ~categoria, ~url,
  "leches", "https://farmacorp.com/collections/lacteos-y-derivados/leches?grid_list=grid-view" ,
  "leche soya", "https://farmacorp.com/collections/agua-jugos-y-gaseosas/leches-de-soya?grid_list=grid-view",
  "mantequilla", "https://farmacorp.com/collections/lacteos-y-derivados/mantequillas?view=view-48&grid_list=grid-view",
  "yogurt", "https://farmacorp.com/collections/lacteos-y-derivados/yogurt?view=view-48&grid_list=grid-view")
# Scrapear todas las subcategorías y unir
bd_lacteos <- map2_dfr(
  subcategorias_farmacorp$url,
  subcategorias_farmacorp$categoria,
  scrap_farmacorp
)
print(nrow(bd_lacteos))
View(bd_lacteos)

##Scraping para Limpieza de hogar (Limpieza hogar)
scrap_farmacorp <- function(categoria_url, nombre_categoria) {
  cat("Scrapeando Limpieza Hogar:", nombre_categoria, "\n")
  pagina <- tryCatch(read_html(categoria_url), error = function(e) NULL)
  if (is.null(pagina)) return(NULL)
  
  productos <- html_nodes(pagina, ".productitem")
  nombres <- html_text2(html_nodes(productos, "h2.productitem--title"))
  precios <- html_text2(html_nodes(productos, "div.price--main span.money"))
  n <- min(length(nombres), length(precios))
  if (n == 0) return(NULL)
  
  df <- data.frame(
    tienda = "Farmacorp",
    sub_categoria = nombre_categoria,
    nombre = nombres[1:n],
    precio = precios[1:n],
    fecha = Sys.Date(),
    stringsAsFactors = FALSE
  )
  return(df)
}

# Tabla de subcategorías
subcategorias_farmacorp <- tribble(
  ~categoria, ~url,
  "ambientadores", "https://farmacorp.com/collections/limpieza-del-hogar/ambientadores?grid_list=grid-view" ,
  "antiacidos y digestivos", "https://farmacorp.com/collections/limpieza-del-hogar/anti-acidos-y-digestivos?grid_list=grid-view",
  "bicarbonato de sodio", "https://farmacorp.com/collections/limpieza-del-hogar/bicarbonato-de-sodio?grid_list=grid-view",
  "bolsas ecologicas", "https://farmacorp.com/collections/limpieza-del-hogar/bolsas-ecologicas?grid_list=grid-view",
  "cuidado de la casa", "https://farmacorp.com/collections/limpieza-del-hogar/cuidado-de-la-casa?grid_list=grid-view",
  "desechables", "https://farmacorp.com/collections/limpieza-del-hogar/desechables?grid_list=grid-view",
  "detergentes de ropa", "https://farmacorp.com/collections/limpieza-del-hogar/detergentes-de-ropa?grid_list=grid-view",
  "detergentes de ropa","https://farmacorp.com/collections/limpieza-del-hogar/detergentes-de-ropa?page=2&grid_list=grid-view",
  "escobas y mopas", "https://farmacorp.com/collections/limpieza-del-hogar/escobas-y-mopas?grid_list=grid-view",
  "esponjas y toallas de cocina", "https://farmacorp.com/collections/limpieza-del-hogar/esponjas-y-toallas-de-cocina?grid_list=grid-view",
  "guantes", "https://farmacorp.com/collections/limpieza-del-hogar/guantes?grid_list=grid-view",
  "guantes y bolsas", "https://farmacorp.com/collections/limpieza-del-hogar/guantes-y-bolsas?grid_list=grid-view",
  "lavandinas y desinfectantes", "https://farmacorp.com/collections/limpieza-del-hogar/lavandinas-y-desinfectantes?grid_list=grid-view",
  "lustra muebles", "https://farmacorp.com/collections/limpieza-del-hogar/lustra-muebles?grid_list=grid-view",
  "pañuelos y papel higienico", "https://farmacorp.com/collections/limpieza-del-hogar/panuelos-y-papel-higienico?grid_list=grid-view",
  "quita sarros y limpiadores", "https://farmacorp.com/collections/limpieza-del-hogar/quita-sarros-y-limpiadores?grid_list=grid-view",
  "quita sarros y limpiadores", "https://farmacorp.com/collections/limpieza-del-hogar/quita-sarros-y-limpiadores?page=2&grid_list=grid-view",
  "regalos", "https://farmacorp.com/collections/limpieza-del-hogar/regalos-y-juguetes?grid_list=grid-view",
  "vidrios", "https://farmacorp.com/collections/limpieza-del-hogar/vidrios?grid_list=grid-view"  )
# Scrapear todas las subcategorías y unir
bd_lim_hog <- map2_dfr(
  subcategorias_farmacorp$url,
  subcategorias_farmacorp$categoria,
  scrap_farmacorp
)
print(nrow(bd_lim_hog))
View(bd_lim_hog)
######
#Scraping para Cuidado Personal
scrap_farmacorp <- function(categoria_url, nombre_categoria) {
  cat("Scrapeando Cuidado Personal:", nombre_categoria, "\n")
  pagina <- tryCatch(read_html(categoria_url), error = function(e) NULL)
  if (is.null(pagina)) return(NULL)
  
  productos <- html_nodes(pagina, ".productitem")
  
  nombres <- html_text2(html_nodes(productos, "h2.productitem--title"))
  precios <- html_text2(html_nodes(productos, "div.price--main span.money"))
  
  n <- min(length(nombres), length(precios))
  if (n == 0) return(NULL)
  
  df <- data.frame(
    tienda = "Farmacorp",
    categoria = nombre_categoria,
    nombre = nombres[1:n],
    precio = precios[1:n],
    fecha = Sys.Date(),
    stringsAsFactors = FALSE
  )
  
  #df <- df %>% distinct()
  return(df)
}

# Tabla de subcategorías
subcategorias_farmacorp <- tribble(
  ~categoria, ~url,
  "accesorios de belleza", "https://farmacorp.com/collections/cuidado-personal/accesorios-de-belleza?grid_list=grid-view",
  "acondicionadores", "https://farmacorp.com/collections/cuidado-personal/acondicionadores?grid_list=grid-view",
  "afeitado","https://farmacorp.com/collections/cuidado-personal/afeitado?grid_list=grid-view",
  "afeitado","https://farmacorp.com/collections/cuidado-personal/afeitado?page=2&grid_list=grid-view",
  "anti edad","https://farmacorp.com/collections/cuidado-personal/anti-edad?grid_list=grid-view",
  "anti escaldantes","https://farmacorp.com/collections/cuidado-personal/anti-escaldantes?grid_list=grid-view",
  "barbijo","https://farmacorp.com/collections/cuidado-personal/barbijo?grid_list=grid-view",
  "cepillos","https://farmacorp.com/collections/cuidado-personal/cepillos?grid_list=grid-view",
  "cepillos","https://farmacorp.com/collections/cuidado-personal/cepillos?page=2&grid_list=grid-view",
  "cepillos","https://farmacorp.com/collections/cuidado-personal/cepillos?page=3&grid_list=grid-view",
  "cepillos","https://farmacorp.com/collections/cuidado-personal/cepillos?page=4&grid_list=grid-view",
  "cremas y oleos","https://farmacorp.com/collections/cuidado-personal/cremas-oleos-y-geles?grid_list=grid-view",
  "cremas y spray para peinar", "https://farmacorp.com/collections/cuidado-personal/cremas-y-spray-para-peinar?grid_list=grid-view",
  "cuidado de piel","https://farmacorp.com/collections/cuidado-personal/cuidado-de-piel?grid_list=grid-view",
  "depilacion","https://farmacorp.com/collections/cuidado-personal/depilacion?grid_list=grid-view",
  "desodorantes","https://farmacorp.com/collections/cuidado-personal/desodorantes?grid_list=grid-view",
  "desodorantes","https://farmacorp.com/collections/cuidado-personal/desodorantes?page=2&grid_list=grid-view",
  "desodorantes","https://farmacorp.com/collections/cuidado-personal/desodorantes?page=3&grid_list=grid-view",
  "desodorantes","https://farmacorp.com/collections/cuidado-personal/desodorantes?page=4&grid_list=grid-view",
  "enjuage bucal","https://farmacorp.com/collections/cuidado-personal/enjuague-bucal?grid_list=grid-view",
  "hilos y pasta dental","https://farmacorp.com/collections/cuidado-personal/enjuagues-hilos-y-pastas-dentales?grid_list=grid-view",
  "hilos y pasta dental","https://farmacorp.com/collections/cuidado-personal/enjuagues-hilos-y-pastas-dentales?page=2&grid_list=grid-view",
  "hilos y pasta dental","https://farmacorp.com/collections/cuidado-personal/enjuagues-hilos-y-pastas-dentales?page=3&grid_list=grid-view",
  "hilos y pasta dental","https://farmacorp.com/collections/cuidado-personal/enjuagues-hilos-y-pastas-dentales?page=4&grid_list=grid-view",
  "hilos y pasta dental","https://farmacorp.com/collections/cuidado-personal/enjuagues-hilos-y-pastas-dentales?page=5&grid_list=grid-view",
  "fijadores", "https://farmacorp.com/collections/cuidado-personal/fijadores?grid_list=grid-view",
  "hilo dental","https://farmacorp.com/collections/cuidado-personal/hilo-dental?grid_list=grid-view",
  "indumentaria","https://farmacorp.com/collections/cuidado-personal/indumentaria?grid_list=grid-view",
  "jabon liquido intimo","https://farmacorp.com/collections/cuidado-personal/jabon-liquido-intimo?grid_list=grid-view",
  "jabones","https://farmacorp.com/collections/cuidado-personal/jabones?grid_list=grid-view",
  "jabones","https://farmacorp.com/collections/cuidado-personal/jabones?page=2&grid_list=grid-view",
  "jabones","https://farmacorp.com/collections/cuidado-personal/jabones?page=3&grid_list=grid-view",
  "pañales adulto","https://farmacorp.com/collections/cuidado-personal/panales-adulto?grid_list=grid-view",
  "pañales adulto","https://farmacorp.com/collections/cuidado-personal/panales-adulto?page=2&grid_list=grid-view",
  "pañuelos", "https://farmacorp.com/collections/cuidado-personal/panuelos?grid_list=grid-view",
  "papel higienico", "https://farmacorp.com/collections/cuidado-personal/papel-higienico?grid_list=grid-view",
  "pasta dental","https://farmacorp.com/collections/cuidado-personal/pasta-dental?grid_list=grid-view",
  "protectores diarios","https://farmacorp.com/collections/cuidado-personal/protectores-diarios?grid_list=grid-view",
  "protectores solares","https://farmacorp.com/collections/cuidado-personal/protectores-solares-y-bronceadores?grid_list=grid-view",
  "protectores solares","https://farmacorp.com/collections/cuidado-personal/protectores-solares-y-bronceadores?page=2&grid_list=grid-view",
  "protectores solares","https://farmacorp.com/collections/cuidado-personal/protectores-solares-y-bronceadores?page=3&grid_list=grid-view",
  "repelentes","https://farmacorp.com/collections/cuidado-personal/repelentes?grid_list=grid-view",
  "repelentes","https://farmacorp.com/collections/cuidado-personal/repelentes?page=2&grid_list=grid-view",
  "shampoo", "https://farmacorp.com/collections/cuidado-personal/shampoo?grid_list=grid-view",
  "talcos","https://farmacorp.com/collections/cuidado-personal/talcos?grid_list=grid-view",
  "tintes", "https://farmacorp.com/collections/cuidado-personal/tintes?grid_list=grid-view",
  "toallas femeninas y tampones","https://farmacorp.com/collections/cuidado-personal/toallas-femeninas-y-tampones?grid_list=grid-view",
  "toallas femeninas y tampones", "https://farmacorp.com/collections/cuidado-personal/toallas-femeninas-y-tampones?page=2&grid_list=grid-view",
  "toallas humedas","https://farmacorp.com/collections/cuidado-personal/toallas-humedas?grid_list=grid-view",
  "tratamiento", "https://farmacorp.com/collections/cuidado-personal/tratamiento?grid_list=grid-view")
# Scrapear todas las subcategorías y unir
bd_cuid_pers <- map2_dfr(
  subcategorias_farmacorp$url,
  subcategorias_farmacorp$categoria,
  scrap_farmacorp
)
print(nrow(bd_cuid_pers))
View(bd_cuid_pers)

##--Scraping para Cuidado de bebe 
scrap_farmacorp <- function(categoria_url, nombre_categoria) {
  cat("Scrapeando Cuidado bebe:", nombre_categoria, "\n")
  pagina <- tryCatch(read_html(categoria_url), error = function(e) NULL)
  if (is.null(pagina)) return(NULL)
  
  productos <- html_nodes(pagina, ".productitem")
  nombres <- html_text2(html_nodes(productos, "h2.productitem--title"))
  precios <- html_text2(html_nodes(productos, "div.price--main span.money"))
  n <- min(length(nombres), length(precios))
  if (n == 0) return(NULL)
  
  df <- data.frame(
    tienda = "Farmacorp",
    sub_categoria = nombre_categoria,
    nombre = nombres[1:n],
    precio = precios[1:n],
    fecha = Sys.Date(),
    stringsAsFactors = FALSE
  )
  return(df)
}

# Tabla de subcategorías
subcategorias_farmacorp <- tribble(
  ~categoria, ~url,
  "accesorios para el bebe","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/accesorios-para-el-bebe?grid_list=grid-view",
  "acondicionadores","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/acondicionadores?grid_list=grid-view",
  "anti escaldantes","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/anti-escaldantes?grid_list=grid-view",
  "cuidado corporal infantil","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/cuidado-corporal-infantil?grid_list=grid-view",
  "formulas etapa 1","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/formulas-etapa-1?grid_list=grid-view",
  "fromulas etapa 2","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/formulas-etapa-2?grid_list=grid-view",
  "formulas etapa 3","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/formulas-etapa-3?grid_list=grid-view",
  "leche bebe","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/leches?grid_list=grid-view",
  "chupetes","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/mordedores-y-chupetes?grid_list=grid-view",
  "pañales y toallas humedas","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/panales-y-toallas-humedas?grid_list=grid-view",
  "papillas","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/papillas?grid_list=grid-view",
  "shampoo","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/shampoo?grid_list=grid-view",
  "talco", "https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/talcos?grid_list=grid-view",
  "termometro","https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/termometros?grid_list=grid-view",
  "toallas humedas", "https://farmacorp.com/collections/cuidado-de-la-mama-y-bebe/toallas-humedas?grid_list=grid-view")
# Scrapear todas las subcategorías y unir
bd_cui_beb <- map2_dfr(
  subcategorias_farmacorp$url,
  subcategorias_farmacorp$categoria,
  scrap_farmacorp
)
print(nrow(bd_cui_beb))
View(bd_cui_beb)
###
#Al realizar el scraping por subcategorias, ahora se procede a la unificacion
#en una sola bd de datos que nos servira para nuestros modelos, se llama bd_farmacorp
#de esta amnera haremos el analisis exploratorio.
###
#Unir las bd de sub categorias en una sola bd
# Añadir categoría manualmente si no lo hiciste aún:
bd_abarrotes$categoria <- "abarrotes"
bd_bebidas$categoria <- "bebidas"
bd_lacteos$categoria <- "lacteos"
bd_lim_hog$categoria <- "limpieza_hogar"
bd_cuid_pers$categoria <- "cuidado_personal"
bd_cui_beb$categoria <- "cuidado_bebe"

# Unir todas las bases
bd_farmacorp <- bind_rows(
  bd_abarrotes,
  bd_bebidas,
  bd_lacteos,
  bd_lim_hog,
  bd_cuid_pers,
  bd_cui_beb
) %>%
  select(tienda, categoria, sub_categoria, nombre, precio, fecha)
print(nrow(bd_farmacorp))
View(bd_farmacorp)
write.csv(bd_farmacorp, "farmacorp_canasta_digital.csv", row.names = FALSE)

#Iniciando con el analisis con la bd conjunta
##limpieza de datos, borrar texto en el precio y se crea nueva variable del precio 
library(dplyr)
library(stringr)
library(ggplot2)

bd_farmacorp <- bd_farmacorp %>%
  mutate(
    precio_num = precio %>%
      str_replace_all(",", ".") %>%         #cambia la coma decimal por punto
      str_extract("[0-9]+\\.[0-9]+") %>%  #extrae el número con decimal
      as.numeric()
  )
View(bd_farmacorp)
#---Analisis exploratorio
ggplot(bd_farmacorp, aes(x = categoria, y = precio_num, fill = categoria)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribución de precios por categoría - Farmacorp",
    y = "Precio (Bs)", x = "Categoría"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##Interpretacion:
#En el grafico nos deuestra la distribucion de precios por categoria en Farmacorp 
#muestra que las categorias cuidado del bebe y cuidado personal tienden a tener 
#productos con precios mas elevados y una mayor dispersion. En cambio, los productos 
#de la categoria bebidas presentan precios mas bajos y concentrados.

##---Regresion---
modelo_farma <- lm(precio_num ~ categoria, data = bd_farmacorp)
summary(modelo_farma)
#Interpretacion: En esta regresion se analiz como varian los precios segun la 
#categoria de productos en Farmacorp. La categoria base es abarrotes, con un 
#precio promedio de Bs. 45,68. A partir de eso, se ve que bebidas tiene un 
#precio más bajo, alrededor de Bs. 35 menos. En cambio, cuidado del bebé y 
#cuidado personal tienen precios más altos, superando a los abarrotes en 
#aproximadamente Bs. 50. Las otras categorias como lacteos y limpieza del hogar 
#no mostraron diferencias significativas. En resumen, sí hay relación entre el 
#tipo de producto y su precio, pero tambien influyen otros factores ya que el 
#modelo solo explica una parte de la variacion.

##...Prediccion....
nuevos_farma <- data.frame(
  categoria = c("lacteos", "bebidas", "cuidado_bebe", "cuidado_personal", "limpieza_hogar", "abarrotes")
)
predict(modelo_farma, newdata = nuevos_farma)

#Interpretacion
#Obtenidos los siguientes resultados de la prediccion estiman el precio promedio 
#esperado de productos por categoria. Segun el modelo, los productos de cuidado_bebe y 
#cuidado_personal tienen los precios mas altos, con valores estimados de 97.39 y 95.05 bs 
#respectivamente. En contraste, las categorias bebidas y lacteos presentan precios mas bajos, 
#con predicciones de 10.01 y 24.05 bolivianos. Las categorias limpieza_hogar y abarrotes 
#muestran precios intermedios. Estos resultados permiten identificar en que grupos de 
#productos el consumidor debe destinar mas presupuesto y apoyan el analisis del costo de vida digital desde Farmacorp.

##----Clustering---
set.seed(123)
datos_cluster_farma <- bd_farmacorp %>%
  select(precio_num) %>%
  scale()

clust_farma <- kmeans(datos_cluster_farma, centers = 3)
bd_farmacorp$grupo <- as.factor(clust_farma$cluster)

ggplot(bd_farmacorp, aes(x = categoria, y = precio_num, color = grupo)) +
  geom_jitter(width = 0.3, size = 3, alpha = 0.8) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Clustering de productos Farmacorp ~ precio",
    x = "Categoría",
    y = "Precio (Bs)"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  scale_color_manual(values = c("purple", "orange", "pink"))

#Creacion de docuemnto con los analisis 
write.csv(bd_farmacorp, "farmacorp_canasta_digital_ANALISIS.csv", row.names = FALSE)

##Interpretacion del clustering de Farmacorp
#En el grafico de clustering de productos de Farmacorp se puede ver que los 
#productos se agruparon en tres grupos segun su precio. El grupo 1, representado 
#con color morado, concentra la mayoria de productos con precios bajos y se repite en casi
#todas las categorias. El grupo 2, de color naranja, incluye productos de precio medio 
#a alto y se destaca sobre todo en la categoria cuidado del bebe. El grupo 3, en rosado, 
#contiene los productos con precios mas elevados y es mas visible en categorias como cuidado personal. 
#Este analisis me permite ver que hay diferencias claras en el comportamiento de 
#precios segun la categoria y me ayuda a identificar que productos estan en rangos mas altos o bajos.

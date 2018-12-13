##################PROYECTO ###############
##################CURSO R#################

#librerias
library(tidyverse)
library(rvest)

#scrapea desde el aviso 49 al 1969 en 41 paginas#
pags=seq(49,1969,48)

#genero link desde la pagina 2 hasta la ultima#
url=paste0("https://listado.mercadolibre.com.uy/inmuebles/alquiler/montevideo/alquileres-montevideo_Desde_", pags)
#a?ado la primera, que tiene un link diferente#
url[42]="https://listado.mercadolibre.com.uy/alquileres-montevideo"


#inicializo las variables que acumulan informacion de los avisos
precio=vector() #precio del alquiler
atributo=vector() #metraje y cantidad de habitaciones
info=vector()  #que tipo de inmueble es
it=vector() #lugar, que contiene la palabra del BARRIO

for(i in 1:length(url)){
  
  precio=c(precio, url[i] %>% read_html() %>% html_nodes(".price__fraction") %>% html_text()) 
  atributo=c(atributo, url[i] %>% read_html() %>% html_nodes(".item__attrs") %>% html_text() )
  info=c(info,url[i] %>% read_html() %>% html_nodes(".item__info-title") %>% html_text()) 
  it=c(it,url[i] %>% read_html() %>% html_nodes(".item__title") %>% html_text())
  
}
#creo el data frame con la info recolectada
alquileres_df=data.frame(precio, atributo, info, lugar=it)
#la variable atributo tiene dos variables, asi que las separo, en metraje y dormitorios

atrib_df=stringr::str_split(alquileres_df$atributo,"\\|", simplify = TRUE) %>% data.frame() %>%
       dplyr::rename(metraje=X1) %>% dplyr::rename(dormitorios=X2) 

#saco palabras, asi queda solo el numero de metros
atrib_df$metraje=gsub("m?", "",atrib_df$metraje )
atrib_df$metraje=gsub("totales", "",atrib_df$metraje )
atrib_df$metraje=gsub("cubiertos", "",atrib_df$metraje )
#convierte a numerico
atrib_df$metraje=atrib_df$metraje %>% as.numeric() 
atrib_df$dormitorios=atrib_df$dormitorios %>% as.numeric()

#pego las dos variables a la base y borro la variabe atributo
alquileres_df=alquileres_df %>% cbind(atrib_df) %>% select(-atributo)
#la primer palabra describe el inmueble, el resto del string lo descarto
alquileres_df$info=stringr::word(alquileres_df$info,1)
alquileres_df$info=gsub("en","",alquileres_df$info) #queda "en", lo descarto manualmente

#lista de barrios que usa Mercado Libre para publicitar alquileres
barrio=c("Aguada", "Arroyo Seco", "Aires Puros", "Atahualpa",
         "Bella Vista", "Bolivar", "Buceo", "Belvedere", "Brazo Oriental", 
         "Capurro", "Centro", "Col?n","Barrio Sur", "Carrasco", 
         "Cerrito", "Cord?n", "Casabo", "Cerro", "Casavalle", "Ciudad Vieja", 
         "Goes", "Ituizaing?", "Jacinto Vera", "Jardines Hip?dromo", "Larra?aga",
         "La Blanqueada", "La Figurita", "Las Acacias", "La Comercial", 
         "La Teja", "Lezica", "Malvin", "Maro?as", "Malvin Norte", 
         "Maro?as, Curva", "Manga", "Melilla", "Nuevo Par?s", "Pajas BlancaS",
         "Paso de la Arena", "Prado", "Palermo", "Pe?arol", "Puerto Buceo", 
         "Parque Batlle", "Piedras Blancas", "Punta Carretas", "Parque Rod?", 
         "Pocitos", "Punta Gorda", "Paso Molino", "Pocitos Nuevo", "Punta Rieles",
         "Reducto", "Santiago Vazquez", "Sayago", "Tres Cruces", "Uni?n", 
         "Villa Biarritz", "Villa Espa?ola", "Villa Mu?oz", "Villa Dolores",
         "Villa Garc?a", "Villa del Cerro")

#modifico la variable lugar para que muestre solo el barrio. El resto de los datos los descarto
alquileres_df$lugar=str_extract(alquileres_df$lugar, paste(barrio, collapse = "|" ))



##r



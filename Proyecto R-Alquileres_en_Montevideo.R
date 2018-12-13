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
atrib_df$metraje=gsub("m²", "",atrib_df$metraje )
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

alquileres_df$precio=gsub("\\.","",alquileres_df$precio)%>% as.numeric()

#lista de barrios que usa Mercado Libre para publicitar alquileres
barrio=c("Aguada", "Arroyo Seco", "Aires Puros", "Atahualpa",
         "Bella Vista", "Bolivar", "Buceo", "Belvedere", "Brazo Oriental", 
         "Capurro", "Centro", "Colón","Barrio Sur", "Carrasco", 
         "Cerrito", "Cordón", "Casabo", "Cerro", "Casavalle", "Ciudad Vieja", 
         "Goes", "Ituizaingó", "Jacinto Vera", "Jardines Hipódromo", "Larrañaga",
         "La Blanqueada", "La Figurita", "Las Acacias", "La Comercial", 
         "La Teja", "Lezica", "Malvin", "Maroñas", "Malvin Norte", 
         "Maroñas, Curva", "Manga", "Melilla", "Nuevo París", "Pajas BlancaS",
         "Paso de la Arena", "Prado", "Palermo", "Peñarol", "Puerto Buceo", 
         "Parque Batlle", "Piedras Blancas", "Punta Carretas", "Parque Rodó", 
         "Pocitos", "Punta Gorda", "Paso Molino", "Pocitos Nuevo", "Punta Rieles",
         "Reducto", "Santiago Vazquez", "Sayago", "Tres Cruces", "Unión", 
         "Villa Biarritz", "Villa Española", "Villa Muñoz", "Villa Dolores",
         "Villa García", "Villa del Cerro")

#modifico la variable lugar para que muestre solo el barrio. El resto de los datos los descarto
alquileres_df$lugar=str_extract(alquileres_df$lugar, paste(barrio, collapse = "|" ))



##ANALISIS DE LA BASE##
library(ggplot2)
library(ggthemes)
barrio_plot=alquileres_df  %>% filter(!is.na(lugar))  %>%
              data.frame()

ggplot(barrio_plot, aes(fct_rev(fct_infreq(lugar))))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=90,size=4)) +
  labs(x="Barrio", y="Total ")+
  coord_flip()+
  theme_economist()+
  ggtitle("Cantidad de Avisos por Barrio")+
  theme(plot.title = element_text(hjust = 0.5))

info_plot=alquileres_df  %>% filter(!is.na(info)) %>%
  filter(info!="Llave") %>%data.frame()

ggplot(info_plot, aes(fct_infreq(info)))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=90,size=4)) +
  labs(x="Tipo de Propiedad", y="Total ")+
  theme_economist()+
  ggtitle("Cantidad ")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(alquileres_df, aes(x=metraje))+
  geom_density()+
  xlim(0,1000)
  
ggplot(alquileres_df, aes(x=metraje))+
  geom_density()+
  xlim(0,200)+
  labs(x="Metros cuadrados", y="Densidad ")+
  theme_economist()+
  ggtitle("distribucion del tamaño")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(alquileres_df, aes(x=precio))+
  stat_density()+
  xlim(0,100000)+
  labs(x="Precio", y="Densidad ")+
  theme_economist()+
  ggtitle("distribucion del precio")+
  theme(plot.title = element_text(hjust = 0.5))





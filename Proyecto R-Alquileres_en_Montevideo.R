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
  
  precio=c(precio, url[i] %>% read_html() %>% html_nodes(".item__price") %>% html_text()) 
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
atrib_df$metraje=gsub("m�", "",atrib_df$metraje )
atrib_df$metraje=gsub("totales", "",atrib_df$metraje )
atrib_df$metraje=gsub("cubiertos", "",atrib_df$metraje )
#convierte a numerico
atrib_df$metraje=atrib_df$metraje %>% as.numeric() 
atrib_df$dormitorios=gsub("dorms", "",atrib_df$dormitorios)
atrib_df$dormitorios=gsub("dorm", "",atrib_df$dormitorios)
atrib_df$dormitorios=gsub("\\.", "",atrib_df$dormitorios)
atrib_df$dormitorios=atrib_df$dormitorios %>% as.factor() 
#pego las dos variables a la base y borro la variabe atributo
alquileres_df=alquileres_df %>% cbind(atrib_df) %>% select(-atributo)
#la primer palabra describe el inmueble, el resto del string lo descarto
alquileres_df$info=stringr::word(alquileres_df$info,1)
alquileres_df$info=gsub("en","",alquileres_df$info) #queda "en", lo descarto manualmente
alquileres_df$info=gsub("Apartamto","Apartamento",alquileres_df$info)

#lista de barrios que usa Mercado Libre para publicitar alquileres
barrio=c("Aguada", "Arroyo Seco", "Aires Puros", "Atahualpa",
         "Bella Vista", "Bolivar", "Buceo", "Belvedere", "Brazo Oriental", 
         "Capurro", "Centro", "Col�n","Barrio Sur", "Carrasco", 
         "Cerrito", "Cord�n", "Casabo", "Cerro", "Casavalle", "Ciudad Vieja", 
         "Goes", "Ituizaing�", "Jacinto Vera", "Jardines Hip�dromo", "Larra�aga",
         "La Blanqueada", "La Figurita", "Las Acacias", "La Comercial", 
         "La Teja", "Lezica", "Malvin", "Maro�as", "Malvin Norte", 
         "Maro�as, Curva", "Manga", "Melilla", "Nuevo Par�s", "Pajas BlancaS",
         "Paso de la Arena", "Prado", "Palermo", "Pe�arol", "Puerto Buceo", 
         "Parque Batlle", "Piedras Blancas", "Punta Carretas", "Parque Rod�", 
         "Pocitos", "Punta Gorda", "Paso Molino", "Pocitos Nuevo", "Punta Rieles",
         "Reducto", "Santiago Vazquez", "Sayago", "Tres Cruces", "Uni�n", 
         "Villa Biarritz", "Villa Espa�ola", "Villa Mu�oz", "Villa Dolores",
         "Villa Garc�a", "Villa del Cerro")
#modifico la variable lugar para que muestre solo el barrio. El resto de los datos los descarto
alquileres_df$lugar=str_extract(alquileres_df$lugar, paste(barrio, collapse = "|" ))

#WARNING: HAY PRECIOS EN DOLARES QUE NO HABIA VISTO
#LOS CORRIJO PASANDOLOS A PESOS
db=stringr::str_split(alquileres_df$precio,"\\$", simplify = TRUE) %>% data.frame() %>%
  dplyr::rename(Moneda=X1) %>% dplyr::rename(Precio=X2) 


db$Moneda=db$Moneda %>% as.numeric()
db$Precio=gsub("S", "", db$Precio)
db$Precio=db$Precio %>% as.numeric()
db=db %>% mutate(p_hom=0) 

for(i in 1:length(db$Moneda)){
  if(db$Moneda[i]==2){db$p_hom[i]=db$Precio[i]*32}else{db$p_hom[i]=db$Precio[i]}
}

alquileres_df$precio=db$p_hom



##ANALISIS DE LA BASE##
library(ggplot2)
library(plotly)
library(ggthemes)
barrio_plot=alquileres_df  %>% filter(!is.na(lugar))  %>%
  data.frame()
ggplot(barrio_plot, aes(fct_rev(fct_infreq(lugar))))+
  geom_bar()+
  coord_flip()+
  ggtitle("Oferta de Alquileres")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y=element_text(size=5))+  
  labs(x="Barrio", y="Total ")

info_plot=alquileres_df  %>% filter(!is.na(info)) %>%
  filter(info!="Llave") %>%data.frame()

cat_info=alquileres_df  %>% filter(!is.na(info)) %>%
  filter(info!="Llave") %>% group_by(info) %>% count() %>% data.frame() %>% arrange(desc(n)) %>% filter(n>10) %>% select(info)

ggplot(info_plot[info_plot$info%in%cat_info$info,], aes(fct_infreq(info)))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=90,size=4)) +
  labs(x="Tipo de Propiedad", y="Total ")+
  theme_economist()+
  ggtitle("Cantidad ")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(alquileres_df, aes(x=metraje))+
  geom_density()+
  xlim(0,200)+
  labs(x="Metros cuadrados", y="Densidad ")+
  theme_economist()+
  ggtitle("Distribucion del tama�o")+
  theme(plot.title = element_text(hjust = 0.5))

g1=ggplot(alquileres_df, aes(x=metraje, color=lugar))+
  geom_density()+
  xlim(0,200)+
  labs(x="Metros cuadrados", y="Densidad ")+
  theme_economist()+
  ggtitle("Distribucion del tama�o")+
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(g1)

met_plot= alquileres_df  %>% filter(info!="Llave") %>% group_by(info) %>% count %>% filter(n>50) %>% select(info)

ggplot(alquileres_df[alquileres_df$info%in%met_plot$info,], aes(x=metraje))+
  geom_density()+
  xlim(0,200)+
  facet_grid(~info)+
  labs(x="Metros cuadrados", y="Densidad ")+
  theme_economist()+
  ggtitle("Distribucion del tama�o")+
  theme(plot.title = element_text(hjust = 0.5))


alq_plot= alquileres_df %>%  filter(dormitorios!="") %>% filter(info!="Llave")

#filtro las clases con muy pocas observaciones, asi mejora la visualizacion#
clases_dorm=alquileres_df %>%  filter(dormitorios!="") %>% filter(info!="Llave") %>% 
  group_by(dormitorios) %>% count %>% data.frame() %>% filter(n>10) %>% select(dormitorios)

ggplot(alq_plot[alq_plot$dormitorios%in%clases_dorm$dormitorios,]
       , aes(fct_infreq(dormitorios), fill=info))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=90,size=4)) +
  labs(x="Dormitorios", y="Total ")+
  theme_economist()+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(alquileres_df, aes(x=precio))+
  geom_density()+
  xlim(0,100)+
  labs(x="Precio", y="Densidad ")+
  theme_economist()+
  ggtitle("Distribucion del Precio en Miles de Pesos")+
  theme(plot.title = element_text(hjust = 0.5))


g2=ggplot(alquileres_df, aes(x=precio, color=lugar))+
  geom_density()+
  xlim(0,100)+
  labs(x="Precio", y="Densidad ")+
  theme_economist()+
  ggtitle("Distribucion del Precio en Miles de Pesos")+
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(g2)

ggplot(info_plot[info_plot$info%in%cat_info$info,], aes(y=log(precio), x=info)) +
  geom_boxplot() +
  labs(x="Tipo de Inmueble") +
  theme_economist()+
  ggtitle("")


ggplot(alq_plot[alq_plot$dormitorios%in%clases_dorm$dormitorios,], aes(y=log(precio), x=dormitorios)) +
  geom_boxplot() +
  labs(x="Dormitorios") +
  theme_economist()+
  ggtitle("")



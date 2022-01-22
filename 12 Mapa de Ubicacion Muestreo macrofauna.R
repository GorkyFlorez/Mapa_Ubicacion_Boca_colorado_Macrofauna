library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(ggspatial)
library(ggimage)
CP     =st_read( "SHP/CP.geojson")
CPp <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
CP_xy <- cbind(CPp, st_coordinates(st_centroid(CPp$geometry)))
ANP_MDD =st_read( "SHP/ANP_MDD.geojson")
ANP_MD<- st_transform(ANP_MDD,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_princ =st_read( "SHP/Rio_princ.geojson")
Rio_prin<- st_transform(Rio_princ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Derechos_Mineros =st_read( "SHP/Derechos Mineros.geojson")
Derechos_Minero <- st_transform(Derechos_Mineros,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Castana =st_read( "SHP/Castana.geojson")
Castan <- st_transform(Castana,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
NoMaderable =st_read( "SHP/NoMaderable.shp")
NoMaderabl<- st_transform(NoMaderable,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Predios =st_read( "SHP/Predios.shp")
Predio <- st_transform(Predios,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru_D   <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Peru   <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Brasil   <- getData('GADM', country='Brazil', level=0) %>% st_as_sf()
Bolivia   <- getData('GADM', country='Bolivia', level=0) %>% st_as_sf()
MDD    <- subset(Peru_D , NAME_1  == "Madre de Dios")
MDD_xy <- cbind(MDD , st_coordinates(st_centroid(MDD$geometry)))

Julio_Cesar =st_read( "SHP/Julio Cesar.geojson")
Julio_Cesa  <- st_transform(Julio_Cesar ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio =st_read( "SHP/Rio.geojson")
Derechos_Mineros =st_read( "SHP/Derechos Mineros.geojson")
Derechos_Minero <- st_transform(Derechos_Mineros,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Derechos_Minero_xy <- cbind(Derechos_Minero, st_coordinates(st_centroid(Derechos_Minero$geometry)))

d <- data.frame(longitude = c(-69.665),latitude = c(-12.652),name= c("CAMISEA"),
                image = sample(c("PNG/point1.png")))
Zona =ggplot()+
  geom_sf(data = Derechos_Minero, fill="khaki1", color= "khaki3")+
  geom_sf(data = Julio_Cesa)+
  geom_sf(data = Rio, color="blue")+
  geom_image(data = d, aes( longitude, y = latitude, image = image), size = 0.06) +
  coord_sf(xlim = c(-69.68,-69.653), ylim = c(-12.667 ,-12.64),expand = FALSE)+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.4))+
  geom_label(data =  Derechos_Minero_xy , aes(x= X, y=Y, label = CONCESION), size = 2, color="black", fontface = "bold",fontfamily = "serif", alpha=0.4)

Perr =ggplot() +
  geom_sf(data=Peru, color="white", fill="gray90", size=0.5)+
  geom_sf(data=MDD, fill="gray", color="gray")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.4))

Mapa= ggplot()+
  geom_sf(data = Brasil , fill = "grey",color = "white")+
  geom_sf(data = Bolivia , fill = "grey",color = "white")+
  geom_sf(data = Peru , fill = "grey",color = "white")+
  geom_sf(data = MDD, fill = "grey90",color = "white")+
  geom_sf(data =CP_xy,  aes(x=X, y=Y) ,size=0.8,  show.legend = F)+
  geom_sf(data = ANP_MD, fill = "green3",color = "green3", alpha=0.4)+
  geom_label(data =  MDD_xy , aes(x= X, y=Y, label = NAME_2), size = 2.5, color="black", fontface = "bold",fontfamily = "serif", alpha=0.4)+
  #geom_sf_text(data =  CP_xy , aes(x= X, y=Y, label = nombre), size = 3, color="black", fontface = "bold",fontfamily = "serif", alpha=0.4)+
  geom_sf(data = Rio_prin, color="blue", alpha=0.4, size=0.4)+
  geom_sf(data = Derechos_Minero, fill="khaki1", color= "khaki3", alpha=0.3, size=0.4)+
  geom_sf(data = Castan, fill="mediumorchid3", color= "mediumorchid3", alpha=0.3, size=0.4)+
  geom_sf(data = NoMaderabl, fill="indianred1", color= "indianred1", alpha=0.3, size=0.4)+
  #geom_sf(data = Predio, fill="orangered4", color= "orangered4", alpha=0.4, size=0.4)+
  geom_image(data = d, aes( longitude, y = latitude, image = image), size = 0.02) +
  geom_sf(data = MDD_xy , fill = NA ,color = "white")+
  coord_sf(xlim = c(-72.40404,-68.65311), ylim = c(-13.36179 ,-9.879849),expand = FALSE)+
  annotate(geom = "text", x = -72, y = -12.7, hjust = 0, vjust = 1, 
           label = "CUSCO",
           size = 3, family="serif", color = "black")+
  annotate(geom = "text", x = -72, y = -13, hjust = 0, vjust = 1, 
           label = "CUSCO",
           size = 3, family="serif", color = "black")+
annotate(geom = "text", x = -69.3, y = -13.15, hjust = 0, vjust = 1, 
         label = "PUNO",
         size = 3, family="serif", color = "black")+
  annotate(geom = "text", x = -72, y = -10.5, hjust = 0, vjust = 1, 
           label = "UCAYALI",
           size = 3, family="serif", color = "black")+
  annotate(geom = "text", x = -70.2, y = -10.5, hjust = 0, vjust = 1, 
           label = "BRASIL",
           size = 3, family="serif", color = "black")+
  annotate(geom = "text", x = -69, y = -11.5, hjust = 0, vjust = 1, 
           label = "BOLIVIA",
           size = 3, family="serif", color = "black")+
  geom_vline(xintercept = c(-72,-71.5 ,-71, -70.5,-70, -69.5 ,-69), color = "gray50",linetype = "dashed", size = 0.1)+ 
  geom_hline(yintercept = c(-13, -12.5, -12, -11.5, -11, -10.5, -10), color = "gray50",linetype = "dashed", size = 0.1)+
  theme_bw()+
  labs(x = NULL, y = NULL)+
  theme(panel.border = element_rect(color = "black",size = .5,fill = NA),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        legend.title =element_text(size=10, face = "bold"), #tamaño de titulo de leyenda
        legend.position = c(.8, .4),
        plot.title = element_text(size = 21, hjust = 0.5, color = "#4e4d47", family="serif"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "#4e4d47"),
        plot.caption = element_text(size = 8, hjust = 0.95, color = "#4e4d47", family="serif"),
        axis.text = element_text(colour = "black", size = 8),
        axis.text.y  = element_text(angle = 90),
        legend.background = element_blank(),
        legend.text =element_text(size=9))+
  annotate(geom = "text", x=-72.3, y = -13.1, hjust = 0, vjust = 0,
           label = "Beatriz Ponce de Leon Chucuya", fontface = 2,
           size = 4, family = font_rc, color = "grey20")+
  annotate(geom = "text", x = -72, y = -13.2, hjust = 0, vjust = 0,
           label = "Ingenieria Forestal y Medio Ambiente",
           size = 3 , family = font_rc, color = "grey40")+
  # title
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "Mapa de Ubicacion\n   area de Estudio de \n         Macrofauna edafica",
           size = 8, family="serif", color = "grey20")+
  annotation_custom(ggplotGrob(Perr), xmin = -72.40404, xmax = -71.8, ymin = -10.8, ymax = -9.879849) +
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -71, y = -13.3, hjust = 0, vjust = 0, lineheight = .9,
           label = "Author: Beatriz Ponce de Leon (@BeatrizPon) Original Data: Macrofauna, Geometries: RStudio Data: DRFFS, 2022;",
           size = 3, family = font_rc, color = "grey50")+
  annotation_custom(ggplotGrob(Zona), xmin = -69.8, xmax = -69, ymin = -11.2, ymax = -10.3) +
  geom_segment(aes(x=-69.665, xend=-69.8, y=-12.652, yend=-11.2), 
               linetype = "dashed", color = "red", size = 0.3) +
  geom_segment(aes(x=-69.665, xend=-69, y=-12.652, yend=-11.2), 
               linetype = "dashed", color = "red", size = 0.3) 


ggsave("Mapas/MDD.png", Mapa, width = 14, height = 11.76, 
       dpi = 900, type = "cairo-png")

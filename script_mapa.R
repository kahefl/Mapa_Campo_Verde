library(geobr)
library(dplyr)
library(ggplot2)
library(sf)
library(ggrepel)
library(rgdal)

setwd("C:\\Users\\kahel\\OneDrive\\Documents\\Coisas do R\\scripts\\Campo_Verde\\mapa")

#Estados do Mapa
MT <- read_state(code_state = "MT")#Mapa de Mato Grosso
SP <- read_state(code_state = "SP")#Mapa de São Paulo
MS <- read_state(code_state = "MS")#Mapa do Mato Grosso do SUl
PR <- read_state(code_state = "PR")#Mapa do Paraná

#Cidades do Mapa
cuiaba <-read_municipality(code_muni = 5103403)
campo_verde <-read_municipality(code_muni = 5102678)
rondonopolis <-read_municipality(code_muni = 5107602)
aparecida_taboado <-read_municipality(code_muni = 5001003)
santos <-read_municipality(code_muni = 3548500)
paranagua <-read_municipality(code_muni = 4118204)

#Pontos nas cidades
point_cuiaba <- data.frame(lon=-55.916833, lat=-15.532498, muni="Cuiabá")
point_campo_verde <- data.frame(lon=-55.172311, lat=-15.558072, muni="Campo Verde")
point_ferrovia_rondon <- data.frame(lon=-54.669117, lat=-16.695247, muni="Terminal Ferroviário \n Rumo Rondonópolis")
point_ferrovia_apare_taboado <- data.frame(lon=-51.133474, lat=-20.060821, muni="Terminal Intermodal Fibria \n Aparecida do Taboado")
point_porto_santos <- data.frame(lon=-46.300393, lat=-23.966256, muni="Porto de Santos")
point_porto_paranagua <- data.frame(lon=-48.511650, lat=-25.503588, muni="Porto de Paranaguá")
point_hidrovia_caceres <- data.frame(lon=-57.698027, lat=-16.073087, muni="Porto de Cáceres")

#Rodovias
br_163_376 <- readOGR("rotas_mapa\\BR-1636","BR_163_e_BR_376-line")

rodovia_santos_2 <- readOGR("rotas_mapa\\MS-306", "MS_306-line")

#Legenda
legenda1 = c("BR-163/376","BR-364")#Nome dos itens presentes no mapa

cores = c('#FF0000', '#0014A8')#Cores que serão apresentadas

ggplot(fill ="white",color="black")+theme_void()+
  geom_sf(data=MT,fill ="white",size=1)+
  geom_sf(data=MS,fill ="white",size=1)+
  geom_sf(data=SP,fill ="white",size=1)+
  geom_sf(data=PR,fill ="white",size=1)+
  geom_sf(data=cuiaba, fill="white", color="black", size=0.5)+
  geom_sf(data=campo_verde, fill="blue", color="black", size=0.5)+
  geom_sf(data=rondonopolis, fill="grey", color="black", size=0.5)+
  geom_sf(data=aparecida_taboado, fill="grey", color="black", size=0.5)+
  geom_sf(data=santos, fill="grey", color="black", size=0.5)+
  geom_sf(data=paranagua, fill="grey", color="black", size=0.5)+
  geom_path(data=br_163_376, mapping=aes(x=long, y=lat, group=group, color="#e66101"), size =0.85)+#BR_163/376/Independência
  geom_path(data=rodovia_santos_2, mapping=aes(x=long, y=lat, group=group, color="#51484F"),size=0.85)+#BR_364/Quartz
  geom_point(data=point_cuiaba, aes(x=lon,y=lat),fill="red", color="red", size=1.5)+
  geom_point(data=point_campo_verde, aes(x=lon,y=lat),fill="red", color="red", size=1.5)+
  geom_point(data=point_ferrovia_rondon, aes(x=lon,y=lat),fill="#B7410E", color="#B7410E", size=1.5)+
  geom_point(data=point_ferrovia_apare_taboado, aes(x=lon,y=lat),fill="#B7410E", color="#B7410E", size=1.5)+
  geom_point(data=point_porto_santos, aes(x=lon,y=lat),fill="#191970", color="#191970", size=1.5)+
  geom_point(data=point_porto_paranagua, aes(x=lon,y=lat),fill="#191970", color="#191970", size=1.5)+
  geom_text_repel(data=point_cuiaba, aes(x=lon, y=lat, label=muni),pch=21, segment.color = "black", segment.size = 0.5, direction = "x", nudge_x=-1.03, nudge_y=-1.3, size=5)+
  geom_label_repel(data=point_ferrovia_apare_taboado,aes(x=lon, y=lat, label=muni), segment.color = "black",segment.size =0.25, direction = "x", nudge_x =2, nudge_y =2.035, fontface=2, hjust=0.5,size=5)+
  geom_label_repel(data=point_ferrovia_rondon,aes(x=lon, y=lat, label=muni), segment.color = "black",size=5,segment.size = 0.25, direction = "x", nudge_x=5.5, nudge_y=0.50, hjust= 0.5,fontface=2)+
  geom_text_repel(data=point_campo_verde,aes(x=lon, y=lat, label=muni), nudge_x = .15, nudge_y = 1, fontface=2, size=6)+
  geom_label_repel(data=point_porto_santos, aes(x=lon, y=lat, label=muni), segment.color = "black",size=5, segment.size = 0.25, direction = "x", nudge_x=5.5, nudge_y=1.50, hjust= 0.5,fontface=2)+
  geom_label_repel(data=point_porto_paranagua, aes(x=lon, y=lat, label=muni), segment.color = "black",size=5,segment.size = 0.25, direction = "x", nudge_x=0.5, nudge_y=-0.50, hjust= 0.5,fontface=2)+
  scale_colour_manual(values = cores, labels = legenda1)+#Escala de cores presentes no mapa
  labs(color ='Rodovias')+theme(legend.title = element_text(face = 'bold', size = 14))+#Título referente aos itens da legenda
  theme(legend.text = element_text(face = NULL, size = 11.5))+#Tamanho da letra do título da legenda
  theme(legend.background = element_rect(fill="white", size=.5, linetype="blank"))+#Fundo do quadro da legenda
  theme(legend.position=c(0.11, 0.15))#Posição da legenda

### Datos de Miércoles - GOT

pacman::p_load(tidyverse, readr, wesanderson, RColorBrewer, ggpol, ggparl)
#devtools::install_github('erocoar/ggparl')

tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/rivaquiroga/para-datos-de-miercoles/master/semana2_GOT/tiempo_pantalla2.csv")

cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/rivaquiroga/para-datos-de-miercoles/master/semana2_GOT/cambio_lealtades2.csv")

personajes_libros <- readr::read_csv("https://raw.githubusercontent.com/rivaquiroga/para-datos-de-miercoles/master/semana2_GOT/personajes_libro2.csv")

### Cantidad de personajes por casa/grupo al que pertenece
  personajes_x_casa <- personajes_libros %>%
  group_by(lealtad) %>% 
  summarise(n = n()) %>% 
  mutate(porc = round(n / sum(n) * 100, 1))

t_personajes_x_casa <- personajes_x_casa %>% 
  summarise("No pertenece a ninguna casa" = sum(n[lealtad == "Ninguna"]),
            "Pertenece a alguna de las casas"  = sum(n[lealtad != "Ninguna"])) %>% 
  gather(key = "pertenencia", value = valor) %>% 
  mutate(fraccion = round(valor / sum(valor)*100),2) %>% 
  arrange(desc(pertenencia)) %>%
  mutate(lab.ypos = round(cumsum(fraccion) - 0.5*fraccion),2)

t_personajes_x_casa

jpeg("pertenece a casa o no.png", height=4, width=10, units="in", res=300)
ggplot(t_personajes_x_casa, aes(x = 2, y = fraccion, fill = pertenencia)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(fraccion, "%")), color = "white")+
  theme_void()+
  theme(
    plot.title = element_text(size=14, face="bold", hjust = 0)
    ) +
  xlim(0.5, 2.5) +
  ggtitle("Distribución porcentual de los personajes en todos los libros según 
          pertenencia (o no) a alguna casa / grupo") + 
  scale_fill_manual(" ", values = c("#004762", "#696d6e"))
dev.off()


### tabla con personajes que pertenecen a alguna casa / grupo
personajes_casa <- personajes_libros %>%
  group_by(lealtad) %>% 
  filter(lealtad != "Ninguna") %>% 
  summarise(n = n()) %>% 
  mutate(porc = round(n / sum(n) * 100, 1)) %>% 
  arrange(porc)

### Acumulado
acum_casa <- personajes_casa %>% 
  filter(lealtad != "Ninguna") %>% 
  mutate(acum = cumsum(porc),
         pal  = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", 
                  "#ff7f00", "#ffff99", "#08306b", "black")) %>% 
  arrange(desc(porc))

jpeg("Personajes de los libros según casa a la que pertenecen.png", 
     height=4, width=10, units="in", res=300)
ggplot_parliament(acum_casa$lealtad, acum_casa$n, acum_casa$pal) +
  ggtitle("Distribución de personajes en los libros por casa de pertenencia") + 
  theme(plot.title=element_text(size=14, face="bold", hjust = 0.5))
dev.off()

#temporadas <- colnames(personajes_libros[ ,5:10])

### Por Hacer: loop para cada libro

##### Bases por temporada
libro_1 <- personajes_libros %>% 
  filter(juego_de_tronos==1)
libro_2 <- personajes_libros %>% 
  filter(choque_de_reyes==1)
libro_3 <- personajes_libros %>% 
  filter(tormenta_de_espadas==1)
libro_4 <- personajes_libros %>% 
  filter(festin_de_cuervos==1)
libro_5 <- personajes_libros %>% 
  filter(danza_de_dragones==1)

libros <- c("libro_1", "libro_2", "libro_3", "libro_4", "libro_5")
for(i in libros){
  grabo <- print(i)
}


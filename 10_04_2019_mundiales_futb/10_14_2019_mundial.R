pacman::p_load(tidyverse, ggplot2, readr,  wordcloud, wesanderson)

### Carga de la base
base <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "/t")

### Armado de la variable "resultado", con la que se grafica luego
base <- base %>% 
  mutate(resultado  = case_when(equipo_1_final > equipo_2_final ~ paste0(equipo_1_final, "-", equipo_2_final),
                               equipo_1_final < equipo_2_final  ~ paste0(equipo_2_final, "-", equipo_1_final),
                               equipo_1_final == equipo_2_final ~ paste0(equipo_1_final, "-", equipo_2_final)))

### Tabla de trabajo para las visualizaciones
resultados <- data.frame(table(base$resultado)) %>% 
  arrange(desc(Freq))

### Nube de palabras
png("nube_de_resultados.png", height=4, width=4, units="in", res=300)
resultados %>% 
  with(wordcloud(Var1, Freq, 
                 size = 1.5,
                 min.freq = 0, 
                 max.words=Inf, 
                 random.order=FALSE, 
                 rot.per=-1,
                 colors=wes_palette("Zissou1")))
dev.off()

### Gráfico de barras, para ver la distribución exacta de los resultados más frecuentes
png("freq_de_resultados.png", height=6, width=9, units="in", res=300)
ggplot(data=resultados, aes(x=factor(Var1,
                                     levels = rev(Var1)),
                            y=Freq)) +
  geom_bar(stat="identity", fill="#93CCBC", width = 0.4)+
  geom_text(aes(label=Freq), vjust=0, hjust = -0.1, size=3)+
  theme_minimal() +
  ggtitle("Ranking de resultados más frecuentes") +
  theme(plot.title= element_text(face = "bold", size=12, hjust = 0.1, vjust = 0)) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(panel.grid=element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 8)) + 
  expand_limits(y = 1) +
  coord_flip()
dev.off()

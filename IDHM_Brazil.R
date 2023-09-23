library(tidyverse)
library(geobr)
library(readxl)
library(sf)
library(rayshader)
library(viridis)
library(av)
library(showtext)
library(ggtext)
library(patchwork)

########## Dados apenas com as informações pontuais
dados <- read_excel("Atlas 2013_municipal, estadual e Brasil.xlsx", sheet = "MUN 91-00-10") # Lendo os dados
dados <- dados %>% select(Codmun7, ANO, Município, IDHM) # Selecionando apenas as variáveis que iremos utilizar
dados_2010 <- dados %>% filter(ANO == "2010") # Filtrando apenas o IDHM de 2010

malha_municipal <- geobr::read_municipality() # Lendo os dados geoespaciais

dados_finais <- left_join(dados_2010,
                          malha_municipal %>% dplyr::select(code_muni, geom),
                          by = c("Codmun7"="code_muni")) # Juntando os dois dataframes a partir do código do município

dados_finais <- st_as_sf(dados_finais) # Transformando o dataframe final em um objeto sf

## Tema do mapa
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      plot.title = element_text(hjust = .5, vjust = .5,face = "bold", family = "Gudea",size = 18),
      plot.subtitle = element_text(size= 15, family = "Gudea",hjust=0.5, color = "#4e4d47"),
      plot.caption = element_text(size= 8, family = "Gudea",hjust=0.5, color = "#4e4d47"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#FDF5EF", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#FDF5EF", color = NA), 
      panel.background = element_rect(fill = "#FDF5EF", color = NA), 
      legend.background = element_rect(fill = "#FDF5EF", color = NA),
      panel.border = element_blank(),
      plot.margin = margin(20,20,20,20),
      ...
    )
}

## Gráfico em ggplot
mtplot <- ggplot(data = dados_finais) +
  geom_sf(aes(fill = IDHM), color = NA) +
  theme_map(legend.position = c(0.9, 0.3)) +
  scale_fill_viridis_c("") +
  labs(x = NULL,
       y = NULL,
       title = "Brasil",
       subtitle = "Índice de Desenvolvimento Humano Municipal (IDHM)",
       caption = "Fonte: Atlas Brasil,2010, Autor: João L. Simon")

## Renderização em 3D com Rayshader
plot_gg(mtplot, multicore = TRUE, width = 12 ,height=12, scale = 150, windowsize=c(1400, 866),
        zoom = 0.55, phi = 30, pointcontract=1, background="#ebebe5", units = "cm")


## Movimentação da camera
ease_function = function(beginning, end, steepness = 1, length.out = 180) {
  single = (end) + (beginning - end) * 1/(1 + exp(seq(-10, 10, length.out = length.out)/(1/steepness)))
  single
}

zoom_values = c(ease_function(0.4,0.2), ease_function(0.2,0.4))

## Gerando o vídeo
render_movie(filename = "IDHM_teste4", type = "custom",
             fps = 30,
             phi = 50 + 40 * cos(1:360 * pi /180), 
             theta = 1 - 1:360, 
             zoom = zoom_values)

  
render_camera(theta=.5,phi=68,fov=0,zoom=0.55)
#render_highquality()  


##############################################################################
#### MISC ####
font <- "Gudea" # Text Font
font_add_google(family=font, font, db_cache = TRUE)
bg <- "#F4F5F1" # Background color
txt_col <- "black"
showtext_auto(enable = TRUE)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      plot.title = element_text(hjust = .5, vjust = .5,face = "bold", family = "Gudea",size = 18),
      plot.subtitle = element_text(size= 15, family = "Gudea",hjust=0.5, color = "#4e4d47"),
      plot.caption = element_text(size= 8, family = "Gudea",hjust=0.5, color = "#4e4d47"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#FDF5EF", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#F4F5F1", color = NA), 
      panel.background = element_rect(fill = "#F4F5F1", color = NA), 
      legend.background = element_rect(fill = "#F4F5F1", color = NA),
      panel.border = element_blank(),
      plot.margin = margin(20,20,20,20),
      ...
    )
}

## Heatmap do IDHM em 2D
mtplot <- ggplot(data = dados_finais) +
  geom_sf(aes(fill = IDHM), color = NA) +
  theme_map(legend.position = c(0.8, .1)) +
  scale_fill_viridis_c(guide = guide_legend( keyheight = unit(2, units = "mm"), keywidth=unit(5, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1, ncol = 5)) +
  labs(x = NULL,
       y = NULL,
       title = "BRASIL",
       caption = "**Autor:** João L. Simon <br> **Fonte:** Atlas Brasil, 2010") + 
  theme(plot.caption = element_markdown(hjust = 0))

## Texto do subtítulo
text <- tibble(
  x = 0, y = 0,
  label = "O Índice de Desenvolvimento Humano Municipal (IDHM) é uma medida composta de indicadores de três dimensões do desenvolvimento humano: longevidade, educação e renda. O índice varia de 0 a 1. Quanto mais próximo de 1, maior é o padrão e qualidade de vida da população local."
)

## Gerando o subtítulo no formato ggplot
sub <- ggplot(text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = bg, fill=bg, width = unit(10, "lines"),
    family=font, size = 3, lineheight = 1.2
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color=bg, fill=bg))

# Título
text2 <- tibble(
  x = 0, y = 0,
  label = "**O que é o <br>IDHM?**"
)

## Gerando o título no formato ggplot
title <- ggplot(text2, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = bg, fill=bg, width = unit(8, "lines"),
    family=font, size = 10, lineheight = 1
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color=bg, fill=bg))

## Juntando todos os 3 gráficos e gerando a "borda" branca ao redor
finalPlot <- (title+sub)/mtplot +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    caption = "",
    theme=theme(plot.caption = element_markdown(hjust=0, margin=margin(0,0,0,0), size=6, color=txt_col, lineheight = 0.2),
                plot.margin = margin(15,15,15,15)))


showtext_opts(dpi = 600) 

# Salvando a figura final
ggsave("consumer_confidence.png",
       bg=bg,
       height = 7,
       width = 5,
       dpi = 600)


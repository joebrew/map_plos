library(rgdal)
library(dplyr)
library(raster)
library(readxl)
library(RColorBrewer)
require(maptools)
library(ggrepel) # for avoiding overlapping labels in ggplot2
library(ggthemes)


##### Read in shapefiles
# arruamento <- readOGR('data/spatial/', 'arruamento')
bairros_e_zonas <- readOGR('data/spatial/', 'Bairros_e_Zonas')
# zonas_administrativas <- readOGR('data/spatial/', 'zonas_administrativas')

# For shape files we don't have, get spatial data from raster package
brazil0 <- getData('GADM', country = 'BRA', level = 0)
brazil1 <- getData('GADM', country = 'BRA', level = 1)
brazil2 <- getData('GADM', country = 'BRA', level = 2)
brazil3 <- getData('GADM', country = 'BRA', level = 3)
# save.image('~/Desktop/brazil.RData')

##### Read in data
# Counts by bairro
mulheres <- read_excel('data/spreadsheets/ECO.xls')
# Raw data
raw <- read_excel('data/spreadsheets/aaobserva.xls')
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Figure 1 - bairros
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##### Join data to spatial

# Define which names are good (ie, in map)
goods <- as.character(sort(unique(bairros_e_zonas@data$NOME)))
# Define which names don't need changing
ins <- as.character(sort(unique(mulheres$NOME))) %in% 
  as.character(sort(unique(bairros_e_zonas@data$NOME)))
# Define which names need changing
outs <- as.character(sort(unique(mulheres$NOME))[!ins])

# Define corrections
corrections <- 
  data.frame(mulheres = as.character(sort(unique(mulheres$NOME))),
             stringsAsFactors = FALSE)
corrections$bairros <- ifelse(corrections$mulheres %in% goods,
                              corrections$mulheres, 
                              NA)

# Make corrections

corrections$bairros[corrections$mulheres == 'COLÔNIA TERRA NOVA'] <- 
  'COL TERRA NOVA'
corrections$bairros[corrections$mulheres == 'LIRIO DO VALE'] <-
  'LÍRIO DO VALE'
corrections$bairros[corrections$mulheres == 'NOSSA SENHORA DAS GRAÇAS'] <-
  'N SRA DAS GRAÇAS'
corrections$bairros[corrections$mulheres == 'PARQUE 10 DE NOVEMBRO'] <-
  'PARQUE DEZ DE NOVEMBRO'
corrections$bairros[corrections$mulheres == 'TANCREDO NEVES '] <-
  'TANCREDO NEVES'
corrections$bairros[corrections$mulheres == 'ZUMBI'] <-
  'ZUMBI DOS PALMARES'

# THE FOLLOWING WE ARE JUST TAKING OUT
# corrections$bairros[corrections$mulheres == 'NOVO ALEIXO'] <-
#   'ALEIXO'
# corrections$bairros[corrections$mulheres == 'COLONIA ANTONIO ALEIXO'] <-
#   'ALEIXO'
# corrections$bairros[corrections$mulheres == 'CAMPOS SALES'] <-
#   'SANTA ETELVINA'
# corrections$bairros[corrections$mulheres == 'CIDADE DE DEUS'] <-
#   'CIDADE NOVA'
# corrections$bairros[corrections$mulheres == 'LAGO AZUL'] <-
#   'TARUMÃ'
# corrections$bairros[corrections$mulheres == 'PARQUE DAS LARANJEIRAS'] <-
#   'FLORES'

# Implement the corrections
names(corrections) <- c('NOME', 'new_name')
mulheres <- left_join(mulheres, corrections, by = 'NOME')
mulheres$NOME <- mulheres$new_name; mulheres$new_name <- NULL

# Make the join
bairros_e_zonas@data <- 
  left_join(x = bairros_e_zonas@data,
            y = mulheres,
            by = 'NOME')

# Set to 0 the NAs
bairros_e_zonas@data$MULHERES[is.na(bairros_e_zonas@data$MULHERES)] <- 0

# Define a color vector
# cols <- rev(brewer.pal(n = 9, name = 'Spectral'))
cols <- brewer.pal(n = 9, name = 'Greens')
colors <- colorRampPalette(cols)(max(bairros_e_zonas@data$MULHERES, na.rm = TRUE))
colors <- adjustcolor(colors, alpha.f = 0.6)

bairros_e_zonas@data$color <- 'white'
# bairros_e_zonas@data$color[bairros_e_zonas@data$MULHERES > 0] <-
#   colors[bairros_e_zonas@data$MULHERES[bairros_e_zonas@data$MULHERES > 0]]
#   
# bairros_e_zonas@data$color <- ifelse(
#   bairros_e_zonas@data$MULHERES == 0,
#   'white',
#   ifelse(bairros_e_zonas@data$MULHERES > 0,
#          colors[bairros_e_zonas@data$MULHERES],
#          'orange'))

for (i in 1:nrow(bairros_e_zonas@data)){
  if(bairros_e_zonas@data$MULHERES[i] > 0){
    bairros_e_zonas@data$color[i] <-
      # colors[bairros_e_zonas@data$MULHERES[i]]
      colors[bairros_e_zonas@data$MULHERES[i]]
  }
}

# # PLOT GG STYLE
# 
# # # fortify map
# bairros_e_zonas@data$place_id <- row.names(bairros_e_zonas@data)
# row.names(bairros_e_zonas@data) <- NULL
# bairros_e_zonas_f <- fortify(bairros_e_zonas, region = 'place_id')
# # bring in number of women
# 
# bairros_e_zonas_f <- left_join(bairros_e_zonas_f,
#                                bairros_e_zonas@data %>%
#                                  mutate(OBJECTID = as.character(OBJECTID)),
#                                by = c('id' = 'OBJECTID'))
# 
# Create a labeling dataframe
label_df <- bairros_e_zonas@data[,c('NOME', 'MULHERES')]
label_df <- label_df[!duplicated(label_df$NOME),]
# add lat long
label_df$long <- coordinates(bairros_e_zonas)[,1]
label_df$lat <- coordinates(bairros_e_zonas)[,2]
# Keep only those with > 0 women
label_df <- label_df[label_df$MULHERES > 0,]
# Replace spaces with line breaks
label_df$NOME <- gsub(' ', '\n', label_df$NOME)
# 
# 
# ggplot() +
#   coord_map() +
#   geom_polygon(data = bairros_e_zonas_f,
#                aes(x = long, y =lat, group = group,
#                    fill = MULHERES), color = 'grey') +
#   geom_label_repel(data = label_df, 
#                    aes(long, lat, 
#                        #fill = factor(NOME),
#                        label = factor(NOME)),
#                    fontface = 'bold',
#                    color = 'black',
#                    size = 1.5, 
#                    box.padding = unit(1.75, 'lines')) +
#   theme_tufte() +
#   theme(axis.ticks.length = unit(0.001, "mm")) + labs(x=NULL, y=NULL) +
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="none",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank()) +
#   scale_fill_manual(guide = guide_legend(title = 'Area'),
#                     values = cols)



# Plot
pdf('figure_1.pdf', width = 10, height = 8)
plot(bairros_e_zonas,
     col = bairros_e_zonas@data$color,  
     border = adjustcolor('black', alpha.f = 0.3)
     # border = NA
     )
text(x = label_df$long,
     y = label_df$lat,
     label = label_df$NOME,
     cex = 0.3,
     col = adjustcolor('black', alpha.f = 0.6))
legend('right',
       fill = colors,
       ncol = 1,
       cex = 0.8,
       border = NA,
       col = colors,
       legend = 1:length(colors),
       title = 'Women')
title(main = 'Figure 1')
dev.off()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Figure 2 - bairros
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
amazonas <- brazil2[brazil2@data$NAME_1 == 'Amazonas',]
amazonas@data$mulheres <- 0
amazonas@data$mulheres[amazonas@data$NAME_2 == 'São Gabriel de Cahoeira'] <- 1
amazonas@data$mulheres[amazonas@data$NAME_2 == 'Barcelos'] <- 1
amazonas@data$mulheres[amazonas@data$NAME_2 == 'Presidente Figueiredo'] <- 1
amazonas@data$mulheres[amazonas@data$NAME_2 == 'Tapauá'] <- 1
amazonas@data$mulheres[amazonas@data$NAME_2 == 'Rio Preto da Eva'] <- 2
amazonas@data$color <- adjustcolor(
  ifelse(amazonas@data$mulheres == 0, 'white',
         ifelse(amazonas@data$mulheres == 1, 'lightblue',
                ifelse(amazonas@data$mulheres == 2, 'darkblue', 'black'))),
  alpha.f = 0.6)

# Make a labeling vector
label_df <- amazonas@data
label_df$long <- coordinates(amazonas)[,1]
label_df$lat <- coordinates(amazonas)[,2]

label_df <- label_df[label_df$mulheres > 0,]
label_df$NAME_2 <- gsub(' ', '\n', label_df$NAME_2)


pdf('figure_2.pdf', width = 10, height = 8)
plot(amazonas,
     col = amazonas@data$color,
     border = adjustcolor('black', alpha.f = 0.3))
text(x = label_df$long,
     y = label_df$lat,
     label = label_df$NAME_2,
     cex = 0.4,
     col = adjustcolor('black', alpha.f = 0.6))

legend('bottomright',
       fill = adjustcolor(c('white', 'lightblue', 'darkblue'), alpha.f = 0.6),
       legend = c(0, 1, 2),
       cex = 0.8,
       border = NA,
       title = 'Women')
title(main = 'Figure 2')
dev.off()

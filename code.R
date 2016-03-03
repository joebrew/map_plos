library(rgdal)
library(dplyr)
library(raster)
library(readxl)
library(RColorBrewer)
require(maptools)


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
corrections$bairros[corrections$mulheres == 'COLONIA ANTONIO ALEIXO'] <-
  'ALEIXO'
corrections$bairros[corrections$mulheres == 'COLÔNIA TERRA NOVA'] <- 
  'COL TERRA NOVA'
corrections$bairros[corrections$mulheres == 'LIRIO DO VALE'] <-
  'LÍRIO DO VALE'
corrections$bairros[corrections$mulheres == 'NOSSA SENHORA DAS GRAÇAS'] <-
  'N SRA DAS GRAÇAS'
corrections$bairros[corrections$mulheres == 'NOVO ALEIXO'] <-
  'ALEIXO'
corrections$bairros[corrections$mulheres == 'PARQUE 10 DE NOVEMBRO'] <-
  'PARQUE DEZ DE NOVEMBRO'
corrections$bairros[corrections$mulheres == 'TANCREDO NEVES '] <-
  'TANCREDO NEVES'
corrections$bairros[corrections$mulheres == 'ZUMBI'] <-
  'ZUMBI DOS PALMARES'
corrections$bairros[corrections$mulheres == 'CAMPOS SALES'] <-
  NA
corrections$bairros[corrections$mulheres == 'CIDADE DE DEUS'] <-
  NA
corrections$bairros[corrections$mulheres == 'LAGO AZUL'] <-
  NA
corrections$bairros[corrections$mulheres == 'PARQUE DAS LARANJEIRAS'] <-
  NA

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
cols <- rev(brewer.pal(n = 9, name = 'Spectral'))
# cols <- brewer.pal(n = 9, name = 'Greens')
colors <- colorRampPalette(cols)(max(bairros_e_zonas@data$MULHERES, na.rm = TRUE))
colors <- adjustcolor(colors, alpha.f = 0.6)

# Plot
plot(bairros_e_zonas,
     col = colors[bairros_e_zonas@data$MULHERES],
     border = adjustcolor('black', alpha.f = 0.7))

legend('bottomright',
       fill = colors,
       ncol = 2,
       cex = 0.8,
       border = NA,
       col = colors,
       legend = 1:length(colors),
       title = 'Women')

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

plot(amazonas,
     col = amazonas@data$color,
     border = adjustcolor('black', alpha.f = 0.6))

legend('bottom',
       fill = adjustcolor(c('white', 'lightblue', 'darkblue'), alpha.f = 0.6),
       legend = c(0, 1, 2),
       title = 'Women',
       ncol = 3)
title(main = 'Figure 2 (Amazonas)')

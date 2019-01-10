rm(list = ls())

#=========================================================================
#= IVS MAPA  ============================================================ 
#========================================================================
library(ggmap); library(ggplot2); library(dplyr); library(rgdal); 
library(gridExtra); library(dplyr);library(sp);
library(broom);library(tidyr);library(leaflet);library(RColorBrewer);
library(openxlsx)

## install 'webshot' package
library(devtools);library(htmlwidgets)
library(webshot)

# importando o shape --------------------------------------------------

library(raster)
shape <- shapefile("/home/furriel/Dropbox/Observatório 2018/Shapes/shapeRMMCenso2010PorAeds2/41SEE250GC_RMM.shp")
head(shape)
# merge -------------------------------------------------------------------

df <- read.csv("/home/furriel/Dropbox/Observatório 2018/Dados/grupos.csv")
names(df)[16] <- "areaPond"

shapeRMM <- merge(shape, df, by ="areaPond")
head(shapeRMM)

# -----------------------------------------------------------------------
# Criando os labels
paleta <- c("#FF7F00", "#984EA3", "#377EB8", "#E41A1C", "#4DAF4A")
tbins  <- c(1,2,3,4,5,6)
tutpal <- colorBin(paleta, bins=tbins)

pd <- paste0("<strong>Cluster: </strong>", shapeRMM$cluster, " <strong>Apond: </strong> ",shapeRMM$nomes)

labels <- sprintf(
  "<strong>%s</strong><br/> </strong>",
  shapeRMM$areaPond) %>% lapply(htmltools::HTML)

#-------------------------------------------------------------------------------
# O Mapa --------------------------------------------------------------------------
#--------------------------------------------------------------------------------

mapa <- leaflet() %>% addTiles() %>% 
  # Basemap
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  # Setores
  addPolygons(data = shapeRMM, 
              fillColor = ~CD_GEOCODI,  weight = 1,
              opacity = 0.8, dashArray = "3", color = "white",
              highlight = highlightOptions(
                weight = 5, color = "black",
                dashArray = "",  fillOpacity = 0.7, fillColor = "white", 
                bringToFront = T), label = labels, group = "Aponds",
              labelOptions = labelOptions(noHide = T, direction = "right")) %>%
  #Cluster
  addPolygons(data = shapeRMM, 
              fillColor = ~tutpal(cluster), weight = 1, color = "white", 
              fillOpacity = 0.8, smoothFactor = 0.6,
              group = "Cluster", popup = pd,
              highlightOptions = highlightOptions(color = "Black", weight = 3)) %>% 
  #Legenda
  #addProviderTiles("CartoDB.Positron") %>% # Fixar google maps
  addLegend("bottomright",  # location
            colors = c("#FF7F00", "#984EA3", "#377EB8", "#E41A1C", "#4DAF4A"),    # palette function
            title = "Clusters", opacity = 1,
            labels = c("1", "2","3","4","5"),
            labFormat = labelFormat(prefix = "[", suffix = "]", between = ";"))
mapa




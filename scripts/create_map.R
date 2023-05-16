### MASTER script for Urban gradient analysis

library(rgdal)
library(stringr)
library(ggplot2)
library(ggspatial)
require(sf)
library(tmap)
require(data.table)

# If needed, import the "field_summary" plot data, created from output of Master script
if (!exists('field.summary')) {
  field.summary <- read.csv('results/field_summary.csv')
}

##IMPORT SITES DATA:
# GivenMeta_plots<-data.table::fread(file="data/GivenMeta_plots.csv",
#                        na.strings = "kA",
#                        dec = ",", 
#                        data.table = FALSE,header=TRUE)
MAP<-GivenMeta_plots[c(23,40,11,21,51,10,9,50,48,53,13,
                       34,39,35,37,17,18,25,
                       26,41,42,52,54,56),]
#Format sites information:
MAP<-MAP[,c("ID_plot","Lat","Long","Seal_500")]
MAP$Long<-as.numeric(MAP$Long)
MAP$Lat<-as.numeric(MAP$Lat)
MAP$Seal_500<-as.numeric(MAP$Seal_500)

#Produce sf object:
Sites.selected<-sf::st_as_sf(MAP, coords=c("Long","Lat"))[c(1:9),]
Sites.out<-sf::st_as_sf(MAP, coords=c("Long","Lat"))[c(10:24),]


# import geodata:

## 56 plots of the cityscape labs
total_plots <- readOGR(dsn = "data/geodata", layer ="Grassland_56plots_midpoints",
                       stringsAsFactors = F)
total_plots$Seal_500 <-as.numeric(total_plots$Seal_500)

## subset of all ant behavior locations:
ant_plots <- total_plots[
  which(total_plots$ID_plot %in%
          str_replace(field.summary$plot.simple, "-", "_")),]

explored_plots <- total_plots[
  which(total_plots$ID_plot %in%Sites.out$ID_plot),]

## Geodata info about Berlin
berlin_districts <- readOGR(dsn = "data/geodata", layer ="Districts", stringsAsFactors = F )

# correct projections
CRS.new <- proj4string(ant_plots)
proj4string(berlin_districts)

# Create map for article:
map_ants <-  tm_shape(berlin_districts) +
  tm_fill(col = "grey90") +
  tm_borders(col = "white")  +
  tm_shape(explored_plots) + tm_dots(size = 0.3,
                                   col = "darkgrey") +
  tm_shape(ant_plots) + tm_bubbles(size = 0.5,
                                     col = "Seal_500",
                                     palette ="GnBu",
                                     border.lwd = 1,
                                     title.col = "% Sealing") +
  tm_text(size = 0.4,text ="ID_plot", col = "black",
          ymod = 0.7, xmod = 0.5) +
  tm_compass(type = "8star", position = c(0.8, 0.8),
             text.size = 0.8,text.color = "grey20",
             color.dark = "grey20") +
  tm_scale_bar(breaks = c(0,5,10),color.dark = "grey20",
               text.size =0.7,text.color = "grey20",
               position = c("left","bottom")) +
  tm_layout(title = "Berlin, Germany",
            title.size = 1,
            title.color = "grey20",
            title.position = c(0, 0.95),
            legend.title.size = 0.8,
            legend.title.color = "grey20",
            legend.text.color = "grey20",
            legend.position = c(0.9, 0.45),
            frame = FALSE)

# export as pdf
pdf("figures/StudySites.pdf", height = 4, width =5)
  map_ants
dev.off()


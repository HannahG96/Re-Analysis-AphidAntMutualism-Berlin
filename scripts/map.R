setwd("C:/Hannah/Bachelorarbeit/BA Publication/Re-analysis-AphidAntMutualism-Berlin")
#########################MAKE A MAP OF STUDY SITES###########################
#LOAD PACKAGES:
library("data.table")
library("Rcpp")
library("udunits2")
library("units")
library("sf")
library("tmap")

#IMPORT SITES DATA:
GivenMeta_plots<-fread(file="data/GivenMeta_plots.csv", na.strings = "kA", dec = ",", 
                       data.table = FALSE)
MAP<-GivenMeta_plots[c(23,40,11,21,51,10,9,50,48,53,13,34,39,35,37,17,18,25,
                       26,41,42,52,54,56),]
#Format sites information:
MAP<-MAP[,c("ID_plot","Lat","Long","Seal_500")]
MAP$Long<-as.numeric(MAP$Long)
MAP$Lat<-as.numeric(MAP$Lat)
MAP$Seal_500<-as.numeric(MAP$Seal_500)
MAP$Lat.text<-MAP$Lat-0.01
#Produce sf object:
Sites.selected<-st_as_sf(MAP, coords=c("Long","Lat"))[c(1:9),]
Sites.text<-st_as_sf(MAP, coords=c("Long","Lat.text"))[c(1:9),]
Sites.out<-st_as_sf(MAP, coords=c("Long","Lat"))[c(10:24),]
#IMPORT BERLIN SHAPE FILE:
#-->https://daten.odis-berlin.de/index.html
Berlin<-st_read(dsn="data/bezirksgrenzen.geojson")

#obtain color palette that is consistent with other plots
library("scales")
show_col(viridis_pal(option="mako", begin=0, end=1, direction=-1)(13))
mypal<-c("#DEF5E5FF","#ADE3C0FF","#6CD3ADFF","#43BBADFF",
         "#35A1ABFF","#3487A6FF","#366DA0FF","#3D5296FF",
         "#403A75FF","#35264CFF","#231526FF","#211423FF",
         "#0B0405FF","#0B0405FF")
mybreaks<-c(0,5,10,15,20,25,30,35,40,45,50,55,60,65)
mylabs<-c("0","","10","","20","","30","","40","","50","","60","")

#PLOT MAP OF STUDY SITES:

#open graphical device:
#-->1.5 column width
pdf(file="figures/study_sites.pdf",         # File name
    width = 5, height = 4, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk" )   # Color model (cmyk is required for most publications)


tm_shape(Berlin) +
  #tm_polygons() +
  tm_borders("white", lwd = .5) +
  tm_fill(col = "ivory3") +
  tm_shape(Sites.out)+
  tm_symbols(size=0.4,col="gray21",shape=7)+
  tm_shape(Sites.selected)+
  tm_symbols(size = 0.5, col = "Seal_500",shape=25,
             title.col = "%Sealing",
             breaks=mybreaks,pal = mypal, label=mylabs,
             style="cont")+
  tm_layout(fontface="bold",legend.title.color="gray21",
            legend.text.color="gray21", legend.height=0.55)+
  tm_shape(Sites.text)+
  tm_text("ID_plot", size = 0.7,col="gray21")+
  tm_scale_bar(breaks = c(0, 5),
               position = c(0.15, 0.01),
               text.size = 0.6) 

# close the graphical device:
dev.off() 


#############################################################

# spatial organization 
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(sp)
library(rgdal)
library(raster)

wd0 <- getwd()

filelist <- list.files("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Data\\GISdata\\SHP\\FIREShapeFiles\\", pattern = "\\.shp$")
setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Data\\GISdata\\SHP\\FIREShapeFiles\\")

areadf <- data.frame(den = character(), fire_cat = character(), area = numeric())
for (i in 1:length(filelist)) {
  file <- readOGR(dsn = filelist[i])
  den.i <- unlist(strsplit(filelist[i], "[.]"))[1]
  if (length(file$Fire_Cat) == 5) {
    areadf.i <- data.frame(den = rep(den.i, 5), fire_cat = file$Fire_Cat, area = area(file))
  }
  else {
    areadf.i <- data.frame(den = rep(den.i, 5), fire_cat = file$Fire_Cat2, area = area(file))
  }
  areadf <- rbind(areadf, areadf.i)
}

ggplot(areadf, aes(x=fire_cat, y = area/1000000)) +
  geom_boxplot() +
  theme_ipsum(
    base_size = 12,
    axis_title_size = 14
  ) + 
  xlab("Fire category") + 
  ylab("Area (km2)") +
  ggtitle("Area by Fire Category for each den")

# spatial organization 
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(cleangeo)

# setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Data\\GISdata\\SHP\\")
# target.crs <- "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
#   
# FIRE <- list.files("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Data\\GISdata\\SHP\\FIREShapeFiles\\", pattern = "\\.shp$")
# HR <- list.files("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Data\\GISdata\\SHP\\HRShapeFiles\\", pattern = "\\.shp$")
# 
# fire.name <- unlist(lapply(FIRE, function(x) unlist(strsplit(x, "[.]"))[1]))
# fire.name <- unlist(lapply(fire.name, function(x) unlist(strsplit(x, "[_]"))[2]))
# 
# HR.name <- unlist(lapply(HR, function(x) unlist(strsplit(x, "[.]"))[1]))
# HR.name <- unlist(lapply(HR.name, function(x) unlist(strsplit(x, "[_]"))[2]))
# HR.name <- unlist(lapply(HR.name, function(x) unlist(strsplit(x, "[P]"))[2]))
# 
# # see whether the names/orders match
# sum(HR.name != fire.name) # =0. yay!
# 
# areadf <- data.frame(den = character(), fire_cat = character(), area = numeric())
# for (i in 1:length(FIRE)) {
#   fire.i <- readOGR(dsn = paste0("FIREShapeFiles\\",FIRE[i]))
#   if (is.na(crs(fire.i))) {crs(fire.i) <- target.crs}
#   else {fire.i <-spTransform(fire.i, CRS(target.crs))}
#   HR.i <- readOGR(paste0("HRShapeFiles\\",HR[i]))
#   crs(HR.i) <- target.crs
#   den.i <- unlist(strsplit(FIRE[i], "[.]"))[1]
#   den.i <- unlist(strsplit(den.i, "[_]"))[2]
#   # some files have different fire names. Rename them. 
#   if (length(fire.i$Fire_Cat2) == 5) {
#     names(fire.i)[names(fire.i) == "Fire_Cat2"] <- "Fire_Cat"
#   }
#   # seperate fire categories and clean them 
#   fire.list <- list()
#   letter.list <-  c("A", "B", "C", "D", "U")
#   for (ii in letter.list) {
#     fire.ii <- fire.i[fire.i$Fire_Cat == ii,]
#     fire.list <- c(fire.list, fire.ii)
#   }
#   fire.list.clean <- lapply(fire.list, clgeo_Clean)
#   if (!clgeo_IsValid(HR.i)) {HR.i <- clgeo_Clean(HR.i)}
#   overlap.i <- lapply(fire.list.clean, function(x) gIntersection(HR.i, x))
#   if (sum(unlist(lapply(overlap.i, is.null))) != 0) {
#     which <- which(unlist(lapply(overlap.i, is.null)))
#     whichletter <- letter.list[which]
#     overlap.i <- overlap.i[-which]
#     area.i <- unlist(lapply(overlap.i, area))
#     letter.i <- letter.list[-which]
#     letter.area <- data.frame(fire_cat = letter.i, area = area.i)
#     letter.area0 <- data.frame(fire_cat = whichletter, area = rep(0, length(whichletter)))
#     letter.area <- rbind(letter.area, letter.area0)
#   }
#   else {
#     area.i <- unlist(lapply(overlap.i, area))
#     letter.area <- data.frame(fire_cat = letter.list, area = area.i) 
#   }
#   areadf.i <- data.frame(den = rep(den.i, 5))
#   areadf.i <- cbind(areadf.i, letter.area)
#   areadf <- rbind(areadf, areadf.i)
# }
# 
# write.csv(areadf, "C:/Users/wenjing.xu/Google Drive/RESEARCH/Panther/Panther-Revive-2019/fire_area_by_HR.csv")
areadf <- read.csv("C:/Users/wenjing.xu/Google Drive/RESEARCH/Panther/Panther-Revive-2019/Analysis/FLPanther/fire_area_by_HR.csv")
areadf.1 <- areadf %>% dplyr::select(-X, -area) %>% pivot_wider(names_from = fire_cat, values_from = area.km2)
areadf.2 <- areadf.1 %>% mutate(total = A + B + C + D + U，
                    percent.A = round(A/total*100, digits = 2),
                    percent.B = round(B/total*100, digits = 2),
                    percent.C = round(C/total*100, digits = 2),
                    percent.D = round(D/total*100, digits = 2),
                    percent.U = round(U/total*100, digits = 2)) 
write.csv(areadf.2, "C:/Users/wenjing.xu/Google Drive/RESEARCH/Panther/Panther-Revive-2019/Analysis/FLPanther/SuppTable1.csv")

areadf.3 <- areadf.2 %>% dplyr::select(-A, -B, -C, -D, -U, -total)
names(areadf.3)[2:6] <- c("A", "B", "C", "D", "U")
areadf.3 <- areadf.3 %>% pivot_longer(2:6)
ggplot(areadf.3, aes(x=name, y = value)) +
  geom_boxplot() +
  theme_ipsum(
    base_size = 12,
    axis_title_size = 14
  ) + 
  xlab("Fire category") + 
  ylab("Area (km2)") +
  ggtitle("Area by Fire Category within Home Range Relative to Each Den")

areadf.1 <- areadf %>% select(-X, -area) %>% group_by(den) %>% pivot_wider(names_from = fire_cat, values_from = area.km2) %>% mutate(non_fire = A + B + C + D)

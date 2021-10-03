# this document is to generate random point for each HR
# and to calculate the size of each fire polygon that each point is in

library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(spatialEco)
library(sf)
library(tidyverse)
options(scipen = 999)
set.seed(7)

pts <- read_csv( "G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Analysis\\JWM_resubmission\\pts_all.csv") 

HR <- st_read("G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Analysis\\JWM_resubmission\\HR_goodID.shp")
st_crs(HR) <- 26917
HR$area <- st_area(HR)

FIRE.path = "G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Data\\SpatialData\\SHP\\FIRE_Multi"
FIRE.names <- ogrListLayers(FIRE.path)

############ generate random points, extract fire cat, extrat HR area of the den, extract area of the burn unit #################
firecat.dataframe <- data.frame(NULL)
area.dataframe <- data.frame(NULL)

for(i in 1:nrow(HR)) {
  #get den name
  HR.i <- HR[i,] 
  HR.area <- HR.i$area
  name <- str_sub(HR.i$Den_ID, start = 3, end = str_length(HR.i$Den_ID))
  if ( str_sub(name, start = 1, end = 1) == 0) {
    name <- str_sub(name, start = 2, end = str_length(name))
  }
  #open corresponding fire layer
  FIRE.layer.i <- paste0("FIRE_", name)
  FIRE.i <- readOGR(FIRE.path, layer = FIRE.layer.i)
  if(is.null(FIRE.i)) {stop("double check fire name")}
  
  #get fire cat. Note some files name columes differently
  if (!"Fire_Cat" %in% names(FIRE.i)) {
    names(FIRE.i)[which(grepl("Fire", names(FIRE.i), fixed = T))] <- "Fire_Cat"
  }
  
  # calculate proportion of each fire cat within the HR
  FIRE.i <- FIRE.i %>% st_as_sf(.)
  if(is.na(st_crs(FIRE.i))) {st_crs(FIRE.i) <- 26917} else {
    FIRE.i <- st_transform(FIRE.i, 26917)
  }
  
  area.i <- HR.i %>% st_intersection(FIRE.i) %>% 
    mutate(area = st_area(.)) %>% group_by(Fire_Cat) %>%
    summarise(area = sum(area))
  # change area into df
  st_geometry(area.i) <- NULL
  # sometimes HR goes outside of fire map. set unknown area.
  if (sum(area.i$area) < HR.area) {
    a <- data.frame(Fire_Cat = "unknown", area = (HR.area - sum(area.i$area)))
    area.i <- rbind(area.i, a)
  }
  
  area.i <- area.i %>% pivot_wider(names_from = Fire_Cat, values_from = area) %>% 
     mutate (Den_ID = HR.i$Den_ID, HR.area = HR.area)
  
  area.dataframe <- plyr::rbind.fill(area.i, area.dataframe)
  
  ## extract fire cat for each point
  #get points
  rdp.i <- pts %>% filter(Den_ID == HR.i$Den_ID) 
  rdp.i <- SpatialPointsDataFrame(coords = cbind(rdp.i$UTM_E, rdp.i$UTM_N), data=rdp.i) %>% st_as_sf(.)
  st_crs(rdp.i) <- 26917
  # extract values to points
  pts.poly.i <- rdp.i %>% st_join(FIRE.i) %>% select(Den_ID, UTM_E, UTM_N, used, Fire_Cat)
  pts.poly.i[is.na(pts.poly.i$Fire_Cat), ]$Fire_Cat <- "unknown" 
  #organize into dataframes 
  st_geometry( pts.poly.i ) <- NULL
  firecat.dataframe <- rbind(pts.poly.i, firecat.dataframe)
}

#write.csv(area.dataframe, "G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Analysis\\JWM_resubmission\\HR_area_byfirecat.csv")
#write.csv(firecat.dataframe, "G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Analysis\\JWM_resubmission\\allpts_w_firecat.csv")

# 
# ################### last, organize the all point frame #######################
# all_pts <- read.csv("C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/AllPts_firecat.csv")
# all_pts$ID <- as.character(all_pts$ID)
# x <- strsplit(all_pts$ID, "_") # split ID based on "_"
# v1 <- c() # store first split object
# v2 <- c() # store second split object
# for (i in 1:length(x)) {
#   v1 <- c(v1, x[[i]][1])
#   v2 <- c(v2, x[[i]][2])
# }
# all_pts$CatID <- v1 # add back into all_pts
# all_pts$PointID <- v2
# all_pts <- all_pts[,c(7,8,1,2,3,4,5,6)] # change column order
# colnames(all_pts)[3] <- "UniqueID" # change ID to UniqueID
# 
# write.csv(all_pts, "C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/AllPts_firecat_Updated.csv")

#HR reconstruction
library(adehabitatHR)
library(tidyverse)
library(lubridate)
library(sp)
library(sf)

setwd("/Volumes/GoogleDrive/My Drive/RESEARCH/Panther/Panther-Revive-2019/Analysis")

telem <- read_csv("./panther_Telem_2000_14.csv") %>%
  mutate(CATNUMBER == as.numeric(CATNUMBER)) %>%
  filter(!is.na(CATNUMBER))
dens <- read_csv("./alldens.csv") 
length(unique(dens$Den_ID)) #83

#################### simulate HR as a simple buffer based on averaae 3-mo HR size ########################
r = sqrt(77.4*1000*1000/pi)  # the mean HR is 77.4km2 based on animals with at least 30 points in 90 days.

dens.sf <- st_as_sf(dens, coords = c("UTM_E", "UTM_N"), crs = 26917)
dens.HR <- st_buffer(dens.sf, r)

# save HR to put into ArcGIS for random points generation 
#st_write(dens.HR, "./JWM_resubmission/simple-buffer-HR.shp")

### create random points in HR ##################\
# https://github.com/r-spatial/sf/issues/813
get_random_pt <- function(x, size) {
  random_pt <- st_sample(x , size = size, type = "random")
  while (length(random_pt) < size) {
    random_pt <- c(random_pt, st_sample(x , size = 1, type = "random"))
  }
  return(random_pt)
}

st_sample_exact <- function(x, size) {
  random_pt <- st_sample(x , size = size, type = "random")
  while (length(random_pt) < size) {
    diff <- size - length(random_pt)
    random_pt_new <- st_sample(x , size = diff, type = "random")
    random_pt <- c(random_pt, random_pt_new)
  }
  random_pt
}

rd.pts  <- data.frame()
for (i in 1:nrow(dens.HR)) {
  HR.i <- dens.HR[i,] %>% st_sf()
  Den_ID.i <- HR.i$Den_ID

  set.seed(99)
  rd.pts.i <- HR.i  %>%
    st_sample_exact(.,120) %>%   #create 120 so that even if delete the ones that fall in water each den still get at least 100 random points
    st_sf() %>%
    mutate(Den_ID = Den_ID.i)

  rd.pts <- rbind(rd.pts, rd.pts.i)
}
#st_write(rd.pts, "./JWM_resubmission/MIDPRODUCT/rd_pts_r.shp")

den_pt <- st_read("./JWM_resubmission/den_pt.shp") %>% dplyr::select(Cat_ID) %>% rename( Den_ID = Cat_ID) %>% mutate (used = 1)
all_pts <- rd.pts %>% mutate(used = 0) %>% rbind(., den_pt)
#st_write(all_pts,  "./JWM_resubmission/all_pts_r.shp")

## get geometry of den points and save all points dataframe ##############
all_pts <- st_read("./JWM_resubmission/all_pts_r.shp")
all_pts_df <- all_pts %>% st_set_geometry(NULL)
#write_csv(all_pts_df, "./JWM_resubmission/all_pts_r.csv")

############# EXTRACT FIRE ###############################################
FIRE.path = "/Volumes/GoogleDrive/My Drive/RESEARCH/Panther/Panther-Revive-2019/Data/GISdata/SHP/FIREShapeFiles"
FIRE.names <- list.files(FIRE.path, pattern = ".shp$")

Names <- unique(all_pts$Den_ID)

firecat.dataframe <- data.frame(NULL)
area.dataframe <- data.frame(NULL)

for (i in Names) {
  #get den name
  pts.i <- all_pts %>% filter(Den_ID == i)
  HR.i <- dens.HR %>% dplyr::select(Den_ID) %>% filter(Den_ID == i)
  
  name <- str_sub(i, start = 3, end = str_length(i))
  if ( str_sub(name, start = 1, end = 1) == 0) {
    name <- str_sub(name, start = 2, end = str_length(name))
  }
  #open corresponding fire layer
  FIRE.layer.i <- paste0("FIRE_", name)
  FIRE.i <- readOGR(FIRE.path, layer = FIRE.layer.i)
  if(is.null(FIRE.i)) {stop("double check fire name")}
  
  #Note some files name columes differently
  if (!"Fire_Cat" %in% names(FIRE.i)) {
    names(FIRE.i)[which(grepl("Fire", names(FIRE.i), fixed = T))] <- "Fire_Cat"
  }

  # change to sf
  FIRE.i <- FIRE.i %>% st_as_sf(.) %>% st_make_valid(.)
  if(is.na(st_crs(FIRE.i))) {st_crs(FIRE.i) <- 26917} else {
    FIRE.i <- st_transform(FIRE.i, 26917)
  }
  
  #####################################################
  # calculate proportion of each fire cat within the HR
  area.i <- HR.i %>% st_intersection(FIRE.i) %>% 
    mutate(area = st_area(.)) %>% group_by(Fire_Cat) %>%
    summarise(area = sum(area))
  # change area into df
  st_geometry(area.i) <- NULL
  # sometimes HR goes outside of fire map. add to U.
  if (sum(area.i$area) < st_area(HR.i)) {
    area.i[which(area.i$Fire_Cat == "U"),]$area <- area.i[which(area.i$Fire_Cat == "U"),]$area  + (st_area(HR.i) - sum(area.i$area))
  }
  area.i <- area.i %>% pivot_wider(names_from = Fire_Cat, values_from = area) %>% 
    mutate (Den_ID = HR.i$Den_ID, HR.area = st_area(HR.i))
  
  area.dataframe <- plyr::rbind.fill(area.i, area.dataframe)
  
  ##########################
  # extract values to points
  st_crs(pts.i) <- 26917
  pts.poly.i <- pts.i %>% st_join(FIRE.i) %>% dplyr::select(Den_ID, Fire_Cat, used)
  if(sum(is.na(pts.poly.i$Fire_Cat)) != 0) {
    pts.poly.i[is.na(pts.poly.i$Fire_Cat), ]$Fire_Cat <- "U" 
  }
  #organize into dataframes 
  pts.poly.i <- pts.poly.i %>%
    mutate(X = unlist(map( pts.poly.i$geometry,1)),
           Y = unlist(map( pts.poly.i$geometry,2)))
  st_geometry( pts.poly.i ) <- NULL
  firecat.dataframe <- rbind(pts.poly.i, firecat.dataframe)
}
area.dataframe[is.na(area.dataframe)] = 0

write.csv(area.dataframe, "./JWM_resubmission/HR_area_byfirecat.csv")
write.csv(firecat.dataframe, "./JWM_resubmission/allpts_w_firecat.csv")

firecat.st <- st_as_sf(firecat.dataframe, coords = c("X", "Y"), crs = 26917) 
st_write(firecat.st, "./JWM_resubmission/MIDPRODUCT/allpts_w_firecat_pre.shp")
# add CLC state AND ave_wet in ArcGIS before uploading to GEE

firecat.st
library(raster)
raster("/Volumes/GoogleDrive/My Drive/RESEARCH/Panther/Panther-Revive-2019/Data/GISdata/WaterDepth/WaterDepth_Ave_Dry.tif")

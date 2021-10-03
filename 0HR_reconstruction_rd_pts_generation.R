#HR reconstruction
library(adehabitatHR)
library(tidyverse)
library(lubridate)
library(sp)
library(sf)

telem <- read_csv("G:/My Drive/RESEARCH/Panther/Panther-Revive-2019/Analysis/panther_Telem_2000_14.csv") %>%
  mutate(CATNUMBER == as.numeric(CATNUMBER)) %>%
  filter(!is.na(CATNUMBER))
dens <- read_csv("G:/My Drive/RESEARCH/Panther/Panther-Revive-2019/Analysis/alldens.csv") 
unique(dens$Den_ID)
unique(telem$CATNUMBER)

# #################### calculate number of telemetry locations ########################
# get.n <- function(den, d) {
#   den.time <- mdy((dens %>% filter(Den_ID == den))$Den_Est__BirthDate)
#   telem.d <- telem %>% filter(CATNUMBER == as.numeric(str_sub(den, start = 3, end = 5))) %>%
#     mutate(FLGTDATE = mdy(FLGTDATE),
#            DIFFDATE = FLGTDATE-den.time) %>% 
#     filter(DIFFDATE >= 0 & DIFFDATE <= d) 
#   return (data.frame(n = nrow(telem.d), date.range = max(telem.d$DIFFDATE)))
# }
# 
# n = data.frame(NULL)
# for (i in unique(dens$Den_ID)){
#   n.i <- get.n(i, 90)
#   n <- rbind(n, data.frame(ID = i, d = 90, n = n.i))
# }
# 
# for (i in unique(dens$Den_ID)){
#   n.i <- get.n(i, 365)
#   n <- rbind(n, data.frame(ID = i, d = 365, n = n.i))
# }
# n.1 <- n %>% select(-n.n) %>% pivot_wider(names_from = d, values_from = n.date.range)
# n.2 <- n %>% select(-n.date.range) %>% pivot_wider(names_from = d, values_from = n.n)
# 
# colnames(n.1) <- c("ID", "range_90","range_365")
# colnames(n.2) <- c("ID", "n_90", "n_365")
# n <- n.1 %>% left_join(n.2)
# View(n)
# 
# good.ID <- (n %>% filter(n_90 >= 30,
#                         !(ID %in% c("FP061", "FP094", "FP095A", "FP095B"))))$ID  #these dens are in everglades
# write.csv(data.frame(good.ID),"G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Submission\\JWMrevision\\good_den.csv")


#################### calculate HR for remaining dens ########################

good.ID <- (read_csv("G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Submission\\JWMrevision\\good_den.csv"))$good.ID
get.d.HR <- function (den, d1, d2) {
  options(warn = -1)
  den.time <- mdy((dens %>% filter(Den_ID == den))$Den_Est__BirthDate)
  
  telem.d1 <- telem %>% filter(CATNUMBER == as.numeric(str_sub(den, start = 3, end = 5))) %>%
    mutate(FLGTDATE = mdy(FLGTDATE),
           DIFFDATE = FLGTDATE-den.time,
           d = d1) %>% 
    filter(DIFFDATE >= 0 & DIFFDATE <= d1) %>%
    select(UTM83EAST, UTM83NORTH, d)
  telem.d2 <- telem %>% filter(CATNUMBER == as.numeric(str_sub(den, start = 3, end = 5))) %>%
    mutate(FLGTDATE = mdy(FLGTDATE),
           DIFFDATE = FLGTDATE-den.time,
           d = d2) %>% 
    filter(DIFFDATE >= 0 & DIFFDATE <= d2) %>%
    select(UTM83EAST, UTM83NORTH, d)
  
  if ((nrow(telem.d1) == 0) | (nrow(telem.d2) == 0)) {
    return(NULL)
  } else {
    telem.d <- rbind(telem.d1, telem.d2)
    
    coordinates(telem.d) <- c('UTM83EAST', 'UTM83NORTH')
    proj4string(telem.d) <- CRS('+init=epsg:26917')
    
    HR <- kernelUD(telem.d, grid = 60)
    HR <- getverticeshr(HR, 93)
    
    return(st_as_sf(HR))
  }
  options(warm = defaultW)
}

HR = data.frame(NULL)
for (i in good.ID){
  HR.i <- get.d.HR(i, 90, 365)
  if (is.null(HR.i)) {next}
  HR <- rbind(HR, HR.i %>% mutate(Den_ID = i) %>% rename (d = id) )
}

#HR %>% filter(d == 90) %>% write_sf(., "G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Analysis\\JWM_resubmission\\HR_goodID.shp")

#### create random points in HR 

HR <- read_sf("G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Analysis\\JWM_resubmission\\HR_goodID.shp")

HR.90 <- HR %>% filter(d == 90) 
set.seed(99)
rd.pts <- HR.90 %>% st_sample(.,c(200, nrow(HR.90)), exact = T) %>%
  st_sf() %>%
  st_join(HR.90)

# sampling is not always the exact number for some reason so we are doing this 
rd.pts2 <- HR.90 %>% filter(Den_ID %in% c("FP088", "FP103")) %>% st_sample(.,c(200, nrow(HR.90)), exact = T) %>%
  st_sf() %>%
  st_join(HR.90)

rd.pts3 <- HR.90 %>% filter(Den_ID %in% c("FP103")) %>% st_sample(.,c(200, nrow(HR.90)), exact = T) %>%
  st_sf() %>%
  st_join(HR.90)

rd <- rbind(rd.pts %>% filter(!Den_ID %in% c("FP088", "FP103")),
     rd.pts2 %>% filter(!Den_ID %in% c("FP103")),
     rd.pts3)

set.seed(1)
rd <- rd %>% group_by(Den_ID) %>% slice_sample(n = 150)

# st_write(rd, "G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Analysis\\JWM_resubmission\\rd_pts_all.shp")

st_crs(rd) <- 26917
rd$UTM_E = sf::st_coordinates(rd)[,1]
rd$UTM_N = sf::st_coordinates(rd)[,2]
st_geometry(rd) <- NULL

good.ID <- (read_csv("G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Submission\\JWMrevision\\good_den.csv"))$good.ID
all_pts <- rbind(dens %>% select(Den_ID, UTM_E, UTM_N) %>% filter(Den_ID %in% good.ID) %>% mutate (used = 1), rd %>% select(Den_ID, UTM_E, UTM_N) %>% mutate(used = 0))

plot(all_pts$UTM_E, all_pts$UTM_N)

write_csv(all_pts, "G:\\My Drive\\RESEARCH\\Panther\\Panther-Revive-2019\\Analysis\\JWM_resubmission\\pts_all.csv")



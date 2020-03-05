# this document is to generate random point for each HR
# and to calculate the size of each fire polygon that each point is in

library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(spatialEco)
options(scipen = 999)
set.seed(7)

RD.path <- setwd("C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Data/SpatialData/SHP/RandomPts/")

HR.path = "C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Data/SpatialData/SHP/HRShapeFiles"
HR.names <- ogrListLayers(HR.path)

FIRE.path = "C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Data/SpatialData/SHP/FIRE_Multi"
FIRE.names <- ogrListLayers(FIRE.path)

proj <- crs(as.character("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))

############ generate random points, extract fire cat, extrat HR area of the den, extract area of the burn unit #################
dataframe <- data.frame( "ID" = character(), X = numeric(), Y = numeric (), Fire_Cat = character(), HR_Area = numeric(), Fire_Area = numeric())

for(i in 1:length(ogrListLayers(HR.path))) {
  #read names and open corresponding files
  HR.layer.i <- unlist(strsplit(HR.names[i], ".", fixed = TRUE))[1] 
  name <- substr(as.character(HR.layer.i), 6, nchar(HR.layer.i))
  FIRE.layer.i <- paste0("FIRE_", name)
  HR.i <- readOGR(HR.path, layer = HR.layer.i)
  HR.area <- area(HR.i)
  FIRE.i <- readOGR(FIRE.path, layer = FIRE.layer.i)
  #some files name columes differently
  if (names(FIRE.i)[1] != "Fire_Cat") {
    names(FIRE.i)[1] <- "Fire_Cat"
  }
  #calculate area for the fire polygons, which will be later extracted with overlapping with points
  FIRE.i$Area <- area(FIRE.i)
  #generating random points
  rdp.i <- spsample(HR.i,n=100, type="random")### we can change how many points to choose by changing here
  # reproejct layers 
  rdp.i <- spTransform(rdp.i, proj)
  if (st_crs(FIRE.i) != st_crs(rdp.i)) {
    if (is.na(st_crs(FIRE.i))) {
      projection(FIRE.i) <- proj 
    }
    else FIRE.i <- spTransform(FIRE.i, proj)
  }
  # extract values to points
  pts.poly.i <- point.in.poly(rdp.i, FIRE.i)
  #organize into dataframes 
  dataframe.i <- cbind("ID" = paste0(name, "_", pts.poly.i$pt.ids), coordinates(pts.poly.i), "Fire_Cat" = as.character(pts.poly.i$Fire_Cat), "HR_Area" = rep(HR.area,100), "Fire_Area" = pts.poly.i$Area)
  dataframe <- rbind(dataframe, dataframe.i)
}


#write.csv(dataframe, "C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/randompts_firecat.csv")

################# making same dataframe for den ########################
dataframe <- data.frame( "ID" = character(), X = numeric(), Y = numeric (), Fire_Cat = character(), HR_Area = numeric(), Fire_Area = numeric())

den <- data.frame(read.csv("C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Data/DataSheets/2019/Den_Final.csv"))
coords <- data.frame(as.numeric(den$UTM_E), as.numeric(den$UTM_N))
den.sp <- SpatialPointsDataFrame(coords, den, proj4string = proj)
#den.sp <- SpatialPointsDataFrame(coords, den)

for(i in 1:nrow(den.sp)) {
  name <- substr(as.character(den.sp$Cat_ID[i]), 3, nchar(as.character(den.sp$Cat_ID[i])))
  if (startsWith(name, "0")) {
    name <- substr(name, 2, nchar(name))
    }
  HR.layer.i <- paste0("HR_FP", name)
  FIRE.layer.i <- paste0("FIRE_", name)
  HR.i <- readOGR(HR.path, layer = HR.layer.i)
  HR.area <- area(HR.i)
  FIRE.i <- readOGR(FIRE.path, layer = FIRE.layer.i)
  if (names(FIRE.i)[1] != "Fire_Cat") {
    names(FIRE.i)[1] <- "Fire_Cat"
  }
  FIRE.i$Area <- area(FIRE.i)
  dp.i <- den.sp[den.sp$Cat_ID == den.sp$Cat_ID[i],]
  if (st_crs(FIRE.i) != st_crs(dp.i)) {
    if (is.na(st_crs(FIRE.i))) {
      projection(FIRE.i) <- proj 
    }
    else FIRE.i <- spTransform(FIRE.i, proj)
  }
  pts.poly.i <- point.in.poly(dp.i, FIRE.i)
  dataframe.i <- cbind("ID" = paste0(name, "_0"), coordinates(pts.poly.i), "Fire_Cat" = as.character(pts.poly.i$Fire_Cat), "HR_Area" = rep(HR.area,100), "Fire_Area" = pts.poly.i$Area)
  dataframe <- rbind(dataframe, dataframe.i)
}

write.csv(dataframe, "C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/den_firecat.csv")

## the output still has some mistakes. Used ArcGIS for double check and updated to den_firecat_updated.csv.

################### last, organize the all point frame #######################
all_pts <- read.csv("C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/AllPts_firecat.csv")
all_pts$ID <- as.character(all_pts$ID)
x <- strsplit(all_pts$ID, "_") # split ID based on "_"
v1 <- c() # store first split object
v2 <- c() # store second split object
for (i in 1:length(x)) {
  v1 <- c(v1, x[[i]][1])
  v2 <- c(v2, x[[i]][2])
}
all_pts$CatID <- v1 # add back into all_pts
all_pts$PointID <- v2
all_pts <- all_pts[,c(7,8,1,2,3,4,5,6)] # change column order
colnames(all_pts)[3] <- "UniqueID" # change ID to UniqueID

write.csv(all_pts, "C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/AllPts_firecat_Updated.csv")

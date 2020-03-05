library(dplyr)

ee_gis <- read.csv("C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/ExtractioInProcess/ALLPTS_Arc_EE_organized.csv")
gis <- read.csv("C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/ExtractioInProcess/All_Pts_w_GIS_organized.csv")

gis$CatID <- paste0("AA_", gis$CatID)
gis$UniqueID <- paste0("AA_", gis$UniqueID)
write.csv(gis, "C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/ExtractioInProcess/All_Pts_w_GIS_organized2.csv")

#ee_gis1 <- ee_gis 
ee_gis <- ee_gis1

#ee_gis <- as_tibble(ee_gis)
ee_gis <- dplyr::select(ee_gis, PointID, Fire_Cat, Ave_Dry, Ave_Wet, CLC_SITE, CLC_STATE, Prec_Tree, dist2build)

full_join(gis, ee_gis, by = c("PointID", "Fire_Cat", "Ave_Dry", "Ave_Wet" ))

## ee_gis_new <- merge (gis, ee_gis, by = c("Ave_Dry", "Ave_Wet", "PointID", "Fire_Cat"))

ee_gis_new <- dplyr::select(ee_gis_new, CatID, PointID, UniqueID, coords_x1, coords_x2, Fire_Cat, Fire_Area, HR_Area, Ave_Dry, Ave_Wet, CLC_SITE.x, CLC_STATE.x, Prec_Tree, dist2build)

write.csv(ee_gis_new, "C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/ALLPTS_FINAL.csv")

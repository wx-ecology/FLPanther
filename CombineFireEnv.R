## this script is to combine fire cat from previous analysis with newly extracted environmental variables
#inout: Data/DataSheets/2019, Den_Env_Extraction_eeRaw; Den_Dist2Fire;

library(dplyr)

env_sheet <- read.csv(file.choose())
fire_cat <- read.csv(file.choose())

env_sheet <- select(env_sheet, -X)
env_sheet$Den_ID <- substr(env_sheet$Cat_ID, 3, 6)

env_sheet$Den_ID[which(startsWith(env_sheet$Den_ID, '0'))] <- substr(env_sheet$Den_ID[which(startsWith(env_sheet$Den_ID, '0'))], 2,5)

Fire <- function(row) {
  colnum <- which(row == 0)
  return(colnames(row[colnum]))
}

fire_cat$Fire_Cat <- NA

for (i in 1:nrow(fire_cat)) {
  Fire_Cat_i <- Fire(fire_cat[i,])
  fire_cat[i,]$Fire_Cat <- Fire_Cat_i
}

fire_cat <- select(fire_cat, one_of(c("DEN", "Fire_Cat")))
colnames(fire_cat) <- c("Den_ID", "Fire_Cat")

env_sheet <- left_join(env_sheet, fire_cat, by = "Den_ID")

write.csv(env_sheet, "C:/Users/wenjing.xu/Google Drive/STUDENTS/SPUR_Fall2019/SPUR-Panther-Fall2019/Analysis/env_sheet.csv")

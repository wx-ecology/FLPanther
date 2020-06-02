# spatial organization 
library(dplyr)
library(tidyr)

dens <- read.csv("alldens.csv")
dens$Den_Year <- as.POSIXct(strptime(as.character(dens$Den_Est__BirthDate), format = "%m/%d/%Y"))
dens$Den_Year <- format(dens$Den_Year, "%Y")

names(dens)[2] <- "Den_ID"
dens.1 <- dens %>%
  separate(Den_ID, 
           into = c("text1", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  separate(num, 
            into = c("num", "text2"),
            sep = "(?<=[0-9])(?=[A-Z])")


dens.1 <- dens.1 %>% mutate(Cat_ID = paste0(text1, num))
dens.2 <- dens %>% select(Den_ID, Den_WorkupDate, Den_Est__BirthDate, Est_Kitten.Age_Days, Location, UTM_E, UTM_N, Den_Year)
dens.2 <- cbind((dens.1 %>% select(Cat_ID)), dens.2)


write.csv(dens.2, "alldens.csv")



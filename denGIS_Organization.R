# spatial organization 
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)


## calculate used dens statistics
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

den_vari <- read.csv("Final_Variable_all.csv")
den_vari %>% filter(Used == 1) %>% group_by(Fire_Cat) %>% summarise( Count = n())

## calculalte raw den statistics
rawdens <- read.csv("FWC_Panther_Den_Dbase_2014.csv")
rawdens$Den_ID <- rawdens$Panther.ID

rawdens.1 <- rawdens %>%
  separate(Panther.ID, 
           into = c("text1", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  separate(num, 
           into = c("num", "text2"),
           sep = "(?<=[0-9])(?=[A-Z])") %>%
  mutate(Cat_ID = paste0(text1, num))
rawdens.2 <- cbind(Cat_ID = rawdens.1$Cat_ID, Den_ID= rawdens$Den_ID, rawdens[,2:9])
rawdens.2 <- rawdens.2 %>% separate(Den.Date, into = c("Month", 'Year'), sep = "-|s")
rawdens.2$Month <- factor(rawdens.2$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

rawdens.2 %>%
  ggplot(aes(x = Month)) +
  geom_histogram(stat = "count", fill="grey75", color="grey75", alpha=0.9) +
  ggtitle("Number of dens by month") +
  theme_ipsum(base_size = 14, axis_title_size = 14) +
  theme(
    plot.title = element_text(size=15)
  ) + 
  ylab("Count")

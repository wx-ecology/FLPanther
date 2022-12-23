# spatial organization 
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)


## calculate used dens statistics
dens <- read.csv("alldens.csv") %>% select(-X)

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
rawdens.2 <- rawdens.2 %>% rename(UTM_E = UTM.E.Nad83., UTM_N = UTM.N.Nad83.) %>% select(Month, Year, UTM_E, UTM_N)

dens <- dens %>% left_join(rawdens.2, by = c("UTM_E", "UTM_N") ) 
# mannually add month 
dens[dens$Den_ID == "FP195Z",]$Month = "Jan"
dens[dens$Den_ID == "FP191Y",]$Month = "Jun"
dens[dens$Den_ID == "FP191Z",]$Month = "Mar"
  
a <- dens %>% group_by(Month) %>% summarise(n = n())
a <- rbind(a, data.frame(Month = "Oct", n = 0))
a$Month <- factor(a$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
a %>%
  ggplot(aes(x = Month, y = n)) +
  geom_col(fill="grey75", color="grey75", alpha=0.9) +
  ggtitle("Number of dens by month") +
  theme_ipsum(base_size = 14, axis_title_size = 14) +
  theme(
    plot.title = element_text(size=15)
  ) + 
  ylab("Count")

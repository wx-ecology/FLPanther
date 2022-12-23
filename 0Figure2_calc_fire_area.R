# spatial organization 
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(sp)
library(rgdal)
library(raster)
library(rgeos)

#areadf <- read.csv("./fire_area_by_HR.csv")
areadf <- read.csv("./JWM_resubmission/HR_area_byfirecat.csv")
#areadf.1 <- areadf %>% dplyr::select(-X, -area) %>% pivot_wider(names_from = fire_cat, values_from = area.km2)
areadf.2 <- areadf %>% mutate(
                    percent.A = round(A/HR.area*100, digits = 2),
                    percent.B = round(B/HR.area*100, digits = 2),
                    percent.C = round(C/HR.area*100, digits = 2),
                    percent.D = round(D/HR.area*100, digits = 2),
                    percent.U = round(U/HR.area*100, digits = 2)) 
  #write.csv(areadf.2, "./JWM_resubmission/SuppTable1.csv")

areadf.3 <- areadf.2 %>% dplyr::select(-X, -A, -B, -C, -D, -U, -HR.area)
names(areadf.3)[2:6] <- c("A", "B", "C", "D", "U")
areadf.3 <- areadf.3 %>% pivot_longer(2:6)
ggplot(areadf.3, aes(x=name, y = value)) +
  geom_boxplot() +
  theme_ipsum(
    base_size = 12,
    axis_title_size = 14
  ) + 
  xlab("Fire category") + 
  ylab("%HR") 

data <- read_csv("./JWM_resubmission/allpts_Extraction_2022.csv") %>% dplyr:: select(-'system:index', -.geo) %>% rename (Perc_Tree = Prec_Tree, Used = used )
data <- data %>% filter(Used == 1) %>% group_by(Fire_Cat) %>% summarise(n = n())

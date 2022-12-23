library(survival)
library(tidyverse)
library(ggplot2)
library(viridis)
library(GGally)
library(MuMIn)
library(AICcmodavg)
library(hrbrthemes)
library(scales)

setwd("/Volumes/GoogleDrive/My Drive/RESEARCH/Panther/Panther-Revive-2019/Analysis/WILDLIFE-RESEARCH")

############################################
## ----- set up (100 random points) ------ ##
############################################

## read data 
data <- read_csv("./allpts_Extraction_2022.csv") %>% dplyr:: select(-'system:index', -.geo) %>% rename (Perc_Tree = Prec_Tree, Used = used )
data <- data %>% filter(!CLC_STATE %in% c(3200,4200, 5000, 5250)) # get rid of random points that fall in water
set.seed(78)
data.0 <- data %>% filter(Used == 0) %>% group_by(Den_ID) %>% sample_n(100)  # now every den has 100 corresponding random points
data <- rbind((data %>% filter(Used == 1)), data.0)
# double check whether all den has 101 records. Should return an empty list
data %>% group_by(Den_ID) %>% summarise(n = n()) %>% filter(n != 101)

## reorganize landcover classes by regrouping CLC classes
veg.code <- read.csv("VegCombineCode.csv")
# reshape data
data <- data  %>% dplyr::left_join(veg.code, by = "CLC_STATE") %>%
  mutate(Den_ID = as.factor(Den_ID),
         Fire_Cat = factor(data$Fire_Cat, levels = c("U", "A", "B", "C", "D")),
         Land_Class = ifelse(is.na(Combined), "Other", Combined)) %>% 
  dplyr::select(Used, Den_ID, Fire_Cat, Ave_Wet, Perc_Tree, Land_Class)

## specify factors. Levels order will affect the baseline level in the modeling process
data$Land_Class <- factor(data$Land_Class, levels = c("Upland Forest", "Other", "Marsh-Shrub-Swamp", "Prairie-Grassland", "Wetland Forest"))
data$Land_Class <- relevel(data$Land_Class, ref = 2 )

## finalize dataframe 
data <- data %>% dplyr::arrange (Den_ID) %>%
  mutate(Perc_Tree_raw = Perc_Tree,
         Ave_Wet = scale(Ave_Wet),
         Perc_Tree = scale(Perc_Tree))

## ----- model ------ ##
model <- clogit(Used ~ Fire_Cat + Perc_Tree + Land_Class + Ave_Wet + Perc_Tree*Land_Class + strata(Den_ID), data = data, method='efron')
summary(model)


############################################
## ----- set up (50 random points) ------ ##
############################################
## read data 
data <- read_csv("./allpts_Extraction_2022.csv") %>% dplyr:: select(-'system:index', -.geo) %>% rename (Perc_Tree = Prec_Tree, Used = used )
data <- data %>% filter(!CLC_STATE %in% c(3200,4200, 5000, 5250)) # get rid of random points that fall in water
set.seed(78)
data.0 <- data %>% filter(Used == 0) %>% group_by(Den_ID) %>% sample_n(50)  # now every den has 100 corresponding random points
data <- rbind((data %>% filter(Used == 1)), data.0)
# double check whether all den has 101 records. Should return an empty list
data %>% group_by(Den_ID) %>% summarise(n = n()) %>% filter(n != 51)

## reorganize landcover classes by regrouping CLC classes
veg.code <- read.csv("VegCombineCode.csv")
# reshape data
data <- data  %>% dplyr::left_join(veg.code, by = "CLC_STATE") %>%
  mutate(Den_ID = as.factor(Den_ID),
         Fire_Cat = factor(data$Fire_Cat, levels = c("U", "A", "B", "C", "D")),
         Land_Class = ifelse(is.na(Combined), "Other", Combined)) %>% 
  dplyr::select(Used, Den_ID, Fire_Cat, Ave_Wet, Perc_Tree, Land_Class)

## specify factors. Levels order will affect the baseline level in the modeling process
data$Land_Class <- factor(data$Land_Class, levels = c("Upland Forest", "Other", "Marsh-Shrub-Swamp", "Prairie-Grassland", "Wetland Forest"))
data$Land_Class <- relevel(data$Land_Class, ref = 2)

## finalize dataframe 
data <- data %>% dplyr::arrange (Den_ID) %>%
  mutate(Perc_Tree_raw = Perc_Tree,
         Ave_Wet = scale(Ave_Wet),
         Perc_Tree = scale(Perc_Tree))

## ----- model  ------ ##
model_50 <- clogit(Used ~ Fire_Cat + Perc_Tree + Land_Class + Ave_Wet + Perc_Tree*Land_Class + strata(Den_ID), data = data, method='efron')
summary(model_50)


############################################
## ----- set up (10 random points) ------ ##
############################################
## read data 
data <- read_csv("./allpts_Extraction_2022.csv") %>% dplyr:: select(-'system:index', -.geo) %>% rename (Perc_Tree = Prec_Tree, Used = used )
data <- data %>% filter(!CLC_STATE %in% c(3200,4200, 5000, 5250)) # get rid of random points that fall in water
set.seed(70)
data.0 <- data %>% filter(Used == 0) %>% group_by(Den_ID) %>% sample_n(25)  # now every den has 100 corresponding random points
data <- rbind((data %>% filter(Used == 1)), data.0)
# double check whether all den has 101 records. Should return an empty list
data %>% group_by(Den_ID) %>% summarise(n = n()) %>% filter(n != 11)

## reorganize landcover classes by regrouping CLC classes
veg.code <- read.csv("VegCombineCode.csv")
# reshape data
data <- data  %>% dplyr::left_join(veg.code, by = "CLC_STATE") %>%
  mutate(Den_ID = as.factor(Den_ID),
         Fire_Cat = factor(data$Fire_Cat, levels = c("U", "A", "B", "C", "D")),
         Land_Class = ifelse(is.na(Combined), "Other", Combined)) %>% 
  dplyr::select(Used, Den_ID, Fire_Cat, Ave_Wet, Perc_Tree, Land_Class)

## specify factors. Levels order will affect the baseline level in the modeling process
data$Land_Class <- factor(data$Land_Class, levels = c("Upland Forest", "Other", "Marsh-Shrub-Swamp", "Prairie-Grassland", "Wetland Forest"))
data$Land_Class <- relevel(data$Land_Class, ref = 2)

## finalize dataframe 
data <- data %>% dplyr::arrange (Den_ID) %>%
  mutate(Perc_Tree_raw = Perc_Tree,
         Ave_Wet = scale(Ave_Wet),
         Perc_Tree = scale(Perc_Tree))

## ----- model ----- ##
model_25 <- clogit(Used ~ Fire_Cat + Perc_Tree + Land_Class + Ave_Wet + Perc_Tree*Land_Class + strata(Den_ID), data = data, method='efron')
summary(model_25)

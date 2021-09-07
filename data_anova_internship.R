rm(list=ls())

#load packages
library(knitr)
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

####Growth rate with cell count data####
#import data:
growth<-read.csv("growthrate_cellcounts.txt")
growth<-select(growth,-X)

# look for normality in the data ->  hist(data) & qqnorm(data)
hist(growth$r)
qqnorm(growth$r)
qqline(growth$r)

#anova
aov_r<-aov(r~Phosphat*Nitrogen*Light*Temperature, data = growth)

##ANOVA on interactions on growth rate
summary(aov_r)

####Cell volume####
rm(list=ls())

#import data:
volume<-read.csv("median_cellvolume.txt")
volume<-select(volume,-X)
volume<- rename(volume, Nitrogen = N, Phosphat = P, Light= light, Temperature = temp)

# look for normality in the data ->  hist(data) & qqnorm(data)
hist(log(volume$cell_volume))
qqnorm(log(volume$cell_volume))
qqline(log(volume$cell_volume))

#anova
aov_volume<-aov(log(cell_volume)~Light*Nitrogen*Phosphat*Temperature, data = volume)

##ANOVA on interactions on cell volume
summary(aov_volume)

####Carrying capacity####

#import data:
data_cc<-read.csv("staurastrum_cellcount.txt")
data_end <- filter(data_cc, Day >=21)
data_end <- select(data_end, -X)


# look for normality in the data ->  hist(data) & qqnorm(data)
hist(log(data_end$cells_ml))
qqnorm(log(data_end$cells_ml))
qqline(log(data_end$cells_ml))

#anova 
aov_cc<-aov(log(cells_ml)~Light*Nitrogen*Phosphat*Temperature, data = data_end)

##ANOVA on interactions on carrying capacity
summary(aov_cc)

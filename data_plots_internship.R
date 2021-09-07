library(knitr)
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

### growth rates ####
#import dataset:
data_r<-read.csv("Rdata_030821/Rtables/growthrate_cellcounts.txt")#growthrates
data_ID<-read_excel("Rdata_030821/Rawdata/Rawdata_Staurastrum.xlsx","ID")
#look at the data:
#hist(log(data_r$r))
#rename columns from ID_data:
names(data_ID)<-c("ID","bottle","plankto","temp","light","nutrients","N","P","NPratio")
#delete X column and redundant columns in data_r, rename column to match and join df:
data_r <- rename(data_r, bottle = Bottle_no)
data_r<-select(data_r, bottle, r)
#Dataframes zusammenfügen, um in data_r alle Treatments zu haben:
data <- left_join(data_ID, data_r, by = "bottle")

#Kommastellen reduzieren:
data$P<-round(data$P, digits=1)
data$N<-round(data$N, digits=1)

gg_growthrate <- ggplot(data , 
                        aes(x= as.factor(temp), y = r, fill= factor(light)))+
  geom_boxplot()+
  #scale_fill_manual(limit = c("13.70","23.50","51.20","69.30","100.00"), 
  #                 labels = c("13","23","51","69","100"),values = c('#f2f0f7','#cbc9e2','#9e9ac8','#756bb1','#54278f'), 
  #                name="Lichtintensität [%]")+
  guides(fill = guide_legend(reverse = T))+
  #facet_grid(~Nitrogen,labeller = labeller(Nitrogen = panel_labels))+
  # scale_fill_discrete("Lichtintensität [%]",
  #                    type = getOption("ggplot2.discrete.fill") )+
  scale_fill_brewer("Light intensity [%]", palette = "YlGnBu" ,direction = -1)+ #-1 kehrt die palette um
  labs(title = "Interactive Effects Of Light And Temperature \n    On Staurastrum sp. Growth Rate", x="Temperature [°C]", y= "Growth rate")+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))
#coord_cartesian(ylim = c(0,0.75)) #Daten zoomen

gg_growthrate


####Cell Volume####
rm(list=ls())
#import data:
data<-read.csv("Rdata_030821/Rtables/median_cellvolume.txt")

#only important columns:
cellvolume<-select(data,bottle,arm,cell_volume,temp,light,N,P)

#Kommastellen reduzieren:
cellvolume$P<-round(cellvolume$P, digits=1)
cellvolume$N<-round(cellvolume$N, digits=1)

#plot boxplots

gg_cellvolume <- ggplot(transform(cellvolume, Light = factor(light, levels =  c("13,70","23,50","51,20","69,30","100,00"))), 
                        aes(x = as.factor(temp), y = cell_volume, fill = as.factor(light)))+
  geom_boxplot()+
  labs(title = "Temperature and Light dependence of cell volume",y = "Cell volume [µm³]", x = "Temperature [°C]")+
  #scale_fill_discrete("Temperature [°C]",
  #type = getOption("ggplot2.discrete.fill") )+
  scale_fill_brewer("Light intensity [%]", palette = "YlGnBu" ,direction = -1)+ #-1 kehrt die palette um
  guides(fill = guide_legend(reverse = T))+
  #coord_cartesian(ylim = c(7000,23000))+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))#+
#stat_boxplot(geom ='errorbar', width = 0.25)

gg_cellvolume

#plot boxplots

gg_cellvolume_p <- ggplot(cellvolume, 
                          aes(x = as.factor(temp), y = cell_volume, fill = as.factor(P)))+
  geom_boxplot()+
  labs(title = "Temperature and P dependence of cell volume",y = "Cell volume [µm³]", x = "Temperature [°C]")+
  #scale_fill_discrete("Temperature [°C]",
  #type = getOption("ggplot2.discrete.fill") )+
  scale_fill_hue("P [µmol]", direction = -1)+ #-1 kehrt die palette um
  guides(fill = guide_legend(reverse = T))+
  #coord_cartesian(ylim = c(7000,23000))+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12,face="bold"),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))#+
#stat_boxplot(geom ='errorbar', width = 0.25)

gg_cellvolume_p


### carrying capacity ####
rm(list=ls())
#import data:
data<-read.csv("Rdata_030821/Rtables/staurastrum_cellcount.txt")

#filter only finished samples
data_end <- filter(data, Day >=21)

#plot data
CC <- ggplot(data_end, 
             aes(x =  as.factor(Phosphat), y = cells_ml, fill = as.factor(Nitrogen)))+
  geom_boxplot()+
  # scale_x_discrete(limit = c("13,70","23,50","51,20","69,30","100,00"), labels = c("13","23","51","69","100"))+
  #stat_boxplot(geom ='errorbar', width = 0.25)+
  labs(title = "\n\n Interactive Effects Of P And N \n  On The Carrying Capacity Of Staurastrum sp.", x="P [µmol]", y= "Carrying Capacity \n [Cells/ml]")+
  #facet_grid(~Light)+
  guides(fill = guide_legend(reverse = T))+
  scale_fill_manual("N [µmol]",
                    values= c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026'))+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))#+
#coord_cartesian(ylim = c(0,10000)) #Daten zoomen

CC

#empty environment
rm(list=ls())

#load library
library(readxl)
library(readr)
library(tidyverse)

#### data structure cell volume ####

#import cellvolume data

size <- read_excel("Rawdata_Staurastrum.xlsx", "Cellvolume") #nur das 4. worksheet "Cellvolume" einladen
size_imagej<-read_excel("cellsize_ImageJ.xlsx")#Image J Datensatz einlesen:

#Tabelle mit allen zusätzlich benoetigten Spalten einladen
data_table <- read_excel("Rawdata_Staurastrum.xlsx", "ID")

#unnecessary columns löschen in cell volume Tabelle:
size<-select(size,1:6)
size_imagej<-select(size_imagej,-Date)

#Image J Datensatz in gleiches Format wie Rawdata (size) Datensatz bringen:
size_imagej<-spread(size_imagej,key = "Measurement",value=Value)

##Cellvolume Daten und Cellvolume Image J Daten zu einem Datensatz:
cellvolume<-rbind(size,size_imagej)

#Zellvolumen berechnen
data_cellvolume<- mutate(cellvolume, Cell_volume = ((pi/12)*h*(d1^2+d1*d2+d2^2))*2 )

#Tabelle durch Variablen ergänzen
data_cellvolume <- left_join(data_cellvolume, data_table, by = "Bottle_no")

#rename columns:
names(data_cellvolume)
names(data_cellvolume)<-c("bottle","individuum","d1","h","d2","arm","cellvolume","ID","plankto","temp","light","nutlevel","N","P","NPratio")

#save data as cellvolume.txt
write.csv(x=data_cellvolume, file = "staurastrum_cellvolume.txt")


#Cellvolume Median berechnen für jede Bottle und damit einen neuen Datensatz (Celvolume_Median) erstellen:
cellvolume_median<- group_by(data_cellvolume,temp,light,N,P,bottle)%>%
  summarise(cell_volume=median(cellvolume),sd=sd(cellvolume),arm=median(arm),arm_sd=sd(arm))

hist(data_cellvolume$cellvolume)

#save median data:
write.csv(x= cellvolume_median, file = "median_cellvolume.txt")

#### data structure cell counts ####

# import data sets
data_cellcount <- read_excel("Rawdata_Staurastrum.xlsx", "Cellcount")
data_filtration<-read_excel("Rawdata_Staurastrum.xlsx", "Filtration")#für letzten Tag
data_ID<-read_excel("Rawdata_Staurastrum.xlsx", "ID")#für Treatments für letzten Tag

# rename and filter important columns:
data_filtration<-select(data_filtration,Bottle,day)
names(data_filtration)<-c("Bottle_no","Day")

# rename id datatable column names to match names of data_filtration
names(data_ID)<-c("ID","Bottle_no","plankto","Temperature","Light","Nutrients","Nitrogen","Phosphat","N:Pratio")

#select important columns
data_ID<-select(data_ID,Bottle_no,Temperature,Light,Nutrients,Nitrogen,Phosphat)
data_cellcount<-select(data_cellcount,Bottle_no,GF,Counts,Temperature,Light,Nutrients,Nitrogen,Phosphat,Day)

#Count Dataframe aufsplitten:
counts_13_20<-filter(data_cellcount,Day<=20)
counts_ende<-filter(data_cellcount,Day==30)

#Für letzten Tag:
counts_ende<-arrange(counts_ende)#Zeilen in Spalten nach Größe sortieren
#Manche Proben wurden doppelt gezählt. Davon werden die Werte gemittelt:
counts_ende<-group_by(counts_ende,Bottle_no)%>%
  summarise(Counts=mean(Counts),GF=mean(GF))

#Filtration & counts_ende zusammen führen, um letzten Tag pro Probe zu haben:
counts_end<-left_join(counts_ende,data_filtration,by="Bottle_no")
#Counts end & ID zusammenführen, um Treatments zu haben:
counts_end<-left_join(counts_end,data_ID,by="Bottle_no")

#Dataframes counts_13_20 & counts_end zusammenführen, um alle Tage zu haben:
counts<-rbind(counts_13_20,counts_end)

#Kommastellen der N & P concentrations einstellen:
counts$Nitrogen<-round(counts$Nitrogen, digits=1) ##limits decimal points
counts$Phosphat<-round(counts$Phosphat , digits=1)

#Zellzahl auf Cells/ml berechnen und in neuer Spalte:
counts<- mutate(counts, cells_ml = (Counts*63.9)/(1*1*GF*0.5))


#### Save dataframe as staurastrum_cellcount.txt für weitere Analysen ####
write.csv(x= counts, file = "staurastrum_cellcount.txt")


#### counts growthrate ####
counts<-na.omit(counts)#remove NAs

#calculate growthrate
growth<-counts %>%
  group_by(Bottle_no,Light,Nitrogen,Phosphat,Temperature) %>% 
  summarise(r = (log(cells_ml[Day==max(Day)]) -log(cells_ml[Day==0])) / max(Day))

#write table
write.csv(x= growth, file = "growthrate_cellcounts.txt")

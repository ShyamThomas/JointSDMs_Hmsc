library(tidyverse)
library(data.table)
setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc")

############################################# PREPPING THE DATA FOR (Hmsc) JOINT DISTRIBUTION MODELS ###############################
### Read lake-level species co-occurrence data with few key lake environmental variables
### The lake community data is the end product of 'LakeEnv_PIComm_Data_Merge.R'

###JSDM.data=read_csv("Data/JSDM.lake_level_DOWonly.data.csv") ## this data has more lakes as it DOES NOT include within lake variables
JSDM.dt=fread("Data/JSDM.lake_level_DOWonly.data.csv", sep=",") ### too large; used data. table functions
class(JSDM.dt)
colnames(JSDM.dt)

#### We need to find the species abundance and distribution across lakes and point intercepts
#### This will help identify common and rare taxa in our study; and which taxa to exclude
######Summarize the taxa distribution data at the lake (DOWLKNUM) level
TaxCols=colnames(JSDM.dt)[8:235]
JSDM.SpeciesData.GroupedAtLakelevel=JSDM.dt[, lapply(.SD, sum, na.rm=TRUE), by="DOWLKNUM", .SDcols=TaxCols]
JSDM.SpeciesData.GroupedAtLakelevel
dim(JSDM.SpeciesData.GroupedAtLakelevel)


### Summarise for each species the total number of lakes with positive detection
TaxaCountLakelevel=colSums(JSDM.SpeciesData.GroupedAtLakelevel[,-1]>0)
TaxaCountLakelevel
TaxaCountLakelevel.matrix=as.matrix(TaxaCountLakelevel)
TaxaCountLakelevel.matrix
colnames(TaxaCountLakelevel.matrix)="Count"
write.csv(TaxaCountLakelevel.matrix, "Outputs/TaxaCount_Lakelevel_2.csv")

### Now summarise for each species the total number of detection (i.e. total number of PI samples with positive detection)
TaxaCountPIlevel=colSums(JSDM.SpeciesData.GroupedAtLakelevel[,-1])
TaxaCountPIlevel
TaxaCountPIlevel.matrix=as.matrix(TaxaCountPIlevel)
TaxaCountPIlevel.matrix
colnames(TaxaCountPIlevel.matrix)="Count"
head(TaxaCountPIlevel.matrix)
write.csv(TaxaCountPIlevel.matrix, "Outputs/TaxaCount_PIlevel_2.csv")

TaxaCountPIlevel=read.csv("Outputs/TaxaCount_PIlevel_2.csv")
colnames(TaxaCountPIlevel)[1]="Taxa"
head(TaxaCountPIlevel)
TaxaCountLakelevel=read.csv("Outputs/TaxaCount_Lakelevel_2.csv")
colnames(TaxaCountLakelevel)[1]="Taxa"
head(TaxaCountLakelevel)


### Merge the saved files; but before saving the following variables were calculated on excel: prevalence (total number of lakes
### with positive detection/all lakes sampled in survey  - 204); frequency of occurrence to total point intercept sampling)his
TaxaCountLake_PIlevels=merge(TaxaCountPIlevel,TaxaCountLakelevel, by="Taxa")
head(TaxaCountLake_PIlevels)
colnames(TaxaCountLake_PIlevels)=c("Taxa","Freq. of Occ.", "Distribution")
TaxaCountLake_PIlevels$Prevalence=(TaxaCountLake_PIlevels$Distribution/375)*100
head(TaxaCountLake_PIlevels)
write_csv(TaxaCountLake_PIlevels,"TaxaCount_Lake_PIlevels_LakeLevel.csv")

#################################################################################################################################
#### MAPPING THE DATASET:Capture the spatial distribution of EWM invaded lakes relative to all other lakes in the dataset
library(sf)

Minn.sf=read_sf(dsn="/Users/thom7552/UMNpostdoc/ProjectEWM/MinnEWM/MinnGISlayers", layer="Minn.map")
plot(Minn.sf$geometry)

JSDM.data.geo_coordinates=JSDM.data%>%distinct(DOWLKNUM, LON, LAT)
JSDM.data.geo_coordinates
JSDM.data.geo_coordinates.sf =st_as_sf(JSDM.data.geo_coordinates, coords = c("LON","LAT"), crs=32615)
JSDM.data.geo_coordinates.sf
JSDM.map=ggplot()+
geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
geom_sf(data=JSDM.data.geo_coordinates.sf, cex=1)
JSDM.map
### Need Latitude and Longitude columns
JSDM.SpeciesData.GroupedAtLake.LonLatlevel=JSDM.data%>%group_by(DOWLKNUM, LON, LAT)%>%
  summarise_at(vars(Acorus:Zosterella),sum)
dim(JSDM.SpeciesData.GroupedAtLake.LonLatlevel)
### Replace values greater than zero for all species columns with 1, for a binary map
JSDM.SpeciesData.GroupedAtLake.LonLatlevel[,c(4:231)][JSDM.SpeciesData.GroupedAtLake.LonLatlevel[,c(4:231)] > 0] <- 1
JSDM.SpeciesData.GroupedAtLake.LonLatlevel
JSDM.SpeciesData.GroupedAtLake.LonLatlevel.sf=st_as_sf(JSDM.SpeciesData.GroupedAtLake.LonLatlevel, coords = c("LON","LAT"), crs=32615)
JSDM.SpeciesData.GroupedAtLake.LonLatlevel.sf
### Final map:
JSDM.EWM.map=ggplot()+
geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
geom_sf(data=JSDM.SpeciesData.GroupedAtLake.LonLatlevel.sf$geometry, cex=1, aes(color=as.factor(JSDM.SpeciesData.GroupedAtLake.LonLatlevel.sf$`Myriophyllum spicatum`)))+scale_color_manual(values=c("blue", "red"))
JSDM.EWM.map+theme_gray(base_size = 22)+theme(legend.position = "none")

png("Outputs/Maps/EWMinvadedlakes.png",units="in", width=4.5, height=6, res=900)
JSDM.EWM.map+theme_gray(base_size = 22)+theme(legend.position = "none")
dev.off()

png("Outputs/Maps/EWMinvadedlakes_SouthBlockSeperate.png",units="in", width=4.5, height=6, res=900)
JSDM.EWM.map+theme_gray(base_size = 22)+theme(legend.position = "none")+geom_hline(yintercept=44.5, linetype="dashed",color = "white", size=0.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

### Subset southern lakes for spatially independent validation
library(sp)
coordinates(JSDM.SpeciesData.GroupedAtLake.LonLatlevel) <- c("LON", "LAT")
plot(JSDM.SpeciesData.GroupedAtLake.LonLatlevel, axes=TRUE)
abline(h=4930000, col="blue")
JSDM.data.north=JSDM.SpeciesData.GroupedAtLake.LonLatlevel.df%>%filter(JSDM.SpeciesData.GroupedAtLake.LonLatlevel.df$LAT > 4930000)
dim(JSDM.data.north)
JSDM.data.south=JSDM.SpeciesData.GroupedAtLake.LonLatlevel.df%>%filter(JSDM.SpeciesData.GroupedAtLake.LonLatlevel.df$LAT < 4930000)
dim(JSDM.data.south)

write_csv(JSDM.data.north, "Data/JSDM.data.northernsubset.csv")
write_csv(JSDM.data.south, "Data/JSDM.data.southernsubset.csv")

#################################################################################################################################
#################################################################################################################################



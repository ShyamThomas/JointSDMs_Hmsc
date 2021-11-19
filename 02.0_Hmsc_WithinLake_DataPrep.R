library(tidyverse)
library(data.table)
library(janitor)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc")

############################################# PREPPING THE DATA FOR (Hmsc) JOINT DISTRIBUTION MODELS ###############################
### Read lake-level species co-occurrence data with few key lake environmental variables
### The lake community data is the end product of 'WithinLakeEnv_PIComm_Data_Merge.R'

JSDM.data=read_csv("Data/JSDM.newdata.csv") ### this data has within lake env variables
JSDM.data%>%View()
JSDM.data
dim(JSDM.data)
length(unique(JSDM.data$DOWLKNUM)) ## 204 lakes

#### We need to find the species abundance and distribution across lakes and point intercepts
#### This will help identify common and rare taxa in our study; and which taxa to exclude
######Summarize the taxa distribution data at the lake (DOWLKNUM) level
JSDM.SpeciesData.GroupedAtLakelevel=JSDM.data%>%group_by(DOWLKNUM)%>%summarise_at(vars(Acorus:Zosterella),sum)
dim(JSDM.SpeciesData.GroupedAtLakelevel)
JSDM.SpeciesData.GroupedAtLakelevel

### Summarise for each species the total number of lakes with positive detection
TaxaCountLakelevel=colSums(JSDM.SpeciesData.GroupedAtLakelevel[,-1]>0)
TaxaCountLakelevel
TaxaCountLakelevel.matrix=as.matrix(TaxaCountLakelevel)
TaxaCountLakelevel.matrix
colnames(TaxaCountLakelevel.matrix)="Count"
write.csv(TaxaCountLakelevel.matrix, "Outputs/TaxaCountLakelevel.csv")

### Now summarise for each species the total number of detection (i.e. total number of PI samples with positive detection)
TaxaCountPIlevel=colSums(JSDM.SpeciesData.GroupedAtLakelevel[,-1])
TaxaCountPIlevel
TaxaCountPIlevel.matrix=as.matrix(TaxaCountPIlevel)
TaxaCountPIlevel.matrix
colnames(TaxaCountPIlevel.matrix)="Count"
head(TaxaCountPIlevel.matrix)
write.csv(TaxaCountPIlevel.matrix, "Outputs/TaxaCountPIlevel.csv")

### Read all the newly created files on taxa counts at lake and PI levels
TaxaCountPIlevel=read.csv("Outputs/TaxaCountPIlevel.csv")
colnames(TaxaCountPIlevel)[1]="Taxa"
head(TaxaCountPIlevel)
TaxaCountLakelevel=read.csv("Outputs/TaxaCountLakelevel.csv")
colnames(TaxaCountLakelevel)[1]="Taxa"
head(TaxaCountLakelevel)

### Merge the saved files; but before saving the following variables were calculated on excel: prevalence (total number of lakes
### with positive detection/all lakes sampled in survey  - 204); frequency of occurrence to total point intercept sampling)his
TaxaCountLake_PIlevels=merge(TaxaCountPIlevel,TaxaCountLakelevel, by="Taxa")
head(TaxaCountLake_PIlevels)
colnames(TaxaCountLake_PIlevels)=c("Taxa","Freq. of Occ.", "Distribution")
TaxaCountLake_PIlevels$Prevalence=(TaxaCountLake_PIlevels$Distribution/204)*100
head(TaxaCountLake_PIlevels)
write_csv(TaxaCountLake_PIlevels,"Outputs/TaxaCount_Lake_PIlevels_WLdata.csv")

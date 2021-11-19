###### MERGING LAKE-LEVEL ENV DATA WITH PLANT COMMUNITY POINT-INTERCEPT DATA 
###### A slight variation of Point-level WithinLake data merge in Rfile 02 

library(tidyverse)
library(janitor)

mn.ids2=read_csv("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data/mndow_nhdhr_xwalk.csv")
PlantComm.Data=read_csv("Data/LakeCommunitySpecies_PrsAbsData.csv")

###setwd("~/UMNpostdoc/ProjectEWM/MinnEWM")
LakeIndex=read_csv("MinnEWMdata/Secchi.CDOM.LakeDepth.LatLon.noNAs.csv")
LakeIndex
keep=c("DOWLKNUM", "YEAR","avgSecchi.m","LON","LAT","SIZE_ACRES","DepthMax")
LakeIndex.subset=LakeIndex[keep]
LakeIndex.subset
length(unique(LakeIndex.subset$DOWLKNUM))

LakeIndex.ConnData = read_csv("MinnEWMdata/LakeConnectivityData.Reduced.csv")
LakeIndex.ConnData
LakeConn=LakeIndex.ConnData%>%
dplyr::select(c(DOWLKNUM, allstreams_density_mperha, roaddensity_density_mperha))
LakeConn
length(setdiff(LakeIndex.subset$DOWLKNUM,LakeConn$DOWLKNUM))
LakeIndex.subset_Conn=left_join(LakeIndex.subset, LakeConn, by="DOWLKNUM")
LakeIndex.subset_Conn
colSums(is.na(LakeIndex.subset_Conn))

### Remove all possible NAs
LakeIndex.subset_Conn.narmd=LakeIndex.subset_Conn%>%na.omit() 
LakeIndex.subset_Conn.narmd

PlantComm.Data
length(setdiff(PlantComm.Data$DOWLKNUM,LakeIndex.subset_Conn.narmd$DOWLKNUM))
PlantComm_LakeIndex.Conn_DOW.YEAR=left_join(PlantComm.Data, LakeIndex.subset_Conn.narmd, by=c("DOWLKNUM", "YEAR"))
PlantComm_LakeIndex.Conn_DOW.YEAR
length(unique(PlantComm_LakeIndex.Conn_DOW.YEAR$DOWLKNUM))
PlantComm_LakeIndex.Conn_DOW.YEAR.narmd=PlantComm_LakeIndex.Conn_DOW.YEAR%>%na.omit()
length(unique(PlantComm_LakeIndex.Conn_DOW.YEAR.narmd$DOWLKNUM))


###Including couple more covariates: lake littoral percent, surface water temperature GDD
##setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc")
lake_basin_morpho=read_csv("Data/lake_basin_morphology.csv")
lake_basin_sel.morpho=lake_basin_morpho[,c("DOWLKNUM","LK_PCTLTRL")]
lake_basin_sel.morpho
lake_basin_sel.morpho$DOWLKNUM=as.numeric(lake_basin_sel.morpho$DOWLKNUM)
JSDM.data2=left_join(PlantComm_LakeIndex.Conn_DOW.YEAR.narmd,lake_basin_sel.morpho, by="DOWLKNUM")
JSDM.data2
colSums(is.na(JSDM.data2))

setwd("~/UMNpostdoc/ProjectEWM/MinnEWM")
Temp95_15.DOWYEAR=read_csv("MinnEWMdata/NLDAS.therm9515.aggbyDOW.YEAR.csv")
GDD_10C_1995_2015=Temp95_15.DOWYEAR[,c(2,3,5)]
GDD_10C_1995_2015
colnames(GDD_10C_1995_2015)[2]="YEAR"
#JSDM.data3=left_join(JSDM.data2,GDD_10C_1995_2015, by=c("DOWLKNUM", "YEAR"))
#JSDM.newdata=JSDM.data3%>%drop_na(mean.gdd_wtr_10c, LK_PCTLTRL)
#JSDM.newdata
#dim(JSDM.newdata)
#length(unique(JSDM.newdata$DOWLKNUM))

### Another merge this time only using DOWS, less loss of lakes
JSDM.data4=left_join(JSDM.data2,GDD_10C_1995_2015, by="DOWLKNUM")
JSDM.newdata2=JSDM.data4%>%drop_na(mean.gdd_wtr_10c, LK_PCTLTRL)
dim(JSDM.newdata2)
write_csv(JSDM.newdata2, "Data/JSDM.lake_level_DOWonly.data.csv")


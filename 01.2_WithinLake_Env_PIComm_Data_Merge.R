###### MERGING WITHIN LAKE TEMPERATURE AND LIGHT CONDITIONS WITH PLANT COMMUNITY POINT-INTERCEPT DATA 
library(tidyverse)
library(janitor)

mn.ids2=read_csv("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data/mndow_nhdhr_xwalk.csv")
PlantComm.Data=read_csv("Data/LakeCommunitySpecies_PrsAbsData.csv")
KdIrr.DepthTemp.AnnGLD=readRDS("Data/KdIrr.DepthTemp.AnnCummGLD.rds")

mn.ids2
mn.ids2$DOWLKNUM=sub(".{6}","",mn.ids2$MNDOW_ID)
mn.ids2
colnames(mn.ids2)[2]="FileName"
mn.ids2

KdIrr.DepthTemp.AnnGLD_DOWs=left_join(KdIrr.DepthTemp.AnnGLD,mn.ids2, by="FileName")
KdIrr.DepthTemp.AnnGLD_DOWs$DOWLKNUM=as.numeric(KdIrr.DepthTemp.AnnGLD_DOWs$DOWLKNUM)
head(KdIrr.DepthTemp.AnnGLD_DOWs)
length(unique(KdIrr.DepthTemp.AnnGLD_DOWs$DOWLKNUM)) ### 760 unique DOWs
colnames(KdIrr.DepthTemp.AnnGLD_DOWs)[9]="Depth_mts" ###Make sure the column names match
colnames(KdIrr.DepthTemp.AnnGLD_DOWs)[9]
KdIrr.DepthTemp.AnnGLD_DOWs


PlantComm.Data$Depth_mts=round(PlantComm.Data$DEPTH_FT*0.3,1)
length(unique(PlantComm.Data$DOWLKNUM)) ### 1499 unique DOWs
### Few modifications
PlantComm.Dataset=PlantComm.Data%>%
select(-X1)%>%
select(DOWLKNUM,YEAR,DEPTH_FT,Depth_mts,STA_NBR_DATASOURCE, SURVEY_ID, POINT_ID, everything())
PlantComm.Dataset
colnames(PlantComm.Dataset)[2]="Year" ##Column names should match

length(intersect(PlantComm.Dataset$DOWLKNUM,KdIrr.DepthTemp.AnnGLD_DOWs$DOWLKNUM)) ### 265
length(setdiff(PlantComm.Dataset$DOWLKNUM,KdIrr.DepthTemp.AnnGLD_DOWs$DOWLKNUM)) ### 1234
length(setdiff(KdIrr.DepthTemp.AnnGLD_DOWs$DOWLKNUM,PlantComm.Dataset$DOWLKNUM)) ## 495

### Merge Within lake env data with point intercept plant community data
### Merge by DOWLKNUM, Year, and Depth
dim(KdIrr.DepthTemp.AnnGLD_DOWs)
### Remove few unwanted columns
KdIrr.DepthTemp.AnnGLD_DOWs.sub=KdIrr.DepthTemp.AnnGLD_DOWs[,-c(2,4:6,11,13)]
head(KdIrr.DepthTemp.AnnGLD_DOWs.sub)
colnames(PlantComm.Dataset)[2]="Year" ##Column names should match
colnames(KdIrr.DepthTemp.AnnGLD_DOWs)[9]="Depth_mts"

KdIrr.DepthTemp.AnnGLD_PlantComm_merged=inner_join(KdIrr.DepthTemp.AnnGLD_DOWs.sub,PlantComm.Dataset, by=c("DOWLKNUM","Year","Depth_mts"))

KdIrr.DepthTemp.AnnGLD_PlantComm_merged

dim(KdIrr.DepthTemp.AnnGLD_PlantComm_merged)
length(unique(KdIrr.DepthTemp.AnnGLD_PlantComm_merged$DOWLKNUM))

KdIrr.DepthTemp.AnnGLD_PlantComm_merged%>%View()
write.csv(KdIrr.DepthTemp.AnnGLD_PlantComm_merged, "Data/LakeEnv_PlantComm.MergedByDOWyeardepth.csv")

#######################################################################################################################################
#######################################################################################################################################
### Merging without depth, to get a fuller representation of lake DD and GLD conditions
PlantComm.DOWYear=PlantComm.Dataset[,c(1,2)]
PlantComm.DOWYear
KdIrr.DepthTemp.AnnGLD_PlantComm_merged2=inner_join(KdIrr.DepthTemp.AnnGLD_DOWs.sub,PlantComm.DOWYear, by=c("DOWLKNUM","Year"))
KdIrr.DepthTemp.AnnGLD_PlantComm_merged2

Lake.TempAnnGLD_PlantComm_AllDepths=KdIrr.DepthTemp.AnnGLD_PlantComm_merged2%>%distinct()
Lake.TempAnnGLD_PlantComm_AllDepths

### Lets try plotting it
RndLakes=sample(unique(Lake.TempAnnGLD_PlantComm_AllDepths$DOWLKNUM),1)
RndLakes
RndLakesData=Lake.TempAnnGLD_PlantComm_AllDepths%>%
  filter(DOWLKNUM %in% RndLakes)
RndLakesData

library(plotly)
fig3 <- plot_ly(RndLakesData, x = ~Depth_mts, y = ~CummGLD, z = ~DD, color = ~Year)
fig3=fig3 %>% add_markers()
fig3=fig3%>%layout(scene = list(xaxis = list(title = 'Depth'),
                              yaxis = list(title = 'GLD'),
                              zaxis = list(title = 'GDD')))
fig3

###An attempt at plotting difference in rate of change in water temperature across different depth levels
depth.cl =cut(Lake.TempAnnGLD_PlantComm_AllDepths$Depth_mts, breaks = quantile(Lake.TempAnnGLD_PlantComm_AllDepths$Depth_mts, c(0,0.33,0.66,1.0)), labels = c("low", "medium", "high"),include.lowest = TRUE)
Lake.TempAnnGLD_PlantComm_AllDepths
Lake.TempAnnGLD_PlantComm_AllDepths$DepthClass=depth.cl

ggplot(Lake.TempAnnGLD_PlantComm_AllDepths,aes(x=Year, y= DD, col=DepthClass))+
geom_point(position=position_dodge(1.0), alpha=0.1)+stat_smooth(method="lm")

#######################################################################################################################################
#######################################################################################################################################
###### MERGING WITHIN LAKE TEMPERATURE, LIGHT CONDITIONS AND PLANT COMMUNITY POINT-INTERCEPT DATA WITH LAKE-LEVEL VARIABLES 
##### THIS GIVES US THE UPPER LAKE-LEVEL PREDICTORS (e.g., depth, Secchi, Road density)

WithinLake.Env.PlantComm.Data=read_csv("Data/LakeEnv_PlantComm.MergedByDOWyeardepth.csv")
WithinLake.Env.PlantComm.Data

###setwd("~/UMNpostdoc/ProjectEWM/MinnEWM")
LakeIndex=read_csv("MinnEWMdata/Secchi.CDOM.LakeDepth.LatLon.noNAs.csv")
LakeIndex
colnames(LakeIndex)
keep=c("DOWLKNUM", "YEAR","avgSecchi.m","LON","LAT","SIZE_ACRES","DepthMax")
LakeIndex.subset=LakeIndex[keep]
LakeIndex.subset

LakeIndex.ConnData = read_csv("MinnEWMdata/LakeConnectivityData.Reduced.csv")
LakeIndex.ConnData
LakeConn=LakeIndex.ConnData%>%
dplyr::select(c(DOWLKNUM, allstreams_density_mperha, roaddensity_density_mperha))
LakeConn

length(setdiff(LakeIndex.subset$DOWLKNUM,LakeConn$DOWLKNUM))
length(setdiff(WithinLake.Env.PlantComm.Data$DOWLKNUM,LakeIndex.subset$DOWLKNUM))

LakeIndex.subset_Conn=left_join(LakeIndex.subset, LakeConn, by="DOWLKNUM")
LakeIndex.subset_Conn

colSums(is.na(LakeIndex.subset_Conn))
### there are still NAs
LakeIndex.subset_Conn.narmd=LakeIndex.subset_Conn%>%na.omit() ### Remove all possible NAs
LakeIndex.subset_Conn.narmd
length(setdiff(WithinLake.Env.PlantComm.Data$DOWLKNUM,LakeIndex.subset_Conn.narmd$DOWLKNUM)) ## 6 Lakes will be lost when merged with Within Lake data

### Merge with WITHIN LAKE DATASET
WithinLake.Env.PlantComm.Data
colSums(WithinLake.Env.PlantComm.Data(is.na)) ### NO NAs found!!
colnames(WithinLake.Env.PlantComm.Data)[3]="YEAR" ### Match Colnames before Merging!
WithinLake.Env.PlantComm.Data

WithinLake.Env.PlantComm_LakeIndex.Conn=left_join(WithinLake.Env.PlantComm.Data, LakeIndex.subset_Conn.subset3, by=c("DOWLKNUM", "YEAR"))
colSums(is.na(WithinLake.Env.PlantComm_LakeIndex.Conn))
length(unique(WithinLake.Env.PlantComm_LakeIndex.Conn$DOWLKNUM))## 264 lakes
WithinLake.Env.PlantComm_LakeIndex.Conn.NoNAs=WithinLake.Env.PlantComm_LakeIndex.Conn%>%na.omit()
length(unique(WithinLake.Env.PlantComm_LakeIndex.Conn.NoNAs$DOWLKNUM))## 239 Lakes... final count!!
WithinLake.Env.PlantComm_LakeIndex.Conn.NoNAs%>%View()
colSums(is.na(WithinLake.Env.PlantComm_LakeIndex.Conn.NoNAs))
dim(WithinLake.Env.PlantComm_LakeIndex.Conn.NoNAs)
write_csv(WithinLake.Env.PlantComm_LakeIndex.Conn.NoNAs,"Data/WithinLake.Env.PlantComm_LakeIndex.Conn.NoNAs.csv")

###Including couple more covariates: lake littoral percent, surface water temperature GDD
##setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc")
lake_basin_morpho=read_csv("Data/lake_basin_morphology.csv")
lake_basin_sel.morpho=lake_basin_morpho[,c("DOWLKNUM","LK_PCTLTRL")]
lake_basin_sel.morpho
lake_basin_sel.morpho$DOWLKNUM=as.numeric(lake_basin_sel.morpho$DOWLKNUM)
JSDM.data2=left_join(WithinLake.Env.PlantComm_LakeIndex.Conn.NoNAs,lake_basin_sel.morpho, by="DOWLKNUM")
JSDM.data2

###setwd("~/UMNpostdoc/ProjectEWM/MinnEWM")
Temp95_15.DOWYEAR=read_csv("MinnEWMdata/NLDAS.therm9515.aggbyDOW.YEAR.csv")
GDD_10C_1995_2015=Temp95_15.DOWYEAR[,c(2,3,5)]
GDD_10C_1995_2015
colnames(GDD_10C_1995_2015)[2]="YEAR"
JSDM.data3=left_join(JSDM.data2,GDD_10C_1995_2015, by=c("DOWLKNUM", "YEAR"))
JSDM.newdata=JSDM.data3%>%drop_na(mean.gdd_wtr_10c, LK_PCTLTRL)
dim(JSDM.newdata)
length(unique(JSDM.data$DOWLKNUM)) ### 204 unique lakes

write_csv(JSDM.newdata2,"Data/JSDM.newdata.csv")

JSDM.data=read_csv("Data/JSDM.newdata.csv") 

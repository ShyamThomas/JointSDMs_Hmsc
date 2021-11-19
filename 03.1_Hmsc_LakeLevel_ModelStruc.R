### Codes for calculating lake-level abundance dataset from point-intercept level community data
library(tidyverse) 

setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc")

JSDM.data=read_csv("Data/JSDM.newdata.csv")
dim(JSDM.data)

JSDM.dt=fread("Data/JSDM.lake_level_DOWonly.data.csv", sep=",")

###Calculate lake-level average measures of all covariates
###Also need to calculate the total number of unique point-intercept samples and survey ids.
JSDM.lakelevel.envdata=JSDM.dt%>%
group_by(DOWLKNUM)%>%
summarise(
meanSIZE_ACRES=mean(SIZE_ACRES),
meanDEPTH_MAX=mean(DepthMax),
meanAvgSECCHI=mean(avgSecchi.m),
meanRoad_Density=mean(roaddensity_density_mperha),
meanLittoral=mean(LK_PCTLTRL),
meanGDD_WTR_10c=mean(mean.gdd_wtr_10c),
LONGITUDE=max(LON), LATITUDE=max(LAT),
totPOINT_ID=length(POINT_ID),
totSURVEY_ID=length(unique(SURVEY_ID))
)
JSDM.lakelevel.envdata

JSDM.lakelevel.envdata%>%View()

### Now calculate species specific abundance; i.e. sum of all presences and absences at the lake-level
JSDM.lakelevel.spdata=JSDM.dt%>%
group_by(DOWLKNUM)%>%
summarise_each(funs(sum), c(7:234))

JSDM.lakelevel.spdata%>%View()

JSDM.lakelevel.abun.data=merge(JSDM.lakelevel.envdata, JSDM.lakelevel.spdata, by="DOWLKNUM")
JSDM.lakelevel.abun.data
dim(JSDM.lakelevel.abun.data)

write_csv(JSDM.lakelevel.abun.data, "Data/JSDM.lakelevel.abun.data.csv")

### Estimate abundance weighted by point-intercept sampling effort
JSDM.sp.data_pointid=JSDM.lakelevel.abun.data[,12:239]/JSDM.lakelevel.abun.data$totPOINT_ID
JSDM.sp.data_pointid
JSDM.sp.data_pointid$DOWLKNUM=JSDM.lakelevel.abun.data$DOWLKNUM
JSDM.sp.data_pointid

JSDM.data_std.pointid=merge(JSDM.lakelevel.envdata, JSDM.sp.data_pointid, by="DOWLKNUM")
JSDM.data_std.pointid
dim(JSDM.data_std.pointid) ### 375 lakes

### This is the final abundance dataset; species abundance has been standardized by point-id sampling effort; so the data is
### is in proportion of total sampled point intercepts in a given lake with positive detections
write_csv(JSDM.data_std.pointid, "Data/JSDM.abundata_stdzd.pointid.csv")
JSDM.abun.data=read_csv("Data/JSDM.abundata_stdzd.pointid.csv")

####################################################################################################################################
### Now get back to the top 24 taxa selected previously for final JSDM anlyses 
Top24Taxa=read_csv("Data/Taxa_25PerAbove.csv")
Top24Taxa
TaxaNames_24=Top24Taxa$Taxa
TaxaNames_24

### Subset the abundance dataset above such that it contains only the selected 24 taxa
JSDM.abun.count.data=read_csv("Data/JSDM.lakelevel.abun.data.csv")
JSDM.abun.data=read_csv("Data/JSDM.abundata_stdzd.pointid.csv")
JSDM.abun.data
colnames(JSDM.abun.data)[12] ## Species data starting column
JSDM.abun.subdata=JSDM.abun.data%>%select(c(1:11),TaxaNames_24)
JSDM.abun.subdata
Xabun.data=as.data.frame(cbind(JSDM.abun.subdata$meanDEPTH_MAX,JSDM.abun.subdata$meanAvgSECCHI,JSDM.abun.subdata$meanRoad_Density,
                               JSDM.abun.subdata$meanLittoral,JSDM.abun.subdata$meanGDD_WTR_10c))
colnames(Xabun.data)=c("MaxDepth", "AvgSecchi","RoadDensity","Littoral","SurfGDD")
dim(Xabun.data)
tail(Xabun.data)

library(corrplot)
Xabun.data.cor=cor(Xabun.data, method="pearson")
Xabun.data.cor
corrplot.mixed(Xabun.data.cor, lower.col = "black", tl.pos = "l")

### Quite strong correlation between secchi and max depth; hence dropping max depth
Xabun.data=as.data.frame(cbind(JSDM.abun.subdata$meanAvgSECCHI,JSDM.abun.subdata$meanRoad_Density,
                               JSDM.abun.subdata$meanLittoral,JSDM.abun.subdata$meanGDD_WTR_10c))
colnames(Xabun.data)=c("AvgSecchi","RoadDensity","Littoral","SurfGDD")

Xabun.formula = ~ AvgSecchi + RoadDensity + Littoral
Xabun.formula

Yabun.data=as.matrix(JSDM.abun.subdata[,c(12:35)])
head(Yabun.data)
dim(Yabun.data)

DOWLKNUM=as.factor(JSDM.abun.subdata$DOWLKNUM)
DOWLKNUM
sample.id=as.factor(1:length(JSDM.abun.subdata$DOWLKNUM))
studyDesign.abun.subdata=data.frame(sample=sample.id, plot=DOWLKNUM)
head(studyDesign.abun.subdata)
dim(studyDesign.abun.subdata)
rL.DOWLKNUM = HmscRandomLevel(units = levels(studyDesign.abun.subdata$plot))
rL.DOWLKNUM
rL.DOWLKNUM$nfMax=2 ### !!
ranlevels = list("plot" = rL.DOWLKNUM)
ranlevels

Abun.SXY_Top24Taxa=cbind(studyDesign.abun.subdata,Xabun.data,Yabun.data)
head(Abun.SXY_Top24Taxa)
write_csv(Abun.SXY_Top24Taxa,"Data/Abun.SXY_Top24Taxa.csv")
Abun.SXY_Top24Taxa=read_csv("Data/Abun.SXY_Top24Taxa.csv")

### The unfitted abundance model within Hmsc modeling framework
Abun.DOW_top24taxa_ufmodel = Hmsc(Y=Yabun.data, XData = Xabun.data,
XFormula = Xabun.formula,
studyDesign = studyDesign.abun.subdata,
ranLevels  = ranlevels,
distr = "normal")

save(Abun.DOW_top24taxa_ufmodel,file="Abun.DOW_top24taxa_ufmodel.Rdata")


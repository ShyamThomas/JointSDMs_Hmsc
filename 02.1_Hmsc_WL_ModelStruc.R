library(Hmsc)
library(snow)
library(tidyverse)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc")

### The goal here is set the Hmsc model structure for Within lake JSDMs; i.e. put together the species, environmental data along with
### random-effects specifications. This unfitted model will eventually be fit using the MCMC algorithm
################################################## TRIAL JSDM RUNS USING HMSC ###############################################
### Working first with a small set of species data. 
### The  species prevalence dataset was further modified directly in excel to yeild two truncated datasets: 
### (a) Only species with lake prevalence above 25%, which yields 24 sp. (b) species with lake-level prevalence above 10%

Top24Taxa=read_csv("Data/Taxa_25PerAbove.csv")
Top24Taxa
TaxaNames_24=Top24Taxa$Taxa
TaxaNames_24

JSDM.data=read_csv("Data/JSDM.newdata.csv")
JSDM.data
colnames(JSDM.data)[21] ## Species data starting column
JSDM.subdata=JSDM.data%>%select(c(1:20),TaxaNames_24)
JSDM.subdata
JSDM.subdata%>%View()

## Including all predictors including depth:
Xdata=as.data.frame(cbind(JSDM.subdata$DEPTH_MAX,JSDM.subdata$avgSECCHI,JSDM.subdata$Road_Density,JSDM.subdata$Littoral,
                          JSDM.subdata$GDD_WTR_10c,JSDM.subdata$Depth_mts,JSDM.subdata$DD,JSDM.subdata$CummGLD))
colnames(Xdata)=c("MaxDepth", "AvgSecchi","RoadDensity","Littoral","SurfGDD","WithinLakeDepth","WithinLakeDegreeDays",
                  "WithinLakeGLD")
### After excluding depth as a predictor:
Xdata2=as.data.frame(cbind(JSDM.subdata$DEPTH_MAX,JSDM.subdata$avgSECCHI,JSDM.subdata$Road_Density,JSDM.subdata$Littoral,
                          JSDM.subdata$GDD_WTR_10c,JSDM.subdata$DD,JSDM.subdata$CummGLD))
colnames(Xdata2)=c("MaxDepth", "AvgSecchi","RoadDensity","Littoral","SurfGDD","WithinLakeDegreeDays",
                  "WithinLakeGLD")

tail(Xdata2)
dim(Xdata2)

library(corrplot)
Xdata.cor=cor(Xdata, method="pearson")
Xdata.cor
corrplot.mixed(Xdata.cor, lower.col = "black", tl.pos = "l")


Xformula = ~ MaxDepth + AvgSecchi + RoadDensity + Littoral + SurfGDD + WithinLakeDepth + WithinLakeDegreeDays+WithinLakeGLD
Xformula
Xformula2 = ~ MaxDepth + AvgSecchi + RoadDensity + Littoral + SurfGDD + WithinLakeDegreeDays+WithinLakeGLD
Xformula2

Ydata=as.matrix(JSDM.subdata[,c(21:44)])
head(Ydata)
dim(Ydata)

DOWLKNUM=as.factor(JSDM.subdata$DOWLKNUM)
sample.id=as.factor(1:length(JSDM.subdata$DOWLKNUM))
studyDesign.subdata=data.frame(sample=sample.id, plot=DOWLKNUM)
head(studyDesign.subdata)
dim(studyDesign.subdata)
rL.DOWLKNUM = HmscRandomLevel(units = levels(studyDesign.subdata$plot))
rL.DOWLKNUM
rL.DOWLKNUM$nfMax=2 
ranlevels = list("plot" = rL.DOWLKNUM)
ranlevels


SXY_Top24Taxa=cbind(studyDesign.subdata,Xdata,Ydata)
head(SXY_Top24Taxa)
write_csv(SXY_Top24Taxa,"Data/SXY_Top24Taxa.csv")

DOW_top24taxa_model = Hmsc(Y=Ydata, XData = Xdata,
                            XFormula = Xformula,
                            studyDesign = studyDesign.subdata,
                            ranLevels  = ranlevels,
                            distr = "probit")

DOW_top24taxa_model2 = Hmsc(Y=Ydata, XData = Xdata2,
                           XFormula = Xformula2,
                           studyDesign = studyDesign.subdata,
                           ranLevels  = ranlevels,
                           distr = "probit")

DOW_top24taxa_model
DOW_top24taxa_model2

save(DOW_top24taxa_model,file="DOW_Top24Taxa_unfittedmodel.Rdata")
save(DOW_top24taxa_model2,file="DOW_Top24Taxa_unfittedmodel2.Rdata")

### A trial run of the unfitted model above just to ensure its working
thin = 50
samples = 500
nChains = 3
adaptNf=rep(ceiling(0.4*samples*thin),DOW_top24taxa_model$nr)
transient = ceiling(0.5*samples*thin)

mod_HMSC_thin500_samples500_chains4 = sampleMcmc(DOW_top24taxa_model,
                                                 samples = samples,
                                                 thin = thin,
                                                 adaptNf = adaptNf,
                                                 transient = transient,
                                                 nChains = nChains,
                                                 nParallel = nChains)

############################################################################################################################
### Amore complex model with depth as nested random factor within lake-level random factor
### Only major changes are in the study design 
DOWLKNUM=as.factor(JSDM.subdata$DOWLKNUM)
sample.id=as.factor(1:length(JSDM.subdata$DOWLKNUM))
### Assign depth as a random factor at 4 categorical levels
breaks = quantile(JSDM.subdata$Depth_mts, c(0.0,0.33,0.66,0.99, 1))
new_vec = cut(JSDM.subdata$Depth_mts , breaks = quantile(JSDM.subdata$Depth_mts, c(0.0,0.33,0.66,0.99, 1)),
              labels=c("low","lowermids", "uppermids", "high"), include.lowest=TRUE)
data.frame(JSDM.subdata$Depth_mts, new_vec)%>%View()
JSDM.subdata$Depth_mts_cat=new_vec

### Call the new categorical variable "depth id" and include it as nested random factor 
depth.id=JSDM.subdata$Depth_mts_cat
studyDesign.subdata=data.frame(sample=sample.id, plot=DOWLKNUM, subplot=depth.id)
head(studyDesign.subdata)
class(studyDesign.subdata)
### Now changing the study design slightly to include depth as a nested random effect
#studyDesign.subdata2=studyDesign.subdata%>%group_by(subplot)%>%mutate(wLake_subplotid=1:n())
#studyDesign.subdata2$wLake_subplotid=as.factor(studyDesign.subdata2$wLake_subplotid)
#head(studyDesign.subdata2.df)
#studyDesign.subdata2.df=as.data.frame(studyDesign.subdata2)
#head(studyDesign.subdata2.df)
dim(studyDesign.subdata)
rL1 = HmscRandomLevel(units = unique(studyDesign.subdata$plot))
rL1
rL2=HmscRandomLevel(units=unique(studyDesign.subdata$subplot))
rL2
ranlevels = list("plot" = rL1,"subplot"=rL2)
ranlevels

### Since depth is a random factor; remove depth from fixed-effects component
Xdata2=as.data.frame(cbind(JSDM.subdata$DEPTH_MAX,JSDM.subdata$avgSECCHI,JSDM.subdata$Road_Density,JSDM.subdata$Littoral,
                           JSDM.subdata$GDD_WTR_10c,JSDM.subdata$DD,JSDM.subdata$CummGLD))
colnames(Xdata2)=c("MaxDepth", "AvgSecchi","RoadDensity","Littoral","SurfGDD","WithinLakeDegreeDays",
                   "WithinLakeGLD")
tail(Xdata2)
dim(Xdata2)

Xformula_d = ~MaxDepth+AvgSecchi+RoadDensity+Littoral+SurfGDD+WithinLakeDegreeDays+WithinLakeGLD

DOW_DEPTH_24PerTaxa_model = Hmsc(Y=Ydata, XData = Xdata2,
                                 XFormula = Xformula_d,
                                 studyDesign = studyDesign.subdata,
                                 ranLevels  = ranlevels,
                                 distr = "probit")

DOW_DEPTH_24PerTaxa_model
save(DOW_DEPTH_24PerTaxa_model,file="DOW_DEPTH_24PerTaxa_unfittedmodel.Rdata")

##################################################################################################################################
##################################################################################################################################
### Working with species that record 10 percent prevalence and higher; this yields around 41 species
Taxa_10Per=read_csv("Data/Taxa_10PerAbove.csv")
Taxa_10Per
TaxaNames_10Per=Taxa_10Per$Taxa
TaxaNames_10Per

JSDM.data=read_csv("Data/JSDM.newdata.csv")
JSDM.data
colnames(JSDM.data)[20]
JSDM.subdata=JSDM.data%>%select(c(1:20),all_of(TaxaNames_10Per))
JSDM.subdata%>%View()

Xdata=as.data.frame(cbind(JSDM.subdata$DEPTH_MAX,JSDM.subdata$avgSECCHI,JSDM.subdata$Road_Density,JSDM.subdata$Littoral,
                          JSDM.subdata$GDD_WTR_10c,JSDM.subdata$Depth_mts,JSDM.subdata$DD,JSDM.subdata$CummGLD))
colnames(Xdata)=c("MaxDepth", "AvgSecchi","RoadDensity","Littoral","SurfGDD","WithinLakeDepth","WithinLakeDegreeDays",
                  "WithinLakeGLD")
tail(Xdata)
dim(Xdata)

library(corrplot)
Xdata.cor=cor(Xdata, method="pearson")
Xdata.cor
corrplot.mixed(Xdata.cor, lower.col = "black", tl.pos = "l")

Xformula = ~MaxDepth+AvgSecchi+RoadDensity+Littoral+SurfGDD+WithinLakeDepth+WithinLakeDegreeDays+WithinLakeGLD
Xformula

Ydata=as.matrix(JSDM.subdata[,c(21:61)])
head(Ydata)
dim(Ydata)

DOWLKNUM=as.factor(JSDM.subdata$DOWLKNUM)
sample.id=as.factor(1:length(JSDM.subdata$DOWLKNUM))
studyDesign.subdata=data.frame(sample=sample.id, plot=DOWLKNUM)
head(studyDesign.subdata)
rL.DOWLKNUM = HmscRandomLevel(units = levels(studyDesign.subdata$plot))
rL.DOWLKNUM
ranlevels = list("plot" = rL.DOWLKNUM)
ranlevels

SXY_10PerTaxa=cbind(studyDesign.subdata,Xdata,Ydata)
head(SXY_10PerTaxa)
write_csv(SXY_10PerTaxa,"Data/SXY_JSDMdata_10per.csv")



### An example of model with lake ids as random effects using 10 percent prevalence data
DOW_Top10PerTaxa_RE_model = Hmsc(Y=Ydata, XData = Xdata,
                              XFormula = Xformula,
                              studyDesign = studyDesign.subdata,
                              ranLevels  = ranlevels,
                              distr = "probit")


save(DOW_Top10PerTaxa_RE_model,file="DOW_Top10PerTaxa_unfittedREmodel.Rdata")




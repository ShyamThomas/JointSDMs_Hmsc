library(Hmsc)
library(snow)
library(miceadds)
library(tidyverse)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc")

### Now get back to the top 24 taxa selected previously for final JSDM anlyses 
Top24Taxa=read_csv("Data/Taxa_25PerAbove.csv")
Top24Taxa
TaxaNames_24=Top24Taxa$Taxa
TaxaNames_24

### Subset the abundance dataset above such that it contains only the selected 24 taxa
JSDM.abun.data=read_csv("Data/JSDM.lakelevel.abun.data.csv")
JSDM.abun.data
colnames(JSDM.abun.data)[12] ## Species data starting column
JSDM.abun.subdata=JSDM.abun.data%>%select(c(1:11),TaxaNames_24)
JSDM.abun.subdata


### Subset the abundance (in proportion) dataset above such that it contains only the selected 24 taxa
JSDM.abun.prpn.data=read_csv("Data/JSDM.abundata_stdzd.pointid.csv")
JSDM.abun.prpn.data
colnames(JSDM.abun.prpn.data)[12] ## Species data starting column
JSDM.abun.prpn.subdata=JSDM.abun.prpn.data%>%select(c(1:12),TaxaNames_24)
JSDM.abun.prpn.subdata
Xabun.data=as.data.frame(cbind(JSDM.abun.prpn.subdata$meanDEPTH_MAX,JSDM.abun.prpn.subdata$meanAvgSECCHI,JSDM.abun.prpn.subdata$meanRoad_Density,
                               JSDM.abun.prpn.subdata$meanLittoral,JSDM.abun.prpn.subdata$meanGDD_WTR_10c))
colnames(Xabun.data)=c("MaxDepth", "AvgSecchi","RoadDensity","Littoral","SurfGDD")
dim(Xabun.data)
tail(Xabun.data)

######################################################################
#library(corrplot)
#Xabun.data.cor=cor(Xabun.data, method="pearson")
#Xabun.data.cor
#corrplot.mixed(Xabun.data.cor, lower.col = "black", tl.pos = "l")
######################################################################

### Removed max_depth because it was correlated with avg Secchi

Xabun.formula = ~ AvgSecchi + RoadDensity + Littoral + SurfGDD
Xabun.formula

Yabun.data=as.matrix(JSDM.abun.prpn.subdata[,c(12:35)])
head(Yabun.data)
dim(Yabun.data)

DOWLKNUM=as.factor(JSDM.abun.prpn.subdata$DOWLKNUM)
DOWLKNUM
sample.id=as.factor(1:length(JSDM.abun.prpn.subdata$DOWLKNUM))
studyDesign.abun.prpn.subdata=data.frame(sample=sample.id, plot=DOWLKNUM)
head(studyDesign.abun.prpn.subdata)
dim(studyDesign.abun.prpn.subdata)
rL.DOWLKNUM = HmscRandomLevel(units = levels(studyDesign.abun.prpn.subdata$plot))
rL.DOWLKNUM
rL.DOWLKNUM$nfMax=2 ### !!
ranlevels = list("plot" = rL.DOWLKNUM)
ranlevels

Abun.SXY_Top24Taxa=cbind(studyDesign.abun.subdata,Xabun.data,Yabun.data)
head(Abun.SXY_Top24Taxa)
write_csv(Abun.SXY_Top24Taxa,"Data/Abun.SXY_Top24Taxa.csv")

Abun.SXY_Top24Taxa=read_csv("Data/Abun.SXY_Top24Taxa.csv")
dim(Abun.SXY_Top24Taxa)

### Hurdle models are set in a for loop
uf.models=list()

for (i in 1:2){
Y = as.matrix(Yabun.data)
if (i==1){Y = 1*(Y>0)}
if (i==2) {
Y[Y==0] = NA
}
uf.models[[i]] = Hmsc(Y=Y, XData=Xabun.data, XFormula = Xabun.formula, studyDesign = studyDesign.abun.subdata,
                   ranLevels  = ranlevels, distr= switch(i,  "probit", "normal"), YScale = TRUE)
}

### Hurdle model: prs abs component
uf.models[[1]]
head(uf.models[[1]]$Y)

### Hurdle model: abundance component
uf.models[[2]]
head(uf.models[[2]]$Y)

### Starting out with 100,000 iterations and 3 chains
samples = 1000
thin = 200
nChains = 3
transient = ceiling(0.5*samples*thin)

f.occ.models=sampleMcmc(uf.models[[1]], thin=thin, samples=samples, transient=transient, nChains = nChains,
                        nParallel = nChains)
save(f.occ.models,file="Outputs/Hurdle_Occ_t100s1000c3.Rdata")
mpost.coda_HurdleOcc_t100s1000c3=convertToCodaObject(f.occ.models)
boxplot(effectiveSize(mpost.coda_HurdleOcc_t100s1000c3$Beta))
gd_t100_hurdle.occ=gelman.diag(mpost.coda_HurdleOcc_t100s1000c3$Beta, multivariate = FALSE)$psrf
hist(gd_t100_hurdle.occ)

f.abun.models=sampleMcmc(uf.models[[2]], thin=thin, samples=samples, transient=transient, nChains = nChains, 
                         nParallel = nChains)
save(f.abun.models,file="Outputs/Hurdle_Abun_t100s1000c3.Rdata")
mpost.coda_HurdleAbun_t100s1000c3=convertToCodaObject(f.abun.models)
boxplot(effectiveSize(mpost.coda_HurdleAbun_t100s1000c3$Beta))
gd_t100_hurdle.abun=gelman.diag(mpost.coda_HurdleAbun_t100s1000c3$Beta, multivariate = FALSE)$psrf
hist(gd_t100_hurdle.abun)


###Compute association matrix
load.Rdata("Outputs/Hurdle_Occ_t100s1000c3.Rdata","f.occ.models")
Occ_omega=computeAssociations(f.occ.models)

supportLevel = 0.90

toPlot = ((Occ_omega[[1]]$support>supportLevel)
          + (Occ_omega[[1]]$support<(1-supportLevel))>0)*Occ_omega[[1]]$mean
corrplot(toPlot, method = "color", col=colorRampPalette(c("red","white","blue"))(200),
         title=paste("Species associations matrix"), mar=c(0,0,1,0), type="lower",order = "alphabet", 
         outline = TRUE, tl.cex=0.7,diag=F,tl.col = "black")


load.Rdata("Outputs/Hurdle_Abun_t100s1000c3.Rdata","f.abun.models")
Abun_omega=computeAssociations(f.abun.models)

supportLevel = 0.90

toPlot = ((Abun_omega[[1]]$support>supportLevel)
+ (Abun_omega[[1]]$support<(1-supportLevel))>0)*Abun_omega[[1]]$mean

corrplot(toPlot, method = "color", col=colorRampPalette(c("red","white","blue"))(200),
title=paste("Species associations matrix"), mar=c(0,0,1,0), type="lower",order = "alphabet", 
outline = TRUE, tl.cex=0.7,diag=F,tl.col = "black")



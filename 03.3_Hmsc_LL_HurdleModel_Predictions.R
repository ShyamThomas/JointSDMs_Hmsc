library(Hmsc)
library(snow)
library(miceadds)
library(tidyverse)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc")

load.Rdata("Outputs/Hurdle_Occ_t100s1000c3.Rdata", "Hurdle_Occ_t100s1000c3")
Hurdle_Occ_t100s1000c3
load.Rdata("Outputs/Hurdle_Abun_t100s1000c3.Rdata", "Hurdle_Abun_t100s1000c3")
Hurdle_Abun_t100s1000c3

colnames(Hurdle_Occ_t100s1000c3$Y)

###Partioning data into 2 folds
partitions = createPartition(Hurdle_Occ_t100s1000c3, nfolds = 2)

### To make predictions conditional on P. crispus (curly-leaf pondweed); partition again on by specifying P. crispus as a holdout
partition.sp_P.cris = c(rep(1,times=6),2,rep(1,17))
partition.sp_P.cris
### Similarly make predictions conditional on invasive M. spicatum (Eurasian watermilfoil), by blocking it as distinct fold
partition.sp_M.spic = c(rep(1,times=18),2,rep(1,5))
partition.sp_M.spic
###Predictions conditional on the state of both invasive species
partition.sp_Inv=c(rep(1,times=6),2,rep(1,11),2, rep(1,5))
partition.sp_Inv


Preds_HurdleOcc_P.cris = computePredictedValues(Hurdle_Occ_t100s1000c3, partition = partitions, partition.sp =partition.sp_P.cris, 
                                                mcmcStep = 100, verbose = 0)
save(Preds_HurdleOcc_P.cris,file="Outputs/Preds_HurdleOcc_P.cris.Rdata")

Preds_HurdleOcc = computePredictedValues(Hurdle_Occ_t100s1000c3, partition = partitions, mcmcStep = 100, verbose = 0)
save(Preds_HurdleOcc,file="Outputs/Preds_HurdleOcc.Rdata")

## Now to make the abundance predictions 
partitions.abun = createPartition(Hurdle_Abun_t100s1000c3, nfolds = 2)

MarPreds_HurdleAbun = computePredictedValues(Hurdle_Abun_t100s1000c3, partition = partitions.abun, mcmcStep = 100, verbose = 0)
Mean_mar.preds_abun=data.frame(apply(MarPreds_HurdleAbun,c(1,2), mean))

CondPreds_HurdleAbun_P.Cris = computePredictedValues(Hurdle_Abun_t100s1000c3,partition = partitions.abun, 
                                                     partition.sp = partition.sp_P.cris, mcmcStep = 100,  verbose = 0)
Mean_cond.preds_abun=data.frame(apply(CondPreds_HurdleAbun_P.Cris,c(1,2), mean))


#### Calculating predicitons from the last slice
CondPreds.3k=data.frame(CondPreds_HurdleAbun_P.Cris[,,3000])
CondPreds.3k$folds=partitions.abun
head(CondPreds.3k)

MarPreds.3k=data.frame(MarPreds_HurdleAbun[,,3000])
MarPreds.3k$folds=partitions.abun
head(MarPreds.3k)

Mean.MarPreds_byGroup=MarPreds.3k%>%group_by(folds)%>%
summarise_all(mean)
Mean.CondPreds_byGroup=CondPreds.3k%>%group_by(folds)%>%
summarise_all(mean)

write_csv(Mean.CondPreds_byGroup,"Outputs/Mean.CondPreds_byGroup.csv")
write_csv(Mean.MarPreds_byGroup,"Outputs/Mean.MarPreds_byGroup.csv")


####This time with last 100 slices
MarPreds.last100=MarPreds_HurdleAbun[,,2900:3000]
Mean_mar.preds.last100_abun=data.frame(apply(MarPreds.last100,c(1,2), mean))
Mean_mar.preds.last100_abun$folds=partitions.abun
colnames(Mean_mar.preds.last100_abun)=taxa.names
dim(Mean_mar.preds.last100_abun)
head(Mean_mar.preds.last100_abun)

CondPreds.last100=CondPreds_HurdleAbun_P.Cris[,,2900:3000]
Mean_cond.preds.last100_abun=data.frame(apply(CondPreds.last100,c(1,2), mean))
Mean_cond.preds.last100_abun$folds=partitions.abun
dim(Mean_cond.preds.last100_abun)
head(Mean_cond.preds.last100_abun)

colnames(Mean_cond.preds.last100_abun)[25]="Folds"
colnames(Mean_cond.preds.last100_abun)[25]
colnames(Mean_mar.preds.last100_abun)[25]="Folds"

Mean_cond.preds.last100_byGroup=Mean_cond.preds.last100_abun%>%group_by(Folds)%>%
  summarise_all(mean)
Mean_mar.preds.last100_byGroup=Mean_mar.preds.last100_abun%>%group_by(Folds)%>%
  summarise_all(mean)


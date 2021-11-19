library(Hmsc)
library(snow)
library(miceadds)
library(tidyverse)

load.Rdata("Outputs/Top24Taxa_models_thin_100_samples_1000_chains_3.Rdata", "model24_t100s1000c3")
model24_t100s1000c3

colnames(model24_t100s1000c3$Y)

###Partioning data into 2 folds
partitions = createPartition(model24_t100s1000c3, nfolds = 2)

### To make predictions conditional on P. crispus (curly-leaf pondweed); partition again on by specifying P. crispus as a holdout
partition.sp_P.cris = c(rep(1,times=6),2,rep(1,17))
partition.sp_P.cris
Preds_model24_WL.Occ_P.cris = computePredictedValues(model24_t100s1000c3, partition = partitions, partition.sp =partition.sp_P.cris, 
                                                mcmcStep = 100, verbose = 0)
save(Preds_model24_WL.Occ_P.cris,file="Outputs/Preds_model24_WL.Occ_P.cris.Rdata")
load.Rdata("Outputs/Preds_model24_WL.Occ_P.cris.Rdata", "Preds_model24_WL.Occ_P.cris.Rdata")


Preds_model24_WL.Occ = computePredictedValues(model24_t100s1000c3, partition = partitions, mcmcStep = 100, verbose = 0)
save(Preds_model24_WL.Occ,file="Outputs/Preds_model24_WL.Rdata")
load.Rdata("Outputs/Preds_model24_WL.Rdata", "Preds_model24_WL.Rdata")

library(Hmsc)
library(miceadds)

####################################################################################################################################
#### Evaluate convergence for half the number of taxa, lake-level prevalence 25% and more
#### These converged well by 100, 00 iterations
### Read saved Hmsc output objects
load.Rdata("Outputs/Top24Taxa_models_thin_150_samples_1000_chains_3.Rdata", "model24_t150s1000c3")
model24_t150s1000c3
load.Rdata("Outputs/Top24Taxa_models_thin_125_samples_1000_chains_3.Rdata", "model24_t125s1000c3")
model24_t125s1000c3
load.Rdata("Outputs/Top24Taxa_models_thin_100_samples_1000_chains_3.Rdata", "model24_t100s1000c3")
model24_t100s1000c3
load.Rdata("Outputs/Top24Taxa_models_thin_75_samples_1000_chains_3.Rdata", "model24_t75s1000c3")
model24_t75s1000c3
load.Rdata("Outputs/Top24Taxa_models_thin_50_samples_1000_chains_3.Rdata", "model24_t50s1000c3")
model24_t50s1000c3
load.Rdata("Outputs/Top24Taxa_models_thin_25_samples_1000_chains_3.Rdata", "model24_t25s1000c3")
model24_t25s1000c3

### Covert to coda object
mpost24.coda_t25s1000c3=convertToCodaObject(model24_t25s1000c3)
mpost24.coda_t50s1000c3=convertToCodaObject(model24_t50s1000c3)
mpost24.coda_t75s1000c3=convertToCodaObject(model24_t75s1000c3)
mpost24.coda_t100s1000c3=convertToCodaObject(model24_t100s1000c3)
mpost24.coda_t125s1000c3=convertToCodaObject(model24_t125s1000c3)
mpost24.coda_t150s1000c3=convertToCodaObject(model24_t150s1000c3)

###Effective sample size for 3 chains of 1000 samples each
boxplot(effectiveSize(mpost24.coda_t25s1000c3$Beta),effectiveSize(mpost24.coda_t50s1000c3$Beta),
        effectiveSize(mpost24.coda_t75s1000c3$Beta), effectiveSize(mpost24.coda_t100s1000c3$Beta),
        effectiveSize(mpost24.coda_t125s1000c3$Beta), effectiveSize(mpost24.coda_t150s1000c3$Beta),
        main="ntaxa 24, samples 1000, chains 3", xlab="Effective Sample Size",names=c("t25","t50","t75", "t100","t125", "t150"))

### Gelman convergence statistic; anova of between and within chain variations
gd_t150=gelman.diag(mpost24.coda_t150s1000c3$Beta, multivariate = FALSE)$psrf
gd_t125=gelman.diag(mpost24.coda_t125s1000c3$Beta, multivariate = FALSE)$psrf
gd_t100=gelman.diag(mpost24.coda_t100s1000c3$Beta, multivariate = FALSE)$psrf
gd_t75=gelman.diag(mpost24.coda_t75s1000c3$Beta, multivariate = FALSE)$psrf
gd_t50=gelman.diag(mpost24.coda_t50s1000c3$Beta, multivariate = FALSE)$psrf
gd_t25=gelman.diag(mpost24.coda_t25s1000c3$Beta, multivariate = FALSE)$psrf

boxplot(gd_t25[,1],gd_t50[,1], gd_t75[,1], gd_t100[,1],gd_t125[,1],gd_t150[,1], names=c("t25","t50","t75", "t100", "t125", "t150"), 
        main="beta_psrf: ntaxa 24, samples 1000, chains 3")

### Testing model fit for the best converged model

####################################################################################################################################
####################################################################################################################################
### Below are models with 41 taxa, which NEVER CONVEREGED well, and so were not used further:

### Read saved Hmsc output objects
#load.Rdata("Outputs/models_thin_150_samples_1000_chains_3.Rdata", "model_t1125s1000c3")
#model_t150s1000c3
#load.Rdata("Outputs/models_thin_125_samples_1000_chains_3.Rdata", "model_t1125s1000c3")
#model_t125s1000c3
load.Rdata("Outputs/models_thin_100_samples_1000_chains_3.Rdata", "model_t100s1000c3")
model_t100s1000c3
load.Rdata("Outputs/models_thin_75_samples_1000_chains_3.Rdata", "model_t75s1000c3")
model_t75s1000c3
load.Rdata("Outputs/models_thin_50_samples_1000_chains_3.Rdata", "model_t50s1000c3")
model_t50s1000c3
load.Rdata("Outputs/models_thin_25_samples_1000_chains_3.Rdata", "model_t25s1000c3")
model_t25s1000c3
load.Rdata("Outputs/models_thin_5_samples_1000_chains_3.Rdata","model_t5s1000c3")
model_t5s1000c3

### Covert to coda object
mpost.coda_t5s1000c3=convertToCodaObject(model_t5s1000c3)
mpost.coda_t25s1000c3=convertToCodaObject(model_t25s1000c3)
mpost.coda_t50s1000c3=convertToCodaObject(model_t50s1000c3)
mpost.coda_t75s1000c3=convertToCodaObject(model_t75s1000c3)
mpost.coda_t100s1000c3=convertToCodaObject(model_t100s1000c3)


###Effective sample size for 3 chains of 1000 samples each
boxplot(effectiveSize(mpost.coda_t25s1000c3$Beta),
        effectiveSize(mpost.coda_t50s1000c3$Beta),effectiveSize(mpost.coda_t75s1000c3$Beta), 
        effectiveSize(mpost.coda_t100s1000c3$Beta),
        main="ntaxa 41, samples 1000,chains 3", xlab="Effective Sample Size",names=c("t25","t50","t75", "t100"))

### Gelman convergence statistic; anova of between and within chain variations
gd_t100=gelman.diag(mpost.coda_t100s1000c3$Beta, multivariate = FALSE)$psrf
gd_t75=gelman.diag(mpost.coda_t75s1000c3$Beta, multivariate = FALSE)$psrf
gd_t50=gelman.diag(mpost.coda_t50s1000c3$Beta, multivariate = FALSE)$psrf
gd_t25=gelman.diag(mpost.coda_t25s1000c3$Beta, multivariate = FALSE)$psrf
gd_t5=gelman.diag(mpost.coda_t5s1000c3$Beta, multivariate = FALSE)$psrf

boxplot(gd_t5[,1],gd_t25[,1],gd_t50[,1],gd_t75[,1], gd_t100[,1], names=c("t5","t25","t50","t75", "t100"), 
        main="beta_psrf: ntaxa41, samples 1000,chains 3")

boxplot(gd_t5[,2],gd_t25[,2],gd_t50[,2],gd_t75[,2], gd_t100[,2], names=c("t5","t25","t50","t75", "t100"), 
        main="beta_psrf: samples 1000,chains 3")

####################################################################################################################################
### Testing model fit
load.Rdata("Outputs/models_thin_75_samples_1000_chains_3.Rdata", "model_t75s1000c3")
model_t75s1000c3
modelt75s1000c3_preds = computePredictedValues(model_t5s1000c3)

partition = createPartition(model_t75s1000c3, nfolds = 2)
modelt75s1000c3_predsCV2 = computePredictedValues(model_t75s1000c3, partition = partition,
                                                  partition.sp = 1:model_t75s1000c3$ns, mcmcStep = 1000)
save(modelt75s1000c3_predsCV2, "modelt75s1000c3_predsCV2.Rdata")



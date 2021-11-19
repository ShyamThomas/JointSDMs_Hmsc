library(tidyverse)
library(reshape2)
library(ggplot2)
library(lubridate)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data")

### First step: read the files needed (All these files were downloaded from USGS science base website)
irrad.files=list.files("WithinLakeData/IrradianceData2/AllIrradianceData/")
head(irrad.files) 

kd.files=list.files("WithinLakeData/ClarityData/AllClarityData/")
head(kd.files)

#########################################################################################################
######### Codes from Gretchen to link MNDOW lake ids with NHDHR ids... might need them later.
#########################################################################################################

mn.ids2=read_csv("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data/mndow_nhdhr_xwalk.csv")
mn.ids2
length(mn.ids2$MNDOW_ID)
mn.ids=unique(na.omit(mn.ids2$site_id))
head(mn.ids)
length(mn.ids)
mn.files=data.frame("site_id"=(mn.ids), file.name=NA)
head(mn.files)

for(i in 1:length(mn.ids))
{
  mn.files$file.name[i]=paste("pb0_", mn.ids[i],"_irradiance.csv",sep="")
}
head(mn.files)
available.irr.files=mn.files[mn.files$file.name%in%irrad.files,]
head(available.irr.files)
length(available.irr.files$file.name)

#########################################################################################################
### My codes start agin here!!
### A simple function to split large csv files to smaller subsets with data (2000 onwards) that's most needed
### The function also splits event dates into more detailed multiple columns of year, month,& day-of-year

modify_and_save_from_dataset <- function(csvfile) {
  df = read_csv(csvfile)
  df_new=df%>% filter(df$date > "1999-12-31")%>%
    dplyr::mutate(year = lubridate::year(date),
                  month = lubridate::month(date),
                  day = lubridate::day(date),
                  yearday=lubridate::yday(date))
  
  csvfile_new = gsub(".csv", "_modified.csv", csvfile)
  write_csv(df_new, csvfile_new)
}

### Now read the Irradiance and Clarity files and modify them using the above function
for (csvfile in irrad.files){
  modify_and_save_from_dataset(csvfile)
}

###Repeat the same for Kd files...
for (csvfile in kd.files){
  modify_and_save_from_dataset(csvfile)
}

#### All the modified files were then copied into two new folders: "IrradModified" & "ClarityModified"

####### Now, bind all the separate files into a single one with filenames as a row id
###Step 1: read the all the modified files
irrad.modi.files=list.files("Data/WithinLakeData/IrradianceData/IrradModified/")
head(irrad.modi.files)
kd.modi.files=list.files("Data/WithinLakeData/ClarityData/ClarityModified/")
head(kd.modi.files)

### Step 2: make a simple function to bind all the files by rows with filename added as a column

all_Kd.modi = do.call(rbind, lapply(kd.modi.files, function(x)
transform(read.csv(paste("Data/WithinLakeData/ClarityData/ClarityModified/",x, sep="")), File = basename(x))))
head(all_Kd.modi)
length(unique(all_Kd.modi$File)) ### all 854 lakes' Kd files!

all_Irr.modi = do.call(rbind, lapply(irrad.modi.files, function(x) 
  transform(read.csv(paste("Data/WithinLakeData/IrradianceData2/All_IrradianceModified/",x, sep="")), File = basename(x))))
head(all_Irr.modi)
length(unique(all_Irr.modi$File)) ### all 881 lakes' Irr files!

### Step 3: Fix the filenames so that they match between the two datasets
all_Kd.modi$FileNew0=sub(".{4}", "",all_Kd.modi$File)
all_Kd.modi$FileName=gsub(".{21}$", "",all_Kd.modi$FileNew0)
all_Kd.modi=all_Kd.modi[,-c(7,8)]
head(all_Kd.modi)

all_Irr.modi$FileNew0=sub(".{4}", "",all_Irr.modi$File)
all_Irr.modi$FileName=gsub(".{24}$", "",all_Irr.modi$FileNew0)
head(all_Irr.modi)
all_Irr.modi=all_Irr.modi[,-c(7,8)]

### Step 4: Before merging the two datasets, check for overlap between the dataset's new filenames
length(intersect(all_Irr.modi$FileName, all_Kd.modi$FileName)) ## 854 file names IN COMMMON 
length(setdiff(all_Irr.modi$FileName, all_Kd.modi$FileName))  ## & 27 file names NOT IN COMMON!
length(setdiff(all_Kd.modi$FileName,all_Irr.modi$FileName))

### Step 5: Merge Irr dataset (881 files) to Kd datset (854 files), since former is a larger set that contains all but 27 Kd lake files

AllKdIrr_merge=inner_join(all_Kd.modi,all_Irr.modi, by=c("FileName","date"))
head(AllKdIrr_merge)
AllKdIrr=AllKdIrr_merge%>%
select(FileName,date,year.x,month.x,yearday.x,day.x, everything())
head(AllKdIrr)
dim(AllKdIrr)
AllKdIrr=AllKdIrr[,-c(9:12)]
length(unique(AllKdIrr$FileName))

saveRDS(AllKdIrr, "Data/AllKdIrr_merge.rds")
AllKdIrr=readRDS("Data/AllKdIrr_merge.rds")
head(AllKdIrr)


### Step 6: Read the depth Degree days dataset and check again for overlap in file names
tempBydepth.data=read_csv("Data/WithinLakeData/TempData/dds_final.csv")
tempBydepth.data
colnames(tempBydepth.data)[2]="FileName"
tempBydepth.data
tibble(AllKdIrr)
colnames(AllKdIrr)[c(2:8)]=c("Date","Year","Month","Yearday","Day","Kd","Rad_0")
length(intersect(tempBydepth.data$FileName, AllKdIrr$FileName))

###Running out of memmory, so using data.table functions
library(data.table)
TempDepth.dt=setDT(tempBydepth.data, key = c("FileName", "Year"))
AllKdIrr.dt=setDT(AllKdIrr, key = c("FileName", "Year"))
KdIrr.DepthTemp.Join=AllKdIrr[TempDepth.dt, allow.cartesian=TRUE]
KdIrr.DepthTemp.Join
length(unique(KdIrr.DepthTemp.Join$FileName))

KdIrr.DepthTemp.Join.NoNa=na.omit(KdIrr.DepthTemp.Join)
length(unique(KdIrr.DepthTemp.Join.NoNa$FileName))
KdIrr.DepthTemp.Join.NoNa
saveRDS(KdIrr.DepthTemp.Join.NoNa, "Data/KdIrr.DepthTemp_merge.rds")
KdIrr.DepthTemp=readRDS("Data/KdIrr.DepthTemp_merge.rds")

######################################################################################################################################################
KdIrr.DepthTemp$GLD=KdIrr.DepthTemp$Rad_0*(exp(-KdIrr.DepthTemp$Kd*KdIrr.DepthTemp$depth))
KdIrr.DepthTemp
saveRDS(KdIrr.DepthTemp,"Data/KdIrr.DepthTemp.GLD.rds")

KdIrr.DepthTemp.GLD=readRDS("Data/KdIrr.DepthTemp.GLD.rds")
KdIrr.DepthTemp.GLD

KdIrr.DepthTemp.CummGLD=KdIrr.DepthTemp.GLD[, CummGLD:=cumsum(GLD), c("FileName","Year","depth")][]
KdIrr.DepthTemp.CummGLD

KdIrr.DepthTemp.AnnCummGLD=KdIrr.DepthTemp.CummGLD[KdIrr.DepthTemp.CummGLD[, .I[which.max(CummGLD)], by= c("FileName","Year", "depth")]$V1]
KdIrr.DepthTemp.AnnCummGLD
length(unique(KdIrr.DepthTemp.AnnCummGLD$FileName))
View(KdIrr.DepthTemp.AnnCummGLD[1:10000,])

saveRDS(KdIrr.DepthTemp.AnnCummGLD,"Data/KdIrr.DepthTemp.AnnCummGLD.rds") ###!!!!

##### Try plotting the data in a 3D plot with depth, GDD, and GLD
KdIrr.DepthTemp.AnnGLD=readRDS("Data/KdIrr.DepthTemp.AnnCummGLD.rds")
KdIrr.DepthTemp.AnnGLD

### Try first one randomly selected lake
RndLakes=sample(unique(KdIrr.DepthTemp.AnnGLD$FileName),1)
RndLakes
RndLakesData=KdIrr.DepthTemp.AnnGLD%>%
filter(FileName %in% RndLakes)
RndLakesData

library(plotly)
fig <- plot_ly(RndLakesData, x = ~depth, y = ~CummGLD, z = ~DD, color = ~Year)
fig=fig %>% add_markers()
fig=fig%>%layout(scene = list(xaxis = list(title = 'Depth'),
yaxis = list(title = 'GLD'),
zaxis = list(title = 'GDD')))
fig





######################################################################################################################################
######################################################################################################################################
### The codes below assume a short-cut approach towards estimation annual growing light days estimation 
### Now here, I modify the files further by aggregating by year and finding a mean radiance for each year
### The modification involves summarizing irradiance and clarity as mean annual measures for each lake for all years 
### between 2000 and 2018
irrad.modi.files=list.files("Data/WithinLakeData/IrradianceData/IrradModified/")
head(irrad.modi.files)
length(irrad.modi.files)

kd.modi.files=list.files("Data/WithinLakeData/ClarityData/ClarityModified/")
head(kd.modi.files)
length(kd.modi.files)

### Function to calculate max cummulative annual irradiance
summarize_year_cumsum <- function(csvfile) {
  df_modi = read_csv(csvfile)
  df_ann=df_modi %>% group_by(year) %>%
    dplyr::summarise(max_ann_rad=max(cumsum(rad_0)))
  
  csvfile_ann_rad = gsub(".csv", "cum_annrad.csv", csvfile)
  write_csv(df_ann, csvfile_ann_rad)
}

for (csvfile in irrad.modi.files){
  summarize_year_cumsum(csvfile)
}

### Function to calculate max cummulative clarity (Kd)
summarize_year_mean_min_max <- function(csvfile) {
  df_modi = read_csv(csvfile)
  df_ann=df_modi %>% group_by(year) %>%
    dplyr::summarise(mean_ann_Kd=mean(kd, na.rm=TRUE),
                     min_ann_Kd=min(kd, na.rm=TRUE),
                     max_ann_Kd=max(kd, na.rm=TRUE)
    )
  
  csvfile_ann_rad = gsub(".csv", "MeanMinMax_AnnKd.csv", csvfile)
  write_csv(df_ann, csvfile_ann_rad)
}

for (csvfile in kd.modi.files){
  summarize_year_mean_min_max(csvfile)
}

### Now, bind all the separate files into a single one with filenames as a row id
max.ann.rad=list.files("WithinLakeData/IrradianceData/CummMaxIrrad/")
head(max.ann.rad)
length(max.ann.rad)

all_mar = do.call(rbind, lapply(max.ann.rad, function(x) 
  transform(read.csv(paste("WithinLakeData/IrradianceData/CummMaxIrrad/",x, sep="")), File = basename(x))))

##Remove first four characters from file names
all_mar$site_i=sub(".{4}$", "", all_mar$File)
head(all_mar)

all_mar$site_id=gsub(".{34}$","",all_mar$site_i)
head(all_mar)

length(all_mar)
all_mar2=all_mar[,-c(3,4)]
head(all_mar2)
tail(all_mar2)
length(unique(all_mar2$site_id))

write_csv(all_mar2, "All_CummMax_AnnualRadiance_Combined.csv")

#### Repeat the above for Clarity files and Kd data
mean.min.max_ann.Kd=list.files("WithinLakeData/ClarityData/MeanMinMax_AnnKd/")
head(mean.min.max_ann.Kd)

all_maKd = do.call(rbind, lapply(mean.min.max_ann.Kd, function(x) 
  transform(read.csv(paste("WithinLakeData/ClarityData/MeanMinMax_AnnKd/",x, sep="")), File = basename(x))))

head(all_maKd)

all_maKd$site_i=sub("....", "", all_maKd$File)
head(all_maKd)

all_maKd$site_id=gsub(".{37}$","",all_maKd$site_i)
head(all_maKd)

all_maKd2=all_maKd[,-c(5,6)]
head(all_maKd2)

write_csv(all_maKd2, "All_MeanMinMax_AnnualKd_Combined.csv")

##The new names should match the site_id on 'degree days by depth' files
tempBydepth.data=read_csv("Data/WithinLakeData/TempData/dds_final.csv")
tempBydepth.data

CummAnnIrr.data=read_csv("All_CummMax_AnnualRadiance_Combined.csv")
CummAnnIrr.data
Mean.Min.Max.AnnKd.data=read_csv("All_MeanMinMax_AnnualKd_Combined.csv")
Mean.Min.Max.AnnKd.data

### Now check for overlap  between Cummulative Irradiance data and TempByDepth data
available.sites= intersect(tempBydepth.data$site_id, CummAnnIrr.data$site_id)
unavailable.sites=setdiff(tempBydepth.data$site_id, CummAnnIrr.data$site_id)
length(unavailable.sites)
available.sites ### 568 lakes are common, we are losing 70 lakes (638-568), i.e. 70 lakes with no radiance data

available.sites2= intersect(tempBydepth.data$site_id, CummAnnKd.data$site_id)
length(available.sites2)
unavailable.sites2=setdiff(tempBydepth.data$site_id,CummAnnKd.data$site_id)
length(unavailable.sites2)


### Merge the files by site_id and year (check column names match!)
tempBydepth_MAIR.merge=left_join(tempBydepth.data,CummAnnIrr.data, by=c("site_id", "Year"))
tempBydepth_MAIR.merge.narmd=tempBydepth_MAIR.merge%>%na.omit()
tempBydepth_MAIR.merge.narmd
length(unique(tempBydepth_MAIR.merge.narmd$site_id))
hist(tempBydepth_MAIR.merge.narmd$Year)

tempBydepth_MAIR.merge.narmd_mergeKd=left_join(tempBydepth_MAIR.merge.narmd, Mean.Min.Max.AnnKd.data, by=c("site_id", "Year"))
tempBydepth_MAIR.merge.narmd_mergeKd
length(unique(tempBydepth_MAIR.merge.narmd_mergeKd$site_id))
hist(tempBydepth_MAIR.merge.narmd_mergeKd$Year)

### Now caculate depth specific irradiance using cummulative annual surface irradiance and mean annual water clarity
### Kz=I0*exp(-Kd*Depth)
tempBydepth_MAIR.merge.narmd_mergeKd$Irrad=tempBydepth_MAIR.merge.narmd_mergeKd$max_ann_rad*exp(-tempBydepth_MAIR.merge.narmd_mergeKd$mean_ann_Kd*tempBydepth_MAIR.merge.narmd_mergeKd$depth)
tempBydepth_MAIR.merge.narmd_mergeKd

### Repeat the calculation of depth specific irradiance using min annual water clarity
tempBydepth_MAIR.merge.narmd_mergeKd$MinIrrad=tempBydepth_MAIR.merge.narmd_mergeKd$max_ann_rad*exp(-tempBydepth_MAIR.merge.narmd_mergeKd$min_ann_Kd*tempBydepth_MAIR.merge.narmd_mergeKd$depth)
tempBydepth_MAIR.merge.narmd_mergeKd

### Repeat the calculation of depth specific irradiance using max annual water clarity
tempBydepth_MAIR.merge.narmd_mergeKd$MaxIrrad=tempBydepth_MAIR.merge.narmd_mergeKd$max_ann_rad*exp(-tempBydepth_MAIR.merge.narmd_mergeKd$max_ann_Kd*tempBydepth_MAIR.merge.narmd_mergeKd$depth)
tempBydepth_MAIR.merge.narmd_mergeKd


tempBydepth_MAIR_MAKD.merge.NArmd<- tempBydepth_MAIR.merge.narmd_mergeKd %>% na.omit()
tempBydepth_MAIR_MAKD.merge.NArmd
length(unique(tempBydepth_MAIR_MAKD.merge.NArmd$site_id)) ### the final data has 547 unique lakes

### Now merge it with MN DOWLKNUM ids...
MN_DDnIRRbyDEPTH=left_join(tempBydepth_MAIR_MAKD.merge.NArmd, mn.ids2, by="site_id")
MN_DDnIRRbyDEPTH
length(unique(MN_DDnIRRbyDEPTH$site_id))
MN_DDnIRRbyDEPTH$DOWLKNUM=sub(".{6}","", MN_DDnIRRbyDEPTH$MNDOW_ID)
length(unique(MNDOW_DDnIRRbyDEPTH$DOWLKNUM))
MN_DDnIRRbyDEPTH

MNDOW_DDnIRRbyDEPTH =MN_DDnIRRbyDEPTH%>%
select(DOWLKNUM, everything())
MNDOW_DDnIRRbyDEPTH
write_csv(MNDOW_DDnIRRbyDEPTH, "Data/MN_GDDnLDbyDepth.csv")


#########################################################################################################################
### Plot a random sample of 12 lakes
MNDOW_DDnIRRbyDEPTH2=read_csv("Data/MN_GDDnLDbyDepth.csv")
MNDOW_DDnIRRbyDEPTH2
twelve.lakes= sample(unique(MN_DDnIRRbyDEPTH$MNDOW_ID),12)
twelve.lakes

twelve.plots=MN_DDnIRRbyDEPTH %>%
filter(MNDOW_ID %in% twelve.lakes)
twelve.plots

### Finally, visualize the twelve lakes DD and IRRADIANCE by Depth
ggplot(twelve.plots, aes(depth, DD, group=Year, colour=Year))+geom_path()+theme_bw()+facet_wrap(~MNDOW_ID, scales="free_x")+
scale_colour_viridis_c()+
theme(panel.grid = element_blank(), strip.text = element_text(size=10), axis.text = element_text(size=10), legend.position="top")
ggsave("twelvelakes_example_depth_DD.png", height=8, width=12, units="in")

ggplot(twelve.plots, aes(depth, Irrad, group=Year, colour=Year))+geom_path()+theme_bw()+facet_wrap(~MNDOW_ID, scales="free_x")+
scale_colour_viridis_c()+theme(panel.grid = element_blank(), strip.text = element_text(size=10), axis.text = element_text(size=10), legend.position="top")
ggsave("twelve_lakes_example_depth_Irr.png", height=8, width=12, units="in")


#### Another take with just three lakes
three.lakes= sample(unique(MNDOW_DDnIRRbyDEPTH$MNDOW_ID),3)
three.lakes

three.plots=MNDOW_DDnIRRbyDEPTH %>%
  filter(MNDOW_ID %in% three.lakes)
three.plots

ggplot(three.plots, aes(depth, DD, group=Year, colour=Year))+geom_path()+theme_bw()+facet_wrap(~MNDOW_ID, scales="free_x")+
  scale_colour_viridis_c()+
  theme(panel.grid = element_blank(), strip.text = element_text(size=10), axis.text = element_text(size=10), legend.position="top")




########################################################################################################################
########################################################################################################################
#### Some exploration and testing of single lake-specifc data
irrad.data.sample=read_csv("WithinLakeData/IrradianceData/AllIrradianceData/pb0_nhdhr_34119747_irradiance.csv")
head(irrad.data.sample)

irrad.data.2000sample=irrad.data.sample %>%
filter(irrad.data.sample$date > "1999-12-31")
irrad.data.2000sample

irrad.data.2000sample.dates=irrad.data.2000sample%>%
dplyr::mutate(year = lubridate::year(date),
month = lubridate::month(date),
day = lubridate::day(date),
yearday=lubridate::yday(date))
tail(irrad.data.2000sample.dates)
head(irrad.data.2000sample.dates)
ggplot(data=irrad.data.2000sample.dates, aes(x=yearday, y=rad_0, color=as.factor(year)))+
geom_line()

### A smaller sample: between years 2000 and 2009
irrad.data.2000_2009sample.dates=irrad.data.2000sample.dates%>%
filter(irrad.data.2000sample.dates$year<2010)
irrad.data.2000_2009sample.dates
ggplot(data=irrad.data.2000_2009sample.dates, aes(x=yearday, y=rad_0, color=as.factor(year)))+
geom_line(color="gray")
ggplot(data=irrad.data.2000_2009sample.dates, aes(x=yearday, y=rad_0, color=as.factor(year)))+
geom_line(color="gray")+
geom_smooth(method="gam", se=FALSE)
########################################################################################################################

library(tidyverse)
library(reshape2)
library(ggplot2)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data/WithinLakeData/TempData")

temp.files=list.files("All_PGDL_temp_predictions")
head(temp.files)

mn.ids2=read_csv("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data/mndow_nhdhr_xwalk.csv")
head(mn.ids2)
length(mn.ids2$MNDOW_ID)
mn.ids=unique(na.omit(mn.ids2$site_id))
head(mn.ids)
mn.files=data.frame("site_id"=(mn.ids), file.name=NA)

for(i in 1:length(mn.ids))
{
mn.files$file.name[i]=paste("pgdl_", mn.ids[i],"_temperatures.csv",sep="")
}
head(mn.files)
available.temp.files=mn.files[mn.files$file.name%in%temp.files,]
head(available.temp.files)
length(available.temp.files$file.name)

#### Lets extract a subset of data from each of the csv files 
### The subset here is only years 2000 onwards
extract_and_save_from_dataset <- function(csvfile) {
  df <- read_csv(csvfile)
  df_short=subset(df, date > as.Date("1999-12-31") )
  df_short$Year=as.numeric(format(df_short$date, "%Y"))
  csvfile_short <- gsub(".csv", "_short.csv", csvfile)
  write_csv(df_short, path="~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data/WithinLakeData/TempData"
            , csvfile_short)
}

filenames=available.files$file.name
head(filenames)

setwd("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data/WithinLakeData/TempData/All_PGDL_temp_predictions")

  for (csvfile in filenames){
  extract_and_save_from_dataset(csvfile)
  }



year.list=seq(2000, 2018, 1)
year.list
dds=data.frame("Year"=0, "depth"=0,"DD"=0, "site_id"=0)
dds

short.files=list.files("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data/WithinLakeData/TempData/All_PGDL_temp_short/")
head(short.files)
tail(short.files)

for(i in 1:nrow(available.temp.files)) 
  {
  current=read_csv(paste("~/UMNpostdoc/ProjectEWM/RProjects/JSDMs_hmsc/Data/WithinLakeData/TempData/All_PGDL_temp_short/",
                         short.files[i], sep=""))
  depth.list=colnames(current)[2:(length(current)-1)]
  depth.list= as.numeric(sub("temp_*", "", depth.list))
    
    for (j in 1:length(year.list)) {
      
      current.year=current[current$Year==year.list[j],]
      
      for (k in 1:length(depth.list)) { 
        to.add=data.frame("Year"=year.list[j], "depth"=depth.list[k], "DD"=NA, "site_id"=available.temp.files$site_id[i])
        #calculate DD for the year
        #hack to bypass NA probs
        if(is.na(current.year[1,k+1])) dds==NA else
          temp=current.year[,k+1]-5
        temp[temp<0]=0
        to.add$DD=sum(temp)
        dds=rbind(dds, to.add)
        
        }
    }
}


dds2=dds[2:nrow(dds),]

write.csv(dds2, "depth_DD_example.csv")

dds.MN=merge(dds, mn.ids2, by="site_id")
head(dds.MN)


dds3=dds2 %>%
group_by(Year,site_id) %>%
complete(depth = seq(first(depth), last(depth), by=0.1)) %>%
mutate(DD = na.approx(DD))
head(dds3)

dds3

write_csv(dds3, "dds_final.csv")


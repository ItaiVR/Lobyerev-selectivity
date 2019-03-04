#load packages
library(tidyr)
library(reshape)
library(dplyr)
#Import catch data for manipulation to the R package format
Catch.data.Cod <- read.csv("~/kinneret modeling/selectivity/R code Feodor paper/Lobyerev-selectivity/Data/Catch data Cod.csv", stringsAsFactors=FALSE)
#transform to long format
data_long <- gather(Catch.data.Cod, catch_method, individuals, Wedged:Tangled, factor_key=TRUE)
data_long=data_long[!is.na(data_long$individuals),]
#Undo aggregation
data_long_raw=untable(data_long[,1:3], num = data_long[,4])

##Add maximal girth
#Import and transform data
G_max_wide <- read.csv("~/kinneret modeling/selectivity/R code Feodor paper/Lobyerev-selectivity/Data/G_max_wide.csv", stringsAsFactors=FALSE)
#Transform to long format
G_max_long=gather(G_max_wide, t, G_max, m1:m73, factor_key=TRUE)
#Arrange table
#Drop NA
G_max_long=G_max_long[!is.na(G_max_long$length_group),]
G_max_long=G_max_long[!is.na(G_max_long$G_max),]
#Drop the second column 't'
G_max_long=G_max_long[ , !names(G_max_long) %in% c("t")]
#Order by first column
G_max_long=G_max_long[order(G_max_long$length_group),]

#add to data_long_raw
data_long_raw$maximal_girth=NA

for (r in 1:nrow(data_long_raw)){
  size_group_c=data_long_raw[r,"Length_group"]
  #maximal girth coresponding to this length_group
  gmax_vec=G_max_long[G_max_long$length_group==size_group_c,"G_max"]
  #sample value for data_long_raw
  data_long_raw[r,"maximal_girth"]=sample(gmax_vec,1)
}

##add trial index
data_long_raw$survey_index=sample(c(1:11),nrow(data_long_raw),replace = T)

###Add net name
data_long_raw$Net_name=paste("Net",as.character(data_long_raw$Mesh_size),sep="_")


###Reorder and rename
data_long_raw_edit= data_long_raw %>%
  select(survey_index,Net_name,Mesh_size,Length_group,maximal_girth,catch_method) 

colnames(data_long_raw_edit)=c("Survey_index","Net_name","Mesh_size","Length_FL","Maximal_girth",
                               "Catch_method" ) 
data_long_raw_edit=data_long_raw_edit[order(data_long_raw_edit$Survey_index),]
#Save data 
write.csv(data_long_raw_edit,"~/kinneret modeling/selectivity/R code Feodor paper/Lobyerev-selectivity/Data/Cod_selectivity_data.csv",row.names =F)
nrow(data_long_raw_edit)

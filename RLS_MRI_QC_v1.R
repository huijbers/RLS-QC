# By Willem Huijbers 29/4/2016
library(dplyr); library(tidyr); library(ggplot2);
source('rs-qa-fun.r')

load('MRI_metadata2016-06-28.Rdata') #should already only contain "REAL" data

#MRIcontrareport<- subset(MRIcontrareport,MRIcontrareport$metadata_pt_test == 'REAL')
#MRIvisitreport<- subset(MRIvisitreport,MRIvisitreport$metadata_pt_test == 'REAL')

MRIreport <- merge(MRIcontrareport,MRIvisitreport, by.x = 'metadata_pt_sicn', by.y = 'metadata_pt_sicn')
library(plyr)
library(dplyr)
library(reshape2)


# this file will use the file
#2013-14_KEI_DistrictByDomain.csv
#and create separate files for each of the domains (skills tested) of the KEI data.
# these files will have same structure as each of the files for the other years for ease of handling the data in loops.
# NOTE:
#      There is no number of students tested with the 2014 data.

# Kindergarten inventory work directory
#####  

# setting working directory into github structure
# working with github as no suppression issue here.

rm(list = ls())
setwd("D:/Data/GitHub/KEI")

kei2014all <- read.csv("2013-14_KEI_DistrictByDomain.csv", header=TRUE, stringsAsFactors=FALSE, skip=1, strip.white=TRUE,check.names = FALSE) #nrows=171,

names(kei2014all) <- c("District",
                       "Language Skills (%) Level 1","Language Skills (%) Level 2","Language Skills (%) Level 3",
                       "Literacy Skills (%) Level 1", "Literacy Skills (%) Level 2", "Literacy Skills (%) Level 3",
                       "Numeracy Skills (%) Level 1","Numeracy Skills (%) Level 2","Numeracy Skills (%) Level 3",
                       "Physical/Motor Skills (%) Level 1","Physical/Motor Skills (%) Level 2","Physical/Motor Skills (%) Level 3",
                       "Creative/Aesthetic Skills (%) Level 1","Creative/Aesthetic Skills (%) Level 2","Creative/Aesthetic Skills (%) Level 3",
                       "Personal/Social Skills (%) Level 1","Personal/Social Skills (%) Level 2","Personal/Social Skills (%) Level 3"
                       )
# to be consistent in handling data add "number of students tested field.  No data here. Don't try to estimate
kei2014all$"Number of Students Tested"<- ""
kei2014all$"School Year"<- "2013-14"
kei2014all$DistrictID <- ""
kei2014all<-kei2014all[,c(22,1,21,20,2:19)]

#select columns and write out csv files
kei2014Lang <- kei2014all[,c(1:4,5:7)]
write.csv(kei2014Lang,"2013-14_KEI_Language.csv",row.names=FALSE,quote=FALSE)

kei2014Lit <- kei2014all[,c(1:4,8:10)]
write.csv(kei2014Lit,"2013-14_KEI_Literacy.csv",row.names=FALSE,quote=FALSE)

kei2014Num <- kei2014all[,c(1:4,11:13)]
write.csv(kei2014Num,"2013-14_KEI_Numeracy.csv",row.names=FALSE,quote=FALSE)

kei2014Phys <- kei2014all[,c(1:4,14:16)]
write.csv(kei2014Phys,"2013-14_KEI_Physical.csv",row.names=FALSE,quote=FALSE)

kei2014Creat <- kei2014all[,c(1:4,17:19)]
write.csv(kei2014Creat,"2013-14_KEI_Creative.csv",row.names=FALSE,quote=FALSE)

kei2014Pers <- kei2014all[,c(1:4,20:22)]
write.csv(kei2014Pers,"2013-14_KEI_Personal.csv",row.names=FALSE,quote=FALSE)


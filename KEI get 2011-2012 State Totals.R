library(plyr)
library(dplyr)
library(reshape2)


# 4-9-15 DPS
# get state totals for 2011-2012 for each domain.  No data for state for 2013-2014
#Variable  <- "Kindergarten Entrance Inventory Results"


# now working in github structure
# Kindergarten inventory work directory

#setwd("D:/Data/CTData/School District/KEI")
setwd("D:/Data/GitHub/KEI")

# Code split into 6 sections.  
# These  6 sections are for domain data at state level
# 6 sections are for the skills - 1: Literacy, 2: Language, 3: Creative, 4: Personal, 5: Numeracy 6: physical

# Don't need to remove this dataframe because the first use of it is when it is created.
#rm(KEIyrsvars))


# number of districts changes from 171 in 2009 to 167 in 2010 and 11  I will drop nrows = to let R find number

# Section 1 Literacy 2009, 10, 11
# code will loop through these 4 files.
rm(list = ls())
filename <- c(
  "State_2008-2012_Kindergarten_Entrance_Inventory_Results_-_Creative-Aesthetic_Skills.csv",
  "State_2008-2012_Kindergarten_Entrance_Inventory_Results_-_Language_Skills.csv",
  "State_2008-2012_Kindergarten_Entrance_Inventory_Results_-_Literacy_Skills.csv",
  "State_2008-2012_Kindergarten_Entrance_Inventory_Results_-_Numeracy_Skills.csv",
  "State_2008-2012_Kindergarten_Entrance_Inventory_Results_-_Personal-Social_Skills.csv",
  "State_2008-2012_Kindergarten_Entrance_Inventory_Results_-_Physical-Motor_Skills.csv"
)

# open full data
cc <- c(rep("character",7),"numeric")
KEI<- read.csv("kindergarten-entrance-inventory_2008-09to2013-14_no11_12ST.csv", header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE,colClasses=cc)
#KEI<- read.csv("kindergarten-entrance-inventory 2009-09to2013-14.csv", header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE)

# domain loop
#i=1
for (i in 1:6) {
  CT_KEI <- read.csv(filename[i], header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE, nrows=1)
  names(CT_KEI)[3:6] <- c("Total Tested","Level 1","Level 2","Level 3")
  CT_KEI$District <- "Connecticut"
  CT_KEI$FIPS <- "09"
  CT_KEI$Year <- "2011-2012"
  CT_KEI <- CT_KEI[,c(7,8,9,3,4,5,6)]
  CT_KEI$NLevel1 <- round(CT_KEI$"Total Tested"*CT_KEI$"Level 1"/100,0)
  CT_KEI$NLevel2 <- round(CT_KEI$"Total Tested"*CT_KEI$"Level 2"/100,0)
  CT_KEI$NLevel3 <- round(CT_KEI$"Total Tested"*CT_KEI$"Level 3"/100,0)
  CT_KEI.melt <- melt(CT_KEI, id.vars = c("District", "FIPS","Year"))
  names(CT_KEI.melt)[c(4,5)] <- c("Skill Level","Value")
  
  CT_KEI.melt$"Measure Type" <- sapply(CT_KEI.melt$"Skill Level", function(x){
    if ((x)=="Total Tested") {
      return("Number")
    } else if ((x)=="NLevel1") {
      return("Number")
    } else if ((x)=="NLevel2") {
      return("Number")
    } else if ((x)=="NLevel3") {
      return("Number")
    } else {return("Percent")}
  })
  CT_KEI.melt$"Skill Level"[CT_KEI.melt$"Skill Level"=="NLevel1"]<-"Level 1"
  CT_KEI.melt$"Skill Level"[CT_KEI.melt$"Skill Level"=="NLevel2"]<-"Level 2"
  CT_KEI.melt$"Skill Level"[CT_KEI.melt$"Skill Level"=="NLevel3"]<-"Level 3"

  if (i==1) {CT_KEI.melt$Domain <- "Creative/Aesthetic"}
  if (i==2) {CT_KEI.melt$Domain <- "Language"}
  if (i==3) {CT_KEI.melt$Domain <- "Literacy"}
  if (i==4) {CT_KEI.melt$Domain <- "Numeracy"}
  if (i==5) {CT_KEI.melt$Domain <- "Personal/Social"}
  if (i==6) {CT_KEI.melt$Domain <- "Physical/Motor"}

  CT_KEI.melt$Variable <- "Kindergarten Entrance Inventory Results"
  CT_KEI.melt.col <- CT_KEI.melt[,c(1:2,3,7,4,6,8,5)]
  if (i==1) {STTotKEI <- CT_KEI.melt.col
    } else {STTotKEI <- rbind(STTotKEI,CT_KEI.melt.col)}
}

View (STTotKEI)
KEIFull <- rbind(STTotKEI,KEI)

write.csv(KEIFull,"kindergarten-entrance-inventory 2008-09to2013-14.csv",quote=FALSE, row.names=FALSE)

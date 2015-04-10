library(plyr)
library(dplyr)
library(reshape2)

#Fixes 4-9-15
#Variable  <- "Kindergarten Entrance Inventory Results"
# domain refers to subject area  Originally I used "Skill Type"
# value to be Value with cap.

rm(list = ls())

# now working in github structure
# Kindergarten inventory work directory

#setwd("D:/Data/CTData/School District/KEI")
setwd("D:/Data/GitHub/KEI")

# Code split into 6 sections.  
# These  6 sections are for loops of 4 with years as incrementing loop
# 6 sections are for the skills - 1: Literacy, 2: Language, 3: Creative, 4: Personal, 5: Numeracy 6: physical


# number of districts changes from 171 in 2009 to 167 in 2010 and 11  I will not use in the read.csv nrows = *?. Let R find number

#################

# Section 1 Literacy 
# code will loop through these 4 files.
SD2Twn <- read.csv("CorrectedDistricts4KEI_full.csv",header=TRUE,stringsAsFactors=FALSE, skip=0, strip.white=TRUE)#,nrows=101)
names (SD2Twn) <- c("District","CorrectedDistrict","FIPS")

filename <- c(
  "2008-09_KEI_Literacy.csv",
  "2009-10_KEI_Literacy.csv",
  "2010-11_KEI_Literacy.csv",
  "2011-12_KEI_Literacy.csv",
  "2012-13_KEI_Literacy.csv",
  "2013-14_KEI_Literacy.csv"
)

# Year loop
i=1
for (i in 1:6) {
  KEI <- read.csv(filename[i], header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE) #nrows=171,
  names (KEI) [2] <- "District"
  names (KEI) [3] <- "Year"
  names (KEI) [5] <- "Literacy Skills (%) Level 1"
  names (KEI) [6] <- "Literacy Skills (%) Level 2"
  names (KEI) [7] <- "Literacy Skills (%) Level 3"
  KEI <- KEI[,c(1:7)]
    
  KEI$District <- as.character(gsub(",","",KEI$District))
  
  KEI.join <- join(KEI, SD2Twn, by = "District")
  rm (KEI)
  
  KEI.join <- KEI.join[c(8,9,3,4,5,6,7)]
  names(KEI.join)[1]<- "District"
  
  KEI.join$District <- as.character(gsub(",","",KEI.join$District))
  
  KEI.join$FIPS <- sapply (KEI.join$FIPS, function (x) {
    if (is.na(x)) {
      return("")
    } else {
      return(paste("0",x,sep=""))
    }
  })
  
  #Deal with unique domain 
  KEI.join$"Domain" <- rep("Literacy")
  KEI.join <- KEI.join[c(1,2,3,8,4,5,6,7)]
  if (i<6) {
    KEI.join$"Num Level 1" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Literacy Skills (%) Level 1"/100)
    KEI.join$"Num Level 2" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Literacy Skills (%) Level 2"/100)
    KEI.join$"Num Level 3" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Literacy Skills (%) Level 3"/100)
    
    KEI.nums <- KEI.join[,c(1,2,3,4,9,10,11)]
    #melt the numbers data 
    KEI.meltnums <- melt(KEI.nums, id.vars=c("FIPS","District","Year","Domain"))
    # add metadata for vaiable and measure type
    KEI.meltnums$"Measure Type" <- rep("Number")
    KEI.meltnums$Variable <- rep("Kindergarten Entrance Inventory Results")
    #clean up variable names
    names(KEI.meltnums)[c(5,6)]<- c("Skill Level","Value")
    
    #Fix skill level data on metadata
    KEI.meltnums$"Skill Level" <- sapply(KEI.meltnums$"Skill Level", function(y) {
      if (y=="Num Level 1") {
        "Level 1"
      } else if (y=="Num Level 2") {
        "Level 2"
      } else if (y=="Num Level 3") {
        "Level 3"
      } else {"PROBLEM Column Header Numbers for Literacy"}            
    })  
    
    # following code commented out.  Problem with literacy numbers not showing up.
    KEI.cols.nums <- KEI.meltnums [c(2,1,3,4,5,7,8,6)]
    #}
    
  }
  
  # get the percents data
  KEI.pcs <- KEI.join[,c(1:8)]
  
  # melt the percents and mark the lines as either being number (for total number tested or percents)
  KEI.meltpcs <- melt(KEI.pcs, id.vars=c("FIPS","District","Year","Domain"))
  KEI.meltpcs$"Measure Type" <- sapply(KEI.meltpcs$variable, function(x) {
    if (x =="Number of Students Tested"){
      "Number" 
    } else {"Percent"}
  })
  
  # clean up vars  add metadata and restructure
  names(KEI.meltpcs)[c(5,6)]<- c("Skill Level","Value")
  KEI.meltpcs$Variable <- rep("Kindergarten Entrance Inventory Results")
  KEI.cols.pcs <- KEI.meltpcs[c(2,1,3,4,5,7,8,6)]
  
  # reset metadata for skill level
  KEI.cols.pcs$"Skill Level" <- sapply(KEI.cols.pcs$"Skill Level", function(y) {
    if (y=="Literacy Skills (%) Level 1") {
      "Level 1"
    } else if (y=="Literacy Skills (%) Level 2") {
      "Level 2"
    } else if (y=="Literacy Skills (%) Level 3") {
      "Level 3"
    } else if (y=="Number of Students Tested") {
      "Total Tested"
    } else {"PROBLEM Column Header Literacy"}            
  })
  
  #append the numbers data to the percent data for 1 year and 1 variable
  if (i<6) {
    KEIbound1yr <- rbind(KEI.cols.pcs,KEI.cols.nums)  
  } else {KEIbound1yr <- KEI.cols.pcs}
  
  
  # specify the year value based on the count of the files coming in.
  if (i ==1) {
    KEIbound1yr$Year <- "2008-2009"  
    KEIallyrs1var <- KEIbound1yr
  } else if (i==2)   {
    KEIbound1yr$Year <- "2009-2010"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==3)   {
    KEIbound1yr$Year <- "2010-2011"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==4)   {
    KEIbound1yr$Year <- "2011-2012"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==5) {
    KEIbound1yr$Year <- "2012-2013"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else {
    KEIbound1yr$Year <- "2013-2014"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  }
  #end of for conditional dealing with 6 years of data
  #end of loop for 1 domain time next bracket
}

# Create a dataframe to create all variables
KEIyrsvars <- KEIallyrs1var



### Section 1 Literacy 2009-2011 complete and in KEIbound1yr
###  Data also in KEIyrsvars to store data for all variables and years


############################################################################
############################################################################


### SECTION 2 Language 2009-2013
filename <- c(
  "2008-09_KEI_Language.csv",
  "2009-10_KEI_Language.csv",
  "2010-11_KEI_Language.csv",
  "2011-12_KEI_Language.csv",
  "2012-13_KEI_Language.csv",
  "2013-14_KEI_Language.csv"
)
for (i in 1:6) {
  KEI <- read.csv(filename[i], header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE) #nrows=171,
  names (KEI) [2] <- "District"
  names (KEI) [3] <- "Year"
  names (KEI) [5] <- "Language Skills (%) Level 1"
  names (KEI) [6] <- "Language Skills (%) Level 2"
  names (KEI) [7] <- "Language Skills (%) Level 3"
  KEI <- KEI[,c(1:7)]
  KEI$District <- as.character(gsub(",","",KEI$District))
  
  KEI.join <- join(KEI, SD2Twn, by = "District")
  rm (KEI)
  
  KEI.join <- KEI.join[c(8,9,3,4,5,6,7)]
  names(KEI.join)[1]<- "District"
  
  KEI.join$FIPS <- sapply (KEI.join$FIPS, function (x) {
    if (is.na(x)) {
      return("")
    } else {
      return(paste("0",x,sep=""))
    }
  })
  
  #Deal with unique domain 
  KEI.join$"Domain" <- rep("Language")
  KEI.join <- KEI.join[c(1,2,3,8,4,5,6,7)]
  if (i<6) {
    KEI.join$"Num Level 1" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Language Skills (%) Level 1"/100)
    KEI.join$"Num Level 2" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Language Skills (%) Level 2"/100)
    KEI.join$"Num Level 3" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Language Skills (%) Level 3"/100)
    
    KEI.nums <- KEI.join[,c(1,2,3,4,9,10,11)]
    #melt the numbers data 
    KEI.meltnums <- melt(KEI.nums, id.vars=c("FIPS","District","Year","Domain"))
    # add metadata for vaiable and measure type
    KEI.meltnums$"Measure Type" <- rep("Number")
    KEI.meltnums$Variable <- rep("Kindergarten Entrance Inventory Results")
    #clean up variable names
    names(KEI.meltnums)[c(5,6)]<- c("Skill Level","Value")
    
    #Fix skill level data on metadata
    KEI.meltnums$"Skill Level" <- sapply(KEI.meltnums$"Skill Level", function(y) {
      if (y=="Num Level 1") {
        "Level 1"
      } else if (y=="Num Level 2") {
        "Level 2"
      } else if (y=="Num Level 3") {
        "Level 3"
      } else {"PROBLEM Column Header Numbers for Language"}            
    })  
    
    # following code commented out.  Problem with Language numbers not showing up.
    KEI.cols.nums <- KEI.meltnums [c(2,1,3,4,5,7,8,6)]
    #}
    
  }
  
  # get the percents data
  KEI.pcs <- KEI.join[,c(1:8)]
  
  # melt the percents and mark the lines as either being number (for total number tested or percents)
  KEI.meltpcs <- melt(KEI.pcs, id.vars=c("FIPS","District","Year","Domain"))
  KEI.meltpcs$"Measure Type" <- sapply(KEI.meltpcs$variable, function(x) {
    if (x =="Number of Students Tested"){
      "Number" 
    } else {"Percent"}
  })
  
  # clean up vars  add metadata and restructure
  names(KEI.meltpcs)[c(5,6)]<- c("Skill Level","Value")
  KEI.meltpcs$Variable <- rep("Kindergarten Entrance Inventory Results")
  KEI.cols.pcs <- KEI.meltpcs[c(2,1,3,4,5,7,8,6)]
  
  # reset metadata for skill level
  KEI.cols.pcs$"Skill Level" <- sapply(KEI.cols.pcs$"Skill Level", function(y) {
    if (y=="Language Skills (%) Level 1") {
      "Level 1"
    } else if (y=="Language Skills (%) Level 2") {
      "Level 2"
    } else if (y=="Language Skills (%) Level 3") {
      "Level 3"
    } else if (y=="Number of Students Tested") {
      "Total Tested"
    } else {"PROBLEM Column Header Language"}            
  })
  
  #append the numbers data to the percent data for 1 year and 1 variable
  if (i<6) {
    KEIbound1yr <- rbind(KEI.cols.pcs,KEI.cols.nums)  
  } else {KEIbound1yr <- KEI.cols.pcs}
  
  
  # specify the year value based on the count of the files coming in.
  if (i ==1) {
    KEIbound1yr$Year <- "2008-2009"  
    KEIallyrs1var <- KEIbound1yr
  } else if (i==2)   {
    KEIbound1yr$Year <- "2009-2010"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==3)   {
    KEIbound1yr$Year <- "2010-2011"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==4)   {
    KEIbound1yr$Year <- "2011-2012"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==5) {
    KEIbound1yr$Year <- "2012-2013"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else {
    KEIbound1yr$Year <- "2013-2014"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  }
  #end of for conditional dealing with 6 years of data
  #end of loop for 1 domain time next bracket
}

# append to dataframe established in section 1
KEIyrsvars <- rbind (KEIyrsvars,KEIallyrs1var)
# end of section 2 Language

############################################################################
############################################################################


### SECTION 3 Creative 2009-2011
filename <- c(
  "2008-09_KEI_Creative.csv",
  "2009-10_KEI_Creative.csv",
  "2010-11_KEI_Creative.csv",
  "2011-12_KEI_Creative.csv",
  "2012-13_KEI_Creative.csv",
  "2013-14_KEI_Creative.csv"
)
for (i in 1:6) {
  KEI <- read.csv(filename[i], header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE) #nrows=171,
  names (KEI) [2] <- "District"
  names (KEI) [3] <- "Year"
  names (KEI) [5] <- "Creative/Aesthetic Skills (%) Level 1"
  names (KEI) [6] <- "Creative/Aesthetic Skills (%) Level 2"
  names (KEI) [7] <- "Creative/Aesthetic Skills (%) Level 3"
  KEI <- KEI[,c(1:7)]
  KEI$District <- as.character(gsub(",","",KEI$District))
  
  KEI.join <- join(KEI, SD2Twn, by = "District")
  rm (KEI)
  
  KEI.join <- KEI.join[c(8,9,3,4,5,6,7)]
  names(KEI.join)[1]<- "District"
  
  KEI.join$FIPS <- sapply (KEI.join$FIPS, function (x) {
    if (is.na(x)) {
      return("")
    } else {
      return(paste("0",x,sep=""))
    }
  })
  
  #Deal with unique domain 
  KEI.join$"Domain" <- rep("Creative/Aesthetic")
  KEI.join <- KEI.join[c(1,2,3,8,4,5,6,7)]
  if (i<6) {
    KEI.join$"Num Level 1" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Creative/Aesthetic Skills (%) Level 1"/100)
    KEI.join$"Num Level 2" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Creative/Aesthetic Skills (%) Level 2"/100)
    KEI.join$"Num Level 3" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Creative/Aesthetic Skills (%) Level 3"/100)
    
    KEI.nums <- KEI.join[,c(1,2,3,4,9,10,11)]
    #melt the numbers data 
    KEI.meltnums <- melt(KEI.nums, id.vars=c("FIPS","District","Year","Domain"))
    # add metadata for vaiable and measure type
    KEI.meltnums$"Measure Type" <- rep("Number")
    KEI.meltnums$Variable <- rep("Kindergarten Entrance Inventory Results")
    #clean up variable names
    names(KEI.meltnums)[c(5,6)]<- c("Skill Level","Value")
    
    #Fix skill level data on metadata
    KEI.meltnums$"Skill Level" <- sapply(KEI.meltnums$"Skill Level", function(y) {
      if (y=="Num Level 1") {
        "Level 1"
      } else if (y=="Num Level 2") {
        "Level 2"
      } else if (y=="Num Level 3") {
        "Level 3"
      } else {"PROBLEM Column Header Numbers for Creative/Aesthetic"}            
    })  
    
    # following code commented out.  Problem with Creative/Aesthetic numbers not showing up.
    KEI.cols.nums <- KEI.meltnums [c(2,1,3,4,5,7,8,6)]
    #}
    
  }
  
  # get the percents data
  KEI.pcs <- KEI.join[,c(1:8)]
  
  # melt the percents and mark the lines as either being number (for total number tested or percents)
  KEI.meltpcs <- melt(KEI.pcs, id.vars=c("FIPS","District","Year","Domain"))
  KEI.meltpcs$"Measure Type" <- sapply(KEI.meltpcs$variable, function(x) {
    if (x =="Number of Students Tested"){
      "Number" 
    } else {"Percent"}
  })
  
  # clean up vars  add metadata and restructure
  names(KEI.meltpcs)[c(5,6)]<- c("Skill Level","Value")
  KEI.meltpcs$Variable <- rep("Kindergarten Entrance Inventory Results")
  KEI.cols.pcs <- KEI.meltpcs[c(2,1,3,4,5,7,8,6)]
  
  # reset metadata for skill level
  KEI.cols.pcs$"Skill Level" <- sapply(KEI.cols.pcs$"Skill Level", function(y) {
    if (y=="Creative/Aesthetic Skills (%) Level 1") {
      "Level 1"
    } else if (y=="Creative/Aesthetic Skills (%) Level 2") {
      "Level 2"
    } else if (y=="Creative/Aesthetic Skills (%) Level 3") {
      "Level 3"
    } else if (y=="Number of Students Tested") {
      "Total Tested"
    } else {"PROBLEM Column Header Creative/Aesthetic"}            
  })
  
  #append the numbers data to the percent data for 1 year and 1 variable
  if (i<6) {
    KEIbound1yr <- rbind(KEI.cols.pcs,KEI.cols.nums)  
  } else {KEIbound1yr <- KEI.cols.pcs}
  
  
  # specify the year value based on the count of the files coming in.
  if (i ==1) {
    KEIbound1yr$Year <- "2008-2009"  
    KEIallyrs1var <- KEIbound1yr
  } else if (i==2)   {
    KEIbound1yr$Year <- "2009-2010"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==3)   {
    KEIbound1yr$Year <- "2010-2011"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==4)   {
    KEIbound1yr$Year <- "2011-2012"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==5) {
    KEIbound1yr$Year <- "2012-2013"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else {
    KEIbound1yr$Year <- "2013-2014"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  }
  #end of for conditional dealing with 6 years of data
  #end of loop for 1 domain time next bracket
}

# append to dataframe established in section 1
KEIyrsvars <- rbind (KEIyrsvars,KEIallyrs1var)
# section 3 completed

############################################################################
############################################################################


### SECTION 4 Personal 2009-2011
filename <- c(
  "2008-09_KEI_Personal.csv",
  "2009-10_KEI_Personal.csv",
  "2010-11_KEI_Personal.csv",
  "2011-12_KEI_Personal.csv",
  "2012-13_KEI_Personal.csv",
  "2013-14_KEI_Personal.csv"
)
for (i in 1:6) {
  KEI <- read.csv(filename[i], header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE) #nrows=171,
  names (KEI) [2] <- "District"
  names (KEI) [3] <- "Year"
  names (KEI) [5] <- "Personal/Social Skills (%) Level 1"
  names (KEI) [6] <- "Personal/Social Skills (%) Level 2"
  names (KEI) [7] <- "Personal/Social Skills (%) Level 3"
  KEI <- KEI[,c(1:7)]
  KEI$District <- as.character(gsub(",","",KEI$District))
  
  KEI.join <- join(KEI, SD2Twn, by = "District")
  rm (KEI)
  
  KEI.join <- KEI.join[c(8,9,3,4,5,6,7)]
  names(KEI.join)[1]<- "District"
  
  KEI.join$FIPS <- sapply (KEI.join$FIPS, function (x) {
    if (is.na(x)) {
      return("")
    } else {
      return(paste("0",x,sep=""))
    }
  })
  
  #Deal with unique domain 
  KEI.join$"Domain" <- rep("Personal/Social")
  KEI.join <- KEI.join[c(1,2,3,8,4,5,6,7)]
  if (i<6) {
    KEI.join$"Num Level 1" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Personal/Social Skills (%) Level 1"/100)
    KEI.join$"Num Level 2" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Personal/Social Skills (%) Level 2"/100)
    KEI.join$"Num Level 3" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Personal/Social Skills (%) Level 3"/100)
    
    KEI.nums <- KEI.join[,c(1,2,3,4,9,10,11)]
    #melt the numbers data 
    KEI.meltnums <- melt(KEI.nums, id.vars=c("FIPS","District","Year","Domain"))
    # add metadata for vaiable and measure type
    KEI.meltnums$"Measure Type" <- rep("Number")
    KEI.meltnums$Variable <- rep("Kindergarten Entrance Inventory Results")
    #clean up variable names
    names(KEI.meltnums)[c(5,6)]<- c("Skill Level","Value")
    
    #Fix skill level data on metadata
    KEI.meltnums$"Skill Level" <- sapply(KEI.meltnums$"Skill Level", function(y) {
      if (y=="Num Level 1") {
        "Level 1"
      } else if (y=="Num Level 2") {
        "Level 2"
      } else if (y=="Num Level 3") {
        "Level 3"
      } else {"PROBLEM Column Header Numbers for Personal/Social"}            
    })  
    
    # following code commented out.  Problem with Personal/Social numbers not showing up.
    KEI.cols.nums <- KEI.meltnums [c(2,1,3,4,5,7,8,6)]
    #}
    
  }
  
  # get the percents data
  KEI.pcs <- KEI.join[,c(1:8)]
  
  # melt the percents and mark the lines as either being number (for total number tested or percents)
  KEI.meltpcs <- melt(KEI.pcs, id.vars=c("FIPS","District","Year","Domain"))
  KEI.meltpcs$"Measure Type" <- sapply(KEI.meltpcs$variable, function(x) {
    if (x =="Number of Students Tested"){
      "Number" 
    } else {"Percent"}
  })
  
  # clean up vars  add metadata and restructure
  names(KEI.meltpcs)[c(5,6)]<- c("Skill Level","Value")
  KEI.meltpcs$Variable <- rep("Kindergarten Entrance Inventory Results")
  KEI.cols.pcs <- KEI.meltpcs[c(2,1,3,4,5,7,8,6)]
  
  # reset metadata for skill level
  KEI.cols.pcs$"Skill Level" <- sapply(KEI.cols.pcs$"Skill Level", function(y) {
    if (y=="Personal/Social Skills (%) Level 1") {
      "Level 1"
    } else if (y=="Personal/Social Skills (%) Level 2") {
      "Level 2"
    } else if (y=="Personal/Social Skills (%) Level 3") {
      "Level 3"
    } else if (y=="Number of Students Tested") {
      "Total Tested"
    } else {"PROBLEM Column Header Personal/Social"}            
  })
  
  #append the numbers data to the percent data for 1 year and 1 variable
  if (i<6) {
    KEIbound1yr <- rbind(KEI.cols.pcs,KEI.cols.nums)  
  } else {KEIbound1yr <- KEI.cols.pcs}
  
  
  # specify the year value based on the count of the files coming in.
  if (i ==1) {
    KEIbound1yr$Year <- "2008-2009"  
    KEIallyrs1var <- KEIbound1yr
  } else if (i==2)   {
    KEIbound1yr$Year <- "2009-2010"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==3)   {
    KEIbound1yr$Year <- "2010-2011"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==4)   {
    KEIbound1yr$Year <- "2011-2012"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==5) {
    KEIbound1yr$Year <- "2012-2013"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else {
    KEIbound1yr$Year <- "2013-2014"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  }
  #end of for conditional dealing with 6 years of data
  #end of loop for 1 domain time next bracket
}

# append to dataframe established in section 1
KEIyrsvars <- rbind (KEIyrsvars,KEIallyrs1var)
# section 4 Personal Social completed

############################################################################
############################################################################


### SECTION 5 Numeracy 2009-2011
filename <- c(
  "2008-09_KEI_Numeracy.csv",
  "2009-10_KEI_Numeracy.csv",
  "2010-11_KEI_Numeracy.csv",
  "2011-12_KEI_Numeracy.csv",
  "2012-13_KEI_Numeracy.csv",
  "2013-14_KEI_Numeracy.csv"
)
for (i in 1:6) {
  KEI <- read.csv(filename[i], header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE) #nrows=171,
  names (KEI) [2] <- "District"
  names (KEI) [3] <- "Year"
  names (KEI) [5] <- "Numeracy Skills (%) Level 1"
  names (KEI) [6] <- "Numeracy Skills (%) Level 2"
  names (KEI) [7] <- "Numeracy Skills (%) Level 3"
  KEI <- KEI[,c(1:7)]
  KEI$District <- as.character(gsub(",","",KEI$District))
  
  KEI.join <- join(KEI, SD2Twn, by = "District")
  rm (KEI)
  
  KEI.join <- KEI.join[c(8,9,3,4,5,6,7)]
  names(KEI.join)[1]<- "District"
  
  KEI.join$FIPS <- sapply (KEI.join$FIPS, function (x) {
    if (is.na(x)) {
      return("")
    } else {
      return(paste("0",x,sep=""))
    }
  })
  
  #Deal with unique domain 
  KEI.join$"Domain" <- rep("Numeracy")
  KEI.join <- KEI.join[c(1,2,3,8,4,5,6,7)]
  if (i<6) {
    KEI.join$"Num Level 1" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Numeracy Skills (%) Level 1"/100)
    KEI.join$"Num Level 2" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Numeracy Skills (%) Level 2"/100)
    KEI.join$"Num Level 3" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Numeracy Skills (%) Level 3"/100)
    
    KEI.nums <- KEI.join[,c(1,2,3,4,9,10,11)]
    #melt the numbers data 
    KEI.meltnums <- melt(KEI.nums, id.vars=c("FIPS","District","Year","Domain"))
    # add metadata for vaiable and measure type
    KEI.meltnums$"Measure Type" <- rep("Number")
    KEI.meltnums$Variable <- rep("Kindergarten Entrance Inventory Results")
    #clean up variable names
    names(KEI.meltnums)[c(5,6)]<- c("Skill Level","Value")
    
    #Fix skill level data on metadata
    KEI.meltnums$"Skill Level" <- sapply(KEI.meltnums$"Skill Level", function(y) {
      if (y=="Num Level 1") {
        "Level 1"
      } else if (y=="Num Level 2") {
        "Level 2"
      } else if (y=="Num Level 3") {
        "Level 3"
      } else {"PROBLEM Column Header Numbers for Numeracy"}            
    })  
    
    # following code commented out.  Problem with Numeracy numbers not showing up.
    KEI.cols.nums <- KEI.meltnums [c(2,1,3,4,5,7,8,6)]
    #}
    
  }
  
  # get the percents data
  KEI.pcs <- KEI.join[,c(1:8)]
  
  # melt the percents and mark the lines as either being number (for total number tested or percents)
  KEI.meltpcs <- melt(KEI.pcs, id.vars=c("FIPS","District","Year","Domain"))
  KEI.meltpcs$"Measure Type" <- sapply(KEI.meltpcs$variable, function(x) {
    if (x =="Number of Students Tested"){
      "Number" 
    } else {"Percent"}
  })
  
  # clean up vars  add metadata and restructure
  names(KEI.meltpcs)[c(5,6)]<- c("Skill Level","Value")
  KEI.meltpcs$Variable <- rep("Kindergarten Entrance Inventory Results")
  KEI.cols.pcs <- KEI.meltpcs[c(2,1,3,4,5,7,8,6)]
  
  # reset metadata for skill level
  KEI.cols.pcs$"Skill Level" <- sapply(KEI.cols.pcs$"Skill Level", function(y) {
    if (y=="Numeracy Skills (%) Level 1") {
      "Level 1"
    } else if (y=="Numeracy Skills (%) Level 2") {
      "Level 2"
    } else if (y=="Numeracy Skills (%) Level 3") {
      "Level 3"
    } else if (y=="Number of Students Tested") {
      "Total Tested"
    } else {"PROBLEM Column Header Numeracy"}            
  })
  
  #append the numbers data to the percent data for 1 year and 1 variable
  if (i<6) {
    KEIbound1yr <- rbind(KEI.cols.pcs,KEI.cols.nums)  
  } else {KEIbound1yr <- KEI.cols.pcs}
  
  
  # specify the year value based on the count of the files coming in.
  if (i ==1) {
    KEIbound1yr$Year <- "2008-2009"  
    KEIallyrs1var <- KEIbound1yr
  } else if (i==2)   {
    KEIbound1yr$Year <- "2009-2010"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==3)   {
    KEIbound1yr$Year <- "2010-2011"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==4)   {
    KEIbound1yr$Year <- "2011-2012"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==5) {
    KEIbound1yr$Year <- "2012-2013"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else {
    KEIbound1yr$Year <- "2013-2014"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  }
  #end of for conditional dealing with 6 years of data
  #end of loop for 1 domain time next bracket
}

# append to dataframe established in section 1
KEIyrsvars <- rbind (KEIyrsvars,KEIallyrs1var)
# section 5 Numeracy completed

############################################################################
############################################################################


### SECTION 6 Physical motor 2009-2011
filename <- c(
  "2008-09_KEI_Physical.csv",
  "2009-10_KEI_Physical.csv",
  "2010-11_KEI_Physical.csv",
  "2011-12_KEI_Physical.csv",
  "2012-13_KEI_Physical.csv",
  "2013-14_KEI_Physical.csv"
)
for (i in 1:6) {
  KEI <- read.csv(filename[i], header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE) #nrows=171,
  names (KEI) [2] <- "District"
  names (KEI) [3] <- "Year"
  names (KEI) [5] <- "Physical/Motor Skills (%) Level 1"
  names (KEI) [6] <- "Physical/Motor Skills (%) Level 2"
  names (KEI) [7] <- "Physical/Motor Skills (%) Level 3"
  KEI <- KEI[,c(1:7)]
  KEI$District <- as.character(gsub(",","",KEI$District))
  
  KEI.join <- join(KEI, SD2Twn, by = "District")
  rm (KEI)
  
  KEI.join <- KEI.join[c(8,9,3,4,5,6,7)]
  names(KEI.join)[1]<- "District"
  
  KEI.join$FIPS <- sapply (KEI.join$FIPS, function (x) {
    if (is.na(x)) {
      return("")
    } else {
      return(paste("0",x,sep=""))
    }
  })
  
  #Deal with unique domain 
  KEI.join$"Domain" <- rep("Physical/Motor")
  KEI.join <- KEI.join[c(1,2,3,8,4,5,6,7)]
  if (i<6) {
    KEI.join$"Num Level 1" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Physical/Motor Skills (%) Level 1"/100)
    KEI.join$"Num Level 2" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Physical/Motor Skills (%) Level 2"/100)
    KEI.join$"Num Level 3" <- round(KEI.join$"Number of Students Tested"*KEI.join$"Physical/Motor Skills (%) Level 3"/100)
    
    KEI.nums <- KEI.join[,c(1,2,3,4,9,10,11)]
    #melt the numbers data 
    KEI.meltnums <- melt(KEI.nums, id.vars=c("FIPS","District","Year","Domain"))
    # add metadata for vaiable and measure type
    KEI.meltnums$"Measure Type" <- rep("Number")
    KEI.meltnums$Variable <- rep("Kindergarten Entrance Inventory Results")
    #clean up variable names
    names(KEI.meltnums)[c(5,6)]<- c("Skill Level","Value")
    
    #Fix skill level data on metadata
    KEI.meltnums$"Skill Level" <- sapply(KEI.meltnums$"Skill Level", function(y) {
      if (y=="Num Level 1") {
        "Level 1"
      } else if (y=="Num Level 2") {
        "Level 2"
      } else if (y=="Num Level 3") {
        "Level 3"
      } else {"PROBLEM Column Header Numbers for Physical/Motor"}            
    })  
    
    # following code commented out.  Problem with Physical/Motor numbers not showing up.
    KEI.cols.nums <- KEI.meltnums [c(2,1,3,4,5,7,8,6)]
    #}
    
  }
  
  # get the percents data
  KEI.pcs <- KEI.join[,c(1:8)]
  
  # melt the percents and mark the lines as either being number (for total number tested or percents)
  KEI.meltpcs <- melt(KEI.pcs, id.vars=c("FIPS","District","Year","Domain"))
  KEI.meltpcs$"Measure Type" <- sapply(KEI.meltpcs$variable, function(x) {
    if (x =="Number of Students Tested"){
      "Number" 
    } else {"Percent"}
  })
  
  # clean up vars  add metadata and restructure
  names(KEI.meltpcs)[c(5,6)]<- c("Skill Level","Value")
  KEI.meltpcs$Variable <- rep("Kindergarten Entrance Inventory Results")
  KEI.cols.pcs <- KEI.meltpcs[c(2,1,3,4,5,7,8,6)]
  
  # reset metadata for skill level
  KEI.cols.pcs$"Skill Level" <- sapply(KEI.cols.pcs$"Skill Level", function(y) {
    if (y=="Physical/Motor Skills (%) Level 1") {
      "Level 1"
    } else if (y=="Physical/Motor Skills (%) Level 2") {
      "Level 2"
    } else if (y=="Physical/Motor Skills (%) Level 3") {
      "Level 3"
    } else if (y=="Number of Students Tested") {
      "Total Tested"
    } else {"PROBLEM Column Header Physical/Motor"}            
  })
  
  #append the numbers data to the percent data for 1 year and 1 variable
  if (i<6) {
    KEIbound1yr <- rbind(KEI.cols.pcs,KEI.cols.nums)  
  } else {KEIbound1yr <- KEI.cols.pcs}
  
  
  # specify the year value based on the count of the files coming in.
  if (i ==1) {
    KEIbound1yr$Year <- "2008-2009"  
    KEIallyrs1var <- KEIbound1yr
  } else if (i==2)   {
    KEIbound1yr$Year <- "2009-2010"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==3)   {
    KEIbound1yr$Year <- "2010-2011"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==4)   {
    KEIbound1yr$Year <- "2011-2012"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else if (i==5) {
    KEIbound1yr$Year <- "2012-2013"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  } else {
    KEIbound1yr$Year <- "2013-2014"
    KEIallyrs1var <- rbind(KEIallyrs1var,KEIbound1yr)
  }
  #end of for conditional dealing with 6 years of data
  #end of loop for 1 domain time next bracket
}

# append to dataframe established in section 1
KEIyrsvars <- rbind (KEIyrsvars,KEIallyrs1var)
# section 6 Physical completed

############################################################################
############################################################################

#write.csv(KEIyrsvars,"previewKEI.csv",row.names=FALSE, quote=FALSE)
# look at groupings for districts
setgrp <- group_by(KEIyrsvars,District,FIPS)
DistFips<- summarize(setgrp,count=n())
View (DistFips)
distprob <- KEIyrsvars[KEIyrsvars$District=="NA",]
# check for district problems
setgrp <- group_by(KEIyrsvars,District, FIPS)
test <- summarize(setgrp)
setgrp <- group_by(test,District)
testa <- summarize(setgrp)
setgrp <- group_by(test,District, FIPS)
test1 <- summarize(setgrp, count=n())
setgrp <- group_by(test1,count)
test3 <- summarize(setgrp)
if (nrow(test)==nrow(testa)) {rm (test, testa)}
if (nrow(test3)==1) {rm(test1,test3)}



# all data should be in KEIyrsvars.  There NAs for numbers in 2012-13 and 2013-14 and maybe for other years as well.
# also the FIPS link and district data may not be complete so recheck that!
# the following df has NA data.  
KEIyrsvars.NAS <- KEIyrsvars[is.na(KEIyrsvars$Value),]
KEIyrsvars.NAS <- KEIyrsvars.NAS[!KEIyrsvars.NAS$Year=="2012-2013",]
KEIyrsvars.NAS <- KEIyrsvars.NAS[!KEIyrsvars.NAS$Year=="2013-2014",]
nrow(KEIyrsvars.NAS)
if (nrow(KEIyrsvars.NAS)==0) {rm(KEIyrsvars.NAS)}

KEIyrsvars.noNAS <- KEIyrsvars[!is.na(KEIyrsvars$Value),]


# Clean up objects
rm (KEI.meltnums)
rm (KEI.meltpcs)
rm (KEI.pcs)
rm (KEI.nums)
rm (KEI.join)
rm (KEIallyrs1var)
rm (KEIbound1yr)
rm (KEI.cols.pcs)
rm (KEI.cols.nums)


#fixedDistricts <- read.csv("CorrectedDistricts4KEI.csv", header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE)

#KEIyrsVars.noNAS.fixDist <- join(KEIyrsvars.noNAS,fixedDistricts, by="District")
#Prob<- KEIyrsVars.noNAS.fixDist[is.na(KEIyrsVars.noNAS.fixDist$Correct_4_KEI),]


#KEIyrsVars.noNAS.fixDist <- KEIyrsVars.noNAS.fixDist[c(9,2,3,4,5,6,7,8)]
#names(KEIyrsVars.noNAS.fixDist)[1]<- "District"
#names(KEIyrsVars.noNAS.fixDist)[1]

# WHAT is this join doing?  fixDist doeswn't exist
#KEIyrsVars.noNAS.fixDist <- join(KEIyrsVars.noNAS.fixDist, SD2Twn, by = "District")

#Prob<- KEIyrsVars.noNAS.fixDist[is.na(KEIyrsVars.noNAS.fixDist$Correct_4_KEI),]


#KEIyrsVars.noNAS.fixDist <- KEIyrsVars.noNAS.fixDist[c(1,11,3:8)]

#KEIyrsVars.noNAS.fixDist$FIPS <- sapply (KEIyrsVars.noNAS.fixDist$FIPS, function (x) {
#  if (is.na(x)) {
#    return("")
#  } else {
#    return(paste("0",x,sep=""))
#  }
#})


###############################################################
# Clean up decimals

#Folling cell has extended decimals
KEIyrsvars.noNAS[342,8]
# this code should round all decimals to 1 place
KEIyrsvars.noNAS[KEIyrsvars.noNAS$"Measure Type"=="Percent",]$Value <- round(KEIyrsvars.noNAS[KEIyrsvars.noNAS$"Measure Type"=="Percent",]$Value,1)
#check the cell.
KEIyrsvars.noNAS[342,8]
#that worked.

##############################################################


# A few dataframe checks before it is written out.

#"This is a data frame that checks if the header of the imported csv files work"
HeaderProblems <- KEIyrsvars.noNAS[grepl("PROBLEM",KEIyrsvars.noNAS$"Skill Level",ignore.case=TRUE),]

#there are a number of blanks in the data that need to be cleaned of the NA 

# now check for problems in dataframe after 
NAProblems <- KEIyrsvars.noNAS[is.na(KEIyrsvars.noNAS$Value),]


BadData <- nrow(NAProblems)+nrow(HeaderProblems)
BadData
nrow(NAProblems)
nrow(HeaderProblems)



if (BadData == 0) {
  write.csv(KEIyrsvars.noNAS,file="kindergarten-entrance-inventory_2008-09to2013-14_no11_12ST.csv", row.names=FALSE, quote=FALSE)
}

# before finalizing data this needs to be picked up and state totals need to be added.
#  the state file processing is kept in a separate file incase future needs for file of that structure.
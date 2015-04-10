library(plyr)
library(dplyr)
library(reshape2)

setwd("D:/Data/GitHub/KEI")
rm(list = ls())
cc <- c(rep("character",7),"numeric")

KEI<- read.csv("kindergarten-entrance-inventory 2008-09to2013-14.csv", header=TRUE, stringsAsFactors=FALSE, skip=0, strip.white=TRUE,check.names = FALSE,colClasses=cc)

setgrp <- group_by(KEIyrsvars,District,FIPS)
DistFips<- summarize(setgrp,count=n())
View (DistFips)
distprob <- KEIyrsvars[KEIyrsvars$District=="NA",]
# check for district problems
setgrp <- group_by(KEI,District, FIPS)
test <- summarize(setgrp)
setgrp <- group_by(test,District)
testa <- summarize(setgrp)
setgrp <- group_by(test,District, FIPS)
test1 <- summarize(setgrp, count=n())
setgrp <- group_by(test1,count)
test3 <- summarize(setgrp)
if (nrow(test)==nrow(testa)) {rm (test, testa)}
if (nrow(test3)==1) {rm(test1,test3)}

duptest<-which(duplicated(KEI[,c(1:3,5)]))

rowsinds2check <- sample.int(38012,20)
data2check<-KEI[rowsinds2check,]
View (data2check)

# random selection had no 2013-2014 years.
setgrp <- group_by(KEI,District, Year)
testYr<- summarize(setgrp)
testyr1314<- testYr[testYr$Year=="2013-2014",]
#they are there
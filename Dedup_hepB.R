###########################
#Deduplication By Andrew Cervantes
#HEP B specific
#Created June 2019
#Happy Merging!
###########################

########CODE PART 1########
install.packages("dplyr")
install.packages("stringr")
install.packages("data.table")
install.packages("pracma")
install.packages("readr")

#Only run code above when running the code for the first time
#and you have not used any of the packages above before.

########CODE PART 2########
rm(list=ls(all=TRUE))

#Change below location to the location of the
#folder with your data if you moved the folder
my.working.directory <- "X:/DISEASE INFORMATION/HEPATITIS C/DATA & STATS (BY YEAR)/Dedup/Summer 2019/Hep B"
setwd(my.working.directory)

########CODE PART 3########

library("dplyr")
library("stringr")
library("data.table")
library("pracma")
library("readr")

#Cutoff: what percent you want the merges to be above
cutoff <- 90

source("dedup_data_HEPB.R")
source("dedup_person_HepB.R")
source("case_merge.R")

########CODE PART 4########
###Run code above here before running the program###

ptm <- proc.time()
person_merge(cutoff)
case_merge(length(case_id))
proc.time() -ptm

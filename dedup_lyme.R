install.packages("dplyr")
install.packages("stringr")
install.packages("data.table")
install.packages("pracma")

#Only run code above when running the code for the first time
#and you have not used any of the packages above before.





#After running install.packages code ONCE, run from here until the
#comment below depecifying to run all code up to that point
library("dplyr")
library("stringr")
library("data.table")
library("pracma")
library("readr")

#Cutoff: what percent you want the merges to be above
cutoff <- 0.9

source("dedup_data_lyme.R")
source("dedup_person_lyme.R")

###Run code above here before running the program###

ptm <- proc.time()
person_merge(cutoff)
proc.time() -ptm

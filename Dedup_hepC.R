library("dplyr")
library("stringr")
library("data.table")
library("pracma")
library("readr")


#Cutoff: what percent you want the merges to be above
cutoff <- 90

source("dedup_data_format.R")
source("dedup_person_v3.R")
source("HEPC_case.R")

###Run all code up to here before running the program###
ptm <- proc.time()
person_merge(cutoff)
case_merge(length(case_id))
proc.time() -ptm
library(haven)
library(dplyr)
library(tidyverse)
library(tidyr)

#load data from csv files

source('scripts/Clean Data.R')

#import data
as_2026_raw <- read_sas('/Volumes/rsphprojects-ts/tsanche/ATMOS-SUNS/CYCLES/2026 Cycle/Data/Clean Datasets/2026 Final Dataset/NOID/atmos_suns2026_analysis_noid.sas7bdat', .name_repair="universal")

#save data
save(as_2026_raw,file='Data/as_2026.RData')

#load data
load('Data/as_2026.Rdata')

#clean data
as_2026_clean <- clean_as_data(as_2026_raw)

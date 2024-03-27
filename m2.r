##################################################################################################################
# Project 2: UKBB modifiable risk 
# meff() function to calculate an effective number of independent tests
# First need to build a correlation matrix
# Then run meff() fxn
##################################################################################################################

rm(list=ls(all=TRUE))
dev.off()
#options(stringsAsFactors = FALSE)
setwd('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/')

ukbb_final_file <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/ukbb_final_file.rds")
###########################################################################################################

# Recode categorical variables as ordinal variables (replace categorical values w/corresponding numeric values)
# mapping of categories to numeric values
av_alc_day_cat_mapping <- c('avg1' = 1, 'avg2' = 2, 'avg3' = 3, 'avg4' = 4)
# replace categorical values with numeric values
ukbb_final_file$av_alc_day_cat <- av_alc_day_cat_mapping[ukbb_final_file$av_alc_day_cat]

# mapping of categories to numeric values
alc_drinker_cat_mapping <- c('Missing' = 0, 'never' = 1, 'former' = 3, 'cur1' = 4, 'cur2' = 5, 'cur3' = 6, 'cur4' = 7)
# replace categrocal values with numeric values
ukbb_final_file$alc_drinker_cat <- alc_drinker_cat_mapping[ukbb_final_file$alc_drinker_cat]

# Calculate the correlation matrix
sapply(ukbb_final_file, is.numeric) #14 FALSE

# converting ordinal variables to numeric using as.numeric (cor fxn will not run without it)
ukbb_final_file$sleep_6 = as.numeric(ukbb_final_file$sleep_6)
ukbb_final_file$chronotype = as.numeric(ukbb_final_file$chronotype)
ukbb_final_file$insomnia = as.numeric(ukbb_final_file$insomnia)
ukbb_final_file$snoring = as.numeric(ukbb_final_file$snoring)
ukbb_final_file$narcolepsy = as.numeric(ukbb_final_file$narcolepsy)
ukbb_final_file$IPAQ_CAT = as.numeric(ukbb_final_file$IPAQ_CAT)
ukbb_final_file$job_night_shift = as.numeric(ukbb_final_file$job_night_shift)
ukbb_final_file$job_shift_work = as.numeric(ukbb_final_file$job_shift_work)
ukbb_final_file$job_shift_worked = as.numeric(ukbb_final_file$job_shift_worked)

sapply(ukbb_final_file, is.numeric) #8 FALSE                 

# Sub-setting ukbb_final_file to only include England Index Ind (>85% of pop.)
# fixes the issue of NAs in correlation matrix
subset_data <- ukbb_final_file[!is.na(ukbb_final_file$England_Index), ]
ukbb_final_file <- subset_data
ukbb_final_file <- ukbb_final_file[, !(colnames(ukbb_final_file) %in% c("Wales_Index", "Scotland_Index"))]
dim(ukbb_final_file) #417164, 77

# Building correlation matrix for all variables in models
# 1st line removes Scotland & Wales Indices from correlation matrix since they are all NAs 
ukbb_cor_matrix <- cor(ukbb_final_file[c('auto', 'mLOY', 'mLOX', 'CHIP', "England_Index", "age_squared", "smoke_detailed", "CEU", "YRI", "ASN", "bmi", "redwine_wk", "whitewine_wk", "beer_wk", "spirits_wk", "fortwine_wk", "otheralc_wk", "sum_alc_wk", "sum_alc_day", "av_alc_day", "alc_drinker", "sleepdur", "sleep_6", "chronotype", "insomnia", "snoring", "narcolepsy", "mod_freq_week", "mod_dur_day", "mod_dur_week", "vig_freq_week", "vig_dur_day", "vig_dur_week", "walk_freq_week", "walk_dur_day", "walk_dur_week", "mod_METmin_week", "vig_METmin_week", "walk_METmin_week", "tot_METmin_week", "tot_dur_week", "tot_dur_day", "tot_freq_week", "IPAQ_CAT", "job_night_shift", "job_shift_work", "job_shift_worked", "ageatassess", "geneticsex", "mCA_value", "av_alc_day_cat")], use = 'pairwise.complete.obs')
###########################################################################################################

# Code to Estimate number of effective test using meff()    
#install.packages('poolr')
library(poolr)

# Code to identify how many NAs are in each column in auto_mCA_clean
apply(ukbb_cor_matrix, 2, function(x) sum(is.na(x)))

# Remove rows and columns with NAs from ukbb_cor_matrix
# ukbb_cor_matrix <- ukbb_cor_matrix[complete.cases(ukbb_cor_matrix), complete.cases(ukbb_cor_matrix)]

# Check diagonal values
diag_values <- diag(ukbb_cor_matrix)
print(diag_values)
diag(ukbb_cor_matrix) <- 1 # Set diagonal values to 1
dim(ukbb_cor_matrix)

# Estimate the effective number of tests using the correlation matrix
ukbb_effective_tests <- meff(R = ukbb_cor_matrix, method = "galwey")
# auto_effective_tests <- meff(R = auto_cor_matrix, method = "galwey")
# mLOX_effective_tests <- meff(R = mLOX_cor_matrix, method = "galwey")
# mLOY_effective_tests <- meff(R = mLOY_cor_matrix, method = "galwey")
# CHIP_effective_tests <- meff(R = CHIP_cor_matrix, method = "galwey")

# Print the estimated effective number of tests
print(ukbb_effective_tests)
#print(c(ukbb_effective_tests, auto_effective_tests, mLOX_effective_tests, mLOY_effective_tests, CHIP_effective_tests))

# Update Bonferroni Correction put apply meff
ukbb_sign <- 0.05/ukbb_effective_tests
# auto_sign <- 1 - (1 - 0.05)^(1/auto_effective_tests)
# mLOX_sign <- 1 - (1 - 0.05)^(1/mLOX_effective_tests)
# mLOY_sign <- 1 - (1 - 0.05)^(1/mLOY_effective_tests)
# CHIP_sign <- 1 - (1 - 0.05)^(1/CHIP_effective_tests)

print(ukbb_sign)
#print(c(ukbb_sign, auto_sign, mLOX_sign, mLOY_sign, CHIP_sign))

p4_effective_test <- ukbb_sign
saveRDS(p4_effective_test, file = 'p4_effective_test.rds')


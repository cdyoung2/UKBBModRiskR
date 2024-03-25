##################################################################################################################
# Project 2: UKBB modifiable risk 
# R code to run models: (1) create clean mCA free controls, (2) create outcome variables (auto, mLOX, mLOY), (3) run adjusted models for auto, mlox & mloy w/each variable of interest
# contains Erikka+Pedro updated SAS transcribed data, +CHIP, +SES data   
##################################################################################################################

rm(list=ls(all=TRUE))
dev.off()
#options(stringsAsFactors = FALSE)

ukbb_final_file <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/ukbb_final_file.rds")

############# Building df for running models #############
# Take out people with loss of x or loss of Y from controls of autosomal mCAs.
# Case = 1, control = 0
auto_mCA_clean <- ukbb_final_file[ukbb_final_file$mLOY != 1 & ukbb_final_file$mLOX != 1,]

#Take out people with autosomal mCAs from Loss of Y controls
#Subset to women only for Loss of Y analyses
mLOX_mCA_clean.ref <- ukbb_final_file[ukbb_final_file$auto != 1,]
mLOX_mCA_clean <- mLOX_mCA_clean.ref[mLOX_mCA_clean.ref$geneticsex==0,]

#Take out people with autosomal mCAs from Loss of X controls
#Subset to men only of Loss of X analyses
mLOY_mCA_clean.ref <- ukbb_final_file[ukbb_final_file$auto != 1,]
mLOY_mCA_clean <- mLOY_mCA_clean.ref[mLOY_mCA_clean.ref$geneticsex==1,]

#CHIP clean group either CHIP (1) or no mCAs/CHIP etc. (clean group)
#clean set of controls w/o CHIP
CHIP_clean <- ukbb_final_file #comparison is only made between CHIP & non-CHIP groups no removal of mCA groups like below
#CHIP_clean.ref <- ukbb_final_file[ukbb_final_file$mLOY != 1 & ukbb_final_file$mLOX != 1 & ukbb_final_file$auto != 1,]
#CHIP_clean <- CHIP_clean.ref

############# Autosomal Logistic Regression Models: Effects of behavior on mCAs ############# 
# Covariates: age, age2, smoking, sex (autosomal only), genetic ancestry

# outcome variable: auto_mCA_clean (auto/cases = 1, no auto/controls = 0)
table(auto_mCA_clean$auto)

library(biostat3) #for eform command
library(tidyverse) #tidybroom


###########################################################################
###########################################################################
# Autosomal mCA Models #
###########################################################################
###########################################################################

###################
# sleep models #
###################

############# auto_model: sleepdur ############# 
auto_model.sleepdur <- glm(auto ~ sleepdur + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# dataset has binary response (outcome, dependent) variable called auto
# predictor variable: f.1160.0.0 (sleepdur) 
# counfounders: f.21003.0.0 (age) + age_squared + f.22001.0.0 (sex) + smoke_detailed + YRI (Afrian) + ASN (Asian) 
# Additional info: age (continous/discrete), sex & smoke (treated as categorical variable)
# get summary and confidence intervals, note = beta is not the OR, needs to be exponentiated
#confint(auto_model.sleepdur, 'sleepdur', level=0.95)
#summary(auto_model.sleepdur)
eform(auto_model.sleepdur, level = 0.95) # outputs OR, and 95% CI
summary(auto_model.sleepdur)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# auto_model: sleep_6 ############# 
auto_model_sleep_6 <- glm(auto ~ sleep_6 + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
#confint(auto_model_chronotype, 'f.1180.0.0', level=0.95)
#summary(auto_model_chronotype)
eform(auto_model_sleep_6, level = 0.95)
summary(auto_model_sleep_6)$coefficients[,4]

############# auto_model: chronotype ############# 
auto_model_chronotype <- glm(auto ~ chronotype + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
#confint(auto_model_chronotype, 'f.1180.0.0', level=0.95)
#summary(auto_model_chronotype)
eform(auto_model_chronotype, level = 0.95)
summary(auto_model_chronotype)$coefficients[,4]

############# auto_model: Insomnia ############# 
auto_model_insomnia <- glm(auto ~ insomnia + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_insomnia, 'f.1200.0.0', level=0.95)
# summary(auto_model_insomnia)
eform(auto_model_insomnia, level = 0.95)
summary(auto_model_insomnia)$coefficients[,4]

############# auto_model: Snoring ############# 
auto_model_snoring <- glm(auto ~ snoring +ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_snoring, 'f.1210.0.0', level=0.95)
# summary(auto_model_snoring)
eform(auto_model_snoring, level = 0.95)
summary(auto_model_snoring)$coefficients[,4]

############# auto_model: Narcolepsy ############# 
auto_model_narcolepsy <- glm(auto ~ narcolepsy + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_narcolepsy, 'f.1220.0.0', level=0.95)
# summary(auto_model_narcolepsy)
eform(auto_model_narcolepsy, level = 0.95)
summary(auto_model_narcolepsy)$coefficients[,4]

#################################
# physical activity (PA) models #
#################################

############# auto_model: mod_freq_week ############# 
auto_model_mod_freq_week <- glm(auto ~ mod_freq_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
#confint(auto_model_10plusminwalks, 'f.864.0.0', level=0.95)
#summary(auto_model_10plusminwalks)
eform(auto_model_mod_freq_week, level = 0.95)
summary(auto_model_mod_freq_week)$coefficients[,4]

############# auto_model: mod_dur_day ############# 
auto_model_mod_dur_day <- glm(auto ~ mod_dur_day + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
#confint(auto_model_mod_dur_day, 'f.874.0.0', level=0.95)
#summary(auto_model_mod_dur_day)
eform(auto_model_mod_dur_day, level = 0.95)
summary(auto_model_mod_dur_day)$coefficients[,4]

############# auto_model: mod_dur_week	 ############# 
auto_model_mod_dur_week <- glm(auto ~ mod_dur_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_mod_dur_week, 'f.884.0.0', level=0.95)
# summary(auto_model_mod_dur_week)
eform(auto_model_mod_dur_week, level = 0.95)
summary(auto_model_mod_dur_week)$coefficients[,4]

############# auto_model: vig_freq_week	############# 
auto_model_vig_freq_week <- glm(auto ~ vig_freq_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_vig_freq_week, 'f.894.0.0', level=0.95)
# summary(auto_model_vig_freq_week)
eform(auto_model_vig_freq_week, level = 0.95)
summary(auto_model_vig_freq_week)$coefficients[,4]

############# auto_model: vig_dur_day ############# 
auto_model_vig_dur_day <- glm(auto ~ vig_dur_day + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_vig_dur_day, 'f.904.0.0', level=0.95)
# summary(auto_model_vig_dur_day)
eform(auto_model_vig_dur_day, level = 0.95)
summary(auto_model_vig_dur_day)$coefficients[,4]

############# auto_model: vig_dur_week ############# 
auto_model_vig_dur_week <- glm(auto ~ vig_dur_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_vig_dur_week, 'f.914.0.0', level=0.95)
# summary(auto_model_vig_dur_week)
eform(auto_model_vig_dur_week, level = 0.95)
summary(auto_model_vig_dur_week)$coefficients[,4]

############# auto_model: walk_freq_week ############# 
auto_model_walk_freq_week <- glm(auto ~ walk_freq_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_walk_freq_week, 'f.104900.0.0', level=0.95)
# summary(auto_model_walk_freq_week)
eform(auto_model_walk_freq_week, level = 0.95)
summary(auto_model_walk_freq_week)$coefficients[,4]

############# auto_model: walk_dur_day ############# 
auto_model_walk_dur_day <- glm(auto ~ walk_dur_day + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_walk_dur_day, 'f.104910.0.0', level=0.95)
# summary(auto_model_walk_dur_day)
eform(auto_model_walk_dur_day, level = 0.95)
summary(auto_model_walk_dur_day)$coefficients[,4]

############# auto_model: walk_dur_week ############# 
auto_model_walk_dur_week <- glm(auto ~ walk_dur_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_walk_dur_week, 'f.104920.0.0', level=0.95)
# summary(auto_model_walk_dur_week)
eform(auto_model_walk_dur_week, level = 0.95)
summary(auto_model_walk_dur_week)$coefficients[,4]

############# auto_model: mod_METmin_week ############# 
auto_model_mod_METmin_week <- glm(auto ~ mod_METmin_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_mod_METmin_week, 'f.104920.0.0', level=0.95)
# summary(auto_model_mod_METmin_week)
eform(auto_model_mod_METmin_week, level = 0.95)
summary(auto_model_mod_METmin_week)$coefficients[,4]

############# auto_model: vig_METmin_week ############# 
auto_model_vig_METmin_week <- glm(auto ~ vig_METmin_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_vig_METmin_week, 'f.104920.0.0', level=0.95)
# summary(auto_model_vig_METmin_week)
eform(auto_model_vig_METmin_week, level = 0.95)
summary(auto_model_vig_METmin_week)$coefficients[,4]

############# auto_model: walk_METmin_week ############# 
auto_model_walk_METmin_week <- glm(auto ~ walk_METmin_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_walk_METmin_week, 'f.104920.0.0', level=0.95)
# summary(auto_model_walk_METmin_week)
eform(auto_model_walk_METmin_week, level = 0.95)
summary(auto_model_walk_METmin_week)$coefficients[,4]

############# auto_model: tot_METmin_week ############# 
auto_model_tot_METmin_week <- glm(auto ~ tot_METmin_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_tot_METmin_week, 'f.104920.0.0', level=0.95)
# summary(auto_model_tot_METmin_week)
eform(auto_model_tot_METmin_week, level = 0.95)
summary(auto_model_tot_METmin_week)$coefficients[,4]

############# auto_model: tot_dur_week ############# 
auto_model_tot_dur_week <- glm(auto ~ tot_dur_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_tot_dur_week, 'f.104920.0.0', level=0.95)
# summary(auto_model_tot_dur_week)
eform(auto_model_tot_dur_week, level = 0.95)
summary(auto_model_tot_dur_week)$coefficients[,4]

############# auto_model: tot_dur_day ############# 
auto_model_tot_dur_day <- glm(auto ~ tot_dur_day + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_tot_dur_day, 'f.104920.0.0', level=0.95)
# summary(auto_model_tot_dur_day)
eform(auto_model_tot_dur_day, level = 0.95)
summary(auto_model_tot_dur_day)$coefficients[,4]

############# auto_model: tot_freq_week ############# 
auto_model_tot_freq_week <- glm(auto ~ tot_freq_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_tot_freq_week, 'f.104920.0.0', level=0.95)
# summary(auto_model_tot_freq_week)
eform(auto_model_tot_freq_week, level = 0.95)
summary(auto_model_tot_freq_week)$coefficients[,4]

############# auto_model: IPAQ_CAT ############# 
auto_model_IPAQ_CAT <- glm(auto ~ IPAQ_CAT + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_IPAQ_CAT, 'f.104920.0.0', level=0.95)
# summary(auto_model_IPAQ_CAT)
eform(auto_model_IPAQ_CAT, level = 0.95)
summary(auto_model_IPAQ_CAT)$coefficients[,4]


#################################
# alcohol models #
#################################
############# auto_model: alcohol redwine_wk ############# 
auto_model_redwine_wk <- glm(auto ~ redwine_wk + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_redwine_wk, 'f.1558.0.0', level=0.95)
# summary(auto_model_redwine_wk)
eform(auto_model_redwine_wk, level = 0.95)
summary(auto_model_redwine_wk)$coefficients[,4]

############# auto_model: alcohol whitewine_wk ############# 
auto_model_whitewine_wk <- glm(auto ~ whitewine_wk + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_whitewine_wk, 'f.1558.0.0', level=0.95)
# summary(auto_model_whitewine_wk)
eform(auto_model_whitewine_wk, level = 0.95)
summary(auto_model_whitewine_wk)$coefficients[,4]

############# auto_model: alcohol beer_wk ############# 
auto_model_beer_wk <- glm(auto ~ beer_wk + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_beer_wk, 'f.1558.0.0', level=0.95)
# summary(auto_model_beer_wk)
eform(auto_model_beer_wk, level = 0.95)
summary(auto_model_beer_wk)$coefficients[,4]

############# auto_model: alcohol spirits_wk ############# 
auto_model_spirits_wk <- glm(auto ~ spirits_wk + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_spirits_wk, 'f.1558.0.0', level=0.95)
# summary(auto_model_spirits_wk)
eform(auto_model_spirits_wk, level = 0.95)
summary(auto_model_spirits_wk)$coefficients[,4]

############# auto_model: alcohol fortwine_wk ############# 
auto_model_fortwine_wk <- glm(auto ~ fortwine_wk + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_fortwine_wk, 'f.1558.0.0', level=0.95)
# summary(auto_model_fortwine_wk)
eform(auto_model_fortwine_wk, level = 0.95)
summary(auto_model_fortwine_wk)$coefficients[,4]

############# auto_model: alcohol otheralc_wk ############# 
auto_model_otheralc_wk <- glm(auto ~ otheralc_wk + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_otheralc_wk, 'f.1558.0.0', level=0.95)
# summary(auto_model_otheralc_wk)
eform(auto_model_otheralc_wk, level = 0.95)
summary(auto_model_otheralc_wk)$coefficients[,4]

############# auto_model: alcohol sum_alc_wk ############# 
auto_model_sum_alc_wk <- glm(auto ~ sum_alc_wk + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_sum_alc_wk, 'f.1558.0.0', level=0.95)
# summary(auto_model_sum_alc_wk)
eform(auto_model_sum_alc_wk, level = 0.95)
summary(auto_model_sum_alc_wk)$coefficients[,4]

############# auto_model: alcohol sum_alc_day ############# 
auto_model_sum_alc_day <- glm(auto ~ sum_alc_day + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_sum_alc_day, 'f.1558.0.0', level=0.95)
# summary(auto_model_sum_alc_day)
eform(auto_model_sum_alc_day, level = 0.95)
summary(auto_model_sum_alc_day)$coefficients[,4]

############# auto_model: alcohol av_alc_day ############# 
auto_model_av_alc_day <- glm(auto ~ av_alc_day + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_av_alc_day, 'f.1558.0.0', level=0.95)
# summary(auto_model_av_alc_day)
eform(auto_model_av_alc_day, level = 0.95)
summary(auto_model_av_alc_day)$coefficients[,4]

############# auto_model: alcohol av_alc_day_cat ############# 
auto_model_av_alc_day_cat <- glm(auto ~ av_alc_day_cat + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_av_alc_day_cat, 'f.1558.0.0', level=0.95)
# summary(auto_model_av_alc_day_cat)
eform(auto_model_av_alc_day_cat, level = 0.95)
summary(auto_model_av_alc_day_cat)$coefficients[,4]

############# auto_model: alcohol alc_drinker ############# 
auto_model_alc_drinker <- glm(auto ~ alc_drinker + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_alc_drinker, 'f.1558.0.0', level=0.95)
# summary(auto_model_alc_drinker)
eform(auto_model_alc_drinker, level = 0.95)
summary(auto_model_alc_drinker)$coefficients[,4]

############# auto_model: alcohol alc_drinker_cat ############# 
auto_model_alc_drinker_cat <- glm(auto ~ alc_drinker_cat + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_alc_drinker_cat, 'f.1558.0.0', level=0.95)
# summary(auto_model_alc_drinker_cat)
eform(auto_model_alc_drinker_cat, level = 0.95)
summary(auto_model_alc_drinker_cat)$coefficients[,4]


#################################
# SES models #
#################################
############# auto_model: SES Scotland_Index_scaled ############# 
auto_model_Scotland_Index_scaled <- glm(auto ~ Scotland_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_Scotland_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(auto_model_Scotland_Index_scaled)
eform(auto_model_Scotland_Index_scaled, level = 0.95)
summary(auto_model_Scotland_Index_scaled)$coefficients[,4]

############# auto_model: SES England_Index_scaled ############# 
auto_model_England_Index_scaled <- glm(auto ~ England_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_England_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(auto_model_England_Index_scaled)
eform(auto_model_England_Index_scaled, level = 0.95)
summary(auto_model_England_Index_scaled)$coefficients[,4]

############# auto_model: SES Wales_Index_scaled ############# 
auto_model_Wales_Index_scaled <- glm(auto ~ Wales_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# confint(auto_model_Wales_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(auto_model_Wales_Index_scaled)
eform(auto_model_Wales_Index_scaled, level = 0.95)
summary(auto_model_Wales_Index_scaled)$coefficients[,4]

#################################
# Shift-work models #
#################################
############# auto_model: job_night_shift ############# 
auto_model_job_night_shift <- glm(auto ~ job_night_shift + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
# dataset has binary response (outcome, dependent) variable called auto
# predictor variable: f.1160.0.0 (sleepdur) 
# counfounders: f.21003.0.0 (age) + age_squared + f.22001.0.0 (sex) + smoke_detailed + YRI (Afrian) + ASN (Asian) 
# Additional info: age (continous/discrete), sex & smoke (treated as categorical variable)
# get summary and confidence intervals, note = beta is not the OR, needs to be exponentiated
#confint(auto_model.sleepdur, 'sleepdur', level=0.95)
#summary(auto_model.sleepdur)
eform(auto_model_job_night_shift, level = 0.95) # outputs OR, and 95% CI
summary(auto_model_job_night_shift)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# auto_model: job_shift_work ############# 
auto_model_job_shift_work <- glm(auto ~ job_shift_work + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
eform(auto_model_job_shift_work, level = 0.95) # outputs OR, and 95% CI
summary(auto_model_job_shift_work)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# auto_model: job_shift_worked ############# 
auto_model_job_shift_worked <- glm(auto ~ job_shift_worked + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=auto_mCA_clean, family=binomial)
eform(auto_model_job_shift_worked, level = 0.95) # outputs OR, and 95% CI
summary(auto_model_job_shift_worked)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.
###########################################################################
###########################################################################
# mLOX mCA Models #
###########################################################################
###########################################################################

###################
# sleep models #
###################
############# mLOX_model: sleepdur ############# 
mLOX_model.sleepdur <- glm(mLOX ~ sleepdur + ageatassess + age_squared +as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# dataset has binary response (outcome, dependent) variable called mLOX
# predictor variable: f.1160.0.0 (sleepdur) 
# counfounders: f.21003.0.0 (age) + age_squared + f.22001.0.0 (sex) + smoke_detailed + YRI (Afrian) + ASN (Asian) 
# Additional info: age (continous/discrete), sex & smoke (treated as categorical variable)
# get summary and confidence intervals, note = beta is not the OR, needs to be exponentiated
#confint(mLOX_model.sleepdur, 'sleepdur', level=0.95)
#summary(mLOX_model.sleepdur)
eform(mLOX_model.sleepdur, level = 0.95) # outputs OR, and 95% CI
summary(mLOX_model.sleepdur)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOX_model: sleep_6 ############# 
mLOX_model_sleep_6 <- glm(mLOX ~ sleep_6 + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
#confint(mLOX_model_sleep_6, 'f.1180.0.0', level=0.95)
#summary(mLOX_model_sleep_6)
eform(mLOX_model_sleep_6, level = 0.95)
summary(mLOX_model_sleep_6)$coefficients[,4]

############# mLOX_model: chronotype ############# 
mLOX_model_chronotype <- glm(mLOX ~ chronotype + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
#confint(mLOX_model_chronotype, 'f.1180.0.0', level=0.95)
#summary(mLOX_model_chronotype)
eform(mLOX_model_chronotype, level = 0.95)
summary(mLOX_model_chronotype)$coefficients[,4]

############# mLOX_model: Insomnia ############# 
mLOX_model_insomnia <- glm(mLOX ~ insomnia + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_insomnia, 'f.1200.0.0', level=0.95)
# summary(mLOX_model_insomnia)
eform(mLOX_model_insomnia, level = 0.95)
summary(mLOX_model_insomnia)$coefficients[,4]

############# mLOX_model: Snoring ############# 
mLOX_model_snoring <- glm(mLOX ~ snoring +ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_snoring, 'f.1210.0.0', level=0.95)
# summary(mLOX_model_snoring)
eform(mLOX_model_snoring, level = 0.95)
summary(mLOX_model_snoring)$coefficients[,4]

############# mLOX_model: Narcolepsy ############# 
mLOX_model_narcolepsy <- glm(mLOX ~ narcolepsy + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_narcolepsy, 'f.1220.0.0', level=0.95)
# summary(mLOX_model_narcolepsy)
eform(mLOX_model_narcolepsy, level = 0.95)
summary(mLOX_model_narcolepsy)$coefficients[,4]

#################################
# physical activity (PA) models #
#################################

############# mLOX_model: mod_freq_week ############# 
mLOX_model_mod_freq_week <- glm(mLOX ~ mod_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
#confint(mLOX_model_10plusminwalks, 'f.864.0.0', level=0.95)
#summary(mLOX_model_10plusminwalks)
eform(mLOX_model_mod_freq_week, level = 0.95)
summary(mLOX_model_mod_freq_week)$coefficients[,4]

############# mLOX_model: mod_dur_day ############# 
mLOX_model_mod_dur_day <- glm(mLOX ~ mod_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
#confint(mLOX_model_mod_dur_day, 'f.874.0.0', level=0.95)
#summary(mLOX_model_mod_dur_day)
eform(mLOX_model_mod_dur_day, level = 0.95)
summary(mLOX_model_mod_dur_day)$coefficients[,4]

############# mLOX_model: mod_dur_week	 ############# 
mLOX_model_mod_dur_week <- glm(mLOX ~ mod_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_mod_dur_week, 'f.884.0.0', level=0.95)
# summary(mLOX_model_mod_dur_week)
eform(mLOX_model_mod_dur_week, level = 0.95)
summary(mLOX_model_mod_dur_week)$coefficients[,4]

############# mLOX_model: vig_freq_week	############# 
mLOX_model_vig_freq_week <- glm(mLOX ~ vig_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_vig_freq_week, 'f.894.0.0', level=0.95)
# summary(mLOX_model_vig_freq_week)
eform(mLOX_model_vig_freq_week, level = 0.95)
summary(mLOX_model_vig_freq_week)$coefficients[,4]

############# mLOX_model: vig_dur_day ############# 
mLOX_model_vig_dur_day <- glm(mLOX ~ vig_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_vig_dur_day, 'f.904.0.0', level=0.95)
# summary(mLOX_model_vig_dur_day)
eform(mLOX_model_vig_dur_day, level = 0.95)
summary(mLOX_model_vig_dur_day)$coefficients[,4]

############# mLOX_model: vig_dur_week ############# 
mLOX_model_vig_dur_week <- glm(mLOX ~ vig_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_vig_dur_week, 'f.914.0.0', level=0.95)
# summary(mLOX_model_vig_dur_week)
eform(mLOX_model_vig_dur_week, level = 0.95)
summary(mLOX_model_vig_dur_week)$coefficients[,4]

############# mLOX_model: walk_freq_week ############# 
mLOX_model_walk_freq_week <- glm(mLOX ~ walk_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_walk_freq_week, 'f.104900.0.0', level=0.95)
# summary(mLOX_model_walk_freq_week)
eform(mLOX_model_walk_freq_week, level = 0.95)
summary(mLOX_model_walk_freq_week)$coefficients[,4]

############# mLOX_model: walk_dur_day ############# 
mLOX_model_walk_dur_day <- glm(mLOX ~ walk_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_walk_dur_day, 'f.104910.0.0', level=0.95)
# summary(mLOX_model_walk_dur_day)
eform(mLOX_model_walk_dur_day, level = 0.95)
summary(mLOX_model_walk_dur_day)$coefficients[,4]

############# mLOX_model: walk_dur_week ############# 
mLOX_model_walk_dur_week <- glm(mLOX ~ walk_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_walk_dur_week, 'f.104920.0.0', level=0.95)
# summary(mLOX_model_walk_dur_week)
eform(mLOX_model_walk_dur_week, level = 0.95)
summary(mLOX_model_walk_dur_week)$coefficients[,4]

############# mLOX_model: mod_METmin_week ############# 
mLOX_model_mod_METmin_week <- glm(mLOX ~ mod_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_mod_METmin_week, 'f.104920.0.0', level=0.95)
# summary(mLOX_model_mod_METmin_week)
eform(mLOX_model_mod_METmin_week, level = 0.95)
summary(mLOX_model_mod_METmin_week)$coefficients[,4]

############# mLOX_model: vig_METmin_week ############# 
mLOX_model_vig_METmin_week <- glm(mLOX ~ vig_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_vig_METmin_week, 'f.104920.0.0', level=0.95)
# summary(mLOX_model_vig_METmin_week)
eform(mLOX_model_vig_METmin_week, level = 0.95)
summary(mLOX_model_vig_METmin_week)$coefficients[,4]

############# mLOX_model: walk_METmin_week ############# 
mLOX_model_walk_METmin_week <- glm(mLOX ~ walk_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_walk_METmin_week, 'f.104920.0.0', level=0.95)
# summary(mLOX_model_walk_METmin_week)
eform(mLOX_model_walk_METmin_week, level = 0.95)
summary(mLOX_model_walk_METmin_week)$coefficients[,4]

############# mLOX_model: tot_METmin_week ############# 
mLOX_model_tot_METmin_week <- glm(mLOX ~ tot_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_tot_METmin_week, 'f.104920.0.0', level=0.95)
# summary(mLOX_model_tot_METmin_week)
eform(mLOX_model_tot_METmin_week, level = 0.95)
summary(mLOX_model_tot_METmin_week)$coefficients[,4]

############# mLOX_model: tot_dur_week ############# 
mLOX_model_tot_dur_week <- glm(mLOX ~ tot_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_tot_dur_week, 'f.104920.0.0', level=0.95)
# summary(mLOX_model_tot_dur_week)
eform(mLOX_model_tot_dur_week, level = 0.95)
summary(mLOX_model_tot_dur_week)$coefficients[,4]

############# mLOX_model: tot_dur_day ############# 
mLOX_model_tot_dur_day <- glm(mLOX ~ tot_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_tot_dur_day, 'f.104920.0.0', level=0.95)
# summary(mLOX_model_tot_dur_day)
eform(mLOX_model_tot_dur_day, level = 0.95)
summary(mLOX_model_tot_dur_day)$coefficients[,4]

############# mLOX_model: tot_freq_week ############# 
mLOX_model_tot_freq_week <- glm(mLOX ~ tot_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_tot_freq_week, 'f.104920.0.0', level=0.95)
# summary(mLOX_model_tot_freq_week)
eform(mLOX_model_tot_freq_week, level = 0.95)
summary(mLOX_model_tot_freq_week)$coefficients[,4]

############# mLOX_model: IPAQ_CAT ############# 
mLOX_model_IPAQ_CAT <- glm(mLOX ~ IPAQ_CAT + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_IPAQ_CAT, 'f.104920.0.0', level=0.95)
# summary(mLOX_model_IPAQ_CAT)
eform(mLOX_model_IPAQ_CAT, level = 0.95)
summary(mLOX_model_IPAQ_CAT)$coefficients[,4]


#################################
# alcohol models #
#################################
############# mLOX_model: alcohol redwine_wk ############# 
mLOX_model_redwine_wk <- glm(mLOX ~ redwine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_redwine_wk, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_redwine_wk)
eform(mLOX_model_redwine_wk, level = 0.95)
summary(mLOX_model_redwine_wk)$coefficients[,4]

############# mLOX_model: alcohol whitewine_wk ############# 
mLOX_model_whitewine_wk <- glm(mLOX ~ whitewine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_whitewine_wk, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_whitewine_wk)
eform(mLOX_model_whitewine_wk, level = 0.95)
summary(mLOX_model_whitewine_wk)$coefficients[,4]

############# mLOX_model: alcohol beer_wk ############# 
mLOX_model_beer_wk <- glm(mLOX ~ beer_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_beer_wk, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_beer_wk)
eform(mLOX_model_beer_wk, level = 0.95)
summary(mLOX_model_beer_wk)$coefficients[,4]

############# mLOX_model: alcohol spirits_wk ############# 
mLOX_model_spirits_wk <- glm(mLOX ~ spirits_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_spirits_wk, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_spirits_wk)
eform(mLOX_model_spirits_wk, level = 0.95)
summary(mLOX_model_spirits_wk)$coefficients[,4]

############# mLOX_model: alcohol fortwine_wk ############# 
mLOX_model_fortwine_wk <- glm(mLOX ~ fortwine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_fortwine_wk, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_fortwine_wk)
eform(mLOX_model_fortwine_wk, level = 0.95)
summary(mLOX_model_fortwine_wk)$coefficients[,4]

############# mLOX_model: alcohol otheralc_wk ############# 
mLOX_model_otheralc_wk <- glm(mLOX ~ otheralc_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_otheralc_wk, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_otheralc_wk)
eform(mLOX_model_otheralc_wk, level = 0.95)
summary(mLOX_model_otheralc_wk)$coefficients[,4]

############# mLOX_model: alcohol sum_alc_wk ############# 
mLOX_model_sum_alc_wk <- glm(mLOX ~ sum_alc_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_sum_alc_wk, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_sum_alc_wk)
eform(mLOX_model_sum_alc_wk, level = 0.95)
summary(mLOX_model_sum_alc_wk)$coefficients[,4]

############# mLOX_model: alcohol sum_alc_day ############# 
mLOX_model_sum_alc_day <- glm(mLOX ~ sum_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_sum_alc_day, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_sum_alc_day)
eform(mLOX_model_sum_alc_day, level = 0.95)
summary(mLOX_model_sum_alc_day)$coefficients[,4]

############# mLOX_model: alcohol av_alc_day ############# 
mLOX_model_av_alc_day <- glm(mLOX ~ av_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_av_alc_day, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_av_alc_day)
eform(mLOX_model_av_alc_day, level = 0.95)
summary(mLOX_model_av_alc_day)$coefficients[,4]

############# mLOX_model: alcohol av_alc_day_cat ############# 
mLOX_model_av_alc_day_cat <- glm(mLOX ~ av_alc_day_cat + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_av_alc_day_cat, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_av_alc_day_cat)
eform(mLOX_model_av_alc_day_cat, level = 0.95)
summary(mLOX_model_av_alc_day_cat)$coefficients[,4]

############# mLOX_model: alcohol alc_drinker ############# 
mLOX_model_alc_drinker <- glm(mLOX ~ alc_drinker + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_alc_drinker, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_alc_drinker)
eform(mLOX_model_alc_drinker, level = 0.95)
summary(mLOX_model_alc_drinker)$coefficients[,4]

############# mLOX_model: alcohol alc_drinker_cat ############# 
mLOX_model_alc_drinker_cat <- glm(mLOX ~ alc_drinker_cat + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_alc_drinker_cat, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_alc_drinker_cat)
eform(mLOX_model_alc_drinker_cat, level = 0.95)
summary(mLOX_model_alc_drinker_cat)$coefficients[,4]


#################################
# SES models #
#################################
############# mLOX_model: SES Scotland_Index_scaled ############# 
mLOX_model_Scotland_Index_scaled <- glm(mLOX ~ Scotland_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_Scotland_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_Scotland_Index_scaled)
eform(mLOX_model_Scotland_Index_scaled, level = 0.95)
summary(mLOX_model_Scotland_Index_scaled)$coefficients[,4]

############# mLOX_model: SES England_Index_scaled ############# 
mLOX_model_England_Index_scaled <- glm(mLOX ~ England_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_England_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_England_Index_scaled)
eform(mLOX_model_England_Index_scaled, level = 0.95)
summary(mLOX_model_England_Index_scaled)$coefficients[,4]

############# mLOX_model: SES Wales_Index_scaled ############# 
mLOX_model_Wales_Index_scaled <- glm(mLOX ~ Wales_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
# confint(mLOX_model_Wales_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(mLOX_model_Wales_Index_scaled)
eform(mLOX_model_Wales_Index_scaled, level = 0.95)
summary(mLOX_model_Wales_Index_scaled)$coefficients[,4]

#################################
# Shift-work models #
#################################
############# mLOX_model: job_night_shift ############# 
mLOX_model_job_night_shift <- glm(mLOX ~ job_night_shift + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
eform(mLOX_model_job_night_shift, level = 0.95) # outputs OR, and 95% CI
summary(mLOX_model_job_night_shift)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOX_model: job_shift_work ############# 
mLOX_model_job_shift_work <- glm(mLOX ~ job_shift_work + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
eform(mLOX_model_job_shift_work, level = 0.95) # outputs OR, and 95% CI
summary(mLOX_model_job_shift_work)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOX_model: job_shift_worked ############# 
mLOX_model_job_shift_worked <- glm(mLOX ~ job_shift_worked + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
eform(mLOX_model_job_shift_worked, level = 0.95) # outputs OR, and 95% CI
summary(mLOX_model_job_shift_worked)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

###########################################################################
###########################################################################
# mLOY mCA Models #
###########################################################################
###########################################################################

###################
# sleep models #
###################
############# mLOY_model: sleepdur ############# 
mLOY_model.sleepdur <- glm(mLOY ~ sleepdur + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# dataset has binary response (outcome, dependent) variable called mLOY
# predictor variable: f.1160.0.0 (sleepdur) 
# counfounders: f.21003.0.0 (age) + age_squared + f.22001.0.0 (sex) + smoke_detailed + YRI (Afrian) + ASN (Asian) 
# Additional info: age (continous/discrete), sex & smoke (treated as categorical variable)
# get summary and confidence intervals, note = beta is not the OR, needs to be exponentiated
#confint(mLOY_model.sleepdur, 'sleepdur', level=0.95)
#summary(mLOY_model.sleepdur)
eform(mLOY_model.sleepdur, level = 0.95) # outputs OR, and 95% CI
summary(mLOY_model.sleepdur)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOY_model: sleep_6 ############# 
mLOY_model_sleep_6 <- glm(mLOY ~ sleep_6 + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
#confint(mLOY_model_chronotype, 'f.1180.0.0', level=0.95)
#summary(mLOY_model_chronotype)
eform(mLOY_model_sleep_6, level = 0.95)
summary(mLOY_model_sleep_6)$coefficients[,4]

############# mLOY_model: chronotype ############# 
mLOY_model_chronotype <- glm(mLOY ~ chronotype + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
#confint(mLOY_model_chronotype, 'f.1180.0.0', level=0.95)
#summary(mLOY_model_chronotype)
eform(mLOY_model_chronotype, level = 0.95)
summary(mLOY_model_chronotype)$coefficients[,4]

############# mLOY_model: Insomnia ############# 
mLOY_model_insomnia <- glm(mLOY ~ insomnia + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_insomnia, 'f.1200.0.0', level=0.95)
# summary(mLOY_model_insomnia)
eform(mLOY_model_insomnia, level = 0.95)
summary(mLOY_model_insomnia)$coefficients[,4]

############# mLOY_model: Snoring ############# 
mLOY_model_snoring <- glm(mLOY ~ snoring +ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_snoring, 'f.1210.0.0', level=0.95)
# summary(mLOY_model_snoring)
eform(mLOY_model_snoring, level = 0.95)
summary(mLOY_model_snoring)$coefficients[,4]

############# mLOY_model: Narcolepsy ############# 
mLOY_model_narcolepsy <- glm(mLOY ~ narcolepsy + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_narcolepsy, 'f.1220.0.0', level=0.95)
# summary(mLOY_model_narcolepsy)
eform(mLOY_model_narcolepsy, level = 0.95)
summary(mLOY_model_narcolepsy)$coefficients[,4]

#################################
# physical activity (PA) models #
#################################

############# mLOY_model: mod_freq_week ############# 
mLOY_model_mod_freq_week <- glm(mLOY ~ mod_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
#confint(mLOY_model_10plusminwalks, 'f.864.0.0', level=0.95)
#summary(mLOY_model_10plusminwalks)
eform(mLOY_model_mod_freq_week, level = 0.95)
summary(mLOY_model_mod_freq_week)$coefficients[,4]

############# mLOY_model: mod_dur_day ############# 
mLOY_model_mod_dur_day <- glm(mLOY ~ mod_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
#confint(mLOY_model_mod_dur_day, 'f.874.0.0', level=0.95)
#summary(mLOY_model_mod_dur_day)
eform(mLOY_model_mod_dur_day, level = 0.95)
summary(mLOY_model_mod_dur_day)$coefficients[,4]

############# mLOY_model: mod_dur_week	 ############# 
mLOY_model_mod_dur_week <- glm(mLOY ~ mod_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_mod_dur_week, 'f.884.0.0', level=0.95)
# summary(mLOY_model_mod_dur_week)
eform(mLOY_model_mod_dur_week, level = 0.95)
summary(mLOY_model_mod_dur_week)$coefficients[,4]

############# mLOY_model: vig_freq_week	############# 
mLOY_model_vig_freq_week <- glm(mLOY ~ vig_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_vig_freq_week, 'f.894.0.0', level=0.95)
# summary(mLOY_model_vig_freq_week)
eform(mLOY_model_vig_freq_week, level = 0.95)
summary(mLOY_model_vig_freq_week)$coefficients[,4]

############# mLOY_model: vig_dur_day ############# 
mLOY_model_vig_dur_day <- glm(mLOY ~ vig_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_vig_dur_day, 'f.904.0.0', level=0.95)
# summary(mLOY_model_vig_dur_day)
eform(mLOY_model_vig_dur_day, level = 0.95)
summary(mLOY_model_vig_dur_day)$coefficients[,4]

############# mLOY_model: vig_dur_week ############# 
mLOY_model_vig_dur_week <- glm(mLOY ~ vig_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_vig_dur_week, 'f.914.0.0', level=0.95)
# summary(mLOY_model_vig_dur_week)
eform(mLOY_model_vig_dur_week, level = 0.95)
summary(mLOY_model_vig_dur_week)$coefficients[,4]

############# mLOY_model: walk_freq_week ############# 
mLOY_model_walk_freq_week <- glm(mLOY ~ walk_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_walk_freq_week, 'f.104900.0.0', level=0.95)
# summary(mLOY_model_walk_freq_week)
eform(mLOY_model_walk_freq_week, level = 0.95)
summary(mLOY_model_walk_freq_week)$coefficients[,4]

############# mLOY_model: walk_dur_day ############# 
mLOY_model_walk_dur_day <- glm(mLOY ~ walk_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_walk_dur_day, 'f.104910.0.0', level=0.95)
# summary(mLOY_model_walk_dur_day)
eform(mLOY_model_walk_dur_day, level = 0.95)
summary(mLOY_model_walk_dur_day)$coefficients[,4]

############# mLOY_model: walk_dur_week ############# 
mLOY_model_walk_dur_week <- glm(mLOY ~ walk_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_walk_dur_week, 'f.104920.0.0', level=0.95)
# summary(mLOY_model_walk_dur_week)
eform(mLOY_model_walk_dur_week, level = 0.95)
summary(mLOY_model_walk_dur_week)$coefficients[,4]

############# mLOY_model: mod_METmin_week ############# 
mLOY_model_mod_METmin_week <- glm(mLOY ~ mod_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_mod_METmin_week, 'f.104920.0.0', level=0.95)
# summary(mLOY_model_mod_METmin_week)
eform(mLOY_model_mod_METmin_week, level = 0.95)
summary(mLOY_model_mod_METmin_week)$coefficients[,4]

############# mLOY_model: vig_METmin_week ############# 
mLOY_model_vig_METmin_week <- glm(mLOY ~ vig_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_vig_METmin_week, 'f.104920.0.0', level=0.95)
# summary(mLOY_model_vig_METmin_week)
eform(mLOY_model_vig_METmin_week, level = 0.95)
summary(mLOY_model_vig_METmin_week)$coefficients[,4]

############# mLOY_model: walk_METmin_week ############# 
mLOY_model_walk_METmin_week <- glm(mLOY ~ walk_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_walk_METmin_week, 'f.104920.0.0', level=0.95)
# summary(mLOY_model_walk_METmin_week)
eform(mLOY_model_walk_METmin_week, level = 0.95)
summary(mLOY_model_walk_METmin_week)$coefficients[,4]

############# mLOY_model: tot_METmin_week ############# 
mLOY_model_tot_METmin_week <- glm(mLOY ~ tot_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_tot_METmin_week, 'f.104920.0.0', level=0.95)
# summary(mLOY_model_tot_METmin_week)
eform(mLOY_model_tot_METmin_week, level = 0.95)
summary(mLOY_model_tot_METmin_week)$coefficients[,4]

############# mLOY_model: tot_dur_week ############# 
mLOY_model_tot_dur_week <- glm(mLOY ~ tot_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_tot_dur_week, 'f.104920.0.0', level=0.95)
# summary(mLOY_model_tot_dur_week)
eform(mLOY_model_tot_dur_week, level = 0.95)
summary(mLOY_model_tot_dur_week)$coefficients[,4]

############# mLOY_model: tot_dur_day ############# 
mLOY_model_tot_dur_day <- glm(mLOY ~ tot_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_tot_dur_day, 'f.104920.0.0', level=0.95)
# summary(mLOY_model_tot_dur_day)
eform(mLOY_model_tot_dur_day, level = 0.95)
summary(mLOY_model_tot_dur_day)$coefficients[,4]

############# mLOY_model: tot_freq_week ############# 
mLOY_model_tot_freq_week <- glm(mLOY ~ tot_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_tot_freq_week, 'f.104920.0.0', level=0.95)
# summary(mLOY_model_tot_freq_week)
eform(mLOY_model_tot_freq_week, level = 0.95)
summary(mLOY_model_tot_freq_week)$coefficients[,4]

############# mLOY_model: IPAQ_CAT ############# 
mLOY_model_IPAQ_CAT <- glm(mLOY ~ IPAQ_CAT + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_IPAQ_CAT, 'f.104920.0.0', level=0.95)
# summary(mLOY_model_IPAQ_CAT)
eform(mLOY_model_IPAQ_CAT, level = 0.95)
summary(mLOY_model_IPAQ_CAT)$coefficients[,4]


#################################
# alcohol models #
#################################
############# mLOY_model: alcohol redwine_wk ############# 
mLOY_model_redwine_wk <- glm(mLOY ~ redwine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_redwine_wk, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_redwine_wk)
eform(mLOY_model_redwine_wk, level = 0.95)
summary(mLOY_model_redwine_wk)$coefficients[,4]

############# mLOY_model: alcohol whitewine_wk ############# 
mLOY_model_whitewine_wk <- glm(mLOY ~ whitewine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_whitewine_wk, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_whitewine_wk)
eform(mLOY_model_whitewine_wk, level = 0.95)
summary(mLOY_model_whitewine_wk)$coefficients[,4]

############# mLOY_model: alcohol beer_wk ############# 
mLOY_model_beer_wk <- glm(mLOY ~ beer_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_beer_wk, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_beer_wk)
eform(mLOY_model_beer_wk, level = 0.95)
summary(mLOY_model_beer_wk)$coefficients[,4]

############# mLOY_model: alcohol spirits_wk ############# 
mLOY_model_spirits_wk <- glm(mLOY ~ spirits_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_spirits_wk, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_spirits_wk)
eform(mLOY_model_spirits_wk, level = 0.95)
summary(mLOY_model_spirits_wk)$coefficients[,4]

############# mLOY_model: alcohol fortwine_wk ############# 
mLOY_model_fortwine_wk <- glm(mLOY ~ fortwine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_fortwine_wk, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_fortwine_wk)
eform(mLOY_model_fortwine_wk, level = 0.95)
summary(mLOY_model_fortwine_wk)$coefficients[,4]

############# mLOY_model: alcohol otheralc_wk ############# 
mLOY_model_otheralc_wk <- glm(mLOY ~ otheralc_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_otheralc_wk, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_otheralc_wk)
eform(mLOY_model_otheralc_wk, level = 0.95)
summary(mLOY_model_otheralc_wk)$coefficients[,4]

############# mLOY_model: alcohol sum_alc_wk ############# 
mLOY_model_sum_alc_wk <- glm(mLOY ~ sum_alc_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_sum_alc_wk, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_sum_alc_wk)
eform(mLOY_model_sum_alc_wk, level = 0.95)
summary(mLOY_model_sum_alc_wk)$coefficients[,4]

############# mLOY_model: alcohol sum_alc_day ############# 
mLOY_model_sum_alc_day <- glm(mLOY ~ sum_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_sum_alc_day, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_sum_alc_day)
eform(mLOY_model_sum_alc_day, level = 0.95)
summary(mLOY_model_sum_alc_day)$coefficients[,4]

############# mLOY_model: alcohol av_alc_day ############# 
mLOY_model_av_alc_day <- glm(mLOY ~ av_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_av_alc_day, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_av_alc_day)
eform(mLOY_model_av_alc_day, level = 0.95)
summary(mLOY_model_av_alc_day)$coefficients[,4]

############# mLOY_model: alcohol av_alc_day_cat ############# 
mLOY_model_av_alc_day_cat <- glm(mLOY ~ av_alc_day_cat + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_av_alc_day_cat, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_av_alc_day_cat)
eform(mLOY_model_av_alc_day_cat, level = 0.95)
summary(mLOY_model_av_alc_day_cat)$coefficients[,4]

############# mLOY_model: alcohol alc_drinker ############# 
mLOY_model_alc_drinker <- glm(mLOY ~ alc_drinker + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_alc_drinker, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_alc_drinker)
eform(mLOY_model_alc_drinker, level = 0.95)
summary(mLOY_model_alc_drinker)$coefficients[,4]

############# mLOY_model: alcohol alc_drinker_cat ############# 
mLOY_model_alc_drinker_cat <- glm(mLOY ~ alc_drinker_cat + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_alc_drinker_cat, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_alc_drinker_cat)
eform(mLOY_model_alc_drinker_cat, level = 0.95)
summary(mLOY_model_alc_drinker_cat)$coefficients[,4]


#################################
# SES models #
#################################
############# mLOY_model: SES Scotland_Index_scaled ############# 
mLOY_model_Scotland_Index_scaled <- glm(mLOY ~ Scotland_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_Scotland_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_Scotland_Index_scaled)
eform(mLOY_model_Scotland_Index_scaled, level = 0.95)
summary(mLOY_model_Scotland_Index_scaled)$coefficients[,4]

############# mLOY_model: SES England_Index_scaled ############# 
mLOY_model_England_Index_scaled <- glm(mLOY ~ England_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_England_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_England_Index_scaled)
eform(mLOY_model_England_Index_scaled, level = 0.95)
summary(mLOY_model_England_Index_scaled)$coefficients[,4]

############# mLOY_model: SES Wales_Index_scaled ############# 
mLOY_model_Wales_Index_scaled <- glm(mLOY ~ Wales_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
# confint(mLOY_model_Wales_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(mLOY_model_Wales_Index_scaled)
eform(mLOY_model_Wales_Index_scaled, level = 0.95)
summary(mLOY_model_Wales_Index_scaled)$coefficients[,4]

#################################
# Shift-work models #
#################################
############# mLOY_model: job_night_shift ############# 
mLOY_model_job_night_shift <- glm(mLOY ~ job_night_shift + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
eform(mLOY_model_job_night_shift, level = 0.95) # outputs OR, and 95% CI
summary(mLOY_model_job_night_shift)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOY_model: job_shift_work ############# 
mLOY_model_job_shift_work <- glm(mLOY ~ job_shift_work + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
eform(mLOY_model_job_shift_work, level = 0.95) # outputs OR, and 95% CI
summary(mLOY_model_job_shift_work)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOY_model: job_shift_worked ############# 
mLOY_model_job_shift_worked <- glm(mLOY ~ job_shift_worked + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
eform(mLOY_model_job_shift_worked, level = 0.95) # outputs OR, and 95% CI
summary(mLOY_model_job_shift_worked)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

###########################################################################
###########################################################################
# CHIP mCA Models #
###########################################################################
###########################################################################

###################
# sleep models #
###################
############# CHIP_model: sleepdur ############# 
CHIP_model.sleepdur <- glm(CHIP ~ sleepdur + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# dataset has binary response (outcome, dependent) variable called CHIP
# predictor variable: f.1160.0.0 (sleepdur) 
# counfounders: f.21003.0.0 (age) + age_squared + f.22001.0.0 (sex) + smoke_detailed + YRI (Afrian) + ASN (Asian) 
# Additional info: age (continous/discrete), sex & smoke (treated as categorical variable)
# get summary and confidence intervals, note = beta is not the OR, needs to be exponentiated
#confint(CHIP_model.sleepdur, 'sleepdur', level=0.95)
#summary(CHIP_model.sleepdur)
eform(CHIP_model.sleepdur, level = 0.95) # outputs OR, and 95% CI
summary(CHIP_model.sleepdur)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# CHIP_model: sleep_6 ############# 
CHIP_model_sleep_6 <- glm(CHIP ~ sleep_6 + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
#confint(CHIP_model_chronotype, 'f.1180.0.0', level=0.95)
#summary(CHIP_model_chronotype)
eform(CHIP_model_sleep_6, level = 0.95)
summary(CHIP_model_sleep_6)$coefficients[,4]

############# CHIP_model: chronotype ############# 
CHIP_model_chronotype <- glm(CHIP ~ chronotype + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
#confint(CHIP_model_chronotype, 'f.1180.0.0', level=0.95)
#summary(CHIP_model_chronotype)
eform(CHIP_model_chronotype, level = 0.95)
summary(CHIP_model_chronotype)$coefficients[,4]

############# CHIP_model: Insomnia ############# 
CHIP_model_insomnia <- glm(CHIP ~ insomnia + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_insomnia, 'f.1200.0.0', level=0.95)
# summary(CHIP_model_insomnia)
eform(CHIP_model_insomnia, level = 0.95)
summary(CHIP_model_insomnia)$coefficients[,4]

############# CHIP_model: Snoring ############# 
CHIP_model_snoring <- glm(CHIP ~ snoring +ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_snoring, 'f.1210.0.0', level=0.95)
# summary(CHIP_model_snoring)
eform(CHIP_model_snoring, level = 0.95)
summary(CHIP_model_snoring)$coefficients[,4]

############# CHIP_model: Narcolepsy ############# 
CHIP_model_narcolepsy <- glm(CHIP ~ narcolepsy + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_narcolepsy, 'f.1220.0.0', level=0.95)
# summary(CHIP_model_narcolepsy)
eform(CHIP_model_narcolepsy, level = 0.95)
summary(CHIP_model_narcolepsy)$coefficients[,4]

#################################
# physical activity (PA) models #
#################################

############# CHIP_model: mod_freq_week ############# 
CHIP_model_mod_freq_week <- glm(CHIP ~ mod_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
#confint(CHIP_model_10plusminwalks, 'f.864.0.0', level=0.95)
#summary(CHIP_model_10plusminwalks)
eform(CHIP_model_mod_freq_week, level = 0.95)
summary(CHIP_model_mod_freq_week)$coefficients[,4]

############# CHIP_model: mod_dur_day ############# 
CHIP_model_mod_dur_day <- glm(CHIP ~ mod_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
#confint(CHIP_model_mod_dur_day, 'f.874.0.0', level=0.95)
#summary(CHIP_model_mod_dur_day)
eform(CHIP_model_mod_dur_day, level = 0.95)
summary(CHIP_model_mod_dur_day)$coefficients[,4]

############# CHIP_model: mod_dur_week	 ############# 
CHIP_model_mod_dur_week <- glm(CHIP ~ mod_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_mod_dur_week, 'f.884.0.0', level=0.95)
# summary(CHIP_model_mod_dur_week)
eform(CHIP_model_mod_dur_week, level = 0.95)
summary(CHIP_model_mod_dur_week)$coefficients[,4]

############# CHIP_model: vig_freq_week	############# 
CHIP_model_vig_freq_week <- glm(CHIP ~ vig_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_vig_freq_week, 'f.894.0.0', level=0.95)
# summary(CHIP_model_vig_freq_week)
eform(CHIP_model_vig_freq_week, level = 0.95)
summary(CHIP_model_vig_freq_week)$coefficients[,4]

############# CHIP_model: vig_dur_day ############# 
CHIP_model_vig_dur_day <- glm(CHIP ~ vig_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_vig_dur_day, 'f.904.0.0', level=0.95)
# summary(CHIP_model_vig_dur_day)
eform(CHIP_model_vig_dur_day, level = 0.95)
summary(CHIP_model_vig_dur_day)$coefficients[,4]

############# CHIP_model: vig_dur_week ############# 
CHIP_model_vig_dur_week <- glm(CHIP ~ vig_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_vig_dur_week, 'f.914.0.0', level=0.95)
# summary(CHIP_model_vig_dur_week)
eform(CHIP_model_vig_dur_week, level = 0.95)
summary(CHIP_model_vig_dur_week)$coefficients[,4]

############# CHIP_model: walk_freq_week ############# 
CHIP_model_walk_freq_week <- glm(CHIP ~ walk_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_walk_freq_week, 'f.104900.0.0', level=0.95)
# summary(CHIP_model_walk_freq_week)
eform(CHIP_model_walk_freq_week, level = 0.95)
summary(CHIP_model_walk_freq_week)$coefficients[,4]

############# CHIP_model: walk_dur_day ############# 
CHIP_model_walk_dur_day <- glm(CHIP ~ walk_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_walk_dur_day, 'f.104910.0.0', level=0.95)
# summary(CHIP_model_walk_dur_day)
eform(CHIP_model_walk_dur_day, level = 0.95)
summary(CHIP_model_walk_dur_day)$coefficients[,4]

############# CHIP_model: walk_dur_week ############# 
CHIP_model_walk_dur_week <- glm(CHIP ~ walk_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_walk_dur_week, 'f.104920.0.0', level=0.95)
# summary(CHIP_model_walk_dur_week)
eform(CHIP_model_walk_dur_week, level = 0.95)
summary(CHIP_model_walk_dur_week)$coefficients[,4]

############# CHIP_model: mod_METmin_week ############# 
CHIP_model_mod_METmin_week <- glm(CHIP ~ mod_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_mod_METmin_week, 'f.104920.0.0', level=0.95)
# summary(CHIP_model_mod_METmin_week)
eform(CHIP_model_mod_METmin_week, level = 0.95)
summary(CHIP_model_mod_METmin_week)$coefficients[,4]

############# CHIP_model: vig_METmin_week ############# 
CHIP_model_vig_METmin_week <- glm(CHIP ~ vig_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_vig_METmin_week, 'f.104920.0.0', level=0.95)
# summary(CHIP_model_vig_METmin_week)
eform(CHIP_model_vig_METmin_week, level = 0.95)
summary(CHIP_model_vig_METmin_week)$coefficients[,4]

############# CHIP_model: walk_METmin_week ############# 
CHIP_model_walk_METmin_week <- glm(CHIP ~ walk_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_walk_METmin_week, 'f.104920.0.0', level=0.95)
# summary(CHIP_model_walk_METmin_week)
eform(CHIP_model_walk_METmin_week, level = 0.95)
summary(CHIP_model_walk_METmin_week)$coefficients[,4]

############# CHIP_model: tot_METmin_week ############# 
CHIP_model_tot_METmin_week <- glm(CHIP ~ tot_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_tot_METmin_week, 'f.104920.0.0', level=0.95)
# summary(CHIP_model_tot_METmin_week)
eform(CHIP_model_tot_METmin_week, level = 0.95)
summary(CHIP_model_tot_METmin_week)$coefficients[,4]

############# CHIP_model: tot_dur_week ############# 
CHIP_model_tot_dur_week <- glm(CHIP ~ tot_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_tot_dur_week, 'f.104920.0.0', level=0.95)
# summary(CHIP_model_tot_dur_week)
eform(CHIP_model_tot_dur_week, level = 0.95)
summary(CHIP_model_tot_dur_week)$coefficients[,4]

############# CHIP_model: tot_dur_day ############# 
CHIP_model_tot_dur_day <- glm(CHIP ~ tot_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_tot_dur_day, 'f.104920.0.0', level=0.95)
# summary(CHIP_model_tot_dur_day)
eform(CHIP_model_tot_dur_day, level = 0.95)
summary(CHIP_model_tot_dur_day)$coefficients[,4]

############# CHIP_model: tot_freq_week ############# 
CHIP_model_tot_freq_week <- glm(CHIP ~ tot_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_tot_freq_week, 'f.104920.0.0', level=0.95)
# summary(CHIP_model_tot_freq_week)
eform(CHIP_model_tot_freq_week, level = 0.95)
summary(CHIP_model_tot_freq_week)$coefficients[,4]

############# CHIP_model: IPAQ_CAT ############# 
CHIP_model_IPAQ_CAT <- glm(CHIP ~ IPAQ_CAT + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_IPAQ_CAT, 'f.104920.0.0', level=0.95)
# summary(CHIP_model_IPAQ_CAT)
eform(CHIP_model_IPAQ_CAT, level = 0.95)
summary(CHIP_model_IPAQ_CAT)$coefficients[,4]


#################################
# alcohol models #
#################################
############# CHIP_model: alcohol redwine_wk ############# 
CHIP_model_redwine_wk <- glm(CHIP ~ redwine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_redwine_wk, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_redwine_wk)
eform(CHIP_model_redwine_wk, level = 0.95)
summary(CHIP_model_redwine_wk)$coefficients[,4]

############# CHIP_model: alcohol whitewine_wk ############# 
CHIP_model_whitewine_wk <- glm(CHIP ~ whitewine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_whitewine_wk, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_whitewine_wk)
eform(CHIP_model_whitewine_wk, level = 0.95)
summary(CHIP_model_whitewine_wk)$coefficients[,4]

############# CHIP_model: alcohol beer_wk ############# 
CHIP_model_beer_wk <- glm(CHIP ~ beer_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_beer_wk, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_beer_wk)
eform(CHIP_model_beer_wk, level = 0.95)
summary(CHIP_model_beer_wk)$coefficients[,4]

############# CHIP_model: alcohol spirits_wk ############# 
CHIP_model_spirits_wk <- glm(CHIP ~ spirits_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_spirits_wk, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_spirits_wk)
eform(CHIP_model_spirits_wk, level = 0.95)
summary(CHIP_model_spirits_wk)$coefficients[,4]

############# CHIP_model: alcohol fortwine_wk ############# 
CHIP_model_fortwine_wk <- glm(CHIP ~ fortwine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_fortwine_wk, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_fortwine_wk)
eform(CHIP_model_fortwine_wk, level = 0.95)
summary(CHIP_model_fortwine_wk)$coefficients[,4]

############# CHIP_model: alcohol otheralc_wk ############# 
CHIP_model_otheralc_wk <- glm(CHIP ~ otheralc_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_otheralc_wk, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_otheralc_wk)
eform(CHIP_model_otheralc_wk, level = 0.95)
summary(CHIP_model_otheralc_wk)$coefficients[,4]

############# CHIP_model: alcohol sum_alc_wk ############# 
CHIP_model_sum_alc_wk <- glm(CHIP ~ sum_alc_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_sum_alc_wk, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_sum_alc_wk)
eform(CHIP_model_sum_alc_wk, level = 0.95)
summary(CHIP_model_sum_alc_wk)$coefficients[,4]

############# CHIP_model: alcohol sum_alc_day ############# 
CHIP_model_sum_alc_day <- glm(CHIP ~ sum_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_sum_alc_day, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_sum_alc_day)
eform(CHIP_model_sum_alc_day, level = 0.95)
summary(CHIP_model_sum_alc_day)$coefficients[,4]

############# CHIP_model: alcohol av_alc_day ############# 
CHIP_model_av_alc_day <- glm(CHIP ~ av_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_av_alc_day, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_av_alc_day)
eform(CHIP_model_av_alc_day, level = 0.95)
summary(CHIP_model_av_alc_day)$coefficients[,4]

############# CHIP_model: alcohol av_alc_day_cat ############# 
CHIP_model_av_alc_day_cat <- glm(CHIP ~ av_alc_day_cat + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_av_alc_day_cat, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_av_alc_day_cat)
eform(CHIP_model_av_alc_day_cat, level = 0.95)
summary(CHIP_model_av_alc_day_cat)$coefficients[,4]

############# CHIP_model: alcohol alc_drinker ############# 
CHIP_model_alc_drinker <- glm(CHIP ~ alc_drinker + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_alc_drinker, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_alc_drinker)
eform(CHIP_model_alc_drinker, level = 0.95)
summary(CHIP_model_alc_drinker)$coefficients[,4]

############# CHIP_model: alcohol alc_drinker_cat ############# 
CHIP_model_alc_drinker_cat <- glm(CHIP ~ alc_drinker_cat + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_alc_drinker_cat, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_alc_drinker_cat)
eform(CHIP_model_alc_drinker_cat, level = 0.95)
summary(CHIP_model_alc_drinker_cat)$coefficients[,4]


#################################
# SES models #
#################################
############# CHIP_model: SES Scotland_Index_scaled ############# 
CHIP_model_Scotland_Index_scaled <- glm(CHIP ~ Scotland_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_Scotland_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_Scotland_Index_scaled)
eform(CHIP_model_Scotland_Index_scaled, level = 0.95)
summary(CHIP_model_Scotland_Index_scaled)$coefficients[,4]

############# CHIP_model: SES England_Index_scaled ############# 
CHIP_model_England_Index_scaled <- glm(CHIP ~ England_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_England_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_England_Index_scaled)
eform(CHIP_model_England_Index_scaled, level = 0.95)
summary(CHIP_model_England_Index_scaled)$coefficients[,4]

############# CHIP_model: SES Wales_Index_scaled ############# 
CHIP_model_Wales_Index_scaled <- glm(CHIP ~ Wales_Index_scaled + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
# confint(CHIP_model_Wales_Index_scaled, 'f.1558.0.0', level=0.95)
# summary(CHIP_model_Wales_Index_scaled)
eform(CHIP_model_Wales_Index_scaled, level = 0.95)
summary(CHIP_model_Wales_Index_scaled)$coefficients[,4]

#################################
# Shift-work models #
#################################
############# CHIP_model: job_night_shift ############# 
CHIP_model_job_night_shift <- glm(CHIP ~ job_night_shift + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
eform(CHIP_model_job_night_shift, level = 0.95) # outputs OR, and 95% CI
summary(CHIP_model_job_night_shift)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# CHIP_model: job_shift_work ############# 
CHIP_model_job_shift_work <- glm(CHIP ~ job_shift_work + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
eform(CHIP_model_job_shift_work, level = 0.95) # outputs OR, and 95% CI
summary(CHIP_model_job_shift_work)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# CHIP_model: job_shift_worked ############# 
CHIP_model_job_shift_worked <- glm(CHIP ~ job_shift_worked + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN, data=CHIP_clean, family=binomial)
eform(CHIP_model_job_shift_worked, level = 0.95) # outputs OR, and 95% CI
summary(CHIP_model_job_shift_worked)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

#################################
# code to build csv for output of model info
#################################
library(broom)

# List of model names
model_names <- c('auto_model.sleepdur', 'auto_model_sleep_6', 'auto_model_chronotype', 'auto_model_insomnia', 'auto_model_snoring', 'auto_model_narcolepsy', 'auto_model_mod_freq_week', 'auto_model_mod_dur_day', 'auto_model_mod_dur_week', 'auto_model_vig_freq_week', 'auto_model_vig_dur_day', 'auto_model_vig_dur_week', 'auto_model_walk_freq_week', 'auto_model_walk_dur_day', 'auto_model_walk_dur_week', 'auto_model_mod_METmin_week', 'auto_model_vig_METmin_week', 'auto_model_walk_METmin_week', 'auto_model_tot_METmin_week', 'auto_model_tot_dur_week', 'auto_model_tot_dur_day', 'auto_model_tot_freq_week', 'auto_model_IPAQ_CAT', 'auto_model_redwine_wk', 'auto_model_whitewine_wk', 'auto_model_beer_wk', 'auto_model_spirits_wk', 'auto_model_fortwine_wk', 'auto_model_otheralc_wk', 'auto_model_sum_alc_wk', 'auto_model_sum_alc_day', 'auto_model_av_alc_day', 'auto_model_av_alc_day_cat', 'auto_model_alc_drinker', 'auto_model_alc_drinker_cat', 'auto_model_Scotland_Index_scaled', 'auto_model_England_Index_scaled', 'auto_model_Wales_Index_scaled', "auto_model_job_night_shift", "auto_model_job_shift_work", "auto_model_job_shift_worked",
                 'mLOX_model.sleepdur', 'mLOX_model_sleep_6', 'mLOX_model_chronotype', 'mLOX_model_insomnia', 'mLOX_model_snoring', 'mLOX_model_narcolepsy', 'mLOX_model_mod_freq_week', 'mLOX_model_mod_dur_day', 'mLOX_model_mod_dur_week', 'mLOX_model_vig_freq_week', 'mLOX_model_vig_dur_day', 'mLOX_model_vig_dur_week', 'mLOX_model_walk_freq_week', 'mLOX_model_walk_dur_day', 'mLOX_model_walk_dur_week', 'mLOX_model_mod_METmin_week', 'mLOX_model_vig_METmin_week', 'mLOX_model_walk_METmin_week', 'mLOX_model_tot_METmin_week', 'mLOX_model_tot_dur_week', 'mLOX_model_tot_dur_day', 'mLOX_model_tot_freq_week', 'mLOX_model_IPAQ_CAT', 'mLOX_model_redwine_wk', 'mLOX_model_whitewine_wk', 'mLOX_model_beer_wk', 'mLOX_model_spirits_wk', 'mLOX_model_fortwine_wk', 'mLOX_model_otheralc_wk', 'mLOX_model_sum_alc_wk', 'mLOX_model_sum_alc_day', 'mLOX_model_av_alc_day', 'mLOX_model_av_alc_day_cat', 'mLOX_model_alc_drinker', 'mLOX_model_alc_drinker_cat', 'mLOX_model_Scotland_Index_scaled', 'mLOX_model_England_Index_scaled', 'mLOX_model_Wales_Index_scaled', "mLOX_model_job_night_shift", "mLOX_model_job_shift_work", "mLOX_model_job_shift_worked",
                 'mLOY_model.sleepdur', 'mLOY_model_sleep_6', 'mLOY_model_chronotype', 'mLOY_model_insomnia', 'mLOY_model_snoring', 'mLOY_model_narcolepsy', 'mLOY_model_mod_freq_week', 'mLOY_model_mod_dur_day', 'mLOY_model_mod_dur_week', 'mLOY_model_vig_freq_week', 'mLOY_model_vig_dur_day', 'mLOY_model_vig_dur_week', 'mLOY_model_walk_freq_week', 'mLOY_model_walk_dur_day', 'mLOY_model_walk_dur_week', 'mLOY_model_mod_METmin_week', 'mLOY_model_vig_METmin_week', 'mLOY_model_walk_METmin_week', 'mLOY_model_tot_METmin_week', 'mLOY_model_tot_dur_week', 'mLOY_model_tot_dur_day', 'mLOY_model_tot_freq_week', 'mLOY_model_IPAQ_CAT', 'mLOY_model_redwine_wk', 'mLOY_model_whitewine_wk', 'mLOY_model_beer_wk', 'mLOY_model_spirits_wk', 'mLOY_model_fortwine_wk', 'mLOY_model_otheralc_wk', 'mLOY_model_sum_alc_wk', 'mLOY_model_sum_alc_day', 'mLOY_model_av_alc_day', 'mLOY_model_av_alc_day_cat', 'mLOY_model_alc_drinker', 'mLOY_model_alc_drinker_cat', 'mLOY_model_Scotland_Index_scaled', 'mLOY_model_England_Index_scaled', 'mLOY_model_Wales_Index_scaled',"mLOY_model_job_night_shift", "mLOY_model_job_shift_work", "mLOY_model_job_shift_worked",
                 'CHIP_model.sleepdur', 'CHIP_model_sleep_6', 'CHIP_model_chronotype', 'CHIP_model_insomnia', 'CHIP_model_snoring', 'CHIP_model_narcolepsy', 'CHIP_model_mod_freq_week', 'CHIP_model_mod_dur_day', 'CHIP_model_mod_dur_week', 'CHIP_model_vig_freq_week', 'CHIP_model_vig_dur_day', 'CHIP_model_vig_dur_week', 'CHIP_model_walk_freq_week', 'CHIP_model_walk_dur_day', 'CHIP_model_walk_dur_week', 'CHIP_model_mod_METmin_week', 'CHIP_model_vig_METmin_week', 'CHIP_model_walk_METmin_week', 'CHIP_model_tot_METmin_week', 'CHIP_model_tot_dur_week', 'CHIP_model_tot_dur_day', 'CHIP_model_tot_freq_week', 'CHIP_model_IPAQ_CAT', 'CHIP_model_redwine_wk', 'CHIP_model_whitewine_wk', 'CHIP_model_beer_wk', 'CHIP_model_spirits_wk', 'CHIP_model_fortwine_wk', 'CHIP_model_otheralc_wk', 'CHIP_model_sum_alc_wk', 'CHIP_model_sum_alc_day', 'CHIP_model_av_alc_day', 'CHIP_model_av_alc_day_cat', 'CHIP_model_alc_drinker', 'CHIP_model_alc_drinker_cat', 'CHIP_model_Scotland_Index_scaled', 'CHIP_model_England_Index_scaled', 'CHIP_model_Wales_Index_scaled', "CHIP_model_job_night_shift", "CHIP_model_job_shift_work", "CHIP_model_job_shift_worked")

# Create an empty data frame to store the results
result_df <- data.frame(Model = character(),
                        Description = character(),
                        OR = character(),
                        'CI (2.5%)' = character(),
                        'CI (97.5%)' = character(),
                        Pvalue = character(),
                        stringsAsFactors = FALSE)

# Iterate over each model
for (model_name in model_names) {
  # Get the model object
  model <- eval(parse(text = model_name))
  # Perform the eform and summary calculations
  eform_output <- eform(model, level = 0.95)
  summary_output <- summary(model)$coefficients[, 4]
  # Extract the first variable from the model name
  first_variable <- substring(model_name, 12)
  # Select the relevant output based on the first variable
  filtered_eform <- subset(eform_output, grepl(first_variable, row.names(eform_output)))
  filtered_summary <- summary_output[grepl(first_variable, names(summary_output))]
  # Convert filtered_eform to a data frame if it has only one row
  #  if (nrow(filtered_eform) == 1) {
  #    filtered_eform <- as.data.frame(filtered_eform, row.names = 1)}
  
  # Combine the results into a single data frame
  model_result <- data.frame(Model = model_name,
                             Description = as.character(rownames(filtered_eform)),
                             OR = as.character(filtered_eform[,1]),
                             'CI (2.5%)' = as.character(filtered_eform[,2]),
                             'CI (97.5%)'= as.character(filtered_eform[,3]),
                             p_value = as.character(filtered_summary),
                             stringsAsFactors = FALSE)
  
  # Append the model result to the overall result data frame
  result_df <- rbind(result_df, model_result)
}


#Addition of Bonferroni Correction
result_df$p_adjust <- p.adjust(result_df$p_value,'bonferroni')
p4_effective_test <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/p4_effective_test.rds')
result_df$meff <- ifelse(as.numeric(result_df$p_value)<p4_effective_test, 'Significant', 'NS')

# Export the result to a CSV file
write.csv(result_df, "model_ORs+CI+pvalue.csv", row.names = FALSE)
#write.csv(result_df, "update2model_output2.csv", row.names = FALSE)


#save.image(file = 'models_E+P+SES+CHIP_workspace.RData')
# saveRDS(result_df, file = 'models_resultv3.rds')
# saveRDS(CHIP_clean, file = 'CHIP_clean.rds')
# saveRDS(auto_mCA_clean, file = 'auto_mCA_clean.rds')
# saveRDS(mLOY_mCA_clean, file = 'mLOY_mCA_clean.rds')
# saveRDS(mLOX_mCA_clean, file = 'mLOX_mCA_clean.rds')

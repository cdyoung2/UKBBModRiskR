##################################################################################################################
# Project 2: UKBB modifiable risk 
# Additional Sensitivity Analysis
# Additional sensitivity analysis in which we explore alcohol and autosomal mCAs & CHIP in females, inlcude bmi adjustments in all signficant vars for AllAdjustedModels & shift-work vars.
# Creates Supplemental Table 3, 4 and 5 (possible; not added to manuscript yet)
##################################################################################################################

rm(list=ls(all=TRUE))
dev.off()
#options(stringsAsFactors = FALSE)

ukbb_final_file <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/ukbb_final_file.rds")
###########################################################################################################

# Part 1: Looking at the relationship between CHIP & autosomal CH and alcohol consumption in females (supp table 3)
############# Building df for running models #############
# Take out people with loss of x or loss of Y from controls of autosomal mCAs.
# Case = 1, control = 0
auto_mCA_clean <- ukbb_final_file[ukbb_final_file$mLOY != 1 & ukbb_final_file$mLOX != 1,]
auto_mCA_clean_f <- auto_mCA_clean[auto_mCA_clean$geneticsex==0,]

#CHIP clean group either CHIP (1) or no mCAs/CHIP etc. (clean group)
#clean set of controls w/o CHIP
CHIP_clean <- ukbb_final_file #comparison is only made between CHIP & non-CHIP groups no removal of mCA groups like below
#CHIP_clean.ref <- ukbb_final_file[ukbb_final_file$mLOY != 1 & ukbb_final_file$mLOX != 1 & ukbb_final_file$auto != 1,]
#CHIP_clean <- CHIP_clean.ref
CHIP_clean_f <- CHIP_clean[CHIP_clean$geneticsex==0,]

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

# Only looking at alcohol
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

###########################################################################
###########################################################################
# CHIP mCA Models #
###########################################################################
###########################################################################

# Only looking at alcohol
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
# code to build csv for output of model info
#################################
library(broom)

# List of model names
model_names <- c('auto_model_redwine_wk', 'auto_model_whitewine_wk', 'auto_model_beer_wk', 'auto_model_spirits_wk', 'auto_model_fortwine_wk', 'auto_model_otheralc_wk', 'auto_model_sum_alc_wk', 'auto_model_sum_alc_day', 'auto_model_av_alc_day', 'auto_model_av_alc_day_cat', 'auto_model_alc_drinker', 'auto_model_alc_drinker_cat', 'CHIP_model_redwine_wk', 'CHIP_model_whitewine_wk', 'CHIP_model_beer_wk', 'CHIP_model_spirits_wk', 'CHIP_model_fortwine_wk', 'CHIP_model_otheralc_wk', 'CHIP_model_sum_alc_wk', 'CHIP_model_sum_alc_day', 'CHIP_model_av_alc_day', 'CHIP_model_av_alc_day_cat', 'CHIP_model_alc_drinker', 'CHIP_model_alc_drinker_cat')

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
lowest_p4 <- min(p4_effective_test)
result_df$p_adjust <- ifelse(result_df$p_value<lowest_p4, 'Significant', 'NS')



# Export the result to a CSV file
write.csv(result_df, "Alcohol_AutoCHIP_Models.csv", row.names = FALSE)

#save.image(file = 'models_E+P+SES+CHIP_workspace.RData')
saveRDS(result_df, file = 'Alcohol_AutoCHIP_Models.rds')
###########################################################################################################

# Part 2: Adding in BMI adjustment for models that were significant in AllAdjustedModels.r (supp table 4)
library(biostat3) #for eform command
library(tidyverse) #tidybroom

# Loading dfs used for models
CHIP_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/CHIP_clean.rds')
auto_mCA_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/auto_mCA_clean.rds')
mLOY_mCA_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/mLOY_mCA_clean.rds')
mLOX_mCA_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/mLOX_mCA_clean.rds')

# Read in data. Logistic regression table output from "models_ipdated_Erikka+Pedro+SES+CHIPv2.R" & identify significant vs NS associations 
logit <- read.csv('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/model_ORs+CI+pvalue.csv', header = TRUE)
p4_effective_test <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/p4_effective_test.rds')
lowest_p4 <- min(p4_effective_test)
logit$meff <- ifelse(logit$p_value<lowest_p4, 'Significant', 'NS')

# How many associations are <0.05 & how many are <adjusted p-value 
length(which(logit$p_value<=0.05)) #45
length(which(logit$p_value<=p4_effective_test)) #16
logit_sign <- logit[which(logit$p_value<=0.05),] # only associations < 0.05

# 39 models (minus shift-work models; part-3) ran again with continuous BMI as a covariate (42 significant associations)
############# auto_model: vig_dur_day ############# 
auto_model_vig_dur_day <- glm(auto ~ vig_dur_day + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)

############# auto_model: vig_dur_week ############# 
auto_model_vig_dur_week <- glm(auto ~ vig_dur_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)
#eform(auto_model_vig_dur_week, level = 0.95)
#summary(auto_model_vig_dur_week)$coefficients[,4]
############# auto_model: vig_METmin_week ############# 
auto_model_vig_METmin_week <- glm(auto ~ vig_METmin_week + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)

############# auto_model: tot_dur_day ############# 
auto_model_tot_dur_day <- glm(auto ~ tot_dur_day + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)


############# mLOX_model: IPAQ_CAT ############# 
mLOX_model_IPAQ_CAT <- glm(mLOX ~ IPAQ_CAT + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)

############# mLOX_model: alcohol whitewine_wk ############# 
mLOX_model_whitewine_wk <- glm(mLOX ~ whitewine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)

############# mLOX_model: alcohol sum_alc_wk ############# 
mLOX_model_sum_alc_wk <- glm(mLOX ~ sum_alc_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)

############# mLOX_model: alcohol sum_alc_day ############# 
mLOX_model_sum_alc_day <- glm(mLOX ~ sum_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)

############# mLOX_model: alcohol av_alc_day ############# 
mLOX_model_av_alc_day <- glm(mLOX ~ av_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)

############# mLOX_model: alcohol av_alc_day_cat ############# 
mLOX_model_av_alc_day_cat <- glm(mLOX ~ av_alc_day_cat + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)

############# mLOX_model: alcohol alc_drinker_cat ############# 
mLOX_model_alc_drinker_cat <- glm(mLOX ~ alc_drinker_cat + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)

############# mLOX_model: SES England_Index ############# 
mLOX_model_England_Index <- glm(mLOX ~ England_Index + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)


############# mLOY_model: sleep_6 ############# 
mLOY_model_sleep_6 <- glm(mLOY ~ sleep_6 + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: Narcolepsy ############# 
mLOY_model_narcolepsy <- glm(mLOY ~ narcolepsy + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: mod_freq_week ############# 
mLOY_model_mod_freq_week <- glm(mLOY ~ mod_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: walk_freq_week ############# 
mLOY_model_walk_freq_week <- glm(mLOY ~ walk_freq_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: walk_dur_day ############# 
mLOY_model_walk_dur_day <- glm(mLOY ~ walk_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: walk_dur_week ############# 
mLOY_model_walk_dur_week <- glm(mLOY ~ walk_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: walk_METmin_week ############# 
mLOY_model_walk_METmin_week <- glm(mLOY ~ walk_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: tot_METmin_week ############# 
mLOY_model_tot_METmin_week <- glm(mLOY ~ tot_METmin_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: tot_dur_week ############# 
mLOY_model_tot_dur_week <- glm(mLOY ~ tot_dur_week + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: tot_dur_day ############# 
mLOY_model_tot_dur_day <- glm(mLOY ~ tot_dur_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: IPAQ_CAT ############# 
mLOY_model_IPAQ_CAT <- glm(mLOY ~ IPAQ_CAT + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: alcohol redwine_wk ############# 
mLOY_model_redwine_wk <- glm(mLOY ~ redwine_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: alcohol beer_wk ############# 
mLOY_model_beer_wk <- glm(mLOY ~ beer_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: alcohol spirits_wk ############# 
mLOY_model_spirits_wk <- glm(mLOY ~ spirits_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: alcohol sum_alc_wk ############# 
mLOY_model_sum_alc_wk <- glm(mLOY ~ sum_alc_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: alcohol sum_alc_day ############# 
mLOY_model_sum_alc_day <- glm(mLOY ~ sum_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: alcohol alc_drinker_cat ############# 
mLOY_model_alc_drinker_cat <- glm(mLOY ~ alc_drinker_cat + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: SES Scotland_Index ############# 
mLOY_model_Scotland_Index <- glm(mLOY ~ Scotland_Index + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)

############# mLOY_model: SES England_Index ############# 
mLOY_model_England_Index <- glm(mLOY ~ England_Index + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)


############# CHIP_model: sleepdur ############# 
CHIP_model.sleepdur <- glm(CHIP ~ sleepdur + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

############# CHIP_model: sleep_6 ############# 
CHIP_model_sleep_6 <- glm(CHIP ~ sleep_6 + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

############# CHIP_model: chronotype ############# 
CHIP_model_chronotype <- glm(CHIP ~ chronotype + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

############# CHIP_model: Insomnia ############# 
CHIP_model_insomnia <- glm(CHIP ~ insomnia + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

############# CHIP_model: alcohol spirits_wk ############# 
CHIP_model_spirits_wk <- glm(CHIP ~ spirits_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

############# CHIP_model: alcohol sum_alc_wk ############# 
CHIP_model_sum_alc_wk <- glm(CHIP ~ sum_alc_wk + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

############# CHIP_model: alcohol sum_alc_day ############# 
CHIP_model_sum_alc_day <- glm(CHIP ~ sum_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

############# CHIP_model: alcohol av_alc_day ############# 
CHIP_model_av_alc_day <- glm(CHIP ~ av_alc_day + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

# re-purposing code from models_ipdated_Erikka+Pedro+SES+CHIPv2.R to build csv for logit information
# model names
model_names <- c('auto_model_vig_dur_day', 'auto_model_vig_dur_week', 'auto_model_vig_METmin_week', 'auto_model_tot_dur_day', 'mLOX_model_IPAQ_CAT', 'mLOX_model_whitewine_wk', 'mLOX_model_sum_alc_wk', 'mLOX_model_sum_alc_day', 'mLOX_model_av_alc_day', 'mLOX_model_av_alc_day_cat', 'mLOX_model_alc_drinker_cat', 'mLOX_model_England_Index', 'mLOY_model_sleep_6', 'mLOY_model_narcolepsy', 'mLOY_model_mod_freq_week', 'mLOY_model_walk_freq_week', 'mLOY_model_walk_dur_day', 'mLOY_model_walk_dur_week', 'mLOY_model_walk_METmin_week', 'mLOY_model_tot_METmin_week', 'mLOY_model_tot_dur_week', 'mLOY_model_tot_dur_day', 'mLOY_model_IPAQ_CAT', 'mLOY_model_redwine_wk', 'mLOY_model_beer_wk', 'mLOY_model_spirits_wk', 'mLOY_model_sum_alc_wk', 'mLOY_model_sum_alc_day', 'mLOY_model_alc_drinker_cat', 'mLOY_model_Scotland_Index', 'mLOY_model_England_Index', 'CHIP_model.sleepdur', 'CHIP_model_sleep_6', 'CHIP_model_chronotype', 'CHIP_model_insomnia', 'CHIP_model_spirits_wk', 'CHIP_model_sum_alc_wk', 'CHIP_model_sum_alc_day', 'CHIP_model_av_alc_day')

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
# Export the result to a CSV file
write.csv(result_df, "/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/significant_models+BMI_association.csv", row.names = FALSE)

saveRDS(result_df, file = '/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/significant_models+BMI_association.rds')
# Final table is likely called "Comparison of Significant Associations in Models with and without BMI Adjustment.xlsx"
##################################################################################################################

# Part 3: Looking at relationship of shift-work variables with bmi (adding in bmi adjustment)
# Rerunning models with bmi adjustment included to see change
# bmi continous and bmi.cat adjustments were done previously (not much difference between them, so I just went w/bmi)
 
# Loading dfs used for models
CHIP_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/CHIP_clean.rds')
auto_mCA_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/auto_mCA_clean.rds')
mLOY_mCA_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/mLOY_mCA_clean.rds')
mLOX_mCA_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/mLOX_mCA_clean.rds')

# Read in data. Logistic regression table output from "models_ipdated_Erikka+Pedro+SES+CHIPv2.R" & identify significant vs NS associations 
logit <- read.csv('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/model_ORs+CI+pvalue.csv', header = TRUE)
p4_effective_test <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/p4_effective_test.rds')
lowest_p4 <- min(p4_effective_test)
logit$meff <- ifelse(logit$p_value<lowest_p4, 'Significant', 'NS')

# How many associations are <0.05 & how many are <adjusted p-value 
length(which(logit$p_value<=0.05)) #45
length(which(logit$p_value<=p4_effective_test)) #16
logit_sign <- logit[which(logit$p_value<=0.05),] # only associations < 0.05

library(biostat3) #for eform command
library(tidyverse) #tidybroom

############# auto_model: job_night_shift ############# 
auto_model_job_night_shift <- glm(auto ~ job_night_shift + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)
eform(auto_model_job_night_shift, level = 0.95) # outputs OR, and 95% CI
summary(auto_model_job_night_shift)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# auto_model: job_shift_work ############# 
auto_model_job_shift_work <- glm(auto ~ job_shift_work + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)
eform(auto_model_job_shift_work, level = 0.95) # outputs OR, and 95% CI
summary(auto_model_job_shift_work)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# auto_model: job_shift_worked ############# 
auto_model_job_shift_worked <- glm(auto ~ job_shift_worked + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)
eform(auto_model_job_shift_worked, level = 0.95) # outputs OR, and 95% CI
summary(auto_model_job_shift_worked)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.


############# mLOX_model: job_night_shift ############# 
mLOX_model_job_night_shift <- glm(mLOX ~ job_night_shift + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)
eform(mLOX_model_job_night_shift, level = 0.95) # outputs OR, and 95% CI
summary(mLOX_model_job_night_shift)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOX_model: job_shift_work ############# 
mLOX_model_job_shift_work <- glm(mLOX ~ job_shift_work + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)
eform(mLOX_model_job_shift_work, level = 0.95) # outputs OR, and 95% CI
summary(mLOX_model_job_shift_work)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOX_model: job_shift_worked ############# 
mLOX_model_job_shift_worked <- glm(mLOX ~ job_shift_worked + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)
eform(mLOX_model_job_shift_worked, level = 0.95) # outputs OR, and 95% CI
summary(mLOX_model_job_shift_worked)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.


############# mLOY_model: job_night_shift ############# 
mLOY_model_job_night_shift <- glm(mLOY ~ job_night_shift + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)
eform(mLOY_model_job_night_shift, level = 0.95) # outputs OR, and 95% CI
summary(mLOY_model_job_night_shift)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOY_model: job_shift_work ############# 
mLOY_model_job_shift_work <- glm(mLOY ~ job_shift_work + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)
eform(mLOY_model_job_shift_work, level = 0.95) # outputs OR, and 95% CI
summary(mLOY_model_job_shift_work)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# mLOY_model: job_shift_worked ############# 
mLOY_model_job_shift_worked <- glm(mLOY ~ job_shift_worked + ageatassess + age_squared + as.factor(smok_detailed) + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)
eform(mLOY_model_job_shift_worked, level = 0.95) # outputs OR, and 95% CI
summary(mLOY_model_job_shift_worked)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.


############# CHIP_model: job_night_shift ############# 
CHIP_model_job_night_shift <- glm(CHIP ~ job_night_shift + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)
eform(CHIP_model_job_night_shift, level = 0.95) # outputs OR, and 95% CI
summary(CHIP_model_job_night_shift)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# CHIP_model: job_shift_work ############# 
CHIP_model_job_shift_work <- glm(CHIP ~ job_shift_work + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)
eform(CHIP_model_job_shift_work, level = 0.95) # outputs OR, and 95% CI
summary(CHIP_model_job_shift_work)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.

############# CHIP_model: job_shift_worked ############# 
CHIP_model_job_shift_worked <- glm(CHIP ~ job_shift_worked + ageatassess + age_squared + as.factor(geneticsex) + as.factor(smok_detailed) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)
eform(CHIP_model_job_shift_worked, level = 0.95) # outputs OR, and 95% CI
summary(CHIP_model_job_shift_worked)$coefficients[,4] #give coordinates of the coefficients you want pvalues for.


# code to build csv for output of model info
library(broom)
# List of model names
model_names <- c('auto_model_job_night_shift', 'auto_model_job_shift_work', 'auto_model_job_shift_worked', 'mLOX_model_job_night_shift', 'mLOX_model_job_shift_work', "mLOX_model_job_shift_worked", 'mLOY_model_job_night_shift', 'mLOY_model_job_shift_work', 'mLOY_model_job_shift_worked', 'CHIP_model_job_night_shift', 'CHIP_model_job_shift_work', 'CHIP_model_job_shift_worked')

# Create an empty data frame to store the results
result_df_bmi <- data.frame(Model = character(),
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
  model_result_bmi <- data.frame(Model = model_name,
                                 Description = as.character(rownames(filtered_eform)),
                                 OR = as.character(filtered_eform[,1]),
                                 'CI (2.5%)' = as.character(filtered_eform[,2]),
                                 'CI (97.5%)'= as.character(filtered_eform[,3]),
                                 p_value = as.character(filtered_summary),
                                 stringsAsFactors = FALSE)
  
  # Append the model result to the overall result data frame
  result_df_bmi <- rbind(result_df_bmi, model_result_bmi)
}
result_df_bmi

# Export the result to a CSV file
write.csv(result_df_bmi, "shift_work_ORs_bmiadjusted.csv", row.names = FALSE)

saveRDS(result_df_bmi, file = 'shift_work_ORs_bmiadjusted.rds')
###########################################################################################################















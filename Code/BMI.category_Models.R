##################################################################################################################
# Project 2: UKBB Modifiable Risk Factors
# Simple code to run models looking into BMI
##################################################################################################################
rm(list=ls(all=TRUE))
dev.off()
#options(stringsAsFactors = FALSE)
setwd('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/')

# Bring in data need to run this analysis
# This ukbb_final_file is current most current ukbb_final_file + bmi from 'models_updated_Erikka+Pedro+SES+CHIP+BMI.R'
# ukbb_final_file <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/ukbb_final_file+BMI.rds")
ukbb_final_file <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/ukbb_final_file.rds")

# Update the bmi variable into for models
# ukbb_final_file$bmi.cat <- cut(ukbb_final_file$bmi, c(0, 18.5, 25, 30, 40, 125), labels = c("<18.5","18.5 - <25","25 - <30","30 - <40","≥ 40"))
# # Setting reference to 18.5 - 24.9 category
# lvl.1 <- c("<18.5","18.5 - <25","25 - <30","30 - <40","≥ 40")
# ukbb_final_file$bmi.cat <- factor(ukbb_final_file$bmi.cat, levels = lvl.1)
# ukbb_final_file$bmi.cat <- factor(ukbb_final_file$bmi.cat, ordered = FALSE)
# ukbb_final_file$bmi.cat <-relevel(ukbb_final_file$bmi.cat, ref = 2)

library(biostat3) #for eform command
library(tidyverse) #tidybroom

# Building data frames for auto, mLOX, mLOY & CHIP with updated ukbb_final_file made earlier 
# Take out people with loss of x or loss of Y from controls of autosomal mCAs, Case = 1, control = 0
auto_mCA_clean <- ukbb_final_file[ukbb_final_file$mLOY != 1 & ukbb_final_file$mLOX != 1,]
#Take out people with autosomal mCAs from Loss of Y controls, Subset to women only for Loss of Y analyses
mLOX_mCA_clean.ref <- ukbb_final_file[ukbb_final_file$auto != 1,]
mLOX_mCA_clean <- mLOX_mCA_clean.ref[mLOX_mCA_clean.ref$geneticsex==0,]
#Take out people with autosomal mCAs from Loss of X controls, Subset to men only of Loss of X analyses
mLOY_mCA_clean.ref <- ukbb_final_file[ukbb_final_file$auto != 1,]
mLOY_mCA_clean <- mLOY_mCA_clean.ref[mLOY_mCA_clean.ref$geneticsex==1,]
#CHIP clean group either CHIP (1) or no mCAs/CHIP etc. (clean group), clean set of controls w/o CHIP or mCAs
CHIP_clean <- ukbb_final_file #comparison is only made between CHIP & non-CHIP groups no removal of mCA groups like below

# Running logistic regression models to 
auto_model_bmi.cat <- glm(auto ~ as.factor(bmi.cat) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN, data=auto_mCA_clean, family=binomial)
#eform(auto_model_bmi.cat, level = 0.95)
#summary(auto_model_bmi.cat)$coefficients[,4]
mLOX_model_bmi.cat <- glm(mLOX ~ as.factor(bmi.cat) + ageatassess + age_squared + YRI + ASN, data=mLOX_mCA_clean, family=binomial)
mLOY_model_bmi.cat <- glm(mLOY ~ as.factor(bmi.cat) + ageatassess + age_squared + YRI + ASN, data=mLOY_mCA_clean, family=binomial)
CHIP_model_bmi.cat <- glm(CHIP ~ as.factor(bmi.cat) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN, data=CHIP_clean, family=binomial)

model_names <- c('auto_model_bmi.cat', 'mLOX_model_bmi.cat', 'mLOY_model_bmi.cat', 'CHIP_model_bmi.cat')

# Create an empty data frame to store the results
result_df <- data.frame(Model = character(), Description = character(), OR = character(), 'CI_(2.5%)' = character(), 'CI_(97.5%)' = character(),Pvalue = character(), stringsAsFactors = FALSE)

# Iterate over each model
for (model_name in model_names) {
  model <- eval(parse(text = model_name)) # Get the model object
  eform_output <- eform(model, level = 0.95)   # Perform the eform and summary calculations
  summary_output <- summary(model)$coefficients[, 4] # Extract the first variable from the model name
  first_variable <- substring(model_name, 12) # Select the relevant output based on the first variable
  filtered_eform <- subset(eform_output, grepl(first_variable, row.names(eform_output)))
  filtered_summary <- summary_output[grepl(first_variable, names(summary_output))]
  # Convert filtered_eform to a data frame if it has only one row
  #  if (nrow(filtered_eform) == 1) {
  #    filtered_eform <- as.data.frame(filtered_eform, row.names = 1)}
  
  # Combine the results into a single data frame
  model_result <- data.frame(Model = model_name, Description = as.character(rownames(filtered_eform)), OR = as.character(filtered_eform[,1]), 
                             'CI (2.5%)' = as.character(filtered_eform[,2]), 'CI (97.5%)'= as.character(filtered_eform[,3]), p_value = as.character(filtered_summary),
                             stringsAsFactors = FALSE)
  
  # Append the model result to the overall result data frame
  result_df <- rbind(result_df, model_result)
}
bmi.cat_models_results <- result_df

# Remove "as.factor(smok_detailed_quit_years2)" from the Description column
bmi.cat_models_results$Description <- sub("as\\.factor\\(bmi.cat\\)\\s*", "", bmi.cat_models_results$Description)
print(bmi.cat_models_results)
##################################################################################################################
saveRDS(bmi.cat_models_results,'bmi.cat_models_resultsv2.rds')
write.csv(bmi.cat_models_results, "bmi.cat_models_resultsv2.csv", row.names = FALSE)

# Code Generating a vertical bar grpah that detail association values between smoking status (smok_Detailed_quit_years2) has been removed

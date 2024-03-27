##################################################################################################################
# Project 2: UKBB Modifiable Risk Factors
# Simple code to run models differences across never, former & current smokers
# Output as one of panes for forest plot figure (smoking)
##################################################################################################################
rm(list=ls(all=TRUE))
dev.off()
#options(stringsAsFactors = FALSE)
setwd('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/')

# Bring in data need to run this analysis
# This ukbb_final_file is current most current ukbb_final_file + bmi from 'models_updated_Erikka+Pedro+SES+CHIP+BMI.R'
#ukbb_final_file <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/ukbb_final_file+BMI+smoke_cessationVars.rds")
ukbb_final_file <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Older_Run/Rdata/ukbb_final_file+BMI+smoke_cessationVars.rds")

# Setting Never Smoker as the ref category
table(ukbb_final_file$smok_detailed_NFC, useNA = 'ifany')
lvl.1 <- c("Current Smoker", "Former Smoker", "Never Smoker")
ukbb_final_file$smok_detailed_NFC <- factor(ukbb_final_file$smok_detailed_NFC, levels = lvl.1)
ukbb_final_file$smok_detailed_NFC <- factor(ukbb_final_file$smok_detailed_NFC, ordered = FALSE)
ukbb_final_file$smok_detailed_NFC <-relevel(ukbb_final_file$smok_detailed_NFC, ref = 3)


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
auto_model_smok_detailed_NFC <- glm(auto ~ as.factor(smok_detailed_NFC) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)
eform(auto_model_smok_detailed_NFC, level = 0.95)
summary(auto_model_smok_detailed_NFC)$coefficients[,4]
mLOX_model_smok_detailed_NFC <- glm(mLOX ~ as.factor(smok_detailed_NFC) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)
mLOY_model_smok_detailed_NFC <- glm(mLOY ~ as.factor(smok_detailed_NFC) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)
CHIP_model_smok_detailed_NFC <- glm(CHIP ~ as.factor(smok_detailed_NFC) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

model_names <- c('auto_model_smok_detailed_NFC', 'mLOX_model_smok_detailed_NFC', 'mLOY_model_smok_detailed_NFC', 'CHIP_model_smok_detailed_NFC')

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
smok_detailed_NFC_models_results <- result_df

# Remove "as.factor(smok_detailed_quit_years2)" from the Description column
smok_detailed_NFC_models_results$Description <- sub("as\\.factor\\(smok_detailed_NFC\\)\\s*", "", smok_detailed_NFC_models_results$Description)
print(smok_detailed_NFC_models_results)
##################################################################################################################
saveRDS(smok_detailed_NFC_models_results,'smok_detailed_NFC_models_results.rds')
write.csv(smok_detailed_NFC_models_results, "smok_detailed_NFC_models_results.csv", row.names = FALSE)

# Code Generating a vertical bar grpah that detail association values between smoking status (smok_Detailed_quit_years2) has been removed


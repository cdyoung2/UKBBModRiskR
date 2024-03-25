##################################################################################################################
# UKBB Modifiable Risk Factors
# Smoking Cessation Analysis v3
# Table for smoking cessation & bar plot construction of ORs based on quit-years, current smoking (cs) & cs intensity based on smoke_detailed info
# Figure 2,  
# You can run part 1-5 separately w/o running other parts
##################################################################################################################
rm(list=ls(all=TRUE))
dev.off()
#options(stringsAsFactors = FALSE)
setwd('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/')

# This ukbb_final_file is current most current ukbb_final_file + bmi from 'models_updated_Erikka+Pedro+SES+CHIP+BMI.R'
ukbb_final_file <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/ukbb_final_file.rds")

# lets load in dfs used for models (created in models_ipdated_Erikka+Pedro+SES+CHIPv2.R)
auto_mCA_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/auto_mCA_clean.rds')
mLOX_mCA_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/mLOX_mCA_clean.rds')
mLOY_mCA_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/mLOY_mCA_clean.rds')
CHIP_clean <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/CHIP_clean.rds')

# Building datasets used in below analysis (smoke_detailed_NFC/smok_detailed_quit_years [table], smok_detailed_quit_years2 [OG barplot], smok_detailed_f2c_OR [updated barplot])
# Update the smok_detailed variable into what I want to showcase in the table
ukbb_final_file$smok_detailed_NFC <- ifelse(
  grepl("current", ukbb_final_file$smok_detailed, ignore.case = TRUE),"Current Smoker", ukbb_final_file$smok_detailed)
ukbb_final_file$smok_detailed_NFC <- ifelse(
  grepl("never", ukbb_final_file$smok_detailed_NFC, ignore.case = TRUE),"Never Smoker", ukbb_final_file$smok_detailed_NFC)
ukbb_final_file$smok_detailed_NFC <- ifelse(
  grepl("former", ukbb_final_file$smok_detailed_NFC, ignore.case = TRUE),"Former Smoker", ukbb_final_file$smok_detailed_NFC)
ukbb_final_file$smok_detailed_NFC <- ifelse(
  grepl("missing", ukbb_final_file$smok_detailed_NFC, ignore.case = TRUE), NA, ukbb_final_file$smok_detailed_NFC)
# Analysis is not looking at intenstiy of smoking so we are going to reduce categories just based on quit year
ukbb_final_file$smok_detailed_quit_years <- ifelse(
  grepl("quit <1 year ago", ukbb_final_file$smok_detailed, ignore.case = TRUE),"≤1 quit-years", ukbb_final_file$smok_detailed)
ukbb_final_file$smok_detailed_quit_years <- ifelse(
  grepl("quit 1-5 year ago", ukbb_final_file$smok_detailed_quit_years, ignore.case = TRUE),"1 - 5 quit-years", ukbb_final_file$smok_detailed_quit_years)
ukbb_final_file$smok_detailed_quit_years <- ifelse(
  grepl("quit 5-10 year ago", ukbb_final_file$smok_detailed_quit_years, ignore.case = TRUE),"5 - 10 quit-years", ukbb_final_file$smok_detailed_quit_years)
ukbb_final_file$smok_detailed_quit_years <- ifelse(
  grepl("quit 10-20 year ago", ukbb_final_file$smok_detailed_quit_years, ignore.case = TRUE),"10 - 20 quit-years", ukbb_final_file$smok_detailed_quit_years)
ukbb_final_file$smok_detailed_quit_years <- ifelse(
  grepl("quit ≥20 year ago", ukbb_final_file$smok_detailed_quit_years, ignore.case = TRUE),"≥ 20 quit-years", ukbb_final_file$smok_detailed_quit_years)
ukbb_final_file$smok_detailed_quit_years <- ifelse(
  grepl("current", ukbb_final_file$smok_detailed_quit_years, ignore.case = TRUE) | #everything that has current word
    grepl("former", ukbb_final_file$smok_detailed_quit_years, ignore.case = TRUE) | #everything that has former word
    grepl("never", ukbb_final_file$smok_detailed_quit_years, ignore.case = TRUE), #everything that has never word
  "missing", ukbb_final_file$smok_detailed_quit_years) #adding to missing category all over rows that do not have quit-year data to be omitted in table formation
ukbb_final_file$smok_detailed_quit_years <- ifelse(
  grepl("missing", ukbb_final_file$smok_detailed_quit_years, ignore.case = TRUE), NA, ukbb_final_file$smok_detailed_quit_years)

# Replicate the building of smok_detailed_quit_years but for logistic regression analysis where we keep in never smokers as the reference category
ukbb_final_file$smok_detailed_quit_years2 <- ifelse(
  grepl("quit <1 year ago", ukbb_final_file$smok_detailed, ignore.case = TRUE),"≤1 quit-years", ukbb_final_file$smok_detailed)
ukbb_final_file$smok_detailed_quit_years2 <- ifelse(
  grepl("quit 1-5 year ago", ukbb_final_file$smok_detailed_quit_years2, ignore.case = TRUE),"1 - 5 quit-years", ukbb_final_file$smok_detailed_quit_years2)
ukbb_final_file$smok_detailed_quit_years2 <- ifelse(
  grepl("quit 5-10 year ago", ukbb_final_file$smok_detailed_quit_years2, ignore.case = TRUE),"5 - 10 quit-years", ukbb_final_file$smok_detailed_quit_years2)
ukbb_final_file$smok_detailed_quit_years2 <- ifelse(
  grepl("quit 10-20 year ago", ukbb_final_file$smok_detailed_quit_years2, ignore.case = TRUE),"10 - 20 quit-years", ukbb_final_file$smok_detailed_quit_years2)
ukbb_final_file$smok_detailed_quit_years2 <- ifelse(
  grepl("quit ≥20 year ago", ukbb_final_file$smok_detailed_quit_years2, ignore.case = TRUE),"≥ 20 quit-years", ukbb_final_file$smok_detailed_quit_years2)
ukbb_final_file$smok_detailed_quit_years2 <- ifelse(
  grepl("former", ukbb_final_file$smok_detailed_quit_years2, ignore.case = TRUE),"Other Former Smoker (no Quit-Years)", ukbb_final_file$smok_detailed_quit_years2)
ukbb_final_file$smok_detailed_quit_years2 <- ifelse(
  grepl("current", ukbb_final_file$smok_detailed_quit_years2, ignore.case = TRUE),"Current Smoker", ukbb_final_file$smok_detailed_quit_years2)
ukbb_final_file$smok_detailed_quit_years2 <- ifelse(
  grepl("never", ukbb_final_file$smok_detailed_quit_years2, ignore.case = TRUE),"Never Smoker", ukbb_final_file$smok_detailed_quit_years2)

# Replicate the building of smok_detailed_quit_years2 but for logistic regression analysis where we keep in never smokers as the reference category [updated barplot]
ukbb_final_file$smok_detailed_f2c_OR <- ifelse(
  grepl("quit <1 year ago", ukbb_final_file$smok_detailed, ignore.case = TRUE),"≤1 quit-years", ukbb_final_file$smok_detailed)
ukbb_final_file$smok_detailed_f2c_OR <- ifelse(
  grepl("quit 1-5 year ago", ukbb_final_file$smok_detailed_f2c_OR, ignore.case = TRUE),"1 - 5 quit-years", ukbb_final_file$smok_detailed_f2c_OR)
ukbb_final_file$smok_detailed_f2c_OR <- ifelse(
  grepl("quit 5-10 year ago", ukbb_final_file$smok_detailed_f2c_OR, ignore.case = TRUE),"5 - 10 quit-years", ukbb_final_file$smok_detailed_f2c_OR)
ukbb_final_file$smok_detailed_f2c_OR <- ifelse(
  grepl("quit 10-20 year ago", ukbb_final_file$smok_detailed_f2c_OR, ignore.case = TRUE),"10 - 20 quit-years", ukbb_final_file$smok_detailed_f2c_OR)
ukbb_final_file$smok_detailed_f2c_OR <- ifelse(
  grepl("quit ≥20 year ago", ukbb_final_file$smok_detailed_f2c_OR, ignore.case = TRUE),"≥ 20 quit-years", ukbb_final_file$smok_detailed_f2c_OR)
ukbb_final_file$smok_detailed_f2c_OR <- ifelse(
  grepl("former", ukbb_final_file$smok_detailed_f2c_OR, ignore.case = TRUE),"Other Former Smoker (no Quit-Years)", ukbb_final_file$smok_detailed_f2c_OR)
ukbb_final_file$smok_detailed_f2c_OR <- ifelse(
  grepl("never", ukbb_final_file$smok_detailed_f2c_OR, ignore.case = TRUE),"Never Smoker", ukbb_final_file$smok_detailed_f2c_OR)
ukbb_final_file$smok_detailed_f2c_OR <- ifelse(
  grepl("current occasional", ukbb_final_file$smok_detailed_f2c_OR, ignore.case = TRUE),"Current Occasional Smoker", ukbb_final_file$smok_detailed_f2c_OR)
##################################################################################################################

# Part 1: Table of quit-year breakdown, inlcudes mloy, mLOX, auto & CHIP & breakdown of never, former or current smoker
# Need to order the quit_years levels for the table generation, but causes issues w/model so I made smok_detailed_quit_years2 above
# Define custom levels for the smok_detailed_quit_years column
custom_levels <- c("≤1 quit-years", "1 - 5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years")
# Convert the smok_detailed_quit_years column to a factor with custom levels
ukbb_final_file$smok_detailed_quit_years <- factor(ukbb_final_file$smok_detailed_quit_years,  levels = custom_levels,  ordered = TRUE)
# Check the order of the levels
levels(ukbb_final_file$smok_detailed_quit_years)

# Generate auto, mLOX, mLOY & CHIP data frames w/new cols (Case = 1, control = 0)
ukbb_final_file_auto <- ukbb_final_file[which(ukbb_final_file$auto==1),]
ukbb_final_file_mLOX <- ukbb_final_file[which(ukbb_final_file$mLOX==1),]
ukbb_final_file_mLOY <- ukbb_final_file[which(ukbb_final_file$mLOY==1),]
ukbb_final_file_CHIP <- ukbb_final_file[which(ukbb_final_file$CHIP==1),]
ukbb_final_file_no <- ukbb_final_file[which(ukbb_final_file$CH_value==0),]

#saveRDS(ukbb_final_file, file = 'ukbb_final_file+BMI+smoke_cessationVars.rds')

# Load libraries
library(gtsummary)
library(tidyverse)
#library(flextable)

# Construct label list for gtsummary 
lable.list <- list(smok_detailed_NFC ~ 'Smoker Status', smok_detailed_quit_years ~ 'Quit-Years for Former Smokers')

# Table creation
# easy_merged_total <- ukbb_final_file %>%
#  select(smok_detailed_NFC, smok_detailed_quit_years) %>%
#  tbl_summary(label = lable.list, missing_text = 'Missing')
# Table w/NAs/missing group removed
easy_merged_no <- ukbb_final_file_no %>%
  select(smok_detailed_NFC, smok_detailed_quit_years) %>%
  tbl_summary(label = lable.list, missing_text = "Missing")
# Table for auto
easy_merged_auto <- ukbb_final_file_auto %>%
  select(smok_detailed_NFC, smok_detailed_quit_years) %>%
  tbl_summary(label = lable.list, missing_text = 'Missing')
easy_merged_mLOX <- ukbb_final_file_mLOX %>%
  select(smok_detailed_NFC, smok_detailed_quit_years) %>%
  tbl_summary(label = lable.list, missing_text = 'Missing')
easy_merged_mLOY <- ukbb_final_file_mLOY %>%
  select(smok_detailed_NFC, smok_detailed_quit_years) %>%
  tbl_summary(label = lable.list, missing_text = 'Missing')
easy_merged_CHIP <- ukbb_final_file_CHIP %>%
  select(smok_detailed_NFC, smok_detailed_quit_years) %>%
  tbl_summary(label = lable.list, missing_text = 'Missing')

easy_merged <- tbl_merge(tbls = list(easy_merged_auto, easy_merged_mLOX, easy_merged_mLOY, easy_merged_CHIP, easy_merged_no), tab_spanner = c('**Autosomal mCA**', '**Loss of X (mLOX)**', '**Loss of Y (mLOY)**', '**CHIP**', '**No mCA or CHIP**'))

easy_merged %>%
  gtsummary::as_flex_table() %>%
  flextable::save_as_image(path="quit_years_tbl.png")
saveRDS(easy_merged,'quit_years_tbl.png.rds')
##################################################################################################################

# Part 2: Running analysis to make barplot for OR for auto, mLOX, mLOY & CHIP for quityears
# Setting reference to never smoker
lvl.1 <- c("Never Smoker", "≤1 quit-years","1 - 5 quit-years","5 - 10 quit-years","10 - 20 quit-years","≥ 20 quit-years","Other Former Smoker (no Quit-Years)","Current Smoker", "missing")
ukbb_final_file$smok_detailed_quit_years2 <- factor(ukbb_final_file$smok_detailed_quit_years2, levels = lvl.1)
ukbb_final_file$smok_detailed_quit_years2 <- factor(ukbb_final_file$smok_detailed_quit_years2, ordered = FALSE)
ukbb_final_file$smok_detailed_quit_years2 <-relevel(ukbb_final_file$smok_detailed_quit_years2, ref = 1)

library(biostat3) #for eform command
library(tidyverse) #tidybroom

# Building data frames for auto, mLOX, mLOY & CHIP with updated ukbb_final_file made earlier in code
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
auto_model_quit_years <- glm(auto ~ as.factor(smok_detailed_quit_years2) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)
mLOX_model_quit_years <- glm(mLOX ~ as.factor(smok_detailed_quit_years2) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)
mLOY_model_quit_years <- glm(mLOY ~ as.factor(smok_detailed_quit_years2) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)
CHIP_model_quit_years <- glm(CHIP ~ as.factor(smok_detailed_quit_years2) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

model_names <- c('auto_model_quit_years', 'mLOX_model_quit_years', 'mLOY_model_quit_years', 'CHIP_model_quit_years')

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
quit_year_models_results <- result_df

# Remove "as.factor(smok_detailed_quit_years2)" from the Description column
quit_year_models_results$Description <- sub("as\\.factor\\(smok_detailed_quit_years2\\)\\s*", "", quit_year_models_results$Description)
print(quit_year_models_results)
write.csv(quit_year_models_results, "quit_year_models_results.csv", row.names = FALSE)

# Generating a vertical bar graph that detail association values between smoking status (smok_Detailed_quit_years2)
library(ggplot2)

for (model in model_names) {
  # Filter the results for the specific model you want to visualize (e.g., auto_model_quit_years)
  model_name <- model  # Change this to the model you want to visualize
  # Filter the results dataframe to include only the categories you want to display
  categories_to_include <- c("≤1 quit-years", "1 - 5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years")
  model_results_subset <- quit_year_models_results[quit_year_models_results$Model == model_name, ] %>%
    filter(Description %in% categories_to_include)
  # Create a ggplot bar graph with error bars
  plot <- ggplot(model_results_subset, aes(x = as.numeric(OR), y = Description)) +
    geom_bar(stat = "identity", fill = "gray", color = "black", size = 0.7) +
    geom_errorbarh(aes(xmin = as.numeric(`CI..2.5..`), xmax = as.numeric(`CI..97.5..`)), height = 0.2, color = "black") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    labs(
      title = "Association Between Smoking Status and Quit-Years",
      x = "Odds Ratio (OR)",
      y = "Quit-Years",
      caption = "Error bars represent 95% confidence intervals"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    coord_flip() +  # Flip the plot to make it horizontal
    ylim(rev(categories_to_include))  # Set the y-axis limits in reverse order
  # Save the plot as a PNG file
  file_name <- paste0(model_name, "v1", ".png")
  ggsave(file_name, plot, width = 8, height = 6, units = "in")
}

saveRDS(quit_year_models_results,'quit_year_models_results.rds')
##################################################################################################################

# Part 3: Running analysis to make barplot for OR for auto, mLOX, mLOY & CHIP for quityears and current smokers
library(biostat3) #for eform command
library(tidyverse) #tidybroom

# Setting reference to never smoker
#lvl.1 <- c("Never Smoker", "≤1 quit-years", "1 - 5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years", "current cigarette smoker, <10/day", "current cigarette smoker, ≥40/day", "current cigarette smoker, 10 to <20/day", "current cigarette smoker, 20 to <40/day", "missing")
lvl.1 <- c("Never Smoker", "≤1 quit-years", "1 - 5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years", "current cigarette smoker, ≥40/day", "current cigarette smoker, 20 to <40/day", "current cigarette smoker, 10 to <20/day","current cigarette smoker, <10/day", "missing")

ukbb_final_file$smok_detailed_f2c_OR <- factor(ukbb_final_file$smok_detailed_f2c_OR, levels = lvl.1)
ukbb_final_file$smok_detailed_f2c_OR <- factor(ukbb_final_file$smok_detailed_f2c_OR, ordered = FALSE)
ukbb_final_file$smok_detailed_f2c_OR <-relevel(ukbb_final_file$smok_detailed_f2c_OR, ref = 1)

# Define custom levels for the smok_detailed_f2c_OR column
#custom_levels <- c("≤1 quit-years", "1 - 5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years", "current cigarette smoker, <10/day", "current cigarette smoker, ≥40/day", "current cigarette smoker, 10 to <20/day", "current cigarette smoker, 20 to <40/day")
# Convert the smok_detailed_f2c_OR column to a factor with custom levels
#ukbb_final_file$smok_detailed_f2c_OR <- factor(ukbb_final_file$smok_detailed_f2c_OR,  levels = custom_levels,  ordered = TRUE)
# Check the order of the levels
levels(ukbb_final_file$smok_detailed_f2c_OR)

# saveRDS(ukbb_final_file, file = 'ukbb_final_file+BMI+smoke_f2cVars.rds')

# Building data frames for auto, mLOX, mLOY & CHIP with updated ukbb_final_file made earlier in code
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

# Running logistic regression models (f2c-OR = former to current for ORs)
auto_model_f2c_OR <- glm(auto ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)
mLOX_model_f2c_OR <- glm(mLOX ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)
mLOY_model_f2c_OR <- glm(mLOY ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)
CHIP_model_f2c_OR <- glm(CHIP ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

model_names <- c('auto_model_f2c_OR', 'mLOX_model_f2c_OR', 'mLOY_model_f2c_OR', 'CHIP_model_f2c_OR')

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
c2f_OR_models_results <- result_df

# Remove "as.factor(c2f_OR_models_results)" from the Description column
c2f_OR_models_results$Description <- sub("as\\.factor\\(smok_detailed_f2c_OR\\)\\s*", "", c2f_OR_models_results$Description)
print(c2f_OR_models_results)
write.csv(c2f_OR_models_results, "c2f_OR_models_results.CSV", row.names = FALSE)

# Generating a vertical bar graph that details association values between smoking status (smok_detailed_f2c_OR)
library(ggplot2)

for (model in model_names) {
  # Filter the results for the specific model you want to visualize (e.g., auto_model_quit_years)
  model_name <- model  # Change this to the model you want to visualize
  # Filter the results dataframe to include only the categories you want to display
  categories_to_include <- c("current cigarette smoker, ≥40/day", "current cigarette smoker, 20 to <40/day", "current cigarette smoker, 10 to <20/day", "current cigarette smoker, <10/day", "≤1 quit-years", "1 - 5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years")
  model_results_subset <- c2f_OR_models_results[c2f_OR_models_results$Model == model_name, ] %>%
    filter(Description %in% categories_to_include) 
  # Create a vector of colors based on the position of each category
  color_vector <- ifelse(grepl("current", model_results_subset$Description), "#ADD8E6", "#E9967A")  # Hex codes for lightblue and lightpink
  # Create a ggplot bar graph with error bars
  plot <- ggplot(model_results_subset, aes(x = as.numeric(OR), y = Description, fill = color_vector)) +
    geom_bar(stat = "identity", color = "black", size = 0.7) +
    geom_errorbarh(aes(xmin = as.numeric(`CI..2.5..`), xmax = as.numeric(`CI..97.5..`)), height = 0.2, color = "black") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    labs(
      title = "Association Between Quit-Years/Smoking Intensity ",
      x = "Odds Ratio (OR)",
      y = "Quit-Years",
      caption = "Error bars represent 95% confidence intervals"
    ) +
    scale_fill_identity() +  # Use the specified colors
    scale_x_continuous(limits = c(0, 3.5)) +  # Set x-axis limits from 0 to 4
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    coord_flip() +  # Flip the plot to make it horizontal
    ylim(rev(categories_to_include))  # Set the y-axis limits in reverse order
  # Save the plot as a PNG file
  file_name <- paste0(model_name, "v2.3", ".pdf")
  ggsave(file_name, plot, width = 24, height = 6, units = "in")
}

# saveRDS(c2f_OR_models_results, 'c2f_OR_models_results.rds')
##################################################################################################################

# Part 4: Combining 1-5 quityears group & <1 quityears group, making barplot for OR for auto, mLOX, mLOY & CHIP for quityears and current smokers
library(biostat3) #for eform command
library(tidyverse) #tidybroom

# Combining ≤1 quit-years & 1 - 5 quit-years groups
ukbb_final_file$smok_detailed_f2c_OR[ukbb_final_file$smok_detailed_f2c_OR %in% c("≤1 quit-years", "1 - 5 quit-years")] <- "<5 quit-years"

# Setting reference to never smoker
custom_levels <- c("Never Smoker", "<5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years", "current cigarette smoker, <10/day", "current cigarette smoker, ≥40/day", "current cigarette smoker, 10 to <20/day", "current cigarette smoker, 20 to <40/day")

ukbb_final_file$smok_detailed_f2c_OR <- factor(ukbb_final_file$smok_detailed_f2c_OR, levels = custom_levels, ordered = FALSE)
#ukbb_final_file$smok_detailed_f2c_OR <-relevel(ukbb_final_file$smok_detailed_f2c_OR, ref = 1)

# Define custom levels for the smok_detailed_f2c_OR column
#custom_levels <- c("≤1 quit-years", "1 - 5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years", "current cigarette smoker, <10/day", "current cigarette smoker, ≥40/day", "current cigarette smoker, 10 to <20/day", "current cigarette smoker, 20 to <40/day")
# Convert the smok_detailed_f2c_OR column to a factor with custom levels
#ukbb_final_file$smok_detailed_f2c_OR <- factor(ukbb_final_file$smok_detailed_f2c_OR,  levels = custom_levels,  ordered = TRUE)
# Check the order of the levels
levels(ukbb_final_file$smok_detailed_f2c_OR)

saveRDS(ukbb_final_file, file = 'ukbb_final_file+BMI+smoke_f2cVars.rds')

# Building data frames for auto, mLOX, mLOY & CHIP with updated ukbb_final_file made earlier in code
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

# Running logistic regression models (f2c-OR = former to current for ORs)
auto_model_f2c_OR <- glm(auto ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)
mLOX_model_f2c_OR <- glm(mLOX ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)
mLOY_model_f2c_OR <- glm(mLOY ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)
CHIP_model_f2c_OR <- glm(CHIP ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

model_names <- c('auto_model_f2c_OR', 'mLOX_model_f2c_OR', 'mLOY_model_f2c_OR', 'CHIP_model_f2c_OR')

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
c2f_OR_models_results_combined <- result_df

# Remove "as.factor(c2f_OR_models_results_combined)" from the Description column
c2f_OR_models_results_combined$Description <- sub("as\\.factor\\(smok_detailed_f2c_OR\\)\\s*", "", c2f_OR_models_results_combined$Description)
print(c2f_OR_models_results_combined)
write.csv(c2f_OR_models_results_combined, "c2f_OR_models_results_combined.CSV", row.names = FALSE)

# Generating a vertical bar graph that details association values between smoking status (smok_detailed_f2c_OR)
library(ggplot2)

for (model in model_names) {
  # Filter the results for the specific model you want to visualize (e.g., auto_model_quit_years)
  model_name <- model  # Change this to the model you want to visualize
  # Filter the results dataframe to include only the categories you want to display
  categories_to_include <- c("current cigarette smoker, ≥40/day", "current cigarette smoker, 20 to <40/day", "current cigarette smoker, 10 to <20/day", "current cigarette smoker, <10/day", "<5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years", "Never Smoker")
  model_results_subset <- c2f_OR_models_results_combined[c2f_OR_models_results_combined$Model == model_name, ] %>%
    filter(Description %in% categories_to_include) 
  # Adding row for 'Never Smoker' with an odds ratio of 1
  reference_row <- data.frame(Model = model_name, Description = "Never Smoker", OR = 1, 'CI (2.5%)' = NA, 'CI (97.5%)' = NA, p_value = NA, stringsAsFactors = FALSE)
  model_results_subset <- rbind(model_results_subset, reference_row)
  # Create a vector of colors based on the position of each category
  color_vector <- ifelse(grepl("current", model_results_subset$Description), "#ADD8E6", ifelse(model_results_subset$Description == "Never Smoker", "#808080", "#8BC34A"))
  # Create a ggplot bar graph with error bars
  plot <- ggplot(model_results_subset, aes(x = as.numeric(OR), y = Description, fill = color_vector)) +
    geom_bar(stat = "identity", color = "black", size = 0.7, width = 0.9) +
    geom_errorbarh(aes(xmin = as.numeric(`CI..2.5..`), xmax = as.numeric(`CI..97.5..`)), height = 0.2, color = "black") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
    labs(
      title = "Association Between Quit-Years/Smoking Intensity ",
      x = "Odds Ratio (OR)",
      y = "Quit-Years",
      caption = "Error bars represent 95% confidence intervals"
    ) +
    scale_fill_identity() +  # Use the specified colors
    scale_x_continuous(limits = c(0, 3.5)) +  # Set x-axis limits from 0 to 4
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1)
    ) +
    coord_flip() +  # Flip the plot to make it horizontal
    ylim(rev(categories_to_include))  # Set the y-axis limits in reverse order
  # Save the plot as a PNG file
  file_name <- paste0(model_name, "v3", ".pdf")
  ggsave(file_name, plot, width = 4, height = 8, units = "in")
}

saveRDS(c2f_OR_models_results_combined, 'c2f_OR_models_results_combined.rds')
##################################################################################################################

# Part 5: Combining 1-5 quityears group & <1 quityears group & grouping <40 cigs/day into the 20 to 40 category, making barplot for OR for auto, mLOX, mLOY & CHIP for quityears and current smokers
library(biostat3) #for eform command
library(tidyverse) #tidybroom

# Combining Groups: ≤1 quit-years + 1 - 5 quit-years & <40 cigs/day + 20 to 40 
ukbb_final_file$smok_detailed_f2c_OR[ukbb_final_file$smok_detailed_f2c_OR %in% c("≤1 quit-years", "1 - 5 quit-years")] <- "<5 quit-years"
ukbb_final_file$smok_detailed_f2c_OR[ukbb_final_file$smok_detailed_f2c_OR %in% c("current cigarette smoker, ≥40/day", "current cigarette smoker, 20 to <40/day")] <- "current cigarette smoker, ≥20/day"

# Setting reference to never smoker
custom_levels <- c("Never Smoker", "<5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years", "current cigarette smoker, <10/day", "current cigarette smoker, ≥20/day", "current cigarette smoker, 10 to <20/day")
ukbb_final_file$smok_detailed_f2c_OR <- factor(ukbb_final_file$smok_detailed_f2c_OR, levels = custom_levels, ordered = FALSE)
#ukbb_final_file$smok_detailed_f2c_OR <-relevel(ukbb_final_file$smok_detailed_f2c_OR, ref = 1)

# Define custom levels for the smok_detailed_f2c_OR column
#custom_levels <- c("≤1 quit-years", "1 - 5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years", "current cigarette smoker, <10/day", "current cigarette smoker, ≥40/day", "current cigarette smoker, 10 to <20/day", "current cigarette smoker, 20 to <40/day")
# Convert the smok_detailed_f2c_OR column to a factor with custom levels
#ukbb_final_file$smok_detailed_f2c_OR <- factor(ukbb_final_file$smok_detailed_f2c_OR,  levels = custom_levels,  ordered = TRUE)
# Check the order of the levels
levels(ukbb_final_file$smok_detailed_f2c_OR)

#saveRDS(ukbb_final_file, file = 'ukbb_final_file+BMI+smoke_f2cVars.rds')

# Building data frames for auto, mLOX, mLOY & CHIP with updated ukbb_final_file made earlier in code
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

# Running logistic regression models (f2c-OR = former to current for ORs)
auto_model_f2c_OR <- glm(auto ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=auto_mCA_clean, family=binomial)
mLOX_model_f2c_OR <- glm(mLOX ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOX_mCA_clean, family=binomial)
mLOY_model_f2c_OR <- glm(mLOY ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + YRI + ASN + bmi, data=mLOY_mCA_clean, family=binomial)
CHIP_model_f2c_OR <- glm(CHIP ~ as.factor(smok_detailed_f2c_OR) + ageatassess + age_squared + as.factor(geneticsex) + YRI + ASN + bmi, data=CHIP_clean, family=binomial)

model_names <- c('auto_model_f2c_OR', 'mLOX_model_f2c_OR', 'mLOY_model_f2c_OR', 'CHIP_model_f2c_OR')

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
c2f_combinedgroup <- result_df

# Remove "as.factor(c2f_combinedgroup)" from the Description column
c2f_combinedgroup$Description <- sub("as\\.factor\\(smok_detailed_f2c_OR\\)\\s*", "", c2f_combinedgroup$Description)
print(c2f_combinedgroup)
write.csv(c2f_combinedgroup, "c2f_combinedgroup.csv", row.names = FALSE)

# Generating a vertical bar graph that details association values between smoking status (smok_detailed_f2c_OR)
library(ggplot2)

for (model in model_names) {
  # Filter the results for the specific model you want to visualize (e.g., auto_model_quit_years)
  model_name <- model  # Change this to the model you want to visualize
  # Filter the results dataframe to include only the categories you want to display
  categories_to_include <- c("current cigarette smoker, ≥20/day", "current cigarette smoker, 10 to <20/day", "current cigarette smoker, <10/day", "<5 quit-years", "5 - 10 quit-years", "10 - 20 quit-years", "≥ 20 quit-years", "Never Smoker")
  model_results_subset <- c2f_combinedgroup[c2f_combinedgroup$Model == model_name, ] %>%
    filter(Description %in% categories_to_include) 
  # Adding row for 'Never Smoker' with an odds ratio of 1
  reference_row <- data.frame(Model = model_name, Description = "Never Smoker", OR = 1, 'CI (2.5%)' = NA, 'CI (97.5%)' = NA, p_value = NA, stringsAsFactors = FALSE)
  model_results_subset <- rbind(model_results_subset, reference_row)
  # Create a vector of colors based on the position of each category
  color_vector <- ifelse(grepl("current", model_results_subset$Description), "#ADD8E6", ifelse(model_results_subset$Description == "Never Smoker", "#808080", "#8BC34A"))
  # Create a ggplot bar graph with error bars
  plot <- ggplot(model_results_subset, aes(x = as.numeric(OR), y = Description, fill = color_vector)) +
    geom_bar(stat = "identity", color = "black", size = 0.7, width = 0.9) +
    geom_errorbarh(aes(xmin = as.numeric(`CI..2.5..`), xmax = as.numeric(`CI..97.5..`)), height = 0.2, color = "black") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
    labs(
      title = "Association Between Quit-Years/Smoking Intensity ",
      x = "Odds Ratio (OR)",
      y = "Quit-Years",
      caption = "Error bars represent 95% confidence intervals"
    ) +
    scale_fill_identity() +  # Use the specified colors
    scale_x_continuous(limits = c(0, 3.5)) +  # Set x-axis limits from 0 to 4
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1)
    ) +
    coord_flip() +  # Flip the plot to make it horizontal
    ylim(rev(categories_to_include))  # Set the y-axis limits in reverse order
  # Save the plot as a PNG file
  file_name <- paste0(model_name, "v4", ".pdf")
  ggsave(file_name, plot, width = 4, height = 8, units = "in")
}

saveRDS(c2f_combinedgroup, 'c2f_combinedgroup.rds')
##################################################################################################################
# Figure editing done in inkscape

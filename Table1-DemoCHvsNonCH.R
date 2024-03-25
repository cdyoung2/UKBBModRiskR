##################################################################################################################
# Project 2: UKBB modifiable risk 
# table 1 construction
# CY edited for use on local computer
##################################################################################################################

rm(list=ls(all=TRUE))
dev.off()
#options(stringsAsFactors = FALSE)
library(dplyr)

setwd('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/')
wkdir <- setwd('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/')
ukbb_final_file <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/ukbb_final_file.rds")
dim(ukbb_final_file)

#Add in Graftpop ancestry data
UKBB_grafpop <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Machiela Lab/Projects/UK Biobank/UKBB_ancestry/Grafpop/UKBB_grafpop.rds")
UKBB_grafpop$f.eid <- UKBB_grafpop$feid_92005
dim(UKBB_grafpop)

# Merging Grafpop & current ukbb_final_file
merged_df <- merge(UKBB_grafpop, ukbb_final_file, by = 'f.eid', all = TRUE)
ukbb_final_file <- merged_df[match(ukbb_final_file$f.eid,merged_df$f.eid),] # added ~2k f.eids, so I subset w/loaded ukbb_final_file
##################################################################################################################

# Changes to the levels of variables in table 1
# Making prelimiary changes to ukbb_final_file (geneticsex, smoke_NFC & bmi)
ukbb_final_file$geneticsex <- factor(ukbb_final_file$geneticsex,
                                     levels = c(0, 1),
                                     labels = c("Female", "Male"))
ukbb_final_file$smoke_NFC <- factor(ukbb_final_file$smoke_NFC,
                                    levels = c(0, 1, 2, 9),
                                    labels = c("Never", "Former", "Current", "Missing"))
ukbb_final_file$bmi.cat <- cut(ukbb_final_file$bmi, breaks = c(-Inf, 18.5, 25, 30, 40, Inf), labels = c("<18.5", "18.5 - <25", "25 - <30", "30 - <40", "≥ 40"), include.lowest = TRUE)
ukbb_final_file$bmi.cat <- as.character(ukbb_final_file$bmi.cat)
ukbb_final_file$bmi.cat[is.na(ukbb_final_file$bmi.cat)] <- "Missing"
ukbb_final_file$bmi.cat <- factor(ukbb_final_file$bmi.cat, levels = c("<18.5", "18.5 - <25", "25 - <30", "30 - <40", "≥ 40", "Missing"))
ukbb_final_file$grafpop.cat <- ifelse(ukbb_final_file$Computed_population=='', 'Other', ukbb_final_file$Computed_population)
ukbb_final_file <- ukbb_final_file %>%
  mutate(grafpop.cat = recode(grafpop.cat,
                              "European" = "European",
                              "South Asian" = "South Asian",
                              "African American" = "African/Admix African",
                              "African" = "African/Admix African",
                              "Latin American 1" = "Hispanic",
                              "Other" = "Other",
                              "East Asian" = "East Asian/AAPI",
                              "Asian-Pacific Islander" = "East Asian/AAPI",
                              "Latin American 2" = "Hispanic"))
ukbb_final_file$grafpop.cat <- factor(ukbb_final_file$grafpop.cat, levels = c("European", "South Asian", "African/Admix African", "Hispanic", "East Asian/AAPI", "Other"))
##################################################################################################################

# Table 1 creation using gtsummary
#install.packages("gtsummary")
library(gtsummary)
library(tidyverse)

# Label List #
lable.list <- list(ageatassess ~'Age, Mean (SD)', geneticsex ~ 'Sex, N (%)', smoke_NFC ~ 'Smoking Status, N (%)', bmi.cat ~ 'BMI at baseline, N (%)', grafpop.cat ~ 'Genetic Ancestry, N (%)')

####### tbl_summary + tbl_merge #######
# will have to make separate inputs then merge together in gtsummary
ukbb_final_file_total <- ukbb_final_file
ukbb_final_file_auto <- ukbb_final_file[which(ukbb_final_file$auto==1),]
ukbb_final_file_mLOX <- ukbb_final_file[which(ukbb_final_file$mLOX==1),]
ukbb_final_file_mLOY <- ukbb_final_file[which(ukbb_final_file$mLOY==1),]
ukbb_final_file_chip <- ukbb_final_file[which(ukbb_final_file$CHIP==1),]
ukbb_final_file_no <- ukbb_final_file[which(ukbb_final_file$CH_value==0),]

easy_merged_auto <- ukbb_final_file_auto %>% 
  select(ageatassess, geneticsex, smoke_NFC, bmi.cat, grafpop.cat) %>%
  tbl_summary(
    statistic = 'ageatassess' ~ "{mean} ({sd})",
    label = lable.list,
    digits = c(ageatassess) ~ 1,
    #sort = all_categorical() ~ 'frequency',
    missing_text = "Missing") %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "1 Mean (SD); N (%)")

easy_merged_mLOX <- ukbb_final_file_mLOX %>% 
  select(ageatassess, geneticsex, smoke_NFC, bmi.cat, grafpop.cat) %>%
  tbl_summary(
    statistic = 'ageatassess' ~ "{mean} ({sd})",
    label = lable.list,
    digits = c(ageatassess) ~ 1,
    missing_text = "Missing") %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "1 Mean (SD); N (%)")

easy_merged_mLOY <- ukbb_final_file_mLOY %>% 
  select(ageatassess, geneticsex, smoke_NFC, bmi.cat, grafpop.cat) %>%
  tbl_summary(
    statistic = 'ageatassess' ~ "{mean} ({sd})",
    label = lable.list,
    digits = c(ageatassess) ~ 1,
    missing_text = "Missing") %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "1 Mean (SD); N (%)")

easy_merged_chip <- ukbb_final_file_chip %>% 
  select(ageatassess, geneticsex, smoke_NFC, bmi.cat, grafpop.cat) %>%
  tbl_summary(
    statistic = 'ageatassess' ~ "{mean} ({sd})",
    label = lable.list,
    digits = c(ageatassess) ~ 1,
    missing_text = "Missing") %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "1 Mean (SD); N (%)")

easy_merged_no <- ukbb_final_file_no %>% 
  select(ageatassess, geneticsex, smoke_NFC, bmi.cat, grafpop.cat) %>%
  tbl_summary(
    statistic = 'ageatassess' ~ "{mean} ({sd})",
    label = lable.list,
    digits = c(ageatassess) ~ 1,
    missing_text = "Missing") %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "1 Mean (SD); N (%)")

easy_merged <- tbl_merge(tbls = list(easy_merged_auto, easy_merged_mLOX, easy_merged_mLOY, easy_merged_chip, easy_merged_no), tab_spanner = c('**Autosomal mCA**', '**Loss of X (mLOX)**', '**Loss of Y (mLOY)**','**CHIP**', '**No mCA or CHIP**'))
easy_merged
##################################################################################################################

# Saving table to csv/excel file
library(writexl)
tibble <- gtsummary::as_tibble(easy_merged) # Convert the table to a tibble
writexl::write_xlsx(tibble, "/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/Table1.xlsx") # Export the tibble to an Excel file
readr::write_csv(tibble, "/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/Table1.csv") # Export the tibble to a CSV file

# Saving table to word/pdf document
#install.packages('flextable')
library(flextable)
# saving the new table to a word document
easy_merged %>%
  gtsummary::as_flex_table() %>%
  flextable::save_as_docx(path="/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/Table1.docx")
easy_merged %>%
  gtsummary::as_flex_table() %>%
  flextable::save_as_image(path="/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/Table1.png")
easy_merged_final <- as_gt(easy_merged)

# Saving as RDS
saveRDS(easy_merged, file = "/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/Table1.rds")



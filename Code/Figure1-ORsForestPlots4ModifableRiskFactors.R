##################################################################################################################
# UKBB Modifiable Risk Factors
# Forest Plot generation (final figure)
# Updates to include BMI, smoking (total 6 panels) & color eac CH type separately
# ### Code needs to be updated: including extra plot & starting x-axis at 0.25 or 0.5 instead of 0
##################################################################################################################
rm(list=ls(all=TRUE))
dev.off()
#options(stringsAsFactors = FALSE)
setwd('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/')

library(ggplot2)
library(cowplot)

# Read in data. Logistic regression table output from "models_updated_Erikka+Pedro+SES+CHIP.R"
logit <- read.csv('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/model_ORs+CI+pvalue.csv', header = TRUE)
head(logit)
lowest_p4 <- 0.001666667 #update to pull from updated p4_effective_test obejct
  #p4_effective_test <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/p4_effective_test.rds')
logit$meff <- ifelse(logit$p_value<lowest_p4, 'Significant', 'NS')

pdf(file="Summary_Measures_Plotsv5.pdf",width=16,height=12)

##################################################################
# Indices Forest Plot 
##################################################################
index_logit <- logit[c(52:54, 113:115, 174:176, 235:237),]
head(index_logit)
row_colors_index <- c(rep("Black", 3), rep("darkred", 3), rep("darkgreen", 3), rep("Blue", 3)) # custom colors for each CH subtypes based on rows

# Add in column to distinguish position of data in ggplot
index_logit$overlap <- seq(4.8, 0.4, by = -0.4)

# Naming the labels for plot
y_labels <- c('Autosomal Scotland Index', "Autosomal England Index", 'Autosomal Wales Index', 'mLOX Scotland Index', "mLOX England Index", 'mLOX Wales Index', 'mLOY Scotland Index', "mLOY England Index", 'mLOY Wales Index', 'CHIP Scotland Index', "CHIP England Index", 'CHIP Wales Index')

# Construction of the plot using ggplot
index <- ggplot(data = index_logit, aes(x = OR, y = overlap, color = row_colors_index)) + 
  geom_point(aes(size = p_value < lowest_p4), show.legend = FALSE) +  # Remove legend for this geom
  geom_errorbar(aes(xmin = CI..2.5.., xmax = CI..97.5..), width = 0.05, size = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  #labs(title = 'Risk Associations between UK Social Deprivation Indices and CH') +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none"  # Remove the legend
  ) +
  theme_classic(base_size = 13.5) +
  ylab("") +
  xlab(expression("Odds Ratio")) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)) +
  scale_y_continuous(limits = c(-0.1, 5), breaks = seq(4.8,0.4, by = -0.4), labels = y_labels) +
  scale_size_manual(
    values = c(2, 4),
    breaks = c(FALSE, TRUE),
    labels = c('Non-Significant', 'Significant')
  ) +
  scale_color_manual(
    values = c("black", "darkred", "darkgreen", "blue"),
    guide = FALSE  # Remove the color legend
  )
index

##################################################################
# Physical Activity Forest Plot
##################################################################
ipaq_pa_logit <- logit[c(31,32,92,93,153,154,214,215),]
row_colors_pa <- c(rep("Black", 2), rep("darkred", 2), rep("darkgreen", 2), rep("Blue", 2)) # custom colors for each CH subtypes based on rows
ipaq_pa_logit$overlap <- seq(3.2, 0.4, by = -0.4)
y_labels <- c('Autosomal IPAQ Moderate PA', 'Autosomal IPAQ High PA', 'mLOX IPAQ Moderate PA', 'mLOX IPAQ High PA', 'mLOY IPAQ Moderate PA', 'mLOY IPAQ High PA', 'CHIP IPAQ Moderate PA', 'CHIP IPAQ High PA')
# Construction of the plot using ggplot (edits suggested by Mitch include zooming in, black vs grey dots based on significance, adjusted p-value)
pa <- ggplot(data=ipaq_pa_logit, aes(x = OR, y = overlap, color = row_colors_pa)) + 
  geom_point(aes(size = p_value < lowest_p4), show.legend = FALSE) +
  geom_errorbar(aes(xmin = CI..2.5.., xmax = CI..97.5..), width = 0.05, size  = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed") + 
  #labs(title = 'Risk Association between IPAQ Categories and CH') +
  theme(
    panel.border = element_blank(),# Hide panel borders and remove grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"), # Change axis line
    legend.position = "none"  # Remove the legend
  ) + 
  theme_classic( base_size = 13.5) +
  ylab("") +
  xlab(expression("Odds Ratio")) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)) +
  scale_y_continuous(limits = c(-0.1, 3.4), breaks = seq(3.2,0.4, by = -0.4) ,labels = y_labels) +
  scale_size_manual(
    values = c(2, 4), # Size is doubled for less than lowest p4
    breaks = c(FALSE, TRUE), # Values of p_value
    labels = c('Non-Significant', 'Significant')
  ) +
  scale_color_manual(
    values = c("black", "darkred", "darkgreen", "blue"),
    guide = FALSE  # Remove the color legend
  )
pa

##################################################################
# Alcohol Forest Plot 
##################################################################
alcohol_logit <- logit[c(48:50, 109:111, 170:172, 231:233),]
head(alcohol_logit)
row_colors_alc <- c(rep("Black", 3), rep("darkred", 3), rep("darkgreen", 3), rep("Blue", 3)) # custom colors for each CH subtypes based on rows

# Add in column to distinguish position of data in ggplot
alcohol_logit$overlap <- seq(4.8, 0.4, by = -0.4)

# Naming the labels for plot 
y_labels <- c('Autosomal 1-2 Drinks/Day', 'Autosomal >2-3 Drinks/Day', 'Autosomal >3 Drinks/Day', 'mLOX 1-2 Drinks/Day', 'mLOX >2-3 Drinks/Day', 'mLOX >3 Drinks/Day', 'mLOY 1-2 Drinks/Day', 'mLOY >2-3 Drinks/Day', 'mLOY >3 Drinks/Day', 'CHIP 1-2 Drinks/Day', 'CHIP >2-3 Drinks/Day', 'CHIP >3 Drinks/Day')

# Construction of the plot using ggplot
alcohol <- ggplot(data=alcohol_logit, aes(x = OR, y = overlap, color = row_colors_alc)) + 
  geom_point(aes(size = p_value < lowest_p4), show.legend = FALSE) +
  geom_errorbar(aes(xmin = CI..2.5.., xmax = CI..97.5..), width = 0.05, size  = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  #labs(title = 'Risk Association between Alcoholic Drinks/Day and CH') +
  theme(
    panel.border = element_blank(), # Hide panel borders and remove grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"), # Change axis line
    legend.position = "none"  # Remove the legend
  ) + 
  theme_classic( base_size = 13.5) +
  ylab("") +
  xlab(expression("Odds Ratio")) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)) +
  scale_y_continuous(limits = c(-0.01, 5), breaks = seq(4.8, 0.4, by = -0.4),labels = y_labels) +
  scale_size_manual(
    values = c(2, 4), # Size is doubled for less than lowest p4
    breaks = c(FALSE, TRUE), # Values of p_value
    labels = c('Non-Significant', 'Significant')
  ) +
  scale_color_manual(
    values = c("black", "darkred", "darkgreen", "blue"),
    guide = FALSE  # Remove the color legend
  )
alcohol

##################################################################
# Sleep + Shift-work Forest Plot
##################################################################
sleep_logit <- logit[c(1, 55:57, 62, 116:118, 123, 177:179, 184, 238:240),]
head(sleep_logit)
row_colors_sleep <- c(rep("black", 4), rep("darkred", 4), rep("darkgreen", 4), rep("blue", 4)) # custom colors for each CH subtypes based on rows

# Add in column to distinguish position of data in ggplot
sleep_logit$overlap <- seq(6.4, 0.4, by = -0.4)

# Naming the labels for plot 
y_labels <- c('Autosomal Net Sleep Duration/Night', 'Autosomal Night Shift Work (Sometimes)', 'Autosomal Night Shift Work (Usually)', 'Autosomal Night Shift Work (Always)', 'mLOX Net Sleep Duration/Night', 'mLOX Night Shift Work (Sometimes)', 'mLOX Night Shift Work (Usually)', 'mLOX Night Shift Work (Always)', 'mLOY Net Sleep Duration/Night', 'mLOY Night Shift Work (Sometimes)', 'mLOY Night Shift Work (Usually)', 'mLOY Night Shift Work (Always)', 'CHIP Net Sleep Duration/Night', 'CHIP Night Shift Work (Sometimes)', 'CHIP Night Shift Work (Usually)','CHIP Night Shift Work (Always)')
# could use gsub to edit the characters in col
# could just write out updated col names to what you want when you finalize figures after showing Mitch

# Construction of the plot using ggplot
sleep <- ggplot(data=sleep_logit, aes(x = OR, y = overlap, color = row_colors_sleep)) + 
  geom_point(aes(size = p_value < lowest_p4), show.legend = FALSE) +
  geom_errorbar(aes(xmin = CI..2.5.., xmax = CI..97.5..), width = 0.05, size  = 0.5) + 
  geom_vline(xintercept = 1, linetype = "dashed") +
  #labs(title = 'Risk Association between Net Sleep Duration and CH') +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none"  # Remove the legend
  ) +
  theme_classic(base_size = 13.5) +
  ylab("") +
  xlab(expression("Odds Ratio")) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)) +
  scale_y_continuous(limits = c(-0.01, 6.6), breaks = seq(6.4, 0.4, by = -0.4),  labels = y_labels) +
  scale_size_manual(
    values = c(2, 4), # Size is doubled for less than lowest p4
    breaks = c(FALSE, TRUE), # Values of p_value
    labels = c('Non-Significant', 'Significant')
  ) +
  scale_color_manual(
    values = c("black", "darkred", "darkgreen", "blue"),
    guide = FALSE  # Remove the color legend
  )
sleep

##################################################################
# Setup for additional forest plots for BMI & Smoking #
bmi_logit <- read.csv('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/bmi.cat_models_resultsv2.csv', header = TRUE)
smok_logit <- read.csv('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Code_Output/smok_detailed_NFC_models_results.csv', header = TRUE)

##################################################################
# BMI Forest Plot
##################################################################
head(bmi_logit)
row_colors_bmi <- c(rep("black", 4), rep("darkred", 4), rep("darkgreen", 4), rep("blue", 4)) # custom colors for each CH subtypes based on rows

# Add in column to distinguish position of data in ggplot
bmi_logit$overlap <- seq(6.4, 0.4, by = -0.4)

# Naming the labels for plot 
y_labels <- c('Autosomal w/BMI <18.5', 'Autosomal w/BMI 25 - <30', 'Autosomal w/BMI 30 - <40', 'Autosomal w/BMI >= 40', 'mLOX w/BMI <18.5', 'mLOX w/BMI 25 - <30', 'mLOX w/BMI 30 - <40', 'mLOX w/BMI >= 40', 'mLOY w/BMI <18.5', 'mLOY w/BMI 25 - <30', 'mLOY w/BMI 30 - <40', 'mLOY w/BMI >= 40', 'CHIP w/BMI <18.5', 'CHIP w/BMI 25 - <30', 'CHIP w/BMI 30 - <40', 'CHIP w/BMI >= 40')
# could use gsub to edit the characters in col
# could just write out updated col names to what you want when you finalize figures after showing Mitch

# Construction of the plot using ggplot
bmi <- ggplot(data=bmi_logit, aes(x = OR, y = overlap, color = row_colors_bmi)) + 
  geom_point(aes(size = p_value < lowest_p4), show.legend = FALSE) +
  geom_errorbar(aes(xmin = CI..2.5.., xmax = CI..97.5..), width = 0.05, size  = 0.5) + 
  geom_vline(xintercept = 1, linetype = "dashed") +
  #labs(title = 'Risk Association between BMI and CH') +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none"  # Remove the legend
  ) +
  theme_classic(base_size = 13.5) +
  ylab("") +
  xlab(expression("Odds Ratio")) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)) +
  scale_y_continuous(limits = c(-0.01, 6.6), breaks = seq(6.4, 0.4, by = -0.4),  labels = y_labels) +
  scale_size_manual(
    values = c(2, 4), # Size is doubled for less than lowest p4
    breaks = c(FALSE, TRUE), # Values of p_value
    labels = c('Non-Significant', 'Significant')
  ) +
  scale_color_manual(
    values = c("black", "darkred", "darkgreen", "blue"),
    guide = FALSE  # Remove the color legend
  )
bmi

##################################################################
# Smoking Forest Plot
##################################################################
head(smok_logit)
row_colors_smok <- c(rep("Black", 2), rep("darkred", 2), rep("darkgreen", 2), rep("Blue", 2)) # custom colors for each CH subtypes based on rows

# Add in column to distinguish position of data in ggplot
smok_logit$overlap <- seq(3.2, 0.4, by = -0.4)

# Naming the labels for plot 
y_labels <- c('Autosomal Current Smoker', 'Autosomal Former Smoker', 'mLOX Current Smoker', 'mLOX Former Smoker', 'mLOY Current Smoker', 'mLOY Former Smoker', 'CHIP Current Smoker', 'CHIP Former Smoker')

# Construction of the plot using ggplot
smok <- ggplot(data=smok_logit, aes(x = OR, y = overlap, color = row_colors_smok)) + 
  geom_point(aes(size = p_value < lowest_p4), show.legend = FALSE) +
  geom_errorbar(aes(xmin = CI..2.5.., xmax = CI..97.5..), width = 0.05, size  = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  #labs(title = 'Risk Association between Smoking Status and CH') +
  theme(
    panel.border = element_blank(), # Hide panel borders and remove grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"), # Change axis line
    legend.position = "none"  # Remove the legend
  ) + 
  theme_classic( base_size = 13.5) +
  ylab("") +
  xlab(expression("Odds Ratio")) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)) +
  scale_y_continuous(limits = c(-0.01, 3.4), breaks = seq(3.2, 0.4, by = -0.4),labels = y_labels) +
  scale_size_manual(
    values = c(2, 4), # Size is doubled for less than lowest p4
    breaks = c(FALSE, TRUE), # Values of p_value
    labels = c('Non-Significant', 'Significant')
  ) +
  scale_color_manual(
    values = c("black", "darkred", "darkgreen", "blue"),
    guide = FALSE  # Remove the color legend
  )
smok

library(cowplot)

# Create a grid of plots
plot_grid(index, smok, alcohol, bmi, pa, sleep,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3,
          align = "hv",
          axis = "tblr")

##################################################################
# Code to develop legend - Smok Forest plot
##################################################################
# Construction of the plot using ggplot
smok <- ggplot(data=smok_logit, aes(x = OR, y = overlap, color = row_colors_smok)) + 
  geom_point(aes(size = p_value < lowest_p4), show.legend = FALSE) +
  geom_errorbar(aes(xmin = CI..2.5.., xmax = CI..97.5..), width = 0.05, size  = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  #labs(title = 'Risk Association between Smoking Status and CH') +
  theme(
    panel.border = element_blank(), # Hide panel borders and remove grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"), # Change axis line
    legend.position = "none"  # Remove the legend
  ) + 
  theme_classic( base_size = 13.5) +
  ylab("") +
  xlab(expression("Odds Ratio")) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)) +
  scale_y_continuous(limits = c(-0.01, 3.4), breaks = seq(3.2, 0.4, by = -0.4),labels = y_labels) +
  scale_size_manual(
    values = c(2, 4), # Size is doubled for less than lowest p4
    breaks = c(FALSE, TRUE), # Values of p_value
    labels = c('Non-Significant', 'Significant')
  ) +
  scale_color_manual(
    values = c("black", "darkred", "darkgreen", "blue"),
    guide = FALSE  # Remove the color legend
  )
smok
dev.off()

##################################################################
# Legend Creation #
##################################################################

# I am using smok code again to develop legend. I will take legned only & then combine in inkscape
# Construction of the plot using ggplot
smok <- ggplot(data=smok_logit, aes(x = OR, y = overlap, color = row_colors_smok)) + 
  geom_point(aes(size = p_value < lowest_p4), show.legend = TRUE) +
  geom_errorbar(aes(xmin = CI..2.5.., xmax = CI..97.5..), width = 0.05, size  = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  #labs(title = 'Risk Association between Smoking Status and CH') +
  theme(
    panel.border = element_blank(), # Hide panel borders and remove grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"), # Change axis line
    #legend.position = "none"  # Remove the legend
  ) + 
  theme_classic( base_size = 13.5) +
  ylab("") +
  xlab(expression("Odds Ratio")) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)) +
  scale_y_continuous(limits = c(-0.01, 3.4), breaks = seq(3.2, 0.4, by = -0.4),labels = y_labels) +
  scale_size_manual(
    values = c(2, 4), # Size is doubled for less than lowest p4
    breaks = c(FALSE, TRUE), # Values of p_value
    labels = c('Non-Significant', 'Significant')
  ) +
  scale_color_manual(
    values = c("black", "darkred", "darkgreen", "blue"),
    labels = c("Black", "Dark Red", "Dark Green", "Blue")
  ) +
  guides(
    color = guide_legend(title = "Color"),
    size = guide_legend(title = "Size")
  )

# Code to pull out only legend & save
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(smok)
ggsave("legend.png", legend, width = 2, height = 3)
ggsave("legend.pdf", legend, width = 2, height = 3)

# Not using currently
# Adding ggsave to PDF file
# existing_pdf <- "Summary_Measures_Plotsv3.pdf"
# Use pdftk to concatenate the existing PDF and the legend PDF
# system(sprintf("pdftk %s %s cat output combined.pdf", existing_pdf, legend))

# R script using bd_read (demo data for UKBB) to construct 25-level Smoking Variable 
# created 2023-04-19, adapted from: smoking_ull_25_level_variable_creation.R example code
# Project 2: UKBB modifiable risk 
# Smaller demo file (bd_read) created using ukbcon, untitled.sh, feild.list_Corey etc. in UKBB_table1.R & only contains feildIDs of interest
# Files are located: /Box/Coreys_Folder/UKBB_risk_Project2/ukbconv/ukbconv_code_2ndRun 
# CY edited for use on local computer

##############################
# Smoking: 25-level variable #
##############################


## Variables of interest: 

### 1239= Current tobacco smoking
### 1249= Past tobacco smoking
### 2644= Light smokers, at least 100 smokes in lifetime
### 2867= Age started smoking in former smokers
### 2877= Type of tobacco previously smoked
### 2887= Number of cigarettes previously smoked daily
### 2897= Age stopped smoking
### 3436= Age started smoking in current smokers
### 3446= Type of tobacco currently smoked
### 3456= Number of cigarettes currently smoked daily (current cigarette smokers)
### 3466= Time from waking to first cigarette
### 5959= Previously smoked cigarettes on most/all days
### 6183= Number of cigarettes previously smoked daily (current cigar/pipe smokers)
### 6194= Age stopped smoking cigarettes (current cigar/pipe or previous cigarette smoker)
### 20116= Smoking status
### 20160= Ever smoked 
### 21003=

### Note from OG document 'smoking_ull_25_level_variable_creation.R'
#Should probably pull variables needed above from new application rather than reading in old basket.  Then use script below to help making 25 level variable

# Read in demo data for all ukbb genotyped particapants
bd_read <- read.table("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/ukbconv/ukbconv_code_2ndRun/ukb.app.92005_young_2nd.tab", header=TRUE, sep="\t")


# New files available on box/desktop/CCAD (Box: updated input files)
mloy.with.eids.for.appl.92005 <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela\ Lab/Projects/UK\ Biobank/Application_92005/data/mca_files/mloy_eids.appl.92005.rds')
mlox.with.eids.for.appl.92005 <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela\ Lab/Projects/UK\ Biobank/Application_92005/data/mca_files/mlox_eids.appl.92005.rds')
auto.with.eids.for.appl.92005 <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela\ Lab/Projects/UK\ Biobank/Application_92005/data/mca_files/auto_eids.appl.92005.rds')
ukb.run.ukb.stats_eids.appl.92005 <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela\ Lab/Projects/UK\ Biobank/Application_92005/data/mca_files/ukb.run.ukb.stats_eids.appl.92005.rds')


#############  Adding if ind. has mLOX, mLOY or autosomal mCAs #############
#mlox.with.eids.for.appl.92005$f.eid <- mlox.with.eids.for.appl.92005$eid.current #setting id columns to the same colname
#merged_df <- merge(bd_read, mlox.with.eids.for.appl.92005, by = "f.eid", all.x = TRUE) #did not work
bd_read$mLOX <- ifelse(bd_read$f.eid %in% mlox.with.eids.for.appl.92005$eid_appl.92005, 1, 0) #/%in% (review how that works again)
#mloy.with.eids.for.appl.92005$f.eid <- mloy.with.eids.for.appl.92005$eid.current # setting id
bd_read$mLOY <- ifelse(bd_read$f.eid %in% mloy.with.eids.for.appl.92005$eid_appl.92005, 1, 0)
#auto.with.eids.for.appl.92005$f.eid <- auto.with.eids.for.appl.92005$eid.current # setting id
bd_read$auto <- ifelse(bd_read$f.eid %in% auto.with.eids.for.appl.92005$eid_appl.92005, 1, 0)
bd_read$total <- ifelse(bd_read$f.eid %in% ukb.run.ukb.stats_eids.appl.92005$eid_appl.92005, 1, 0) #everyone that was genotyped that is in .tab file
bd_read$mCA_value <- bd_read$mLOX+bd_read$mLOY+bd_read$auto # col adding the instances of mCAs


#############  Separate bd_reads (all genotyped, each mCA type) #############
# Use bd_read_total for demo of all inds, not edited for table 1 construction, no additional levels or bins
bd_read_total <- bd_read[which(bd_read$total==1),]
bd_read_auto <- bd_read[which(bd_read$auto==1),]
bd_read_mLOX <- bd_read[which(bd_read$mLOX==1),]
bd_read_mLOY <- bd_read[which(bd_read$mLOY==1),]
bd_read_no <- bd_read[which(bd_read$mCA_value==0),]

#############  Selecting all genotpyed inds & vars needed for smoking variable #############  
smoking <- bd_read_total[, c('f.eid', 'f.1239.0.0', 'f.1249.0.0', 'f.2644.0.0', 'f.2867.0.0', 'f.2877.0.0', 'f.2887.0.0', 'f.2897.0.0', 'f.3436.0.0', 'f.3446.0.0', 'f.3456.0.0', 'f.3466.0.0', 'f.5959.0.0', 'f.6183.0.0', 'f.6194.0.0', 'f.20116.0.0', 'f.20160.0.0', 'f.21003.0.0')]
ukb<- smoking  
dim(ukb) 


# Generate 25-level smoking variable (based on Shu-Hong's detailed_smoking script)
ukb$smoke_detailed = rep(NA,nrow(ukb))
ukb$smok_detailed = rep(NA,nrow(ukb))

# Define quit years
ukb$smoke_quit_years = rep(NA,nrow(ukb))
ukb$smoke_quit_years[ukb$f.1249.0.0==1 & !is.na(ukb$f.2897.0.0)] = ukb$f.21003.0.0[ukb$f.1249.0.0==1 & !is.na(ukb$f.2897.0.0)] -ukb$f.2897.0.0[ukb$f.1249.0.0==1 & !is.na(ukb$f.2897.0.0)]
ukb$smoke_quit_years[ukb$f.2897.0.0 %in% c(-3,-1)] = NA # f.2897.0.0 Age stopped smoking: -1 do not know. n_21003_0_9: Age when attended assessment centre; f.1249.0.0: Past tobacco smoking 

# Define detailed smoking status
ukb$smoke_detailed[ukb$f.20116.0.0 == 0] = 0 # never smoker
#smoke_detailed[f.20116.0.0 == -3] = NA
#smoke_detailed[is.na(f.20116.0.0)] = NA
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 %in% c(2,3) & ukb$f.2644.0.0 == 0] = 1 # current occasional smoker, smoked <100 cigarettes in lifetime
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 == 2 & ukb$f.2644.0.0 == 0] = 1 # current occasional smoker, smoked <100 cigarettes in lifetime
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 %in% c(2,3) & ukb$f.2644.0.0 == 1] = 2 # current occasional smoker, smoked >=100 cigarettes in lifetime
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 == 2 & ukb$f.2644.0.0 == 1] = 2 # current occasional smoker, smoked occasionally in past, lifetime smoking unknown

ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 == -3] = 9901 # current occasional smoker, unknown frequency in past
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 == 4] = 9902 # current occasional smoker, unknown frequency in past
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 %in% c(2,3) & ukb$f.2644.0.0 %in% c(NA,-1,-3)] = 9903 # current occasional smoker, smoked occasionally in past, lifetime smoking unknown

ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(3)] = 3 # current occasional smoker, smoked cigars or pipes daily in past
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 < 20 & ukb$f.2887.0.0 >=1] = 4 # current occasional smoker, smoked cigarettes daily in past, <20/day
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 %in% c(-10)] = 4 # current occasional smoker, smoked cigarettes daily in past, <20/day
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 20] = 5 # current occasional smoker, smoked cigarettes daily in past, >=20/day

ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(-3, -7, NA)] = 9904 # current occasional smoker, smoked daily in past, unknown type of tabacco in past
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 %in% c(-1, NA)] = 9905 # current occasional smoker, smoked cigarettes daily in past, unknown intensity of smoking in past

ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(3) & ukb$f.5959.0.0 %in% c(1)] = 6 # current cigar pipe smoker, former cigarette smoker
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(3) & ukb$f.5959.0.0 %in% c(0)] = 7 # current cigar pipe smoker, not former cigarette smoker
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(-3,-7, NA)] = 9906 # current daily smoker, unknown type of tobacco
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(3) & ukb$f.5959.0.0 %in% c(NA,-3)] = 9907 # current cigar pipe smoker, history of cigarette smoking unknown

ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(1,2) & ukb$f.3456.0.0 >= 1 & ukb$f.3456.0.0 < 10] = 8 # current cigarette smoker, <10/day
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(1,2) & ukb$f.3456.0.0 %in% c(-10)] = 8 # current cigarette smoker, <10/day
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(1,2) & ukb$f.3456.0.0 >= 10 & ukb$f.3456.0.0 < 20] = 9 # current cigarette smoker, 10 to <20/day
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(1,2) & ukb$f.3456.0.0 >= 20 & ukb$f.3456.0.0 < 40] = 10 # current cigarette smoker, 20 to <40/day
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(1,2) & ukb$f.3456.0.0 >= 40] = 11 # current cigarette smoker, >=40/day
ukb$smoke_detailed[ukb$f.20116.0.0 == 2 & ukb$f.1239.0.0 %in% c(1) & ukb$f.3446.0.0 %in% c(1,2) & ukb$f.3456.0.0 %in% c(-1,-3,NA)] = 9908 # current cigarette smoker, intensity unknown

ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(2,3) & ukb$f.2644.0.0 %in% c(0)] = 12 # former occasional ciagarette smoker, smoked <100 cigarettes in lifetime
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(2,3) & ukb$f.2644.0.0 %in% c(1)] = 13 # former occasional ciagarette smoker, smoked >=100 cigarettes in lifetime
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(2,3) & ukb$f.2644.0.0 %in% c(-1,-3,NA)] = 14 # former occasional ciagarette smoker, lifetime smoking unknown

ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(3)] = 15 # former daily cigar pipe smoker
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(-3,-7,NA)] = 9909 # former daily tobacco smoker, type unknown

ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 1 & ukb$f.2887.0.0 < 20 & 0 <= ukb$smoke_quit_years & ukb$smoke_quit_years < 1] = 16 # former cigarette smoker, <20/day, quit <1 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 == -10 & 0 <= ukb$smoke_quit_years & ukb$smoke_quit_years < 1] = 16 # former cigarette smoker, <20/day (LT 1/day), quit <1 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 20 & 0 <= ukb$smoke_quit_years & ukb$smoke_quit_years < 1] = 17 # former cigarette smoker, >=20/day, quit <1 year ago

ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 1 & ukb$f.2887.0.0 < 20 & ukb$smoke_quit_years >= 1 & ukb$smoke_quit_years < 5] = 18 # former cigarette smoker, <20/day, quit 1-5 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 == -10 & ukb$smoke_quit_years >= 1 & ukb$smoke_quit_years < 5] = 18 # former cigarette smoker, <20/day (LT 1/day), quit 1-5 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 20 & ukb$smoke_quit_years >= 1 & ukb$smoke_quit_years < 5] = 19 # former cigarette smoker, >=20/day, quit 1-5 year ago

ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 1 & ukb$f.2887.0.0 < 20 & ukb$smoke_quit_years >= 5 & ukb$smoke_quit_years < 10] = 20 # former cigarette smoker, <20/day, quit 5-10 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 == -10 & ukb$smoke_quit_years >= 5 & ukb$smoke_quit_years < 10] = 20 # former cigarette smoker, <20/day, quit 5-10 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 20 & ukb$smoke_quit_years >= 5 & ukb$smoke_quit_years < 10] = 21 # former cigarette smoker, >=20/day, quit 5-10 year ago

ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 1 & ukb$f.2887.0.0 < 20 & ukb$smoke_quit_years >= 10 & ukb$smoke_quit_years < 20] = 22 # former cigarette smoker, <20/day, quit 10-20 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 == -10 & ukb$smoke_quit_years >= 10 & ukb$smoke_quit_years < 20] = 22 # former cigarette smoker, <20/day, quit 10-20 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 20 & ukb$smoke_quit_years >= 10 & ukb$smoke_quit_years < 20] = 23 # former cigarette smoker, >=20/day, quit 10-20 year ago

ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 1 & ukb$f.2887.0.0 < 20 & ukb$smoke_quit_years >= 20] = 24 # former cigarette smoker, <20/day, quit 10-20 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 == -10 & ukb$smoke_quit_years >= 20] = 24 # former cigarette smoker, <20/day, quit 10-20 year ago
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$f.2887.0.0 >= 20 & ukb$smoke_quit_years >= 20] = 25 # former cigarette smoker, >=20/day, quit 10-20 year ago

ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2) & ukb$smoke_quit_years %in% c(NA)] = 9911 # former daily cigarette smoker, time since quit unknown
ukb$smoke_detailed[ukb$f.20116.0.0 == 1 & ukb$f.1249.0.0 %in% c(1) & ukb$f.2877.0.0 %in% c(1,2)& ukb$f.2887.0.0 %in% c(-1,NA)] = 9910 # former daily cigarette smoker, intensity unknown

ukb$smok_detailed[ukb$smoke_detailed==0] <- 'never smoker'
ukb$smok_detailed[ukb$smoke_detailed==1] <- 'current occasional smoker, smoked <100 cigarettes in lifetime'
ukb$smok_detailed[ukb$smoke_detailed==2] <- 'current occasional smoker, smoked ≥100 cigarettes in lifetime'
ukb$smok_detailed[ukb$smoke_detailed==3] <- 'current occasional smoker, smoked cigars or pipes daily in past'
ukb$smok_detailed[ukb$smoke_detailed==4] <- 'current occasional smoker, smoked cigarettes daily in past, <20/day'
ukb$smok_detailed[ukb$smoke_detailed==5] <- 'current occasional smoker, smoked cigarettes daily in past, ≥20/day'
ukb$smok_detailed[ukb$smoke_detailed==6] <- 'current cigar pipe smoker, former cigarette smoker'
ukb$smok_detailed[ukb$smoke_detailed==7] <- 'current cigar pipe smoker, not former cigarette smoker'
ukb$smok_detailed[ukb$smoke_detailed==8] <- 'current cigarette smoker, <10/day'
ukb$smok_detailed[ukb$smoke_detailed==9] <- 'current cigarette smoker, 10 to <20/day'
ukb$smok_detailed[ukb$smoke_detailed==10] <- 'current cigarette smoker, 20 to <40/day'
ukb$smok_detailed[ukb$smoke_detailed==11] <- 'current cigarette smoker, ≥40/day'
ukb$smok_detailed[ukb$smoke_detailed==12] <- 'former occasional cigarette smoker, smoked <100 cigarettes in lifetime'
ukb$smok_detailed[ukb$smoke_detailed==13] <- 'former occasional cigarette smoker, smoked ≥100 cigarettes in lifetime'
ukb$smok_detailed[ukb$smoke_detailed==14] <- 'former occasional cigarette smoker, lifetime cigarette smoking unknown'
ukb$smok_detailed[ukb$smoke_detailed==15] <- 'former daily cigar pipe smoker'
ukb$smok_detailed[ukb$smoke_detailed==16] <- 'former cigarette smoker, <20/day, quit <1 year ago'
ukb$smok_detailed[ukb$smoke_detailed==17] <- 'former cigarette smoker, ≥20/day, quit <1 year ago'
ukb$smok_detailed[ukb$smoke_detailed==18] <- 'former cigarette smoker, <20/day, quit 1-5 year ago'
ukb$smok_detailed[ukb$smoke_detailed==19] <- 'former cigarette smoker, ≥20/day, quit 1-5 year ago'
ukb$smok_detailed[ukb$smoke_detailed==20] <- 'former cigarette smoker, <20/day, quit 5-10 year ago'
ukb$smok_detailed[ukb$smoke_detailed==21] <- 'former cigarette smoker, ≥20/day, quit 5-10 year ago'
ukb$smok_detailed[ukb$smoke_detailed==22] <- 'former cigarette smoker, <20/day, quit 10-20 year ago'
ukb$smok_detailed[ukb$smoke_detailed==23] <- 'former cigarette smoker, ≥20/day, quit 10-20 year ago'
ukb$smok_detailed[ukb$smoke_detailed==24] <- 'former cigarette smoker, <20/day, quit ≥20 year ago'
ukb$smok_detailed[ukb$smoke_detailed==25] <- 'former cigarette smoker, ≥20/day, quit ≥20 year ago'
ukb$smok_detailed[ukb$smoke_detailed==99] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9901] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9902] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9903] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9904] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9905] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9906] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9907] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9908] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9909] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9910] <- 'missing'
ukb$smok_detailed[ukb$smoke_detailed==9911] <- 'missing'


#######
# END #
#######


summary(ukb$smok_detailed)
table(ukb$smok_detailed)
table(is.na(ukb$smok_detailed))

# Create final missing category 
ukb[is.na(ukb$smok_detailed),]$smok_detailed<- "missing"
table(ukb$smok_detailed)


ukb_final<- ukb[,c("f.eid","smoke_detailed", "smok_detailed")]
head(ukb_final)

# Output final 25-level smoking variable dataframe
saveRDS(ukb_final, file="/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/smoking_25_level.rds")


#################
# N/F/C smoking #
#################
### 20116= Smoking status
### 20160= Ever smoked 

table(ukb$f.20116.0.0)

ukb$smoke_NFC<- 9
ukb$smoke_NFC[which(ukb$f.20116.0.0 == 0)]<- 0 # Never
ukb$smoke_NFC[which(ukb$f.20116.0.0 == 1)]<- 1 # Former
ukb$smoke_NFC[which(ukb$f.20116.0.0 == 2)]<- 2 # Current

table(ukb$smoke_NFC)
ukb$smoke_NFC[which(ukb$f.20116.0.0 == 0)]<- 0

# Output final N/F/C smoking variable dataframe
saveRDS(ukb[,c("f.eid","smoke_NFC")], file="/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/smoking_NFC.rds")



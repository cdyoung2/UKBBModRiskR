# UKBB Modifiable Risk #
# Raw UKKB file generation & UKBB_final_file construction #

# Part 1: Using output from ukbconv to load in .tab file and r code to construction raw ukbb data file 
rm(list=ls(all=TRUE))
dev.off()
#options(stringsAsFactors = FALSE)

setwd('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/')
wkdir <- setwd('/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/')

# R file ukb677293_reducedvars.tab & ukb677293_reducedvars.r created on 2023-02-12 
# Loading of .tab file
bd <- read.table("/Users/youngcd/Library/CloudStorage/Box-Box/Machiela Lab/Projects/UK Biobank/UKBB_ModifiableRisk/ukb677293_reducedvars.tab", header=TRUE, sep="\t")
bd.orginal <- bd

# Code from ukb677293_reducedvars.r to adapt data from ukbconv, not needed currently
# bd <- read.table("/mnt/nfs/gigantor/ifs/DCEG/Projects/Aneuploidy/UKBiobank/Application_92005/datasets/original.main.datasets/basket_4/ukb677293_reducedvars.tab", header=TRUE, sep="\t")
# bd$f.53.0.0 <- as.Date(bd$f.53.0.0)
# bd$f.53.1.0 <- as.Date(bd$f.53.1.0)
# bd$f.53.2.0 <- as.Date(bd$f.53.2.0)
# bd$f.53.3.0 <- as.Date(bd$f.53.3.0)
# lvl.1965 <- c(1,2,3,4,5)
# lbl.1965 <- c("Death reported to UK Biobank by a relative","NHS records indicate they are lost to follow-up","NHS records indicate they have left the UK","UK Biobank sources report they have left the UK","Participant has withdrawn consent for future linkage")
# bd$f.190.0.0 <- ordered(bd$f.190.0.0, levels=lvl.1965, labels=lbl.1965)
# bd$f.191.0.0 <- as.Date(bd$f.191.0.0)
# lvl.100301 <- c(-3,-1,1,2,3,4)
# lbl.100301 <- c("Prefer not to answer","Do not know","Never/rarely","Sometimes","Usually","Always")
# bd$f.826.0.0 <- ordered(bd$f.826.0.0, levels=lvl.100301, labels=lbl.100301)
# bd$f.826.1.0 <- ordered(bd$f.826.1.0, levels=lvl.100301, labels=lbl.100301)
# bd$f.826.2.0 <- ordered(bd$f.826.2.0, levels=lvl.100301, labels=lbl.100301)
# bd$f.826.3.0 <- ordered(bd$f.826.3.0, levels=lvl.100301, labels=lbl.100301)
# lvl.100307 <- c(-3,-2,-1)
# lbl.100307 <- c("Prefer not to answer","Unable to walk","Do not know")
# lvl.100291 <- c(-3,-1)
# lbl.100291 <- c("Prefer not to answer","Do not know")
# lvl.100329 <- c(-10,-3,-1)
# lbl.100329 <- c("Less than an hour a day","Prefer not to answer","Do not know")
# lvl.100342 <- c(-3,-1,1,2,3,4)
# lbl.100342 <- c("Prefer not to answer","Do not know","Definitely a \'morning\' person","More a \'morning\' than \'evening\' person","More an \'evening\' than a \'morning\' person","Definitely an \'evening\' person")
# bd$f.1180.0.0 <- ordered(bd$f.1180.0.0, levels=lvl.100342, labels=lbl.100342)
# bd$f.1180.1.0 <- ordered(bd$f.1180.1.0, levels=lvl.100342, labels=lbl.100342)
# bd$f.1180.2.0 <- ordered(bd$f.1180.2.0, levels=lvl.100342, labels=lbl.100342)
# bd$f.1180.3.0 <- ordered(bd$f.1180.3.0, levels=lvl.100342, labels=lbl.100342)
# lvl.100343 <- c(-3,1,2,3)
# lbl.100343 <- c("Prefer not to answer","Never/rarely","Sometimes","Usually")
# bd$f.1200.0.0 <- ordered(bd$f.1200.0.0, levels=lvl.100343, labels=lbl.100343)
# bd$f.1200.1.0 <- ordered(bd$f.1200.1.0, levels=lvl.100343, labels=lbl.100343)
# bd$f.1200.2.0 <- ordered(bd$f.1200.2.0, levels=lvl.100343, labels=lbl.100343)
# bd$f.1200.3.0 <- ordered(bd$f.1200.3.0, levels=lvl.100343, labels=lbl.100343)
# lvl.100345 <- c(-3,-1,1,2)
# lbl.100345 <- c("Prefer not to answer","Do not know","Yes","No")
# bd$f.1210.0.0 <- ordered(bd$f.1210.0.0, levels=lvl.100345, labels=lbl.100345)
# bd$f.1210.1.0 <- ordered(bd$f.1210.1.0, levels=lvl.100345, labels=lbl.100345)
# bd$f.1210.2.0 <- ordered(bd$f.1210.2.0, levels=lvl.100345, labels=lbl.100345)
# bd$f.1210.3.0 <- ordered(bd$f.1210.3.0, levels=lvl.100345, labels=lbl.100345)
# lvl.100346 <- c(-3,-1,0,1,2,3)
# lbl.100346 <- c("Prefer not to answer","Do not know","Never/rarely","Sometimes","Often","All of the time")
# bd$f.1220.0.0 <- ordered(bd$f.1220.0.0, levels=lvl.100346, labels=lbl.100346)
# bd$f.1220.1.0 <- ordered(bd$f.1220.1.0, levels=lvl.100346, labels=lbl.100346)
# bd$f.1220.2.0 <- ordered(bd$f.1220.2.0, levels=lvl.100346, labels=lbl.100346)
# bd$f.1220.3.0 <- ordered(bd$f.1220.3.0, levels=lvl.100346, labels=lbl.100346)
# lvl.100347 <- c(-3,0,1,2)
# lbl.100347 <- c("Prefer not to answer","No","Yes, on most or all days","Only occasionally")
# bd$f.1239.0.0 <- ordered(bd$f.1239.0.0, levels=lvl.100347, labels=lbl.100347)
# bd$f.1239.1.0 <- ordered(bd$f.1239.1.0, levels=lvl.100347, labels=lbl.100347)
# bd$f.1239.2.0 <- ordered(bd$f.1239.2.0, levels=lvl.100347, labels=lbl.100347)
# bd$f.1239.3.0 <- ordered(bd$f.1239.3.0, levels=lvl.100347, labels=lbl.100347)
# lvl.100348 <- c(-3,1,2,3,4)
# lbl.100348 <- c("Prefer not to answer","Smoked on most or all days","Smoked occasionally","Just tried once or twice","I have never smoked")
# bd$f.1249.0.0 <- ordered(bd$f.1249.0.0, levels=lvl.100348, labels=lbl.100348)
# bd$f.1249.1.0 <- ordered(bd$f.1249.1.0, levels=lvl.100348, labels=lbl.100348)
# bd$f.1249.2.0 <- ordered(bd$f.1249.2.0, levels=lvl.100348, labels=lbl.100348)
# bd$f.1249.3.0 <- ordered(bd$f.1249.3.0, levels=lvl.100348, labels=lbl.100348)
# lvl.100402 <- c(-3,1,2,3,4,5,6)
# lbl.100402 <- c("Prefer not to answer","Daily or almost daily","Three or four times a week","Once or twice a week","One to three times a month","Special occasions only","Never")
# bd$f.1558.0.0 <- ordered(bd$f.1558.0.0, levels=lvl.100402, labels=lbl.100402)
# bd$f.1558.1.0 <- ordered(bd$f.1558.1.0, levels=lvl.100402, labels=lbl.100402)
# bd$f.1558.2.0 <- ordered(bd$f.1558.2.0, levels=lvl.100402, labels=lbl.100402)
# bd$f.1558.3.0 <- ordered(bd$f.1558.3.0, levels=lvl.100402, labels=lbl.100402)
# lvl.100349 <- c(-3,-1,0,1)
# lbl.100349 <- c("Prefer not to answer","Do not know","No","Yes")
# bd$f.2644.0.0 <- ordered(bd$f.2644.0.0, levels=lvl.100349, labels=lbl.100349)
# bd$f.2644.1.0 <- ordered(bd$f.2644.1.0, levels=lvl.100349, labels=lbl.100349)
# bd$f.2644.2.0 <- ordered(bd$f.2644.2.0, levels=lvl.100349, labels=lbl.100349)
# bd$f.2644.3.0 <- ordered(bd$f.2644.3.0, levels=lvl.100349, labels=lbl.100349)
# lvl.100351 <- c(-7,-3,1,2,3)
# lbl.100351 <- c("None of the above","Prefer not to answer","Manufactured cigarettes","Hand-rolled cigarettes","Cigars or pipes")
# bd$f.2877.0.0 <- ordered(bd$f.2877.0.0, levels=lvl.100351, labels=lbl.100351)
# bd$f.2877.1.0 <- ordered(bd$f.2877.1.0, levels=lvl.100351, labels=lbl.100351)
# bd$f.2877.2.0 <- ordered(bd$f.2877.2.0, levels=lvl.100351, labels=lbl.100351)
# bd$f.2877.3.0 <- ordered(bd$f.2877.3.0, levels=lvl.100351, labels=lbl.100351)
# lvl.100353 <- c(-10,-1)
# lbl.100353 <- c("Less than one a day","Do not know")
# bd$f.3426.0.0 <- ordered(bd$f.3426.0.0, levels=lvl.100301, labels=lbl.100301)
# bd$f.3426.1.0 <- ordered(bd$f.3426.1.0, levels=lvl.100301, labels=lbl.100301)
# bd$f.3426.2.0 <- ordered(bd$f.3426.2.0, levels=lvl.100301, labels=lbl.100301)
# bd$f.3426.3.0 <- ordered(bd$f.3426.3.0, levels=lvl.100301, labels=lbl.100301)
# bd$f.3446.0.0 <- ordered(bd$f.3446.0.0, levels=lvl.100351, labels=lbl.100351)
# bd$f.3446.1.0 <- ordered(bd$f.3446.1.0, levels=lvl.100351, labels=lbl.100351)
# bd$f.3446.2.0 <- ordered(bd$f.3446.2.0, levels=lvl.100351, labels=lbl.100351)
# bd$f.3446.3.0 <- ordered(bd$f.3446.3.0, levels=lvl.100351, labels=lbl.100351)
# lvl.100355 <- c(-10,-3,-1)
# lbl.100355 <- c("Less than one a day","Prefer not to answer","Do not know")
# lvl.100356 <- c(-3,-1,1,2,3,4,5)
# lbl.100356 <- c("Prefer not to answer","Do not know","Less than 5 minutes","Between 5-15 minutes","Between 30 minutes - 1 hour","Between 1 and 2 hours","Longer than 2 hours")
# bd$f.3466.0.0 <- ordered(bd$f.3466.0.0, levels=lvl.100356, labels=lbl.100356)
# bd$f.3466.1.0 <- ordered(bd$f.3466.1.0, levels=lvl.100356, labels=lbl.100356)
# bd$f.3466.2.0 <- ordered(bd$f.3466.2.0, levels=lvl.100356, labels=lbl.100356)
# bd$f.3466.3.0 <- ordered(bd$f.3466.3.0, levels=lvl.100356, labels=lbl.100356)
# lvl.100352 <- c(-3,0,1)
# lbl.100352 <- c("Prefer not to answer","No","Yes")
# bd$f.5959.0.0 <- ordered(bd$f.5959.0.0, levels=lvl.100352, labels=lbl.100352)
# bd$f.5959.1.0 <- ordered(bd$f.5959.1.0, levels=lvl.100352, labels=lbl.100352)
# bd$f.5959.2.0 <- ordered(bd$f.5959.2.0, levels=lvl.100352, labels=lbl.100352)
# bd$f.5959.3.0 <- ordered(bd$f.5959.3.0, levels=lvl.100352, labels=lbl.100352)
# lvl.0090 <- c(-3,0,1,2)
# lbl.0090 <- c("Prefer not to answer","Never","Previous","Current")
# bd$f.20116.0.0 <- ordered(bd$f.20116.0.0, levels=lvl.0090, labels=lbl.0090)
# bd$f.20116.1.0 <- ordered(bd$f.20116.1.0, levels=lvl.0090, labels=lbl.0090)
# bd$f.20116.2.0 <- ordered(bd$f.20116.2.0, levels=lvl.0090, labels=lbl.0090)
# bd$f.20116.3.0 <- ordered(bd$f.20116.3.0, levels=lvl.0090, labels=lbl.0090)
# bd$f.20117.0.0 <- ordered(bd$f.20117.0.0, levels=lvl.0090, labels=lbl.0090)
# bd$f.20117.1.0 <- ordered(bd$f.20117.1.0, levels=lvl.0090, labels=lbl.0090)
# bd$f.20117.2.0 <- ordered(bd$f.20117.2.0, levels=lvl.0090, labels=lbl.0090)
# bd$f.20117.3.0 <- ordered(bd$f.20117.3.0, levels=lvl.0090, labels=lbl.0090)
# lvl.0007 <- c(0,1)
# lbl.0007 <- c("No","Yes")
# bd$f.20160.0.0 <- ordered(bd$f.20160.0.0, levels=lvl.0007, labels=lbl.0007)
# bd$f.20160.1.0 <- ordered(bd$f.20160.1.0, levels=lvl.0007, labels=lbl.0007)
# bd$f.20160.2.0 <- ordered(bd$f.20160.2.0, levels=lvl.0007, labels=lbl.0007)
# bd$f.20160.3.0 <- ordered(bd$f.20160.3.0, levels=lvl.0007, labels=lbl.0007)
# lvl.0522 <- c(-818,1,2,3,4,5)
# lbl.0522 <- c("Prefer not to answer","1 or 2","3 or 4","5 or 6","7, 8 or 9","10 or more")
# bd$f.20403.0.0 <- ordered(bd$f.20403.0.0, levels=lvl.0522, labels=lbl.0522)
# lvl.0521 <- c(-818,0,1,2,3,4)
# lbl.0521 <- c("Prefer not to answer","Never","Monthly or less","2 to 4 times a month","2 to 3 times a week","4 or more times a week")
# bd$f.20414.0.0 <- ordered(bd$f.20414.0.0, levels=lvl.0521, labels=lbl.0521)
# lvl.0523 <- c(-818,1,2,3,4,5)
# lbl.0523 <- c("Prefer not to answer","Never","Less than monthly","Monthly","Weekly","Daily or almost daily")
# bd$f.20416.0.0 <- ordered(bd$f.20416.0.0, levels=lvl.0523, labels=lbl.0523)
# lvl.1001 <- c(-3,-1,1,2,3,4,5,6,1001,1002,1003,2001,2002,2003,2004,3001,3002,3003,3004,4001,4002,4003)
# lbl.1001 <- c("Prefer not to answer","Do not know","White","Mixed","Asian or Asian British","Black or Black British","Chinese","Other ethnic group","British","Irish","Any other white background","White and Black Caribbean","White and Black African","White and Asian","Any other mixed background","Indian","Pakistani","Bangladeshi","Any other Asian background","Caribbean","African","Any other Black background")
# bd$f.21000.0.0 <- ordered(bd$f.21000.0.0, levels=lvl.1001, labels=lbl.1001)
# bd$f.21000.1.0 <- ordered(bd$f.21000.1.0, levels=lvl.1001, labels=lbl.1001)
# bd$f.21000.2.0 <- ordered(bd$f.21000.2.0, levels=lvl.1001, labels=lbl.1001)
# bd$f.21000.3.0 <- ordered(bd$f.21000.3.0, levels=lvl.1001, labels=lbl.1001)
# lvl.0009 <- c(0,1)
# lbl.0009 <- c("Female","Male")
# bd$f.22001.0.0 <- ordered(bd$f.22001.0.0, levels=lvl.0009, labels=lbl.0009)
# lvl.1002 <- c(1)
# lbl.1002 <- c("Caucasian")
# bd$f.22006.0.0 <- ordered(bd$f.22006.0.0, levels=lvl.1002, labels=lbl.1002)
# bd$f.22620.0.0 <- ordered(bd$f.22620.0.0, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.1 <- ordered(bd$f.22620.0.1, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.2 <- ordered(bd$f.22620.0.2, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.3 <- ordered(bd$f.22620.0.3, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.4 <- ordered(bd$f.22620.0.4, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.5 <- ordered(bd$f.22620.0.5, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.6 <- ordered(bd$f.22620.0.6, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.7 <- ordered(bd$f.22620.0.7, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.8 <- ordered(bd$f.22620.0.8, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.9 <- ordered(bd$f.22620.0.9, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.10 <- ordered(bd$f.22620.0.10, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.11 <- ordered(bd$f.22620.0.11, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.12 <- ordered(bd$f.22620.0.12, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.13 <- ordered(bd$f.22620.0.13, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.14 <- ordered(bd$f.22620.0.14, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.15 <- ordered(bd$f.22620.0.15, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.16 <- ordered(bd$f.22620.0.16, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.17 <- ordered(bd$f.22620.0.17, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.18 <- ordered(bd$f.22620.0.18, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.19 <- ordered(bd$f.22620.0.19, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.20 <- ordered(bd$f.22620.0.20, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.21 <- ordered(bd$f.22620.0.21, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.22 <- ordered(bd$f.22620.0.22, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.23 <- ordered(bd$f.22620.0.23, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.24 <- ordered(bd$f.22620.0.24, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.25 <- ordered(bd$f.22620.0.25, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.26 <- ordered(bd$f.22620.0.26, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.27 <- ordered(bd$f.22620.0.27, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.28 <- ordered(bd$f.22620.0.28, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.29 <- ordered(bd$f.22620.0.29, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.30 <- ordered(bd$f.22620.0.30, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.31 <- ordered(bd$f.22620.0.31, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.32 <- ordered(bd$f.22620.0.32, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.33 <- ordered(bd$f.22620.0.33, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.34 <- ordered(bd$f.22620.0.34, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.35 <- ordered(bd$f.22620.0.35, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.36 <- ordered(bd$f.22620.0.36, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.37 <- ordered(bd$f.22620.0.37, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.38 <- ordered(bd$f.22620.0.38, levels=lvl.0007, labels=lbl.0007)
# bd$f.22620.0.39 <- ordered(bd$f.22620.0.39, levels=lvl.0007, labels=lbl.0007)
# lvl.0489 <- c(0,1,9)
# lbl.0489 <- c("Shift pattern was worked for some (but not all) of job","Shift pattern was worked for whole of job","This type of shift pattern was not worked during job")
# bd$f.22630.0.0 <- ordered(bd$f.22630.0.0, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.1 <- ordered(bd$f.22630.0.1, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.2 <- ordered(bd$f.22630.0.2, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.3 <- ordered(bd$f.22630.0.3, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.4 <- ordered(bd$f.22630.0.4, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.5 <- ordered(bd$f.22630.0.5, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.6 <- ordered(bd$f.22630.0.6, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.7 <- ordered(bd$f.22630.0.7, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.8 <- ordered(bd$f.22630.0.8, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.9 <- ordered(bd$f.22630.0.9, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.10 <- ordered(bd$f.22630.0.10, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.11 <- ordered(bd$f.22630.0.11, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.12 <- ordered(bd$f.22630.0.12, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.13 <- ordered(bd$f.22630.0.13, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.14 <- ordered(bd$f.22630.0.14, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.15 <- ordered(bd$f.22630.0.15, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.16 <- ordered(bd$f.22630.0.16, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.17 <- ordered(bd$f.22630.0.17, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.18 <- ordered(bd$f.22630.0.18, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.19 <- ordered(bd$f.22630.0.19, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.20 <- ordered(bd$f.22630.0.20, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.21 <- ordered(bd$f.22630.0.21, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.22 <- ordered(bd$f.22630.0.22, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.23 <- ordered(bd$f.22630.0.23, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.24 <- ordered(bd$f.22630.0.24, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.25 <- ordered(bd$f.22630.0.25, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.26 <- ordered(bd$f.22630.0.26, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.27 <- ordered(bd$f.22630.0.27, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.28 <- ordered(bd$f.22630.0.28, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.29 <- ordered(bd$f.22630.0.29, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.30 <- ordered(bd$f.22630.0.30, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.31 <- ordered(bd$f.22630.0.31, levels=lvl.0489, labels=lbl.0489)
# bd$f.22630.0.32 <- ordered(bd$f.22630.0.32, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.0 <- ordered(bd$f.22640.0.0, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.1 <- ordered(bd$f.22640.0.1, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.2 <- ordered(bd$f.22640.0.2, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.3 <- ordered(bd$f.22640.0.3, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.4 <- ordered(bd$f.22640.0.4, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.5 <- ordered(bd$f.22640.0.5, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.6 <- ordered(bd$f.22640.0.6, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.7 <- ordered(bd$f.22640.0.7, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.8 <- ordered(bd$f.22640.0.8, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.9 <- ordered(bd$f.22640.0.9, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.10 <- ordered(bd$f.22640.0.10, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.11 <- ordered(bd$f.22640.0.11, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.12 <- ordered(bd$f.22640.0.12, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.13 <- ordered(bd$f.22640.0.13, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.14 <- ordered(bd$f.22640.0.14, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.15 <- ordered(bd$f.22640.0.15, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.16 <- ordered(bd$f.22640.0.16, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.17 <- ordered(bd$f.22640.0.17, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.18 <- ordered(bd$f.22640.0.18, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.19 <- ordered(bd$f.22640.0.19, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.20 <- ordered(bd$f.22640.0.20, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.21 <- ordered(bd$f.22640.0.21, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.22 <- ordered(bd$f.22640.0.22, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.23 <- ordered(bd$f.22640.0.23, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.24 <- ordered(bd$f.22640.0.24, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.25 <- ordered(bd$f.22640.0.25, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.26 <- ordered(bd$f.22640.0.26, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.27 <- ordered(bd$f.22640.0.27, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.28 <- ordered(bd$f.22640.0.28, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.29 <- ordered(bd$f.22640.0.29, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.30 <- ordered(bd$f.22640.0.30, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.31 <- ordered(bd$f.22640.0.31, levels=lvl.0489, labels=lbl.0489)
# bd$f.22640.0.32 <- ordered(bd$f.22640.0.32, levels=lvl.0489, labels=lbl.0489)
# lvl.0488 <- c(-1001)
# lbl.0488 <- c("Less than one year")
# lvl.0487 <- c(-1)
# lbl.0487 <- c("More than a month")
# bd$f.22650.0.0 <- ordered(bd$f.22650.0.0, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.1 <- ordered(bd$f.22650.0.1, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.2 <- ordered(bd$f.22650.0.2, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.3 <- ordered(bd$f.22650.0.3, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.4 <- ordered(bd$f.22650.0.4, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.5 <- ordered(bd$f.22650.0.5, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.6 <- ordered(bd$f.22650.0.6, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.7 <- ordered(bd$f.22650.0.7, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.8 <- ordered(bd$f.22650.0.8, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.9 <- ordered(bd$f.22650.0.9, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.10 <- ordered(bd$f.22650.0.10, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.11 <- ordered(bd$f.22650.0.11, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.12 <- ordered(bd$f.22650.0.12, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.13 <- ordered(bd$f.22650.0.13, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.14 <- ordered(bd$f.22650.0.14, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.15 <- ordered(bd$f.22650.0.15, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.16 <- ordered(bd$f.22650.0.16, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.17 <- ordered(bd$f.22650.0.17, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.18 <- ordered(bd$f.22650.0.18, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.19 <- ordered(bd$f.22650.0.19, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.20 <- ordered(bd$f.22650.0.20, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.21 <- ordered(bd$f.22650.0.21, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.22 <- ordered(bd$f.22650.0.22, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.23 <- ordered(bd$f.22650.0.23, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.24 <- ordered(bd$f.22650.0.24, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.25 <- ordered(bd$f.22650.0.25, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.26 <- ordered(bd$f.22650.0.26, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.27 <- ordered(bd$f.22650.0.27, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.28 <- ordered(bd$f.22650.0.28, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.29 <- ordered(bd$f.22650.0.29, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.30 <- ordered(bd$f.22650.0.30, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.31 <- ordered(bd$f.22650.0.31, levels=lvl.0489, labels=lbl.0489)
# bd$f.22650.0.32 <- ordered(bd$f.22650.0.32, levels=lvl.0489, labels=lbl.0489)
# bd$f.40000.0.0 <- as.Date(bd$f.40000.0.0)
# bd$f.40000.1.0 <- as.Date(bd$f.40000.1.0)
# bd$f.40005.0.0 <- as.Date(bd$f.40005.0.0)
# bd$f.40005.1.0 <- as.Date(bd$f.40005.1.0)
# bd$f.40005.2.0 <- as.Date(bd$f.40005.2.0)
# bd$f.40005.3.0 <- as.Date(bd$f.40005.3.0)
# bd$f.40005.4.0 <- as.Date(bd$f.40005.4.0)
# bd$f.40005.5.0 <- as.Date(bd$f.40005.5.0)
# bd$f.40005.6.0 <- as.Date(bd$f.40005.6.0)
# bd$f.40005.7.0 <- as.Date(bd$f.40005.7.0)
# bd$f.40005.8.0 <- as.Date(bd$f.40005.8.0)
# bd$f.40005.9.0 <- as.Date(bd$f.40005.9.0)
# bd$f.40005.10.0 <- as.Date(bd$f.40005.10.0)
# bd$f.40005.11.0 <- as.Date(bd$f.40005.11.0)
# bd$f.40005.12.0 <- as.Date(bd$f.40005.12.0)
# bd$f.40005.13.0 <- as.Date(bd$f.40005.13.0)
# bd$f.40005.14.0 <- as.Date(bd$f.40005.14.0)
# bd$f.40005.15.0 <- as.Date(bd$f.40005.15.0)
# bd$f.40005.16.0 <- as.Date(bd$f.40005.16.0)
# bd$f.40005.17.0 <- as.Date(bd$f.40005.17.0)
# bd$f.40005.18.0 <- as.Date(bd$f.40005.18.0)
# bd$f.40005.19.0 <- as.Date(bd$f.40005.19.0)
# bd$f.40005.20.0 <- as.Date(bd$f.40005.20.0)
# bd$f.40005.21.0 <- as.Date(bd$f.40005.21.0)
# bd$f.41280.0.0 <- as.Date(bd$f.41280.0.0)
# bd$f.41280.0.1 <- as.Date(bd$f.41280.0.1)
# bd$f.41280.0.2 <- as.Date(bd$f.41280.0.2)
# bd$f.41280.0.3 <- as.Date(bd$f.41280.0.3)
# bd$f.41280.0.4 <- as.Date(bd$f.41280.0.4)
# bd$f.41280.0.5 <- as.Date(bd$f.41280.0.5)
# bd$f.41280.0.6 <- as.Date(bd$f.41280.0.6)
# bd$f.41280.0.7 <- as.Date(bd$f.41280.0.7)
# bd$f.41280.0.8 <- as.Date(bd$f.41280.0.8)
# bd$f.41280.0.9 <- as.Date(bd$f.41280.0.9)
# bd$f.41280.0.10 <- as.Date(bd$f.41280.0.10)
# bd$f.41280.0.11 <- as.Date(bd$f.41280.0.11)
# bd$f.41280.0.12 <- as.Date(bd$f.41280.0.12)
# bd$f.41280.0.13 <- as.Date(bd$f.41280.0.13)
# bd$f.41280.0.14 <- as.Date(bd$f.41280.0.14)
# bd$f.41280.0.15 <- as.Date(bd$f.41280.0.15)
# bd$f.41280.0.16 <- as.Date(bd$f.41280.0.16)
# bd$f.41280.0.17 <- as.Date(bd$f.41280.0.17)
# bd$f.41280.0.18 <- as.Date(bd$f.41280.0.18)
# bd$f.41280.0.19 <- as.Date(bd$f.41280.0.19)
# bd$f.41280.0.20 <- as.Date(bd$f.41280.0.20)
# bd$f.41280.0.21 <- as.Date(bd$f.41280.0.21)
# bd$f.41280.0.22 <- as.Date(bd$f.41280.0.22)
# bd$f.41280.0.23 <- as.Date(bd$f.41280.0.23)
# bd$f.41280.0.24 <- as.Date(bd$f.41280.0.24)
# bd$f.41280.0.25 <- as.Date(bd$f.41280.0.25)
# bd$f.41280.0.26 <- as.Date(bd$f.41280.0.26)
# bd$f.41280.0.27 <- as.Date(bd$f.41280.0.27)
# bd$f.41280.0.28 <- as.Date(bd$f.41280.0.28)
# bd$f.41280.0.29 <- as.Date(bd$f.41280.0.29)
# bd$f.41280.0.30 <- as.Date(bd$f.41280.0.30)
# bd$f.41280.0.31 <- as.Date(bd$f.41280.0.31)
# bd$f.41280.0.32 <- as.Date(bd$f.41280.0.32)
# bd$f.41280.0.33 <- as.Date(bd$f.41280.0.33)
# bd$f.41280.0.34 <- as.Date(bd$f.41280.0.34)
# bd$f.41280.0.35 <- as.Date(bd$f.41280.0.35)
# bd$f.41280.0.36 <- as.Date(bd$f.41280.0.36)
# bd$f.41280.0.37 <- as.Date(bd$f.41280.0.37)
# bd$f.41280.0.38 <- as.Date(bd$f.41280.0.38)
# bd$f.41280.0.39 <- as.Date(bd$f.41280.0.39)
# bd$f.41280.0.40 <- as.Date(bd$f.41280.0.40)
# bd$f.41280.0.41 <- as.Date(bd$f.41280.0.41)
# bd$f.41280.0.42 <- as.Date(bd$f.41280.0.42)
# bd$f.41280.0.43 <- as.Date(bd$f.41280.0.43)
# bd$f.41280.0.44 <- as.Date(bd$f.41280.0.44)
# bd$f.41280.0.45 <- as.Date(bd$f.41280.0.45)
# bd$f.41280.0.46 <- as.Date(bd$f.41280.0.46)
# bd$f.41280.0.47 <- as.Date(bd$f.41280.0.47)
# bd$f.41280.0.48 <- as.Date(bd$f.41280.0.48)
# bd$f.41280.0.49 <- as.Date(bd$f.41280.0.49)
# bd$f.41280.0.50 <- as.Date(bd$f.41280.0.50)
# bd$f.41280.0.51 <- as.Date(bd$f.41280.0.51)
# bd$f.41280.0.52 <- as.Date(bd$f.41280.0.52)
# bd$f.41280.0.53 <- as.Date(bd$f.41280.0.53)
# bd$f.41280.0.54 <- as.Date(bd$f.41280.0.54)
# bd$f.41280.0.55 <- as.Date(bd$f.41280.0.55)
# bd$f.41280.0.56 <- as.Date(bd$f.41280.0.56)
# bd$f.41280.0.57 <- as.Date(bd$f.41280.0.57)
# bd$f.41280.0.58 <- as.Date(bd$f.41280.0.58)
# bd$f.41280.0.59 <- as.Date(bd$f.41280.0.59)
# bd$f.41280.0.60 <- as.Date(bd$f.41280.0.60)
# bd$f.41280.0.61 <- as.Date(bd$f.41280.0.61)
# bd$f.41280.0.62 <- as.Date(bd$f.41280.0.62)
# bd$f.41280.0.63 <- as.Date(bd$f.41280.0.63)
# bd$f.41280.0.64 <- as.Date(bd$f.41280.0.64)
# bd$f.41280.0.65 <- as.Date(bd$f.41280.0.65)
# bd$f.41280.0.66 <- as.Date(bd$f.41280.0.66)
# bd$f.41280.0.67 <- as.Date(bd$f.41280.0.67)
# bd$f.41280.0.68 <- as.Date(bd$f.41280.0.68)
# bd$f.41280.0.69 <- as.Date(bd$f.41280.0.69)
# bd$f.41280.0.70 <- as.Date(bd$f.41280.0.70)
# bd$f.41280.0.71 <- as.Date(bd$f.41280.0.71)
# bd$f.41280.0.72 <- as.Date(bd$f.41280.0.72)
# bd$f.41280.0.73 <- as.Date(bd$f.41280.0.73)
# bd$f.41280.0.74 <- as.Date(bd$f.41280.0.74)
# bd$f.41280.0.75 <- as.Date(bd$f.41280.0.75)
# bd$f.41280.0.76 <- as.Date(bd$f.41280.0.76)
# bd$f.41280.0.77 <- as.Date(bd$f.41280.0.77)
# bd$f.41280.0.78 <- as.Date(bd$f.41280.0.78)
# bd$f.41280.0.79 <- as.Date(bd$f.41280.0.79)
# bd$f.41280.0.80 <- as.Date(bd$f.41280.0.80)
# bd$f.41280.0.81 <- as.Date(bd$f.41280.0.81)
# bd$f.41280.0.82 <- as.Date(bd$f.41280.0.82)
# bd$f.41280.0.83 <- as.Date(bd$f.41280.0.83)
# bd$f.41280.0.84 <- as.Date(bd$f.41280.0.84)
# bd$f.41280.0.85 <- as.Date(bd$f.41280.0.85)
# bd$f.41280.0.86 <- as.Date(bd$f.41280.0.86)
# bd$f.41280.0.87 <- as.Date(bd$f.41280.0.87)
# bd$f.41280.0.88 <- as.Date(bd$f.41280.0.88)
# bd$f.41280.0.89 <- as.Date(bd$f.41280.0.89)
# bd$f.41280.0.90 <- as.Date(bd$f.41280.0.90)
# bd$f.41280.0.91 <- as.Date(bd$f.41280.0.91)
# bd$f.41280.0.92 <- as.Date(bd$f.41280.0.92)
# bd$f.41280.0.93 <- as.Date(bd$f.41280.0.93)
# bd$f.41280.0.94 <- as.Date(bd$f.41280.0.94)
# bd$f.41280.0.95 <- as.Date(bd$f.41280.0.95)
# bd$f.41280.0.96 <- as.Date(bd$f.41280.0.96)
# bd$f.41280.0.97 <- as.Date(bd$f.41280.0.97)
# bd$f.41280.0.98 <- as.Date(bd$f.41280.0.98)
# bd$f.41280.0.99 <- as.Date(bd$f.41280.0.99)
# bd$f.41280.0.100 <- as.Date(bd$f.41280.0.100)
# bd$f.41280.0.101 <- as.Date(bd$f.41280.0.101)
# bd$f.41280.0.102 <- as.Date(bd$f.41280.0.102)
# bd$f.41280.0.103 <- as.Date(bd$f.41280.0.103)
# bd$f.41280.0.104 <- as.Date(bd$f.41280.0.104)
# bd$f.41280.0.105 <- as.Date(bd$f.41280.0.105)
# bd$f.41280.0.106 <- as.Date(bd$f.41280.0.106)
# bd$f.41280.0.107 <- as.Date(bd$f.41280.0.107)
# bd$f.41280.0.108 <- as.Date(bd$f.41280.0.108)
# bd$f.41280.0.109 <- as.Date(bd$f.41280.0.109)
# bd$f.41280.0.110 <- as.Date(bd$f.41280.0.110)
# bd$f.41280.0.111 <- as.Date(bd$f.41280.0.111)
# bd$f.41280.0.112 <- as.Date(bd$f.41280.0.112)
# bd$f.41280.0.113 <- as.Date(bd$f.41280.0.113)
# bd$f.41280.0.114 <- as.Date(bd$f.41280.0.114)
# bd$f.41280.0.115 <- as.Date(bd$f.41280.0.115)
# bd$f.41280.0.116 <- as.Date(bd$f.41280.0.116)
# bd$f.41280.0.117 <- as.Date(bd$f.41280.0.117)
# bd$f.41280.0.118 <- as.Date(bd$f.41280.0.118)
# bd$f.41280.0.119 <- as.Date(bd$f.41280.0.119)
# bd$f.41280.0.120 <- as.Date(bd$f.41280.0.120)
# bd$f.41280.0.121 <- as.Date(bd$f.41280.0.121)
# bd$f.41280.0.122 <- as.Date(bd$f.41280.0.122)
# bd$f.41280.0.123 <- as.Date(bd$f.41280.0.123)
# bd$f.41280.0.124 <- as.Date(bd$f.41280.0.124)
# bd$f.41280.0.125 <- as.Date(bd$f.41280.0.125)
# bd$f.41280.0.126 <- as.Date(bd$f.41280.0.126)
# bd$f.41280.0.127 <- as.Date(bd$f.41280.0.127)
# bd$f.41280.0.128 <- as.Date(bd$f.41280.0.128)
# bd$f.41280.0.129 <- as.Date(bd$f.41280.0.129)
# bd$f.41280.0.130 <- as.Date(bd$f.41280.0.130)
# bd$f.41280.0.131 <- as.Date(bd$f.41280.0.131)
# bd$f.41280.0.132 <- as.Date(bd$f.41280.0.132)
# bd$f.41280.0.133 <- as.Date(bd$f.41280.0.133)
# bd$f.41280.0.134 <- as.Date(bd$f.41280.0.134)
# bd$f.41280.0.135 <- as.Date(bd$f.41280.0.135)
# bd$f.41280.0.136 <- as.Date(bd$f.41280.0.136)
# bd$f.41280.0.137 <- as.Date(bd$f.41280.0.137)
# bd$f.41280.0.138 <- as.Date(bd$f.41280.0.138)
# bd$f.41280.0.139 <- as.Date(bd$f.41280.0.139)
# bd$f.41280.0.140 <- as.Date(bd$f.41280.0.140)
# bd$f.41280.0.141 <- as.Date(bd$f.41280.0.141)
# bd$f.41280.0.142 <- as.Date(bd$f.41280.0.142)
# bd$f.41280.0.143 <- as.Date(bd$f.41280.0.143)
# bd$f.41280.0.144 <- as.Date(bd$f.41280.0.144)
# bd$f.41280.0.145 <- as.Date(bd$f.41280.0.145)
# bd$f.41280.0.146 <- as.Date(bd$f.41280.0.146)
# bd$f.41280.0.147 <- as.Date(bd$f.41280.0.147)
# bd$f.41280.0.148 <- as.Date(bd$f.41280.0.148)
# bd$f.41280.0.149 <- as.Date(bd$f.41280.0.149)
# bd$f.41280.0.150 <- as.Date(bd$f.41280.0.150)
# bd$f.41280.0.151 <- as.Date(bd$f.41280.0.151)
# bd$f.41280.0.152 <- as.Date(bd$f.41280.0.152)
# bd$f.41280.0.153 <- as.Date(bd$f.41280.0.153)
# bd$f.41280.0.154 <- as.Date(bd$f.41280.0.154)
# bd$f.41280.0.155 <- as.Date(bd$f.41280.0.155)
# bd$f.41280.0.156 <- as.Date(bd$f.41280.0.156)
# bd$f.41280.0.157 <- as.Date(bd$f.41280.0.157)
# bd$f.41280.0.158 <- as.Date(bd$f.41280.0.158)
# bd$f.41280.0.159 <- as.Date(bd$f.41280.0.159)
# bd$f.41280.0.160 <- as.Date(bd$f.41280.0.160)
# bd$f.41280.0.161 <- as.Date(bd$f.41280.0.161)
# bd$f.41280.0.162 <- as.Date(bd$f.41280.0.162)
# bd$f.41280.0.163 <- as.Date(bd$f.41280.0.163)
# bd$f.41280.0.164 <- as.Date(bd$f.41280.0.164)
# bd$f.41280.0.165 <- as.Date(bd$f.41280.0.165)
# bd$f.41280.0.166 <- as.Date(bd$f.41280.0.166)
# bd$f.41280.0.167 <- as.Date(bd$f.41280.0.167)
# bd$f.41280.0.168 <- as.Date(bd$f.41280.0.168)
# bd$f.41280.0.169 <- as.Date(bd$f.41280.0.169)
# bd$f.41280.0.170 <- as.Date(bd$f.41280.0.170)
# bd$f.41280.0.171 <- as.Date(bd$f.41280.0.171)
# bd$f.41280.0.172 <- as.Date(bd$f.41280.0.172)
# bd$f.41280.0.173 <- as.Date(bd$f.41280.0.173)
# bd$f.41280.0.174 <- as.Date(bd$f.41280.0.174)
# bd$f.41280.0.175 <- as.Date(bd$f.41280.0.175)
# bd$f.41280.0.176 <- as.Date(bd$f.41280.0.176)
# bd$f.41280.0.177 <- as.Date(bd$f.41280.0.177)
# bd$f.41280.0.178 <- as.Date(bd$f.41280.0.178)
# bd$f.41280.0.179 <- as.Date(bd$f.41280.0.179)
# bd$f.41280.0.180 <- as.Date(bd$f.41280.0.180)
# bd$f.41280.0.181 <- as.Date(bd$f.41280.0.181)
# bd$f.41280.0.182 <- as.Date(bd$f.41280.0.182)
# bd$f.41280.0.183 <- as.Date(bd$f.41280.0.183)
# bd$f.41280.0.184 <- as.Date(bd$f.41280.0.184)
# bd$f.41280.0.185 <- as.Date(bd$f.41280.0.185)
# bd$f.41280.0.186 <- as.Date(bd$f.41280.0.186)
# bd$f.41280.0.187 <- as.Date(bd$f.41280.0.187)
# bd$f.41280.0.188 <- as.Date(bd$f.41280.0.188)
# bd$f.41280.0.189 <- as.Date(bd$f.41280.0.189)
# bd$f.41280.0.190 <- as.Date(bd$f.41280.0.190)
# bd$f.41280.0.191 <- as.Date(bd$f.41280.0.191)
# bd$f.41280.0.192 <- as.Date(bd$f.41280.0.192)
# bd$f.41280.0.193 <- as.Date(bd$f.41280.0.193)
# bd$f.41280.0.194 <- as.Date(bd$f.41280.0.194)
# bd$f.41280.0.195 <- as.Date(bd$f.41280.0.195)
# bd$f.41280.0.196 <- as.Date(bd$f.41280.0.196)
# bd$f.41280.0.197 <- as.Date(bd$f.41280.0.197)
# bd$f.41280.0.198 <- as.Date(bd$f.41280.0.198)
# bd$f.41280.0.199 <- as.Date(bd$f.41280.0.199)
# bd$f.41280.0.200 <- as.Date(bd$f.41280.0.200)
# bd$f.41280.0.201 <- as.Date(bd$f.41280.0.201)
# bd$f.41280.0.202 <- as.Date(bd$f.41280.0.202)
# bd$f.41280.0.203 <- as.Date(bd$f.41280.0.203)
# bd$f.41280.0.204 <- as.Date(bd$f.41280.0.204)
# bd$f.41280.0.205 <- as.Date(bd$f.41280.0.205)
# bd$f.41280.0.206 <- as.Date(bd$f.41280.0.206)
# bd$f.41280.0.207 <- as.Date(bd$f.41280.0.207)
# bd$f.41280.0.208 <- as.Date(bd$f.41280.0.208)
# bd$f.41280.0.209 <- as.Date(bd$f.41280.0.209)
# bd$f.41280.0.210 <- as.Date(bd$f.41280.0.210)
# bd$f.41280.0.211 <- as.Date(bd$f.41280.0.211)
# bd$f.41280.0.212 <- as.Date(bd$f.41280.0.212)
# bd$f.41280.0.213 <- as.Date(bd$f.41280.0.213)
# bd$f.41280.0.214 <- as.Date(bd$f.41280.0.214)
# bd$f.41280.0.215 <- as.Date(bd$f.41280.0.215)
# bd$f.41280.0.216 <- as.Date(bd$f.41280.0.216)
# bd$f.41280.0.217 <- as.Date(bd$f.41280.0.217)
# bd$f.41280.0.218 <- as.Date(bd$f.41280.0.218)
# bd$f.41280.0.219 <- as.Date(bd$f.41280.0.219)
# bd$f.41280.0.220 <- as.Date(bd$f.41280.0.220)
# bd$f.41280.0.221 <- as.Date(bd$f.41280.0.221)
# bd$f.41280.0.222 <- as.Date(bd$f.41280.0.222)
# bd$f.41280.0.223 <- as.Date(bd$f.41280.0.223)
# bd$f.41280.0.224 <- as.Date(bd$f.41280.0.224)
# bd$f.41280.0.225 <- as.Date(bd$f.41280.0.225)
# bd$f.41280.0.226 <- as.Date(bd$f.41280.0.226)
# bd$f.41280.0.227 <- as.Date(bd$f.41280.0.227)
# bd$f.41280.0.228 <- as.Date(bd$f.41280.0.228)
# bd$f.41280.0.229 <- as.Date(bd$f.41280.0.229)
# bd$f.41280.0.230 <- as.Date(bd$f.41280.0.230)
# bd$f.41280.0.231 <- as.Date(bd$f.41280.0.231)
# bd$f.41280.0.232 <- as.Date(bd$f.41280.0.232)
# bd$f.41280.0.233 <- as.Date(bd$f.41280.0.233)
# bd$f.41280.0.234 <- as.Date(bd$f.41280.0.234)
# bd$f.41280.0.235 <- as.Date(bd$f.41280.0.235)
# bd$f.41280.0.236 <- as.Date(bd$f.41280.0.236)
# bd$f.41280.0.237 <- as.Date(bd$f.41280.0.237)
# bd$f.41280.0.238 <- as.Date(bd$f.41280.0.238)
# bd$f.41280.0.239 <- as.Date(bd$f.41280.0.239)
# bd$f.41280.0.240 <- as.Date(bd$f.41280.0.240)
# bd$f.41280.0.241 <- as.Date(bd$f.41280.0.241)
# bd$f.41280.0.242 <- as.Date(bd$f.41280.0.242)
# bd$f.41280.0.243 <- as.Date(bd$f.41280.0.243)
# bd$f.41280.0.244 <- as.Date(bd$f.41280.0.244)
# bd$f.41280.0.245 <- as.Date(bd$f.41280.0.245)
# bd$f.41280.0.246 <- as.Date(bd$f.41280.0.246)
# bd$f.41280.0.247 <- as.Date(bd$f.41280.0.247)
# bd$f.41280.0.248 <- as.Date(bd$f.41280.0.248)
# bd$f.41280.0.249 <- as.Date(bd$f.41280.0.249)
# bd$f.41280.0.250 <- as.Date(bd$f.41280.0.250)
# bd$f.41280.0.251 <- as.Date(bd$f.41280.0.251)
# bd$f.41280.0.252 <- as.Date(bd$f.41280.0.252)
# bd$f.41280.0.253 <- as.Date(bd$f.41280.0.253)
# bd$f.41280.0.254 <- as.Date(bd$f.41280.0.254)
# bd$f.41280.0.255 <- as.Date(bd$f.41280.0.255)
# bd$f.41280.0.256 <- as.Date(bd$f.41280.0.256)
# bd$f.41280.0.257 <- as.Date(bd$f.41280.0.257)
# bd$f.41280.0.258 <- as.Date(bd$f.41280.0.258)
# lvl.100011 <- c(0,10,12,24,46,600,1030,3060)
# lbl.100011 <- c("None","Under 10 minutes","1-2 hours","2-4 hours","4-6 hours","6+ hours","10-30 minutes","30-60 minutes")
# bd$f.104900.0.0 <- ordered(bd$f.104900.0.0, levels=lvl.100011, labels=lbl.100011)
# bd$f.104900.1.0 <- ordered(bd$f.104900.1.0, levels=lvl.100011, labels=lbl.100011)
# bd$f.104900.2.0 <- ordered(bd$f.104900.2.0, levels=lvl.100011, labels=lbl.100011)
# bd$f.104900.3.0 <- ordered(bd$f.104900.3.0, levels=lvl.100011, labels=lbl.100011)
# bd$f.104900.4.0 <- ordered(bd$f.104900.4.0, levels=lvl.100011, labels=lbl.100011)
# bd$f.104910.0.0 <- ordered(bd$f.104910.0.0, levels=lvl.100011, labels=lbl.100011)
# bd$f.104910.1.0 <- ordered(bd$f.104910.1.0, levels=lvl.100011, labels=lbl.100011)
# bd$f.104910.2.0 <- ordered(bd$f.104910.2.0, levels=lvl.100011, labels=lbl.100011)
# bd$f.104910.3.0 <- ordered(bd$f.104910.3.0, levels=lvl.100011, labels=lbl.100011)
# bd$f.104910.4.0 <- ordered(bd$f.104910.4.0, levels=lvl.100011, labels=lbl.100011)
# lvl.100012 <- c(0,1,13,35,57,79,912,1200)
# lbl.100012 <- c("None","Under 1 hour","1-3 hours","3-5 hours","5-7 hours","7-9 hours","9-12 hours","12+ hours")
# bd$f.104920.0.0 <- ordered(bd$f.104920.0.0, levels=lvl.100012, labels=lbl.100012)
# bd$f.104920.1.0 <- ordered(bd$f.104920.1.0, levels=lvl.100012, labels=lbl.100012)
# bd$f.104920.2.0 <- ordered(bd$f.104920.2.0, levels=lvl.100012, labels=lbl.100012)
# bd$f.104920.3.0 <- ordered(bd$f.104920.3.0, levels=lvl.100012, labels=lbl.100012)
# bd$f.104920.4.0 <- ordered(bd$f.104920.4.0, levels=lvl.100012, labels=lbl.100012)

raw_ukbb_modrisk <- bd
dim(raw_ukbb_modrisk) #502226, 1189

# The number of inds in basket_1 differs from number of inds in basket_4
# Code block adds 163 inds from OG run to updated basket to keep OR & values the same
old_raw_ukbb_modrisk <- read.table("/Users/youngcd/Library/CloudStorage/Box-Box/Machiela Lab/Projects/UK Biobank/UKBB_ModifiableRisk/ukb.app.92005_young_updated.tab", header=TRUE, sep="\t")
dim(old_raw_ukbb_modrisk) #502389, 739
missing_columns <- setdiff(names(raw_ukbb_modrisk), names(old_raw_ukbb_modrisk))
result <- setdiff(old_raw_ukbb_modrisk$f.eid, raw_ukbb_modrisk$f.eid) #163 
matching_rows <- old_raw_ukbb_modrisk[old_raw_ukbb_modrisk$f.eid %in% result, ]
# Add missing columns to matching_rows filled with NA
matching_rows[, missing_columns] <- NA
# Combine data frames vertically
combined_df <- rbind(raw_ukbb_modrisk, matching_rows)

raw_ukbb_modrisk <- combined_df
dim(raw_ukbb_modrisk) #502389, 1189
###########################################################################################################

# Part 2: Loading all files needed
# Load in input files (SES files from basket 2 in Box)
SES_ukbb_modrisk <- read.table("/Users/youngcd/Library/CloudStorage/Box-Box/Machiela Lab/Projects/UK Biobank/Application_92005/data/original.main.datasets/basket_2/ukb671232.tab", header=TRUE, sep="\t")
chip_92005 <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela Lab/Projects/UK Biobank/Application_92005/data/chip/chip_92005.rds')
# auto, mLOX, mLOY file & all who were genotyped files
mloy.with.eids.for.appl.92005 <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela\ Lab/Projects/UK\ Biobank/Application_92005/data/mca_files/mloy_eids.appl.92005.rds')
mlox.with.eids.for.appl.92005 <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela\ Lab/Projects/UK\ Biobank/Application_92005/data/mca_files/mlox_eids.appl.92005.rds')
auto.with.eids.for.appl.92005 <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela\ Lab/Projects/UK\ Biobank/Application_92005/data/mca_files/auto_eids.appl.92005.rds')
ukb.run.ukb.stats_eids.appl.92005 <- readRDS('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela\ Lab/Projects/UK\ Biobank/Application_92005/data/mca_files/ukb.run.ukb.stats_eids.appl.92005.rds')

### Adding in variables used for adjustments in models ### 
smoking_NFC <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/Input/smoking_NFC.rds")
smoking_25_level <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/Input/smoking_25_level.rds")
raw_ukbb_modrisk$age_squared <- raw_ukbb_modrisk$f.21003.0.0^2
ancestry_92005 <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/Input/ancestry_92005.rds") #only contains % not other info, that is in UKBB_grafpop
bmi <- readRDS("/Users/youngcd/Library/CloudStorage/Box-Box/Coreys_Folder/UKBB_risk_Project2/Rdata/Input/bmi.rds")
###########################################################################################################

# Part 3: Cleaning all input files/data
# Removing chrX from autosomal mCA file & clean the data so that there is only 1 observation per individual, use cleanR to remove prevalent heme cancers
# Cleaning CHIP data
chip_92005$f.eid <- chip_92005$f.eid_92005
diff_between <- setdiff(chip_92005$f.eid, raw_ukbb_modrisk$f.eid) # diff between two cols in regards to f.eids
diff_between #3 (f.eids in col: -180 -133  -69)
new_chip_92005 <- chip_92005[which(chip_92005$f.eid!=diff_between),] # removal of 1st 2 f.eids (-69 is not removed, maybe b/c of space?)
new_chip_92005 <- new_chip_92005[which(new_chip_92005$f.eid!=-69),] # removal of -69
new_chip_92005$chip_type[is.na(new_chip_92005$chip_type)] <- 'wasNA' #wasNA, Some CHIP genes do not belong to any type and therefore are missing this variable (but these are still CHIPs, hence the 'wasNA)
# Merging raw_ukbb, SES & CHIP data
merged_df <- merge(raw_ukbb_modrisk, merge(new_chip_92005, SES_ukbb_modrisk, by = 'f.eid', all = TRUE), by = 'f.eid', all = TRUE)
raw_ukbb_modrisk <- merged_df[match(raw_ukbb_modrisk$f.eid,merged_df$f.eid),] # added ~2k f.eids, so I subset to our raw_ukbb_modrisk
dim(raw_ukbb_modrisk) #502389, 1243

# Checking for number of unquie ids in mlox, mloy, auto & ukb.run files
length(unique(mlox.with.eids.for.appl.92005$eid_appl.92005)) #16454
length(unique(mloy.with.eids.for.appl.92005$eid_appl.92005)) #45407 (high mLOY is most abundant & pop is ~74% is over 50)
length(unique(auto.with.eids.for.appl.92005$eid_appl.92005)) #74575 (too many should only be ~5% of pop)
length(unique(ukb.run.ukb.stats_eids.appl.92005$eid_appl.92005)) # 488377

# Removing chrX from autosomal mCA dataset
library(tidyverse)
table(auto.with.eids.for.appl.92005$type)
table(auto.with.eids.for.appl.92005$chrom)
auto.with.eids.for.appl.92005.ref = auto.with.eids.for.appl.92005
auto.with.eids.for.appl.92005 <- auto.with.eids.for.appl.92005 %>% filter(chrom != "chrX")
dim(auto.with.eids.for.appl.92005) #17865, 24

# How many duplicates for each dataset
subset(mlox.with.eids.for.appl.92005,duplicated(eid_appl.92005)) #0
subset(mloy.with.eids.for.appl.92005,duplicated(eid_appl.92005)) #0
dim(subset(auto.with.eids.for.appl.92005,duplicated(eid_appl.92005))) #2757
subset(ukb.run.ukb.stats_eids.appl.92005,duplicated(eid_appl.92005)) #0

# Selecting 1 eid per individual w/an autosomal mCA (does not matter currently if person has multiple mCAs)
library(tidyverse)
# keeping only the 1st row of duplicates in the df
dim(auto.with.eids.for.appl.92005) #2757 dups & 17865 total
auto.with.eids.for.appl.92005 <- auto.with.eids.for.appl.92005 %>% distinct(eid_appl.92005, .keep_all = TRUE) #keep_all keeps all variables in data
dim(auto.with.eids.for.appl.92005) #15108, 24
dim(raw_ukbb_modrisk) #502389, 1243

# Subsetting only eids that match across all genotyped inds + are in raw ukbb file
raw_ukbb_modrisk <- raw_ukbb_modrisk[na.omit(match(ukb.run.ukb.stats_eids.appl.92005$eid_appl.92005,raw_ukbb_modrisk$f.eid)),]
dim(raw_ukbb_modrisk) #488146, 1243

# Counting mLOX, mLOY and auto mCAs and CHIP
# Adding binary (0/1, no/yes) if person genotpyed has mLOX, mLOY or auto mCA 
raw_ukbb_modrisk$mLOX <- ifelse(raw_ukbb_modrisk$f.eid %in% mlox.with.eids.for.appl.92005$eid_appl.92005, 1, 0)
raw_ukbb_modrisk$mLOY <- ifelse(raw_ukbb_modrisk$f.eid %in% mloy.with.eids.for.appl.92005$eid_appl.92005, 1, 0)
raw_ukbb_modrisk$auto <- ifelse(raw_ukbb_modrisk$f.eid %in% auto.with.eids.for.appl.92005$eid_appl.92005, 1, 0)
# Adding CHIP data (will include chip_type also)
raw_ukbb_modrisk$CHIP <- ifelse(raw_ukbb_modrisk$f.eid %in% new_chip_92005$f.eid, 1, 0)

# Renaming SES (deprivation values)
raw_ukbb_modrisk$Scotland_Index <- raw_ukbb_modrisk$f.26427.0.0
raw_ukbb_modrisk$England_Index <- raw_ukbb_modrisk$f.26410.0.0
raw_ukbb_modrisk$Wales_Index <- raw_ukbb_modrisk$f.26426.0.0

# Comment from abstract to scale indices/index
# scale(): subtracts the mean and divides by the standard deviation
raw_ukbb_modrisk$England_Index_scaled <- scale(raw_ukbb_modrisk$England_Index)
raw_ukbb_modrisk$Scotland_Index_scaled <- scale(raw_ukbb_modrisk$Scotland_Index)
raw_ukbb_modrisk$Wales_Index_scaled <- scale(raw_ukbb_modrisk$Wales_Index)

# Removing UKBB study withdrawls from final file
ukb.withdrawls <- read.table('/Users/youngcd/Library/CloudStorage/Box-Box/Machiela Lab/Projects/UK Biobank/Application_92005/data/withdrawals/ukb.app.92005_withdrawals_2023.04.23.txt', header=TRUE, sep="\t")
# had to find which ids were in raw_ukbb_modrisk manually
# total 38 IDs, 23 IDs in list have already been removed, 15 IDs left
saveRDS(ukb.withdrawls, file = 'ukb.withdrawls.rds') 

raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=1771685,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=1992722,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=2102468,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=2202405,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=2639517,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=2748193,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=3660835,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=3841733,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=3849862,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=4160077,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=4164177,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=4531836,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=4978368,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=5092160,]
raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$f.eid!=5630477,]
#list of all withdrawls = c(1490956, 1635323, 1730716, 1771685, 1904538, 1992722, 2072099, 2102468, 2202405, 2374507, 2467382, 2639517, 2663447, 2748193, 2848196, 2941339, 2998646, 3020909, 3404668, 3600177, 3660835, 3841733, 3849862, 4020864, 4057000, 4160077, 4164177, 4398868, 4466085, 4531836, 4754989, 4978368, 5092160, 5430965, 5500598, 5582950, 5630477, 5666676))
dim(raw_ukbb_modrisk) ##488131, 1253

# Matching ancestry  & smoking files to ids in raw_ukbb_modrisk
ancestry_92005 <- ancestry_92005[na.omit(match(raw_ukbb_modrisk$f.eid,ancestry_92005$f.eid)),]
smoking_25_level <- smoking_25_level[na.omit(match(raw_ukbb_modrisk$f.eid,smoking_25_level$f.eid)),]
smoking_NFC <- smoking_NFC[na.omit(match(raw_ukbb_modrisk$f.eid,smoking_NFC$f.eid)),]

# Merging all 4 dataframes (ancestry) into 1
raw_ukbb_modrisk <- merge(raw_ukbb_modrisk, smoking_25_level, by = "f.eid")
raw_ukbb_modrisk <- merge(raw_ukbb_modrisk, smoking_NFC, by = "f.eid")
raw_ukbb_modrisk <- merge(raw_ukbb_modrisk, ancestry_92005, by = "f.eid")
dim(raw_ukbb_modrisk) #487163, 1259

# Adding in BMI to the dataset
merged_data <- merge(raw_ukbb_modrisk, bmi[, c("f.eid", "f.21001.0.0")], by = "f.eid", all.x = TRUE)
colnames(merged_data)[colnames(merged_data) == "f.21001.0.0"] <- "bmi"
head(merged_data$bmi)
raw_ukbb_modrisk <- merged_data
dim(raw_ukbb_modrisk) #487163, 1260

# Running cleanR to remove prevalent heme cancers (code remove [heme cancers patients w/heme cancers prior to assessment)
# Link: https://htmlpreview.github.io/?https://github.com/machiela-lab/UKBBcleanR/blob/main/vignettes/vignette.html
#install.packages('devtools')
library(devtools)
#devtools::install_github("machiela-lab/UKBBcleanR")
library(UKBBcleanR)
testdata <- as.data.frame(raw_ukbb_modrisk)
cancer_outcome <- c("C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90", "C91", "C92", "C93", "C94", "C95", "C96") # 
prevalent_cancers <- c("C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90", "C91", "C92", "C93", "C94", "C95", "C96") # Prevalent ICD-10 codes to identify - can be blank ex. "c()"
incident_cancers <- c("C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90", "C91", "C92", "C93", "C94", "C95", "C96") # Prevalent ICD-10 codes to identify - can be blank ex. "c()"
test1 <- tte(combined_data = testdata, 
             cancer_of_interest_ICD10 = cancer_outcome, 
             prevalent_cancer_list = prevalent_cancers, # Identify specified prevalent cancers
             prevalent_C_cancers = FALSE, # Include all ICD-10 codes that begin with 'C' to the 'prevalent_cancer_list'
             incident_cancer_list = incident_cancers, # Identify specified incident cancers 
             remove_prevalent_cancer = TRUE, # Do exclude individuals with prevalent cancer(s) or disease(s) 
             remove_self_reported_cancer = FALSE) # Do not exclude individuals with self reported cancer
dim(test1) #485028, 22
table(test1$case_control_cancer_ignore)  # tte outcome ignoring other incident cancers
table(test1$case_control_cancer_control) # tte outcome controlling for other incident cancers
dim(raw_ukbb_modrisk) #487163, 1260
raw_ukbb_modrisk.ref <- raw_ukbb_modrisk[na.omit(match(test1$f.eid,raw_ukbb_modrisk$f.eid)),]
raw_ukbb_modrisk_cleanR <- merge(raw_ukbb_modrisk.ref, test1, by = "f.eid") # contains all info from ukbconv, test1 & user created cols
raw_ukbb_modrisk <- raw_ukbb_modrisk_cleanR
dim(raw_ukbb_modrisk) #485028, 1281
###########################################################################################################

# Part 4: Add in Erikka and Pedro transcribed SAS code/data

# Coding for alcohol
# Adapted from Erikka code in R, ukbconv code to make readable was not used prior to changes
# weekly amount #
if (any(raw_ukbb_modrisk$f.1558.0.0 %in% c(1, 2, 3, -3))) { # expression is considered true in any are present in f.1558.0.0
  # By wrapping the condition with any(), you ensure that the condition is evaluated for the entire vector, and the if statement is triggered if any element of raw_ukbb_modrisk$f.1558.0.0 matches any value in the specified vector.
  # Red wine
  raw_ukbb_modrisk$redwine_wk <- raw_ukbb_modrisk$f.1568.0.0
  raw_ukbb_modrisk$redwine_wk[raw_ukbb_modrisk$f.1568.0.0 %in% c(-1, -3)] <- NA
  # White wine
  raw_ukbb_modrisk$whitewine_wk <- raw_ukbb_modrisk$f.1578.0.0
  raw_ukbb_modrisk$whitewine_wk[raw_ukbb_modrisk$f.1578.0.0 %in% c(-1, -3)] <- NA
  # Beer and cider
  raw_ukbb_modrisk$beer_wk <- raw_ukbb_modrisk$f.1588.0.0
  raw_ukbb_modrisk$beer_wk[raw_ukbb_modrisk$f.1588.0.0 %in% c(-1, -3)] <- NA
  # Spirits
  raw_ukbb_modrisk$spirits_wk <- raw_ukbb_modrisk$f.1598.0.0
  raw_ukbb_modrisk$spirits_wk[raw_ukbb_modrisk$f.1598.0.0 %in% c(-1, -3)] <- NA
  # Fortified wine
  raw_ukbb_modrisk$fortwine_wk <- raw_ukbb_modrisk$f.1608.0.0
  raw_ukbb_modrisk$fortwine_wk[raw_ukbb_modrisk$f.1608.0.0 %in% c(-1, -3)] <- NA
  # Other alcohol
  raw_ukbb_modrisk$otheralc_wk <- raw_ukbb_modrisk$f.5364.0.0
  raw_ukbb_modrisk$otheralc_wk[raw_ukbb_modrisk$f.5364.0.0 %in% c(-1, -3)] <- NA
  # Sum of all alcoholic beverages per week
  raw_ukbb_modrisk$sum_alc_wk <- rowSums(raw_ukbb_modrisk[, c("redwine_wk", "whitewine_wk", "beer_wk", "spirits_wk", "fortwine_wk", "otheralc_wk")], na.rm = TRUE)
  # Sum of all alcoholic beverages per day
  raw_ukbb_modrisk$sum_alc_day <- raw_ukbb_modrisk$sum_alc_wk / 7
}

# av_al_day = categories 1-4 based on average alcohol consumption per day
raw_ukbb_modrisk$av_alc_day <- ifelse(0 <= raw_ukbb_modrisk$sum_alc_day & raw_ukbb_modrisk$sum_alc_day < 1, 1,
                                      ifelse(1 <= raw_ukbb_modrisk$sum_alc_day & raw_ukbb_modrisk$sum_alc_day <= 2, 2,
                                             ifelse(2 < raw_ukbb_modrisk$sum_alc_day & raw_ukbb_modrisk$sum_alc_day <= 3, 3,
                                                    ifelse(3 < raw_ukbb_modrisk$sum_alc_day, 4, NA))))

# creating alc_drinker status variable, that uses 20117 & 1558 outputs to categorize where higher number = more drinking (av_al_day)
raw_ukbb_modrisk$alc_drinker <- ifelse(is.na(raw_ukbb_modrisk$f.20117.0.0) | is.na(raw_ukbb_modrisk$f.1558.0.0), 99,
                                       ifelse(raw_ukbb_modrisk$f.1558.0.0 == -3, 99,
                                              ifelse(raw_ukbb_modrisk$f.20117.0.0 == 2 & is.na(raw_ukbb_modrisk$av_alc_day), 99,
                                                     ifelse(raw_ukbb_modrisk$f.20117.0.0 == 0, 0,
                                                            ifelse(raw_ukbb_modrisk$f.20117.0.0 == 1, 1,
                                                                   ifelse(raw_ukbb_modrisk$f.20117.0.0 == 2 & raw_ukbb_modrisk$f.1558.0.0 %in% c(4, 5), 2,
                                                                          ifelse(raw_ukbb_modrisk$f.20117.0.0 == 2 & raw_ukbb_modrisk$f.1558.0.0 %in% c(1, 2, 3, -3) & raw_ukbb_modrisk$av_alc_day == 1, 2,
                                                                                 ifelse(raw_ukbb_modrisk$f.20117.0.0 == 2 & raw_ukbb_modrisk$f.1558.0.0 %in% c(1, 2, 3, -3) & raw_ukbb_modrisk$av_alc_day == 2, 3,
                                                                                        ifelse(raw_ukbb_modrisk$f.20117.0.0 == 2 & raw_ukbb_modrisk$f.1558.0.0 %in% c(1, 2, 3, -3) & raw_ukbb_modrisk$av_alc_day == 3, 4,
                                                                                               ifelse(raw_ukbb_modrisk$f.20117.0.0 == 2 & raw_ukbb_modrisk$f.1558.0.0 %in% c(1, 2, 3, -3) & raw_ukbb_modrisk$av_alc_day == 4, 5, NA)))))))))) #has 142 NAs (what are you going to do with that)

# Categorical variables 
# First block is based on SAS code but is not included in df
# alc_drinker_miss <- (raw_ukbb_modrisk$alc_drinker == 99)
# alc_drinker_never <- (raw_ukbb_modrisk$alc_drinker == 0)
# alc_drinker_former <- (raw_ukbb_modrisk$alc_drinker == 1)
# alc_drinker_cur1 <- (raw_ukbb_modrisk$alc_drinker == 2)
# alc_drinker_cur2 <- (raw_ukbb_modrisk$alc_drinker == 3)
# alc_drinker_cur3 <- (raw_ukbb_modrisk$alc_drinker == 4)
# alc_drinker_cur4 <- (raw_ukbb_modrisk$alc_drinker == 5)
# inclusion in df
lvl <- c(0,1,2,3,4,5,99)
lbl <- c("never","former","cur1","cur2","cur3","cur4","Missing")
raw_ukbb_modrisk$alc_drinker_cat <- factor(raw_ukbb_modrisk$alc_drinker, levels = lvl, labels = lbl)

# Coding for sleep exposures
# Adapted from Pedro code in R, ukbconv code to make readable was not used prior to changes
raw_ukbb_modrisk$sleepdur <- NA
# Continuous
raw_ukbb_modrisk$sleepdur <- ifelse(raw_ukbb_modrisk$f.1160.0.0 >= 0, raw_ukbb_modrisk$f.1160.0.0, NA)
# Categorical 
raw_ukbb_modrisk$sleep_6 <- ifelse(0 < raw_ukbb_modrisk$f.1160.0.0 & raw_ukbb_modrisk$f.1160.0.0 <= 5, 0, raw_ukbb_modrisk$f.1160.0.0)
raw_ukbb_modrisk$sleep_6 <- ifelse(5 < raw_ukbb_modrisk$f.1160.0.0 & raw_ukbb_modrisk$f.1160.0.0 <= 6, 1, raw_ukbb_modrisk$sleep_6)
raw_ukbb_modrisk$sleep_6 <- ifelse(6 < raw_ukbb_modrisk$f.1160.0.0 & raw_ukbb_modrisk$f.1160.0.0 <= 7, 2, raw_ukbb_modrisk$sleep_6)
raw_ukbb_modrisk$sleep_6 <- ifelse(7 < raw_ukbb_modrisk$f.1160.0.0 & raw_ukbb_modrisk$f.1160.0.0 <= 8, 3, raw_ukbb_modrisk$sleep_6)
raw_ukbb_modrisk$sleep_6 <- ifelse(8 < raw_ukbb_modrisk$f.1160.0.0 & raw_ukbb_modrisk$f.1160.0.0 <= 9, 4, raw_ukbb_modrisk$sleep_6)
raw_ukbb_modrisk$sleep_6 <- ifelse(9 < raw_ukbb_modrisk$f.1160.0.0, 5, raw_ukbb_modrisk$sleep_6)
raw_ukbb_modrisk$sleep_6 <- ifelse(0 > raw_ukbb_modrisk$f.1160.0.0, NA, raw_ukbb_modrisk$sleep_6)
raw_ukbb_modrisk$chronotype <- ifelse(raw_ukbb_modrisk$f.1180.0.0 > 0, raw_ukbb_modrisk$f.1180.0.0, NA)
raw_ukbb_modrisk$insomnia <- ifelse(raw_ukbb_modrisk$f.1200.0.0 >= 0, raw_ukbb_modrisk$f.1200.0.0, NA)
raw_ukbb_modrisk$snoring <- ifelse(raw_ukbb_modrisk$f.1210.0.0 >= 0, raw_ukbb_modrisk$f.1210.0.0, NA)
raw_ukbb_modrisk$narcolepsy <- ifelse(raw_ukbb_modrisk$f.1220.0.0 >= 0, raw_ukbb_modrisk$f.1220.0.0, NA)
raw_ukbb_modrisk$narcolepsy[raw_ukbb_modrisk$narcolepsy==3] <-2 

# Coding for PA #
# UKBB PA vars scored according to IPAQ guidelines
# Moderate PA #
# Frequency of moderate PA per week
raw_ukbb_modrisk$mod_freq_week <- ifelse(raw_ukbb_modrisk$f.884.0.0 < 0 & !is.na(raw_ukbb_modrisk$f.884.0.0), -1, raw_ukbb_modrisk$f.884.0.0)
# Recode frequency to 0 if moderate duration < 10 mins
raw_ukbb_modrisk$mod_freq_week <- ifelse(raw_ukbb_modrisk$f.894.0.0 < 10, 0, raw_ukbb_modrisk$mod_freq_week)
# Duration of moderate PA  per day (min/d)
raw_ukbb_modrisk$mod_dur_day <- ifelse(raw_ukbb_modrisk$f.894.0.0 < 0 & !is.na(raw_ukbb_modrisk$f.894.0.0), -1, raw_ukbb_modrisk$f.894.0.0)
# Truncate to 180 min/day
raw_ukbb_modrisk$mod_dur_day <- ifelse(raw_ukbb_modrisk$f.894.0.0 > 180, 180, raw_ukbb_modrisk$mod_dur_day)
# Recode duration to 0 if moderate duration < 10 min
raw_ukbb_modrisk$mod_dur_day <- ifelse(raw_ukbb_modrisk$f.894.0.0 < 10, 0, raw_ukbb_modrisk$mod_dur_day)
# Recode duration to 0 if frequency < 0
raw_ukbb_modrisk$mod_dur_day <- ifelse(raw_ukbb_modrisk$f.884.0.0 < 0, 0, raw_ukbb_modrisk$mod_dur_day)
# Duration of moderate PA per week (min/wk)
raw_ukbb_modrisk$mod_dur_week <- ifelse(raw_ukbb_modrisk$mod_dur_day >= 0 & raw_ukbb_modrisk$mod_freq_week >= 0, raw_ukbb_modrisk$mod_dur_day * raw_ukbb_modrisk$mod_freq_week, NA) 

# Vigorous PA #
# Frequency of vigorous PA per week
raw_ukbb_modrisk$vig_freq_week <- ifelse(raw_ukbb_modrisk$f.904.0.0 < 0 & !is.na(raw_ukbb_modrisk$f.904.0.0), -1, raw_ukbb_modrisk$f.904.0.0)
# Recode frequency to 0 if moderate duration < 10 mins
raw_ukbb_modrisk$vig_freq_week <- ifelse(raw_ukbb_modrisk$f.914.0.0 < 10, 0, raw_ukbb_modrisk$vig_freq_week)
# Duration of vigorous PA  per day (min/d)
raw_ukbb_modrisk$vig_dur_day <- ifelse(raw_ukbb_modrisk$f.914.0.0 < 0 & !is.na(raw_ukbb_modrisk$f.914.0.0), -1, raw_ukbb_modrisk$f.914.0.0)
# Truncate to 180 min/day
raw_ukbb_modrisk$vig_dur_day <- ifelse(raw_ukbb_modrisk$f.914.0.0 > 180, 180, raw_ukbb_modrisk$vig_dur_day)
# Recode duration to 0 if moderate duration < 10 min
raw_ukbb_modrisk$vig_dur_day <- ifelse(raw_ukbb_modrisk$f.914.0.0 < 10, 0, raw_ukbb_modrisk$vig_dur_day)
# Recode duration to 0 if frequency < 0
raw_ukbb_modrisk$vig_dur_day <- ifelse(raw_ukbb_modrisk$f.904.0.0 < 0, 0, raw_ukbb_modrisk$vig_dur_day)
# Duration of vigorous PA per week (min/wk)
raw_ukbb_modrisk$vig_dur_week <- ifelse(raw_ukbb_modrisk$vig_dur_day >= 0 & raw_ukbb_modrisk$vig_freq_week >= 0, raw_ukbb_modrisk$vig_dur_day * raw_ukbb_modrisk$vig_freq_week, NA)

# Walking PA #
# Frequency of walks per week
raw_ukbb_modrisk$walk_freq_week <- ifelse(raw_ukbb_modrisk$f.864.0.0 < 0 & !is.na(raw_ukbb_modrisk$f.864.0.0), -1, raw_ukbb_modrisk$f.864.0.0)
# Recode frequency to 0 if walking duration < 10 min
raw_ukbb_modrisk$walk_freq_week <- ifelse(raw_ukbb_modrisk$f.874.0.0 < 10, 0, raw_ukbb_modrisk$walk_freq_week)
# Duration of walking minutes per week
raw_ukbb_modrisk$walk_dur_day <- ifelse(raw_ukbb_modrisk$f.874.0.0 < 0 & !is.na(raw_ukbb_modrisk$f.874.0.0), -1, raw_ukbb_modrisk$f.874.0.0)
# Truncate to 180 min/day
raw_ukbb_modrisk$walk_dur_day <- ifelse(raw_ukbb_modrisk$f.874.0.0 > 180, 180, raw_ukbb_modrisk$walk_dur_day)
# Recode duration to 0 if walking duration < 10 min
raw_ukbb_modrisk$walk_dur_day <- ifelse(raw_ukbb_modrisk$f.874.0.0 < 10, 0, raw_ukbb_modrisk$walk_dur_day)
# Recode duration to 0 if frequency < 0
raw_ukbb_modrisk$walk_dur_day <- ifelse(raw_ukbb_modrisk$f.864.0.0 < 0, 0, raw_ukbb_modrisk$walk_dur_day)
# Duration of walking per week (min/wk)
raw_ukbb_modrisk$walk_dur_week <- ifelse(raw_ukbb_modrisk$walk_dur_day >= 0 & raw_ukbb_modrisk$walk_freq_week >= 0, raw_ukbb_modrisk$walk_dur_day * raw_ukbb_modrisk$walk_freq_week, NA)
# Duration of walking per week (min/wk)

# PA MET-hrs #
raw_ukbb_modrisk$mod_METmin_week <- raw_ukbb_modrisk$mod_dur_week*4.0
raw_ukbb_modrisk$vig_METmin_week <- raw_ukbb_modrisk$vig_dur_week*8.0
raw_ukbb_modrisk$walk_METmin_week<- raw_ukbb_modrisk$walk_dur_week*3.3
# summary variables
raw_ukbb_modrisk$tot_METmin_week <- rowSums(raw_ukbb_modrisk[, c('mod_METmin_week', 'vig_METmin_week', 'walk_METmin_week')], na.rm = TRUE)
raw_ukbb_modrisk$tot_dur_week <- rowSums(raw_ukbb_modrisk[, c('mod_dur_week', 'vig_dur_week', 'walk_dur_week')], na.rm = TRUE)
raw_ukbb_modrisk$tot_dur_day <- ifelse(raw_ukbb_modrisk$mod_dur_day >= 0 & raw_ukbb_modrisk$vig_dur_day >= 0 & raw_ukbb_modrisk$walk_dur_day >= 0, rowSums(raw_ukbb_modrisk[, c('mod_dur_day', 'vig_dur_day', 'walk_dur_day')], na.rm = TRUE), NA) # check how many NAs
# Subset the data frame to exclude rows where TOT_DUR_DAY is greater than to 960
unique_values <- sort(unique(raw_ukbb_modrisk$tot_dur_day)) # values greater than 960 so next line is commented out
#raw_ukbb_modrisk <- raw_ukbb_modrisk[raw_ukbb_modrisk$tot_dur_day >= 960,] # exclude maximum outliers, assuming slept for 8 hours, code removes anyone over 960 in the tot_dur_day col
raw_ukbb_modrisk$tot_freq_week <- ifelse(raw_ukbb_modrisk$mod_freq_week >= 0 & raw_ukbb_modrisk$vig_freq_week >= 0 & raw_ukbb_modrisk$walk_freq_week >= 0, rowSums(raw_ukbb_modrisk[, c('mod_dur_day', 'vig_dur_day', 'walk_dur_day')], na.rm = TRUE), NA) # check how many NAs

# Categorical 
raw_ukbb_modrisk$IPAQ_CAT <- 1  # LOW
# Categorical scoring conditions
raw_ukbb_modrisk$IPAQ_CAT[raw_ukbb_modrisk$vig_freq_week >= 3 & raw_ukbb_modrisk$vig_dur_day >= 20] <- 2  # MODERATE
raw_ukbb_modrisk$IPAQ_CAT[raw_ukbb_modrisk$mod_freq_week >= 5 & raw_ukbb_modrisk$vig_dur_day >= 30] <- 2  # MODERATE
raw_ukbb_modrisk$IPAQ_CAT[raw_ukbb_modrisk$tot_freq_week >= 5 & raw_ukbb_modrisk$tot_METmin_week >= 600] <- 2  # MODERATE
raw_ukbb_modrisk$IPAQ_CAT[raw_ukbb_modrisk$vig_freq_week >= 3 & raw_ukbb_modrisk$tot_METmin_week >= 1500] <- 3  # HIGH
raw_ukbb_modrisk$IPAQ_CAT[raw_ukbb_modrisk$tot_freq_week >= 7 & raw_ukbb_modrisk$tot_METmin_week >= 3000] <- 3  # HIGH
###########################################################################################################

# Part 5: Naming and editing to night-shift variables
# Naming
raw_ukbb_modrisk$consec_night_shifts_mixed <- raw_ukbb_modrisk$f.22644.0.0 #22644:	Consecutive night shifts during mixed shift periods	Employment history  
raw_ukbb_modrisk$consec_night_shifts_night <- raw_ukbb_modrisk$f.22654.0.0 #22654:	Consecutive night shifts during night shift periods	Employment history  
raw_ukbb_modrisk$job_night_shift <- raw_ukbb_modrisk$f.3426.0.0 #3426: Job involves night shift work	Employment  
raw_ukbb_modrisk$job_shift_work <- raw_ukbb_modrisk$f.826.0.0 #826: Job involves shift work	Employment  
raw_ukbb_modrisk$num_night_shifts_monthly_mixed <- raw_ukbb_modrisk$f.22643.0.0 #22643:	Number of night shifts worked monthly during mixed shift periods	Employment history  
raw_ukbb_modrisk$num_night_shifts_monthly_night <- raw_ukbb_modrisk$f.22653.0.0 #22653:	Number of night shifts worked monthly during night shift periods	Employment history  
raw_ukbb_modrisk$rest_days_mixed <- raw_ukbb_modrisk$f.22645.0.0 #22645:	Rest days during mixed shift periods	Employment history  
raw_ukbb_modrisk$rest_days_night <- raw_ukbb_modrisk$f.22655.0.0 #22655:	Rest days during night shift periods	Employment history  
raw_ukbb_modrisk$len_night_shift_mixed <- raw_ukbb_modrisk$f.22642.0.0 #22642:	Usual length of each night shift during mixed shift periods	Employment history  
raw_ukbb_modrisk$len_night_shift_night <- raw_ukbb_modrisk$f.22652.0.0 #22652:	Usual length of each night shift during night shift periods	Employment history  
raw_ukbb_modrisk$job_shift_worked <- raw_ukbb_modrisk$f.22620.0.0 #22620:	Job involved shift work	Employment history  
raw_ukbb_modrisk$day_shifts_worked <- raw_ukbb_modrisk$f.22630.0.0 #22630:	Day shifts worked	Employment history  
raw_ukbb_modrisk$mix_day_night_shifts <- raw_ukbb_modrisk$f.22640.0.0 #22640:	Mixture of day and night shifts worked	Employment history  
raw_ukbb_modrisk$night_shifts_worked <- raw_ukbb_modrisk$f.22650.0.0 #22650:	Night shifts worked	Employment history  
raw_ukbb_modrisk$period_day_shifts <- raw_ukbb_modrisk$f.22631.0.0 #22631:	Period spent working day shifts	Employment history  
raw_ukbb_modrisk$period_mix_day_night_shifts <- raw_ukbb_modrisk$f.22641.0.0 #22641:	Period spent working mix of day and night shifts	Employment history  
raw_ukbb_modrisk$period_night_shifts <- raw_ukbb_modrisk$f.22651.0.0 #22651: Period spent working night shifts	Employment history  

# Editing
raw_ukbb_modrisk$job_night_shift <- ifelse(raw_ukbb_modrisk$job_night_shift >= 0, raw_ukbb_modrisk$job_night_shift, NA)
raw_ukbb_modrisk$job_shift_work <- ifelse(raw_ukbb_modrisk$job_shift_work >= 0, raw_ukbb_modrisk$job_shift_work, NA)
raw_ukbb_modrisk$job_shift_worked <- ifelse(raw_ukbb_modrisk$job_shift_worked >= 0, raw_ukbb_modrisk$job_shift_worked, NA)
###########################################################################################################

# Part 6: Additional categories to make data more human readable, models & to categorize variables for table 1
raw_ukbb_modrisk$ageatassess <- raw_ukbb_modrisk$f.21003.0.0
raw_ukbb_modrisk$geneticsex <- raw_ukbb_modrisk$f.22001.0.0
# developing age_cat so gtsummary details age in 10 year categories
raw_ukbb_modrisk$age_cat <- cut(raw_ukbb_modrisk$f.21003.0.0, c(0, 40, 50, 60, 70, 80, 125), labels = c('≤40', '41-50', '51-60', '61-70', '71-80', '≥81'))
# developing packyear_cat so gtsummary details age in 10 year categories
raw_ukbb_modrisk$packyear_cat <- cut(raw_ukbb_modrisk$f.20161.0.0, c(0, 10, 20, 30, 40, 200), labels = c('≤10', '>10-20', '>20-30', '>30-40', '>40'))
raw_ukbb_modrisk$smokestopage_cat <- cut(raw_ukbb_modrisk$f.22507.0.0, c(0, 40, 50, 60, 70, 80, 125), labels = c('≤40', '41-50', '51-60', '61-70', '71-80', '≥81')) # could edit age dist
raw_ukbb_modrisk$mCA_value <- raw_ukbb_modrisk$mLOX+raw_ukbb_modrisk$mLOY+raw_ukbb_modrisk$auto # col adding the instances of mCAs, if 0 then person has no mCAs
raw_ukbb_modrisk$CH_value <- raw_ukbb_modrisk$mLOX+raw_ukbb_modrisk$mLOY+raw_ukbb_modrisk$auto+raw_ukbb_modrisk$CHIP # col adding the instances of mCAs or CHIP, if 0 then person has no mCAs or CHIP
raw_ukbb_modrisk$bmi.cat <- cut(raw_ukbb_modrisk$bmi, c(0, 18.5, 25, 30, 40, 125), labels = c("<18.5","18.5 - <25","25 - <30","30 - <40","≥ 40")) # Update the bmi variable into for models
###########################################################################################################

# Part 7: Checking number of NAs in each column & Aggregating NA's into 'missing' category
# Check the number of NA values in each column
na_count <-sapply(raw_ukbb_modrisk, function(y) sum(is.na(y)))
na_count  # gives list of NAs in each column

# Subseting just for cols needed for models 
ukbb_final_file <- raw_ukbb_modrisk[,c(1, 1190, 1216, 1244:1260, 1282:1340)]
colnames(ukbb_final_file)
###########################################################################################################

# Part 8: Setting reference categories for variables in models
# Cols that are factors
ukbb_final_file$sleep_6 <- factor(ukbb_final_file$sleep_6, ordered = FALSE)
ukbb_final_file$chronotype <- factor(ukbb_final_file$chronotype, ordered = FALSE)
ukbb_final_file$insomnia <- factor(ukbb_final_file$insomnia, ordered = FALSE)
ukbb_final_file$snoring <- factor(ukbb_final_file$snoring, ordered = FALSE)
ukbb_final_file$narcolepsy <- factor(ukbb_final_file$narcolepsy, ordered = FALSE)
ukbb_final_file$age_cat <- factor(ukbb_final_file$age_cat, ordered = TRUE)
ukbb_final_file$packyear_cat <- factor(ukbb_final_file$packyear_cat, ordered = TRUE)
ukbb_final_file$smokestopage_cat <- factor(ukbb_final_file$smokestopage_cat, ordered = TRUE)
ukbb_final_file$av_alc_day_cat <- ukbb_final_file$av_alc_day
lvl.1 <- c(1,2,3,4)
lbl.1 <- c("avg1","avg2","avg3","avg4")
ukbb_final_file$av_alc_day_cat <- factor(ukbb_final_file$av_alc_day_cat, levels = lvl.1, labels = lbl.1)
ukbb_final_file$IPAQ_CAT <- factor(ukbb_final_file$IPAQ_CAT, ordered = FALSE)
ukbb_final_file$job_night_shift <- factor(ukbb_final_file$job_night_shift, ordered = FALSE)
ukbb_final_file$job_shift_work <- factor(ukbb_final_file$job_shift_work, ordered = FALSE)
ukbb_final_file$job_shift_worked <- factor(ukbb_final_file$job_shift_worked, ordered = FALSE)
lvl.1 <- c("<18.5","18.5 - <25","25 - <30","30 - <40","≥ 40") # Setting reference to 18.5 - 24.9 category
ukbb_final_file$bmi.cat <- factor(ukbb_final_file$bmi.cat, levels = lvl.1)
ukbb_final_file$bmi.cat <- factor(ukbb_final_file$bmi.cat, ordered = FALSE)

# Setting up those ref categories
# ukbb_final_file$sleepdur <- relevel(ukbb_final_file$sleepdur, ref = 8)
ukbb_final_file$sleep_6 <-relevel(ukbb_final_file$sleep_6, ref = 5) #sleepdur categorical, selects the 5 object in table not actually number 5 (actually refers to the 1st level of the factor variable, and changes the order of the factors to but '5' first when you check levels())
ukbb_final_file$chronotype <- relevel(ukbb_final_file$chronotype, ref = 1)
ukbb_final_file$insomnia <- relevel(ukbb_final_file$insomnia, ref = 1)
ukbb_final_file$snoring <- relevel(ukbb_final_file$snoring, ref = 2)
#ukbb_final_file$narcolepsy <- relevel(ukbb_final_file$narcolepsy, ref = 0)
ukbb_final_file$alc_drinker_cat <- relevel(ukbb_final_file$alc_drinker_cat, ref = 'cur1')
ukbb_final_file$IPAQ_CAT <- relevel(ukbb_final_file$IPAQ_CAT, ref = 1)
ukbb_final_file$job_night_shift <- relevel(ukbb_final_file$job_night_shift, ref = 1)
ukbb_final_file$job_shift_work <- relevel(ukbb_final_file$job_shift_work, ref = 1)
ukbb_final_file$bmi.cat <-relevel(ukbb_final_file$bmi.cat, ref = 2)
###########################################################################################################

# Part 9: Doing sanity checks and checking data types
# Checking data typeof, nlevels & is.factor of each column 
data_types <- sapply(ukbb_final_file, typeof)
factors <- sapply(ukbb_final_file, nlevels)
ask_fact <- sapply(ukbb_final_file, is.factor)
data_types
factors
ask_fact
###########################################################################################################

# Part 10: Saving final file
dim(ukbb_final_file) #484893, 80
dim(raw_ukbb_modrisk) #484893, 1340
saveRDS(ukbb_final_file, file = 'ukbb_final_file.rds')
saveRDS(raw_ukbb_modrisk, file = 'raw_ukbb_modrisk.rds')
saveRDS(bd.orginal, file = 'bd.orginal.rds')
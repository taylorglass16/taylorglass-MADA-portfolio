####################################
# Data cleaning script 
# Cleans and merges data 
# Data from UHC flu season of 2016-2017
# Last edit 12-5-2019
# Author: Brian McKay
####################################

# Clean up enviroment
rm(list=ls())
if(!is.null(names(sessionInfo()$otherPkgs))){
  lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)}

# loading required packages
if (require('tidyverse')==FALSE) {install.packages('tidyverse', repos="https://cran.rstudio.com"); require(tidyverse);}
if (require('DescTools')==FALSE) {install.packages('DescTools', repos="https://cran.rstudio.com"); require(DescTools)}

# Don't need the bookdown package now but is required for the included Markdown files so we will just install it now.
if (require('bookdown')==FALSE) {install.packages('bookdown', repos="https://cran.rstudio.com"); require(bookdown);}

# Load Data Merged Data Set  
#####
dat<-readRDS("1 Anonymized Data/Data.Rda")

# This analysis requires data from two data sets
# The "fieldnotes.csv" dataset contains the variables collected at the time of registration
# This includes self reported activity level and symptoms
# The "temps.csv" has the temp of the pateint recorded at the clinic 
# This is generally data from the day after self-reported data is collected
# They have already been merged into the DatRaw.Rda file so now I will select the observations and variables I need


###########################
# CLEANING UP VARIABLES
dat.2<-dat

# making a factor version of the activity levels varaible
dat.2$PtQ.URI.24ActivityImpactF<-as.factor(as.character(dat.2$PtQ.URI.24ActivityImpact))

summary(dat.2$PtQ.URI.24ActivityImpactF)

# fixing the order of the factor levels
dat.2$PtQ.URI.24ActivityImpactF<-factor(dat.2$PtQ.URI.24ActivityImpactF, levels =c("0","1","2","3","4","5","6","7","8","9","10"))


# need to create the right order for factor varables 

# Myalgia
summary(dat.2$PtQ.URI.Intensity.Myalgia)
dat.2$PtQ.URI.Intensity.Myalgia<-factor(dat.2$PtQ.URI.Intensity.Myalgia, levels =c("None","Mild","Moderate","Severe"))

# Asthenia
summary(dat.2$PtQ.URI.Intensity.Asthenia)
dat.2$PtQ.URI.Intensity.Asthenia<-factor(dat.2$PtQ.URI.Intensity.Asthenia, levels =c("None","Mild","Moderate","Severe")) 

# Cough
summary(dat.2$PtQ.URI.Intensity.Cough)
dat.2$PtQ.URI.Intensity.Cough<-factor(dat.2$PtQ.URI.Intensity.Cough, levels =c("None","Mild","Moderate","Severe")) 



# Changing variable names to make them more clear 
dat.3<-dat.2

# Activity level numeric
dat.3$ActivityLevel<-dat.3$PtQ.URI.24ActivityImpact
dat.3$PtQ.URI.24ActivityImpact<-NULL

# Activity level factor
dat.3$ActivityLevelF<-dat.3$PtQ.URI.24ActivityImpactF
dat.3$PtQ.URI.24ActivityImpactF<-NULL


# Adenopathy or swollen lymph nodes
dat.3$SwollenLymphNodes<-dat.3$PtQ.URI.Adenopathy
dat.3$PtQ.URI.Adenopathy<-NULL

# Flu shot self reported by students 
dat.3$FluShot<-dat.3$PtQ.URI.AnnualFluShotPtEntered
dat.3$PtQ.URI.AnnualFluShotPtEntered<-NULL

# Chest cogestion
dat.3$ChestCongestion<-dat.3$PtQ.URI.ChestCongestion
dat.3$PtQ.URI.ChestCongestion<-NULL

# Chills and sweats
dat.3$ChillsSweats<-dat.3$PtQ.URI.ChillsSweats
dat.3$PtQ.URI.ChillsSweats<-NULL

# Nasal congestion 
dat.3$NasalCongestion<-dat.3$PtQ.URI.Congested
dat.3$PtQ.URI.Congested<-NULL

# Cough
dat.3$CoughYN<-dat.3$PtQ.URI.Cough
dat.3$PtQ.URI.Cough<-NULL

# Sneeze
dat.3$Sneeze<-dat.3$PtQ.URI.Sneeze
dat.3$PtQ.URI.Sneeze<-NULL

# Duration of illness 
dat.3$DurationOfIllness<-dat.3$PtQ.URI.DaysDuration
dat.3$PtQ.URI.DaysDuration<-NULL

# Fatigue
dat.3$Fatigue<-dat.3$PtQ.URI.FAtigue
dat.3$PtQ.URI.FAtigue<-NULL

# Subjective Fever (patient asked if they feel like they have a fever)
dat.3$SubjectiveFever<-dat.3$PtQ.URI.Fever
dat.3$PtQ.URI.Fever<-NULL

# HA is Head ache 
dat.3$Headache<-dat.3$PtQ.URI.HA
dat.3$PtQ.URI.HA<-NULL

# Asthenia (Weakness or lack of energy)
dat.3$Weakness<-dat.3$PtQ.URI.Intensity.Asthenia
dat.3$PtQ.URI.Intensity.Asthenia<-NULL
# also making a yes no version 
dat.3$WeaknessYN<-plyr::revalue(dat.3$Weakness, c("None"="No", "Mild"="Yes", "Moderate"="Yes", "Severe"="Yes"))

# Cough intensity
dat.3$CoughIntensity<-dat.3$PtQ.URI.Intensity.Cough
dat.3$PtQ.URI.Intensity.Cough<-NULL
# also making a yes no version 
dat.3$CoughYN2 <-plyr::revalue(dat.3$CoughIntensity, c("None"="No", "Mild"="Yes", "Moderate"="Yes", "Severe"="Yes")) 

# Myalgia or body pain 
dat.3$Myalgia<-dat.3$PtQ.URI.Intensity.Myalgia
dat.3$PtQ.URI.Intensity.Myalgia<-NULL
# also making a yes no version 
dat.3$MyalgiaYN<-plyr::revalue(dat.3$Myalgia, c("None"="No", "Mild"="Yes", "Moderate"="Yes", "Severe"="Yes"))

# Rhinorrhea or Runny nose
dat.3$RunnyNose<-dat.3$PtQ.URI.Rhinorrhea
dat.3$PtQ.URI.Rhinorrhea<-NULL

# AbdPain is abdominal pain 
dat.3$AbPain<-dat.3$PtQ.URI.AbdPain
dat.3$PtQ.URI.AbdPain<-NULL

# Chest pain is pain in the chest
dat.3$ChestPain<-dat.3$PtQ.URI.ChestPain
dat.3$PtQ.URI.ChestPain<-NULL

# Diarrhea
dat.3$Diarrhea<-dat.3$PtQ.URI.Diarrhea
dat.3$PtQ.URI.Diarrhea<-NULL

# EyePn is Eye pain 
dat.3$EyePn<-dat.3$PtQ.URI.EyePn
dat.3$PtQ.URI.EyePn<-NULL

# Insomnia is the can't sleep 
dat.3$Insomnia<-dat.3$PtQ.URI.Insomnia
dat.3$PtQ.URI.Insomnia<-NULL

# Itchy Eyes
dat.3$ItchyEye<-dat.3$PtQ.URI.ItchyEye
dat.3$PtQ.URI.ItchyEye<-NULL

# Nausea feeling like you are going to be sick 
dat.3$Nausea<-dat.3$PtQ.URI.Nausea
dat.3$PtQ.URI.Nausea<-NULL

# Otalgia is ear pain 
dat.3$EarPn<-dat.3$PtQ.URI.Otalgia
dat.3$PtQ.URI.Otalgia<-NULL

# CantHear is hearing loss
dat.3$Hearing<-dat.3$PtQ.URI.CantHear
dat.3$PtQ.URI.CantHear<-NULL

# Pharyngitis is throat pain 
dat.3$Pharyngitis<-dat.3$PtQ.URI.Pharyngitis
dat.3$PtQ.URI.Pharyngitis<-NULL

# SOB is shortness of breath
dat.3$Breathless<-dat.3$PtQ.URI.SOB
dat.3$PtQ.URI.SOB<-NULL

# Toothpn is tooth pain 
dat.3$ToothPn<-dat.3$PtQ.URI.ToothPn
dat.3$PtQ.URI.ToothPn<-NULL

# Vision is blurred vision 
dat.3$Vision<-dat.3$PtQ.URI.Vision
dat.3$PtQ.URI.Vision<-NULL

# Vomit
dat.3$Vomit<-dat.3$PtQ.URI.Vomit
dat.3$PtQ.URI.Vomit<-NULL

# Wheezing 
dat.3$Wheeze<-dat.3$PtQ.URI.Wheeze
dat.3$PtQ.URI.Wheeze<-NULL

# temp taken at by nurse at office 
dat.3$BodyTemp<-dat.3$temp1
dat.3$temp1<-NULL

# Rapid antigen flu A test 
dat.3$RapidFluA<-dat.3$FLU.A.ANTIGEN
dat.3$FLU.A.ANTIGEN<-NULL

# Rapid antigen flu B test
dat.3$RapidFluB<-dat.3$FLU.B.ANTIGEN
dat.3$FLU.B.ANTIGEN<-NULL

# Rapid PCR flu A test
dat.3$PCRFluA<-dat.3$Flu.A.RNA.PCR
dat.3$Flu.A.RNA.PCR<-NULL

# Rapid PCR flu B test
dat.3$PCRFluB<-dat.3$Flu.B.RNA.PCR
dat.3$Flu.B.RNA.PCR<-NULL


#--------------------------------------------------------------------------
# Create numeric versions of the transmission variables
#--------------------------------------------------------------------------


dat.3 <- mutate(dat.3,
                SneezeScore= if_else(Sneeze=="Yes",1,0), 
                RunnyNoseScore= if_else(RunnyNose=="Yes",1,0),
                CoughYNScore= if_else(CoughYN=="Yes",1,0),
                CoughYN2Score= if_else(CoughYN2=="Yes",1,0),
                ChestCongestionScore= if_else(ChestCongestion=="Yes",1,0),
                NasalCongestionScore= if_else(NasalCongestion=="Yes",1,0))


#--------------------------------------------------------------------------
# Create logical versions of the morbidity variables
#--------------------------------------------------------------------------


dat.3$hasSubjectiveFever<-dat.3$SubjectiveFever=="Yes"
dat.3$hasChillsSweats<-dat.3$ChillsSweats=="Yes"
dat.3$hasMyalgiaYN<-dat.3$MyalgiaYN=="Yes"
dat.3$hasWeaknessYN<-dat.3$WeaknessYN=="Yes"
dat.3$hasHeadache<-dat.3$Headache=="Yes"
dat.3$hasFatigue<-dat.3$Fatigue=="Yes"
dat.3$hasInsomnia<-dat.3$Insomnia =="Yes"
dat.3$hasBreathless<-dat.3$Breathless =="Yes"
dat.3$hasWheeze<-dat.3$Wheeze =="Yes"
dat.3$hasChestPain<-dat.3$ChestPain =="Yes"
dat.3$hasPharyngitis<-dat.3$Pharyngitis =="Yes"
dat.3$hasAbPain<-dat.3$AbPain =="Yes"
dat.3$hasDiarrhea<-dat.3$Diarrhea =="Yes"
dat.3$hasNausea<-dat.3$Nausea =="Yes"
dat.3$hasVomit<-dat.3$Vomit =="Yes"
dat.3$hasEarPn<-dat.3$EarPn =="Yes"
dat.3$hasToothPn<-dat.3$ToothPn =="Yes"
dat.3$hasEyePn<-dat.3$EyePn =="Yes"
dat.3$hasItchyEye<-dat.3$ItchyEye =="Yes"
dat.3$hasSwollenLymphNodes<-dat.3$SwollenLymphNodes=="Yes"


######

# dat.3 contains all 2380 students who reported a self-rated activity level 
# Rename data to something meaningful

SympAct<-dat.3

#-------------------------------------------------------------
# Calculating Infection score for main text 
#-------------------------------------------------------------

# Score 1: All Y/N variables included with original cough y/n 
SympAct <- mutate(SympAct,
                  TransScore1 = CoughYNScore + SneezeScore + RunnyNoseScore + NasalCongestionScore + ChestCongestionScore)

# keeping one as numeric creating new one that is a factor
SympAct$TransScore1F<-as.factor(SympAct$TransScore1)


#-------------------------------------------------------------
# Calculating Infection scores for SM
#-------------------------------------------------------------

# Score 2: All Y/N variables included with original cough y/n excluding chest congestion
SympAct <- mutate(SympAct,
                  TransScore2 = CoughYNScore + SneezeScore + RunnyNoseScore + NasalCongestionScore)

# keeping one as numeric creating new one that is a factor
SympAct$TransScore2F<-as.factor(SympAct$TransScore2)


# Score 3: All Y/N variables included with original cough y/n excluding chest and nasal congestion
SympAct<- mutate(SympAct,
                 TransScore3 = CoughYNScore + SneezeScore + RunnyNoseScore)

# keeping one as numeric creating new one that is a factor
SympAct$TransScore3F<-as.factor(SympAct$TransScore3)

# Score 4: All Y/N variables included to start but then use correlation cut off see function at the end of script
# at both 0.9 and 0.75 the same variables are included in the score
SympAct<- mutate(SympAct,
                 TransScore4 = SneezeScore + RunnyNoseScore + NasalCongestionScore + ChestCongestionScore)

# keeping one as numeric creating new one that is a factor
SympAct$TransScore4F<-as.factor(SympAct$TransScore4)


#-------------------------------------------------------------
# Calculating morbidity scores for main text
#-------------------------------------------------------------

SympAct <- mutate(SympAct, ImpactScore = hasSubjectiveFever+hasChillsSweats+
                    hasMyalgiaYN+hasWeaknessYN+hasHeadache+hasFatigue+hasInsomnia+
                    hasBreathless+hasWheeze+hasChestPain+hasPharyngitis+hasAbPain+
                    hasDiarrhea+hasNausea+hasVomit+hasEarPn+hasToothPn+hasEyePn+
                    hasItchyEye+hasSwollenLymphNodes)

SympAct <- mutate(SympAct, ImpactScore2 = hasSubjectiveFever+hasChillsSweats+
                    hasMyalgiaYN+hasHeadache+hasFatigue+hasInsomnia+
                    hasBreathless+hasWheeze+hasChestPain+hasPharyngitis+hasAbPain+
                    hasDiarrhea+hasNausea+hasVomit+hasEarPn+hasToothPn+hasEyePn+
                    hasItchyEye+hasSwollenLymphNodes)

SympAct <- mutate(SympAct, ImpactScore3 = hasSubjectiveFever+hasMyalgiaYN+
                    hasInsomnia+hasBreathless+hasWheeze+hasChestPain+hasAbPain+
                    hasDiarrhea+hasNausea+hasEarPn+hasToothPn+hasEyePn+
                    hasItchyEye+hasSwollenLymphNodes)



# keeping one as numeric creating new one that is a factor for each score
SympAct$ImpactScoreF<-as.factor(SympAct$ImpactScore)
SympAct$ImpactScore2F<-as.factor(SympAct$ImpactScore2)
SympAct$ImpactScore3F<-as.factor(SympAct$ImpactScore3)
# Need to add empty factor level since the possible score is 0-20 but no one has a score of 19,20
SympAct$ImpactScoreF <- factor(SympAct$ImpactScoreF, levels = c("0","1",levels(SympAct$ImpactScoreF),"19","20"))
# Need to add empty factor level since the possible score is 0-19 but no one has a score of 0,1,19
SympAct$ImpactScore2F <- factor(SympAct$ImpactScore2F, levels = c("0","1",levels(SympAct$ImpactScore2F), "19"))
# Need to add empty factor level since the possible score is 0-19 but no one has a score of 19
SympAct$ImpactScore3F <- factor(SympAct$ImpactScore3F, levels = c(levels(SympAct$ImpactScore3F), "14"))



#--------------------------
# Nice labels for variables
#--------------------------

#Labels for variables with better names for the final table and figures
var_Label_List<-list(CoughYN = "Cough",
                     CoughIntensity = "Cough Severity",
                     Sneeze = "Sneeze",
                     RunnyNose ="Runny Nose",
                     SubjectiveFever = "Subjective Fever",
                     ChillsSweats = "Chills/Sweats",
                     Fatigue = "Fatigue",
                     Headache = "Headache",
                     WeaknessYN = "Weakness",
                     MyalgiaYN = "Myalgia",
                     ActivityLevel = "Activity Level",
                     SwollenLymphNodes = "Swollen Lymph Nodes",
                     AbPain = "Abdominal Pain",
                     ChestPain = "Chest Pain",
                     ChestCongestion = "Chest Congestion",
                     NasalCongestion = "Nasal Congestion",
                     EyePn = "Eye Pain",
                     Insomnia = "Sleeplessness",
                     ItchyEye = "Itchy Eyes",
                     EarPn = "Ear Pain",
                     Hearing = "Loss of Hearing",
                     Pharyngitis = "Sore Throat",
                     Breathless = "Breathlessness",
                     ToothPn = "Tooth Pain",
                     Vision = "Blurred Vision",
                     Vomit = "Vomiting",
                     Wheeze = "Wheezing",
                     TransScore1F = "Infectiousness Score",
                     TransScore2F = "Infectiousness Score",
                     TransScore3F = "Infectiousness Score",
                     ImpactScoreF = "Morbidity Score",
                     ImpactScore2F = "Morbidity Score",
                     ImpactScore3F = "Morbidity Score")

labelled::var_label(SympAct)<-var_Label_List

#str(SympAct)



#-------------------------------------
# Data stratified by diagnosis type
#-------------------------------------

## This dataset contians all the possible diagnosis (PCR, Rapid antigen, or clinical)
SympAct_Any_Pos<-SympAct

## This dataset contains all the individuals with a lab test confirmed for FLU
SympAct_Lab_Pos<-filter(SympAct,RapidFluA %in% c("Positive for Influenza A") |  RapidFluB %in% c("Positive for Influenza B")
                        | PCRFluA %in% c(" Influenza A Detected") |  PCRFluB %in% c(" Influenza B Detected"))


# for what ever reason variable labels are not making it throught the subsetting process 
labelled::var_label(SympAct_Lab_Pos)<-var_Label_List

# need to drop empty levels for the analysis in the data subset
SympAct_Lab_Pos$ImpactScoreFD<-droplevels(SympAct_Lab_Pos$ImpactScoreF)
SympAct_Lab_Pos$ImpactScore2FD<-droplevels(SympAct_Lab_Pos$ImpactScore2F)
SympAct_Lab_Pos$ImpactScore3FD<-droplevels(SympAct_Lab_Pos$ImpactScore3F)

SympAct_Any_Pos$ImpactScoreFD<-droplevels(SympAct_Any_Pos$ImpactScoreF)


# Check for symptoms with a prevalance of 5% or less
summary(SympAct_Any_Pos)# Hearing, Vision 
summary(SympAct_Lab_Pos)# Hearing, Vision


#------------------------------------------------------------------------------------------
# Create a total symptom score by adding the corresponding impact and morbidity scores
#------------------------------------------------------------------------------------------

SympAct_Lab_Pos<-mutate(SympAct_Lab_Pos, TotalSymp1= ImpactScore + TransScore1)
# keeping one as numeric creating new one that is a factor
SympAct_Lab_Pos$TotalSymp1F<-as.factor(SympAct_Lab_Pos$TotalSymp1)

# Total symptom scores for different TransScores used in SM
SympAct_Lab_Pos<-mutate(SympAct_Lab_Pos, TotalSymp2= ImpactScore + TransScore2) 
SympAct_Lab_Pos<-mutate(SympAct_Lab_Pos, TotalSymp3= ImpactScore + TransScore3) 

SympAct_Any_Pos<-mutate(SympAct_Any_Pos, TotalSymp1= ImpactScore + TransScore1)
# keeping one as numeric creating new one that is a factor
SympAct_Any_Pos$TotalSymp1F<-as.factor(SympAct_Any_Pos$TotalSymp1)

# Total symptom scores for different TransScores used in SM
SympAct_Any_Pos<-mutate(SympAct_Any_Pos, TotalSymp2= ImpactScore + TransScore2) 
SympAct_Any_Pos<-mutate(SympAct_Any_Pos, TotalSymp3= ImpactScore + TransScore3) 



#-------------------------------------
# Removing unnessary vars
#-------------------------------------

# get rid of the score versions 
SympAct_Lab_Pos$SneezeScore<-NULL 
SympAct_Lab_Pos$RunnyNoseScore<-NULL
SympAct_Lab_Pos$CoughYNScore<-NULL
SympAct_Lab_Pos$CoughYN2Score<-NULL
SympAct_Lab_Pos$ChestCongestionScore<-NULL
SympAct_Lab_Pos$NasalCongestionScore<-NULL

SympAct_Any_Pos$SneezeScore<-NULL 
SympAct_Any_Pos$RunnyNoseScore<-NULL
SympAct_Any_Pos$CoughYNScore<-NULL
SympAct_Any_Pos$CoughYN2Score<-NULL
SympAct_Any_Pos$ChestCongestionScore<-NULL
SympAct_Any_Pos$NasalCongestionScore<-NULL

SympAct$SneezeScore<-NULL 
SympAct$RunnyNoseScore<-NULL
SympAct$CoughYNScore<-NULL
SympAct$CoughYN2Score<-NULL
SympAct$ChestCongestionScore<-NULL
SympAct$NasalCongestionScore<-NULL

# get rid of the logical versions 
SympAct_Any_Pos$hasSubjectiveFever<-NULL
SympAct_Any_Pos$hasChillsSweats<-NULL
SympAct_Any_Pos$hasMyalgiaYN<-NULL 
SympAct_Any_Pos$hasWeaknessYN<-NULL
SympAct_Any_Pos$hasHeadache<-NULL
SympAct_Any_Pos$hasFatigue<-NULL
SympAct_Any_Pos$hasInsomnia<-NULL
SympAct_Any_Pos$hasBreathless<-NULL
SympAct_Any_Pos$hasWheeze<-NULL
SympAct_Any_Pos$hasChestPain<-NULL
SympAct_Any_Pos$hasPharyngitis<-NULL
SympAct_Any_Pos$hasAbPain<-NULL
SympAct_Any_Pos$hasDiarrhea<-NULL
SympAct_Any_Pos$hasNausea<-NULL
SympAct_Any_Pos$hasVomit<-NULL
SympAct_Any_Pos$hasEarPn<-NULL
SympAct_Any_Pos$hasToothPn<-NULL
SympAct_Any_Pos$hasEyePn<-NULL
SympAct_Any_Pos$hasItchyEye<-NULL
SympAct_Any_Pos$hasSwollenLymphNodes<-NULL
SympAct_Any_Pos$hasCough<-NULL
SympAct_Any_Pos$hasSneeze<-NULL
SympAct_Any_Pos$hasRunnyNose<-NULL

SympAct_Lab_Pos$hasSubjectiveFever<-NULL
SympAct_Lab_Pos$hasChillsSweats<-NULL
SympAct_Lab_Pos$hasMyalgiaYN<-NULL 
SympAct_Lab_Pos$hasWeaknessYN<-NULL
SympAct_Lab_Pos$hasHeadache<-NULL
SympAct_Lab_Pos$hasFatigue<-NULL
SympAct_Lab_Pos$hasInsomnia<-NULL
SympAct_Lab_Pos$hasBreathless<-NULL
SympAct_Lab_Pos$hasWheeze<-NULL
SympAct_Lab_Pos$hasChestPain<-NULL
SympAct_Lab_Pos$hasPharyngitis<-NULL
SympAct_Lab_Pos$hasAbPain<-NULL
SympAct_Lab_Pos$hasDiarrhea<-NULL
SympAct_Lab_Pos$hasNausea<-NULL
SympAct_Lab_Pos$hasVomit<-NULL
SympAct_Lab_Pos$hasEarPn<-NULL
SympAct_Lab_Pos$hasToothPn<-NULL
SympAct_Lab_Pos$hasEyePn<-NULL
SympAct_Lab_Pos$hasItchyEye<-NULL
SympAct_Lab_Pos$hasSwollenLymphNodes<-NULL
SympAct_Lab_Pos$hasCough<-NULL
SympAct_Lab_Pos$hasSneeze<-NULL
SympAct_Lab_Pos$hasRunnyNose<-NULL


SympAct$hasSubjectiveFever<-NULL
SympAct$hasChillsSweats<-NULL
SympAct$hasMyalgiaYN<-NULL 
SympAct$hasWeaknessYN<-NULL
SympAct$hasHeadache<-NULL
SympAct$hasFatigue<-NULL
SympAct$hasInsomnia<-NULL
SympAct$hasBreathless<-NULL
SympAct$hasWheeze<-NULL
SympAct$hasChestPain<-NULL
SympAct$hasPharyngitis<-NULL
SympAct$hasAbPain<-NULL
SympAct$hasDiarrhea<-NULL
SympAct$hasNausea<-NULL
SympAct$hasVomit<-NULL
SympAct$hasEarPn<-NULL
SympAct$hasToothPn<-NULL
SympAct$hasEyePn<-NULL
SympAct$hasItchyEye<-NULL
SympAct$hasSwollenLymphNodes<-NULL
SympAct$hasCough<-NULL
SympAct$hasSneeze<-NULL
SympAct$hasRunnyNose<-NULL

#---------------------
# Save Data files 
#---------------------

saveRDS(SympAct_Any_Pos, file = "3 Clean Data/SympAct_Any_Pos.Rda")

saveRDS(SympAct_Lab_Pos, file = "3 Clean Data/SympAct_Lab_Pos.Rda")


#----------------------------------------------------------------
# This Function is used to compare correlations between symptoms
#----------------------------------------------------------------

# # We assess the correlation of infection symptoms to prevent redundant variables
# # from being included. correlation cut offs of 0.9 and 0.75 are used.
# 
# # Check for correlation of symptoms. If two symptoms have r >= cor_val or r <= -cor_val
# # one needs to be removed.
# # This function removes correlated variables until there are none.
# compare_cor <- function(df,cor_val) {
#   df <- as.data.frame(df)
#   # Create Yule's Q matrix.
#   Q_mat <- PairApply(x = df, FUN = YuleQ, symmetric = TRUE)
#   # Set the main diagonal to zero, so that the same column is not compared to itself.
#   diag(Q_mat) <- 0
# 
#   # While an entry in the matrix is outside the tolerance range, continue
#   # comparing pairs of variables, removing a variable, and updated the matrix.
#   while (TRUE %in% (Q_mat <= -cor_val | Q_mat >= cor_val)) {
#     # Determine which pair of variables to compare. Index_max will contain two
#     # entries, the row # and col # for the matrix where the first maximum entry
#     # is located.
#     Q_mat <- apply(Q_mat, c(1,2), abs)
#     index_max <- which(Q_mat == max(Q_mat), arr.ind=TRUE)[1,1:2]
# 
#     # Compare the variables. If variable 1 has a VARIANCE greater than or equal to
#     #  the variance of variable 2, discard variable 2. Else, discard variable 1.
#     #  This ensures that we keep the variable with the mean closer to p = 0.5,
#     #  which theoretically encodes the most information.
#     index_1 <- as.numeric(index_max[1])
#     variable_name_1 <- rownames(Q_mat)[index_1]
#     index_2 <- as.numeric(index_max[2])
#     variable_name_2 <- colnames(Q_mat)[index_2]
# 
#     variable_1 <- df[ , variable_name_1]
#     variable_2 <- df[ , variable_name_2]
# 
#     # Remove the correct variable based on above output.
#     variance_1 <- mean(variable_1, na.rm = TRUE)*(1 - mean(variable_1, na.rm = TRUE))
#     variance_2 <- mean(variable_2, na.rm = TRUE)*(1 - mean(variable_2, na.rm = TRUE))
#     if (variance_1 >= variance_2) {
#       temp <- colnames(df) != variable_name_2
#       df <- df[ , temp]
#     } else {
#       temp <- colnames(df) != variable_name_1
#       df <- df[ , temp]
#     }
# 
#     # Update the Yule's Q matrix.
#     Q_mat <- PairApply(x = df, FUN = YuleQ, symmetric = TRUE)
#     # Set the main diagonal to zero, so that the same column is not compared to itself.
#     diag(Q_mat) <- 0
# 
#     # Option to make plots of steps. If you want to make plots, uncomment this.
# 
#     # plot <- ggcorrplot(Q_mat, lab = TRUE, outline.col = "white", digits = 2,
#     #                    ggtheme = ggplot2::theme_gray, tl.srt = 75, lab_size = 2.5,
#     #                    colors = c("#6D9EC1", "white", "#E46726"),
#     #                    legend.title = "Yule's Q")
#     # print(plot)
#   }
# 
# 
#   return(names(df))
# }
# 
# # Infectiousness symptoms compared
# ISDF<-dplyr::select(SympAct_Lab_Pos, CoughYN, Sneeze, RunnyNose, NasalCongestion, ChestCongestion)
# 
# ISDF<-ISDF=="Yes"
# 
# ISDF<-as.data.frame(ISDF)
# 
# ISDF<-ISDF%>%mutate_all(funs(as.numeric(.)))
# 
# # At cor 0.9 cough is dropped
# compare_cor(ISDF,0.9)
# 
# # At cor 0.75 nothing changes only cough is dropped
# compare_cor(ISDF,0.75)
# 
# 
# # Morbidity symptoms compared
# MSDF<-dplyr::select(SympAct_Lab_Pos, SubjectiveFever, ChillsSweats, 
#                       MyalgiaYN, WeaknessYN, Headache, Fatigue, Insomnia, 
#                       Breathless, Wheeze, ChestPain, Pharyngitis, AbPain, 
#                       Diarrhea, Nausea, Vomit, EarPn, ToothPn, EyePn, 
#                       ItchyEye, SwollenLymphNodes)
# 
# MSDF<-MSDF=="Yes"
# 
# MSDF<-as.data.frame(MSDF)
# 
# MSDF<-MSDF%>%mutate_all(funs(as.numeric(.)))
# 
# # At cor 0.9 cough is dropped
# compare_cor(MSDF,0.9)
# 
# # At cor 0.75 nothing changes only cough is dropped
# compare_cor(MSDF,0.75)

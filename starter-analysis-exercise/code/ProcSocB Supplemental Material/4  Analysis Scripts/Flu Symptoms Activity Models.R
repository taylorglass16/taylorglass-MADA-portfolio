###################
# Regression Models and Cochran-Mantel-Haenszel Test
# Brian McKay 
# 3-23-19
####################


# Clean up global enviroment ####  
rm(list=ls())
if(!is.null(names(sessionInfo()$otherPkgs))){
  lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)}

#Load or install required packages
#Used for the ordinal regression 
if (require('finalfit')==FALSE) {install.packages('finalfit', repos="https://cran.rstudio.com"); require(finalfit)}
if (require('vcdExtra')==FALSE) {install.packages('vcdExtra', repos="https://cran.rstudio.com"); require(vcdExtra)}
if (require('tidyverse')==FALSE) {install.packages('tidyverse', repos="https://cran.rstudio.com"); require(tidyverse)}
if (require('DescTools')==FALSE) {install.packages('DescTools', repos="https://cran.rstudio.com"); require(DescTools)}

#Load data from "Data-Cleaning.R" script 
SympAct_Lab_Pos<-readRDS("3 Clean Data/SympAct_Lab_Pos.Rda")
SympAct_Any_Pos<-readRDS("3 Clean Data/SympAct_Any_Pos.Rda") 


##### Models and Tests for Manuscript ##### 

#------------------------------------------------------------------------
#Score Trend Tests Lab Diagnosis
#------------------------------------------------------------------------

##Impact Score  and Activity 
Impact_Trend_Lab<-CMHtest(Freq~ActivityLevelF+ImpactScoreFD, SympAct_Lab_Pos)
Impact_Trend_Lab
saveRDS(Impact_Trend_Lab, file = "5 Results/Models/Impact_Trend_Lab.Rda")
###Checking the expected values for Impact
ImpactTable<-table(SympAct_Lab_Pos$ActivityLevelF,SympAct_Lab_Pos$ImpactScoreFD)
Impact_Chi<-chisq.test(ImpactTable)
Impact_Chi$expected

##Trans Score and Activity
Trans_Trend_Lab<-CMHtest(Freq~ActivityLevelF+TransScore1F, SympAct_Lab_Pos)
Trans_Trend_Lab
saveRDS(Trans_Trend_Lab, file = "5 Results/Models/Trans_Trend_Lab.Rda")
###Checking the expected values for Trans
TransTable<-table(SympAct_Lab_Pos$ActivityLevelF,SympAct_Lab_Pos$TransScore1F)
Trans_Chi<-chisq.test(TransTable)
Trans_Chi$expected

##Trans Vs Impact
TvI_Trend_Lab<-CMHtest(Freq~ImpactScoreFD+TransScore1F, SympAct_Lab_Pos)
TvI_Trend_Lab
saveRDS(TvI_Trend_Lab, file = "5 Results/Models/TvI_Trend_Lab.Rda")
###Checking the expected values for Trans Vs Impact
TvITable<-table(SympAct_Lab_Pos$ImpactScoreFD,SympAct_Lab_Pos$TransScore1F)
TvI_Chi<-chisq.test(TvITable)
TvI_Chi$expected

#------------------------------------------------------------------------
# Correlation of For scores and activity Lab Diagnosis 
#------------------------------------------------------------------------

Impact_Cor_Lab<-SpearmanRho(SympAct_Lab_Pos$ImpactScore, SympAct_Lab_Pos$ActivityLevel, conf.level = 0.95)
saveRDS(Impact_Cor_Lab, file = "5 Results/Models/Impact_Cor_Lab.Rda")

Trans_Cor_Lab<-SpearmanRho(SympAct_Lab_Pos$TransScore1, SympAct_Lab_Pos$ActivityLevel, conf.level = 0.95)
saveRDS(Trans_Cor_Lab, file = "5 Results/Models/Trans_Cor_Lab.Rda")

TvI_Cor_Lab<-SpearmanRho(SympAct_Lab_Pos$ImpactScore, SympAct_Lab_Pos$TransScore1, conf.level = 0.95)
saveRDS(TvI_Cor_Lab, file = "5 Results/Models/TvI_Cor_Lab.Rda")


#------------------------------------------------------------------------
# Linear Regression For Morbidity score and activity / infectiousness Lab Diagnosis 
#------------------------------------------------------------------------

# Linear regression of impact of infectiousness score on activity 
ImpactTrans_LM_Lab<-lm(TransScore1~ImpactScore, SympAct_Lab_Pos)
saveRDS(ImpactTrans_LM_Lab, file = "5 Results/Models/ImpactTrans_LM_Lab.Rda")

# Linear regression of impact of morbidity score on activity
ImpactAct_LM_Lab<-lm(ActivityLevel~ImpactScore, SympAct_Lab_Pos)
saveRDS(ImpactAct_LM_Lab, file = "5 Results/Models/ImpactAct_LM_Lab.Rda")

#------------------------------------------------------------------------
# Linear Regression For Symptoms and Activity Lab Diagnosis
#------------------------------------------------------------------------

explanatory<-c("CoughYN", "Sneeze", "RunnyNose", "NasalCongestion", "ChestCongestion",
               "ChillsSweats", "Fatigue", "SubjectiveFever", 
               "Headache", "WeaknessYN", "MyalgiaYN", "SwollenLymphNodes",
               "AbPain", "ChestPain", "Diarrhea", "EyePn", "Insomnia", 
               "ItchyEye", "Nausea", "EarPn", "Pharyngitis", "Breathless", 
               "ToothPn", "Vomit", "Wheeze")

explanatory <- str_sort(explanatory)

# Based on subset selection using MLR (code in "Multivariate Subset Selection.R" script)
# Check results by loading sfeat_res_Lab<-readRDS("5 Results/Models/sfeat_res_Lab.Rda")
explanatory_multi<-c("ChillsSweats", "SubjectiveFever", "Headache", "WeaknessYN", "Insomnia", "Vomit")

dependent<-"ActivityLevel"


LmActvSympLab<-SympAct_Lab_Pos %>% finalfit(dependent, explanatory, explanatory_multi, add_dependent_label=TRUE)

saveRDS(LmActvSympLab, file = "5 Results/Models/LmActvSympLab.Rda")

##### Models and Tests for the Supplement #####

#------------------------------------------------------------------------
# Sensitivity Analysis of Infection Score Calculations
#------------------------------------------------------------------------

# Infection Score without chest congestion 

##Trans Score and Activity
Trans_Trend_Lab2<-CMHtest(Freq~ActivityLevelF+TransScore2F, SympAct_Lab_Pos)
Trans_Trend_Lab2
saveRDS(Trans_Trend_Lab2, file = "5 Results/Models/Trans_Trend_Lab2.Rda")
###Checking the expected values for Trans
TransTable<-table(SympAct_Lab_Pos$ActivityLevelF,SympAct_Lab_Pos$TransScore2F)
Trans_Chi<-chisq.test(TransTable)
Trans_Chi$expected

##Trans Vs Impact
TvI_Trend_Lab2<-CMHtest(Freq~ImpactScoreFD+TransScore2F, SympAct_Lab_Pos)
TvI_Trend_Lab2
saveRDS(TvI_Trend_Lab2, file = "5 Results/Models/TvI_Trend_Lab2.Rda")
###Checking the expected values for Trans Vs Impact
TvITable<-table(SympAct_Lab_Pos$ImpactScoreFD,SympAct_Lab_Pos$TransScore2F)
TvI_Chi<-chisq.test(TvITable)
TvI_Chi$expected

Trans_Cor_Lab2<-SpearmanRho(SympAct_Lab_Pos$TransScore2, SympAct_Lab_Pos$ActivityLevel, conf.level = 0.95)
saveRDS(Trans_Cor_Lab2, file = "5 Results/Models/Trans_Cor_Lab2.Rda")

TvI_Cor_Lab2<-SpearmanRho(SympAct_Lab_Pos$ImpactScore, SympAct_Lab_Pos$TransScore2, conf.level = 0.95)
saveRDS(TvI_Cor_Lab2, file = "5 Results/Models/TvI_Cor_Lab2.Rda")

# Infection Score without nasal and chest congestion 

##Trans Score and Activity
Trans_Trend_Lab3<-CMHtest(Freq~ActivityLevelF+TransScore3F, SympAct_Lab_Pos)
Trans_Trend_Lab3
saveRDS(Trans_Trend_Lab3, file = "5 Results/Models/Trans_Trend_Lab3.Rda")
###Checking the expected values for Trans
TransTable<-table(SympAct_Lab_Pos$ActivityLevelF,SympAct_Lab_Pos$TransScore3F)
Trans_Chi<-chisq.test(TransTable)
Trans_Chi$expected

##Trans Vs Impact
TvI_Trend_Lab3<-CMHtest(Freq~ImpactScoreFD+TransScore3F, SympAct_Lab_Pos)
TvI_Trend_Lab3
saveRDS(TvI_Trend_Lab3, file = "5 Results/Models/TvI_Trend_Lab3.Rda")
###Checking the expected values for Trans Vs Impact
TvITable<-table(SympAct_Lab_Pos$ImpactScoreFD,SympAct_Lab_Pos$TransScore3F)
TvI_Chi<-chisq.test(TvITable)
TvI_Chi$expected

Trans_Cor_Lab3<-SpearmanRho(SympAct_Lab_Pos$TransScore3, SympAct_Lab_Pos$ActivityLevel, conf.level = 0.95)
saveRDS(Trans_Cor_Lab3, file = "5 Results/Models/Trans_Cor_Lab3.Rda")

TvI_Cor_Lab3<-SpearmanRho(SympAct_Lab_Pos$ImpactScore, SympAct_Lab_Pos$TransScore3, conf.level = 0.95)
saveRDS(TvI_Cor_Lab3, file = "5 Results/Models/TvI_Cor_Lab3.Rda")



# Infection Score using .9 and .75 correlation cut off.  

##Trans Score and Activity
Trans_Trend_Lab4<-CMHtest(Freq~ActivityLevelF+TransScore4F, SympAct_Lab_Pos)
Trans_Trend_Lab4
saveRDS(Trans_Trend_Lab4, file = "5 Results/Models/Trans_Trend_Lab4.Rda")
###Checking the expected values for Trans
TransTable<-table(SympAct_Lab_Pos$ActivityLevelF,SympAct_Lab_Pos$TransScore4F)
Trans_Chi<-chisq.test(TransTable)
Trans_Chi$expected

##Trans Vs Impact
TvI_Trend_Lab4<-CMHtest(Freq~ImpactScoreFD+TransScore4F, SympAct_Lab_Pos)
TvI_Trend_Lab4
saveRDS(TvI_Trend_Lab4, file = "5 Results/Models/TvI_Trend_Lab4.Rda")
###Checking the expected values for Trans Vs Impact
TvITable<-table(SympAct_Lab_Pos$ImpactScoreFD,SympAct_Lab_Pos$TransScore4F)
TvI_Chi<-chisq.test(TvITable)
TvI_Chi$expected

Trans_Cor_Lab4<-SpearmanRho(SympAct_Lab_Pos$TransScore4, SympAct_Lab_Pos$ActivityLevel, conf.level = 0.95)
saveRDS(Trans_Cor_Lab4, file = "5 Results/Models/Trans_Cor_Lab4.Rda")

TvI_Cor_Lab4<-SpearmanRho(SympAct_Lab_Pos$ImpactScore, SympAct_Lab_Pos$TransScore4, conf.level = 0.95)
saveRDS(TvI_Cor_Lab4, file = "5 Results/Models/TvI_Cor_Lab4.Rda")

# Linear regression of impact of morbidity score on infectiousness score 

ImpactTrans2_LM_Lab<-lm(TransScore2~ImpactScore, SympAct_Lab_Pos)
saveRDS(ImpactTrans2_LM_Lab, file = "5 Results/Models/ImpactTrans2_LM_Lab.Rda")

ImpactTrans3_LM_Lab<-lm(TransScore3~ImpactScore, SympAct_Lab_Pos)
saveRDS(ImpactTrans3_LM_Lab, file = "5 Results/Models/ImpactTrans3_LM_Lab.Rda")

ImpactTrans4_LM_Lab<-lm(TransScore4~ImpactScore, SympAct_Lab_Pos)
saveRDS(ImpactTrans4_LM_Lab, file = "5 Results/Models/ImpactTrans4_LM_Lab.Rda")



#------------------------------------------------------------------------
# Sensitivity Analysis of Morbidity Score Calculations
#------------------------------------------------------------------------

# Morbidity score 0.9 cut off and activity and infectiousness score

## Trend Tests

##Impact Score  and Activity 
Impact2_Trend_Lab<-CMHtest(Freq~ActivityLevelF+ImpactScore2FD, SympAct_Lab_Pos)
Impact2_Trend_Lab
saveRDS(Impact2_Trend_Lab, file = "5 Results/Models/Impact2_Trend_Lab.Rda")
###Checking the expected values for Impact
ImpactTable<-table(SympAct_Lab_Pos$ActivityLevelF,SympAct_Lab_Pos$ImpactScore2FD)
Impact_Chi<-chisq.test(ImpactTable)
Impact_Chi$expected

##Trans Vs Impact
TvI2_Trend_Lab<-CMHtest(Freq~ImpactScore2FD+TransScore1F, SympAct_Lab_Pos)
TvI2_Trend_Lab
saveRDS(TvI2_Trend_Lab, file = "5 Results/Models/TvI2_Trend_Lab.Rda")
###Checking the expected values for Trans Vs Impact
TvITable<-table(SympAct_Lab_Pos$ImpactScore2FD,SympAct_Lab_Pos$TransScore1F)
TvI_Chi<-chisq.test(TvITable)
TvI_Chi$expected

## Correlations 

Impact2_Cor_Lab<-SpearmanRho(SympAct_Lab_Pos$ImpactScore2, SympAct_Lab_Pos$ActivityLevel, conf.level = 0.95)
saveRDS(Impact2_Cor_Lab, file = "5 Results/Models/Impact2_Cor_Lab.Rda")

TvI2_Cor_Lab<-SpearmanRho(SympAct_Lab_Pos$ImpactScore2, SympAct_Lab_Pos$TransScore1, conf.level = 0.95)
saveRDS(TvI2_Cor_Lab, file = "5 Results/Models/TvI2_Cor_Lab.Rda")

# Morbidity score 0.75 cut off and activity and infectiousness score

## Trend Tests

##Impact Score  and Activity 
Impact3_Trend_Lab<-CMHtest(Freq~ActivityLevelF+ImpactScore3FD, SympAct_Lab_Pos)
Impact3_Trend_Lab
saveRDS(Impact3_Trend_Lab, file = "5 Results/Models/Impact3_Trend_Lab.Rda")
###Checking the expected values for Impact
ImpactTable<-table(SympAct_Lab_Pos$ActivityLevelF,SympAct_Lab_Pos$ImpactScore3FD)
Impact_Chi<-chisq.test(ImpactTable)
Impact_Chi$expected


##Trans Vs Impact
TvI3_Trend_Lab<-CMHtest(Freq~ImpactScore3FD+TransScore1F, SympAct_Lab_Pos)
TvI3_Trend_Lab
saveRDS(TvI2_Trend_Lab, file = "5 Results/Models/TvI3_Trend_Lab.Rda")
###Checking the expected values for Trans Vs Impact
TvITable<-table(SympAct_Lab_Pos$ImpactScore3FD,SympAct_Lab_Pos$TransScore1F)
TvI_Chi<-chisq.test(TvITable)
TvI_Chi$expected


## Correlations 

Impact3_Cor_Lab<-SpearmanRho(SympAct_Lab_Pos$ImpactScore3, SympAct_Lab_Pos$ActivityLevel, conf.level = 0.95)
saveRDS(Impact3_Cor_Lab, file = "5 Results/Models/Impact3_Cor_Lab.Rda")

TvI3_Cor_Lab<-SpearmanRho(SympAct_Lab_Pos$ImpactScore3, SympAct_Lab_Pos$TransScore1, conf.level = 0.95)
saveRDS(TvI3_Cor_Lab, file = "5 Results/Models/TvI3_Cor_Lab.Rda")

# Linear regression of impact of morbidity score on activity
Impact2Act_LM_Lab<-lm(ActivityLevel~ImpactScore2, SympAct_Lab_Pos)
saveRDS(Impact2Act_LM_Lab, file = "5 Results/Models/Impact2Act_LM_Lab.Rda")

Impact3Act_LM_Lab<-lm(ActivityLevel~ImpactScore3, SympAct_Lab_Pos)
saveRDS(Impact3Act_LM_Lab, file = "5 Results/Models/Impact3Act_LM_Lab.Rda")

Impact2Trans1_LM_Lab<-lm(TransScore1~ImpactScore2, SympAct_Lab_Pos)
saveRDS(Impact2Trans1_LM_Lab, file = "5 Results/Models/Impact2Trans1_LM_Lab.Rda")

Impact3Trans1_LM_Lab<-lm(TransScore1~ImpactScore3, SympAct_Lab_Pos)
saveRDS(Impact3Trans1_LM_Lab, file = "5 Results/Models/Impact3Trans1_LM_Lab.Rda")

#------------------------------------------------------------------------------------------
# Same Analysis from the main text for Any Diagnosis (Rapid, PCR, and Empirical)
#------------------------------------------------------------------------------------------


#------------------------------------------------------------------------
# Score Trend Tests Any Diagnosis (Rapid, PCR, and Empirical)
#------------------------------------------------------------------------
##Impact Score  and Activity
Impact_Trend_Any<-CMHtest(Freq~ActivityLevelF+ImpactScoreFD, SympAct_Any_Pos)
Impact_Trend_Any
saveRDS(Impact_Trend_Any, file = "5 Results/Models/Impact_Trend_Any.Rda")
###Checking the expected values for Impact
ImpactTable<-table(SympAct_Any_Pos$ActivityLevelF,SympAct_Any_Pos$ImpactScoreFD)
Impact_Chi<-chisq.test(ImpactTable)
Impact_Chi$expected

##Trans Score and Activity
Trans_Trend_Any<-CMHtest(Freq~ActivityLevelF+TransScore1F, SympAct_Any_Pos)
Trans_Trend_Any
saveRDS(Trans_Trend_Any, file = "5 Results/Models/Trans_Trend_Any.Rda")
###Checking the expected values for Trans
TransTable<-table(SympAct_Any_Pos$ActivityLevelF,SympAct_Any_Pos$TransScore1F)
Trans_Chi<-chisq.test(TransTable)
Trans_Chi$expected

##Trans Vs Impact
TvI_Trend_Any<-CMHtest(Freq~ImpactScoreFD+TransScore1F, SympAct_Any_Pos)
TvI_Trend_Any
saveRDS(TvI_Trend_Any, file = "5 Results/Models/TvI_Trend_Any.Rda")
###Checking the expected values for Trans Vs Impact
TvITable<-table(SympAct_Any_Pos$ImpactScoreFD,SympAct_Any_Pos$TransScore1F)
TvI_Chi<-chisq.test(TvITable)
TvI_Chi$expected

#------------------------------------------------------------------------
# Correlation of For scores and activity Any Diagnosis
#------------------------------------------------------------------------
Impact_Cor_Any<-SpearmanRho(SympAct_Any_Pos$ImpactScore, SympAct_Any_Pos$ActivityLevel, conf.level = 0.95)
saveRDS(Impact_Cor_Any, file = "5 Results/Models/Impact_Cor_Any.Rda")

Trans_Cor_Any<-SpearmanRho(SympAct_Any_Pos$TransScore1, SympAct_Any_Pos$ActivityLevel, conf.level = 0.95)
saveRDS(Trans_Cor_Any, file = "5 Results/Models/Trans_Cor_Any.Rda")

TvI_Cor_Any<-SpearmanRho(SympAct_Any_Pos$TransScore1, SympAct_Any_Pos$ImpactScore, conf.level = 0.95)
saveRDS(TvI_Cor_Any, file = "5 Results/Models/TvI_Cor_Any.Rda")

#------------------------------------------------------------------------
# Linear Regression For morbidity score on activity and infectiousness score Lab Diagnosis 
#------------------------------------------------------------------------

# Linear regression of impact of infectiousness score on activity 
ImpactTrans_LM_Any<-lm(TransScore1~ImpactScore, SympAct_Any_Pos)
saveRDS(ImpactTrans_LM_Any, file = "5 Results/Models/ImpactTrans_LM_Any.Rda")

# Linear regression of impact of morbidity score on activity
ImpactAct_LM_Any<-lm(ActivityLevel~ImpactScore, SympAct_Any_Pos)
saveRDS(ImpactAct_LM_Any, file = "5 Results/Models/ImpactAct_LM_Any.Rda")

#------------------------------------------------------------------------
# Linear Regression For Symptoms and Activity Any Diagnosis
#------------------------------------------------------------------------
explanatory<-c("CoughYN", "Sneeze", "RunnyNose", "NasalCongestion", "ChestCongestion",
               "ChillsSweats", "Fatigue", "SubjectiveFever", 
               "Headache", "WeaknessYN", "MyalgiaYN", "SwollenLymphNodes",
               "AbPain", "ChestPain", "Diarrhea", "EyePn", "Insomnia", 
               "ItchyEye", "Nausea", "EarPn", "Pharyngitis", "Breathless", 
               "ToothPn", "Vomit", "Wheeze")

explanatory <- str_sort(explanatory)

# Based on subset selection using MLR (code in "Multivariate Subset Selection.R" script)
# Check results by loading sfeat_res_Any<-readRDS("5 Results/Models/sfeat_res_Any.Rda")
explanatory_multi<-c("ChillsSweats", "SubjectiveFever", "Headache", "WeaknessYN", "Insomnia", "Vomit")

dependent<-"ActivityLevel"


LmActvSympAny<-SympAct_Any_Pos %>% finalfit(dependent, explanatory, explanatory_multi, add_dependent_label=TRUE)
saveRDS(LmActvSympAny, file = "5 Results/Models/LmActvSympAny.Rda")



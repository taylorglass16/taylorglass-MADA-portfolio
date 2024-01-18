###################
# Tables for Manuscript and SM
# Brian McKay 
# 4-10-19
####################


# Clean up global enviroment ######
rm(list=ls())

#Remove any packages that might have conflicts with the required packages
if(!is.null(names(sessionInfo()$otherPkgs))){
  lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)}


#Load or install required packages
if (require('tableone')==FALSE) {install.packages('tableone', repos="https://cran.rstudio.com"); require(tableone)}
if (require('tidyverse')==FALSE) {install.packages('tidyverse', repos="https://cran.rstudio.com"); require(tidyverse)}


#Load data from "Data-Cleaning.R" script 
SympAct_Any_Pos<-readRDS("3 Clean Data/SympAct_Any_Pos.Rda") 
SympAct_Lab_Pos<-readRDS("3 Clean Data/SympAct_Lab_Pos.Rda")



# Tables in the Manuscript ######

#### Table Symptom Frequencies
# Create a list of variable we want in the table 
#cat(paste(shQuote(colnames(SympAct_Any_Pos),type="cmd"),collapse = ", "))
listVars <- c("AbPain", "Breathless", "ChestCongestion", "ChestPain", "ChillsSweats", 
              "CoughYN", "Diarrhea", "EarPn", "EyePn", "Fatigue", "Headache", 
              "ItchyEye", "MyalgiaYN", "NasalCongestion", "Nausea", "RunnyNose", 
              "Insomnia", "Sneeze", "Pharyngitis", "SubjectiveFever", "SwollenLymphNodes", 
              "ToothPn", "Vomit", "WeaknessYN", "Wheeze")


table <- CreateTableOne(vars = listVars, data = SympAct_Lab_Pos)

SympLabTable<-print(table, varLabels = TRUE)

saveRDS(SympLabTable, file = "5 Results/Tables/SympLabTable.Rda")



# Tables in the Supporting Materials  ######


#### Table Symptom Frequencies For ANY Diagnosis (PCR, Rapid, Empirical)
# Create a list of variable we want in the table
#cat(paste(shQuote(colnames(SympAct_Any_Pos),type="cmd"),collapse = ", "))
listVars <- c("AbPain", "Breathless", "ChestCongestion", "ChestPain", "ChillsSweats", 
              "CoughYN", "Diarrhea", "EarPn", "EyePn", "Fatigue", "Headache", 
              "ItchyEye", "MyalgiaYN", "NasalCongestion", "Nausea", "RunnyNose", 
              "Insomnia", "Sneeze", "Pharyngitis", "SubjectiveFever", "SwollenLymphNodes", 
              "ToothPn", "Vomit", "WeaknessYN", "Wheeze")


table <- CreateTableOne(vars = listVars, data = SympAct_Any_Pos)

SympAnyTable<-print(table, varLabels = TRUE)

saveRDS(SympAnyTable, file = "5 Results/Tables/SympAnyTable.Rda")


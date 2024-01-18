###################
# Plots For Manuscript and Supplement
# Brian McKay 
# 3-23-19
####################

#Clean up global enviroment
rm(list=ls())

#Remove any packages that might have conflicts with the required packages
if(!is.null(names(sessionInfo()$otherPkgs))){
  lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)}

#Load or install required packages ####
# For figures 
if (require('ggplot2')==FALSE) {install.packages('ggplot2', repos="https://cran.rstudio.com"); require(ggplot2)}
if (require('ggcorrplot')==FALSE) {install.packages('ggcorrplot', repos="https://cran.rstudio.com"); require(ggcorrplot)}
if (require('gridExtra')==FALSE) {install.packages('gridExtra', repos="https://cran.rstudio.com"); require(gridExtra)}


# Analysis and Data Wrangling
if (require('tidyverse')==FALSE) {install.packages('tidyverse', repos="https://cran.rstudio.com"); require(tidyverse);}
if (require('reshape2')==FALSE) {install.packages('reshape2', repos="https://cran.rstudio.com"); require(reshape2)}
if (require('DescTools')==FALSE) {install.packages('DescTools', repos="https://cran.rstudio.com"); require(DescTools)}



#Load data from "Data Cleaning.R" script in the "2 Data Cleaning Script" folder ####
SympAct_Lab_Pos<-readRDS("3 Clean Data/SympAct_Lab_Pos.Rda")
SympAct_Any_Pos<-readRDS("3 Clean Data/SympAct_Any_Pos.Rda")

#--------------------------------------------------------------------------
# Generalized plot to convey understanding to reader 
#--------------------------------------------------------------------------


sigmoidTS <- function(x) {
  b=2
  d=2
  a=1.25
  c=0.17
  ((a*x^b)/(c+x^d))+0
}

# This function describes the contact rate
sigmoidCR <- function(x) {
  1 / (1 + exp((9*x)-4)+.30)
}

# # This function describes the morbidity symptoms
# expoMS <- function(x) {
#   exp(1.9*x-7)
# }
#lines(x,yMS, col="brown", lty=3, lwd=3)
#yMS<- expoMS(x)

#  X Values 
x<-seq(0,1.5,0.001)

# Corresponding Y values
yTS<-sigmoidTS(x)

yCR<-sigmoidCR(x)

# Total transmission potential
TS_CR<-yTS*yCR

# Mean Morbidity Score for each level of activity
SympAct_Lab_Pos$ActivityLevelB<-(SympAct_Lab_Pos$ActivityLevel/max(SympAct_Lab_Pos$ActivityLevel))

medActivity<-SympAct_Lab_Pos %>% 
  group_by(ImpactScoreF) %>%
  summarise_at(vars(ActivityLevelB), list(~mean(., na.rm=TRUE)))

# Mean Transmission Score for each level of activity
SympAct_Lab_Pos$TransScoreB<-(SympAct_Lab_Pos$TransScore1/max(SympAct_Lab_Pos$TransScore1))

medTsymp<-SympAct_Lab_Pos %>% 
  group_by(ImpactScoreF) %>%
  summarise_at(vars(TransScoreB), list(~mean(., na.rm=TRUE)))

#graphics.off(); #close all graphics windows
 
ww=8; wh=2/3*ww; #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# reduce white space around figure
par(mar=c(3,3,3,3))

# Create black and white plot
plot(x, yTS, axes = F, xlab= "", ylab = "", ylim=c(-0.25,1.5), type = "l", lty=1, lwd=2, xaxs="i", yaxs="i")
lines(x, yCR, lty=2, lwd=2)
lines(x, TS_CR, lty=4, lwd=2)
#points()
axis(side = 2, pos = 0, tck=0, labels = F)
axis(side = 1, pos = 0, tck=0, labels = F)
arrows(1.18, -0.2, 1.45, -0.2, xpd = TRUE)
text(x=.95, y=-0.2, label="Increasing Virulence (v)")
par(new=T)
plot(y=medActivity$ActivityLevelB, x=seq(0.25,0.7,length=length(medActivity$ActivityLevelB)), xlim = c(0,1.5), ylim=c(-0.25,1.5), axes=F, ylab="", xlab="", pch= 5, cex=1.5, lwd=3)
text(x=.45, y=-0.2, label = "Morbidity Score" )
axis(side = 1, at =seq(0.25,0.7,length=length(levels(SympAct_Lab_Pos$ImpactScoreFD))), labels = c(levels(SympAct_Lab_Pos$ImpactScoreFD)))
par(new=T)
plot(y=medTsymp$TransScoreB, x=seq(0.25,0.7,length=length(medActivity$ActivityLevelB)), xlim = c(0,1.5), ylim=c(-0.2,1.5), axes=F, ylab="", xlab="", pch= 1, cex=1.5, lwd=2)
legend("right", xjust = 0.5, yjust = 1, title = "Lines", 
       c("Per-Contact Transmission Potential (p)",
         "Contact Rate (c)", 
         expression(paste("Total Transmission Potential (",`T`[pc],")", sep = ""))), 
       cex = 1, lty = c(1,2,4), lwd=c(1,1,1))
legend("topleft", inset = .05, title = "Points", 
       c("Mean Activity", "Mean Infectiousness Score" ), 
       cex = 1, pch=c(5,1))

dev.print(device=tiff,filename ="5 Results/Figures/ConceptFigDATA.tiff",width=ww, height=wh, units="in",pointsize = 12, compression =c("lzw"), res=300)

dev.off()

# Color version for online
windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# reduce white space around figure
par(mar=c(3,3,3,3))

# color plot
plot(x, yTS, axes = F, xlab= "", ylab = "", ylim=c(-0.25,1.5), type = "l", lty=1, lwd=2, col="red", xaxs="i", yaxs="i")
lines(x, yCR, lty=2, lwd=2, col="green")
lines(x, TS_CR, lty=4, lwd=2, col="blue")
#points()
axis(side = 2, pos = 0, tck=0, labels = F)
axis(side = 1, pos = 0, tck=0, labels = F)
arrows(1.18, -0.2, 1.45, -0.2, xpd = TRUE)
text(x=.95, y=-0.2, label="Increasing Virulence (v)")
par(new=T)
plot(y=medActivity$ActivityLevelB, x=seq(0.25,0.7,length=length(medActivity$ActivityLevelB)), xlim = c(0,1.5), ylim=c(-0.25,1.5), axes=F, ylab="", xlab="", pch= 5, cex=1.5, lwd=3)
text(x=.45, y=-0.2, label = "Morbidity Score" )
axis(side = 1, at =seq(0.25,0.7,length=length(levels(SympAct_Lab_Pos$ImpactScoreFD))), labels = c(levels(SympAct_Lab_Pos$ImpactScoreFD)))
par(new=T)
plot(y=medTsymp$TransScoreB, x=seq(0.25,0.7,length=length(medActivity$ActivityLevelB)), xlim = c(0,1.5), ylim=c(-0.2,1.5), axes=F, ylab="", xlab="", pch= 1, cex=1.5, lwd=2)
legend("right", xjust = 0.5, yjust = 1, title = "Lines", 
       c("Per-Contact Transmission Potential (p)",
         "Contact Rate (c)", 
         expression(paste("Total Transmission Potential (",`T`[pc],")", sep = ""))), 
       cex = 1, lty = c(1,2,4), lwd=c(1,1,1), col= c("red","green","blue"))
legend("topleft", inset = .05, title = "Points", 
       c("Mean Activity", "Mean Infectiousness Score" ), 
       cex = 1, pch=c(5,1))

dev.print(device=tiff,filename ="5 Results/Figures/ColorConceptFigDATA.tiff",width=ww, height=wh, units="in",pointsize = 12, compression =c("lzw"), res=300)

dev.off()





#--------------------------------------------------------------------------
# Setting up ggplot Themes 
#--------------------------------------------------------------------------
#Don't need the legend for right now
#Need text to be smaller
manuscriptTheme <- theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
                         plot.subtitle = element_text(size = 15, face = "bold"),
                         axis.title.x = element_text(size = 18, face = "bold"),
                         axis.title.y = element_text(size = 18, face = "bold"),
                         axis.text = element_text(size = 14),
                         legend.position = "none",
                         panel.background = element_rect(fill = "white", color = "black"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank())

SMTheme <- theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
                         plot.subtitle = element_text(size = 12, face = "bold"),
                         axis.title.x = element_text(size = 14, face = "bold"),
                         axis.title.y = element_text(size = 14, face = "bold"),
                         axis.text = element_text(size = 12),
                         panel.background = element_rect(fill = "white", color = "black"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank())

SMTheme1 <- theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
                         plot.subtitle = element_text(size = 10, face = "bold"),
                         axis.title.x = element_text(size = 12, face = "bold"),
                         axis.title.y = element_text(size = 12, face = "bold"),
                         axis.text = element_text(size = 10),
                         panel.background = element_rect(fill = "white", color = "black"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank())

# Set window size for the two following figures
graphics.off(); #close all graphics windows




#--------------------------------------------------------------------------
# Plot labels with number of observations for each factor level
#--------------------------------------------------------------------------
#Lab DX
##Transmission
TransLabtab<-table(SympAct_Lab_Pos$TransScore1F)
TransLab_xlabs<-paste0(c(seq(0,to=length(TransLabtab)-1, by=1))," (n=", TransLabtab, ")")
##Transmission using cough yes/no and no chest conjestion variable
TransLabtabSM2<-table(SympAct_Lab_Pos$TransScore2F)
TransLabSM2_xlabs<-paste0(c(seq(0,to=length(TransLabtabSM2)-1, by=1))," (n=", TransLabtabSM2, ")")
##Transmission using cough yes/no and no chest or nasal conjestion variables
TransLabtabSM3<-table(SympAct_Lab_Pos$TransScore3F)
TransLabSM3_xlabs<-paste0(c(seq(0,to=length(TransLabtabSM3)-1, by=1))," (n=", TransLabtabSM3, ")")
##Transmission using using 0.9 and 0.75 correlation cut offs. 
TransLabtabSM4<-table(SympAct_Lab_Pos$TransScore4F)
TransLabSM4_xlabs<-paste0(c(seq(0,to=length(TransLabtabSM4)-1, by=1))," (n=", TransLabtabSM4, ")")
##Morbidity
ImpactLabtab<-table(SympAct_Lab_Pos$ImpactScoreF)
ImpactLab_xlabs<-paste0(c(seq(0,to=length(ImpactLabtab)-1, by=1))," (n=", ImpactLabtab, ")")
##Morbidity using Yule's Q cut off of 0.9
ImpactLabtabSM2<-table(SympAct_Lab_Pos$ImpactScore2F)
ImpactLabSM2_xlabs<-paste0(c(seq(0,to=length(ImpactLabtabSM2)-1, by=1))," (n=", ImpactLabtabSM2, ")")
##Morbidity using Yule's Q cut off of 0.75
ImpactLabtabSM3<-table(SympAct_Lab_Pos$ImpactScore3F)
ImpactLabSM3_xlabs<-paste0(c(seq(0,to=length(ImpactLabtabSM3)-1, by=1))," (n=", ImpactLabtabSM3, ")")



#Any DX Only the scores for the main text are used for the any DX method group
##Transmission 
TransAnytab<-table(SympAct_Any_Pos$TransScore1F)
TransAny_xlabs<-paste0(c(seq(0,to=length(TransAnytab)-1, by=1))," (n=", TransAnytab, ")")
##Impact
ImpactAnytab<-table(SympAct_Any_Pos$ImpactScoreF)
ImpactAny_xlabs<-paste0(c(seq(0,to=length(ImpactAnytab)-1, by=1))," (n=", ImpactAnytab, ")")




#--------------------------------------------------------------------------
# ggplots for Manuscript
#--------------------------------------------------------------------------

ww=8; wh=2/3*ww; #window size, can be adjusted
windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Lab flu Dx TransScore and ActivityLevel for manuscript
pA<- ggplot(SympAct_Lab_Pos, aes(x=TransScore1F, y=ActivityLevel))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=4)+
  scale_y_continuous(limits = c(0,10), breaks=seq(from=0, to=10, by=2))+
  scale_x_discrete(breaks=0:(length(TransLab_xlabs)-1), labels=TransLab_xlabs)+
  labs(x="Infectiousness Score", y="Activity Level")+
  manuscriptTheme+
  theme(axis.text.x = element_text(size = 11))
ggsave(file = "5 Results/Figures/ActivityVSTransScore_Lab.tiff",pA, dpi=320, compression = "lzw")

# Lab flu Dx ImpactScore and ActivityLevel for manuscript
pB<- ggplot(SympAct_Lab_Pos, aes(x=ImpactScoreF, y=ActivityLevel))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=4)+
  scale_y_continuous(limits = c(0,10), breaks=seq(from=0, to=10, by=2))+
  scale_x_discrete(breaks=0:(length(ImpactLab_xlabs)-1), labels=ImpactLab_xlabs)+
  labs(x="Morbidity Score", y="Activity Level")+
  manuscriptTheme+
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 0.75, vjust = 0.85))
ggsave(file = "5 Results/Figures/ActivityVSImpactScore_Lab.tiff",pB, dpi=320, compression = "lzw")

# Lab flu Dx ImpactScore and TransScore for manuscript
pC<-ggplot(SympAct_Lab_Pos, aes(x=ImpactScoreF, y=TransScore1))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=4)+
  scale_x_discrete(breaks=0:(length(ImpactLab_xlabs)-1), labels=ImpactLab_xlabs)+
  labs(y="Infectiousness Score", x="Morbidity Score")+
  manuscriptTheme+
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 0.75, vjust = 0.85))

ggsave(file = "5 Results/Figures/TransScoreVSImpactScore_Lab.tiff",pC, dpi=320, compression = "lzw")

#Set window size for the two combined figures
graphics.off() #close all graphics windows

ww=8; wh=3*(2/3*ww); #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Make a combined figure
pA<-pA+ggtitle("A")
pB<-pB+ggtitle("B")
pC<-pC+ggtitle("C")

plotG <- grid.arrange(pA, pB, pC, nrow = 3)

ggsave(file="5 Results/Figures/IandMScore_Lab.tiff", plot=plotG, dpi = 320, compression = "lzw")




#--------------------------------------------------------------------------
# ggplots for SM 
#--------------------------------------------------------------------------

# These are the plots for the additional analysis performed 
# These results will be included in the SM

#Set window size for all the following figures
graphics.off(); #close all graphics windows
ww=8; wh=2/3*ww; #window size, can be adjusted
windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Barchart of activity for manuscript
p1<-ggplot(SympAct_Lab_Pos, aes(ActivityLevel))+ 
  geom_bar()+
  scale_x_continuous(breaks=0:10)+
  geom_text(stat='count', aes(label=..count..), vjust=-.60)+
  labs(x="Activity Level", y="Patient Count")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/ActivityLabBarChart.tiff",p1, dpi=320, compression = "lzw")


# Barchart of infectiousness score
pA<-ggplot(SympAct_Lab_Pos, aes(TransScore1))+ 
  geom_bar()+
  scale_x_continuous(breaks=min(levels(SympAct_Lab_Pos$TransScore1F)):max(levels(SympAct_Lab_Pos$TransScore1F)))+
  geom_text(stat='count', aes(label=..count..), vjust=-.30)+
  labs(x="Infectiousness Score", y="Patient Count")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/InfectScoreLabBarChart.tiff",pA, dpi=320, compression = "lzw")

# Barchart of morbidity score
pB<-ggplot(SympAct_Lab_Pos, aes(ImpactScore))+ 
  geom_bar()+
  scale_x_continuous(limits = c(as.numeric(min(levels(SympAct_Lab_Pos$ImpactScoreF))),
                                as.numeric(length(levels(SympAct_Lab_Pos$ImpactScoreF))-1)),
                     breaks=min(levels(SympAct_Lab_Pos$ImpactScoreF)):(length(levels(SympAct_Lab_Pos$ImpactScoreF))-1))+
  ylim(0,55)+
  geom_text(stat='count', aes(label=..count..), vjust=-.60)+
  labs(x="Morbidity Score", y="Patient Count")+
  manuscriptTheme+
  theme(axis.text.x = element_text(size = 12))
ggsave(file = "5 Results/Figures/MorbScoreLabBarChart.tiff",pB, dpi=320, compression = "lzw")


#Set window size for the combined figures
graphics.off() #close all graphics windows

ww=8; wh=2.75*(2/3*ww); #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Make a combined figure
pA<-pA+ggtitle("A")
pB<-pB+ggtitle("B")

plotG <- grid.arrange(pA, pB, nrow = 2)

ggsave(file="5 Results/Figures/IandMScoresLabBarChart.tiff", plot=plotG, dpi = 320, compression = "lzw")

graphics.off(); #close all graphics windows

#--------------------------------------------------------------------------
# Lab diagnosis morbidity score sensitivity analysis
#--------------------------------------------------------------------------

ww=8; wh=2/3*ww; #window size, can be adjusted
windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Barchart of morbidity score 0.9
pA<-ggplot(SympAct_Lab_Pos, aes(ImpactScore2))+ 
  geom_bar()+
  scale_x_continuous(limits = c(-0.5, as.numeric(length(levels(SympAct_Lab_Pos$ImpactScore2F))-1)),
                     breaks=min(levels(SympAct_Lab_Pos$ImpactScore2F)):(length(levels(SympAct_Lab_Pos$ImpactScore2F))-1))+
  ylim(0,RoundTo(max(summary(SympAct_Lab_Pos$ImpactScore2F)), 10, ceiling))+
  geom_text(stat='count', aes(label=..count..), vjust=-.40)+
  labs(x="Morbidity Score (0.9 Cut Off)", y="Patient Count")+
  SMTheme1
ggsave(file = "5 Results/Figures/SM_MorbScore2LabBarChart.tiff",pA, dpi=320, compression = "lzw")

# Lab flu Dx morbidity score 0.9 and ActivityLevel for manuscript
pB<- ggplot(SympAct_Lab_Pos, aes(x=ImpactScore2F, y=ActivityLevel))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_y_continuous(limits = c(0,10), breaks=seq(from=0, to=10, by=2))+
  scale_x_discrete(breaks=0:(length(ImpactLabSM2_xlabs)-1), labels=ImpactLabSM2_xlabs)+
  labs(x="Morbidity Score (0.9 Cut Off)", y="Activity Level")+
  SMTheme1+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.85))
ggsave(file = "5 Results/Figures/SM_ActivityVSImpactScore2_Lab.tiff",pB, dpi=320, compression = "lzw")

# Lab flu Dx ImpactScore2 0.9 and TransScore from main manuscript
pC<-ggplot(SympAct_Lab_Pos, aes(x=ImpactScore2F, y=TransScore1))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_x_discrete(breaks=0:(length(ImpactLabSM2_xlabs)-1), labels=ImpactLabSM2_xlabs)+
  labs(y="Infectiousness Score", x="Morbidity Score (0.9 Cut Off)")+
  SMTheme1+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.85))

ggsave(file = "5 Results/Figures/SM_TransScoreVSImpactScore2_Lab.tiff",pC, dpi=320, compression = "lzw")


#Set window size for the two combined figures
graphics.off() #close all graphics windows

ww=8; wh=3*(2/3*ww); #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Make a combined figure
pA<-pA+ggtitle("A")
pB<-pB+ggtitle("B")
pC<-pC+ggtitle("C")

plotG <- grid.arrange(pA, pB, pC, nrow = 3)

ggsave(file="5 Results/Figures/SM_ImpactScore2_Lab.tiff", plot=plotG, dpi = 320, compression = "lzw")

graphics.off(); #close all graphics windows


ww=8; wh=2/3*ww; #window size, can be adjusted
windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Barchart of morbidity score 0.75
pA<-ggplot(SympAct_Lab_Pos, aes(ImpactScore3))+ 
  geom_bar()+
  scale_x_continuous(limits = c(-0.5, as.numeric(length(levels(SympAct_Lab_Pos$ImpactScore3F))-1)),
                     breaks=0:(length(levels(SympAct_Lab_Pos$ImpactScore3F))-1))+
  ylim(0,RoundTo(max(summary(SympAct_Lab_Pos$ImpactScore3F)),13, ceiling))+
  geom_text(stat='count', aes(label=..count..), vjust=-.60)+
  labs(x="Morbidity Score (0.75 Cut Off)", y="Patient Count")+
  SMTheme1
ggsave(file = "5 Results/Figures/SM_MorbScore3LabBarChart.tiff",pA, dpi=320, compression = "lzw")

# Lab flu Dx morbidity score 0.75 and ActivityLevel for manuscript
pB<- ggplot(SympAct_Lab_Pos, aes(x=ImpactScore3F, y=ActivityLevel))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_y_continuous(limits = c(0,10), breaks=seq(from=0, to=10, by=2))+
  scale_x_discrete(breaks=0:(length(ImpactLabSM3_xlabs)-1), labels=ImpactLabSM3_xlabs)+
  labs(x="Morbidity Score (0.75 Cut Off)", y="Activity Level")+
  SMTheme1+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.85))
ggsave(file = "5 Results/Figures/SM_ActivityVSImpactScore3_Lab.tiff",pB, dpi=320, compression = "lzw")


# Lab flu Dx ImpactScore3 0.75 and TransScore from main manuscript
pC<-ggplot(SympAct_Lab_Pos, aes(x=ImpactScore3F, y=TransScore1))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_x_discrete(breaks=0:(length(ImpactLabSM3_xlabs)-1), labels=ImpactLabSM3_xlabs)+
  labs(y="Infectiousness Score", x="Morbidity Score (0.75 Cut Off)")+
  SMTheme1+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.85))

ggsave(file = "5 Results/Figures/SM_TransScoreVSImpactScore3_Lab.tiff",pC, dpi=320, compression = "lzw")

#Set window size for the two combined figures
graphics.off() #close all graphics windows

ww=8; wh=3*(2/3*ww); #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Make a combined figure
pA<-pA+ggtitle("A")
pB<-pB+ggtitle("B")
pC<-pC+ggtitle("C")

plotG <- grid.arrange(pA, pB, pC, nrow = 3)

ggsave(file="5 Results/Figures/SM_ImpactScore3_Lab.tiff", plot=plotG, dpi = 320, compression = "lzw")

graphics.off(); #close all graphics windows


#--------------------------------------------------------------------------
# Lab diagnosis infect score sensitivity analysis
#--------------------------------------------------------------------------

# Infect score without chest congestion
ww=8; wh=2/3*ww; #window size, can be adjusted
windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Barchart of infectiousness score
pA<-ggplot(SympAct_Lab_Pos, aes(TransScore2))+ 
  geom_bar()+
  scale_x_continuous(breaks=min(levels(SympAct_Lab_Pos$TransScore2F)):max(levels(SympAct_Lab_Pos$TransScore2F)))+
  scale_y_continuous(limits=c(0,RoundTo(max(summary(SympAct_Lab_Pos$TransScore2F)), 10, ceiling)))+
  geom_text(stat='count', aes(label=..count..), vjust=-.60, size = 3)+
  labs(x="Infectiousness Score 2", y="Patient Count")+
  SMTheme1
ggsave(file = "5 Results/Figures/InfectScore2LabBarChart.tiff",pA, dpi=320, compression = "lzw")


# Lab flu Dx TransScore2 and ActivityLevel 
pB<- ggplot(SympAct_Lab_Pos, aes(x=TransScore2F, y=ActivityLevel))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_y_continuous(limits = c(0,10), breaks=seq(from=0, to=10, by=2))+
  scale_x_discrete(breaks=0:(length(TransLabSM2_xlabs)-1), labels=TransLabSM2_xlabs)+
  labs(x="Infectiousness Score 2", y="Activity Level")+
  SMTheme1
ggsave(file = "5 Results/Figures/ActivityVSTransScore2_Lab.tiff",pB, dpi=320, compression = "lzw")

# Lab flu Dx TransScore2 and Morbidity score 
pC<-ggplot(SympAct_Lab_Pos, aes(x=ImpactScoreF, y=TransScore2))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_x_discrete(breaks=0:(length(ImpactLab_xlabs)-1), labels=ImpactLab_xlabs)+
  labs(y="Infectiousness Score 2", x="Morbidity Score")+
  SMTheme1+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.85))

ggsave(file = "5 Results/Figures/TransScore2VSImpactScore_Lab.tiff",pC, dpi=320, compression = "lzw")

#Set window size for the two combined figures
graphics.off() #close all graphics windows

ww=8; wh=3*(2/3*ww); #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Make a combined figure
pA<-pA+ggtitle("A")
pB<-pB+ggtitle("B")
pC<-pC+ggtitle("C")

plotG <- grid.arrange(pA, pB, pC, nrow = 3)

ggsave(file="5 Results/Figures/SM_TransScore2_Lab.tiff", plot=plotG, dpi = 320, compression = "lzw")

graphics.off(); #close all graphics windows


# Infect score without chest and nasal congestion

ww=8; wh=2/3*ww; #window size, can be adjusted
windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Barchart of infectiousness score 3
pA<-ggplot(SympAct_Lab_Pos, aes(TransScore3))+ 
  geom_bar()+
  scale_x_continuous(breaks=min(levels(SympAct_Lab_Pos$TransScore3F)):max(levels(SympAct_Lab_Pos$TransScore3F)))+
  scale_y_continuous(limits=c(0,RoundTo(max(summary(SympAct_Lab_Pos$TransScore3F)), 20, ceiling)))+
  geom_text(stat='count', aes(label=..count..), vjust=-.60)+
  labs(x="Infectiousness Score 3", y="Patient Count")+
  SMTheme1
ggsave(file = "5 Results/Figures/InfectScore3LabBarChart.tiff",pA, dpi=320, compression = "lzw")


# Lab flu Dx TransScore3 and ActivityLevel for SM
pB<- ggplot(SympAct_Lab_Pos, aes(x=TransScore3F, y=ActivityLevel))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_y_continuous(limits = c(0,10), breaks=seq(from=0, to=10, by=2))+
  scale_x_discrete(breaks=0:(length(TransLabSM3_xlabs)-1), labels=TransLabSM3_xlabs)+
  labs(x="Infectiousness Score 3", y="Activity Level")+
  SMTheme1
ggsave(file = "5 Results/Figures/ActivityVSTransScore3_Lab.tiff",pB, dpi=320, compression = "lzw")

# Lab flu Dx TransScore3 and Morbidity score 
pC<-ggplot(SympAct_Lab_Pos, aes(x=ImpactScoreF, y=TransScore3))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_x_discrete(breaks=0:(length(ImpactLab_xlabs)-1), labels=ImpactLab_xlabs)+
  labs(y="Infectiousness Score 3", x="Morbidity Score")+
  SMTheme1+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.85))

ggsave(file = "5 Results/Figures/TransScore3VSImpactScore_Lab.tiff",pC, dpi=320, compression = "lzw")

#Set window size for the two combined figures
graphics.off() #close all graphics windows

ww=8; wh=3*(2/3*ww); #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Make a combined figure
pA<-pA+ggtitle("A")
pB<-pB+ggtitle("B")
pC<-pC+ggtitle("C")

plotG <- grid.arrange(pA, pB, pC, nrow = 3)

ggsave(file="5 Results/Figures/SM_TransScore3_Lab.tiff", plot=plotG, dpi = 320, compression = "lzw")

graphics.off(); #close all graphics windows

# Score based on .9 and .75 correlation cut-off

ww=8; wh=2/3*ww; #window size, can be adjusted
windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Barchart of infectiousness score
pA<-ggplot(SympAct_Lab_Pos, aes(TransScore4))+ 
  geom_bar()+
  scale_x_continuous(breaks=min(levels(SympAct_Lab_Pos$TransScore4F)):max(levels(SympAct_Lab_Pos$TransScore4F)))+
  scale_y_continuous(limits=c(0,RoundTo(max(summary(SympAct_Lab_Pos$TransScore4F)), 10, ceiling)))+
  geom_text(stat='count', aes(label=..count..), vjust=-.60)+
  labs(x="Infectiousness Score 4", y="Patient Count")+
  SMTheme1
ggsave(file = "5 Results/Figures/InfectScore4LabBarChart.tiff",pA, dpi=320, compression = "lzw")


# Lab flu Dx TransScore4 and ActivityLevel for SM
pB<- ggplot(SympAct_Lab_Pos, aes(x=TransScore4F, y=ActivityLevel))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_y_continuous(limits = c(0,10), breaks=seq(from=0, to=10, by=2))+
  scale_x_discrete(breaks=0:(length(TransLabSM4_xlabs)-1), labels=TransLabSM4_xlabs)+
  labs(x="Infectiousness Score 4", y="Activity Level")+
  SMTheme1
ggsave(file = "5 Results/Figures/ActivityVSTransScore4_Lab.tiff",pB, dpi=320, compression = "lzw")

# Lab flu Dx TransScore4 and Morbidity score 
pC<-ggplot(SympAct_Lab_Pos, aes(x=ImpactScoreF, y=TransScore4))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=3)+
  scale_x_discrete(breaks=0:(length(ImpactLab_xlabs)-1), labels=ImpactLab_xlabs)+
  labs(y="Infectiousness Score 4", x="Morbidity Score")+
  SMTheme1+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.85))

ggsave(file = "5 Results/Figures/TransScore4VSImpactScore_Lab.tiff",pC, dpi=320, compression = "lzw")

#Set window size for the two combined figures
graphics.off() #close all graphics windows

ww=8; wh=3*(2/3*ww); #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Make a combined figure
pA<-pA+ggtitle("A")
pB<-pB+ggtitle("B")
pC<-pC+ggtitle("C")

plotG <- grid.arrange(pA, pB, pC, nrow = 3)

ggsave(file="5 Results/Figures/SM_TransScore4_Lab.tiff", plot=plotG, dpi = 320, compression = "lzw")

graphics.off(); #close all graphics windows


#--------------------------------------------------------------------------
# SM Plots for Lab Dx Trans Score vs Cough Intensity  
#--------------------------------------------------------------------------
ww=8; wh=2/3*ww; #window size, can be adjusted
windows(width=ww, height=wh) #for windows: opens window of the specified dimensions


# Trans Score 1
p<-ggplot(SympAct_Lab_Pos, aes(y=TransScore1, x=CoughIntensity))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=2))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$CoughIntensity)," (n=", table(SympAct_Lab_Pos$CoughIntensity), ")"))+
  labs(y="Infectiousness Score", x="Cough Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/TransScorevsCoughIntensity_Lab_Score1.tiff",p, dpi=320, compression = "lzw")


# Trans Score 2
p<-ggplot(SympAct_Lab_Pos, aes(y=TransScore2, x=CoughIntensity))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$CoughIntensity)," (n=", table(SympAct_Lab_Pos$CoughIntensity), ")"))+
  labs(y="Infectiousness Score 2", x="Cough Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/TransScorevsCoughIntensity_Lab_Score2.tiff",p, dpi=320, compression = "lzw")


# Trans Score 3
p<-ggplot(SympAct_Lab_Pos, aes(y=TransScore3, x=CoughIntensity))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$CoughIntensity)," (n=", table(SympAct_Lab_Pos$CoughIntensity), ")"))+
  labs(y="Infectiousness Score 3", x="Cough Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/TransScorevsCoughIntensity_Lab_Score3.tiff",p, dpi=320, compression = "lzw")

# Trans Score 4
p<-ggplot(SympAct_Lab_Pos, aes(y=TransScore4, x=CoughIntensity))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$CoughIntensity)," (n=", table(SympAct_Lab_Pos$CoughIntensity), ")"))+
  labs(y="Infectiousness Score 4", x="Cough Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/TransScorevsCoughIntensity_Lab_Score4.tiff",p, dpi=320, compression = "lzw")




#--------------------------------------------------------------------------
# SM Plots for Lab Dx Morbidity Score vs Weakness and Body aches Intensity  
#--------------------------------------------------------------------------


# Morbidity Score Main Text Weakness
p<-ggplot(SympAct_Lab_Pos, aes(y=ImpactScore, x=Weakness))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=2))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$Weakness)," (n=", table(SympAct_Lab_Pos$Weakness), ")"))+
  labs(y="Morbidity Score", x="Weakness Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/ImpactScorevsWeakness_Lab_Score1.tiff",p, dpi=320, compression = "lzw")


# Morbidity Score Main Text Body Aches
p<-ggplot(SympAct_Lab_Pos, aes(y=ImpactScore, x=Myalgia))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$Myalgia)," (n=", table(SympAct_Lab_Pos$Myalgia), ")"))+
  labs(y="Morbidity Score", x="Body Ache Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/ImpactScorevsMyalgia_Lab_Score1.tiff",p, dpi=320, compression = "lzw")


# Morbidity Score 0.9 correlation cut off Weakness
p<-ggplot(SympAct_Lab_Pos, aes(y=ImpactScore2, x=Weakness))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=2))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$Weakness)," (n=", table(SympAct_Lab_Pos$Weakness), ")"))+
  labs(y="Morbidity Score (0.9 cut off)", x="Weakness Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/ImpactScore2vsWeakness_Lab_Score2.tiff",p, dpi=320, compression = "lzw")


# Morbidity Score 0.9 correlation cut off Body Aches
p<-ggplot(SympAct_Lab_Pos, aes(y=ImpactScore2, x=Myalgia))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$Myalgia)," (n=", table(SympAct_Lab_Pos$Myalgia), ")"))+
  labs(y="Morbidity Score (0.9 cut off)", x="Body Ache Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/ImpactScore2vsMyalgia_Lab_Score2.tiff",p, dpi=320, compression = "lzw")


# Morbidity Score 0.75 correlation cut off Weakness
p<-ggplot(SympAct_Lab_Pos, aes(y=ImpactScore3, x=Weakness))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=2))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$Weakness)," (n=", table(SympAct_Lab_Pos$Weakness), ")"))+
  labs(y="Morbidity Score (0.75 cut off)", x="Weakness Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/ImpactScore3vsWeakness_Lab_Score3.tiff",p, dpi=320, compression = "lzw")


# Morbidity Score 0.75 correlation cut off Body Aches
p<-ggplot(SympAct_Lab_Pos, aes(y=ImpactScore3, x=Myalgia))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(labels=paste0(levels(SympAct_Lab_Pos$Myalgia)," (n=", table(SympAct_Lab_Pos$Myalgia), ")"))+
  labs(y="Morbidity Score (0.75 cut off)", x="Body Ache Intensity")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/ImpactScore3vsMyalgia_Lab_Score3.tiff",p, dpi=320, compression = "lzw")

#-------------------------------------------------------------------------- 
# Main Text Analysis For ANY diagnosis (PCR, Rapid, or Empirically)
#--------------------------------------------------------------------------
#summary(SympAct_Any_Pos)



# Barchart of activity for Any Diagnosis
p1<-ggplot(SympAct_Any_Pos, aes(ActivityLevel))+
  geom_bar()+
  scale_x_continuous(breaks=0:10)+
  geom_text(stat='count', aes(label=..count..), vjust=-.60)+
  manuscriptTheme
ggsave(file = "5 Results/Figures/ActivityAnyBarChart.tiff",p1, dpi=320, compression = "lzw")

# Barchart of infectiousness score for Any Diagnosis
pA<-ggplot(SympAct_Any_Pos, aes(TransScore1))+
  geom_bar()+
  scale_x_continuous(breaks=min(levels(SympAct_Any_Pos$TransScore1F)):max(levels(SympAct_Any_Pos$TransScore1F)))+
  geom_text(stat='count', aes(label=..count..), vjust=-.30)+
  labs(x="Infectiousness Score", y="Patient Count")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/InfectScoreAnyBarChart.tiff",pA, dpi=320, compression = "lzw")

# Barchart of morbidity score for Any Diagnosis
pB<-ggplot(SympAct_Any_Pos, aes(ImpactScore))+
  geom_bar()+
  scale_x_continuous(limits = c(as.numeric(min(levels(SympAct_Any_Pos$ImpactScoreF))),
                                as.numeric(length(levels(SympAct_Any_Pos$ImpactScoreF))-1)),
                     breaks=min(levels(SympAct_Any_Pos$ImpactScoreF)):(length(levels(SympAct_Any_Pos$ImpactScoreF))-1))+
  geom_text(stat='count', aes(label=..count..), vjust=-.30)+
  labs(x="Morbidity Score", y="Patient Count")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/MorbScoreAnyBarChart.tiff",pB, dpi=320, compression = "lzw")

#Set window size for the two combined figures 
graphics.off(); #close all graphics windows

ww=8; wh=2*(2/3*ww); #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions

# Make a combined figure
pA<-pA+ggtitle("A")
pB<-pB+ggtitle("B")

plotG <- grid.arrange(pA, pB, nrow = 2)

ggsave(file="5 Results/Figures/IandMScoresAnyBarChart.tiff", plot=plotG, dpi = 320, compression = "lzw")


#Set window size for all the following figures 
graphics.off(); #close all graphics windows

ww=8; wh=2/3*ww; #window size, can be adjusted

windows(width=ww, height=wh) #for windows: opens window of the specified dimensions


# Any flu Dx TransScore and ActivityLevel
p1<- ggplot(SympAct_Any_Pos, aes(x=TransScore1F, y=ActivityLevel))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_y_continuous(limits = c(0,10), breaks=seq(from=0, to=10, by=2))+
  scale_x_discrete(breaks=0:(length(TransAny_xlabs)-1), labels=TransAny_xlabs)+
  labs(x="Infectiousness Score", y="Activity Level")+
  manuscriptTheme
ggsave(file = "5 Results/Figures/ActivityVSTransScore_Any.tiff",p1, dpi=320, compression = "lzw")

# Any flu Dx ImpactScore and ActivityLevel
p1<- ggplot(SympAct_Any_Pos, aes(x=ImpactScoreF, y=ActivityLevel))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_y_continuous(limits = c(0,10), breaks=seq(from=0, to=10, by=2))+
  scale_x_discrete(breaks=0:(length(ImpactAny_xlabs)-1), labels=ImpactAny_xlabs)+
  labs(x="Morbidity Score", y="Activity Level")+
  manuscriptTheme+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 0.75, vjust = 0.85))
ggsave(file = "5 Results/Figures/ActivityVSImpactScore_Any.tiff",p1, dpi=320, compression = "lzw")

# Any flu Dx ImpactScore and TransScore
p1<-ggplot(SympAct_Any_Pos, aes(x=ImpactScoreF, y=TransScore1))+
  geom_boxplot()+
  stat_smooth(method = 'lm', se=T, aes(group=1))+
  geom_jitter(height = 0, width = .3, size = 1, shape=1)+
  stat_summary(fun.y = mean, geom = "point", shape = 18, color="red", size=5)+
  scale_x_discrete(breaks=0:(length(ImpactAny_xlabs)-1), labels=ImpactAny_xlabs)+
  labs(y="Infectiousness Score", x="Morbidity Score")+
  manuscriptTheme+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 0.75, vjust = 0.85))

ggsave(file = "5 Results/Figures/TransScoreVSImpactScore_Any.tiff",p1, dpi=320, compression = "lzw")

#close all graphics windows
graphics.off() 





# Load packages
require(nlme)
require(pastecs)
require(lme4)
require(lsmeans)
require(MuMIn)
require(Rcmdr)
require(ggplot2)
require(readxl)
require(lawstat)
require(Hmisc)
require(tidyverse)
require(GGally)
require(dplyr)
require(reshape2)

# clear workspace
rm(list = ls(all = TRUE))


# load data file
setwd
ME13data<-read.csv("ME13.csv",header=T)

#subset to select the difference scores from the dataframe and check
ME13data_s<-subset (ME13data, ME13data$condition =="IMRE_diff" | ME13data$condition =="SPSN_diff")
head(ME13data_s)
tail(ME13data_s)
dim(ME13data_s)


# set contrasts: age group, YA reference level
ME13data_s$group<-factor(ME13data_s$group, level=c("3","1", "2", "4", "5", "6"))
class(ME13data_s$group)

#check
ME13data_s$group



 
#Articulation rate -sex(F,M), Condition (IM,EM), Age group (1-6)
lmer.sup = lmer(ME13 ~ sex + 
                  condition +
                  group + 
                  sex:group +
                  group:condition +
                  sex:condition +
                  sex:condition:group
                + (1|part), data=ME13data_s)


#summary(lmer.sup)

m1<-update(lmer.sup, .~. - sex:condition:group)
m2<-update(m1, .~. - sex:condition)
m3<-update(m2, .~. - group:condition)
m4<-update(m3, .~. - sex:group)
m5<-update(m4, .~. - group)
m6<-update(m5, .~. - condition)
anova(lmer.sup, m1, m2, m3, m4, m5, m6)
summary (m6)

m7<-update(m5, .~. - sex)
anova(m7, m6)


summary(m5)
R2_BestFit <-r.squaredGLMM(m5)
R2_BestFit



# descriptives


tapply(ME13data_s$ME13, ME13data_s$condition, summary)


#re-level age
ME13data_s$group<-factor(ME13data_s$group, 
                        levels = c(1, 2, 3, 4, 5, 6),
                        labels = c("CH-Y", "CH-O", "YA", "MA-Y", "MA-O", "OA"))



# plot interaction (m4)

library("ggpubr")

ME13data_s %>%
  ggplot(aes(x=group, y=ME13, fill=condition)) + geom_boxplot() + ylab("ME13") 

  







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
ARTdata<-read.csv("articulationrate.csv",header=T)

#subset to select the difference scores from the dataframe and check
ARTdata_s<-subset (ARTdata, ARTdata$condition =="IMRE_diff" | ARTdata$condition =="SPSN_diff")
head(ARTdata_s)
tail(ARTdata_s)
dim(ARTdata_s)


# set contrasts: age group, YA reference level
ARTdata_s$group<-factor(ARTdata_s$group, level=c("3","1", "2", "4", "5", "6"))
class(ARTdata_s$group)

#check
ARTdata_s$group



 
#Articulation rate -sex(F,M), Condition (IM,EM), Age group (1-6)
lmer.sup = lmer(rate ~ sex + 
                  condition +
                  group + 
                  sex:group +
                  group:condition +
                  sex:condition +
                  sex:condition:group
                + (1|part), data=ARTdata_s)


#summary(lmer.sup)

m1<-update(lmer.sup, .~. - sex:condition:group)
m2<-update(m1, .~. - sex:condition)
m3<-update(m2, .~. - group:condition)
#anova(lmer.sup, m1, m2, m3)
m4<-update(m3, .~. - sex:group)
anova(lmer.sup, m1, m2, m3, m4)
summary(m3)

m5<-update(m4, .~. - condition)
anova(m4, m5)


#Use R square as an index of goodness of fit for the selected model
m1<-update(lmer.sup, .~. - sex:condition:group)
m2<-update(m1, .~. - sex:condition)
m3<-update(m2, .~. - group:condition)
m4<-update(m3, .~. - sex:group)
m5<-update(m4, .~. - group)
m6<-update(m5, .~. - condition)
m7<-update(m6, .~. - sex)
anova(lmer.sup, m1, m2, m3, m4, m5, m6, m7)


R2_BestFit <-r.squaredGLMM(m3)
R2_BestFit

R2_BestFit <-r.squaredGLMM(m4)
R2_BestFit

R2_BestFit <-r.squaredGLMM(m6)
R2_BestFit

# Decompose the interaction sex and age group

m1<-update(lmer.sup, .~. - sex:condition:group)
m2<-update(m1, .~. - sex:condition)
m3<-update(m2, .~. - group:condition)
#anova(lmer.sup, m1, m2, m3)
m4<-update(m3, .~. - sex:group)
anova(lmer.sup, m1, m2, m3, m4)
summary(m3)

require(emmeans)
emmeans(m3, pairwise ~ group | sex)
emmeans(m3, pairwise ~ sex | group)

#print(emmeans(m3, pairwise ~ sex | group ))


# subset further for looking at interactions

m<- subset(ARTdata_s, sex == "M")
f<- subset(ARTdata_s, sex == "F")


# one-way anova for female
res.aovf <- aov(rate ~ group, data = f)
# Summary of the analysis
summary(res.aovf)

# one-way anova for male
res.aovm <- aov(rate ~ group, data = m)
# Summary of the analysis
summary(res.aovm)

# descriptives

tapply(ARTdata_s$rate, ARTdata_s$group, summary)
tapply(ARTdata_s$rate, ARTdata_s$sex, summary)

#re-level age 
ARTdata_s$group<-factor(ARTdata_s$group, 
                        levels = c(1, 2, 3, 4, 5, 6),
                        labels = c("CH-Y", "CH-O", "YA", "MA-Y", "MA-O", "OA"))



# plot interaction (m3)

library("ggpubr")

ARTdata_s %>%
  ggplot(aes(x=group, y=rate, fill=sex)) + geom_boxplot() + ylab("Articulation rate relative to NORM") 
  


ARTdata_s %>%
  ggplot(aes(x=group, y=rate, fill=sex)) + geom_boxplot(alpha=0.5) + ylab("Articulation rate relative to NORM") + xlab("Age group") + theme_bw(base_size = 16) +
  scale_fill_manual(values = c("#999999", "#FFFFFF")) + scale_y_continuous(breaks=seq(-2, 2, by =0.5), limits = c(-1.5,1)) + geom_hline(yintercept = 0.0, linetype = "dashed") + theme (legend.position = "none")






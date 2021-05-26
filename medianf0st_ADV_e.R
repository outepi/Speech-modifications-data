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
F0data<-read.csv("f0median.csv",header=T)

#subset to select the difference scores from the dataframe and check
F0data_s<-subset (F0data, F0data$condition =="IMRE_diff" | F0data$condition =="SPSN_diff")
head(F0data_s)
tail(F0data_s)
dim(F0data_s)


# set contrasts: age group, YA reference level
F0data_s$group<-factor(F0data_s$group, level=c("3","1", "2", "4", "5", "6"))
class(F0data_s$group)

#check
F0data_s$group



 
#Articulation rate -sex(F,M), Condition (IM,EM), Age group (1-6)
lmer.sup = lmer(f0 ~ sex + 
                  condition +
                  group + 
                  sex:group +
                  group:condition +
                  sex:condition +
                  sex:condition:group
                + (1|part), data=F0data_s)


#summary(lmer.sup)

m1<-update(lmer.sup, .~. - sex:condition:group)
m2<-update(m1, .~. - sex:condition)
m3<-update(m2, .~. - group:condition)
anova(lmer.sup, m1, m2, m3)
m4<-update(m2, .~. - sex:group)
m7<-update(m4, .~. - sex)
anova(lmer.sup, m1, m2, m4, m7)

summary(m4)
R2_BestFit <-r.squaredGLMM(m4)
R2_BestFit

#Use R square as an index of goodness of fit for the selected model
m1<-update(lmer.sup, .~. - sex:condition:group)
m2<-update(m1, .~. - sex:condition)
m3<-update(m2, .~. - group:condition)
m4<-update(m3, .~. - sex:group)
m5<-update(m4, .~. - group)
m6<-update(m5, .~. - condition)
m7<-update(m6, .~. - sex)
anova(lmer.sup, m1, m2, m3, m4, m5, m6, m7)


R2_BestFit <-r.squaredGLMM(m4)
R2_BestFit

R2_BestFit <-r.squaredGLMM(m5)
R2_BestFit

R2_BestFit <-r.squaredGLMM(m6)
R2_BestFit

# Decompose the interaction sex and age group

#summary(lmer.sup)

m1<-update(lmer.sup, .~. - sex:condition:group)
m2<-update(m1, .~. - sex:condition)
m3<-update(m2, .~. - group:condition)
anova(lmer.sup, m1, m2, m3)
m4<-update(m2, .~. - sex:group)
m7<-update(m4, .~. - sex)
anova(lmer.sup, m1, m2, m4, m7)

require(emmeans)
emmeans(m4, pairwise ~ group | condition)
emmeans(m4, pairwise ~ sex | condition)
emmeans(m4, pairwise ~ condition | group)
emmeans(m4, pairwise ~ condition | sex)


# descriptives

tapply(F0data_s$f0, F0data_s$group, summary)
tapply(F0data_s$f0, F0data_s$sex, summary)
tapply(F0data_s$f0, F0data_s$condition, summary)


#re-level age
F0data_s$group<-factor(F0data_s$group, 
                        levels = c(1, 2, 3, 4, 5, 6),
                        labels = c("8-12 years", "13-17 years", "18-34 years", "35-49 years", "50-64 years", "65-80 years"))



# plot interaction (m4)

library("ggpubr")



F0data_s %>%
  ggplot(aes(x=group, y=f0, fill=condition)) + geom_boxplot(alpha=0.5) + ylab("Median f0 relative to QUIET") + xlab("Age group") + theme_bw(base_size = 16) +
  scale_fill_manual(values = c("#999999", "#FFFFFF")) + scale_y_continuous(breaks=seq(-1, 8, by = 2), limits = c(-1.0,8)) + geom_hline(yintercept = 0.0, linetype = "dashed") + theme (legend.position = "none")







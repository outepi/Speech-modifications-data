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


# clear workspace
rm(list = ls(all = TRUE))


# load data file
setwd
T1Data<-read.csv("T1.csv",header=T)

dim(T1Data)
tail(T1Data)
head(T1Data)

# set contrasts: condition
T1Data$condition<-factor(T1Data$condition, level=c("NORM_Time_per_diff", "IMRE_Time_per_diff", "SPSN_Time_per_diff"))
contrasts(T1Data$condition)


# set contrasts: age group, YA reference level
T1Data$age_band<-factor(T1Data$age_band, level=c("3","1", "2", "4", "5", "6"))
# adult is now reference level
contrasts(T1Data$age_band)


T1Data$PART<-factor(T1Data$PART)


#Time-1 -sex(F,M), Condition (IM,EM, NORM), Age group (1-6)


lmer.sup = lmer(time1 ~ 
                  condition +
                  age_band + 
                  age_band:condition +
                + (1|PART), data=T1Data)

#lmer.sup


m1<-update(lmer.sup, .~. - condition:age_band)
m2<-update(m1, .~. - condition)
m3<-update(m1, .~. - age_band)

anova(lmer.sup, m1, m2, m3)


# Use R square as an index of goodness of fit for the selected model (m1-m7)
R2_BestFit <-r.squaredGLMM(m1)
R2_BestFit

#interaction
require(emmeans)
emmeans(m1, pairwise ~ age_band | condition)
emmeans(m1, pairwise ~ condition | age_band)


# descriptives

tapply(T1Data$time1, T1Data$age_band, summary)
tapply(T1Data$time1, T1Data$condition, summary)




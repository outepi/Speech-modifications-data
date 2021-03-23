#load packages
require(psych)
require(car)
require(ggplot2)
require(reshape)
require(nlme)
require(lme4)
require(MCMCglmm)
require(sabreR)
require(beeswarm)
require(grDevices)
require(pastecs)
library(grid)
require(multcomp)


# clear workspace
rm(list = ls(all = TRUE))


# load data file
setwd
ME13data<-read.csv("ME13.csv",header=T)


#subset to select the difference scores from the dataframe and check
ME13data_norm<-subset (ME13data, ME13data$condition =="NORM_ME13") 
head(ME13data_norm)
tail(ME13data_norm)
dim(ME13data_norm)


# check data file
dim(ME13data_norm)
tail(ME13data_norm)
head(ME13data_norm)

ME13data_norm$sex<-as.factor(ME13data_norm$sex)
ME13data_norm$part<-as.factor(ME13data_norm$part)

# set contrasts: age group, YA reference level
ME13data_norm$group<-factor(ME13data_norm$group, level=c("3","1", "2", "4", "5", "6"))
class(ME13data_norm$group)

#anova
res.anova <- aov(ME13 ~ group * sex, data = ME13data_norm)
summary(res.anova)

model.tables(res.anova, type="means", se = TRUE)

tapply(ME13data_norm$ME13, ME13data_norm$group, summary)
tapply(ME13data_norm$ME13, ME13data_norm$sex, summary)

TukeyHSD(res.anova, which = "group")







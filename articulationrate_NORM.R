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
ARTdata<-read.csv("articulationrate.csv",header=T)



#subset to select the difference scores from the dataframe and check
ARTdata_norm<-subset (ARTdata, ARTdata$condition =="NORM_artrate") 
head(ARTdata_norm)
tail(ARTdata_norm)
dim(ARTdata_norm)



ARTdata_norm$part<-as.factor(ARTdata_norm$part)

# set contrasts: age group, YA reference level
ARTdata_norm$group<-factor(ARTdata_norm$group, level=c("3","1", "2", "4", "5", "6"))
class(ARTdata_norm$group)

#anova
res.anova <- aov(rate ~ group * sex, data = ARTdata_norm)
summary(res.anova)

model.tables(res.anova, type="means", se = TRUE)

tapply(ARTdata_norm$rate, ARTdata_norm$group, summary)
tapply(ARTdata_norm$rate, ARTdata_norm$sex, summary)

TukeyHSD(res.anova, which = "group")






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
F0data<-read.csv("f0median.csv",header=T)


#subset to select the difference scores from the dataframe and check
F0data_norm<-subset (F0data, F0data$condition =="NORM_medianf0st") 
head(F0data_norm)
tail(F0data_norm)
dim(F0data_norm)


F0data_norm$part<-as.factor(F0data_norm$part)

# set contrasts: age group, YA reference level
F0data_norm$group<-factor(F0data_norm$group, level=c("3","1", "2", "4", "5", "6"))


#anova
res.anova <- aov(f0 ~ group * sex, data = F0data_norm)
summary(res.anova)

model.tables(res.anova, type="means", se = TRUE)

tapply(F0data_norm$f0, F0data_norm$group, summary)
tapply(F0data_norm$f0, F0data_norm$sex, summary)

TukeyHSD(res.anova, which = "group")


# Decompose the interaction and subset data

#re-level age subset and perform t-tests
F0data_norm$group<-factor(F0data_norm$group, 
                       levels = c(1, 2, 3, 4, 5, 6),
                       labels = c("CH-Y", "CH-O", "YA", "MA-Y", "MA-O", "OA"))


CHYF<-subset(F0data_norm, sex == "F" & group == "CH-Y")
CHYM<-subset(F0data_norm, sex == "M" & group == "CH-Y")
CHOF<-subset(F0data_norm, sex == "F" & group == "CH-O")
CHOM<-subset(F0data_norm, sex == "M" & group == "CH-O")
YAF<-subset(F0data_norm, sex == "F" & group == "YA")
YAM<-subset(F0data_norm, sex == "M" & group == "YA")
MAYF<-subset(F0data_norm, sex == "F" & group == "MA-Y")
MAYM<-subset(F0data_norm, sex == "M" & group == "MA-Y")
MAOF<-subset(F0data_norm, sex == "F" & group == "MA-O")
MAOM<-subset(F0data_norm, sex == "M" & group == "MA-O")
OAF<-subset(F0data_norm, sex == "F" & group == "OA")
OAM<-subset(F0data_norm, sex == "M" & group == "OA")




t.test(CHYF$f0,CHYM$f0)
t.test(CHOF$f0,CHOM$f0)
t.test(YAF$f0,YAM$f0)
t.test(MAYF$f0,MAYM$f0)
t.test(MAOF$f0,MAOM$f0)
t.test(OAF$f0,OAM$f0)

# plot interaction (m3) with steps of 6 st 

library("ggpubr")


F0data_norm %>%
ggplot(aes(x=group, y=f0, fill=sex)) + geom_boxplot(alpha=0.5) + ylab("Median f0 (semitones re 1 Hz)") + xlab("Age group") + theme_bw(base_size = 16) +
  scale_fill_manual(values = c("#999999", "#FFFFFF")) + scale_y_continuous(breaks=seq(72, 108, by = 6)) + theme (legend.position = "none")



# clear workspace
rm(list = ls(all = TRUE))


# load data file and check
setwd
corrData<-read.csv("vocaleffort.csv",header=T)

dim(corrData)
tail(corrData)
head(corrData)

#correlate f0 and ME13 (=vocal effort) across all age groups IM
res <- cor.test(corrData$IMRE_diff_f0_median, corrData$IMRE_diff_ME13, 
                method = "pearson")
res

#correlate f0 and ME13 (=vocal effort) across all age groups EM
res <- cor.test(corrData$SPSN_diff_f0_median, corrData$SPSN_diff_ME13, 
                method = "pearson")
res



#rename
m<- corrData[grep("^M", corrData$sex),]
f<-corrData[grep("^F",corrData$sex),]
chY<- corrData[grep("^1", corrData$group),]
chO<- corrData[grep("^2", corrData$group),]
YA<- corrData[grep("^3", corrData$group),]
MAY<-corrData[grep("^4", corrData$group),]
MAO<- corrData[grep("^5", corrData$group),]
OA<- corrData[grep("^6", corrData$group),]

#IM per group correlations for vocal effort (f0 and ME13)

res_chY <- cor.test(chY$IMRE_diff_f0_median, chY$IMRE_diff_ME13, 
                method = "pearson")
res_chY

res_chO <- cor.test(chO$IMRE_diff_f0_median, chO$IMRE_diff_ME13, 
                    method = "pearson")
res_chO

res_YA <- cor.test(YA$IMRE_diff_f0_median, YA$IMRE_diff_ME13, 
                    method = "pearson")
res_YA

res_MAY <- cor.test(MAY$IMRE_diff_f0_median, MAY$IMRE_diff_ME13, 
                    method = "pearson")
res_MAY

res_MAO <- cor.test(MAO$IMRE_diff_f0_median, MAO$IMRE_diff_ME13, 
                    method = "pearson")
res_MAO

res_OA <- cor.test(OA$IMRE_diff_f0_median, OA$IMRE_diff_ME13, 
                    method = "pearson")
res_OA

#plotting IM per age group

library("ggpubr")
ggscatter(chY, x = "IMRE_diff_f0_median", y = "IMRE_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")

library("ggpubr")
ggscatter(chO, x = "IMRE_diff_f0_median", y = "IMRE_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")

library("ggpubr")
ggscatter(YA, x = "IMRE_diff_f0_median", y = "IMRE_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")

library("ggpubr")
ggscatter(MAY, x = "IMRE_diff_f0_median", y = "IMRE_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")
library("ggpubr")
ggscatter(MAO, x = "IMRE_diff_f0_median", y = "IMRE_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")

library("ggpubr")
ggscatter(OA, x = "IMRE_diff_f0_median", y = "IMRE_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")





#EM per group correlations for vocal effort (f0 and ME13)

res_chY <- cor.test(chY$SPSN_diff_f0_median, chY$SPSN_diff_ME13, 
                    method = "pearson")
res_chY

res_chO <- cor.test(chO$SPSN_diff_f0_median, chO$SPSN_diff_ME13, 
                    method = "pearson")
res_chO

res_YA <- cor.test(YA$SPSN_diff_f0_median, YA$SPSN_diff_ME13, 
                   method = "pearson")
res_YA

res_MAY <- cor.test(MAY$SPSN_diff_f0_median, MAY$SPSN_diff_ME13, 
                   method = "pearson")
res_MAY

res_MAO <- cor.test(MAO$SPSN_diff_f0_median, MAO$SPSN_diff_ME13, 
                    method = "pearson")
res_MAO

res_OA <- cor.test(OA$SPSN_diff_f0_median, OA$SPSN_diff_ME13, 
                    method = "pearson")
res_OA

#plotting EM per age group

library("ggpubr")
ggscatter(chY, x = "SPSN_diff_f0_median", y = "SPSN_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")

library("ggpubr")
ggscatter(chO, x = "SPSN_diff_f0_median", y = "SPSN_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")

library("ggpubr")
ggscatter(YA, x = "SPSN_diff_f0_median", y = "SPSN_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")

library("ggpubr")
ggscatter(MAY, x = "SPSN_diff_f0_median", y = "SPSN_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")
library("ggpubr")
ggscatter(MAO, x = "SPSN_diff_f0_median", y = "SPSN_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")

library("ggpubr")
ggscatter(OA, x = "SPSN_diff_f0_median", y = "SPSN_diff_ME13", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "f0", ylab = "ME13")






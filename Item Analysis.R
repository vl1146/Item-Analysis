#######Part 1 - Item Analysis for Conscientiousness Test

data <-  read.csv("")

library(dplyr)
library(sjPlot)
library(ggcorrplot)

#check item dif, item discrim, and Cronbach's of data

tab_itemscale(data)

#interitem corr

ggcorrplot(data, type ="lower", lab = TRUE)

#distribution of scores

datat <- data %>% mutate(Total = rowSums(.))
hist(datat$Total)

#remove Q7 and see changes

data2 <- subset(data, select = -Q7)
tab_itemscale(data2)
corr1 <- cor(apply(data2,2,as.numeric))
ggcorrplot(corr1, type ="lower", lab = TRUE)

#remove Q2

data3 <- subset(data2, select = -Q2)
tab_itemscale(data3)
corr2 <- cor(apply(data3,2,as.numeric))
ggcorrplot(corr2, type ="lower", lab = TRUE)

#remove Q16

data4 <- subset(data3, select = -Q16)
tab_itemscale(data4)
corr3 <- cor(apply(data4,2,as.numeric))
ggcorrplot(corr3, type ="lower", lab = TRUE)

#remove Q11

data5 <- subset(data4, select = -Q11)
tab_itemscale(data5)
corr4 <- cor(apply(data5,2,as.numeric))
ggcorrplot(corr4, type ="lower", lab = TRUE)

#remove Q3

data6 <- subset(data5, select = -Q3)
tab_itemscale(data6)
corr5 <- cor(apply(data6,2,as.numeric))
ggcorrplot(corr5, type ="lower", lab = TRUE)

#View distribution and descriptives for remaining items

data6t <- data6 %>% mutate(Total = rowSums(.))
hist(data6t$Total)
mean(data6t$Total)
sd(data6t$Total)
median(data6t$Total)


#CFA to check unidimensionality of test
library(lavaan)

#CFA for all questions

model <- '
Conscientiousness=~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q11+Q12+Q13+Q14+Q15'

fit <- cfa(model,data=data)
summary(fit,fit.measures=TRUE,standardized=TRUE)

#CFA for model chosen

model2 <- '
Conscientiousness=~Q1+Q4+Q5+Q6+Q8+Q9+Q10+Q12+Q13+Q14+Q15'

fit <- cfa(model2,data=data)
summary(fit,fit.measures=TRUE,standardized=TRUE)

#EFA to explore factor structures

data <-  read.csv("C:/Users/Vin/Desktop/Selection/IA Project/datascored.csv")

library(psych)
library(REdaS)
library(dplyr)

fa(data, nfactors = 6, rotate = "oblimin")


#4 major factors identified accounting for 61% of the variance


#Factor 1, 22% of variance explained

F1 <- subset(data, select = c(Q4,Q8,Q9,Q12,Q13,Q14,Q15))
tab_itemscale(F1)
corrF1 <- cor(apply(F1,2,as.numeric))
ggcorrplot(corrF1, type ="lower", lab = TRUE)
F1t <- F1 %>% mutate(Total = rowSums(.))
hist(F1t$Total)


#Factor 2, 18% of variance explained

F2 <- subset(data, select = c(Q3,Q5,Q6,Q16))
tab_itemscale(F2)
corrF2 <- cor(apply(F2,2,as.numeric))
ggcorrplot(corrF2, type ="lower", lab = TRUE)
F2t <- F2 %>% mutate(Total = rowSums(.))
hist(F2t$Total)

#Factor 3, 11% of variance explained

F3 <- subset(data, select = c(Q1,Q10))
tab_itemscale(F3)
corrF3 <- cor(apply(F3,2,as.numeric))
ggcorrplot(corrF3, type ="lower", lab = TRUE)
F3t <- F3 %>% mutate(Total = rowSums(.))
hist(F3t$Total)

#Factor 4, 11% of variance explained

F4 <- subset(data, select = c(Q2,Q11,Q16))
tab_itemscale(F4)
corrF4 <- cor(apply(F4,2,as.numeric))
ggcorrplot(corrF4, type ="lower", lab = TRUE)
F4t <- F4 %>% mutate(Total = rowSums(.))
hist(F4t$Total)


#Combine Factor 1 and Factor 2

F12 <- subset(data, select = c(Q4,Q8,Q9,Q12,Q13,Q14,Q15,Q3,Q5,Q6,Q16))
tab_itemscale(F12)
corrF12 <- cor(apply(F12,2,as.numeric))
ggcorrplot(corrF12, type ="lower", lab = TRUE)


data11 <- subset(F12, select = -Q16)
tab_itemscale(data11)
corr10 <- cor(apply(data11,2,as.numeric))
ggcorrplot(corr10, type ="lower", lab = TRUE)
data10t <- data11 %>% mutate(Total = rowSums(.))

data11t <- data11 %>% mutate(Total = rowSums(.))


#Distribution and descriptives for final result

hist(data11t$Total)
mean(data10t$Total)
sd(data10t$Total)
median(data10t$Total)


#######Part 2 - Concurrent Validation Study

vdata <-  read.csv("C:/Users/Vin/Desktop/Selection/IA Project/validationdata.csv")
str(vdata)

X <- vdata[,6:8]
Y <- vdata[,9:12]
XY <- vdata[,6:12]

library(ggcorrplot)

##Corr Matrix

#Within predictors

Xcorr <- cor(X)
ggcorrplot(Xcorr, type ="lower", lab = TRUE)

#Within criterions

Ycorr <- cor(Y)
ggcorrplot(Ycorr, type ="lower", lab = TRUE)

XYcorr <- cor(XY)
ggcorrplot(XYcorr, type ="lower", lab = TRUE)

#predict performance yr1

pre1 <- lm(vdata$PerformY1~vdata$Cognitive)
summary(pre1)

#sig p <.01 R^2 .1719

pre2 <- lm(vdata$PerformY1~vdata$Conscientiousness)
summary(pre2)

#sig p <.01 R^2 .1384

pre7 <- lm(vdata$PerformY1~vdata$Interview)
summary(pre7)

#sig p <.01 R^2 .1842

pre3 <- lm(vdata$PerformY1~vdata$Cognitive+vdata$Conscientiousness+vdata$Interview)
summary(pre3)

#sig model p < .01, R^2 .269

pre4 <- lm(vdata$PerformY1~vdata$Conscientiousness+vdata$Interview)
summary(pre4)

#sig model p < .01, R^2 .246

pre5 <- lm(vdata$PerformY1~vdata$Conscientiousness+vdata$Cognitive)
summary(pre5)

#sig model p < .01, R^2 .2397

pre6 <- lm(vdata$PerformY1~vdata$Interview+vdata$Cognitive)
summary(pre6)

#sig model p < .01, R^2 .2176


#predict performance yr2

pre1 <- lm(vdata$PerformY2~vdata$Cognitive)
summary(pre1)

#sig model p < .01, R^2 .2715

pre2 <- lm(vdata$PerformY2~vdata$Conscientiousness)
summary(pre2)

#sig model p < .01, R^2 .0764

pre7 <- lm(vdata$PerformY2~vdata$Interview)
summary(pre7)

#sig model p < .01, R^2 .2685

pre3 <- lm(vdata$PerformY2~vdata$Cognitive+vdata$Conscientiousness+vdata$Interview)
summary(pre3)

#sig model p < .01, R^2 .3371

pre4 <- lm(vdata$PerformY2~vdata$Conscientiousness+vdata$Interview)
summary(pre4)

#sig model p < .01, R^2 .2823

pre5 <- lm(vdata$PerformY2~vdata$Conscientiousness+vdata$Cognitive)
summary(pre5)

#sig model p < .01, R^2 .2877

pre6 <- lm(vdata$PerformY2~vdata$Interview+vdata$Cognitive)
summary(pre6)

#sig model p < .01, R^2 .3297

#complaints

pre1 <- lm(vdata$Complaints~vdata$Cognitive)
summary(pre1)

#sig model p < .01, R^2 .1923

pre2 <- lm(vdata$Complaints~vdata$Conscientiousness)
summary(pre2)

#sig model p < .01, R^2 .1539

pre7 <- lm(vdata$Complaints~vdata$Interview)
summary(pre7)

#sig model p < .01, R^2 .2093

pre3 <- lm(vdata$Complaints~vdata$Cognitive+vdata$Conscientiousness+vdata$Interview)
summary(pre3)

#sig model p < .01, R^2 .302

pre4 <- lm(vdata$Complaints~vdata$Conscientiousness+vdata$Interview)
summary(pre4)

#sig model p < .01, R^2 .2771

pre5 <- lm(vdata$Complaints~vdata$Conscientiousness+vdata$Cognitive)
summary(pre5)

#sig model p < .01, R^2 .2675

pre6 <- lm(vdata$Complaints~vdata$Interview+vdata$Cognitive)
summary(pre6)

#sig model p < .01, R^2 .2456

#recommendations

pre1 <- lm(vdata$Commendations~vdata$Cognitive)
summary(pre1)

#sig model p < .01, R^2 .1899

pre2 <- lm(vdata$Commendations~vdata$Conscientiousness)
summary(pre2)

#sig model p < .01, R^2 .0413

pre7 <- lm(vdata$Commendations~vdata$Interview)
summary(pre7)

#sig model p < .01, R^2 .1527

pre3 <- lm(vdata$Commendations~vdata$Cognitive+vdata$Conscientiousness+vdata$Interview)
summary(pre3)

#sig model p < .01, R^2 .2141

pre4 <- lm(vdata$Commendations~vdata$Conscientiousness+vdata$Interview)
summary(pre4)

#sig model p < .01, R^2 .1596

pre5 <- lm(vdata$Commendations~vdata$Conscientiousness+vdata$Cognitive)
summary(pre5)

#sig model p < .01, R^2 .1958

pre6 <- lm(vdata$Commendations~vdata$Interview+vdata$Cognitive)
summary(pre6)

#sig model p < .01, R^2 .2113

#Gender Diff

reg1 <- lm(vdata$Cognitive~vdata$Sex)
summary(reg1)

#no sig p .09

reg2 <- lm(vdata$Conscientiousness~vdata$Sex)
summary(reg2)

#no sig p .46

reg3 <- lm(vdata$Interview~vdata$Sex)
summary(reg3)

#no sig p.11

reg4 <- lm(vdata$PerformY1~vdata$Sex)
summary(reg4)

#no sig p .94

reg5 <- lm(vdata$PerformY2~vdata$Sex)
summary(reg5)

#no sig p .78

reg6 <- lm(vdata$Commendations~vdata$Sex)
summary(reg6)

#no sig .64

reg7 <- lm(vdata$Complaints~vdata$Sex)
summary(reg7)

#no sig p .99

#Race Diff using regression and anova

regr1 <- lm(vdata$Cognitive~vdata$Race)
summary(regr1)

a1 <- aov(vdata$Cognitive~vdata$Race)
summary(a1)
t1 <- TukeyHSD(a1)
t1

#sig effect of race p < .01

regr2 <- lm(vdata$Conscientiousness~vdata$Race)
summary(regr2)

a2 <- aov(vdata$Conscientiousness~vdata$Race)
summary(a2)
t2 <- TukeyHSD(a2)
t2

#sig effect of A, B, & H p < .01

regr3 <- lm(vdata$Interview~vdata$Race)
summary(regr3)

a3 <- aov(vdata$Interview~vdata$Race)
summary(a3)
t3 <- TukeyHSD(a3)
t3

#sig lower performance of B and maybe H (p = .057), sig higher performance of A

regr4 <- lm(vdata$PerformY1~vdata$Race)
summary(regr4)

a4 <- aov(vdata$PerformY1~vdata$Race)
summary(a4)
t4 <- TukeyHSD(a4)
t4

#sig lower performance of B, sig higher performance of A

regr5 <- lm(vdata$PerformY2~vdata$Race)
summary(regr5)

a5 <- aov(vdata$PerformY2~vdata$Race)
summary(a5)
t5 <- TukeyHSD(a5)
t5

#sig lower performance of B, sig higher performance of A

regr6 <- lm(vdata$Commendations~vdata$Race)
summary(regr6)

a6 <- aov(vdata$Commendations~vdata$Race)
summary(a6)
t6 <- TukeyHSD(a6)
t6

#sig higher performance of A, sig lower performance of B, W, and maybe H (p = .0507)

regr7 <- lm(vdata$Complaints~vdata$Race)
summary(regr7)

a7 <- aov(vdata$Complaints~vdata$Race)
summary(a7)
t7 <- TukeyHSD(a7)
t7

#sig higher complaints for B

#job lvl - no real sig for anything

regj1 <- lm(vdata$Cognitive~vdata$Title)
summary(regj1)


regj2 <- lm(vdata$Conscientiousness~vdata$Title)
summary(regj2)


regj3 <- lm(vdata$Interview~vdata$Title)
summary(regj3)


regj4 <- lm(vdata$PerformY1~vdata$Title)
summary(regj4)


regj5 <- lm(vdata$PerformY2~vdata$Title)
summary(regj5)


regj6 <- lm(vdata$Commendations~vdata$Title)
summary(regj6)


regj7 <- lm(vdata$Complaints~vdata$Title)
summary(regj7)


#Chi Square to see gender differences in job title

tbl <- table(vdata$Title,vdata$Sex)
library(MASS)
chisq.test(tbl) 

tbl2 <- table(vdata$Title,vdata$Race)
library(MASS)
chisq.test(tbl2) 

# no sig

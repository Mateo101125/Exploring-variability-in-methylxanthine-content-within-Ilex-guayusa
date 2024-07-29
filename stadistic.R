#import data_q
str(data_q)
attach(data_q)
library(psych)
library(car)
library(stats)
library(tidyverse)
library(rstatix)
library(ggpubr)
#ANOVA caffeine

aov_caffeine<-aov(caffeine~location*light*age,data =data_q )
par(mfrow=c(2,2))
aov_1 <-aov(caffeine~location*light*age,data =data_q )
par(mfroplot(aov_caffeine)

#anova assumptions
#normality of residuals
shapiro.test(residuals(aov_caffeine))
#Homogeneity
# Perform Levene's test for homogeneity of variance
result_levene <- leveneTest(aov_caffeine)
print(resultado_levene)

# Independence
durbinWatsonTest(aov_caffeine)

#ANOVA
summary.aov(aov_caffeine)
#Mean contrasts
#Tukey's mean contrast
tukey_Caffeine<- tukey_hsd(aov_caffeine)


#ANOVA Theobromine

aov_theobromine <-aov(theobromine~location*light*age,data =data_q)
par(mfrow=c(2,2))
plot(aov_theobromine)
#anova assumptions
#normality of residuals
shapiro.test(residuals(aov_theobromine))
#Homogeneity
# Perform Levene's test for homogeneity of variance
result_levenetb <- leveneTest(aov_theobromine)
print(result_levenetb)

# Independence
durbinWatsonTest(aov_theobromine)

#ANOVA
summary.aov(aov_theobromine)

#Mean contrasts
#Tukey's mean contrast
tukey_theobromine<- tukey_hsd(aov_theobromine)



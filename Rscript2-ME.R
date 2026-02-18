library(tidyverse); library(car)

occ.wide <- read.csv("data-occ-original-raw-22dec21.csv", stringsAsFactors = TRUE)

#long format data
occ.long <- occ.wide %>%
    gather(key, Value, -Group, -Site, -IID, -Age, -Sex, -Sexn) %>% #gather all var
    extract(key, c("Variable", "Replicate"), "([a-zA-Z]+)(\\d)") #split key into two parts
rm(occ.wide)

#rename columns
occ.long <- occ.long %>% mutate(Variable=str_replace(Variable, 'depthop', 'DepthOP'))
occ.long <- occ.long %>% mutate(Variable=str_replace(Variable, 'genform', 'GeneralForm'))
occ.long <- occ.long %>% mutate(Variable=str_replace(Variable, 'lambinio', 'LambInion'))
occ.long <- occ.long %>% mutate(Variable=str_replace(Variable, 'MaxBiCondBrth', 'BiCondBreadth'))
occ.long <- occ.long %>% mutate(Variable=str_replace(Variable, 'nuchcres', 'NuchalCrest'))
occ.long <- occ.long %>% mutate(Variable=str_replace(Variable, 'nuchline', 'NuchalLine'))

#make factors
occ.long$Sex <- as.factor(occ.long$Sex)
occ.long$Sexn <- as.factor(occ.long$Sexn)
occ.long$Variable <- as.factor(occ.long$Variable)
occ.long$Replicate <- as.factor(occ.long$Replicate)

#ME or differences between pairs of trials
occ.ME <- occ.long %>% spread(Replicate, Value) #split replicates into separate columns for ME
occ.ME <- occ.ME %>% rename(Rep1='1') #rename columns
occ.ME <- occ.ME %>% rename(Rep2='2') #rename columns
occ.ME <- occ.ME %>% rename(Rep3='3') #rename columns
occ.ME$diff12 <- occ.ME$Rep1 - occ.ME$Rep2 #create new ME variable
occ.ME$diff13 <- occ.ME$Rep1 - occ.ME$Rep3 #create new ME variable
occ.ME$diff23 <- occ.ME$Rep2 - occ.ME$Rep3 #create new ME variable

#delete rep columns and gather then spread data for Levene Test
occ.ME <- dplyr::select(occ.ME, -Rep1, -Rep2, -Rep3)
occ.diff <- occ.ME %>%
    gather(key, Value, -Group, -Site, -IID, -Age, -Sex, -Sexn, -Variable)
occ.diff <- spread(occ.diff, Variable, Value)
occ.diff <- dplyr::select(occ.diff, -GeneralForm, -NuchalCrest, -NuchalLine)
occ.diff <- occ.diff %>% rename(Differences=key)
occ.diff$Differences <- as.factor(occ.diff$Differences)

#test variation due to ME
car::leveneTest(BiCondBreadth~Differences, data=occ.diff, center=mean)
car::leveneTest(DepthOP~Differences, data=occ.diff, center=mean)
car::leveneTest(FMSag~Differences, data=occ.diff, center=mean)
car::leveneTest(FMTrans~Differences, data=occ.diff, center=mean)
car::leveneTest(LambInion~Differences, data=occ.diff, center=mean)

###Prepare ME Table
#get trait means
occ.mean <- spread(occ.long, Variable, Value)
tmean1 <- mean(occ.mean$BiCondBreadth, na.rm = TRUE)
tmean2 <- mean(occ.mean$DepthOP, na.rm = TRUE)
tmean3 <- mean(occ.mean$FMSag, na.rm = TRUE)
tmean4 <- mean(occ.mean$FMTrans, na.rm = TRUE)
tmean5 <- mean(occ.mean$LambInion, na.rm = TRUE)
tmean <- c(tmean1, tmean2, tmean3, tmean4, tmean5)
tsd1 <- sd(occ.mean$BiCondBreadth, na.rm = TRUE)
tsd2 <- sd(occ.mean$DepthOP, na.rm = TRUE)
tsd3 <- sd(occ.mean$FMSag, na.rm = TRUE)
tsd4 <- sd(occ.mean$FMTrans, na.rm = TRUE)
tsd5 <- sd(occ.mean$LambInion, na.rm = TRUE)
tsd <- c(tsd1, tsd2, tsd3, tsd4, tsd5)
rm(tmean1, tmean2, tmean3, tmean4, tmean5, tsd1, tsd2, tsd3, tsd4, tsd5)

#get mean differences by trait
dmean1 <- mean(occ.diff$BiCondBreadth, na.rm = TRUE)
dmean2 <- mean(occ.diff$DepthOP, na.rm = TRUE)
dmean3 <- mean(occ.diff$FMSag, na.rm = TRUE)
dmean4 <- mean(occ.diff$FMTrans, na.rm = TRUE)
dmean5 <- mean(occ.diff$LambInion, na.rm = TRUE)
dmean <- c(dmean1, dmean2, dmean3, dmean4, dmean5)
dsd1 <- sd(occ.diff$BiCondBreadth, na.rm = TRUE)
dsd2 <- sd(occ.diff$DepthOP, na.rm = TRUE)
dsd3 <- sd(occ.diff$FMSag, na.rm = TRUE)
dsd4 <- sd(occ.diff$FMTrans, na.rm = TRUE)
dsd5 <- sd(occ.diff$LambInion, na.rm = TRUE)
#revised to correct typo dsd4 listed twice rather than dsd5
dsd <- c(dsd1, dsd2, dsd3, dsd4, dsd5)
rm(dmean1, dmean2, dmean3, dmean4, dmean5, dsd1, dsd2, dsd3, dsd4, dsd5)

#ME Table
me.desc <- cbind(dmean, dsd, tmean, tsd)
me.desc <- data.frame(me.desc)
me.desc$PercentError = (me.desc$dmean / me.desc$tmean) *100
me.desc <- abs(round(me.desc, 2))
write.csv(me.desc, file="occ.ME-orig.csv", quote = FALSE,
          row.names = TRUE)

#difference in the values for dsd because of the typo
#dmean	dsd	tmean	tsd	PercentError
#0.33	1.59	64.43	7.18	0.51	new, revised dsd5
#0.33	0.14	64.43	7.18	0.51	old, typo repeating dsd4
#1.59 vs 0.14 as a SD in isolation is large but as a percentage of the trait mean of ~65mm, it's negligible with no practical impact on the ME assessment.

rm(list = ls())
gc()


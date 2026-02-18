library(psych) #descriptives
library(car) #anova
library(EnvStats) #

############################ DATA
occ.orig = read.csv("revised-preparation-original.csv")
occ.mined = read.csv("revised-preparation-mined.csv")

#variables for assumptions testing
anal_vars = c("BicondylarBreadth", "LambdaInion", "Index_FM", "Index_Form", "Index_Nuchal")


############################ ORIGINAL DATA: ASSUMPTIONS TEST
# original data contain more variables than found in mined data
# assumptions testing on those variables prior to combining with mined

# Descriptives by Subsistence and Sex
substats = describeBy(occ.orig[, anal_vars], occ.orig$Subsistence)
write.csv(do.call("rbind", substats), file = "revised-descstats-sub-orig.csv", quote = FALSE, row.names = TRUE)

sexstats = describeBy(occ.orig[, anal_vars], occ.orig$Sex)
write.csv(do.call("rbind", sexstats), file = "revised-descstats-sex-orig.csv", quote = FALSE, row.names = TRUE)

# Normality: QQ plots
qq_vars = list(
    BicondylarBreadth = "Bicondylar Breadth",
    LambdaInion       = "Lambda Inion",
    Index_FM          = "Foramen Magnum Area",
    Index_Form        = "Occipital Morphology",
    Index_Nuchal      = "Nuchal Variation"
)

for (v in names(qq_vars)) {
    png(paste0("revised-qqplot-", tolower(v), "-orig.png"),
        width = 12, height = 8, units = "in", pointsize = 15, res = 300,
        family = "sans", type = "cairo")
    car::qqp(occ.orig[[v]], "norm", grid = FALSE,
             ylab = qq_vars[[v]], xlab = "Normal Distribution Quantiles")
    dev.off()
}

# Equality of variances: Levene's test
levene_tests = list(
    BicondylarBreadth_Sub = leveneTest(occ.orig$BicondylarBreadth ~ as.factor(occ.orig$Subsistence)),
    BicondylarBreadth_Sex = leveneTest(occ.orig$BicondylarBreadth ~ as.factor(occ.orig$Sex)),
    LambdaInion_Sub       = leveneTest(occ.orig$LambdaInion       ~ as.factor(occ.orig$Subsistence)),
    LambdaInion_Sex       = leveneTest(occ.orig$LambdaInion       ~ as.factor(occ.orig$Sex)),
    Index_FM_Sub          = leveneTest(occ.orig$Index_FM          ~ as.factor(occ.orig$Subsistence)),
    Index_FM_Sex          = leveneTest(occ.orig$Index_FM          ~ as.factor(occ.orig$Sex)),
    Index_Form_Sub        = leveneTest(occ.orig$Index_Form        ~ as.factor(occ.orig$Subsistence)),
    Index_Form_Sex        = leveneTest(occ.orig$Index_Form        ~ as.factor(occ.orig$Sex)),
    Index_Nuchal_Sub      = leveneTest(occ.orig$Index_Nuchal      ~ as.factor(occ.orig$Subsistence)),
    Index_Nuchal_Sex      = leveneTest(occ.orig$Index_Nuchal      ~ as.factor(occ.orig$Sex))
)

levene.orig = do.call("rbind", lapply(names(levene_tests), function(nm) {
    d = levene_tests[[nm]]
    data.frame(Test = nm, Df = d[1,1], F = round(d[1,2], 4), p = round(d[1,3], 4))
}))

write.csv(levene.orig, file = "revised-levenes-orig.csv", quote = FALSE, row.names = FALSE)

# Check variance ratio for Index_Form by Sex (flagged in original)
# Female variance ~29% of male - within 4x cutoff, use white.adjust in ANOVA
var(occ.orig$Index_Form[occ.orig$Sex == "Female"], na.rm = TRUE)
var(occ.orig$Index_Form[occ.orig$Sex == "Male"],   na.rm = TRUE)

# Outliers: Rosner test
for (v in anal_vars) {
    cat("\n??????", v, "??????\n")
    print(rosnerTest(occ.orig[[v]], k = 5, alpha = 0.05, warn = TRUE))
}



############################ COMBINED DATA (original + mined)
#reduce original data to subset for merge with mined
occ.orig = dplyr::select(occ.orig, Collection, Subsistence, Sex, BicondylarBreadth, Index_FM)

#merge and save
occ.combined = rbind(occ.orig, occ.mined)
write.csv(occ.combined, "revised-preparation-combined.csv", row.names = FALSE, quote = FALSE)

#variables for assumptions testing
anal_vars2 = c("BicondylarBreadth", "Index_FM")

# Descriptives by Subsistence and Sex
substats = describeBy(occ.combined[, anal_vars2], occ.combined$Subsistence)
write.csv(do.call("rbind", substats), file = "revised-descstats-sub-combined.csv", quote = FALSE, row.names = TRUE)

sexstats = describeBy(occ.combined[, anal_vars2], occ.combined$Sex)
write.csv(do.call("rbind", sexstats), file = "revised-descstats-sex-combined.csv", quote = FALSE, row.names = TRUE)

# Normality: QQ plots
qq_combined = list(
    BicondylarBreadth = "Bicondylar Breadth",
    Index_FM          = "Foramen Magnum Area"
)

for (v in names(qq_combined)) {
    png(paste0("revised-qqplot-", tolower(v), "-combined.png"),
        width = 12, height = 8, units = "in", pointsize = 15, res = 300,
        family = "sans", type = "cairo")
    car::qqp(occ.combined[[v]], "norm", grid = FALSE,
             ylab = qq_combined[[v]], xlab = "Normal Distribution Quantiles")
    dev.off()
}

# Equality of variances: Levene's test
levene_tests = list(
    BicondylarBreadth_Sub = leveneTest(occ.combined$BicondylarBreadth ~ as.factor(occ.combined$Subsistence)),
    BicondylarBreadth_Sex = leveneTest(occ.combined$BicondylarBreadth ~ as.factor(occ.combined$Sex)),
    Index_FM_Sub          = leveneTest(occ.combined$Index_FM          ~ as.factor(occ.combined$Subsistence)),
    Index_FM_Sex          = leveneTest(occ.combined$Index_FM          ~ as.factor(occ.combined$Sex))
)

levene.combined = do.call("rbind", lapply(names(levene_tests), function(nm) {
    d = levene_tests[[nm]]
    data.frame(Test = nm, Df = d[1,1], F = round(d[1,2], 4), p = round(d[1,3], 4))
}))

write.csv(levene.combined, file = "revised-levenes-combined.csv", quote = FALSE, row.names = FALSE)

# Check variance ratio for Index_FM by Sex (flagged in original)
# Female variance ~71% of male - use white.adjust in ANOVA
var(occ.combined$Index_FM[occ.combined$Sex == "Female"], na.rm = TRUE)
var(occ.combined$Index_FM[occ.combined$Sex == "Male"],   na.rm = TRUE)

# Outliers: Rosner test
for (v in anal_vars2) {
    cat("\n??????", v, "??????\n")
    print(rosnerTest(occ.combined[[v]], k = 5, alpha = 0.05, warn = TRUE))
}




############################ TIDY
rm(list = ls())
gc()
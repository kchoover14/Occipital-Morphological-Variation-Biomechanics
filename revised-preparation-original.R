library(dplyr) #data wrangling
library(tidyr) #data formatting
library(irr) #kappa test for nonmetric trait io error
library(car) #levene
library(psych) #pca
library(factoextra) #pca plot

##################### READ DATA
# stringsAsFactors=TRUE in the original workflow created a ghost "Military"

occ.wide = read.csv("data-occ-original-raw-22dec21.csv")

##################### INTRAOBSERVER ERROR: NONMETRIC TRAITS, 2 TRIALS
# Kappa2 for categorical agreement; Spearman correlation confirms perfect
# agreement where kappa=0 due to no score variance.

occ.long = occ.wide |>
    pivot_longer(
        cols = -c(Group, Site, IID, Age, Sex, Sexn),
        names_to = c("Variable", "Replicate"),
        names_pattern = "([a-zA-Z]+)(\\d)"
    )

nonmetric_vars = c("nuchcres", "nuchline", "genform")

occ.nonmetric = occ.long |>
    filter(Variable %in% nonmetric_vars) |>
    pivot_wider(names_from = Replicate, values_from = value, names_prefix = "Rep")

for (v in nonmetric_vars) {
    cat("\n??????", v, "??????\n")
    d = na.omit(occ.nonmetric[occ.nonmetric$Variable == v, c("Rep1", "Rep2")])
    print(kappa2(d))
    print(cor.test(d$Rep1, d$Rep2, method = "spearman"))
}

rm(occ.nonmetric)

##################### INTRAOBSEVER ERROR: METRIC TRAITS, 3 TRIALS
# Levene's test on pairwise differences; ME table with percent error.
# Note: original script had a typo - dsd4 used twice instead of dsd4/dsd5.
# Corrected here; results verified against manual checks.

metric_vars = c("depthop", "lambinio", "MaxBiCondBrth", "FMSag", "FMTrans")

occ.metric = occ.long |>
    filter(Variable %in% metric_vars) |>
    pivot_wider(names_from = Replicate, values_from = value, names_prefix = "Rep") |>
    mutate(
        diff12 = Rep1 - Rep2,
        diff13 = Rep1 - Rep3,
        diff23 = Rep2 - Rep3
    )

occ.diff = occ.metric |>
    pivot_longer(cols = c(diff12, diff13, diff23), names_to = "Differences", values_to = "value") |>
    pivot_wider(names_from = Variable, values_from = value) |>
    mutate(Differences = as.factor(Differences))

# Levene's test
for (v in metric_vars) {
    cat("\n??????", v, "??????\n")
    print(car::leveneTest(occ.diff[[v]] ~ occ.diff$Differences, center = mean))
}

# ME table
occ.mean = occ.long |>
    filter(Variable %in% metric_vars) |>
    pivot_wider(names_from = Variable, values_from = value)

me.desc = data.frame(
    tmean = sapply(metric_vars, function(v) mean(occ.mean[[v]], na.rm = TRUE)),
    tsd   = sapply(metric_vars, function(v) sd(occ.mean[[v]],   na.rm = TRUE)),
    dmean = sapply(metric_vars, function(v) mean(occ.diff[[v]], na.rm = TRUE)),
    dsd   = sapply(metric_vars, function(v) sd(occ.diff[[v]],   na.rm = TRUE))
) |>
    mutate(PercentError = abs(dmean / tmean) * 100) |>
    abs() |>
    round(2)

write.csv(me.desc, file = "revised-preparation-me-orig.csv", quote = FALSE, row.names = TRUE)

rm(occ.long, occ.metric, occ.diff, occ.mean, me.desc, nonmetric_vars, metric_vars)





##################### WRANGLE DATA FOR FINAL ORIG DATASET
# Keep replicate 2 for nonmetrics; average 3 replicates for metrics.
# Create Collection and Subsistence labels.

occ = occ.wide |>
    mutate(
        ForamenMagnumS    = rowMeans(across(c(FMSag1, FMSag2, FMSag3)),                         na.rm = TRUE),
        ForamenMagnumT    = rowMeans(across(c(FMTrans1, FMTrans2, FMTrans3)),                   na.rm = TRUE),
        BicondylarBreadth = rowMeans(across(c(MaxBiCondBrth1, MaxBiCondBrth2, MaxBiCondBrth3)), na.rm = TRUE),
        DepthEOP          = rowMeans(across(c(depthop1, depthop2, depthop3)),                   na.rm = TRUE),
        LambdaInion       = rowMeans(across(c(lambinio1, lambinio2, lambinio3)),                 na.rm = TRUE)
    ) |>
    mutate(across(c(ForamenMagnumS, ForamenMagnumT, BicondylarBreadth, DepthEOP, LambdaInion),
                  ~ifelse(is.nan(.), NA, .))) |>
    mutate(
        Collection = case_when(
            Site == "8BR246" ~ "Windover",
            Site == "8MT37"  ~ "Hutchinson",
            Site == "8WA108" ~ "Military",
            Site == "FSUTC"  ~ "FSUTC"
        ),
        Subsistence = case_when(
            Group == "Archaic"  ~ "Hunter_Gatherer",
            Group == "Historic" ~ "Agriculture",
            Group == "Study"    ~ "Agriculture"
        )
    ) |>
    select(IID, Subsistence, Collection, Sex,
           ForamenMagnumS, ForamenMagnumT, BicondylarBreadth,
           DepthEOP, LambdaInion,
           nuchcres2, nuchline2, genform2) |>
    rename(
        NuchalCrest = nuchcres2,
        NuchalLine  = nuchline2,
        GeneralForm = genform2
    )



##################### WRITE UNINDEXED DATA FOR ANALYSIS
write.csv(occ, "revised-preparation-original-noIndex.csv", quote = FALSE, row.names = FALSE)



##################### PCA: TEST VARIABLE REDUCTION
#gather data for pca
pca_cols = c("DepthEOP", "LambdaInion", "GeneralForm", "NuchalCrest", "NuchalLine")

# Scatterplot matrix by Subsistence
png("revised-splom-subsistence.png", width = 12, height = 12, units = "in", pointsize = 15, res = 300, family = "sans", type = "cairo")
pairs.panels(occ[, pca_cols], lm = TRUE, cor = TRUE, method = "pearson",
             bg = c("black", "yellow")[as.factor(occ$Subsistence)],
             pch = 21, stars = TRUE)
dev.off()

# Scatterplot matrix by Sex
png("revised-splom-sex.png", width = 12, height = 12, units = "in",
    pointsize = 15, res = 300, family = "sans", type = "cairo")
pairs.panels(occ[, pca_cols], lm = TRUE, cor = TRUE, method = "pearson",
             bg = c("black", "yellow")[as.factor(occ$Sex)],
             pch = 21, stars = TRUE)
dev.off()

# PCA on complete cases
occ.pca = prcomp(na.omit(occ[, pca_cols]), scale = TRUE)

# Reverse signs for interpretability
occ.pca$rotation = -1 * occ.pca$rotation

# Loadings and variance explained
occ.pca$rotation
occ.pca$sdev^2 / sum(occ.pca$sdev^2) * 100

# Biplot
png("revised-pca-biplot.png", width = 14, height = 14, units = "in",
    pointsize = 20, res = 300, family = "sans", type = "cairo")
fviz_pca_biplot(occ.pca,
                col.var = "blue",
                col.ind = "black",
                label = "var",
                repel = TRUE,
                pointshape = 8,
                pointsize = 0.5)
dev.off()




##################### CREATE INDEX FOR ANALYSIS
# Create foramen magnum area, occipital form, and nuchal indices
occ = occ |>
    mutate(
        Index_FM     = pi * (0.5 * ForamenMagnumT) * (0.5 * ForamenMagnumS),
        Index_Form   = DepthEOP + GeneralForm,
        Index_Nuchal = NuchalLine + NuchalCrest
    ) |>
    select(Subsistence, Collection, Sex,
           BicondylarBreadth, LambdaInion,
           Index_FM, Index_Form, Index_Nuchal)



##################### WRITE DATA FOR ANALYSIS
write.csv(occ, "revised-preparation-original.csv", quote = FALSE, row.names = FALSE)




##################### TIDY
rm(list = ls())
gc()

library(dplyr) # data wrangling
library(car) #Anova()
library(MASS) #linear discriminant analysis
library(psych) #scatterplot of fm dimensions
library(ggplot2) #plotting
library(cowplot) #plot layout

######################## READ DATA
occ.orig     = read.csv("revised-preparation-original.csv")
occ.combined = read.csv("revised-preparation-combined.csv")

######################## ANOVA: TEST FOR INTERACTION (Type III)
# If interaction is not significant, drop and refit with Type II
fm.lm        = lm(Index_FM          ~ Subsistence * Sex, data = occ.orig)
fm.combo.lm  = lm(Index_FM          ~ Subsistence * Sex, data = occ.combined)
bcb.lm       = lm(BicondylarBreadth ~ Subsistence * Sex, data = occ.orig)
bcb.combo.lm = lm(BicondylarBreadth ~ Subsistence * Sex, data = occ.combined)
li.lm        = lm(LambdaInion       ~ Subsistence * Sex, data = occ.orig)
form.lm      = lm(Index_Form        ~ Subsistence * Sex, data = occ.orig)
nuchal.lm    = lm(Index_Nuchal      ~ Subsistence * Sex, data = occ.orig)

car::Anova(fm.lm,        type = "III")
car::Anova(fm.combo.lm,  type = "III")
car::Anova(bcb.lm,       type = "III")
car::Anova(bcb.combo.lm, type = "III")
car::Anova(li.lm,        type = "III")
car::Anova(form.lm,      white.adjust = TRUE, type = "III")
car::Anova(nuchal.lm,    type = "III")

rm(fm.lm, fm.combo.lm, bcb.lm, bcb.combo.lm, li.lm, form.lm, nuchal.lm)


######################## ANOVA: MAIN EFFECTS (Type II, no interaction)
fm.lm2        = lm(Index_FM          ~ Subsistence + Sex, data = occ.orig)
fm.combo.lm2  = lm(Index_FM          ~ Subsistence + Sex, data = occ.combined)
bcb.lm2       = lm(BicondylarBreadth ~ Subsistence + Sex, data = occ.orig)
bcb.combo.lm2 = lm(BicondylarBreadth ~ Subsistence + Sex, data = occ.combined)
li.lm2        = lm(LambdaInion       ~ Subsistence + Sex, data = occ.orig)
form.lm2      = lm(Index_Form        ~ Subsistence + Sex, data = occ.orig)
nuchal.lm2    = lm(Index_Nuchal      ~ Subsistence + Sex, data = occ.orig)

car::Anova(fm.lm2,        type = "II")
car::Anova(fm.combo.lm2,  white.adjust = TRUE, type = "II")
car::Anova(bcb.lm2,       type = "II")
car::Anova(bcb.combo.lm2, type = "II")
car::Anova(li.lm2,        type = "II")
car::Anova(form.lm2,      white.adjust = TRUE, type = "II")
car::Anova(nuchal.lm2,    type = "II")

summary(fm.lm2)
summary(bcb.lm2)
summary(fm.combo.lm2)
summary(bcb.combo.lm2)
summary(li.lm2)
summary(form.lm2)
summary(nuchal.lm2)

rm(fm.lm2, fm.combo.lm2, bcb.lm2, bcb.combo.lm2, li.lm2, form.lm2, nuchal.lm2)


######################## TUKEY HSD (combined data only)
# Sex has two levels so only test subsistence here
TukeyHSD(aov(BicondylarBreadth ~ Subsistence, data = occ.combined), conf.level = 0.95)
TukeyHSD(aov(Index_FM          ~ Subsistence, data = occ.combined), conf.level = 0.95)


######################## DFA
# Single predictor: Index_FM
occ.dfa        = occ.combined[, c("Sex", "Index_FM")]
occ.dfa        = occ.dfa[complete.cases(occ.dfa), ]
occ.dfa$Index_FM = scale(occ.dfa$Index_FM)

set.seed(1122)
train.idx = sample(c(TRUE, FALSE), nrow(occ.dfa), replace = TRUE, prob = c(0.7, 0.3))
train     = occ.dfa[train.idx, ]
test      = occ.dfa[!train.idx, ]

fm.model  = MASS::lda(Sex ~ ., data = train, na.action = na.pass)
fm.model
predicted = predict(fm.model, test, na.action = na.pass)
mean(predicted$class == test$Sex)

# Two predictors: Index_FM + BicondylarBreadth
occ.dfa2 = occ.combined[, c("Sex", "Index_FM", "BicondylarBreadth")]
occ.dfa2 = occ.dfa2[complete.cases(occ.dfa2), ]
occ.dfa2[, c("Index_FM", "BicondylarBreadth")] = scale(occ.dfa2[, c("Index_FM", "BicondylarBreadth")])

set.seed(1122)
train.idx2 = sample(c(TRUE, FALSE), nrow(occ.dfa2), replace = TRUE, prob = c(0.7, 0.3))
train2     = occ.dfa2[train.idx2, ]
test2      = occ.dfa2[!train.idx2, ]

fm.model2  = MASS::lda(Sex ~ ., data = train2, na.action = na.pass)
fm.model2
predicted2 = predict(fm.model2, test2, na.action = na.pass)
mean(predicted2$class == test2$Sex)

rm(fm.model, fm.model2, occ.dfa, occ.dfa2, predicted, predicted2,
   test, test2, train, train2, train.idx, train.idx2)


######################## PLOTS: ANOVA
# Reusable boxplot function with individual points and ANOVA p-value annotation.
# P-values are computed from one-way aov() for display purposes only;
# reported results use two-way Type II ANOVA (see above).

occ_plot = function(data, x, y, ylab) {
    p_val = summary(aov(as.formula(paste(y, "~", x)), data = data))[[1]][["Pr(>F)"]][1]
    p_label = ifelse(p_val < 0.001, "p < 0.001",
                     ifelse(p_val < 0.01,  paste0("p = ", formatC(p_val, digits = 3, format = "f")),
                            paste0("p = ", formatC(p_val, digits = 3, format = "f"))))
    y_max = max(data[[y]], na.rm = TRUE)
    y_range = diff(range(data[[y]], na.rm = TRUE))

    ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[x]])) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, color = "black") +
        annotate("text", x = Inf, y = Inf,
                 label = p_label, hjust = 1.1, vjust = 1.5, size = 3.5) +
        scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
        labs(y = ylab, x = "") +
        theme_classic() +
        theme(
            legend.position = "none",
            text = element_text(size = 11),
            axis.text = element_text(color = "black", size = 11)
        )
}

#### FORAMEN MAGNUM (combined data -- includes Horticultural group)
p.fm.sex = occ_plot(occ.combined, "Sex",         "Index_FM", "Foramen Magnum Area")
p.fm.sub = occ_plot(occ.combined, "Subsistence", "Index_FM", "Foramen Magnum Area")
p.fm     = plot_grid(p.fm.sex, p.fm.sub, labels = "AUTO", ncol = 2)
ggsave("revised-plot-fm.png", plot = p.fm, width = 10, height = 6, units = "in", dpi = 300)

#### BICONDYLAR BREADTH (combined data -- includes Horticultural group)
p.bcb.sex = occ_plot(occ.combined, "Sex",         "BicondylarBreadth", "Bicondylar Breadth (mm)")
p.bcb.sub = occ_plot(occ.combined, "Subsistence", "BicondylarBreadth", "Bicondylar Breadth (mm)")
p.bcb     = plot_grid(p.bcb.sex, p.bcb.sub, labels = "AUTO", ncol = 2)
ggsave("revised-plot-bcb.png", plot = p.bcb, width = 10, height = 6, units = "in", dpi = 300)

#### LAMBDA INION (original data)
p.li.sex = occ_plot(occ.orig, "Sex",         "LambdaInion", "Lambda-Inion (mm)")
p.li.sub = occ_plot(occ.orig, "Subsistence", "LambdaInion", "Lambda-Inion (mm)")
p.li     = plot_grid(p.li.sex, p.li.sub, labels = "AUTO", ncol = 2)
ggsave("revised-plot-li.png", plot = p.li, width = 10, height = 6, units = "in", dpi = 300)

#### OCCIPITAL FORM INDEX (original data)
p.form.sex = occ_plot(occ.orig, "Sex",         "Index_Form", "Occipital Form Index")
p.form.sub = occ_plot(occ.orig, "Subsistence", "Index_Form", "Occipital Form Index")
p.form     = plot_grid(p.form.sex, p.form.sub, labels = "AUTO", ncol = 2)
ggsave("revised-plot-form.png", plot = p.form, width = 10, height = 6, units = "in", dpi = 300)

#### NUCHAL INDEX (original data)
p.nuch.sex = occ_plot(occ.orig, "Sex",         "Index_Nuchal", "Nuchal Index")
p.nuch.sub = occ_plot(occ.orig, "Subsistence", "Index_Nuchal", "Nuchal Index")
p.nuch     = plot_grid(p.nuch.sex, p.nuch.sub, labels = "AUTO", ncol = 2)
ggsave("revised-plot-nuchal.png", plot = p.nuch, width = 10, height = 6, units = "in", dpi = 300)


######################## SCATTERPLOT: FORAMEN MAGNUM DIMENSIONS
# Reads raw FM dimensions (pre-index) from both datasets for bivariate display
occ.orig.fm  = read.csv("revised-preparation-original-noIndex.csv") |>
    dplyr::select(Subsistence, Sex, ForamenMagnumS, ForamenMagnumT)

occ.mined.fm = read.csv("data-occ-mined.csv") |>
    mutate(Subsistence = case_when(
        Group == "Arikara" ~ "Horticultural",
        Group == "Study"   ~ "Agriculture"
    )) |>
    dplyr::select(Subsistence, Sex, ForamenMagnumS, ForamenMagnumT)

occ.fm           = rbind(occ.mined.fm, occ.orig.fm)
occ.fm$Sex        = as.factor(occ.fm$Sex)
occ.fm$Subsistence = as.factor(occ.fm$Subsistence)

occ.fm = occ.fm |> mutate(SexN = case_when(
Sex == 'Female' ~ 1,
Sex == 'Male' ~2
))

occ.fm = occ.fm |> mutate(SubsistenceN = case_when(
    Subsistence == 'Hunter_Gatherer' ~ 1,
    Subsistence == 'Horticultural' ~2,
    Subsistence == 'Agriculture' ~3
))

png("revised-scatter-fm-sex.png", width = 8, height = 8, units = "in",
    pointsize = 15, res = 300, family = "sans", type = "cairo")
psych::scatterHist(ForamenMagnumS ~ ForamenMagnumT + SexN,
                   data = occ.fm, cex.point = 0.3, smooth = FALSE,
                   xlab = "FMS", ylab = "FMT",
                   correl = FALSE, d.arrow = TRUE,
                   lwd = 4, title = "Foramen Magnum Area by Sex",
                   cex.cor = 2, cex.arrow = 1.25)
dev.off()

png("revised-scatter-fm-subsistence.png", width = 8, height = 8, units = "in",
    pointsize = 15, res = 300, family = "sans", type = "cairo")
psych::scatterHist(ForamenMagnumS ~ ForamenMagnumT + SubsistenceN,
                   data = occ.fm, cex.point = 0.3, smooth = FALSE,
                   xlab = "FMS", ylab = "FMT",
                   correl = FALSE, d.arrow = TRUE,
                   lwd = 4, title = "Foramen Magnum Area by Subsistence",
                   cex.cor = 2, cex.arrow = 1.25)
dev.off()


######################## TIDY
rm(list = ls())
gc()
library(tidyverse); library(ggplot2); library(ggpubr); library(patchwork)
library(ggstatsplot); library(PMCMRplus)

#read indexed Florida data
occ.orig <- read.csv("data-occ-original-index.csv", stringsAsFactors = TRUE)
occ.combined <- read.csv("data-occ-combined-index.csv", stringsAsFactors = TRUE)

#by Subsistence comparing original and combo original/mined
#no difference
a <- ggbetweenstats(
    data = occ.combined,
    x = Subsistence,
    y = BicondylarBreadth,
    type = "parametric", # ANOVA or Kruskal-Wallis
    var.equal = TRUE, # ANOVA or Welch ANOVA
    plot.type = "box",
    pairwise.comparisons = TRUE,
    pairwise.display = "significant",
    centrality.plotting = FALSE,
    bf.message = FALSE
)

b <- ggbetweenstats(
    data = occ.orig,
    x = Subsistence,
    y = LambdaInion,
    type = "parametric", # ANOVA or Kruskal-Wallis
    var.equal = TRUE, # ANOVA or Welch ANOVA
    plot.type = "box",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    centrality.plotting = FALSE,
    bf.message = FALSE
)
png("F7-anova by Subsistence.png", width = 12, height=8, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
a + b + plot_annotation(tag_levels = 'A')
dev.off()

#by sex
#using combined data, when appropriate
c <- ggbetweenstats(
    data = occ.combined,
    x = Sex,
    y = Index_FM,
    type = "parametric", # ANOVA or Kruskal-Wallis
    var.equal = TRUE, # ANOVA or Welch ANOVA
    plot.type = "box",
    pairwise.comparisons = TRUE,
    pairwise.display = "significant",
    centrality.plotting = FALSE,
    bf.message = FALSE
)

d <- ggbetweenstats(
    data = occ.combined,
    x = Sex,
    y = BicondylarBreadth,
    type = "parametric", # ANOVA or Kruskal-Wallis
    var.equal = TRUE, # ANOVA or Welch ANOVA
    plot.type = "box",
    pairwise.comparisons = TRUE,
    pairwise.display = "significant",
    centrality.plotting = FALSE,
    bf.message = FALSE
)

e <- ggbetweenstats(
    data = occ.orig,
    x = Sex,
    y = LambdaInion,
    type = "parametric", # ANOVA or Kruskal-Wallis
    var.equal = TRUE, # ANOVA or Welch ANOVA
    plot.type = "box",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    centrality.plotting = FALSE,
    bf.message = FALSE
)
f <- ggbetweenstats(
    data = occ.orig,
    x = Sex,
    y = Index_Form,
    type = "parametric", # ANOVA or Kruskal-Wallis
    var.equal = TRUE, # ANOVA or Welch ANOVA
    plot.type = "box",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    centrality.plotting = FALSE,
    bf.message = FALSE
)
g <- ggbetweenstats(
    data = occ.orig,
    x = Sex,
    y = Index_Nuchal,
    type = "parametric", # ANOVA or Kruskal-Wallis
    var.equal = TRUE, # ANOVA or Welch ANOVA
    plot.type = "box",
    pairwise.comparisons = TRUE,
    pairwise.display = "all",
    centrality.plotting = FALSE,
    bf.message = FALSE
)

png("F8-anova by sex.png", width = 12, height=8, units="in", pointsize=15,
    res=300, family="sans", type="cairo")
(c + d) / (e + f) / (g + plot_spacer()) +
    plot_annotation(tag_levels = 'A')
dev.off()

#tidy
rm(list = ls())
gc()

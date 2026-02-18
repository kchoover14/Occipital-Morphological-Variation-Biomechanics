library(psych) # ommitted in first version, required for analysis

#compare FM by sex
occ.orig.noindex <- read.csv("data-occ-original.csv", stringsAsFactors = TRUE)
occ.orig.noindex <- occ.orig.noindex %>% mutate(SexNumber = case_when(
    Sex == "Female"  ~ "1",
    Sex == "Male"  ~ "2"))

#Explore foramen area, non indexed
psych::scatterHist(ForamenMagnumS ~ ForamenMagnumT + Sex,
                   data=occ.orig.noindex, cex.point=.3,smooth=FALSE,
                   xlab="FMS",ylab="FMT",
                   correl=FALSE, d.arrow=TRUE,
                   lwd=4, title="Foramen Magnum Area by Sex",
                   cex.cor=2, cex.arrow=1.25)
#compare FM by subsistence
psych::scatterHist(ForamenMagnumS ~ ForamenMagnumT + Subsistence,
                   data=occ.orig.noindex, cex.point=.3,smooth=FALSE,
                   xlab="FMS",ylab="FMT",
                   correl=FALSE, d.arrow=TRUE,
                   lwd=4, title="Foramen Magnum Area by Subsistence",
                   cex.cor=2, cex.arrow=1.25)
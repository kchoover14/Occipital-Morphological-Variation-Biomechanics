library(dplyr) #data wrangling

##################### READ DATA

occ.westcott = read.csv("data-occ-westcott-raw.csv")
occ.williams = read.csv("data-occ-williams-raw.csv")


##################### CLEAN AND STANDARDIZE
occ.westcott = occ.westcott |>
    select(Group, Coll, Sex, BCBOC, FMLOC, FMBOC) |>
    rename(
        Collection        = Coll,
        BicondylarBreadth = BCBOC,
        ForamenMagnumS    = FMLOC,
        ForamenMagnumT    = FMBOC
    )

occ.williams = occ.williams |>
    select(Group, Site, Sex, BCB, LFM, WFM) |>
    rename(
        Collection        = Site,
        BicondylarBreadth = BCB,
        ForamenMagnumS    = LFM,
        ForamenMagnumT    = WFM
    ) |>
    mutate(Sex = case_when(
        Sex == "F" ~ "Female",
        Sex == "M" ~ "Male"
    ))





##################### MERGE
occ.mined = rbind(occ.westcott, occ.williams)
rm(occ.westcott, occ.williams)





##################### INDEX
# Add Subsistence label and foramen magnum area index.
occ.mined = occ.mined |>
    mutate(
        Subsistence = case_when(
            Group == "Arikara" ~ "Horticultural",
            Group == "Study"   ~ "Agriculture"
        ),
        Index_FM = pi * (0.5 * ForamenMagnumT) * (0.5 * ForamenMagnumS)
    ) |>
    select(Subsistence, Collection, Sex, BicondylarBreadth, Index_FM)

write.csv(occ.mined, "revised-preparation-mined.csv", row.names = FALSE, quote = FALSE)

##################### TIDY
rm(list = ls())
gc()

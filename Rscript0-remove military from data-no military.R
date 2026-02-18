library(tidyverse)

#redundant category created by from strings as factors
#script not needed or used if not reading strings as factors
occ.wide <- read.csv("data-occ-original-raw-22dec21.csv", stringsAsFactors = TRUE)

#remove military from sample
occ.wide <- occ.wide %>% filter(Group=='Archaic' | Group=='Study')

write.csv(occ.wide, file="data-occ-original-noMIL-raw-22dec21.csv", quote = FALSE,
          row.names = TRUE)

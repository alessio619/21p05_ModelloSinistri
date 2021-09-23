

# : =====================================================================================


# A. Packages --------------------------------------------

library(here)
library(data.table)



# B. Data Upload ---------------------------------------

df1 = readRDS(here('data', 'clean', 'train.rds'))

dt = as.data.table(df1)
dt = janitor::clean_names(dt)

skim(dt)


# Confusion Matrix Recreation =====================================================

### Baseline (Floor)

dt[, TP := fifelse(liquidato_basso == 'Si' & predittivo_as_is == 'Si', 1, 0)]
dt[, TN := fifelse(liquidato_basso == 'NO' & predittivo_as_is == 'NO', 1, 0)]
dt[, FP := fifelse(liquidato_basso == 'NO' & predittivo_as_is == 'Si', 1, 0)]
dt[, FN := fifelse(liquidato_basso == 'Si' & predittivo_as_is == 'NO', 1, 0)]



total = nrow(dt)
TP = sum(dt$TP) / total * 100
TN = sum(dt$TN) / total * 100
FP = sum(dt$FP) / total * 100
FN = sum(dt$FN) / total * 100

###$ TRUE POSITIVE 03.2%
###$ TRUE NEGATIVE 79.5%
###$ FALSE POSITIVE 08.1%
###$ FALSE NEGATIVE 9.22%
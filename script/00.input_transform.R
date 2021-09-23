
library(here)
library(readxl)
library(feather)

# Upload raw data from .xlsx
df1 = read_excel(here('data', 'raw', '210916 Supporto Modello predittivo.xlsx'),
                sheet = 1)

df2 = read_excel(here('data', 'raw', '210916 Supporto Modello predittivo.xlsx'),
                sheet = 2)


### Export as RDS 
saveRDS(df1, file = here('data', 'clean', 'train.rds'))
saveRDS(df2, file = here('data', 'clean', 'test.rds'))

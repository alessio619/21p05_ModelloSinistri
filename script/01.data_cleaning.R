

# : =====================================================================================


# A. Packages --------------------------------------------

library(here)
library(data.table)
library(skimr)
library(magrittr)
library(innteamUtils)
library(lubridate)
library(ggplot2)

### Aux Data
dc = innteamUtils::decodifica_comuni

# B. Data Upload ---------------------------------------

library(readxl)

df1 <- read_excel("data/raw/211110 - Subset di controllo.xlsx")

dt = as.data.table(df1)
dt = janitor::clean_names(dt)



# C. EDA =======================================================================


## FACTOR ---------------------------------------------------------

### Denuncia Mittente
dt[, denuncia_mittente := as.factor(denuncia_mittente)]
plot(dt$denuncia_mittente)    


### Denuncia Firme
dt[, denuncia_firme := as.factor(denuncia_firme)]
plot(dt$denuncia_firme) 


### Responsabilita Totale o Parziale
dt[, denuncia_resp_totale_o_parziale := as.factor(denuncia_resp_totale_o_parziale)]
plot(dt$denuncia_resp_totale_o_parziale) 


### Responsabilita Totale o Parziale
dt[, eventuali_dichiarazioni_difformi := as.factor(eventuali_dichiarazioni_difformi)]
plot(dt$eventuali_dichiarazioni_difformi)


### Responsabilita Totale o Parziale
dt[, veicolo_gestionaria_da_ania := as.factor(veicolo_gestionaria_da_ania)]
plot(dt$veicolo_gestionaria_da_ania)


### Responsabilita Totale o Parziale
dt[, residenza_gestionaria_da_ania := as.factor(residenza_gestionaria_da_ania)]
plot(dt$residenza_gestionaria_da_ania)


### Responsabilita Totale o Parziale
dt[, tipo_sx := as.factor(tipo_sx)]
plot(dt$tipo_sx)


### Responsabilita Totale o Parziale
dt[, ultimo_codice_resp := as.factor(ultimo_codice_resp)]
plot(dt$ultimo_codice_resp)

plot(prop.table(table(as.factor(dt$liquidato_basso))))


### Responsabilita Totale o Parziale
dt_10k = dt[pagato_da_unipol_sai <= 9000]
hist(dt_10k$pagato_da_unipol_sai)





# D. Data Transformation -------------------------------------------------

### Use plate as proxy variable for age of the cars
targa_normal_gest = grep("\\*", dt$targa_gestionaria)

dt.n = dt[targa_normal_gest] %>% .[, targa_group_gest := 'normal']
dt.nn = dt[!targa_normal_gest] %>% .[, targa_group_gest := 'special']

dt = rbind(dt.n, dt.nn)




### MOTO X, MOTOCICLI 2 LET, 4 NUM
### 
dt[, `:=` (year_gestionaria = str_mid(targa_gestionaria, 2,2))]

dt[, `:=` (year_gestionaria = fifelse(targa_group_gest == 'normal', year_gestionaria, 'special'))]

plot(as.factor(dt$year_gestionaria))



### Use Month of the collision 

dt[, datetime_collision := ymd(data_accadimento)]
dt[, `:=` (month = month(datetime_collision),
           day = day(datetime_collision))]






# E. EXPORT ----------------------------------------------------------------


dt[, liquidato_basso := as.factor(fifelse(liquidato_basso == 'Si', 1, 0))]


### INSERT FILE NAME TO TEST HERE ### 
write_parquet(dt, file.path('data', 'clean', 'dt_test.parquet'))
### INSERT FILE NAME TO TEST HERE ### 



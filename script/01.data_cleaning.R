
# A. Packages --------------------------------------------

library(here)
library(data.table)
library(skimr)
library(readxl)
library(magrittr)
library(innteamUtils)
library(lubridate)


### Aux Data
dc = innteamUtils::decodifica_comuni

# B. Data Upload ---------------------------------------

df = read_excel(here('data', 'inputs', '210916 Supporto Modello predittivo.xlsx'),
                sheet = 2)

#skim(df)

dt = as.data.table(df)
dt = janitor::clean_names(dt)





# C. EDA =======================================================================


## FACTOR ---------------------------------------------------------

### Denuncia Mittente
dt[, denuncia_mittente := as.factor(denuncia_mittente)]
#plot(dt$denuncia_mittente)    


### Denuncia Firme
dt[, denuncia_firme := as.factor(denuncia_firme)]
#plot(dt$denuncia_firme) 


### Responsabilita Totale o Parziale
dt[, denuncia_resp_totale_o_parziale := as.factor(denuncia_resp_totale_o_parziale)]
#plot(dt$denuncia_resp_totale_o_parziale) 


### Responsabilita Totale o Parziale
dt[, eventuali_dichiarazioni_difformi := as.factor(eventuali_dichiarazioni_difformi)]
#plot(dt$eventuali_dichiarazioni_difformi)


### Responsabilita Totale o Parziale
dt[, veicolo_gestionaria_da_ania := as.factor(veicolo_gestionaria_da_ania)]
#plot(dt$veicolo_gestionaria_da_ania)


### Responsabilita Totale o Parziale
dt[, residenza_gestionaria_da_ania := as.factor(residenza_gestionaria_da_ania)]
plot(dt$residenza_gestionaria_da_ania)


### Responsabilita Totale o Parziale
dt[, tipo_sx := as.factor(tipo_sx)]
#plot(dt$tipo_sx)


### Responsabilita Totale o Parziale
dt[, ultimo_codice_resp := as.factor(ultimo_codice_resp)]
#plot(dt$ultimo_codice_resp)

#plot(prop.table(table(as.factor(dt$liquidato_basso))))


## NUMERIC --------------------------------------------------------------------

### Responsabilita Totale o Parziale
dt_10k = dt[pagato_da_unipol_sai <= 9000]
#hist(dt_10k$pagato_da_unipol_sai)





# D. Data Transformation -------------------------------------------------

### Use plate as proxy variable for age of the cars
targa_normal_gest = grep("\\*", dt$targa_gestionaria)
targa_normal_debi = grep("\\*", dt$targa_debitrice)

dt.n = dt[targa_normal_gest] %>% .[, targa_group_gest := 'normal']
#dt.nn = dt[!targa_normal_gest] %>% .[, targa_group_gest := 'special']


dt.n2 = dt[targa_normal_debi]
dt.n2[, targa_group_gest := 'normal']
#dt.nn2 = dt[!targa_normal_debi]
#dt.nn2[, targa_group_debi := 'special']

dt = rbind(dt.n, dt.n2)




dt[, `:=` (year_gestionaria = str.mid(targa_gestionaria, 2,2),
           year_debitrice = str.mid(targa_debitrice, 2,2))]

dt[, `:=` (year_gestionaria = fifelse(targa_group_gest == 'normal', year_gestionaria, 'special'),
           year_debitrice = fifelse(targa_group_gest == 'normal', year_debitrice, 'special'))]

plot(as.factor(dt$year_gestionaria))
plot(as.factor(dt$year_debitrice))



### Use Month of the collision 

dt[, datetime_collision := ymd(data_accadimento)]
dt[, `:=` (month = month(datetime_collision),
           day = day(datetime_collision))]






### E.FIRST MODEL ----------------------------------------------------------------



model_fast <- glm(pagato_da_unipol_sai ~ denuncia_mittente + denuncia_firme + denuncia_resp_totale_o_parziale + veicolo_gestionaria_da_ania  + month + year_gestionaria + year_debitrice,
                  data = dt)

summary(model_fast)


dt$predict <- predict(model_fast,
                      newdata = dt,
                      type = 'response')

dt[, validation := fifelse(predict > 500, 'NO', 'Si')]




# Confusion Matrix Recreation =====================================================

dt[, TP_fast := fifelse(liquidato_basso == 'Si' & validation == 'Si', 1, 0)]
dt[, TN_fast := fifelse(liquidato_basso == 'NO' & validation == 'NO', 1, 0)]
dt[, FP_fast := fifelse(liquidato_basso == 'NO' & validation == 'Si', 1, 0)]
dt[, FN_fast := fifelse(liquidato_basso == 'Si' & validation == 'NO', 1, 0)]


total = nrow(dt)
TP_fast = sum(dt$TP_fast) / total * 100
TN_fast = sum(dt$TN_fast) / total * 100
FP_fast = sum(dt$FP_fast) / total * 100
FN_fast = sum(dt$FN_fast) / total * 100




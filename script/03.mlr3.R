



# : ====================================================================




# A. Package Loading ===============================================

library("mlr3verse", quietly = TRUE)
library('mlr3extralearners')
library(innteamUtils)
set.seed(42)



# B. Data Uploading ================================================



### DECODIFICA PROVINCIE - REGIONI

dc = decodifica_comuni[, .(Regione_UPPER_OUT, ACI_OUT)]
setkey(dc, 'ACI_OUT')
dc = unique(dc)
dc[is.na(ACI_OUT)]$ACI_OUT <- 'NA'



## Upload DT ------------------------------------------

dt.classif = readRDS(here('data', 'clean', 'dt_mlr3.rds'))





# C. Data Cleaning =========================================================


dt.classif = dt.classif[, .(denuncia_mittente, denuncia_firme, denuncia_resp_totale_o_parziale, bareme_gest_1_mittente, residenza_gestionaria_da_ania, veicolo_gestionaria_da_ania, tipo_sx, liquidato_basso, year_gestionaria, month, targa_group_gest)]

dt.classif[, `:=`(bareme_gest_1_mittente = as.factor(bareme_gest_1_mittente))]

dt.classif = merge(dt.classif, dc, by.x = 'residenza_gestionaria_da_ania', by.y = 'ACI_OUT', all.x = TRUE)

setnames(dt.classif, names(dt.classif), c('provincia', 'd_mitt', 'd_firme', 'd_respTP', 'bareme_1', 'tipo_veicolo', 'tipo_sx', 'liq_basso', 'veicolo_anno', 'mese', 'g_targa', 'regione'))

dt.classif = dt.classif[, regione := fifelse(is.na(regione), 'CAMPANIA', regione)]

## ERROR ## REGIONE HA 495 MISSING
dt.classif[, `:=` (provincia = as.factor(provincia),
                   veicolo_anno = as.factor(veicolo_anno),
                   g_targa = as.factor(g_targa),
                   regione = as.factor(regione))]

dt.classif = dt.classif[, provincia := NULL]


# : =======================================================================================






# D. PROBLEM SETTING ===============================================================

### Set the task


dt.classif_train_si = dt.classif[liq_basso == 1]
dt.classif_train_si = dt.classif_train_si[sample(.N, 30000)]

dt.classif_train_no = dt.classif[liq_basso == 0]
dt.classif_train_no = dt.classif_train_no[sample(.N, 45000)]

dt.classif_train = rbind(dt.classif_train_no, dt.classif_train_si)

task <- as_task_classif(dt.classif_train, target = 'liq_basso', id = 'UNI')
task$col_roles$stratum = c('regione', 'tipo_veicolo')


### learners groups
lrn_1 <- lrn(('classif.log_reg'))

lrn_1$train(task)
predictions = lrn_1$predict(task)

predictions = lrn_1$predict_newdata(dt.classif)


measure = msrs(c('classif.acc'))
predictions$score(measure)

predictions$confusion

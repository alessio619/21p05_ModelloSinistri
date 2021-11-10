



# : ====================================================================




# A. Package Loading ===============================================

library(mlr3verse)
library(innteamUtils)
library(arrow)

set.seed(42)



# B. Data Uploading ================================================

## Upload DT ------------------------------------------

dt.classif = read_parquet(file.path('data', 'clean', 'train_set.parquet'))




# C. PROBLEM SETTING ===============================================================

### Set the task


dt.classif_train_si = dt.classif[liq_basso == 1]
dt.classif_train_si = dt.classif_train_si[sample(.N, 30000)]

dt.classif_train_no = dt.classif[liq_basso == 0]
dt.classif_train_no = dt.classif_train_no[sample(.N, 45000)]

dt.classif_train = rbind(dt.classif_train_no, dt.classif_train_si)

task <- as_task_classif(dt.classif_train, target = 'liq_basso', id = 'UNI')
task$set_col_roles(c('regione', 'tipo_veicolo'), add_to = 'stratum')
task$set_col_roles(c('cinquina'), add_to = 'name', remove_from = 'feature')

### learners groups
lrn_1 <- lrn(('classif.log_reg'), predict_type = "prob")





## Train the Model -----------------------------------------

lrn_1$train(task)
predictions = lrn_1$predict(task)

predictions = lrn_1$predict_newdata(dt.classif)

measure = msrs(c('classif.acc', 'classif.precision', 'classif.recall', 'classif.sensitivity', 'classif.specificity'))
predictions$score(measure)

predictions$confusion
predictions$prob[,2]
predictions$response

summary(lrn_1$model)







# D. APPLICATION ===============================================================

dt.controllo = read_parquet(file.path('data', 'clean', 'dt_test.parquet'))

dt.controllo = dt.controllo[, denuncia_resp_totale_o_parziale := as.character(denuncia_resp_totale_o_parziale)]
dt.controllo = dt.controllo[, denuncia_resp_totale_o_parziale := fifelse(denuncia_resp_totale_o_parziale == 'R', 'T', denuncia_resp_totale_o_parziale)]



dt.controllo = dt.controllo[, .(cinquina, denuncia_mittente, denuncia_firme, denuncia_resp_totale_o_parziale, bareme_gest_1_mittente, residenza_gestionaria_da_ania, veicolo_gestionaria_da_ania, tipo_sx, year_gestionaria, month, targa_group_gest)]

dt.controllo[, `:=`(bareme_gest_1_mittente = as.factor(bareme_gest_1_mittente))]


### DECODIFICA PROVINCIE - REGIONI

dc = decodifica_comuni[, .(Regione_UPPER_OUT, ACI_OUT)]
setkey(dc, 'ACI_OUT')
dc = unique(dc)
dc[is.na(ACI_OUT)]$ACI_OUT <- 'NA'

dt.controllo = merge(dt.controllo, dc, by.x = 'residenza_gestionaria_da_ania', by.y = 'ACI_OUT', all.x = TRUE)

setnames(dt.controllo, names(dt.controllo), c('provincia', 'cinquina', 'd_mitt', 'd_firme', 'd_respTP', 'bareme_1', 'tipo_veicolo', 'tipo_sx', 'veicolo_anno', 'mese', 'g_targa', 'regione'))

dt.controllo = dt.controllo[, regione := fifelse(is.na(regione), 'CAMPANIA', regione)]


dt.controllo[, `:=` (provincia = as.factor(provincia),
                   veicolo_anno = as.factor(veicolo_anno),
                   g_targa = as.factor(g_targa),
                   regione = as.factor(regione))]

dt.controllo = dt.controllo[, provincia := NULL]




predictions = lrn_1$predict_newdata(dt.controllo)


dt.controllo$pred_prob = predictions$prob[,2]
dt.controllo$pred_response = predictions$response

fwrite(dt.controllo, file = 'dt.test.csv')

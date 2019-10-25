
# example Rscript 4.train.xgboost 7 FALSE .70

rm(list=ls())
#directories
dir         <- 'output/'
figures.dir <- 'fig/'

#source
source('utils/Rlibraries.R')
source('src/prepare_data.R')
source('src/visualize_data.R')
source('src/matrizdeconfusion.R')


cat("[INFO] Train Model()\n")
set.seed(3676)

# load data.tables
load(paste0(dir, "dt.train.RData"))
load(paste0(dir, "dt.test.RData"))

dt.train <-  return_numeric_NAs(dt.train)
dt.test <-  return_numeric_NAs(dt.test)

dt.dataset      <- dt.train
dt.dataset.test <- dt.test
dt.dataset <- dt.dataset[, CLASS := convert][, convert := NULL]
dt.dataset.pos <- dt.dataset[CLASS == 1] #failure
dt.dataset.neg <- dt.dataset[CLASS == 0] #no failure
cat("[INFO] Initial sample", "\n")
cat("[INFO] There are: ", nrow(dt.dataset.pos), "positive instances (Failures)\n")
cat("[INFO] There are: ", nrow(dt.dataset.neg), "negative instances (Regular Behaviour)\n")


dt.train.class  <-  dt.dataset[, .(CLASS)]
dt.test         <-  dt.test[, CLASS := convert][, convert := NULL]
dt.test.class   <-  dt.test[ , .(CLASS)]

cat("\n")
cat("[INFO] Number of instances in Train Data:      ", nrow(dt.train), 
    "Positives: ", nrow(dt.train.class[CLASS == '1']), "Negatives: ", nrow(dt.train.class[CLASS == '0']), "\n")


param <- list(
  "eta"              = .1,
  "min_child_weight" = 5, #1,    # small for imbalanced data
  "subsample"        = 0.5, #0.9,
  "colsample_bytree" = 0.85, #0.85, #0.8 to tune
  "max_depth"        = 5, #5,  # OR 6 #learning rate, we start with 5 to learn
  "gamma"            = 1, #turned lated
  "scale_pos_weight" = 1, #1,
  "max_delta_step"   = 1, #1, # for umbalanced data
  verbose            = 1 ,
  nthread            = 5 #, #booster = "gblinear"
) #4 #booster = "gblinear"

dt.train <- dt.train %>%  select(-c('convert'))
dt.test <- dt.test %>%  select(-c('CLASS'))

dtrain    <- xgb.DMatrix(data = as.matrix(dt.train), label = as.numeric(as.character(dt.train.class$CLASS)),  missing = NA)
dtest     <- xgb.DMatrix(data = as.matrix(dt.test), label = as.numeric(as.character(dt.test.class$CLASS)),  missing = NA)
watchlist <- list(train=dtrain, test=dtest)
xgb.model <- xgb.train(param, data=dtrain, nrounds = 5000, watchlist=watchlist,  early_stopping_rounds = 100, maximize=FALSE,
                       #eval_metric= evalerror,
                       eval_metric = "auc", 
                       eval_metric = "logloss", 
                       objective = "binary:logistic"
)

#---------------------------------------------------------------------------------------------------------------------------------
# save xgboostobject

file_name <- "output/model_forest.rds"
saveRDS(rf.train, file_name)

#------------------------------------------------------------------------------------------------------------------------------
# cross validation
cv.nround = 1000
cv.nfold = 5
cross.val <- xgb.cv(data = dtrain, params = param, nfold = cv.nfold, nrounds = cv.nround, metrics = "AUC", verbose = T)
cat("[INFO] Cross validation mean: ", min(cross.val$test.error.mean), "\n")
cat("[INFO] Cross validation sd: ", min(cross.val$test.error.std), "\n")
#-------------------------------------------------------------------------------------------------------------------------------    

cat('[INFO] TRAINING', '\n')
label    <- dt.train.class[, .(CLASS)]
label    <- as.vector(label)
#dt.train <- dt.train.class[, predictor.feats, with = F]
pred     <- predict(xgb.model, newdata =  as.matrix(dt.train), missing = NA)
#save data 
#predicted_training <- pred
dt.predicted <- as.data.table(cbind(dt.dataset[, .(CLASS)], pred))
save(dt.predicted, file = paste0(dir, "predicted_training.xgboost.RData")) 

#------------------------
#density plot

pred.1      <- prediction(pred, dt.train.class$CLASS)
pred.1 
output.file <- paste0(figures.dir, "/", 'xgboost',".density.png")
density_plot(pred.1, output.file, pos = 1, legend = c('NO FAULTS', 'FAULTS'))

#hist_plot_prediction(dt.predicted, type = 'train', model = 'xgboost')
#bip plot
BIP_GRAPH(dt.predicted, type = 'train', model = 'xgboost')
#--------------------------

#MATRIZ DE CONFUSIÓN
threshold <- 0.5
real <- as.vector(as.matrix(label))
g <- pred
result <- rep(0, length(g))
result[g >= threshold] = 1
cat(" [INFO] Confusion Matrix: \n")
mc(result, real, imprimir = 1)


#model.name <- 'train.xgboost'
#output.file <- paste0(figures.dir, "/", model.name,".density.png")
#density_plot(pred, output.file, pos = 1, legend = c('No Incidents', 'Incidents')

cat("\n")
cat(" [INFO] Importance Matrix: \n")
importance <- xgb.importance(feature_names = names(dt.train), model =  xgb.model)
top.ten.importance <- importance[1:20, ]
print(head(top.ten.importance, 20))
# PLOT --------------------------------------------------------------------------------------------------------------------------------------
# top 20
d <- ggplot(top.ten.importance, aes(y = Gain, x = reorder(Feature, Gain))) +  geom_bar(stat = "identity", color = 'blue') + coord_flip()
d <- d + ylab('Importance') + xlab('Features') 
d <- d + theme(axis.title.y = element_text(size = rel(1.5)))
d <- d + theme(axis.text.y = element_text(size = rel(1.8)))
d <- d + theme(axis.text.x = element_text(size = rel(1.8)))
d <- d + theme(axis.title.x = element_text(size = rel(1.5)))
d <- d + theme(legend.text = element_text(size = 15))
d <- d + theme(legend.title = element_text(size = 15))
d <- d +  theme(
  panel.background = element_rect(fill="white") ,
  panel.grid.minor.y = element_line(size=3),
  panel.grid.major = element_line(colour = "lightgray"),
  plot.background = element_rect(fill="white")
)

plot.file <- paste0(figures.dir,"TOP.20.importance_xgboost", ".png")
ggsave(plot.file, width=200, height=200, units="mm")
#xgb.plot.importance(importance_matrix)
#xgb.dump(xgb.model, with.stats = T)

# ALL
d <- ggplot(importance, aes(y = Gain, x = reorder(Feature, Gain))) +  geom_bar(stat = "identity", color = 'blue') + coord_flip()
d <- d + ylab('Importance') + xlab('Features') 
d <- d + theme(axis.title.y = element_text(size = rel(1.5)))
d <- d + theme(axis.text.y = element_text(size = rel(1.8)))
d <- d + theme(axis.text.x = element_text(size = rel(1.8)))
d <- d + theme(axis.title.x = element_text(size = rel(1.5)))
d <- d + theme(legend.text = element_text(size = 15))
d <- d + theme(legend.title = element_text(size = 15))
d <- d +  theme(
  panel.background = element_rect(fill="white") ,
  panel.grid.minor.y = element_line(size=3),
  panel.grid.major = element_line(colour = "lightgray"),
  plot.background = element_rect(fill="white")
)

plot.file <- paste0(figures.dir,"importance_xgboost", ".png")
ggsave(plot.file, width=200, height=200, units="mm")
#------------------------------------------------------------------------------------------------------------------------------------------------
#plot map

#xgb.plot.tree(feature_names = most_predicted.var, model = xgb.model)
xgb.plot.tree(model =  xgb.model)
#------------------------------------------------------------------------------------------------------------------------------------------------
err <- as.numeric(sum(as.integer(pred > threshold) != label))/nrow(label)
cat("\n")
cat(" [INFO] test-error", err, "\n")

mk_stats(pred,  as.numeric(as.vector(as.matrix(label))))
#--------------------------------------------------------------------------------------------------------------------------------------------------------
# TEST
# quitar
#load(ifile)
#dt.test.class <- dt.features.tag
#setnames(dt.test.class, 'LABEL', 'CLASS')
#---------------------------------------------------------------------------------------------------------------------------------------------
cat("\n")
cat("[INFO] Number of instances in Test Data:      ", nrow(dt.test), 
    "Positives: ", nrow(dt.test.class[CLASS == '1']), "Negatives: ", nrow(dt.test.class[CLASS == '0']), "\n")

label   <- dt.test.class[, .(CLASS)]
label   <- as.vector(label)
#dt.test <- dt.test.class #[, predictor.feats, with = F]
pred    <- predict(xgb.model, newdata =  as.matrix(dt.test), missing = NA)
hist(pred)


err <- as.numeric(sum(as.integer(pred > threshold) != label))/nrow(label)
cat("\n")
cat(" [INFO] test-error", err, "\n")

pred.1 <- prediction(pred, dt.test.class$CLASS)
perf.1 <- performance(pred.1, "tpr", "fpr")
png(paste0(figures.dir, "/",'xgboost', ".test.pos.ROC.png"), width = 1024, height = 768)
plot(perf.1, main = "Positive Instances ROC Curve", ylab = 'Recall (TPR)', type = 'S', col = 'blue', lty = 6, lwd = 3, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=2.5)
lines(seq(0,1, 0.01), seq(0,1, 0.01), col = 'blue', lwd = 3)
grid (10,10, lty = 6, col = "cornsilk2")
dev.off()


perf.2 <- performance(pred.1, "tnr", "fnr")
png(paste0(figures.dir, "/", 'xgboost', ".test.neg.ROC.png"), width = 1124, height = 768)
plot(perf.2, main = "Negative Instances ROC Curve", type = 'l', col = 'blue', lty = 6, lwd = 3, cex.lab=2.5, cex.axis=2.5, cex.main=1.5, cex.sub=2.5)
lines(seq(0,1, 0.01), seq(0,1, 0.01), col = 'blue', lwd = 3)
dev.off()

#recall
perf.3 <- performance(pred.1, "rec")

png(paste0(figures.dir, "/",'xgboost', ".recall.png"), width = 1024, height = 768)
plot(perf.3, main = "RECALL", ylab = 'Recall (TPR)', type = 'S', col = 'blue', lty = 6, lwd = 3, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=2.5)
grid (10,10, lty = 6, col = "cornsilk2")
dev.off()

#precision
perf.4 <- performance(pred.1, "prec")
#perf.3 <- performance(pred.1, "f")
png(paste0(figures.dir, "/",'xgboost', ".PREC.png"), width = 1024, height = 768)
plot(perf.4, main = "PREC", ylab = 'PREC', type = 'S', col = 'blue', lty = 6, lwd = 3, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=2.5)
grid (10,10, lty = 6, col = "cornsilk2")
dev.off()

#F
perf.5 <- performance(pred.1, "f")
png(paste0(figures.dir, "/",'xgboost', ".F.png"), width = 1024, height = 768)
plot(perf.5, main = "F", ylab = 'F', type = 'S', col = 'blue', lty = 6, lwd = 3, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=2.5)
grid (10,10, lty = 6, col = "cornsilk2")
dev.off()

dt.predicted <- as.data.table(cbind(dt.test.class[, .(CLASS)], pred))
save(dt.predicted, file = paste0(dir, "predicted.test.xgboost.RData")) 
#------------------------
#density plot
pred.1 <- prediction(pred, dt.test.class$CLASS)
output.file <- paste0(figures.dir, "/", 'xgboost.test',".density.png")
density_plot(pred.1, output.file, pos = 1, legend = c('NO FAULT', 'FAULT'))

hist_plot_prediction(dt.predicted, type = 'test', model = 'xgboost')
#bip plot
BIP_GRAPH(dt.predicted, type = 'test', model = 'xgboost')
#--------------------------

#model.name <- 'train.xgboost'
#output.file <- paste0(figures.dir, "/", model.name,".density.png")
#density_plot(pred, output.file, pos = 1, legend = c('No Incidents', 'Incidents'))

#MATRIZ DE CONFUSIÓN
real <- as.vector(as.matrix(label))
g <- pred
predicted.test <- pred
result <- rep(0, length(g))
result[g >= threshold] = 1
cat(" [INFO] Confusion Matrix: \n")
mc(result, real, imprimir = 1)

# #---------------------------------------------------------------------------------------------------
# #test data
# # source(paste0('/src/prepare_data.R'))
# 
# dt.test.t      <- cbind(dt.test,   dt.dataset.test$convert)
# setnames(dt.test.t, c('V2'), c('Sales'))
# dt.result_test <- expected_values_xgboost_sales(dt.test.t, text = 'test_data')
# dim(dt.result_test)
# #---------------------------------------------------------------------------------------------------
# #second dataset
# set.seed(5)
# source(paste0(base.dir, '/src/prepare_data.R'))
# load(paste0(dir, "dt.set2.RData"))
# 
# dt.result <- expected_values_xgboost(dt.set2, text = 'true_data')
# 
# head(dt.result)
# dim(dt.result)
# 
# dt <- dt.result[1:450, ]
# dt <- dt[, expected_random_price := sum(random_price1)]

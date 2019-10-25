

rm(list=ls())
#directories
dir         <- 'output/'
figures.dir <- 'fig/'

#source
source('utils/Rlibraries.R')
source('src/prepare_data.R')
source('src/visualize_data.R')
source('src/matrizdeconfusion.R')


# load data.tables
# load(paste0(dir, "dt.train.RData"))
# load(paste0(dir, "dt.test.RData"))
load(paste0(dir, "dt.train_with_iteractions.RData"))
load(paste0(dir, "dt.test_with_iteractions.RData"))

dt.train  <- dt.train %>%  select(convert, premium, annual_revenue ,ratio_carrier_winning, total_payroll, 
                                  ratio_wins_products, Number_year_established,                               
  avg_carriers_winning_premium, Carriers_winning_premium_per_sell,                      
  Number_wins_products, num_employees, Carriers_winning_bids, Number_bids_products,                                  
  Carriers_bids, sector_Food.and.Accommodation,                         
  sector_Professional..Scientific.and.Technical.Services, sector_Contractors,                                    
  sector_Consultants, max_carriers_winning_premium,                          
  sector_Other.Services, Region_SOUTH,                                          
  business_structure_Limited.Liability.Company, sector_other,                                          
  sd_carriers_winning_premium, Region_NORTHEAST,                                      
  sector_Retail.Trade, business_structure_Individual,                         
  Region_WEST, rng_carriers_winning_premium,                          
  sector_Manufacturing, sector_Healthcare #,                                     
  # sector_UNKNOWN, sector_Transportation.and.Warehousing,                 
  # business_structure_Non.Profit, business_structure_Partnership,                        
  # business_structure_Other, business_structure_Not.sure.yet,                       
  # business_structure_Limited.Partnership, Region_UNKNOWN,                                        
  # min_carriers_winning_premium, business_structure_Trust,                              
  # business_structure_UNKNOWN 
  # iteractions
  # `ratio_wins_products:ratio_carrier_winning`,
  # `ratio_wins_products:avg_carriers_winning_premium` #,
  # Number_wins_products:avg_carriers_winning_premium,
  # Number_wins_products:Carriers_winning_premium_per_sell,
  # ratio_wins_products:Carriers_winning_premium_per_sell,
  # Number_bids_products:ratio_carrier_winning,
  # ratio_wins_products:Carriers_winning_bids,
  # Number_bids_products:avg_carriers_winning_premium,
  # Number_bids_products:Carriers_bids,
  # ratio_wins_products:Carriers_bids
  )
                 
dt.test  <- dt.test  %>%  select(convert, premium, annual_revenue ,ratio_carrier_winning, total_payroll, ratio_wins_products, 
                                 Number_year_established,                               
                                 avg_carriers_winning_premium, Carriers_winning_premium_per_sell,                      
                                 Number_wins_products, num_employees, Carriers_winning_bids, Number_bids_products,                                  
                                 Carriers_bids, sector_Food.and.Accommodation,                         
                                 sector_Professional..Scientific.and.Technical.Services, sector_Contractors,                                    
                                 sector_Consultants, max_carriers_winning_premium,                          
                                 sector_Other.Services, Region_SOUTH,                                          
                                 business_structure_Limited.Liability.Company, sector_other,                                          
                                 sd_carriers_winning_premium, Region_NORTHEAST,                                      
                                 sector_Retail.Trade, business_structure_Individual,                         
                                 Region_WEST, rng_carriers_winning_premium,                          
                                 sector_Manufacturing, sector_Healthcare #,                                     
                                 # sector_UNKNOWN, sector_Transportation.and.Warehousing,                 
                                 # business_structure_Non.Profit, business_structure_Partnership,                        
                                 # business_structure_Other, business_structure_Not.sure.yet,                       
                                 # business_structure_Limited.Partnership, Region_UNKNOWN,                                        
                                 # min_carriers_winning_premium, business_structure_Trust,                              
                                 # business_structure_UNKNOWN 
                                 # iteractions
                                 # `ratio_wins_products:ratio_carrier_winning`,
                                 # `ratio_wins_products:avg_carriers_winning_premium` #,
                                 # Number_wins_products:avg_carriers_winning_premium,
                                 # Number_wins_products:Carriers_winning_premium_per_sell,
                                 # ratio_wins_products:Carriers_winning_premium_per_sell,
                                 # Number_bids_products:ratio_carrier_winning,
                                 # ratio_wins_products:Carriers_winning_bids,
                                 # Number_bids_products:avg_carriers_winning_premium,
                                 # Number_bids_products:Carriers_bids,
                                 # ratio_wins_products:Carriers_bids
  
                     )
  

# dt.train <-  return_numeric_NAs(dt.train)
# dt.test <-  return_numeric_NAs(dt.test)
#Random forest

dt.train.class <- dt.train[, c('convert'), with = F]  
dt.test.class  <- dt.test[,  c('convert'), with = F]
dt.train <- dt.train[, -c('convert'), with = F]
dt.test  <- dt.test[,  -c('convert'), with = F]


cat("\n")
cat("[INFO] Number of instances in Train Data:      ", nrow(dt.train), 
    "Positives: ", nrow(dt.train.class[convert == 1]), "Negatives: ", nrow(dt.train.class[convert == 0]), "\n")

set.seed(3676)
n_size   <- 7 #10
max_nodes <- 10
number_of_trees <- 50 # obtained by cross-validation
cat("[INFO] Training model...\n")
model.name <- 'train.RF'
rf.train  <- randomForest(dt.train, y = as.factor(dt.train.class$convert), 
                          # cutoff works like a cost function
                          # cutoff = c(nrow(dt.train.class[CLASS == '1',])/nrow(dt.train.class), nrow(dt.train.class[CLASS == '0',])/nrow(dt.train.class)), 
                          #maxnodes =  max_nodes, #Maximum number of terminal nodes
                          #sampsize = 1, #Size(s) of sample to draw. 
                          #method = "class", 
                          ntree = number_of_trees, #number.trees,
                          nodesize = n_size, #minimum size of terminal nodes, , cutoff  classwt
                          mtry = sqrt(ncol(dt.train)),      # number of var sampled
                          #replace = T,
                          do.trace = T #,
                          # na.action = na.omit
)

#---------------------------------------------------------------------------------------------------------------------------------
# save random forest object

file_name <- "output/model_forest.rds"
saveRDS(rf.train, file_name)
#---------------------------------------------------------------------------------------------------------------------------------
# 
# rf.cv <- rf.crossValidation(rf.train, dt.train, p=0.10, n= 10, ntree= 50)
# rf.cv

png(paste0(figures.dir, "/", model.name, "-", ".VarImpPlot.png"), width = 1024, height = 768)
varImpPlot(rf.train, main = paste0("VarImpPlot ", model.name), n.var = 60)
dev.off()

png(paste0(figures.dir, "/", model.name, "-", ".error.png"), width = 1024, height = 768)
plot(rf.train, log = "y", main = paste0("Error ", model.name))
dev.off()

#⨪--------------------------------------------------------------------------------------------------------------------
# PLOT --------------------------------------------------------------------------------------------------------------------------------------
# top 20
dt.importance <- melt(as.matrix(rf.train$importance))
setorder(dt.importance, -value)
dt.importance$Var1
d <- ggplot(dt.importance, aes(y = value, x = reorder(Var1, value))) +  geom_bar(stat = "identity", color = 'blue') + coord_flip()
d <- d + ylab('MeanDecreaseGini') + xlab('Features') 
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

d
plot.file <- paste0(figures.dir,"TOP.RF", ".png")
ggsave(plot.file, width=200, height=200, units="mm")

#⨪-----------------------------------------------------------------------------------------------------------------


pred.rf.train <- predict(rf.train, dt.train, type = "prob")
pred <- pred.rf.train[,2]
label <- as.numeric(as.vector(as.matrix(dt.train.class$convert)))
dt.predicted <- as.data.table(cbind(label, pred)) 
save(dt.predicted, file = paste0(dir, "predicted.RF.training.RData")) 

dt.predicted.1      <- copy(dt.predicted)
names(dt.predicted.1) <- c('CLASS', 'pred')
results <- BIP_GRAPH(dt.predicted.1, type = 'train', model = 'rf') #0.52631579

#MATRIZ DE CONFUSIÓN
threshold <- 0.52631579
source("src/matrizdeconfusion.R")
real <- as.vector(as.matrix(label))
g <- pred
predicted.test <- pred
result <- rep(0, length(g))
result[g >= threshold] = 1
cat(" [INFO] Confusion Matrix: \n")
mc(result, real, imprimir = 1)

#density plot
# output.file <- paste0(figures.dir, "/", model.name,".density.png")
# density_plot(as.factor(pred), output.file, pos = 1, legend = c('No Incidents', 'Incidents'))

# TEST
cat("\n")
cat("[INFO] Number of instances in Test Data:      ", nrow(dt.test), "Positives: ", nrow(dt.test.class[convert == 1]), "Negatives: ", nrow(dt.test.class[convert == 0]), "\n")

model.name <- 'test.RF'

cat("\n")
cat("[INFO] Number of instances in Test Data:      ", nrow(dt.test), 
    "Positives: ", nrow(dt.test.class[convert == '1']), "Negatives: ", nrow(dt.test.class[convert == '0']), "\n")

cat("predict with type = prob\n")
pred.rf.test.1 <- predict(rf.train, dt.test, type = "prob")
pred.1 <- prediction(pred.rf.test.1[,2], dt.test.class$convert)


output.file <- paste0(figures.dir, "/", model.name,".density.png")
density_plot(pred.1, output.file, pos = 1, legend = c('No Incidents', 'Incidents'))


perf.1 <- performance(pred.1, "tpr", "fpr")
png(paste0(figures.dir, "/", model.name, ".pos.ROC.png"), width = 1024, height = 768)
plot(perf.1, main = "Positive Instances ROC Curve")
dev.off()

perf.2 <- performance(pred.1, "tnr", "fnr")
png(paste0(figures.dir, "/", model.name, ".neg.ROC.png"), width = 1024, height = 768)
plot(perf.2, main = "Negative Instances ROC Curve")
dev.off()


pred  <- pred.rf.test.1[,2]
label <- dt.test.class$convert
dt.predicted <- as.data.table(cbind(label, pred))
save(dt.predicted, file = paste0(dir, "predicted.RF.test.RData")) 
#MATRIZ DE CONFUSIÓN
source(paste0("src/matrizdeconfusion.R"))
real <- as.vector(as.matrix(label))
g <- pred
predicted.test <- pred
result <- rep(0, length(g))
test.threshold <- threshold
result[g >= test.threshold] = 1
cat(" [INFO] Confusion Matrix: \n")
mc(result, real, imprimir = 1)

cat('[INFO]: Saving data...', '\n')


mk_stats(pred,  as.numeric(as.vector(as.matrix(label))))
